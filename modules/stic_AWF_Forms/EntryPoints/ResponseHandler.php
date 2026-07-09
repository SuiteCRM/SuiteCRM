<?php
/**
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License along with
 * this program; if not, see http://www.gnu.org/licenses or write to the Free
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 *
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once "modules/stic_AWF_Forms/core/includes.php";

/**
 * EntryPoint: ResponseHandler
 * Entry point class responsible for handling form responses submitted by users. 
 * It processes the incoming data, performs validations, detects spam, saves the response, and executes the defined action flows based on the form configuration.
 */
class ResponseHandler
{
    /** Main method to handle the form response submission. It performs the following steps:
     * 1. Retrieves and sanitizes input data
     * 2. Detects potential spam submissions using honeypot, time trap, and user agent checks
     * 3. Validates the form data against the form configuration
     * 4. Saves the response in the database with an initial status
     * 5. Executes the defined action flows based on the form configuration and updates the response status accordingly
     */
    public function run(): void {
        global $current_user;
        
        // If we have a real user, we will use it as execution context, 
        // but we will switch to admin to avoid permission issues when processing the response

        // Real user (before changing to admin)
        $realUserId = null;
        // If SuiteCRM does not load the session and user (auth is false), get authenticated_user_id from the session (if available)
        if (empty($current_user) || empty($current_user->id)) {
            if (isset($_SESSION['authenticated_user_id'])) {
                $current_user = BeanFactory::getBean('Users', $_SESSION['authenticated_user_id']);
            }
        }
        if (!empty($current_user) && !empty($current_user->id)) {
            $realUserId = $current_user->id;
        }

        // Admin user
        $current_user = BeanFactory::newBean('Users');
        $current_user->getSystemUser();

        // Data retrieval
        $formId = $_REQUEST['id'] ?? null;
        $rawPostData = $_POST;
        $cleanData = $this->sanitizeInput($rawPostData);

        $isSpam = false;
        $userAgent = $_SERVER['HTTP_USER_AGENT'] ?? '';
        $remoteIp = $_SERVER['REMOTE_ADDR'] ?? '';

        // Anti-Spam Detection 
        // 1- Honeypot: Hidden field that bots usually fill in
        if (!$isSpam) {
            if (isset($cleanData['awf_honey_pot']) && $cleanData['awf_honey_pot'] !== '') {
                $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ": ResponseHandler: Spam detected by Honeypot protection");
                $isSpam = true;
                $responseDescription = translate('LBL_RESPONSE_HONEYPOT_SPAM', 'stic_AWF_Responses');
            }
        }
        // 2- TimeTrap: Normally bots submit the form immediately and/or without executing JS
        if (!$isSpam) {
            $currentTs = microtime(true);
            $submissionTs = (float)($_POST['awf_submission_ts'] ?? $currentTs);
            $duration = round($currentTs - $submissionTs, 2);
            if ($submissionTs === 0 || $duration < 2.0) {
                $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ": ResponseHandler: Spam detected by Timetrap protection. Duration: {$duration}s");
                $isSpam = true;
                $responseDescription = translate('LBL_RESPONSE_TIMETRAP_SPAM', 'stic_AWF_Responses'). " ({$duration}s)";
            } else {
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": ResponseHandler: Form valid submission. Time to complete: {$duration}s");
            }
        }
        // 3- UserAgent: Some bots don't impersonate browsers
        if (!$isSpam) {
            if ($this->isBotUserAgent($userAgent)) {
                $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ": ResponseHandler: Spam detected by UserAgent filter");
                $isSpam = true;
                $responseDescription = translate('LBL_RESPONSE_USERAGENT_SPAM', 'stic_AWF_Responses');
            }
        }

        // Form URL
        $formUrl = $_POST['awf_form_url'] ?? $_SERVER['HTTP_REFERER'] ?? '';
        $formUrl = substr(strip_tags($formUrl), 0, 255);

        // Clean URL
        $cleanUrl = '';
        if (!empty($formUrl)) {
            $parsedUrl = parse_url($formUrl);
            $scheme = isset($parsedUrl['scheme']) ? $parsedUrl['scheme'] . '://' : '';
            $host = $parsedUrl['host'] ?? '';
            $path = $parsedUrl['path'] ?? '';
            $cleanUrl = $scheme . $host . $path;
        }

        // Data sanitization
        unset($cleanData['module']);
        unset($cleanData['action']);
        unset($cleanData['entryPoint']);
        unset($cleanData['id']);
        unset($cleanData['awf_honey_pot']);
        unset($cleanData['awf_submission_ts']);
        unset($cleanData['awf_form_url']);


        // Initial validations
        if (empty($formId)) {
            if ($isSpam) {
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": ResponseHandler: Spam without formId");
                stic_AWFUtils::renderGenericSpamResponse();
                return;
            }
            $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ": ResponseHandler. EntryPoint called without ID");
            $this->terminateRawError("No Form ID provided.");
        }

        /** @var stic_AWF_Forms $formBean */
        $formBean = BeanFactory::getBean('stic_AWF_Forms', $formId);
        if (!$formBean || empty($formBean->id)) {
            if ($isSpam) {
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": ResponseHandler: Spam with bad formId");
                stic_AWFUtils::renderGenericSpamResponse();
                return;
            }
            $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ": ResponseHandler: Form not found. ID: $formId");
            $this->terminateRawError("Form not found.");
        }

        // Duplicate detection: Fingerprint
        //      For humans: check if we have received the same response from the same origin within a 5-minute window
        //      For bots: check if we have received the same response from the same origin at any time
        if ($isSpam) {
            $timeSlot = 0; // $timeSlot is const
        } else {
            $timeWindow = 300; // 300s = 5 min
            $timeSlot = floor(time() / $timeWindow); // $timeSlot will change every 5 minutes: we avoid accidental F5
        }
        $payloadJson = json_encode($cleanData);

        $fingerprintString = $payloadJson . $formId . $remoteIp . $userAgent . $formUrl . $timeSlot;
        $responseHash = md5($fingerprintString);

        if ($this->checkDuplicateSubmission($formId, $responseHash)) {
            if ($isSpam) {
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": ResponseHandler: Spam duplicated");
                stic_AWFUtils::renderGenericSpamResponse();
                return;
            }
            $GLOBALS['log']->warn('Line ' . __LINE__ . ': ' . __METHOD__ . ": ResponseHandler: Duplicated response detected");
            $this->handleDuplicateError($formBean);
            return;
        }

        // Load the configuration
        $jsonConfig = $formBean->configuration;
        $jsonConfig = html_entity_decode($jsonConfig, ENT_QUOTES, 'UTF-8');
        $configData = json_decode($jsonConfig, true);
        if (!$configData) {
            $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ": ResponseHandler: Form Configuration not found. ID: $formId");
            $this->terminateRawError("Invalid Form Configuration.");
        }
        $formConfig = FormConfig::fromJsonArray($configData);

        // AJAX: Remote validation for js
        if (isset($_REQUEST['ajax_validation_only']) && $_REQUEST['ajax_validation_only'] == '1') {
            if (ob_get_length()) ob_clean();
            header('Content-Type: application/json');

            if ($isSpam) {
                echo json_encode(['status' => 'success']); 
                exit;
            }

            $errors = $this->validateSubmission($formConfig, $cleanData);
            if (!empty($errors) && !empty($errors['errors'])) {
                echo json_encode(['status' => 'error', 'errors' => $errors['errors']]);
            } else {
                echo json_encode(['status' => 'success']);
            }
            exit; // Exit: Only validation, no data saved
        }

        // We look for the response status
        $isPublic = ($formBean->status === 'public');
        if ($isSpam) {
            $responseStatus = 'spam';
        } elseif ($isPublic) {
            $responseStatus = 'pending';
            $responseDescription = '';

            // Data validation
            $validationErrors = $this->validateSubmission($formConfig, $cleanData);
            if (!empty($validationErrors) && !empty($validationErrors['errorDescriptions'])) {
                $errorString = implode(", ", $validationErrors['errorDescriptions']);
                $GLOBALS['log']->warn('Line ' . __LINE__ . ': ' . __METHOD__ . ": ResponseHandler: Data Validation errors for Form ID {$formId}:" . $errorString);

                $errorTitle = translate('LBL_ERROR_GENERIC_TITLE', 'stic_AWF_Responses');
                stic_AWFUtils::renderErrorWithBackButton($formConfig, $errorTitle, $validationErrors['errorDescriptions']);
                return;
            }
        } else {
            global $app_list_strings;

            $responseStatus = 'rejected';
            $responseDescription = translate('LBL_RESPONSE_NO_PUBLIC_STATUS', 'stic_AWF_Responses')." ".
                                   translate('LBL_STATUS', 'stic_AWF_Responses').": ". 
                                   "'{$app_list_strings['stic_awf_responses_status_list'][$formBean->status]}'";
        }

        // Execution context
        $defaultAssignedUserId = $realUserId ?? $formBean->assigned_user_id;
        if (empty($defaultAssignedUserId)) {
            $defaultAssignedUserId = $current_user->id;
        }

        // Save the response
        $responseBean = BeanFactory::newBean('stic_AWF_Responses');
        $responseBean->is_automated_save = true;
        $responseBean->name = $formBean->name ." - ". date('Y-m-d H:i:s');
        $responseBean->status = $responseStatus;
        $responseBean->raw_payload = $payloadJson;
        $responseBean->response_hash = $responseHash;
        $responseBean->remote_ip = $remoteIp;
        $responseBean->form_url = $formUrl;
        $responseBean->clean_referrer = $cleanUrl;
        $responseBean->user_agent = $userAgent;
        $responseBean->description = $responseDescription;
        $responseBean->assigned_user_id = $formBean->assigned_user_id;
        $responseBean->save();

        // Execution Context
        $context = new ExecutionContext($formBean->id, $responseBean->id, $cleanData, $formConfig, null, $defaultAssignedUserId, $responseBean);
        $context->visitorUserId = $realUserId;

        // Html Summary
        $htmlSummary = '';
        try {
            $htmlSummary = stic_AWFUtils::generateSummaryHtml($context, ['includeCss' => false]);
        } catch (Exception $e) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": Error generating HTML snapshot for response {$responseBean->id}: " . $e->getMessage());
            $htmlSummary = "<div class='alert alert-danger'>". translate('LBL_ERROR_GENERATING_HTML_SUMMARY', 'stic_AWF_Responses') ."</div>";
        }
        $responseBean->html_summary = $htmlSummary;
        $responseBean->save();


        // Link the response with the form
        if ($formBean->load_relationship('stic_69c1s_responses')) {
            $formBean->stic_69c1s_responses->add($responseBean->id);
        } else {
            $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ": ResponseHandler: Could not load relationship form-responses for Form ID {$formId}");
            if ($isSpam) {
                stic_AWFUtils::renderGenericSpamResponse();
                return;
            }
            $this->terminateRawError("Form relationship not found.");
        }

        // Stop if it's SPAM (Fake success)
        if ($isSpam) {
            $GLOBALS['log']->warn('Line ' . __LINE__ . ': ' . __METHOD__ . ": ResponseHandler: Spam detected and saved for Form ID: $formId");

            // Update spam counter
            $db = DBManagerFactory::getInstance();
            $safeId = $db->quote($formId);
            $db->query("UPDATE stic_awf_forms SET analytics_spam = analytics_spam + 1 WHERE id = '$safeId'");
            
            stic_AWFUtils::renderGenericSpamResponse();
            return;
        }

        // Only 'public' forms process responses
        if (!$isPublic) {
            $title = $formConfig->layout->closed_form_title ?? translate('LBL_THEME_CLOSED_FORM_TITLE_VALUE', 'stic_AWF_Forms');
            $msg = $formConfig->layout->closed_form_text ?? translate('LBL_THEME_CLOSED_FORM_TEXT_VALUE', 'stic_AWF_Forms');
            stic_AWFUtils::renderGenericResponse($formConfig, $title, $msg);
            return;
        }

        // Update valid responses counter
        $db = DBManagerFactory::getInstance();
        $safeId = $db->quote($formId);
        $db->query("UPDATE stic_awf_forms SET analytics_submissions = analytics_submissions + 1 WHERE id = '$safeId'");

        // Execution flow
        $executor = new ServerActionFlowExecutor($context);
        
        // Preparation of the Action Flows
        //    0: Main    (Main)
        //    1: Receipt (Received/Confirmation for 'async')
        //   -1: OnError (Error)
        $mainFlow = $formConfig->flows['0'] ?? null;
        $receiptFlow = $formConfig->flows['1'] ?? null;
        $errorFlow = $formConfig->flows['-1'] ?? null;

        $isAsync = ($formBean->processing_mode === 'async');
        if ($isAsync) {
            // Async mode - Execute Flow 1: 'receiptFlow' for immediate data feedback to user
            if ($receiptFlow) {
                // Execute all actions
                $lastResult = $executor->executeFlow($receiptFlow, $errorFlow);
                // We don't update the status: it will continue to be 'pending'

                // Get last action and check if is Terminal
                $lastAction = $lastResult->getAction();
                if ($lastAction instanceof ITerminalAction) {
                    try {
                        $lastAction->performTerminal($context, $lastResult);
                    } catch (\Throwable $t) {
                        $context->addError($t, $lastResult->actionConfig);
                        $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": Error performing Terminal action {$lastResult->actionConfig?->name}: " . $t->getMessage());
                    }
                }
                // No terminal (or error runing terminal)
                $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ": Terminal action not found or failed in Receipt flow in form. ID: $formId");
            } else {
                // No receipt flow
                $GLOBALS['log']->warn('Line ' . __LINE__ . ': ' . __METHOD__ . ": Receipt flow not found in form. ID: $formId");
            }
            // If we get here, no flow or terminal has been run: Show generic message
            $title = $formConfig->layout->receipt_form_title ?? translate('LBL_THEME_RECEIPT_FORM_TITLE_VALUE', 'stic_AWF_Forms');
            $msg = $formConfig->layout->receipt_form_text ?? translate('LBL_THEME_RECEIPT_FORM_TEXT_VALUE', 'stic_AWF_Forms');
            stic_AWFUtils::renderGenericResponse($formConfig, $title, $msg);                

        } else {
            // Sync mode - Execute Flow 0: 'mainFlow' immediately
            $responseBean->status = 'processing';
            $responseBean->save();

            if ($mainFlow) {
                // Execute all actions
                $lastResult = $executor->executeFlow($mainFlow, $errorFlow);

                // Save traceability links (created/modified records)                
                $this->saveLinks($responseBean, $context);

                // Generate analytical response details
                $this->generateResponseDetails($responseBean, $formBean, $formConfig, $cleanData);

                // Update status and generate execution log
                $hasErrors = false;
                $logSummary = "[" . date('Y-m-d H:i:s') . "]\n";
                foreach ($context->actionResults as $result) {
                    if ($result->isError()) {
                        $hasErrors = true;
                        $icon = translate('LBL_EXECUTION_ITEM_ERROR', 'stic_AWF_Responses');
                    } elseif ($result->isSkipped()) {
                        $icon = translate('LBL_EXECUTION_ITEM_SKIPPED', 'stic_AWF_Responses');
                    } else {
                        $icon = translate('LBL_EXECUTION_ITEM_OK', 'stic_AWF_Responses');
                    }
                    $actionName = $result->actionConfig->text ?? $result->actionConfig->name ?? 'Unknown Action';
                    $logSummary .= "{$icon} {$actionName}";
                    if (!empty($result->message)) {
                        $logSummary .= ": " . $result->message;
                    }
                    $logSummary .= "\n";
                }
                $responseBean->execution_log = $logSummary;

                // Update status
                if ($lastResult->isError()) {
                    $responseBean->status = 'error';
                } elseif ($lastResult->isWait()) {
                    $responseBean->status = 'awaiting_action'; 
                } else {
                    $responseBean->status = $hasErrors ? 'error' : 'processed';
                }
                $responseBean->save();

                if ($lastResult->isWait()) {
                    $this->createDeferredTicket($context, $lastResult);
                }

                // Get last action and check if is Terminal
                $lastAction = $lastResult->getAction();
                if ($lastAction instanceof ITerminalAction) {
                    try {
                        $lastAction->performTerminal($context, $lastResult);
                    } catch (\Throwable $t) {
                        $context->addError($t, $lastResult->actionConfig);
                        $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": Error performing Terminal action in Main flow {$lastResult->actionConfig?->name}: " . $t->getMessage());
                    }
                }
                // No terminal (or error runing terminal): Show generic message
                $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ": Terminal action not found or failed in Main flow in form. ID: $formId");
                if ($hasErrors) {
                    $title = translate('LBL_ERROR_GENERIC_TITLE', 'stic_AWF_Responses');
                    $msg = translate('LBL_ERROR_GENERIC_MSG', 'stic_AWF_Responses');
                    stic_AWFUtils::renderGenericResponse($formConfig, $title, $msg);
                } else {
                    $title = $formConfig->layout->processed_form_title ?? translate('LBL_THEME_PROCESSED_FORM_TITLE_VALUE', 'stic_AWF_Forms');
                    $msg = $formConfig->layout->processed_form_text ?? translate('LBL_THEME_PROCESSED_FORM_TEXT_VALUE', 'stic_AWF_Forms');
                    stic_AWFUtils::renderGenericResponse($formConfig, $title, $msg); 
                }
                
            } else {
                // If there is no main flow, we show generic message
                $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ": Main flow not found in form. ID: $formId");
                stic_AWFUtils::renderGenericResponse($formConfig, "Error", "Configuration Error: Main flow missing.");
            }
        }
    }

    /**
     * Create a record in the deferred tickets table to resume the flow.
     * @param ExecutionContext $context Execution context of the flow
     * @param ActionResult $lastResult Result of the last action
     */
    private function createDeferredTicket(ExecutionContext $context, ActionResult $lastResult): void {
        $ticket = BeanFactory::newBean('stic_AWF_Deferred_Tickets');
        $ticket->name = "Deferred: " . $context->formId . " - " . date('Y-m-d H:i');
        $ticket->status = 'pending';
        $data = $lastResult->getData();

        // Essential data to recover context
        $ticket->form_id = $context->formId;
        $ticket->response_id = $context->responseId;
        
        // Unique token generation (hash)
        $token = bin2hex(random_bytes(16));
        $ticket->token_hash = $token;

        // Store the specific data of the action (ex: strategy_class, order_id)
        $ticket->context_data = json_encode($data);

        // IEPA!!
        // TODO: Recuperar External ID de forma agnóstica
        // // Optional: If the strategy has given us an external ID (ex: Redsys Order ID), we save it
        // if (isset($data['external_ref_id'])) {
        //     $ticket->external_ref_id = $data['external_ref_id'];
        // } elseif (isset($data['redsys_order_id'])) {
        //     $ticket->external_ref_id = $data['redsys_order_id'];
        // }

        $ticket->save();
        
        // Optional: Update the result with the ticket_id in case the terminal action wants to use it
        // $data['ticket_id'] = $ticket->id;
    }

    /**
     * Terminates the execution and returns a raw error message with HTTP 400 status code. 
     * This is used for critical errors where we cannot render a proper response.
     * @param string $msg The error message to return
     */
    private function terminateRawError(string $msg): void {
        http_response_code(400);
        die("System Error: " . htmlspecialchars($msg));
    }

    /**
     * Checks if a response with the same hash has been received for the same form. This is used for duplicate detection.
     * For human users, we check if we have received the same response from the same origin within a 5-minute window.
     * For bots (identified as spam), we check if we have received the same response from the same origin at any time.
     * @param string $formId The ID of the form
     * @param string $hash The hash of the response to check for duplicates
     * @return bool True if a duplicate submission is detected, false otherwise
     */
    private function checkDuplicateSubmission(string $formId, string $hash): bool {
        global $db;
        $safeFormId = $db->quote($formId);

        $query = "SELECT count(response.id) as count FROM stic_awf_responses response
                    INNER JOIN stic_awf_forms_stic_awf_responses_c form_response
                        ON form_response.stic_awf_forms_stic_awf_responsesresponses_idb = response.id
                  WHERE
                    form_response.stic_awf_forms_stic_awf_responsesforms_ida = '{$safeFormId}'
                    AND form_response.deleted = 0
                    AND response.response_hash = '{$hash}'
                    AND response.deleted = 0";

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": " . $query);
        $result = $db->query($query);
        $data = $db->fetchByAssoc($result);
        return $data['count'] > 0;
    }

    /**
     * Handles the case of a duplicate submission by rendering a generic response indicating that the response has already been submitted.
     * @param SugarBean $formBean The form bean for which the duplicate response was detected
     */
    private function handleDuplicateError(SugarBean $formBean): void {
        $configData = json_decode(html_entity_decode($formBean->configuration), true);
        $formConfig = $configData ? FormConfig::fromJsonArray($configData) : null;
        if ($formConfig) {
            stic_AWFUtils::renderGenericResponse($formConfig, 
                                             translate('LBL_DUPLICATE_RESPONSE_TITLE', 'stic_AWF_Responses'),
                                             translate('LBL_DUPLICATE_RESPONSE_MSG', 'stic_AWF_Responses'));
        } else {
            $this->terminateRawError("This response has already been submitted.");
        }
    }

    /**
     * Saves the links between the response and the modified/created beans during the execution of the action flows.
     * It consolidates the modified beans from all actions, determines the type of modification (created, updated, enriched, skipped), 
     * and saves a link record for each affected bean with the relevant details
     * @param SugarBean $responseBean The response bean to which the links will be associated
     * @param ExecutionContext $context The execution context containing the action results with the modified beans
     */
    private function saveLinks(SugarBean $responseBean, ExecutionContext $context): void {
        if (!$responseBean->load_relationship('stic_1c31forms_links')) {
            $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ": ResponseHandler: Could not load relationship responses-links for Response ID {$responseBean->id}");
            return;
        }

        global $app_list_strings;

        // Consolidate links: A single link for each affected bean
        $consolidatedBeans = []; 
        foreach ($context->actionResults as $result) {
            foreach ($result->modifiedBeans as $modBean) {
                $key = $modBean->moduleName . ':' . $modBean->beanId;

                if (!isset($consolidatedBeans[$key])) {
                    // First time we touch it
                    $consolidatedBeans[$key] = [
                        'module' => $modBean->moduleName,
                        'id' => $modBean->beanId,
                        'type' => $modBean->modificationType, // (CREATED, UPDATED, ENRICHED, SKIPPED, METADATA)
                        'data' => $modBean->submittedData
                    ];
                } else {
                    // It already exists, we merge
                    $currentEntry = &$consolidatedBeans[$key];

                    // Merge of action type performed. Priority: CREATED > UPDATED > ENRICHED > SKIPPED > METADATA
                    $oldType = $currentEntry['type'];
                    $newType = $modBean->modificationType;

                    if ($newType === BeanModificationType::CREATED) {
                        // CREATED wins over absolutely everything
                        $currentEntry['type'] = BeanModificationType::CREATED;
                    } 
                    elseif ($newType === BeanModificationType::UPDATED && $oldType !== BeanModificationType::CREATED) {
                        // UPDATED wins over all except CREATED
                        $currentEntry['type'] = BeanModificationType::UPDATED;
                    } 
                    elseif ($newType === BeanModificationType::ENRICHED && !in_array($oldType, [BeanModificationType::CREATED, BeanModificationType::UPDATED])) {
                        // ENRICHED wins over all except CREATED and UPDATED
                        $currentEntry['type'] = BeanModificationType::ENRICHED;
                    }
                    elseif ($oldType === BeanModificationType::METADATA && $newType !== BeanModificationType::METADATA) {
                        // If we only had METADATA, any physical action (even SKIPPED) overrides it
                        $currentEntry['type'] = $newType;
                    }
                    // Notice: If oldType is SKIPPED and newType is METADATA, no condition matches, so we preserve the SKIPPED state.

                    // Merge of touched data: We accumulate the fields (not SKIPPED)
                    if ($newType !== BeanModificationType::SKIPPED && !empty($modBean->submittedData)) {
                        foreach ($modBean->submittedData as $fieldName => $fieldMod) {
                            if (!isset($currentEntry['data'][$fieldName])) {
                                $currentEntry['data'][$fieldName] = $fieldMod;
                            } else {
                                // Merge logic: Decide which state prevails: APPLIED > IGNORED_ENRICH > UNCHANGED
                                /** @var FieldModification $existingMod */
                                $existingMod = $currentEntry['data'][$fieldName];
                                
                                // If the new status is APPLIED, it overwrites whatever was there.
                                if ($fieldMod->status === FieldModificationStatus::APPLIED) {
                                    $currentEntry['data'][$fieldName] = $fieldMod;
                                } 
                                // If the new state is IGNORED_ENRICH, it only wins if the old state was UNCHANGED
                                elseif ($fieldMod->status === FieldModificationStatus::IGNORED_ENRICH && $existingMod->status === FieldModificationStatus::UNCHANGED) {
                                    $currentEntry['data'][$fieldName] = $fieldMod;
                                }
                                // In any other case (eg: the old one was APPLIED and the new one is UNCHANGED), do nothing and keep the old one
                            }
                        }
                    }                    
                }
            }
        }

        // Save the links
        $sequence = 1;
        foreach ($consolidatedBeans as $item) {
            $linkBean = BeanFactory::newBean('stic_AWF_Links');

            $targetBean = BeanFactory::getBean($item['module'], $item['id']);
            $targetBeanName = $targetBean ? $targetBean->get_summary_text() : $item['id'];
            
            $actionValue = $item['type']->value;
            $recordActionName = $app_list_strings['stic_awf_links_record_action_list'][$actionValue] ?? $actionValue;
            $moduleSingular = $app_list_strings['moduleListSingular'][$item['module']] ?? $item['module'];

            $linkBean->is_automated_save = true;
            // Link name format: "FormName - Module: RecordName (Created)"
            $linkBean->name = $responseBean->name ." - ". $moduleSingular .": ". $targetBeanName ." (". $recordActionName .")";
            $linkBean->sequence_number = $sequence++;
            $linkBean->parent_id = $item['id']; 
            $linkBean->parent_type = $item['module']; 
            $linkBean->record_action = $actionValue;
            $linkBean->assigned_user_id = $responseBean->assigned_user_id;

            if (!empty($item['data'])) {
                $linkBean->submitted_data = json_encode($item['data'], JSON_UNESCAPED_UNICODE);
            }

            $linkBean->save();

            // Link the Link to the Response
            $responseBean->stic_1c31forms_links->add($linkBean->id);
        }
    }

    /**
     * Validates submission data against field definition in vardefs
     * @param FormConfig $config Form configuration
     * @param array $data Received data in the submission
     * @return ?array List of errors or null if no errors
     */
    private function validateSubmission(FormConfig $config, array $data): ?array {
        $errors = [
            'errors' => [], 
            'errorDescriptions' => [],
        ];

        // Preprocess data to fill in missing boolean/checkbox fields
        // (browsers don't send unchecked checkboxes)
        stic_AWFUtils::fillMissingBooleanFields($config, $data);

        foreach ($config->data_blocks as $block) {
            // Datablock is detached
            if (empty($block->module)) continue; 
            
            $targetBean = BeanFactory::newBean($block->module);
            $realVardefs = $targetBean ? $targetBean->field_defs : [];
            if (empty($realVardefs)) continue;

            foreach ($block->fields as $formField) {
                $inputKeyInForm = $formField->getKey();
                $inputKey = $formField->getPhpKey();
                $value = $data[$inputKey] ?? null;
                $label = rtrim($formField->label, ":");
                
                if ($formField->type == 'relate') {
                    continue; // Skip relate fields
                }

                // Validation of required field (Required) (in Form)
                if ($formField->required_in_form) {
                    if ($value === null || $value === '' || (is_array($value) && empty($value))) {
                        $errors['errorDescriptions'][$inputKeyInForm] = translate('LBL_FIELD', 'stic_AWF_Responses') ." '{$label}': ". 
                                                                        translate('LBL_ERROR_REQUIRED_FIELD', 'stic_AWF_Responses');
                        $errors['errors'][$inputKeyInForm] = translate('LBL_ERROR_REQUIRED_FIELD', 'stic_AWF_Responses');
                        continue;
                    }
                }

                // If is empty and not required: Do not validate type
                if ($value === null || $value === '' || (is_array($value) && empty($value))) {
                    continue;
                }

                // Validate Selected values in form
                if (!empty($formField->value_options) && $formField->value_type === DataBlockFieldValueType::SELECTABLE) {
                    $validValues = array_map(fn($opt) => $opt->value, $formField->value_options);
                    $submittedValues = is_array($value) ? $value : [$value];
                    foreach ($submittedValues as $subVal) {
                        if ($subVal === '' || $subVal === null) {
                            continue;
                        }
                        if (!in_array($subVal, $validValues)) {
                            $errors['errorDescriptions'][$inputKeyInForm] = translate('LBL_FIELD', 'stic_AWF_Responses') ." '{$label}': ". 
                                                                            translate('LBL_ERROR_VALUE_FIELD', 'stic_AWF_Responses') . ' ({$subVal})';
                            $errors['errors'][$inputKeyInForm] = translate('LBL_ERROR_VALUE_FIELD', 'stic_AWF_Responses') . ' ({$subVal})';
                            break; 
                        }
                    }
                }                

                // Validation of data type (from VarDefs)
                $defField = isset($realVardefs[$formField->name]) ? $realVardefs[$formField->name] : null;
                $typeToCheck = $defField ? $defField['type'] : $formField->type_in_form;
                // Special case for emails
                if ($typeToCheck === 'varchar' && str_starts_with($defField['name'], 'email')) {
                    $typeToCheck = 'email';
                }
                
                switch ($typeToCheck) {
                    case 'int':
                    case 'integer':
                        if (!filter_var($value, FILTER_VALIDATE_INT) && $value !== '0' && $value !== 0) {
                            $errors['errorDescriptions'][$inputKeyInForm] = translate('LBL_FIELD', 'stic_AWF_Responses') ." '{$label}': ". 
                                                                            translate('LBL_ERROR_INTEGER_FIELD', 'stic_AWF_Responses');
                            $errors['errors'][$inputKeyInForm] = translate('LBL_ERROR_INTEGER_FIELD', 'stic_AWF_Responses');
                        }
                        break;

                    case 'decimal':
                    case 'float':
                    case 'currency':
                    case 'number': // type_in_form
                        if (!is_numeric($value)) {
                            $errors['errorDescriptions'][$inputKeyInForm] = translate('LBL_FIELD', 'stic_AWF_Responses') ." '{$label}': ". 
                                                                            translate('LBL_ERROR_NUMERIC_FIELD', 'stic_AWF_Responses');
                            $errors['errors'][$inputKeyInForm] = translate('LBL_ERROR_NUMERIC_FIELD', 'stic_AWF_Responses');
                        }
                        break;

                    case 'date':
                    case 'datetime':
                    case 'datetimecombo':
                        if (strtotime($value) === false) {
                            $errors['errorDescriptions'][$inputKeyInForm] = translate('LBL_FIELD', 'stic_AWF_Responses') ." '{$label}': ". 
                                                                            translate('LBL_ERROR_DATE_FIELD', 'stic_AWF_Responses');
                            $errors['errors'][$inputKeyInForm] = translate('LBL_ERROR_DATE_FIELD', 'stic_AWF_Responses');
                        }
                        break;

                    case 'bool':
                    case 'boolean':
                    case 'select_checkbox':
                    case 'select_switch':
                        if (!filter_var($value, FILTER_VALIDATE_BOOLEAN, FILTER_NULL_ON_FAILURE)) {
                            $errors['errorDescriptions'][$inputKeyInForm] = translate('LBL_FIELD', 'stic_AWF_Responses') ." '{$label}': ". 
                                                                            translate('LBL_ERROR_BOOL_FIELD', 'stic_AWF_Responses');
                            $errors['errors'][$inputKeyInForm] = translate('LBL_ERROR_BOOL_FIELD', 'stic_AWF_Responses');
                        }                        
                        break;

                    case 'enum':
                    case 'radioenum':
                    case 'select': // type_in_form
                    case 'select_radio': // type_in_form
                        // Validate that the option actually exists in the CRM
                        if ($defField && isset($defField['options'])) {
                            global $app_list_strings;
                            $domain = $app_list_strings[$defField['options']] ?? [];
                            if (!empty($domain) && !array_key_exists($value, $domain)) {
                                $errors['errorDescriptions'][$inputKeyInForm] = translate('LBL_FIELD', 'stic_AWF_Responses') ." '{$label}': ". 
                                                                                translate('LBL_ERROR_ENUM_FIELD', 'stic_AWF_Responses');
                                $errors['errors'][$inputKeyInForm] = translate('LBL_ERROR_ENUM_FIELD', 'stic_AWF_Responses');
                            }
                        }
                        break;

                    case 'email':
                    case 'text_email':
                        if (!filter_var($value, FILTER_VALIDATE_EMAIL)) {
                            $errors['errorDescriptions'][$inputKeyInForm] = translate('LBL_FIELD', 'stic_AWF_Responses') ." '{$label}': ". 
                                                                            translate('LBL_ERROR_EMAIL_FIELD', 'stic_AWF_Responses');
                            $errors['errors'][$inputKeyInForm] = translate('LBL_ERROR_EMAIL_FIELD', 'stic_AWF_Responses');
                        }
                        break;
                }

                // BACKEND VALIDATION FOR CUSTOM VALIDATORS
                if (!empty($formField->validations)) {
                    foreach ($formField->validations as $valConfig) {
                        // Check conditions (if any)
                        if (!stic_AWFUtils::evaluateConditions($valConfig->conditions, $data)) {
                            continue; // Conditions are not met, skip this validation
                        }
                       
                        $validatorDef = ActionDiscoveryService::discoverActions([ActionType::VALIDATOR]);
                        foreach ($validatorDef as $def) {
                            if ($def->getName() === $valConfig->validator && $def instanceof ValidatorActionDefinition) {
                                if (!$def->validateBackend($value, $valConfig->params)) {
                                    $errorMsg = !empty($valConfig->message) ? $valConfig->message : $def->getDefaultErrorMessage();
                                    $errors['errorDescriptions'][$inputKeyInForm] = translate('LBL_FIELD', 'stic_AWF_Responses') ." '{$label}': " . $errorMsg;
                                    $errors['errors'][$inputKeyInForm] = $errorMsg;
                                    break 2; // If a validation fails: stop processing that field.
                                }
                            }
                        }
                    }
                }                
            }
        }

        return $errors;
    }

    /**
     * Generates response details for storage and subsequent analysis.
     * @param SugarBean $responseBean Response bean
     * @param SugarBean $formBean Form bean
     * @param FormConfig $formConfig Form configuration
     * @param array $submittedData Data sent in the submission
     */
    private function generateResponseDetails(SugarBean $responseBean, SugarBean $formBean, FormConfig $formConfig, array $submittedData): void {
        global $app_strings;

        // Global counter
        $orderCounter = 1;

        // if (!$responseBean->load_relationship('details_link')) {
        //     $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ": ResponseHandler: Could not load relationship 'details_link' in Responses bean.");
        // }

        foreach ($formConfig->data_blocks as $block) {
            foreach ($block->fields as $field) {
                $currentOrder = $orderCounter;
                $orderCounter += 1;

                // Skip fixed fields
                if ($field->type_field === DataBlockFieldType::FIXED) continue;

                // Input key
                $inputKey = $field->getPhpKey();
                
                $rawValue = $submittedData[$inputKey] ?? null;
                
                // Calculate readable text and value to store
                $readableText = $rawValue;
                $storedValue = $rawValue;
                
                // List type fields (select, multiselect, radio)
                if (!empty($field->value_options)) {
                    if (is_array($rawValue)) {
                        // Multi-selection
                        $labels = [];
                        foreach ($rawValue as $valItem) {
                            $opt = $this->findOption($field->value_options, $valItem);
                            $labels[] = $opt ? $opt->text : $valItem;
                        }
                        $storedValue = json_encode($rawValue, JSON_UNESCAPED_UNICODE); // Store JSON ["A","B"]
                        $readableText = implode(', ', $labels); // Text: "Option A, Option B"
                    } else {
                        // Single selection
                        $opt = $this->findOption($field->value_options, $rawValue);
                        if ($opt) {
                            $readableText = $opt->text;
                        }
                    }
                } 
                // Boolean fields (checkbox)
                elseif ($field->type === 'bool' || $field->type === 'checkbox') {
                    $isTrue = ($rawValue === '1' || $rawValue === 'on' || $rawValue === 'true' || $rawValue === true);
                    $readableText = $isTrue ? $app_strings['LBL_YES'] : $app_strings['LBL_NO'];
                    $storedValue = $isTrue ? '1' : '0';
                }
                // Generic arrays that are not lists
                elseif (is_array($rawValue)) {
                    $storedValue = json_encode($rawValue, JSON_UNESCAPED_UNICODE);
                    $readableText = 'Array'; 
                }

                // Create analytical response bean
                $detailBean = BeanFactory::newBean('stic_AWF_Response_Details');
                $detailBean->stic_awf_responses_id_c = $responseBean->id;
                $detailBean->stic_awf_forms_id_c = $formBean->id ?? ''; 
                $detailBean->assigned_user_id = $responseBean->assigned_user_id;
                
                $detailBean->question_key = $block->name . '.' . $field->name;
                $detailBean->question_label = $field->label ?? $field->text_original ?? $field->name;
                $detailBean->question_label = rtrim($detailBean->question_label, ' :');
                if (!empty($field->description)) {
                    $detailBean->question_help_text = stic_AWFUtils::parseAnchorMarkdown($field->description);
                }
                $detailBean->question_section = $block->text;
                
                $detailBean->question_sort_order = $currentOrder;

                $detailBean->answer_value = (string)$storedValue;
                $detailBean->answer_text = (string)$readableText;
                $detailBean->answer_type = $field->type_in_form;
                
                // Save the value as integer to facilitate analysis
                // Special handling for rating fields to normalize them to a 0-100 scale
                if ($field->type_in_form === 'rating' && is_numeric($rawValue)) {
                    $rawNum = (float)$rawValue;
                    
                    if ($field->subtype_in_form === 'rating_nps') {
                        // NPS (0-10) -> Scale 0-100
                        $normalized = $rawNum * 10;
                    } else {
                        // Stars, emojis, lights, thumbs (1-5) -> Scale 0-100: 1=20, 2=40, 3=60, 4=80, 5=100
                        $normalized = $rawNum * 20;
                    }
                    // Make sure to store a clean integer limited between 0 and 100
                    $detailBean->answer_integer = (int)max(0, min(100, round($normalized)));
                } elseif (!is_array($rawValue) && is_numeric($rawValue)) {
                    // Save the numeric value if applicable
                    $detailBean->answer_integer = (int)round((float)$rawValue);
                } else {
                    $detailBean->answer_integer = 0;
                }

                $detailBean->save();
            }
        }
    }

    /** 
     * Helper function to find an option object by its value in a list of options
     * @param array $options List of option objects with 'value' and 'text' properties
     * @param string $value The value to find
     * @return ?object The option object if found, null otherwise
    */
    private function findOption(array $options, $value) {
        foreach ($options as $opt) {
            if ($opt->value == $value) return $opt;
        }
        return null;
    }

    /**
     * Function that recursively cleans the input to avoid XSS
     * Removes dangerous HTML tags and extra spaces
     *  If the input is an array, it cleans each element and the keys. 
     *  If it's a boolean, null or numeric, it lets it pass. 
     *  Otherwise, it applies strip_tags and trim.
     */
    private function sanitizeInput($input) {
        if (is_array($input)) {
            // If it is an array, we clean each element, also the key
            $clean = [];
            foreach ($input as $key => $value) {
                $clean[strip_tags($key)] = $this->sanitizeInput($value);
            }
            return $clean;
        }
        
        // If it is a bool, null or pure numeric, let it pass
        if (is_bool($input) || is_null($input) || is_numeric($input)) {
            return $input;
        }

        // Clean the input
        return strip_tags(trim((string)$input));
    }


    /**
     * Determines if the User Agent string belongs to a bot or script. This is used for spam detection and to apply different duplicate detection rules.
     * The function checks if the User Agent is empty (which is suspicious) 
     *  or if it contains known signatures of programming tools and libraries commonly used for making HTTP requests (like curl, wget, python, java, etc.).
     * If any of these conditions are met, it returns true, indicating that the User Agent is likely a bot. 
     * Otherwise, it returns false, indicating that it is likely a human user.
     * @param string $userAgent The User Agent string from the request headers
     * @return bool True if the User Agent is identified as a bot, false otherwise
     */
    private function isBotUserAgent(string $userAgent): bool 
    {
        // If it's empty, it's suspicious (all browsers send something)
        if (empty($userAgent)) {
            return true; 
        }

        // Blacklist of programming tools that are NOT browsers
        // If the User Agent contains any of these words, it is a script.
        $botSignatures = [
            'curl',          // Linux command tool
            'wget',          // Download tool
            'python',        // Requests/Ulllib library
            'java/',         // Java HTTP Client
            'libwww',        // Perl library
            'httpclient',    // Generic Apache/Java
            'php/',          // PHP scripts (file_get_contents)
            'postman',       // API testing tool
            'insomnia',      // API testing tool
            'node-fetch',    // NodeJS
            'axios',         // JS library (server side)
            'go-http-client' // Golang
        ];

        foreach ($botSignatures as $bot) {
            if (strpos($userAgent, $bot) !== false) {
                return true;
            }
        }

        return false;
    }
}

// Handler execution
$handler = new ResponseHandler();
$handler->run();