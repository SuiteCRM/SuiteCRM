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
require_once "modules/stic_AWF_Deferred_Tickets/stic_AWF_Deferred_Tickets.php";
require_once "modules/stic_AWF_Incoming_Events/stic_AWF_Incoming_Events.php";
require_once "include/SugarQueue/SugarJobQueue.php";

/**
 * EntryPoint: stic_AWF_webhookHandler
 * Receives and processes webhook responses from payment gateways.
 *
 * This entry point is fully gateway-agnostic. All gateway-specific logic
 * (signature verification, response parsing, event handling) lives in the
 * individual payment strategy classes. The WebhookHandler only:
 *   1. Creates an IncomingEvent log record
 *   2. Extracts the external transaction ID (delegated to the strategy)
 *   3. Atomically finds and locks the Deferred Ticket
 *   4. Rebuilds the ExecutionContext from the ticket
 *   5. Calls PaymentRouterAction::processWebhook() → strategy->resolve()
 *   6. Updates the ticket status and resumes the form flow
 *
 * When no ticket is found (e.g. Stripe recurring events), the strategy's
 * resolve() method handles it directly.
 */
class WebhookHandler
{
    public function run(): void
    {
        global $current_user;
        $current_user = BeanFactory::newBean('Users');
        $current_user->getSystemUser();

        $source  = $_REQUEST['source'] ?? '';
        $requestData = $_POST;
        $rawPayload = file_get_contents('php://input');
        $headers = function_exists('getallheaders') ? getallheaders() : [];

        // Get Token
        $token = $_REQUEST['token'] ?? '';

        // Create IncomingEvent log record
        $incomingEvent = BeanFactory::newBean('stic_AWF_Incoming_Events');
        $incomingEvent->name = 'AWF Webhook: ' . $source . ' - ' . date('Y-m-d H:i:s');
        $incomingEvent->token = $token;
        $incomingEvent->source = $source;
        $incomingEvent->raw_payload = $rawPayload ?: json_encode($requestData);
        $incomingEvent->status = 'new';
        $incomingEvent->date_received = date('Y-m-d H:i:s');
        $incomingEvent->save();

        $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ": AWF WebhookHandler: Received webhook from source='{$source}'. IncomingEvent ID={$incomingEvent->id}");

        // Extract Identifier
        $searchField = 'token_hash';
        $identifier = $token;
        if (empty($identifier) && !empty($source)) {
            // No token in URL, but we have a source. Let's ask the actions.
            $searchField = 'external_transaction_id';
            
            $deferredActions = ActionDiscoveryService::discoverActions([ActionType::DEFERRED]);
            foreach ($deferredActions as $action) {
                if ($action instanceof IWebhookDecodable && $action->handlesSource($source)) {
                    $identifier = $action->extractTokenFromEvent($source, $requestData, $rawPayload, $headers);
                    $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ": AWF WebhookHandler: Action '{$action->getName()}' handled source '{$source}' and extracted identifier.");
                    break;
                }
            }
        }

        if (empty($identifier)) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": AWF WebhookHandler: Could not extract identifier for source='{$source}'. No matching Decodable action found or extraction failed.");
            $incomingEvent->status = 'ignored';
            $incomingEvent->last_error_message = "Could not extract identifier";
            $incomingEvent->date_processed = date('Y-m-d H:i:s');
            $incomingEvent->save();
            http_response_code(400);
            die("Cannot determine transaction ID");
        }

        if ($searchField === 'external_transaction_id') {
            $incomingEvent->external_transaction_id = $identifier;
        }
        $incomingEvent->save();

        // Atomically find and lock the Deferred Ticket (for tickets in 'pending' state)
        $ticket = $this->findTicket($identifier, $searchField);
        $context = null;

        if (!$ticket) {
            // If ticket is not in pending state, analyze the cause
            /** @var stic_AWF_Deferred_Tickets $existingTicket */
            $existingTicket = BeanFactory::getBean('stic_AWF_Deferred_Tickets');
            $existingTicket->retrieve_by_string_fields([$searchField => $identifier, 'deleted' => 0]);

            if (!empty($existingTicket->id)) {
                $ticketStatus = $existingTicket->status ?? 'pending';

                // If the ticket has been 'processing' for more than 2 minutes: unlock it.
                if ($ticketStatus === 'processing') {
                    $modifiedTime = strtotime($existingTicket->date_modified);
                    if ($modifiedTime < (time() - 120)) { 
                        $GLOBALS['log']->warn('Line ' . __LINE__ . ': ' . __METHOD__ . ": AWF WebhookHandler: Interactive unstick triggered for ticket {$existingTicket->id} stuck in 'processing' since " . $existingTicket->date_modified);
                        $existingTicket->status = 'pending';
                        $existingTicket->save();
                        
                        // Find and lock the Deferred Ticket again
                        $ticket = $this->findTicket($identifier, $searchField); 
                    }
                }
                
                if (!$ticket) {
                    // Ticket is already resolved or is running right now
                    if (!empty($_REQUEST['redirect'])) {
                        // Comes from an human browser (UI)
                        if (in_array($ticketStatus, ['resolved', 'processed', 'failed'])) { 
                            // The user re-clicks an email link that was already processed in the past. 
                            $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ": AWF WebhookHandler: Browser re-clicked a completed link. Rendering final UI inline without redirect.");
                            stic_AWFUtils::rebuildContextAndResumeDeferredFlow($existingTicket);
                        } else {
                            // Ticket is running right now
                            $GLOBALS['log']->info("AWF WebhookHandler: Browser hit an actively processing lock. Rendering unified waiting screen.");
                            
                            $formConfig = null;
                            try {
                                $context = stic_AWFUtils::rebuildContextFromTicket($existingTicket);
                                $formConfig = $context->formConfig;
                            } catch (Exception $e) {}

                            $title = translate('LBL_PROCESSING_TITLE', 'stic_AWF_Deferred_Tickets');
                            $msg = translate('LBL_PROCESSING_MSG', 'stic_AWF_Deferred_Tickets');
                            stic_AWFUtils::renderGenericResponse($formConfig, $title, $msg);
                            return;
                        }
                    } else {
                        // Is a S2S webhook call. Respond with 200
                        $incomingEvent->status = 'ignored'; 
                        $incomingEvent->last_error_message = "Ticket is already processing or completed (Status: {$ticketStatus})";
                        $incomingEvent->date_processed = date('Y-m-d H:i:s');
                        $incomingEvent->save();
                        
                        http_response_code(200);
                        echo "Acknowledged: Ticket current status is {$ticketStatus}";
                        return;
                    }
                }
            }
        }

        // Build context and resolve
        if ($ticket) {
            $result = $this->processWithTicket($ticket, $requestData, $rawPayload, $incomingEvent, $context);
            $this->handleResult($result, $ticket, $incomingEvent, $context);
         } else {
            // Event without direct ticket: process it
            $result = $this->processWithoutTicket($source, $requestData, $rawPayload, $incomingEvent);
            $this->handleResult($result, null, $incomingEvent, null);
        }

    }

    /**
     * Process a webhook when a matching Deferred Ticket exists.
     * Rebuilds the execution context from the ticket, then delegates
     * to PaymentRouterAction::processWebhook() which calls strategy->resolve().
     */
    private function processWithTicket(stic_AWF_Deferred_Tickets $ticket, array $rawData, string $rawBody, $incomingEvent, ?ExecutionContext &$outContext): ActionResult
    {
        try {
            $context = stic_AWFUtils::rebuildContextFromTicket($ticket);
        } catch (Exception $e) {
            $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ": AWF WebhookHandler: Failed to rebuild context for Ticket ID={$ticket->id}: " . $e->getMessage());
            $ticket->status = 'failed';
            $ticket->save();
            $incomingEvent->status = 'error';
            $incomingEvent->last_error_message = "Context rebuild failed: " . $e->getMessage();
            $incomingEvent->date_processed = date('Y-m-d H:i:s');
            $incomingEvent->save();
            http_response_code(500);
            die("Internal error");
        }

        // Inject rawBody into deferred context for strategies that need it
        $context->deferredContext->setCustom('_rawBody', $rawBody);

        $outContext = $context;

        // Discover actions to require class files
        ActionDiscoveryService::discoverActions([ActionType::DEFERRED]);

        $actionClass = $context->deferredContext->actionClass;
        if (empty($actionClass) || !class_exists($actionClass)) {
            $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ": AWF WebhookHandler: Handler class {$actionClass} not found for webhook processing.");
            $res = new ActionResult(ResultStatus::ERROR, null, "Handler class '{$actionClass}' not found for webhook processing.");
            $context->addActionResult($res);
        } else {
            $actionDefinition = new $actionClass();
            $res = $actionDefinition->processWebhook($context, $rawData);
        }

        stic_AWFUtils::updateResponseExecutionLog($context);
        return $res;
    }

    /**
     * Process a webhook when no matching Deferred Ticket exists.
     * Creates a strategy from the source identifier and calls resolve() directly.
     * This handles events like Stripe subscription/invoice events that occur
     * after the initial checkout and have no associated ticket.
     */
    private function processWithoutTicket(string $source, array $rawData, string $rawBody, $incomingEvent): ActionResult
    {
        $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ": AWF WebhookHandler: No ticket found for source='{$source}'. Delegating to strategy directly.");

        $deferredActions = ActionDiscoveryService::discoverActions([ActionType::DEFERRED]);
        foreach ($deferredActions as $action) {
            if ($action instanceof IWebhookDecodable && $action->handlesSource($source)) {
                // Initialize the isolated emergency typed context
                $context = new ExecutionContext('', '', [], new FormConfig(), null, '');
                $context->deferredContext = new DeferredContextData('', '');
                $context->deferredContext->setCustom('_rawBody', $rawBody);

                // Give up control of the resolution to the deferred action itself
                $result = $action->processOrphanWebhook($context, $source, $rawData);
                
                $incomingEvent->status = $result->isOk() ? 'processed' : 'error';
                $incomingEvent->last_error_message = $result->message ?? '';
                $incomingEvent->date_processed = date('Y-m-d H:i:s');
                $incomingEvent->save();

                return $result;
            }
        }

        // Fallback if no deferred action can attend the source
        $GLOBALS['log']->warn('Line ' . __LINE__ . ': ' . __METHOD__ . ": AWF WebhookHandler: Unknown source '{$source}' and no matching deferred action handles it.");
        $incomingEvent->status = 'ignored';
        $incomingEvent->last_error_message = "No matching deferred action found for orphan webhook source";
        $incomingEvent->date_processed = date('Y-m-d H:i:s');
        $incomingEvent->save();

        http_response_code(200);
        die("Ignored: Source not handled");
    }

    /**
     * Handles the ActionResult from strategy->resolve(), updating ticket,
     * resuming flows, and sending the HTTP response.
     */
    private function handleResult(ActionResult $result, ?stic_AWF_Deferred_Tickets $ticket, stic_AWF_Incoming_Events $incomingEvent, ?ExecutionContext $context): void
    {
        // Update status depending on result
        if ($ticket) {
            if ($result->isOk()) {
                $ticket->status = 'resolved';
                $ticket->save();
            } elseif ($result->isError()) {
                $maxRetries = 3;
                $retryCount = intval($ticket->retry_count ?? 0) + 1;
                $ticket->retry_count = $retryCount;
                $ticket->last_error_message = $result->message ?? 'Unknown error';

                if ($retryCount < $maxRetries) {
                    $ticket->status = 'pending';
                    $GLOBALS['log']->warn('Line ' . __LINE__ . ': ' . __METHOD__ . ": Ticket [{$ticket->id}] failed (attempt {$retryCount}/{$maxRetries}). Reset to pending for retry.");
                } else {
                    $ticket->status = 'failed';
                    $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ": Ticket [{$ticket->id}] permanently failed after {$maxRetries} attempts. Error: " . $ticket->last_error_message);
                }
                $ticket->save();

                // If final status is failed: Change to error flow
                if ($ticket->status === 'failed') {
                    $this->enqueueDeferredFlow($ticket->id, false);
                }
            }
            // If is WAIT, do not change status
        }

        // Register incoming event
        if ($incomingEvent->status !== 'processed') {
            // Set event as processed but save error if any
            $incomingEvent->status = 'processed';
            $incomingEvent->last_error_message = $result->isError() ? ($result->message ?? 'Unknown error') : '';
            $incomingEvent->date_processed = date('Y-m-d H:i:s');
            $incomingEvent->save();
        }

        // Redirect UI if url has param 'redirect'
        if (!empty($_REQUEST['redirect']) && $ticket) {
            $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ": AWF WebhookHandler: Browser redirection flag detected. Resuming final UI inline.");
            
            stic_AWFUtils::rebuildContextAndResumeDeferredFlow($ticket);
            return;
        }

        // Return 200 even for rejected responses: the webhook itself was processed correctly.
        http_response_code(200);
        if ($result->isOk()) {
            echo "OK";
        } elseif ($result->isError()) {
            echo "Error: " . ($result->message ?? 'Unknown error');
        } else {
            echo "Pending / Waiting";
        }
    }

    /**
     * Enqueues a deferred flow for async execution via SuiteCRM job queue.
     * This ensures the webhook returns HTTP 200 immediately without waiting
     * for the flow (emails, PDFs, etc.) to complete.
     *
     * @param string $ticketId The deferred ticket ID
     * @param bool $isSuccess Whether to run the success or error flow
     */
    private function enqueueDeferredFlow(string $ticketId, bool $isSuccess): void
    {
        try {
            $job = BeanFactory::newBean('SchedulersJobs');
            $job->name = 'AWF Deferred Flow - Ticket ' . $ticketId;
            $job->target = 'sticAWFResumeDeferredFlow';
            $job->data = json_encode([
                'ticket_id' => $ticketId,
                'is_success' => $isSuccess,
            ]);
            $job->assigned_user_id = $GLOBALS['current_user']->id ?? '1';

            $queue = new SugarJobQueue();
            $jobId = $queue->submitJob($job);

            $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ": Enqueued deferred flow Job ID={$jobId} for ticket {$ticketId} (success=" . ($isSuccess ? 'true' : 'false') . ")");
        } catch (Exception $e) {
            $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ": Failed to enqueue deferred flow for ticket {$ticketId}: " . $e->getMessage());
        }
    }

    /**
     * Atomically finds and locks the Deferred Ticket using an UPDATE...WHERE status='pending'.
     * This prevents race conditions when the same webhook arrives multiple times.
     */
    private function findTicket(string $identifier, string $searchField): ?stic_AWF_Deferred_Tickets
    {
        global $db;
        $safeId = $db->quote($identifier);
        // Note: The searchField is validated to be either 'token_hash' or 'external_transaction_id'.
        if (!in_array($searchField, ['token_hash', 'external_transaction_id'])) {
            return null;
        }

        $sql = "UPDATE stic_awf_deferred_tickets 
                SET status = 'processing', date_modified = '" . date('Y-m-d H:i:s') . "' 
                WHERE {$searchField} = '{$safeId}' 
                AND status = 'pending' 
                AND deleted = 0";
        $result = $db->query($sql);

        if ($db->getAffectedRowCount($result) === 0) {
            return null;
        }

        $ticket = BeanFactory::newBean('stic_AWF_Deferred_Tickets');
        $ticket->retrieve_by_string_fields([$searchField => $identifier, 'deleted' => '0']);

        return (!empty($ticket->id)) ? $ticket : null;
    }

}

$handler = new WebhookHandler();
$handler->run();
