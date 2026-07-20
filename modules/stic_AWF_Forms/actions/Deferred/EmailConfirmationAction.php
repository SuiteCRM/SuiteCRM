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
// Prevents directly accessing this file from a web browser
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

/**
 * EmailConfirmationAction
 * Deferred action that halts the form processing to wait for an email confirmation.
 * Generates a unique URL, emails it using a custom macro {::confirmation_url::}, 
 * and waits for the user to visit it via ReturnHandler.
 */
class EmailConfirmationAction extends DeferredBeanActionDefinition
{
    public function __construct() {
        $this->isActive = true;
        $this->isUserSelectable = true;
        $this->category = 'security';
        $this->baseLabel = 'LBL_EMAIL_CONFIRMATION_ACTION';
        $this->defaultExpirationDays = '7';
    }

    /**
     * Declares who will resume this deferred process and how.
     */
    public function getResumptionContext(): DeferredResumptionContext
    {
        return DeferredResumptionContext::ORIGINAL_USER;
    }

    /**
     * Modules supported by the action
     */
    protected function getSupportedModules(): array {
        return ['Contacts', 'Users', 'Prospects', 'Leads', 'Accounts'];
    }

    /**
     * Name of the parameter that contains the data block.
     * @return string
     */
    protected function getDataBlockParameterText(): string {
        return $this->translate('RECIPIENT_BLOCK_TEXT');
    }

    /**
     * The description (help text) of the data block parameter.
     * @return string
     */
    protected function getDataBlockParameterDescription(): string {
        return $this->translate('RECIPIENT_BLOCK_DESC');
    }

    /**
     * Definition of the ADDITIONAL parameters needed for the deferred action
     */
    protected function getDeferredCustomParameters(): array
    {
        // The email template to use (required)
        $paramTemplate = new ActionParameterDefinition();
        $paramTemplate->name = 'email_template';
        $paramTemplate->text = $this->translate('TEMPLATE_TEXT');
        $paramTemplate->description = $this->translate('TEMPLATE_DESC');
        $paramTemplate->type = ActionParameterType::CRM_RECORD;
        $paramTemplate->supportedModules = ['EmailTemplates'];
        $paramTemplate->required = true;

        return [$paramTemplate];
    }


    /**
     * Executes the action, receives the loaded bean and the main data block with the form data
     *
     * @param ExecutionContext $context The global context.
     * @param FormAction $actionConfig The configuration of the action.
     * @param SugarBean $bean The bean loaded from the DB (saved data).
     * @param DataBlockResolved $block The data block (form data).
     * @return ActionResult
     */
    public function executeWithBean(ExecutionContext $context, FormAction $actionConfig, SugarBean $bean, DataBlockResolved $block): ActionResult {
        // Get additional parameters (ParameterResolver ensures they are not null because they are required)

        /** @var BeanReference $templateRef */
        $templateRef = $actionConfig->getResolvedParameter('email_template');
        if (empty($templateRef)) {
            return new ActionResult(ResultStatus::ERROR, $actionConfig, "Email template parameter is missing.");
        }

        // Get email from Bean. We assume the standard field 'email1'
        $emailAddress = $bean->email1 ?? null;
        // Validate that the email is correct
        if (empty($emailAddress) || !filter_var($emailAddress, FILTER_VALIDATE_EMAIL)) {
            return new ActionResult(ResultStatus::ERROR, $actionConfig, "DataBlock '{$block->dataBlock->name}' does not have a valid 'email1' field ('{$emailAddress}').");
        }

        // Create a deferred ticket
        $ticket = $this->createDeferredTicket(
            $context,
            $actionConfig,
            $bean,
            [
                // Custom data
                'email' => $emailAddress
            ], 
            'Email Confirmation: ' . $emailAddress, 
            $this->defaultExpirationDays
        );
        
        // Save the Ticket to DB
        $ticket->save();

        // Generate the confirmation URL
        $confirmationUrl = $this->getAsyncCallbackUrl('email_confirmation_action', $ticket, ['redirect' => 1]);

        // Insert the confirmation URL into the email template using a custom macro
        $customVars = [ '{::confirmation_url::}' => $confirmationUrl ];

        // Send the email and update Email Opt-in status
        try {
            stic_AWFUtils::sendTemplateEmail($emailAddress, $templateRef->beanId, $context, $bean, $customVars);
            $this->updateEmailOptInStatus($emailAddress, 'sent', $ticket->token_hash);
        } catch (\Exception $e) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": Error trying to send Email to '{$emailAddress}': " . $e->getMessage());
            try {
                $this->updateEmailOptInStatus($emailAddress, 'failed');
            } catch (\Exception $e2) {
                $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": Error trying to set Opt-in as failed: " . $e2->getMessage());
            }
            return new ActionResult(ResultStatus::ERROR, $actionConfig, $e->getMessage());
        }

        // Return a WAIT result to halt the flow until the user confirms via email
        return new ActionResult(ResultStatus::OK, $actionConfig, "Confirmation email sent to {$emailAddress}. Continuing the flow execution");
    }

    /**
     * Processes an incoming request (webhook) from an external service.
     * 
     * This method is only relevant for actions that expect a server callback.
     * @param ExecutionContext $context The global context.
     * @param array $requestData The data of the incoming request.
     * @return ActionResult Result of the execution of the action.
     */
    public function processWebhook(ExecutionContext $context, array $requestData): ActionResult {
        // Email confirmation received: extract the email address from the context data and update the opt-in status
        $emailAddress = $context->deferredContext?->getCustom('email');
        if (empty($emailAddress)) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": AWF EmailConfirmationAction: Email address missing in Webhook request");
            return new ActionResult(ResultStatus::ERROR, null, "No valid email address found in the context data.");
        }

        try {
            $this->updateEmailOptInStatus($emailAddress, 'confirmed');
        } catch (\Exception $e) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": Error trying to set Opt-in as confirmed: " . $e->getMessage());
            return new ActionResult(ResultStatus::ERROR, null, "Error confirming email: " . $e->getMessage());
        }
    
        return new ActionResult(ResultStatus::OK, null, "Email confirmed.");
    }

    /**
     * Updates the opt-in status of the email address in the email_addresses table.
     * @param string $email The email address to update.
     * @param string $action The action performed ('sent', 'confirmed', 'failed').
     * @param string|null $token The confirmation token (only for 'sent' action).
     */
    private function updateEmailOptInStatus(string $email, string $action, ?string $token = null)
    {
        $now = TimeDate::getInstance()->nowDb();

        /** @var EmailAddress $emailBean */
        $emailBean = BeanFactory::newBean('EmailAddresses');
        $emailBean->retrieve_by_string_fields([ 'email_address' => $email, 'deleted' => 0 ]);

        if (empty($emailBean->id)) {
            throw new \Exception("Email record not found for '{$email}' to update opt-in status.");
        }

        if ($action === 'sent') {
            $emailBean->confirm_opt_in = 'not-opt-in';
            $emailBean->confirm_opt_in_sent_date = $now;
            $emailBean->confirm_opt_in_token = $token ?? '';
            
        } elseif ($action === 'confirmed') {
            $emailBean->confirm_opt_in = 'confirmed-opt-in';
            $emailBean->opt_out = 0;
            $emailBean->confirm_opt_in_date = $now;
            
        } elseif ($action === 'failed') {
            $emailBean->confirm_opt_in_fail_date = $now;
        }

        $emailBean->save();
    }

    /**
     * Indicates whether the action knows how to handle the specified Source.
     * @param string $source The source url parameter
     * @return bool indicating if the action can handle the specified source
     */
    public function handlesSource(string $source): bool 
    { 
        return $source === 'email_confirmation_action'; 
    }

    /**
     * Asks the action to extract the Token from the raw payload.
     * Returns the hash of the Deferred_Ticket.
     * @param string $source The source url parameter
     * @param array $requestData the request data received (POST or GET)
     * @param string $rawPayload the body raw payload received
     * @param array $headers the headers received
     * @return string|null the hash of the Deferred_Ticket
     */
    public function extractTokenFromEvent(string $source, array $requestData, string $rawPayload, array $headers): ?string 
    { 
        return $_REQUEST['token'] ?? null; 
    }
}