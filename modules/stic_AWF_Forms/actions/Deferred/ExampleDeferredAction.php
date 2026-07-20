<?php
/**
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2026 SinergiaTIC Association
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 */

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

include_once "modules/stic_AWF_Forms/actions/coreActions.php";

/**
 * ExampleDeferredAction
 *
 * A comprehensive reference implementation for developers building custom Deferred Actions in SinergiaCRM AWF.
 *
 * This template demonstrates:
 * 1. Extending 'DeferredActionDefinition' to implement a generic, DataBlock-agnostic deferred action.
 * 2. Implementing 'IWebhookDecodable' to allow SinergiaCRM's WebhookHandler to parse custom incoming payloads,
 * extract tokens via external transaction IDs, and handle orphan webhooks (e.g., subscription renewals).
 * 3. Following SinergiaCRM's 'create_guid()' + 'new_with_id = true' single-save performance pattern.
 * 4. Correctly utilizing 'ResultStatus::WAIT' to pause execution, and 'ResultStatus::OK' when resuming flows.
 * 5. Differentiating between blocking (WAIT) and non-blocking (OK) initiations.
 *
 * SUGGESTED NAMING: <Purpose>Action (e.g., DocumentSigningAction, ExternalValidationAction).
 * FILE LOCATION: modules/stic_AWF_Forms/actions/Deferred/ExampleDeferredAction.php
 */
class ExampleDeferredAction extends DeferredActionDefinition implements IWebhookDecodable
{
    /**
     * Constructor
     */
    public function __construct()
    {
        // Must be true for SinergiaCRM to recognize and discover this action
        $this->isActive = true;

        // Whether this action is visible and selectable in the Wizard
        $this->isUserSelectable = false;

        // Categorization for organization in the designer dropdowns (from the list stic_awf_forms_action_definition_category_list)
        $this->category = 'integration';

        // Translation prefix defined in the language files (e.g., LBL_EXAMPLE_DEFERRED_ACTION_TITLE)
        $this->baseLabel = 'LBL_EXAMPLE_DEFERRED_ACTION';

        // Override default expiration days if needed (the parent class default is '30')
        $this->defaultExpirationDays = '14';
    }

    /**
     * Declares who will resume this deferred process and how.
     *
     * - SERVER_WEBHOOK:  The flow continues when an external machine makes an API call in the background.
     * No human session is active. Terminal actions cannot render HTML outputs here.
     * - ORIGINAL_USER:   The flow continues when the original visitor clicks a link in their browser (Front-Channel).
     * HTML responses, summaries, or redirects can be safely rendered.
     * - THIRD_PARTY_HUMAN: The flow is resumed manually by a CRM Administrator.
     */
    public function getResumptionContext(): DeferredResumptionContext
    {
        return DeferredResumptionContext::SERVER_WEBHOOK;
    }

    /**
     * Defines ADDITIONAL configuration parameters displayed in Step 3 of the Form Wizard.
     * Note: The parent class automatically injects 'expiration_days'.
     *
     * @return ActionParameterDefinition[]
     */
    protected function getDeferredParameters(): array
    {
        $parameters = [];

        // Example 1: Endpoint URL of the external platform we need to communicate with
        $paramUrl = new ActionParameterDefinition();
        $paramUrl->name = 'external_endpoint';
        $paramUrl->text = "External API Endpoint (VALUE)";
        $paramUrl->description = "The target endpoint URL where we will dispatch our verification request.";
        $paramUrl->type = ActionParameterType::VALUE;
        $paramUrl->dataType = ActionDataType::URL;
        $paramUrl->required = true;
        $parameters[] = $paramUrl;

        // Example 2: Static key or Identifier for target campaigns/accounts
        $paramCampaign = new ActionParameterDefinition();
        $paramCampaign->name = 'target_campaign';
        $paramCampaign->text = "Target Campaign (CRM_RECORD)";
        $paramCampaign->description = "Link this deferred validation to a SinergiaCRM Campaign record.";
        $paramCampaign->type = ActionParameterType::CRM_RECORD;
        $paramCampaign->supportedModules = ['Campaigns'];
        $paramCampaign->required = false;
        $parameters[] = $paramCampaign;

        return $parameters;
    }

    /**
     * Label displayed in SinergiaCRM's Wizard for the green subflow (executed when resolved successfully).
     */
    public function getFlowSuccessLabel(): string
    {
        return "On Validation Success (Sub-flow)";
    }

    /**
     * Label displayed in SinergiaCRM's Wizard for the red subflow (executed on failure or expiration).
     */
    public function getFlowErrorLabel(): string
    {
        return "On Validation Failure / Expired (Sub-flow)";
    }

    // =========================================================================================
    // SCRIPT INITIATION (IDeferredAction contract)
    // =========================================================================================

    /**
     * execute()
     * First-stage execution: Dispatches the task to the external platform and pauses the current thread.
     *
     * @param ExecutionContext $context  Global execution context.
     * @param FormAction $actionConfig   The parsed visual configuration of this action.
     * @return ActionResult              Typically ResultStatus::WAIT to halt current flow execution.
     */
    public function execute(ExecutionContext $context, FormAction $actionConfig): ActionResult
    {
        // Retrieve visual configuration parameters resolved by the framework
        $endpoint = $actionConfig->getResolvedParameter('external_endpoint');
        /** @var ?BeanReference $campaignRef */
        $campaignRef = $actionConfig->getResolvedParameter('target_campaign');

        // Generate an external transactional ID to mimic gateway tracking (Stripe/Redsys/CECA style)
        $externalTxId = 'TX_EXT_' . bin2hex(random_bytes(8));

        // Create the Deferred Ticket to track the transaction
        /** @var stic_AWF_Deferred_Tickets $ticket */
        $ticket = $this->createDeferredTicket(
            $context,
            $actionConfig,
            null,
            [   // Custom data
                'external_tx_id' => $externalTxId,
                'campaign_id' => $campaignRef?->beanId ?? ''
            ], 
            'External API Validation'
        );

        // Map physical relational columns required by WebhookHandler query lookups
        $ticket->external_transaction_id = $externalTxId;

        // Save the Ticket to DB
        $ticket->save();

        $GLOBALS['log']->info("Line " . __LINE__ . " - " . __METHOD__ . ": Created AWF Deferred Ticket ID={$ticket->id} for external transaction {$externalTxId}");

        // Dispatch payload to External API (Simulated)
        // In a real-world integration, you would trigger a cURL/Guzzle call to $endpoint here, 
        // passing the $ticket->token_hash (for redirects) or the $externalTxId (for webhooks).
        
        // Return WAIT to pause the flow and wait for the callback (Front-Channel or Back-Channel Webhook)
        $result = new ActionResult(ResultStatus::WAIT, $actionConfig, "Dispatched validation task. Awaiting external callback.");
        return $result;
    }

    /**
     * processWebhook()
     * Resumption-stage execution: Called by SinergiaCRM's WebhookHandler when the callback arrives.
     *
     * @param ExecutionContext $context    Global context rebuilt from SinergiaCRM's database snapshot.
     * @param array $requestData           Raw HTTP payload (usually POST/GET variables).
     * @return ActionResult                ResultStatus::OK (triggers Success Subflow) or ResultStatus::ERROR (triggers Error Subflow).
     */
    public function processWebhook(ExecutionContext $context, array $requestData): ActionResult
    {
        $deferredCtx = $context->deferredContext;
        if ($deferredCtx === null) {
            return new ActionResult(ResultStatus::ERROR, null, "Critical Error: Stored deferred execution context not found.");
        }

        // Retrieve custom properties serialized during the execute() phase
        $storedTxId = $deferredCtx->getCustom('external_tx_id');
        $storedCampaignId = $deferredCtx->getCustom('campaign_id');

        // Extract values from incoming payload (Example expects JSON/POST with 'status' and 'error_message')
        $externalStatus = $requestData['status'] ?? 'unknown';
        $errorMessage = $requestData['error_message'] ?? 'No error message provided';

        $GLOBALS['log']->info("Line " . __LINE__ . " - " . __METHOD__ . ": Processing resumption for transaction={$storedTxId}, campaign={$storedCampaignId}");

        // Evaluate the result of the third-party operation
        if ($externalStatus === 'verified' || $externalStatus === 'success') {
            return new ActionResult(ResultStatus::OK, null, "External verification completed successfully.");
        }

        return new ActionResult(ResultStatus::ERROR, null, "External verification failed. Reason: " . $errorMessage);
    }

    // =========================================================================================
    // DECODING & ORPHANS (IWebhookDecodable contract)
    // =========================================================================================

    /**
     * Indicates whether this action is registered to handle incoming calls from a specific source identifier.
     * E.g., URL contains: index.php?entryPoint=stic_AWF_webhookHandler&source=example_deferred_action
     */
    public function handlesSource(string $source): bool
    {
        return $source === 'example_deferred_action';
    }

    /**
     * Invoked when an incoming webhook lacks the direct 'token' parameter in SinergiaCRM's URL.
     * Allows the action to inspect the payload, headers, or body, and return the unique tracking ID
     * (either the internal token_hash or the external_transaction_id) to look up and lock the Ticket.
     *
     * @param string $source       Source url parameter.
     * @param array $requestData   POST/GET parameters.
     * @param string $rawPayload   Raw body payload (useful for JSON/XML signatures).
     * @param array $headers       HTTP request headers.
     * @return string|null         The transaction ID used for ticket matching.
     */
    public function extractTokenFromEvent(string $source, array $requestData, string $rawPayload, array $headers): ?string
    {
        // Try to parse raw body if JSON
        $data = json_decode($rawPayload, true);
        if (is_array($data) && isset($data['transaction_id'])) {
            return $data['transaction_id']; // This will match $ticket->external_transaction_id
        }

        // Fallback to standard request parameters
        return $requestData['tx_id'] ?? $_REQUEST['token'] ?? null;
    }

    /**
     * Handles incoming transactional events that have NO corresponding active ticket in the CRM.
     * E.g., Recurrent subscription payments, or sudden notifications from a deactivated integration.
     *
     * @param ExecutionContext $context  Isolated context.
     * @param string $source             The source url parameter.
     * @param array $rawData             Incoming HTTP payload.
     * @return ActionResult              The output of the emergency operation.
     */
    public function processOrphanWebhook(ExecutionContext $context, string $source, array $rawData): ActionResult
    {
        $GLOBALS['log']->warn("Line " . __LINE__ . " - " . __METHOD__ . ": Received orphan webhook for source '{$source}'. Initiating standalone emergency logging.");

        // Handled as an infrastructure log
        return new ActionResult(ResultStatus::OK, null, "Orphan webhook logged. No active ticket was modified.");
    }
}