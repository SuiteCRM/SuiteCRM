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

include_once "modules/stic_AWF_Forms/actions/coreActions.php";
require_once "modules/stic_Payment_Commitments/stic_Payment_Commitments.php";
require_once "modules/stic_Payments/stic_Payments.php";
require_once "modules/stic_AWF_Deferred_Tickets/stic_AWF_Deferred_Tickets.php";

abstract class stic_AWF_PaymentStrategy {
    use DeferredActionHelperTrait;
    
    protected ?string $suffix = null;

    protected string $configType = ''; // 'TPV', 'STRIPE'...
    protected string $configKeyPrefix = ''; // 'TPV', 'TPVCECA', 'STRIPE'...

    /** @var ?stic_AWF_Deferred_Tickets Ticket created in initiate(), used by getReturnUrl() */
    protected ?stic_AWF_Deferred_Tickets $ticket = null;

    protected ?array $settings = null; // Cache with loaded configurations from DB

    /**
     * Configure suffix to load alternative constants
     * Ex. Football...
     */
    public function setSuffix(string $suffix): void {
        $this->suffix = $suffix;
    }

    /**
    * Loads the configurations and resolves the values.
    * @param array $keys List of keys without prefix (ex: ['MERCHANT_CODE'])
    * @return array
    */
    protected function getConfigValues(array $keys): array {
        require_once "modules/stic_Settings/Utils.php";

        // Lazy load all the configuration of this type
        if ($this->settings === null) {
            $this->settings = stic_SettingsUtils::getSettingsByType($this->configType);
            if (!is_array($this->settings)) {
                $this->settings = array();
                $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ": Could not load settings of type ". $this->configType);
                return [];
            }
        }

        $resolvedValues = array();
        $prefix = $this->configKeyPrefix; // Ex: 'TPV'

        foreach ($keys as $key) {
            // General Key: {PREFIX}_{KEY}  => TPV_MERCHANT_CODE
            $defaultConfigKey = $prefix . '_' . $key;
            // Initial value
            $value = isset($this->settings[$defaultConfigKey]) ? $this->settings[$defaultConfigKey] : null;

            // Key with Suffix: {PREFIX}_ALT_{SUFFIX}_{KEY} => TPV_ALT_FOOTBALL_MERCHANT_CODE
            if ($this->suffix) {
                $altConfigKey = $prefix . '_ALT_' . $this->suffix . '_' . $key;
                if (isset($this->settings[$altConfigKey]) && $this->settings[$altConfigKey] !== '') {
                    $value = $this->settings[$altConfigKey];
                }
            }
            $resolvedValues[$key] = $value;
        }

        return $resolvedValues;
    }

    /**
     * Creates a Deferred Ticket record to track this payment.
     * Stores strategy_class, strategy_suffix, payment_id, flow_success_id and flow_error_id
     * in context_data so the webhook can reconstruct the context.
     *
     * @param ExecutionContext $context The execution context
     * @param FormAction $actionConfig The action configuration
     * @param stic_Payments $beanPayment The payment bean
     * @param string $externalTransactionId The external transaction ID from the gateway
     * @return stic_AWF_Deferred_Tickets The created ticket
     */
    protected function createTicket(ExecutionContext $context, FormAction $actionConfig, stic_Payments $beanPayment, string $externalTransactionId): stic_AWF_Deferred_Tickets {
        /** @var stic_AWF_Deferred_Tickets $ticket */
        // Create a deferred ticket
        $ticket = $this->createDeferredTicket(
            $context,
            $actionConfig,
            $beanPayment,
            [   // Custom data
                'strategy_class' => static::class,
                'strategy_suffix' => $this->suffix,
                'payment_id' => $beanPayment->id
            ], 
            'AWF Payment: ' . $beanPayment->id
        );

        $ticket->external_transaction_id = $externalTransactionId;

        // Save the ticket
        $ticket->save();

        $this->ticket = $ticket;

        $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ": AWF PaymentStrategy: Created Deferred Ticket ID={$ticket->id} for payment {$beanPayment->id}");

        return $ticket;
    }

    /**
     * Updates the payment status and related fields, then saves the bean.
     *
     * @param stic_Payments $beanPayment The payment bean
     * @param string $status The new status value (paid, not_remitted, rejected_gateway, pending, etc.)
     * @param array $options Optional fields: authCode, gatewayLog, gatewayRejectionReason, amount
     */
    protected function updatePayment(stic_Payments $beanPayment, string $status, array $options = []): void {
        // Ensure we have the latest data to prevent overwriting concurrent updates (e.g. from another webhook)
        $beanPayment->retrieve($beanPayment->id);

        // Check if the payment is still pending and not deleted
        if ($beanPayment->status !== 'pending' || !empty($beanPayment->deleted)) {
            $GLOBALS['log']->warn('Line ' . __LINE__ . ': ' . __METHOD__ . ": Update skipped for payment [{$beanPayment->id}]. Status is no longer 'pending' (concurrent webhook detected).");
            return;
        }

        // Assign new values
        $beanPayment->status = $status;
        if (isset($options['authCode'])) {
            $beanPayment->banking_concept = (string)$options['authCode'];
        }
        if (isset($options['gatewayLog'])) {
            $beanPayment->gateway_log = ($beanPayment->gateway_log ?? '') . '##### ' . $options['gatewayLog'];
        }
        if (isset($options['gatewayRejectionReason'])) {
            $beanPayment->gateway_rejection_reason = $options['gatewayRejectionReason'];
        }
        if (isset($options['amount'])) {
            $beanPayment->amount = floatval($options['amount']);
        }

        // Save the updated payment bean
        $beanPayment->save();
    }

    /**
     * Disable related payment commitment by setting end_date when a recurring payment is rejected.
     * Matches stic_Web_Forms PaymentBO::disablePaymentCommitment() behavior.
     *
     * @param stic_Payments $paymentBean The rejected payment bean
     */
    protected static function disablePaymentCommitment(stic_Payments $paymentBean): void {
        require_once 'SticInclude/Utils.php';
        $PCBean = SticUtils::getRelatedBeanObject($paymentBean, 'stic_payments_stic_payment_commitments');
        if ($PCBean && $PCBean->periodicity != 'punctual') {
            $PCBean->end_date = date('Y-m-d');
            $PCBean->save(false);
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Payment commitment [{$PCBean->id}] has been deactivated (end_date = today) because the first payment has been rejected by the gateway.");
        }
    }

    /**
     * Checks if a payment bean is already in a terminal processed state.
     * Used for idempotency: if the payment is already paid/rejected, the webhook
     * is a duplicate and should be acknowledged without re-processing.
     *
     * @param stic_Payments $paymentBean The payment bean to check
     * @return bool True if the payment is already in a terminal state
     */
    protected static function isAlreadyProcessed(stic_Payments $paymentBean): bool {
        $terminalStatuses = ['paid', 'not_remitted', 'rejected_gateway', 'rejected_manual', 'cancelled'];
        return in_array($paymentBean->status ?? '', $terminalStatuses, true);
    }

    /**
     * Get the related Payment Commitment bean from a Payment bean.
     *
     * @param stic_Payments $paymentBean The payment bean
     * @return stic_Payment_Commitments|false The related PC bean or false
     */
    protected static function getPaymentCommitment(stic_Payments $paymentBean) {
        require_once 'SticInclude/Utils.php';
        return SticUtils::getRelatedBeanObject($paymentBean, 'stic_payments_stic_payment_commitments');
    }

    /**
     * Returns the URL to redirect the user after the gateway processes the payment.
     * The ReturnHandler uses the token to look up the ticket and relies on the
     * database status (updated via webhook), not on URL parameters.
     * Requires $this->ticket to be set (call createTicket first).
     *
     * @param string $status The return context status ('ok' or 'error')
     * @return string The full return URL
     */
    protected function getReturnUrl(string $status = 'ok'): string {
        return $this->getSequentialReturnUrl($this->ticket, $status);
    }

    /**
     * Returns the webhook callback URL for a given payment source.
     *
     * @param string $source The payment source identifier (e.g. 'redsys', 'stripe')
     * @param array $extraParams
     * @return string The full callback URL
     */
    protected function getCallbackUrl(string $source, array $extraParams = []): string {
        return $this->getAsyncCallbackUrl($source, $this->ticket, $extraParams);
    }

    /**
     * Renders an HTML template by substituting {VAR_NAME} placeholders.
     * Looks first in modules/stic_AWF_Forms/tpls/, then falls back to
     * modules/stic_Web_Forms/Catcher/Include/Payment/tpls/.
     *
     * @param string $templateName Template file name without extension (e.g. 'TPVFirstStep')
     * @param array $vars Associative array of placeholder => value substitutions
     * @return string The rendered HTML string
     */
    protected function renderTemplate(string $templateName, array $vars): string {
        $awfPath = "modules/stic_AWF_Forms/tpls/{$templateName}.html";
        $wfPath  = "modules/stic_Web_Forms/Catcher/Include/Payment/tpls/{$templateName}.html";

        $templateFile = null;
        if (file_exists($awfPath)) {
            $templateFile = $awfPath;
        } elseif (file_exists($wfPath)) {
            $templateFile = $wfPath;
        }

        if ($templateFile === null) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": AWF PaymentStrategy: Template not found: {$templateName}");
            return '';
        }

        $html = file_get_contents($templateFile);
        foreach ($vars as $key => $value) {
            $html = str_replace('{' . $key . '}', (string)$value, $html);
        }
        return $html;
    }

    /**
     * Prepare payment.
     * If Offline -> Returns OK.
     * If External platform -> Returns WAIT with data to redirection.
     */
    public function initiate(ExecutionContext $context, FormAction $actionConfig, stic_Payments $beanPayment): ActionResult {
        $strategyClass = static::class;
        $strategySuffix = $this->suffix ?? '';
        $paymentId = $beanPayment->id;
        $paymentAmount = $beanPayment->amount;

        $GLOBALS['log']->info("Line " . __LINE__ . " - " . __METHOD__ . ": AWF Payment initiation started for Strategy '" . $strategyClass . "' (Suffix: '{$strategySuffix}'). Payment ID: {$paymentId}, Amount: {$paymentAmount}");

        try {
            $result = $this->initiateStrategy($context, $actionConfig, $beanPayment);
            $GLOBALS['log']->info("Line " . __LINE__ . " - " . __METHOD__ . ": AWF PaymentStrategy unrolled successfully with status '" . $result->status->value . "' for Payment ID: {$paymentId}");
        } catch (\Throwable $t) {
            $GLOBALS['log']->fatal("Line " . __LINE__ . " - " . __METHOD__ . ": AWF Payment infrastructure collapsed during strategy execution. Error: " . $t->getMessage());
            $result = new ActionResult(ResultStatus::ERROR, $actionConfig, $t->getMessage());
        }
     
        if ($result->isWait() || $result->isOk()) {
            $result->setData(array_merge($result->getData(), [
                'strategy_class' => $strategyClass,
                'strategy_suffix' => $strategySuffix,
                'payment_id' => $paymentId,
                'payment_amount' => $paymentAmount
            ]));
        }

        return $result;
    }

    /**
     * Returns the webhook source identifier for this strategy.
     * Used by WebhookHandler to route incoming webhooks to the correct strategy.
     */
    abstract public static function getSourceName(): string;

    /**
     * Extracts the external transaction ID from the raw webhook request data.
     * Each gateway sends the ID in a different location/format.
     *
     * @param array $rawData POST data array
     * @param string $rawBody Raw request body (for JSON-based gateways)
     * @return string|null The external transaction ID or null if not found
     */
    abstract public static function extractExternalId(array $rawData, string $rawBody , array $headers): ?string;

    /**
     * Prepare payment for the current Strategy (Offline, RedSys, CECA...)
     */
    abstract protected function initiateStrategy(ExecutionContext $context, FormAction $actionConfig, stic_Payments $beanPayment): ActionResult;


    /**
    * Terminal: Execute the output (HTML form, Redirect header...).
    * Only called if initiate() has returned WAIT.
    */
    abstract public function performTerminal(ExecutionContext $context, ActionResult $result): void;

    /**
    * WEBHOOK: Process action when notification arrives from external event.
    * Can be called with or without a Deferred Ticket:
    * - With ticket: $context->deferredContext contains strategy_class, payment_id, etc.
    * - Without ticket: context is minimal; strategy handles recurring events directly.
    */ 
    abstract public function processNotification(ExecutionContext $context, ActionResult $result): ActionResult;
}