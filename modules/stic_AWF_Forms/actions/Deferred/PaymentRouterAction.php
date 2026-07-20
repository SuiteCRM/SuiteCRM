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

require_once __DIR__.'/payment/stic_AWF_PaymentStrategyFactory.php';
require_once __DIR__.'/payment/stic_AWF_PaymentStrategy.php';
require_once 'modules/stic_Payment_Commitments/stic_Payment_Commitments.php';

class PaymentRouterAction extends DeferredBeanActionDefinition implements ITerminalAction, IWebhookDecodable
{
    public function __construct() {
        $this->isActive = false;
        $this->isUserSelectable = false;
        $this->category = 'integration';
        $this->baseLabel = 'LBL_PAYMENT_ROUTER_ACTION';
    }

    /**
     * Declares who will resume this deferred process and how.
     */
    public function getResumptionContext(): DeferredResumptionContext {
        return DeferredResumptionContext::SERVER_WEBHOOK;
    }

    /**
     * Modules supported by the action
     */
    protected function getSupportedModules(): array {
        return ['stic_Payment_Commitments'];
    }
    
    /**
     * Name of the parameter that contains the data block.
     * @return string
     */
    protected function getDataBlockParameterText(): string {
        return $this->translate('PAYMENT_COMMITMENT_TEXT');
    }

    /**
     * The description (help text) of the data block parameter.
     * @return string
     */
    protected function getDataBlockParameterDescription(): string {
        return $this->translate('PAYMENT_COMMITMENT_DESC');
    }

    /**
     * Definition of the ADDITIONAL parameters needed for the deferred action
     */
    protected function getDeferredCustomParameters(): array {
        return [];
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
        // $bean is a stic_Payment_Commitments registry
        /** @var stic_Payment_Commitments $paymentCommitmentBean */
        $paymentCommitmentBean = $bean;
     
        // Basic validations
        // Amount 
        if (!is_numeric($paymentCommitmentBean->amount) || $paymentCommitmentBean->amount <= 0) {
            return new ActionResult(ResultStatus::ERROR, $actionConfig, "Invalid amount in Payment Commitment (ID: {$paymentCommitmentBean->id}). Amount: {$paymentCommitmentBean->amount}");
        }
        // Payment method
        if (empty($paymentCommitmentBean->payment_method)) {
            return new ActionResult(ResultStatus::ERROR, $actionConfig, "Payment method is empty in Payment Commitment (ID: {$paymentCommitmentBean->id})");
        }
        // Active
        if (!$paymentCommitmentBean->active) {
            return new ActionResult(ResultStatus::ERROR, $actionConfig, "Inactive Payment Commitment (ID: {$paymentCommitmentBean->id})");
        }

        // Get Payment Strategy
        try {
            /** @var stic_AWF_PaymentStrategy $strategy */
            $strategy = stic_AWF_PaymentStrategyFactory::createFromMethodValue($paymentCommitmentBean->payment_method);
        } catch (Exception $e) {
            return new ActionResult(ResultStatus::ERROR, $actionConfig, "Error getting Payment Strategy for Payment Commitment (ID: {$paymentCommitmentBean->id}): " . $e->getMessage());
        }

        // Get the first Payment from PaymentCommitment

        // Reload the payment commitment bean in order to properly load relationships
        $paymentCommitmentBean->retrieve($paymentCommitmentBean->id);

        // Get the generated payment
        $paymentCommitmentBean->load_relationship('stic_payments_stic_payment_commitments');
        $payments = $paymentCommitmentBean->stic_payments_stic_payment_commitments->getBeans();

        if (empty($payments) || count($payments) < 1) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": An error occurred while trying to get payments from Payment Commitment (ID: {$paymentCommitmentBean->id})");
            return new ActionResult(ResultStatus::ERROR, $actionConfig, "Error getting Payments from Payment Commitment (ID: {$paymentCommitmentBean->id})");
        } 

        $paymentBean = null;
        foreach ($payments as $p) {
            if ($p->status == 'pending' || $p->status == 'not_remitted') {
                $paymentBean = $p;
                break;
            }
        }
        if (!$paymentBean) {
            $paymentBean = reset($payments);
        }

        $paymentMethod = $paymentCommitmentBean->payment_method;
        if ($paymentMethod == 'card' || substr($paymentMethod, 0, 5) == 'card_' || 
            $paymentMethod == 'paypal' || 
            $paymentMethod == 'bizum' || substr($paymentMethod, 0, 5) == 'bizum' || 
            $paymentMethod == 'stripe' || substr($paymentMethod, 0, 7) == 'stripe_') {

            $paymentBean->status = 'pending';
            $paymentBean->save();
        }

        // Reload the object since otherwise will not have reported the id (mysteries of sugar)
        $paymentBean = $paymentBean->retrieve($paymentBean->id);

        // Execute Strategy initiation
        $strategyResult = $strategy->initiate($context, $actionConfig, $paymentBean);

        // If the strategy returns OK immediately (e.g. Offline payment), execute the
        // Deferred OK flow right away for symmetry so confirmation emails etc. are sent.
        if ($strategyResult->isOk()) {
            $this->executeDeferredOkFlow($context, $actionConfig);
        }

        return $strategyResult;
    }

    /**
     * Indicates whether the action knows how to handle the specified Source.
     * @param string $source The source url parameter
     * @return bool indicating if the action can handle the specified source
     */
    public function handlesSource(string $source): bool  {
        // Check if some payment strategy can handle the source (delegated to the factory)
        $strategy = stic_AWF_PaymentStrategyFactory::createFromSource($source);
        return $strategy !== null;
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
    public function extractTokenFromEvent(string $source, array $requestData, string $rawPayload, array $headers): ?string  {
        // Delegated to the factory, which will call the extractExternalId method of the appropriate strategy
        return stic_AWF_PaymentStrategyFactory::extractExternalIdBySource($source, $requestData, $rawPayload, $headers);
    }

    /**
     * Executes the success flow configured on the action (used when a payment resolves OK immediately).
     * Falls back to the error flow if the success flow fails.
     *
     * @param ExecutionContext $context Execution context
     * @param ?FormAction $actionConfig Action configuration containing flow_success_id / flow_error_id
     */
    private function executeDeferredOkFlow(ExecutionContext $context, ?FormAction $actionConfig = null): void {
        $successFlowId = null;
        $successFlow = null;
        $errorFlowId = null;
        $errorFlow = null;

        if ($actionConfig !== null) {
            $successFlowId = $actionConfig->flow_success_id ?? null;
            $errorFlowId = $actionConfig->flow_error_id ?? null;
        } elseif ($context->deferredContext !== null) {
            $successFlowId = $context->deferredContext->flowSuccessId ?? null;
            $errorFlowId = $context->deferredContext->flowErrorId ?? null;
        }

        if ($successFlowId !== null && $successFlowId !== '') {
            $successFlow = $context->formConfig->flows[$successFlowId] ?? null;
        }
        if ($errorFlowId !== null && $errorFlowId !== '') {
            $errorFlow = $context->formConfig->flows[$errorFlowId] ?? null;
        }

        if ($successFlow === null) {
            $GLOBALS['log']->warn('Line ' . __LINE__ . ': ' . __METHOD__ . ": PaymentRouterAction: No success flow configured (flow_success_id={$successFlowId}). Skipping deferred OK flow.");
            return;
        }

        $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ": PaymentRouterAction: Executing Deferred OK flow (ID={$successFlowId}).");

        $executor = new ServerActionFlowExecutor($context);
        $executor->executeFlow($successFlow, $errorFlow);
    }

    /**
     * Called only if execute() was successful.
     * This is where the 'exit', 'header' or HTML is rendered, losing control of execution.
     * 
     * @param ExecutionContext $context Execution context of the action
     * @param ActionResult Result of the execution of the action (last ActionResult)
     */
    public function performTerminal(ExecutionContext $context, ActionResult $executionResult): void {
        // If the action is not in Wait state: do not redirect
        if (!$executionResult->isWait()) return;

        // Recover using the Factory
        try {
            $strategy = stic_AWF_PaymentStrategyFactory::createFromStoredData($executionResult->getData());
            $strategy->performTerminal($context, $executionResult);
        } catch (Exception $e) {
            $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ": PaymentRouter: " . $e->getMessage());
        }
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
        $savedData = $context->deferredContext ? $context->deferredContext->toArray() : [];
        
        try {
            $strategy = stic_AWF_PaymentStrategyFactory::createFromStoredData($savedData);
            $result = new ActionResult(ResultStatus::WAIT, null, '');
            $result->setData($savedData);
            $resolveResult = $strategy->processNotification($context, $result);

            if ($resolveResult->isOk()) {
                $this->enqueueDeferredFlow($context, true);
            } elseif ($resolveResult->isError()) {
                $this->enqueueDeferredFlow($context, false);
            }

            return $resolveResult;
        } catch (Exception $e) {
            return new ActionResult(ResultStatus::ERROR, null, "Error processing webhook response: " . $e->getMessage());
        }
    }

    /**
    * Receives the call for orphan events (without a transactional ticket in the CRM, such as recurrences).
    * Dynamically routes the thread to the appropriate factory and financial strategy.
    */
    public function processOrphanWebhook(ExecutionContext $context, string $source, array $rawData): ActionResult  {
        try {
            // Retrieve the corresponding payment strategy from the webhook source
            $strategy = stic_AWF_PaymentStrategyFactory::createFromSource($source);
            if ($strategy === null) {
                return new ActionResult(ResultStatus::ERROR, null, "Unknown strategy source identifier '{$source}'");
            }
            
            // Launch the polymorphic emergency resolution of the financial strategy
            return $strategy->processNotification($context, new ActionResult(ResultStatus::WAIT, null, ''));
        } catch (\Exception $e) {
            return new ActionResult(ResultStatus::ERROR, null, "PaymentRouter orfan execution failed: " . $e->getMessage());
        }
    }

    /**
     * Enqueues the deferred flow for async execution via SuiteCRM job queue.
     * This ensures the webhook returns HTTP 200 immediately without waiting
     * for the flow (emails, PDFs, etc.) to complete, preventing gateway timeouts.
     *
     * @param ExecutionContext $context The execution context
     * @param bool $isSuccess Whether to run the success or error flow
     */
    private function enqueueDeferredFlow(ExecutionContext $context, bool $isSuccess): void {
        $ticketId = $context->deferredContext ? $context->deferredContext->ticketId : null;

        if (empty($ticketId)) {
            $GLOBALS['log']->warn('Line ' . __LINE__ . ': ' . __METHOD__ . ": No ticket_id in context data. Falling back to synchronous flow execution.");
            if ($isSuccess) {
                $this->executeDeferredOkFlow($context);
            }
            return;
        }

        try {
            require_once 'include/SugarQueue/SugarJobQueue.php';
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
            $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ": Failed to enqueue deferred flow for ticket {$ticketId}. Falling back to sync: " . $e->getMessage());
            if ($isSuccess) {
                $this->executeDeferredOkFlow($context);
            }
        }
    }

}