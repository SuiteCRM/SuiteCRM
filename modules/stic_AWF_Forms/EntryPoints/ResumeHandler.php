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
require_once "modules/stic_AWF_Forms/core/actiondefs/includes.php";
require_once "modules/stic_AWF_Deferred_Tickets/stic_AWF_Deferred_Tickets.php";
require_once "modules/stic_Web_Forms/Catcher/FormConfig.php";

/**
 * EntryPoint: stic_AWF_resumeHandler
 * Exclusively handles Sequential/Linear Resumes (e.g., user returning from Stripe/Redsys).
 *
 * Exclusively manages the synchronous return of the user (Browser) of sequential actions.
 * Resumes the thread of remaining actions or redirects to the error flow depending on the platform response.
 */
class ResumeHandler
{
    public function run(): void
    {
        // Get Token
        $token = $_REQUEST['token'] ?? '';
        if (empty($token)) {
            $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ": Invalid request: missing token");
            stic_AWFUtils::renderGenericResponseError(null);
            return;
        }

        // Get Ticket from token
        /** @var stic_AWF_Deferred_Tickets $ticket */
        $ticket = BeanFactory::getBean('stic_AWF_Deferred_Tickets');
        $ticket->retrieve_by_string_fields(['token_hash' => $token, 'deleted' => 0]);
        if (empty($ticket->id)) {
            $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ": Invalid request: no ticket found for token");
            stic_AWFUtils::renderGenericResponseError(null);
            return;
        }

        // Get request status
        $status = $_REQUEST['status'] ?? 'ok';
        $isCli = (php_sapi_name() === 'cli');

        try {
            // Rebuild context from ticket
            $context = stic_AWFUtils::rebuildContextFromTicket($ticket);
            $deferredData = DeferredContextData::fromJson($ticket->context_data);
            $contextData = $deferredData->toArray();

            $specificErrorFlowId = $contextData['flow_error_id'] ?? '-1';
            $errorFlow = $context->formConfig->flows[$specificErrorFlowId] ?? $context->formConfig->flows['-1'] ?? null;

            $executor = new ServerActionFlowExecutor($context);

            // Error case: Execute error flow
            if ($status === 'error' || $ticket->status === 'failed') {
                $GLOBALS['log']->warn('Line ' . __LINE__ . ': ' . __METHOD__ . ": Gateway returned error status 'error' for ticket ID {$ticket->id}. Executing contextual error flow.");

                if ($ticket->status !== 'failed') {
                    $ticket->status = 'failed';
                    $ticket->save();
                }

                if ($context->responseBean && $context->responseBean->status === 'awaiting_action') {
                    $context->responseBean->status = 'error';
                    $context->responseBean->save();
                }

                if ($errorFlow) {
                    $lastResult = $executor->executeFlow($errorFlow);
                    $lastAction = $lastResult->getAction();
                    if ($lastAction instanceof ITerminalAction && !$isCli) {
                        $lastAction->performTerminal($context, $lastResult);
                    }
                }
                stic_AWFUtils::renderGenericResponseError($context->formConfig);
                return;
            }

            // Ok case: Execute the action's flow
            $originFlow = null;
            foreach ($context->formConfig->flows as $flow) {
                if (isset($flow->actions[$ticket->handler_action_id])) {
                    $originFlow = $flow;
                    break;
                }
            }
            if (!$originFlow) {
                $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ": Origin flow containing action '{$ticket->handler_action_id}' not found.");
                stic_AWFUtils::renderGenericResponseError($context->formConfig);
                return;
            }

            // Get remaining actions, after current deferred action
            $remainingActions = [];
            $foundPausingAction = false;
            foreach ($originFlow->actions as $action) {
                if ($foundPausingAction) {
                    $remainingActions[$action->id] = $action;
                }
                if ($action->id === $ticket->handler_action_id) {
                    $foundPausingAction = true;
                }
            }

            // Build a virtual flow to resume
            $virtualFlow = new FormFlow();
            $virtualFlow->id = 'virtual_resume_flow';
            $virtualFlow->actions = $remainingActions;

            if ($ticket->status === 'processed') {
                $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ": The ticket has already been processed previously. Delegating the terminal action exclusively.");
                if (!$isCli) {
                    $executor->executeTerminalActionOnly($virtualFlow);
                }
                stic_AWFUtils::renderGenericResponseSuccess($context->formConfig);
                return;
            }
            
            $lastResult = new ActionResult(ResultStatus::OK, null);
            if (!empty($remainingActions)) {
                $lastResult = $executor->executeFlow($virtualFlow, $errorFlow);
                stic_AWFUtils::updateResponseExecutionLog($context);
                
                if ($lastResult->isError()) {
                    $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": Virtual flow collapsed into unrecovered error. Rendering fallback.");
                    $ticket->status = 'failed';
                    $ticket->save();

                    if ($context->responseBean) {
                        $context->responseBean->status = 'error';
                        $context->responseBean->save();
                    }
                    stic_AWFUtils::renderGenericResponseError($context->formConfig);
                    return;
                }
            }

            $ticket->status = 'processed';
            $ticket->save();

            if ($context->responseBean && !$lastResult->isError()) {
                $context->responseBean->status = 'processed';
                $context->responseBean->save();
            }

            $lastAction = $lastResult->getAction();
            if ($lastAction instanceof ITerminalAction && !$isCli) {
                try {
                    $lastAction->performTerminal($context, $lastResult);
                } catch (\Throwable $t) {
                    $context->addError($t, $lastResult->actionConfig);
                    $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": Error executing sequential terminal action: " . $t->getMessage());
                }
            }

            // Visual Fallback
            stic_AWFUtils::renderGenericResponseSuccess($context->formConfig);
        } catch (Exception $e) {
            $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ": ResumeHandler exception: " . $e->getMessage());
            stic_AWFUtils::renderGenericResponseError(null);
        }
    }
}

$handler = new ResumeHandler();
$handler->run();
