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

class ServerActionFlowExecutor {
    private ExecutionContext $context; 
    private ServerActionFactory $factory;
    private ParameterResolverService $resolver;

    public function __construct(ExecutionContext $context) {
        $this->context = $context;
        $this->factory = new ServerActionFactory();
        $this->resolver = new ParameterResolverService();
    }

    /**
     * Executes the main flow and manages errors by switching to the error flow if needed.
     * @param FormFlow $flowConfig The flow definition to execute.
     * @param ?FormFlow $errorFlowConfig The error flow definition (null if none).
     * @return ActionResult Returns last ActionResult
     */
    public function executeFlow(FormFlow $flowConfig, ?FormFlow $errorFlowConfig = null): ActionResult {
        $lastResult = new ActionResult(ResultStatus::OK, null);
        $lastActionConfig = null;
        try {
            $actions = $flowConfig->actions ?? [];

            // Preprocess formData to fill in missing boolean/checkbox fields
            // (browsers don't send unchecked checkboxes, so without this the condition would
            // compare null vs '0' and fail)
            stic_AWFUtils::fillMissingBooleanFields($this->context->formConfig, $this->context->formData);

            foreach ($actions as $actionConfig) {
                $lastActionConfig = $actionConfig;

                // Check the Conditions (if any)
                if(!stic_AWFUtils::evaluateConditions($actionConfig->conditions, $this->context->formData)) {
                    $GLOBALS['log']->info('Line '.__LINE__.': '.__METHOD__.': '. "Advanced Web Forms: Skipping action '{$actionConfig->text}' because condition failed.");
                    
                    // Record the action as skipped
                    $skippedResult = new ActionResult(ResultStatus::SKIPPED, $actionConfig, "Condition not met.");
                    $this->context->addActionResult($skippedResult);
                    continue; 
                }

                // Find the action executor (throws if not found)
                $actionExecutor = $this->factory->createAction($actionConfig);

                // Parameter resolution
                $paramDefinitions  = $actionExecutor->getParameters();
                $paramConfigurations = $actionConfig->parameters;
                $resolvedParameters = $this->resolver->resolveAll($actionConfig, $paramDefinitions, $paramConfigurations, $this->context);
                $actionConfig->setResolvedParameters($resolvedParameters);

                // Execute the action
                $lastResult = $actionExecutor->execute($this->context, $actionConfig);
                $lastResult->setAction($actionExecutor);
                
                // Context update
                $this->context->addActionResult($lastResult);

                if ($lastResult->isWait()) {
                    // Mark the response as waiting
                    if ($this->context->responseBean) {
                        $this->context->responseBean->status = 'awaiting_action';
                        $this->context->responseBean->save();
                    }
                    
                    $GLOBALS['log']->info('Line '.__LINE__.': '.__METHOD__.': '. "Advanced Web Forms: Flow paused by action '{$actionConfig->name}'. Reason: " . $lastResult->message);

                    // Return $lastResult to finish: the engine will be put on hold
                    return $lastResult; 
                }

                // Error detection
                if ($lastResult->isError()) {
                    // If the action is marked to continue on error, we log the error but we continue with the next actions of the flow.
                    if ($actionConfig->continue_on_error) {
                        $lastResult->status = ResultStatus::SKIPPED;
                        $lastResult->message = "Ignored Error: " . $lastResult->message;
                        $GLOBALS['log']->warning('Line '.__LINE__.': '.__METHOD__.': '. "Advanced Web Forms: Action '{$actionConfig->name}' failed but is marked to continue. Error: " . $lastResult->message);
                        continue; 
                    }

                    // If there's an error flow: immediately switch to the error flow
                    if ($errorFlowConfig !== null) {
                        return $this->executeFlow($errorFlowConfig);
                    }
                    // If there is no error flow, finish
                    return $lastResult; 
                }
            }
        } catch (\Throwable $t) {
            // Catch any Exception or PHP Fatal Error and convert it into a context error
            $GLOBALS['log']->fatal('Line '.__LINE__.': '.__METHOD__.': '."CRITICAL ERROR in ServerActionFlowExecutor: " . $t->getMessage());
            $lastResult = $this->context->addError($t, $lastActionConfig);
            
            // If there's an error flow: immediately switch to it
            if ($errorFlowConfig !== null) {
                try {
                    return $this->executeFlow($errorFlowConfig);
                } catch (\Throwable $t2) {
                    $lastResult = $this->context->addError($t2, $lastActionConfig);
                    $GLOBALS['log']->fatal('Line '.__LINE__.': '.__METHOD__.': '."Double Fault: Error flow failed too: " . $t2->getMessage());
                }
            }

            // If there is no error flow, we finish
            return $lastResult; 
        }
        
        return $lastResult;
    }

}