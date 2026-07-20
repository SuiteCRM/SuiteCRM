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
 * Abstract class representing a deferred action definition, which is a type of server action that is executed asynchronously after the form submission.
 * This class implements the IDeferredAction interface and extends the ServerActionDefinition class, 
 * providing a common structure for all deferred actions while enforcing the implementation of the getType method to return ActionType::
 */
abstract class DeferredActionDefinition extends ServerActionDefinition implements IDeferredAction {
    use DeferredActionHelperTrait;

    protected string $defaultExpirationDays = '30';

    /**
     * Returns the type of the action, which is ActionType::DEFERRED for all classes extending DeferredActionDefinition.
     * This method is final to ensure that all deferred actions consistently return the correct type which is ActionType::DEFERRED.
     * @return ActionType The type of the action, which is ActionType::DEFERRED
     */
    final public function getType(): ActionType {
        return ActionType::DEFERRED;
    }

    /**
     * Returns the Subflow success label
     */
    public function getFlowSuccessLabel(): string { return $this->translate('FLOW_SUCCESS'); }

    /**
     * Returns the Subflow error label
     */
    public function getFlowErrorLabel(): string { return $this->translate('FLOW_ERROR'); }

    /**
     * Returns the parameters defined for the action
     * @return ActionParameterDefinition[] The parameters of the action
     */
    final public function getParameters(): array
    {
        /** @var ActionParameterDefinition[] $parameters */
        $parameters = [];

        $paramDays = new ActionParameterDefinition();
        $paramDays->name = 'expiration_days';
        $paramDays->text = translate('LBL_PARAM_EXPIRATION_DAYS', 'stic_AWF_Forms');
        $paramDays->description = translate('LBL_PARAM_EXPIRATION_DAYS_DESC', 'stic_AWF_Forms');
        $paramDays->type = ActionParameterType::VALUE;
        $paramDays->dataType = ActionDataType::INTEGER;
        $paramDays->defaultValue = $this->defaultExpirationDays;
        $paramDays->colSize = 'col-9';
        $paramDays->required = true;
        $parameters[] = $paramDays;

        $paramSpacer = new ActionParameterDefinition();
        $paramSpacer->name = 'spacer_1';
        $paramSpacer->text = '';
        $paramSpacer->type = ActionParameterType::EMPTY; 
        $paramSpacer->colSize = 'col-10';
        $parameters[] = $paramSpacer;

        if ($this->getResumptionContext() !== DeferredResumptionContext::SERVER_WEBHOOK) {
            $paramProcTitle = new ActionParameterDefinition();
            $paramProcTitle->name = 'already_processed_title';
            $paramProcTitle->text = translate('LBL_PARAM_ALREADY_PROCESSED_TITLE', 'stic_AWF_Forms');
            $paramProcTitle->description = translate('LBL_PARAM_ALREADY_PROCESSED_TITLE_DESC', 'stic_AWF_Forms');
            $paramProcTitle->type = ActionParameterType::VALUE;
            $paramProcTitle->dataType = ActionDataType::TEXT;
            $paramProcTitle->defaultValue = translate('LBL_PARAM_ALREADY_PROCESSED_TITLE_DEFAULT', 'stic_AWF_Forms');
            $paramProcTitle->required = false;
            $paramProcTitle->colSize = 'col-6';
            $parameters[] = $paramProcTitle;

            $paramProcMsg = new ActionParameterDefinition();
            $paramProcMsg->name = 'already_processed_message';
            $paramProcMsg->text = translate('LBL_PARAM_ALREADY_PROCESSED_TEXT', 'stic_AWF_Forms');
            $paramProcMsg->description = translate('LBL_PARAM_ALREADY_PROCESSED_TEXT_DESC', 'stic_AWF_Forms');
            $paramProcMsg->type = ActionParameterType::VALUE;
            $paramProcMsg->dataType = ActionDataType::TEXTAREA;
            $paramProcMsg->defaultValue = translate('LBL_PARAM_ALREADY_PROCESSED_TEXT_DEFAULT', 'stic_AWF_Forms');
            $paramProcMsg->required = false;
            $paramProcMsg->colSize = 'col-6';
            $parameters[] = $paramProcMsg;

            $paramTitle = new ActionParameterDefinition();
            $paramTitle->name = 'expired_title';
            $paramTitle->text = translate('LBL_PARAM_EXPIRED_TITLE', 'stic_AWF_Forms');
            $paramTitle->description = translate('LBL_PARAM_EXPIRED_TITLE_DESC', 'stic_AWF_Forms');
            $paramTitle->type = ActionParameterType::VALUE;
            $paramTitle->dataType = ActionDataType::TEXT;
            $paramTitle->defaultValue = translate('LBL_PARAM_EXPIRED_TITLE_DEFAULT', 'stic_AWF_Forms');
            $paramTitle->required = false;
            $paramTitle->colSize = 'col-6';
            $parameters[] = $paramTitle;

            $paramMsg = new ActionParameterDefinition();
            $paramMsg->name = 'expired_message';
            $paramMsg->text = translate('LBL_PARAM_EXPIRED_TEXT', 'stic_AWF_Forms');
            $paramMsg->description = translate('LBL_PARAM_EXPIRED_TEXT_DESC', 'stic_AWF_Forms');
            $paramMsg->type = ActionParameterType::VALUE;
            $paramMsg->dataType = ActionDataType::TEXTAREA;
            $paramMsg->defaultValue = translate('LBL_PARAM_EXPIRED_TEXT_DEFAULT', 'stic_AWF_Forms');
            $paramMsg->required = false;
            $paramMsg->colSize = 'col-6';
            $parameters[] = $paramMsg;
        }

        return array_merge($parameters, $this->getDeferredParameters());
    }
    
    /**
     * Returns the parameters defined for the deferred action
     * @return ActionParameterDefinition[] The parameters of the deferred action
     */
    protected function getDeferredParameters(): array {
        return [];
    }

}