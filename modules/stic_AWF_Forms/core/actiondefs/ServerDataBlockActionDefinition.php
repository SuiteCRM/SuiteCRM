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
 * Abstract class for actions that operate on ONE data block.
 * Automates the definition, obtaining and validation of the DataBlock parameter.
 */
abstract class ServerDataBlockActionDefinition extends ServerActionDefinition {

    /** 
     * (Optional) Override to change the name of the parameter that contains the data block.
     * @return string
    */
    protected function getDataBlockParameterName(): string {
        return 'data_block_id';
    }

    /**
     * (Optional) Override to define the text (label) of the data block parameter.
     * @return string
     */
    protected function getDataBlockParameterText(): string {
        return translate('LBL_CUSTOM_ACTION_DATABLOCK_PARAM_TEXT', 'stic_AWF_Forms');
    }

    /**
     * (Optional) Override to define the description (help text) of the data block parameter.
     * @return string
     */
    protected function getDataBlockParameterDescription(): string {
        return translate('LBL_CUSTOM_ACTION_DATABLOCK_PARAM_DESC', 'stic_AWF_Forms');
    }

    /**
     * (Optional) Override to limit which modules the data block parameter can point to.
     * @return string[] List of allowed modules (e.g.: ['Contacts', 'Accounts'])
     */
    protected function getSupportedModules(): array {
        return []; // Empty = all modules
    }

    /**
     * (Optional) Override to add ADDITIONAL parameters before the data block parameter.
     * @return ActionParameterDefinition[]
     */
    protected function getInitialCustomParameters(): array {
        return [];
    }

    /**
     * (Optional) Override to add ADDITIONAL parameters besides the data block parameter.
     * @return ActionParameterDefinition[]
     */
    protected function getCustomParameters(): array {
        return [];
    }

    final public function getParameters(): array
    {
        // Create the data block parameter
        $blockParam = new ActionParameterDefinition();
        $blockParam->name = $this->getDataBlockParameterName();
        $blockParam->text = $this->getDataBlockParameterText();
        $blockParam->description = $this->getDataBlockParameterDescription();
        $blockParam->type = ActionParameterType::DATA_BLOCK;
        $blockParam->supportedModules = $this->getSupportedModules();
        $blockParam->required = true;

        // Add the custom parameters from the developer
        $initialCustomParams = $this->getInitialCustomParameters();
        $customParams = $this->getCustomParameters();
        
        return array_merge($initialCustomParams, [$blockParam], $customParams);
    }

    final public function execute(ExecutionContext $context, FormAction $actionConfig): ActionResult
    {
        /** @var ?DataBlockResolved $block */
        $block = $actionConfig->getResolvedParameter($this->getDataBlockParameterName());

        if ($block === null) {
            return new ActionResult(ResultStatus::ERROR, $actionConfig, "Can not resolve DataBlock parameter '{$this->getDataBlockParameterName()}'.");
        }

        // Call the method that will be implemented in the action
        return $this->executeWithBlock($context, $actionConfig, $block);
    }

    /**
     * Method to be implemented
     * Executes the action, receives the main data block resolved and validated.
     *
     * @param ExecutionContext $context The global context.
     * @param FormAction $actionConfig The configuration of the action.
     * @param DataBlockResolved $block The main data block, ready to be used.
     * @return ActionResult
     */
    public abstract function executeWithBlock(ExecutionContext $context, FormAction $actionConfig, DataBlockResolved $block): ActionResult;
}