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

/**
 * RedirectToRecordAction
 * Terminal action that redirects the internal user directly to the DetailView 
 * or EditView of a record processed in a preceding Data Block.
 * This action is restricted strictly to CRM-type forms.
 */
class RedirectToRecordAction extends HookDataBlockActionDefinition implements ITerminalAction
{
    public function __construct()
    {
        $this->isActive = true;
        $this->isUserSelectable = true;
        $this->category = 'navigation';
        $this->baseLabel = 'LBL_REDIRECT_TO_RECORD_ACTION';
        $this->supportedFormTypes = ['crm']; // Applicable to CRM forms
    }

    /**
     * Customizes the automatically injected Data Block parameter label.
     */
    protected function getDataBlockParameterText(): string {
        return $this->translate('TARGET_DATA_BLOCK_TEXT');
    }

    /**
     * Customizes the automatically injected Data Block parameter description.
     */
    protected function getDataBlockParameterDescription(): string {
        return $this->translate('TARGET_DATA_BLOCK_DESC');
    }

    /**
     * Defines ADDITIONAL parameters besides the automatically injected Data Block.
     * * @return ActionParameterDefinition[]
     */
    protected function getCustomParameters(): array {
        $paramView = new ActionParameterDefinition();
        $paramView->name = 'crm_view';
        $paramView->text = $this->translate('CRM_VIEW_TEXT');
        $paramView->type = ActionParameterType::VALUE;
        $paramView->dataType = ActionDataType::SELECT;
        $paramView->defaultValue = 'DetailView';
        $paramView->required = true;
        $paramView->options = [
            new ActionParameterOption('DetailView', $this->translate('CRM_VIEW_DETAILVIEW_TEXT')),
            new ActionParameterOption('EditView', $this->translate('CRM_VIEW_EDITVIEW_TEXT'))
        ];

        return [$paramView];
    }
    
    /**
     * Executes the action receiving the automatically validated DataBlock.
     */
    public function executeWithBlock(ExecutionContext $context, FormAction $actionConfig, DataBlockResolved $block): ActionResult 
    {
        // Fetch additional custom parameter
        $view = $actionConfig->getResolvedParameter('crm_view') ?? 'DetailView';

        // $block is already checked and guaranteed not to be null by parent class
        $beanRef = $block->dataBlock->getBeanReference();
        if ($beanRef === null || empty($beanRef->beanId)) {
            return new ActionResult(ResultStatus::ERROR, $actionConfig, "The selected Data Block has no saved record ID.");
        }

        // Build the internal SuiteCRM URL
        global $sugar_config;
        $siteUrl = rtrim($sugar_config['site_url'] ?? '', '/');
        $redirectTo = "{$siteUrl}/index.php?module=" . urlencode($beanRef->moduleName) . 
                      "&action=" . urlencode($view) . 
                      "&record=" . urlencode($beanRef->beanId);

        $result = new ActionResult(ResultStatus::OK, $actionConfig, "Redirecting to CRM record: {$redirectTo}");
        $result->setData(['redirect_url' => $redirectTo]);

        return $result;
    }

    /**
     * Performs the final browser redirection (Terminal phase).
     */
    public function performTerminal(ExecutionContext $context, ActionResult $executionResult): void {
        $data = $executionResult->getData();
        $url = $data['redirect_url'] ?? 'index.php';

        while (ob_get_level()) {
            ob_end_clean();
        }

        header("Location: " . $url);
        exit;
    }
}