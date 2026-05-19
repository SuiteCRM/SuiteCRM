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

/**
 * This custom class, allows you to create a button in each record of the subpanel
 * which open the QuickCreate View to edit the record.
 */

require_once('modules/stic_Messages/Utils.php');

class SugarWidgetSubPanelEditMessagesButton extends SugarWidgetSubPanelTopButtonQuickCreate
{

    /**
     * Function to get the label for the button based on the module, if the module is stic_Conversations it will return a different label than for the rest of modules
      * @param string $moduleName The name of the module to get the label
      * @return string The label for the button
     */
    protected function getButtonLabelByModule($moduleName)
    {
        global $app_strings;

        if ($moduleName === 'stic_Conversations' && !empty($app_strings['LBL_SUBPANEL_NEW_MESSAGE_CONVERSATION_LABEL'])) {
            return $app_strings['LBL_SUBPANEL_NEW_MESSAGE_CONVERSATION_LABEL'];
        }

        return $app_strings['LBL_SUBPANEL_NEW_MESSAGE_LABEL'];
    }

    public function getWidgetId($buttonSuffix = true)
    {
        global $focus;

        // Get the module name to determinate the label for the button
        $moduleName = !empty($focus->module_name) ? $focus->module_name : '';
        $this->form_value = $this->getButtonLabelByModule($moduleName);
        return parent::getWidgetId();
    }


    public function &_get_form($defines, $additionalFormFields = null, $asUrl = false)
    {
        global $app_strings;

        $bean = $defines['focus'];

        // Get the module name to determinate the label for the button
        $button = $this->getButtonLabelByModule($bean->module_name ?? '');

        $phone = '';
        // Get the phone number to send the message, only if the module of the record has a phone field configured for messages
        $messageableModules = stic_MessagesUtils::getMessageableModules();
        if (!empty($bean->module_name) && in_array($bean->module_name, $messageableModules, true)) {
            $phone = stic_MessagesUtils::getPhoneForMessage($bean);
        }

        $jsonData = json_encode([
            'return_action' => 'DetailView',
        ]);
        $jsonData = str_replace("'", "\\'", $jsonData);
        $form = "<input type='button' name='button' id='custom_modal_button' class='button' 
            title='{$button}' value='{$button}'
            onclick='openMessagesModal(this); return false;'
                 data-phone='{$phone}'
                 data-module='{$bean->module_name}'
                 data-record-id='{$bean->id}'
                 data-name='{$bean->name}'
                 >";

        return $form;
    }

    public function display($defines, $additionalFormFields = null, $nonbutton = false)
    {
        if (ACLController::checkAccess('stic_Messages', 'edit', true)) {
            require_once 'modules/MySettings/TabController.php';
            $controller = new TabController();
            $currentTabs = $controller->get_system_tabs();
    
            if (!isset($currentTabs['stic_Messages']) || !$currentTabs['stic_Messages']){
                return '';
            }
    
            $button = $this->_get_form($defines, $additionalFormFields);
            
            return $button;
        }
        return '';
    }
}
