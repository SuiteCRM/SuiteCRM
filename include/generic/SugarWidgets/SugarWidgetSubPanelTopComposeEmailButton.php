<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2019 SalesAgility Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUGARCRM, SUGARCRM DISCLAIMS THE WARRANTY
 * OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
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
 * You can contact SugarCRM, Inc. headquarters at 10050 North Wolfe Road,
 * SW2-130, Cupertino, CA 95014, USA. or at email address contact@sugarcrm.com.
 *
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo and "Supercharged by SuiteCRM" logo. If the display of the logos is not
 * reasonably feasible for technical reasons, the Appropriate Legal Notices must
 * display the words "Powered by SugarCRM" and "Supercharged by SuiteCRM".
 */
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

#[\AllowDynamicProperties]
class SugarWidgetSubPanelTopComposeEmailButton extends SugarWidgetSubPanelTopButton
{
    public $form_value = '';

    public function getWidgetId($buttonSuffix = true)
    {
        global $app_strings;
        $this->form_value = $app_strings['LBL_COMPOSE_EMAIL_BUTTON_LABEL'];
        return parent::getWidgetId();
    }

    public function &_get_form($defines, $additionalFormFields = null, $nonbutton = false)
    {
        if ((ACLController::moduleSupportsACL($defines['module']) && !ACLController::checkAccess($defines['module'], 'edit', true) ||
                $defines['module'] == "Activities" & !ACLController::checkAccess("Emails", 'edit', true))) {
            $temp = '';
            return $temp;
        }

        global $app_strings, $current_user;
        $title = $app_strings['LBL_COMPOSE_EMAIL_BUTTON_TITLE'];
        $value = $app_strings['LBL_COMPOSE_EMAIL_BUTTON_LABEL'];

        //martin Bug 19660
        $client = $current_user->getEmailClient();

        /** @var Person|Company|Opportunity $bean */
        $bean = $defines['focus'];

        if ($client != 'sugar') {
            // awu: Not all beans have emailAddress property, we must account for this
            if (isset($bean->emailAddress)) {
                $to_addrs = $bean->emailAddress->getPrimaryAddress($bean);
                $button = "<input class='button' type='button' value='$value' id='" . $this->getWidgetId() . "' name='" . preg_replace('[ ]', '', (string) $value) . "' title='$title' onclick=\"location.href='mailto:$to_addrs';return false;\"/>";
            } else {
                $button = "<input class='button' type='button' value='$value' id='" . $this->getWidgetId() . "' name='" . preg_replace('[ ]', '', (string) $value) . "' title='$title' onclick=\"location.href='mailto:';return false;\"/>";
            }
        } else {
            // Generate the compose package for the quick create options.
            require_once 'modules/Emails/EmailUI.php';


            // Opportunities does not have an email1 field
            // we need to use the related account email instead
            if ($bean->module_name === 'Opportunities') {
                $relatedAccountId = $bean->account_id;
                /** @var Account $relatedAccountBean */
                $relatedAccountBean = BeanFactory::getBean('Accounts', $relatedAccountId);
                if (!empty($relatedAccountBean) && !empty($relatedAccountBean->email1)) {
                    $bean->email1 = $relatedAccountBean->email1;
                    $bean->name = $relatedAccountBean->name;
                }
            }

            if (empty($bean->email1)) {
                $bean->email1 = '';
            }

            $emailUI = new EmailUI();
            $emailUI->appendTick = false;
            $button = '<div type="hidden" onclick="currentModule=\''
              . $bean->module_name . '\';$(document).openComposeViewModal(this);" data-module="'
              . $bean->module_name . '" data-record-id="'
              . $bean->id . '" data-module-name="'
              . $bean->name .'" data-email-address="'
              . $bean->email1 .'">';
        }

        return $button;
    }

    public function display($defines, $additionalFormFields = null, $nonbutton = false)
    {
        $focus = new Meeting;
        if (!$focus->ACLAccess('EditView')) {
            return '';
        }
        
        $inputID = $this->getWidgetId();

        $button = $this->_get_form($defines, $additionalFormFields);

        global $current_user;
        $client = $current_user->getEmailClient();

        if ($client == 'sugar') {
            $button .= "<input class='button' onclick='return false;' type='button' id='$inputID' value='$this->form_value'>";
        }

        return $button;
    }
}
