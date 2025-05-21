<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
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






#[\AllowDynamicProperties]
class SugarWidgetSubPanelTopArchiveEmailButton extends SugarWidgetSubPanelTopButton
{
    public function display($defines, $additionalFormFields = null, $nonbutton = false)
    {
        if ((ACLController::moduleSupportsACL($defines['module'])  && !ACLController::checkAccess($defines['module'], 'edit', true) ||
            $defines['module'] == "History" & !ACLController::checkAccess("Emails", 'edit', true))) {
            $temp = '';
            return $temp;
        }
        
        global $app_strings;
        global $mod_strings;
        global $currentModule;

        $title = $app_strings['LBL_TRACK_EMAIL_BUTTON_TITLE'];
        $value = $app_strings['LBL_TRACK_EMAIL_BUTTON_LABEL'];
        $this->module = 'Emails';

        $additionalFormFields = array();
        $additionalFormFields['type'] = 'archived';
        // cn: bug 5727 - must override the parents' parent for contacts (which could be an Account)
        $additionalFormFields['parent_type'] = $defines['focus']->module_dir;
        $additionalFormFields['parent_id'] = $defines['focus']->id;
        $additionalFormFields['parent_name'] = $defines['focus']->name;

        if (isset($defines['focus']->email1)) {
            $additionalFormFields['to_email_addrs'] = $defines['focus']->email1;
        }
        if (ACLController::moduleSupportsACL($defines['module'])  && !ACLController::checkAccess($defines['module'], 'edit', true)) {
            $button = "<input id='".preg_replace('[ ]', '', (string) $value)."_button'  title='$title' class='button' type='button' name='".preg_replace('[ ]', '', mb_strtolower($value, 'UTF-8'))."_button' value='$value' disabled/>\n";
            return $button;
        }
        $button = $this->_get_form($defines, $additionalFormFields);
        $button .= "<input id='".preg_replace('[ ]', '', (string) $value)."_button' title='$title' class='button' type='submit' name='".preg_replace('[ ]', '', mb_strtolower($value, 'UTF-8'))."_button' value='$value'/>\n";
        $button .= "</form>";
        return $button;
    }
}
