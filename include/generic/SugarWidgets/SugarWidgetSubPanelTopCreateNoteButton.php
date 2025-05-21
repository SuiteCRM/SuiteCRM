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






class SugarWidgetSubPanelTopCreateNoteButton extends SugarWidgetSubPanelTopButtonQuickCreate
{
    public $subpanelDiv;
    public function &_get_form($defines, $additionalFormFields = null, $asUrl = false)
    {
        global $app_strings;
        global $currentModule;

        $this->module="Notes";
        $this->subpanelDiv = "history";

        // Create the additional form fields with real values if they were not passed in
        if (empty($additionalFormFields) && $this->additional_form_fields) {
            foreach ($this->additional_form_fields as $key=>$value) {
                if (!empty($defines['focus']->$value)) {
                    $additionalFormFields[$key] = $defines['focus']->$value;
                } else {
                    $additionalFormFields[$key] = '';
                }
            }
        }

        if (!empty($this->module)) {
            $defines['child_module_name'] = $this->module;
        } else {
            $defines['child_module_name'] = $defines['module'];
        }

        if (!empty($this->subpanelDiv)) {
            $defines['subpanelDiv'] = $this->subpanelDiv;
        }

        $defines['parent_bean_name'] = get_class($defines['focus']);

        $form = 'form' . $defines['child_module_name'];
        $button = '<form onsubmit="return SUGAR.subpanelUtils.sendAndRetrieve(this.id, \'subpanel_' . strtolower($defines['subpanelDiv']) . '\', \'' . addslashes($app_strings['LBL_LOADING']) . '\');" action="index.php" method="post" name="form" id="form' . $form . "\">\n";

        //module_button is used to override the value of module name
        $button .= "<input type='hidden' name='target_module' value='".$defines['child_module_name']."'>\n";
        $button .= "<input type='hidden' name='".strtolower($defines['parent_bean_name'])."_id' value='".$defines['focus']->id."'>\n";

        if (isset($defines['focus']->name)) {
            $button .= "<input type='hidden' name='".strtolower($defines['parent_bean_name'])."_name' value='".$defines['focus']->name."'>";
        }

        $button .= '<input type="hidden" name="to_pdf" value="true" />';
        $button .= '<input type="hidden" name="tpl" value="QuickCreate.tpl" />';
        $button .= '<input type="hidden" name="return_module" value="' . $currentModule . "\" />\n";
        $button .= '<input type="hidden" name="return_action" value="' . $defines['action'] . "\" />\n";
        $button .= '<input type="hidden" name="return_id" value="' . $defines['focus']->id . "\" />\n";
        $button .= '<input type="hidden" name="record" value="" />';

        // TODO: move this out and get $additionalFormFields working properly
        if (empty($additionalFormFields['parent_type'])) {
            if ($defines['focus']->object_name=='Contact') {
                $additionalFormFields['parent_type'] = 'Accounts';
            } else {
                $additionalFormFields['parent_type'] = $defines['focus']->module_dir;
            }
        }
        if (empty($additionalFormFields['parent_name'])) {
            if ($defines['focus']->object_name=='Contact') {
                $additionalFormFields['parent_name'] = $defines['focus']->account_name;
                $additionalFormFields['account_name'] = $defines['focus']->account_name;
            } else {
                $additionalFormFields['parent_name'] = $defines['focus']->name;
            }
        }
        if (empty($additionalFormFields['parent_id'])) {
            if ($defines['focus']->object_name=='Contact') {
                $additionalFormFields['parent_id'] = $defines['focus']->account_id;
                $additionalFormFields['account_id'] = $defines['focus']->account_id;
            } else {
                $additionalFormFields['parent_id'] = $defines['focus']->id;
            }
        }

        $button .= '<input type="hidden" name="action" value="SubpanelCreates" />' . "\n";
        $button .= '<input type="hidden" name="module" value="Home" />' . "\n";
        $button .= '<input type="hidden" name="target_action" value="QuickCreate" />' . "\n";

        // fill in additional form fields for all but action
        foreach ($additionalFormFields as $key => $value) {
            if ($key != 'action') {
                $button .= '<input type="hidden" name="' . $key . '" value="' . $value . '" />' . "\n";
            }
        }

        return $button;
    }


    public function display($defines, $additionalFormFields = null, $nonbutton = false)
    {
        $focus = new Note;
        if (!$focus->ACLAccess('EditView')) {
            return '';
        }

        return parent::display($defines, $additionalFormFields);
    }
}
