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

/*********************************************************************************

 * Description:  Base Form For Notes
 * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
 * All Rights Reserved.
 * Contributor(s): ______________________________________..
 ********************************************************************************/


#[\AllowDynamicProperties]
class ProspectListFormBase
{
    public function getForm($prefix, $mod='', $form='')
    {
        if (!ACLController::checkAccess('ProspectLists', 'edit', true)) {
            return '';
        }

        if (!empty($mod)) {
            global $current_language;
            $mod_strings = return_module_language($current_language, $mod);
        } else {
            global $mod_strings;
        }
        global $app_strings,$current_user;

        $lbl_save_button_title = $app_strings['LBL_SAVE_BUTTON_TITLE'];
        $lbl_save_button_key = $app_strings['LBL_SAVE_BUTTON_KEY'];
        $lbl_save_button_label = $app_strings['LBL_SAVE_BUTTON_LABEL'];
        $user_id = $current_user->id;


        $the_form = get_left_form_header($mod_strings['LBL_NEW_FORM_TITLE']);
        $the_form .= <<<EOQ
		<form name="{$prefix}ProspectListSave" onSubmit="return check_form('{$prefix}ProspectListSave');" method="POST" action="index.php">
			<input type="hidden" name="{$prefix}module" value="ProspectLists">
			<input type="hidden" name="{$prefix}action" value="Save">
			<input type="hidden" name="assigned_user_id" value='{$user_id}'>
EOQ;

        $the_form .= $this->getFormBody($prefix, $mod, $prefix."ProspectListSave");
        $the_form .= <<<EOQ
		<p><input title="$lbl_save_button_title" accessKey="$lbl_save_button_key" class="button" type="submit" name="button" value="  $lbl_save_button_label  " ></p>
		</form>

EOQ;

        $the_form .= get_left_form_footer();
        $the_form .= get_validate_record_js();

        return $the_form;
    }

    public function getFormBody($prefix, $mod='', $formname='', $size='30', $script=true)
    {
        if (!ACLController::checkAccess('ProspectLists', 'edit', true)) {
            return '';
        }
        global $mod_strings;
        $temp_strings = $mod_strings;
        if (!empty($mod)) {
            global $current_language;
            $mod_strings = return_module_language($current_language, $mod);
        }
        global $app_strings;
        global $current_user;
        global $app_list_strings;

        $lbl_required_symbol = $app_strings['LBL_REQUIRED_SYMBOL'];
        $lbl_save_button_title = $app_strings['LBL_SAVE_BUTTON_TITLE'];
        $lbl_save_button_key = $app_strings['LBL_SAVE_BUTTON_KEY'];
        $lbl_save_button_label = $app_strings['LBL_SAVE_BUTTON_LABEL'];
        $user_id = $current_user->id;

        $list_options=get_select_options_with_id($app_list_strings['prospect_list_type_dom'], 'default');

        $lbl_prospect_list_name = $mod_strings['LBL_PROSPECT_LIST_NAME'];
        $lbl_list_type = $mod_strings['LBL_LIST_TYPE'];

        $form = <<<EOQ
			<p><input type="hidden" name="record" value="">
			$lbl_prospect_list_name&nbsp;<span class="required">$lbl_required_symbol</span><br>
			<input name='name' type="text" value=""><br>
			$lbl_list_type&nbsp;<span class="required">$lbl_required_symbol</span><br>
			<select name="list_type">$list_options</select></p>
EOQ;



        $javascript = new javascript();
        $javascript->setFormName($formname);
        $javascript->setSugarBean(BeanFactory::newBean('ProspectLists'));
        $javascript->addRequiredFields($prefix);
        $form .=$javascript->getScript();
        $mod_strings = $temp_strings;
        return $form;
    }

    public function handleSave($prefix, $redirect=true, $useRequired=false)
    {
        require_once('include/formbase.php');


        $focus = BeanFactory::newBean('ProspectLists');
        if ($useRequired &&  !checkRequired($prefix, array_keys($focus->required_fields))) {
            return null;
        }
        $focus = populateFromPost($prefix, $focus);
        if (!$focus->ACLAccess('Save')) {
            ACLController::displayNoAccess(true);
            sugar_cleanup(true);
        }
        if (empty($focus->name)) {
            return null;
        }
        if (!isset($focus->assigned_user_id) || $focus->assigned_user_id == '') {
            $focus->assigned_user_id = $GLOBALS['current_user']->id;
        }

        $return_id = $focus->save();
        if ($redirect) {
            $GLOBALS['log']->debug("Saved record with id of ".$return_id);
            handleRedirect($return_id, "ProspectLists");
        } else {
            return $focus;
        }
    }
}
