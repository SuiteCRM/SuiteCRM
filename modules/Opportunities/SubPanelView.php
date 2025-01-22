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







global $app_strings;
//we don't want the parent module's string file, but rather the string file specifc to this subpanel
global $current_language;
$current_module_strings = return_module_language($current_language, 'Opportunities');

global $currentModule;

global $theme;
global $focus;
global $action;




// focus_list is the means of passing data to a SubPanelView.
global $focus_list;

$button  = "<form action='index.php' method='post' name='form' id='form'>\n";
$button .= "<input type='hidden' name='module' value='Opportunities'>\n";
if ($currentModule == 'Accounts') {
    $button .= "<input type='hidden' name='account_id' value='$focus->id'>\n<input type='hidden' name='account_name' value=\"".urlencode($focus->name)."\">\n";
}
if ($currentModule == 'Contacts') {
    $button .= "<input type='hidden' name='account_id' value='$focus->account_id'>\n<input type='hidden' name='account_name' value=\"".urlencode($focus->account_name)."\">\n";
    $button .= "<input type='hidden' name='contact_id' value='$focus->id'>\n";
}
$button .= "<input type='hidden' name='return_module' value='".$currentModule."'>\n";
$button .= "<input type='hidden' name='return_action' value='".$action."'>\n";
$button .= "<input type='hidden' name='return_id' value='".$focus->id."'>\n";
$button .= "<input type='hidden' name='action'>\n";

$button .= "<input title='".$app_strings['LBL_NEW_BUTTON_TITLE']."' accessyKey='".$app_strings['LBL_NEW_BUTTON_KEY']."' class='button' onclick=\"this.form.action.value='EditView'\" type='submit' name='New' value='  ".$app_strings['LBL_NEW_BUTTON_LABEL']."  '>\n";
if ($currentModule == 'Accounts') {
    ///////////////////////////////////////
    ///
    /// SETUP PARENT POPUP

    $popup_request_data = array(
        'call_back_function' => 'set_return_and_save',
        'form_name' => 'DetailView',
        'field_to_name_array' => array(
            'id' => 'opportunity_id',
            ),
        );

    $json = getJSONobj();
    $encoded_popup_request_data = $json->encode($popup_request_data);

    //
    ///////////////////////////////////////

    $button .= "<input title='".$app_strings['LBL_SELECT_BUTTON_TITLE']
        ."' accessyKey='".$app_strings['LBL_SELECT_BUTTON_KEY']
        ."' type='button' class='button' value='  ".$app_strings['LBL_SELECT_BUTTON_LABEL']
        ."  ' name='button' onclick='open_popup(\"Opportunities\", 600, 400, \"\", false, true, {$encoded_popup_request_data});'>\n";
//		."  ' name='button' onclick='window.open(\"index.php?module=Opportunities&action=Popup&html=Popup_picker&form=DetailView&form_submit=true\",\"new\",\"width=600,height=400,resizable=1,scrollbars=1\");'>\n";
} else {
    ///////////////////////////////////////
    ///
    /// SETUP PARENT POPUP

    $popup_request_data = array(
        'call_back_function' => 'set_return_and_save',
        'form_name' => 'DetailView',
        'field_to_name_array' => array(
            'id' => 'opportunity_id',
            ),
        );

    $json = getJSONobj();
    $encoded_popup_request_data = $json->encode($popup_request_data);

    //
    ///////////////////////////////////////

    $button .= "<input title='".$app_strings['LBL_SELECT_BUTTON_TITLE']
        ."' accessyKey='".$app_strings['LBL_SELECT_BUTTON_KEY']
        ."' type='button' class='button' value='  ".$app_strings['LBL_SELECT_BUTTON_LABEL']
        ."  ' name='button' onclick='open_popup(\"Opportunities\", 600, 400, \"\", false, true, {$encoded_popup_request_data});'>\n";
    //		."  ' name='button' onclick='window.open(\"index.php?module=Opportunities&action=Popup&html=Popup_picker&form=ContactDetailView&form_submit=true&query=true&account_id=$focus->account_id&account_name=$focus->account_name\",\"new\",\"width=600,height=400,resizable=1,scrollbars=1\");'>\n";
}
$button .= "</form>\n";
$header_text = '';
if (is_admin($current_user) && $_REQUEST['module'] != 'DynamicLayout' && !empty($_SESSION['editinplace'])) {
    $header_text = "&nbsp;<a href='index.php?action=index&module=DynamicLayout&from_action=SubPanelView&from_module=Opportunities&record=". $_REQUEST['record']."'>".SugarThemeRegistry::current()->getImage("EditLayout", "border='0' align='bottom'", null, null, '.gif', $mod_strings['LBL_EDITLAYOUT'])."</a>";
}
$ListView = new ListView();
$ListView->initNewXTemplate('modules/Opportunities/SubPanelView.html', $current_module_strings);
$ListView->xTemplateAssign("RETURN_URL", "&return_module=".$currentModule."&return_action=DetailView&return_id={$_REQUEST['record']}");
$ListView->xTemplateAssign("EDIT_INLINE_PNG", SugarThemeRegistry::current()->getImage('edit_inline', 'align="absmiddle" border="0"', null, null, '.gif', $app_strings['LNK_EDIT']));
$ListView->setHeaderTitle($current_module_strings['LBL_MODULE_NAME']. $header_text);
$ListView->setHeaderText($button);
$ListView->processListView($focus_list, "main", "OPPORTUNITY");
