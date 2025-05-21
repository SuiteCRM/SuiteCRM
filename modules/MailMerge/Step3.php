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


require_once('include/JSON.php');
require_once('modules/MailMerge/modules_array.php');
require_once('modules/MailMerge/merge_query.php');

global $app_strings;
global $app_list_strings;
global $mod_strings;
global $current_user;
global $odd_bg;
global $even_bg;
global $sugar_version, $sugar_config;
global $locale;

$xtpl = new XTemplate('modules/MailMerge/Step3.html');
$xtpl->assign("MOD", $mod_strings);
$xtpl->assign("APP", $app_strings);

if (!isset($_SESSION['MAILMERGE_MODULE'])) {
    if (isset($_POST['mailmerge_module'])) {
        $_SESSION['MAILMERGE_MODULE'] = $_POST['mailmerge_module'];
    }
}

if (isset($_POST['contains_contact_info'])) {
    $_SESSION['MAILMERGE_CONTAINS_CONTACT_INFO'] = $_POST['contains_contact_info'];
}

if (!isset($_SESSION["MAILMERGE_DOCUMENT_ID"])) {
    if (!empty($_POST['document_id'])) {
        $_SESSION['MAILMERGE_DOCUMENT_ID'] = $_POST['document_id'];
    }
}
$document_id = $_SESSION["MAILMERGE_DOCUMENT_ID"];
$document = BeanFactory::newBean('Documents');
$document->retrieve($document_id);
$_SESSION["MAILMERGE_TEMPLATE"] = $document->document_name;

if (!empty($_POST['selected_objects'])) {
    $selObjs = urldecode($_POST['selected_objects']);
    $_SESSION['SELECTED_OBJECTS_DEF'] = $selObjs;
} else {
    $selObjs = $_SESSION['SELECTED_OBJECTS_DEF'];
}
$sel_obj = array();
parse_str(html_entity_decode((string) $selObjs, ENT_QUOTES), $sel_obj);
$step_num = 3;
$xtpl->assign("PREV_STEP", '2');
$xtpl->assign("STEP_NUM", "Step 3:");
$popup_request_data = array('call_back_function' => 'set_return', 'form_name' => 'EditView', 'field_to_name_array' => array('id' => 'rel_id', 'name' => 'rel_name',),);
    $json = getJSONobj();

    // must urlencode to put into the filter request string
    // because IE gets an out of memory error when it is passed
    // as the usual object literal
$encoded_popup_request_data = urlencode($json->encode($popup_request_data));

$modules = $modules_array;


$xtpl->assign("MAILMERGE_MODULE_OPTIONS", get_select_options_with_id($modules, '0'));
$change_parent_button = "<input title='".$app_strings['LBL_SELECT_BUTTON_TITLE']."' tabindex='2' type='button' class='button' value='".$app_strings['LBL_SELECT_BUTTON_LABEL']."' name='button' onclick='open_popup(document.EditView.rel_type.value, 600, 400, \"&request_data=$encoded_popup_request_data\", true, false, {});' />";

$change_parent_button = "<input title='".$app_strings['LBL_SELECT_BUTTON_TITLE']."' tabindex='2' type='button' class='button' value='".$app_strings['LBL_SELECT_BUTTON_LABEL']."' name='button' onclick='open_popup(document.EditView.parent_type.value, 600, 400, \"&request_data=$encoded_popup_request_data\", true, false, {});' />";
$xtpl->assign("CHANGE_PARENT_BUTTON", $change_parent_button);

$relModule = $_SESSION['MAILMERGE_CONTAINS_CONTACT_INFO'];
$xtpl->assign("STEP3_HEADER", "Set ".get_singular_bean_name($relModule)." Association");


$select = "Select id, name from contacts";
global $beanList, $beanFiles;
$class_name = $beanList[$relModule];
require_once($beanFiles[$class_name]);
$seed = new $class_name();

if (isset($_SESSION['MAILMERGE_SKIP_REL']) && $_SESSION['MAILMERGE_SKIP_REL']) {
    $disabled = 'disabled';
} else {
    $disabled = '';
}
$oddRow = true;


foreach ($sel_obj as $key => $value) {
    $value = str_replace("##", "&", $value);
    $value = stripslashes($value);
    $code = md5($key);
    $popup_request_data = array('call_back_function' => 'set_return', 'form_name' => 'EditView', 'field_to_name_array' => array('id' => 'rel_id_'.$code, 'name' => 'rel_name_'.$code,),);
    $encoded_popup_request_data = urlencode($json->encode($popup_request_data));

    $fullQuery = get_merge_query($seed, $_SESSION['MAILMERGE_MODULE'], $key);
    $result = $seed->db->limitQuery($fullQuery, 0, 1, true, "Error performing limit query");

    $full_name = '';
    $contact_id = '';
    if ($row = $seed->db->fetchByAssoc($result, 0)) {
        if ($relModule == "Contacts") {
            $full_name = $locale->getLocaleFormattedName($row['first_name'], $row['last_name']);
        } else {
            $full_name = $row['name'];
        }
        $contact_id = $row['id'];
    }
    $umodule =urlencode($_SESSION['MAILMERGE_MODULE']);
    $ukey = urlencode($key);
    $change_parent_button = "<input title='{$app_strings['LBL_SELECT_BUTTON_TITLE']}' tabindex='2'  type='button' class='button' value='{$app_strings['LBL_SELECT_BUTTON_LABEL']}'
		name='button' onclick='open_popup(document.EditView.rel_type_{$code}.value, 600, 400,
			\"&html=mail_merge&rel_module=$umodule&id=$ukey&request_data=$encoded_popup_request_data\", true, false, {});' $disabled/>";
    $items = array(
    'ID' => $key,
    'NAME' => $value,
    'CODE' => $code,
    'TYPE_OPTIONS' => get_select_options_with_id($modules, '0'),
    'CHANGE_RELATIONSHIP' => $change_parent_button,
    'CONTACT_ID' => $contact_id,
    'CONTACT_NAME' => $full_name,
    'REL_MODULE' => $_SESSION['MAILMERGE_CONTAINS_CONTACT_INFO'],
    );

    $xtpl->assign("MAILMERGE", $items);

    if ($oddRow) {
        //todo move to themes
        $xtpl->assign("ROW_COLOR", 'oddListRow');
        $xtpl->assign("BG_COLOR", $odd_bg);
    } else {
        //todo move to themes
        $xtpl->assign("ROW_COLOR", 'evenListRow');
        $xtpl->assign("BG_COLOR", $even_bg);
    }
    $oddRow = !$oddRow;
    $xtpl->parse("main.items.row");
}
$xtpl->parse("main.items");


$xtpl->parse("main");
$xtpl->out("main");


function generateSelect($seed, $relModule)
{
    $lowerRelModule = strtolower($relModule);
    if ($seed->load_relationship($lowerRelModule)) {
        $params = array();
        $params['join_table_alias'] = 'r1';
        $params['join_table_link_alias'] = 'r2';
        $params['join_type'] = 'LEFT JOIN';
        $join = $seed->$lowerRelModule->getJoin($params);
        $select = "SELECT {$seed->table_name}.* FROM {$seed->table_name} ".$join;
        return $select;
    }
    return "";
}
