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

global $app_strings;
global $mod_strings;
global $app_list_strings;
global $current_language;
global $currentModule;
global $theme;

$json = new JSON();

$current_module_strings = return_module_language($current_language, 'MergeRecords');

if (!isset($where)) {
    $where = "";
}

$focus = BeanFactory::newBean('MergeRecords');

////////////////////////////////////////////////////////////
//get instance of master record and retrieve related record
//and items
////////////////////////////////////////////////////////////
$focus->merge_module = $_REQUEST['return_module'];
$focus->load_merge_bean($focus->merge_module, true, $_REQUEST['record']);


//get all available column fields
//TO DO: add custom field handling
$avail_fields=array();
$sel_fields=array();
$temp_field_array = $focus->merge_bean->field_defs;
$bean_data=array();
foreach ($temp_field_array as $field_array) {
    if (isset($field_array['merge_filter'])
    ) {
        if (strtolower($field_array['merge_filter'])=='enabled' || strtolower($field_array['merge_filter'])=='selected') {
            $col_name = $field_array['name'];

                            
            if (!isset($focus->merge_bean_strings[$field_array['vname']])) {
                $col_label = $col_name;
            } else {
                $col_label = str_replace(':', '', (string) $focus->merge_bean_strings[$field_array['vname']]);
            }
            
            if (strtolower($field_array['merge_filter'])=='selected') {
                $sel_fields[$col_name]=$col_label;
            } else {
                $avail_fields[$col_name] = $col_label;
            }
            
            $bean_data[$col_name]=$focus->merge_bean->$col_name;
        }
    }
}

/////////////////////////////////////////////////////////

//Print the master record header to the page
$params = array();
$params[] = "<a href='index.php?module={$focus->merge_bean->module_dir}&action=index'>{$GLOBALS['app_list_strings']['moduleList'][$focus->merge_bean->module_dir]}</a>";
$params[] = $mod_strings['LBL_LBL_MERGE_RECORDS_STEP_1'];
$params[] = $focus->merge_bean->name;
echo getClassicModuleTitle($focus->merge_bean->module_dir, $params, true);

$xtpl = new XTemplate('modules/MergeRecords/Step1.html');
$xtpl->assign("MOD", $mod_strings);
$xtpl->assign("APP", $app_strings);
$xtpl->assign("BEANDATA", $json->encode($bean_data));
//This is for the implemetation of finding all dupes for a module, not just
//dupes for a particular record
//commenting this out for now
//$choose_master_by_options = array('First Record Found', 'Most Recent Record', 'Oldest Record', 'Record Containing Most Data');
//$xtpl->assign("CHOOSE_MASTER_BY_OPTIONS", get_select_options_with_id($choose_master_by_options, 'First Record Found'));

$xtpl->assign("MERGE_MODULE", $focus->merge_module);
$xtpl->assign("ID", $focus->merge_bean->id);

$xtpl->assign("FIELD_AVAIL_OPTIONS", get_select_options_with_id($avail_fields, ''));
$xtpl->assign("LBL_ADD_BUTTON", translate('LBL_ADD_BUTTON'));

if (isset($_REQUEST['return_id'])) {
    $xtpl->assign("RETURN_ID", validate_input($_REQUEST['return_id']));
}

$xtpl->assign("RETURN_ACTION", validate_input($_REQUEST['return_action']));
$xtpl->assign("RETURN_MODULE", validate_input($_REQUEST['return_module']));

//set the url
$port=null;
if (isset($_SERVER['SERVER_PORT']) && $_SERVER['SERVER_PORT'] != 80 && $_SERVER['SERVER_PORT'] != 443) {
    $port = $_SERVER['SERVER_PORT'];
}
$xtpl->assign("URL", appendPortToHost($sugar_config['site_url'], $port));
//set images
$xtpl->assign("RIGHTARROW_BIG_IMAGE", SugarThemeRegistry::current()->getImageURL('rightarrow_big.gif'));
$xtpl->assign("DELETE_INLINE_IMAGE", SugarThemeRegistry::current()->getImageURL('delete_inline.gif'));

//process preloaded filter.
$pre_loaded=null;
foreach ($sel_fields as $colName=>$colLabel) {
    $pre_loaded.=addFieldRow($colName, $colLabel, $bean_data[$colName]);
}
$xtpl->assign("PRE_LOADED_FIELDS", $pre_loaded);
$xtpl->assign("OPERATOR_OPTIONS", $json->encode($app_list_strings['merge_operators_dom']));


$xtpl->parse("main.field_select_block");

$xtpl->parse("main");
$xtpl->out("main");


/**
 * @param string $requestData
 * @return string
 */
function validate_input($requestData)
{
    return htmlspecialchars((string) remove_xss($requestData), ENT_QUOTES | ENT_HTML5);
}

/**
 * This function is equivalent of AddFieldRow in merge.js. is being used to
 * preload the filter criteria based on the vardef.
 * <span><table><tr><td></td><td></td><td></td></tr></table></span>
 */
function addFieldRow($colName, $colLabel, $colValue)
{
    global $theme, $app_list_strings;
    
    static $operator_options;
    if (empty($operator_options)) {
        $operator_options= get_select_options_with_id($app_list_strings['merge_operators_dom'], '');
    }

    $LBL_REMOVE = translate('LBL_REMOVE');
    $deleteInlineImage = SugarThemeRegistry::current()->getImageURL('delete_inline.gif');
    $snippet=<<<EOQ
    <span id=filter_{$colName} style='visibility:visible' value="{$colLabel}" valueId="{$colName}">
        <table width='100%' border='0' cellpadding='0'>
            <tr>
                <td width='2%'><a class="listViewTdToolsS1" href="javascript:remove_filter('filter_{$colName}')"><!--not_in_theme!--><img src='{$deleteInlineImage}' align='absmiddle' alt='{$LBL_REMOVE}' border='0' height='12' width='12'>&nbsp;</a></td>
                <td width='20%'>{$colLabel}:&nbsp;</td>
                <td width='10%'><select name='{$colName}SearchType'>{$operator_options}</select></td>
                <td width='68%'><input value="{$colValue}" id="{$colName}SearchField" name="{$colName}SearchField" type="text"></td>                  
            </tr> 
        </table>
    </span>
EOQ;

    return $snippet;
}
