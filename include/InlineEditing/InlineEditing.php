<?php
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

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

function getEditFieldHTML($module, $fieldname, $aow_field, $view = 'EditView', $id = '', $alt_type = '', $currency_id = '')
{
    $focus = null;
    $vardefFields = [];
    $parentfieldlist = [];
    $displayParams = [];
    $rel_value = '';

    global $current_language, $app_strings, $app_list_strings, $current_user, $beanFiles, $beanList;

    $bean = BeanFactory::getBean($module, $id);

    if (!checkAccess($bean)) {
        return false;
    }

    $vardef = $bean->getFieldDefinition($fieldname);

    if (isTrue($vardef['sensitive'] ?? false) || isFalse($vardef['api-visible'] ?? true)){
        return false;
    }

    $value = getFieldValueFromModule($fieldname, $module, $id);
    // use the mod_strings for this module
    $mod_strings = return_module_language($current_language, $module);

    // set the filename for this control
    $file = create_cache_directory('include/InlineEditing/') . $module . $view . $alt_type . $fieldname . '.tpl';

    if (!is_file($file)
        || inDeveloperMode()
        || !empty($_SESSION['developerMode'])
    ) {
        if (!isset($vardef)) {
            require_once($beanFiles[$beanList[$module]]);
            $focus = new $beanList[$module];
            $vardef = $focus->getFieldDefinition($fieldname);
        }

        $displayParams = array();
        //$displayParams['formName'] = 'EditView';

        // if this is the id relation field, then don't have a pop-up selector.
        if ($vardef['type'] == 'relate' && $vardef['id_name'] == $vardef['name']) {
            $vardef['type'] = 'varchar';
        }

        if (isset($vardef['precision'])) {
            unset($vardef['precision']);
        }

        //$vardef['precision'] = $locale->getPrecedentPreference('default_currency_significant_digits', $current_user);

        //TODO Fix datetimecomebo
        //temp work around
        if ($vardef['type'] == 'datetime') {
            $vardef['type'] = 'datetimecombo';
        }

        // trim down textbox display
        if ($vardef['type'] == 'text') {
            $vardef['rows'] = 2;
            $vardef['cols'] = 32;
        }

        // create the dropdowns for the parent type fields
        if ($vardef['type'] == 'parent_type') {
            $vardef['type'] = 'enum';
        }

        if ($vardef['type'] == 'link') {
            $vardef['type'] = 'relate';
            $vardef['rname'] = 'name';
            $vardef['id_name'] = $vardef['name'] . '_id';
            if ((!isset($vardef['module']) || $vardef['module'] == '') && $focus->load_relationship($vardef['name'])) {
                $vardef['module'] = $focus->{$vardef['name']}->getRelatedModuleName();
            }
        }

        //check for $alt_type
        if ($alt_type != '') {
            $vardef['type'] = $alt_type;
        }

        // remove the special text entry field function 'getEmailAddressWidget'
        if (isset($vardef['function'])
            && ($vardef['function'] == 'getEmailAddressWidget'
                || $vardef['function']['name'] == 'getEmailAddressWidget')
        ) {
            unset($vardef['function']);
        }

        if (isset($vardef['name']) && ($vardef['name'] == 'date_modified')) {
            $vardef['name'] = 'aow_temp_date';
        }

        if (isset($vardef['help'])) {
            $vardef['help'] = htmlspecialchars((string) $vardef['help'],ENT_QUOTES);
        }

        // load SugarFieldHandler to render the field tpl file
        static $sfh;

        if (!isset($sfh)) {
            require_once('include/SugarFields/SugarFieldHandler.php');
            $sfh = new SugarFieldHandler();
        }

        $contents = $sfh->displaySmarty('fields', $vardef, $view, $displayParams);

        // Remove all the copyright comments
        $contents = preg_replace('/\{\*[^\}]*?\*\}/', '', (string) $contents);
        // remove extra wrong javascript which breaks auto complete on flexi relationship parent fields
        $contents = preg_replace("/<script language=\"javascript\">if\(typeof sqs_objects == \'undefined\'\){var sqs_objects = new Array;}sqs_objects\[\'EditView_parent_name\'\].*?<\/script>/", "", $contents);


        if ($view == 'EditView' && ($vardef['type'] == 'relate' || $vardef['type'] == 'parent')) {
            $contents = str_replace('"' . $vardef['id_name'] . '"', '{/literal}"{$fields.' . $vardef['name'] . '.id_name}"{literal}', $contents);
            $contents = str_replace('"' . $vardef['name'] . '"', '{/literal}"{$fields.' . $vardef['name'] . '.name}"{literal}', $contents);
            // regex below fixes button javascript for flexi relationship
            if ($vardef['type'] == 'parent') {
                $contents = str_replace("onclick='open_popup(document.{\$form_name}.parent_type.value, 600, 400, \"\", true, false, {literal}{\"call_back_function\":\"set_return\",\"form_name\":\"EditView\",\"field_to_name_array\":{\"id\":{/literal}\"{\$fields.parent_name.id_name}", "onclick='open_popup(document.{\$form_name}.parent_type.value, 600, 400, \"\", true, false, {literal}{\"call_back_function\":\"set_return\",\"form_name\":\"EditView\",\"field_to_name_array\":{\"id\":{/literal}\"parent_id", $contents);
            }
        }

        // hack to disable one of the js calls in this control
        if (isset($vardef['function']) && ($vardef['function'] == 'getCurrencyDropDown' || $vardef['function']['name'] == 'getCurrencyDropDown')) {
            $contents .= "{literal}<script>function CurrencyConvertAll() { return; }</script>{/literal}";
        }



        // Save it to the cache file
        if ($fh = @sugar_fopen($file, 'w')) {
            fwrite($fh, $contents);
            fclose($fh);
        }
    }

    // Now render the template we received
    $ss = new Sugar_Smarty();

    // Create Smarty variables for the Calendar picker widget
    global $timedate;
    $time_format = $timedate->get_user_time_format();
    $date_format = $timedate->get_cal_date_format();
    $ss->assign('USER_DATEFORMAT', $timedate->get_user_date_format());
    $ss->assign('TIME_FORMAT', $time_format);
    $time_separator = ":";
    $match = array();
    if (preg_match('/\d+([^\d])\d+([^\d]*)/s', (string) $time_format, $match)) {
        $time_separator = $match[1];
    }
    $t23 = strpos((string) $time_format, '23') !== false ? '%H' : '%I';
    if (!isset($match[2]) || $match[2] == '') {
        $ss->assign('CALENDAR_FORMAT', $date_format . ' ' . $t23 . $time_separator . "%M");
    } else {
        $pm = $match[2] == "pm" ? "%P" : "%p";
        $ss->assign('CALENDAR_FORMAT', $date_format . ' ' . $t23 . $time_separator . "%M" . $pm);
    }

    $ss->assign('CALENDAR_FDOW', $current_user->get_first_day_of_week());

    $fieldlist = array();
    if (!isset($focus) || !($focus instanceof SugarBean)) {
        require_once($beanFiles[$beanList[$module]]);
    }
    $focus = new $beanList[$module];
    // create the dropdowns for the parent type fields
    $vardefFields[$fieldname] = $focus->field_defs[$fieldname];
    if ($vardefFields[$fieldname]['type'] == 'parent') {
        $focus->field_defs[$fieldname]['options'] = $focus->field_defs[$vardefFields[$fieldname]['group']]['options'];
    }
    foreach ($vardefFields as $name => $properties) {
        $fieldlist[$name] = $properties;
        // fill in enums
        if (isset($fieldlist[$name]['options']) && is_string($fieldlist[$name]['options']) && isset($app_list_strings[$fieldlist[$name]['options']])) {
            $fieldlist[$name]['options'] = $app_list_strings[$fieldlist[$name]['options']];
        }
        // Bug 32626: fall back on checking the mod_strings if not in the app_list_strings
        elseif (isset($fieldlist[$name]['options']) && is_string($fieldlist[$name]['options']) && isset($mod_strings[$fieldlist[$name]['options']])) {
            $fieldlist[$name]['options'] = $mod_strings[$fieldlist[$name]['options']];
        }
    }

    // fill in function return values
    if (!in_array($fieldname, array('email1', 'email2'))) {
        if (!empty($fieldlist[$fieldname]['function']['returns']) && $fieldlist[$fieldname]['function']['returns'] == 'html') {
            $function = $fieldlist[$fieldname]['function']['name'];
            // include various functions required in the various vardefs
            if (isset($fieldlist[$fieldname]['function']['include']) && is_file($fieldlist[$fieldname]['function']['include'])) {
                require_once($fieldlist[$fieldname]['function']['include']);
            }
            $_REQUEST[$fieldname] = $value;
            $value = $function($focus, $fieldname, $value, $view);

            $value = str_ireplace((string) $fieldname, (string) $aow_field, (string) $value);
        }
    }

    if ($fieldlist[$fieldname]['type'] == 'link') {
        $fieldlist[$fieldname]['id_name'] = $fieldlist[$fieldname]['name'] . '_id';

        if ((!isset($fieldlist[$fieldname]['module']) || $fieldlist[$fieldname]['module'] == '') && $focus->load_relationship($fieldlist[$fieldname]['name'])) {
            $relateField = $fieldlist[$fieldname]['name'];
            $fieldlist[$fieldname]['module'] = $focus->$relateField->getRelatedModuleName();
        }
    }

    if ($fieldlist[$fieldname]['type'] == 'parent') {
        $fieldlist['parent_id']['name'] = 'parent_id';
    }

    if (isset($fieldlist[$fieldname]['name']) && ($fieldlist[$fieldname]['name'] == 'date_modified')) {
        $fieldlist[$fieldname]['name'] = 'aow_temp_date';
        $fieldlist['aow_temp_date'] = $fieldlist[$fieldname];
        $fieldname = 'aow_temp_date';
    }

    if (isset($fieldlist[$fieldname]['id_name']) && $fieldlist[$fieldname]['id_name'] != '' && $fieldlist[$fieldname]['id_name'] != $fieldlist[$fieldname]['name']) {
        $rel_value = '';
        if ($value) {
            $relateIdField = $fieldlist[$fieldname]['id_name'];
            $rel_value =  $bean->$relateIdField;
        }
        $fieldlist[$fieldlist[$fieldname]['id_name']]['value'] = $rel_value;
        $fieldlist[$fieldname]['value'] = $value;
        $fieldlist[$fieldname]['id_name'] = $aow_field;
        $fieldlist[$fieldname]['name'] = $aow_field . '_display';
    } elseif (isset($fieldlist[$fieldname]['type']) && ($fieldlist[$fieldname]['type'] == 'datetimecombo' || $fieldlist[$fieldname]['type'] == 'datetime' || $fieldlist[$fieldname]['type'] == 'date')) {
        $value = $focus->convertField($value, $fieldlist[$fieldname]);
        if (!$value) {
            $value = date($timedate->get_date_time_format());
        }
        $fieldlist[$fieldname]['name'] = $aow_field;
        $fieldlist[$fieldname]['value'] = $value;
    } elseif (isset($fieldlist[$fieldname]['type']) && ($fieldlist[$fieldname]['type'] == 'date')) {
        $value = $focus->convertField($value, $fieldlist[$fieldname]);
        $fieldlist[$fieldname]['name'] = $aow_field;
        if (empty($value)) {
            $value = str_replace("%", "", date($date_format));
        }
        $fieldlist[$fieldname]['value'] = $value;
    } else {
        $fieldlist[$fieldname]['value'] = $value;
        $fieldlist[$fieldname]['name'] = $aow_field;
    }

    if ($fieldlist[$fieldname]['type'] == 'currency' && $view != 'EditView') {

        if (!isset($sfh)) {
            require_once('include/SugarFields/SugarFieldHandler.php');
            $sfh = new SugarFieldHandler();
        }

        if ($currency_id != '' && !stripos((string) $fieldname, '_USD')) {
            $userCurrencyId = $current_user->getPreference('currency');
            if ($currency_id != $userCurrencyId) {
                $currency = BeanFactory::newBean('Currencies');
                $currency->retrieve($currency_id);
                $value = $currency->convertToDollar($value);
                $currency->retrieve($userCurrencyId);
                $value = $currency->convertFromDollar($value);
            }
        }

        $parentfieldlist[strtoupper($fieldname)] = $value;

        return ($sfh->displaySmarty($parentfieldlist, $fieldlist[$fieldname], 'ListView', $displayParams));
    }

    $ss->assign("fields", $fieldlist);
    $ss->assign("form_name", $view);
    $ss->assign("bean", $focus);

    $ss->assign("MOD", $mod_strings);
    $ss->assign("APP", $app_strings);

    return json_encode($ss->fetch($file));
}

function saveField($field, $id, $module, $value)
{
    global $current_user;

    if ($module == 'Users' && $field == 'is_admin' && !$current_user->is_admin) {
        $err = 'SECURITY: Only admin user can change user type';
        $GLOBALS['log']->fatal($err);
        throw new RuntimeException($err);
    }

    $bean = BeanFactory::getBean($module, $id);

    if (is_object($bean) && $bean->id != "") {
        if ($bean->field_defs[$field]['type'] == "multienum") {
            $bean->$field = encodeMultienumValue($value);
        } elseif ($bean->field_defs[$field]['type'] == "relate" || $bean->field_defs[$field]['type'] == 'parent') {
            $save_field = $bean->field_defs[$field]['id_name'];
            $bean->$save_field = $value;
            if ($bean->field_defs[$field]['type'] == 'parent') {
                $bean->parent_type = $_REQUEST['parent_type'];
                $bean->fill_in_additional_parent_fields(); // get up to date parent info as need it to display name
            }
        } elseif ($bean->field_defs[$field]['type'] == "currency") {
            if (stripos((string) $field, 'usdollar')) {
                $newfield = str_replace("_usdollar", "", (string) $field);
                $bean->$newfield = $value;
            } else {
                $bean->$field = $value;
            }
        } elseif ($module === 'Leads' && $field === 'account_name') {
            require_once('modules/Leads/LeadFormBase.php');
            $bean->$field = $value;
            $bean->account_id = LeadFormBase::handleLeadAccountName($bean);
        // Fix #9408 Allow deleting an email address from inline Edit
        } else if(($bean->field_defs[$field]['function']['name'] ?? '') == 'getEmailAddressWidget'){
            $bean->$field = empty($value) ? ' ' : $value;
        } else {
            $bean->$field = $value;
        }

        $check_notify = false;

        if (isset($bean->fetched_row['assigned_user_id']) && $field == "assigned_user_name") {
            $old_assigned_user_id = $bean->fetched_row['assigned_user_id'];
            if (!empty($value) && ($old_assigned_user_id != $value) && ($value != $current_user->id)) {
                $check_notify = true;
            }
        }

        $adminOnlyModules = array('Users', 'Employees');

        $enabled = true;
        if (in_array($module, $adminOnlyModules) && !is_admin($current_user)) {
            $enabled = false;
        }

        if (($bean->ACLAccess("edit") || is_admin($current_user)) && $enabled) {
            $bean->in_workflow=true;
            if ($field == 'email1') {
                $bean->email1_set_in_workflow=true;
            }
            if (!$bean->save($check_notify)) {
                $GLOBALS['log']->fatal("Saving probably failed or bean->save() method did not return with a positive result.");
            }
        } else {
            $GLOBALS['log']->fatal("ACLAccess denied to save this field.");
        }
        $bean->retrieve();
        return getDisplayValue($bean, $field);
    }
    return false;
}

function getDisplayValue($bean, $field, $method = "save")
{
    $fieldlist = [];
    $listViewDefs = [];
    global $log;

    if (file_exists("custom/modules/Accounts/metadata/listviewdefs.php")) {
        $metadata = require("custom/modules/Accounts/metadata/listviewdefs.php");
    } else {
        $metadata = require("modules/Accounts/metadata/listviewdefs.php");
    }

    if (!$bean->ACLAccess('view')) {
        $log->security("getDisplayValue - trying to access unauthorized view/module");
        throw new BadMethodCallException('Unauthorized');
    }

    $fieldlist[$field] = $bean->getFieldDefinition($field);
    $isSensitive = !empty($fieldlist[$field]['sensitive']);
    $notApiVisible = !empty($fieldlist[$field]['api-visible']);

    if ($isSensitive || $notApiVisible){
        $log->security("getDisplayValue - trying to access sensitive field");
        throw new BadMethodCallException('Unauthorized');
    }

    if (is_array($listViewDefs)) {
        $fieldlist[$field] = array_merge($fieldlist[$field], $listViewDefs);
    }

    $value = formatDisplayValue($bean, $bean->$field, $fieldlist[$field], $method);

    return $value;
}

function formatDisplayValue($bean, $value, $vardef, $method = "save")
{
    $name = '';
    global $app_list_strings, $timedate, $current_user;

    //Fake the params so we can pass the values through the sugarwidgets to get the correct display html.

    $GLOBALS['focus'] = $bean;
    $_REQUEST['record'] = $bean->id;
    $vardef['fields']['ID'] = $bean->id;
    $vardef['fields'][strtoupper($vardef['name'])] = $value;
    $vardef['ext2'] = '';

    // If field is of type email.
    if ($vardef['name'] == "email1" && $vardef['group'] == "email1") {
        require_once("include/generic/SugarWidgets/SugarWidgetSubPanelEmailLink.php");
        $SugarWidgetSubPanelEmailLink = new SugarWidgetSubPanelEmailLink($vardef);
        $value = $SugarWidgetSubPanelEmailLink->displayList($vardef);
    }

    //If field is of type link and name.
    if (isset($vardef['link']) && $vardef['link'] && $vardef['type'] == "name" && $_REQUEST['view'] != "DetailView") {
        require_once("include/generic/SugarWidgets/SugarWidgetSubPanelDetailViewLink.php");

        $vardef['module'] = $bean->module_dir;

        $SugarWidgetSubPanelDetailViewLink = new SugarWidgetSubPanelDetailViewLink($vardef);
        $value = "<b>" . $SugarWidgetSubPanelDetailViewLink->displayList($vardef) . "</b>";
    }

    if ($method !== 'close' && ($vardef['type'] === 'datetimecombo' || $vardef['type'] === 'datetime' || $vardef['type'] === 'date')) {
        if ($method != 'save') {
            $value = convertDateUserToDB($value);
        }
        if ($vardef['type'] == 'datetime' || $vardef['type'] == 'datetimecombo') {
            $datetime_format = $timedate->get_date_time_format($current_user);
        } elseif ($vardef['type'] === 'date') {
            $datetime_format = $timedate->get_date_format($current_user);
        }
        // create utc date (as it's utc in db)
        // use the calculated datetime_format
        $datetime = DateTime::createFromFormat($datetime_format, $value, new DateTimeZone('UTC'));

        $value = $datetime->format($datetime_format);
    }

    //If field is of type bool, checkbox.
    if ($vardef['type'] == "bool") {
        require_once("include/generic/LayoutManager.php");
        $layoutManager = new LayoutManager();

        require_once("include/generic/SugarWidgets/SugarWidgetFieldbool.php");

        $SugarWidgetFieldbool = new SugarWidgetFieldbool($layoutManager);
        $value = $SugarWidgetFieldbool->displayListPlain($vardef);
    }

    //if field is of type multienum.
    if ($vardef['type'] == "multienum") {
        $value = str_replace("^", "", (string) $value);

        $array_values = explode(",", $value);

        foreach ($array_values as $value) {
            $values[] = $app_list_strings[$vardef['options']][$value];
        }
        $value = implode(", ", $values);
    }

    //if field is of type radio.
    if ($vardef['type'] == "radioenum" || $vardef['type'] == "enum" || $vardef['type'] == "dynamicenum") {
        $value = $app_list_strings[$vardef['options']][$value];
    }

    //if field is of type relate.
    if ($vardef['type'] == "relate" || $vardef['type'] == "parent") {
        if ($vardef['source'] == "non-db") {
            if (($vardef['module'] ?? '') == "Employees") {
                $vardef['ext2'] = "Users";
                $vardef['rname'] = "full_name";
            }
        }
        if ($vardef['type'] == "parent") {
            $vardef['module'] = $bean->parent_type;
            $name = $bean->parent_name;
        }
        $idName = $vardef['id_name'];
        $record = $bean->$idName;

        if ($vardef['name'] != "assigned_user_name") {
            $value = "<a class=\"listViewTdLinkS1\" href=\"index.php?action=DetailView&module=".$vardef['module']."&record=$record\">";
        } else {
            $value = "";
        }


        //To fix github bug 880 (the rname was null and was causing a 500 error in the getFieldValueFromModule call to $fieldname
        $fieldName = 'name';//$vardef['name'];


        if (!is_null($vardef['rname'] ?? null)) {
            $fieldName = $vardef['rname'];
        }

        $ext2 = $vardef['ext2'] ?? '';

        if (!empty($ext2)) {
            $value .= getFieldValueFromModule($fieldName, $ext2, $record);
        } elseif (!empty($vardef['rname']) || $vardef['name'] == "related_doc_name") {
            $value .= getFieldValueFromModule($fieldName, $vardef['module'], $record);
        } else {
            $value .= $name;
        }

        if ($vardef['name'] != "assigned_user_name") {
            $value .= "</a>";
        }
    }
    if ($vardef['type'] == "url") {
        $link = (substr((string) $value, 0, 7) == 'http://' || substr((string) $value, 0, 8) == 'https://' ?
            $value : 'http://' . $value);
        $value = '<a href=' . $link . ' target="_blank">' . $value . '</a>';
    }

    if ($vardef['type'] == "currency") {
        if ($_REQUEST['view'] != "DetailView") {
            $value = currency_format_number($value);
        } else {
            $value = format_number($value);
        }
    }
    return $value;
}

function getFieldValueFromModule($fieldname, $module, $id)
{
    //Github bug 880, if the fieldname is null, do no call from bean
    if (is_null($fieldname)) {
        return '';
    }

    $bean = BeanFactory::getBean($module, $id);
    if (is_object($bean) && $bean->id != "") {
        return $bean->$fieldname;
    }
}

function convertDateUserToDB($value)
{
    global $timedate;

    $datetime_format = $timedate->get_date_time_format();
    $datetime = DateTime::createFromFormat($datetime_format, $value);

    $value = $datetime->format("Y-m-d H:i:s");
    return $value;
}

function checkAccess($bean)
{
    if ($bean->ACLAccess('EditView')) {
        return true;
    }
    return false;
}
