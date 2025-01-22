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

/**

 * Description:  Contains a variety of utility functions for the Import module
 * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
 * All Rights Reserved.
 */

/**
 * Returns an input control for this fieldname given
 *
 * @param  string $module
 * @param  string $fieldname
 * @param  string $vardef
 * @param  string $value
 * @return string html for input element for this control
 */
function getControl(
    $module,
    $fieldname,
    $vardef = null,
    $value = ''
    ) {
    global $current_language, $app_strings, $dictionary, $app_list_strings, $current_user;

    // use the mod_strings for this module
    $mod_strings = return_module_language($current_language, $module);

    // set the filename for this control
    $file = create_cache_directory('modules/Import/') . $module . $fieldname . '.tpl';

    if (!is_file($file)
            || inDeveloperMode()
            || !empty($_SESSION['developerMode'])) {
        if (!isset($vardef)) {
            $focus = loadBean($module);
            $vardef = $focus->getFieldDefinition($fieldname);
        }

        // if this is the id relation field, then don't have a pop-up selector.
        $idName = $vardef['id_name'] ?? '';
        if ($vardef['type'] == 'relate' && $idName == $vardef['name']) {
            $vardef['type'] = 'varchar';
        }

        // create the dropdowns for the parent type fields
        if ($vardef['type'] == 'parent_type') {
            $vardef['type'] = 'enum';
        }

        // remove the special text entry field function 'getEmailAddressWidget'
        if (isset($vardef['function']) && $vardef['function'] == 'getEmailAddressWidget') {
            unset($vardef['function']);
        }

        // load SugarFieldHandler to render the field tpl file
        static $sfh;

        if (!isset($sfh)) {
            require_once('include/SugarFields/SugarFieldHandler.php');
            $sfh = new SugarFieldHandler();
        }

        $displayParams = array();
        $displayParams['formName'] = 'importstep3';

        $contents = $sfh->displaySmarty('fields', $vardef, 'ImportView', $displayParams);

        // Remove all the copyright comments
        $contents = preg_replace('/\{\*[^\}]*?\*\}/', '', (string) $contents);

        // hack to disable one of the js calls in this control
        if (isset($vardef['function']) && $vardef['function'] == 'getCurrencyDropDown') {
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

    // populate the fieldlist from the vardefs
    $fieldlist = array();
    if (!isset($focus) || !($focus instanceof SugarBean)) {
        $focus = loadBean($module);
    }
    // create the dropdowns for the parent type fields

    $type = $vardef['type'] ?? '';

    if ($type === 'parent_type') {
        $focus->field_defs[$vardef['name']]['options'] = $focus->field_defs[$vardef['group']]['options'];
    }
    $vardefFields = $focus->getFieldDefinitions();
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
        // Bug 22730: make sure all enums have the ability to select blank as the default value.
        if (!isset($fieldlist[$name]['options'][''])) {
            if ($name === 'report_module'){
                continue;
            }
            $fieldlist[$name]['options'][''] = '';
        }
    }
    // fill in function return values
    if (!in_array($fieldname, array('email1','email2'))) {
        if (!empty($fieldlist[$fieldname]['function']['returns']) && $fieldlist[$fieldname]['function']['returns'] == 'html') {
            $function = $fieldlist[$fieldname]['function']['name'];
            // include various functions required in the various vardefs
            if (isset($fieldlist[$fieldname]['function']['include']) && is_file($fieldlist[$fieldname]['function']['include'])) {
                require_once($fieldlist[$fieldname]['function']['include']);
            }
            $value = $function($focus, $fieldname, $value, 'EditView');
            // Bug 22730 - add a hack for the currency type dropdown, since it's built by a function.
            if (preg_match('/getCurrency.*DropDown/s', (string) $function)) {
                $value = str_ireplace('</select>', '<option value="">'.$app_strings['LBL_NONE'].'</option></select>', (string) $value);
            }
        } elseif ($fieldname == 'assigned_user_name' && empty($value)) {
            $fieldlist['assigned_user_id']['value'] = $GLOBALS['current_user']->id;
            $value = get_assigned_user_name($GLOBALS['current_user']->id);
        } elseif ($fieldname == 'team_name' && empty($value)) {
            $value = json_encode(array());
        }
    }
    $fieldlist[$fieldname]['value'] = $value;
    $ss->assign("fields", $fieldlist);
    $ss->assign("form_name", 'importstep3');
    $ss->assign("bean", $focus);

    // add in any additional strings
    $ss->assign("MOD", $mod_strings);
    $ss->assign("APP", $app_strings);
    return $ss->fetch($file);
}
