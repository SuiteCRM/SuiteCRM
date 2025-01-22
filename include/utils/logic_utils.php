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


function get_hook_array($module_name)
{
    $hook_array = null;
    // This will load an array of the hooks to process
    $file = "custom/modules/$module_name/logic_hooks.php";
    if (file_exists($file)) {
        include($file);
    } else {
        LoggerManager::getLogger()->warn('File not found: ' . $file);
    }
    return $hook_array;

    //end function return_hook_array
}



function check_existing_element($hook_array, $event, $action_array)
{
    if (isset($hook_array[$event])) {
        foreach ($hook_array[$event] as $action) {
            if ($action[1] == $action_array[1]) {
                return true;
            }
        }
    }
    return false;

    //end function check_existing_element
}

function replace_or_add_logic_type($hook_array)
{
    $new_entry = build_logic_file($hook_array);

    $new_contents = "<?php\n$new_entry\n?>";

    return $new_contents;
}



function write_logic_file($module_name, $contents)
{
    $file = "modules/".$module_name . '/logic_hooks.php';
    $file = create_custom_directory($file);

    return sugar_file_put_contents($file, $contents) !== false;

    //end function write_logic_file
}

function build_logic_file($hook_array)
{
    $hook_contents = "";

    $hook_contents .= "// Do not store anything in this file that is not part of the array or the hook version.  This file will	\n";
    $hook_contents .= "// be automatically rebuilt in the future. \n ";
    $hook_contents .= "\$hook_version = 1; \n";
    $hook_contents .= "\$hook_array = Array(); \n";
    $hook_contents .= "// position, file, function \n";

    foreach ($hook_array as $event_array => $event) {
        $hook_contents .= "\$hook_array['".$event_array."'] = Array(); \n";

        foreach ($event as $second_key => $elements) {
            $hook_contents .= "\$hook_array['".$event_array."'][] = ";
            $hook_contents .= "Array(".$elements[0].", '".$elements[1]."', '".$elements[2]."','".$elements[3]."', '".$elements[4]."'); \n";
        }

        //end foreach hook_array as event => action_array
    }

    $hook_contents .= "\n\n";

    return $hook_contents;

    //end function build_logic_file
}
