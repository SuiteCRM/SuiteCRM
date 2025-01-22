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

/**
 * EmployeeStatus.php
 * This is a helper file used by the meta-data framework
 * @see modules/Users/vardefs.php (employee_status)
 * @author Collin Lee
 */
function getEmployeeStatusOptions($focus, $name = 'employee_status', $value = null, $view = 'DetailView')
{
    global $current_user, $app_list_strings, $sugar_config;

    $sugar_config = $sugar_config ?? [];

    if (($view == 'EditView' || $view == 'MassUpdate') && is_admin($current_user)) {
        $employee_status  = "<select name='$name'";
        if (!empty($sugar_config['default_user_name'])
            && $sugar_config['default_user_name'] == $focus->user_name
            && isset($sugar_config['lock_default_user_name'])
            && $sugar_config['lock_default_user_name']) {
            $employee_status .= " disabled ";
        }
        $employee_status .= ">";
        $employee_status .= get_select_options_with_id($app_list_strings['employee_status_dom'], $focus->employee_status);
        $employee_status .= "</select>\n";
        return $employee_status;
    }

    if (!empty($value)){
    	$focus->employee_status = $value;
    }
    if (isset($app_list_strings['employee_status_dom'][$focus->employee_status])) {
        return $app_list_strings['employee_status_dom'][$focus->employee_status];
    }

    return $focus->employee_status;
}

function getMessengerTypeOptions($focus, $name = 'messenger_type', $value = null, $view = 'DetailView')
{
    global $current_user, $app_list_strings;
    if ($view == 'EditView' || $view == 'MassUpdate') {
        $messenger_type = "<select name=\"$name\">";
        $messenger_type .= get_select_options_with_id($app_list_strings['messenger_type_dom'], $focus->messenger_type);
        $messenger_type .= '</select>';
        return $messenger_type;
    }

    return $app_list_strings['messenger_type_dom'][$focus->messenger_type];
}
