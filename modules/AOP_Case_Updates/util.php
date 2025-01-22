<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2023 SalesAgility Ltd.
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
 * @param string $assignField
 * @param array $value
 * @return string
 */
function getAOPAssignField($assignField, $value)
{
    global $app_list_strings;

    $roles = get_bean_select_array(true, 'ACLRole', 'name', '', 'name', true);
    $securityGroups = get_bean_select_array(true, 'SecurityGroup', 'name', '', 'name', true);

    $field = "<select type='text' name='$assignField" . '[0]' . "' id='$assignField" . '[0]' . "' onchange='assign_field_change(\"$assignField\")' title='' tabindex='116'>" . get_select_options_with_id($app_list_strings['aow_assign_options'], $value[0] ?? null) . '</select>&nbsp;&nbsp;';
    if (!file_exists('modules/SecurityGroups/SecurityGroup.php')) {
        $field .= "<input type='hidden' name='$assignField" . '[1]' . "' id='$assignField" . '[1]' . "' value=''  />";
    } else {
        $display = 'none';
        if (isset($value[0]) && $value[0] === 'security_group') {
            $display = '';
        }
        $field .= "<select type='text' style='display:$display' name='$assignField" . '[1]' . "' id='$assignField" . '[1]' . "' title='' tabindex='116'>" . get_select_options_with_id($securityGroups, $value[0][1] ?? null) . '</select>&nbsp;&nbsp;';
    }
    $display = 'none';
    if (isset($value[0]) && ($value[0] === 'role' || $value[0] === 'security_group')) {
        $display = '';
    }
    $field .= "<select type='text' style='display:$display' name='$assignField" . '[2]' . "' id='$assignField" . '[2]' . "' title='' tabindex='116'>" . get_select_options_with_id($roles, $value[0][2] ?? null) . '</select>&nbsp;&nbsp;';

    return $field;
}

/**
 * @param mixed $value
 * @return string
 */
function getAOPAssignFieldDetailView($value)
{
    global $app_list_strings, $app_strings;

    if (empty($value)){
        return '';
    }

    if (is_string($value)){
        return $value;
    }

    $roles = get_bean_select_array(true, 'ACLRole', 'name', '', 'name', true);
    $securityGroups = get_bean_select_array(true, 'SecurityGroup', 'name', '', 'name', true);

    $field = '';
    $type = $value[0] ?? null;
    $field .= $app_list_strings['aow_assign_options'][$type] ?? '';

    if (file_exists('modules/SecurityGroups/SecurityGroup.php'))  {
        $display = 'none';
        if (isset($value[0]) && $value[0] === 'security_group') {
            $display = '';
        }
        if ($display !== 'none') {
            $securityGroup = $value[1] ?? null;
            $field .= ' | ' . $app_strings['LBL_SECURITYGROUP'] . ': ' . $securityGroups[$securityGroup] ?? '';
        }

    }

    $display = 'none';
    if (isset($value[0]) && ($value[0] === 'role' || $value[0] === 'security_group')) {
        $display = '';
    }

    if ($display !== 'none') {
        $role = $value[2] ?? null;

        $field .= ' | ' . $app_strings['LBL_ROLE'] . ': ' .  $roles[$role] ?? '';
    }

    return $field;
}

/**
 * @return bool
 */
function isAOPEnabled()
{
    global $sugar_config;
    if (array_key_exists('aop', $sugar_config) && array_key_exists('enable_aop', $sugar_config['aop'])) {
        return !empty($sugar_config['aop']['enable_aop']);
    }

    //Defaults to enabled.
    return true;
}

/**
 * @return array
 */
function getPortalEmailSettings()
{
    global $sugar_config;
    $settings = array('from_name' => '', 'from_address' => '');

    if (array_key_exists('aop', $sugar_config)) {
        if (array_key_exists('support_from_address', $sugar_config['aop'])) {
            $settings['from_address'] = $sugar_config['aop']['support_from_address'];
        }
        if (array_key_exists('support_from_name', $sugar_config['aop'])) {
            $settings['from_name'] = $sugar_config['aop']['support_from_name'];
        }
    }
    if ($settings['from_name'] && $settings['from_address']) {
        return $settings;
    }

    //Fallback to sugar settings
    $admin = BeanFactory::newBean('Administration');
    $admin->retrieveSettings();
    if (!$settings['from_name']) {
        $settings['from_name'] = $admin->settings['notify_fromname'];
    }
    if (!$settings['from_address']) {
        $settings['from_address'] = $admin->settings['notify_fromaddress'];
    }

    return $settings;
}

/**
 * Custom parse template method since sugars own does not deal with custom fields.
 *
 * @param string $string
 * @param array $bean_arr
 * @return string
 */
function aop_parse_template($string, $bean_arr)
{
    $typeMap = array('dynamicenum' => 'enum');

    foreach ($bean_arr as $bean_name => $bean_id) {
        $focus = BeanFactory::getBean($bean_name, $bean_id);

        if ($bean_name === 'Leads' || $bean_name === 'Prospects') {
            $bean_name = 'Contacts';
        }

        foreach ($focus->field_defs as $key => $field_def) {
            if (array_key_exists($field_def['type'], $typeMap)) {
                $focus->field_defs[$key]['type'] = $typeMap[$field_def['type']];
            }
        }

        $emailTemplate = BeanFactory::newBean('EmailTemplates');
        $string = $emailTemplate->parse_template_bean($string, $bean_name, $focus);

    }

    return $string;
}
