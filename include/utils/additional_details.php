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


    function additional_details($fields, SugarBean $bean, $params)
    {
        global $current_language, $timedate, $app_list_strings;
        $mod_strings = return_module_language($current_language, $bean->module_name);

        // Create DB Date versions of each date field
        foreach ($fields as $i => $f) {
            if (empty($f)) {
                continue;
            }

            if (!isset($bean->field_name_map[strtolower($i)])) {
                continue;
            }

            if ($bean->field_name_map[strtolower($i)]['type'] == 'datetime' || $bean->field_name_map[strtolower($i)]['type'] == 'datetimecombo') {
                $db_date = $timedate->fromUser($f);
                $db_date_format = $db_date->format('Y-m-d H:i:s');
                $fields['DB_'.$i] = $db_date_format;
            }
        }

        // Load smarty templates
        $templateCaption = new Sugar_Smarty();
        $templateCaption->assign('APP', $app_list_strings);
        $templateCaption->assign('MOD', $mod_strings);
        $templateCaption->assign('FIELD', $fields);
        $templateCaption->assign('ACL_EDIT_VIEW', $bean->ACLAccess('EditView'));
        $templateCaption->assign('ACL_DETAIL_VIEW', $bean->ACLAccess('DetailView'));
        $templateCaption->assign('PARAM', $params);
        $templateCaption->assign('MODULE_NAME', $bean->module_name);
        $templateCaption->assign('OBJECT_NAME', $bean->object_name);
        $caption = $templateCaption->fetch('modules/'. $bean->module_name .'/tpls/additionalDetails.caption.tpl');

        $templateBody = new Sugar_Smarty();
        $templateBody->assign('APP', $app_list_strings);
        $templateBody->assign('MOD', $mod_strings);
        $templateBody->assign('FIELD', $fields);
        $templateBody->assign('ACL_EDIT_VIEW', $bean->ACLAccess('EditView'));
        $templateBody->assign('ACL_DETAIL_VIEW', $bean->ACLAccess('DetailView'));
        $templateBody->assign('PARAM', $params);
        $templateBody->assign('MODULE_NAME', $bean->module_name);
        $templateBody->assign('OBJECT_NAME', $bean->object_name);
        $body = $templateBody->fetch('modules/'. $bean->module_name .'/tpls/additionalDetails.body.tpl');

        $editLink = "index.php?action=EditView&module='. $bean->module_name .'&record={$fields['ID']}";
        $viewLink = "index.php?action=DetailView&module='. $bean->module_name .'&record={$fields['ID']}";

        return array(
            'fieldToAddTo' => 'NAME',
            'string' => $body,
            'editLink' => $editLink,
            'viewLink' => $viewLink,
            'caption'=> $caption,
            'body' => $body,
            'version' => '7.7.6'
        );
    }
