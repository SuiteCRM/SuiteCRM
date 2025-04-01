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

global $mod_strings, $app_strings;
global $current_user, $sugar_config, $current_language;

$module_menu = array();
if ($GLOBALS['current_user']->isAdminForModule('Users')
) {
    $module_menu = array(
        array("index.php?module=Users&action=EditView&return_module=Users&return_action=DetailView", $mod_strings['LNK_NEW_USER'], "Create"),
        array("index.php?module=Users&action=EditView&usertype=group&return_module=Users&return_action=DetailView", $mod_strings['LNK_NEW_GROUP_USER'], "Create_Group_User")
    );
    $module_menu[] = array("index.php?module=Users&action=ListView&return_module=Users&return_action=DetailView", $mod_strings['LNK_USER_LIST'], "List");

    $module_menu[] = array("index.php?module=Import&action=Step1&import_module=Users&return_module=Users&return_action=index", $mod_strings['LNK_IMPORT_USERS'], "Import", 'Contacts');
}

$sg_mod_strings = return_module_language($current_language, 'SecurityGroups');
$module_menu[] = array("index.php?module=SecurityGroups&action=EditView&return_module=SecurityGroups&return_action=DetailView", $sg_mod_strings['LNK_NEW_RECORD'], "Create_Security_Group");
$module_menu[] = array("index.php?module=SecurityGroups&action=ListView&return_module=SecurityGroups&return_action=ListView", $sg_mod_strings['LBL_LIST_FORM_TITLE'], "Security_Groups");

if (is_admin($current_user)) {
    global $current_language;
    $admin_mod_strings = return_module_language($current_language, 'Administration');
    $module_menu[] = array("index.php?module=ACLRoles&action=index&return_module=SecurityGroups&return_action=ListView", $admin_mod_strings['LBL_MANAGE_ROLES_TITLE'], "Role_Management");
    $module_menu[] = array("index.php?module=SecurityGroups&action=config&return_module=SecurityGroups&return_action=ListView", $admin_mod_strings['LBL_CONFIG_SECURITYGROUPS_TITLE'], "Security_Suite_Settings");
}

$module_menu[]= array("index.php?module=InboundEmail&action=index", $mod_strings['LNK_LIST_INBOUND_EMAIL_ACCOUNTS'] ?? '',"List");
$module_menu[]= array("index.php?module=OutboundEmailAccounts&action=index", $mod_strings['LNK_LIST_OUTBOUND_EMAIL_ACCOUNTS'],"List");
$module_menu[]= array("index.php?module=ExternalOAuthConnection&action=index", $mod_strings['LNK_EXTERNAL_OAUTH_CONNECTIONS'],"List");
