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


require_once('modules/Administration/Administration.php');
require_once('modules/SecurityGroups/SecurityGroup.php');

if (!empty($_REQUEST['remove_default_id'])) {
    SecurityGroup::removeDefaultGroup($_REQUEST['remove_default_id']);
} else {
    if (!empty($_REQUEST['default_group'])) {
        SecurityGroup::saveDefaultGroup($_REQUEST['default_group'], $_REQUEST['default_module']);
    }

    require_once('modules/Configurator/Configurator.php');
    $cfg = new Configurator();

    $securitysuiteAdditive = $_REQUEST['securitysuite_additive'] ?? 0;
    $securitysuiteStrictRights = $_REQUEST['securitysuite_strict_rights'] ?? 0;
    $securitysuiteFilterUserList = $_REQUEST['securitysuite_filter_user_list'] ?? 0;
    $securitysuiteUserRolePrecedence = $_REQUEST['securitysuite_user_role_precedence'] ?? 0;
    $securitysuiteUserPopup = $_REQUEST['securitysuite_user_popup'] ?? 0;
    $securitysuitePopupSelect = $_REQUEST['securitysuite_popup_select'] ?? 0;
    $securitysuiteInheritCreator = $_REQUEST['securitysuite_inherit_creator'] ?? 0;
    $securitysuiteInheritParent = $_REQUEST['securitysuite_inherit_parent'] ?? 0;
    $securitysuiteInheritAssigned = $_REQUEST['securitysuite_inherit_assigned'] ?? 0;
    $securitysuiteInboundEmail = $_REQUEST['securitysuite_inbound_email'] ?? 0;

    // save securitysuite_additive setting
    $cfg->config['securitysuite_additive'] = $securitysuiteAdditive == 1;
    // save securitysuite_strict_rights setting
    $cfg->config['securitysuite_strict_rights'] = $securitysuiteStrictRights == 1;
    // save securitysuite_filter_user_list setting
    $cfg->config['securitysuite_filter_user_list'] = $securitysuiteFilterUserList== 1;
    // save securitysuite_user_role_precedence setting
    $cfg->config['securitysuite_user_role_precedence'] = $securitysuiteUserRolePrecedence == 1;
    // save securitysuite_user_popup setting
    $cfg->config['securitysuite_user_popup'] = $securitysuiteUserPopup == 1;
    // save securitysuite_popup_select setting
    $cfg->config['securitysuite_popup_select'] = $securitysuitePopupSelect == 1;
    // save securitysuite_inherit_creator setting
    $cfg->config['securitysuite_inherit_creator'] = $securitysuiteInheritCreator == 1;
    // save securitysuite_inherit_parent setting
    $cfg->config['securitysuite_inherit_parent'] = $securitysuiteInheritParent == 1;
    // save securitysuite_inherit_assigned setting
    $cfg->config['securitysuite_inherit_assigned'] = $securitysuiteInheritAssigned == 1;
    // save securitysuite_inbound_email setting
    $cfg->config['securitysuite_inbound_email'] = $securitysuiteInboundEmail == 1;

    if (!isset($cfg->config['addAjaxBannedModules'])) {
        $cfg->config['addAjaxBannedModules'] = array();
    }
    if (!in_array('SecurityGroups', $cfg->config['addAjaxBannedModules'])) {
        $cfg->config['addAjaxBannedModules'][] = 'SecurityGroups';
    }

    $cfg->handleOverride();
}

header("Location: index.php?action={$_POST['return_action']}&module={$_POST['return_module']}");
