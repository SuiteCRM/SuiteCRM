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




global $app_list_strings, $app_strings, $current_user;

$mod_strings = return_module_language($GLOBALS['current_language'], 'Users');

$focus = BeanFactory::newBean('Users');
$focus->retrieve($_REQUEST['record']);
if (!is_admin($focus)) {
    $sugar_smarty = new Sugar_Smarty();
    $sugar_smarty->assign('MOD', $mod_strings);
    $sugar_smarty->assign('APP', $app_strings);
    $sugar_smarty->assign('APP_LIST', $app_list_strings);
    
    $categories = ACLAction::getUserActions($_REQUEST['record'], true);
    
    //clear out any removed tabs from user display
    if (!$GLOBALS['current_user']->isAdminForModule('Users')) {
        $tabs = $focus->getPreference('display_tabs');
        global $modInvisList;
        if (!empty($tabs)) {
            foreach ($categories as $key=>$value) {
                if (!in_array($key, $tabs) &&  !in_array($key, $modInvisList)) {
                    unset($categories[$key]);
                }
            }
        }
    }
    
    $names = array();
    $names = ACLAction::setupCategoriesMatrix($categories);
    if (!empty($names)) {
        $tdwidth = 100 / (is_countable($names) ? count($names) : 0);
    }
    $sugar_smarty->assign('APP', $app_list_strings);
    $sugar_smarty->assign('CATEGORIES', $categories);
    $sugar_smarty->assign('TDWIDTH', $tdwidth);
    $sugar_smarty->assign('ACTION_NAMES', $names);
    
    $title = getClassicModuleTitle('', array($mod_strings['LBL_MODULE_NAME'],$mod_strings['LBL_ROLES_SUBPANEL_TITLE']), '');
    
    $sugar_smarty->assign('TITLE', $title);
    $sugar_smarty->assign('USER_ID', $focus->id);
    $sugar_smarty->assign('LAYOUT_DEF_KEY', 'UserRoles');
    echo $sugar_smarty->fetch('modules/ACLRoles/DetailViewUser.tpl');
    
    
    //this gets its layout_defs.php file from the user not from ACLRoles so look in modules/Users for the layout defs
    require_once('include/SubPanel/SubPanelTiles.php');
    $modules_exempt_from_availability_check=array('Users'=>'Users','ACLRoles'=>'ACLRoles',);
    $subpanel = new SubPanelTiles($focus, 'UserRoles');
    
    echo $subpanel->display(true, true);
}
