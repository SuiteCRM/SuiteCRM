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





global $app_list_strings;// $modInvisList

$sugar_smarty = new Sugar_Smarty();

$sugar_smarty->assign('MOD', $mod_strings);
$sugar_smarty->assign('APP', $app_strings);
//mass localization
/*foreach($modInvisList as $modinvisname){
    $app_list_strings['moduleList'][$modinvisname] = $modinvisname;
}*/
$sugar_smarty->assign('APP_LIST', $app_list_strings);
/*foreach($modInvisList as $modinvisname){
    unset($app_list_strings['moduleList'][$modinvisname]);
}*/
$role = BeanFactory::newBean('ACLRoles');
$role_name = '';
$return= array('module'=>'ACLRoles', 'action'=>'index', 'record'=>'');
if (!empty($_REQUEST['record'])) {
    $role->retrieve($_REQUEST['record']);
    $categories = $role->getRoleActions($_REQUEST['record']);
    
    $role_name =  $role->name;
    if (!empty($_REQUEST['isDuplicate'])) {
        //role id is stripped here in duplicate so anything using role id after this will not have it
        $role->id = '';
    } else {
        $return['record']= $role->id;
        $return['action']='DetailView';
    }
} else {
    $categories = $role->getRoleActions('');
}
$sugar_smarty->assign('ROLE', $role->toArray());
$tdwidth = 10;

if (isset($_REQUEST['return_module'])) {
    $return['module']=$_REQUEST['return_module'];
    if (isset($_REQUEST['return_action'])) {
        $return['action']=$_REQUEST['return_action'];
    }
    if (isset($_REQUEST['return_record'])) {
        $return['record']=$_REQUEST['return_record'];
    }
}

$sugar_smarty->assign('RETURN', $return);
$names = ACLAction::setupCategoriesMatrix($categories);
if (!empty($names)) {
    $tdwidth = 100 / (is_countable($names) ? count($names) : 0);
}
$sugar_smarty->assign('CATEGORIES', $categories);
$sugar_smarty->assign('CATEGORY_NAME', $_REQUEST['category_name']);
$sugar_smarty->assign('TDWIDTH', $tdwidth);
$sugar_smarty->assign('ACTION_NAMES', $names);
$actions = !empty($categories[$_REQUEST['category_name']]['module']) ? $categories[$_REQUEST['category_name']]['module'] : '' ;
$sugar_smarty->assign('ACTIONS', $actions);
ob_clean();

if ($_REQUEST['category_name'] == 'All') {
    echo $sugar_smarty->fetch('modules/ACLRoles/EditAllBody.tpl');
} else {
    //WDong Bug 23195: Strings not localized in Role Management.
    echo getClassicModuleTitle($_REQUEST['category_name'], array($app_list_strings['moduleList'][$_REQUEST['category_name']]), false);
    echo $sugar_smarty->fetch('modules/ACLRoles/EditRole.tpl');
    echo '</form>';
}
sugar_cleanup(true);
