<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2019 SalesAgility Ltd.
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

global $mod_strings, $app_strings, $app_list_strings;

$sugar_smarty = new Sugar_Smarty();
$sugar_smarty->assign('MOD', $mod_strings);
$sugar_smarty->assign('APP', $app_strings);
$sugar_smarty->assign('APP_LIST', $app_list_strings);

$role = BeanFactory::newBean('ACLRoles');
$categories = $role->getRoleActions($_REQUEST['record']);
$role->retrieve($_REQUEST['record']);
$names = ACLAction::setupCategoriesMatrix($categories);
if (!empty($names)) {
    $tdWidth = 100 / (is_countable($names) ? count($names) : 0);
}
$sugar_smarty->assign('ROLE', $role->toArray());
$sugar_smarty->assign('CATEGORIES', $categories);
$sugar_smarty->assign('TDWIDTH', $tdWidth);
$sugar_smarty->assign('ACTION_NAMES', $names);

$return = ['module' => 'ACLRoles', 'action' => 'DetailView', 'record' => $role->id];
$sugar_smarty->assign('RETURN', $return);
$params = [];
$params[] = "<a href='index.php?module=ACLRoles&action=index'>{$mod_strings['LBL_MODULE_NAME']}</a>";
$params[] = $role->get_summary_text();
echo getClassicModuleTitle("ACLRoles", $params, true);
$hide_hide_supanels = true;

$buttons = [];
$buttons[] = "<input title=\"{$app_strings['LBL_EDIT_BUTTON_TITLE']}\" accessKey=\"{$app_strings['LBL_EDIT_BUTTON_KEY']}\" class=\"btn btn-danger\" onclick=\"var _form = $('#form')[0]; _form.action.value='EditView'; _form.submit();\" type=\"submit\" name=\"button\" value=\"{$app_strings['LBL_EDIT_BUTTON']}\" />";
$buttons[] = "<input title=\"{$app_strings['LBL_DUPLICATE_BUTTON_TITLE']}\" accessKey=\"{$app_strings['LBL_DUPLICATE_BUTTON_KEY']}\" class=\"btn btn-danger\" onclick=\"this.form.isDuplicate.value='1'; this.form.action.value='EditView'\" type=\"submit\" name=\"button\" value=\" {$app_strings['LBL_DUPLICATE_BUTTON']} \" />";
$buttons[] = "<input title=\"{$app_strings['LBL_DELETE_BUTTON_TITLE']}\" accessKey=\"{$app_strings['LBL_DELETE_BUTTON_KEY']}\" class=\"btn btn-danger\" onclick=\"this.form.return_module.value='ACLRoles'; this.form.return_action.value='index'; this.form.action.value='Delete'; return confirm('{$app_strings['NTC_DELETE_CONFIRMATION']}')\" type=\"submit\" name=\"button\" value=\" {$app_strings['LBL_DELETE_BUTTON']} \" />";

$sugar_smarty->assign('buttons', $buttons);

echo $sugar_smarty->fetch('modules/ACLRoles/DetailView.tpl');
// For subpanels the variable must be named focus;
$focus =& $role;
$_REQUEST['module'] = 'ACLRoles';
require_once __DIR__ . '/../../include/SubPanel/SubPanelTiles.php';

$subPanel = new SubPanelTiles($role, 'ACLRoles');

echo $subPanel->display();
