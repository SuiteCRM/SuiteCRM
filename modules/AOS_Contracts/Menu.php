<?php
/**
 * SuiteCRM is a customer relationship management program developed by SuiteCRM Ltd.
 * Copyright (C) 2011 - 2025 SuiteCRM Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUITECRM, SUITECRM DISCLAIMS THE
 * WARRANTY OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License
 * version 3, these Appropriate Legal Notices must retain the display of the
 * "Supercharged by SuiteCRM" logo. If the display of the logos is not reasonably
 * feasible for technical reasons, the Appropriate Legal Notices must display
 * the words "Supercharged by SuiteCRM".
 */
 
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}


global $mod_strings, $app_strings, $sugar_config;
 
if (ACLController::checkAccess('AOS_Contracts', 'edit', true)) {
    $module_menu[]=array("index.php?module=AOS_Contracts&action=EditView&return_module=AOS_Contracts&return_action=DetailView", $mod_strings['LNK_NEW_RECORD'],"Create", 'AOS_Contracts');
}
if (ACLController::checkAccess('AOS_Contracts', 'list', true)) {
    $module_menu[]=array("index.php?module=AOS_Contracts&action=index&return_module=AOS_Contracts&return_action=DetailView", $mod_strings['LNK_LIST'],"List", 'List');
}
if (ACLController::checkAccess('AOS_Contracts', 'import', true)) {
    $module_menu[]=array("index.php?module=Import&action=Step1&import_module=AOS_Contracts&return_module=AOS_Contracts&return_action=index", $app_strings['LBL_IMPORT'],"Import", 'AOS_Contracts');
}
