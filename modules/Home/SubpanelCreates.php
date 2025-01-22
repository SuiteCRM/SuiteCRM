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

$mod_strings   = return_module_language($current_language, $_REQUEST['target_module']);
$target_module = $_REQUEST['target_module'] ?? ''; // target class

if (empty($target_module) || !isAllowedModuleName($target_module)) {
    throw new InvalidArgumentException('Invalid target_module');
}

if (file_exists('modules/'. $_REQUEST['target_module'] . '/EditView.php')) {
    $tpl = $_REQUEST['tpl'];
    if (is_file('modules/' . $target_module . '/' . $target_module . 'QuickCreate.php')) { // if there is a quickcreate override
        require_once('modules/' . $target_module . '/' . $target_module . 'QuickCreate.php');
        $editviewClass     = $target_module . 'QuickCreate'; // eg. OpportunitiesQuickCreate
        $editview          = new $editviewClass($target_module, 'modules/' . $target_module . '/tpls/' . $tpl);
        $editview->viaAJAX = true;
    } else { // else use base class
        require_once('include/EditView/EditViewQuickCreate.php');
        $editview = new EditViewQuickCreate($target_module, 'modules/' . $target_module . '/tpls/' . $tpl);
    }
    $editview->process();
    echo $editview->display();
} else {
    $subpanelView = 'modules/'. $target_module . '/views/view.subpanelquickcreate.php';
    $view = (!empty($_REQUEST['target_view'])) ? $_REQUEST['target_view'] : 'QuickCreate';
    //Check if there is a custom override, then check for module override, finally use default (SubpanelQuickCreate)
    if (file_exists('custom/' . $subpanelView)) {
        require_once('custom/' . $subpanelView);
        $subpanelClass = $target_module . 'SubpanelQuickCreate';
        $customClass = 'Custom' . $subpanelClass;
        if (class_exists($customClass)) {
            $subpanelClass = $customClass;
        }
        $sqc  = new $subpanelClass($target_module, $view);
    } else {
        if (file_exists($subpanelView)) {
            require_once($subpanelView);
            $subpanelClass = $target_module . 'SubpanelQuickCreate';
            $sqc  = new $subpanelClass($target_module, $view);
        } else {
            require_once('include/EditView/SubpanelQuickCreate.php');
            $sqc  = new SubpanelQuickCreate($target_module, $view);
        }
    }
}
