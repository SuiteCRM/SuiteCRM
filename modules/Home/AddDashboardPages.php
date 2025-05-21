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



global $current_user;


if (!isset($_POST['dashName'])) {
    $html  ='<form method="post" name="addpageform" id="addpageform" action="index.php?module=Home&action=AddDashboardPages"/>';
    $html .='<table>';
    $html .='<tr>';
    $html .='<td><label for="dashName">'.$GLOBALS['app_strings']['LBL_ENTER_DASHBOARD_NAME'].'</label></td>';
    $html .='<td><input type="text" name="dashName" id="dashName"/></td>';
    $html .='</tr>';
    $html .='<tr>';
    $html .='<td><label for="numColumns">'.$GLOBALS['app_strings']['LBL_NUMBER_OF_COLUMNS'].' </label></td>';
    $html .='<td><select name="numColumns">';
    $html .='<option value="1">1</option>';
    $html .='<option value="2">2</option>';
    $html .='<option value="3">3</option>';
    $html .='</select></td>';
    $html .='</tr>';
    $html .='</table>';
    $html .='</form>';

    echo $html;
} else {

    global $current_language;

    $type = 'Home';
    $pages = [];

    $existingPages = $current_user->getPreference('pages', $type);
    $dashboardPage = array();
    $numberColumns = $_POST['numColumns'];
    $pageName = $_POST['dashName'];

    switch ($numberColumns) {
        case 1:
            $pagecolumns[0] = array();
            $pagecolumns[0]['dashlets'] = array();
            $pagecolumns[0]['width'] = '100%';
            break;
        case 2:
            $pagecolumns[0] = array();
            $pagecolumns[0]['dashlets'] = array();
            $pagecolumns[0]['width'] = '60%';
            $pagecolumns[1] = array();
            $pagecolumns[1]['dashlets'] = array();
            $pagecolumns[1]['width'] = '40%';
            break;
        case 3:
            $pagecolumns[0] = array();
            $pagecolumns[0]['dashlets'] = array();
            $pagecolumns[0]['width'] = '30%';
            $pagecolumns[1] = array();
            $pagecolumns[1]['dashlets'] = array();
            $pagecolumns[1]['width'] = '30%';
            $pagecolumns[2] = array();
            $pagecolumns[2]['dashlets'] = array();
            $pagecolumns[2]['width'] = '30%';
            break;
    }

    $dashboardPage['columns'] = $pagecolumns;
    $dashboardPage['pageTitle'] = $pageName;
    $dashboardPage['numColumns'] = $numberColumns;

    array_push($existingPages, $dashboardPage);

    $current_user->setPreference('pages', $existingPages, 0, $type);

    $display = array();

    foreach ($dashboardPage['columns'] as $colNum => $column) {
        $display[$colNum]['width'] = $column['width'];
    }

    $home_mod_strings = return_module_language($current_language, $type);

    require_once("include/MySugar/retrieve_dash_page.php");

    $sugar_smarty = new Sugar_Smarty();
    $sugar_smarty->assign('columns', $display);
    $sugar_smarty->assign('selectedPage', (is_countable($pages) ? count($pages) : 0) - 1);
    $sugar_smarty->assign('mod', $home_mod_strings);
    $sugar_smarty->assign('app', $GLOBALS['app_strings']);
    $sugar_smarty->assign('lblAddDashlets', $home_mod_strings['LBL_ADD_DASHLETS']);
    $sugar_smarty->assign('numCols', $dashboardPage['numColumns']);
}
