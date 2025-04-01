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

/**

 * Description:
 */


$header_text = '';
global $mod_strings;
global $app_list_strings;
global $app_strings;
global $current_user;

if ((!is_admin($GLOBALS['current_user']) && (!is_admin_for_module($GLOBALS['current_user'], 'Bugs')))) {
    sugar_die("Unauthorized access to administration.");
}

$focus = BeanFactory::newBean('Releases');
echo getClassicModuleTitle($mod_strings['LBL_MODULE_NAME'], array($mod_strings['LBL_MODULE_TITLE']), true);
$is_edit = false;
if (!empty($_REQUEST['record'])) {
    $result = $focus->retrieve($_REQUEST['record']);
    if ($result == null) {
        sugar_die($app_strings['ERROR_NO_RECORD']);
    }
    $is_edit=true;
}
if (isset($_REQUEST['edit']) && $_REQUEST['edit']=='true') {
    $is_edit=true;
    //Only allow admins to enter this screen
    if (!is_admin($current_user)&& !is_admin_for_module($GLOBALS['current_user'], 'Bugs')) {
        $GLOBALS['log']->error("Non-admin user ($current_user->user_name) attempted to enter the Releases edit screen");
        session_destroy();
        include('modules/Users/Logout.php');
    }
}

$GLOBALS['log']->info("Release list view");
global $theme;

$button  = "<form border='0' action='index.php' method='post' name='form'>\n";
$button .= "<input type='hidden' name='module' value='Releases'>\n";
$button .= "<input type='hidden' name='action' value='EditView'>\n";
$button .= "<input type='hidden' name='edit' value='true'>\n";
$button .= "<input type='hidden' name='return_module' value='".$currentModule."'>\n";
$button .= "<input type='hidden' name='return_action' value='".$action."'>\n";
$button .= "<input title='".$app_strings['LBL_NEW_BUTTON_TITLE']."' accessyKey='".$app_strings['LBL_NEW_BUTTON_KEY']."' class='button' type='submit' name='New' value='  ".$app_strings['LBL_NEW_BUTTON_LABEL']."  '>\n";
$button .= "</form>\n";

$ListView = new ListView();
if ((is_admin($current_user)|| is_admin_for_module($GLOBALS['current_user'], 'Bugs')) && $_REQUEST['module'] != 'DynamicLayout' && !empty($_SESSION['editinplace'])) {
    $header_text = "&nbsp;<a href='index.php?action=index&module=DynamicLayout&from_action=ListView&from_module=".$_REQUEST['module'] ."'>".SugarThemeRegistry::current()->getImage("EditLayout", "border='0' align='bottom'", null, null, '.gif', $mod_strings['LBL_EDITLAYOUT'])."</a>";
}
$ListView->initNewXTemplate('modules/Releases/ListView.html', $mod_strings);
$ListView->xTemplateAssign("DELETE_INLINE_PNG", SugarThemeRegistry::current()->getImage('delete_inline', 'align="absmiddle" border="0"', null, null, '.gif', $app_strings['LNK_DELETE']));
$ListView->setHeaderTitle($mod_strings['LBL_LIST_FORM_TITLE'] . $header_text);
$ListView->setHeaderText($button);
$ListView->show_export_button = false;
$ListView->show_mass_update = false;
$ListView->show_delete_button = false;
$ListView->show_select_menu = false;
$ListView->setQuery("", "", "list_order", "RELEASE");
$ListView->processListView($focus, "main", "RELEASE");

if ($is_edit) {
    $edit_button ="<form name='EditView' method='POST' action='index.php'>\n";
    $edit_button .="<input type='hidden' name='module' value='Releases'>\n";
    $edit_button .="<input type='hidden' name='record' value='$focus->id'>\n";
    $edit_button .="<input type='hidden' name='action'>\n";
    $edit_button .="<input type='hidden' name='edit'>\n";
    $edit_button .="<input type='hidden' name='isDuplicate'>\n";
    $edit_button .="<input type='hidden' name='return_module' value='Releases'>\n";
    $edit_button .="<input type='hidden' name='return_action' value='index'>\n";
    $edit_button .="<input type='hidden' name='return_id' value=''>\n";
    $edit_button .='<input title="'.$app_strings['LBL_SAVE_BUTTON_TITLE'].'" accessKey="'.$app_strings['LBL_SAVE_BUTTON_KEY'].'" class="button" onclick="this.form.action.value=\'Save\'; return check_form(\'EditView\');" type="submit" name="button" value="  '.$app_strings['LBL_SAVE_BUTTON_LABEL'].'  " >';
    $edit_button .=' <input title="'.$app_strings['LBL_SAVE_NEW_BUTTON_TITLE'].'" class="button" onclick="this.form.action.value=\'Save\'; this.form.isDuplicate.value=\'true\'; this.form.edit.value=\'true\'; this.form.return_action.value=\'EditView\'; return check_form(\'EditView\')" type="submit" name="button" value="  '.$app_strings['LBL_SAVE_NEW_BUTTON_LABEL'].'  " >';
    if ((is_admin($current_user) || is_admin_for_module($GLOBALS['current_user'], 'Bugs')) && $_REQUEST['module'] != 'DynamicLayout' && !empty($_SESSION['editinplace'])) {
        $header_text = "&nbsp;<a href='index.php?action=index&module=DynamicLayout&edit=true&from_action=EditView&from_module=".$_REQUEST['module'] ."'>".SugarThemeRegistry::current()->getImage("EditLayout", "border='0' align='bottom'", null, null, '.gif', $mod_strings['LBL_EDITLAYOUT'])."</a>";
    }
    echo get_form_header($mod_strings['LBL_RELEASE']." ".$focus->name . '&nbsp;' . $header_text, $edit_button, false);


    $GLOBALS['log']->info("Releases edit view");
    $xtpl=new XTemplate('modules/Releases/EditView.html');
    $xtpl->assign("MOD", $mod_strings);
    $xtpl->assign("APP", $app_strings);

    if (isset($_REQUEST['return_module'])) {
        $xtpl->assign("RETURN_MODULE", $_REQUEST['return_module']);
    }
    if (isset($_REQUEST['return_action'])) {
        $xtpl->assign("RETURN_ACTION", $_REQUEST['return_action']);
    }
    if (isset($_REQUEST['return_id'])) {
        $xtpl->assign("RETURN_ID", $_REQUEST['return_id']);
    }
    $xtpl->assign("PRINT_URL", "index.php?".$GLOBALS['request_string']);
    $xtpl->assign("JAVASCRIPT", get_set_focus_js());
    $xtpl->assign("ID", $focus->id);
    $xtpl->assign('NAME', $focus->name);
    $xtpl->assign('STATUS', $focus->status);


    if (empty($focus->list_order)) {
        $xtpl->assign('LIST_ORDER', (is_countable($focus->get_releases(false, 'All')) ? count($focus->get_releases(false, 'All')) : 0)+1);
    } else {
        $xtpl->assign('LIST_ORDER', $focus->list_order);
    }
    $xtpl->assign('STATUS_OPTIONS', get_select_options_with_id($app_list_strings['release_status_dom'], $focus->status));

    // adding custom fields:

    require_once('modules/DynamicFields/templates/Files/EditView.php');


    $xtpl->parse("main");
    $xtpl->out("main");
    
    $javascript = new javascript();
    $javascript->setFormName('EditView');
    $javascript->setSugarBean($focus);
    $javascript->addAllFields('');
    echo $javascript->getScript();
}
