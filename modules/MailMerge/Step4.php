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

/*
 * Created on Oct 4, 2005
 *
 * To change the template for this generated file go to
 * Window - Preferences - PHPeclipse - PHP - Code Templates
 */








global $app_strings;
global $app_list_strings;
global $mod_strings;
global $current_user;
global $locale;

$xtpl = new XTemplate('modules/MailMerge/Step4.html');
$xtpl->assign("MOD", $mod_strings);
$xtpl->assign("APP", $app_strings);

if (!empty($_POST['document_id'])) {
    $_SESSION['MAILMERGE_DOCUMENT_ID'] = $_POST['document_id'];
}
$document_id = $_SESSION['MAILMERGE_DOCUMENT_ID'];
$revision = BeanFactory::newBean('DocumentRevisions');
$revision->retrieve($document_id);
//$document = BeanFactory::newBean('Documents');
//$document->retrieve($document_id);

if (!empty($_POST['selected_objects'])) {
    $selObjs = urldecode($_POST['selected_objects']);
    $_SESSION['SELECTED_OBJECTS_DEF'] = $selObjs;
}
$selObjs = $_SESSION['SELECTED_OBJECTS_DEF'];
$sel_obj = array();

parse_str(stripslashes(html_entity_decode((string) $selObjs, ENT_QUOTES)), $sel_obj);
foreach ($sel_obj as $key=>$value) {
    $sel_obj[$key] = stripslashes($value);
}
$relArray = array();
//build relationship array
foreach ($sel_obj as $key=>$value) {
    $id = 'rel_id_'.md5($key);
    if (isset($_POST[$id]) && !empty($_POST[$id])) {
        $relArray[$key] = $_POST[$id];
    }
}


$builtArray = array();
if (count($relArray) > 0) {
    $_SESSION['MAILMERGE_RELATED_CONTACTS'] = $relArray;

    $relModule = $_SESSION['MAILMERGE_CONTAINS_CONTACT_INFO'];
    global $beanList, $beanFiles;
    $class_name = $beanList[$relModule ];
    require_once($beanFiles[$class_name]);

    $seed = new $class_name();
    foreach ($sel_obj as $key=>$value) {
        $builtArray[$key] = $value;
        if (isset($relArray[$key])) {
            $seed->retrieve($relArray[$key]);
            $name = "";
            if ($relModule  == "Contacts") {
                $name = $locale->getLocaleFormattedName($seed->first_name, $seed->last_name);
            } else {
                $name = $seed->name;
            }
            $builtArray[$key] = str_replace('##', '&', $value)." (".$name.")";
        }
    }
} else {
    $builtArray = $sel_obj;
}

$xtpl->assign("MAILMERGE_MODULE", $_SESSION['MAILMERGE_MODULE']);
$xtpl->assign("MAILMERGE_DOCUMENT_ID", $document_id);
$xtpl->assign("MAILMERGE_TEMPLATE", $revision->filename." (rev. ".$revision->revision.")");
$xtpl->assign("MAILMERGE_SELECTED_OBJECTS", get_select_options_with_id($builtArray, '0'));
$xtpl->assign("MAILMERGE_SELECTED_OBJECTS_DEF", urlencode($selObjs));
$step_num = 4;

if (isset($_SESSION['MAILMERGE_SKIP_REL']) && $_SESSION['MAILMERGE_SKIP_REL'] || !isset($_SESSION['MAILMERGE_RELATED_CONTACTS']) || empty($_SESSION['MAILMERGE_RELATED_CONTACTS'])) {
    $xtpl->assign("PREV_STEP", "2");
    $step_num = 3;
} else {
    $xtpl->assign("PREV_STEP", "3");
}

$xtpl->assign("STEP_NUM", "Step ".$step_num.":");

$xtpl->parse("main");
$xtpl->out("main");
