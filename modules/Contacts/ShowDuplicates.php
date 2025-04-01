<?php
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

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

if (!isset($_SESSION['SHOW_DUPLICATES'])) {
    sugar_die("Unauthorized access to this area.");
}
// retrieve $_POST values out of the $_SESSION variable - placed in there by ContactFormBase to avoid the length limitations on URLs implicit with GETS
//$GLOBALS['log']->debug('ShowDuplicates.php: _POST = '.print_r($_SESSION['SHOW_DUPLICATES'],true));
parse_str($_SESSION['SHOW_DUPLICATES'], $_POST);
$post = array_map("securexss", $_POST);
foreach ($post as $k => $v) {
    $_POST[$k] = $v;
}
unset($_SESSION['SHOW_DUPLICATES']);
//$GLOBALS['log']->debug('ShowDuplicates.php: _POST = '.print_r($_POST,true));

global $app_strings;
global $app_list_strings;
global $theme;

$error_msg = '';
global $current_language;
$mod_strings = return_module_language($current_language, 'Contacts');
$moduleName = $GLOBALS['app_list_strings']['moduleList']['Contacts'];
echo getClassicModuleTitle('Contacts', array($moduleName,$mod_strings['LBL_SAVE_CONTACT']), true);
$xtpl=new XTemplate('modules/Contacts/ShowDuplicates.html');
$xtpl->assign("MOD", $mod_strings);
$xtpl->assign("APP", $app_strings);
$xtpl->assign("PRINT_URL", "index.php?".$GLOBALS['request_string']);
$xtpl->assign("MODULE", $_REQUEST['module']);
if ($error_msg !== '') {
    $xtpl->assign("ERROR", $error_msg);
    $xtpl->parse("main.error");
}

if ((isset($_REQUEST['popup']) && $_REQUEST['popup'] == 'true') ||(isset($_POST['popup']) && $_POST['popup']==true)) {
    insert_popup_header($theme);
}


$contact = BeanFactory::newBean('Contacts');
require_once('modules/Contacts/ContactFormBase.php');
$contactForm = new ContactFormBase();
$GLOBALS['check_notify'] = false;


$query = 'select contacts.id, first_name, last_name, title, accounts.name, primary_address_city from contacts LEFT JOIN accounts_contacts ON contacts.id=accounts_contacts.contact_id and accounts_contacts.deleted = 0 LEFT JOIN accounts ON accounts_contacts.account_id=accounts.id AND accounts_contacts.deleted=0 AND accounts.deleted=0 where contacts.deleted=0 ';
$duplicates = $_POST['duplicate'];
$count = is_countable($duplicates) ? count($duplicates) : 0;
$db = DBManagerFactory::getInstance();
if ($count > 0) {
    $query .= "and (";
    $first = true;
    foreach ($duplicates as $duplicate_id) {
        if (!$first) {
            $query .= ' OR ';
        }
        $first = false;
        $duplicateIdQuoted = $db->quote($duplicate_id);
        $query .= "contacts.id='$duplicateIdQuoted' ";
    }
    $query .= ')';
}

$duplicateContacts = array();

$result = $db->query($query);
$i=0;
while (($row=$db->fetchByAssoc($result)) != null) {
    $duplicateContacts[$i] = $row;
    $i++;
}

$xtpl->assign('FORMBODY', $contactForm->buildTableForm($duplicateContacts));

$input = '';
foreach ($contact->column_fields as $field) {
    if (!empty($_POST['Contacts'.$field])) {
        $input .= "<input type='hidden' name='$field' value='{$_POST['Contacts'.$field]}'>\n";
    }
}

foreach ($contact->additional_column_fields as $field) {
    if (!empty($_POST['Contacts'.$field])) {
        $input .= "<input type='hidden' name='$field' value='{$_POST['Contacts'.$field]}'>\n";
    }
}

// Bug 25311 - Add special handling for when the form specifies many-to-many relationships
if (!empty($_POST['Contactsrelate_to'])) {
    $input .= "<input type='hidden' name='relate_to' value='{$_POST['Contactsrelate_to']}'>\n";
}
if (!empty($_POST['Contactsrelate_id'])) {
    $input .= "<input type='hidden' name='relate_id' value='{$_POST['Contactsrelate_id']}'>\n";
}


$emailAddress = new SugarEmailAddress();
$input .= $emailAddress->getEmailAddressWidgetDuplicatesView($contact);

$get = '';
if (!empty($_POST['return_module'])) {
    $xtpl->assign('RETURN_MODULE', $_POST['return_module']);
} else {
    $get .= "Contacts";
}
$get .= "&return_action=";
if (!empty($_POST['return_action'])) {
    $xtpl->assign('RETURN_ACTION', $_POST['return_action']);
} else {
    $get .= "DetailView";
}

///////////////////////////////////////////////////////////////////////////////
////	INBOUND EMAIL WORKFLOW
if (isset($_REQUEST['inbound_email_id'])) {
    $xtpl->assign('INBOUND_EMAIL_ID', $_REQUEST['inbound_email_id']);
    $xtpl->assign('RETURN_MODULE', 'Emails');
    $xtpl->assign('RETURN_ACTION', 'EditView');
    if (isset($_REQUEST['start'])) {
        $xtpl->assign('START', $_REQUEST['start']);
    }
}
////	END INBOUND EMAIL WORKFLOW
///////////////////////////////////////////////////////////////////////////////



if (!empty($_POST['popup'])) {
    $input .= '<input type="hidden" name="popup" value="'.$_POST['popup'].'">';
} else {
    $input .= '<input type="hidden" name="popup" value="false">';
}

if (!empty($_POST['to_pdf'])) {
    $input .= '<input type="hidden" name="to_pdf" value="'.$_POST['to_pdf'].'">';
} else {
    $input .= '<input type="hidden" name="to_pdf" value="false">';
}

if (!empty($_POST['create'])) {
    $input .= '<input type="hidden" name="create" value="'.$_POST['create'].'">';
} else {
    $input .= '<input type="hidden" name="create" value="false">';
}

if (!empty($_POST['return_id'])) {
    $xtpl->assign('RETURN_ID', $_POST['return_id']);
}

$xtpl->assign('INPUT_FIELDS', $input);
$xtpl->parse('main');
$xtpl->out('main');
