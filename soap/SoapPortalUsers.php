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


require_once('soap/SoapHelperFunctions.php');
require_once('soap/SoapTypes.php');


require_once('soap/SoapPortalHelper.php');




/******

THIS IS FOR PORTAL USERS


*************************************************************************************/
/*
this authenticates a user as a portal user and returns the session id or it returns false otherwise;
*/
$server->register(
    'portal_login',
    array('portal_auth'=>'tns:user_auth','user_name'=>'xsd:string', 'application_name'=>'xsd:string'),
    array('return'=>'tns:set_entry_result'),
    $NAMESPACE
);

function portal_login($portal_auth, $user_name, $application_name)
{
    $error = new SoapError();
    $contact = BeanFactory::newBean('Contacts');
    $result = login_user($portal_auth);

    if ($result == 'fail' || $result == 'sessions_exceeded') {
        if ($result == 'sessions_exceeded') {
            $error->set_error('sessions_exceeded');
        } else {
            $error->set_error('no_portal');
        }
        return array('id'=>-1, 'error'=>$error->get_soap_array());
    }
    global $current_user;

    if ($user_name == 'lead') {
        session_start();
        $_SESSION['is_valid_session']= true;
        $_SESSION['ip_address'] = query_client_ip();
        $_SESSION['portal_id'] = $current_user->id;
        $_SESSION['type'] = 'lead';
        login_success();
        return array('id'=>session_id(), 'error'=>$error->get_soap_array());
    } elseif ($user_name == 'portal') {
        session_start();
        $_SESSION['is_valid_session']= true;
        $_SESSION['ip_address'] = query_client_ip();
        $_SESSION['portal_id'] = $current_user->id;
        $_SESSION['type'] = 'portal';
        $GLOBALS['log']->debug("Saving new session");
        login_success();
        return array('id'=>session_id(), 'error'=>$error->get_soap_array());
    }
    $contact = $contact->retrieve_by_string_fields(array('portal_name'=>$user_name, 'portal_active'=>'1', 'deleted'=>0));
    if ($contact != null) {
        session_start();
        $_SESSION['is_valid_session']= true;
        $_SESSION['ip_address'] = query_client_ip();
        $_SESSION['user_id'] = $contact->id;
        $_SESSION['portal_id'] = $current_user->id;

        $_SESSION['type'] = 'contact';
        $_SESSION['assigned_user_id'] = $contact->assigned_user_id;
        login_success();
        build_relationship_tree($contact);
        return array('id'=>session_id(), 'error'=>$error->get_soap_array());
    }

    $error->set_error('invalid_login');
    return array('id'=>-1, 'error'=>$error->get_soap_array());
}

/*
this validates the session and starts the session;
*/
function portal_validate_authenticated($session_id)
{
    $old_error_reporting = error_reporting();
    error_reporting(0);
    session_id($session_id);

    // This little construct checks to see if the session validated
    if (session_start()) {
        $valid_session = true;

        if (!empty($_SESSION['is_valid_session']) && $_SESSION['ip_address'] == query_client_ip() && $valid_session != null && ($_SESSION['type'] == 'contact' || $_SESSION['type'] == 'lead' || $_SESSION['type'] == 'portal')) {
            global $current_user;
            $current_user = BeanFactory::newBean('Users');
            $current_user->retrieve($_SESSION['portal_id']);
            login_success();
            return true;
        }
    }
    session_destroy();
    $GLOBALS['log']->fatal('SECURITY: The session ID is invalid');
    return false;
}


$server->register(
    'portal_logout',
    array('session'=>'xsd:string'),
    array('return'=>'tns:error_value'),
    $NAMESPACE
);
function portal_logout($session)
{
    $error = new SoapError();
    if (portal_validate_authenticated($session)) {
        session_destroy();
        return $error->get_soap_array();
    }
    $error->set_error('invalid_session');
    return $error->get_soap_array();
}

$server->register(
    'portal_get_sugar_id',
    array('session'=>'xsd:string'),
    array('return'=>'tns:set_entry_result'),
    $NAMESPACE
);
function portal_get_sugar_id($session)
{
    $error = new SoapError();
    if (portal_validate_authenticated($session)) {
        return array('id'=>$_SESSION['portal_id'], 'error'=>$error->get_soap_array());
    }
    $error->set_error('invalid_session');
    return array('id'=>-1, 'error'=>$error->get_soap_array());
}

$server->register(
    'portal_get_sugar_contact_id',
    array('session'=>'xsd:string'),
    array('return'=>'tns:set_entry_result'),
    $NAMESPACE
);
function portal_get_sugar_contact_id($session)
{
    $error = new SoapError();
    if (portal_validate_authenticated($session)) {
        return array('id'=>$_SESSION['user_id'], 'error'=>$error->get_soap_array());
    }
    $error->set_error('invalid_session');
    return array('id'=>-1, 'error'=>$error->get_soap_array());
}


$server->register(
    'portal_get_entry_list',
    array('session'=>'xsd:string', 'module_name'=>'xsd:string','where'=>'xsd:string', 'order_by'=>'xsd:string', 'select_fields'=>'tns:select_fields'),
    array('return'=>'tns:get_entry_list_result'),
    $NAMESPACE
);

function portal_get_entry_list($session, $module_name, $where, $order_by, $select_fields)
{
    return portal_get_entry_list_limited($session, $module_name, $where, $order_by, $select_fields, 0, "");
}

/*
 * Acts like a normal get_entry_list except it will build the where clause based on the name_value pairs passed
 * Here we assume 'AND'
 */
$server->register(
    'portal_get_entry_list_filter',
    array('session'=>'xsd:string', 'module_name'=>'xsd:string', 'order_by'=>'xsd:string', 'select_fields'=>'tns:select_fields', 'row_offset' => 'xsd:int', 'limit'=>'xsd:int', 'filter' =>'tns:name_value_operator_list'),
    array('return'=>'tns:get_entry_list_result'),
    $NAMESPACE
);


function portal_get_entry_list_filter($session, $module_name, $order_by, $select_fields, $row_offset, $limit, $filter)
{
    global  $beanList, $beanFiles, $portal_modules;
    $error = new SoapError();
    if (! portal_validate_authenticated($session)) {
        $error->set_error('invalid_session');
        return array('result_count'=>-1, 'entry_list'=>array(), 'error'=>$error->get_soap_array());
    }
    if ($_SESSION['type'] == 'lead') {
        $error->set_error('no_access');
        return array('result_count'=>-1, 'entry_list'=>array(), 'error'=>$error->get_soap_array());
    }
    if (empty($beanList[$module_name])) {
        $error->set_error('no_module');
        return array('result_count'=>-1, 'entry_list'=>array(), 'error'=>$error->get_soap_array());
    }

    //build the where clause

    $sugar = null;
    if ($module_name == 'Cases') {
        $sugar = BeanFactory::newBean('Cases');
    } elseif ($module_name == 'Contacts') {
        $sugar = BeanFactory::newBean('Contacts');
    } elseif ($module_name == 'Accounts') {
        $sugar = BeanFactory::newBean('Accounts');
    } elseif ($module_name == 'Bugs') {
        $sugar = BeanFactory::newBean('Bugs');
    } elseif ($module_name == 'KBDocuments' || $module_name == 'FAQ') {
        $sugar = new KBDocument();
    } else {
        $error->set_error('no_module_support');
        return array('result_count'=>-1, 'entry_list'=>array(), 'error'=>$error->get_soap_array());
    }

    if ($sugar != null) {
        if (isset($filter) && is_array($filter)) {
            $where = "";
            foreach ($filter as $nvOp) {
                $name = $nvOp['name'];
                $value = $nvOp['value'];
                $value_array = $nvOp['value_array'];
                $operator = $nvOp['operator'];
                //do nothing if all three values are not set
                if (isset($name) && (isset($value) || isset($value_array)) && isset($operator)) {
                    if (!empty($where)) {
                        $where .= ' AND ';
                    }
                    if (isset($sugar->field_defs[$name])) {
                        // MFH - Added Support For Custom Fields in Searches
                        $cstm = isset($sugar->field_defs[$name]['source']) && $sugar->field_defs[$name]['source'] == 'custom_fields' ? '_cstm' : '';

                        $where .=  "$sugar->table_name$cstm.$name $operator ";
                        if ($sugar->field_defs['name']['type'] == 'datetime') {
                            $where .= DBManagerFactory::getInstance()->convert("'".DBManagerFactory::getInstance()->quote($value)."'", 'datetime');
                        } else {
                            if (empty($value)) {
                                $tmp = array();
                                foreach ($value_array as $v) {
                                    $tmp[] = DBManagerFactory::getInstance()->quote($v);
                                }
                                $where .= "('" . implode("', '", $tmp) . "')";
                            } else {
                                $where .= "'".DBManagerFactory::getInstance()->quote($value)."'";
                            }
                        }
                    }
                }
            }
        }
        return portal_get_entry_list_limited($session, $module_name, $where, $order_by, $select_fields, $row_offset, $limit);
    }
    $error->set_error('no_module_support');
    return array('result_count'=>-1, 'entry_list'=>array(), 'error'=>$error->get_soap_array());
}


$server->register(
    'portal_get_entry',
    array('session'=>'xsd:string', 'module_name'=>'xsd:string', 'id'=>'xsd:string', 'select_fields'=>'tns:select_fields'),
    array('return'=>'tns:get_entry_result'),
    $NAMESPACE
);

function portal_get_entry($session, $module_name, $id, $select_fields)
{
    global  $beanList, $beanFiles;
    $error = new SoapError();

    if (!portal_validate_authenticated($session)) {
        $error->set_error('invalid_session');
        return array('result_count'=>-1, 'entry_list'=>array(), 'error'=>$error->get_soap_array());
    }

    //set the working module
    set_module_in(array('list'=>array($id=>$id), 'in'=>'('.$id.')'), $module_name);

    if ($_SESSION['type'] == 'lead') {
        $error->set_error('no_access');
        return array('result_count'=>-1, 'entry_list'=>array(), 'error'=>$error->get_soap_array());
    }
    if (empty($beanList[$module_name])) {
        $error->set_error('no_module');
        return array('result_count'=>-1, 'entry_list'=>array(), 'error'=>$error->get_soap_array());
    }

    if (empty($_SESSION['viewable'][$module_name][$id])) {
        $error->set_error('no_access');
        return array('result_count'=>-1, 'entry_list'=>array(), 'error'=>$error->get_soap_array());
    }

    $class_name = $beanList[$module_name];
    require_once($beanFiles[$class_name]);
    $seed = new $class_name();
    $seed->retrieve($id);
    if ($module_name == 'KBDocuments') {
        $body = $seed->get_kbdoc_body($id);
        $seed->description = $body;
    }

    $output_list = array();
    $output_list[] = get_return_value($seed, $module_name);

    //$output_list[0]['name_value_list']['description'] = array('name'=>'description', 'value'=>$seed->description);
    //$output_list = filter_return_list($output_list, $select_fields, $module_name);
    $field_list = array();
    if (empty($field_list)) {
        $field_list = get_field_list($seed, true);
    }
    $output_list = filter_return_list($output_list, $select_fields, $module_name);
    $field_list = filter_field_list($field_list, $select_fields, $module_name);

    return array('field_list'=>$field_list, 'entry_list'=>$output_list, 'error'=>$error->get_soap_array());
}


$server->register(
    'portal_set_entry',
    array('session'=>'xsd:string', 'module_name'=>'xsd:string',  'name_value_list'=>'tns:name_value_list'),
    array('return'=>'tns:set_entry_result'),
    $NAMESPACE
);

function portal_set_entry($session, $module_name, $name_value_list)
{
    global  $beanList, $beanFiles, $valid_modules_for_contact;

    $error = new SoapError();
    if (!portal_validate_authenticated($session)) {
        $error->set_error('invalid_session');
        return array('id'=>-1,  'error'=>$error->get_soap_array());
    }
    if (empty($beanList[$module_name])) {
        $error->set_error('no_module');
        return array('id'=>-1, 'error'=>$error->get_soap_array());
    }
    if ($_SESSION['type'] == 'lead' && $module_name != 'Leads') {
        $error->set_error('no_access');
        return array('id'=>-1, 'error'=>$error->get_soap_array());
    }

    if ($_SESSION['type'] == 'contact' && !array_key_exists($module_name, $valid_modules_for_contact)) {
        $error->set_error('no_access');
        return array('id'=>-1, 'error'=>$error->get_soap_array());
    }


    $class_name = $beanList[$module_name];
    require_once($beanFiles[$class_name]);
    $seed = new $class_name();
    $is_update = false;
    $values_set = array();

    foreach ($name_value_list as $value) {
        if ($value['name'] == 'id' && !empty($value['value'])) {
            $seed->disable_row_level_security = true;
            $seed->retrieve($value['value']);
            $is_update = true;
            break;
        }
        $values_set[$value['name']] = $value['value'];
        $seed->{$value['name']} = $value['value'];
    }

    // If it was an update, we have to set the values again
    if ($is_update) {
        foreach ($name_value_list as $value) {
            $seed->{$value['name']} = $value['value'];
        }
    }

    if (!isset($_SESSION['viewable'][$module_name])) {
        $_SESSION['viewable'][$module_name] = array();
    }

    if (!$is_update) {
        if (isset($_SESSION['assigned_user_id']) && (!array_key_exists('assigned_user_id', $values_set) || empty($values_set['assigned_user_id']))) {
            $seed->assigned_user_id = $_SESSION['assigned_user_id'];
        }
        if (isset($_SESSION['account_id']) && (!array_key_exists('account_id', $values_set) || empty($values_set['account_id']))) {
            $seed->account_id = $_SESSION['account_id'];
        }
        $seed->portal_flag = 1;
        $seed->portal_viewable = true;
    }
    $id = $seed->save();
    set_module_in(array('in'=>"('".DBManagerFactory::getInstance()->quote($id)."')", 'list'=>array($id)), $module_name);
    if ($_SESSION['type'] == 'contact' && $module_name != 'Contacts' && !$is_update) {
        if ($module_name == 'Notes') {
            $seed->contact_id = $_SESSION['user_id'];
            if (isset($_SESSION['account_id'])) {
                $seed->parent_type = 'Accounts';
                $seed->parent_id = $_SESSION['account_id'];
            }
            $id = $seed->save();
        } else {
            $contact = BeanFactory::newBean('Contacts');
            $contact->disable_row_level_security = true;
            $contact->retrieve($_SESSION['user_id']);
            $seed->contact_id = $contact;

            if (isset($_SESSION['account_id'])) {
                $seed->account_id = $_SESSION['account_id'];
            }
            $seed->save_relationship_changes(false);
        }
    }
    return array('id'=>$id, 'error'=>$error->get_soap_array());
}



/*

NOTE SPECIFIC CODE
*/
$server->register(
    'portal_set_note_attachment',
    array('session'=>'xsd:string','note'=>'tns:note_attachment'),
    array('return'=>'tns:set_entry_result'),
    $NAMESPACE
);

function portal_set_note_attachment($session, $note)
{
    $error = new SoapError();
    if (!portal_validate_authenticated($session)) {
        $error->set_error('invalid_session');
        return array('id'=>'-1', 'error'=>$error->get_soap_array());
    }
    if ($_SESSION['type'] == 'lead' || !isset($_SESSION['viewable']['Notes'][$note['id']])) {
        $error->set_error('no_access');
        return array('id'=>-1, 'error'=>$error->get_soap_array());
    }
    require_once('modules/Notes/NoteSoap.php');
    $ns = new NoteSoap();
    $id = $ns->saveFile($note, true);
    return array('id'=>$id, 'error'=>$error->get_soap_array());
}

$server->register(
    'portal_remove_note_attachment',
    array('session'=>'xsd:string', 'id'=>'xsd:string'),
    array('return'=>'tns:error_value'),
    $NAMESPACE
);

function portal_remove_note_attachment($session, $id)
{
    $error = new SoapError();
    if (! portal_validate_authenticated($session)) {
        $error->set_error('invalid_session');
        return array('result_count'=>-1, 'entry_list'=>array(), 'error'=>$error->get_soap_array());
    }
    if ($_SESSION['type'] == 'lead' || !isset($_SESSION['viewable']['Notes'][$id])) {
        $error->set_error('no_access');
        return array('result_count'=>-1, 'entry_list'=>array(), 'error'=>$error->get_soap_array());
    }

    $focus = BeanFactory::newBean('Notes');
    $focus->retrieve($id);
    $result = $focus->deleteAttachment();

    return $error->get_soap_array();
}

$server->register(
    'portal_get_note_attachment',
    array('session'=>'xsd:string', 'id'=>'xsd:string'),
    array('return'=>'tns:return_note_attachment'),
    $NAMESPACE
);

function portal_get_note_attachment($session, $id)
{
    $error = new SoapError();
    if (! portal_validate_authenticated($session)) {
        $error->set_error('invalid_session');
        return array('result_count'=>-1, 'entry_list'=>array(), 'error'=>$error->get_soap_array());
    }
    if ($_SESSION['type'] == 'lead' || !isset($_SESSION['viewable']['Notes'][$id])) {
        $error->set_error('no_access');
        return array('result_count'=>-1, 'entry_list'=>array(), 'error'=>$error->get_soap_array());
    }
    $current_user = $seed_user;

    $note = BeanFactory::newBean('Notes');
    $note->retrieve($id);
    require_once('modules/Notes/NoteSoap.php');
    $ns = new NoteSoap();
    if (!isset($note->filename)) {
        $note->filename = '';
    }
    $file= $ns->retrieveFile($id, $note->filename);
    if ($file == -1) {
        $error->set_error('no_file');
        $file = '';
    }

    return array('note_attachment'=>array('id'=>$id, 'filename'=>$note->filename, 'file'=>$file), 'error'=>$error->get_soap_array());
}
$server->register(
    'portal_relate_note_to_module',
    array('session'=>'xsd:string', 'note_id'=>'xsd:string', 'module_name'=>'xsd:string', 'module_id'=>'xsd:string'),
    array('return'=>'tns:error_value'),
    $NAMESPACE
);

function portal_relate_note_to_module($session, $note_id, $module_name, $module_id)
{
    global  $beanList, $beanFiles, $current_user;
    $error = new SoapError();
    if (! portal_validate_authenticated($session)) {
        $error->set_error('invalid_session');
        return $error->get_soap_array();
    }
    if ($_SESSION['type'] == 'lead' || !isset($_SESSION['viewable']['Notes'][$note_id]) || !isset($_SESSION['viewable'][$module_name][$module_id])) {
        $error->set_error('no_access');
        return $error->get_soap_array();
    }
    if (empty($beanList[$module_name])) {
        $error->set_error('no_module');
        return $error->get_soap_array();
    }

    $class_name = $beanList[$module_name];
    require_once($beanFiles[$class_name]);

    $seed = new $class_name();
    $seed->retrieve($module_id);
    if ($module_name == 'Cases' || $module_name == 'Bugs') {
        $seed->note_id =  $note_id;
        $seed->save(false);
    } else {
        $error->set_error('no_module_support');
        $error->description .= ': '. $module_name;
    }
    return $error->get_soap_array();
}
$server->register(
    'portal_get_related_notes',
    array('session'=>'xsd:string', 'module_name'=>'xsd:string', 'module_id'=>'xsd:string',  'select_fields'=>'tns:select_fields', 'order_by'=>'xsd:string'),
    array('return'=>'tns:get_entry_result'),
    $NAMESPACE
);

function portal_get_related_notes($session, $module_name, $module_id, $select_fields, $order_by)
{
    global  $beanList, $beanFiles;
    $error = new SoapError();
    if (! portal_validate_authenticated($session)) {
        $error->set_error('invalid_session');
        return array('result_count'=>-1, 'entry_list'=>array(), 'error'=>$error->get_soap_array());
    }
    if ($_SESSION['type'] == 'lead') {
        $error->set_error('no_access');
        return array('result_count'=>-1, 'entry_list'=>array(), 'error'=>$error->get_soap_array());
    }
    if (empty($beanList[$module_name])) {
        $error->set_error('no_module');
        return array('result_count'=>-1, 'entry_list'=>array(), 'error'=>$error->get_soap_array());
    }
    if (empty($_SESSION['viewable'][$module_name][$module_id])) {
        $error->set_error('no_access');
        return array('result_count'=>-1, 'entry_list'=>array(), 'error'=>$error->get_soap_array());
    }

    if ($module_name =='Contacts') {
        if ($_SESSION['user_id'] != $module_id) {
            $error->set_error('no_access');
            return array('result_count'=>-1, 'entry_list'=>array(), 'error'=>$error->get_soap_array());
        }
        $list = get_notes_in_contacts("('".DBManagerFactory::getInstance()->quote($module_id)."')", $order_by);
    } else {
        $list = get_notes_in_module("('".DBManagerFactory::getInstance()->quote($module_id)."')", $module_name, $order_by);
    }



    $output_list = array();
    $field_list = array();
    foreach ($list as $value) {
        $output_list[] = get_return_value($value, 'Notes');
        $_SESSION['viewable']['Notes'][$value->id] = $value->id;
        if (empty($field_list)) {
            $field_list = get_field_list($value, true);
        }
    }
    $output_list = filter_return_list($output_list, $select_fields, $module_name);
    $field_list = filter_field_list($field_list, $select_fields, $module_name);


    return array('result_count'=>is_countable($output_list) ? count($output_list) : 0, 'next_offset'=>0,'field_list'=>$field_list, 'entry_list'=>$output_list, 'error'=>$error->get_soap_array());
}

$server->register(
    'portal_get_related_list',
    array('session'=>'xsd:string', 'module_name'=>'xsd:string', 'rel_module'=>'xsd:string', 'module_id'=>'xsd:string',  'select_fields'=>'tns:select_fields', 'order_by'=>'xsd:string', 'offset' => 'xsd:int', 'limit' => 'xsd:int'),
    array('return'=>'tns:get_entry_result'),
    $NAMESPACE
);

function portal_get_related_list($session, $module_name, $rel_module, $module_id, $select_fields, $order_by, $offset, $limit)
{
    global  $beanList, $beanFiles;
    $error = new SoapError();
    if (! portal_validate_authenticated($session)) {
        $error->set_error('invalid_session');
        return array('result_count'=>-1, 'entry_list'=>array(), 'error'=>$error->get_soap_array());
    }
    if ($_SESSION['type'] == 'lead') {
        $error->set_error('no_access');
        return array('result_count'=>-1, 'entry_list'=>array(), 'error'=>$error->get_soap_array());
    }
    if (empty($beanList[$module_name])) {
        $error->set_error('no_module');
        return array('result_count'=>-1, 'entry_list'=>array(), 'error'=>$error->get_soap_array());
    }
    if (empty($_SESSION['viewable'][$module_name][$module_id])) {
        $error->set_error('no_access');
        return array('result_count'=>-1, 'entry_list'=>array(), 'error'=>$error->get_soap_array());
    }

    $list = get_related_in_module("('".DBManagerFactory::getInstance()->quote($module_id)."')", $module_name, $rel_module, $order_by, $offset, $limit);

    $output_list = array();
    $field_list = array();
    foreach ($list as $value) {
        $output_list[] = get_return_value($value, $rel_module);
        $_SESSION['viewable'][$rel_module][$value->id] = $value->id;
        if (empty($field_list)) {
            $field_list = get_field_list($value, true);
        }
    }
    $output_list = filter_return_list($output_list, $select_fields, $module_name);
    $field_list = filter_field_list($field_list, $select_fields, $module_name);


    return array('result_count'=>$list['result_count'], 'next_offset'=>0,'field_list'=>$field_list, 'entry_list'=>$output_list, 'error'=>$error->get_soap_array());
}

$server->register(
    'portal_get_module_fields',
    array('session'=>'xsd:string', 'module_name'=>'xsd:string'),
    array('return'=>'tns:module_fields'),
    $NAMESPACE
);

function portal_get_module_fields($session, $module_name)
{
    global  $beanList, $beanFiles, $portal_modules, $valid_modules_for_contact;
    $error = new SoapError();
    $module_fields = array();
    if (! portal_validate_authenticated($session)) {
        $error->set_error('invalid_session');
        $error->description .=$session;
        return array('module_name'=>$module_name, 'module_fields'=>$module_fields, 'error'=>$error->get_soap_array());
    }
    if ($_SESSION['type'] == 'lead' && $module_name != 'Leads') {
        $error->set_error('no_access');
        return array('module_name'=>$module_name, 'module_fields'=>$module_fields, 'error'=>$error->get_soap_array());
    }

    if (empty($beanList[$module_name])) {
        $error->set_error('no_module');
        return array('module_name'=>$module_name, 'module_fields'=>$module_fields, 'error'=>$error->get_soap_array());
    }

    if (($_SESSION['type'] == 'portal'||$_SESSION['type'] == 'contact') &&  !array_key_exists($module_name, $valid_modules_for_contact)) {
        $error->set_error('no_module');
        return array('module_name'=>$module_name, 'module_fields'=>$module_fields, 'error'=>$error->get_soap_array());
    }

    $class_name = $beanList[$module_name];
    require_once($beanFiles[$class_name]);
    $seed = new $class_name();
    $seed->fill_in_additional_detail_fields();
    $returnFields = get_return_module_fields($seed, $module_name, $error->get_soap_array(), true);
    if (is_subclass_of($seed, 'Person')) {
        $returnFields['module_fields']['email1'] = array('name'=>'email1', 'type'=>'email', 'required'=>0, 'label'=>translate('LBL_EMAIL_ADDRESS', $seed->module_dir));
        $returnFields['module_fields']['email_opt_out'] = array('name'=>'email_opt_out', 'type'=>'bool', 'required'=>0, 'label'=>translate('LBL_EMAIL_OPT_OUT', $seed->module_dir), 'options'=>array());
    } //if

    return $returnFields;
}
$server->register(
    'portal_get_subscription_lists',
    array('session'=>'xsd:string'),
    array('return'=>'tns:get_subscription_lists_result'),
    $NAMESPACE
);

function portal_get_subscription_lists($session)
{
    global  $beanList, $beanFiles;

    $error = new SoapError();
    if (! portal_validate_authenticated($session)) {
        $error->set_error('invalid_session');
        return array('result_count'=>-1, 'entry_list'=>array(), 'error'=>$error->get_soap_array());
    }

    require_once('modules/Campaigns/utils.php');

    $contact = BeanFactory::newBean('Contacts');
    $contact->retrieve($_SESSION['user_id']);

    if (!empty($contact->id)) {
        $result = get_subscription_lists_keyed($contact, true);
    }


    $return_results = array('unsubscribed' => array(), 'subscribed' => array());

    foreach ($result['unsubscribed'] as $newsletter_name => $data) {
        $return_results['unsubscribed'][] = array('name' => $newsletter_name, 'prospect_list_id' => $data['prospect_list_id'],
                                                  'campaign_id' => $data['campaign_id'], 'description' => $data['description'],
                                                  'frequency' => $data['frequency']);
    }
    foreach ($result['subscribed'] as $newsletter_name => $data) {
        $return_results['subscribed'][] = array('name' => $newsletter_name, 'prospect_list_id' => $data['prospect_list_id'],
                                                'campaign_id' => $data['campaign_id'], 'description' => $data['description'],
                                                'frequency' => $data['frequency']);
    }

    return array('unsubscribed'=>$return_results['unsubscribed'], 'subscribed' => $return_results['subscribed'], 'error'=>$error->get_soap_array());
}

$server->register(
    'portal_set_newsletters',
    array('session'=>'xsd:string', 'subscribe_ids' => 'tns:select_fields', 'unsubscribe_ids' => 'tns:select_fields'),
    array('return'=>'tns:error_value'),
    $NAMESPACE
);

function portal_set_newsletters($session, $subscribe_ids, $unsubscribe_ids)
{
    global  $beanList, $beanFiles;

    $error = new SoapError();
    if (! portal_validate_authenticated($session)) {
        $error->set_error('invalid_session');
        return array('result_count'=>-1, 'entry_list'=>array(), 'error'=>$error->get_soap_array());
    }

    require_once('modules/Campaigns/utils.php');

    $contact = BeanFactory::newBean('Contacts');
    $contact->retrieve($_SESSION['user_id']);

    if (!empty($contact->id)) {
        foreach ($subscribe_ids as $campaign_id) {
            subscribe($campaign_id, null, $contact, true);
        }
        foreach ($unsubscribe_ids as $campaign_id) {
            unsubscribe($campaign_id, $contact);
        }
    }

    return $error->get_soap_array();
}
