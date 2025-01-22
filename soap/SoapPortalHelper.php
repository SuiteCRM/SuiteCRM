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

$portal_modules = array('Contacts', 'Accounts', 'Notes');
$portal_modules[] = 'Cases';
$portal_modules[] = 'Bugs';


/*
BUGS
*/





function get_bugs_in_contacts($in, $orderBy = '')
{
    //bail if the in is empty
    if (empty($in)  || $in =='()' || $in =="('')") {
        return;
    }
    // First, get the list of IDs.

    $query = "SELECT bug_id as id from contacts_bugs where contact_id IN $in AND deleted=0";
    if (!empty($orderBy)) {
        $query .= ' ORDER BY ' . $orderBy;
    }

    $sugar = BeanFactory::newBean('Contacts');
    set_module_in($sugar->build_related_in($query), 'Bugs');
}

function get_bugs_in_accounts($in, $orderBy = '')
{
    //bail if the in is empty
    if (empty($in)  || $in =='()' || $in =="('')") {
        return;
    }
    // First, get the list of IDs.

    $query = "SELECT bug_id as id from accounts_bugs where account_id IN $in AND deleted=0";
    if (!empty($orderBy)) {
        $query .= ' ORDER BY ' . $orderBy;
    }

    $sugar = BeanFactory::newBean('Accounts');

    set_module_in($sugar->build_related_in($query), 'Bugs');
}

/*
Cases
*/

function get_cases_in_contacts($in, $orderBy = '')
{
    //bail if the in is empty
    if (empty($in)  || $in =='()' || $in =="('')") {
        return;
    }
    // First, get the list of IDs.

    $query = "SELECT case_id as id from contacts_cases where contact_id IN $in AND deleted=0";
    if (!empty($orderBy)) {
        $query .= ' ORDER BY ' . $orderBy;
    }

    $sugar = BeanFactory::newBean('Contacts');
    set_module_in($sugar->build_related_in($query), 'Cases');
}

function get_cases_in_accounts($in, $orderBy = '')
{
    if (empty($_SESSION['viewable']['Accounts'])) {
        return;
    }
    //bail if the in is empty
    if (empty($in)  || $in =='()' || $in =="('')") {
        return;
    }
    // First, get the list of IDs.
    $query = "SELECT id  from cases where account_id IN $in AND deleted=0";
    if (!empty($orderBy)) {
        $query .= ' ORDER BY ' . $orderBy;
    }

    $sugar = BeanFactory::newBean('Accounts');
    set_module_in($sugar->build_related_in($query), 'Cases');
}



/*
NOTES
*/


function get_notes_in_contacts($in, $orderBy = '')
{
    //bail if the in is empty
    if (empty($in)  || $in =='()' || $in =="('')") {
        return;
    }
    // First, get the list of IDs.
    $query = "SELECT id from notes where contact_id IN $in AND deleted=0 AND portal_flag=1";
    if (!empty($orderBy)) {
        $query .= ' ORDER BY ' . $orderBy;
    }

    $contact = BeanFactory::newBean('Contacts');
    $note = BeanFactory::newBean('Notes');
    return $contact->build_related_list($query, $note);
}

function get_notes_in_module($in, $module, $orderBy = '')
{
    //bail if the in is empty
    if (empty($in)  || $in =='()' || $in =="('')") {
        return;
    }
    // First, get the list of IDs.
    $query = "SELECT id from notes where parent_id IN $in AND parent_type='$module' AND deleted=0 AND portal_flag = 1";
    if (!empty($orderBy)) {
        $query .= ' ORDER BY ' . $orderBy;
    }
    global $beanList, $beanFiles;

    if (!empty($beanList[$module])) {
        $class_name = $beanList[$module];
        require_once($beanFiles[$class_name]);
        $sugar = new $class_name();
    } else {
        return array();
    }

    $note = BeanFactory::newBean('Notes');
    return $sugar->build_related_list($query, $note);
}

    function get_related_in_module($in, $module, $rel_module, $orderBy = '', $row_offset = 0, $limit= -1)
    {
        global $beanList, $beanFiles;
        if (!empty($beanList[$rel_module])) {
            $class_name = $beanList[$rel_module];
            require_once($beanFiles[$class_name]);
            $rel = new $class_name();
        } else {
            return array();
        }

        //bail if the in is empty
        if (empty($in)  || $in =='()' || $in =="('')") {
            return;
        }

        // First, get the list of IDs.
        if ($module == 'KBDocuments' || $module == 'DocumentRevisions') {
            $query = "SELECT dr.* from document_revisions dr
                      inner join kbdocument_revisions kr on kr.document_revision_id = dr.id AND kr.kbdocument_id IN ($in)
                      AND dr.file_mime_type is not null";
        } else {
            $query = "SELECT id from $rel->table_name where parent_id IN $in AND parent_type='".DBManagerFactory::getInstance()->quote($module)."' AND deleted=0 AND portal_flag = 1";
        }

        if (!empty($orderBy)) {
            require_once 'include/SugarSQLValidate.php';
            $valid = new SugarSQLValidate();
            $fakeWhere = " 1=1 ";
            if ($valid->validateQueryClauses($fakeWhere, $orderBy)) {
                $query .= ' ORDER BY '. $orderBy;
            } else {
                $GLOBALS['log']->error("Bad order by: $orderBy");
            }
        }

        if (!empty($beanList[$module])) {
            $class_name = $beanList[$module];
            require_once($beanFiles[$class_name]);
            $sugar = new $class_name();
        } else {
            return array();
        }


        $count_query = $sugar->create_list_count_query($query);
        if (!empty($count_query)) {
            // We have a count query.  Run it and get the results.
            $result = $sugar->db->query($count_query, true, "Error running count query for $sugar->object_name List: ");
            $assoc = $sugar->db->fetchByAssoc($result);
            if (!empty($assoc['c'])) {
                $rows_found = $assoc['c'];
            }
        }
        $list = $sugar->build_related_list($query, $rel, $row_offset, $limit);
        $list['result_count'] = $rows_found;
        return $list;
    }

function get_accounts_from_contact($contact_id, $orderBy = '')
{
    // First, get the list of IDs.
    $query = "SELECT account_id as id from accounts_contacts where contact_id='".DBManagerFactory::getInstance()->quote($contact_id)."' AND deleted=0";
    if (!empty($orderBy)) {
        $query .= ' ORDER BY ' . $orderBy;
    }
    $sugar = BeanFactory::newBean('Contacts');
    set_module_in($sugar->build_related_in($query), 'Accounts');
}

function get_contacts_from_account($account_id, $orderBy = '')
{
    // First, get the list of IDs.
    $query = "SELECT contact_id as id from accounts_contacts where account_id='".DBManagerFactory::getInstance()->quote($account_id)."' AND deleted=0";
    if (!empty($orderBy)) {
        $query .= ' ORDER BY ' . $orderBy;
    }
    $sugar = BeanFactory::newBean('Accounts');
    set_module_in($sugar->build_related_in($query), 'Contacts');
}

function get_related_list($in, $template, $where, $order_by, $row_offset = 0, $limit = "")
{
    $q = '';
    //if $in is empty then pass in a query to get the list of related list
    if (empty($in)  || $in =='()' || $in =="('')") {
        $in = '';
        //build the query to pass into the template list function
        $q = 'select id from '.$template->table_name.' where deleted = 0 ';
        //add where statement if it is not empty
        if (!empty($where)) {
            require_once 'include/SugarSQLValidate.php';
            $valid = new SugarSQLValidate();
            if (!$valid->validateQueryClauses($where)) {
                $GLOBALS['log']->error("Bad query: $where");
                // No way to directly pass back an error.
                return array();
            }

            $q .= ' and ( '.$where.' ) ';
        }
    }

    return $template->build_related_list_where($q, $template, $where, $in, $order_by, $limit, $row_offset);
}

function build_relationship_tree($contact)
{
    global $sugar_config;
    $contact->retrieve($contact->id);

    get_accounts_from_contact($contact->id);

    set_module_in(array('list'=>array($contact->id), 'in'=> "('".DBManagerFactory::getInstance()->quote($contact->id)."')"), 'Contacts');

    $accounts = $_SESSION['viewable']['Accounts'];
    foreach ($accounts as $id) {
        if (!isset($sugar_config['portal_view']) || $sugar_config['portal_view'] != 'single_user') {
            get_contacts_from_account($id);
        }
    }
}

function get_contacts_in()
{
    return $_SESSION['viewable']['contacts_in'];
}

function get_accounts_in()
{
    return $_SESSION['viewable']['accounts_in'];
}

function get_module_in($module_name)
{
    if (!isset($_SESSION['viewable'][$module_name])) {
        return '()';
    }

    $module_name_in = array_keys($_SESSION['viewable'][$module_name]);
    $module_name_list = array();
    foreach ($module_name_in as $name) {
        $module_name_list[] = DBManagerFactory::getInstance()->quote($name);
    }

    $mod_in = "('" . implode("','", $module_name_list) . "')";
    $_SESSION['viewable'][strtolower($module_name).'_in'] = $mod_in;

    return $mod_in;
}

function set_module_in($arrayList, $module_name)
{
    if (!isset($_SESSION['viewable'][$module_name])) {
        $_SESSION['viewable'][$module_name] = array();
    }
    foreach ($arrayList['list'] as $id) {
        $_SESSION['viewable'][$module_name][$id] = $id;
    }
    if ($module_name == 'Accounts' && isset($id)) {
        $_SESSION['account_id'] = $id;
    }

    if (!empty($_SESSION['viewable'][strtolower($module_name).'_in'])) {
        if ($arrayList['in'] != '()') {
            $newList = array();
            if (is_array($_SESSION['viewable'][strtolower($module_name).'_in'])) {
                foreach ($_SESSION['viewable'][strtolower($module_name).'_in'] as $name) {
                    $newList[] = DBManagerFactory::getInstance()->quote($name);
                }
            }
            if (is_array($arrayList['list'])) {
                foreach ($arrayList['list'] as $name) {
                    $newList[] = DBManagerFactory::getInstance()->quote($name);
                }
            }
            $_SESSION['viewable'][strtolower($module_name).'_in'] = "('" . implode("', '", $newList) . "')";
        }
    } else {
        $_SESSION['viewable'][strtolower($module_name).'_in'] = $arrayList['in'];
    }
}

/*
 * Given the user auth, attempt to log the user in.
 * used by SoapPortalUsers.php
 */
function login_user($portal_auth)
{
    $error = new SoapError();
    $user = User::findUserPassword($portal_auth['user_name'], $portal_auth['password'], "portal_only='1' AND status = 'Active'");

    if (!empty($user)) {
        global $current_user;
        $bean = BeanFactory::newBean('Users');
        $bean->retrieve($user['id']);
        $current_user = $bean;
        return 'success';
    }
    $GLOBALS['log']->fatal('SECURITY: User authentication for '. $portal_auth['user_name']. ' failed');
    return 'fail';
}


function portal_get_entry_list_limited($session, $module_name, $where, $order_by, $select_fields, $row_offset, $limit)
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
    if ($module_name == 'Cases') {

        //if the related cases have not yet been loaded into the session object,
        //then call the methods that will load the cases related to the contact/accounts for this user
        if (!isset($_SESSION['viewable'][$module_name])) {
            //retrieve the contact/account id's for this user
            $c =get_contacts_in();
            $a = get_accounts_in();
            if (!empty($c)) {
                get_cases_in_contacts($c);
            }
            if (!empty($a)) {
                get_cases_in_accounts($a);
            }
        }

        $sugar = BeanFactory::newBean('Cases');

        $list = array();
        //if no Cases have been loaded into the session as viewable, then do not issue query, just return empty list
        //issuing a query with no cases loaded in session will return ALL the Cases, which is not a good thing
        if (!empty($_SESSION['viewable'][$module_name])) {
            $list =  get_related_list(get_module_in($module_name), BeanFactory::newBean('Cases'), $where, $order_by, $row_offset, $limit);
        }
    } elseif ($module_name == 'Contacts') {
        $sugar = BeanFactory::newBean('Contacts');
        $list =  get_related_list(get_module_in($module_name), BeanFactory::newBean('Contacts'), $where, $order_by);
    } elseif ($module_name == 'Accounts') {
        $sugar = BeanFactory::newBean('Accounts');
        $list =  get_related_list(get_module_in($module_name), BeanFactory::newBean('Accounts'), $where, $order_by);
    } elseif ($module_name == 'Bugs') {

        //if the related bugs have not yet been loaded into the session object,
        //then call the methods that will load the bugs related to the contact/accounts for this user
        if (!isset($_SESSION['viewable'][$module_name])) {
            //retrieve the contact/account id's for this user
            $c =get_contacts_in();
            $a = get_accounts_in();
            if (!empty($c)) {
                get_bugs_in_contacts($c);
            }
            if (!empty($a)) {
                get_bugs_in_accounts($a);
            }
        }

        $list = array();
        //if no Bugs have been loaded into the session as viewable, then do not issue query, just return empty list
        //issuing a query with no bugs loaded in session will return ALL the Bugs, which is not a good thing
        if (!empty($_SESSION['viewable'][$module_name])) {
            $list = get_related_list(get_module_in($module_name), BeanFactory::newBean('Bugs'), $where, $order_by, $row_offset, $limit);
        }
    } elseif ($module_name == 'KBDocuments') {
    } elseif ($module_name == 'FAQ') {
    } else {
        $error->set_error('no_module_support');
        return array('result_count'=>-1, 'entry_list'=>array(), 'error'=>$error->get_soap_array());
    }

    $output_list = array();
    $field_list = array();
    foreach ($list as $value) {

        //$loga->fatal("Adding another account to the list");
        $output_list[] = get_return_value($value, $module_name);
        $_SESSION['viewable'][$module_name][$value->id] = $value->id;
        if (empty($field_list)) {
            $field_list = get_field_list($value);
        }
    }
    $output_list = filter_return_list($output_list, $select_fields, $module_name);
    $field_list = filter_field_list($field_list, $select_fields, $module_name);

    return array('result_count'=>is_countable($output_list) ? count($output_list) : 0, 'next_offset'=>0,'field_list'=>$field_list, 'entry_list'=>$output_list, 'error'=>$error->get_soap_array());
}

$invalid_contact_fields = array('portal_password'=>1, 'portal_active'=>1);
$valid_modules_for_contact = array('Contacts'=>1, 'Cases'=>1, 'Notes'=>1, 'Bugs'=>1, 'Accounts'=>1, 'Leads'=>1, 'KBDocuments'=>1);
