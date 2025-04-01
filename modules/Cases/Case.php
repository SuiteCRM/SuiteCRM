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

// Case is used to store customer information.
#[\AllowDynamicProperties]
class aCase extends Basic
{
    public $field_name_map = array();
    // Stored fields
    public $id;
    public $date_entered;
    public $date_modified;
    public $modified_user_id;
    public $assigned_user_id;
    public $case_number;
    public $resolution;
    public $description;
    public $name;
    public $status;
    public $priority;
    public $state;
    public $update_text;
    public $internal;
    public $send_closure_email;

    public $created_by;
    public $created_by_name;
    public $modified_by_name;

    /**
     * @var Link2
     */
    public $contacts;

    // These are related
    public $bug_id;
    public $account_name;
    public $account_id;
    public $contact_id;
    public $task_id;
    public $note_id;
    public $meeting_id;
    public $call_id;
    public $email_id;
    public $assigned_user_name;
    public $account_name1;

    public $table_name = 'cases';
    public $rel_account_table = 'accounts_cases';
    public $rel_contact_table = 'contacts_cases';
    public $module_dir = 'Cases';
    public $object_name = 'Case';
    public $importable = true;
    public $new_schema = true;
    /** "%1" is the case_number, for emails
     * leave the %1 in if you customize this.
     * YOU MUST LEAVE THE BRACKETS AS WELL*/
    public $emailSubjectMacro = '[CASE:%1]';

    // This is used to retrieve related fields from form posts.
    public $additional_column_fields = array(
        'bug_id',
        'assigned_user_name',
        'assigned_user_id',
        'contact_id',
        'task_id',
        'note_id',
        'meeting_id',
        'call_id',
        'email_id',
    );

    public $relationship_fields = array(
        'account_id' => 'accounts',
        'bug_id'     => 'bugs',
        'task_id'    => 'tasks',
        'note_id'    => 'notes',
        'meeting_id' => 'meetings',
        'call_id'    => 'calls',
        'email_id'   => 'emails',
    );

    /**
     * aCase constructor.
     */
    public function __construct()
    {
        parent::__construct();
        global $sugar_config;
        if (!$sugar_config['require_accounts']) {
            unset($this->required_fields['account_name']);
        }

        $this->setupCustomFields('Cases');
        foreach ($this->field_defs as $name => $field) {
            $this->field_name_map[$name] = $field;
        }
    }

    /**
     * @return string
     */
    public function get_summary_text()
    {
        return (string)$this->name;
    }

    /**
     * @return string[]
     */
    public function listviewACLHelper()
    {
        $array_assign = parent::listviewACLHelper();
        $is_owner = false;
        $in_group = false; //SECURITY GROUPS
        if (!empty($this->account_id)) {
            if (!empty($this->account_id_owner)) {
                global $current_user;
                $is_owner = $current_user->id === $this->account_id_owner;
            } else {
                global $current_user;
                $parent_bean = BeanFactory::getBean('Accounts', $this->account_id);
                if ($parent_bean !== false) {
                    $is_owner = $current_user->id === $parent_bean->assigned_user_id;
                }
            }
            require_once 'modules/SecurityGroups/SecurityGroup.php';
            $in_group = SecurityGroup::groupHasAccess('Accounts', $this->account_id, 'view');
        }
        if (!ACLController::moduleSupportsACL('Accounts') ||
            ACLController::checkAccess('Accounts', 'view', $is_owner, 'module', $in_group)
        ) {
            $array_assign['ACCOUNT'] = 'a';
        } else {
            $array_assign['ACCOUNT'] = 'span';
        }

        return $array_assign;
    }

    /**
     * @param bool $is_update
     * @param array $exclude
     */
    public function save_relationship_changes($is_update, $exclude = array())
    {
        parent::save_relationship_changes($is_update, $exclude);

        if (!empty($this->contact_id)) {
            $this->set_case_contact_relationship($this->contact_id);
        }
    }

    /**
     * @param $contact_id
     */
    public function set_case_contact_relationship($contact_id)
    {
        global $app_list_strings;
        $default = $app_list_strings['case_relationship_type_default_key'];
        $this->load_relationship('contacts');
        $this->contacts->add($contact_id, array('contact_role' => $default));
    }

    public function fill_in_additional_detail_fields()
    {
        parent::fill_in_additional_detail_fields();
        // Fill in the assigned_user_name
        $this->assigned_user_name = get_assigned_user_name($this->assigned_user_id);

        $this->created_by_name = get_assigned_user_name($this->created_by);
        $this->modified_by_name = get_assigned_user_name($this->modified_user_id);

        if (!empty($this->id)) {
            $account_info = $this->getAccount($this->id);
            if (!empty($account_info)) {
                $this->account_name = $account_info['account_name'];
                $this->account_id = $account_info['account_id'];
            }
        }
    }

    /** Returns a list of the associated contacts
     * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc..
     * All Rights Reserved..
     * Contributor(s): ______________________________________..
     */
    public function get_contacts()
    {
        $this->load_relationship('contacts');
        $query_array=$this->contacts->getQuery();

        //update the select clause in the returned query.

        if (!is_array($query_array)) {
            LoggerManager::getLogger()->fatal('Building database selection for contacts but the query information format is not an array.');
            return false;
        }

        $query_array['select'] =
            'SELECT contacts.id, contacts.first_name, contacts.last_name, contacts.title, contacts.email1, contacts.phone_work, contacts_cases.contact_role as case_role, contacts_cases.id as case_rel_id ';

        $query = '';
        foreach ($query_array as $qString) {
            $query .= ' ' . $qString;
        }
        $temp = array('id', 'first_name', 'last_name', 'title', 'email1', 'phone_work', 'case_role', 'case_rel_id');

        return $this->build_related_list2($query, BeanFactory::newBean('Contacts'), $temp);
    }

    /**
     * @return array
     */
    public function get_list_view_data()
    {
        global $current_language;
        $app_list_strings = return_app_list_strings_language($current_language);

        $temp_array = $this->get_list_view_array();
        $temp_array['NAME'] = empty($this->name) ? '<em>blank</em>' : $this->name;
        $temp_array['PRIORITY'] =
            empty($this->priority) ? '' :
                (!isset($app_list_strings[$this->field_name_map['priority']['options']][$this->priority]) ?
                    $this->priority : $app_list_strings[$this->field_name_map['priority']['options']][$this->priority]);
        $temp_array['STATUS'] =
            empty($this->status) ? '' :
                (!isset($app_list_strings[$this->field_name_map['status']['options']][$this->status]) ? $this->status :
                    $app_list_strings[$this->field_name_map['status']['options']][$this->status]);
        $temp_array['ENCODED_NAME'] = $this->name;
        $temp_array['CASE_NUMBER'] = $this->case_number;
        $temp_array['SET_COMPLETE'] =
            "<a href='index.php?return_module=Home&return_action=index&action=EditView&module=Cases&record=$this->id&status=Closed'>" .
            SugarThemeRegistry::current()->getImage(
                'close_inline',
                'title=' . translate('LBL_LIST_CLOSE', 'Cases') . " border='0'",
                null,
                null,
                '.gif',
                translate('LBL_LIST_CLOSE', 'Cases')
            ) .
            '</a>';

        //$temp_array['ACCOUNT_NAME'] = $this->account_name; //overwrites the account_name value returned from the cases table.
        return $temp_array;
    }

    /**
     * builds a generic search based on the query string using or
     * do not include any $this-> because this is called on without having the class instantiated.
     *
     * @param $the_query_string
     *
     * @return string|void
     */
    public function build_generic_where_clause($the_query_string)
    {
        $where_clauses = array();
        $the_query_string = $this->db->quote($the_query_string);
        $where_clauses[] = "cases.name like '$the_query_string%'";
        $where_clauses[] = "accounts.name like '$the_query_string%'";

        if (is_numeric($the_query_string)) {
            $where_clauses[] = "cases.case_number like '$the_query_string%'";
        }

        $the_where = '';

        foreach ($where_clauses as $clause) {
            if ($the_where !== '') {
                $the_where .= ' or ';
            }
            $the_where .= $clause;
        }

        if ($the_where !== '') {
            $the_where = '(' . $the_where . ')';
        }

        return $the_where;
    }

    /**
     * @param Sugar_Smarty $xtpl
     * @param aCase $case
     *
     * @return mixed
     */
    public function set_notification_body($xtpl, $case)
    {
        global $app_list_strings;

        $xtpl->assign('CASE_NUMBER', $case->case_number);
        $xtpl->assign('CASE_SUBJECT', $case->name);
        $xtpl->assign(
            'CASE_PRIORITY',
            (isset($case->priority)  ? $app_list_strings['case_priority_dom'][$case->priority] : '')
        );
        $xtpl->assign('CASE_STATUS', (isset($case->status) ? $app_list_strings['case_status_dom'][$case->status] : ''));
        $xtpl->assign('CASE_DESCRIPTION', nl2br($case->description));

        return $xtpl;
    }

    /**
     * @param $interface
     *
     * @return bool
     */
    public function bean_implements($interface)
    {
        switch ($interface) {
            case 'ACL':
                return true;
        }

        return false;
    }

    /**
     * retrieves the Subject line macro for InboundEmail parsing.
     *
     * @return string
     */
    public function getEmailSubjectMacro()
    {
        global $sugar_config;

        return (isset($sugar_config['inbound_email_case_subject_macro']) &&
                !empty($sugar_config['inbound_email_case_subject_macro'])) ?
            $sugar_config['inbound_email_case_subject_macro'] : $this->emailSubjectMacro;
    }

    /**
     * @param $case_id
     *
     * @return array
     */
    public function getAccount($case_id)
    {
        if (empty($case_id)) {
            return array();
        }
        $ret_array = array();
        $query =
            "SELECT acc.id, acc.name FROM accounts  acc, cases  WHERE acc.id = cases.account_id AND cases.id = '" .
            $case_id .
            "' AND cases.deleted=0 AND acc.deleted=0";
        $result = $this->db->query($query, true, ' Error filling in additional detail fields: ');

        // Get the id and the name.
        $row = $this->db->fetchByAssoc($result);

        if ($row !== null && $row !== false) {
            $ret_array['account_name'] = stripslashes($row['name']);
            $ret_array['account_id'] = $row['id'];
        } else {
            $ret_array['account_name'] = '';
            $ret_array['account_id'] = '';
        }

        return $ret_array;
    }
}
