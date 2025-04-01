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

/*********************************************************************************
 * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
 * All Rights Reserved.
 * Contributor(s): ______________________________________..
 ********************************************************************************/

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once('include/SugarObjects/templates/person/Person.php');
require_once __DIR__ . '/../../include/EmailInterface.php';

// Contact is used to store customer information.
#[\AllowDynamicProperties]
class Contact extends Person implements EmailInterface
{
    public $field_name_map;
    // Stored fields
    public $id;
    public $name = '';
    public $lead_source;
    public $date_entered;
    public $date_modified;
    public $modified_user_id;
    public $assigned_user_id;
    public $created_by;
    public $created_by_name;
    public $modified_by_name;

    public $description;
    public $salutation;
    public $first_name;
    public $last_name;
    public $title;
    public $department;
    public $birthdate;
    public $reports_to_id;
    public $do_not_call;
    public $phone_home;
    public $phone_mobile;
    public $phone_work;
    public $phone_other;
    public $phone_fax;
    public $email1;
    public $email_and_name1;
    public $email_and_name2;
    public $email2;
    public $assistant;
    public $assistant_phone;
    public $email_opt_out;
    public $primary_address_street;
    public $primary_address_city;
    public $primary_address_state;
    public $primary_address_postalcode;
    public $primary_address_country;
    public $alt_address_street;
    public $alt_address_city;
    public $alt_address_state;
    public $alt_address_postalcode;
    public $alt_address_country;
    public $portal_name;
    public $portal_app;
    public $portal_active;
    public $contacts_users_id;
    // These are for related fields
    public $bug_id;
    public $account_name;
    public $account_id;
    public $report_to_name;
    public $opportunity_role;
    public $opportunity_rel_id;
    public $opportunity_id;
    public $case_role;
    public $case_rel_id;
    public $case_id;
    public $task_id;
    public $note_id;
    public $meeting_id;
    public $call_id;
    public $email_id;
    public $assigned_user_name;
    public $accept_status;
    public $accept_status_id;
    public $accept_status_name;
    public $alt_address_street_2;
    public $alt_address_street_3;
    public $opportunity_role_id;
    public $portal_password;
    public $primary_address_street_2;
    public $primary_address_street_3;
    public $campaign_id;
    public $sync_contact;
    public $full_name; // l10n localized name
    public $invalid_email;
    public $table_name = "contacts";
    public $rel_account_table = "accounts_contacts";
    //This is needed for upgrade.  This table definition moved to Opportunity module.
    public $rel_opportunity_table = "opportunities_contacts";

    public $object_name = "Contact";
    public $module_dir = 'Contacts';
    public $new_schema = true;
    public $importable = true;

    // This is used to retrieve related fields from form posts.
    public $additional_column_fields = array(
        'bug_id',
        'assigned_user_name',
        'account_name',
        'account_id',
        'opportunity_id',
        'case_id',
        'task_id',
        'note_id',
        'meeting_id',
        'call_id',
        'email_id'
    );

    public $relationship_fields = array(
        'account_id' => 'accounts',
        'bug_id' => 'bugs',
        'call_id' => 'calls',
        'case_id' => 'cases',
        'email_id' => 'emails',
        'meeting_id' => 'meetings',
        'note_id' => 'notes',
        'task_id' => 'tasks',
        'opportunity_id' => 'opportunities',
        'contacts_users_id' => 'user_sync'
    );

    public function __construct()
    {
        parent::__construct();
    }




    public function add_list_count_joins(&$query, $where)
    {
        // accounts.name
        if (stristr((string) $where, "accounts.name")) {
            // add a join to the accounts table.
            $query .= "
	            LEFT JOIN accounts_contacts
	            ON contacts.id=accounts_contacts.contact_id
	            LEFT JOIN accounts
	            ON accounts_contacts.account_id=accounts.id
			";
        }
        $custom_join = $this->getCustomJoin();
        $query .= $custom_join['join'];
    }

    public function listviewACLHelper()
    {
        $array_assign = parent::listviewACLHelper();
        $is_owner = false;
        //MFH BUG 18281; JChi #15255
        $is_owner = !empty($this->assigned_user_id) && $this->assigned_user_id == $GLOBALS['current_user']->id;
        if (!ACLController::moduleSupportsACL('Accounts') || ACLController::checkAccess(
            'Accounts',
            'view',
            $is_owner
        )
        ) {
            $array_assign['ACCOUNT'] = 'a';
        } else {
            $array_assign['ACCOUNT'] = 'span';
        }

        return $array_assign;
    }

    public function create_new_list_query(
        $order_by,
        $where,
        $filter = array(),
        $params = array(),
        $show_deleted = 0,
        $join_type = '',
        $return_array = false,
        $parentbean = null,
        $singleSelect = false,
        $ifListForExport = false
    ) {
        //if this is from "contact address popup" action, then process popup list query
        if (isset($_REQUEST['action']) && $_REQUEST['action'] == 'ContactAddressPopup') {
            return $this->address_popup_create_new_list_query(
                $order_by,
                $where,
                $filter,
                $params,
                $show_deleted,
                $join_type,
                $return_array,
                $parentbean,
                $singleSelect,
                $ifListForExport
            );
        }
        //any other action goes to parent function in sugarbean
        if (strpos((string) $order_by, 'sync_contact') !== false) {
            //we have found that the user is ordering by the sync_contact field, it would be troublesome to sort by this field
            //and perhaps a performance issue, so just remove it
            $order_by = '';
        }

        return parent::create_new_list_query(
            $order_by,
            $where,
            $filter,
            $params,
            $show_deleted,
            $join_type,
            $return_array,
            $parentbean,
            $singleSelect,
            $ifListForExport
            );
    }


    public function address_popup_create_new_list_query(
        $order_by,
        $where,
        $filter = array(),
        $params = array(),
        $show_deleted = 0,
        $join_type = '',
        $return_array = false,
        $parentbean = null,
        $singleSelect = false
    ) {
        //if this is any action that is not the contact address popup, then go to parent function in sugarbean
        if (isset($_REQUEST['action']) && $_REQUEST['action'] !== 'ContactAddressPopup') {
            return parent::create_new_list_query(
                $order_by,
                $where,
                $filter,
                $params,
                $show_deleted,
                $join_type,
                $return_array,
                $parentbean,
                $singleSelect
            );
        }

        $custom_join = $this->getCustomJoin();
        // MFH - BUG #14208 creates alias name for select
        $select_query = "SELECT ";
        $select_query .= $this->db->concat($this->table_name, array('first_name', 'last_name')) . " name, ";
        $select_query .= "
				$this->table_name.*,
                accounts.name as account_name,
                accounts.id as account_id,
                accounts.assigned_user_id account_id_owner,
                users.user_name as assigned_user_name ";
        $select_query .= $custom_join['select'];
        $ret_array = [];
        $ret_array['select'] = $select_query;

        $from_query = "
                FROM contacts ";

        $from_query .= "LEFT JOIN users
	                    ON contacts.assigned_user_id=users.id
	                    LEFT JOIN accounts_contacts
	                    ON contacts.id=accounts_contacts.contact_id  and accounts_contacts.deleted = 0
	                    LEFT JOIN accounts
	                    ON accounts_contacts.account_id=accounts.id AND accounts.deleted=0 ";
        $from_query .= "LEFT JOIN email_addr_bean_rel eabl  ON eabl.bean_id = contacts.id AND eabl.bean_module = 'Contacts' and eabl.primary_address = 1 and eabl.deleted=0 ";
        $from_query .= "LEFT JOIN email_addresses ea ON (ea.id = eabl.email_address_id) ";
        $from_query .= $custom_join['join'];
        $ret_array['from'] = $from_query;
        $ret_array['from_min'] = 'from contacts';

        $where_auto = '1=1';
        if ($show_deleted == 0) {
            $where_auto = " $this->table_name.deleted=0 ";
        //$where_auto .= " AND accounts.deleted=0  ";
        } elseif ($show_deleted == 1) {
            $where_auto = " $this->table_name.deleted=1 ";
        }


        if ($where != "") {
            $where_query = "where ($where) AND " . $where_auto;
        } else {
            $where_query = "where " . $where_auto;
        }


        $ret_array['where'] = $where_query;
        $ret_array['order_by'] = '';

        //process order by and add if it's not empty
        $order_by = $this->process_order_by($order_by);
        if (!empty($order_by)) {
            $ret_array['order_by'] = ' ORDER BY ' . $order_by;
        }

        if ($return_array) {
            return $ret_array;
        }

        return $ret_array['select'] . $ret_array['from'] . $ret_array['where'] . $ret_array['order_by'];
    }


    public function create_export_query($order_by, $where, $relate_link_join = '')
    {
        $custom_join = $this->getCustomJoin(true, true, $where);
        $custom_join['join'] .= $relate_link_join;
        $query = "SELECT
                                contacts.*,
                                email_addresses.email_address email_address,
                                '' email_addresses_non_primary, " . // email_addresses_non_primary needed for get_field_order_mapping()
            "accounts.name as account_name,
                                users.user_name as assigned_user_name ";
        $query .= $custom_join['select'];
        $query .= " FROM contacts ";
        $query .= "LEFT JOIN users
	                                ON contacts.assigned_user_id=users.id ";
        $query .= "LEFT JOIN accounts_contacts
	                                ON ( contacts.id=accounts_contacts.contact_id and (accounts_contacts.deleted is null or accounts_contacts.deleted = 0))
	                                LEFT JOIN accounts
	                                ON accounts_contacts.account_id=accounts.id ";

        //join email address table too.
        $query .= ' LEFT JOIN  email_addr_bean_rel on contacts.id = email_addr_bean_rel.bean_id and email_addr_bean_rel.bean_module=\'Contacts\' and email_addr_bean_rel.deleted=0 and email_addr_bean_rel.primary_address=1 ';
        $query .= ' LEFT JOIN email_addresses on email_addresses.id = email_addr_bean_rel.email_address_id ';

        $query .= $custom_join['join'];

        $where_auto = "( accounts.deleted IS NULL OR accounts.deleted=0 )
                      AND contacts.deleted=0 ";

        if ($where != "") {
            $query .= "where ($where) AND " . $where_auto;
        } else {
            $query .= "where " . $where_auto;
        }

        $order_by = $this->process_order_by($order_by);
        if (!empty($order_by)) {
            $query .= ' ORDER BY ' . $order_by;
        }

        return $query;
    }

    public function fill_in_additional_list_fields()
    {
        parent::fill_in_additional_list_fields();
        $this->_create_proper_name_field();
        // cn: bug 8586 - l10n names for Contacts in Email TO: field
        $this->email_and_name1 = "{$this->full_name} &lt;" . $this->email1 . "&gt;";
        $this->email_and_name2 = "{$this->full_name} &lt;" . $this->email2 . "&gt;";

        if ($this->force_load_details == true) {
            $this->fill_in_additional_detail_fields();
        }
    }

    public function fill_in_additional_detail_fields()
    {
        parent::fill_in_additional_detail_fields();
        if (empty($this->id)) {
            return;
        }

        global $locale, $app_list_strings, $current_user;

        // retrieve the account information and the information about the person the contact reports to.
        $query = "SELECT acc.id, acc.name, con_reports_to.first_name, con_reports_to.last_name
		from contacts
		left join accounts_contacts a_c on a_c.contact_id = '" . $this->id . "' and a_c.deleted=0
		left join accounts acc on a_c.account_id = acc.id and acc.deleted=0
		left join contacts con_reports_to on con_reports_to.id = contacts.reports_to_id
		where contacts.id = '" . $this->id . "'";
        // Bug 43196 - If a contact is related to multiple accounts, make sure we pull the one we are looking for
        // Bug 44730  was introduced due to this, fix is to simply clear any whitespaces around the account_id first

        $clean_account_id = trim($this->account_id);

        if (!empty($clean_account_id)) {
            $query .= " and acc.id = '{$this->account_id}'";
        }

        $query .= " ORDER BY a_c.date_modified DESC";

        $result = $this->db->query($query, true, " Error filling in additional detail fields: ");

        // Get the id and the name.
        $row = $this->db->fetchByAssoc($result);

        if ($row != null) {
            $this->account_name = $row['name'];
            $this->account_id = $row['id'];
            if (null === $locale || !is_object($locale) || !method_exists($locale, 'getLocaleFormattedName')) {
                $GLOBALS['log']->fatal('Call to a member function getLocaleFormattedName() on ' . gettype($locale));
            } else {
                $this->report_to_name = $locale->getLocaleFormattedName(
                    $row['first_name'],
                    $row['last_name'],
                    '',
                    '',
                    '',
                    null,
                    true
                );
            }
        } else {
            $this->account_name = '';
            $this->account_id = '';
            $this->report_to_name = '';
        }
        $this->load_contacts_users_relationship();
        /** concating this here because newly created Contacts do not have a
         * 'name' attribute constructed to pass onto related items, such as Tasks
         * Notes, etc.
         */
        $this->name = $locale->getLocaleFormattedName($this->first_name, $this->last_name);
        if (!empty($this->contacts_users_id)) {
            $this->sync_contact = true;
        }

        if (!empty($this->portal_active) && $this->portal_active == 1) {
            $this->portal_active = true;
        }
        // Set campaign name if there is a campaign id
        if (!empty($this->campaign_id)) {
            $camp = BeanFactory::newBean('Campaigns');
            $where = "campaigns.id='{$this->campaign_id}'";
            $campaign_list = $camp->get_full_list("campaigns.name", $where, true);
            if (!empty($campaign_list) && !empty($campaign_list[0]->name)) {
                $this->campaign_name = $campaign_list[0]->name;
            }
        }
    }

    /**
     * loads the contacts_users relationship to populate a checkbox
     * where a user can select if they would like to sync a particular
     * contact to Outlook
     */
    public function load_contacts_users_relationship()
    {
        global $current_user;

        $this->load_relationship("user_sync");

        if (!isset($this->user_sync)) {
            $GLOBALS['log']->fatal('Contact::$user_sync is not set');
            $beanIDs = null;
        } elseif (!is_object($this->user_sync)) {
            $GLOBALS['log']->fatal('Contact::$user_sync is not an object');
            $beanIDs = null;
        } elseif (!method_exists($this->user_sync, 'get')) {
            $GLOBALS['log']->fatal('Contact::$user_sync::get() is not a function');
            $beanIDs = null;
        } else {
            $beanIDs = $this->user_sync->get();
        }

        if (in_array($current_user->id, $beanIDs)) {
            $this->contacts_users_id = $current_user->id;
        }
    }

    public function get_list_view_data($filter_fields = array())
    {
        $temp_array = parent::get_list_view_data();

        if ($filter_fields && !empty($filter_fields['sync_contact'])) {
            $this->load_contacts_users_relationship();
            $temp_array['SYNC_CONTACT'] = !empty($this->contacts_users_id) ? 1 : 0;
        }

        $temp_array['EMAIL_AND_NAME1'] = "{$this->full_name} &lt;" . $temp_array['EMAIL1'] . "&gt;";

        return $temp_array;
    }

    /**
     * builds a generic search based on the query string using or
     * do not include any $this-> because this is called on without having the class instantiated
     */
    public function build_generic_where_clause($the_query_string)
    {
        $where_clauses = array();
        $the_query_string = $this->db->quote($the_query_string);

        array_push($where_clauses, "contacts.last_name like '$the_query_string%'");
        array_push($where_clauses, "contacts.first_name like '$the_query_string%'");
        array_push($where_clauses, "accounts.name like '$the_query_string%'");
        array_push($where_clauses, "contacts.assistant like '$the_query_string%'");
        array_push($where_clauses, "ea.email_address like '$the_query_string%'");

        if (is_numeric($the_query_string)) {
            array_push($where_clauses, "contacts.phone_home like '%$the_query_string%'");
            array_push($where_clauses, "contacts.phone_mobile like '%$the_query_string%'");
            array_push($where_clauses, "contacts.phone_work like '%$the_query_string%'");
            array_push($where_clauses, "contacts.phone_other like '%$the_query_string%'");
            array_push($where_clauses, "contacts.phone_fax like '%$the_query_string%'");
            array_push($where_clauses, "contacts.assistant_phone like '%$the_query_string%'");
        }

        $the_where = "";
        foreach ($where_clauses as $clause) {
            if ($the_where != "") {
                $the_where .= " or ";
            }
            $the_where .= $clause;
        }


        return $the_where;
    }

    public function set_notification_body($xtpl, $contact)
    {
        global $locale;

        $xtpl->assign("CONTACT_NAME", trim($locale->getLocaleFormattedName($contact->first_name, $contact->last_name)));
        $xtpl->assign("CONTACT_DESCRIPTION", nl2br($contact->description));

        return $xtpl;
    }

    public function get_contact_id_by_email($email)
    {
        $email = trim($email);
        if (empty($email)) {
            //email is empty, no need to query, return null
            return null;
        }

        $where_clause = "(email1='$email' OR email2='$email') AND deleted=0";

        $query = "SELECT id FROM $this->table_name WHERE $where_clause";
        $GLOBALS['log']->debug("Retrieve $this->object_name: " . $query);
        $result = $this->db->getOne($query, true, "Retrieving record $where_clause:");

        return empty($result) ? null : $result;
    }

    public function save_relationship_changes($is_update, $exclude = array())
    {

        //if account_id was replaced unlink the previous account_id.
        //this rel_fields_before_value is populated by sugarbean during the retrieve call.
        if (!empty($this->account_id) && !empty($this->rel_fields_before_value['account_id']) && trim($this->account_id) !== trim($this->rel_fields_before_value['account_id'])
        ) {
            //unlink the old record.
            $this->load_relationship('accounts');
            $this->accounts->delete($this->id, $this->rel_fields_before_value['account_id']);
        }
        parent::save_relationship_changes($is_update);
    }

    public function bean_implements($interface)
    {
        switch ($interface) {
            case 'ACL':
                return true;
        }

        return false;
    }

    public function get_unlinked_email_query($type = array())
    {
        return get_unlinked_email_query($type, $this);
    }

    /**
     * used by import to add a list of users
     *
     * Parameter can be one of the following:
     * - string 'all': add this contact for all users
     * - comma deliminated lists of teams and/or users
     *
     * @param string $list_of_user
     */
    public function process_sync_to_outlook($list_of_users)
    {
        static $focus_user;

        // cache this object since we'll be reusing it a bunch
        if (!($focus_user instanceof User)) {
            $focus_user = BeanFactory::newBean('Users');
        }


        if (empty($list_of_users)) {
            return;
        }
        if (!isset($this->users)) {
            $this->load_relationship('user_sync');
        }

        if (strtolower($list_of_users) == 'all') {
            // add all non-deleted users
            $sql = "SELECT id FROM users WHERE deleted=0 AND is_group=0 AND portal_only=0";
            $result = $this->db->query($sql);
            while ($hash = $this->db->fetchByAssoc($result)) {
                if (!isset($this->user_sync)) {
                    $GLOBALS['log']->fatal('Contact::$user_sync is not set');
                } elseif (!is_object($this->user_sync)) {
                    $GLOBALS['log']->fatal('Contact::$user_sync is not an object');
                } elseif (!method_exists($this->user_sync, 'add')) {
                    $GLOBALS['log']->fatal('Contact::$user_sync::add() is not a function');
                } else {
                    $this->user_sync->add($hash['id']);
                }
            }
        } else {
            $theList = explode(",", $list_of_users);
            foreach ($theList as $eachItem) {
                if (($user_id = $focus_user->retrieve_user_id($eachItem))
                    || $focus_user->retrieve($eachItem)
                ) {
                    // it is a user, add user
                    if (!isset($this->user_sync)) {
                        $GLOBALS['log']->fatal('Contact::$user_sync is not set');
                    } elseif (!is_object($this->user_sync)) {
                        $GLOBALS['log']->fatal('Contact::$user_sync is not an object');
                    } elseif (!method_exists($this->user_sync, 'add')) {
                        $GLOBALS['log']->fatal('Contact::$user_sync::add() is not a function');
                    } else {
                        $this->user_sync->add($user_id ? $user_id : $focus_user->id);
                    }

                    return;
                }
            }
        }
    }
}
