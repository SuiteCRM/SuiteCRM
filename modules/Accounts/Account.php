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

 * Description:  Defines the Account SugarBean Account entity with the necessary
 * methods and variables.
 */

require_once("include/SugarObjects/templates/company/Company.php");
require_once __DIR__ . '/../../include/EmailInterface.php';

// Account is used to store account information.
#[\AllowDynamicProperties]
class Account extends Company implements EmailInterface
{
    public $field_name_map = array();
    // Stored fields
    public $date_entered;
    public $date_modified;
    public $modified_user_id;
    public $assigned_user_id;
    public $annual_revenue;
    public $billing_address_street;
    public $billing_address_city;
    public $billing_address_state;
    public $billing_address_country;
    public $billing_address_postalcode;

    public $billing_address_street_2;
    public $billing_address_street_3;
    public $billing_address_street_4;

    public $description;
    public $email1;
    public $email2;
    public $email_opt_out;
    public $invalid_email;
    public $employees;
    public $id;
    public $industry;
    public $name;
    public $ownership;
    public $parent_id;
    public $phone_alternate;
    public $phone_fax;
    public $phone_office;
    public $rating;
    public $shipping_address_street;
    public $shipping_address_city;
    public $shipping_address_state;
    public $shipping_address_country;
    public $shipping_address_postalcode;

    public $shipping_address_street_2;
    public $shipping_address_street_3;
    public $shipping_address_street_4;

    public $campaign_id;

    public $sic_code;
    public $ticker_symbol;
    public $account_type;
    public $website;
    public $custom_fields;

    public $created_by;
    public $created_by_name;
    public $modified_by_name;

    // These are for related fields
    public $opportunity_id;
    public $case_id;
    public $contact_id;
    public $task_id;
    public $note_id;
    public $meeting_id;
    public $call_id;
    public $email_id;
    public $member_id;
    public $parent_name;
    public $assigned_user_name;
    public $account_id = '';
    public $account_name = '';
    public $bug_id ='';
    public $module_dir = 'Accounts';
    public $emailAddress;


    public $table_name = "accounts";
    public $object_name = "Account";
    public $importable = true;
    public $new_schema = true;
    // This is used to retrieve related fields from form posts.
    public $additional_column_fields = array('assigned_user_name', 'assigned_user_id', 'opportunity_id', 'bug_id', 'case_id', 'contact_id', 'task_id', 'note_id', 'meeting_id', 'call_id', 'email_id', 'parent_name', 'member_id'
    );
    public $relationship_fields = array('opportunity_id'=>'opportunities', 'bug_id' => 'bugs', 'case_id'=>'cases',
                                    'contact_id'=>'contacts', 'task_id'=>'tasks', 'note_id'=>'notes',
                                    'meeting_id'=>'meetings', 'call_id'=>'calls', 'email_id'=>'emails','member_id'=>'members',
                                    'project_id'=>'project',
                                    );

    //Meta-Data Framework fields
    public $push_billing;
    public $push_shipping;

    public function __construct()
    {
        parent::__construct();

        $this->setupCustomFields('Accounts');

        foreach ($this->field_defs as $field) {
            if (isset($field['name'])) {
                $this->field_name_map[$field['name']] = $field;
            }
        }


        //Email logic
        if (!empty($_REQUEST['parent_id']) && !empty($_REQUEST['parent_type']) && $_REQUEST['parent_type'] == 'Emails'
            && !empty($_REQUEST['return_module']) && $_REQUEST['return_module'] == 'Emails') {
            $_REQUEST['parent_name'] = '';
            $_REQUEST['parent_id'] = '';
        }
    }

    public function get_summary_text()
    {
        return $this->name;
    }

    public function get_contacts()
    {
        return $this->get_linked_beans('contacts', 'Contact');
    }



    public function clear_account_case_relationship($account_id = '', $case_id = '')
    {
        $where = '';

        $accountIdQuoted = $this->db->quoted($account_id);

        if (!empty($case_id)) {
            $caseIdQuoted = $this->db->quoted($case_id);
            $where = " and id = " . $caseIdQuoted;
        }

        $query = "UPDATE cases SET account_name = '', account_id = '' WHERE account_id = " . $accountIdQuoted . " AND deleted = 0 " . $where;

        $this->db->query($query, true, "Error clearing account to case relationship: ");
    }

    /**
    * This method is used to provide backward compatibility with old data that was prefixed with http://
    * We now automatically prefix http://
    * @deprecated.
    */
    public function remove_redundant_http()
    {	/*
        if(preg_match("@http://@", $this->website))
        {
            $this->website = substr($this->website, 7);
        }
        */
    }

    public function fill_in_additional_list_fields()
    {
        parent::fill_in_additional_list_fields();
        // Fill in the assigned_user_name
    //	$this->assigned_user_name = get_assigned_user_name($this->assigned_user_id);
    }

    public function fill_in_additional_detail_fields()
    {
        parent::fill_in_additional_detail_fields();

        //rrs bug: 28184 - instead of removing this code altogether just adding this check to ensure that if the parent_name
        //is empty then go ahead and fill it.
        if (empty($this->parent_name) && !empty($this->id)) {

            $idQuoted = $this->db->quoted($this->id);

            $query = "SELECT a1.name FROM accounts a1, accounts a2 WHERE a1.id = a2.parent_id AND a2.id = " . $idQuoted . " and a1.deleted=0";
            $result = $this->db->query($query, true, " Error filling in additional detail fields: ");

            // Get the id and the name.
            $row = $this->db->fetchByAssoc($result);

            if ($row != null) {
                $this->parent_name = $row['name'];
            } else {
                $this->parent_name = '';
            }
        }

        // Set campaign name if there is a campaign id
        if (!empty($this->campaign_id)) {
            $camp = BeanFactory::newBean('Campaigns');

            $campaignIdQuoted = $this->db->quoted($this->campaign_id);

            $where = "campaigns.id = " . $campaignIdQuoted;
            $campaign_list = $camp->get_full_list("campaigns.name", $where, true);
            $this->campaign_name = $campaign_list[0]->name;
        }
    }

    public function get_list_view_data()
    {
        $temp_array = parent::get_list_view_data();

        $temp_array["ENCODED_NAME"] = $this->name;

        if (!empty($this->billing_address_state)) {
            $temp_array["CITY"] = $this->billing_address_city . ', '. $this->billing_address_state;
        } else {
            $temp_array["CITY"] = $this->billing_address_city;
        }
        $temp_array["BILLING_ADDRESS_STREET"]  = $this->billing_address_street;
        $temp_array["SHIPPING_ADDRESS_STREET"] = $this->shipping_address_street;

        return $temp_array;
    }
    /**
    	builds a generic search based on the query string using or
    	do not include any $this-> because this is called on without having the class instantiated
    */
    public function build_generic_where_clause($the_query_string)
    {
        $where_clauses = array();
        $the_query_string = $this->db->quote($the_query_string);
        array_push($where_clauses, "accounts.name like '$the_query_string%'");
        if (is_numeric($the_query_string)) {
            array_push($where_clauses, "accounts.phone_alternate like '%$the_query_string%'");
            array_push($where_clauses, "accounts.phone_fax like '%$the_query_string%'");
            array_push($where_clauses, "accounts.phone_office like '%$the_query_string%'");
        }

        $the_where = "";
        foreach ($where_clauses as $clause) {
            if (!empty($the_where)) {
                $the_where .= " or ";
            }
            $the_where .= $clause;
        }

        return $the_where;
    }


    public function create_export_query($order_by, $where, $relate_link_join='')
    {
        $relatedJoins = [];
        $relatedSelects = [];
        foreach (explode('AND', $where) as $whereClause) {
            $newWhereClause = str_replace('( ', '(', $whereClause);
            foreach ($this->field_defs as $field_def) {
                $needle = '(' . $field_def['name'] . ' LIKE';
                if (strpos($newWhereClause, $needle) !== false) {
                    $joinAlias = 'rjt' . count($relatedJoins);
                    $relatedJoins[] = ' LEFT JOIN ' . $field_def['table'] . ' as ' . $joinAlias . ' ON ' .
                            $joinAlias . '.id ' . ' = accounts.' . $field_def['id_name'] . ' ';
                    $relatedSelects[] = ' ,' . $joinAlias . '.id ,' . $joinAlias . '.' . $field_def['rname'] . ' ';
                    $newWhereClause = str_replace(
                        $field_def['name'],
                        $joinAlias . '.' . $field_def['rname'],
                        $newWhereClause
                    );
                    $where = str_replace($whereClause, $newWhereClause, (string) $where);
                }
            }
        }

        $custom_join = $this->getCustomJoin(true, true, $where);
        $custom_join['join'] .= $relate_link_join;
        $query = "SELECT
                    accounts.*,
                    email_addresses.email_address email_address,
                    '' email_addresses_non_primary, " . // email_addresses_non_primary needed for get_field_order_mapping()
                    "accounts.name as account_name,
                    users.user_name as assigned_user_name ";
        $query .= $custom_join['select'];

        $query .= implode('', $relatedSelects);

        $query .= " FROM accounts ";
        $query .= "LEFT JOIN users
	                ON accounts.assigned_user_id = users.id ";

        //join email address table too.
        $query .=  ' LEFT JOIN email_addr_bean_rel ON accounts.id = email_addr_bean_rel.bean_id
                     AND email_addr_bean_rel.bean_module=\'Accounts\'
                     AND email_addr_bean_rel.deleted = 0
                     AND email_addr_bean_rel.primary_address = 1 ';
        $query .=  ' LEFT JOIN email_addresses on email_addresses.id = email_addr_bean_rel.email_address_id ' ;

        $query .= implode('', $relatedJoins);

        $query .= $custom_join['join'];

        $where_auto = "( accounts.deleted IS NULL OR accounts.deleted=0 )";

        if ($where != "") {
            $query .= "where ($where) AND ".$where_auto;
        } else {
            $query .= "where ".$where_auto;
        }

        $order_by = $this->process_order_by($order_by);
        if (!empty($order_by)) {
            $query .= ' ORDER BY ' . $order_by;
        }

        return $query;
    }

    public function set_notification_body($xtpl, $account)
    {
        $xtpl->assign("ACCOUNT_NAME", $account->name);
        $xtpl->assign("ACCOUNT_TYPE", $account->account_type);
        $xtpl->assign("ACCOUNT_DESCRIPTION", nl2br($account->description));

        return $xtpl;
    }

    public function bean_implements($interface)
    {
        switch ($interface) {
            case 'ACL':return true;
        }
        return false;
    }
    public function get_unlinked_email_query($type=array())
    {
        return get_unlinked_email_query($type, $this);
    }

    /**
     * Create a query string for select Products/Services Purchased list from database.
     * @return string final query
     */
    public function getProductsServicesPurchasedQuery()
    {
        $idQuoted = $this->db->quoted($this->id);

        $query = "
			SELECT
				aos_products_quotes.*
			FROM
				aos_products_quotes
			JOIN aos_quotes ON aos_quotes.id = aos_products_quotes.parent_id AND aos_quotes.stage LIKE 'Closed Accepted' AND aos_quotes.deleted = 0 AND aos_products_quotes.deleted = 0
			JOIN accounts ON accounts.id = aos_quotes.billing_account_id AND accounts.id = $idQuoted

			";
        return $query;
    }
}
