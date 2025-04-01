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



require_once('include/SugarObjects/templates/person/Person.php');
require_once __DIR__ . '/../../include/EmailInterface.php';

#[\AllowDynamicProperties]
class Prospect extends Person implements EmailInterface
{
    public $field_name_map;
    // Stored fields
    public $id;
    public $name = '';
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
    public $full_name;
    public $title;
    public $department;
    public $birthdate;
    public $do_not_call;
    public $phone_home;
    public $phone_mobile;
    public $phone_work;
    public $phone_other;
    public $phone_fax;
    public $email1;
    public $email2;
    public $email_and_name1;
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
    public $tracker_key;
    public $lead_id;
    public $account_name;
    public $assigned_real_user_name;
    // These are for related fields
    public $assigned_user_name;
    public $module_dir = 'Prospects';
    public $table_name = "prospects";
    public $object_name = "Prospect";
    public $new_schema = true;
    public $emailAddress;

    public $importable = true;
    // This is used to retrieve related fields from form posts.
    public $additional_column_fields = array('assigned_user_name');


    public function __construct()
    {
        parent::__construct();
    }




    public function fill_in_additional_list_fields()
    {
        parent::fill_in_additional_list_fields();
        $this->_create_proper_name_field();
        $this->email_and_name1 = $this->full_name." &lt;".$this->email1."&gt;";
    }

    public function fill_in_additional_detail_fields()
    {
        parent::fill_in_additional_list_fields();
        $this->_create_proper_name_field();
    }

    /**
        builds a generic search based on the query string using or
        do not include any $this-> because this is called on without having the class instantiated
    */
    public function build_generic_where_clause($the_query_string)
    {
        $where_clauses = array();
        $the_query_string = DBManagerFactory::getInstance()->quote($the_query_string);

        array_push($where_clauses, "prospects.last_name like '$the_query_string%'");
        array_push($where_clauses, "prospects.first_name like '$the_query_string%'");
        array_push($where_clauses, "prospects.assistant like '$the_query_string%'");

        if (is_numeric($the_query_string)) {
            array_push($where_clauses, "prospects.phone_home like '%$the_query_string%'");
            array_push($where_clauses, "prospects.phone_mobile like '%$the_query_string%'");
            array_push($where_clauses, "prospects.phone_work like '%$the_query_string%'");
            array_push($where_clauses, "prospects.phone_other like '%$the_query_string%'");
            array_push($where_clauses, "prospects.phone_fax like '%$the_query_string%'");
            array_push($where_clauses, "prospects.assistant_phone like '%$the_query_string%'");
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

    public function converted_prospect($prospectid, $contactid, $accountid, $opportunityid)
    {
        $query = "UPDATE prospects set  contact_id=$contactid, account_id=$accountid, opportunity_id=$opportunityid where  id=$prospectid and deleted=0";
        $this->db->query($query, true, "Error converting prospect: ");
        //todo--status='Converted', converted='1',
    }
    public function bean_implements($interface)
    {
        switch ($interface) {
            case 'ACL':return true;
        }
        return false;
    }

    /**
     *  This method will be used by Mail Merge in order to retieve the targets as specified in the query
     *  @param query String - this is the query which contains the where clause for the query
     */
    public function retrieveTargetList($query, $fields, $offset = 0, $limit= -99, $max = -99, $deleted = 0, $module = '')
    {
        global  $beanList, $beanFiles;
        $module_name = $this->module_dir;

        if (empty($module)) {
            //The call to retrieveTargetList contains a query that may contain a pound token
            $pattern = '/AND related_type = [\'#]([a-zA-Z]+)[\'#]/i';
            if (preg_match($pattern, (string) $query, $matches)) {
                $module_name = $matches[1];
                $query = preg_replace($pattern, "", (string) $query);
            }
        }

        $count = is_countable($fields) ? count($fields) : 0;
        $index = 1;
        $sel_fields = "";
        if (!empty($fields)) {
            foreach ($fields as $field) {
                if ($field == 'id') {
                    $sel_fields .= 'prospect_lists_prospects.id id';
                } else {
                    $sel_fields .= strtolower($module_name).".".$field;
                }
                if ($index < $count) {
                    $sel_fields .= ",";
                }
                $index++;
            }
        }

        $module_name = ucfirst($module_name);
        $class_name = $beanList[$module_name];
        require_once($beanFiles[$class_name]);
        $seed = new $class_name();
        if (empty($sel_fields)) {
            $sel_fields = $seed->table_name.'.*';
        }
        $select = "SELECT ".$sel_fields." FROM ".$seed->table_name;
        $select .= " INNER JOIN prospect_lists_prospects ON prospect_lists_prospects.related_id = ".$seed->table_name.".id";
        $select .= " INNER JOIN prospect_lists ON prospect_lists_prospects.prospect_list_id = prospect_lists.id";
        $select .= " INNER JOIN prospect_list_campaigns ON prospect_list_campaigns.prospect_list_id = prospect_lists.id";
        $select .= " INNER JOIN campaigns on campaigns.id = prospect_list_campaigns.campaign_id";
        $select .= " WHERE prospect_list_campaigns.deleted = 0";
        $select .= " AND prospect_lists_prospects.deleted = 0";
        $select .= " AND prospect_lists.deleted = 0";
        if (!empty($query)) {
            $select .= " AND ".$query;
        }

        return $this->process_list_query($select, $offset, $limit, $max, $query);
    }

    /**
     *  Given an id, looks up in the prospect_lists_prospects table
     *  and retrieve the correct type for this id
     */
    public function retrieveTarget($id)
    {
        $query = "SELECT related_id, related_type FROM prospect_lists_prospects WHERE id = '".$this->db->quote($id)."'";
        $result = $this->db->query($query);
        if (($row = $this->db->fetchByAssoc($result))) {
            global  $beanList, $beanFiles;
            $module_name = $row['related_type'];
            $class_name = $beanList[$module_name];
            require_once($beanFiles[$class_name]);
            $seed = new $class_name();
            return $seed->retrieve($row['related_id']);
        } else {
            return null;
        }
    }


    public function get_unlinked_email_query($type=array())
    {
        return get_unlinked_email_query($type, $this);
    }
}
