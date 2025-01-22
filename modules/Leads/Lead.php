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








require_once('include/SugarObjects/templates/person/Person.php');
require_once __DIR__ . '/../../include/EmailInterface.php';

// Lead is used to store profile information for people who may become customers.
#[\AllowDynamicProperties]
class Lead extends Person implements EmailInterface
{
    public $field_name_map;
    // Stored fields
    public $id;
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
    public $reports_to_id;
    public $do_not_call;
    public $phone_home;
    public $phone_mobile;
    public $phone_work;
    public $phone_other;
    public $phone_fax;
    public $refered_by;
    public $email1;
    public $email2;
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
    public $name;
    public $full_name;
    public $portal_name;
    public $portal_app;
    public $contact_id;
    public $contact_name;
    public $account_id;
    public $opportunity_id;
    public $opportunity_name;
    public $opportunity_amount;
    //used for vcard export only
    public $birthdate;
    public $status;
    public $status_description;

    public $lead_source;
    public $lead_source_description;
    // These are for related fields
    public $account_name;
    public $acc_name_from_accounts;
    public $account_site;
    public $account_description;
    public $case_role;
    public $case_rel_id;
    public $case_id;
    public $task_id;
    public $note_id;
    public $meeting_id;
    public $call_id;
    public $email_id;
    public $assigned_user_name;
    public $campaign_id;
    public $campaign_name;
    public $alt_address_street_2;
    public $alt_address_street_3;
    public $primary_address_street_2;
    public $primary_address_street_3;


    public $table_name = "leads";
    public $object_name = "Lead";
    public $object_names = "Leads";
    public $module_dir = "Leads";
    public $new_schema = true;
    public $emailAddress;
    public $prospect_id;

    public $importable = true;

    // This is used to retrieve related fields from form posts.
    public $additional_column_fields = array('assigned_user_name', 'task_id', 'note_id', 'meeting_id', 'call_id', 'email_id');
    public $relationship_fields = array('email_id'=>'emails','call_id'=>'calls','meeting_id'=>'meetings','task_id'=>'tasks',);

    public function __construct()
    {
        parent::__construct();
    }




    public function get_account()
    {
        if (isset($this->account_id) && !empty($this->account_id)) {
            $query = "SELECT name , assigned_user_id account_name_owner FROM accounts WHERE id='{$this->account_id}'";

            //requireSingleResult has beeen deprecated.
            //$result = $this->db->requireSingleResult($query);
            $result = $this->db->limitQuery($query, 0, 1, true, "Want only a single row");

            if (!empty($result)) {
                $row = $this->db->fetchByAssoc($result);
                if (!is_null($row) && !is_bool($row)) {
                    $this->account_name = $row['name'];
                    $this->account_name_owner = $row['account_name_owner'];
                } else {
                    $this->account_name = null;
                    $this->account_name_owner = null;
                }
                $this->account_name_mod = 'Accounts';
            }
        }
    }
    public function get_opportunity()
    {
        if (isset($this->opportunity_id) && !empty($this->opportunity_id)) {
            $query = "SELECT name, assigned_user_id opportunity_name_owner FROM opportunities WHERE id='{$this->opportunity_id}'";

            //requireSingleResult has beeen deprecated.
            //$result = $this->db->requireSingleResult($query);
            $result = $this->db->limitQuery($query, 0, 1, true, "Want only a single row");

            if (!empty($result)) {
                $row = $this->db->fetchByAssoc($result);

                if (!is_null($row) && !is_bool($row)) {
                    $this->opportunity_name = $row['name'];
                    $this->opportunity_name_owner = $row['opportunity_name_owner'];
                } else {
                    $this->opportunity_name = null;
                    $this->opportunity_name_owner = null;
                }

                $this->opportunity_name_mod = 'Opportunities';
            }
        }
    }
    public function get_contact()
    {
        global $locale;
        if (isset($this->contact_id) && !empty($this->contact_id)) {
            $query = "SELECT first_name, last_name, assigned_user_id contact_name_owner FROM contacts WHERE id='{$this->contact_id}'";

            //requireSingleResult has beeen deprecated.
            //$result = $this->db->requireSingleResult($query);
            $result = $this->db->limitQuery($query, 0, 1, true, "Want only a single row");
            if (!empty($result)) {
                $row= $this->db->fetchByAssoc($result);

                if (!is_null($row) && !is_bool($row)) {
                    $this->contact_name = $locale->getLocaleFormattedName($row['first_name'], $row['last_name']);
                    $this->contact_name_owner = $row['contact_name_owner'];
                } else {
                    $this->contact_name = null;
                    $this->contact_name_owner = null;
                }
                $this->contact_name_mod = 'Contacts';
            }
        }
    }

    public function create_list_query($order_by, $where, $show_deleted=0)
    {
        $custom_join = $this->getCustomJoin();
        $query = "SELECT ";


        $query .= "$this->table_name.*, users.user_name assigned_user_name";
        $query .= $custom_join['select'];
        $query .= " FROM leads ";

        $query .= "			LEFT JOIN users
                                ON leads.assigned_user_id=users.id ";
        $query .= "LEFT JOIN email_addr_bean_rel eabl  ON eabl.bean_id = leads.id AND eabl.bean_module = 'Leads' and eabl.primary_address = 1 and eabl.deleted=0 ";
        $query .= "LEFT JOIN email_addresses ea ON (ea.id = eabl.email_address_id) ";
        $query .= $custom_join['join'];
        $where_auto = '1=1';
        if ($show_deleted == 0) {
            $where_auto = " leads.deleted=0 ";
        } elseif ($show_deleted == 1) {
            $where_auto = " leads.deleted=1 ";
        }

        if ($where != "") {
            $query .= "where ($where) AND ".$where_auto;
        } else {
            $query .= "where ".$where_auto;
        } //."and (leads.converted='0')";

        if (!empty($order_by)) {
            $query .= " ORDER BY $order_by";
        }

        return $query;
    }

    public function create_new_list_query($order_by, $where, $filter=array(), $params=array(), $show_deleted = 0, $join_type='', $return_array = false, $parentbean=null, $singleSelect = false, $ifListForExport = false)
    {
        $ret_array = parent::create_new_list_query($order_by, $where, $filter, $params, $show_deleted, $join_type, true, $parentbean, $singleSelect, $ifListForExport);
        if (strpos((string) $ret_array['select'], "leads.account_name") == false && strpos((string) $ret_array['select'], "leads.*") == false) {
            $ret_array['select'] .= " ,leads.account_name";
        }
        if (!$return_array) {
            return  $ret_array['select'] . $ret_array['from'] . $ret_array['where']. $ret_array['order_by'];
        }
        return $ret_array;
    }

    public function converted_lead($leadid, $contactid, $accountid, $opportunityid)
    {
        $query = "UPDATE leads set converted='1', contact_id=$contactid, account_id=$accountid, opportunity_id=$opportunityid where  id=$leadid and deleted=0";
        $this->db->query($query, true, "Error converting lead: ");

        //we must move the status out here in order to be able to capture workflow conditions
        $leadid = str_replace("'", "", (string) $leadid);
        $lead = BeanFactory::newBean('Leads');
        $lead->retrieve($leadid);
        $lead->status='Converted';
        $lead->save();
    }

    public function fill_in_additional_list_fields()
    {
        parent::fill_in_additional_list_fields();
        $this->_create_proper_name_field();
        $this->get_account();
    }

    public function fill_in_additional_detail_fields()
    {
        //Fill in the assigned_user_name
        //if(!empty($this->status))
        //$this->status = translate('lead_status_dom', '', $this->status);
        parent::fill_in_additional_detail_fields();
        $this->_create_proper_name_field();
        $this->get_contact();
        $this->get_opportunity();
        $this->get_account();

        if (!empty($this->campaign_id)) {
            $camp = BeanFactory::newBean('Campaigns');
            $where = "campaigns.id='$this->campaign_id'";
            $campaign_list = $camp->get_full_list("campaigns.name", $where, true);
            if (!empty($campaign_list)) {
                $this->campaign_name = $campaign_list[0]->name;
            }
        }
    }

    public function get_list_view_data()
    {
        $temp_array = parent::get_list_view_data();

        if (!isset($temp_array['ACCOUNT_NAME'])) {
            LoggerManager::getLogger()->warn('Leads get list view data error: account name is not defined in list view data.');
            $tempArrayAccountName = null;
        } else {
            $tempArrayAccountName = $temp_array['ACCOUNT_NAME'];
        }

        $temp_array['ACC_NAME_FROM_ACCOUNTS'] = empty($temp_array['ACC_NAME_FROM_ACCOUNTS']) ? ($tempArrayAccountName) : ($temp_array['ACC_NAME_FROM_ACCOUNTS']);

        return $temp_array;
    }

    /**
     * Returns an array of fields that are of type link.
     *
     * @return array List of fields.
     *
     * Internal function, do not override.
     */
    //fix for bug 27339 Shine
    public function get_linked_fields()
    {
        $linked_fields=array();
        $fieldDefs = $this->getFieldDefinitions();

        //find all definitions of type link.
        if (!empty($fieldDefs)) {
            foreach ($fieldDefs as $name=>$properties) {
                if ($name == 'oldmeetings' || $name == 'oldcalls') {
                    continue;
                } elseif (array_search('link', $properties, true) === 'type') {
                    $linked_fields[$name]=$properties;
                }
            }
        }
        return $linked_fields;
    }

    /**
    	builds a generic search based on the query string using or
    	do not include any $this-> because this is called on without having the class instantiated
    */
    public function build_generic_where_clause($the_query_string)
    {
        $where_clauses = array();
        $the_query_string = DBManagerFactory::getInstance()->quote($the_query_string);

        array_push($where_clauses, "leads.last_name like '$the_query_string%'");
        array_push($where_clauses, "leads.account_name like '$the_query_string%'");
        array_push($where_clauses, "leads.first_name like '$the_query_string%'");
        array_push($where_clauses, "ea.email_address like '$the_query_string%'");

        if (is_numeric($the_query_string)) {
            array_push($where_clauses, "leads.phone_home like '%$the_query_string%'");
            array_push($where_clauses, "leads.phone_mobile like '%$the_query_string%'");
            array_push($where_clauses, "leads.phone_work like '%$the_query_string%'");
            array_push($where_clauses, "leads.phone_other like '%$the_query_string%'");
            array_push($where_clauses, "leads.phone_fax like '%$the_query_string%'");
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

    public function set_notification_body($xtpl, $lead)
    {
        global $app_list_strings;
        global $locale;

        $xtpl->assign("LEAD_NAME", $locale->getLocaleFormattedName($lead->first_name, $lead->last_name, $lead->salutation));
        $xtpl->assign("LEAD_SOURCE", (isset($lead->lead_source) ? $app_list_strings['lead_source_dom'][$lead->lead_source] : ""));
        $xtpl->assign("LEAD_STATUS", (isset($lead->status)? $app_list_strings['lead_status_dom'][$lead->status]:""));
        $xtpl->assign("LEAD_DESCRIPTION", nl2br($lead->description));

        return $xtpl;
    }

    public function bean_implements($interface)
    {
        switch ($interface) {
            case 'ACL':return true;
        }
        return false;
    }
    public function listviewACLHelper()
    {
        $array_assign = parent::listviewACLHelper();
        $is_owner = false;
        $in_group = false; //SECURITY GROUPS
        if (!empty($this->account_name)) {
            if (!empty($this->account_name_owner)) {
                global $current_user;
                $is_owner = $current_user->id == $this->account_name_owner;
            }
            /* BEGIN - SECURITY GROUPS */
            else {
                global $current_user;
                $parent_bean = BeanFactory::getBean('Accounts', $this->account_id);
                if ($parent_bean !== false) {
                    $is_owner = $current_user->id == $parent_bean->assigned_user_id;
                }
            }
            require_once("modules/SecurityGroups/SecurityGroup.php");
            $in_group = SecurityGroup::groupHasAccess('Accounts', $this->account_id, 'view');
            /* END - SECURITY GROUPS */
        }
        /* BEGIN - SECURITY GROUPS */
        /**
        if( ACLController::checkAccess('Accounts', 'view', $is_owner)){
        */
        if (ACLController::checkAccess('Accounts', 'view', $is_owner, 'module', $in_group)) {
            /* END - SECURITY GROUPS */
            $array_assign['ACCOUNT'] = 'a';
        } else {
            $array_assign['ACCOUNT'] = 'span';
        }
        $is_owner = false;
        $in_group = false; //SECURITY GROUPS
        if (!empty($this->opportunity_name)) {
            if (!empty($this->opportunity_name_owner)) {
                global $current_user;
                $is_owner = $current_user->id == $this->opportunity_name_owner;
            }
            /* BEGIN - SECURITY GROUPS */
            else {
                global $current_user;
                $parent_bean = BeanFactory::getBean('Opportunities', $this->opportunity_id);
                if ($parent_bean !== false) {
                    $is_owner = $current_user->id == $parent_bean->assigned_user_id;
                }
            }
            require_once("modules/SecurityGroups/SecurityGroup.php");
            $in_group = SecurityGroup::groupHasAccess('Opportunities', $this->opportunity_id, 'view');
            /* END - SECURITY GROUPS */
        }
        /* BEGIN - SECURITY GROUPS */
        /**
        if( ACLController::checkAccess('Opportunities', 'view', $is_owner)){
        */
        if (ACLController::checkAccess('Opportunities', 'view', $is_owner, 'module', $in_group)) {
            /* END - SECURITY GROUPS */
            $array_assign['OPPORTUNITY'] = 'a';
        } else {
            $array_assign['OPPORTUNITY'] = 'span';
        }


        $is_owner = false;
        $in_group = false; //SECURITY GROUPS
        if (!empty($this->contact_name)) {
            if (!empty($this->contact_name_owner)) {
                global $current_user;
                $is_owner = $current_user->id == $this->contact_name_owner;
            }
            /* BEGIN - SECURITY GROUPS */
            //contact_name_owner not being set for whatever reason so we need to figure this out
            else {
                global $current_user;
                $parent_bean = BeanFactory::getBean('Contacts', $this->contact_id);
                if ($parent_bean !== false) {
                    $is_owner = $current_user->id == $parent_bean->assigned_user_id;
                }
            }
            require_once("modules/SecurityGroups/SecurityGroup.php");
            $in_group = SecurityGroup::groupHasAccess('Contacts', $this->contact_id, 'view');
            /* END - SECURITY GROUPS */
        }
        /* BEGIN - SECURITY GROUPS */
        /**
        if( ACLController::checkAccess('Contacts', 'view', $is_owner)){
        */
        if (ACLController::checkAccess('Contacts', 'view', $is_owner, 'module', $in_group)) {
            /* END - SECURITY GROUPS */
            $array_assign['CONTACT'] = 'a';
        } else {
            $array_assign['CONTACT'] = 'span';
        }

        return $array_assign;
    }

    //carrys forward custom lead fields to contacts, accounts, opportunities during Lead Conversion
    public function convertCustomFieldsForm(&$form, &$tempBean, &$prefix)
    {
        global $mod_strings, $app_list_strings, $app_strings, $lbl_required_symbol;

        foreach ($this->field_defs as $field => $value) {
            if (!empty($value['source']) && $value['source'] == 'custom_fields') {
                if (!empty($tempBean->field_defs[$field]) && isset($tempBean->field_defs[$field])) {
                    $label = $tempBean->field_defs[$field]['vname'];
                    if(isset($mod_strings[$label])){
                        $label = $mod_strings[$label];
                    } elseif(isset($app_strings[$label])){
                        $label = $app_strings[$label];
                    }
                    $form .= "<tr><td nowrap colspan='4' class='dataLabel'>".$label.":";

                    if (!empty($tempBean->custom_fields->avail_fields[$field]['required']) && ($tempBean->custom_fields->avail_fields[$field]['required']== 1 || $tempBean->custom_fields->avail_fields[$field]['required']== '1' || $tempBean->custom_fields->avail_fields[$field]['required']== 'true' || $tempBean->custom_fields->avail_fields[$field]['required']== true)) {
                        $form .= "&nbsp;<span class='required'>".$lbl_required_symbol."</span>";
                    }
                    $form .= "</td></tr>";
                    $form .= "<tr><td nowrap colspan='4' class='dataField' nowrap>";

                    if (isset($value['isMultiSelect']) && $value['isMultiSelect'] == 1) {
                        $this->$field = unencodeMultienum($this->$field);
                        $multiple = "multiple";
                        $array = '[]';
                    } else {
                        $multiple = null;
                        $array = null;
                    }

                    if (!empty($value['options']) && isset($value['options'])) {
                        $form .= "<select " . $multiple . " name='".$prefix.$field.$array."'>";
                        $form .= get_select_options_with_id($app_list_strings[$value['options']], $this->$field);
                        $form .= "</select";
                    } elseif ($value['type'] == 'bool') {
                        if ($this->$field == 1 || $this->$field == '1') {
                            $checked = 'checked';
                        } else {
                            $checked = '';
                        }
                        $form .= "<input type='checkbox' name='".$prefix.$field."' id='".$prefix.$field."'  value='1' ".$checked."/>";
                    } elseif ($value['type'] == 'text') {
                        $form .= "<textarea name='".$prefix.$field."' rows='6' cols='50'>".$this->$field."</textarea>";
                    } elseif ($value['type'] == 'date') {
                        $form .= "<input name='".$prefix.$field."' id='jscal_field".$field."' type='text'  size='11' maxlength='10' value='".$this->$field."'>&nbsp;<span id=\"jscal_trigger\" class='suitepicon suitepicon-module-calendar'></span> <span class='dateFormat'>yyyy-mm-dd</span><script type='text/javascript'>Calendar.setup ({inputField : 'jscal_field".$field."', ifFormat : '%Y-%m-%d', showsTime : false, button : 'jscal_trigger".$field."', singleClick : true, step : 1, weekNumbers:false}); addToValidate('ConvertLead', '".$field."', 'date', false,'".$mod_strings[$tempBean->field_defs[$field]['vname']]."' );</script>";
                    } else {
                        if (!isset($this->$field)) {
                            LoggerManager::getLogger()->warn('Field not found: ' . $field);
                            $thisField = null;
                        } else {
                            $thisField = $this->$field;
                        }

                        $form .= "<input name='".$prefix.$field."' type='text' value='".$thisField."'>";

                        if (!isset($this->custom_fields->avail_fields)) {
                            LoggerManager::getLogger()->warn('Undefined property: $avail_fields');
                        }

                        if (isset($this->custom_fields->avail_fields) && $this->custom_fields->avail_fields[$field]['type'] == 'int') {
                            $form .= "<script>addToValidate('ConvertLead', '".$prefix.$field."', 'int', false,'".$prefix.":".$mod_strings[$tempBean->field_defs[$field]['vname']]."' );</script>";
                        } elseif (isset($this->custom_fields->avail_fields) && $this->custom_fields->avail_fields[$field]['type'] == 'float') {
                            $form .= "<script>addToValidate('ConvertLead', '".$prefix.$field."', 'float', false,'".$prefix.":".$mod_strings[$tempBean->field_defs[$field]['vname']]."' );</script>";
                        }
                    }

                    if (!empty($tempBean->custom_fields->avail_fields[$field]['required']) && ($tempBean->custom_fields->avail_fields[$field]['required']== 1 || $tempBean->custom_fields->avail_fields[$field]['required']== '1' || $tempBean->custom_fields->avail_fields[$field]['required']== 'true' || $tempBean->custom_fields->avail_fields[$field]['required']== true)) {
                        $form .= "<script>addToValidate('ConvertLead', '".$prefix.$field."', 'relate', true,'".$prefix.":".$mod_strings[$tempBean->field_defs[$field]['vname']]."' );</script>";
                    }

                    $form .= "</td></tr>";
                }
            }
        }

        return true;
    }

    public function save($check_notify = false)
    {
        if (empty($this->status)) {
            $this->status = 'New';
        }
        // call save first so that $this->id will be set
        $value = parent::save($check_notify);
        return $value;
    }
    public function get_unlinked_email_query($type=array())
    {
        return get_unlinked_email_query($type, $this);
    }

    /**
     * Returns query to find the related calls created pre-5.1
     *
     * @return string SQL statement
     */
    public function get_old_related_calls()
    {
        $return_array = [];
        $return_array['select']='SELECT calls.id ';
        $return_array['from']='FROM calls ';
        $return_array['where']=" WHERE calls.parent_id = '$this->id'
            AND calls.parent_type = 'Leads' AND calls.id NOT IN ( SELECT call_id FROM calls_leads ) ";
        $return_array['join'] = "";
        $return_array['join_tables'][0] = '';

        return $return_array;
    }

    /**
     * Returns array of lead conversion activity options
     *
     * @return string SQL statement
     */
    public static function getActivitiesOptions()
    {
        if (isset($GLOBALS['app_list_strings']['lead_conv_activity_opt'])) {
            return $GLOBALS['app_list_strings']['lead_conv_activity_opt'];
        }
        return array();
    }

    /**
     * Returns query to find the related meetings created pre-5.1
     *
     * @return string SQL statement
     */
    public function get_old_related_meetings()
    {
        $return_array = [];
        $return_array['select']='SELECT meetings.id ';
        $return_array['from']='FROM meetings ';
        $return_array['where']=" WHERE meetings.parent_id = '$this->id'
            AND meetings.parent_type = 'Leads' AND meetings.id NOT IN ( SELECT meeting_id FROM meetings_leads ) ";
        $return_array['join'] = "";
        $return_array['join_tables'][0] = '';

        return $return_array;
    }
}
