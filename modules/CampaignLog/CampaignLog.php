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


#[\AllowDynamicProperties]
class CampaignLog extends SugarBean
{
    public $table_name = 'campaign_log';
    public $object_name = 'CampaignLog';
    public $module_dir = 'CampaignLog';

    public $new_schema = true;

    public $campaign_id;
    public $target_tracker_key;
    public $target_id;
    public $target_type;
    public $date_modified;
    public $activity_type;
    public $activity_date;
    public $related_id;
    public $related_type;
    public $deleted;
    public $list_id;
    public $hits;
    public $more_information;
    public $marketing_id;

    public function __construct()
    {
        global $sugar_config;
        parent::__construct();
    }




    public function get_list_view_data()
    {
        global $locale;
        $temp_array = $this->get_list_view_array();
        //make sure that both items in array are set to some value, else return null
        if (!(isset($temp_array['TARGET_TYPE']) && $temp_array['TARGET_TYPE']!= '') || !(isset($temp_array['TARGET_ID']) && $temp_array['TARGET_ID']!= '')) {   //needed values to construct query are empty/null, so return null
            $GLOBALS['log']->debug("CampaignLog.php:get_list_view_data: temp_array['TARGET_TYPE'] and/or temp_array['TARGET_ID'] are empty, return null");
            $emptyArr = array();
            return $emptyArr;
        }

        $table = strtolower($temp_array['TARGET_TYPE']);

        if ($temp_array['TARGET_TYPE']=='Accounts') {
            $query = "select name from $table where id = ".$this->db->quoted($temp_array['TARGET_ID']);
        } else {
            $query = "select first_name, last_name, ".$this->db->concat($table, array('first_name', 'last_name'))." name from $table" .
                " where id = ".$this->db->quoted($temp_array['TARGET_ID']);
        }
        $result=$this->db->query($query);
        $row=$this->db->fetchByAssoc($result);

        if ($row) {
            if ($temp_array['TARGET_TYPE']=='Accounts') {
                $temp_array['RECIPIENT_NAME']=$row['name'];
            } else {
                $full_name = $locale->getLocaleFormattedName($row['first_name'], $row['last_name'], '');
                $temp_array['RECIPIENT_NAME']=$full_name;
            }
        }
        $temp_array['RECIPIENT_EMAIL']=$this->retrieve_email_address($temp_array['TARGET_ID']);

        $query = 'select name from email_marketing where id = \'' . $temp_array['MARKETING_ID'] . '\'';
        $result=$this->db->query($query);
        $row=$this->db->fetchByAssoc($result);

        if ($row) {
            $temp_array['MARKETING_NAME'] = $row['name'];
        }

        return $temp_array;
    }

    public function retrieve_email_address($trgt_id = '')
    {
        $return_str = '';
        if (!empty($trgt_id)) {
            $qry  = " select eabr.primary_address, ea.email_address";
            $qry .= " from email_addresses ea ";
            $qry .= " Left Join email_addr_bean_rel eabr on eabr.email_address_id = ea.id ";
            $qry .= " where eabr.bean_id = '{$trgt_id}' ";
            $qry .= " and ea.deleted = 0 ";
            $qry .= " and eabr.deleted = 0" ;
            $qry .= " order by primary_address desc ";

            $result=$this->db->query($qry);
            $row=$this->db->fetchByAssoc($result);

            if (!empty($row['email_address'])) {
                $return_str = $row['email_address'];
            }
        }
        return $return_str;
    }




    //this function is called statically by the campaign_log subpanel.
    public static function get_related_name($related_id, $related_type)
    {
        global $locale;
        $db= DBManagerFactory::getInstance();
        if ($related_type == 'Emails') {
            $query="SELECT name from emails where id='$related_id'";
            $result=$db->query($query);
            $row=$db->fetchByAssoc($result);
            if ($row != null) {
                return $row['name'];
            }
        }
        if ($related_type == 'Contacts') {
            $query="SELECT first_name, last_name from contacts where id='$related_id'";
            $result=$db->query($query);
            $row=$db->fetchByAssoc($result);
            if ($row != null) {
                return $full_name = $locale->getLocaleFormattedName($row['first_name'], $row['last_name']);
            }
        }
        if ($related_type == 'Leads') {
            $query="SELECT first_name, last_name from leads where id='$related_id'";
            $result=$db->query($query);
            $row=$db->fetchByAssoc($result);
            if ($row != null) {
                return $full_name = $locale->getLocaleFormattedName($row['first_name'], $row['last_name']);
            }
        }
        if ($related_type == 'Prospects') {
            $query="SELECT first_name, last_name from prospects where id='$related_id'";
            $result=$db->query($query);
            $row=$db->fetchByAssoc($result);
            if ($row != null) {
                return $full_name = $locale->getLocaleFormattedName($row['first_name'], $row['last_name']);
            }
        }
        if ($related_type == 'CampaignTrackers') {
            $query="SELECT tracker_url from campaign_trkrs where id='$related_id'";
            $result=$db->query($query);
            $row=$db->fetchByAssoc($result);
            if ($row != null) {
                return $row['tracker_url'] ;
            }
        }
        if ($related_type == 'Accounts') {
            $query="SELECT name from accounts where id='$related_id'";
            $result=$db->query($query);
            $row=$db->fetchByAssoc($result);
            if ($row != null) {
                return $row['name'];
            }
        }
        return $related_id.$related_type;
    }
}
