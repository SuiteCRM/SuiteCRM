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

/**

 * Description: The primary Function of this file is to manage all the data
 * used by other files in this nodule. It should extend the SugarBean which implements
 * all the basic database operations. Any custom behaviors can be implemented here by
 * implementing functions available in the SugarBean.
 */






#[\AllowDynamicProperties]
class CampaignTracker extends SugarBean
{
    /* Foreach instance of the bean you will need to access the fields in the table.
    * So define a variable for each one of them, the variable name should be same as the field name
    * Use this module's vardef file as a reference to create these variables.
    */
    public $id;
    public $date_entered;
    public $created_by;
    public $date_modified;
    public $modified_by;
    public $deleted;
    public $tracker_key;
    public $tracker_url;
    public $tracker_name;
    public $campaign_id;
    public $campaign_name;
    public $message_url;
    public $is_optout;

    /* End field definitions*/

    /* variable $table_name is used by SugarBean and methods in this file to constructs queries
    * set this variables value to the table associated with this bean.
    */
    public $table_name = 'campaign_trkrs';

    /*This  variable overrides the object_name variable in SugarBean, wher it has a value of null.*/
    public $object_name = 'CampaignTracker';

    /**/
    public $module_dir = 'CampaignTrackers';

    /* This is a legacy variable, set its value to true for new modules*/
    public $new_schema = true;

    /* $column_fields holds a list of columns that exist in this bean's table. This list is referenced
    * when fetching or saving data for the bean. As you modify a table you need to keep this up to date.
    */
    public $column_fields = array(
            'id'
            ,'tracker_key'
            ,'tracker_url'
            ,'tracker_name'
            ,'campaign_id'
    );

    // This is used to retrieve related fields from form posts.
    public $additional_column_fields = array('campaign_id');
    public $relationship_fields = array('campaing_id'=>'campaign');

    public $required_fields =  array('tracker_name'=>1,'tracker_url'=>1);

    /*This bean's constructor*/
    public function __construct()
    {
        parent::__construct();
    }




    public function save($check_notify = false)
    {
        //make sure that the url has a scheme, if not then add http:// scheme
        if ($this->is_optout!=1) {
            $url = strtolower(trim($this->tracker_url));
            if (!preg_match('/^(http|https|ftp):\/\//i', $url)) {
                $this->tracker_url = 'http://'.$url;
            }
        }

        return parent::save($check_notify);
    }

    /* This method should return the summary text which is used to build the bread crumb navigation*/
    /* Generally from this method you would return value of a field that is required and is of type string*/
    public function get_summary_text()
    {
        return (string)$this->tracker_name;
    }


    /* This method is used to generate query for the list form. The base implementation of this method
    * uses the table_name and list_field variable to generate the basic query and then  adds the custom field
    * join and team filter. If you are implementing this function do not forget to consider the additional conditions.
    */

    public function fill_in_additional_detail_fields()
    {
        global $sugar_config;

        //setup campaign name.
        $query = "SELECT name from campaigns where id = '$this->campaign_id'";
        $result =$this->db->query($query, true, " Error filling in additional detail fields: ");

        // Get the id and the name.
        $row = $this->db->fetchByAssoc($result);
        if ($row != null) {
            $this->campaign_name=$row['name'];
        }

        if (!class_exists('Administration')) {
        }
        $admin=BeanFactory::newBean('Administration');
        $admin->retrieveSettings('massemailer'); //retrieve all admin settings.
        if (isset($admin->settings['massemailer_tracking_entities_location_type']) && $admin->settings['massemailer_tracking_entities_location_type']=='2' && isset($admin->settings['massemailer_tracking_entities_location'])) {
            $this->message_url=$admin->settings['massemailer_tracking_entities_location'];
        } else {
            $this->message_url=$sugar_config['site_url'];
        }
        if ($this->is_optout == 1) {
            $this->message_url .= '/index.php?entryPoint=removeme&identifier={MESSAGE_ID}';
        } else {
            $this->message_url .= '/index.php?entryPoint=campaign_trackerv2&track=' . $this->id;
        }
    }
}
