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



require_once('modules/Import/maps/ImportMapOther.php');

#[\AllowDynamicProperties]
class ImportMapGoogle extends ImportMapOther
{
    /**
     * String identifier for this import
     */
    public $name = 'google';
    
    /**
     * Gets the default mapping for a module
     *
     * @param  string $module
     * @return array field mappings
     */
    public function getMapping($module)
    {
        $return_array = array(
             'first_name' => array('sugar_key' => 'first_name', 'sugar_label' => '', 'default_label' => 'Given Name'),
             'last_name' => array('sugar_key' => 'last_name', 'sugar_label' => '', 'default_label' => 'Family Name'),
             'birthday' => array('sugar_key' => 'birthdate', 'sugar_label' => '', 'default_label' => 'Birthday'),
             'title' => array('sugar_key' => 'title', 'sugar_label' => '', 'default_label' => 'Title'),
             'notes' => array('sugar_key' => 'description', 'sugar_label' => '', 'default_label' => 'Notes'),

             'alt_address_street' => array('sugar_key' => 'alt_address_street', 'sugar_label' => '', 'default_label' => 'Home Street'),
             'alt_address_postcode' => array('sugar_key' => 'alt_address_postalcode', 'sugar_label' => '', 'default_label' => 'Home Postcode'),
             'alt_address_city' => array('sugar_key' => 'alt_address_city', 'sugar_label' => '', 'default_label' => 'Home City'),
             'alt_address_state' => array('sugar_key' => 'alt_address_state', 'sugar_label' => '', 'default_label' => 'Home State'),
             'alt_address_country' => array('sugar_key' => 'alt_address_country', 'sugar_label' => '', 'default_label' => 'Home Country'),

             'primary_address_street' => array('sugar_key' => 'primary_address_street', 'sugar_label' => '', 'default_label' => 'Work Street'),
             'primary_address_postcode' => array('sugar_key' => 'primary_address_postalcode', 'sugar_label' => '', 'default_label' => 'Work Postcode'),
             'primary_address_city' => array('sugar_key' => 'primary_address_city', 'sugar_label' => '', 'default_label' => 'Work City'),
             'primary_address_state' => array('sugar_key' => 'primary_address_state', 'sugar_label' => '', 'default_label' => 'Work State'),
             'primary_address_country' => array('sugar_key' => 'primary_address_country', 'sugar_label' => '', 'default_label' => 'Work Country'),

             'phone_main' => array('sugar_key' => 'phone_other', 'sugar_label' => '', 'default_label' => 'Main Phone'),
             'phone_mobile' => array('sugar_key' => 'phone_mobile', 'sugar_label' => '', 'default_label' => 'Mobile Phone'),
             'phone_home' => array('sugar_key' => 'phone_home', 'sugar_label' => '', 'default_label' => 'Home phone'),
             'phone_work' => array('sugar_key' => 'phone_work', 'sugar_label' => '', 'default_label' => 'Work phone'),
             'phone_fax' => array('sugar_key' => 'phone_fax', 'sugar_label' => '', 'default_label' => 'Fax phone'),

             'email1' => array('sugar_key' => 'email1', 'sugar_label' => 'LBL_EMAIL_ADDRESS', 'default_label' => 'Email Address'),
             'email2' => array('sugar_key' => 'email2', 'sugar_label' => 'LBL_OTHER_EMAIL_ADDRESS', 'default_label' => 'Other Email'),

             'assigned_user_name' => array('sugar_key' => 'assigned_user_name', 'sugar_help_key' => 'LBL_EXTERNAL_ASSIGNED_TOOLTIP', 'sugar_label' => 'LBL_ASSIGNED_TO_NAME', 'default_label' => 'Assigned To'),
             'team_name' => array('sugar_key' => 'team_name', 'sugar_help_key' => 'LBL_EXTERNAL_TEAM_TOOLTIP','sugar_label' => 'LBL_TEAMS', 'default_label' => 'Teams'),
            );

        if ($module == 'Users') {
            $return_array['status'] =  array('sugar_key' => 'status', 'sugar_label' => '', 'default_label' => 'Status');
            $return_array['full_name'] =  array('sugar_key' => 'user_name', 'sugar_label' => '', 'default_label' => 'Full Name');
        }
        return $return_array;
    }
}
