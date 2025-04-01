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

 * Description: Holds import setting for standard delimited files
 * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
 * All Rights Reserved.
 */

#[\AllowDynamicProperties]
class ImportMapOther
{
    /**
     * String identifier for this import
     */
    public $name = 'other';
    /**
     * Field delimiter
     */
    public $delimiter;
    /**
     * Field enclosure
     */
    public $enclosure;
    /**
     * Do we have a header?
     */
    public $has_header = true;

    /**
     * Gets the default mapping for a module
     *
     * @param  string $module
     * @return array field mappings
     */
    public function getMapping(
        $module
        ) {
        switch ($module) {
        case 'Contacts':
        case 'Leads':
            return array(
                "Salutation"=>"salutation",
                "Full Name"=>"full_name",
                "Company"=>"company",
                "First Name"=>"first_name",
                "Last Name"=>"last_name",
                "Title"=>"title",
                "Department"=>"department",
                "Birthday"=>"birthdate",
                "Home Phone"=>"phone_home",
                "Mobile Phone"=>"phone_mobile",
                "Business Phone"=>"phone_work",
                "Other Phone"=>"phone_other",
                "Business Fax"=>"phone_fax",
                "E-mail Address"=>"email1",
                "E-mail 2"=>"email2",
                "Assistant's Name"=>"assistant",
                "Assistant's Phone"=>"assistant_phone",
                "Business Street"=>"primary_address_street",
                "Business Street 2"=>"primary_address_street_2",
                "Business Street 3"=>"primary_address_street_3",
                "Business City"=>"primary_address_city",
                "Business State"=>"primary_address_state",
                "Business Postal Code"=>"primary_address_postalcode",
                "Business Country/Region"=>"primary_address_country",
                "Home Street"=>"alt_address_street",
                "Home Street 2"=>"alt_address_street_2",
                "Home Street 3"=>"alt_address_street_3",
                "Home City"=>"alt_address_city",
                "Home State"=>"alt_address_state",
                "Home Postal Code"=>"alt_address_postalcode",
                "Home Country/Region"=>"alt_address_country",
                );
            break;
        case 'Accounts':
            return array(
                "Company"=>"name",
                "Business Street"=>"billing_address_street",
                "Business City"=>"billing_address_city",
                "Business State"=>"billing_address_state",
                "Business Country"=>"billing_address_country",
                "Business Postal Code"=>"billing_address_postalcode",
                "Business Fax"=>"phone_fax",
                "Company Main Phone"=>"phone_office",
                "Web Page"=>"website",
                );
            break;
        case 'Opportunities':
            return array(
                "Opportunity Name"=>"name" ,
                "Type"=>"opportunity_type",
                "Lead Source"=>"lead_source",
                "Amount"=>"amount",
                "Created Date"=>"date_entered",
                "Close Date"=>"date_closed",
                "Next Step"=>"next_step",
                "Stage"=>"sales_stage",
                "Probability (%)"=>"probability",
                "Account Name"=>"account_name");
            break;
        default:
            return array();
        }
    }
    
    /**
     * Returns a list of fields that should be ignorred for the module during import
     *
     * @param  string $module
     * @return array of fields to ignor
     */
    public function getIgnoredFields(
        $module
        ) {
        return array();
    }
}
