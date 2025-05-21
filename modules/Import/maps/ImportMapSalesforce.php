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

 * Description: Holds import setting for Salesforce.com files
 * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
 * All Rights Reserved.
 */

require_once('modules/Import/maps/ImportMapOther.php');

#[\AllowDynamicProperties]
class ImportMapSalesforce extends ImportMapOther
{
    /**
     * String identifier for this import
     */
    public $name = 'salesforce';
    /**
     * Field delimiter
     */
    public $delimiter = ',';
    /**
     * Field enclosure
     */
    public $enclosure = '"';
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
        $return_array = parent::getMapping($module);
        switch ($module) {
        case 'Contacts':
        case 'Leads':
            return $return_array + array(
                "Description"=>"description",
                "Birthdate"=>"birthdate",
                "Lead Source"=>"lead_source",
                "Assistant"=>"assistant",
                "Asst. Phone"=>"assistant_phone",
                "Mailing Street"=>"primary_address_street",
                "Mailing Address Line1"=>"primary_address_street_2",
                "Mailing Address Line2"=>"primary_address_street_3",
                "Mailing Address Line3"=>"primary_address_street_4",
                "Mailing City"=>"primary_address_city",
                "Mailing State"=>"primary_address_state",
                "Mailing Zip/Postal Code"=>"primary_address_postalcode",
                "Mailing Country"=>"primary_address_country",
                "Other Street"=>"alt_address_street",
                "Other Address Line 1"=>"alt_address_street_2",
                "Other Address Line 2"=>"alt_address_street_3",
                "Other Address Line 3"=>"alt_address_street_4",
                "Other City"=>"alt_address_city",
                "Other State"=>"alt_address_state",
                "Other Zip/Postal Code"=>"alt_address_postalcode",
                "Other Country"=>"alt_address_country",
                "Phone"=>"phone_work",
                "Mobile"=>"phone_mobile",
                "Home Phone"=>"phone_home",
                "Other Phone"=>"phone_other",
                "Fax"=>"phone_fax",
                "Email"=>"email1",
                "Email Opt Out"=>"email_opt_out",
                "Do Not Call"=>"do_not_call",
                "Account Name"=>"account_name",
                );
            break;
        case 'Accounts':
            return array(
                "Account Name"=>"name",
                "Annual Revenue"=>"annual_revenue",
                "Type"=>"account_type",
                "Ticker Symbol"=>"ticker_symbol",
                "Rating"=>"rating",
                "Industry"=>"industry",
                "SIC Code"=>"sic_code",
                "Ownership"=>"ownership",
                "Employees"=>"employees",
                "Description"=>"description",
                "Billing Street"=>"billing_address_street",
                "Billing Address Line1"=>"billing_address_street_2",
                "Billing Address Line2"=>"billing_address_street_3",
                "Billing City"=>"billing_address_city",
                "Billing State"=>"billing_address_state",
                "Billing State/Province"=>"billing_address_state",
                "Billing Zip/Postal Code"=>"billing_address_postalcode",
                "Billing Country"=>"billing_address_country",
                "Shipping Street"=>"shipping_address_street",
                "Shipping Address Line1"=>"shipping_address_street_2",
                "Shipping Address Line2"=>"shipping_address_street_3",
                "Shipping City"=>"shipping_address_city",
                "Shipping State"=>"shipping_address_state",
                "Shipping Zip/Postal Code"=>"shipping_address_postalcode",
                "Shipping Country"=>"shipping_address_country",
                "Phone"=>"phone_office",
                "Fax"=>"phone_fax",
                "Website"=>"website",
                "Created Date"=>"date_entered",
                );
            break;
        default:
            return $return_array;
        }
    }
    
    /**
    * @see ImportMapOther::getIgnoredFields()
     */
    public function getIgnoredFields(
        $module
        ) {
        return array_merge(parent::getIgnoredFields($module), array('id'));
    }
}
