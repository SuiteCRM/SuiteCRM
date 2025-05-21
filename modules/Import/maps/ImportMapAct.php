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

 * Description: Holds import setting for ACT! files
 * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
 * All Rights Reserved.
 */

require_once('modules/Import/maps/ImportMapOther.php');

#[\AllowDynamicProperties]
class ImportMapAct extends ImportMapOther
{
    /**
     * String identifier for this import
     */
    public $name = 'act';
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
                "Web Site"=>"website",
                "Company"=>"account_name",
                "Name Suffix"=>"salutation",
                "Address 1"=>"primary_address_street",
                "Address 2"=>"primary_address_street_2",
                "Address 3"=>"primary_address_street_3",
                "City"=>"primary_address_city",
                "State"=>"primary_address_state",
                "Zip"=>"primary_address_postalcode",
                "Country"=>"primary_address_country",
                "Phone"=>"phone_work",
                "Phone Ext-"=>"phone_work_ext",
                "Mobile Phone"=>"phone_mobile",
                "Alt Phone"=>"phone_other",
                "Fax"=>"phone_fax",
                "E-mail Login"=>"email1",
                "E-mail"=>"email1",
                "Assistant"=>"assistant",
                "Asst. Phone"=>"assistant_phone",
                "Home Address 1"=>"alt_address_street",
                "Home Address 2"=>"alt_address_street_2",
                "Home Address 3"=>"alt_address_street_3",
                "Home Zip"=>"alt_address_postalcode",
                "Home Country"=>"alt_address_country",
                "Home Phone"=>"phone_home",
                );
            break;
        case 'Accounts':
            return $return_array + array(
                "Revenue"=>"annual_revenue",
                "Number of Employees"=>"employees",
                "Address 1"=>"billing_address_street",
                "City"=>"billing_address_city",
                "State"=>"billing_address_state",
                "Zip Code"=>"billing_address_postalcode",
                "Country"=>"billing_address_country",
                "Phone"=>"phone_office",
                "Fax Phone"=>"phone_fax",
                "Ticker Symbol"=>"ticker_symbol",
                );
            break;
        default:
            return $return_array;
        }
    }
}
