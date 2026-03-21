<?php
/**
 * SuiteCRM is a customer relationship management program developed by SuiteCRM Ltd.
 * Copyright (C) 2011 - 2025 SuiteCRM Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUITECRM, SUITECRM DISCLAIMS THE
 * WARRANTY OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License
 * version 3, these Appropriate Legal Notices must retain the display of the
 * "Supercharged by SuiteCRM" logo. If the display of the logos is not reasonably
 * feasible for technical reasons, the Appropriate Legal Notices must display
 * the words "Supercharged by SuiteCRM".
 */

#[\AllowDynamicProperties]
class AOS_Quotes_sugar extends Basic
{
    public $new_schema = true;
    public $module_dir = 'AOS_Quotes';
    public $object_name = 'AOS_Quotes';
    public $table_name = 'aos_quotes';
    public $importable = true;
    public $lineItems = true;

    public $disable_row_level_security = true ; // to ensure that modules created and deployed under CE will continue to function under team security if the instance is upgraded to PRO

    public $id;
    public $name;
    public $date_entered;
    public $date_modified;
    public $modified_user_id;
    public $modified_by_name;
    public $created_by;
    public $created_by_name;
    public $description;
    public $deleted;
    public $created_by_link;
    public $modified_user_link;
    public $assigned_user_id;
    public $assigned_user_name;
    public $assigned_user_link;
    public $aos_quotes_type;
    public $industry;
    public $annual_revenue;
    public $phone_fax;
    public $billing_address_street;
    public $billing_address_city;
    public $billing_address_state;
    public $billing_address_postalcode;
    public $billing_address_country;
    public $rating;
    public $phone_office;
    public $phone_alternate;
    public $website;
    public $ownership;
    public $employees;
    public $ticker_symbol;
    public $shipping_address_street;
    public $shipping_address_city;
    public $shipping_address_state;
    public $shipping_address_postalcode;
    public $shipping_address_country;
    public $email1;
    public $email_addresses_primary;
    public $approval_issue;
    public $billing_account_id;
    public $billing_account;
    public $billing_contact_id;
    public $billing_contact;
    public $expiration;
    public $number;
    public $opportunity_id;
    public $opportunity;
    public $shipping_account_id;
    public $shipping_account;
    public $template_ddown_c;
    public $shipping_contact_id;
    public $shipping_contact;
    public $subtotal_amount;
    public $tax_amount;
    public $shipping_amount;
    public $total_amount;
    public $stage;
    public $term;
    public $terms_c;
    public $approval_status;
    public $invoice_status;





    public function __construct()
    {
        parent::__construct();
    }




    public function bean_implements($interface)
    {
        switch ($interface) {
            case 'ACL': return true;
        }
        return false;
    }
}
