<?php
/**
 * Products, Quotations & Invoices modules.
 * Extensions to SugarCRM
 * @package Advanced OpenSales for SugarCRM
 * @subpackage Products
 * @copyright SalesAgility Ltd http://www.salesagility.com
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU AFFERO GENERAL PUBLIC LICENSE as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU AFFERO GENERAL PUBLIC LICENSE
 * along with this program; if not, see http://www.gnu.org/licenses
 * or write to the Free Software Foundation,Inc., 51 Franklin Street,
 * Fifth Floor, Boston, MA 02110-1301  USA
 *
 * @author SalesAgility Ltd <support@salesagility.com>
 */
#[\AllowDynamicProperties]
class AOS_Invoices_sugar extends Basic
{
    public $new_schema = true;
    public $module_dir = 'AOS_Invoices';
    public $object_name = 'AOS_Invoices';
    public $table_name = 'aos_invoices';
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
    public $aos_invoices_type;
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
    public $billing_account_id;
    public $billing_account;
    public $billing_contact_id;
    public $billing_contact;
    public $number;
    public $shipping_account_id;
    public $shipping_account;
    public $shipping_contact_id;
    public $shipping_contact;
    public $subtotal_amount;
    public $tax_amount;
    public $shipping_amount;
    public $total_amount;
    public $quote_number;
    public $quote_date;
    public $invoice_date;
    public $due_date;
    public $status;
    public $template_ddown_c;





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
