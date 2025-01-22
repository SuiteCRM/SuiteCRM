<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}
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
global $sugar_config;
$listViewDefs['AOS_Invoices'] =
array(
  'NUMBER' =>
  array(
    'width' => '5%',
    'label' => 'LBL_LIST_NUM',
    'default' => true,
  ),
  'NAME' =>
  array(
    'width' => '15%',
    'label' => 'LBL_ACCOUNT_NAME',
    'link' => true,
    'default' => true,
  ),
  'STATUS' =>
  array(
    'width' => '10%',
    'label' => 'LBL_STATUS',
    'default' => true,
  ),
  'BILLING_CONTACT' =>
  array(
    'width' => '11%',
    'label' => 'LBL_BILLING_CONTACT',
    'default' => true,
    'module' => 'Contacts',
    'id' => 'BILLING_CONTACT_ID',
    'link' => true,
      'related_fields' =>
          array(
              'billing_contact_id',
          ),
  ),
  'BILLING_ACCOUNT' =>
  array(
    'width' => '15%',
    'label' => 'LBL_BILLING_ACCOUNT',
    'default' => true,
    'module' => 'Accounts',
    'id' => 'BILLING_ACCOUNT_ID',
    'link' => true,
      'related_fields' =>
          array(
              'billing_account_id',
          ),
  ),
  'TOTAL_AMOUNT' =>
  array(
    'width' => '10%',
    'label' => 'LBL_GRAND_TOTAL',
    'default' => true,
    'currency_format' => true,
  ),
  'DUE_DATE' =>
  array(
    'width' => '10%',
    'label' => 'LBL_DUE_DATE',
    'default' => true,
  ),
  'ASSIGNED_USER_NAME' =>
  array(
    'width' => '10%',
    'label' => 'LBL_ASSIGNED_USER',
    'default' => true,
    'module' => 'Users',
    'id' => 'ASSIGNED_USER_ID',
    'link' => true,
    'related_fields' => array('assigned_user_id')
  ),
  'ANNUAL_REVENUE' =>
  array(
    'width' => '10%',
    'label' => 'LBL_ANNUAL_REVENUE',
    'default' => false,
  ),
  'BILLING_ADDRESS_STREET' =>
  array(
    'width' => '15%',
    'label' => 'LBL_BILLING_ADDRESS_STREET',
    'default' => false,
  ),
  'BILLING_ADDRESS_CITY' =>
  array(
    'width' => '10%',
    'label' => 'LBL_CITY',
    'default' => false,
  ),
  'BILLING_ADDRESS_STATE' =>
  array(
    'width' => '7%',
    'label' => 'LBL_BILLING_ADDRESS_STATE',
    'default' => false,
  ),
  'BILLING_ADDRESS_POSTALCODE' =>
  array(
    'width' => '10%',
    'label' => 'LBL_BILLING_ADDRESS_POSTALCODE',
    'default' => false,
  ),
  'BILLING_ADDRESS_COUNTRY' =>
  array(
    'width' => '10%',
    'label' => 'LBL_BILLING_ADDRESS_COUNTRY',
    'default' => false,
  ),
  'SHIPPING_ADDRESS_STREET' =>
  array(
    'width' => '15%',
    'label' => 'LBL_SHIPPING_ADDRESS_STREET',
    'default' => false,
  ),
  'SHIPPING_ADDRESS_CITY' =>
  array(
    'width' => '10%',
    'label' => 'LBL_SHIPPING_ADDRESS_CITY',
    'default' => false,
  ),
  'SHIPPING_ADDRESS_STATE' =>
  array(
    'width' => '7%',
    'label' => 'LBL_SHIPPING_ADDRESS_STATE',
    'default' => false,
  ),
  'SHIPPING_ADDRESS_POSTALCODE' =>
  array(
    'width' => '10%',
    'label' => 'LBL_SHIPPING_ADDRESS_POSTALCODE',
    'default' => false,
  ),
  'SHIPPING_ADDRESS_COUNTRY' =>
  array(
    'width' => '10%',
    'label' => 'LBL_SHIPPING_ADDRESS_COUNTRY',
    'default' => false,
  ),
  'PHONE_ALTERNATE' =>
  array(
    'width' => '10%',
    'label' => 'LBL_PHONE_ALT',
    'default' => false,
  ),
  'WEBSITE' =>
  array(
    'width' => '10%',
    'label' => 'LBL_WEBSITE',
    'default' => false,
  ),
  'OWNERSHIP' =>
  array(
    'width' => '10%',
    'label' => 'LBL_OWNERSHIP',
    'default' => false,
  ),
  'EMPLOYEES' =>
  array(
    'width' => '10%',
    'label' => 'LBL_EMPLOYEES',
    'default' => false,
  ),
  'TICKER_SYMBOL' =>
  array(
    'width' => '10%',
    'label' => 'LBL_TICKER_SYMBOL',
    'default' => false,
  ),
  'DATE_ENTERED' =>
  array(
    'width' => '5%',
    'label' => 'LBL_DATE_ENTERED',
    'default' => preg_match('/^6\.?[2-9]/', (string) $sugar_config['sugar_version']),
  ),
);
