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
 
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

global $sugar_config;
$listViewDefs ['AOS_Quotes'] =
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
  'STAGE' =>
  array(
    'width' => '10%',
    'label' => 'LBL_STAGE',
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
  'EXPIRATION' =>
  array(
    'width' => '10%',
    'label' => 'LBL_EXPIRATION',
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
  ),
  'AOS_QUOTES_TYPE' =>
  array(
    'width' => '10%',
    'label' => 'LBL_TYPE',
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
