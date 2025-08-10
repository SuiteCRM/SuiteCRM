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


$dictionary['AOS_Invoices'] = array(
    'table' => 'aos_invoices',
    'audited' => true,
    'fields' => array(
        'billing_account_id' =>
            array(
                'required' => false,
                'name' => 'billing_account_id',
                'vname' => '',
                'type' => 'id',
                'massupdate' => 0,
                'comments' => '',
                'help' => '',
                'importable' => 'true',
                'duplicate_merge' => 'disabled',
                'duplicate_merge_dom_value' => 0,
                'audited' => 0,
                'reportable' => 0,
                'len' => 36,
            ),
        'billing_account' =>
            array(
                'required' => false,
                'source' => 'non-db',
                'name' => 'billing_account',
                'vname' => 'LBL_BILLING_ACCOUNT',
                'type' => 'relate',
                'massupdate' => 0,
                'comments' => '',
                'help' => '',
                'importable' => 'true',
                'duplicate_merge' => 'disabled',
                'duplicate_merge_dom_value' => '0',
                'audited' => 1,
                'reportable' => 1,
                'len' => '255',
                'id_name' => 'billing_account_id',
                'ext2' => 'Accounts',
                'module' => 'Accounts',
                'quicksearch' => 'enabled',
                'studio' => 'visible',
            ),
        'billing_contact_id' =>
            array(
                'required' => false,
                'name' => 'billing_contact_id',
                'vname' => '',
                'type' => 'id',
                'massupdate' => 0,
                'comments' => '',
                'help' => '',
                'importable' => 'true',
                'duplicate_merge' => 'disabled',
                'duplicate_merge_dom_value' => 0,
                'audited' => 0,
                'reportable' => 0,
                'len' => 36,
            ),
        'billing_contact' =>
            array(
                'required' => false,
                'source' => 'non-db',
                'name' => 'billing_contact',
                'vname' => 'LBL_BILLING_CONTACT',
                'type' => 'relate',
                'massupdate' => 0,
                'comments' => '',
                'help' => '',
                'importable' => 'true',
                'duplicate_merge' => 'disabled',
                'duplicate_merge_dom_value' => '0',
                'audited' => 1,
                'reportable' => 1,
                'len' => '255',
                'id_name' => 'billing_contact_id',
                'ext2' => 'Contacts',
                'module' => 'Contacts',
                'quicksearch' => 'enabled',
                'studio' => 'visible',
            ),

        'billing_address_street' =>
            array(
                'name' => 'billing_address_street',
                'vname' => 'LBL_BILLING_ADDRESS_STREET',
                'type' => 'varchar',
                'len' => '150',
                'comment' => 'The street address used for billing address',
                'group' => 'billing_address',
                'merge_filter' => 'enabled',
            ),
        'billing_address_city' =>
            array(
                'name' => 'billing_address_city',
                'vname' => 'LBL_BILLING_ADDRESS_CITY',
                'type' => 'varchar',
                'len' => '100',
                'comment' => 'The city used for billing address',
                'group' => 'billing_address',
                'merge_filter' => 'enabled',
            ),
        'billing_address_state' =>
            array(
                'name' => 'billing_address_state',
                'vname' => 'LBL_BILLING_ADDRESS_STATE',
                'type' => 'varchar',
                'len' => '100',
                'group' => 'billing_address',
                'comment' => 'The state used for billing address',
                'merge_filter' => 'enabled',
            ),
        'billing_address_postalcode' =>
            array(
                'name' => 'billing_address_postalcode',
                'vname' => 'LBL_BILLING_ADDRESS_POSTALCODE',
                'type' => 'varchar',
                'len' => '20',
                'group' => 'billing_address',
                'comment' => 'The postal code used for billing address',
                'merge_filter' => 'enabled',

            ),
        'billing_address_country' =>
            array(
                'name' => 'billing_address_country',
                'vname' => 'LBL_BILLING_ADDRESS_COUNTRY',
                'type' => 'varchar',
                'group' => 'billing_address',
                'comment' => 'The country used for the billing address',
                'merge_filter' => 'enabled',
            ),

        'shipping_address_street' =>
            array(
                'name' => 'shipping_address_street',
                'vname' => 'LBL_SHIPPING_ADDRESS_STREET',
                'type' => 'varchar',
                'len' => 150,
                'group' => 'shipping_address',
                'comment' => 'The street address used for for shipping purposes',
                'merge_filter' => 'enabled',
            ),
        'shipping_address_city' =>
            array(
                'name' => 'shipping_address_city',
                'vname' => 'LBL_SHIPPING_ADDRESS_CITY',
                'type' => 'varchar',
                'len' => 100,
                'group' => 'shipping_address',
                'comment' => 'The city used for the shipping address',
                'merge_filter' => 'enabled',
            ),
        'shipping_address_state' =>
            array(
                'name' => 'shipping_address_state',
                'vname' => 'LBL_SHIPPING_ADDRESS_STATE',
                'type' => 'varchar',
                'len' => 100,
                'group' => 'shipping_address',
                'comment' => 'The state used for the shipping address',
                'merge_filter' => 'enabled',
            ),
        'shipping_address_postalcode' =>
            array(
                'name' => 'shipping_address_postalcode',
                'vname' => 'LBL_SHIPPING_ADDRESS_POSTALCODE',
                'type' => 'varchar',
                'len' => 20,
                'group' => 'shipping_address',
                'comment' => 'The zip code used for the shipping address',
                'merge_filter' => 'enabled',
            ),
        'shipping_address_country' =>
            array(
                'name' => 'shipping_address_country',
                'vname' => 'LBL_SHIPPING_ADDRESS_COUNTRY',
                'type' => 'varchar',
                'group' => 'shipping_address',
                'comment' => 'The country used for the shipping address',
                'merge_filter' => 'enabled',
            ),

        'number' =>
            array(
                'required' => true,
                'name' => 'number',
                'vname' => 'LBL_INVOICE_NUMBER',
                'type' => 'int',
                'len' => 11,
                'isnull' => 'false',
                'unified_search' => true,
                'comments' => '',
                'importable' => 'true',
                'duplicate_merge' => 'disabled',
                'reportable' => true,
                'disable_num_format' => true,
            ),
        'line_items' =>
            array(
                'required' => false,
                'name' => 'line_items',
                'vname' => 'LBL_LINE_ITEMS',
                'type' => 'function',
                'source' => 'non-db',
                'massupdate' => 0,
                'importable' => 'false',
                'duplicate_merge' => 'disabled',
                'duplicate_merge_dom_value' => 0,
                'audited' => false,
                'reportable' => false,
                'inline_edit' => false,
                'studio' => array(
                    'searchview' => false,
                ),
                'function' =>
                    array(
                        'name' => 'display_lines',
                        'returns' => 'html',
                        'include' => 'modules/AOS_Products_Quotes/Line_Items.php'
                    ),
            ),
        'total_amt' =>
            array(
                'required' => false,
                'name' => 'total_amt',
                'vname' => 'LBL_TOTAL_AMT',
                'type' => 'currency',
                'massupdate' => 0,
                'comments' => '',
                'help' => '',
                'importable' => 'true',
                'duplicate_merge' => 'disabled',
                'duplicate_merge_dom_value' => '0',
                'audited' => 1,
                'reportable' => true,
                'len' => '26,6',
            ),
        'total_amt_usdollar' =>
            array(
                'name' => 'total_amt_usdollar',
                'vname' => 'LBL_TOTAL_AMT_USDOLLAR',
                'type' => 'currency',
                'group' => 'total_amt',
                'disable_num_format' => true,
                'duplicate_merge' => '0',
                'audited' => true,
                'comment' => '',
                'studio' => array(
                    'editview' => false,
                    'detailview' => false,
                    'quickcreate' => false,
                ),
                'len' => '26,6',
            ),
        'subtotal_amount' =>
            array(
                'required' => false,
                'name' => 'subtotal_amount',
                'vname' => 'LBL_SUBTOTAL_AMOUNT',
                'type' => 'currency',
                'massupdate' => 0,
                'comments' => '',
                'help' => '',
                'importable' => 'true',
                'duplicate_merge' => 'disabled',
                'duplicate_merge_dom_value' => '0',
                'audited' => 1,
                'reportable' => true,
                'len' => '26,6',
            ),
        'subtotal_amount_usdollar' =>
            array(
                'name' => 'subtotal_amount_usdollar',
                'vname' => 'LBL_SUBTOTAL_AMOUNT_USDOLLAR',
                'type' => 'currency',
                'group' => 'subtotal_amount',
                'disable_num_format' => true,
                'duplicate_merge' => '0',
                'audited' => true,
                'comment' => 'Formatted amount of the opportunity',
                'studio' => array(
                    'editview' => false,
                    'detailview' => false,
                    'quickcreate' => false,
                ),
                'len' => '26,6',
            ),
        'discount_amount' =>
            array(
                'required' => false,
                'name' => 'discount_amount',
                'vname' => 'LBL_DISCOUNT_AMOUNT',
                'type' => 'currency',
                'massupdate' => 0,
                'comments' => '',
                'help' => '',
                'importable' => 'true',
                'duplicate_merge' => 'disabled',
                'duplicate_merge_dom_value' => '0',
                'audited' => 1,
                'reportable' => true,
                'len' => '26,6',
            ),
        'discount_amount_usdollar' =>
            array(
                'name' => 'discount_amount_usdollar',
                'vname' => 'LBL_DISCOUNT_AMOUNT_USDOLLAR',
                'type' => 'currency',
                'group' => 'discount_amount',
                'disable_num_format' => true,
                'duplicate_merge' => '0',
                'audited' => true,
                'comment' => '',
                'studio' => array(
                    'editview' => false,
                    'detailview' => false,
                    'quickcreate' => false,
                ),
                'len' => '26,6',
            ),
        'tax_amount' =>
            array(
                'required' => false,
                'name' => 'tax_amount',
                'vname' => 'LBL_TAX_AMOUNT',
                'type' => 'currency',
                'massupdate' => 0,
                'comments' => '',
                'help' => '',
                'importable' => 'true',
                'duplicate_merge' => 'disabled',
                'duplicate_merge_dom_value' => '0',
                'audited' => 1,
                'reportable' => true,
                'len' => '26,6',
            ),
        'tax_amount_usdollar' =>
            array(
                'name' => 'tax_amount_usdollar',
                'vname' => 'LBL_TAX_AMOUNT_USDOLLAR',
                'type' => 'currency',
                'group' => 'tax_amount',
                'disable_num_format' => true,
                'duplicate_merge' => '0',
                'audited' => true,
                'comment' => '',
                'studio' => array(
                    'editview' => false,
                    'detailview' => false,
                    'quickcreate' => false,
                ),
                'len' => '26,6',
            ),
        'shipping_amount' =>
            array(
                'required' => false,
                'name' => 'shipping_amount',
                'vname' => 'LBL_SHIPPING_AMOUNT',
                'type' => 'currency',
                'massupdate' => 0,
                'comments' => '',
                'help' => '',
                'importable' => 'true',
                'duplicate_merge' => 'disabled',
                'duplicate_merge_dom_value' => '0',
                'audited' => 0,
                'reportable' => true,
                'len' => '26,6',
            ),
        'shipping_amount_usdollar' =>
            array(
                'name' => 'shipping_amount_usdollar',
                'vname' => 'LBL_SHIPPING_AMOUNT_USDOLLAR',
                'type' => 'currency',
                'group' => 'shipping_amount',
                'disable_num_format' => true,
                'duplicate_merge' => '0',
                'audited' => true,
                'comment' => '',
                'studio' => array(
                    'editview' => false,
                    'detailview' => false,
                    'quickcreate' => false,
                ),
                'len' => '26,6',
            ),
        'shipping_tax' =>
            array(
                'required' => false,
                'name' => 'shipping_tax',
                'vname' => 'LBL_SHIPPING_TAX',
                'type' => 'enum',
                'massupdate' => 0,
                'comments' => '',
                'help' => '',
                'importable' => 'true',
                'duplicate_merge' => 'disabled',
                'duplicate_merge_dom_value' => '0',
                'audited' => 0,
                'reportable' => true,
                'len' => 100,
                'options' => 'vat_list',
                'studio' => 'visible',
            ),
        'shipping_tax_amt' =>
            array(
                'required' => false,
                'name' => 'shipping_tax_amt',
                'vname' => 'LBL_SHIPPING_TAX_AMT',
                'type' => 'currency',
                'massupdate' => 0,
                'comments' => '',
                'help' => '',
                'importable' => 'true',
                'duplicate_merge' => 'disabled',
                'duplicate_merge_dom_value' => '0',
                'audited' => 0,
                'reportable' => true,
                'len' => '26,6',
                'size' => '10',
                'enable_range_search' => false,
                'function' =>
                    array(
                        'name' => 'display_shipping_vat',
                        'returns' => 'html',
                        'include' => 'modules/AOS_Products_Quotes/Line_Items.php'
                    ),
            ),
        'shipping_tax_amt_usdollar' =>
            array(
                'name' => 'shipping_tax_amt_usdollar',
                'vname' => 'LBL_SHIPPING_TAX_AMT_USDOLLAR',
                'type' => 'currency',
                'group' => 'shipping_tax_amt',
                'disable_num_format' => true,
                'duplicate_merge' => '0',
                'audited' => true,
                'comment' => '',
                'studio' => array(
                    'editview' => false,
                    'detailview' => false,
                    'quickcreate' => false,
                ),
                'len' => '26,6',
            ),
        'total_amount' =>
            array(
                'required' => false,
                'name' => 'total_amount',
                'vname' => 'LBL_GRAND_TOTAL',
                'type' => 'currency',
                'massupdate' => 0,
                'comments' => '',
                'help' => '',
                'importable' => 'true',
                'duplicate_merge' => 'disabled',
                'duplicate_merge_dom_value' => '0',
                'audited' => false,
                'reportable' => true,
                'len' => '26,6',
                'enable_range_search' => true,
                'options' => 'numeric_range_search_dom',
            ),
        'total_amount_usdollar' =>
            array(
                'name' => 'total_amount_usdollar',
                'vname' => 'LBL_GRAND_TOTAL_USDOLLAR',
                'type' => 'currency',
                'group' => 'total_amount',
                'disable_num_format' => true,
                'duplicate_merge' => '0',
                'audited' => true,
                'comment' => '',
                'studio' => array(
                    'editview' => false,
                    'detailview' => false,
                    'quickcreate' => false,
                ),
                'len' => '26,6',
            ),
        'currency_id' =>
            array(
                'required' => false,
                'name' => 'currency_id',
                'vname' => 'LBL_CURRENCY',
                'type' => 'id',
                'massupdate' => 0,
                'comments' => '',
                'help' => '',
                'importable' => 'true',
                'duplicate_merge' => 'disabled',
                'duplicate_merge_dom_value' => 0,
                'audited' => false,
                'reportable' => false,
                'len' => 36,
                'size' => '20',
                'studio' => 'visible',
                'function' =>
                    array(
                        'name' => 'getCurrencyDropDown',
                        'returns' => 'html',
                        'onListView' => true,
                    ),
            ),
        'quote_number' =>
            array(
                'required' => false,
                'name' => 'quote_number',
                'vname' => 'LBL_QUOTE_NUMBER',
                'type' => 'int',
                'massupdate' => 0,
                'comments' => '',
                'help' => '',
                'importable' => 'true',
                'duplicate_merge' => 'disabled',
                'duplicate_merge_dom_value' => '0',
                'audited' => 1,
                'reportable' => true,
                'len' => '11',
                'disable_num_format' => '',
            ),
        'quote_date' =>
            array(
                'required' => false,
                'name' => 'quote_date',
                'vname' => 'LBL_QUOTE_DATE',
                'type' => 'date',
                'massupdate' => 0,
                'comments' => '',
                'help' => '',
                'importable' => 'true',
                'duplicate_merge' => 'disabled',
                'duplicate_merge_dom_value' => '0',
                'audited' => 0,
                'reportable' => true,
                'display_default' => 'now',
                'enable_range_search' => true,
                'options' => 'date_range_search_dom',
            ),
        'invoice_date' =>
            array(
                'required' => false,
                'name' => 'invoice_date',
                'vname' => 'LBL_INVOICE_DATE',
                'type' => 'date',
                'massupdate' => 0,
                'comments' => '',
                'help' => '',
                'importable' => 'true',
                'duplicate_merge' => 'disabled',
                'duplicate_merge_dom_value' => '0',
                'audited' => 0,
                'reportable' => true,
                'display_default' => 'now',
                'enable_range_search' => true,
                'options' => 'date_range_search_dom',
            ),
        'due_date' =>
            array(
                'required' => false,
                'name' => 'due_date',
                'vname' => 'LBL_DUE_DATE',
                'type' => 'date',
                'massupdate' => 0,
                'comments' => '',
                'help' => '',
                'importable' => 'true',
                'duplicate_merge' => 'disabled',
                'duplicate_merge_dom_value' => '0',
                'audited' => 0,
                'reportable' => true,
                'enable_range_search' => true,
                'options' => 'date_range_search_dom',
            ),
        'status' =>
            array(
                'required' => false,
                'name' => 'status',
                'vname' => 'LBL_STATUS',
                'type' => 'enum',
                'massupdate' => 0,
                'comments' => '',
                'help' => '',
                'importable' => 'true',
                'duplicate_merge' => 'disabled',
                'duplicate_merge_dom_value' => '0',
                'audited' => 0,
                'reportable' => true,
                'len' => 100,
                'options' => 'invoice_status_dom',
                'studio' => 'visible',
            ),
        'template_ddown_c' =>
            array(
                'required' => '0',
                'name' => 'template_ddown_c',
                'vname' => 'LBL_TEMPLATE_DDOWN_C',
                'type' => 'multienum',
                'massupdate' => 0,
                'comments' => '',
                'help' => '',
                'importable' => 'true',
                'duplicate_merge' => 'disabled',
                'duplicate_merge_dom_value' => 0,
                'audited' => 0,
                'reportable' => 0,
                'options' => 'template_ddown_c_list',
                'studio' => 'visible',
                'isMultiSelect' => true,
            ),
        'subtotal_tax_amount' =>
            array(
                'required' => false,
                'name' => 'subtotal_tax_amount',
                'vname' => 'LBL_SUBTOTAL_TAX_AMOUNT',
                'type' => 'currency',
                'massupdate' => 0,
                'comments' => '',
                'help' => '',
                'importable' => 'true',
                'duplicate_merge' => 'disabled',
                'duplicate_merge_dom_value' => '0',
                'audited' => 1,
                'reportable' => true,
                'len' => '26,6',
            ),
        'subtotal_tax_amount_usdollar' =>
            array(
                'name' => 'subtotal_tax_amount_usdollar',
                'vname' => 'LBL_GRAND_TOTAL_USDOLLAR',
                'type' => 'currency',
                'group' => 'subtotal_tax_amount',
                'disable_num_format' => true,
                'duplicate_merge' => '0',
                'audited' => true,
                'comment' => '',
                'studio' => array(
                    'editview' => false,
                    'detailview' => false,
                    'quickcreate' => false,
                ),
                'len' => '26,6',
            ),

        'accounts' =>
            array(
                'name' => 'accounts',
                'vname' => 'LBL_ACCOUNTS',
                'type' => 'link',
                'relationship' => 'account_aos_invoices',
                'module' => 'Accounts',
                'bean_name' => 'Account',
                'source' => 'non-db',
            ),
        'contacts' =>
            array(
                'name' => 'contacts',
                'vname' => 'LBL_CONTACTS',
                'type' => 'link',
                'relationship' => 'contact_aos_invoices',
                'module' => 'Contacts',
                'bean_name' => 'Contact',
                'source' => 'non-db',
            ),
        'aos_quotes_aos_invoices' =>
            array(
                'name' => 'aos_quotes_aos_invoices',
                'vname' => 'LBL_AOS_QUOTES_AOS_INVOICES',
                'type' => 'link',
                'relationship' => 'aos_quotes_aos_invoices',
                'source' => 'non-db',
                'module' => 'AOS_Quotes',
            ),
        'aos_products_quotes' =>
            array(
                'name' => 'aos_products_quotes',
                'vname' => 'LBL_AOS_PRODUCT_QUOTES',
                'type' => 'link',
                'relationship' => 'aos_invoices_aos_product_quotes',
                'module' => 'AOS_Products_Quotes',
                'bean_name' => 'AOS_Products_Quotes',
                'source' => 'non-db',
            ),
        'aos_line_item_groups' =>
            array(
                'name' => 'aos_line_item_groups',
                'vname' => 'LBL_AOS_LINE_ITEM_GROUPS',
                'type' => 'link',
                'relationship' => 'aos_invoices_aos_line_item_groups',
                'module' => 'AOS_Line_Item_Groups',
                'bean_name' => 'AOS_Line_Item_Groups',
                'source' => 'non-db',
            ),

    ),

    'relationships' => array(
        'aos_invoices_aos_product_quotes' =>
            array(
                'lhs_module' => 'AOS_Invoices',
                'lhs_table' => 'aos_invoices',
                'lhs_key' => 'id',
                'rhs_module' => 'AOS_Products_Quotes',
                'rhs_table' => 'aos_products_quotes',
                'rhs_key' => 'parent_id',
                'relationship_type' => 'one-to-many',
            ),
        'aos_invoices_aos_line_item_groups' =>
            array(
                'lhs_module' => 'AOS_Invoices',
                'lhs_table' => 'aos_invoices',
                'lhs_key' => 'id',
                'rhs_module' => 'AOS_Line_Item_Groups',
                'rhs_table' => 'aos_line_item_groups',
                'rhs_key' => 'parent_id',
                'relationship_type' => 'one-to-many',
            ),
    ),
    'optimistic_locking' => true,
);
require_once('include/SugarObjects/VardefManager.php');
VardefManager::createVardef('AOS_Invoices', 'AOS_Invoices', array('basic', 'assignable', 'security_groups'));
