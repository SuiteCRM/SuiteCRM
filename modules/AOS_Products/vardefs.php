<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2024 SalesAgility Ltd.
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

$dictionary['AOS_Products'] = array(
    'table' => 'aos_products',
    'audited' => true,
    'duplicate_merge' => true,
    'fields' => array(
        'aos_products_purchases' => array(
            'name' => 'aos_products_purchases',
            'type' => 'link',
            'relationship' => 'aos_products_purchases',
            'source' => 'non-db',
            'module' => 'AOS_Quotes',
            'bean_name' => 'AOS_Quotes',
            'vname' => 'LBL_PRODUCTS_PURCHASES',
        ),
        'maincode' =>
            array(
                'required' => '0',
                'name' => 'maincode',
                'vname' => 'LBL_MAINCODE',
                'type' => 'enum',
                'massupdate' => 0,
                'default' => 'XXXX',
                'comments' => '',
                'help' => '',
                'importable' => 'true',
                'duplicate_merge' => 'disabled',
                'duplicate_merge_dom_value' => '0',
                'audited' => 1,
                'reportable' => true,
                'len' => 100,
                'options' => 'product_code_dom',
                'studio' => 'visible',
            ),
        'part_number' =>
            array(
                'required' => false,
                'name' => 'part_number',
                'vname' => 'LBL_PART_NUMBER',
                'type' => 'varchar',
                'massupdate' => 0,
                'comments' => '',
                'help' => '',
                'importable' => 'true',
                'duplicate_merge' => 'disabled',
                'duplicate_merge_dom_value' => '0',
                'audited' => 1,
                'reportable' => true,
                'len' => '25',
            ),
        'category' =>
            array(
                'required' => false,
                'name' => 'category',
                'vname' => 'LBL_CATEGORY',
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
                'options' => 'product_category_dom',
                'studio' => 'visible',
            ),
        'type' =>
            array(
                'required' => false,
                'name' => 'type',
                'vname' => 'LBL_TYPE',
                'type' => 'enum',
                'massupdate' => 0,
                'default' => 'Good',
                'comments' => '',
                'help' => '',
                'importable' => 'true',
                'duplicate_merge' => 'disabled',
                'duplicate_merge_dom_value' => '0',
                'audited' => 1,
                'reportable' => true,
                'len' => 100,
                'options' => 'product_type_dom',
                'studio' => 'visible',
            ),
        'cost' =>
            array(
                'required' => '0',
                'name' => 'cost',
                'vname' => 'LBL_COST',
                'type' => 'currency',
                'len' => '26,6',
                'massupdate' => 0,
                'comments' => '',
                'help' => '',
                'importable' => 'true',
                'duplicate_merge' => 'disabled',
                'duplicate_merge_dom_value' => '0',
                'audited' => 1,
                'reportable' => true,
                'enable_range_search' => true,
                'options' => 'numeric_range_search_dom',
            ),
        'cost_usdollar' =>
            array(
                'required' => '0',
                'name' => 'cost_usdollar',
                'vname' => 'LBL_COST_USDOLLAR',
                'type' => 'currency',
                'group' => 'cost',
                'len' => '26,6',
                'massupdate' => 0,
                'comments' => '',
                'help' => '',
                'importable' => 'true',
                'audited' => true,
                'comment' => '',
                'studio' => array(
                    'editview' => false,
                    'detailview' => false,
                    'quickcreate' => false,
                ),
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
                'audited' => 0,
                'reportable' => 0,
                'len' => 36,
                'studio' => 'visible',
                'function' =>
                    array(
                        'name' => 'getCurrencyDropDown',
                        'returns' => 'html',
                        'onListView' => true,
                    ),
            ),
        'price' =>
            array(
                'required' => '1',
                'name' => 'price',
                'vname' => 'LBL_PRICE',
                'type' => 'currency',
                'len' => '26,6',
                'massupdate' => 0,
                'comments' => '',
                'help' => '',
                'importable' => 'true',
                'duplicate_merge' => 'disabled',
                'duplicate_merge_dom_value' => '0',
                'audited' => 1,
                'reportable' => true,
                'enable_range_search' => true,
                'options' => 'numeric_range_search_dom',
            ),
        'price_usdollar' =>
            array(
                'name' => 'price_usdollar',
                'vname' => 'LBL_PRICE_USDOLLAR',
                'type' => 'currency',
                'disable_num_format' => true,
                'group' => 'price',
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
        'url' =>
            array(
                'required' => false,
                'name' => 'url',
                'vname' => 'LBL_URL',
                'type' => 'url',
                'massupdate' => 0,
                'comments' => '',
                'help' => '',
                'importable' => 'true',
                'duplicate_merge' => 'disabled',
                'duplicate_merge_dom_value' => '0',
                'audited' => 1,
                'reportable' => true,
                'len' => '255',
            ),
        'contact_id' =>
            array(
                'required' => false,
                'name' => 'contact_id',
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
        'contact' =>
            array(
                'required' => false,
                'source' => 'non-db',
                'name' => 'contact',
                'vname' => 'LBL_CONTACT',
                'type' => 'relate',
                'massupdate' => 0,
                'comments' => '',
                'help' => '',
                'importable' => 'true',
                'duplicate_merge' => 'disabled',
                'duplicate_merge_dom_value' => '0',
                'audited' => 1,
                'reportable' => true,
                'len' => '255',
                'id_name' => 'contact_id',
                'ext2' => 'Contacts',
                'module' => 'Contacts',
                'quicksearch' => 'enabled',
                'studio' => 'visible',
            ),
        'product_image' =>
            array(
                'name' => 'product_image',
                'vname' => 'LBL_PRODUCT_IMAGE',
                'type' => 'varchar',
                'len' => '255',
                'reportable' => true,
                'inline_edit' => false,
                'comment' => 'File name associated with the note (attachment)'
            ),
        'file_url' =>
            array(
                'name' => 'file_url',
                'vname' => 'LBL_FILE_URL',
                'type' => 'function',
                'function_require' => 'include/upload_file.php',
                'function_class' => 'UploadFile',
                'function_name' => 'get_url',
                'function_params' => array('filename', 'id'),
                'source' => 'function',
                'reportable' => false,
                'comment' => 'Path to file (can be URL)'
            ),
        "aos_product_category" => array(
            'name' => 'aos_product_category',
            'type' => 'link',
            'relationship' => 'product_categories',
            'source' => 'non-db',
            'link_type' => 'one',
            'module' => 'AOS_Product_Categories',
            'bean_name' => 'AOS_Product_Categories',
            'vname' => 'LBL_AOS_PRODUCT_CATEGORIES',
        ),
        'aos_product_category_name' => array(
            'required' => false,
            'source' => 'non-db',
            'name' => 'aos_product_category_name',
            'vname' => 'LBL_AOS_PRODUCT_CATEGORYS_NAME',
            'type' => 'relate',
            'massupdate' => 0,
            'comments' => '',
            'help' => '',
            'importable' => 'true',
            'duplicate_merge' => 'disabled',
            'duplicate_merge_dom_value' => '0',
            'audited' => 1,
            'reportable' => true,
            'len' => '255',
            'id_name' => 'aos_product_category_id',
            'ext2' => 'AOS_Product_Categories',
            'module' => 'AOS_Product_Categories',
            'quicksearch' => 'enabled',
            'studio' => 'visible',
        ),
        "aos_product_category_id" => array(
            'name' => 'aos_product_category_id',
            'type' => 'id',
            'reportable' => false,
            'vname' => 'LBL_AOS_PRODUCT_CATEGORY',
        ),
    ),
    'relationships' => array(
        "product_categories" => array(
            'lhs_module' => 'AOS_Product_Categories',
            'lhs_table' => 'aos_product_categories',
            'lhs_key' => 'id',
            'rhs_module' => 'AOS_Products',
            'rhs_table' => 'aos_products',
            'rhs_key' => 'aos_product_category_id',
            'relationship_type' => 'one-to-many',
        ),
    ),
    'optimistic_locking' => true,
);
require_once('include/SugarObjects/VardefManager.php');
VardefManager::createVardef('AOS_Products', 'AOS_Products', array('basic', 'assignable', 'security_groups'));
