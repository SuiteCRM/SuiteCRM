<?php
/**
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
 *
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 * 
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo, "Supercharged by SuiteCRM" logo and “Nonprofitized by SinergiaCRM” logo. 
 * If the display of the logos is not reasonably feasible for technical reasons, 
 * the Appropriate Legal Notices must display the words "Powered by SugarCRM", 
 * "Supercharged by SuiteCRM" and “Nonprofitized by SinergiaCRM”. 
 */


$module_name = 'AOS_Products';

// STIC-Custom - MHP - 20240201 - Override the core metadata files with the custom metadata files 
// https://github.com/SinergiaTIC/SinergiaCRM/pull/105
// $viewdefs [$module_name] =
// array(
//   'EditView' =>
//   array(
//     'templateMeta' =>
//     array(
//       'maxColumns' => '2',
//       'widths' =>
//       array(
//         0 =>
//         array(
//           'label' => '10',
//           'field' => '30',
//         ),
//         1 =>
//         array(
//           'label' => '10',
//           'field' => '30',
//         ),
//       ),
//       'form' =>
//       array(
//         'enctype' => 'multipart/form-data',
//         'headerTpl' => 'modules/AOS_Products/tpls/EditViewHeader.tpl',
//       ),
//       'includes' =>
//       array(
//         0 =>
//         array(
//           'file' => 'modules/AOS_Products/js/products.js',
//         ),
//       ),
//       'useTabs' => false,
//       'tabDefs' =>
//       array(
//         'DEFAULT' =>
//         array(
//           'newTab' => false,
//           'panelDefault' => 'expanded',
//         ),
//       ),
//     ),
//     'panels' =>
//     array(
//       'default' =>
//       array(
//         0 =>
//         array(
//           0 =>
//           array(
//             'name' => 'name',
//             'label' => 'LBL_NAME',
//           ),
//           1 =>
//           array(
//             'name' => 'part_number',
//             'label' => 'LBL_PART_NUMBER',
//           ),
//         ),
//         1 =>
//         array(
//           0 =>
//           array(
//             'name' => 'aos_product_category_name',
//             'label' => 'LBL_AOS_PRODUCT_CATEGORYS_NAME',
//           ),
//           1 =>
//           array(
//             'name' => 'type',
//             'label' => 'LBL_TYPE',
//           ),
//         ),
//         2 =>
//         array(
//           0 =>
//           array(
//             'name' => 'currency_id',
//             'studio' => 'visible',
//             'label' => 'LBL_CURRENCY',
//           ),
//         ),
//         3 =>
//         array(
//           0 =>
//           array(
//             'name' => 'cost',
//             'label' => 'LBL_COST',
//           ),
//           1 =>
//           array(
//             'name' => 'price',
//             'label' => 'LBL_PRICE',
//           ),
//         ),
//         4 =>
//         array(
//           0 =>
//           array(
//             'name' => 'contact',
//             'label' => 'LBL_CONTACT',
//           ),
//           1 =>
//           array(
//             'name' => 'url',
//             'label' => 'LBL_URL',
//           ),
//         ),
//         5 =>
//         array(
//           0 =>
//           array(
//             'name' => 'description',
//             'label' => 'LBL_DESCRIPTION',
//           ),
//         ),
//         6 =>
//         array(
//           0 =>
//           array(
//             'name' => 'product_image',
//             'customCode' => '{$PRODUCT_IMAGE}',
//           ),
//         ),
//       ),
//     ),
//   ),
// );

$viewdefs[$module_name] =
array(
    'EditView' => array(
        'templateMeta' => array(
            'maxColumns' => '2',
            'widths' => array(
                0 => array(
                    'label' => '10',
                    'field' => '30',
                ),
                1 => array(
                    'label' => '10',
                    'field' => '30',
                ),
            ),
            'form' => array(
                'enctype' => 'multipart/form-data',
                'headerTpl' => 'modules/AOS_Products/tpls/EditViewHeader.tpl',
            ),
            'includes' => array(
                0 => array(
                    'file' => 'modules/AOS_Products/js/products.js',
                ),
            ),
            'useTabs' => true,
            'tabDefs' => array(
                'LBL_DEFAULT_PANEL' => array(
                    'newTab' => true,
                    'panelDefault' => 'expanded',
                ),
            ),
        ),
        'panels' => array(
            'LBL_DEFAULT_PANEL' => array(
                0 => array(
                    0 => array(
                        'name' => 'product_image',
                        // STIC Custom 20250722 JBL - Treat product image as image
                        // https://github.com/SinergiaTIC/SinergiaCRM/pull/740
                        // 'customCode' => '{$PRODUCT_IMAGE}',
                        'label' => 'LBL_PRODUCT_IMAGE',
                        // END STIC Custom
                    ),
                    1 => array(
                        'name' => 'assigned_user_name',
                        'label' => 'LBL_ASSIGNED_TO_NAME',
                    ),
                ),
                1 => array(
                    0 => array(
                        'name' => 'name',
                        'label' => 'LBL_NAME',
                    ),
                    1 => array(
                        'name' => 'type',
                        'label' => 'LBL_TYPE',
                    ),
                ),
                2 => array(
                    0 => array(
                        'name' => 'aos_product_category_name',
                        'label' => 'LBL_AOS_PRODUCT_CATEGORYS_NAME',
                    ),
                    1 => array(
                        'name' => 'part_number',
                        'label' => 'LBL_PART_NUMBER',
                    ),
                ),
                3 => array(
                    0 => array(
                        'name' => 'url',
                        'label' => 'LBL_URL',
                    ),
                    1 => array(
                        'name' => 'contact',
                        'label' => 'LBL_CONTACT',
                    ),
                ),
                4 => array(
                    0 => array(
                        'name' => 'price',
                        'label' => 'LBL_PRICE',
                    ),
                    1 => array(
                        'name' => 'cost',
                        'label' => 'LBL_COST',
                    ),
                ),
                5 => array(
                    0 => array(
                        'name' => 'description',
                        'label' => 'LBL_DESCRIPTION',
                    ),
                ),
            ),
        ),
    ),
);
// STIC-Custom