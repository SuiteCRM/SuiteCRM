<?php
/**
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation.
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */
$module_name = 'stic_Resources';
$searchdefs[$module_name] =
array(
    'layout' => array(
        'basic_search' => array(
            'name' => array(
                'name' => 'name',
                'default' => true,
                'width' => '10%',
            ),
            'code' => array(
                'type' => 'varchar',
                'label' => 'LBL_CODE',
                'width' => '10%',
                'default' => true,
                'name' => 'code',
            ),
            'status' => array(
                'studio' => 'visible',
                'label' => 'LBL_STATUS',
                'width' => '10%',
                'default' => true,
                'name' => 'status',
            ),
            'type' => array(
                'type' => 'enum',
                'studio' => 'visible',
                'label' => 'LBL_TYPE',
                'width' => '10%',
                'default' => true,
                'name' => 'type',
            ),
            'color' => array(
                'type' => 'ColorPicker',
                'default' => true,
                'label' => 'LBL_COLOR',
                'width' => '10%',
                'name' => 'color',
            ),
            'place_type' => array(
                'type' => 'enum',
                'studio' => 'visible',
                'label' => 'LBL_PLACE_TYPE',
                'width' => '10%',
                'default' => true,
                'name' => 'place_type',
            ),
            'user_type' => array(
                'type' => 'enum',
                'studio' => 'visible',
                'label' => 'LBL_USER_TYPE',
                'width' => '10%',
                'default' => true,
                'name' => 'user_type',
            ),
            'gender' => array(
                'type' => 'enum',
                'studio' => 'visible',
                'label' => 'LBL_GENDER',
                'width' => '10%',
                'default' => true,
                'name' => 'gender',
            ),
            'hourly_rate' => array(
                'type' => 'decimal',
                'label' => 'LBL_HOURLY_RATE',
                'width' => '10%',
                'default' => true,
                'name' => 'hourly_rate',
            ),
            'daily_rate' => array(
                'type' => 'decimal',
                'label' => 'LBL_DAILY_RATE',
                'width' => '10%',
                'default' => true,
                'name' => 'daily_rate',
            ),
            'stic_resources_stic_centers_name' => 
            array (
              'type' => 'relate',
              'link' => true,
              'label' => 'LBL_STIC_RESOURCES_STIC_CENTERS_FROM_STIC_CENTERS_TITLE',
              'id' => 'STIC_RESOURCES_STIC_CENTERSSTIC_CENTERS_IDA',
              'width' => '10%',
              'default' => true,
              'name' => 'stic_resources_stic_centers_name',
            ),
            'assigned_user_id' => array(
                'name' => 'assigned_user_id',
                'label' => 'LBL_ASSIGNED_TO',
                'type' => 'enum',
                'function' => array(
                    'name' => 'get_user_array',
                    'params' => array(
                        0 => false,
                    ),
                ),
                'width' => '10%',
                'default' => true,
            ),
            'current_user_only' => array(
                'name' => 'current_user_only',
                'label' => 'LBL_CURRENT_USER_FILTER',
                'type' => 'bool',
                'default' => true,
                'width' => '10%',
            ),
            'favorites_only' => array(
                'name' => 'favorites_only',
                'label' => 'LBL_FAVORITES_FILTER',
                'type' => 'bool',
                'default' => true,
                'width' => '10%',
            ),
        ),
        'advanced_search' => array(
            'name' => array(
                'name' => 'name',
                'default' => true,
                'width' => '10%',
            ),
            'code' => array(
                'type' => 'varchar',
                'label' => 'LBL_CODE',
                'width' => '10%',
                'default' => true,
                'name' => 'code',
            ),
            'status' => array(
                'type' => 'enum',
                'studio' => 'visible',
                'label' => 'LBL_STATUS',
                'width' => '10%',
                'default' => true,
                'name' => 'status',
            ),
            'type' => array(
                'type' => 'enum',
                'studio' => 'visible',
                'label' => 'LBL_TYPE',
                'width' => '10%',
                'default' => true,
                'name' => 'type',
            ),
            'color' => array(
                'type' => 'ColorPicker',
                'default' => true,
                'label' => 'LBL_COLOR',
                'width' => '10%',
                'name' => 'color',
            ),
            'place_type' => array(
                'type' => 'enum',
                'studio' => 'visible',
                'label' => 'LBL_PLACE_TYPE',
                'width' => '10%',
                'default' => true,
                'name' => 'place_type',
            ),
            'user_type' => array(
                'type' => 'enum',
                'studio' => 'visible',
                'label' => 'LBL_USER_TYPE',
                'width' => '10%',
                'default' => true,
                'name' => 'user_type',
            ),
            'gender' => array(
                'type' => 'enum',
                'studio' => 'visible',
                'label' => 'LBL_GENDER',
                'width' => '10%',
                'default' => true,
                'name' => 'gender',
            ),
            'amount_day_occupied' => array(
                'type' => 'decimal',
                'studio' => 'visible',
                'label' => 'LBL_AMOUNT_DAY_OCCUPIED',
                'width' => '10%',
                'default' => true,
                'name' => 'amount_day_occupied',
            ),
            'amount_day_unoccupied' => array(
                'type' => 'decimal',
                'studio' => 'visible',
                'label' => 'LBL_AMOUNT_DAY_UNOCCUPIED',
                'width' => '10%',
                'default' => true,
                'name' => 'amount_day_unoccupied',
            ),
            'amount_copayment' => array(
                'type' => 'enum',
                'studio' => 'visible',
                'label' => 'LBL_AMOUNT_COPAYMENT',
                'width' => '10%',
                'default' => true,
                'name' => 'amount_copayment',
            ),
            'hourly_rate' => array(
                'type' => 'decimal',
                'label' => 'LBL_HOURLY_RATE',
                'width' => '10%',
                'default' => true,
                'name' => 'hourly_rate',
            ),
            'daily_rate' => array(
                'type' => 'decimal',
                'label' => 'LBL_DAILY_RATE',
                'width' => '10%',
                'default' => true,
                'name' => 'daily_rate',
            ),
            'owner_contact' => array(
                'type' => 'relate',
                'studio' => 'visible',
                'label' => 'LBL_OWNER_CONTACT',
                'id' => 'CONTACT_ID_C',
                'link' => true,
                'width' => '10%',
                'default' => true,
                'name' => 'owner_contact',
            ),
            'owner_account' => array(
                'type' => 'relate',
                'studio' => 'visible',
                'label' => 'LBL_OWNER_ACCOUNT',
                'id' => 'ACCOUNT_ID_C',
                'link' => true,
                'width' => '10%',
                'default' => true,
                'name' => 'owner_account',
            ),
            'stic_resources_stic_centers_name' => 
            array (
              'type' => 'relate',
              'link' => true,
              'label' => 'LBL_STIC_RESOURCES_STIC_CENTERS_FROM_STIC_CENTERS_TITLE',
              'id' => 'STIC_RESOURCES_STIC_CENTERSSTIC_CENTERS_IDA',
              'width' => '10%',
              'default' => true,
              'name' => 'stic_resources_stic_centers_name',
            ),
            'description' => array(
                'type' => 'text',
                'label' => 'LBL_DESCRIPTION',
                'sortable' => false,
                'width' => '10%',
                'default' => true,
                'name' => 'description',
            ),
            'created_by' => array(
                'type' => 'assigned_user_name',
                'label' => 'LBL_CREATED',
                'width' => '10%',
                'default' => true,
                'name' => 'created_by',
            ),
            'date_entered' => array(
                'type' => 'datetime',
                'label' => 'LBL_DATE_ENTERED',
                'width' => '10%',
                'default' => true,
                'name' => 'date_entered',
            ),
            'modified_user_id' => array(
                'type' => 'assigned_user_name',
                'label' => 'LBL_MODIFIED',
                'width' => '10%',
                'default' => true,
                'name' => 'modified_user_id',
            ),
            'date_modified' => array(
                'type' => 'datetime',
                'label' => 'LBL_DATE_MODIFIED',
                'width' => '10%',
                'default' => true,
                'name' => 'date_modified',
            ),
            'assigned_user_id' => array(
                'name' => 'assigned_user_id',
                'label' => 'LBL_ASSIGNED_TO',
                'type' => 'enum',
                'function' => array(
                    'name' => 'get_user_array',
                    'params' => array(
                        0 => false,
                    ),
                ),
                'default' => true,
                'width' => '10%',
            ),
            'current_user_only' => array(
                'label' => 'LBL_CURRENT_USER_FILTER',
                'type' => 'bool',
                'default' => true,
                'width' => '10%',
                'name' => 'current_user_only',
            ),
            'favorites_only' => array(
                'name' => 'favorites_only',
                'label' => 'LBL_FAVORITES_FILTER',
                'type' => 'bool',
                'default' => true,
                'width' => '10%',
            ),
        ),
    ),
    'templateMeta' => array(
        'maxColumns' => '3',
        'maxColumnsBasic' => '4',
        'widths' => array(
            'label' => '10',
            'field' => '30',
        ),
    ),
);
