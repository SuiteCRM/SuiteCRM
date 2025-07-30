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
$popupMeta = array(
    'moduleMain' => 'stic_Resources',
    'varName' => 'stic_Resources',
    'orderBy' => 'stic_resources.name',
    'whereClauses' => array(
        'name' => 'stic_resources.name',
        'code' => 'stic_resources.code',
        'type' => 'stic_resources.type',
        'gender' => 'stic_resources.gender',
        'place_type' => 'stic_resources.place_type',
        'user_type' => 'stic_resources.user_type',
        'hourly_rate' => 'stic_resources.hourly_rate',
        'daily_rate' => 'stic_resources.daily_rate',
        'owner_contact' => 'stic_resources.owner_contact',
        'owner_account' => 'stic_resources.owner_account',
        'description' => 'stic_resources.description',
        'created_by_name' => 'stic_resources.created_by_name',
        'date_entered' => 'stic_resources.date_entered',
        'modified_by_name' => 'stic_resources.modified_by_name',
        'date_modified' => 'stic_resources.date_modified',
        'assigned_user_name' => 'stic_resources.assigned_user_name',
    ),
    'searchInputs' => array(
        1 => 'name',
        3 => 'status',
        4 => 'code',
        5 => 'type',
        6 => 'hourly_rate',
        7 => 'daily_rate',
        8 => 'owner_contact',
        9 => 'owner_account',
        10 => 'gender',
        11 => 'place_type',
        12 => 'user_type',
        13 => 'description',
        14 => 'created_by_name',
        15 => 'date_entered',
        16 => 'modified_by_name',
        17 => 'date_modified',
        18 => 'assigned_user_name',
    ),
    'searchdefs' => array(
        'name' => array(
            'type' => 'name',
            'link' => true,
            'label' => 'LBL_NAME',
            'width' => '10%',
            'name' => 'name',
        ),
        'code' => array(
            'type' => 'varchar',
            'label' => 'LBL_CODE',
            'width' => '10%',
            'name' => 'code',
        ),
        'STATUS' => array(
            'label' => 'LBL_STATUS',
            'width' => '10%',
            'name' => 'status',
        ),
        'type' => array(
            'type' => 'enum',
            'studio' => 'visible',
            'label' => 'LBL_TYPE',
            'width' => '10%',
            'name' => 'type',
        ),
        'place_type' => array(
            'type' => 'enum',
            'studio' => 'visible',
            'label' => 'LBL_PLACE_TYPE',
            'width' => '10%',
            'name' => 'place_type',
        ),
        'user_type' => array(
            'type' => 'enum',
            'studio' => 'visible',
            'label' => 'LBL_USER_TYPE',
            'width' => '10%',
            'name' => 'user_type',
        ),
        'gender' => array(
            'type' => 'enum',
            'studio' => 'visible',
            'label' => 'LBL_GENDER',
            'width' => '10%',
            'name' => 'gender',
        ),
        'hourly_rate' => array(
            'type' => 'decimal',
            'label' => 'LBL_HOURLY_RATE',
            'width' => '10%',
            'name' => 'hourly_rate',
        ),
        'daily_rate' => array(
            'type' => 'decimal',
            'label' => 'LBL_DAILY_RATE',
            'width' => '10%',
            'name' => 'daily_rate',
        ),
        'owner_contact' => array(
            'type' => 'relate',
            'studio' => 'visible',
            'label' => 'LBL_OWNER_CONTACT',
            'id' => 'CONTACT_ID_C',
            'link' => true,
            'width' => '10%',
            'name' => 'owner_contact',
        ),
        'owner_account' => array(
            'type' => 'relate',
            'studio' => 'visible',
            'label' => 'LBL_OWNER_ACCOUNT',
            'id' => 'ACCOUNT_ID_C',
            'link' => true,
            'width' => '10%',
            'name' => 'owner_account',
        ),
        'description' => array(
            'type' => 'text',
            'label' => 'LBL_DESCRIPTION',
            'sortable' => false,
            'width' => '10%',
            'name' => 'description',
        ),
        'created_by_name' => array(
            'type' => 'relate',
            'link' => true,
            'label' => 'LBL_CREATED',
            'id' => 'CREATED_BY',
            'width' => '10%',
            'name' => 'created_by_name',
        ),
        'date_entered' => array(
            'type' => 'datetime',
            'label' => 'LBL_DATE_ENTERED',
            'width' => '10%',
            'name' => 'date_entered',
        ),
        'modified_by_name' => array(
            'type' => 'relate',
            'link' => true,
            'label' => 'LBL_MODIFIED_NAME',
            'id' => 'MODIFIED_USER_ID',
            'width' => '10%',
            'name' => 'modified_by_name',
        ),
        'date_modified' => array(
            'type' => 'datetime',
            'label' => 'LBL_DATE_MODIFIED',
            'width' => '10%',
            'name' => 'date_modified',
        ),
        'assigned_user_name' => array(
            'link' => true,
            'type' => 'relate',
            'label' => 'LBL_ASSIGNED_TO_NAME',
            'id' => 'ASSIGNED_USER_ID',
            'width' => '10%',
            'name' => 'assigned_user_name',
        ),
    ),
    'listviewdefs' => array(
        'NAME' => array(
            'type' => 'name',
            'link' => true,
            'label' => 'LBL_NAME',
            'width' => '10%',
            'default' => true,
        ),
        'CODE' => array(
            'type' => 'varchar',
            'label' => 'LBL_CODE',
            'width' => '10%',
            'default' => true,
        ),
        'TYPE' => array(
            'type' => 'enum',
            'studio' => 'visible',
            'label' => 'LBL_TYPE',
            'width' => '10%',
            'default' => true,
        ),
        'HOURLY_RATE' => array(
            'type' => 'decimal',
            'label' => 'LBL_HOURLY_RATE',
            'width' => '10%',
            'default' => true,
        ),
        'DAILY_RATE' => array(
            'type' => 'decimal',
            'label' => 'LBL_DAILY_RATE',
            'width' => '10%',
            'default' => true,
        ),
    ),
);
