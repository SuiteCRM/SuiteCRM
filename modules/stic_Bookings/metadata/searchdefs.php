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
$module_name = 'stic_Bookings';
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
                'name' => 'code',
                'default' => true,
                'width' => '10%',
            ),
            'place_booking' => array(
                'name' => 'place_booking',
                'type' => 'bool',
                'default' => true,
                'width' => '10%',
            ),
            'status' => array(
                'name' => 'status',
                'default' => true,
                'width' => '10%',
            ),
            'start_date' => array(
                'type' => 'datetimecombo',
                'label' => 'LBL_START_DATE',
                'width' => '10%',
                'default' => true,
                'name' => 'start_date',
            ),
            'end_date' => array(
                'type' => 'datetimecombo',
                'label' => 'LBL_END_DATE',
                'width' => '10%',
                'default' => true,
                'name' => 'end_date',
            ),
            'stic_bookings_accounts_name' => array(
                'type' => 'relate',
                'link' => true,
                'label' => 'LBL_STIC_BOOKINGS_ACCOUNTS_FROM_ACCOUNTS_TITLE',
                'id' => 'STIC_BOOKINGS_ACCOUNTSACCOUNTS_IDA',
                'width' => '10%',
                'default' => true,
                'name' => 'stic_bookings_accounts_name',
            ),
            'stic_bookings_contacts_name' => array(
                'type' => 'relate',
                'link' => true,
                'label' => 'LBL_STIC_BOOKINGS_CONTACTS_FROM_CONTACTS_TITLE',
                'id' => 'STIC_BOOKINGS_CONTACTSCONTACTS_IDA',
                'width' => '10%',
                'default' => true,
                'name' => 'stic_bookings_contacts_name',
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
                'name' => 'code',
                'default' => true,
                'width' => '10%',
            ),
            'place_booking' => array(
                'name' => 'place_booking',
                'type' => 'bool',
                'default' => true,
                'width' => '10%',
            ),
            'status' => array(
                'name' => 'status',
                'default' => true,
                'width' => '10%',
            ),
            'start_date' => array(
                'type' => 'datetimecombo',
                'label' => 'LBL_START_DATE',
                'width' => '10%',
                'default' => true,
                'name' => 'start_date',
            ),
            'end_date' => array(
                'type' => 'datetimecombo',
                'label' => 'LBL_END_DATE',
                'width' => '10%',
                'default' => true,
                'name' => 'end_date',
            ),
            'stic_bookings_accounts_name' => array(
                'type' => 'relate',
                'link' => true,
                'label' => 'LBL_STIC_BOOKINGS_ACCOUNTS_FROM_ACCOUNTS_TITLE',
                'width' => '10%',
                'default' => true,
                'id' => 'STIC_BOOKINGS_ACCOUNTSACCOUNTS_IDA',
                'name' => 'stic_bookings_accounts_name',
            ),
            'stic_bookings_contacts_name' => array(
                'type' => 'relate',
                'link' => true,
                'label' => 'LBL_STIC_BOOKINGS_CONTACTS_FROM_CONTACTS_TITLE',
                'width' => '10%',
                'default' => true,
                'id' => 'STIC_BOOKINGS_CONTACTSCONTACTS_IDA',
                'name' => 'stic_bookings_contacts_name',
            ),
            'parent_name' => array(
                'type' => 'parent',
                'studio' => 'visible',
                'label' => 'LBL_FLEX_RELATE',
                'link' => true,
                'sortable' => false,
                'related_fields' => array(
                    0 => 'parent_id',
                    1 => 'parent_type',
                ),
                'width' => '10%',
                'default' => true,
                'ACLTag' => 'PARENT',
                'dynamic_module' => 'PARENT_TYPE',
                'id' => 'PARENT_ID',
                'name' => 'parent_name',
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
