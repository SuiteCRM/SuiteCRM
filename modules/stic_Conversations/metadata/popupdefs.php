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

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

$popupMeta = array(
    'moduleMain' => 'stic_Conversations',
    'varName' => 'stic_Conversations',
    'orderBy' => 'stic_Conversations.name',
    'whereClauses' => array(
        'name' => 'stic_Conversations.name',
        'code' => 'stic_conversations.code',
        'subject' => 'stic_conversations.subject',
        'type' => 'stic_conversations.type',
        'assigned_user_id' => 'stic_conversations.assigned_user_id',
        'contact_name' => 'stic_conversations.contact_name',
    ),
    'searchInputs' => array(
        1 => 'name',
        4 => 'code',
        5 => 'subject',
        6 => 'type',
        7 => 'assigned_user_id',
        8 => 'contact_name',
    ),
    'searchdefs' => array(
        'code' =>
        array(
            'type' => 'int',
            'label' => 'LBL_CODE',
            'width' => '10%',
            'name' => 'code',
        ),
        'name' =>
        array(
            'name' => 'name',
            'width' => '10%',
        ),
        'subject' =>
        array(
            'type' => 'varchar',
            'label' => 'LBL_SUBJECT',
            'width' => '10%',
            'name' => 'subject',
        ),
        'type' =>
        array(
            'type' => 'enum',
            'studio' => 'visible',
            'label' => 'LBL_TYPE',
            'width' => '10%',
            'name' => 'type',
        ),
        'contact_name' => 
        array (
            'type' => 'relate',
            'link' => true,
            'label' => 'LBL_CONTACT_NAME',
            'id' => 'CONTACTS_IDA',
            'width' => '10%',
            'name' => 'contact_name',
        ),
        'assigned_user_id' =>
        array(
            'name' => 'assigned_user_id',
            'label' => 'LBL_ASSIGNED_TO',
            'type' => 'enum',
            'function' =>
            array(
                'name' => 'get_user_array',
                'params' =>
                array(
                    0 => false,
                ),
            ),
            'width' => '10%',
        ),
    ),
    'listviewdefs' => array(
        'CODE' =>
        array(
            'type' => 'int',
            'label' => 'LBL_CODE',
            'width' => '10%',
            'default' => true,
            'name' => 'code',
        ),
        'NAME' =>
        array(
            'width' => '32%',
            'label' => 'LBL_NAME',
            'default' => true,
            'link' => true,
            'name' => 'name',
        ),
        'SUBJECT' =>
        array(
            'type' => 'varchar',
            'label' => 'LBL_SUBJECT',
            'width' => '10%',
            'default' => true,
            'name' => 'subject',
        ),
        'TYPE' =>
        array(
            'type' => 'enum',
            'studio' => 'visible',
            'label' => 'LBL_TYPE',
            'width' => '10%',
            'default' => true,
            'name' => 'type',
        ),
        'CONTACT_NAME' => 
        array (
            'type' => 'relate',
            'link' => true,
            'label' => 'LBL_CONTACT_NAME',
            'id' => 'CONTACTS_IDA',
            'width' => '10%',
            'default' => true,
        ),
        'ASSIGNED_USER_NAME' =>
        array(
            'width' => '9%',
            'label' => 'LBL_ASSIGNED_TO_NAME',
            'module' => 'Employees',
            'id' => 'ASSIGNED_USER_ID',
            'default' => true,
            'name' => 'assigned_user_name',
        ),
    ),
);
