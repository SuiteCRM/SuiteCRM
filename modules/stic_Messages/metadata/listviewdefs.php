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
$module_name = 'stic_Messages';
$listViewDefs[$module_name] =
array(
    'NAME' => array(
        'width' => '30%',
        'label' => 'LBL_NAME',
        'default' => true,
        'link' => true,
    ),
    'PARENT_NAME' => array(
        'width' => '20%',
        'label' => 'LBL_LIST_RELATED_TO',
        'dynamic_module' => 'PARENT_TYPE',
        'id' => 'PARENT_ID',
        'link' => true,
        'default' => true,
        'sortable' => false,
        'ACLTag' => 'PARENT',
        'related_fields' =>
            array(
            0 => 'parent_id',
            1 => 'parent_type',
            ),
    ),
    'PHONE' => array(
        'width' => '7%',
        'label' => 'LBL_PHONE',
        'default' => true
    ),
    'TEMPLATE' => 
    array (
      'type' => 'relate',
      'default' => true,
      'studio' => 'visible',
      'label' => 'LBL_TEMPLATE',
      'id' => 'TEMPLATE_ID',
      'link' => true,
      'width' => '7%',
    ),
    'TYPE' => array(
        'type' => 'enum',
        'studio' => 'visible',
        'label' => 'LBL_TYPE',
        'width' => '7%',
        'default' => true,
    ),
    'STATUS' => array(
        'type' => 'enum',
        'studio' => 'visible',
        'width' => '7%',
        'label' => 'LBL_STATUS',
        'default' => true
    ),
    'SENT_DATE' => array(
        'type' => 'datetime',
        'studio' => 'visible',
        'label' => 'LBL_SENT_DATE',
        'width' => '7%',
        'default' => true,
    ),
    'STIC_CONVERSATIONS_STIC_MESSAGES_NAME' => array (
        // 'type' => 'relate',
        'link' => true,
        'label' => 'LBL_STIC_CONVERSATIONS_STIC_MESSAGES',
        'id' => 'STIC_CONVERSATIONS_IDA',
        'width' => '10%',
        'default' => true,
    ),
    'SENDER' => array(
        'width' => '10%',
        'label' => 'LBL_SENDER',
        'default' => false
    ),
    'MESSAGE' => array(
        'type' => 'text',
        'label' => 'LBL_MESSAGE',
        'sortable' => false,
        'width' => '20%',
        'default' => false,
    ),
    'ASSIGNED_USER_NAME' => array(
        'width' => '9%',
        'label' => 'LBL_ASSIGNED_TO_NAME',
        'module' => 'Employees',
        'id' => 'ASSIGNED_USER_ID',
        'default' => true,
    ),
    'CREATED_BY_NAME' => array(
        'type' => 'relate',
        'link' => true,
        'label' => 'LBL_CREATED',
        'id' => 'CREATED_BY',
        'width' => '10%',
        'default' => false,
    ),
    'MODIFIED_BY_NAME' => array(
        'type' => 'relate',
        'link' => true,
        'label' => 'LBL_MODIFIED_NAME',
        'id' => 'MODIFIED_USER_ID',
        'width' => '10%',
        'default' => false,
    ),
    'DATE_MODIFIED' => array(
        'type' => 'datetime',
        'label' => 'LBL_DATE_MODIFIED',
        'width' => '10%',
        'default' => false,
    ),
    'DATE_ENTERED' => array(
        'type' => 'datetime',
        'label' => 'LBL_DATE_ENTERED',
        'width' => '10%',
        'default' => false,
    ),
);
