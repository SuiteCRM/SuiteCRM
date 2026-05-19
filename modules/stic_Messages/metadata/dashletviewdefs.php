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
$dashletData['stic_MessagesDashlet']['searchFields'] = array(
    'name' => array(
        'default' => '',
    ),
    // 'parent_name' => array(
    //     'default' => '',
    // ),
    'type' => array(
        'default' => '',
    ),
    'direction' => array(
        'default' => '',
    ),
    'status' => array(
        'default' => '',
    ),
    'phone' => array(
        'default' => '',
    ),
    'date_entered' => array(
        'default' => '',
    ),
    'assigned_user_id' => array(
        'default' => '',
    ),
    'stic_conversations_stic_messages_name' => 
    array (
      'default' => '',
    ),
);
$dashletData['stic_MessagesDashlet']['columns'] = array(
    'name' => array(
        'width' => '40%',
        'label' => 'LBL_LIST_NAME',
        'link' => true,
        'default' => true,
        'name' => 'name',
    ),
    'parent_name' => array(
        'width' => '30%',
        'label' => 'LBL_LIST_RELATED_TO',
        'sortable' => false,
        'dynamic_module' => 'PARENT_TYPE',
        'link' => true,
        'id' => 'PARENT_ID',
        'ACLTag' => 'PARENT',
        'related_fields' => array(
            0 => 'parent_id',
            1 => 'parent_type',
        ),
        'default' => true,
        'name' => 'parent_name',
    ),
    'type' => array(
        'width' => '7%',
        'label' => 'LBL_TYPE',
        'sortable' => true,
        'default' => true,
        'name' => 'type',
    ),
    'direction' => array(
        'width' => '7%',
        'label' => 'LBL_DIRECTION',
        'sortable' => true,
        'default' => false,
        'name' => 'direction',
    ),
    'stic_conversations_stic_messages_name' => 
    array (
      'type' => 'relate',
      'link' => true,
      'label' => 'LBL_STIC_CONVERSATIONS_STIC_MESSAGES',
      'id' => 'STIC_CONVERSATIONS_IDA',
      'width' => '10%',
      'default' => true,
    ),
    'template' => array(
        'type' => 'relate',
        'link' => true,
        'label' => 'LBL_TEMPLATE',
        'id' => 'TEMPLATE',
        'width' => '10%',
        'default' => true,
        'name' => 'template',
    ),
    'status' => array(
        'width' => '7%',
        'label' => 'LBL_STATUS',
        'default' => true,
        'name' => 'status',
    ),
    'date_entered' => array(
        'width' => '15%',
        'label' => 'LBL_DATE_ENTERED',
        'default' => true,
        'name' => 'date_entered',
    ),
    'created_by_name' => array(
        'type' => 'relate',
        'link' => true,
        'label' => 'LBL_CREATED',
        'id' => 'CREATED_BY',
        'width' => '10%',
        'default' => false,
        'name' => 'created_by_name',
    ),
    'message' => array(
        'type' => 'text',
        'label' => 'LBL_MESSAGE',
        'sortable' => false,
        'width' => '10%',
        'default' => false,
        'name' => 'message',
    ),
);
