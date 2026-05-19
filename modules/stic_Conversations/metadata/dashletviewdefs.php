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

global $current_user;

$dashletData['stic_ConversationsDashlet']['searchFields'] = array (
  'code' => 
  array (
    'default' => '',
  ),
  'name' => 
  array (
    'default' => '',
  ),
  'subject' => 
  array (
    'default' => '',
  ),
  'type' => 
  array (
    'default' => '',
  ),
  'assigned_user_name' => 
  array (
    'default' => '',
  ),
  'contact_name' => 
  array (
    'default' => '',
  ),
  'created_by_name' => 
  array (
    'default' => '',
  ),
  'date_entered' => 
  array (
    'default' => '',
  ),
  'modified_by_name' => 
  array (
    'default' => '',
  ),
  'date_modified' => 
  array (
    'default' => '',
  ),
);
$dashletData['stic_ConversationsDashlet']['columns'] = array (
  'code' => 
  array (
    'type' => 'int',
    'label' => 'LBL_CODE',
    'width' => '10%',
    'default' => true,
    'name' => 'code',
  ),
  'name' => 
  array (
    'width' => '40%',
    'label' => 'LBL_LIST_NAME',
    'link' => true,
    'default' => true,
    'name' => 'name',
  ),
  'subject' => 
  array (
    'type' => 'varchar',
    'label' => 'LBL_SUBJECT',
    'width' => '10%',
    'default' => true,
    'name' => 'subject',
  ),
  'type' => 
  array (
    'type' => 'enum',
    'studio' => 'visible',
    'label' => 'LBL_TYPE',
    'width' => '10%',
    'default' => true,
    'name' => 'type',
  ),
  'contact_name' => 
  array (
    'type' => 'relate',
    'link' => true,
    'label' => 'LBL_CONTACT_NAME',
    'id' => 'CONTACTS_IDA',
    'width' => '10%',
    'default' => true,
  ),
  'assigned_user_name' => 
  array (
    'width' => '8%',
    'label' => 'LBL_LIST_ASSIGNED_USER',
    'name' => 'assigned_user_name',
    'default' => true,
  ),
  'description' => 
  array (
    'type' => 'text',
    'label' => 'LBL_DESCRIPTION',
    'sortable' => false,
    'width' => '10%',
    'default' => false,
    'name' => 'description',
  ),
  'created_by_name' => 
  array (
    'type' => 'relate',
    'link' => true,
    'label' => 'LBL_CREATED',
    'id' => 'CREATED_BY',
    'width' => '10%',
    'default' => false,
    'name' => 'created_by_name',
  ),
  'date_entered' => 
  array (
    'width' => '15%',
    'label' => 'LBL_DATE_ENTERED',
    'default' => false,
    'name' => 'date_entered',
  ),
  'modified_by_name' => 
  array (
    'type' => 'relate',
    'link' => true,
    'label' => 'LBL_MODIFIED_NAME',
    'id' => 'MODIFIED_USER_ID',
    'width' => '10%',
    'default' => false,
    'name' => 'modified_by_name',
  ),
  'date_modified' => 
  array (
    'width' => '15%',
    'label' => 'LBL_DATE_MODIFIED',
    'name' => 'date_modified',
    'default' => false,
  ),
);
