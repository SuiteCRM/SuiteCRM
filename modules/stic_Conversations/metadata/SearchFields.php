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

$module_name = 'stic_Conversations';
$searchFields['stic_Conversations'] = array (
  'name' => 
  array (
    'query_type' => 'default',
  ),
  'contact_name' =>
  array (
    'query_type' => 'format',
    'operator' => 'subquery',
    'subquery' => "SELECT contacts_stic_conversations_c.stic_conversations_idb FROM contacts_stic_conversations_c INNER JOIN contacts ON contacts.id = contacts_stic_conversations_c.contacts_ida AND contacts.deleted = 0 WHERE contacts_stic_conversations_c.deleted = 0 AND (contacts.first_name LIKE '%{0}%' OR contacts.last_name LIKE '%{0}%')",
    'db_field' =>
    array (
      0 => 'id',
    ),
  ),
  'CONTACTS_IDA' =>
  array (
    'query_type' => 'format',
    'operator' => 'subquery',
    'subquery' => "SELECT contacts_stic_conversations_c.stic_conversations_idb FROM contacts_stic_conversations_c WHERE contacts_stic_conversations_c.deleted = 0 AND contacts_stic_conversations_c.contacts_ida = '{0}'",
    'db_field' =>
    array (
      0 => 'id',
    ),
  ),
  'current_user_only' => 
  array (
    'query_type' => 'default',
    'db_field' => 
    array (
      0 => 'assigned_user_id',
    ),
    'my_items' => true,
    'vname' => 'LBL_CURRENT_USER_FILTER',
    'type' => 'bool',
  ),
  'assigned_user_id' => 
  array (
    'query_type' => 'default',
  ),
  'range_date_entered' => 
  array (
    'query_type' => 'default',
    'enable_range_search' => true,
    'is_date_field' => true,
  ),
  'start_range_date_entered' => 
  array (
    'query_type' => 'default',
    'enable_range_search' => true,
    'is_date_field' => true,
  ),
  'end_range_date_entered' => 
  array (
    'query_type' => 'default',
    'enable_range_search' => true,
    'is_date_field' => true,
  ),
  'range_date_modified' => 
  array (
    'query_type' => 'default',
    'enable_range_search' => true,
    'is_date_field' => true,
  ),
  'start_range_date_modified' => 
  array (
    'query_type' => 'default',
    'enable_range_search' => true,
    'is_date_field' => true,
  ),
  'end_range_date_modified' => 
  array (
    'query_type' => 'default',
    'enable_range_search' => true,
    'is_date_field' => true,
  ),

  'favorites_only' => array(
        'query_type' => 'format',
        'operator' => 'subquery',
        'checked_only' => true,
        'subquery' => 'SELECT favorites.parent_id FROM favorites
			                    WHERE favorites.deleted = 0
			                        and favorites.parent_type = \'stic_Conversations\'
			                        and favorites.assigned_user_id = \'{1}\'',
        'db_field' => array(
            0 => 'id',
        ),
    ),
);
