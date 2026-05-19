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

$dictionary['stic_Conversations'] = array(
  'table' => 'stic_conversations',
  'audited' => true,
  'inline_edit' => true,
  'duplicate_merge' => true,
  'fields' => array(
    'code' =>
    array(
      'required' => false,
      'name' => 'code',
      'vname' => 'LBL_CODE',
      'type' => 'int',
      'massupdate' => 0,
      'no_default' => false,
      'comments' => '',
      'help' => '',
      'importable' => 'true',
      'duplicate_merge' => 'disabled',
      'duplicate_merge_dom_value' => '0',
      'audited' => false,
      'inline_edit' => false,
      'reportable' => true,
      'unified_search' => false,
      'merge_filter' => 'disabled',
      'len' => '255',
      'size' => '20',
      'enable_range_search' => false,
      'disable_num_format' => '',
      'min' => false,
      'max' => false,
    ),
    'subject' =>
    array(
      'required' => false,
      'name' => 'subject',
      'vname' => 'LBL_SUBJECT',
      'type' => 'varchar',
      'massupdate' => 0,
      'no_default' => false,
      'comments' => '',
      'help' => '',
      'importable' => 'true',
      'duplicate_merge' => 'disabled',
      'duplicate_merge_dom_value' => '0',
      'audited' => false,
      'inline_edit' => true,
      'reportable' => true,
      'unified_search' => false,
      'merge_filter' => 'disabled',
      'len' => '255',
      'size' => '20',
      'required' => true,
    ),
    'type' =>
    array(
      'required' => false,
      'name' => 'type',
      'vname' => 'LBL_TYPE',
      'type' => 'enum',
      'massupdate' => 1,
      'no_default' => false,
      'comments' => '',
      'help' => '',
      'importable' => 'true',
      'duplicate_merge' => 'disabled',
      'duplicate_merge_dom_value' => '0',
      'audited' => false,
      'inline_edit' => true,
      'reportable' => true,
      'unified_search' => false,
      'merge_filter' => 'disabled',
      'len' => 100,
      'size' => '20',
      'options' => 'stic_conversations_types_list',
      'studio' => 'visible',
      'dependency' => false,
    ),
    // Relationships
    'stic_conversations_stic_messages' => array (
      'name' => 'stic_conversations_stic_messages',
      'type' => 'link',
      'relationship' => 'stic_conversations_stic_messages',
      'source' => 'non-db',
      'module' => 'stic_Messages',
      'bean_name' => false,
      'side' => 'right',
      'vname' => 'LBL_STIC_MESSAGES_SUBPANEL_TITLE',
    ),
    'contacts_stic_conversations' => array(
      'name' => 'contacts_stic_conversations',
      'type' => 'link',
      'relationship' => 'contacts_stic_conversations',
      'source' => 'non-db',
      'module' => 'Contacts',
      'bean_name' => 'Contact',
      'vname' => 'LBL_CONTACT_NAME',
      'id_name' => 'contacts_ida',
    ),
    'contact_name' => array(
      'name' => 'contact_name',
      'type' => 'relate',
      'source' => 'non-db',
      'vname' => 'LBL_CONTACT_NAME',
      'save' => true,
      'id_name' => 'contacts_ida',
      'link' => 'contacts_stic_conversations',
      'table' => 'contacts',
      'module' => 'Contacts',
      'rname' => 'name',
      'required' => true,
      'inline_edit' => false,
    ),
    'contacts_ida' => array(
      'name' => 'contacts_ida',
      'type' => 'link',
      'relationship' => 'contacts_stic_conversations',
      'source' => 'non-db',
      'reportable' => false,
      'side' => 'right',
      'vname' => 'LBL_CONTACT_NAME',
    ),
  ),
  'relationships' => array(),
  'optimistic_locking' => true,
  'unified_search' => true,
);
if (!class_exists('VardefManager')) {
  require_once('include/SugarObjects/VardefManager.php');
}
VardefManager::createVardef('stic_Conversations', 'stic_Conversations', array('basic', 'assignable', 'security_groups'));

// Set special values for SuiteCRM base fields
$dictionary['stic_Conversations']['fields']['name']['required'] = '0'; // Name is not required in this module
