<?php
// created: 2013-04-30 14:55:07
$dictionary["fp_events_prospects_1"] = array(
  'true_relationship_type' => 'many-to-many',
  'from_studio' => true,
  'relationships' =>
  array(
    'fp_events_prospects_1' =>
    array(
      'lhs_module' => 'FP_events',
      'lhs_table' => 'fp_events',
      'lhs_key' => 'id',
      'rhs_module' => 'Prospects',
      'rhs_table' => 'prospects',
      'rhs_key' => 'id',
      'relationship_type' => 'many-to-many',
      'join_table' => 'fp_events_prospects_1_c',
      'join_key_lhs' => 'fp_events_prospects_1fp_events_ida',
      'join_key_rhs' => 'fp_events_prospects_1prospects_idb',
    ),
  ),
  'table' => 'fp_events_prospects_1_c',
  'fields' =>
  array(
    0 =>
    array(
      'name' => 'id',
      'type' => 'varchar',
      'len' => 36,
    ),
    1 =>
    array(
      'name' => 'date_modified',
      'type' => 'datetime',
    ),
    2 =>
    array(
      'name' => 'deleted',
      'type' => 'bool',
      'len' => '1',
      'default' => '0',
      'required' => true,
    ),
    3 =>
    array(
      'name' => 'fp_events_prospects_1fp_events_ida',
      'type' => 'varchar',
      'len' => 36,
    ),
    4 =>
    array(
      'name' => 'fp_events_prospects_1prospects_idb',
      'type' => 'varchar',
      'len' => 36,
    ),
    5 =>
    array(
      'name' => 'invite_status',
      'type' => 'varchar',
      'len'=>'25',
      'default'=>'Not Invited',
    ),
    6 =>
    array(
      'name' => 'accept_status',
      'type' => 'varchar',
      'len'=>'25',
      'default'=>'No Response',
    ),
    7 =>
    array(
      'name' => 'email_responded',
      'type' => 'int',
      'len' => '2',
      'default' => '0',
   ),
  ),
  'indices' =>
  array(
    0 =>
    array(
      'name' => 'fp_events_prospects_1spk',
      'type' => 'primary',
      'fields' =>
      array(
        0 => 'id',
      ),
    ),
    1 =>
    array(
      'name' => 'fp_events_prospects_1_alt',
      'type' => 'alternate_key',
      'fields' =>
      array(
        0 => 'fp_events_prospects_1fp_events_ida',
        1 => 'fp_events_prospects_1prospects_idb',
      ),
    ),
    2 =>
    array(
      'name' => 'idx_events_prospects_prospect_id',
      'type' => 'index',
      'fields' =>
      array(
        0 => 'fp_events_prospects_1prospects_idb',
        1 => 'deleted',
        2 => 'fp_events_prospects_1fp_events_ida',
      ),
    ),
  ),
);
