<?php
/**
 * SuiteCRM is a customer relationship management program developed by SuiteCRM Ltd.
 * Copyright (C) 2011 - 2025 SuiteCRM Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUITECRM, SUITECRM DISCLAIMS THE
 * WARRANTY OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License
 * version 3, these Appropriate Legal Notices must retain the display of the
 * "Supercharged by SuiteCRM" logo. If the display of the logos is not reasonably
 * feasible for technical reasons, the Appropriate Legal Notices must display
 * the words "Supercharged by SuiteCRM".
 */


$dictionary['AOW_Processed'] = array(
    'table'=>'aow_processed',
    'audited'=>false,
    'duplicate_merge'=>true,
    'fields'=>array(
  'aow_workflow_id' =>
  array(
    'required' => false,
    'name' => 'aow_workflow_id',
    'vname' => 'LBL_AOW_WORKFLOW_ID',
    'type' => 'id',
    'massupdate' => 0,
    'comments' => '',
    'help' => '',
    'importable' => 'true',
    'duplicate_merge' => 'disabled',
    'duplicate_merge_dom_value' => 0,
    'audited' => false,
    'reportable' => false,
    'unified_search' => false,
    'merge_filter' => 'disabled',
    'len' => 36,
    'size' => '20',
  ),
  'aow_workflow' =>
  array(
    'required' => false,
    'source' => 'non-db',
    'name' => 'aow_workflow',
    'vname' => 'LBL_AOW_WORKFLOW',
    'type' => 'relate',
    'massupdate' => 0,
    'comments' => '',
    'help' => '',
    'importable' => 'true',
    'duplicate_merge' => 'disabled',
    'duplicate_merge_dom_value' => '0',
    'audited' => false,
    'reportable' => true,
    'unified_search' => false,
    'merge_filter' => 'disabled',
    'len' => '255',
    'size' => '20',
    'id_name' => 'aow_workflow_id',
    'ext2' => 'AOW_WorkFlow',
    'module' => 'AOW_WorkFlow',
    'rname' => 'name',
    'quicksearch' => 'enabled',
    'studio' => 'visible',
  ),
  'parent_id' =>
  array(
    'required' => false,
    'name' => 'parent_id',
    'vname' => 'LBL_BEAN_ID',
    'type' => 'id',
    'massupdate' => 0,
    'comments' => '',
    'help' => '',
    'importable' => 'true',
    'duplicate_merge' => 'disabled',
    'duplicate_merge_dom_value' => 0,
    'audited' => false,
    'reportable' => false,
    'unified_search' => false,
    'group'=>'bean',
    'merge_filter' => 'disabled',
    'len' => 36,
    'size' => '20',
  ),
  'parent_name'=>
  array(
    'name'=> 'parent_name',
    'parent_type'=>'record_type_display' ,
    'type_name'=>'parent_type',
    'id_name'=>'parent_id',
    'vname'=>'LBL_BEAN',
    'type'=>'parent',
    'group'=>'parent_name',
    'source'=>'non-db',
    'options'=> 'moduleList',
  ),

  'parent_type' =>
  array(
    'required' => false,
    'name' => 'parent_type',
    'vname' => 'LBL_MODULE',
    'type' => 'parent_type',
    'dbType'=>'varchar',
    'massupdate' => 0,
    'comments' => '',
    'help' => '',
    'importable' => 'true',
    'duplicate_merge' => 'disabled',
    'duplicate_merge_dom_value' => '0',
    'audited' => false,
    'reportable' => true,
    'unified_search' => false,
    'merge_filter' => 'disabled',
    'len' => 100,
    'size' => '20',
    'group'=>'bean',
    'options' => 'moduleList',
    'studio' => 'visible',
    'dependency' => false,
  ),
  'status' =>
  array(
    'required' => false,
    'name' => 'status',
    'vname' => 'LBL_STATUS',
    'type' => 'enum',
    'massupdate' => 0,
    'default' => 'Pending',
    'comments' => '',
    'help' => '',
    'importable' => 'true',
    'duplicate_merge' => 'disabled',
    'duplicate_merge_dom_value' => '0',
    'audited' => false,
    'reportable' => true,
    'unified_search' => false,
    'merge_filter' => 'disabled',
    'len' => 100,
    'size' => '20',
    'options' => 'aow_process_status_list',
    'studio' => 'visible',
    'dependency' => false,
  ),
  'aow_actions' =>
  array(
    'name' => 'aow_actions',
    'type' => 'link',
    'relationship' => 'aow_processed_aow_actions',
    'module'=>'AOW_Actions',
    'bean_name'=>'AOW_Action',
    'source'=>'non-db',
  ),
),
    'relationships'=>array(
),
    'indices' => array(
        array(
            'name' => 'aow_processed_index_workflow',
            'type' => 'index',
            'fields' => array('aow_workflow_id','status','parent_id','deleted'),
        ),
        array(
            'name' => 'aow_processed_index_status',
            'type' => 'index',
            'fields' => array('status'),
        ),
        array(
            'name' => 'aow_processed_index_workflow_id',
            'type' => 'index',
            'fields' => array('aow_workflow_id'),
        ),
    ),
    'optimistic_locking'=>true,
        'unified_search'=>true,
    );
if (!class_exists('VardefManager')) {
    require_once('include/SugarObjects/VardefManager.php');
}
VardefManager::createVardef('AOW_Processed', 'AOW_Processed', array('basic'));
