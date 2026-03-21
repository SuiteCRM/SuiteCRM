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

$dictionary['AOR_Condition'] = array(
    'table'=>'aor_conditions',
    'audited'=>false,
    'duplicate_merge'=>true,
    'fields'=>array(
  'aor_report_id' =>
  array(
    'required' => false,
    'name' => 'aor_report_id',
    'vname' => 'LBL_AOR_REPORT_ID',
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
  'condition_order' =>
  array(
    'required' => false,
    'name' => 'condition_order',
    'vname' => 'LBL_ORDER',
    'type' => 'int',
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
    'enable_range_search' => false,
    'disable_num_format' => '',
  ),
        'logic_op' => array(
            'name' => 'logic_op',
            'vname' => 'LBL_LOGIC_OP',
            'type' => 'varchar',
            'default_value' => 'AND',
        ),
        'parenthesis' => array(
            'name' => 'parenthesis',
            'vname' => 'LBL_PARENTHESIS',
            'type' => 'varchar',
        ),
  'module_path' =>
  array(
    'name' => 'module_path',
    'type' => 'longtext',
    'vname' => 'LBL_MODULE_PATH',
    'isnull' => true,
  ),
  'field' =>
  array(
    'required' => false,
    'name' => 'field',
    'vname' => 'LBL_FIELD',
    'type' => 'enum',
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
    'options' => 'user_type_dom',
    'studio' => 'visible',
    'dependency' => false,
  ),
  'operator' =>
  array(
    'required' => false,
    'name' => 'operator',
    'vname' => 'LBL_OPERATOR',
    'type' => 'enum',
    'massupdate' => 0,
    'default' => '',
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
    'options' => 'aor_operator_list',
    'studio' => 'visible',
    'dependency' => false,
  ),
    'value_type' =>
    array(
    'required' => false,
    'name' => 'value_type',
    'vname' => 'LBL_VALUE_TYPE',
    'type' => 'enum',
    'massupdate' => 0,
    'default' => '',
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
    'options' => 'aor_condition_type_list',
    'studio' => 'visible',
    'dependency' => false,
  ),
  'value' =>
  array(
    'required' => false,
    'name' => 'value',
    'vname' => 'LBL_VALUE',
    'type' => 'varchar',
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
  ),
        'parameter' =>
            array(
                'required' => false,
                'name' => 'parameter',
                'vname' => 'LBL_PARAMETER',
                'type' => 'bool',
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
                'studio' => 'visible',
            ),
  'aor_reports' =>
  array(
    'name' => 'aor_reports',
    'type' => 'link',
    'relationship' => 'aor_reports_aor_conditions',
    'module'=>'AOR_Reports',
    'bean_name'=>'AOR_Reports',
    'source'=>'non-db',
  ),
),
    'relationships'=>array(
),
    'indices' => array(
        array(
            'name' => 'aor_conditions_index_report_id',
            'type' => 'index',
            'fields' => array('aor_report_id'),
        ),
    ),
    'optimistic_locking'=>true,
    'unified_search'=>true,
);

if (!class_exists('VardefManager')) {
    require_once('include/SugarObjects/VardefManager.php');
}
VardefManager::createVardef('AOR_Conditions', 'AOR_Condition', array('basic'));
