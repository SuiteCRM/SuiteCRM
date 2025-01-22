<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUGARCRM, SUGARCRM DISCLAIMS THE WARRANTY
 * OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
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
 * You can contact SugarCRM, Inc. headquarters at 10050 North Wolfe Road,
 * SW2-130, Cupertino, CA 95014, USA. or at email address contact@sugarcrm.com.
 *
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo and "Supercharged by SuiteCRM" logo. If the display of the logos is not
 * reasonably feasible for technical reasons, the Appropriate Legal Notices must
 * display the words "Powered by SugarCRM" and "Supercharged by SuiteCRM".
 */


$dictionary['AOW_WorkFlow'] = array(
    'table' => 'aow_workflow',
    'audited' => true,
    'duplicate_merge' => true,
    'fields' => array(
        'flow_module' =>
            array(
                'required' => true,
                'name' => 'flow_module',
                'vname' => 'LBL_FLOW_MODULE',
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
                'options' => 'aow_moduleList',
                'studio' => 'visible',
                'dependency' => false,
            ),
        'flow_run_on' =>
            array(
                'required' => true,
                'name' => 'flow_run_on',
                'vname' => 'LBL_RUN_ON',
                'type' => 'enum',
                'massupdate' => 0,
                'default' => '0',
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
                'options' => 'aow_run_on_list',
                'studio' => 'visible',
                'dependency' => false,
            ),
        'status' =>
            array(
                'required' => false,
                'name' => 'status',
                'vname' => 'LBL_STATUS',
                'type' => 'enum',
                'massupdate' => 1,
                'default' => 'Active',
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
                'options' => 'aow_status_list',
                'studio' => 'visible',
                'dependency' => false,
            ),
        'run_when' =>
            array(
                'required' => false,
                'name' => 'run_when',
                'vname' => 'LBL_RUN_WHEN',
                'type' => 'enum',
                'massupdate' => 0,
                'default' => 'Always',
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
                'options' => 'aow_run_when_list',
                'studio' => 'visible',
                'dependency' => false,
            ),
        'multiple_runs' =>
            array(
                'name' => 'multiple_runs',
                'vname' => 'LBL_MULTIPLE_RUNS',
                'type' => 'bool',
                'default' => '0',
                'reportable' => false,
            ),
        'condition_lines' =>
            array(
                'required' => false,
                'name' => 'condition_lines',
                'vname' => 'LBL_CONDITION_LINES',
                'type' => 'function',
                'source' => 'non-db',
                'massupdate' => 0,
                'importable' => 'false',
                'duplicate_merge' => 'disabled',
                'duplicate_merge_dom_value' => 0,
                'audited' => false,
                'reportable' => false,
                'inline_edit' => false,
                'function' =>
                    array(
                        'name' => 'display_condition_lines',
                        'returns' => 'html',
                        'include' => 'modules/AOW_Conditions/conditionLines.php'
                    ),
            ),
        'action_lines' =>
            array(
                'required' => false,
                'name' => 'action_lines',
                'vname' => 'LBL_ACTION_LINES',
                'type' => 'function',
                'source' => 'non-db',
                'massupdate' => 0,
                'importable' => 'false',
                'duplicate_merge' => 'disabled',
                'duplicate_merge_dom_value' => 0,
                'audited' => false,
                'reportable' => false,
                'inline_edit' => false,
                'function' =>
                    array(
                        'name' => 'display_action_lines',
                        'returns' => 'html',
                        'include' => 'modules/AOW_Actions/actionLines.php'
                    ),
            ),
        'aow_conditions' =>
            array(
                'name' => 'aow_conditions',
                'type' => 'link',
                'relationship' => 'aow_workflow_aow_conditions',
                'module' => 'AOW_Conditions',
                'bean_name' => 'AOW_Condition',
                'source' => 'non-db',
            ),
        'aow_actions' =>
            array(
                'name' => 'aow_actions',
                'type' => 'link',
                'relationship' => 'aow_workflow_aow_actions',
                'module' => 'AOW_Actions',
                'bean_name' => 'AOW_Action',
                'source' => 'non-db',
            ),
        'aow_processed' =>
            array(
                'name' => 'aow_processed',
                'type' => 'link',
                'relationship' => 'aow_workflow_aow_processed',
                'module' => 'AOW_Processed',
                'bean_name' => 'AOW_Processed',
                'source' => 'non-db',
            ),
        'run_on_import' =>
            array(
                'name' => 'run_on_import',
                'vname' => 'LBL_RUN_ON_IMPORT',
                'type' => 'bool',
                'default' => '0',
                'reportable' => false,
            ),
    ),
    'relationships' => array(
        'aow_workflow_aow_conditions' =>
            array(
                'lhs_module' => 'AOW_WorkFlow',
                'lhs_table' => 'aow_workflow',
                'lhs_key' => 'id',
                'rhs_module' => 'AOW_Conditions',
                'rhs_table' => 'aow_conditions',
                'rhs_key' => 'aow_workflow_id',
                'relationship_type' => 'one-to-many',
            ),
        'aow_workflow_aow_actions' =>
            array(
                'lhs_module' => 'AOW_WorkFlow',
                'lhs_table' => 'aow_workflow',
                'lhs_key' => 'id',
                'rhs_module' => 'AOW_Actions',
                'rhs_table' => 'aow_actions',
                'rhs_key' => 'aow_workflow_id',
                'relationship_type' => 'one-to-many',
            ),
        'aow_workflow_aow_processed' =>
            array(
                'lhs_module' => 'AOW_WorkFlow',
                'lhs_table' => 'aow_workflow',
                'lhs_key' => 'id',
                'rhs_module' => 'AOW_Processed',
                'rhs_table' => 'aow_processed',
                'rhs_key' => 'aow_workflow_id',
                'relationship_type' => 'one-to-many',
            ),
    ),
    'indices' => array(
        array(
            'name' => 'aow_workflow_index_status',
            'type' => 'index',
            'fields' => array('status'),
        ),
    ),
    'optimistic_locking' => true,
    'unified_search' => true,
);
if (!class_exists('VardefManager')) {
    require_once('include/SugarObjects/VardefManager.php');
}
VardefManager::createVardef('AOW_WorkFlow', 'AOW_WorkFlow', array('basic', 'assignable', 'security_groups'));
