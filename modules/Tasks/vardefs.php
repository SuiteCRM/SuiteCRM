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

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

$dictionary['Task'] = array(
    'table' => 'tasks',
    'unified_search' => true,
    'full_text_search' => true,
    'fields' => array(
        'name' =>
            array(
                'name' => 'name',
                'vname' => 'LBL_SUBJECT',
                'dbType' => 'varchar',
                'type' => 'name',
                'len' => '50',
                'unified_search' => true,
                'full_text_search' => array('boost' => 3),
                'importable' => 'required',
                'required' => 'true',
            ),
        'status' =>
            array(
                'name' => 'status',
                'vname' => 'LBL_STATUS',
                'type' => 'enum',
                'options' => 'task_status_dom',
                'len' => 100,
                'required' => 'true',
                'default' => 'Not Started',
            ),
        'date_due_flag' =>
            array(
                'name' => 'date_due_flag',
                'vname' => 'LBL_DATE_DUE_FLAG',
                'type' => 'bool',
                'default' => 0,
                'group' => 'date_due',
                'studio' => false,
            ),
        'date_due' =>
            array(
                'name' => 'date_due',
                'vname' => 'LBL_DUE_DATE',
                'type' => 'datetimecombo',
                'dbType' => 'datetime',
                'group' => 'date_due',
                'studio' => array('required' => true, 'no_duplicate' => true),
                'enable_range_search' => true,
                'options' => 'date_range_search_dom',
            ),
        'time_due' =>
            array(
                'name' => 'time_due',
                'vname' => 'LBL_DUE_TIME',
                'type' => 'datetime',
                //'db_concat_fields'=> array(0=>'date_due'),
                'source' => 'non-db',
                'importable' => 'false',
                'massupdate' => false,
                'inline_edit' => false,
            ),
        'date_start_flag' =>
            array(
                'name' => 'date_start_flag',
                'vname' => 'LBL_DATE_START_FLAG',
                'type' => 'bool',
                'group' => 'date_start',
                'default' => 0,
                'studio' => false,
            ),
        'date_start' =>
            array(
                'name' => 'date_start',
                'vname' => 'LBL_START_DATE',
                'type' => 'datetimecombo',
                'dbType' => 'datetime',
                'group' => 'date_start',
                'validation' => array('type' => 'isbefore', 'compareto' => 'date_due', 'blank' => false),
                'studio' => array('required' => true, 'no_duplicate' => true),
                'enable_range_search' => true,
                'options' => 'date_range_search_dom',
            ),
        'parent_type' =>
            array(
                'name' => 'parent_type',
                'vname' => 'LBL_PARENT_TYPE',
                'type' => 'parent_type',
                'dbType' => 'varchar',
                'group' => 'parent_name',
                'options' => 'parent_type_display',
                'required' => false,
                'len' => '255',
                'comment' => 'The Sugar object to which the call is related',
                'options' => 'parent_type_display',
            ),

        'parent_name' =>
            array(
                'name' => 'parent_name',
                'parent_type' => 'record_type_display',
                'type_name' => 'parent_type',
                'id_name' => 'parent_id',
                'vname' => 'LBL_LIST_RELATED_TO',
                'type' => 'parent',
                'group' => 'parent_name',
                'source' => 'non-db',
                'options' => 'parent_type_display',
            ),

        'parent_id' =>
            array(
                'name' => 'parent_id',
                'type' => 'id',
                'group' => 'parent_name',
                'reportable' => false,
                'vname' => 'LBL_PARENT_ID',
            ),
        'contact_id' =>
            array(
                'name' => 'contact_id',
                'type' => 'id',
                'group' => 'contact_name',
                'reportable' => false,
                'vname' => 'LBL_CONTACT_ID',
            ),

        'contact_name' =>
            array(
                'name' => 'contact_name',
                'rname' => 'name',
                'db_concat_fields' => array(0 => 'first_name', 1 => 'last_name'),
                'source' => 'non-db',
                'len' => '510',
                'group' => 'contact_name',
                'vname' => 'LBL_CONTACT_NAME',
                'reportable' => false,
                'id_name' => 'contact_id',
                'join_name' => 'contacts',
                'type' => 'relate',
                'module' => 'Contacts',
                'link' => 'contacts',
                'table' => 'contacts',
            ),

        'contact_phone' =>
            array(
                'name' => 'contact_phone',
                'type' => 'phone',
                'source' => 'non-db',
                'vname' => 'LBL_CONTACT_PHONE',
                'studio' => array('listview' => true)
            ),

        'contact_email' =>
            array(
                'name' => 'contact_email',
                'type' => 'varchar',
                'vname' => 'LBL_EMAIL_ADDRESS',
                'source' => 'non-db',
                'studio' => false
            ),

        'priority' =>
            array(
                'name' => 'priority',
                'vname' => 'LBL_PRIORITY',
                'type' => 'enum',
                'options' => 'task_priority_dom',
                'len' => 100,
                'required' => 'true',
            ),
        'contacts' => array(
            'name' => 'contacts',
            'type' => 'link',
            'relationship' => 'contact_tasks',
            'source' => 'non-db',
            'side' => 'right',
            'vname' => 'LBL_CONTACT',
        ),
        'accounts' =>
            array(
                'name' => 'accounts',
                'type' => 'link',
                'relationship' => 'account_tasks',
                'source' => 'non-db',
                'vname' => 'LBL_ACCOUNT',
            ),
        'opportunities' =>
            array(
                'name' => 'opportunities',
                'type' => 'link',
                'relationship' => 'opportunity_tasks',
                'source' => 'non-db',
                'vname' => 'LBL_OPPORTUNITY',
            ),
        'cases' =>
            array(
                'name' => 'cases',
                'type' => 'link',
                'relationship' => 'case_tasks',
                'source' => 'non-db',
                'vname' => 'LBL_CASE',
            ),
        'bugs' =>
            array(
                'name' => 'bugs',
                'type' => 'link',
                'relationship' => 'bug_tasks',
                'source' => 'non-db',
                'vname' => 'LBL_BUGS',
            ),
        'leads' =>
            array(
                'name' => 'leads',
                'type' => 'link',
                'relationship' => 'lead_tasks',
                'source' => 'non-db',
                'vname' => 'LBL_LEADS',
            ),
        'projects' =>
            array(
                'name' => 'projects',
                'type' => 'link',
                'relationship' => 'projects_tasks',
                'source' => 'non-db',
                'vname' => 'LBL_PROJECTS',
            ),
        'project_tasks' =>
            array(
                'name' => 'project_tasks',
                'type' => 'link',
                'relationship' => 'project_tasks_tasks',
                'source' => 'non-db',
                'vname' => 'LBL_PROJECT_TASKS',
            ),
        'aos_contracts' =>
            array(
                'name' => 'aos_contracts',
                'type' => 'link',
                'relationship' => 'aos_contracts_tasks',
                'source' => 'non-db',
                'vname' => 'LBL_CONTRACT',
            ),
        'notes' =>
            array(
                'name' => 'notes',
                'type' => 'link',
                'relationship' => 'tasks_notes',
                'module' => 'Notes',
                'bean_name' => 'Note',
                'source' => 'non-db',
                'vname' => 'LBL_NOTES',
            ),

        'contact_parent' =>
            array(
                'name' => 'contact_parent',
                'type' => 'link',
                'relationship' => 'contact_tasks_parent',
                'source' => 'non-db',
                'reportable' => false
            ),
    )
,
    'relationships' => array(
        'tasks_notes' => array(
            'lhs_module' => 'Tasks',
            'lhs_table' => 'tasks',
            'lhs_key' => 'id',
            'rhs_module' => 'Notes',
            'rhs_table' => 'notes',
            'rhs_key' => 'parent_id',
            'relationship_type' => 'one-to-many',
        ),

        'tasks_assigned_user' =>
            array(
                'lhs_module' => 'Users',
                'lhs_table' => 'users',
                'lhs_key' => 'id',
                'rhs_module' => 'Tasks',
                'rhs_table' => 'tasks',
                'rhs_key' => 'assigned_user_id',
                'relationship_type' => 'one-to-many'
            )

    ,
        'tasks_modified_user' =>
            array(
                'lhs_module' => 'Users',
                'lhs_table' => 'users',
                'lhs_key' => 'id',
                'rhs_module' => 'Tasks',
                'rhs_table' => 'tasks',
                'rhs_key' => 'modified_user_id',
                'relationship_type' => 'one-to-many'
            )

    ,
        'tasks_created_by' =>
            array(
                'lhs_module' => 'Users',
                'lhs_table' => 'users',
                'lhs_key' => 'id',
                'rhs_module' => 'Tasks',
                'rhs_table' => 'tasks',
                'rhs_key' => 'created_by',
                'relationship_type' => 'one-to-many'
            )
    )
,
    'indices' => array(
        array('name' => 'idx_tsk_name', 'type' => 'index', 'fields' => array('name')),
        array('name' => 'idx_task_con_del', 'type' => 'index', 'fields' => array('contact_id', 'deleted')),
        array(
            'name' => 'idx_task_par_del',
            'type' => 'index',
            'fields' => array('parent_id', 'parent_type', 'deleted')
        ),
        array('name' => 'idx_task_assigned', 'type' => 'index', 'fields' => array('assigned_user_id')),
        array('name' => 'idx_task_status', 'type' => 'index', 'fields' => array('status')),
    )

    //This enables optimistic locking for Saves From EditView
,
    'optimistic_locking' => true,
);
VardefManager::createVardef('Tasks', 'Task', array(
    'default',
    'assignable',
    'security_groups',
));
