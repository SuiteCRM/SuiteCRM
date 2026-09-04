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

$dictionary['Case'] = array(
    'table' => 'cases',
    'audited' => true,
    'unified_search' => true,
    'full_text_search' => true,
    'unified_search_default_enabled' => true,
    'duplicate_merge' => true,
    'comment' => 'Cases are issues or problems that a customer asks a support representative to resolve',
    'fields' => array(
        'suggestion_box' => array(
            'name' => 'suggestion_box',
            'vname' => 'LBL_SUGGESTION_BOX',
            'type' => 'readonly',
            'source' => 'non-db',
        ),
        'description' => array(
            'name' => 'description',
            'vname' => 'LBL_DESCRIPTION',
            'type' => 'text',
            'editor' => 'html',
            'comment' => 'Full text of the description',
            'rows' => 6,
            'cols' => 80,
        ),

        'account_name' => array(
            'name' => 'account_name',
            'rname' => 'name',
            'id_name' => 'account_id',
            'vname' => 'LBL_ACCOUNT_NAME',
            'type' => 'relate',
            'link' => 'accounts',
            'table' => 'accounts',
            'join_name' => 'accounts',
            'isnull' => 'true',
            'module' => 'Accounts',
            'dbType' => 'varchar',
            'len' => 100,
            'source' => 'non-db',
            'unified_search' => true,
            'comment' => 'The name of the account represented by the account_id field',
            'required' => true,
            'importable' => 'required',
        ),
        'account_name1' => array(
            'name' => 'account_name1',
            'source' => 'non-db',
            'type' => 'text',
            'len' => 100,
            'importable' => 'false',
            'studio' => array("formula" => false),
        ),
        'account_id' => array(
            'name' => 'account_id',
            'type' => 'relate',
            'dbType' => 'id',
            'rname' => 'id',
            'module' => 'Accounts',
            'id_name' => 'account_id',
            'reportable' => false,
            'vname' => 'LBL_ACCOUNT_ID',
            'audited' => true,
            'massupdate' => false,
            'comment' => 'The account to which the case is associated'
        ),

        'state' => array(
            'name' => 'state',
            'vname' => 'LBL_STATE',
            'type' => 'enum',
            'options' => 'case_state_dom',
            'len' => 100,
            'audited' => true,
            'comment' => 'The state of the case (i.e. open/closed)',
            'default' => 'Open',
            'parentenum' => 'status',
            'merge_filter' => 'disabled',
        ),
        'status' => array(
            'name' => 'status',
            'vname' => 'LBL_STATUS',
            'type' => 'dynamicenum',
            'options' => 'case_status_dom',
            'len' => 100,
            'audited' => true,
            'comment' => 'The status of the case',
            'dbtype' => 'enum',
            'parentenum' => 'state',
        ),
        'priority' => array(
            'name' => 'priority',
            'vname' => 'LBL_PRIORITY',
            'type' => 'enum',
            'options' => 'case_priority_dom',
            'len' => 100,
            'audited' => true,
            'comment' => 'The priority of the case',

        ),
        'resolution' => array(
            'name' => 'resolution',
            'vname' => 'LBL_RESOLUTION',
            'type' => 'text',
            'comment' => 'The resolution of the case',
            'rows' => 6,
            'cols' => 80,
        ),
        'case_update_form' => array(
            'required' => false,
            'name' => 'case_update_form',
            'vname' => 'LBL_CASE_UPDATE_FORM',
            'type' => 'function',
            'source' => 'non-db',
            'massupdate' => 0,
            'studio' => 'visible',
            'importable' => 'false',
            'duplicate_merge' => 'disabled',
            'duplicate_merge_dom_value' => 0,
            'audited' => false,
            'reportable' => false,
            'function' => array(
                'name' => 'display_update_form',
                'returns' => 'html',
                'include' => 'modules/AOP_Case_Updates/Case_Updates.php',
            ),
        ),
        'contact_created_by' => array(
            'name' => 'contact_created_by',
            'type' => 'link',
            'relationship' => 'cases_created_contact',
            'module' => 'Contacts',
            'bean_name' => 'Contact',
            'link_type' => 'one',
            'source' => 'non-db',
            'vname' => 'LBL_CONTACT_CREATED_BY',
            'side' => 'left',
            'id_name' => 'contact_created_by_id',
        ),
        'contact_created_by_name' => array(
            'name' => 'contact_created_by_name',
            'type' => 'relate',
            'source' => 'non-db',
            'vname' => 'LBL_CONTACT_CREATED_BY_NAME',
            'save' => true,
            'id_name' => 'contact_created_by_id',
            'link' => 'cases_created_contact',
            'table' => 'Contacts',
            'module' => 'Contacts',
            'rname' => 'name',
        ),
        'contact_created_by_id' => array(
            'name' => 'contact_created_by_id',
            'type' => 'id',
            'reportable' => false,
            'vname' => 'LBL_CONTACT_CREATED_BY_ID',
        ),

        'tasks' => array(
            'name' => 'tasks',
            'type' => 'link',
            'relationship' => 'case_tasks',
            'source' => 'non-db',
            'vname' => 'LBL_TASKS',
        ),
        'notes' => array(
            'name' => 'notes',
            'type' => 'link',
            'relationship' => 'case_notes',
            'source' => 'non-db',
            'vname' => 'LBL_NOTES',
        ),
        'meetings' => array(
            'name' => 'meetings',
            'type' => 'link',
            'relationship' => 'case_meetings',
            'bean_name' => 'Meeting',
            'source' => 'non-db',
            'vname' => 'LBL_MEETINGS',
        ),
        'emails' => array(
            'name' => 'emails',
            'type' => 'link',
            'relationship' => 'emails_cases_rel',
            'source' => 'non-db',
            'vname' => 'LBL_EMAILS',
        ),
        'documents' => array(
            'name' => 'documents',
            'type' => 'link',
            'relationship' => 'documents_cases',
            'source' => 'non-db',
            'vname' => 'LBL_DOCUMENTS_SUBPANEL_TITLE',
        ),
        'calls' => array(
            'name' => 'calls',
            'type' => 'link',
            'relationship' => 'case_calls',
            'source' => 'non-db',
            'vname' => 'LBL_CALLS',
        ),
        'bugs' => array(
            'name' => 'bugs',
            'type' => 'link',
            'relationship' => 'cases_bugs',
            'source' => 'non-db',
            'vname' => 'LBL_BUGS',
        ),
        'contacts' => array(
            'name' => 'contacts',
            'type' => 'link',
            'relationship' => 'contacts_cases',
            'source' => 'non-db',
            'vname' => 'LBL_CONTACTS',
        ),
        'accounts' => array(
            'name' => 'accounts',
            'type' => 'link',
            'relationship' => 'account_cases',
            'link_type' => 'one',
            'side' => 'right',
            'source' => 'non-db',
            'vname' => 'LBL_ACCOUNT',
        ),
        'project' => array(
            'name' => 'project',
            'type' => 'link',
            'relationship' => 'projects_cases',
            'source' => 'non-db',
            'vname' => 'LBL_PROJECTS',
        ),
        'update_text' => array(
            'required' => false,
            'name' => 'update_text',
            'vname' => 'LBL_UPDATE_TEXT',
            'source' => 'non-db',
            'type' => 'text',
            'editor' => 'html',
            'massupdate' => '0',
            'default' => '',
            'no_default' => false,
            'comments' => '',
            'help' => '',
            'importable' => 'true',
            'duplicate_merge' => 'disabled',
            'duplicate_merge_dom_value' => '0',
            'audited' => false,
            'reportable' => true,
            'unified_search' => false,
            'merge_filter' => 'disabled',
            'size' => '20',
            'studio' => 'visible',
            'rows' => 6,
            'cols' => 80,
            'id' => 'Casesupdate_text',
        ),
        'internal' => array(
            'name' => 'internal',
            'source' => 'non-db',
            'vname' => 'LBL_INTERNAL',
            'type' => 'bool',
            'studio' => 'visible',
        ),
        'aop_case_updates_threaded' => array(
            'required' => false,
            'name' => 'aop_case_updates_threaded',
            'vname' => 'LBL_AOP_CASE_UPDATES_THREADED',
            'type' => 'function',
            'source' => 'non-db',
            'massupdate' => 0,
            'studio' => 'visible',
            'importable' => 'false',
            'duplicate_merge' => 'disabled',
            'duplicate_merge_dom_value' => 0,
            'audited' => false,
            'reportable' => false,
            'inline_edit' => 0,
            'function' => array(
                'name' => 'display_updates',
                'returns' => 'html',
                'include' => 'modules/AOP_Case_Updates/Case_Updates.php',
            ),
        ),
        'aop_case_updates' => array(
            'name' => 'aop_case_updates',
            'type' => 'link',
            'relationship' => 'cases_aop_case_updates',
            'source' => 'non-db',
            'id_name' => 'case_id',
            'vname' => 'LBL_AOP_CASE_UPDATES',
        ),
        'aop_case_events' => array(
            'name' => 'aop_case_events',
            'type' => 'link',
            'relationship' => 'cases_aop_case_events',
            'source' => 'non-db',
            'id_name' => 'case_id',
            'vname' => 'LBL_AOP_CASE_EVENTS',
        ),
    ),
    'indices' => array(
        array('name' => 'case_number', 'type' => 'index', 'fields' => array('case_number')),
        array('name' => 'idx_case_name', 'type' => 'index', 'fields' => array('name')),
        array('name' => 'idx_account_id', 'type' => 'index', 'fields' => array('account_id')),
        array(
            'name' => 'idx_cases_stat_del',
            'type' => 'index',
            'fields' => array('assigned_user_id', 'status', 'deleted')
        ),
    ),
    'relationships' => array(
        'case_calls' => array(
            'lhs_module' => 'Cases',
            'lhs_table' => 'cases',
            'lhs_key' => 'id',
            'rhs_module' => 'Calls',
            'rhs_table' => 'calls',
            'rhs_key' => 'parent_id',
            'relationship_type' => 'one-to-many',
            'relationship_role_column' => 'parent_type',
            'relationship_role_column_value' => 'Cases'
        ),
        'case_tasks' => array(
            'lhs_module' => 'Cases',
            'lhs_table' => 'cases',
            'lhs_key' => 'id',
            'rhs_module' => 'Tasks',
            'rhs_table' => 'tasks',
            'rhs_key' => 'parent_id',
            'relationship_type' => 'one-to-many',
            'relationship_role_column' => 'parent_type',
            'relationship_role_column_value' => 'Cases'
        ),
        'case_notes' => array(
            'lhs_module' => 'Cases',
            'lhs_table' => 'cases',
            'lhs_key' => 'id',
            'rhs_module' => 'Notes',
            'rhs_table' => 'notes',
            'rhs_key' => 'parent_id',
            'relationship_type' => 'one-to-many',
            'relationship_role_column' => 'parent_type',
            'relationship_role_column_value' => 'Cases'
        ),
        'case_meetings' => array(
            'lhs_module' => 'Cases',
            'lhs_table' => 'cases',
            'lhs_key' => 'id',
            'rhs_module' => 'Meetings',
            'rhs_table' => 'meetings',
            'rhs_key' => 'parent_id',
            'relationship_type' => 'one-to-many',
            'relationship_role_column' => 'parent_type',
            'relationship_role_column_value' => 'Cases'
        ),
        'case_emails' => array(
            'lhs_module' => 'Cases',
            'lhs_table' => 'cases',
            'lhs_key' => 'id',
            'rhs_module' => 'Emails',
            'rhs_table' => 'emails',
            'rhs_key' => 'parent_id',
            'relationship_type' => 'one-to-many',
            'relationship_role_column' => 'parent_type',
            'relationship_role_column_value' => 'Cases'
        ),
        'cases_assigned_user' => array(
            'lhs_module' => 'Users',
            'lhs_table' => 'users',
            'lhs_key' => 'id',
            'rhs_module' => 'Cases',
            'rhs_table' => 'cases',
            'rhs_key' => 'assigned_user_id',
            'relationship_type' => 'one-to-many'
        ),
        'cases_modified_user' => array(
            'lhs_module' => 'Users',
            'lhs_table' => 'users',
            'lhs_key' => 'id',
            'rhs_module' => 'Cases',
            'rhs_table' => 'cases',
            'rhs_key' => 'modified_user_id',
            'relationship_type' => 'one-to-many'
        ),
        'cases_created_by' => array(
            'lhs_module' => 'Users',
            'lhs_table' => 'users',
            'lhs_key' => 'id',
            'rhs_module' => 'Cases',
            'rhs_table' => 'cases',
            'rhs_key' => 'created_by',
            'relationship_type' => 'one-to-many'
        ),

        'cases_created_contact' => array(
            'lhs_module' => 'Contacts',
            'lhs_table' => 'contacts',
            'lhs_key' => 'id',
            'rhs_module' => 'Cases',
            'rhs_table' => 'cases',
            'rhs_key' => 'contact_created_by_id',
            'relationship_type' => 'one-to-many',
        ),
    ),
    'optimistic_locking' => true,
);
VardefManager::createVardef(
    'Cases',
    'Case',
    array(
        'default',
        'assignable',
        'security_groups',
        'issue',
    ),
    'case'
);
