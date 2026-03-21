<?php
/**
 * SuiteCRM is a customer relationship management program developed by SuiteCRM Ltd.
 * Copyright (C) 2025 SuiteCRM Ltd.
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

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

$dictionary['CalendarAccount'] = [
    'table' => 'calendar_accounts',
    'comment' => 'Calendar Account',
    'audited' => true,
    'inline_edit' => false,
    'duplicate_merge' => true,
    'massupdate' => false,
    'exportable' => false,
    'importable' => false,
    'fields' => [
        'source' => [
            'name' => 'source',
            'vname' => 'LBL_SOURCE',
            'type' => 'enum',
            'options' => 'calendar_source_types',
            'len' => 50,
            'required' => true,
            'reportable' => true,
            'massupdate' => false,
            'inline_edit' => false,
            'importable' => false,
            'exportable' => false,
            'unified_search' => false,
        ],
        'type' => [
            'name' => 'type',
            'vname' => 'LBL_TYPE',
            'type' => 'enum',
            'options' => 'calendar_account_types',
            'default' => 'personal',
            'display' => 'readonly',
            'inline_edit' => false,
            'reportable' => false,
            'massupdate' => false,
            'importable' => false,
            'exportable' => false,
            'unified_search' => false,
        ],

        // OAuth2 Fields
        'oauth_connection_id' => [
            'name' => 'oauth_connection_id',
            'vname' => 'LBL_OAUTH_CONNECTION',
            'type' => 'relate',
            'module' => 'ExternalOAuthConnection',
            'rname' => 'name',
            'id_name' => 'oauth_connection_id',
            'table' => 'external_oauth_connections',
            'dbType' => 'id',
            'required' => false,
            'reportable' => false,
            'massupdate' => false,
            'inline_edit' => false,
            'importable' => false,
            'exportable' => false,
            'unified_search' => false,
        ],
        'oauth_connection_name' => [
            'name' => 'oauth_connection_name',
            'vname' => 'LBL_OAUTH_CONNECTION',
            'type' => 'relate',
            'source' => 'non-db',
            'rname' => 'name',
            'id_name' => 'oauth_connection_id',
            'table' => 'external_oauth_connections',
            'module' => 'ExternalOAuthConnection',
            'dbType' => 'varchar',
            'len' => 255,
            'required' => false,
            'reportable' => false,
            'massupdate' => false,
            'inline_edit' => false,
            'importable' => false,
            'exportable' => false,
            'unified_search' => false,
        ],

        // Basic Auth Fields
        'username' => [
            'name' => 'username',
            'vname' => 'LBL_USERNAME',
            'type' => 'varchar',
            'len' => 255,
            'required' => false,
            'reportable' => false,
            'massupdate' => false,
            'inline_edit' => false,
            'importable' => false,
            'exportable' => false,
            'unified_search' => false,
        ],
        'password' => [
            'name' => 'password',
            'vname' => 'LBL_PASSWORD',
            'type' => 'password',
            'dbType' => 'varchar',
            'len' => 255,
            'required' => false,
            'display' => 'writeonly',
            'reportable' => false,
            'massupdate' => false,
            'inline_edit' => false,
            'importable' => false,
            'exportable' => false,
            'unified_search' => false,
            'sensitive' => true,
            'api-visible' => false,
            'db_encrypted' => true,
        ],
        'server_url' => [
            'name' => 'server_url',
            'vname' => 'LBL_SERVER_URL',
            'type' => 'url',
            'len' => 500,
            'required' => false,
            'reportable' => false,
            'massupdate' => false,
            'inline_edit' => false,
            'importable' => false,
            'exportable' => false,
            'unified_search' => false,
        ],

        // API Key Fields
        'api_key' => [
            'name' => 'api_key',
            'vname' => 'LBL_API_KEY',
            'type' => 'password',
            'dbType' => 'varchar',
            'len' => 255,
            'required' => false,
            'display' => 'writeonly',
            'reportable' => false,
            'massupdate' => false,
            'inline_edit' => false,
            'importable' => false,
            'exportable' => false,
            'unified_search' => false,
            'sensitive' => true,
            'api-visible' => false,
            'db_encrypted' => true,
        ],
        'api_endpoint' => [
            'name' => 'api_endpoint',
            'vname' => 'LBL_API_ENDPOINT',
            'type' => 'url',
            'len' => 500,
            'required' => false,
            'reportable' => false,
            'massupdate' => false,
            'inline_edit' => false,
            'importable' => false,
            'exportable' => false,
            'unified_search' => false,
        ],

        // Calendar User Fields
        'calendar_user_id' => [
            'name' => 'calendar_user_id',
            'rname' => 'user_name',
            'id_name' => 'calendar_user_id',
            'vname' => 'LBL_CALENDAR_USER_ID',
            'group' => 'calendar_user_name',
            'type' => 'relate',
            'table' => 'users',
            'module' => 'Users',
            'reportable' => true,
            'isnull' => 'false',
            'dbType' => 'id',
            'audited' => true,
            'comment' => 'User whose calendar will be synced',
            'duplicate_merge' => 'disabled',
            'required' => true,
            'massupdate' => true,
            'importable' => true,
            'exportable' => true,
        ],
        'calendar_user_name' => [
            'name' => 'calendar_user_name',
            'link' => 'calendar_user_link',
            'vname' => 'LBL_CALENDAR_USER_NAME',
            'rname' => 'user_name',
            'type' => 'relate',
            'reportable' => false,
            'source' => 'non-db',
            'table' => 'users',
            'id_name' => 'calendar_user_id',
            'module' => 'Users',
            'duplicate_merge' => 'disabled',
            'massupdate' => false,
            'importable' => false,
            'exportable' => true,
        ],
        'calendar_user_link' => [
            'name' => 'calendar_user_link',
            'type' => 'link',
            'relationship' => 'calendar_accounts_calendar_user',
            'vname' => 'LBL_CALENDAR_USER',
            'link_type' => 'one',
            'module' => 'Users',
            'bean_name' => 'User',
            'source' => 'non-db',
            'duplicate_merge' => 'enabled',
            'rname' => 'user_name',
            'id_name' => 'calendar_user_id',
            'table' => 'users',
        ],

        // Connection Status Field
        'last_connection_status' => [
            'name' => 'last_connection_status',
            'vname' => 'LBL_LAST_CONNECTION_STATUS',
            'type' => 'bool',
            'default' => '0',
            'required' => false,
            'display' => 'readonly',
            'reportable' => true,
            'massupdate' => false,
            'inline_edit' => false,
            'importable' => false,
            'exportable' => true,
            'unified_search' => false,
        ],
        'last_connection_test' => [
            'name' => 'last_connection_test',
            'vname' => 'LBL_LAST_CONNECTION_TEST',
            'type' => 'datetimecombo',
            'required' => false,
            'display' => 'readonly',
            'reportable' => true,
            'massupdate' => false,
            'inline_edit' => false,
            'importable' => false,
            'exportable' => true,
            'unified_search' => false,
        ],
        'last_sync_attempt_date' => [
            'name' => 'last_sync_attempt_date',
            'vname' => 'LBL_LAST_SYNC_ATTEMPT_DATE',
            'type' => 'datetimecombo',
            'required' => false,
            'display' => 'readonly',
            'reportable' => true,
            'massupdate' => false,
            'inline_edit' => false,
            'importable' => false,
            'exportable' => true,
            'unified_search' => false,
        ],
        'last_sync_attempt_status' => [
            'name' => 'last_sync_attempt_status',
            'vname' => 'LBL_LAST_SYNC_ATTEMPT_STATUS',
            'type' => 'enum',
            'options' => 'sync_attempt_status_list',
            'len' => 20,
            'required' => false,
            'display' => 'readonly',
            'reportable' => true,
            'massupdate' => false,
            'inline_edit' => false,
            'importable' => false,
            'exportable' => true,
            'unified_search' => false,
        ],
        'last_sync_attempt_message' => [
            'name' => 'last_sync_attempt_message',
            'vname' => 'LBL_LAST_SYNC_ATTEMPT_MESSAGE',
            'type' => 'enum',
            'options' => 'sync_attempt_message_list',
            'len' => 50,
            'required' => false,
            'display' => 'readonly',
            'reportable' => false,
            'massupdate' => false,
            'inline_edit' => false,
            'importable' => false,
            'exportable' => true,
            'unified_search' => false,
        ],
        'last_sync_date' => [
            'name' => 'last_sync_date',
            'vname' => 'LBL_LAST_SYNC_DATE',
            'type' => 'datetimecombo',
            'required' => false,
            'display' => 'readonly',
            'reportable' => true,
            'massupdate' => false,
            'inline_edit' => false,
            'importable' => false,
            'exportable' => true,
            'unified_search' => false,
        ],
        'external_calendar_id' => [
            'name' => 'external_calendar_id',
            'vname' => 'LBL_EXTERNAL_CALENDAR_ID',
            'type' => 'varchar',
            'len' => 255,
            'required' => false,
            'reportable' => true,
            'massupdate' => false,
            'inline_edit' => false,
            'importable' => false,
            'exportable' => true,
            'unified_search' => false,
        ],
        'calendar_account_meetings' => [
            'name' => 'calendar_account_meetings',
            'type' => 'link',
            'relationship' => 'calendar_account_meetings',
            'source' => 'non-db',
            'module' => 'Meetings',
            'vname' => 'LBL_MEETINGS',
        ],
    ],
    'indices' => [
        [
            'name' => 'idx_cal_acct_user_type_status',
            'type' => 'index',
            'fields' => ['calendar_user_id', 'type', 'last_connection_status', 'deleted'],
        ],
        [
            'name' => 'idx_cal_acct_external_cal_id',
            'type' => 'index',
            'fields' => ['external_calendar_id'],
        ],
    ],
    'relationships' => [
        'calendar_accounts_calendar_user' => [
            'lhs_module' => 'Users',
            'lhs_table' => 'users',
            'lhs_key' => 'id',
            'rhs_module' => 'CalendarAccount',
            'rhs_table' => 'calendar_accounts',
            'rhs_key' => 'calendar_user_id',
            'relationship_type' => 'one-to-many'
        ]
    ],
    'optimistic_locking' => true,
    'unified_search' => true,
];

if (!class_exists('VardefManager')) {
    require_once('include/SugarObjects/VardefManager.php');
}
VardefManager::createVardef('CalendarAccount', 'CalendarAccount', ['basic', 'assignable', 'security_groups']);