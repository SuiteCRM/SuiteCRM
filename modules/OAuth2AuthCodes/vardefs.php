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

$dictionary['OAuth2AuthCodes'] = [
    'table' => 'oauth2authcodes',
    'audited' => false,
    'comment' => 'Provides authorization codes for security services',
    'fields' => [
        'id' => [
            'name' => 'id',
            'vname' => 'LBL_ID',
            'type' => 'id',
            'required' => true,
            'reportable' => false,
            'api-visible' => false,
            'inline_edit' => false,
        ],
        'auth_code_is_revoked' => [
            'name' => 'auth_code_is_revoked',
            'vname' => 'LBL_AUTH_CODE_IS_REVOKED',
            'type' => 'bool',
            'required' => true,
            'reportable' => false,
            'api-visible' => false,
        ],
        'auto_authorize' => [
            'name' => 'auto_authorize',
            'vname' => 'LBL_AUTO_AUTHORIZE',
            'type' => 'bool',
            'required' => true,
            'reportable' => false,
            'api-visible' => false,
        ],
        'auth_code_expires' => [
            'name' => 'auth_code_expires',
            'vname' => 'LBL_AUTH_CODE_EXPIRES',
            'type' => 'datetime',
            'required' => true,
            'reportable' => false,
            'api-visible' => false,
            'inline_edit' => false,
        ],
        'auth_code' => [
            'name' => 'auth_code',
            'vname' => 'LBL_AUTH_CODE',
            'type' => 'varchar',
            'required' => true,
            'reportable' => false,
            'api-visible' => false,
            'len' => '4000',
            'inline_edit' => false,
        ],
        'oauth2client_name' => [
            'required' => false,
            'name' => 'oauth2client_name',
            'rname' => 'name',
            'id_name' => 'client',
            'vname' => 'LBL_CLIENT',
            'type' => 'relate',
            'link' => 'oauth2clients',
            'table' => 'oauth2clients',
            'isnull' => 'true',
            'module' => 'OAuth2Clients',
            'dbType' => 'varchar',
            'len' => 255,
            'source' => 'non-db',
            'unified_search' => true,
            'quicksearch' => 'enabled',
            'reportable' => true,
            'inline_edit' => false,
        ],
        'oauth2clients' => [
            'name' => 'oauth2clients',
            'vname' => 'LBL_CLIENT',
            'type' => 'link',
            'relationship' => 'oauth2clients_oauth2authcodes',
            'module' => 'OAuth2Clients',
            'bean_name' => 'OAuth2Clients',
            'source' => 'non-db',
            'inline_edit' => false,
        ],
        'client' => [
            'name' => 'client',
            'type' => 'relate',
            'dbType' => 'id',
            'rname' => 'id',
            'module' => 'OAuth2Clients',
            'id_name' => 'client',
            'reportable' => false,
            'vname' => 'LBL_CLIENT_ID',
            'massupdate' => false,
            'inline_edit' => false,
        ],
        'assigned_user_id' => [
            'name' => 'assigned_user_id',
            'rname' => 'user_name',
            'id_name' => 'assigned_user_id',
            'vname' => 'LBL_USER',
            'group' => 'assigned_user_name',
            'type' => 'relate',
            'table' => 'users',
            'module' => 'Users',
            'reportable' => true,
            'isnull' => 'false',
            'dbType' => 'id',
            'audited' => true,
            'comment' => 'User ID assigned to record',
            'duplicate_merge' => 'disabled',
            'inline_edit' => false,
        ],
        'assigned_user_name' => [
            'name' => 'assigned_user_name',
            'link' => 'assigned_user_link',
            'vname' => 'LBL_USER',
            'rname' => 'user_name',
            'type' => 'relate',
            'reportable' => false,
            'source' => 'non-db',
            'table' => 'users',
            'id_name' => 'assigned_user_id',
            'module' => 'Users',
            'duplicate_merge' => 'disabled',
            'inline_edit' => false,
        ],
        'assigned_user_link' => [
            'name' => 'assigned_user_link',
            'type' => 'link',
            'relationship' => 'oauth2authcodes_assigned_user',
            'vname' => 'LBL_USER',
            'link_type' => 'one',
            'module' => 'Users',
            'bean_name' => 'User',
            'source' => 'non-db',
            'duplicate_merge' => 'enabled',
            'rname' => 'user_name',
            'id_name' => 'assigned_user_id',
            'table' => 'users',
            'inline_edit' => false,
        ],
    ],
    'optimistic_locking' => true,
    'relationships' => [
        'oauth2clients_oauth2authcodes' => [
            'lhs_module' => 'OAuth2Clients',
            'lhs_table' => 'oauth2clients',
            'lhs_key' => 'id',
            'rhs_module' => 'Oauth2AuthCodes',
            'rhs_table' => 'oauth2authcodes',
            'rhs_key' => 'client',
            'relationship_type' => 'one-to-many',
        ],
        'oauth2authcodes_assigned_user' => [
            'lhs_module' => 'Users',
            'lhs_table' => 'users',
            'lhs_key' => 'id',
            'rhs_module' => 'OAuth2AuthCodes',
            'rhs_table' => 'oauth2authcodes',
            'rhs_key' => 'assigned_user_id',
            'relationship_type' => 'one-to-many'
        ]
    ],
];
if (!class_exists('VardefManager')) {
    require_once('include/SugarObjects/VardefManager.php');
}

VardefManager::createVardef(
    'OAuth2AuthCodes',
    'OAuth2AuthCodes',
    [
        'default',

    ]
);
