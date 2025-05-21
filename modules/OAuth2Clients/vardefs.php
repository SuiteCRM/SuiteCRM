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

$dictionary['OAuth2Clients'] = [
    'table' => 'oauth2clients',
    'audited' => false,
    'comment' => 'Provides tokens for security services',
    'fields' => [
        'id' => [
            'name' => 'id',
            'vname' => 'LBL_ID',
            'type' => 'id',
            'required' => true,
            'reportable' => false,
            'inline_edit' => false,
        ],
        'name' => [
            'name' => 'name',
            'vname' => 'LBL_NAME',
            'type' => 'varchar',
            'required' => true,
            'reportable' => false,
            'inline_edit' => false,
            'duplicate_merge' => 'disabled',
        ],
        'secret' => [
            'name' => 'secret',
            'vname' => 'LBL_SECRET',
            'type' => 'varchar',
            'required' => true,
            'reportable' => false,
            'api-visible' => false,
            'inline_edit' => false,
            'len' => '4000',
        ],
        'redirect_url' => [
            'name' => 'redirect_url',
            'vname' => 'LBL_REDIRECT_URL',
            'type' => 'varchar',
            'required' => false,
            'reportable' => false,
            'inline_edit' => false,

        ],
        'is_confidential' => [
            'name' => 'is_confidential',
            'vname' => 'LBL_IS_CONFIDENTIAL',
            'type' => 'bool',
            'default' => true,
            'reportable' => false,
            'api-visible' => false,
            'inline_edit' => false,
        ],
        'allowed_grant_type' => [
            'name' => 'allowed_grant_type',
            'vname' => 'LBL_ALLOWED_GRANT_TYPE',
            'type' => 'enum',
            'options' => 'oauth2_grant_type_dom',
            'default' => 'password',
            'required' => true,
            'reportable' => false,
            'api-visible' => false,
            'inline_edit' => false,
        ],
        'duration_value' => [
            'name' => 'duration_value',
            'vname' => 'LBL_DURATION_VALUE',
            'type' => 'int',
            'len' => 11,
            'required' => true, 
            'reportable' => false,
            'api-visible' => false,
            'inline_edit' => false,
        ],
        'duration_amount' => [
            'name' => 'duration_amount',
            'vname' => 'LBL_DURATION_AMOUNT',
            'type' => 'int',
            'len' => 11,
            'required' => true,
            'reportable' => false,
            'api-visible' => false,
            'inline_edit' => false,
        ],
        'duration_unit' => [
            'name' => 'duration_unit',
            'vname' => 'LBL_DURATION_UNIT',
            'type' => 'enum',
            'options' => 'oauth2_duration_units',
            'default' => 'Duration Unit',
            'required' => true,
            'reportable' => false,
            'api-visible' => false,
            'inline_edit' => false,
        ],
        'oauth2tokens' => [
            'name' => 'oauth2tokens',
            'vname' => 'LBL_RELATED_OAUTH2TOKENS',
            'type' => 'link',
            'relationship' => 'oauth2clients_oauth2tokens',
            'module' => 'OAuth2Tokens',
            'bean_name' => 'OAuth2Tokens',
            'source' => 'non-db',
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
            'duplicate_merge' => 'disabled'
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
            'required' => true,
        ],
        'assigned_user_link' => [
            'name' => 'assigned_user_link',
            'type' => 'link',
            'relationship' => 'oauth2clients_assigned_user',
            'vname' => 'LBL_USER',
            'link_type' => 'one',
            'module' => 'Users',
            'bean_name' => 'User',
            'source' => 'non-db',
            'duplicate_merge' => 'enabled',
            'rname' => 'user_name',
            'id_name' => 'assigned_user_id',
            'table' => 'users',
        ],
    ],
    'optimistic_locking' => true,
    'relationships' => [
        'oauth2clients_oauth2tokens' => [
            'rhs_module' => 'OAuth2Tokens',
            'rhs_table' => 'oauth2tokens',
            'rhs_key' => 'client',
            'lhs_module' => 'OAuth2Clients',
            'lhs_table' => 'oauth2clients',
            'lhs_key' => 'id',
            'relationship_type' => 'one-to-many',
        ],
        'oauth2clients_assigned_user' => [
            'lhs_module' => 'Users',
            'lhs_table' => 'users',
            'lhs_key' => 'id',
            'rhs_module' => 'OAuth2Clients',
            'rhs_table' => 'oauth2clients',
            'rhs_key' => 'assigned_user_id',
            'relationship_type' => 'one-to-many'
        ]
    ],
];
if (!class_exists('VardefManager')) {
    require_once('include/SugarObjects/VardefManager.php');
}

VardefManager::createVardef(
    'OAuth2Clients',
    'OAuth2Clients',
    [
        'default',
    ]
);
