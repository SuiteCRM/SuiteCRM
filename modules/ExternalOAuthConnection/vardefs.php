<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2022 SalesAgility Ltd.
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

$dictionary['ExternalOAuthConnection'] = [
    'table' => 'external_oauth_connections',
    'comment' => 'External OAuth Connection',
    'audited' => false,
    'inline_edit' => false,
    'massupdate' => false,
    'exportable' => false,
    'importable' => false,
    'fields' => [
        'id' => [
            'name' => 'id',
            'vname' => 'LBL_ID',
            'type' => 'id',
            'required' => true,
            'comment' => 'Unique identifier',
            'reportable' => false,
            'massupdate' => false,
            'inline_edit' => false,
            'importable' => false,
            'exportable' => false,
            'unified_search' => false,
        ],
        'name' => array(
            'name' => 'name',
            'vname' => 'LBL_NAME',
            'type' => 'name',
            'link' => true,
            'dbType' => 'varchar',
            'len' => 255,
            'required' => true,
            'duplicate_merge' => 'disabled',
            'reportable' => false,
            'massupdate' => false,
            'inline_edit' => false,
            'importable' => false,
            'exportable' => false,
            'unified_search' => false,
        ),
        'type' => [
            'name' => 'type',
            'vname' => 'LBL_TYPE',
            'type' => 'enum',
            'options' => 'dom_external_oauth_connection_types',
            'display' => 'readonly',
            'inline_edit' => false,
            'reportable' => false,
            'massupdate' => false,
            'importable' => false,
            'exportable' => false,
            'unified_search' => false,
        ],
        'client_id' => [
            'name' => 'client_id',
            'vname' => 'LBL_CLIENT_ID',
            'type' => 'varchar',
            'len' => 32,
            'required' => false,
            'reportable' => false,
            'massupdate' => false,
            'inline_edit' => false,
            'importable' => false,
            'exportable' => false,
            'unified_search' => false,
        ],
        'client_secret' => [
            'name' => 'client_secret',
            'vname' => 'LBL_CLIENT_SECRET',
            'type' => 'varchar',
            'display' => 'writeonly',
            'len' => 32,
            'required' => false,
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
        'token_type' => [
            'name' => 'token_type',
            'vname' => 'LBL_TOKEN_TYPE',
            'type' => 'varchar',
            'len' => 32,
            'required' => false,
            'reportable' => false,
            'massupdate' => false,
            'inline_edit' => false,
            'importable' => false,
            'exportable' => false,
            'unified_search' => false,
        ],
        'expires_in' => [
            'name' => 'expires_in',
            'vname' => 'LBL_EXPIRES_IN',
            'type' => 'varchar',
            'display' => 'writeonly',
            'len' => 32,
            'required' => false,
            'reportable' => false,
            'massupdate' => false,
            'inline_edit' => false,
            'importable' => false,
            'exportable' => false,
            'unified_search' => false,
        ],
        'access_token' => [
            'name' => 'access_token',
            'vname' => 'LBL_ACCESS_TOKEN',
            'type' => 'text',
            'display' => 'writeonly',
            'required' => false,
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
        'refresh_token' => [
            'name' => 'refresh_token',
            'vname' => 'LBL_REFRESH_TOKEN',
            'type' => 'text',
            'display' => 'writeonly',
            'required' => false,
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
        'external_oauth_provider' => [
            'name' => 'external_oauth_provider',
            'type' => 'link',
            'relationship' => 'external_oauth_connections_external_oauth_providers',
            'link_type' => 'one',
            'source' => 'non-db',
            'vname' => 'LBL_EXTERNAL_OAUTH_PROVIDER',
            'duplicate_merge' => 'disabled',
            'reportable' => false,
            'massupdate' => false,
            'inline_edit' => false,
            'importable' => false,
            'exportable' => false,
            'unified_search' => false,
        ],
        'external_oauth_provider_id' => [
            'name' => 'external_oauth_provider_id',
            'rname' => 'id',
            'id_name' => 'external_oauth_provider_id',
            'vname' => 'LBL_EXTERNAL_OAUTH_PROVIDER_ID',
            'type' => 'relate',
            'table' => 'external_oauth_providers',
            'isnull' => 'true',
            'module' => 'ExternalOAuthProvider',
            'dbType' => 'id',
            'duplicate_merge' => 'disabled',
            'hideacl' => true,
            'reportable' => false,
            'massupdate' => false,
            'inline_edit' => false,
            'importable' => false,
            'exportable' => false,
            'unified_search' => false,
        ],
        'external_oauth_provider_name' => [
            'name' => 'external_oauth_provider_name',
            'rname' => 'name',
            'id_name' => 'external_oauth_provider_id',
            'vname' => 'LBL_EXTERNAL_OAUTH_PROVIDER_NAME',
            'join_name' => 'external_oauth_providers',
            'type' => 'relate',
            'link' => 'external_oauth_provider',
            'table' => 'external_oauth_providers',
            'isnull' => 'true',
            'module' => 'ExternalOAuthProvider',
            'dbType' => 'varchar',
            'len' => '255',
            'source' => 'non-db',
            'reportable' => false,
            'massupdate' => false,
            'inline_edit' => false,
            'importable' => false,
            'exportable' => false,
            'unified_search' => false,
        ],
    ],
    'relationships' => [
        'external_oauth_connections_external_oauth_providers' => [
            'lhs_module' => 'ExternalOAuthProvider',
            'lhs_table' => 'external_oauth_providers',
            'lhs_key' => 'id',
            'rhs_module' => 'ExternalOAuthConnection',
            'rhs_table' => 'external_oauth_connections',
            'rhs_key' => 'external_oauth_provider_id',
            'relationship_type' => 'one-to-many'
        ],
    ]
];

VardefManager::createVardef('ExternalOAuthConnection', 'ExternalOAuthConnection', ['basic', 'security_groups']);
