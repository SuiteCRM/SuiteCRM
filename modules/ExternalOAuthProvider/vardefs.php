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

$dictionary['ExternalOAuthProvider'] = [
    'table' => 'external_oauth_providers',
    'comment' => 'External OAuth Provider',
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
        'name' => [
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
        ],
        'type' => [
            'name' => 'type',
            'vname' => 'LBL_TYPE',
            'type' => 'enum',
            'options' => 'dom_external_oauth_provider_types',
            'display' => 'readonly',
            'inline_edit' => false,
            'reportable' => false,
            'massupdate' => false,
            'importable' => false,
            'exportable' => false,
            'unified_search' => false,
        ],
        'connector' => [
            'name' => 'connector',
            'vname' => 'LBL_CONNECTOR',
            'function' => [
                'name' => 'getExternalOAuthProviderConnectors',
                'returns' => 'html',
                'include' => 'modules/ExternalOAuthProvider/utils.php',
            ],
            'type' => 'varchar',
            'required' => true,
            'reportable' => false,
            'massupdate' => false,
            'inline_edit' => false,
            'importable' => false,
            'exportable' => false,
            'unified_search' => false,
        ],
        'redirect_uri' => [
            'name' => 'redirect_uri',
            'vname' => 'LBL_REDIRECT_URI',
            'function' => [
                'name' => 'getExternalOAuthProviderRedirectURI',
                'returns' => 'html',
                'include' => 'modules/ExternalOAuthProvider/utils.php',
            ],
            'type' => 'varchar',
            'source' => 'non-db',
            'required' => false,
            'reportable' => false,
            'massupdate' => false,
            'inline_edit' => false,
            'importable' => false,
            'exportable' => false,
            'unified_search' => false,
        ],
        'client_id' => [
            'name' => 'client_id',
            'vname' => 'LBL_CLIENT_ID',
            'type' => 'varchar',
            'required' => true,
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
        'scope' => [
            'name' => 'scope',
            'vname' => 'LBL_SCOPE',
            'type' => 'stringmap',
            'dbType' => 'text',
            'required' => false,
            'reportable' => false,
            'massupdate' => false,
            'inline_edit' => false,
            'importable' => false,
            'exportable' => false,
            'unified_search' => false,
        ],
        'url_authorize' => [
            'name' => 'url_authorize',
            'vname' => 'LBL_URL_AUTHORIZE',
            'type' => 'varchar',
            'required' => false,
            'reportable' => false,
            'massupdate' => false,
            'inline_edit' => false,
            'importable' => false,
            'exportable' => false,
            'unified_search' => false,
        ],
        'authorize_url_options' => [
            'name' => 'authorize_url_options',
            'vname' => 'LBL_AUTHORIZE_URL_OPTIONS',
            'type' => 'stringmap',
            'dbType' => 'text',
            'show_keys' => true,
            'required' => false,
            'reportable' => false,
            'massupdate' => false,
            'inline_edit' => false,
            'importable' => false,
            'exportable' => false,
            'unified_search' => false,
        ],
        'url_access_token' => [
            'name' => 'url_access_token',
            'vname' => 'LBL_URL_ACCESS_TOKEN',
            'type' => 'varchar',
            'required' => false,
            'reportable' => false,
            'massupdate' => false,
            'inline_edit' => false,
            'importable' => false,
            'exportable' => false,
            'unified_search' => false,
        ],
        'extra_provider_params' => [
            'name' => 'extra_provider_params',
            'vname' => 'LBL_EXTRA_PROVIDER_PARAMS',
            'type' => 'stringmap',
            'dbType' => 'text',
            'show_keys' => true,
            'required' => false,
            'reportable' => false,
            'massupdate' => false,
            'inline_edit' => false,
            'importable' => false,
            'exportable' => false,
            'unified_search' => false,
        ],
        'get_token_request_grant' => [
            'name' => 'get_token_request_grant',
            'vname' => 'LBL_GET_TOKEN_REQUEST_GRANT',
            'type' => 'varchar',
            'default' => 'authorization_code',
            'required' => false,
            'reportable' => false,
            'massupdate' => false,
            'inline_edit' => false,
            'importable' => false,
            'exportable' => false,
            'unified_search' => false,
        ],
        'get_token_request_options' => [
            'name' => 'get_token_request_options',
            'vname' => 'LBL_GET_TOKEN_REQUEST_OPTIONS',
            'type' => 'stringmap',
            'dbType' => 'text',
            'show_keys' => true,
            'required' => false,
            'reportable' => false,
            'massupdate' => false,
            'inline_edit' => false,
            'importable' => false,
            'exportable' => false,
            'unified_search' => false,
        ],
        'refresh_token_request_grant' => [
            'name' => 'refresh_token_request_grant',
            'vname' => 'LBL_REFRESH_TOKEN_REQUEST_GRANT',
            'type' => 'varchar',
            'default' => 'refresh_token',
            'required' => false,
            'reportable' => false,
            'massupdate' => false,
            'inline_edit' => false,
            'importable' => false,
            'exportable' => false,
            'unified_search' => false,
        ],
        'refresh_token_request_options' => [
            'name' => 'refresh_token_request_options',
            'vname' => 'LBL_REFRESH_TOKEN_REQUEST_OPTIONS',
            'type' => 'stringmap',
            'dbType' => 'text',
            'show_keys' => true,
            'required' => false,
            'reportable' => false,
            'massupdate' => false,
            'inline_edit' => false,
            'importable' => false,
            'exportable' => false,
            'unified_search' => false,
        ],
        'access_token_mapping' => [
            'name' => 'access_token_mapping',
            'vname' => 'LBL_ACCESS_TOKEN_MAPPING',
            'type' => 'varchar',
            'default' => 'access_token',
            'required' => false,
            'reportable' => false,
            'massupdate' => false,
            'inline_edit' => false,
            'importable' => false,
            'exportable' => false,
            'unified_search' => false,
        ],
        'expires_in_mapping' => [
            'name' => 'expires_in_mapping',
            'vname' => 'LBL_EXPIRES_IN_MAPPING',
            'type' => 'varchar',
            'default' => 'expires_in',
            'required' => false,
            'reportable' => false,
            'massupdate' => false,
            'inline_edit' => false,
            'importable' => false,
            'exportable' => false,
            'unified_search' => false,
        ],
        'refresh_token_mapping' => [
            'name' => 'refresh_token_mapping',
            'vname' => 'LBL_REFRESH_TOKEN_MAPPING',
            'type' => 'varchar',
            'default' => 'refresh_token',
            'required' => false,
            'reportable' => false,
            'massupdate' => false,
            'inline_edit' => false,
            'importable' => false,
            'exportable' => false,
            'unified_search' => false,
        ],
        'token_type_mapping' => [
            'name' => 'token_type_mapping',
            'vname' => 'LBL_TOKEN_TYPE_MAPPING',
            'type' => 'varchar',
            'required' => false,
            'reportable' => false,
            'massupdate' => false,
            'inline_edit' => false,
            'importable' => false,
            'exportable' => false,
            'unified_search' => false,
        ],
    ],
    'relationships' => [
    ]
];

VardefManager::createVardef('ExternalOAuthProvider', 'ExternalOAuthProvider', ['basic', 'security_groups']);
