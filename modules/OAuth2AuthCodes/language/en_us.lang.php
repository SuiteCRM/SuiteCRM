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

$mod_strings = [
    'LBL_MODULE_NAME' => 'OAuth2 Authorization Codes',
    'LBL_MODULE_TITLE' => 'OAuth2 Authorization Codes',
    'LBL_MODULE_ID'=> 'OAuth2 Authorization Codes',
    'LBL_CLIENT' => 'Client',
    'LBL_SCOPES' => 'Scopes',
    'LBL_AUTO_AUTHORIZE' => 'Automatically approve',
    'LBL_USER' => 'Authorizing user',
    'LBL_OAUTH_AUTHORIZE_AND_SAVE' => 'Authorize Client',
    'LBL_OAUTH_AUTHORIZE_ONCE' => 'Authorize Client Once',
    'LBL_OAUTH_ABORT' => 'Decline Client Request',
    'LBL_OAUTH_CLIENT' => 'Client:',
    'LBL_AUTH_CODE_EXPIRES' => 'Auth Code Expires',
    'LBL_OAUTH_REDIRECT' => 'Redirect URL:',
    'LBL_OAUTH_INFO' => 'A client requested you to authenticate with SuiteCRM. In order to grant the client access to your SuiteCRM account, please authorize this request. In case you do not know or do not trust the client, you should not grant access.',
    'LBL_OAUTH_BACK_TO_HOME' => '<< Click here to go back to the dashboard.',
    'LBL_INVALID_REQUEST' => 'Invalid request',
    'LBL_DELETE_BUTTON_LABEL' => 'Revoke and delete',
    'LBL_DELETE_CONFIRMATION' => 'Are you sure you want to delete this record? The client will not be granted an authorization code any more.',

    'LNK_OAUTH2_TOKEN_LIST'=> 'List OAuth2 Tokens',
    'LNK_OAUTH2_CLIENT_LIST' => 'List OAuth2 Clients',
    'LNK_OAUTH2_AUTHCODE_LIST' => 'List OAuth2 Authorization Codes',

    'LNK_NEW_OAUTH2_CLIENT' => 'Create OAuth2 Client',
    'LNK_NEW_OAUTH2_PASSWORD_CLIENT' => 'New Password Client',
    'LNK_NEW_OAUTH2_CREDENTIALS_CLIENT' => 'New Client Credentials Client',
    'LNK_NEW_OAUTH2_IMPLICIT_CLIENT' => 'New Implicit Client',
    'LNK_NEW_OAUTH2_AUTHORIZATION_CLIENT' => 'New Authorization Client',
];
