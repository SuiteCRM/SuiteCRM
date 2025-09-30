<?php
/**
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation.
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */
$mod_strings['LBL_CONNECTOR_DEFAULT_CONFIGURED_OPTIONS'] = 'Default settings';
$mod_strings['LBL_CONNECTOR_HELP'] = 'To facilitate the creation of the external OAuth provider record, some options are set by default when selecting a connector, so it is not necessary to specify them explicitly. The default settings are displayed in the area below. Please note that the generic connector does not configure any default settings. For more information, see the <a href="https://wiki.sinergiatic.org/index.php?title=Configuraci%C3%B3n_de_correo#Proveedores_OAuth_externos" target="_blank">External OAuth Providers</a> section of the SinergiaCRM wiki.';
$mod_strings['LBL_CLIENT_ID_HELP'] = 'This field must specify the client ID generated in the application created in the external email provider.';    
$mod_strings['LBL_CLIENT_SECRET_HELP'] = 'This field must specify the value of the secret generated in the application created in the external email provider. It is important not to confuse the secret value with the secret ID.';
$mod_strings['LBL_SCOPE_HELP'] = 'This field allows to specify the permissions granted to the application created in the external email provider.';
$mod_strings['LBL_AUTHORIZE_URL_HELP'] = 'This field must specify the URL where SinergiaCRM will connect to perform the authentication and authorization process.';
$mod_strings['LBL_AUTHORIZE_URL_OPTIONS_HELP'] = 'This field allows you to specify parameters that will be added to the authorization URL and that serve to customize the user experience during the authentication and authorization process with the email provider. For example: "access_type=offline" requests a refresh token for offline access, "prompt=consent" forces the provider to show the permissions screen even if they have already been granted, etc.';
$mod_strings['LBL_URL_ACCESS_TOKEN_HELP'] = 'This field must specify the URL to which SinergiaCRM will send the authorization code obtained with the authorization process to obtain the access and refresh tokens.';
$mod_strings['LBL_REDIRECT_URI_HELP'] = 'This field displays the redirect URL that must be specified in the Redirect URI field of the application created in the external email provider. This is the SinergiaCRM URL to which the external provider will redirect the user after completing the authentication and authorization process.';
$mod_strings['LBL_EXTRA_PROVIDER_PARAMS_HELP'] = 'This field allows you to specify additional parameters (URL or data) provider-specific to achieve certain actions. For example "urlResourceOwnerDetails=><url_information_user>" defines the URL where SinergiaCRM can connect to obtain user information once authenticated.';
