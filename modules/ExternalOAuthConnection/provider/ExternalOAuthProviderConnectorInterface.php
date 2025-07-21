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

use League\OAuth2\Client\Provider\AbstractProvider;
use League\OAuth2\Client\Token\AccessTokenInterface;

interface ExternalOAuthProviderConnectorInterface
{
    /**
     * Get the provider id
     * @return string
     */
    public function getProviderID(): string;

    /**
     * Get the provider type
     * @return string
     */
    public function getProviderType(): string;

    /**
     * @param string $requestClientId
     * @param string $requestClientSecret
     * @return string
     */
    public function getAuthorizeURL(string $requestClientId, string $requestClientSecret): string;

    /**
     * Get access token
     * @param string $code
     * @return AccessTokenInterface | null
     */
    public function getAccessToken(string $code): ?AccessTokenInterface;

    /**
     * Refresh access token
     * @param string $refreshToken
     * @return AccessTokenInterface | null
     */
    public function refreshAccessToken(string $refreshToken): ?AccessTokenInterface;

    /**
     * Map access token info to internal format
     * @param AccessTokenInterface|null $token
     * @return array
     */
    public function mapAccessToken(?AccessTokenInterface $token): array;

    /**
     * Get provider config
     * @return array
     */
    public function getProviderConfig(): array;

    /**
     * Get applicable client id
     * @param string $requestClientId
     * @param array $providerConfig
     * @return string
     */
    public function getClientID(string $requestClientId, array $providerConfig): string;

    /**
     * Get applicable client secret
     * @param string $requestClientSecret
     * @param array $providerConfig
     * @return string
     */
    public function getClientSecret(string $requestClientSecret, array $providerConfig): string;

    /**
     * Get applicable redirect uri
     * @param array $providerConfig
     * @return string
     */
    public function getRedirectUri(array $providerConfig): string;

    /**
     * Get extra provider params
     * @param array $providerConfig
     * @return array
     */
    public function getExtraProviderParams(array $providerConfig): array;

    /**
     * Get options for building the authorize url
     * @param array $providerConfig
     * @return array
     */
    public function getAuthorizeURLOptions(array $providerConfig): array;

    /**
     * Get options for the get token call
     * @param array $providerConfig
     * @return array
     */
    public function getAccessTokenRequestOptions(array $providerConfig): array;

    /**
     * Get grant for access token request
     * @param array $providerConfig
     * @return string
     */
    public function getAccessTokenRequestGrant(array $providerConfig): string;

    /**
     * Get Provider
     * @param string $requestClientId
     * @param string $requestClientSecret
     * @return AbstractProvider|null
     */
    public function getProvider(string $requestClientId, string $requestClientSecret): ?AbstractProvider;
}
