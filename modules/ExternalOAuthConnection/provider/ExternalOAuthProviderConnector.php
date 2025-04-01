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
use League\OAuth2\Client\Provider\Exception\IdentityProviderException;
use League\OAuth2\Client\Provider\GenericProvider;
use League\OAuth2\Client\Token\AccessTokenInterface;

require_once __DIR__ . '/ExternalOAuthProviderConnectorInterface.php';

#[\AllowDynamicProperties]
abstract class ExternalOAuthProviderConnector implements ExternalOAuthProviderConnectorInterface
{
    /**
     * @var string
     */
    public $providerId;

    /**
     * @param string $providerId
     */
    public function __construct(string $providerId)
    {
        $this->providerId = $providerId;
    }

    /**
     * @inheritDoc
     */
    public function getProviderID(): string
    {
        return $this->providerId;
    }

    /**
     * Get Provider
     * @param string $requestClientId
     * @param string $requestClientSecret
     * @return AbstractProvider|null
     */
    public function getProvider(string $requestClientId, string $requestClientSecret): ?AbstractProvider
    {

        $config = $this->getProviderConfig();

        if (empty($config)) {
            return null;
        }

        $paramsToSet = [
            'clientId' => $this->getClientID($requestClientId, $config),
            'clientSecret' => $this->getClientSecret($requestClientSecret, $config),
            'redirectUri' => $this->getRedirectUri($config),
        ];

        $params = [];

        foreach ($paramsToSet as $key => $value) {
            if (!empty($value)) {
                $params[$key] = $value;
            }
        }

        $params = array_merge($params, $this->getExtraProviderParams($config));

        return new GenericProvider($params);
    }

    /**
     * @inheritDoc
     */
    public function getAuthorizeURL(string $requestClientId, string $requestClientSecret): string
    {
        $provider = $this->getProvider($requestClientId, $requestClientSecret);
        $config = $this->getProviderConfig();

        if ($provider === null || empty($config)) {
            return '';
        }

        $authUrl = $provider->getAuthorizationUrl($this->getAuthorizeURLOptions($config));
        $_SESSION['oauth2state'] = $provider->getState();

        return $authUrl;
    }

    /**
     * @inheritDoc
     * @throws IdentityProviderException
     */
    public function getAccessToken(string $code): ?AccessTokenInterface
    {
        $config = $this->getProviderConfig();
        $provider = $this->getProvider('', '');

        if ($provider === null || empty($config)) {
            return null;
        }

        $options = [
            'code' => $code,
        ];

        $options = array_merge_recursive($options, $this->getAccessTokenRequestOptions($config));

        return $provider->getAccessToken(
            $this->getAccessTokenRequestGrant($config),
            $options
        );
    }

    /**
     * @inheritDoc
     */
    public function refreshAccessToken(string $refreshToken): ?AccessTokenInterface
    {
        $config = $this->getProviderConfig();
        $provider = $this->getProvider('', '');

        if ($provider === null || empty($config)) {
            return null;
        }

        $options = [
            'refresh_token' => $refreshToken,
        ];

        $options = array_merge_recursive($options, $this->getRefreshTokenRequestOptions($config));

        return $provider->getAccessToken(
            $this->getRefreshTokenRequestGrant($config),
            $options
        );
    }

    /**
     * Get provider config
     * @return array
     */
    public function getProviderConfig(): array
    {
        $providerId = $this->getProviderID();

        if (empty($providerId)) {
            return [];
        }

        /** @var ExternalOAuthProvider $providerBean */
        $providerBean = BeanFactory::getBean('ExternalOAuthProvider', $providerId);

        if (empty($providerBean)) {
            return [];
        }

        return $providerBean->getConfigArray();
    }

    /**
     * Get applicable client id
     * @param string $requestClientId
     * @param array $providerConfig
     * @return string
     */
    public function getClientID(string $requestClientId, array $providerConfig): string
    {

        $clientId = $requestClientId ?? '';
        if (!empty($_SESSION['external_oauth_client_id'])) {
            $clientId = $_SESSION['external_oauth_client_id'];
        } elseif (empty($clientId) && !empty($providerConfig['client_id'])) {
            $clientId = $providerConfig['client_id'];
        }

        return $clientId;
    }

    /**
     * Get applicable client secret
     * @param string $requestClientSecret
     * @param array $providerConfig
     * @return string
     */
    public function getClientSecret(string $requestClientSecret, array $providerConfig): string
    {

        $clientSecret = $requestClientSecret ?? '';
        if (!empty($_SESSION['external_oauth_client_secret'])) {
            $clientSecret = $_SESSION['external_oauth_client_secret'];
        } elseif (empty($clientSecret) && !empty($providerConfig['client_secret'])) {
            $clientSecret = $providerConfig['client_secret'];
        }

        return $clientSecret;
    }

    /**
     * Get applicable redirect uri
     * @param array $providerConfig
     * @return string
     */
    public function getRedirectUri(array $providerConfig): string
    {
        return $providerConfig['redirect_uri'] ?? '';
    }

    /**
     * Get extra provider params
     * @param array $providerConfig
     * @return array
     */
    public function getExtraProviderParams(array $providerConfig): array
    {
        return $providerConfig['extra_provider_params'] ?? [];
    }

    /**
     * Get options for building the authorize url
     * @param array $providerConfig
     * @return array
     */
    public function getAuthorizeURLOptions(array $providerConfig): array
    {
        return $providerConfig['authorize_url_options'] ?? [];
    }

    /**
     * Get options for the get token call
     * @param array $providerConfig
     * @return array
     */
    public function getAccessTokenRequestOptions(array $providerConfig): array
    {
        return $providerConfig['get_token_request_options'] ?? [];
    }

    /**
     * Get grant for access token request
     * @param array $providerConfig
     * @return string
     */
    public function getAccessTokenRequestGrant(array $providerConfig): string
    {
        return $providerConfig['get_token_request_grant'] ?? 'authorization_code';
    }

    /**
     * Get options for the refresh token call
     * @param array $providerConfig
     * @return array
     */
    public function getRefreshTokenRequestOptions(array $providerConfig): array
    {
        return $providerConfig['refresh_token_request_options'] ?? [];
    }

    /**
     * Get grant for refresh token request
     * @param array $providerConfig
     * @return string
     */
    public function getRefreshTokenRequestGrant(array $providerConfig): string
    {
        return $providerConfig['refresh_token_request_grant'] ?? 'refresh_token';
    }

    /**
     * Get token mapping configuration
     * @return array|mixed
     */
    protected function getTokenMapping()
    {
        $config = $this->getProviderConfig();
        $tokenMapping = [];

        if (!empty($config)) {
            $tokenMapping = $config['token_mapping'] ?? [];
        }

        return $tokenMapping;
    }

    /**
     * Get value from nested array using the given path
     * @param array $data
     * @param string $path
     * @return array|mixed|null
     */
    public function getArrayValue(array $data, string $path)
    {

        if (empty($path)) {
            return null;
        }

        $indexes = explode('.', $path);

        if (empty($indexes)) {
            return null;
        }

        $result = $data;

        foreach ($indexes as $index) {
            $result = $result[$index] ?? [];
        }

        return $result;
    }

    /**
     * Map token according to token mapping configuration
     * @param AccessTokenInterface $token
     * @param $tokenMapping
     * @return array
     */
    protected function mapTokenDynamically(AccessTokenInterface $token, $tokenMapping): array
    {
        $tokenData = [
            'access_token' => $token->getToken(),
            'expires_in' => $token->getExpires(),
            'refresh_token' => $token->getRefreshToken(),
            'values' => $token->getValues(),
        ];

        $mapped = [];
        foreach ($tokenMapping as $tokenEntryKey => $tokenEntryValuePath) {
            $mapped[$tokenEntryKey] = $this->getArrayValue($tokenData, $tokenEntryValuePath) ?? '';
        }

        return $mapped;
    }

}
