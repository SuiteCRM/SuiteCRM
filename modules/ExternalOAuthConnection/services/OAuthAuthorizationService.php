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

use League\OAuth2\Client\Provider\Exception\IdentityProviderException;
use League\OAuth2\Client\Token\AccessTokenInterface;

require_once __DIR__ . '/../provider/ExternalOAuthProviderConnectorInterface.php';
require_once __DIR__ . '/../provider/Generic/GenericOAuthProviderConnector.php';
require_once __DIR__ . '/../provider/Microsoft/MicrosoftOAuthProviderConnector.php';

#[\AllowDynamicProperties]
class OAuthAuthorizationService
{

    /**
     * Check if provider exists
     * @param string $providerId
     * @return bool
     */
    public function hasProvider(string $providerId): bool
    {

        $providerConfig = $this->getProviderConfig($providerId);

        if (empty($providerConfig)) {
            return false;
        }

        $provider = $this->getProvider($providerId);

        return $provider !== null;
    }

    /**
     * Get provider
     * @param string $providerId
     * @return ExternalOAuthProviderConnectorInterface|null
     */
    public function getProvider(string $providerId): ?ExternalOAuthProviderConnectorInterface
    {
        $provider = null;

        $providerConfig = $this->getProviderConfig($providerId);

        if (empty($providerConfig) || empty($providerConfig['type'])) {
            return null;
        }

        $providerType = $providerConfig['type'];


        $external_oauth_providers_connectors = $this->getExternalOauthProvidersConnectors();

        if (!empty($external_oauth_providers_connectors[$providerType]['class']) && class_exists($external_oauth_providers_connectors[$providerType]['class'])) {
            $providerClass = $external_oauth_providers_connectors[$providerType]['class'];
            $provider = new $providerClass($providerId);
        }

        return $provider;
    }

    /**
     * Redirect to authorization endpoint
     * @param string $providerId
     * @param string $requestClientId
     * @param string $requestClientSecret
     * @return void
     */
    public function authorizationRedirect(
        string $providerId,
        string $requestClientId,
        string $requestClientSecret
    ): void {
        $provider = $this->getProvider($providerId);

        if ($provider === null) {
            $this->log('fatal', 'OAuthAuthorizationService::authorizationRedirect::provider', 'provider not found');
            return;
        }

        $authUrl = $provider->getAuthorizeURL($requestClientId, $requestClientSecret);

        $this->log('debug', 'authorizationRedirect::authUrl', $authUrl);

        header('Location: ' . $authUrl);
    }

    /**
     * Get access token
     * @param string $providerId
     * @param string $code
     * @return AccessTokenInterface|null
     */
    public function getAccessToken(string $providerId, string $code): ?AccessTokenInterface
    {
        $provider = $this->getProvider($providerId);

        if ($provider === null) {
            $this->log('fatal', 'OAuthAuthorizationService::getAccessToken::provider', 'provider not found');
            return null;
        }

        try {
            $token = $provider->getAccessToken($code);
        } catch (IdentityProviderException $e) {
            $this->logResponse($e);
            $this->cleanSession();
            return null;
        }

        $this->cleanSession();

        return $token;
    }

    /**
     * Refresh the current token for connection
     * @param ExternalOAuthConnection $connection
     * @return void
     */
    public function refreshConnectionToken(ExternalOAuthConnection $connection): array {


        $providerId = $connection->external_oauth_provider_id ?? '';

        if (!$this->hasProvider($providerId)) {
            $this->log('fatal', 'OAuthAuthorizationService::refreshConnectionToken::provider', "The specified OAuth2 provider '$providerId' is not supported or not properly configured");
            return [
                'success' => false,
                'reLogin' => false,
                'message' => "The specified OAuth2 provider '$providerId' is not supported or not properly configured"
            ];
        }

        $provider = $this->getProvider($providerId);

        if ($provider === null) {
            $this->log('fatal', 'OAuthAuthorizationService::refreshConnectionToken::provider', 'provider not found');
            return [
                'success' => false,
                'reLogin' => false,
                'message' => "The specified OAuth2 provider '$providerId' was not found"
            ];
        }

        $refreshToken = $connection->refresh_token ?? '';

        if ($refreshToken === '') {
            $this->log('fatal', 'OAuthAuthorizationService::refreshConnectionToken::refreshToken', 'Refersh token not set');
            return [
                'success' => false,
                'reLogin' => true,
                'message' => "Refresh token not set. Need to re-login"
            ];
        }

        $token =  $provider->refreshAccessToken($refreshToken);

        if ($token === null) {
            $this->log('fatal', 'OAuthAuthorizationService::refreshToken::token', 'Not able to get access token. Check logs for more details');
            return [
                'success' => false,
                'reLogin' => true,
                'message' => "Was not able to refresh the token. Your session may have expired. Please try to re-login."
            ];
        }

        $mappedToken = $this->mapToken($providerId, $token);

        $connection->access_token = $mappedToken['access_token'];
        $connection->expires_in = $mappedToken['expires_in'];
        $connection->refresh_token = $mappedToken['refresh_token'];
        $connection->token_type = $mappedToken['token_type'];

        $connection->save();

        $this->log('debug', 'OAuthAuthorizationService::refreshConnectionToken::token', 'successfully refreshed token');

        // reset as the connection tokens have now been encripted
        $connection->access_token = $mappedToken['access_token'];
        $connection->expires_in = $mappedToken['expires_in'];
        $connection->refresh_token = $mappedToken['refresh_token'];
        $connection->token_type = $mappedToken['token_type'];

        return [
            'success' => true,
            'reLogin' => false,
            'message' => "successfully refreshed token"
        ];
    }

    /**
     * Check if token for connection has expired
     * @param ExternalOAuthConnection $connection
     * @return void
     */
    public function hasConnectionTokenExpired(ExternalOAuthConnection $connection): array {

        $expireTimeStamp = $connection->expires_in ?? '';

        if (empty($expireTimeStamp)) {
            $this->log('fatal', 'OAuthAuthorizationService::hasConnectionTokenExpired', 'expires_in not set');
            return [
                'expired' => true,
                'refreshToken' => false,
                'message' => "Expiry date not set"
            ];
        }

        if (!empty($expireTimeStamp)) {
            $expireTimeStamp = (int) $expireTimeStamp;
        }

        if(time() > $expireTimeStamp) {
            $this->log('fatal', 'OAuthAuthorizationService::hasConnectionTokenExpired', 'Access token has expired');
            return [
                'expired' => true,
                'refreshToken' => true,
                'message' => "Token expired"
            ];
        }

        return [
            'expired' => false,
            'refreshToken' => false,
            'message' => "Token valid"
        ];
    }

    /**
     * Map token to bean fields array
     * @param string $providerId
     * @param AccessTokenInterface|null $token
     * @return mixed[]
     */
    public function mapToken(string $providerId, ?AccessTokenInterface $token): array
    {
        $provider = $this->getProvider($providerId);

        if ($provider === null || $token === null) {
            return [];
        }

        return $provider->mapAccessToken($token);
    }

    /**
     * Get provider config
     * @param string $providerId
     * @return array|null
     */
    public function getProviderConfig(string $providerId): ?array
    {
        if (empty($providerId)) {
            return null;
        }

        /** @var ExternalOAuthProvider $providerBean */
        $providerBean = BeanFactory::getBean('ExternalOAuthProvider', $providerId);

        if (empty($providerBean)) {
            return null;
        }

        return $providerBean->getConfigArray();
    }

    /**
     * Log message
     * @param string $level
     * @param string $context
     * @param string $message
     * @return void
     */
    public function log(string $level, string $context, string $message): void
    {
        global $log;

        $log->$level($context . ' | ' . $message);
    }

    /**
     * Log get access token response
     * @param IdentityProviderException $e
     * @return void
     */
    protected function logResponse(IdentityProviderException $e): void
    {
        $this->log('fatal', 'OAuthAuthorizationService::getAccessToken', 'exception while trying to get access token');
        $this->log('fatal', 'OAuthAuthorizationService::getAccessToken', 'exception message: ' . $e->getMessage());

        $responseBody = $e->getResponseBody();
        if (is_array($e->getResponseBody())) {
            try {
                $responseBody = json_encode($e->getResponseBody(), JSON_THROW_ON_ERROR);
            } catch (JsonException $e) {
                $responseBody = print_r($e->getResponseBody(), true);
            }
        }
        $this->log('fatal', 'OAuthAuthorizationService::getAccessToken', 'exception response body: ' . $responseBody);
        $this->log('fatal', 'OAuthAuthorizationService::getAccessToken', 'exception trace: ' . $e->getTraceAsString());
    }

    /**
     * Clean user session variables
     * @return void
     */
    public function cleanSession(): void
    {
        $_SESSION['external_oauth_client_id'] = '';
        $_SESSION['external_oauth_client_secret'] = '';
        $_SESSION['provider'] = '';
        $_SESSION['oauth2state'] = '';
    }

    /**
     * Get External OAuth Provider Connectors
     * @return string[][]
     */
    public function getExternalOauthProvidersConnectors(): array
    {
        $external_oauth_providers = [
            'Microsoft' => [
                'class' => 'MicrosoftOAuthProviderConnector'
            ],
            'Generic' => [
                'class' => 'GenericOAuthProviderConnector'
            ],
        ];

        if (file_exists('custom/application/Ext/ExternalOAuthProviders/externaloauthproviders.ext.php')) {
            include('custom/application/Ext/ExternalOAuthProviders/externaloauthproviders.ext.php');
        }

        return $external_oauth_providers;
    }

}
