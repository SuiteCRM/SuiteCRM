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

use League\OAuth2\Client\Token\AccessTokenInterface;

require_once __DIR__ . '/../ExternalOAuthProviderConnector.php';

class GoogleOAuthProviderConnector extends ExternalOAuthProviderConnector
{
    /**
     * @inheritDoc
     */
    public function getProviderType(): string
    {
        return 'Google';
    }

    /**
     * @inheritDoc
     */
    public function getExtraProviderParams(array $providerConfig): array
    {
        $defaults = [
            'urlAuthorize' => 'https://accounts.google.com/o/oauth2/auth',
            'urlAccessToken' => 'https://oauth2.googleapis.com/token',
            'urlResourceOwnerDetails' => 'https://www.googleapis.com/oauth2/v2/userinfo',
        ];

        $extraProviderParams = parent::getExtraProviderParams($providerConfig);

        if (empty($extraProviderParams)) {
            $extraProviderParams = [];
        }

        return array_merge($defaults, $extraProviderParams);
    }

    /**
     * @inheritDoc
     */
    public function getAuthorizeURLOptions(array $providerConfig): array
    {
        $defaults = [
            'access_type' => 'offline',
            'prompt' => 'consent',
        ];

        $authorizeUrlOptions = parent::getAuthorizeURLOptions($providerConfig);

        if (empty($authorizeUrlOptions)) {
            $authorizeUrlOptions = [];
        }

        $merged = array_merge($defaults, $authorizeUrlOptions);

        // Remove deprecated approval_prompt parameter to avoid conflicts with prompt
        unset($merged['approval_prompt']);

        return $merged;
    }

    /**
     * @inheritDoc
     */
    public function mapAccessToken(?AccessTokenInterface $token): array
    {
        if ($token === null) {
            return [];
        }

        $defaults = [
            'access_token' => 'access_token',
            'expires_in' => 'expires_in',
            'refresh_token' => 'refresh_token',
            'token_type' => 'values.token_type'
        ];

        $tokenMapping = $this->getTokenMapping();

        if (empty($tokenMapping) || !is_array($tokenMapping)) {
            $tokenMapping = [];
        }

        foreach ($defaults as $key => $default) {
            if (empty($tokenMapping[$key])) {
                $tokenMapping[$key] = $default;
            }
        }

        return $this->mapTokenDynamically($token, $tokenMapping);
    }

    /**
     * @inheritDoc
     * Override to handle Google OAuth URL generation and remove deprecated parameters
     */
    public function getAuthorizeURL(string $requestClientId, string $requestClientSecret): string
    {
        $authUrl = parent::getAuthorizeURL($requestClientId, $requestClientSecret);

        // Clean the URL to remove deprecated approval_prompt parameter
        $parsedUrl = parse_url($authUrl);
        if (isset($parsedUrl['query'])) {
            parse_str($parsedUrl['query'], $queryParams);

            // Remove deprecated approval_prompt parameter
            unset($queryParams['approval_prompt']);

            // Ensure we have the correct Google OAuth parameters
            $queryParams['access_type'] = 'offline';
            $queryParams['prompt'] = 'consent';

            // Rebuild the URL
            $parsedUrl['query'] = http_build_query($queryParams);
            $authUrl = $this->buildUrl($parsedUrl);
        }

        return $authUrl;
    }

    /**
     * Build URL from parsed URL array
     * @param array $parsedUrl
     * @return string
     */
    private function buildUrl(array $parsedUrl): string
    {
        $scheme = isset($parsedUrl['scheme']) ? $parsedUrl['scheme'] . '://' : '';
        $host = $parsedUrl['host'] ?? '';
        $port = isset($parsedUrl['port']) ? ':' . $parsedUrl['port'] : '';
        $path = $parsedUrl['path'] ?? '';
        $query = isset($parsedUrl['query']) ? '?' . $parsedUrl['query'] : '';
        $fragment = isset($parsedUrl['fragment']) ? '#' . $parsedUrl['fragment'] : '';

        return $scheme . $host . $port . $path . $query . $fragment;
    }
}