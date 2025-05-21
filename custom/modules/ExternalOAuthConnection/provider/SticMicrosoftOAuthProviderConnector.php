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

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

use League\OAuth2\Client\Token\AccessTokenInterface;

require_once 'modules/ExternalOAuthConnection/provider/ExternalOAuthProviderConnector.php';

class SticMicrosoftOAuthProviderConnector extends ExternalOAuthProviderConnector
{
    /**
     * Get the provider type
     * @return string
     */
    public function getProviderType(): string
    {
        return 'Stic Microsoft';
    }

    /**
     * Get provider config
     * @return array
     */
    public function getProviderConfig(): array
    {
        $config = parent::getProviderConfig();
        $authorizeUrlOptions = $config["authorize_url_options"] ?? [];
        
        // Add extra params in authorize_url_options if these are not indicated in the OAuth provider record configuration
        $scope = $authorizeUrlOptions["scope"] ?? '';
        if (empty($scope)) {
            $sticScope = "https://outlook.office.com/IMAP.AccessAsUser.All offline_access User.Read";
            $config["authorize_url_options"]["scope"] = $sticScope;
        }

        return $config;
    }

    /**
     * Get extra provider params
     * @param array $providerConfig
     * @return array
     */
    public function getExtraProviderParams(array $providerConfig): array
    {
        $extraProviderParams = parent::getExtraProviderParams($providerConfig) ?? [];     
        
        $scopes = $extraProviderParams["scopes"] ?? null;
        if (empty($scopes)) {
            $sticScopes = "https://outlook.office.com/IMAP.AccessAsUser.All offline_access User.Read";
            $extraProviderParams["scopes"] = $sticScopes;
        }

        $urlResourceOwnerDetails = $extraProviderParams["urlResourceOwnerDetails"] ?? null;
        if (!isset($urlResourceOwnerDetails)) {  // This parameter can be empty so isset is used 
            $sticUrlResourceOwnerDetails = "";
            $extraProviderParams["urlResourceOwnerDetails"] = $sticUrlResourceOwnerDetails;
        }

        return $extraProviderParams;
    }


    /**
     * Map access token info to internal format
     * @param AccessTokenInterface|null $token
     * @return array
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
            if (empty($tokenMapping[$key])){
                $tokenMapping[$key] = $default;
            }
        }

        return $this->mapTokenDynamically($token, $tokenMapping);
    }
}