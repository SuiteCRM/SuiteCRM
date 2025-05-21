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
     * Get provider config
     * @return array
     */
    public function getProviderConfig(): array
    {

        $config = parent::getProviderConfig();
        $authorizeUrlOptions = $config["authorize_url_options"] ?? [];
        
        // Add extra params in authorize_url_options if these are not indicated in the OAuth provider record configuration
        $approval_prompt = $authorizeUrlOptions["approval_prompt"] ?? '';
        if (empty($approval_prompt)) {
            $sticApprovalPrompt = "force";
            $config["authorize_url_options"]["approval_prompt"] = $sticApprovalPrompt;
        }

        $access_type = $authorizeUrlOptions["access_type"] ?? '';
        if (empty($access_type)) {
            $sticAccesType = "offline";
            $config["authorize_url_options"]["access_type"] = $sticAccesType;
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
        
        // Add extra provider params if these are not indicated in the OAuth provider record configuration
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

        $tokenMapping = $this->getTokenMapping();

        if (!empty($tokenMapping)) {

            return $this->mapTokenDynamically($token, $tokenMapping);
        }

        return [
            'access_token' => $token->getToken(),
            'expires_in' => $token->getExpires(),
            'refresh_token' => $token->getRefreshToken()
        ];
    }
}
