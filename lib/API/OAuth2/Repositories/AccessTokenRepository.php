<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
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


namespace SuiteCRM\API\OAuth2\Repositories;

use League\OAuth2\Server\Entities\AccessTokenEntityInterface;
use League\OAuth2\Server\Entities\ClientEntityInterface;
use League\OAuth2\Server\Repositories\AccessTokenRepositoryInterface;
use SuiteCRM\API\OAuth2\Entities\AccessTokenEntity;

#[\AllowDynamicProperties]
class AccessTokenRepository implements AccessTokenRepositoryInterface
{
    public const ACCESS_TOKEN_FIELD = 'access_token';
    /**
     * {@inheritdoc}
     */
    public function persistNewAccessToken(AccessTokenEntityInterface $accessTokenEntity)
    {
        global $timedate;

        // Used by password grand
        // Some logic here to save the access token to a database
        $token = new \OAuth2Tokens();
        $token->token_is_revoked = false;
        $token->access_token = $accessTokenEntity->getIdentifier();
        $token->access_token_expires = $timedate->asUser($accessTokenEntity->getExpiryDateTime());
        $token->client = $accessTokenEntity->getClient()->getIdentifier();
        $token->assigned_user_id = $accessTokenEntity->getUserIdentifier();

        if (!$token->assigned_user_id) {
            $client = new \OAuth2Clients();
            $client->retrieve($token->client);
            $token->assigned_user_id = $client->assigned_user_id;
        }

        $token->save();
    }

    /**
     * {@inheritdoc}
     */
    public function revokeAccessToken($tokenId)
    {
        // Some logic here to revoke the access token
        $token = new \OAuth2Tokens();
        $tokens =$token->get_list(
            '',
            self::ACCESS_TOKEN_FIELD.' = "'.$tokenId.'"'
        );
        /**
         * @var \OAuth2Tokens $token
         */
        foreach ($tokens['list'] as $token) {
            $token->token_is_revoked = true;
            $token->save();
        }
    }

    /**
     * {@inheritdoc}
     * @return bool
     */
    public function isAccessTokenRevoked($tokenId)
    {
        global $timedate;

        $token = new \OAuth2Tokens();
        $tokens =$token->get_list(
            '',
            self::ACCESS_TOKEN_FIELD.' = "'.$tokenId.'"'
        );
        /**
         * @var \OAuth2Tokens $token
         */
        foreach ($tokens['list'] as $token) {
            $expires = $timedate->fromUser($token->access_token_expires);
            if (!empty($expires)) {
                $now = new \DateTime('now', $expires->getTimezone());
                if ($now > $expires || (bool)$token->token_is_revoked === true) {
                    $token->token_is_revoked = true;
                    $token->save();
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * {@inheritdoc}
     * @return AccessTokenEntity
     */
    public function getNewToken(ClientEntityInterface $clientEntity, array $scopes, $userIdentifier = null)
    {
        $accessToken = new AccessTokenEntity();
        $accessToken->setClient($clientEntity);
        foreach ($scopes as $scope) {
            $accessToken->addScope($scope);
        }
        $accessToken->setUserIdentifier($userIdentifier);

        return $accessToken;
    }
}
