<?php
/**
 * @author      Alex Bilbie <hello@alexbilbie.com>
 * @copyright   Copyright (c) Alex Bilbie
 * @license     http://mit-license.org/
 *
 * @link        https://github.com/thephpleague/oauth2-server
 */

namespace SuiteCRM\API\OAuth2\Repositories;

use League\OAuth2\Server\Entities\RefreshTokenEntityInterface;
use League\OAuth2\Server\Repositories\RefreshTokenRepositoryInterface;
use SuiteCRM\API\OAuth2\Entities\RefreshTokenEntity;

#[\AllowDynamicProperties]
class RefreshTokenRepository implements RefreshTokenRepositoryInterface
{
    public const ACCESS_TOKEN_FIELD = 'access_token';

    /**
     * {@inheritdoc}
     */
    public function persistNewRefreshToken(RefreshTokenEntityInterface $refreshTokenEntityInterface)
    {
        $token = new \OAuth2Tokens();
        $tokens =$token->get_list(
            '',
            self::ACCESS_TOKEN_FIELD.' = "'.$refreshTokenEntityInterface->getAccessToken()->getIdentifier().'"'
        );
        foreach ($tokens['list'] as $token) {
            $token->token_is_revoked = false;
            $token->refresh_token = $refreshTokenEntityInterface->getIdentifier();
            $token->refresh_token_expires = $refreshTokenEntityInterface->getExpiryDateTime()->format('Y-m-d H:i:s');
            $token->save();
        }
    }

    /**
     * {@inheritdoc}
     */
    public function revokeRefreshToken($tokenId)
    {
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
    public function isRefreshTokenRevoked($tokenId)
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
            $expires = $timedate->fromUser($token->refresh_token_expires);
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
     * @return RefreshTokenEntity
     */
    public function getNewRefreshToken()
    {
        return new RefreshTokenEntity();
    }
}
