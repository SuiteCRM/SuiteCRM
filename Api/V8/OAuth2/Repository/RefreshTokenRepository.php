<?php
namespace Api\V8\OAuth2\Repository;

use Api\V8\BeanDecorator\BeanManager;
use Api\V8\OAuth2\Entity\RefreshTokenEntity;
use League\OAuth2\Server\Entities\RefreshTokenEntityInterface;
use League\OAuth2\Server\Repositories\RefreshTokenRepositoryInterface;

#[\AllowDynamicProperties]
class RefreshTokenRepository implements RefreshTokenRepositoryInterface
{
    /**
     * @var BeanManager
     */
    private $beanManager;

    /**
     * @param BeanManager $beanManager
     */
    public function __construct(BeanManager $beanManager)
    {
        $this->beanManager = $beanManager;
    }

    /**
     * @inheritdoc
     */
    public function getNewRefreshToken()
    {
        return new RefreshTokenEntity();
    }

    /**
     * @inheritdoc
     *
     * @throws \InvalidArgumentException When access token is not found.
     */
    public function persistNewRefreshToken(RefreshTokenEntityInterface $refreshTokenEntity)
    {
        /** @var \OAuth2Tokens $token */
        $token = $this->beanManager->newBeanSafe(\OAuth2Tokens::class);
        $token->retrieve_by_string_fields(
            ['access_token' => $refreshTokenEntity->getAccessToken()->getIdentifier()]
        );

        if ($token->id === null) {
            throw new \InvalidArgumentException('Access token is not found for this client');
        }

        $token->refresh_token = $refreshTokenEntity->getIdentifier();
        $token->refresh_token_expires = $refreshTokenEntity->getExpiryDateTime()->format('Y-m-d H:i:s');
        $token->save();
    }

    /**
     * @inheritdoc
     *
     * @throws \InvalidArgumentException When refresh token is not found.
     */
    public function revokeRefreshToken($tokenId)
    {
        $token = $this->beanManager->newBeanSafe(\OAuth2Tokens::class);
        $token->retrieve_by_string_fields(
            ['refresh_token' => $tokenId],
            true,
            false
        );

        if ($token->id === null) {
            throw new \InvalidArgumentException('Refresh token is not found for this client');
        }

        $token->mark_deleted($token->id);
    }

    /**
     * @inheritdoc
     */
    public function isRefreshTokenRevoked($tokenId)
    {
        /** @var \OAuth2Tokens $token */
        $token = $this->beanManager->newBeanSafe(\OAuth2Tokens::class);
        $token->retrieve_by_string_fields(
            ['refresh_token' => $tokenId]
        );

        if (new \DateTime() > new \DateTime($token->refresh_token_expires) || $token->id === null) {
            return true;
        }

        return false;
    }
}
