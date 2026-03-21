<?php
namespace Api\V8\OAuth2\Repository;

use Api\V8\BeanDecorator\BeanManager;
use Api\V8\OAuth2\Entity\AuthCodeEntity;
use League\OAuth2\Server\Entities\AuthCodeEntityInterface;
use League\OAuth2\Server\Repositories\AuthCodeRepositoryInterface;

/**
 * Class AuthCodeRepository
 * @package Api\V8\OAuth2\Repository
 */
class AuthCodeRepository implements AuthCodeRepositoryInterface
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
     * {@inheritdoc}
     */
    public function persistNewAuthCode(AuthCodeEntityInterface $authCodeEntity)
    {
        // Some logic to persist the auth code to a database

        $clientId = $authCodeEntity->getClient()->getIdentifier();

        /** @var \OAuth2AuthCodes $authCode */
        $authCode = $this->beanManager->newBeanSafe(\OAuth2AuthCodes::class);

        $authCode->auth_code = $authCodeEntity->getIdentifier();
        $authCode->auth_code_expires = $authCodeEntity->getExpiryDateTime()->format('Y-m-d H:i:s');
        $authCode->client = $clientId;
        $authCode->assigned_user_id = $authCodeEntity->getUserIdentifier();
        $authCode->auto_authorize = !empty($_POST['confirmed']) && $_POST['confirmed'] === 'always';

        $authCode->save();
    }

    /**
     * {@inheritdoc}
     */
    public function revokeAuthCode($codeId)
    {
        /** @var \OAuth2AuthCodes $authCode */
        $authCode = $this->beanManager->newBeanSafe(\OAuth2AuthCodes::class);
        $authCode->retrieve_by_string_fields(
            ['auth_code' => $codeId]
        );

        if ($authCode->id === null) {
            throw new \InvalidArgumentException('Authorization code is not found for this client');
        }

        if($authCode->auto_authorize === '1') {
            $authCode->auth_code_is_revoked = '1';
            $authCode->save();
            return;
        }

        $authCode->mark_deleted($authCode->id);
    }

    /**
     * {@inheritdoc}
     */
    public function isAuthCodeRevoked($codeId)
    {
        /** @var \OAuth2AuthCodes $authCode */
        $authCode = $this->beanManager->newBeanSafe(\OAuth2AuthCodes::class);
        $authCode->retrieve_by_string_fields(
            ['auth_code' => $codeId]
        );

        return $authCode->is_revoked();
    }

    /**
     * {@inheritdoc}
     */
    public function getNewAuthCode()
    {
        return new AuthCodeEntity();
    }
}
