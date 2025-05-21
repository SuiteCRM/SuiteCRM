<?php
namespace Api\V8\Service;

use Api\V8\BeanDecorator\BeanManager;
use Api\V8\JsonApi\Response\DocumentResponse;
use Api\V8\JsonApi\Response\MetaResponse;

#[\AllowDynamicProperties]
class LogoutService
{
    /**
     * @var BeanManager
     */
    protected $beanManager;

    /**
     * @param BeanManager $beanManager
     */
    public function __construct(BeanManager $beanManager)
    {
        $this->beanManager = $beanManager;
    }

    /**
     * @param string $accessToken
     *
     * @return DocumentResponse
     * @throws \InvalidArgumentException When access token is not found.
     */
    public function logout($accessToken)
    {
        // same logic in Access and Refresh token repository, refactor this later
        $token = $this->beanManager->newBeanSafe(\OAuth2Tokens::class);
        $token->retrieve_by_string_fields(
            ['access_token' => $accessToken]
        );

        if ($token->id === null) {
            throw new \InvalidArgumentException('Access token is not found for this client');
        }

        $token->mark_deleted($token->id);

        $response = new DocumentResponse();
        $response->setMeta(
            new MetaResponse(['message' => 'You have been successfully logged out'])
        );

        return $response;
    }
}
