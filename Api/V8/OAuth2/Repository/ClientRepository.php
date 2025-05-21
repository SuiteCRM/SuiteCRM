<?php
namespace Api\V8\OAuth2\Repository;

use Api\V8\BeanDecorator\BeanManager;
use Api\V8\OAuth2\Entity\ClientEntity;
use League\OAuth2\Server\Repositories\ClientRepositoryInterface;

#[\AllowDynamicProperties]
class ClientRepository implements ClientRepositoryInterface
{
    /**
     * @var ClientEntity
     */
    private $clientEntity;

    /**
     * @var BeanManager
     */
    private $beanManager;

    /**
     * @param ClientEntity $clientEntity
     * @param BeanManager $beanManager
     */
    public function __construct(ClientEntity $clientEntity, BeanManager $beanManager)
    {
        $this->clientEntity = $clientEntity;
        $this->beanManager = $beanManager;
    }

    /**
     * @inheritdoc
     */
    public function getClientEntity($clientIdentifier)
    {
        /** @var \OAuth2Clients $client */
        $client = $this->beanManager->getBeanSafe(\OAuth2Clients::class, $clientIdentifier);

        $this->clientEntity->setIdentifier($clientIdentifier);
        $this->clientEntity->setName($client->name);
        $this->clientEntity->setRedirectUri($client->redirect_uri ?? '');
		$this->clientEntity->setIsConfidential($client->is_confidential ?? false);

        return $this->clientEntity;
    }

    public function validateClient($clientIdentifier, $clientSecret, $grantType)
    {
        /** @var \OAuth2Clients $client */
        $client = $this->beanManager->getBeanSafe(\OAuth2Clients::class, $clientIdentifier);

        if ($grantType === $client->allowed_grant_type || $grantType === 'refresh_token')
        {
            return hash('sha256', $clientSecret) === $client->secret;
        }

        return false;
    }
}
