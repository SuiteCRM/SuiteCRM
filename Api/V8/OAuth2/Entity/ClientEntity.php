<?php
namespace Api\V8\OAuth2\Entity;

use League\OAuth2\Server\Entities\ClientEntityInterface;
use League\OAuth2\Server\Entities\Traits\ClientTrait;
use League\OAuth2\Server\Entities\Traits\EntityTrait;

#[\AllowDynamicProperties]
class ClientEntity implements ClientEntityInterface
{
    use EntityTrait, ClientTrait;

    /**
     * @param string $name
     */
    public function setName($name)
    {
        $this->name = $name;
    }

    /**
     * @param string $uri
     */
    public function setRedirectUri($uri)
    {
        $this->redirectUri = $uri;
    }

    /**
     * Returns the registered name (as a string).
     *
     * @return string|string[]
     */
	public function getName()
	{
	    return $this->name;
	}

	/**
     * Returns the registered redirect URI (as a string).
     *
     * @return string|string[]
     */
	public function getRedirectUri()
	{
	    return $this->redirectUri;
	}

    public function setIsConfidential($confidential)
    {
        $this->isConfidential = $confidential;
    }
}
