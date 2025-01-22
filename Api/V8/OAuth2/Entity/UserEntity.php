<?php
namespace Api\V8\OAuth2\Entity;

use League\OAuth2\Server\Entities\UserEntityInterface;

#[\AllowDynamicProperties]
class UserEntity implements UserEntityInterface
{
    /**
     * @var $userId
     */
    private $userId;

    /**
     * @param string $userId
     */
    public function __construct($userId)
    {
        $this->userId = $userId;
    }

    /**
     * @inheritdoc
     */
    public function getIdentifier()
    {
        return $this->userId;
    }
}
