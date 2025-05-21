<?php
namespace Api\V8\OAuth2\Repository;

use Api\V8\BeanDecorator\BeanManager;
use Api\V8\OAuth2\Entity\UserEntity;
use League\OAuth2\Server\Entities\ClientEntityInterface;
use League\OAuth2\Server\Repositories\UserRepositoryInterface;

#[\AllowDynamicProperties]
class UserRepository implements UserRepositoryInterface
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
     *
     * @throws \InvalidArgumentException If user does not exist or the password is invalid.
     */
    public function getUserEntityByUserCredentials(
        $username,
        $password,
        $grantType,
        ClientEntityInterface $clientEntity
    ) {
        /** @var \User $user */
        $user = $this->beanManager->newBeanSafe('Users');
        $user->retrieve_by_string_fields(
            ['user_name' => $username]
        );

        if ($user->id === null) {
            throw new \InvalidArgumentException('No user found with this username: ' . $username);
        }

        if (!\User::checkPassword($password, $user->user_hash)) {
            throw new \InvalidArgumentException('The password is invalid: ' . $password);
        }

        return new UserEntity($user->id);
    }
}
