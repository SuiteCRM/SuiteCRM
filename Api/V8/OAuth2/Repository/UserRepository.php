<?php
namespace Api\V8\OAuth2\Repository;

use Api\V8\BeanDecorator\BeanManager;
use Api\V8\OAuth2\Entity\UserEntity;
use League\OAuth2\Server\Entities\ClientEntityInterface;
use League\OAuth2\Server\Repositories\UserRepositoryInterface;

#[\AllowDynamicProperties]
class UserRepository implements UserRepositoryInterface
{
    private const DUMMY_HASH = 'c5673848f2e5767db3f8f0f2f12cd60f';

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

        $userFound = $user->id !== null;
        $passwordHash = $userFound ? $user->user_hash : self::DUMMY_HASH;

        // Always run the password check, even for a nonexistent user, so the
        // response and its timing are identical whether the username or the
        // password was wrong - neither should be inferable from the outside.
        $passwordValid = \User::checkPassword($password, $passwordHash);

        if (!$userFound || !$passwordValid) {
            throw new \InvalidArgumentException('Invalid username or password.');
        }

        return new UserEntity($user->id);
    }
}
