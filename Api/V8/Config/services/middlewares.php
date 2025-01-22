<?php

use Api\Core\Config\ApiConfig;
use Api\V8\BeanDecorator\BeanManager;
use Api\V8\OAuth2\Entity\AccessTokenEntity;
use Api\V8\OAuth2\Entity\ClientEntity;
use Api\V8\OAuth2\Repository\AccessTokenRepository;
use Api\V8\OAuth2\Repository\ClientRepository;
use Api\V8\OAuth2\Repository\RefreshTokenRepository;
use Api\V8\OAuth2\Repository\ScopeRepository;
use Api\V8\OAuth2\Repository\UserRepository;
use League\OAuth2\Server\Grant\ClientCredentialsGrant;
use Psr\Container\ContainerInterface as Container;
use League\OAuth2\Server\AuthorizationServer;
use League\OAuth2\Server\Grant\PasswordGrant;
use League\OAuth2\Server\Grant\RefreshTokenGrant;
use League\OAuth2\Server\ResourceServer;
use Api\Core\Loader\CustomLoader;
use Api\V8\Helper\OsHelper;
use League\OAuth2\Server\CryptKey;

return CustomLoader::mergeCustomArray([
    AuthorizationServer::class => static function (Container $container) {
        // base dir must exist in entryPoint.php
        $baseDir = $GLOBALS['BASE_DIR'];

        $shouldCheckPermissions = OsHelper::getOS() !== OsHelper::OS_WINDOWS;

	    $oauth2EncKey = $GLOBALS['sugar_config']['oauth2_encryption_key'] ?? '';
	    if (empty($oauth2EncKey)) {
		    $oauth2EncKey = 'SCRM-DEFK';
		    if (isset($GLOBALS['log'])) {
			    $GLOBALS['log']->fatal('WARNING: `oauth2_encryption_key` not set in config.php');
		    }
	    }

        $server = new AuthorizationServer(
            new ClientRepository(
                new ClientEntity(),
                $container->get(BeanManager::class)
            ),
            new AccessTokenRepository(
                new AccessTokenEntity(),
                $container->get(BeanManager::class)
            ),
            new ScopeRepository(),
            new CryptKey(
                sprintf('file://%s/%s', $baseDir, ApiConfig::OAUTH2_PRIVATE_KEY),
                null,
                $shouldCheckPermissions
            ),
	        $oauth2EncKey
        );

        // Client credentials grant
        $server->enableGrantType(
            new ClientCredentialsGrant(),
            new DateInterval('PT1H')
        );

        // Password credentials grant
        $server->enableGrantType(
            new PasswordGrant(
                new UserRepository($container->get(BeanManager::class)),
                new RefreshTokenRepository($container->get(BeanManager::class))
            ),
            new DateInterval('PT1H')
        );

        $refreshGrant = new RefreshTokenGrant(
            new RefreshTokenRepository($container->get(BeanManager::class))
        );

        $refreshGrant->setRefreshTokenTTL(new DateInterval('P1M'));

        $server->enableGrantType(
            $refreshGrant,
            new DateInterval('PT1H')
        );

        return $server;
    },
    ResourceServer::class => static function (Container $container) {
        $baseDir = $GLOBALS['BASE_DIR'];

        $shouldCheckPermissions = OsHelper::getOS() !== OsHelper::OS_WINDOWS;

        return new ResourceServer(
            new AccessTokenRepository(
                new AccessTokenEntity(),
                $container->get(BeanManager::class)
            ),
            new CryptKey(
                sprintf('file://%s/%s', $baseDir, ApiConfig::OAUTH2_PUBLIC_KEY),
                null,
                $shouldCheckPermissions
            )
        );
    },
], basename(__FILE__));
