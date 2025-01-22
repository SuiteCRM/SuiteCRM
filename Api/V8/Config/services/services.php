<?php

use Api\V8\BeanDecorator\BeanManager;
use Api\V8\Helper\ModuleListProvider;
use Api\V8\JsonApi\Helper\AttributeObjectHelper;
use Api\V8\JsonApi\Helper\PaginationObjectHelper;
use Api\V8\JsonApi\Helper\RelationshipObjectHelper;
use Api\V8\Service;
use Psr\Container\ContainerInterface as Container;
use Api\Core\Loader\CustomLoader;

return CustomLoader::mergeCustomArray([
    Service\ListViewSearchService::class => function (Container $container) {
        return new Service\ListViewSearchService(
            $container->get(BeanManager::class)
        );
    },
    Service\UserPreferencesService::class => function (Container $container) {
        return new Service\UserPreferencesService(
            $container->get(BeanManager::class)
        );
    },
    Service\UserService::class => function (Container $container) {
        return new Service\UserService(
            $container->get(BeanManager::class),
            $container->get(AttributeObjectHelper::class),
            $container->get(RelationshipObjectHelper::class)
        );
    },
    Service\MetaService::class => function (Container $container) {
        return new Service\MetaService(
            $container->get(BeanManager::class),
            $container->get(ModuleListProvider::class)
        );
    },
    Service\ListViewService::class => function (Container $container) {
        return new Service\ListViewService(
            $container->get(BeanManager::class),
            $container->get(AttributeObjectHelper::class),
            $container->get(RelationshipObjectHelper::class),
            $container->get(PaginationObjectHelper::class)
        );
    },
    Service\ModuleService::class => function (Container $container) {
        return new Service\ModuleService(
            $container->get(BeanManager::class),
            $container->get(AttributeObjectHelper::class),
            $container->get(RelationshipObjectHelper::class),
            $container->get(PaginationObjectHelper::class)
        );
    },
    Service\LogoutService::class => function (Container $container) {
        return new Service\LogoutService(
            $container->get(BeanManager::class)
        );
    },
    Service\RelationshipService::class => function (Container $container) {
        return new Service\RelationshipService(
            $container->get(BeanManager::class),
            $container->get(AttributeObjectHelper::class),
            $container->get(PaginationObjectHelper::class)
        );
    },
], basename(__FILE__));
