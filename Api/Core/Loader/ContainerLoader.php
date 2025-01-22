<?php
namespace Api\Core\Loader;

use Api\Core\Config\ApiConfig;
use Api\Core\Resolver\ConfigResolver;
use Psr\Container\ContainerInterface;
use Slim\Container;

#[\AllowDynamicProperties]
class ContainerLoader
{
    /**
     * Load all service containers and add slim settings
     *
     * @return ContainerInterface
     */
    public static function configure()
    {
        $slimSettings = ConfigResolver::loadFiles(ApiConfig::getSlimSettings());
        // if we want to use this without DI, should create an instance for it
        $container = new Container($slimSettings);

        $services = ConfigResolver::loadFiles(ApiConfig::getContainers());
        foreach ($services as $service => $closure) {
            $container[$service] = $closure;
        }

        return $container;
    }
}
