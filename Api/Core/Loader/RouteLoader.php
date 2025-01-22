<?php
namespace Api\Core\Loader;

use Api\Core\Config\ApiConfig;
use Api\Core\Resolver\ConfigResolver;
use Slim\App;

#[\AllowDynamicProperties]
class RouteLoader
{
    /**
     * Load all app routes
     *
     * @param App $app
     */
    public function configureRoutes(App $app)
    {
        $routes = ApiConfig::getRoutes();

        foreach ($routes as $route) {
            if (ConfigResolver::isFileExist($route)) {
                require $route;
            }
        }
    }
}
