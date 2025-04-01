<?php
namespace Api\V8\Controller\InvocationStrategy;

use Psr\Http\Message\ResponseInterface;
use Psr\Http\Message\ServerRequestInterface;
use Slim\Interfaces\InvocationStrategyInterface;

#[\AllowDynamicProperties]
class SuiteInvocationStrategy implements InvocationStrategyInterface
{
    /**
     * @inheritdoc
     */
    public function __invoke(
        callable $callable,
        ServerRequestInterface $request,
        ResponseInterface $response,
        array $routeArguments
    ) {
        foreach ($routeArguments as $attribute => $value) {
            $request = $request->withAttribute($attribute, $value);
        }

        // since we support 5.5.9, we can't use splat op here
        return $callable(
            $request,
            $response,
            $routeArguments,
            $request->getAttribute('params') ? $request->getAttribute('params') : null
        );
    }
}
