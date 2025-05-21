<?php
namespace Api\V8\Middleware;

use Api\V8\JsonApi\Response\ErrorResponse;
use Api\V8\Param\BaseParam;
use Exception;
use LoggerManager;
use Slim\Http\Request;
use Slim\Http\Response;
use Api\V8\BeanDecorator\BeanManager;

#[\AllowDynamicProperties]
class ParamsMiddleware
{
    /**
     * @var BaseParam
     */
    private $params;

    /**
     * @var BeanManager
     */
    private $beanManager;

    /**
     * ParamsMiddleware constructor.
     * @param BaseParam $params
     * @param BeanManager $beanManager
     */
    public function __construct(BaseParam $params, BeanManager $beanManager)
    {
        $this->params = $params;
        $this->beanManager = $beanManager;
    }

    /**
     * @param Request $request
     * @param Response $httpResponse
     * @param callable $next
     *
     * @return Response
     */
    public function __invoke(Request $request, Response $httpResponse, callable $next)
    {
        try {
            $this->setCurrentUserGlobal($request);
            $parameters = $this->getParameters($request);
            $this->params->configure($parameters);
            $request = $request->withAttribute('params', $this->params);
        } catch (Exception $exception) {
            $response = new ErrorResponse();
            $response->setStatus(400);
            $msg = $exception->getMessage();
            $dbg = "\nCode:" . $exception->getCode() .
                "\n" . $exception->getFile() . ':' . $exception->getLine() .
                "\nTrace:\n" . $exception->getTraceAsString() .
                "\n";
            LoggerManager::getLogger()->fatal("API Exception detected:\nMessage was: $msg\nException details:\n$dbg");
            $response->setDetail($msg);

            return $httpResponse->withJson(
                $response,
                400,
                JSON_PRETTY_PRINT | JSON_UNESCAPED_UNICODE | JSON_UNESCAPED_SLASHES
            );
        }

        return $next($request, $httpResponse);
    }

    /**
     * @param Request $request
     */
    protected function setCurrentUserGlobal(Request $request)
    {
        $oauth2Token = $this->beanManager->newBeanSafe('OAuth2Tokens');

        $oauth2Token->retrieve_by_string_fields(
            ['access_token' => $request->getAttribute('oauth_access_token_id')]
        );

        $currentUser = $this->beanManager->getBeanSafe('Users', $oauth2Token->assigned_user_id);

        $GLOBALS['current_user'] = $currentUser;
    }

    /**
     * @param Request $request
     *
     * @return array
     */
    protected function getParameters(Request $request)
    {
        $routeParams = array_map(
            function ($value) {
                return is_bool($value) ? $value : urldecode($value);
            },
            $request->getAttribute('route')->getArguments()
        );

        $queryParams = $request->getQueryParams();
        $parsedBody = $request->getParsedBody();

        return array_merge(
            $routeParams,
            isset($queryParams) ? $queryParams : [],
            isset($parsedBody) ? $parsedBody : []
        );
    }
}
