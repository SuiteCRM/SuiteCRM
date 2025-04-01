<?php
namespace Api\V8\Controller;

use Api\V8\JsonApi\Response\ErrorResponse;
use Slim\Http\Response as HttpResponse;

#[\AllowDynamicProperties]
abstract class BaseController
{
    public const MEDIA_TYPE = 'application/vnd.api+json';

    /**
     * @param HttpResponse $httpResponse
     * @param mixed $response
     * @param integer $status
     *
     * @return HttpResponse
     */
    public function generateResponse(
        HttpResponse $httpResponse,
        $response,
        $status
    ) {
        return $httpResponse
            ->withStatus($status)
            ->withHeader('Accept', static::MEDIA_TYPE)
            ->withHeader('Content-type', static::MEDIA_TYPE)
            ->write(
                json_encode(
                    $response,
                    JSON_PRETTY_PRINT | JSON_UNESCAPED_UNICODE | JSON_UNESCAPED_SLASHES
                )
            );
    }

    /**
     * @param HttpResponse $httpResponse
     * @param \Exception $exception
     * @param integer $status
     *
     * @return HttpResponse
     */
    public function generateErrorResponse(HttpResponse $httpResponse, \Exception $exception, $status)
    {
        $response = new ErrorResponse();
        $response->setStatus($status);
        $response->setDetail($exception->getMessage());
        $response->setException($exception);

        return $this->generateResponse($httpResponse, $response, $status);
    }
}
