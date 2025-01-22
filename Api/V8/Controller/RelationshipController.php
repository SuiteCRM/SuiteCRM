<?php
namespace Api\V8\Controller;

use Api\V8\Param\CreateRelationshipParams;
use Api\V8\Param\CreateRelationshipByLinkParams;
use Api\V8\Param\DeleteRelationshipParams;
use Api\V8\Param\GetRelationshipParams;
use Api\V8\Service\RelationshipService;
use Slim\Http\Request;
use Slim\Http\Response;

class RelationshipController extends BaseController
{
    /**
     * @var RelationshipService
     */
    protected $relationshipService;

    /**
     * @param RelationshipService $relationshipService
     */
    public function __construct(RelationshipService $relationshipService)
    {
        $this->relationshipService = $relationshipService;
    }

    /**
     * @param Request $request
     * @param Response $response
     * @param array $args
     * @param GetRelationshipParams $params
     *
     * @return Response
     */
    public function getRelationship(Request $request, Response $response, array $args, GetRelationshipParams $params)
    {
        try {
            $jsonResponse = $this->relationshipService->getRelationship($params, $request);

            return $this->generateResponse($response, $jsonResponse, 200);
        } catch (\Exception $exception) {
            return $this->generateErrorResponse($response, $exception, 400);
        }
    }

    /**
     * @param Request $request
     * @param Response $response
     * @param array $args
     * @param CreateRelationshipParams $params
     *
     * @return Response
     */
    public function createRelationship(
        Request $request,
        Response $response,
        array $args,
        CreateRelationshipParams $params
    ) {
        try {
            $jsonResponse = $this->relationshipService->createRelationship($params);

            return $this->generateResponse($response, $jsonResponse, 201);
        } catch (\Exception $exception) {
            return $this->generateErrorResponse($response, $exception, 400);
        }
    }

    /**
     * @param Request $request
     * @param Response $response
     * @param array $args
     * @param CreateRelationshipByLinkParams $params
     *
     * @return Response
     */
    public function createRelationshipByLink(
        Request $request,
        Response $response,
        array $args,
        CreateRelationshipByLinkParams $params
    ) {
        try {
            $jsonResponse = $this->relationshipService->createRelationshipByLink($params);

            return $this->generateResponse($response, $jsonResponse, 201);
        } catch (\Exception $exception) {
            return $this->generateErrorResponse($response, $exception, 400);
        }
    }

    /**
     * @param Request $request
     * @param Response $response
     * @param array $args
     * @param DeleteRelationshipParams $params
     *
     * @return Response
     */
    public function deleteRelationship(
        Request $request,
        Response $response,
        array $args,
        DeleteRelationshipParams $params
    ) {
        try {
            $jsonResponse = $this->relationshipService->deleteRelationship($params);

            return $this->generateResponse($response, $jsonResponse, 200);
        } catch (\Exception $exception) {
            return $this->generateErrorResponse($response, $exception, 400);
        }
    }
}
