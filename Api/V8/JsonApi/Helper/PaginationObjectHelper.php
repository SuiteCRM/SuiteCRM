<?php
namespace Api\V8\JsonApi\Helper;

use Api\V8\JsonApi\Response\MetaResponse;
use Api\V8\JsonApi\Response\PaginationResponse;
use Slim\Http\Request;

#[\AllowDynamicProperties]
class PaginationObjectHelper
{
    /**
     * @param integer $totalPages
     * @param integer $numOfRecords
     *
     * @return MetaResponse
     */
    public function getPaginationMeta($totalPages, $numOfRecords)
    {
        return new MetaResponse(
            ['total-pages' => $totalPages, 'records-on-this-page' => $numOfRecords]
        );
    }

    /**
     * @param Request $request
     * @param integer $totalPages
     * @param integer $number
     *
     * @return PaginationResponse
     */
    public function getPaginationLinks(Request $request, $totalPages, $number)
    {
        $pagination = new PaginationResponse();

        if ($number > 1) {
            $pagination->setFirst($this->createPaginationLink($request, 1));
            $pagination->setPrev($this->createPaginationLink($request, $number - 1));
        }

        if ($number + 1 <= $totalPages) {
            $pagination->setNext($this->createPaginationLink($request, $number + 1));
            $pagination->setLast($this->createPaginationLink($request, $totalPages));
        }

        return $pagination;
    }

    /**
     * @param Request $request
     * @param integer $number
     *
     * @return string
     */
    private function createPaginationLink(Request $request, $number)
    {
        $queryParams = $request->getQueryParams();
        $queryParams['page']['number'] = $number;

        return sprintf('%s?%s', $request->getUri()->getPath(), urldecode(http_build_query($queryParams)));
    }
}
