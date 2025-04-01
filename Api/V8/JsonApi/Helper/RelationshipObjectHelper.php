<?php
namespace Api\V8\JsonApi\Helper;

use Api\V8\Helper\VarDefHelper;
use Api\V8\JsonApi\Response\LinksResponse;
use Api\V8\JsonApi\Response\RelationshipResponse;

#[\AllowDynamicProperties]
class RelationshipObjectHelper
{
    /**
     * @var VarDefHelper
     */
    protected $varDefHelper;

    /**
     * @param VarDefHelper $varDefHelper
     */
    public function __construct(VarDefHelper $varDefHelper)
    {
        $this->varDefHelper = $varDefHelper;
    }

    /**
     * @param \SugarBean $bean
     * @param string $uriPath
     *
     * @return RelationshipResponse
     */
    public function getRelationships(\SugarBean $bean, $uriPath)
    {
        $relationships = $this->varDefHelper->getAllRelationships($bean);
        asort($relationships);

        $relationshipsLinks = [];
        foreach (array_unique($relationships) as $relationshipName => $module) {
            $linkResponse = new LinksResponse();
            $linkResponse->setRelated(
                sprintf('%s/%s/%s', $uriPath, 'relationships', $relationshipName)
            );

            $relationshipsLinks[$module] = ['links' => $linkResponse];
        }

        return new RelationshipResponse($relationshipsLinks);
    }
}
