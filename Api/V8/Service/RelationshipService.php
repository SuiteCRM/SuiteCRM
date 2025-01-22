<?php
namespace Api\V8\Service;

use Api\V8\BeanDecorator\BeanManager;
use Api\V8\JsonApi\Helper\AttributeObjectHelper;
use Api\V8\JsonApi\Helper\PaginationObjectHelper;
use Api\V8\JsonApi\Response\DataResponse;
use Api\V8\JsonApi\Response\DocumentResponse;
use Api\V8\JsonApi\Response\LinksResponse;
use Api\V8\JsonApi\Response\MetaResponse;
use Api\V8\Param\CreateRelationshipParams;
use Api\V8\Param\CreateRelationshipByLinkParams;
use Api\V8\Param\DeleteRelationshipParams;
use Api\V8\Param\GetRelationshipParams;

use Slim\Http\Request;
use \SugarBean;
use \DomainException;

#[\AllowDynamicProperties]
class RelationshipService
{
    /**
     * @var BeanManager
     */
    protected $beanManager;

    /**
     * @var AttributeObjectHelper
     */
    protected $attributeHelper;

    /**
     * @var PaginationObjectHelper
     */
    protected $paginationHelper;

    /**
     * @param BeanManager $beanManager
     * @param AttributeObjectHelper $attributeHelper
     * @param PaginationObjectHelper $paginationHelper
     */
    public function __construct(BeanManager $beanManager, AttributeObjectHelper $attributeHelper, PaginationObjectHelper $paginationHelper)
    {
        $this->beanManager = $beanManager;
        $this->attributeHelper = $attributeHelper;
        $this->paginationHelper = $paginationHelper;
    }

    /**
     * @param GetRelationshipParams $params
     * @param Request $request
     * @return DocumentResponse
     */
    public function getRelationship(GetRelationshipParams $params, Request $request)
    {
        $response = new DocumentResponse();
        $sourceBean = $params->getSourceBean();

        $linkFieldName = $params->getLinkedFieldName();

        $size = $params->getPage()->getSize();
        $number = $params->getPage()->getNumber();

        $linkParams = [
            'order_by' => $params->getSort(),
            'where' => $params->getFilter(),
            'limit' => $size,
            'offset' => $number !== 0 ? ($number - 1) * $size : $number
        ];

        $relatedBeans = $sourceBean->$linkFieldName->getBeans($linkParams);

        if (!$relatedBeans) {
            $response->setMeta(new MetaResponse(
                [
                    'message' => sprintf(
                        'There is no relationship set in %s module with %s link field',
                        $sourceBean->getObjectName(),
                        $linkFieldName
                    )
                ]
            ));
        } else {
            $data = [];
            /** @var SugarBean $relatedBean */
            foreach ($relatedBeans as $relatedBean) {
                $linkResponse = new LinksResponse();
                $linkResponse->setSelf(sprintf('V8/module/%s/%s', $relatedBean->getObjectName(), $relatedBean->id));

                $dataResponse = new DataResponse($relatedBean->getObjectName(), $relatedBean->id);
                $dataResponse->setAttributes($this->attributeHelper->getAttributes($relatedBean));
                $dataResponse->setLinks($linkResponse);
                $data[] = $dataResponse;
            }

            if ($size > 0) {
                unset($linkParams['limit'], $linkParams['offset']);
                $realRowCount = $sourceBean->_get_num_rows_in_query($sourceBean->$linkFieldName->getQuery($linkParams));
                $totalPages = ceil($realRowCount / $size);
                $paginationLinks = $this->paginationHelper->getPaginationLinks($request, $totalPages, $number);
                $response->setLinks($paginationLinks);
            } else {
                $totalPages = 1;
                $realRowCount = count($data);
            }

            $paginationMeta = new MetaResponse(['total-records' => $realRowCount, 'total-pages' => $totalPages, 'records-on-this-page' => count($data)]);
            $response->setMeta($paginationMeta);

            $response->setData($data);
        }

        return $response;
    }

    /**
     * @param CreateRelationshipParams $params
     *
     * @return DocumentResponse
     */
    public function createRelationship(CreateRelationshipParams $params)
    {
        $sourceBean = $params->getSourceBean();
        $relatedBean = $params->getRelatedBean();
        $linkFieldName = $this->beanManager->getLinkedFieldName($sourceBean, $relatedBean);

        $this->beanManager->createRelationshipSafe($sourceBean, $relatedBean, $linkFieldName);

        $response = new DocumentResponse();
        $response->setMeta(new MetaResponse(
            [
                'message' => sprintf(
                    '%s with id %s has been added to %s with id %s',
                    $relatedBean->getObjectName(),
                    $relatedBean->id,
                    $sourceBean->getObjectName(),
                    $sourceBean->id
                )
            ]
        ));

        return $response;
    }

    /**
     * @param CreateRelationshipByLinkParams $params
     *
     * @return DocumentResponse
     */
    public function createRelationshipByLink(CreateRelationshipByLinkParams $params)
    {
        $sourceBean = $params->getSourceBean();

        $relatedBean = $params->getRelatedBean();

        $sourceLabel = translate($sourceBean->module_dir);

        $relatedLabel = translate($relatedBean->module_dir);

        $linkFieldName = $params->getLinkedFieldName();

        $this->beanManager->createRelationshipSafe($sourceBean, $relatedBean, $linkFieldName);

        $response = new DocumentResponse();

        $response->setMeta(
            new MetaResponse(
                [
                    'message' => sprintf(
                        '%s record with id %s has been related to %s record with id %s using link %s',
                        $relatedLabel,
                        $relatedBean->id,
                        $sourceLabel,
                        $sourceBean->id,
                        $linkFieldName
                    ),
                    'sourceModule' => $sourceBean->module_dir,
                    'sourceModuleLabel' => $sourceLabel,
                    'sourceId' => $sourceBean->id,
                    'relatedModule' => $relatedBean->module_dir,
                    'relatedModuleLabel' => $relatedLabel,
                    'relatedId' => $relatedBean->id,
                    'relationshipLink' => $linkFieldName,
                ]
            )
        );

        return $response;
    }

    /**
     * @param DeleteRelationshipParams $params
     *
     * @return DocumentResponse
     * @throws DomainException When the source module is not related to the target module.
     */
    public function deleteRelationship(DeleteRelationshipParams $params)
    {
        $sourceBean = $params->getSourceBean();
        $linkFieldName = $params->getLinkedFieldName();
        $relatedBeans = $sourceBean->get_linked_beans($linkFieldName);
        $relatedBeanId = $params->getRelatedBeanId();

        $relatedBean = array_filter($relatedBeans, function ($bean) use ($relatedBeanId) {
            return $bean->id === $relatedBeanId;
        });

        if (!$relatedBean) {
            throw new DomainException(
                sprintf(
                    'Module with %s id is not related to %s',
                    $relatedBeanId,
                    $sourceBean->getObjectName()
                )
            );
        }

        $relatedBean = array_shift($relatedBean);
        $this->beanManager->deleteRelationshipSafe($sourceBean, $relatedBean, $linkFieldName);

        $response = new DocumentResponse();
        $response->setMeta(new MetaResponse(
            [
                'message' => sprintf(
                    'Relationship has been deleted between %s with id %s and %s with id %s',
                    $sourceBean->getObjectName(),
                    $sourceBean->id,
                    $relatedBean->getObjectName(),
                    $relatedBean->id
                )
            ]
        ));

        return $response;
    }
}
