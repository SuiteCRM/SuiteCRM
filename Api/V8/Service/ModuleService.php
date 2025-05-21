<?php

namespace Api\V8\Service;

use Api\V8\BeanDecorator\BeanListResponse;
use Api\V8\BeanDecorator\BeanManager;
use Api\V8\JsonApi\Helper\AttributeObjectHelper;
use Api\V8\JsonApi\Helper\PaginationObjectHelper;
use Api\V8\JsonApi\Helper\RelationshipObjectHelper;
use Api\V8\JsonApi\Response\DataResponse;
use Api\V8\JsonApi\Response\DocumentResponse;
use Api\V8\JsonApi\Response\MetaResponse;
use Api\V8\Param\CreateModuleParams;
use BeanFactory;
use DocumentRevision;
use Api\V8\Param\DeleteModuleParams;
use Api\V8\Param\GetModuleParams;
use Api\V8\Param\GetModulesParams;
use Api\V8\Param\UpdateModuleParams;
use Exception;
use InvalidArgumentException;
use LoggerManager;
use Slim\Http\Request;
use SugarBean;
use SuiteCRM\Exception\AccessDeniedException;

#[\AllowDynamicProperties]
class ModuleService
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
     * @var RelationshipObjectHelper
     */
    protected $relationshipHelper;

    /**
     * @var PaginationObjectHelper
     */
    protected $paginationHelper;

    /**
     * @param BeanManager $beanManager
     * @param AttributeObjectHelper $attributeHelper
     * @param RelationshipObjectHelper $relationshipHelper
     * @param PaginationObjectHelper $paginationHelper
     */
    public function __construct(
        BeanManager $beanManager,
        AttributeObjectHelper $attributeHelper,
        RelationshipObjectHelper $relationshipHelper,
        PaginationObjectHelper $paginationHelper
    ) {
        $this->beanManager = $beanManager;
        $this->attributeHelper = $attributeHelper;
        $this->relationshipHelper = $relationshipHelper;
        $this->paginationHelper = $paginationHelper;
    }

    /**
     * @param GetModuleParams $params
     * @param $path
     * @return DocumentResponse
     * @throws AccessDeniedException
     */
    public function getRecord(GetModuleParams $params, $path)
    {
        $fields = $params->getFields();
        $bean = $this->beanManager->getBeanSafe(
            $params->getModuleName(),
            $params->getId()
        );

        if (!$bean->ACLAccess('view')) {
            throw new AccessDeniedException();
        }

        $dataResponse = $this->getDataResponse($bean, $fields, $path);

        $response = new DocumentResponse();
        $response->setData($dataResponse);

        return $response;
    }

    /**
     * @param GetModulesParams $params
     * @param Request $request
     * @return DocumentResponse
     * @throws AccessDeniedException
     */
    public function getRecords(GetModulesParams $params, Request $request)
    {
        $beanResult = [];
        global $db;
        // this whole method should split into separated classes later
        $module = $params->getModuleName();
        $orderBy = $params->getSort();
        $where = $params->getFilter();
        $fields = $params->getFields();

        $size = $params->getPage()->getSize();
        $number = $params->getPage()->getNumber();

        $bean = $this->beanManager->newBeanSafe(
            $params->getModuleName()
        );

        if (!$bean->ACLAccess('view')) {
            throw new AccessDeniedException();
        }

        // negative numbers are validated in params
        $offset = $number !== 0 ? ($number - 1) * $size : $number;
        $realRowCount = $this->beanManager->countRecords($module, $where);
        $limit = $size === BeanManager::DEFAULT_ALL_RECORDS ? BeanManager::DEFAULT_LIMIT : $size;
        $deleted = $params->getDeleted();

        if (empty($fields)) {
            $fields = $this->beanManager->getDefaultFields($bean);
        }

        // Detect if bean has email field
        if ((property_exists($bean, 'email1')
                && strpos($where, 'email1') !== false)
            || (property_exists($bean, 'email2')
                && strpos($where, 'email2') !== false)
        ) {

            $selectedModule = strtolower($module);

            // Selects Module or COUNT(*) and will add one to the query.
            $idSelect = 'SELECT ' . $selectedModule . '.id ';
            $countSelect = 'SELECT COUNT(*) AS cnt ';

            $quotedCountSelect = $db->quote($countSelect);

            // Email where clause
            $fromQuery
                = 'FROM email_addresses JOIN email_addr_bean_rel ON email_addresses.id = email_addr_bean_rel.email_address_id JOIN '
                . $selectedModule . ' ON ' . $selectedModule
                . '.id = email_addr_bean_rel.bean_id ';
            $modifiedWhere = str_replace($selectedModule.'.email1',
                'email_addresses.email_address', $where);
            $where = $modifiedWhere;

            // Sets and adds deleted to the query
            if ($deleted === false) {
                $whereAuto = '' . $bean->table_name . ' .deleted=0';
            } else {
                $whereAuto = '' . $bean->table_name . ' .deleted=1';
            }
            if ($where !== '') {
                $where = ' where (' . $where . ') AND ' . $whereAuto . '';
            } else {
                $where = ' where ' . $whereAuto . '';
            }

            // Joins parts together to form query
            $query = $idSelect . $fromQuery . $where;
            $countQuery = $quotedCountSelect . $fromQuery . $where;
            $realRowCount = (int)$db->fetchRow($db->query($countQuery, true, ''))['cnt'];

            // Sets order by into the query
            $order_by = $bean->process_order_by($orderBy);
            if (!empty($orderBy)) {
                $query .= ' ORDER BY ' . $order_by;
            }

            $result = $bean->process_list_query($query, $offset, $limit, -1, $where);
            $beanResult['row_count'] = $result['row_count'];
            $beanList = [];

            foreach ($result['list'] as $resultBean) {
                $queryModuleBean = BeanFactory::newBean($module);
                $queryModuleBean->id = $resultBean->id;
                $beanList[] = $queryModuleBean;
            }
            $beanResult['list'] = $beanList;
            $beanListResponse = new BeanListResponse($beanResult);
        } else {
            $beanListResponse = $this->beanManager->getList($module)
                ->orderBy($orderBy)
                ->where($where)
                ->offset($offset)
                ->limit($limit)
                ->max($size)
                ->deleted($deleted)
                ->fields($this->beanManager->filterAcceptanceFields($bean, $fields))
                ->fetch();
        }

        $beanArray = [];
        foreach ($beanListResponse->getBeans() as $bean) {
            $bean = $this->beanManager->getBeanSafe(
                $params->getModuleName(),
                $bean->id
            );
            $beanArray[] = $bean;
        }
        $data = [];
        foreach ($beanArray as $bean) {
            $dataResponse = $this->getDataResponse(
                $bean,
                $fields,
                $request->getUri()->getPath() . '/' . $bean->id
            );

            $data[] = $dataResponse;
        }

        $response = new DocumentResponse();
        $response->setData($data);

        // pagination
        if ($data && $limit !== BeanManager::DEFAULT_LIMIT) {
            $totalPages = ceil($realRowCount / $size);

            $paginationMeta = $this->paginationHelper->getPaginationMeta($totalPages, count($data));
            $paginationLinks = $this->paginationHelper->getPaginationLinks($request, $totalPages, $number);

            $response->setMeta($paginationMeta);
            $response->setLinks($paginationLinks);
        }

        return $response;
    }

    /**
     * @param CreateModuleParams $params
     * @param Request $request
     *
     * @return DocumentResponse
     * @throws InvalidArgumentException When bean is already exist.
     * @throws AccessDeniedException
     * @throws Exception
     */
    public function createRecord(CreateModuleParams $params, Request $request)
    {
        $module = $params->getData()->getType();
        $id = $params->getData()->getId();
        $attributes = $params->getData()->getAttributes();

        if ($id !== null && $this->beanManager->getBean($module, $id, [], false) instanceof SugarBean) {
            throw new InvalidArgumentException(
                sprintf(
                    'Bean %s with id %s is already exist',
                    $module,
                    $id
                )
            );
        }

        $bean = $this->beanManager->newBeanSafe($module);

        if (!$bean->ACLAccess('save')) {
            throw new AccessDeniedException();
        }

        if ($id !== null) {
            $bean->id = $id;
            $bean->new_with_id = true;
        }

        $this->setRecordUpdateParams($bean, $attributes);
        $fileUpload = $this->processAttributes($bean, $attributes);

        $bean->save();
        if ($fileUpload && $bean->module_dir === 'Notes') {
            $this->addFileToNote($bean->id, $attributes);
        }
        if ($fileUpload && $bean->module_dir === 'Documents') {
            $this->addFileToDocument($bean, $attributes);
        }
        $bean->retrieve($bean->id);

        $dataResponse = $this->getDataResponse(
            $bean,
            null,
            $request->getUri()->getPath() . '/' . $bean->id
        );

        $response = new DocumentResponse();
        $response->setData($dataResponse);

        return $response;
    }

    /**
     * @param SugarBean $bean
     * @param array $attributes
     * @throws Exception
     */
    private function addFileToDocument(SugarBean $bean, array $attributes)
    {
        global $sugar_config;
        BeanFactory::unregisterBean('Documents', $bean->id);
        $bean = BeanFactory::getBean('Documents', $bean->id);
        $bean->filename = $attributes['filename'];
        // Core code in this function
        $Revision = new DocumentRevision();
        $Revision->in_workflow = true;
        $Revision->not_use_rel_in_req = true;
        $Revision->new_rel_id = $bean->id;
        $Revision->new_rel_relname = 'Documents';
        $Revision->change_log = translate('DEF_CREATE_LOG', 'Documents');
        $Revision->revision = '1';
        $Revision->document_id = $bean->id;
        $Revision->filename = $bean->filename;
        if (isset($bean->file_ext)) {
            $Revision->file_ext = $bean->file_ext;
        }
        if (isset($bean->file_mime_type)) {
            $Revision->file_mime_type = $bean->file_mime_type;
        }
        $Revision->doc_type = $bean->doc_type;
        if (isset($bean->doc_id)) {
            $Revision->doc_id = $bean->doc_id;
        }
        if (isset($bean->doc_url)) {
            $Revision->doc_url = $this->doc_url;
        }
        $Revision->id = create_guid();
        $Revision->new_with_id = true;

        // Write file to upload dir
        try {
            // Checking file extension
            $extPos = strrpos((string) $attributes['filename'], '.');
            $fileExtension = substr((string) $attributes['filename'], $extPos + 1);

            if ($extPos === false || empty($fileExtension) || in_array(
                    $fileExtension,
                    $sugar_config['upload_badext'],
                    true
                )) {
                throw new Exception('File upload failed: File extension is not included or is not valid.');
            }

            $fileName = $Revision->id;
            $fileContents = $attributes['filecontents'];
            $targetPath = 'upload/' . $fileName;
            $content = base64_decode($fileContents);

            $file = fopen($targetPath, 'wb');
            fwrite($file, $content);
            fclose($file);
        } catch (Exception $e) {
            LoggerManager::getLogger()->error('addFileToNote: ' . $e->getMessage());
            throw new Exception($e->getMessage());
        }
        $Revision->save();
        $bean->document_revision_id = $Revision->id;
        $bean->save();
    }

    /**
     * @param $beanId
     * @param $attributes
     * @throws Exception
     */
    protected function addFileToNote($beanId, $attributes)
    {
        global $sugar_config, $log;

        $module = 'Notes';
        if (!empty($attributes['moduleName'])) {
            $module = $attributes['moduleName'];
            unset($attributes['moduleName']);
        }
        BeanFactory::unregisterBean($module, $beanId);
        $bean = $this->beanManager->getBeanSafe($module, $beanId);

        // Write file to upload dir
        try {
            // Checking file extension
            $extPos = strrpos((string) $attributes['filename'], '.');
            $fileExtension = substr((string) $attributes['filename'], $extPos + 1);

            if ($extPos === false || empty($fileExtension) || in_array(
                    $fileExtension,
                    $sugar_config['upload_badext'],
                    true
                )) {
                throw new Exception('File upload failed: File extension is not included or is not valid.');
            }

            $fileName = $bean->id;
            $fileContents = $attributes['filecontents'];
            $targetPath = 'upload/' . $fileName;
            $content = base64_decode($fileContents);

            $file = fopen($targetPath, 'wb');
            fwrite($file, $content);
            fclose($file);
        } catch (Exception $e) {
            $log->error('addFileToNote: ' . $e->getMessage());
            throw new Exception($e->getMessage());
        }

        // Fill in file details for use with upload checks
        $mimeType = mime_content_type($targetPath);
        $bean->filename = $attributes['filename'];
        $bean->uploadfile = $attributes['filename'];
        $bean->file_mime_type = $mimeType;
        $bean->save();
    }

    /**
     * @param UpdateModuleParams $params
     * @param Request $request
     * @return DocumentResponse
     * @throws AccessDeniedException
     * @throws Exception
     */
    public function updateRecord(UpdateModuleParams $params, Request $request)
    {
        $module = $params->getData()->getType();
        $id = $params->getData()->getId();
        $attributes = $params->getData()->getAttributes();
        $bean = $this->beanManager->getBeanSafe($module, $id);

        if (!$bean->ACLAccess('save')) {
            throw new AccessDeniedException();
        }

        if (isset($attributes['deleted']) && isTrue($attributes['deleted']) && !$bean->ACLAccess('delete')) {
            throw new AccessDeniedException();
        }

        $this->setRecordUpdateParams($bean, $attributes);
        $fileUpload = $this->processAttributes($bean, $attributes);
        $bean->save();

        if ($fileUpload) {
            $this->addFileToNote($bean->id, $attributes);
        }
        $bean->retrieve($bean->id);

        $dataResponse = $this->getDataResponse(
            $bean,
            null,
            $request->getUri()->getPath() . '/' . $bean->id
        );

        $response = new DocumentResponse();
        $response->setData($dataResponse);

        return $response;
    }

    /**
     * @param $bean
     * @param $attributes
     * @return bool
     */
    protected function processAttributes(&$bean, $attributes)
    {
        $createFile = false;

        foreach ($attributes as $property => $value) {

            if ($property === 'filecontents') {
                continue;
            } elseif ($property === 'filename') {
                $createFile = true;
                continue;
            }

            $bean->$property = $value;
        }

        return $createFile;
    }

    /**
     * @param SugarBean $bean
     * @param array $attributes
     */
    protected function setRecordUpdateParams(SugarBean $bean, array $attributes)
    {
        $bean->set_created_by = !(isset($attributes['created_by']) || isset($attributes['created_by_name']));
        $bean->update_modified_by = !(isset($attributes['modified_user_id']) || isset($attributes['modified_by_name']));
        $bean->update_date_entered = isset($attributes['date_entered']);
        $bean->update_date_modified = !isset($attributes['date_modified']);
    }

    /**
     * @param DeleteModuleParams $params
     * @return DocumentResponse
     * @throws AccessDeniedException
     */
    public function deleteRecord(DeleteModuleParams $params)
    {
        $bean = $this->beanManager->getBeanSafe(
            $params->getModuleName(),
            $params->getId()
        );

        if (!$bean->ACLAccess('delete')) {
            throw new AccessDeniedException();
        }

        $bean->mark_deleted($bean->id);

        $response = new DocumentResponse();
        $response->setMeta(
            new MetaResponse(['message' => sprintf('Record with id %s is deleted', $bean->id)])
        );

        return $response;
    }

    /**
     * @param SugarBean $bean
     * @param array|null $fields
     * @param string|null $path
     *
     * @return DataResponse
     */
    public function getDataResponse(SugarBean $bean, $fields = null, $path = null)
    {
        // this will be split into separated classed later
        $dataResponse = new DataResponse($bean->getObjectName(), $bean->id);
        $dataResponse->setAttributes($this->attributeHelper->getAttributes($bean, $fields));
        $dataResponse->setRelationships($this->relationshipHelper->getRelationships($bean, $path));

        return $dataResponse;
    }
}
