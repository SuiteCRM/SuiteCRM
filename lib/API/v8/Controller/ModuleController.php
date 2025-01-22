<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUGARCRM, SUGARCRM DISCLAIMS THE WARRANTY
 * OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License along with
 * this program; if not, see http://www.gnu.org/licenses or write to the Free
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 *
 * You can contact SugarCRM, Inc. headquarters at 10050 North Wolfe Road,
 * SW2-130, Cupertino, CA 95014, USA. or at email address contact@sugarcrm.com.
 *
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo and "Supercharged by SuiteCRM" logo. If the display of the logos is not
 * reasonably feasible for technical reasons, the Appropriate Legal Notices must
 * display the words "Powered by SugarCRM" and "Supercharged by SuiteCRM".
 */

namespace SuiteCRM\API\v8\Controller;

use BeanFactory;
use DateTime;
use Favorites;
use GroupedTabStructure;
use Link2;
use MBConstants;
use ParserFactory;
use Psr\Container\ContainerExceptionInterface;
use Psr\Container\NotFoundExceptionInterface;
use Psr\Http\Message\ResponseInterface as Response;
use Psr\Http\Message\ServerRequestInterface as Request;
use SugarBean;
use SugarView;
use SuiteCRM\API\JsonApi\v1\Enumerator\RelationshipType;
use SuiteCRM\API\JsonApi\v1\Enumerator\SugarBeanRelationshipType;
use SuiteCRM\API\JsonApi\v1\Links;
use SuiteCRM\API\JsonApi\v1\Resource\Relationship;
use SuiteCRM\API\JsonApi\v1\Resource\Resource;
use SuiteCRM\API\JsonApi\v1\Resource\ResourceIdentifier;
use SuiteCRM\API\JsonApi\v1\Resource\SuiteBeanResource;
use SuiteCRM\API\v8\Exception\ApiException;
use SuiteCRM\API\v8\Exception\BadRequestException;
use SuiteCRM\API\v8\Exception\ConflictException;
use SuiteCRM\API\v8\Exception\EmptyBodyException;
use SuiteCRM\API\v8\Exception\ForbiddenException;
use SuiteCRM\API\v8\Exception\IdAlreadyExistsException;
use SuiteCRM\API\v8\Exception\InvalidJsonApiResponseException;
use SuiteCRM\API\v8\Exception\ModuleNotFoundException;
use SuiteCRM\API\v8\Exception\NotAcceptableException;
use SuiteCRM\API\v8\Exception\NotFoundException;
use SuiteCRM\API\v8\Exception\UnsupportedMediaTypeException;
use SuiteCRM\API\v8\Library\ModulesLib;
use SuiteCRM\Enumerator\ExceptionCode;
use SuiteCRM\Exception\Exception;
use SuiteCRM\Exception\InvalidArgumentException;
use SuiteCRM\Utility\ApplicationLanguage;
use SuiteCRM\Utility\SuiteValidator;
use Tracker;

/**
 * Class ModuleController
 * @package SuiteCRM\API\v8\Controller
 */
class ModuleController extends ApiController
{
    public const MISSING_ID = '[ModuleController] ["id" does not exist]';
    public const SOURCE_TYPE = '/data/attributes/type';

    /**
     * GET /api/v8/modules/meta/list
     * @param Request $req
     * @param Response $res
     * @return Response
     * @throws RuntimeException
     */
    public function getModulesMetaList(Request $req, Response $res)
    {
        try {
            $config = $this->containers->get('ConfigurationManager');
            require_once $this->paths->getProjectPath().'/include/modules.php';
            global $moduleList;

            $payload = array(
                'meta' => array('modules' => array('list' => array()))
            );

            foreach ($moduleList as $module) {
                $payload['meta']['modules']['list'][$module]['links'] =
                    $config['site_url'] . '/api/v'. self::VERSION_MAJOR . '/modules/'.$module;
            }

            $this->negotiatedJsonApiContent($req, $res);
        } catch (\Exception $e) {
            $payload = $this->handleExceptionIntoPayloadError($req, $e, isset($payload) ? $payload : []);
        }
        return $this->generateJsonApiResponse($req, $res, $payload);
    }

    /**
     * GET /api/v8/modules/meta/menu/modules
     * @param Request $req
     * @param Response $res
     * @param array $args
     * @return Response
     * @throws RuntimeException
     */
    public function getModulesMetaMenuModules(Request $req, Response $res, array $args)
    {
        try {
            global $current_user;
            global $sugar_config;
            global $app_strings;

            $config = $this->containers->get('ConfigurationManager');
            $this->negotiatedJsonApiContent($req, $res);

            $payload = array();

            require_once $this->paths->getProjectPath().'/include/GroupedTabs/GroupedTabStructure.php';
            $groupedTabsClass = new GroupedTabStructure();
            $modules = query_module_access_list($current_user);


            $sugarView = new SugarView();
            foreach ($modules as $moduleKey => $module) {
                $moduleName = $module;
                $menu = $sugarView->getMenu($moduleName);

                $self = $config['site_url'] . '/api/v'. self::VERSION_MAJOR . '/modules/' . $moduleName . '/';
                $actions = array();
                foreach ($menu as $item) {
                    $url = parse_url((string) $item[0]);
                    parse_str($url['query'], $orig);
                    $actions[] = array(
                        'href' => $self . $item[2],
                        'label' => $item[1],
                        'action' => $item[2],
                        'module' => $item[3],
                        'type' => $item[3],
                        'query' => $orig,
                    );
                }

                $modules[$moduleKey] = array(
                    'type' => $moduleName,
                    'href' => $config['site_url'] . '/api/v'. self::VERSION_MAJOR . '/modules/' . $moduleName . '/',
                    'menu' => $actions
                );
            }

            $payload['meta']['menu']['modules'] = array(
                'all' => $modules,
            );
        } catch (\Exception $e) {
            $payload = $this->handleExceptionIntoPayloadError($req, $e, isset($payload) ? $payload : []);
        }

        return $this->generateJsonApiResponse($req, $res, $payload);
    }

    /**
     * GET /api/v8/modules/meta/menu/filters
     * @param Request $req
     * @param Response $res
     * @param array $args
     * @return Response
     * @throws RuntimeException
     */
    public function getModulesMetaMenuFilters(Request $req, Response $res, array $args)
    {
        try {
            global $current_user;
            global $sugar_config;
            global $app_strings;

            $config = $this->containers->get('ConfigurationManager');
            $this->negotiatedJsonApiContent($req, $res);

            $payload = array();

            require_once $this->paths->getProjectPath().'/include/GroupedTabs/GroupedTabStructure.php';
            $groupedTabsClass = new GroupedTabStructure();
            $modules = query_module_access_list($current_user);
            //handle with submoremodules
            $max_tabs = $current_user->getPreference('max_tabs');
            // If the max_tabs isn't set incorrectly, set it within the range, to the default max sub tabs size
            if (!isset($max_tabs) || $max_tabs <= 0 || $max_tabs > 10) {
                // We have a default value. Use it
                if (isset($sugar_config['default_max_tabs'])) {
                    $max_tabs = $sugar_config['default_max_tabs'];
                } else {
                    $max_tabs = 8;
                }
            }

            $subMoreModules = false;
            $groupTabs = $groupedTabsClass->get_tab_structure(get_val_array($modules));
            // We need to put this here, so the "All" group is valid for the user's preference.
            $groupTabs[$app_strings['LBL_TABGROUP_ALL']]['modules'] = $fullModuleList;

            // Setup the default group tab.
            $allGroup = $app_strings['LBL_TABGROUP_ALL'];

            // Add url  to modules
            foreach ($modules as $moduleKey => $module) {
                $moduleName = $module;
                $modules[$moduleKey] = array(
                    'type' => $moduleName,
                    'href' => $config['site_url'] . '/api/v'. self::VERSION_MAJOR . '/modules/' . $moduleName . '/',
                    'label' => $moduleKey
                );
            }

            $payload['meta']['menu']['filters'] = array(
                'all' => $modules,
                'tabs' => $groupTabs
            );
        } catch (\Exception $e) {
            $payload = $this->handleExceptionIntoPayloadError($req, $e, isset($payload) ? $payload : []);
        }

        return $this->generateJsonApiResponse($req, $res, $payload);
    }

    /**
     * GET /api/v8/modules/viewed
     * @param Request $req
     * @param Response $res
     * @param array $args
     * @return Response
     * @throws RuntimeException
     */
    public function getModulesMetaViewed(Request $req, Response $res, array $args)
    {
        try {
            $this->negotiatedJsonApiContent($req, $res);

            global $current_user;
            $dateTimeConverter  = $this->containers->get('DateTimeConverter');

            /** @var Tracker $tracker */
            $tracker = BeanFactory::newBean('Trackers');

            $payload = array(
                'data' => array(),
                'included' => array(),
            );

            $recentlyViewedSugarBeans = $tracker->get_recently_viewed($current_user->id);
            foreach ($recentlyViewedSugarBeans as $viewed) {
                // Convert to DB date
                $datetime = $dateTimeConverter->fromUser($viewed['date_modified']);
                if (empty($datetime)) {
                    $datetime = $dateTimeConverter->fromDb($viewed['date_modified']);
                }

                if (empty($datetime)) {
                    throw new ApiException(
                        '[ModulesController] [Unable to convert datetime field from recently viewed] "date_modified"',
                        ExceptionCode::API_DATE_CONVERTION_SUGARBEAN
                    );
                }

                $datetimeISO8601 = $datetime->format(DateTime::ATOM);
                if ($datetime === false) {
                    throw new ApiException(
                        '[ModulesController] [Unable to convert datetime field to ISO 8601] "date_modified"',
                        ExceptionCode::API_DATE_CONVERTION_SUGARBEAN
                    );
                }

                $payload['included'][] = array(
                    'id' => $viewed['item_id'],
                    'type' => $viewed['module_name'],
                    'attributes' => array(
                        'name' => $viewed['item_summary'],
                        'order'=> $viewed['id'],
                        'date_modified' => $datetimeISO8601
                    )
                );
            }
        } catch (\Exception $e) {
            $payload = $this->handleExceptionIntoPayloadError($req, $e, isset($payload) ? $payload : []);
        }

        return $this->generateJsonApiResponse($req, $res, $payload);
    }

    /**
     * GET /api/v8/modules/favorites
     * @param Request $req
     * @param Response $res
     * @param array $args
     * @return Response
     * @throws RuntimeException
     */
    public function getModulesMetaFavorites(Request $req, Response $res, array $args)
    {
        try {
            $this->negotiatedJsonApiContent($req, $res);
            $payload = array(
                'data' => array(),
                'included' => array(),
            );

            /** @var Favorites $favoritesBean */
            $favoritesBean = BeanFactory::newBean('Favorites');
            $favorites = $favoritesBean->getCurrentUserSidebarFavorites(null);

            foreach ($favorites as $favorite) {
                $payload['included'][] = array(
                    'id' => $favorite['id'],
                    'type' => $favorite['module_name'],
                    'attributes' => array(
                        'name' => $favorite['item_summary']
                    )
                );
            }
        } catch (\Exception $e) {
            $payload = $this->handleExceptionIntoPayloadError($req, $e, isset($payload) ? $payload : []);
        }

        return $this->generateJsonApiResponse($req, $res, $payload);
    }

    /**
     * GET /api/v8/modules/{module_name}
     * @param Request $req
     * @param Response $res
     * @param array $args
     * @return Response
     * @throws RuntimeException
     */
    public function getModuleRecords(Request $req, Response $res, array $args)
    {
        try {
            $config = $this->containers->get('ConfigurationManager');

            /** @var ModulesLib $modulesLib; */
            $modulesLib = $this->containers->get('ModulesLib');

            $payload = array(
                'links' => array(),
                'data' => array()
            );

            $this->negotiatedJsonApiContent($req, $res);

            $paginatedModuleRecords = $modulesLib->generatePaginatedModuleRecords($req, $res, $args);
            $payload['data'] = $paginatedModuleRecords['list'];

            $links = $modulesLib->generatePaginatedLinksFromModuleRecords($req, $res, $args, $paginatedModuleRecords);
            $payload['links'] = $links->toJsonApiResponse();

            $page = $req->getParam('page');
            $currentOffset = (integer)$paginatedModuleRecords['current_offset'] < 0 ? 0 : (integer)$paginatedModuleRecords['current_offset'];
            $limit = isset($page['limit']) ? (integer)$page['limit'] : -1;
            $limitOffset = ($limit <= 0) ? $config['list_max_entries_per_page'] : $limit;
            $lastOffset = (integer)floor((integer)$paginatedModuleRecords['row_count'] / $limitOffset);

            $payload['meta']['offsets'] = array(
                'current' => $currentOffset,
                'count' => $lastOffset
            );
        } catch (\Exception $e) {
            $payload = $this->handleExceptionIntoPayloadError($req, $e, isset($payload) ? $payload : []);
        }

        return $this->generateJsonApiResponse($req, $res, $payload);
    }


    /**
     * POST /api/v8/modules/{module}
     * @param Request $req
     * @param Response $res
     * @param array $args
     * @return Response
     * @throws RuntimeException
     */
    public function createModuleRecord(Request $req, Response $res, array $args)
    {
        try {
            $config = $this->containers->get('ConfigurationManager');
            $this->negotiatedJsonApiContent($req, $res);

            $res = $res->withStatus(202);
            $moduleName = $args['module'];
            $module = BeanFactory::newBean($moduleName);
            $body = json_decode($req->getBody()->getContents(), true);
            $payload = array();

            // Validate module
            if (empty($module)) {
                throw new ModuleNotFoundException('Bean factory can not create a new module, module name was: ' . $moduleName);
            }

            // Validate JSON
            if (empty($body)) {
                throw new EmptyBodyException('Request body contents was incorrect, unable to JSON-decode.');
            }

            // Validate Type
            if (!isset($body['data']['type'])) {
                $exception = new ConflictException('[ModuleController] [1] [Request body contents was incorrect, Missing "type" key in data] ');
                $exception->setSource(self::SOURCE_TYPE);

                throw $exception;
            }

            if (isset($body['data']['type']) && $body['data']['type'] !== $module->module_name) {
                $exception = new ConflictException(
                    '[ModuleController] [Request body contents was incorrect, "type" does not match resource type] '.$body['data']['type']. ' !== ' . $moduleName,
                    ExceptionCode::API_MODULE_NOT_FOUND
                );
                $exception->setSource(self::SOURCE_TYPE);
                throw $exception;
            }

            if (
            isset($body['data']['id'])
            && !empty($body['data']['id'])
        ) {
                $beanID = $body['data']['id'];
                $isValidator = new SuiteValidator();
                if (!$isValidator->isValidId($beanID)) {
                    throw new InvalidArgumentException(sprintf('Bean id %s is invalid', $beanID));
                }

                $bean = \BeanFactory::getBean($moduleName, $beanID);

                if ($bean instanceof SugarBean) {
                    throw new IdAlreadyExistsException(sprintf(
                        'Bean id %s already exists in %s module',
                        $beanID,
                        $moduleName
                ), ExceptionCode::API_ID_ALREADY_EXISTS);
                }
            }

            // Handle Request
            /** @var SuiteBeanResource $resource */
            $sugarBeanResource = $this->containers->get('SuiteBeanResource');
            $sugarBean = $sugarBeanResource
                ->fromJsonApiRequest($body['data'])
                ->toSugarBean();

            if (!$sugarBean->ACLAccess('save')) {
                throw new NotAllowedException();
            }

            /** @var Links $links */
            $links = $this->containers->get('Links');
            $self = $config['site_url'] . '/api/' . $req->getUri()->getPath() . '/' . $sugarBean->id;
            $links = $links->withSelf($self);
            $selectFields = $req->getParam('fields');

            /** @var SuiteBeanResource $resource */
            $sugarBeanResource = $sugarBeanResource->fromSugarBean($sugarBean);

            if ($selectFields !== null && isset($selectFields[$moduleName])) {
                $fields = explode(',', $selectFields[$moduleName]);
                $payload['data'] = $sugarBeanResource->toJsonApiResponseWithFields($fields);
            } else {
                $payload['data'] = $sugarBeanResource->toJsonApiResponse();
            }
            $payload['links'] = $links->toJsonApiResponse();
            $res = $res->withStatus(201);
        } catch (\Exception $e) {
            $payload = $this->handleExceptionIntoPayloadError($req, $e, isset($payload) ? $payload : []);
        }

        return $this->generateJsonApiResponse($req, $res, $payload);
    }

    /**
     * GET /api/v8/modules/{module}/{id}
     * @param Request $req
     * @param Response $res
     * @param array $args
     * @return Response
     * @throws RuntimeException
     */
    public function getModuleRecord(Request $req, Response $res, array $args)
    {
        $query = [];
        try {
            if (isset($query['include'])) {
                throw new BadRequestException(
                    '[ModuleController] [include query param is not implemented]',
                    ExceptionCode::API_NOT_IMPLEMENTED
                );
            }

            if (isset($query['filter'])) {
                throw new BadRequestException(
                    '[ModuleController] [filter query param is not implemented]',
                    ExceptionCode::API_NOT_IMPLEMENTED
                );
            }

            $this->negotiatedJsonApiContent($req, $res);
            $res = $res->withStatus(202);
            if (!isset($args['module'])) {
                throw new \InvalidArgumentException('Arguments array should contains a "module" index to describe module name.');
            }
            $moduleName = $args['module'];
            $moduleId = $args['id'];
            $module = BeanFactory::newBean($moduleName);
            $payload = array();

            // Validate module
            if (empty($module)) {
                throw new ModuleNotFoundException('Bean factory can not make a new bean. Module name was: ' . $moduleName);
            }

            $sugarBean = BeanFactory::getBean($moduleName, $moduleId);
            if (!$sugarBean) {
                throw new Exception('Bean Factory can not retrive a bean: ' . $moduleName);
            }
            if ($sugarBean->new_with_id === true) {
                $exception = new NotFoundException('Bean factory can not get a bean with new id. Module name was: ' . $moduleName . ', ' . self::MISSING_ID);
                $exception->setSource('');
                throw $exception;
            }

            if (!$sugarBean->ACLAccess('view')) {
                throw new NotAllowedException();
            }

            // Handle Request
            /** @var SuiteBeanResource $resource */
            $resource = $this->containers->get('SuiteBeanResource');
            $resource = $resource->fromSugarBean($sugarBean);

            // filter fields
            $selectFields = $req->getParam('fields');
            if ($selectFields !== null && isset($selectFields[$moduleName])) {
                $fields = explode(',', $selectFields[$moduleName]);
                $payload['data'] = $resource->toJsonApiResponseWithFields($fields);
            } else {
                $payload['data'] = $resource->toJsonApiResponse();
            }

            $res = $res->withStatus(200);
        } catch (\Exception $e) {
            $payload = $this->handleExceptionIntoPayloadError($req, $e, isset($payload) ? $payload : []);
        }

        return $this->generateJsonApiResponse($req, $res, $payload);
    }

    /**
     *  PATCH /api/v8/modules/{module}/{id}
     * @param Request $req
     * @param Response $res
     * @param array $args
     * @return Response
     * @throws RuntimeException
     */
    public function updateModuleRecord(Request $req, Response $res, array $args)
    {
        try {
            $this->negotiatedJsonApiContent($req, $res);
            $res = $res->withStatus(202);
            $moduleName = $args['module'];
            $moduleId = $args['id'];
            $module = BeanFactory::newBean($moduleName);
            $body = json_decode($req->getBody()->getContents(), true);
            $payload = array();

            // Validate module
            if (empty($module)) {
                throw new ModuleNotFoundException('Bean factory can not retrive a new bean. Module name was: ' . $moduleName);
            }

            // Validate JSON
            if (empty($body)) {
                throw new EmptyBodyException('Request body contents was incorrect, unable to JSON-decode.');
            }

            // Validate Type
            if (!isset($body['data']['type'])) {
                $exception = new ConflictException('[Request body contents was incorrect, Missing "type" key in data]');
                $exception->setSource(self::SOURCE_TYPE);
                throw $exception;
            }

            if (isset($body['data']['type']) && $body['data']['type'] !== $module->module_name) {
                $exception = new ConflictException('[Request body contents was incorrect, "type" does not match with module name]"', ExceptionCode::API_MODULE_NOT_FOUND);
                $exception->setSource(self::SOURCE_TYPE);
                throw $exception;
            }

            // Validate ID
            $sugarBean = BeanFactory::getBean($moduleName, $moduleId);
            if ($sugarBean->new_with_id === true || $sugarBean === false) {
                $exception = new NotFoundException('[ModuleController] ["id" does not exist]');
                $exception->setSource('');
                throw $exception;
            }

            if (!$sugarBean->ACLAccess('save')) {
                throw new NotAllowedException();
            }

            /** @var Resource $resource */
            $resource = $this->containers->get('Resource');
            /** @var SuiteBeanResource $sugarBeanResource */
            $sugarBeanResource = $this->containers->get('SuiteBeanResource');
            $sugarBeanResource = $sugarBeanResource->fromSugarBean($sugarBean);
            $sugarBeanResource->mergeAttributes(
                $resource->fromJsonApiRequest($body['data'])
            );
            $sugarBean = $sugarBeanResource->toSugarBean();
            // Handle Request
            if (empty($sugarBean->save())) {
                throw new ApiException('[ModuleController] [Unable to update record]');
            }

            $sugarBeanResource = $this->containers->get('SuiteBeanResource');
            $sugarBeanResource = $sugarBeanResource->fromSugarBean($sugarBean);
            $selectFields = $req->getParam('fields');

            if ($selectFields !== null && isset($selectFields[$moduleName])) {
                $fields = explode(',', $selectFields[$moduleName]);
                $payload['data'] = $sugarBeanResource->toJsonApiResponseWithFields($fields);
            } else {
                $payload['data'] = $sugarBeanResource->toJsonApiResponse();
            }

            $res = $res->withStatus(200);
        } catch (\Exception $e) {
            $payload = $this->handleExceptionIntoPayloadError($req, $e, isset($payload) ? $payload : []);
        }

        return $this->generateJsonApiResponse($req, $res, $payload);
    }

    /**
     *  DELETE /api/v8/modules/{module}/{id}
     * @param Request $req
     * @param Response $res
     * @param array $args
     * @return Response
     * @throws RuntimeException
     */
    public function deleteModuleRecord(Request $req, Response $res, array $args)
    {
        try {
            $this->negotiatedJsonApiContent($req, $res);
            $res = $res->withStatus(202);
            if (!isset($args['module'])) {
                throw new \InvalidArgumentException('Arguments array should contains a "module" index to describe module name.');
            }
            $moduleName = $args['module'];
            $moduleId = $args['id'];
            $module = BeanFactory::newBean($moduleName);
            $payload = array();

            // Validate module
            if (empty($module)) {
                throw new ModuleNotFoundException('Bean factory can not get a new bean, module name was: ' . $moduleName);
            }

            // Validate ID
            $sugarBean = BeanFactory::getBean($moduleName, $moduleId);
            if ($sugarBean->new_with_id === true) {
                $exception = new NotFoundException('Bean factory can not get a bean. Module name was: ' . $moduleName . ' ' . self::MISSING_ID);
                $exception->setSource('');
                throw $exception;
            }

            if (!$sugarBean->ACLAccess('delete')) {
                throw new NotAllowedException();
            }

            // Handle Request
            $sugarBean->deleted = 1;

            if (empty($sugarBean->save())) {
                throw new ApiException('[Unable to delete record]');
            }

            $payload['meta'] = array(
                'status' => 200
            );
            $res = $res->withStatus(200);
        } catch (\Exception $e) {
            $payload = $this->handleExceptionIntoPayloadError($req, $e, isset($payload) ? $payload : []);
        }

        return $this->generateJsonApiResponse($req, $res, $payload);
    }

    /**
     * GET /api/v8/modules/{id}/meta/language
     *
     * @param Request $req
     * @param Response $res
     * @param array $args
     * @return Response
     * @throws RuntimeException
     */
    public function getModuleMetaLanguage(Request $req, Response $res, array $args)
    {
        $payload = [];
        try {
            if (!isset($args['module'])) {
                throw new \InvalidArgumentException('Arguments array should contains a "module" index to describe module name.');
            }
            $this->negotiatedJsonApiContent($req, $res);

            $currentLanguage = $this->containers->get('CurrentLanguage');
            $moduleLanguage = $this->containers->get('ModuleLanguage');
            $moduleLanguageStrings = $moduleLanguage->getModuleLanguageStrings($currentLanguage, $args['module']);

            $payload['meta'][$args['module']]['language'] = $moduleLanguageStrings;
        } catch (\Exception $e) {
            $payload = $this->handleExceptionIntoPayloadError($req, $e, isset($payload) ? $payload : []);
        }

        return $this->generateJsonApiResponse($req, $res, $payload);
    }


    /**
     * GET /api/v8/meta/language
     *
     * @param Request $req
     * @param Response $res
     * @param array $args
     * @return Response
     * @throws RuntimeException
     */
    public function getApplicationMetaLanguages(Request $req, Response $res, array $args)
    {
        $payload = [];
        try {
            $this->negotiatedJsonApiContent($req, $res);

            $currentLanguage = $this->containers->get('CurrentLanguage');
            /** @var ApplicationLanguage $moduleLanguage */
            $applicationLanguage = $this->containers->get('ApplicationLanguages');

            $payload['meta']['application']['language'] =
                $applicationLanguage->getApplicationLanguageStrings($currentLanguage);
        } catch (\Exception $e) {
            $payload = $this->handleExceptionIntoPayloadError($req, $e, isset($payload) ? $payload : []);
        }

        return $this->generateJsonApiResponse($req, $res, $payload);
    }

    /**
     * GET /api/v8/modules/{id}/meta/attributes
     *
     * @param Request $req
     * @param Response $res
     * @param array $args
     * @return Response
     * @throws RuntimeException
     */
    public function getModuleMetaAttributes(Request $req, Response $res, array $args)
    {
        $payload = [];
        try {
            if (!isset($args['module'])) {
                throw new \InvalidArgumentException('Arguments array should contains a "module" index to describe module name.');
            }

            $this->negotiatedJsonApiContent($req, $res);

            $payload['meta'][$args['module']]['attributes'] = BeanFactory::getBean($args['module'])->field_defs;
        } catch (\Exception $e) {
            $payload = $this->handleExceptionIntoPayloadError($req, $e, isset($payload) ? $payload : []);
        }

        return $this->generateJsonApiResponse($req, $res, $payload);
    }

    /**
     * see: getModuleMetaAttributes
     *
     * @param Request $req
     * @param Response $res
     * @param array $args
     * @return Response
     * @throws InvalidJsonApiResponseException
     * @throws \InvalidArgumentException
     * @throws NotFoundExceptionInterface
     * @throws ContainerExceptionInterface
     * @throws UnsupportedMediaTypeException
     * @throws NotAcceptableException
     */
    public function getModuleMetaFields(Request $req, Response $res, array $args)
    {
        return $this->getModuleMetaAttributes($req, $res, $args);
    }

    /**
     * GET /api/v8/modules/{id}/meta/menu
     *
     * @param Request $req
     * @param Response $res
     * @param array $args
     * @return Response
     * @throws RuntimeException
     */
    public function getModuleMetaMenu(Request $req, Response $res, array $args)
    {
        $payload = [];
        try {
            if (!isset($args['module'])) {
                throw new \InvalidArgumentException('Arguments array should contains a "module" index to describe module name.');
            }
            $this->negotiatedJsonApiContent($req, $res);

            $sugarView = new SugarView();
            $menu = $sugarView->getMenu($args['module']);

            $config = $this->containers->get('ConfigurationManager');

            $self = $config['site_url'] . '/api/v'. self::VERSION_MAJOR . '/modules/' . $args['module'] . '/';
            $results = array();
            foreach ($menu as $item) {
                $url = parse_url((string) $item[0]);
                parse_str($url['query'], $orig);
                $results[] = array(
                    'href' => $self . $item[2],
                    'label' => $item[1],
                    'action' => $item[2],
                    'module' => $item[3],
                    'type' => $item[3],
                    'query' => $orig,
                );
            }

            $payload['meta'][$args['module']]['menu'] = $results;
        } catch (\Exception $e) {
            $payload = $this->handleExceptionIntoPayloadError($req, $e, isset($payload) ? $payload : []);
        }

        return $this->generateJsonApiResponse($req, $res, $payload);
    }

    /**
     * GET /api/v8/modules/{module}/viewed
     * @param Request $req
     * @param Response $res
     * @param array $args
     * @throws RuntimeException
     */
    public function getModuleRecordsViewed(Request $req, Response $res, array $args)
    {
        try {
            if (!isset($args['module'])) {
                throw new \InvalidArgumentException('Arguments array should contains a "module" index to describe module name.');
            }
            $this->negotiatedJsonApiContent($req, $res);

            global $current_user;
            $dateTimeConverter  = $this->containers->get('DateTimeConverter');

            /** @var Tracker $tracker */
            $tracker = BeanFactory::newBean('Trackers');

            $payload = array(
                'data' => array(),
                'included' => array(),
            );

            $recentlyViewedSugarBeans = $tracker->get_recently_viewed($current_user->id, $args['module']);
            foreach ($recentlyViewedSugarBeans as $viewed) {
                // Convert to DB date
                $datetime = $dateTimeConverter->fromUser($viewed['date_modified']);
                if (empty($datetime)) {
                    $datetime = $dateTimeConverter->fromDb($viewed['date_modified']);
                }

                if (empty($datetime)) {
                    throw new ApiException(
                        '[ModulesController] [Unable to convert datetime field from recently viewed] "date_modified"',
                        ExceptionCode::API_DATE_CONVERTION_SUGARBEAN
                    );
                }

                $datetimeISO8601 = $datetime->format(DateTime::ATOM);

                $payload['included'][] = array(
                    'id' => $viewed['item_id'],
                    'type' => $viewed['module_name'],
                    'attributes' => array(
                        'name' => $viewed['item_summary'],
                        'order'=> $viewed['id'],
                        'date_modified' => $datetimeISO8601
                    )
                );
            }
        } catch (\Exception $e) {
            $payload = $this->handleExceptionIntoPayloadError($req, $e, isset($payload) ? $payload : []);
        }

        return $this->generateJsonApiResponse($req, $res, $payload);
    }

    /**
     * GET /api/v8/modules/favorites
     * @param Request $req
     * @param Response $res
     * @param array $args
     * @throws RuntimeException
     */
    public function getModuleFavorites(Request $req, Response $res, array $args)
    {
        try {
            if (!isset($args['module'])) {
                throw new \InvalidArgumentException('Arguments array should contains a "module" index to describe module name.');
            }
            $this->negotiatedJsonApiContent($req, $res);
            $payload = array();

            /** @var Favorites $favoritesBean */
            $favoritesBean = BeanFactory::newBean('Favorites');
            $payload['data'] = $favoritesBean->getCurrentUserFavoritesForModule($args['module']);
        } catch (\Exception $e) {
            $payload = $this->handleExceptionIntoPayloadError($req, $e, isset($payload) ? $payload : []);
        }

        return $this->generateJsonApiResponse($req, $res, $payload);
    }

    /**
     * GET /api/v8/modules/{module}/meta/view/{view}
     * @see MBConstants for {view}
     * @param Request $req
     * @param Response $res
     * @param array $args
     * @return Response
     * @throws RuntimeException
     */
    public function getModuleMetaLayout(Request $req, Response $res, array $args)
    {
        $payload = [];
        try {
            if (!isset($args['module'])) {
                throw new \InvalidArgumentException('Arguments array should contains a "module" index to describe module name.');
            }
            if (!isset($args['view'])) {
                throw new \InvalidArgumentException('Arguments array should contains a "view" index.');
            }
            $this->negotiatedJsonApiContent($req, $res);
            /** @var SugarBean $bean */
            $sugarBean = BeanFactory::newBean($args['module']);

            if (empty($sugarBean)) {
                throw new NotFoundException(
                    '[ModuleController] [Module does not exist] ' . $args['module'],
                    ExceptionCode::API_MODULE_NOT_FOUND
                );
            }

            require_once $this->paths->getProjectPath().'/modules/ModuleBuilder/parsers/ParserFactory.php';
            $parser = ParserFactory::getParser($args['view'], $args['module']);
            $viewdefs = $parser->_viewdefs;

            if (empty($viewdefs)) {
                throw new NotFoundException(
                    '[ModuleController] [ViewDefinitions does not exist] ' . $args['view'],
                    ExceptionCode::API_VIEWDEF_NOT_FOUND
                );
            }

            $payload['meta'][$args['module']]['view'][$args['view']] = $viewdefs;
        } catch (\Exception $e) {
            $payload = $this->handleExceptionIntoPayloadError($req, $e, isset($payload) ? $payload : []);
        }

        return $this->generateJsonApiResponse($req, $res, $payload);
    }


    /**
     * GET /api/v8/modules/{id}/relationships/{link}
     * @param Request $req
     * @param Response $res
     * @param array $args
     * @see http://jsonapi.org/format/1.0/#fetching-relationships
     * @throws RuntimeException
     */
    public function getModuleRelationship(Request $req, Response $res, array $args)
    {
        try {
            if (!isset($args['module'])) {
                throw new \InvalidArgumentException('Arguments array should contains a "module" index to describe module name.');
            }
            if (!isset($args['id'])) {
                throw new \InvalidArgumentException('Arguments array should contains a "id" index.');
            }
            if (!isset($args['link'])) {
                throw new \InvalidArgumentException('Arguments array should contains a "link" index.');
            }

            $query = $req->getQueryParams('include');

            if (isset($query['include'])) {
                throw new BadRequestException('[ModuleController] [include query param is not implemented]', ExceptionCode::API_NOT_IMPLEMENTED);
            }

            if (isset($query['filter'])) {
                throw new BadRequestException('[ModuleController] [filter query param is not implemented]', ExceptionCode::API_NOT_IMPLEMENTED);
            }

            $config = $this->containers->get('ConfigurationManager');
            $this->negotiatedJsonApiContent($req, $res);
            $payload = array(
                'data' => array()
            );
            $sugarBean = BeanFactory::getBean($args['module'], $args['id']);

            if (empty($sugarBean)) {
                throw new NotFoundException(
                    '[ModuleController] [Record does not exist] ' . $args['link'],
                    ExceptionCode::API_RECORD_NOT_FOUND
                );
            }

            if (!$sugarBean->ACLAccess('view')) {
                throw new NotFoundException('[Record]');
            }

            if ($sugarBean->load_relationship($args['link']) === false) {
                throw new NotFoundException(
                    '[ModuleController] [Relationship does not exist] ' . $args['link'],
                    ExceptionCode::API_RELATIONSHIP_NOT_FOUND
            );
            }

            $relationshipType = $sugarBean->{$args['link']}->focus->{$args['link']}->relationship->type;

            /** @var Link2 $sugarBeanRelationship */
            $sugarBeanRelationship = $sugarBean->{$args['link']};

            $sugarBeanRelationshipType = $sugarBeanRelationship->getType();

            switch ($sugarBeanRelationshipType) {
                case 'one':
                    // to one
                    $relatedIds = $sugarBean->{$args['link']}->get();
                    $relatedDefinition = $sugarBean->{$args['link']}->focus->{$args['link']}->relationship->def;

                    if (!isset($relatedDefinition['lhs_module'])) {
                        throw new \Exception('Related definition should contains a "lhs_module" index.');
                    }

                    foreach ($relatedIds as $id) {
                        // only needs one result
                        $data = array(
                            'type' => $relatedDefinition['lhs_module'],
                            'id' => $id
                        );

                        $links = new Links();
                        $data['links'] = $links
                            ->withHref(
                                $config['site_url'] . '/api/v'. self::VERSION_MAJOR . '/modules/'.
                                $relatedDefinition['lhs_module'].'/'.$id
                            )
                            ->toJsonApiResponse();

                        $payload['data'] = $data;
                    }
                    break;
                case 'many':
                    // to many
                    /** @var Resource $resource */
                    $resource = $this->containers->get('Resource');
                    $related = $sugarBeanRelationship->query(
                        array(
                              'include_middle_table_fields' => true
                         )
                    );
                    $relatedDefinition = $sugarBean->field_defs[$args['link']];
                    $relatedType = $sugarBeanRelationship->getRelatedModuleName();
                    foreach ($related['rows'] as $row) {
                        if (!isset($row['id'])) {
                            throw new \Exception('Related definition should contains "id" index.');
                        }

                        $data = array(
                            'id' => $row['id'],
                            'type' => $relatedType
                       );

                        $meta = array(
                            'middle_table' => array(
                                 'data' => array(
                                    'id' => '',
                                    'type' => 'Link',
                                    'attributes' => $row
                                 )
                            )
                       );

                        $links = new Links();
                        $data['links'] = $links
                            ->withHref(
                                $config['site_url'] . '/api/v'. self::VERSION_MAJOR . '/modules/'.
                                $args['module'] . '/' . $row['id']
                            )
                            ->toJsonApiResponse();

                        $data['meta'] = $meta;
                        $payload['data'][] = $data;
                    }

                    if (isset($sugarBeanRelationship->relationship->def['fields'])) {
                        $payload['meta']['attributes'] = $middleTableFieldDefs =  $sugarBeanRelationship->relationship->def['fields'];
                    }
                    break;
                default:
                    throw new BadRequestException('[ModuleController] [Relationship type not supported] type was: ' . $sugarBeanRelationshipType);
            }

            $payload['meta']['relationships']['type'] = $relationshipType;

            $links = new Links();
            $payload['links'] = $links
                ->withSelf(
                    $config['site_url'] . '/api/v'. self::VERSION_MAJOR . '/modules/'.
                    $args['module'].'/'.$args['id'].'/relationships/'.$args['link']
                )
                ->toJsonApiResponse();
        } catch (\Exception $e) {
            $payload = $this->handleExceptionIntoPayloadError($req, $e, isset($payload) ? $payload : []);
        }

        $this->generateJsonApiResponse($req, $res, $payload);
    }

    /**
     * POST /api/v8/modules/{id}/relationships/{link}
     * @see http://jsonapi.org/format/1.0/#crud-creating
     * @param Request $req
     * @param Response $res
     * @param array $args
     * @return Response
     * @throws RuntimeException
     */
    public function createModuleRelationship(Request $req, Response $res, array $args)
    {
        $additional_fields = [];
        $link = [];
        $responsePayload = [];
        try {
            if (!isset($args['module'])) {
                throw new \InvalidArgumentException('Arguments array should contains a "module" index to describe module name.');
            }
            if (!isset($args['id'])) {
                throw new \InvalidArgumentException('Arguments array should contains a "id" index.');
            }
            if (!isset($args['link'])) {
                throw new \InvalidArgumentException('Arguments array should contains a "link" index.');
            }

            $this->negotiatedJsonApiContent($req, $res);

            $sugarBean = BeanFactory::getBean($args['module'], $args['id']);

            if ($sugarBean->new_with_id === true) {
                $exception = new NotFoundException(self::MISSING_ID);
                $exception->setSource('');
                throw $exception;
            }

            if ($sugarBean === false) {
                $exception = new NotFoundException('[ModuleController] [Unable to find SugarBean] /modules/'.$args['module'].'/'.$args['id']);
                $exception->setDetail('Please ensure that the module name and the id is correct.');
                $exception->setSource('');
                throw $exception;
            }

            if ($sugarBean->load_relationship($args['link']) === false) {
                throw new NotFoundException(
                    '[ModuleController] [Relationship does not exist] ' . $args['link'],
                    ExceptionCode::API_RELATIONSHIP_NOT_FOUND
                );
            }

            if (!$sugarBean->ACLAccess('save')) {
                throw new NotAllowedException('[Record]');
            }

            /** @var Link2 $sugarBeanRelationship */
            $sugarBeanRelationship = $sugarBean->{$args['link']};

            $requestPayload = json_decode($req->getBody(), true);

            // Validate JSON
            if (empty($requestPayload)) {
                throw new EmptyBodyException('Unable to JSON decode the requested body.');
            }

            /** @var Relationship $relationship */
            $relationship = $this->containers->get('Relationship');
            $relationship->setRelationshipName($args['link']);
            $sugarBeanRelationshipTypeFromSugarBeanLink = SugarBeanRelationshipType::fromSugarBeanLink($sugarBeanRelationship);

            $relationship->setRelationshipType(
                    // $sugarBeanRelationshipTypeFromSugarBeanLink could be invalid, see below..
                $sugarBeanRelationshipTypeFromSugarBeanLink
            );

            switch ($sugarBeanRelationshipTypeFromSugarBeanLink) {
                case RelationshipType::TO_MANY:

                    if (!isset($requestPayload['data'])) {
                        throw new \InvalidArgumentException('Requested payload should contains a "data" attribute.');
                    }

                    $data = $requestPayload['data'];
                    $links = array();


                    // if a single ResourceIdentifier has been posted
                    if (!isset($data[0])) {
                        // convert to array
                        $data = array($data);
                    }

                    foreach ($data as $link) {
                        if (!isset($link['id'])) {
                            throw new \InvalidArgumentException('Arguments array should contains a "id" index.');
                        }
                        $links[] = $link['id'];
                        /** @var ResourceIdentifier $resourceIdentifier */
                        $resourceIdentifier = $this->containers->get('ResourceIdentifier');

                        $meta = null;
                        $additional_fields = array();
                        if (
                            isset($link['meta']['middle_table']['data']['attributes']) &&
                            !empty($link['meta']['middle_table']['data']['attributes'])
                        ) {
                            $additional_fields = $link['meta']['middle_table']['data']['attributes'];
                            $meta = array(
                                'middle_table' => array(
                                    'data' => array(
                                        'id' => '',
                                        'type' => 'Link',
                                        'attributes' => $link['meta']['middle_table']['data']['attributes']
                                    )
                                )
                            );
                        }

                        if (!isset($link['type'])) {
                            throw new \InvalidArgumentException('Arguments array should contains a "type" index.');
                        }
                        $relationship = $relationship
                            ->withResourceIdentifier(
                                $resourceIdentifier
                                    ->withId($link['id'])
                                    ->withType($link['type'])
                                    ->withMeta($meta)
                            );
                    }

                    $added = $sugarBeanRelationship->add($links, $additional_fields);
                    if ($added !== true) {
                        throw new ConflictException('[ModuleController] [Unable to add relationships (to many)] ' . json_encode($added));
                    }
                    break;
                case RelationshipType::TO_ONE:
                    $resourceIdentifier = $this->containers->get('ResourceIdentifier');

                    if (!isset($requestPayload['data']['id'])) {
                        throw new \InvalidArgumentException('Requested payload date should contains an "id".');
                    }

                    if (empty($requestPayload['data'])) {
                        $relationship = $relationship
                            ->withResourceIdentifier(
                                $resourceIdentifier
                            );
                    } else {
                        if (!isset($requestPayload['data']['type'])) {
                            throw new \InvalidArgumentException('Requested payload date should contains a "type".');
                        }
                        $relationship = $relationship
                            ->withResourceIdentifier(
                                $resourceIdentifier
                                    ->withId($requestPayload['data']['id'])
                                    ->withType($requestPayload['data']['type'])
                            );
                    }

                    $additional_fields = array();
                    if (
                        isset($link['meta']['middle_table']['data']['attributes']) &&
                        !empty($link['meta']['middle_table']['data']['attributes'])
                    ) {
                        $additional_fields = $link['meta']['middle_table']['data']['attributes'];
                    }

                    $sugarBeanRelationship->add($requestPayload['data']['id'], $additional_fields);
                    break;
                default:
                    throw new ForbiddenException('[ModuleController] [Invalid Relationship type]');

            }


            /** @var SuiteBeanResource $sugarBeanResource */
            $sugarBeanResource = $this->containers->get('SuiteBeanResource');
            $sugarBeanResource = $sugarBeanResource
                ->fromSugarBean($sugarBean);

            $sugarBean = $sugarBeanResource->toSugarBean();
            $sugarBean->retrieve($sugarBeanResource->getId());

            $responsePayload['data'] = $relationship->toJsonApiResponse();
        } catch (\Exception $e) {
            $responsePayload = $this->handleExceptionIntoPayloadError($req, $e, isset($responsePayload) ? $responsePayload : []);
        }

        return $this->generateJsonApiResponse($req, $res, $responsePayload);
    }

    /**
     * Replaces related items with request
     * PATCH /api/v8/modules/{id}/relationships/{link}/{id}
     * Note: Clear all related links with empty data payload such as null, [] etc.
     * @see http://jsonapi.org/format/1.0/#crud-updating-relationships
     * @param Request $req
     * @param Response $res
     * @param array $args
     * @return Response
     * @throws RuntimeException
     */
    public function updateModuleRelationship(Request $req, Response $res, array $args)
    {
        $responsePayload = [];
        try {
            if (!isset($args['module'])) {
                throw new \InvalidArgumentException('Arguments array should contains a "module" index to describe module name.');
            }
            if (!isset($args['id'])) {
                throw new \InvalidArgumentException('Arguments array should contains a "id" index.');
            }
            if (!isset($args['link'])) {
                throw new \InvalidArgumentException('Arguments array should contains a "link" index.');
            }

            $this->negotiatedJsonApiContent($req, $res);

            $sugarBean = BeanFactory::getBean($args['module'], $args['id']);

            if ($sugarBean->new_with_id === true) {
                $exception = new NotFoundException(self::MISSING_ID);
                $exception->setSource('');
                throw $exception;
            }

            if ($sugarBean === false) {
                $exception = new NotFoundException('[ModuleController] [Unable to find SugarBean] /modules/'.$args['module'].'/'.$args['id']);
                $exception->setDetail('Please ensure that the module name and the id is correct.');
                $exception->setSource('');
                throw $exception;
            }

            if ($sugarBean->load_relationship($args['link']) === false) {
                throw new NotFoundException(
                    '[ModuleController] [Relationship does not exist] ' . $args['link'],
                    ExceptionCode::API_RELATIONSHIP_NOT_FOUND
                );
            }

            if (!$sugarBean->ACLAccess('save')) {
                throw new NotAllowedException('[Record]');
            }

            /** @var \Link2 $sugarBeanRelationship */
            $sugarBeanRelationship = $sugarBean->{$args['link']};

            $requestPayload = json_decode($req->getBody(), true);

            // Validate JSON
            if (empty($requestPayload)) {
                throw new EmptyBodyException('Invalid Request Payload given, unable to JSON decode body');
            }

            /** @var Relationship $relationship */
            $relationship = $this->containers->get('Relationship');
            $relationship->setRelationshipName($args['link']);
            $relationship->setRelationshipType(
                SugarBeanRelationshipType::fromSugarBeanLink($sugarBeanRelationship)
            );

            if (!isset($requestPayload['data'])) {
                throw new \InvalidArgumentException('Request Payload should contains a "data"');
            }

            if (SugarBeanRelationshipType::fromSugarBeanLink($sugarBeanRelationship) === RelationshipType::TO_MANY) {
                $data = $requestPayload['data'];
                // if a single ResourceIdentifier has been posted
                if (!isset($data[0])) {
                    // convert to array
                    $data = array($data);
                }
                foreach ($data as $link) {
                    /** @var ResourceIdentifier $resourceIdentifier */
                    $resourceIdentifier = $this->containers->get('ResourceIdentifier');

                    $meta = null;
                    if (
                        isset($link['meta']['middle_table']['data']['attributes']) &&
                        !empty($link['meta']['middle_table']['data']['attributes'])
                    ) {
                        $meta = array(
                            'middle_table' => array(
                                'data' => array(
                                    'id' => '',
                                    'type' => 'Link',
                                    'attributes' => $link['meta']['middle_table']['data']['attributes']
                                )
                            )
                        );
                    }

                    // The following removes PHP notice: Undefined index
                    $linkId = isset($link['id']) ? $link['id'] : null;
                    $linkType = isset($link['type']) ? $link['type'] : null;

                    $relationship = $relationship
                        ->withResourceIdentifier(
                            $resourceIdentifier
                                ->withId($linkId)
                                ->withType($linkType)
                                ->withMeta($meta)
                        );
                }
            } elseif (SugarBeanRelationshipType::fromSugarBeanLink($sugarBeanRelationship) === RelationshipType::TO_ONE) {
                /** @var ResourceIdentifier $resourceIdentifier */
                $resourceIdentifier = $this->containers->get('ResourceIdentifier');

                if (empty($requestPayload['data'])) {
                    $relationship = $relationship
                        ->withResourceIdentifier(
                            $resourceIdentifier
                        );
                } else {
                    if (!isset($requestPayload['data']['id'])) {
                        throw new \InvalidArgumentException('Request Payload "data" should contains an "id"');
                    }
                    if (!isset($requestPayload['data']['type'])) {
                        throw new \InvalidArgumentException('Request Payload "data" should contains an "type"');
                    }
                    $relationship = $relationship
                        ->withResourceIdentifier(
                            $resourceIdentifier
                                ->withId($requestPayload['data']['id'])
                                ->withType($requestPayload['data']['type'])
                        );
                }
            }


            /** @var SuiteBeanResource $sugarBeanResource */
            $sugarBeanResource = $this->containers->get('SuiteBeanResource');
            $sugarBeanResource = $sugarBeanResource
                ->fromSugarBean($sugarBean)
                ->withRelationship($relationship);

            $sugarBean = $sugarBeanResource->toSugarBean();
            $sugarBean->retrieve($sugarBeanResource->getId());

            $responsePayload = array();
            $responsePayload['data'] = $sugarBeanResource->getRelationshipByName($args['link']);
        } catch (\Exception $e) {
            $payload = $this->handleExceptionIntoPayloadError($req, $e, isset($payload) ? $payload : []);
        }

        return $this->generateJsonApiResponse($req, $res, $responsePayload);
    }

    /**
     * DELETE /api/v8/modules/{id}/relationships/{link}/{id}
     * @param Request $req
     * @param Response $res
     * @param array $args
     * @see http://jsonapi.org/format/1.0/#crud-updating-relationships
     * @return Response
     * @throws RuntimeException
     */
    public function deleteModuleRelationship(Request $req, Response $res, array $args)
    {
        try {
            if (!isset($args['module'])) {
                throw new \InvalidArgumentException('Arguments array should contains a "module" index to describe module name.');
            }
            if (!isset($args['id'])) {
                throw new \InvalidArgumentException('Arguments array should contains a "id" index.');
            }
            if (!isset($args['link'])) {
                throw new \InvalidArgumentException('Arguments array should contains a "link" index.');
            }

            $this->negotiatedJsonApiContent($req, $res);

            $sugarBean = BeanFactory::getBean($args['module'], $args['id']);

            if ($sugarBean->new_with_id === true) {
                $exception = new NotFoundException(self::MISSING_ID);
                $exception->setSource('');
                throw $exception;
            }

            if ($sugarBean === false) {
                $exception = new NotFoundException('[ModuleController] [Unable to find SugarBean] /modules/'.$args['module'].'/'.$args['id']);
                $exception->setDetail('Please ensure that the module name and the id is correct.');
                $exception->setSource('');
                throw $exception;
            }

            if ($sugarBean->load_relationship($args['link']) === false) {
                throw new NotFoundException(
                    '[ModuleController] [Relationship does not exist] ' . $args['link'],
                    ExceptionCode::API_RELATIONSHIP_NOT_FOUND
                );
            }

            /** @var \Link2 $sugarBeanRelationship */
            $sugarBeanRelationship = $sugarBean->{$args['link']};

            $requestPayload = json_decode($req->getBody(), true);

            /** @var Relationship $relationship */
            $relationship = $this->containers->get('Relationship');
            $relationship->setRelationshipName($args['link']);
            $relationship->setRelationshipType(
                SugarBeanRelationshipType::fromSugarBeanLink($sugarBeanRelationship)
            );

            if (SugarBeanRelationshipType::fromSugarBeanLink($sugarBeanRelationship) === RelationshipType::TO_MANY) {
                if (empty($requestPayload['data'])) {
                    $sugarBeanRelationship->getRelationshipObject()->removeAll($sugarBeanRelationship);
                } else {
                    $data = $requestPayload['data'];
                    // if a single ResourceIdentifier has been posted
                    if (!isset($data[0])) {
                        // convert to array
                        $data = array($data);
                    }
                    $links = array();
                    foreach ($data as $link) {
                        $links[] = $link['id'];
                    }

                    $removed = $sugarBeanRelationship->remove($links);
                    if ($removed !== true) {
                        throw new ConflictException(
                            '[ModuleController] [Unable to remove relationships (to many)]' . json_encode($removed)
                        );
                    }
                }
            } elseif (SugarBeanRelationshipType::fromSugarBeanLink($sugarBeanRelationship) === RelationshipType::TO_ONE) {
                if (empty($requestPayload['data'])) {
                    $sugarBeanRelationship->getRelationshipObject()->removeAll($sugarBeanRelationship);
                } else {
                    if (!isset($requestPayload['data']['id'])) {
                        throw new \InvalidArgumentException('Requested payload date should contains a "id".');
                    }
                    $sugarBeanRelationship->remove($requestPayload['data']['id']);
                }
            } else {
                throw new ForbiddenException('[ModuleController] [Invalid Relationship type]');
            }

            $responsePayload = array();
            $responsePayload['data'] = array();
        } catch (\Exception $e) {
            $responsePayload = $this->handleExceptionIntoPayloadError($req, $e, isset($responsePayload) ? $responsePayload : []);
        }

        return $this->generateJsonApiResponse($req, $res->withStatus(204), $responsePayload);
    }
}
