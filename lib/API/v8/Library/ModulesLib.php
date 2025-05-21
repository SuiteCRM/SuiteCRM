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

namespace SuiteCRM\API\v8\Library;

use Psr\Container\ContainerInterface;
use Psr\Http\Message\ServerRequestInterface as Request;
use Psr\Http\Message\ResponseInterface as Response;
use League\Uri\Components\Query;
use SuiteCRM\API\JsonApi\v1\Filters\Interpreters\FilterInterpreter;
use SuiteCRM\API\JsonApi\v1\Filters\Interpreters\SuiteInterpreter;
use SuiteCRM\API\JsonApi\v1\Links;
use SuiteCRM\API\JsonApi\v1\Repositories\FilterRepository;
use SuiteCRM\API\JsonApi\v1\Resource\SuiteBeanResource;
use SuiteCRM\API\v8\Exception\BadRequestException;
use SuiteCRM\API\v8\Exception\ModuleNotFoundException;

/**
 * Class ModulesLib
 * @package SuiteCRM\API\v8\Library
 */
#[\AllowDynamicProperties]
class ModulesLib
{

    /**
     * @var ContainerInterface
     */
    private $containers;

    /**
     * ModulesLib constructor.
     * @param ContainerInterface $containers
     */
    public function __construct($containers)
    {
        $this->containers = $containers;
    }

    /**
     * @param Request $req
     * @param Response $res
     * @param array $args
     * @return array list => SugarBean[], current_offset => 0, row_count => 0
     * @throws BadRequestException
     * @throws ModuleNotFoundException
     * @throws NotAllowed
     * @throws \Psr\Container\ContainerExceptionInterface
     * @throws \Psr\Container\NotFoundExceptionInterface
     * @throws \SuiteCRM\API\JsonApi\v1\Filters\Interpreters\InvalidArgumentException
     * @throws \SuiteCRM\API\v8\Exception\ApiException
     * @throws \SuiteCRM\Exception\Exception
     */
    public function generatePaginatedModuleRecords(Request $req, Response $res, array $args = array())
    {
        /** @var array $response */
        $response = array();

        /** @var \SugarBean $module */
        $module = \BeanFactory::newBean($args['module']);

        if ($module === false) {
            throw new ModuleNotFoundException('"' . $args['module'] . '"');
        }

        if (!$module->ACLAccess('list')) {
            throw new NotAllowed();
        }

        $moduleList = $this->getModuleList($req, $module, $args);

        $fields = array('fields' => array());
        $selectFields = $req->getParam('fields');

        /** @var array $config */
        $config = $this->containers->get('ConfigurationManager');

        /** @var \SugarBean $moduleBean */
        foreach ($moduleList['list'] as $moduleBean) {
            // Create data item
            if (isset($selectFields[$moduleBean->module_name])) {
                // Only return the fields requested
                $fields['fields'][$moduleBean->module_name] = explode(',', $selectFields[$moduleBean->module_name]);
            } else {
                $fields['fields'][$moduleBean->module_name] = $moduleBean->column_fields;
            }

            // Add attributes
            /** @var SuiteBeanResource $resource */
            $resource = $this->containers->get('SuiteBeanResource');
            $resource = $resource->fromSugarBean($moduleBean);
            $bean = $resource->toJsonApiResponseWithFields($fields['fields'][$moduleBean->module_name]);

            // Add links object to $bean
            $bean['links'] =
                Links::get()
                    ->withSelf($config['site_url'] . '/api/' . $req->getUri()->getPath() . '/' . $moduleBean->id)
                    ->toJsonApiResponse();

            // Append bean to resource object in the response
            $response['list'][] = $bean;
        }
        $response['current_offset'] = $moduleList['current_offset'];
        $response['row_count'] = $moduleList['row_count'];

        return $response;
    }

    /**
     * @see ModulesLib::generatePaginatedLinksFromModuleRecords
     * @param Request $req
     * @param Response $res
     * @param array $args
     * @param array $paginatedModuleRecords return value from ModulesLib::generatePaginatedLinksFromModuleRecords
     * @return Links
     * @throws \Psr\Container\ContainerExceptionInterface
     * @throws \Psr\Container\NotFoundExceptionInterface
     */
    public function generatePaginatedLinksFromModuleRecords(Request $req, Response $res, $args, $paginatedModuleRecords)
    {
        $config = $this->containers->get('ConfigurationManager');
        $page = $req->getParam('page');
        $limit = isset($page['limit']) ? (integer)$page['limit'] : -1;
        $sort = $req->getParam('sort');
        $filter = $req->getParam('filter');
        $fields = $req->getParam('fields');
        $currentOffset = (integer)$paginatedModuleRecords['current_offset'] < 0 ? 0 : (integer)$paginatedModuleRecords['current_offset'];
        $firstOffset = 0;
        $limitOffset = ($limit <= 0) ? $config['list_max_entries_per_page'] : $limit;
        $lastOffset = (integer)floor((integer)$paginatedModuleRecords['row_count'] / $limitOffset);
        $prevOffset = $currentOffset - 1 < $firstOffset ? $firstOffset : $currentOffset - 1;
        $nextOffset = $currentOffset + 1 > $lastOffset ? $lastOffset : $currentOffset + 1;

        $links = Links::get()->withPagination();
        $links->withSelf($this->generatePaginationUrl($req, $currentOffset, $limitOffset, $filter, $sort, $fields));

        if ($firstOffset !== $lastOffset) {
            if ($currentOffset !== $firstOffset) {
                $links->withFirst(
                    $this->generatePaginationUrl(
                        $req,
                        $firstOffset,
                        $limitOffset,
                        $filter,
                        $sort,
                        $fields
                    )
                );
            }

            if ($currentOffset !== $lastOffset) {
                $links->withLast(
                    $this->generatePaginationUrl(
                        $req,
                        $lastOffset,
                        $limitOffset,
                        $filter,
                        $sort,
                        $fields
                    )
                );
            }

            if ($currentOffset > $firstOffset) {
                $links->withPrev(
                    $this->generatePaginationUrl(
                        $req,
                        $prevOffset,
                        $limitOffset,
                        $filter,
                        $sort,
                        $fields
                    )
                );
            }

            if ($currentOffset < $lastOffset) {
                $links->withNext(
                    $this->generatePaginationUrl(
                        $req,
                        $nextOffset,
                        $limitOffset,
                        $filter,
                        $sort,
                        $fields
                    )
                );
            }
        }

        return $links;
    }

    /**
     * Handle sorting in the request
     *
     * @param Request $req
     * @return string
     * @throws \Psr\Container\ContainerExceptionInterface
     * @throws \Psr\Container\NotFoundExceptionInterface
     */
    protected function getSorting(Request $req)
    {
        $db = $this->containers->get('DatabaseManager');
        $orderBy = '';
        if (!empty($req->getParam('sort'))) {
            $sortField = explode(',', $req->getParam('sort'));
            foreach ($sortField as $sortKey => $sortValue) {
                if ($sortValue[0] === '-') {
                    $sortField[$sortKey] = $db->quote(substr($sortValue, 1)) . ' DESC';
                } else {
                    if ($sortValue[0] === '+') {
                        $sortField[$sortKey] = $db->quote(substr($sortValue, 1)) . ' ASC';
                    } else {
                        $sortField[$sortKey] = $db->quote($sortValue) . ' ASC';
                    }
                }
            }
            $orderBy = implode(',', $sortField);
        }

        return $orderBy;
    }

    /**
     * @param Request $req
     * @param \SugarBean $module
     * @param array $args route arguments
     * @return array
     * @throws BadRequestException
     * @throws \Psr\Container\ContainerExceptionInterface
     * @throws \Psr\Container\NotFoundExceptionInterface
     * @throws \SuiteCRM\API\JsonApi\v1\Filters\Interpreters\InvalidArgumentException
     * @throws \SuiteCRM\Exception\Exception
     */
    protected function getModuleList(Request $req, \SugarBean $module, array $args = array())
    {
        $filter = [];
        /** @var array $page */
        $page = $req->getParam('page');

        // Order by (sorting)
        $orderBy = $this->getSorting($req);

        // Pagination (offset)
        $currentOffset = isset($page['offset']) ? (integer)$page['offset'] : -1;

        // Pagination (page limit)
        $limit = isset($page['limit']) ? (integer)$page['limit'] : -1;

        // Maximum results (-1 === Unlimited)
        $maximumResults = -1;

        // Show deleted results
        $show_deleted = 0;
        if (isset($filter['deleted'])) {
            $show_deleted = (integer)$filter['deleted'];
        }

        // Filtering (where clause in SQL)
        /** @var FilterRepository $filterRepository */
        $filterRepository = $this->containers->get('FilterRepository');
        $filterStructure = $filterRepository->fromRequest($req, $args);
        /** @var FilterInterpreter $filterInterpreter */
        $filterInterpreter = $this->containers->get('FilterInterpreter');
        if (empty($filterStructure)) {
            // Do not perform a filter
            $where = '';
            return $module->get_list($orderBy, $where, $currentOffset, $limit, $maximumResults, $show_deleted);
        } elseif ($filterInterpreter->isFilterByPreMadeName($filterStructure)) {
            $where = $filterInterpreter->getFilterByPreMadeName($filterStructure);
            /** @var array $moduleList */
            return $module->get_list($orderBy, $where, $currentOffset, $limit, $maximumResults, $show_deleted);
        } elseif ($filterInterpreter->isFilterById($filterStructure)) {
            $where = $filterInterpreter->getFilterById($filterStructure);
            /** @var array $moduleList */
            return $module->get_list($orderBy, $where, $currentOffset, $limit, $maximumResults, $show_deleted);
        } elseif ($filterInterpreter->isFilterByAttributes($filterStructure)) {
            $where = $filterInterpreter->getFilterByAttributes($filterStructure, $args);

            return $module->get_list($orderBy, $where, $currentOffset, $limit, $maximumResults, $show_deleted);
        }

        throw new BadRequestException('[ModulesLib][getModuleList][Unknown filter strategy]');
    }

    /**
     * @param Request $req
     * @param null|integer $offset
     * @param null|integer $limit
     * @param null|array $filter
     * @param null|array $sort
     * @param null|array $fields eg array ('fields' => 'Accounts' => array('name', 'description'))
     * @return string
     * @throws \Psr\Container\ContainerExceptionInterface
     * @throws \Psr\Container\NotFoundExceptionInterface
     */
    private function generatePaginationUrl(
        Request $req,
        $offset = null,
        $limit = null,
        $filter = null,
        $sort = null,
        $fields = null
    ) {
        $config = $this->containers->get('ConfigurationManager');
        $query = new Query();
        $pagination = [];

        if ($offset !== null) {
            $pagination['page']['offset'] = $offset;
        }

        if ($limit !== null && $limit > 0 && $limit !== $config['list_max_entries_per_page']) {
            $pagination['page']['limit'] = $offset;
        }


        if ($filter !== null) {
            $query->withContent(['filter' => $filter]);
        }

        if ($sort !== null) {
            $query->withContent(['sort' => implode(',', $sort)]);
        }


        if ($fields !== null) {
            $queryFields = [];
            foreach ($fields as $module => $moduleFields) {
                $queryFields['fields'][$module] = $fields[$module];
            }
            $query->withContent($queryFields);
        }

        $query->withContent($pagination);
        $queryString = (string)$query;
        if ($queryString !== null) {
            return $config['site_url'] . '/api/' . $req->getUri()->getPath() . '?' . $queryString;
        }

        return $config['site_url'] . '/api/' . $req->getUri()->getPath();
    }

    /**
     * @param Request $request
     * @return \User
     */
    public function getCurrentUser(Request $request)
    {
        $id = $request->getAttribute('oauth_user_id');
        $user = new \User();
        $user->retrieve($id);
        return $user;
    }
}
