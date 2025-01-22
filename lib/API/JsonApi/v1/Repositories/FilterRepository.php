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

namespace SuiteCRM\API\JsonApi\v1\Repositories;

use Psr\Http\Message\ServerRequestInterface as Request;
use SuiteCRM\API\JsonApi\v1\Filters\Parsers\FilterParser;
use SuiteCRM\API\JsonApi\v1\Resource\SuiteBeanResource;
use Psr\Container\ContainerInterface;
use SuiteCRM\API\v8\Exception\BadRequestException;

/**
 * Class FilterRepository
 * @package SuiteCRM\API\JsonApi\v1\Repositories
 */
#[\AllowDynamicProperties]
class FilterRepository
{
    /**
     * @var ContainerInterface $containers
     */
    private $containers;

    /**
     * @var FilterParser $filterParser
     */
    private $filterParser;

    /**
     * FilterRepository constructor.
     * @param ContainerInterface $containers
     */
    public function __construct(ContainerInterface $containers)
    {
        $this->containers = $containers;
        $this->filterParser = new FilterParser($containers);
    }

    /**
     * @param Request $request
     * @param array route arguments
     * @return array
     * @throws \SuiteCRM\API\v8\Exception\BadRequestException
     */
    public function fromRequest(Request $request, array $args = array())
    {
        /** @var OperatorInterface[] $filterOperators */
        // Parse Filters from request
        $queries = $request->getQueryParams();
        if (empty($queries)) {
            return array();
        }

        $response = array();
        if (isset($queries['filter'])) {
            /** @var array $filters */
            $filters = $queries['filter'];

            if (is_array($filters)) {
                foreach ($filters as $filterKey => $filter) {
                    $response = array_merge($response, $this->filterParser->parseFilter($filterKey, $filter, $args));
                }
            } else {
                if (is_string($filters)) {
                    $response = array($filters);
                } else {
                    throw new BadRequestException('[JsonApi][v1][Repositories][FilterRepository][filter type is invalid]');
                }
            }
        }

        return $response;
    }

    /**
     * @return SuiteBeanResource
     */
    public function toSuiteBeanResource()
    {
        return new SuiteBeanResource($this->containers);
    }
}
