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

namespace Api\V8\Service;

use Api\V8\BeanDecorator\BeanManager;
use Api\V8\Helper\ModuleListProvider;
use Api\V8\JsonApi\Response\AttributeResponse;
use Api\V8\JsonApi\Response\DataResponse;
use Api\V8\JsonApi\Response\DocumentResponse;
use Api\V8\Param\GetFieldListParams;
use Slim\Http\Request;
use SuiteCRM\Exception\Exception;
use SuiteCRM\Exception\NotAllowedException;
use SuiteCRM\Exception\NotFoundException;

/**
 * Class MetaService
 * @package Api\V8\Service
 */
#[\AllowDynamicProperties]
class MetaService
{
    /**
     * @var BeanManager
     */
    private $beanManager;

    /**
     * @var ModuleListProvider
     */
    private $moduleListProvider;

    private static $allowedVardefFields = [
        'type',
        'dbType',
        'source',
        'relationship',
        'default',
        'len',
        'precision',
        'comments',
        'required',
    ];

    /**
     * UserService constructor.
     * @param BeanManager $beanManager
     */
    public function __construct(
        BeanManager $beanManager,
        ModuleListProvider $moduleListProvider
    ) {
        $this->beanManager = $beanManager;
        $this->moduleListProvider = $moduleListProvider;
    }

    /**
     * Build the response with a list of modules to return.
     *
     * @param Request $request
     * @return DocumentResponse
     */
    public function getModuleList(Request $request)
    {
        $modules = $this->moduleListProvider->getModuleList();

        $dataResponse = new DataResponse('modules', '');
        $attributeResponse = new AttributeResponse($modules);
        $dataResponse->setAttributes($attributeResponse);

        $response = new DocumentResponse();
        $response->setData($dataResponse);

        return $response;
    }

    /**
     * Build the response with a list of fields to return.
     *
     * @param Request $request
     * @param GetFieldListParams $fieldListParams
     * @return DocumentResponse
     * @throws NotAllowedException
     */
    public function getFieldList(Request $request, GetFieldListParams $fieldListParams)
    {
        $fieldList = $this->buildFieldList($fieldListParams->getModule());

        $dataResponse = new DataResponse('fields', '');
        $attributeResponse = new AttributeResponse($fieldList);
        $dataResponse->setAttributes($attributeResponse);

        $response = new DocumentResponse();
        $response->setData($dataResponse);

        return $response;
    }

    /**
     * @param string $module
     * @throws NotAllowedException
     */
    private function checkIfUserHasModuleAccess($module)
    {
        global $current_user;

        $modules = query_module_access_list($current_user);
        \ACLController::filterModuleList($modules, false);

        if (!in_array($module, $modules, true)) {
            throw new NotAllowedException('The API user does not have access to this module.');
        }
    }

    /**
     * Build the list of fields for a given module.
     *
     * @param string $module
     * @return array
     * @throws NotAllowedException
     */
    private function buildFieldList($module)
    {
        $this->checkIfUserHasModuleAccess($module);
        $bean = $this->beanManager->newBeanSafe($module);
        $fieldList = [];
        foreach ($bean->field_defs as $fieldName => $fieldDef) {
            $fieldList[$fieldName] = $this->pruneVardef($fieldDef);
        }

        return $fieldList;
    }

    /**
     * We only allow certain fields from the vardefs to be returned in the field list.
     *
     * @param array $def
     * @return array
     */
    private function pruneVardef($def)
    {
        $pruned = [];
        foreach ($def as $var => $val) {
            if (in_array($var, static::$allowedVardefFields, true)) {
                $pruned[$var] = $val;
            }
        }
        if (!isset($def['required'])) {
            $pruned['required'] = false;
        }
        if (!isset($def['dbType'])) {
            $pruned['dbType'] = $def['type'];
        }

        return $pruned;
    }

    /**
     * Build the response with the swagger schema.
     *
     * @return DocumentResponse
     * @throws NotFoundException
     * @throws Exception
     */
    public function getSwaggerSchema()
    {
        $path = __DIR__ . '/../../docs/swagger/swagger.json';
        if (!file_exists($path)) {
            throw new NotFoundException(
                'Unable to find JSON Api Schema file: ' . $path
            );
        }

        $swaggerFile = file_get_contents($path);

        if (!$swaggerFile) {
            throw new Exception(
                'Unable to read JSON Api Schema file: ' . $path
            );
        }

        return json_decode($swaggerFile, true);
    }
}
