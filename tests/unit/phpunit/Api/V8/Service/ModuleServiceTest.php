<?php
/**
 * SuiteCRM is a customer relationship management program developed by SuiteCRM Ltd.
 * Copyright (C) 2026 SuiteCRM Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUITECRM, SUITECRM DISCLAIMS THE
 * WARRANTY OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License
 * version 3, these Appropriate Legal Notices must retain the display of the
 * "Supercharged by SuiteCRM" logo. If the display of the logos is not reasonably
 * feasible for technical reasons, the Appropriate Legal Notices must display
 * the words "Supercharged by SuiteCRM".
 */

namespace SuiteCRM\Tests\Unit\Api\V8\Service;

use Api\V8\BeanDecorator\BeanManager;
use Api\V8\Factory\ValidatorFactory;
use Api\V8\Helper\VarDefHelper;
use Api\V8\JsonApi\Helper\AttributeObjectHelper;
use Api\V8\JsonApi\Helper\PaginationObjectHelper;
use Api\V8\JsonApi\Helper\RelationshipObjectHelper;
use Api\V8\Param\GetModuleParams;
use Api\V8\Service\ModuleService;
use BeanFactory;
use DBManagerFactory;
use SuiteCRM\Exception\AccessDeniedException;
use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;
use Symfony\Component\Validator\ValidatorBuilder;

class ModuleServiceTest extends SuitePHPUnitFrameworkTestCase
{
    public function testUserAliasIsHeldToTheUsersSelfOrAdminRule(): void
    {
        global $current_user;

        $current_user = BeanFactory::newBean('Users');
        $current_user->id = 'module-service-test-user';
        $current_user->is_admin = '0';

        $beanManager = new BeanManager(DBManagerFactory::getInstance(), ['User' => 'Users']);
        $params = new GetModuleParams(new ValidatorFactory((new ValidatorBuilder())->getValidator()), $beanManager);
        $params->configure(['moduleName' => 'User', 'id' => '1']);
        $service = new ModuleService(
            $beanManager,
            new AttributeObjectHelper($beanManager),
            new RelationshipObjectHelper(new VarDefHelper()),
            new PaginationObjectHelper()
        );

        $this->expectException(AccessDeniedException::class);

        $service->getRecord($params, '');
    }
}
