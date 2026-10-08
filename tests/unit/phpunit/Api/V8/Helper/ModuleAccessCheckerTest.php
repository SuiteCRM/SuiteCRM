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

namespace SuiteCRM\Tests\Unit\Api\V8\Helper;

use Api\V8\BeanDecorator\BeanManager;
use Api\V8\Helper\ModuleAccessChecker;
use BeanFactory;
use DBManagerFactory;
use SuiteCRM\Exception\NotAllowedException;
use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

class ModuleAccessCheckerTest extends SuitePHPUnitFrameworkTestCase
{
    private const USER_ID = 'module-access-checker-test-user';

    /**
     * @var ModuleAccessChecker
     */
    private $checker;

    protected function setUp(): void
    {
        parent::setUp();

        $this->checker = new ModuleAccessChecker(new BeanManager(DBManagerFactory::getInstance(), [
            'Account' => 'Accounts',
            'Contracts' => 'AOS_Contracts',
            'Employee' => 'Employees',
            'User' => 'Users',
        ]));
        $this->actAs(false);
        $this->seedAclCache([
            'AOS_Contracts' => ACL_ALLOW_ENABLED,
            'AOP_Case_Updates' => ACL_ALLOW_ENABLED,
            'Schedulers' => ACL_ALLOW_ENABLED,
            'Accounts' => ACL_ALLOW_DISABLED,
        ]);
    }

    protected function tearDown(): void
    {
        unset($_SESSION['ACL'][self::USER_ID]);

        parent::tearDown();
    }

    public function testModuleWithoutANavigationTabIsAllowedWhenAclAccessIsEnabled(): void
    {
        $this->checker->checkAccess('AOP_Case_Updates');

        $this->addToAssertionCount(1);
    }

    public function testAliasIsCheckedAsTheModuleItResolvesTo(): void
    {
        $this->checker->checkAccess('Contracts');

        $this->expectException(NotAllowedException::class);

        $this->checker->checkAccess('Account');
    }

    public function testModuleWithAclAccessDisabledIsDenied(): void
    {
        $this->expectException(NotAllowedException::class);

        $this->checker->checkAccess('Accounts');
    }

    public function testModuleWithoutAclActionsIsDeniedToNonAdmins(): void
    {
        $this->expectException(NotAllowedException::class);

        $this->checker->checkAccess('OAuth2Tokens');
    }

    public function testAdminOnlyModuleIsDeniedToNonAdminsEvenWithAclAccess(): void
    {
        $this->expectException(NotAllowedException::class);

        $this->checker->checkAccess('Schedulers');
    }

    public function testUsersAndEmployeesAreLeftToModuleService(): void
    {
        foreach (['Users', 'User', 'Employees', 'Employee'] as $module) {
            $this->checker->checkAccess($module);
        }

        $this->addToAssertionCount(1);
    }

    public function testAdminIsAllowedEveryModule(): void
    {
        $this->actAs(true);

        foreach (['Contracts', 'AOP_Case_Updates', 'Accounts', 'OAuth2Tokens', 'Schedulers'] as $module) {
            $this->checker->checkAccess($module);
        }

        $this->addToAssertionCount(1);
    }

    private function actAs(bool $isAdmin): void
    {
        global $current_user;

        $current_user = BeanFactory::newBean('Users');
        $current_user->id = self::USER_ID;
        $current_user->is_admin = $isAdmin ? '1' : '0';
    }

    /**
     * @param array<string, int> $accessByModule
     */
    private function seedAclCache(array $accessByModule): void
    {
        foreach ($accessByModule as $module => $access) {
            $_SESSION['ACL'][self::USER_ID][$module]['module']['access']['aclaccess'] = $access;
        }
    }
}
