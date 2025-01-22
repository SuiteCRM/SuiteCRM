<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2021 SalesAgility Ltd.
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

namespace SuiteCRM\Tests\Unit\includes\MVC;

use ReflectionClass;
use ReflectionException;
use SugarModule;
use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

/**
 * Class SugarModuleTest
 * @package SuiteCRM\Tests\Unit\MVC
 */
class SugarModuleTest extends SuitePHPUnitFrameworkTestCase
{
    /**
     * @throws ReflectionException
     */
    public function testconstructor(): void
    {
        // Test for invalid input
        $reflectionProperty = (new ReflectionClass(SugarModule::class))->getProperty('_moduleName');
        $reflectionProperty->setAccessible(true);
        $reflectedValue = $reflectionProperty->getValue(new SugarModule(''));
        self::assertEquals(null, $reflectedValue);

        // Test for valid input
        $sugarModuleUser = SugarModule::get('User');
        $reflectionProperty = (new ReflectionClass($sugarModuleUser))->getProperty('_moduleName');
        $reflectionProperty->setAccessible(true);
        $reflectedValue = $reflectionProperty->getValue(SugarModule::get('User'));
        self::assertEquals('User', $reflectedValue);
    }

    public function testmoduleImplements(): void
    {
        // Test for invalid input
        $result = (new SugarModule(''))->moduleImplements('Basic');
        self::assertEquals(false, $result);

        // Test for invalid input
        $sugarmodule_user = new SugarModule('Users');
        $result = $sugarmodule_user->moduleImplements('SugarModule');
        self::assertFalse($result);

        // Test for valid input
        $sugarmodule_user = new SugarModule('Users');
        $result = $sugarmodule_user->moduleImplements('Basic');
        self::assertEquals(true, $result);
    }

    public function testloadBean(): void
    {
        // Test for invalid input
        $result = (new SugarModule(''))->loadBean();
        self::assertFalse($result);

        // Test for valid input
        $result = (new SugarModule('Users'))->loadBean();
        self::assertInstanceOf('User', $result);
    }
}
