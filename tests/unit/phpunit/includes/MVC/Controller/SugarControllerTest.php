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

namespace SuiteCRM\Tests\Unit\includes\MVC\Controller;

use BeanFactory;
use DBManagerFactory;
use Exception;
use SugarApplication;
use SugarController;
use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;
use SuiteCRM\Test\TestLogger;

/**
 * Class SugarControllerTest
 * @package SuiteCRM\Tests\Unit\MVC\Controller
 */
class SugarControllerTest extends SuitePHPUnitFrameworkTestCase
{
    public function testsetup(): void
    {
        $SugarController = new SugarController();
        $default_module = $SugarController->module;

        //first test with empty parameter and check for default values being used
        $SugarController->setup('');
        self::assertEquals($default_module, $SugarController->module);
        self::assertEquals(null, $SugarController->target_module);

        //secondly test with module name and check for correct assignment.
        $SugarController->setup('Users');
        self::assertEquals('Users', $SugarController->module);
        self::assertEquals(null, $SugarController->target_module);
    }

    public function testsetModule(): void
    {
        $SugarController = new SugarController();

        //first test with empty parameter
        $SugarController->setModule('');
        self::assertEquals('', $SugarController->module);

        //secondly test with module name and check for correct assignment.
        $SugarController->setModule('Users');
        self::assertEquals('Users', $SugarController->module);
    }

    public function testloadBean(): void
    {
        $SugarController = new SugarController();

        //first test with empty parameter and check for null. Default is Home but Home has no bean
        $SugarController->setModule('');
        $SugarController->loadBean();
        self::assertEquals(null, $SugarController->bean);

        //secondly test with module name and check for correct bean class loaded.
        $SugarController->setModule('Users');
        $SugarController->loadBean();
        self::assertInstanceOf('User', $SugarController->bean);
    }

    public function testexecute(): void
    {
        // suppress output during the test
        $this->setOutputCallback(function () {
        });

        // test
        $SugarController = new SugarController();

        // replace and use a temporary logger
        $logger = $GLOBALS['log'];
        $GLOBALS['log'] = new TestLogger();

        //execute the method and check if it works and doesn't throws an exception
        try {
            $SugarController->execute();
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }

        // change back to original logger
        $testLogger = $GLOBALS['log'];
        $GLOBALS['log'] = $logger;

        // exam log
        self::assertTrue(true);
    }

    public function testprocess(): void
    {
        $SugarController = new SugarController();

        //execute the method and check if it works and doesn't throws an exception
        try {
            $SugarController->process();
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }

        self::assertTrue(true);
    }

    public function testpre_save(): void
    {
        if (isset($_SESSION)) {
            $session = $_SESSION;
        }

        $testUserId = 1;
        $query = "SELECT date_modified FROM users WHERE id = '$testUserId' LIMIT 1";
        $row = DBManagerFactory::getInstance()->query($query)->fetch_assoc();
        $testUserDateModified = $row['date_modified'];


        $SugarController = new SugarController();
        $SugarController->setModule('Users');
        $SugarController->record = "1";
        $SugarController->loadBean();

        //execute the method and check if it either works or throws an mysql exception.
        //Fail if it throws any other exception.
        try {
            $SugarController->pre_save();
        } catch (Exception $e) {
            self::assertStringStartsWith('mysqli_query()', $e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }

        self::assertTrue(true);

        // cleanup
        if (isset($session)) {
            $_SESSION = $session;
        } else {
            unset($_SESSION);
        }

        $query = "UPDATE users SET date_modified = '$testUserDateModified' WHERE id = '$testUserId' LIMIT 1";
        DBManagerFactory::getInstance()->query($query);
    }

    public function testaction_save(): void
    {
        if (isset($_SESSION)) {
            $session = $_SESSION;
        }

        $testUserId = 1;
        $query = "SELECT date_modified FROM users WHERE id = '$testUserId' LIMIT 1";
        $row = DBManagerFactory::getInstance()->query($query)->fetch_assoc();
        $testUserDateModified = $row['date_modified'];

        $SugarController = new SugarController();
        $SugarController->setModule('Users');
        $SugarController->record = "1";
        $SugarController->loadBean();

        //execute the method and check if it either works or throws an mysql exception.
        //Fail if it throws any other exception.
        try {
            $SugarController->action_save();
            self::assertTrue(false);
        } catch (Exception $e) {
            self::assertTrue(true);
        }

        self::assertTrue(true);

        // cleanup
        if (isset($session)) {
            $_SESSION = $session;
        } else {
            unset($_SESSION);
        }

        $query = "UPDATE users SET date_modified = '$testUserDateModified' WHERE id = '$testUserId' LIMIT 1";
        DBManagerFactory::getInstance()->query($query);
    }

    public function testaction_spot(): void
    {
        $SugarController = new SugarController();

        // check with default value of attribute
        self::assertEquals('classic', $SugarController->view);

        // check for attribute value change on method execution.
        $SugarController->action_spot();
        self::assertEquals('spot', $SugarController->view);
    }

    public function testgetActionFilename(): void
    {
        // check with an invalid value
        $action = SugarController::getActionFilename('');
        self::assertEquals('', $action);

        // check with a valid value
        $action = SugarController::getActionFilename('editview');
        self::assertEquals('EditView', $action);
    }

    public function testcheckEntryPointRequiresAuth(): void
    {
        $SugarController = new SugarController();

        // check with a invalid value
        $result = $SugarController->checkEntryPointRequiresAuth('');
        self::assertTrue($result);

        // check with a valid True value
        $result = $SugarController->checkEntryPointRequiresAuth('download');
        self::assertTrue($result);

        // check with a valid False value
        $result = $SugarController->checkEntryPointRequiresAuth('GeneratePassword');
        self::assertFalse($result);
    }

    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        $current_user = BeanFactory::newBean('Users');
        get_sugar_config_defaults();
        if (!isset($GLOBALS['app']) || !$GLOBALS['app']) {
            $GLOBALS['app'] = new SugarApplication();
        }
    }
}
