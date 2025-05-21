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

use BeanFactory;
use Exception;
use SugarApplication;
use SugarController;
use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

/**
 * Class SugarApplicationTest
 * @package SuiteCRM\Tests\Unit\MVC
 */
class SugarApplicationTest extends SuitePHPUnitFrameworkTestCase
{
    protected $sessionStartedOk = false;

    public function testACLFilter(): void
    {
        if (isset($_SESSION)) {
            $session = $_SESSION;
        }

        $SugarApplication = new SugarApplication();

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $SugarApplication->ACLFilter();
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }

        self::assertTrue(true);

        if (isset($session)) {
            $_SESSION = $session;
        } else {
            unset($_SESSION);
        }
    }

    public function testsetupResourceManagement(): void
    {
        $SugarApplication = new SugarApplication();

        //execute the method with invalid input and test if it works and does not throws an exception.
        try {
            $SugarApplication->setupResourceManagement('');
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }

        //execute the method with valid input and test if it works and does not throws an exception.
        try {
            $SugarApplication->setupResourceManagement('Users');
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }

        self::assertTrue(true);
    }

    public function testsetupPrint(): void
    {
        $SugarApplication = new SugarApplication();

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $SugarApplication->setupPrint();
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }

        self::assertTrue(true);
    }

    public function testpreProcess(): void
    {
        if (isset($_SESSION)) {
            $session = $_SESSION;
        }

        $SugarApplication = new SugarApplication();
        $SugarApplication->controller = new SugarController();

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $SugarApplication->preProcess();
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }

        self::assertTrue(true);

        if (isset($session)) {
            $_SESSION = $session;
        } else {
            unset($_SESSION);
        }
    }

    public function testhandleOfflineClient(): void
    {
        $SugarApplication = new SugarApplication();

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $SugarApplication->handleOfflineClient();
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }

        self::assertTrue(true);
    }

    public function testhandleAccessControl(): void
    {
        $SugarApplication = new SugarApplication();
        $SugarApplication->controller = new SugarController();

        $result = $SugarApplication->handleAccessControl();

        //check that it returns Null
        self::assertEquals(null, $result);

        //check that controller->hasAccess is true i-e default setting.
        self::assertEquals(true, $SugarApplication->controller->hasAccess);
    }

    public function testpreLoadLanguages(): void
    {
        try {
            SugarApplication::preLoadLanguages();

            //check that method call got the current_language global variable set.
            self::assertTrue(isset($GLOBALS['current_language']));

            //check that method call got the app_strings global variable set.
            self::assertTrue(is_array($GLOBALS['app_strings']) && count($GLOBALS['app_strings']) > 0);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testloadLanguages(): void
    {
        $SugarApplication = new SugarApplication();
        $SugarApplication->controller = new SugarController();

        try {
            $SugarApplication->loadLanguages();

            //check that method call got the current_language global variable set.
            self::assertTrue(isset($GLOBALS['current_language']));

            //check that method call got the app_strings global variable set.
            self::assertTrue(is_array($GLOBALS['app_strings']) && count($GLOBALS['app_strings']) > 0);

            //check that method call got the app_list_strings global variable set.
            self::assertTrue(is_array($GLOBALS['app_list_strings']) && count($GLOBALS['app_list_strings']) > 0);

            //check that method call got the mod_strings global variable set.
            self::assertTrue(is_array($GLOBALS['mod_strings']) && count($GLOBALS['mod_strings']) > 0);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testcheckDatabaseVersion(): void
    {
        self::markTestIncomplete('environment dependency');


        $SugarApplication = new SugarApplication();

        //execute the method with false parameter and check for false returned as it cannot connect to DB.
        //testing with true will allow it to use die() which stops phpunit execution as well.
        include __DIR__ . '/../../../../sugar_version.php';
        self::assertTrue(isset($sugar_db_version) && $sugar_db_version);

        $GLOBALS['sugar_db_version'] = $sugar_db_version;
        $result = $SugarApplication->checkDatabaseVersion(false);
        self::assertTrue($result);
    }

    public function testloadDisplaySettings(): void
    {
        $SugarApplication = new SugarApplication();

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $SugarApplication->loadDisplaySettings();
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }

        self::assertTrue(true);
    }

    public function testloadLicense(): void
    {
        $SugarApplication = new SugarApplication();

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $SugarApplication->loadLicense();
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }

        self::assertTrue(true);
    }

    public function testloadGlobals(): void
    {
        if (isset($_REQUEST)) {
            $request = $_REQUEST;
        }

        $SugarApplication = new SugarApplication();
        $SugarApplication->controller = new SugarController();

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $SugarApplication->loadGlobals();
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }

        self::assertTrue(true);


        if (isset($request)) {
            $_REQUEST = $request;
        } else {
            unset($_REQUEST);
        }
    }

    public function teststartSession(): void
    {
        if (isset($_SESSION)) {
            $session = $_SESSION;
        }

        $SugarApplication = new SugarApplication();
        $SugarApplication->controller = new SugarController();

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            if (!headers_sent()) {
                $SugarApplication->startSession();
                $this->sessionStartedOk = true;
            }
        } catch (Exception $e) {
            $err = $e->getMessage() . ' ' . $e->getCode() . ' ' . $e->getFile() . ' ' . $e->getLine() . ' ' . $e->getTraceAsString();
            var_dump($err);
            self::fail($err);
        }

        self::assertTrue(true);

        // cleanup
        if (isset($session)) {
            $_SESSION = $session;
        } else {
            unset($_SESSION);
        }
    }

    public function testendSession(): void
    {
        $SugarApplication = new SugarApplication();
        $SugarApplication->controller = new SugarController();

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            if ($this->sessionStartedOk) {
                $SugarApplication->endSession();
            }
        } catch (Exception $e) {
            $err = $e->getMessage() . ' ' . $e->getCode() . ' ' . $e->getFile() . ' ' . $e->getLine() . ' ' . $e->getTraceAsString();
            var_dump($err);
            self::fail($err);
        }

        self::assertTrue(true);
    }

    public function testappendErrorMessage(): void
    {
        if (isset($_SESSION)) {
            $session = $_SESSION;
        }

        //execute the method and check that the method adds the message to user_error_message array.
        //there should be one more array element after method execution.
        $_SESSION['user_error_message'] = [];
        $user_error_message_count = count($_SESSION['user_error_message']);
        SugarApplication::appendErrorMessage('some error');
        self::assertGreaterThan($user_error_message_count, count($_SESSION['user_error_message']));

        // cleanup
        if (isset($session)) {
            $_SESSION = $session;
        } else {
            unset($_SESSION);
        }
    }

    public function testgetErrorMessages(): void
    {
        //execute the method and check if it returns a array.
        $errorMessages = SugarApplication::getErrorMessages();
        self::assertIsArray($errorMessages);
    }

    public function testsetCookie(): void
    {
        if (isset($_COOKIE)) {
            $cookie = $_COOKIE;
        }
        //execute the method and check that the method adds the key value pair to cookies array.
        SugarApplication::setCookie('key', 'value');
        self::assertEquals('value', $_COOKIE['key']);

        // cleanup
        if (isset($cookie)) {
            $_COOKIE = $cookie;
        } else {
            unset($_COOKIE);
        }
    }

    public function testcreateLoginVars(): void
    {
        $SugarApplication = new SugarApplication();

        // Execute the method and test that it works and doesn't throw an exception.
        try {
            $vars = $SugarApplication->createLoginVars();
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }

        self::assertTrue(true);
    }

    public function testgetLoginVars(): void
    {
        //execute the method and test that it returns a array.
        $vars = (new SugarApplication())->getLoginVars();
        self::assertIsArray($vars);
    }

    public function testgetLoginRedirect(): void
    {
        //execute the method and test that it returns a plus length string
        $redirect = (new SugarApplication())->getLoginRedirect();
        self::assertGreaterThan(0, strlen((string) $redirect));
    }

    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }
}
