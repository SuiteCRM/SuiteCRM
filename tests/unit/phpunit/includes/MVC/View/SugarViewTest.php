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

namespace SuiteCRM\Tests\Unit\includes\MVC\View;

use BeanFactory;
use Exception;
use SugarApplication;
use SugarView;
use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

/**
 * Class SugarViewTest
 * @package SuiteCRM\Tests\Unit\MVC\View
 */
class SugarViewTest extends SuitePHPUnitFrameworkTestCase
{
    public function testinit(): void
    {
        //error_reporting(E_ERROR | E_WARNING | E_PARSE);
        $SugarView = new SugarView();

        //execute the method and check if it works and doesn't throws an exception
        try {
            $SugarView->init();
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }

        self::assertTrue(true);
    }

    public function testprocess(): void
    {
        // test
        $SugarView = new SugarView();
        $SugarView->module = 'Users';
        $GLOBALS['app'] = new SugarApplication();

        //execute the method and check if it works and doesn't throws an exception
        //secondly check if it outputs any content to browser
        try {
            ob_start();

            $SugarView->process();

            $renderedContent = ob_get_contents();
            ob_end_clean();

            self::assertGreaterThan(0, strlen($renderedContent));
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testdisplayErrors(): void
    {
        $SugarView = new SugarView();

        //execute the method and check if it works and doesn't throws an exception
        try {
            $errors = $SugarView->displayErrors();
            self::assertEmpty($errors, print_r($SugarView, true));
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }

        self::assertTrue(true);
    }

    public function testpreDisplay(): void
    {
        $SugarView = new SugarView();

        //execute the method and check if it works and doesn't throws an exception
        try {
            $SugarView->preDisplay();
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }

        self::assertTrue(true);
    }

    public function testdisplay(): void
    {
        $SugarView = new SugarView();

        //execute the method and check if it works and doesn't throws an exception
        try {
            $SugarView->display();
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }

        self::assertTrue(true);
    }

    public function testdisplayHeader(): void
    {
        $SugarView = new SugarView();
        $SugarView->module = 'Users';
        $GLOBALS['app'] = new SugarApplication();

        //execute the method and check if it works and doesn't throws an exception
        //secondly check if it outputs any content to browser
        try {
            ob_start();

            $SugarView->displayHeader();

            $renderedContent = ob_get_contents();
            ob_end_clean();

            self::assertGreaterThan(0, strlen($renderedContent));
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testgetModuleMenuHTML(): void
    {
        $SugarView = new SugarView();

        //execute the method and check if it works and doesn't throws an exception
        try {
            $SugarView->getModuleMenuHTML();
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }

        self::assertTrue(true);
    }

    public function testincludeClassicFile(): void
    {
        $SugarView = new SugarView();

        //execute the method and check if it works and doesn't throws an exception
        //use any valid file path, we just need to avoid failing require_once
        try {
            $SugarView->includeClassicFile('config.php');
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }

        self::assertTrue(true);
    }

    public function testgetJavascriptValidation(): void
    {
        //check if it returns any text i-e JS code
        $js = SugarView::getJavascriptValidation();
        self::assertGreaterThan(0, strlen((string) $js));
    }

    public function testdisplayFooter(): void
    {
        $SugarView = new SugarView();

        //execute the method and check if it works and doesn't throws an exception
        //secondly check if it outputs any content to browser
        try {
            ob_start();

            $SugarView->displayFooter();

            $renderedContent = ob_get_contents();
            ob_end_clean();

            self::assertGreaterThan(0, strlen($renderedContent));
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testrenderJavascript(): void
    {
        $SugarView = new SugarView();

        //execute the method and check if it works and doesn't throws an exception
        //secondly check if it outputs any content to browser
        try {
            ob_start();

            $SugarView->renderJavascript();

            $renderedContent = ob_get_contents();
            ob_end_clean();

            self::assertGreaterThan(0, strlen($renderedContent));
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testgetMenu(): void
    {
        // Check with valid value and check if it returns an array.
        $menu = (new SugarView())->getMenu('Users');
        self::assertIsArray($menu);
    }

    public function testgetModuleTitle(): void
    {
        $SugarView = new SugarView();

        //first execute the method with default value
        $moduleTitle = $SugarView->getModuleTitle();
        self::assertGreaterThan(0, strlen((string) $moduleTitle));

        //second execute the method with true value
        $moduleTitle = $SugarView->getModuleTitle(true);
        self::assertGreaterThan(0, strlen((string) $moduleTitle));

        //third execute the method with false value
        $moduleTitle = $SugarView->getModuleTitle(false);
        self::assertGreaterThan(0, strlen((string) $moduleTitle));
    }

    public function testgetMetaDataFile(): void
    {
        $SugarView = new SugarView();

        //first execute the method with missing attributes. it should return Null.
        $metaDataFile = $SugarView->getMetaDataFile();
        self::assertEquals(null, $metaDataFile);

        //second execute the method with valid attributes set. it should return a file path string.
        $SugarView->type = 'detail';
        $SugarView->module = 'Users';

        $metaDataFile = $SugarView->getMetaDataFile();
        self::assertGreaterThan(0, strlen((string) $metaDataFile));
    }

    public function testgetBrowserTitle(): void
    {
        //execute the method. it should return a title string.
        $browserTitle = (new SugarView())->getBrowserTitle();
        self::assertGreaterThan(0, strlen((string) $browserTitle));
    }

    public function testgetBreadCrumbSymbol(): void
    {
        //execute the method. it should return a string.
        $breadCrumbSymbol = (new SugarView())->getBreadCrumbSymbol();
        self::assertGreaterThan(0, strlen((string) $breadCrumbSymbol));
    }

    public function testcheckPostMaxSizeError(): void
    {
        //execute the method. it should return False because Request parameters are not available.
        $postMaxSizeError = (new SugarView())->checkPostMaxSizeError();
        self::assertFalse($postMaxSizeError);
    }

    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }
}
