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

namespace SuiteCRM\Tests\Unit\includes;

use SuiteCRM\Exception\Exception;
use SuiteCRM\JsonApiErrorObject;
use SuiteCRM\LangException;
use SuiteCRM\LangText;
use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

/**
 * Class JsonApiErrorObjectTest
 * @package SuiteCRM\Tests\Unit
 */
class JsonApiErrorObjectTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();
        if (!defined('sugarEntry')) {
            define('sugarEntry', true);
        }

        global $app_strings, $mod_strings;

        include_once __DIR__ . '/../../../../include/utils.php';
        include_once __DIR__ . '/../../../../include/SugarTheme/SugarTheme.php';
        include_once __DIR__ . '/../../../../include/SugarTheme/SugarThemeRegistry.php';
        include __DIR__ . '/../../../../include/language/en_us.lang.php';
        include_once __DIR__ . '/../../../../include/SugarObjects/SugarConfig.php';
        include_once __DIR__ . '/../../../../include/SugarLogger/LoggerManager.php';

        include_once __DIR__ . '/../../../../include/ErrorMessageException.php';
        include_once __DIR__ . '/../../../../include/ErrorMessage.php';
        include_once __DIR__ . '/../../../../include/LangText.php';
        include_once __DIR__ . '/../../../../include/JsonApiErrorObject.php';
        include_once __DIR__ . '/../../../../include/LangExceptionInterface.php';
        include_once __DIR__ . '/../../../../include/LangException.php';
    }

    public function testConstruct(): void
    {
        $error = new JsonApiErrorObject();
        $expected = [
            'id' => '1',
            'links' => ['about' => null],
            'status' => '200',
            'code' => '1',
            'title' => 'JSON API Error',
            'detail' => 'JSON API Error occurred.',
            'source' => ['pointer' => null, 'parameter' => null],
            'meta' => [],
        ];
        $actual = $error->export();
        self::assertEquals($expected, $actual, 'API Error Object constructor error: Incorrect default state.');

        global $app_strings;
        $app_strings['LBL_TEST_LABEL_TITLE'] = 'A title';
        $app_strings['LBL_TEST_LABEL_DETAIL'] = 'A detail';

        $error = new JsonApiErrorObject(new LangText('LBL_TEST_LABEL_TITLE'), new LangText('LBL_TEST_LABEL_DETAIL'), 123, 234, 345, ['about' => 'test123'], ['pointer' => '/test/foo/bar', 'parameter' => 'wrong'], ['some' => 'meta info']);
        $expected = [
            'id' => '123',
            'links' => ['about' => 'test123'],
            'status' => '345',
            'code' => '234',
            'title' => 'A title',
            'detail' => 'A detail',
            'source' => ['pointer' => '/test/foo/bar', 'parameter' => 'wrong'],
            'meta' => ['some' => 'meta info'],
        ];
        $actual = $error->export();
        self::assertEquals($expected, $actual, 'API Error Object constructor error: Incorrect state set.');
    }

    public function testRetrieveFromException(): void
    {
        $error = new JsonApiErrorObject();

        global $app_strings;
        $app_strings['LBL_TEST_LANG_TEXT'] = 'Test text with variable {foo}.';

        $prev = new Exception('Test basic exception', 555);
        $error->retrieveFromException(new LangException('test exception message', 123, $prev, new LangText('LBL_TEST_LANG_TEXT', ['foo' => 'bar'])));
        $expected = [
            'id' => '1',
            'links' => ['about' => null],
            'status' => '200',
            'title' => 'JSON API Error',
            'detail' => 'JSON API Error occurred.',
            'source' => ['pointer' => null, 'parameter' => null],
            'meta' => [
                'about' => 'Exception',
                'class' => 'SuiteCRM\\LangException',
                'code' => 123,
                'langMessage' => 'Test text with variable bar.',
            ],
            'code' => 123,
        ];
        $actual = $error->export();

        if (inDeveloperMode()) {
            unset($actual['meta']['debug']);
        }

        self::assertEquals($expected, $actual, 'API Error Object retrive error from exception.');
    }
}
