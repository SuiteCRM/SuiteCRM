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

namespace SuiteCRM\Tests\Unit\includes\utils;

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

require_once __DIR__ . '/../../../../../include/upload_file.php';
require_once __DIR__ . '/../../../../../include/utils/php_zip_utils.php';

/**
 * Class php_zip_utilsTest
 * @package SuiteCRM\Tests\Unit\utils
 */
class php_zip_utilsTest extends SuitePHPUnitFrameworkTestCase
{
    public function testunzip(): void
    {
        //execute the method and test if it returns true and verify the if unzipped files exist

        $cache_dir = rtrim($GLOBALS['sugar_config']['cache_dir'], '/\\');
        $files_list = array('config.php', 'config_override.php');
        $file = $cache_dir . '/zipTest.zip';

        //creata a zip file first, to unzip
        if (!file_exists($file)) {
            zip_files_list($file, $files_list);
        }

        $result = unzip($file, $cache_dir);
        self::assertTrue($result);

        self::markTestIncomplete('File handling doesnt works in localy');
//        $this->assertFileExists($cache_dir.'/config.php');
//        $this->assertFileExists($cache_dir.'/config_override.php');

        unlink($cache_dir . '/config.php');
        unlink($cache_dir . '/config_override.php');
    }

    public function testunzip_file(): void
    {
        // execute the method and test if it returns true and verify the if unzipped files exist
        $cache_dir = rtrim($GLOBALS['sugar_config']['cache_dir'], '/\\');
        $files_list = array('config.php', 'config_override.php');
        $file = $cache_dir . '/zipTest.zip';

        //create a a zip file first, to unzip
        if (!file_exists($file)) {
            zip_files_list($file, $files_list);
        }

        $result = unzip_file($file, null, $cache_dir);
        self::assertTrue($result);

        self::markTestIncomplete("File handling doesn't work locally.");
//        $this->assertFileExists($cache_dir.'/config.php');
//        $this->assertFileExists($cache_dir.'/config_override.php');

        unlink($cache_dir . '/config.php');
        unlink($cache_dir . '/config_override.php');
    }

    public function testzip_dir(): void
    {
        //execute the method and verify the if zipped file exist
        $cache_dir = rtrim($GLOBALS['sugar_config']['cache_dir'], '/\\');
        $file = $cache_dir . '/zipTest.zip';

        if (file_exists($file)) {
            unlink($file);
        }

        zip_dir($cache_dir . '/modules', $file);

        self::assertFileExists($file);

        unlink($file);
    }

    public function testzip_files_list(): void
    {
        //execute the method and verify the if zipped file exist
        $cache_dir = rtrim($GLOBALS['sugar_config']['cache_dir'], '/\\');
        $file = $cache_dir . '/ziplistTest.zip';
        $files_list = array('config.php', 'config_override.php');

        if (file_exists($file)) {
            unlink($file);
        }

        $result = zip_files_list($file, $files_list);

        self::assertTrue($result);
        self::assertFileExists($file);

        unlink($file);
    }
}
