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

use Exception;
use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

require_once __DIR__ . '/../../../../../include/utils/progress_bar_utils.php';

/**
 * Class progress_bar_utilsTest
 * @package SuiteCRM\Tests\Unit\utils
 */
class progress_bar_utilsTest extends SuitePHPUnitFrameworkTestCase
{
    public function testprogress_bar_flush(): void
    {
        //execute the method and test if it doesn't throw an exception.
        //this method uses flush so we cannot get and verify content printed
        try {
            progress_bar_flush(false);
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testdisplay_flow_bar(): void
    {
        //execute the method and test if it doesn't throw an exception.
        //this method uses flush so we cannot get and verify content printed
        try {
            ob_start();
            display_flow_bar('test', 0, 200, false);
            ob_end_clean();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function teststart_flow_bar(): void
    {
        //execute the method and test if it doesn't throw an exception.
        //this method uses flush so we cannot get and verify content printed
        try {
            ob_start();
            start_flow_bar('test', 1, false);
            ob_end_clean();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testdestroy_flow_bar(): void
    {
        //execute the method and test if it doesn't throw an exception.
        //this method uses flush so we cannot get and verify content printed
        try {
            ob_start();
            destroy_flow_bar('test', false);
            ob_end_clean();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testdisplay_progress_bar(): void
    {
        //execute the method and test if it doesn't throw an exception.
        //this method uses flush so we cannot get and verify content printed
        try {
            ob_start();
            display_progress_bar('test', 80, 100, false);
            ob_end_clean();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }

    public function testupdate_progress_bar(): void
    {
        //execute the method and test if it doesn't throw an exception.
        //this method uses flush so we cannot get and verify content printed
        try {
            ob_start();
            update_progress_bar('test', 80, 100, false);
            ob_end_clean();
            self::assertTrue(true);
        } catch (Exception $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }
    }
}
