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

require_once __DIR__ . '/../../../../../include/utils/layout_utils.php';

/**
 * Class layout_utilsTest
 * @package SuiteCRM\Tests\Unit\utils
 */
class layout_utilsTest extends SuitePHPUnitFrameworkTestCase
{
    public function testget_form_header(): void
    {
        //execute the method and test if it returns html and contains the values provided in parameters

        //help param true
        $html1 = get_form_header('test Header', 'test subheader', true);
        self::assertGreaterThan(0, strlen((string) $html1));
        self::assertStringContainsString('test Header', $html1);
        self::assertStringContainsString('test subheader', $html1);

        // help param false
        $html2 = get_form_header('new test Header', 'new test subheader', false);
        self::assertGreaterThan(0, strlen((string) $html2));
        self::assertStringContainsString('new test Header', $html2);
        self::assertStringContainsString('new test subheader', $html2);
        self::assertGreaterThan(strlen((string) $html2), strlen((string) $html1));
    }

    public function testget_module_title(): void
    {
        //execute the method and test if it returns html and contains the values provided in parameters

        //with show_create true, generates more html
        $html1 = get_module_title('Users', 'Users Home', true);
        self::assertGreaterThan(0, strlen((string) $html1));
        self::assertStringContainsString('Users', $html1);
        self::assertStringContainsString('Users Home', $html1);

        //with show_create false, generates less html
        $html2 = get_module_title('Users', 'Users Home', false);
        self::assertGreaterThan(0, strlen((string) $html2));
        self::assertStringContainsString('Users', $html2);
        self::assertStringContainsString('Users Home', $html2);
        self::assertGreaterThan(strlen((string) $html2), strlen((string) $html1));

        //with show_create flase and count > 1, generates more html compared to count =0
        $html3 = get_module_title('Users', 'Users Home', false, 2);
        self::assertGreaterThan(0, strlen((string) $html3));
        self::assertStringContainsString('Users', $html3);
        self::assertStringContainsString('Users Home', $html3);
        self::assertGreaterThan(strlen((string) $html2), strlen((string) $html3));
        self::assertGreaterThan(strlen((string) $html3), strlen((string) $html1));
    }

    public function testgetClassicModuleTitle(): void
    {
        //execute the method and test if it returns html and contains the values provided in parameters

        //with show_create false, generates less html
        $html1 = getClassicModuleTitle('users', array('Users Home'));
        self::assertGreaterThan(0, strlen((string) $html1));
        self::assertStringContainsString('Users Home', $html1);

        //with show_create true, generates more html
        $html2 = getClassicModuleTitle('users', array('Users Home'), true);
        self::assertGreaterThan(0, strlen((string) $html2));
        self::assertStringContainsString('Users Home', $html2);
        self::assertGreaterThan(strlen((string) $html1), strlen((string) $html2));
    }

    public function testinsert_popup_header(): void
    {
        //execute the method and test if it returns html/JS

        //with includeJS true, generates more html
        ob_start();
        insert_popup_header();
        $renderedContent1 = ob_get_contents();
        ob_end_clean();
        self::assertGreaterThan(0, strlen($renderedContent1));

        //with includeJS false, generates less html
        ob_start();
        insert_popup_header('', false);
        $renderedContent2 = ob_get_contents();
        ob_end_clean();
        self::assertGreaterThan(0, strlen($renderedContent2));

        self::assertGreaterThan(strlen($renderedContent2), strlen($renderedContent1));
    }

    public function testinsert_popup_footer(): void
    {
        //execute the method and test if it returns html

        ob_start();
        insert_popup_footer();
        $renderedContent = ob_get_contents();
        ob_end_clean();
        self::assertGreaterThan(0, strlen($renderedContent));
    }
}
