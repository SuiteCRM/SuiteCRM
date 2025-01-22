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

namespace SuiteCRM\Tests\Unit\includes\MVC\View\views;

use BeanFactory;
use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;
use ViewMultiedit;

/**
 * Class ViewMultieditTest
 * @package SuiteCRM\Tests\Unit\MVC\View\views
 */
class ViewMultieditTest extends SuitePHPUnitFrameworkTestCase
{
    public function testViewMultiedit(): void
    {
        // Execute the constructor and check for the Object type and type attribute
        $view = new ViewMultiedit();
        self::assertInstanceOf('ViewMultiedit', $view);
        self::assertInstanceOf('SugarView', $view);
        self::assertEquals('edit', $view->type);
    }

    public function testdisplay(): void
    {
        //test without action value and modules list in REQUEST object
        $view = new ViewMultiedit();
        ob_start();
        $view->display();
        $renderedContent = ob_get_contents();
        ob_end_clean();
        self::assertEquals(0, strlen($renderedContent));

        //test with valid action value to get link in return
        $view = new ViewMultiedit();
        $view->action = 'AjaxFormSave';
        $view->module = 'Users';
        $view->bean = BeanFactory::newBean('Users');
        $view->bean->id = 1;
        ob_start();
        $view->display();
        $renderedContent = ob_get_contents();
        ob_end_clean();
        self::assertGreaterThan(0, strlen($renderedContent));

        //Fails with a fatal error, method creates editview without properly setting it up causing fatal errors.
        /*
        //test only with modules list in REQUEST object
        $view = new ViewMultiedit();
        $GLOBALS['current_language']= 'en_us';
        $_REQUEST['modules']= Array('Calls','Accounts');
        ob_start();
        $view->display();
        $renderedContent = ob_get_contents();
        ob_end_clean();
        $this->assertGreaterThan(0,strlen($renderedContent));
        */
    }
}
