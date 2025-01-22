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

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;
use ViewFactory;

/**
 * Class ViewFactoryTest
 * @package SuiteCRM\Tests\Unit\MVC\View
 */
class ViewFactoryTest extends SuitePHPUnitFrameworkTestCase
{
    public function testloadView(): void
    {
        //check with invalid input. must return sugaview instance
        $view = ViewFactory::loadView('default', '');
        self::assertInstanceOf('SugarView', $view);

        //check with a valid module without a specific view, must return sugarview instance
        $view = ViewFactory::loadView('default', 'Users');
        self::assertInstanceOf('SugarView', $view);

        //check with a valid module and specific view, must reutern speciifc view instance
        $view = ViewFactory::loadView('list', 'Users');
        self::assertInstanceOf('UsersViewList', $view);
    }

    public function test_loadConfig(): void
    {
        //check with a invalid module, method must not change the view options.
        $view = ViewFactory::loadView('default', '');
        $options = $view->options;
        ViewFactory::_loadConfig($view, 'default');
        self::assertSame($options, $view->options);

        //check with a valid module which does not implement it's own view config. method must not change the view options.
        $view = ViewFactory::loadView('detail', 'Users');
        $options = $view->options;
        ViewFactory::_loadConfig($view, 'detail');
        self::assertSame($options, $view->options);

        //check with a valid module which implement it's own view config. method still must not change the view options because it needs.
        $view = ViewFactory::loadView('area_detail_map', 'jjwg_Areas');
        $view->module = 'jjwg_Areas';
        $options = $view->options;
        ViewFactory::_loadConfig($view, 'area_detail_map');
        self::assertSame($options, $view->options);
    }

    public function test_buildFromFile(): void
    {
        //check with valid values and test if it returns correct view instance
        $type = 'list';
        $target_module = 'Users';
        $bean = null;
        $view = ViewFactory::_buildFromFile('modules/' . $target_module . '/views/view.' . $type . '.php', $bean,
            array(), $type, $target_module);
        self::assertInstanceOf('UsersViewList', $view);

        //check with valid values and test if it returns correct view instance
        $type = 'detail';
        $target_module = 'Users';
        $bean = null;
        $view = ViewFactory::_buildFromFile('modules/' . $target_module . '/views/view.' . $type . '.php', $bean,
            array(), $type, $target_module);
        self::assertInstanceOf('UsersViewDetail', $view);
    }

    public function test_buildClass(): void
    {
        //check with valid values and test if it returns correct view instance
        $view = ViewFactory::_buildClass('UsersViewList', null, array());
        self::assertInstanceOf('UsersViewList', $view);

        //check with valid values and test if it returns correct view instance
        $view = ViewFactory::_buildClass('UsersViewDetail', null, array());
        self::assertInstanceOf('UsersViewDetail', $view);
    }
}
