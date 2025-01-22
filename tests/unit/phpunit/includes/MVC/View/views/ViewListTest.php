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
use DBManagerFactory;
use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;
use ViewList;

/**
 * Class ViewListTest
 * @package SuiteCRM\Tests\Unit\MVC\View\views
 */
class ViewListTest extends SuitePHPUnitFrameworkTestCase
{
    public function testlistViewProcess(): void
    {
        $query = "SELECT * FROM aod_index";
        $resource = DBManagerFactory::getInstance()->query($query);
        $rows = [];
        while ($row = $resource->fetch_assoc()) {
            $rows[] = $row;
        }
        $tableAodIndex = $rows;

        $query = "SELECT * FROM email_addresses";
        $resource = DBManagerFactory::getInstance()->query($query);
        $rows = [];
        while ($row = $resource->fetch_assoc()) {
            $rows[] = $row;
        }
        $tableEmailAddresses = $rows;


        //execute the method and call methods to get the required child objects set. it should return some html.
        $view = new ViewList();
        $view->seed = BeanFactory::newBean('Users');
        $view->prepareSearchForm();
        $view->preDisplay();

        ob_start();
        $view->listViewProcess();
        $renderedContent = ob_get_contents();
        ob_end_clean();
        self::assertGreaterThan(0, strlen($renderedContent));


        DBManagerFactory::getInstance()->query("DELETE FROM email_addresses");
        foreach ($tableEmailAddresses as $row) {
            $query = "INSERT email_addresses INTO (";
            $query .= (implode(',', array_keys($row)) . ') VALUES (');
            foreach ($row as $value) {
                $quoteds[] = "'$value'";
            }
            $query .= (implode(', ', $quoteds)) . ')';
            DBManagerFactory::getInstance()->query($query);
        }

        DBManagerFactory::getInstance()->query("DELETE FROM aod_index");
        foreach ($tableAodIndex as $row) {
            $query = "INSERT aod_index INTO (";
            $query .= (implode(',', array_keys($row)) . ') VALUES (');
            foreach ($row as $value) {
                $quoteds[] = "'$value'";
            }
            $query .= (implode(', ', $quoteds)) . ')';
            DBManagerFactory::getInstance()->query($query);
        }
    }

    public function testViewList(): void
    {
        // Execute the constructor and check for the Object type and type attribute
        $view = new ViewList();
        self::assertInstanceOf('ViewList', $view);
        self::assertInstanceOf('SugarView', $view);
        self::assertEquals('list', $view->type);
    }

    public function testlistViewPrepare(): void
    {
        //test without setting parameters. it should return some html
        $view = new ViewList();
        $view->module = 'Users';

        ob_start();
        $view->listViewPrepare();
        $renderedContent = ob_get_contents();
        ob_end_clean();
        self::assertLessThanOrEqual(0, strlen($renderedContent));

        //test with some REQUEST parameters preset. it should return some html and set the REQUEST key we provided in current_query_by_page REQUEST Param.
        $view = new ViewList();
        $view->module = 'Users';
        $GLOBALS['module'] = 'Users';
        $_REQUEST['Users2_USER_offset'] = 1;
        $_REQUEST['current_query_by_page'] = htmlentities(json_encode(array('key' => 'value')));
        $view->bean = BeanFactory::newBean('Users');

        ob_start();
        $view->listViewPrepare();
        $renderedContent = ob_get_contents();
        ob_end_clean();
        self::assertGreaterThan(0, strlen($renderedContent));
        self::assertEquals('value', $_REQUEST['key']);
    }

    public function testprepareSearchForm(): void
    {
        //test without any REQUEST parameters set. it will set searchform attribute to a searchform object.
        $view1 = new ViewList();
        $view1->module = 'Users';
        $view1->prepareSearchForm();
        self::assertInstanceOf('SearchForm', $view1->searchForm);

        //test with REQUEST parameters set. it will set searchform attribute to a searchform object.
        $view2 = new ViewList();
        $view2->module = 'Users';
        $_REQUEST['search_form'] = true;
        $_REQUEST['searchFormTab'] = 'advanced_search';
        $view2->prepareSearchForm();

        self::assertInstanceOf('SearchForm', $view2->searchForm);
    }

    public function testprocessSearchForm(): void
    {
        //test without use_old_search. it should return html.
        $view = new ViewList();
        $view->prepareSearchForm();

        ob_start();
        $view->processSearchForm();
        $renderedContent = ob_get_contents();
        ob_end_clean();
        self::assertGreaterThan(0, strlen($renderedContent));

        //test with use_old_search = true. there is a $view variable which is never set so it doesn't returns anything.
        $view = new ViewList();
        $view->prepareSearchForm();
        $view->use_old_search = true;

        ob_start();
        $view->processSearchForm();
        $renderedContent = ob_get_contents();
        ob_end_clean();
        self::assertEquals(0, strlen($renderedContent));
    }

    public function testpreDisplay(): void
    {
        //execute the method and test if it sets the lv attribute to ListViewSmarty object.
        $view = new ViewList();
        $view->preDisplay();
        self::assertInstanceOf('ListViewSmarty', $view->lv);
    }

    public function testdisplay(): void
    {
        $query = "SELECT * FROM email_addresses";
        $resource = DBManagerFactory::getInstance()->query($query);
        $rows = [];
        while ($row = $resource->fetch_assoc()) {
            $rows[] = $row;
        }
        $tableEmailAddresses = $rows;

        $view = new ViewList();

        //test without setting bean attibute. it shuold return no access html.
        ob_start();
        $view->display();
        $renderedContent1 = ob_get_contents();
        ob_end_clean();
        self::assertGreaterThan(0, strlen($renderedContent1));

        //test with bean, seed and other arrtibutes set. it shuold return html.
        $view->bean = BeanFactory::newBean('Users');
        $view->seed = BeanFactory::newBean('Users');
        $view->module = 'Users';
        $view->prepareSearchForm();
        $view->preDisplay();

        ob_start();

        $view->display();
        $renderedContent2 = ob_get_contents();
        ob_end_clean();
        self::assertGreaterThan(0, strlen($renderedContent2));


        DBManagerFactory::getInstance()->query("DELETE FROM email_addresses");
        foreach ($tableEmailAddresses as $row) {
            $query = "INSERT email_addresses INTO (";
            $query .= (implode(',', array_keys($row)) . ') VALUES (');
            foreach ($row as $value) {
                $quoteds[] = "'$value'";
            }
            $query .= (implode(', ', $quoteds)) . ')';
            DBManagerFactory::getInstance()->query($query);
        }
    }

    protected function setUp(): void
    {
        parent::setUp();

        global $current_user;
        get_sugar_config_defaults();
        $current_user = BeanFactory::newBean('Users');
    }
}
