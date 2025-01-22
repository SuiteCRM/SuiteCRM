<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
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

use SuiteCRM\SubPanel\SubPanelRowCounter;
use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

class SubPanelTest extends SuitePHPUnitFrameworkTestCase
{
    protected function setUp(): void
    {
        parent::setUp();
        if (!defined('sugarEntry')) {
            define('sugarEntry', true);
        }
    }

    public function testFaultySubpanelDef(): void
    {
        $bean = BeanFactory::newBean('Contacts');
        $counter = new SubPanelRowCounter($bean);
        $subPanelDefs = [];

        $count = $counter->getSubPanelRowCount($subPanelDefs);

        self::assertEquals(-1, $count);
    }

    public function testSelectQueryToCountQuery(): void
    {
        $bean = BeanFactory::newBean('Contacts');
        $counter = new SubPanelRowCounter($bean);

        $select = 'SELECT id FROM table';
        $count = $counter->selectQueryToCountQuery($select);
        $expected = 'SELECT COUNT(id) FROM table LIMIT 1';

        self::assertEquals($expected, $count);

        $selectAlias = 'SELECT contact_id id FROM table';
        $countAlias = $counter->selectQueryToCountQuery($selectAlias);
        $expectedAlias = 'SELECT COUNT(contact_id) FROM table LIMIT 1';

        self::assertEquals($expectedAlias, $countAlias);

        $selectAs = 'SELECT contact_id as id FROM table';
        $countAs = $counter->selectQueryToCountQuery($selectAs);
        $expectedAs = 'SELECT COUNT(contact_id) FROM table LIMIT 1';

        self::assertEquals($expectedAs, $countAs);
    }

    public function testMakeFunctionCountQuery(): void
    {
        $bean = BeanFactory::newBean('Accounts');
        $counter = new SubPanelRowCounter($bean);

        $nonExistantQuery = $counter->makeFunctionCountQuery('');
        self::assertEquals('', $nonExistantQuery);

        $existantQuery = $counter->makeFunctionCountQuery('function:getProductsServicesPurchasedQuery');
        $expectedQueryStart = 'SELECT COUNT(aos_products_quotes.id)';
        self::assertStringContainsString($expectedQueryStart, $existantQuery);
    }

    public function testMakeSubPanelRowCountQuery(): void
    {
        $bean = BeanFactory::newBean('Contacts');
        $counter = new SubPanelRowCounter($bean);
        $subPanelDefs = ['get_subpanel_data' => 'accounts'];
        $counter->setSubPanelDefs($subPanelDefs);

        $query  = $counter->makeSubPanelRowCountQuery();
        $expectedQueryStart = 'SELECT COUNT(account_id) FROM accounts_contacts';

        self::assertStringContainsString($expectedQueryStart, $query);
    }
}
