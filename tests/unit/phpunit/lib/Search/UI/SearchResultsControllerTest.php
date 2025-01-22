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

use SuiteCRM\Search\SearchQuery;
use SuiteCRM\Search\SearchResults;
use SuiteCRM\Search\UI\SearchResultsController;
use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;


if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

/**
 * Description of SearchResultsControllerTest
 *
 * @author gyula
 */
class SearchResultsControllerTest extends SuitePHPUnitFrameworkTestCase
{
    public function testDisplayFoundOnePage(): void
    {
        $ids = [];
        for ($i=0; $i<15; $i++) {
            $account = BeanFactory::getBean('Accounts');
            $account->name = 'test account ' . $i;
            $ok = $account->save();
            self::assertTrue((bool)$ok);
            $ids[] = $account->id;
        }
        self::assertCount(15, $ids);

        $request = [
            'search-query-string' => 'test account',
            'query_string' => 'test account',
            'search-query-size' => 10,
            'search-query-from' => 0,
            'search-engine' => 0,
        ];
        $query = SearchQuery::fromRequestArray($request);
        $hits = [
            'Accounts' => $ids,
        ];
        $groupedByModule = true;
        $searchTime = 0.05;
        $total = 15;
        $scores = null;
        $options = null;
        $results = new SearchResults($hits, $groupedByModule, $searchTime, $total, $scores, $options);
        $searchResultsController = new SearchResultsController($query, $results);
        ob_start();
        $searchResultsController->display();
        $content = ob_get_contents();
        ob_end_clean();
        self::assertStringContainsString('Total result(s): 15', $content);

        // add 5 more..
        for ($i=15; $i<20; $i++) {
            $account = BeanFactory::getBean('Accounts');
            $account->name = 'test account ' . $i;
            $ok = $account->save();
            self::assertTrue((bool)$ok);
            $ids[] = $account->id;
        }
        self::assertCount(20, $ids);

        $request = [
            'search-query-string' => 'test account',
            'query_string' => 'test account',
            'search-query-size' => 10,
            'search-query-from' => 10,
            'search-engine' => 0,
        ];
        $query = SearchQuery::fromRequestArray($request);
        $hits = [
            'Accounts' => $ids,
        ];
        $groupedByModule = true;
        $searchTime = 0.05;
        $total = 20;
        $scores = null;
        $options = null;
        $results = new SearchResults($hits, $groupedByModule, $searchTime, $total, $scores, $options);
        $searchResultsController = new SearchResultsController($query, $results);
        ob_start();
        $searchResultsController->display();
        $content = ob_get_contents();
        ob_end_clean();
        self::assertStringContainsString('Total result(s): 20', $content);
    }

    public function testDisplayFoundOne(): void
    {
        $account = BeanFactory::getBean('Accounts');
        $account->name = 'test account 1';
        $ok = $account->save();
        self::assertTrue((bool)$ok);

        $request = [
            'search-query-string' => 'test account',
            'query_string' => 'test account',
            'search-query-size' => 10,
            'search-query-from' => 0,
            'search-engine' => 0,
        ];
        $query = SearchQuery::fromRequestArray($request);
        $hits = [
            'Accounts' => [$account->id],
        ];
        $groupedByModule = true;
        $searchTime = 0.05;
        $total = 1;
        $scores = null;
        $options = null;
        $results = new SearchResults($hits, $groupedByModule, $searchTime, $total, $scores, $options);
        $searchResultsController = new SearchResultsController($query, $results);
        ob_start();
        $searchResultsController->display();
        $content = ob_get_contents();
        ob_end_clean();
        self::assertStringContainsString('test account 1', $content);
    }

    public function testDisplayNotFound(): void
    {
        $request = [
            'search-query-string' => 'test query string (not found)',
            'query_string' => 'test query string (not found) alt',
            'search-query-size' => 10,
            'search-query-from' => 0,
            'search-engine' => 0,
        ];
        $query = SearchQuery::fromRequestArray($request);
        $hits = [];
        $groupedByModule = true;
        $searchTime = null;
        $total = null;
        $scores = null;
        $options = null;
        $results = new SearchResults($hits, $groupedByModule, $searchTime, $total, $scores, $options);
        $searchResultsController = new SearchResultsController($query, $results);
        ob_start();
        $searchResultsController->display();
        $content = ob_get_contents();
        ob_end_clean();
        self::assertStringContainsString('No results matching your search criteria. Try broadening your search.', $content);
    }
}
