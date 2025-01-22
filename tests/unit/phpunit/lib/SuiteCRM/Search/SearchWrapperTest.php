<?php
/**
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

/** @noinspection PhpUnhandledExceptionInspection */
/** @noinspection PhpVoidFunctionResultUsedInspection */

namespace SuiteCRM\Tests\Unit\lib\SuiteCRM\Search;

use Mockery;
use ReflectionException;
use SearchEngineMock;
use SuiteCRM\Search\ElasticSearch\ElasticSearchEngine;
use SuiteCRM\Search\Exceptions\SearchEngineNotFoundException;
use SuiteCRM\Search\SearchEngine;
use SuiteCRM\Search\SearchQuery;
use SuiteCRM\Search\SearchWrapper;

/**
 * Class SearchWrapperTest
 * @package SuiteCRM\Tests\Unit\lib\SuiteCRM\Search
 * @see SearchWrapper
 */
class SearchWrapperTest extends SearchTestAbstract
{
    /**
     * @var string
     */
    private $customEngines = __DIR__ . '/TestCustomEngines/';

    /**
     * @var string
     */
    private $searchEngineMock = __DIR__ . '/SearchEngineMock.php';

    public function testFetchEngine(): void
    {
        $search = new SearchWrapper();

        try {
            $searchEngine = $this->invokeMethod($search, 'fetchEngine', ['ElasticSearchEngine']);
            self::assertInstanceOf(ElasticSearchEngine::class, $searchEngine);
        } catch (ReflectionException $exception) {
            self::fail("Failed to use reflection!");
        }
    }

    public function testFetchEngineNonExisting(): void
    {
        $search = new SearchWrapper();
        $this->setValue($search, 'customEnginePath', $this->customEngines);

        $this->expectException(SearchEngineNotFoundException::class);
        $this->invokeMethod($search, 'fetchEngine', ['VeryFakeEngine']);
    }

    public function testFetchEngineCustom(): void
    {
        $search = new SearchWrapper();
        $this->setValue($search, 'customEnginePath', $this->customEngines);

        $engine = $this->invokeMethod($search, 'fetchEngine', ['MockSearch']);

        self::assertInstanceOf(SearchEngine::class, $engine);
    }

    public function testFetchEngineCustomBad(): void
    {
        $search = new SearchWrapper();
        $this->setValue($search, 'customEnginePath', $this->customEngines);

        $this->expectException(SearchEngineNotFoundException::class);
        $this->invokeMethod($search, 'fetchEngine', ['BadMockSearch']);
    }

    public function testGetEngines(): void
    {
        $expected = [
            0 => 'ElasticSearchEngine',
            1 => 'BasicSearchEngine',
            2 => 'LuceneSearchEngine',
            3 => 'VeryFakeEngine',
            4 => 'MockSearch',
            5 => 'BadMockSearch',
            6 => 'MockSearch',
            7 => 'BadMockSearch',
        ];
        $actual = SearchWrapper::getEngines();

        self::assertEquals($actual, $expected);
    }

    public function testSearchAndDisplayCustom(): void
    {
        $search = new SearchWrapper();
        $this->setValue($search, 'customEnginePath', $this->customEngines);

        $query = SearchQuery::fromString('bar', null, null, 'MockSearch');

        ob_start();
        $search::searchAndDisplay($query);
        $output = ob_get_flush();

        self::assertEquals(1, $output);
    }

    public function testSearchAndDisplayBuiltIn(): void
    {
        SearchWrapper::addEngine('SearchEngineMock', $this->searchEngineMock, SearchEngineMock::class);

        $query = SearchQuery::fromString('foo', null, null, 'SearchEngineMock');

        ob_start();
        SearchWrapper::searchAndDisplay($query);
        $output = ob_get_flush();

        self::assertEquals(1, $output);
    }

    public function testFakeSearch(): void
    {
        SearchWrapper::addEngine('SearchEngineMock', $this->searchEngineMock, SearchEngineMock::class);

        $result = SearchWrapper::search('SearchEngineMock', SearchQuery::fromString('foo'));

        $hits = $result->getHits();

        self::assertEquals('bar', $hits[0]);

        $result = SearchWrapper::search('SearchEngineMock', SearchQuery::fromString('fooz'));

        $hits = $result->getHits();

        self::assertEquals('barz', $hits[0]);
    }

    public function testSearchCustomEngine(): void
    {
        $mockEngine = Mockery::mock(SearchEngine::class);
        $query = SearchQuery::fromString("Test");

        $mockEngine
            ->shouldReceive('search')
            ->once()
            ->with($query);

        try {
            SearchWrapper::search($mockEngine, $query);
            self::assertTrue(true);
        } catch (SearchEngineNotFoundException $e) {
            self::fail($e->getMessage() . "\nTrace:\n" . $e->getTraceAsString());
        }

        Mockery::close();
    }

    public function testGetModules(): void
    {
        $actual = SearchWrapper::getModules();

        self::assertIsArray($actual);
        self::assertGreaterThan(1, count($actual));
    }

    public function testGetDefaultEngine(): void
    {
        global $sugar_config;

        $sugar_config['search']['defaultEngine'] = 'foo';

        self::assertEquals('foo', SearchWrapper::getDefaultEngine());
    }

    public function testGetController(): void
    {
        global $sugar_config;

        $sugar_config['search']['controller'] = 'foo';

        self::assertEquals('foo', SearchWrapper::getController());
    }
}
