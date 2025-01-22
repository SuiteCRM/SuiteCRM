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

use Elasticsearch\Client;
use Mockery\MockInterface;
use SuiteCRM\Search\ElasticSearch\ElasticSearchEngine;
use SuiteCRM\Search\SearchQuery;
use SuiteCRM\Tests\Unit\lib\SuiteCRM\Search\SearchTestAbstract;

/** @noinspection PhpIncludeInspection */
require_once 'lib/Search/ElasticSearch/ElasticSearchEngine.php';

/**
 * Class ElasticSearchEngineTest
 */
class ElasticSearchEngineTest extends SearchTestAbstract
{
    public function testValidateQuery(): void
    {
        $engine = new ElasticSearchEngine();
        $str = " test AND test2 OR t-test3 ";
        $exp = "test AND test2 OR t-test3";
        $query = SearchQuery::fromString($str);

        $this->invokeMethod($engine, 'validateQuery', [&$query]);

        self::assertEquals($exp, $query->getSearchString());
    }

    public function testCreateSearchParams1(): void
    {
        $engine = new ElasticSearchEngine();
        $searchString = "hello search*";
        $size = 30;
        $from = 5;

        $query = SearchQuery::fromString($searchString, $size, $from);

        $expectedParams = [
            'index' => 'accounts,contacts,opportunities,calls,documents,cases,aos_contracts,leads,meetings,notes,campaigns',
            'body' => [
                'stored_fields' => [],
                'from' => $from,
                'size' => $size,
                'query' => [
                    'query_string' => [
                        'query' => $searchString,
                        'analyzer' => 'standard',
                        'fields' => ['name.*^5', '*'],
                        'default_operator' => 'OR',
                        'minimum_should_match' => '66%'
                    ]
                ]
            ]
        ];

        $params = $this->invokeMethod($engine, 'createSearchParams', [$query]);

        self::assertEquals($expectedParams, $params);
    }

    public function testCreateSearchParams2(): void
    {
        $engine = new ElasticSearchEngine();
        $searchString = "test*";
        $size = 30;

        $query = SearchQuery::fromString($searchString, $size);

        $expectedParams = [
            'index' => 'accounts,contacts,opportunities,calls,documents,cases,aos_contracts,leads,meetings,notes,campaigns',
            'body' => [
                'stored_fields' => [],
                'from' => 0,
                'size' => $size,
                'query' => [
                    'query_string' => [
                        'query' => $searchString,
                        'analyzer' => 'standard',
                        'fields' => ['name.*^5', '*'],
                        'default_operator' => 'OR',
                        'minimum_should_match' => '66%'
                    ]
                ]
            ]
        ];

        $params = $this->invokeMethod($engine, 'createSearchParams', [$query]);

        self::assertEquals($expectedParams, $params);
    }

    public function testRunElasticSearch(): void
    {
        $query = SearchQuery::fromString("a");

        $mockedResults = $this->getMockedHits();

        $client = $this->getMockedClient($mockedResults);

        $engine = new ElasticSearchEngine($client);
        $searchQuery = $this->getSearchQuery($engine, $query);
        $results = $this->invokeMethod($engine, 'runElasticSearch', [$searchQuery]);

        self::assertEquals($mockedResults, $results);
    }

    /**
     * @return array
     */
    private function getMockedHits(): array
    {
        return [
            'took' => 5,
            'timed_out' => false,
            '_shards' =>
                [
                    'total' => 3,
                    'successful' => 2,
                    'skipped' => 0,
                    'failed' => 0,
                ],
            'hits' =>
                [
                    'total' => 258,
                    'max_score' => 1.0,
                    'hits' =>
                        [
                            0 =>
                                [
                                    '_index' => 'accounts,contacts,opportunities,calls,documents,cases,aos_contracts,leads,meetings,notes,campaigns',
                                    '_type' => 'Accounts',
                                    '_id' => 'id1',
                                    '_score' => 1.0,
                                ],
                            1 =>
                                [
                                    '_index' => 'accounts,contacts,opportunities,calls,documents,cases,aos_contracts,leads,meetings,notes,campaigns',
                                    '_type' => 'Accounts',
                                    '_id' => 'id2',
                                    '_score' => 1.0,
                                ],
                            2 =>
                                [
                                    '_index' => 'accounts,contacts,opportunities,calls,documents,cases,aos_contracts,leads,meetings,notes,campaigns',
                                    '_type' => 'Contacts',
                                    '_id' => 'id3',
                                    '_score' => 0.5,
                                ],
                        ]
                ],
        ];
    }

    /**
     * @param $mockedResults
     * @return MockInterface
     */
    private function getMockedClient($mockedResults): MockInterface
    {
        $client = Mockery::mock(Client::class);

        $client
            ->shouldReceive('search')
            ->once()
            ->andReturn($mockedResults);

        return $client;
    }

    /**
     * @return array
     */
    private function getExpectedResultsForMockedHits(): array
    {
        return [
            'Accounts' => [
                'id1',
                'id2'
            ],
            'Contacts' => [
                'id3',
            ]
        ];
    }

    public function testParseEmptyHits(): void
    {
        $engine = new ElasticSearchEngine();

        $mockedHits = $this->getMockedHitsEmpty();

        $expectedResults = [];

        $results = $this->invokeMethod($engine, 'parseHits', [$mockedHits]);

        self::assertEquals($expectedResults, $results);
    }

    private function getMockedHitsEmpty(): array
    {
        return [
            'took' => 1,
            'timed_out' => false,
            '_shards' =>
                [
                    'total' => 5,
                    'successful' => 5,
                    'skipped' => 0,
                    'failed' => 0,
                ],
            'hits' => [
                "total" => 0,
                "max_score" => null,
                "hits" => [],
            ]
        ];
    }

    /**
     * @param ElasticSearchEngine $engine
     * @param SearchQuery $query
     * @return array
     * @throws ReflectionException
     */
    private function getSearchQuery(ElasticSearchEngine $engine, SearchQuery $query): array
    {
        $this->invokeMethod($engine, 'validateQuery', [&$query]);

        return $this->invokeMethod($engine, 'createSearchParams', [&$query]);
    }
}
