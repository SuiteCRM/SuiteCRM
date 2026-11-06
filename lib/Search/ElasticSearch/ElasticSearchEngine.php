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

namespace SuiteCRM\Search\ElasticSearch;

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

use Elasticsearch\Client;
use Elasticsearch\Common\Exceptions\BadRequest400Exception;
use SuiteCRM\Exception\InvalidArgumentException;
use SuiteCRM\Search\SearchEngine;
use SuiteCRM\Search\SearchQuery;
use SuiteCRM\Search\SearchResults;

/**
 * SearchEngine that use Elasticsearch index for performing almost real-time search.
 */
#[\AllowDynamicProperties]
class ElasticSearchEngine extends SearchEngine
{
    /** @var Client */
    private $client;

    /**
     * ElasticSearchEngine constructor.
     *
     * @param Client|null $client
     */
    public function __construct(?Client $client = null)
    {
        $this->client = $client ?? ElasticSearchClientBuilder::getClient();
    }

    /**
     * @inheritdoc
     * @throws InvalidArgumentException
     */
    public function search(SearchQuery $query): SearchResults
    {
        $totalHits = 0;
        $results = [];
        $this->validateQuery($query);
        $params = $this->createSearchParams($query);
        $start = microtime(true);
        
        foreach ($params as $param) {
            try {
                $hits = $this->runElasticSearch($param);
            } catch (BadRequest400Exception $exception) {
                // Elasticsearch rejected the query syntax: retry with the search text escaped
                $param['body']['query']['query_string']['query'] = $this->buildQueryString($query->getSearchString(), true);
                $hits = $this->runElasticSearch($param);
            }
            $results += $this->parseHits($hits);
            $totalHits += $hits['hits']['total']['value'];
        }
        
        $end = microtime(true);
        $elapsed = ($end - $start);

        return new SearchResults($results, true, $elapsed, $totalHits);
    }

    /**
     * @param SearchQuery $query
     */
    protected function validateQuery(SearchQuery $query): void
    {
        $query->trim();
        $query->convertEncoding();
    }

    /**
     * Generates the parameter array for the Elasticsearch API from a SearchQuery.
     *
     * @param SearchQuery $query
     *
     * @return array
     */
    private function createSearchParams(SearchQuery $query): array
    {
        $searchStr = $this->buildQueryString($query->getSearchString(), false);
        $searchModules = $query->getModules();

        $indexes = array_map('strtolower', $searchModules);
        $results = [];

        foreach ($indexes as $index) {
            $results[] = [
                'index' => $index,
                'body' => [
                    'stored_fields' => [],
                    'from' => $query->getFrom(),
                    'size' => $query->getSize(),
                    'query' => [
                        'query_string' => [
                            'query' => $searchStr,
                            'fields' => ['name.*^5', '*'],
                            'analyzer' => 'standard',
                            'default_operator' => 'OR',
                            'minimum_should_match' => '66%',
                        ],
                    ],
                ],
            ];
        }
        return $results;
    }

    /**
     * Builds the query_string text sent to Elasticsearch.
     *
     * The configured frontend wildcard becomes '*' first. A '*' is added at the end (and at the start
     * when search_wildcard_infront is set), but never next to a double quote, where it would turn a
     * phrase into a match-all.
     *
     * With $escape (used only to retry a query Elasticsearch rejected), quotes are replaced with spaces,
     * '<' and '>' are removed (query_string cannot escape them), and the other reserved characters are
     * escaped with a backslash.
     *
     * @param string $searchStr
     * @param bool   $escape
     *
     * @return string
     */
    private function buildQueryString(string $searchStr, bool $escape): string
    {
        $wildcard = '*';

        $frontendWildcard = $GLOBALS['sugar_config']['search_wildcard_char'] ?? null;
        if ($frontendWildcard !== $wildcard && strlen((string)$frontendWildcard) === 1) {
            $searchStr = str_replace($frontendWildcard, $wildcard, $searchStr);
        }

        if ($escape) {
            $searchStr = trim(str_replace('"', ' ', $searchStr));
            $searchStr = str_replace(['<', '>'], '', $searchStr);
            $searchStr = preg_replace('/([+\-=&|!(){}\[\]^~?:\\\\\/])/', '\\\\$1', $searchStr);
        }

        if ($searchStr === '') {
            return $searchStr;
        }

        if (($GLOBALS['sugar_config']['search_wildcard_infront'] ?? false) === true
            && $searchStr[0] !== $wildcard && $searchStr[0] !== '"') {
            $searchStr = $wildcard . $searchStr;
        }

        $last = substr($searchStr, -1);
        if ($last !== $wildcard && $last !== '"') {
            $searchStr .= $wildcard;
        }

        return $searchStr;
    }

    /**
     * Calls the Elasticsearch API.
     *
     * @param array $params
     *
     * @return array
     */
    private function runElasticSearch(array $params): array
    {
        return $this->client->search($params);
    }

    /**
     * Reads the array returned from the Elasticsearch API
     * and converts it into an associative array of ids, grouped by Module.
     *
     * @param array $hits
     *
     * @return array
     */
    private function parseHits(array $hits): array
    {
        $hitsArray = $hits['hits']['hits'];

        $initialResults = [];

        foreach ($hitsArray as $hit) {
            $recordModule = $hit['_index'];
            $initialResults[$recordModule][] = $hit['_id'];
        }

        $searchResults = [];

        foreach ($initialResults as $index => $hit) {
            $params = ['index' => $index];
            $meta = $this->client->indices()->getMapping($params);
            $moduleName = $meta[$index]['mappings']['_meta']['module_name'];
            $searchResults[$moduleName]['results'] = $hit;
            $searchResults[$moduleName]['totalHits'] = $hits['hits']['total']['value'] ?: 0;
        }

        return $searchResults;
    }
}
