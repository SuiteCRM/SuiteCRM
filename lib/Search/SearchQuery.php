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

namespace SuiteCRM\Search;

use JsonSerializable;

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

/**
 * Class SearchQuery
 *
 * The current format is the following:
 *
 * ```php
 * [
 *  'query' => 'search this',
 *  'from' => 0,
 *  'size' => 100,
 *  'options' => [...]
 * ]
 * ```
 *
 * Use one of the static `from*()` methods to initialize.
 *
 * @see    fromString()
 * @see    fromArray()
 * @author Vittorio Iocolano
 */
#[\AllowDynamicProperties]
class SearchQuery implements JsonSerializable
{
    public const DEFAULT_SEARCH_SIZE = 10;

    /** @var string Search query string */
    private $query;
    /** @var int The number of results per page */
    private $size;
    /** @var int The where to start */
    private $from;
    /** @var null|string Optional parameter to specify the SearchEngine (unqualified class name) to use. */
    private $engine;
    /** @var array Structure containing additional search parameters */
    private $options;

    /**
     * SearchQuery constructor.
     *
     * @param string $searchString Search query
     * @param string|null $engine Name of the search engine to use. `null` will use the default as specified by the
     * config
     * @param null $size Number of results
     * @param int $from Offset of the search. Used for pagination
     * @param array $options [optional] used for additional options by SearchEngines.
     */
    private function __construct(string $searchString, $engine = null, $size = null, $from = 0, array $options = [])
    {
        $this->query = $searchString;
        $this->size = $size ? (int)$size : $this->getDefaultSearchSize();
        $this->from = (int)$from;
        $this->options = $options;
        $this->engine = $engine ? (string)$engine : $this->getDefaultEngine();
    }

    /**
     * Creates a query object from a query string, i.e. from a search from.
     *
     * `$size` and `$from` are for pagination.
     *
     * @param string $searchString A string containing the search query.
     * @param int $size The number of results
     * @param int $from The results offset (for pagination)
     * @param string|null $engine Name of the search engine to use. Use default if `null`
     * @param mixed[] $options Array with options (optional)
     *
     * @return SearchQuery a fully built query
     */
    public static function fromString(
        string $searchString,
        $size = 50,
        $from = 0,
        $engine = null,
        array $options = []
    ): SearchQuery {
        return new self($searchString, $engine, $size, $from, $options);
    }

    /**
     * Makes a query from an array containing data.
     * Fields are:
     * - search-query-string
     * - search-engine
     * - search-query-size
     * - search-query-from
     *
     * @param array $request
     *
     * @return SearchQuery
     */
    public static function fromRequestArray(array $request): SearchQuery
    {
        $searchQuery = self::filterArray($request, 'search-query-string', '', FILTER_SANITIZE_STRING);
        $searchQueryAlt = self::filterArray($request, 'query_string', '', FILTER_SANITIZE_STRING);
        $searchSize = self::filterArray($request, 'search-query-size', null, FILTER_SANITIZE_NUMBER_INT);
        $searchFrom = self::filterArray($request, 'search-query-from', 0, FILTER_SANITIZE_NUMBER_INT);
        $searchEngine = self::filterArray($request, 'search-engine', null, FILTER_SANITIZE_STRING);

        if (!empty($searchQueryAlt) && empty($searchQuery)) {
            $searchQuery = $searchQueryAlt;
        }

        unset(
            $request['search-query-string'],
            $request['query_string'],
            $request['search-query-size'],
            $request['search-query-from'],
            $request['search-engine']
        );

        return new self($searchQuery, $searchEngine, $searchSize, $searchFrom, $request);
    }

    /**
     * Makes a Query from a GET request.
     *
     * @return SearchQuery
     * @see fromRequestArray
     */
    public static function fromGetRequest(): SearchQuery
    {
        return self::fromRequestArray($_GET);
    }

    /**
     * Validates and filters values from an array.
     *
     * @param array $array The array to filter
     * @param string $key The key of the array to load
     * @param mixed $default The default value in case the array value is empty
     * @param null|string $filter Optional filter to be used. e.g. FILTER_SANITIZE_STRING
     *
     * @return mixed
     */
    private static function filterArray(array $array, $key, $default, $filter = null)
    {
        if (!isset($array[$key])) {
            return $default;
        }

        $value = filter_var($array[$key], $filter);

        if ($value === false) {
            return $default;
        }

        return $value;
    }

    /**
     * The offset of the search results.
     *
     * @return int
     */
    public function getFrom(): int
    {
        return $this->from;
    }

    /**
     * The size of the search results.
     *
     * @return int
     */
    public function getSize(): int
    {
        if ($this->size < 0) {
            $this->size = 1;
        }

        return $this->size;
    }

    /**
     * Get the default engine by checking the config
     *
     * @return string
     */
    public function getDefaultEngine(): string
    {
        global $sugar_config;

        if (!empty($sugar_config['search']['defaultEngine'])) {
            $defaultEngine = $sugar_config['search']['defaultEngine'];

            if ($defaultEngine === 'BasicAndAodEngine') {
                $luceneSearch = !empty($sugar_config['aod']['enable_aod']);

                if (array_key_exists('showGSDiv', $_REQUEST) || !empty($_REQUEST['search_fallback'])) {
                    // Search from vanilla sugar search or request for the same
                    $luceneSearch = false;
                }

                return $luceneSearch ? 'LuceneSearchEngine' : 'BasicSearchEngine';
            }

            return (string)$sugar_config['search']['defaultEngine'];
        }

        return 'BasicSearchEngine';
    }

    /**
     * Get the default Search size by checking the config or falling back to Constant value
     *
     * @return int
     */
    public function getDefaultSearchSize(): int
    {
        global $sugar_config;

        if (isset($sugar_config['search']['query_size'])) {
            return (int)$sugar_config['search']['query_size'];
        }

        if (isset($sugar_config['search']['pagination']['min'])) {
            return (int)$sugar_config['search']['pagination']['min'];
        }

        return static::DEFAULT_SEARCH_SIZE;
    }

    /**
     * @return string
     */
    public function getEngine(): string
    {
        return $this->engine;
    }

    /**
     * @param $key
     *
     * @return mixed value
     */
    public function getOption($key)
    {
        return $this->options[$key];
    }

    /**
     * @return array
     */
    public function getOptions(): array
    {
        return $this->options;
    }

    /**
     * Checks if the query string is empty.
     *
     * @return bool
     */
    public function isEmpty(): bool
    {
        return empty($this->query);
    }

    /**
     * The query string if available.
     *
     * If a query object is present `null` is returned.
     *
     * @return string
     */
    public function getSearchString(): string
    {
        return $this->query;
    }

    /**
     * Makes the search string lowercase.
     */
    public function toLowerCase(): void
    {
        $this->query = strtolower($this->query);
    }

    /**
     * Trims the search string.
     */
    public function trim(): void
    {
        $this->query = trim($this->query);
    }

    /**
     * Replaces $what with $with in the search query string.
     *
     * @param $what
     * @param $with
     */
    public function replace($what, $with): void
    {
        $this->query = str_replace($what, $with, $this->query);
    }

    /**
     * Removes forward facing slashes used for escaping in the query string.
     */
    public function stripSlashes(): void
    {
        $this->query = stripslashes($this->query);
    }

    /**
     * Escapes regular expressions so that they are not recognised as such in the query string.
     */
    public function escapeRegex(): void
    {
        $this->query = preg_quote($this->query, '/');
    }

    /**
     * Removes HTML entities and converts them in UTF-8 characters.
     */
    public function convertEncoding(): void
    {
        $string = $this->query;
        preg_match_all("/&#?\w+;/", $string, $entities, PREG_SET_ORDER);
        $entities = array_unique(array_column($entities, 0));
        foreach ($entities as $entity) {
            $decoded = mb_convert_encoding($entity, 'UTF-8', 'HTML-ENTITIES');
            $string = str_replace($entity, $decoded, $string);
        }
        $this->query = $string;
    }

    /** @inheritdoc */
    public function jsonSerialize()
    {
        return [
            'query' => $this->query,
            'size' => $this->size,
            'from' => $this->from,
            'engine' => $this->engine,
            'options' => $this->options,
        ];
    }
}
