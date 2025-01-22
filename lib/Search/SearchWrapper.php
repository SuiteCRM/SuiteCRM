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

namespace SuiteCRM\Search;

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

use SuiteCRM\Search\AOD\LuceneSearchEngine;
use SuiteCRM\Search\BasicSearch\BasicSearchEngine;
use SuiteCRM\Search\ElasticSearch\ElasticSearchEngine;
use SuiteCRM\Search\Exceptions\SearchEngineNotFoundException;

/**
 * Class SearchWrapper performs a unified search using one of the available search engines.
 *
 * @author Vittorio Iocolano
 */
#[\AllowDynamicProperties]
class SearchWrapper
{
    /**
     * @var array stores an associative array matching the search engine class name with the file it is stored in.
     */
    private static $engines = [
        'ElasticSearchEngine' => [
            'name' => 'ElasticSearchEngine',
            'FQN' => ElasticSearchEngine::class,
            'filepath' => 'lib/Search/ElasticSearch/ElasticSearchEngine.php'
        ],
        'BasicSearchEngine' => [
            'name' => 'BasicSearchEngine',
            'FQN' => BasicSearchEngine::class,
            'filepath' => 'lib/Search/BasicSearch/BasicSearchEngine.php'
        ],
        'LuceneSearchEngine' => [
            'name' => 'LuceneSearchEngine',
            'FQN' => LuceneSearchEngine::class,
            'filepath' => 'lib/Search/AOD/LuceneSearchEngine.php'
        ],
    ];

    /** @var string Path to the folder where to load custom engines from */
    private static $customEnginePath = __DIR__ . '/../../custom/Extension/SearchEngines/';

    /**
     * Perform a search with the given query and engine.
     *
     * If no search engine field is provided in the query, the default one will be used.
     *
     * @param SearchQuery $query
     */
    public static function searchAndDisplay(SearchQuery $query): void
    {
        $engine = !empty($query->getEngine()) ? $query->getEngine() : self::getDefaultEngine();

        $engine = self::fetchEngine($engine);
        $engine->searchAndDisplay($query);
    }

    /**
     * Perform a search with the given query and engine.
     *
     * Results are grouped by module.
     *
     * @param string|SearchEngine $engine
     * @param SearchQuery $query
     *
     * @return SearchResults
     */
    public static function search($engine, SearchQuery $query): SearchResults
    {
        $engine = self::fetchEngine($engine);

        return $engine->search($query);
    }

    /**
     * Binds a class name / engine name to a file.
     *
     * @param string $engineName
     * @param string $file
     * @param $fqn
     */
    public static function addEngine(string $engineName, string $file, $fqn): void
    {
        self::$engines[$engineName] = [
            'name' => $engineName,
            'FQN' => $fqn,
            'filepath' => $file,
        ];
    }

    /**
     * Retrieves the available search engines class names.
     *
     * @return string[]
     */
    public static function getEngines(): array
    {
        $default = array_keys(self::$engines);
        $custom = [];
        foreach (glob(self::$customEnginePath . '*.php', GLOB_NOSORT) as $file) {
            $file = pathinfo((string) $file);
            $custom[] = $file['filename'];
        }

        return array_merge($default, $custom);
    }

    /**
     * Retrieves the default search engine name from the global configuration.
     *
     * If the default config does not have an engine, the first engine from the engines array is returned.
     *
     * @return string
     */
    public static function getDefaultEngine(): string
    {
        $config = self::getSearchConfig('defaultEngine');

        return $config ?? key(self::$engines);
    }

    /**
     * Returns the configured search controller from the sugar config.
     *
     * If the value is, for some reason, not set, `null` is returned.
     *
     * @return string|null
     */
    public static function getController(): ?string
    {
        return self::getSearchConfig('controller');
    }

    /**
     * Returns the configured modules to be used with search.
     *
     * @return array|null
     */
    public static function getModules(): ?array
    {
        return SearchModules::getEnabledModules();
    }

    /**
     * Performs various validation and retrieves an instance of a given search engine.
     *
     * It first searches in the default definitions array `self::$engines`,
     * then attempts to find a matching engine in the folder `self::CUSTOM_ENGINES_PATH`.
     *
     * @param string|SearchEngine $engineName
     *
     * @return SearchEngine
     * @throws SearchEngineNotFoundException
     */
    private static function fetchEngine($engineName): SearchEngine
    {
        if (is_subclass_of($engineName, SearchEngine::class, false)) {
            return $engineName;
        }

        $customEnginePath = self::$customEnginePath . $engineName . '.php';

        if (isset(self::$engines[$engineName])) {
            $engine = self::$engines[$engineName];
        } elseif (isset($customEnginePath)) {
            self::addEngine($engineName, $customEnginePath, $engineName);
            $engine = self::$engines[$engineName];
        } else {
            throw new SearchEngineNotFoundException("Search engine not found for engine '$engineName''.");
        }

        $filename = $engine['filepath'];

        if (!is_file($filename)) {
            throw new SearchEngineNotFoundException("Unable to find search file '$filename'' for engine '$engineName''.");
        }

        /** @noinspection PhpIncludeInspection */
        require_once $filename;

        if (!is_subclass_of($engine['FQN'], SearchEngine::class)) {
            throw new SearchEngineNotFoundException("The provided class '$engineName' is not a subclass of SearchEngine");
        }

        /** @var SearchEngine */
        return new $engine['FQN']();
    }

    /**
     * Returns a configured parameter from the sugar config.
     *
     * If the values is not set, returns `null`.
     *
     * @param $key
     *
     * @return mixed|null
     */
    private static function getSearchConfig($key)
    {
        /** @noinspection PhpVariableNamingConventionInspection */
        global $sugar_config;

        return $sugar_config['search'][$key] ?? null;
    }
}
