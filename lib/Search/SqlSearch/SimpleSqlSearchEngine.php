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

namespace SuiteCRM\Search\SqlSearch;

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

use BeanFactory;
use DBManagerFactory;
use SuiteCRM\Exception\InvalidArgumentException;
use SuiteCRM\Search\SearchEngine;
use SuiteCRM\Search\SearchQuery;
use SuiteCRM\Search\SearchResults;
use SuiteCRM\Search\SearchWrapper;

/**
 * SimpleSqlSearchEngine is a naive search engine that checks the table structure and compares using the LIKE statement.
 *
 * This is mostly intended as a proof of concept.
 */
class SimpleSqlSearchEngine extends SearchEngine
{
    /**
     * Performs a search using the search engine and returns a list SearchResults instance.
     *
     * @param SearchQuery $query
     *
     * @return SearchResults
     * @throws InvalidArgumentException
     */
    public function search(SearchQuery $query): SearchResults
    {
        $modules = SearchWrapper::getModules();

        $results = [];

        $start = microtime(true);
        foreach ($modules as $module) {
            $results[$module] = $this->searchModule($query, $module);
        }
        $end = microtime(true);
        $elapsed = $end - $start;

        return new SearchResults($results, true, $elapsed);
    }

    /**
     * Filters an array of table structures to only retrieve search-relevant fields.
     *
     * @param array $fields
     *
     * @return array
     */
    protected function filterTableStructure(array $fields): array
    {
        $filteredFields = [];

        foreach ($fields as $name => $type) {
            if (strpos((string) $type, 'varchar') === 0) {
                $filteredFields[$name] = 'varchar';
            }

            if (strpos((string) $type, 'text') === 0) {
                $filteredFields[$name] = 'text';
            }
        }

        return $filteredFields;
    }

    /**
     * Uses the DBManager getTableDescription method to retrieve the structure of the table in a name->type format.
     *
     * @param string $table
     *
     * @return array
     * @see \DBManager::getTableDescription()
     */
    protected function getTableStructure(string $table): array
    {
        $descriptions = DBManagerFactory::getInstance()->getTableDescription($table);

        $fields = [];

        foreach ($descriptions as $description) {
            $fields[$description['name']] = $description['type'];
        }

        return $fields;
    }

    /** @inheritdoc */
    protected function validateQuery(SearchQuery $query): void
    {
        parent::validateQuery($query);
        $query->convertEncoding();
    }

    /**
     * Performs a search in a single module table and returns a list of ids.
     *
     * @param SearchQuery $query
     * @param string $module
     *
     * @return array
     */
    private function searchModule(SearchQuery $query, string $module): array
    {
        $table = BeanFactory::getBean($module)->table_name;

        $fields = $this->filterTableStructure($this->getTableStructure($table));

        $sql = $this->makeSearchQuery($query, $table, $fields);

        $hits = [];

        $db = DBManagerFactory::getInstance();

        if (isset($db)) {
            $result = $db->query($sql);

            while ($row = $db->fetchRow($result)) {
                $hits [] = $row['id'];
            }
        }

        return $hits;
    }

    /**
     * Makes the search SQL query.
     *
     * @param SearchQuery $query
     * @param string $table
     * @param array $fields
     *
     * @return string
     */
    private function makeSearchQuery(SearchQuery $query, string $table, array $fields): string
    {
        $sql = 'SELECT id FROM %s WHERE %s AND deleted=0';

        $wheres = [];

        // The database manager is not exposing any database-specific escaping functions,
        // so only addslashes() will be used here.
        $slashedString = addslashes($query->getSearchString());

        foreach (array_keys($fields) as $name) {
            $wheres[] = sprintf("%s LIKE '%s'", $name, $slashedString);
        }

        return sprintf($sql, $table, implode(' OR ', $wheres));
    }
}
