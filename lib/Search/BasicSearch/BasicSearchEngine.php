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

/** @noinspection PhpUndefinedFieldInspection */

namespace SuiteCRM\Search\BasicSearch;

use BeanFactory;
use DBManagerFactory;
use ListViewData;
use SugarBean;
use SuiteCRM\Exception\Exception;
use SuiteCRM\Search\SearchEngine;
use SuiteCRM\Search\SearchModules;
use SuiteCRM\Search\SearchQuery;
use SuiteCRM\Search\SearchResults;

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

/**
 * Class BasicAndAodEngine
 */
#[\AllowDynamicProperties]
class BasicSearchEngine extends SearchEngine
{
    /* path to search form */
    public $searchFormPath = 'include/SearchForm/SearchForm2.php';

    /*search form class name*/
    public $searchFormClass = 'SearchForm';

    /**
     * Search function run when user goes to Show All and runs a search again.  This outputs the search results
     * calling upon the various listview display functions for each module searched on.
     *
     * @param SearchQuery $query
     *
     * @return SearchResults
     * @throws Exception
     */
    public function search(SearchQuery $query): SearchResults
    {
        $totalHits = 0;
        $modulesToSearch = $query->getModules();
        $start = microtime(true);
        $results = $this->searchModules($modulesToSearch, $query->getSearchString(), $query->getSize(), $query->getFrom());
        $end = microtime(true);
        $elapsed = $end - $start;
                
        $totalHits = $_SESSION['basicSearchTotal'] ?: 0;
        
        return new SearchResults($results, true, $elapsed, $totalHits);
    }

    /**
     *
     * @param array $modulesToSearch
     * @param string $searchQuery
     * @return array
     * @noinspection DisconnectedForeachInstructionInspection
     * @noinspection PhpIncludeInspection
     */
    private function searchModules(array $modulesToSearch, string $searchQuery, int $size, int $from): array
    {
        $unifiedSearchModules = SearchModules::getUnifiedSearchModules();
        $_SESSION['basicSearchTotal'] = $from === 0 ? 0 : $_SESSION['basicSearchTotal'];
        $moduleResults = [];
        $moduleCounts = [];
        $listViewDefs = [];
        require_once('include/ListView/ListViewData.php');
        if (!empty($searchQuery)) {
            foreach ($modulesToSearch as $moduleName => $label) {
                $seed = BeanFactory::newBean($moduleName);
                $listViewData = new ListViewData();

                // Retrieve the original list view defs and store for processing in case of custom layout changes
                require(__DIR__ . '/../../../modules/' . $seed->module_dir . '/metadata/listviewdefs.php');
                $origListViewDefs = $listViewDefs;

                if (file_exists('custom/modules/' . $seed->module_dir . '/metadata/listviewdefs.php')) {
                    require('custom/modules/' . $seed->module_dir . '/metadata/listviewdefs.php');
                }

                if (!isset($listViewDefs[$seed->module_dir])) {
                    continue;
                }

                $unifiedSearchFields = [];
                $innerJoins = [];
                
                if (!isset($unifiedSearchModules[$moduleName]['fields'])) {
                    continue;
                }
                
                foreach ($unifiedSearchModules[$moduleName]['fields'] as $field => $def) {
                    $listViewCheckField = strtoupper($field);
                    // Check to see if the field is in listview defs
                    // Check to see if field is in original list view defs (in case we are using custom layout defs)
                    if (empty($listViewDefs[$seed->module_dir][$listViewCheckField]['default']) &&
                        !empty($origListViewDefs[$seed->module_dir][$listViewCheckField]['default'])) {
                        // If we are here then the layout has been customized, but the field is still needed for query
                        // creation
                        $listViewDefs[$seed->module_dir][$listViewCheckField] = $origListViewDefs[$seed->module_dir][$listViewCheckField];
                    }

                    if (!empty($def['innerjoin'])) {
                        if (empty($def['db_field'])) {
                            continue;
                        }
                        $def['innerjoin'] = str_replace('INNER', 'LEFT', (string) $def['innerjoin']);
                    }

                    if (isset($seed->field_defs[$field]['type'])) {
                        $type = $seed->field_defs[$field]['type'];
                        if ($type === 'int' && !is_numeric($searchQuery)) {
                            continue;
                        }
                    }

                    $unifiedSearchFields[$moduleName] [$field] = $def;
                    $unifiedSearchFields[$moduleName] [$field]['value'] = $searchQuery;
                }

                /*
                 * Use searchForm2->generateSearchWhere() to create the search query, as it can generate SQL for the full set of comparisons required
                 * generateSearchWhere() expects to find the search conditions for a field in the 'value' parameter of the searchFields entry for that field
                 */

                require_once $this->searchFormPath;
                $searchForm = new $this->searchFormClass($seed, $moduleName);

                $searchForm->setup(
                    [$moduleName => []],
                    $unifiedSearchFields,
                    '',
                    'saved_views'
                );
                $whereClauses = $searchForm->generateSearchWhere();
                //add inner joins back into the where clause
                $params = ['custom_select' => ""];
                foreach ($innerJoins as $field => $def) {
                    if (isset($def['db_field'])) {
                        foreach ($def['db_field'] as $dbfield) {
                            $whereClauses[] = $dbfield . " LIKE '" . DBManagerFactory::getInstance()->quote($searchQuery) . "%'";
                        }
                        $params['custom_select'] .= ", $dbfield";
                        $params['distinct'] = true;
                    }
                }

                if (!empty($whereClauses)) {
                    $where = '((' . implode(' ) OR ( ', $whereClauses) . '))';
                } else {
                    $where = '';
                }

                $filter_fields = $this->buildFilterFields($seed);

                $listData = $listViewData->getListViewData($seed, $where, $from, $size, $filter_fields, $params);
                if($listData['data']) {
                    $moduleCounts[$moduleName] = $listData['pageData']['offsets']['total'];
                    $_SESSION['basicSearchTotal'] += $from === 0 ? $listData['pageData']['offsets']['total'] : 0;
                } else {
                    $moduleCounts[$moduleName] = 0;
                }

                foreach ($listData['data'] as $hit) {
                    $moduleResults[$moduleName]['results'][] = $hit['ID'];
                }
                $moduleResults[$moduleName]['totalHits'] = $moduleCounts[$moduleName];
            }
        }

        return $moduleResults;
    }

    /**
     * Build filter fields for list view data search
     * @param SugarBean $bean
     * @return array
     */
    protected function buildFilterFields(SugarBean $bean): array
    {
        $parsedFilterFields = array();
        $excludedRelationshipFields = array();

        foreach ($bean->field_defs as $fieldName => $fieldDefinition) {
            $type = $fieldDefinition['type'] ?? '';
            $listShow = $fieldDefinition['list-show'] ?? true;
            if ($type === 'link' || $listShow === false) {
                continue;
            }

            $linkType = $fieldDefinition['link_type'] ?? '';
            if ($linkType === 'relationship_info') {
                $excludedRelationshipFields[] = $fieldName;
                $relationshipFields = $fieldDefinition['relationship_fields'] ?? [];
                if (!empty($relationshipFields)) {
                    foreach ($relationshipFields as $relationshipField) {
                        $excludedRelationshipFields[] = $relationshipField;
                    }
                }
            }

            $parsedFilterFields[] = $fieldName;
        }

        return array_diff($parsedFilterFields, $excludedRelationshipFields);
    }
}
