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

namespace SuiteCRM\Search\UI;

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

use BeanFactory;
use Exception;
use LoggerManager;
use SugarBean;
use SuiteCRM\ErrorMessageException;
use SuiteCRM\LangText;
use SuiteCRM\Search\Exceptions\SearchException;
use SuiteCRM\Search\SearchQuery;
use SuiteCRM\Search\SearchResults;
use SuiteCRM\Search\UI\MVC\Controller;
use ViewList;

/**
 * Controller that handles the search results.
 */
class SearchResultsController extends Controller
{

    /**
     *
     * @var SearchQuery
     */
    private $query;

    /**
     *
     * @var SearchResults
     */
    private $results;

    /**
     * SearchResultsController constructor.
     *
     * @param SearchQuery $query
     * @param SearchResults $results
     */
    public function __construct(SearchQuery $query, SearchResults $results)
    {
        parent::__construct(new SearchResultsView());
        $this->query = $query;
        $this->results = $results;
    }

    public function getQuery(): SearchQuery
    {
        return $this->query;
    }

    public function getResults(): SearchResults
    {
        return $this->results;
    }

    public function display(): void
    {
        global $app_strings;
        $headers = [];

        try {
            $headers = $this->getListViewHeaders();
        } catch (Exception $e) {
            LoggerManager::getLogger()->warn('Failed to fetch list-view headers: ' . $e->getMessage());
        }

        $total = $this->results->getTotal();
        if ($total > 1) {
            $size = $this->query->getSize();
            if ($size) {
                $from = $this->query->getFrom();
                $queryString = $_REQUEST['query_string'] ?? '';
                $string = !empty($_REQUEST['search-query-string']) ? $_REQUEST['search-query-string'] : $queryString;

                $page = (int)($from / $size) + 1;
                $prev = $page > 1;
                $next = $total - $from > $size;
                $last = (int)($total / $size) + ($total % $size === 0 ? 0 : 1);

                $this->view->getTemplate()->assign('pagination', [
                    'prev' => $prev,
                    'next' => $next,
                    'page' => $page,
                    'last' => $last,
                    'size' => $size,
                    'from' => $from,
                    'total' => $total,
                    'string' => $string,
                ]);
            } else {
                throw new SearchException('Search Size can not be Zero.', SearchException::ZERO_SIZE);
            }
        }

        $smarty = $this->view->getTemplate();
        $smarty->assign('total', $total);
        $smarty->assign('headers', $headers);
        $smarty->assign('results', $this->results);
        $smarty->assign('APP', $app_strings);
        try {
            $smarty->assign('resultsAsBean', $this->results->getHitsAsBeans());
        } catch (\SuiteCRM\Exception\Exception $e) {
            LoggerManager::getLogger()->fatal("Failed to retrieve ElasticSearch options");
        }

        parent::display();
    }

    /**
     *
     * @return array of header info
     * @throws SearchException
     */
    protected function getListViewHeaders(): array
    {
        $headers = [];
        foreach ($this->getListViewDefs() as $module => $listViewDef) {
            $bean = BeanFactory::getBean($module);
            if (!$bean) {
                throw new SearchException('Module bean not found for search results: ' . $module);
            }
            foreach ($listViewDef as $fieldKey => $fieldValue) {
                if (!empty($fieldValue['default'])) {
                    $header = [];
                    try {
                        $header = $this->getListViewHeader($bean, $fieldKey, $fieldValue);
                    } catch (ErrorMessageException $e) {
                        LoggerManager::getLogger()->warn('Failed to fetch list-view headers: ' . $e->getMessage());
                    }

                    $headers[$module][$fieldKey] = array_merge($fieldValue, $header);
                }
            }
        }

        return $headers;
    }

    /**
     *
     * @return array of list view definitions
     */
    protected function getListViewDefs(): array
    {
        $listViewDefs = [];
        if ($this->results->isGroupedByModule()) {
            foreach (array_keys($this->results->getHits()) as $module) {
                $viewList = new ViewList();
                $viewList->type = 'list';
                $viewList->module = $module;

                $metaDataFile = $viewList->getMetaDataFile();
                if(empty($metaDataFile)) {
                    $GLOBALS['log']->error('getListViewDefs error: No listviewdefs file found for '.$module);
                    continue;
                }
                /** @noinspection PhpIncludeInspection */
                require($metaDataFile);
            }
        }

        return $listViewDefs;
    }

    /**
     *
     * @param SugarBean $bean
     * @param string $fieldKey
     * @param string $fieldValue
     * @return array of header
     * @throws ErrorMessageException
     */
    protected function getListViewHeader(SugarBean $bean, string $fieldKey, $fieldValue): array
    {
        $fieldDef = $bean->getFieldDefinition(strtolower($fieldKey));

        return [
            'label' => $this->getListViewHeaderLabel($bean, $fieldValue, $fieldDef),
            'comment' => $fieldDef['comment'] ?? null,
            'field' => $fieldDef['name'],
        ];
    }

    /**
     *
     * @param SugarBean $bean
     * @param array $fieldValue
     * @param array $fieldDef
     * @return string|null of label
     * @throws ErrorMessageException
     */
    protected function getListViewHeaderLabel(SugarBean $bean, $fieldValue, $fieldDef): ?string
    {
        $label = isset($fieldValue['label']) ?
            LangText::get(
                $fieldValue['label'],
                null,
                LangText::USING_ALL_STRINGS,
                true,
                false,
                $bean->module_name
            ) :
            null;
        if (!$label) {
            $label = isset($fieldDef['vname']) ?
                LangText::get($fieldDef['vname'], null, LangText::USING_ALL_STRINGS, true, false, $bean->module_name) :
                null;
        }

        return $label;
    }
}
