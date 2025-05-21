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

namespace SuiteCRM\Search\AOD;

use ACLController;
use BeanFactory;
use SecurityGroup;
use stdClass;
use SugarBean;
use SuiteCRM\Exception\Exception;
use SuiteCRM\Search\SearchEngine;
use SuiteCRM\Search\SearchQuery;
use SuiteCRM\Search\SearchResults;


if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

/**
 * Class LuceneSearchEngine
 * @package SuiteCRM\Search\AOD
 */
#[\AllowDynamicProperties]
class LuceneSearchEngine extends SearchEngine
{
    /** @var bool|SugarBean */
    private $index;

    public function __construct()
    {
        $this->index = BeanFactory::getBean("AOD_Index")->getIndex();
    }

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
        $queryString = $query->getSearchString();

        $start = microtime(true);
        $hits = $this->runLucene($queryString);
        $results = $this->parseHits($hits);
        $end = microtime(true);
        $elapsed = $end - $start;

        return new SearchResults($results['modules'], true, $elapsed, is_countable($results['hits']) ? count($results['hits']) : 0);
    }

    /**
     *
     * @param string $queryString
     * @return array
     */
    private function runLucene(string $queryString): array
    {
        $cachePath = 'cache/modules/AOD_Index/QueryCache/' . md5($queryString);
        if (is_file($cachePath)) {
            $mTime = filemtime($cachePath);
            if ($mTime > (time() - 5 * 60)) {
                $hits = unserialize(sugar_file_get_contents($cachePath));
            }
        }

        if (!isset($hits)) {
            $hits = $this->newHit($queryString);
        }

        return $hits;
    }

    /**
     * @param string $queryString
     * @return array
     */
    private function newHit(string $queryString): array
    {
        global $current_user;

        $tmphits = $this->index->find($queryString);
        $hits = [];
        foreach ($tmphits as $hit) {
            $bean = BeanFactory::getBean($hit->record_module, $hit->record_id);
            if (empty($bean)) {
                continue;
            }
            if ($bean->bean_implements('ACL') && !is_admin($current_user)) {
                $in_group = SecurityGroup::groupHasAccess($bean->module_dir, $bean->id, 'list');
                $is_owner = $bean->isOwner($current_user->id);
                $access = ACLController::checkAccess($bean->module_dir, 'list', $is_owner, 'module', $in_group);
                if (!$access) {
                    continue;
                }
            }
            $newHit = new stdClass;
            $newHit->record_module = $hit->record_module;
            $newHit->record_id = $hit->record_id;
            $hits[] = $newHit;
        }
        $this->cacheQuery($queryString, $hits);

        return $hits;
    }

    /**
     * @param mixed $hits
     * @return array
     */
    private function parseHits(array $hits): array
    {
        $searchResults = [];

        foreach ($hits as $hit) {
            $recordModule = $hit->record_module;
            $searchResults[$recordModule][] = $hit->record_id;
        }

        return [
            'hits' => $hits,
            'modules' => $searchResults
        ];
    }

    /**
     * @param string $queryString
     * @param array $resArray
     */
    private function cacheQuery(string $queryString, array $resArray): void
    {
        $file = create_cache_directory('modules/AOD_Index/QueryCache/' . md5($queryString));
        $out = serialize($resArray);
        sugar_file_put_contents_atomic($file, $out);
    }
}
