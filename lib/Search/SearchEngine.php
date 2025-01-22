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

use SuiteCRM\Search\Exceptions\SearchInvalidRequestException;
use SuiteCRM\Search\UI\SearchFormController;
use SuiteCRM\Search\UI\SearchResultsController;

/**
 * This abstract class offers the interface and utilities for other classes to be used as search engines.
 */
#[\AllowDynamicProperties]
abstract class SearchEngine
{
    /**
     * Performs a search using the search engine and returns a list SearchResults instance.
     *
     * @param SearchQuery $query
     *
     * @return SearchResults
     */
    abstract public function search(SearchQuery $query): SearchResults;

    /**
     * Performs a search using the given query and shows a search view.
     *
     * The search view contains both a search bar and search results (if any).
     *
     * @param SearchQuery $query
     */
    public function searchAndDisplay(SearchQuery $query): void
    {
        $this->validateQuery($query);
        $this->displayForm($query);

        if (!$query->isEmpty()) {
            $results = $this->search($query);
            $this->displayResults($query, $results);
        }
    }

    /**
     * Shows the default search form (search bar and options) for a given search query.
     *
     * @param SearchQuery $query
     */
    public function displayForm(SearchQuery $query): void
    {
        $controller = new SearchFormController($query);
        $controller->display();
    }

    /**
     * Shows the default search results for the given search query and results.
     *
     * @param SearchQuery $query
     * @param SearchResults $results
     */
    public function displayResults(SearchQuery $query, SearchResults $results): void
    {
        $controller = new SearchResultsController($query, $results);
        $controller->display();
    }

    /**
     * This method should be extended to sanitize and standardise the request to fill all the values as they are
     * expected to be by the `search()` method.
     *
     * By default the query gets white spaces trimmed.
     *
     * If it is impossible to validate or sanitize the query a `SearchInvalidRequestException` should be thrown.
     *
     * @param SearchQuery $query the query to validate
     *
     * @throws SearchInvalidRequestException if the query is not valid
     */
    protected function validateQuery(SearchQuery $query): void
    {
        $query->trim();
    }
}
