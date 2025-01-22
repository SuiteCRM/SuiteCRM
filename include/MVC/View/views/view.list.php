<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
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

require_once('include/MVC/View/SugarView.php');

require_once('include/ListView/ListViewSmarty.php');

require_once('modules/MySettings/StoreQuery.php');

class ViewList extends SugarView
{
    /**
     * @var string $type
     */
    public $type = 'list';

    /**
     * @var ListViewSmartyEmails $lv
     */
    public $lv;

    /**
     * @var SearchForm $searchForm
     */
    public $searchForm;

    /** @var  array $savedSearchData */
    public $savedSearchData;

    /**
     * @var
     */
    public $use_old_search;

    /**
     * @var bool $headers
     */
    public $headers;

    /**
     * @var SugarBean
     */
    public $seed;

    /**
     * @var array $params
     */
    public $params;

    /**
     * @var array $listViewDefs
     */
    public $listViewDefs;

    /**
     * @var StoreQuery $storeQuery
     */
    public $storeQuery;

    /**
     * @var string $where
     */
    public $where = '';

    /**
     * ViewList constructor.
     */
    public function __construct()
    {
        parent::__construct();
    }



    /**
     * Prepare List View
     */
    public function listViewPrepare()
    {
        $module = isset($GLOBALS['module']) ? $GLOBALS['module'] : null;

        if (!isset($module)) {
            LoggerManager::getLogger()->fatal('Undefined module for list view prepare');
            return false;
        }

        $metadataFile = $this->getMetaDataFile();

        if (!file_exists($metadataFile)) {
            sugar_die(sprintf($GLOBALS['app_strings']['LBL_NO_ACTION'], $this->do_action));
        }

        require($metadataFile);

        $this->listViewDefs = $listViewDefs;

        if (isset($viewdefs[$this->module]['ListView']['templateMeta'])) {
            $this->lv->templateMeta = $viewdefs[$this->module]['ListView']['templateMeta'];
        }

        if (!empty($this->bean->object_name) && isset($_REQUEST[$module . '2_' . strtoupper($this->bean->object_name) . '_offset'])) {//if you click the pagination button, it will populate the search criteria here
            if (!empty($_REQUEST['current_query_by_page'])) {//The code support multi browser tabs pagination
                $blockVariables = array('mass', 'uid', 'massupdate', 'delete', 'merge', 'selectCount', 'request_data', 'current_query_by_page', $module . '2_' . strtoupper($this->bean->object_name) . '_ORDER_BY');
                if (isset($_REQUEST['lvso'])) {
                    $blockVariables[] = 'lvso';
                }
                $current_query_by_page = json_decode(html_entity_decode($_REQUEST['current_query_by_page']), true);
                foreach ($current_query_by_page as $search_key => $search_value) {
                    if ($search_key != $module . '2_' . strtoupper($this->bean->object_name) . '_offset' && !in_array($search_key, $blockVariables)) {
                        if (!is_array($search_value)) {
                            $_REQUEST[$search_key] = securexss($search_value);
                        } else {
                            foreach ($search_value as $key => &$val) {
                                $val = securexss($val);
                            }
                            $_REQUEST[$search_key] = $search_value;
                        }
                    }
                }
            }
        }
        if (!empty($_REQUEST['saved_search_select'])) {
            if ($_REQUEST['saved_search_select'] == '_none' || !empty($_REQUEST['button'])) {
                $_SESSION['LastSavedView'][$_REQUEST['module']] = '';
                unset($_REQUEST['saved_search_select']);
                unset($_REQUEST['saved_search_select_name']);

                //use the current search module, or the current module to clear out layout changes
                if (!empty($_REQUEST['search_module']) || !empty($_REQUEST['module'])) {
                    $mod = !empty($_REQUEST['search_module']) ? $_REQUEST['search_module'] : $_REQUEST['module'];
                    global $current_user;
                    //Reset the current display columns to default.
                    $current_user->setPreference('ListViewDisplayColumns', array(), 0, $mod);
                }
            } else {
                if (empty($_REQUEST['button']) && (empty($_REQUEST['clear_query']) || $_REQUEST['clear_query'] != 'true')) {
                    $this->saved_search = loadBean('SavedSearch');
                    $this->saved_search->retrieveSavedSearch($_REQUEST['saved_search_select']);
                    $this->saved_search->populateRequest();
                } elseif (!empty($_REQUEST['button'])) { // click the search button, after retrieving from saved_search
                    $_SESSION['LastSavedView'][$_REQUEST['module']] = '';
                    unset($_REQUEST['saved_search_select']);
                    unset($_REQUEST['saved_search_select_name']);
                }
            }
        }
        $this->storeQuery = new StoreQuery();
        if (!isset($_REQUEST['query'])) {
            $this->storeQuery->loadQuery($this->module);
            $this->storeQuery->populateRequest();
        } elseif (!empty($_REQUEST['update_stored_query'])) {
            $updateKey = null;
            if (isset($_REQUEST['update_stored_query_key'])) {
                $updateKey = $_REQUEST['update_stored_query_key'];
            } else {
                LoggerManager::getLogger()->warn('update_stored_query_key is not defined for list view at listViewPrepare');
            }

            $updateValue = null;
            if (isset($_REQUEST[$updateKey])) {
                $updateValue = $_REQUEST[$updateKey];
            } else {
                LoggerManager::getLogger()->warn('requested update key is not defined for list view at listViewPrepare: ' . $updateKey);
            }


            $this->storeQuery->loadQuery($this->module);
            $this->storeQuery->populateRequest();
            $_REQUEST[$updateKey] = $updateValue;
            unset($_REQUEST['update_stored_query']);
            $this->storeQuery->saveFromRequest($this->module);
        } else {
            $this->storeQuery->saveFromRequest($this->module);
        }

        $this->seed = $this->bean;

        $displayColumns = array();
        if (!empty($_REQUEST['displayColumns'])) {
            foreach (explode('|', $_REQUEST['displayColumns']) as $num => $col) {
                if (!empty($this->listViewDefs[$module][$col])) {
                    $displayColumns[$col] = $this->listViewDefs[$module][$col];
                }
            }
        } else {
            if (!isset($this->listViewDefs[$module])) {
                LoggerManager::getLogger()->warn('Listview definition is not set for module: ' . $module);
            } else {
                foreach ($this->listViewDefs[$module] as $col => $this->params) {
                    if (!empty($this->params['default']) && $this->params['default']) {
                        $displayColumns[$col] = $this->params;
                    }
                }
            }
        }
        $this->params = array('massupdate' => true);
        if (!empty($_REQUEST['orderBy'])) {
            $this->params['orderBy'] = $_REQUEST['orderBy'];
            $this->params['overrideOrder'] = true;
            if (!empty($_REQUEST['sortOrder'])) {
                $this->params['sortOrder'] = $_REQUEST['sortOrder'];
            }
        }
        if (!isset($this->lv) || !$this->lv) {
            $this->lv = new stdClass();
        }

        if (!isset($this->lv)) {
            $this->lv = new stdClass();
            LoggerManager::getLogger()->warn('List view is not defined');
        }

        $this->lv->displayColumns = $displayColumns;

        $this->module = $module;

        $this->prepareSearchForm();

        if (isset($this->options['show_title']) && $this->options['show_title']) {
            $modStrings = null;
            if (isset($GLOBALS['mod_strings'])) {
                $modStrings = $GLOBALS['mod_strings'];
            } else {
                LoggerManager::getLogger()->warn('Undefined index: mod_strings');
            }

            $moduleName = isset($this->seed->module_dir) ? $this->seed->module_dir : $modStrings['LBL_MODULE_NAME'];
            echo $this->getModuleTitle(true);
        }
    }

    /**
     * Process List View
     */
    public function listViewProcess()
    {
        $this->processSearchForm();
        $this->lv->searchColumns = $this->searchForm->searchColumns;

        if (!$this->headers) {
            return;
        }
        if (empty($_REQUEST['search_form_only']) || $_REQUEST['search_form_only'] == false) {
            $this->lv->ss->assign("SEARCH", true);
            $this->lv->ss->assign('savedSearchData', $this->searchForm->getSavedSearchData());
            $this->lv->setup($this->seed, 'include/ListView/ListViewGeneric.tpl', $this->where, $this->params);
            $savedSearchName = empty($_REQUEST['saved_search_select_name']) ? '' : (' - ' . $_REQUEST['saved_search_select_name']);
            echo $this->lv->display();
        }
    }

    /**
     * Setup Search Form
     */
    public function prepareSearchForm()
    {
        $this->searchForm = null;

        //search
        $view = 'basic_search';
        if (!empty($_REQUEST['search_form_view']) && $_REQUEST['search_form_view'] == 'advanced_search') {
            $view = $_REQUEST['search_form_view'];
        }
        $this->headers = true;

        if (!empty($_REQUEST['search_form_only']) && $_REQUEST['search_form_only']) {
            $this->headers = false;
        } elseif (!isset($_REQUEST['search_form']) || $_REQUEST['search_form'] != 'false') {
            if (isset($_REQUEST['searchFormTab']) && $_REQUEST['searchFormTab'] == 'advanced_search') {
                $view = 'advanced_search';
            } else {
                $view = 'basic_search';
            }
        }

        $this->use_old_search = true;
        if ((file_exists('modules/' . $this->module . '/SearchForm.html')
                && !file_exists('modules/' . $this->module . '/metadata/searchdefs.php'))
            || (file_exists('custom/modules/' . $this->module . '/SearchForm.html')
                && !file_exists('custom/modules/' . $this->module . '/metadata/searchdefs.php'))
        ) {
            require_once('include/SearchForm/SearchForm.php');
            $this->searchForm = new SearchForm($this->module, $this->seed);
        } else {
            $this->use_old_search = false;
            require_once('include/SearchForm/SearchForm2.php');

            $searchMetaData = SearchForm::retrieveSearchDefs($this->module);

            $this->searchForm = $this->getSearchForm2($this->seed, $this->module, $this->action);
            $this->searchForm->setup($searchMetaData['searchdefs'], $searchMetaData['searchFields'], 'SearchFormGeneric.tpl', $view, $this->listViewDefs);
            $this->searchForm->lv = $this->lv;
        }
    }

    /**
     * Process Search Form
     */
    public function processSearchForm()
    {
        if (isset($_REQUEST['query'])) {
            // we have a query
            if (!empty($_SERVER['HTTP_REFERER']) && preg_match('/action=EditView/', $_SERVER['HTTP_REFERER'])) { // from EditView cancel
                $this->searchForm->populateFromArray($this->storeQuery->query);
            } else {
                $this->searchForm->populateFromRequest();
            }

            $where_clauses = $this->searchForm->generateSearchWhere(true, $this->seed->module_dir);

            if (count($where_clauses) > 0) {
                $this->where = '(' . implode(' ) AND ( ', $where_clauses) . ')';
            }
            $GLOBALS['log']->info("List View Where Clause: $this->where");
        }
        if ($this->use_old_search) {
            switch (isset($view) ? $view : null) {
                case 'basic_search':
                    $this->searchForm->setup();
                    $this->searchForm->displayBasic($this->headers);
                    break;
                case 'advanced_search':
                    $this->searchForm->setup();
                    $this->searchForm->displayAdvanced($this->headers);
                    break;
                case 'saved_views':
                    echo $this->searchForm->displaySavedViews($this->listViewDefs, $this->lv, $this->headers);
                    break;
            }
        } else {
            $output = $this->searchForm->display($this->headers);
            $this->savedSearchData = $this->searchForm->getSavedSearchData();
            echo $output;
        }
    }

    /**
     * Setup View
     */
    public function preDisplay()
    {
        $this->lv = new ListViewSmarty();
    }

    /**
     * Display View
     */
    public function display()
    {
        if (!$this->bean || !$this->bean->ACLAccess('list')) {
            ACLController::displayNoAccess();
        } else {
            $this->listViewPrepare();
            $this->listViewProcess();
        }
    }

    /**
     *
     * @return SearchForm
     */
    protected function getSearchForm2($seed, $module, $action = "index")
    {
        // SearchForm2.php is required_onced above before calling this function
        // hence the order of parameters is different from SearchForm.php
        return new SearchForm($seed, $module, $action);
    }
}
