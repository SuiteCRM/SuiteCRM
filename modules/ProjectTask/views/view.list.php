<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

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

/*********************************************************************************

 * Description: This file is used to override the default Meta-data EditView behavior
 * to provide customization specific to the Calls module.
 * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
 * All Rights Reserved.
 * Contributor(s): ______________________________________..
 ********************************************************************************/


#[\AllowDynamicProperties]
class ProjectTaskViewList extends ViewList
{
    public function __construct()
    {
        parent::__construct();
    }




    public function display()
    {
        $metafiles = [];
        $searchdefs = [];
        $searchFields = [];
        $listViewDefs = [];

        if (!$this->bean->ACLAccess('list')) {
            ACLController::displayNoAccess();
            return;
        }
        $module = $GLOBALS['module'];
        $metadataFile = null;
        $foundViewDefs = false;
        if (file_exists('custom/modules/' . $module. '/metadata/listviewdefs.php')) {
            $metadataFile = 'custom/modules/' . $module . '/metadata/listviewdefs.php';
            $foundViewDefs = true;
        } else {
            if (file_exists('custom/modules/'.$module.'/metadata/metafiles.php')) {
                require_once('custom/modules/'.$module.'/metadata/metafiles.php');
                if (!empty($metafiles[$module]['listviewdefs'])) {
                    $metadataFile = $metafiles[$module]['listviewdefs'];
                    $foundViewDefs = true;
                }
            } elseif (file_exists('modules/'.$module.'/metadata/metafiles.php')) {
                require_once('modules/'.$module.'/metadata/metafiles.php');
                if (!empty($metafiles[$module]['listviewdefs'])) {
                    $metadataFile = $metafiles[$module]['listviewdefs'];
                    $foundViewDefs = true;
                }
            }
        }
        if (!$foundViewDefs && file_exists('modules/'.$module.'/metadata/listviewdefs.php')) {
            $metadataFile = 'modules/'.$module.'/metadata/listviewdefs.php';
        }
        require_once($metadataFile);

        $seed = $this->bean;
        if (!empty($this->bean->object_name) && isset($_REQUEST[$module.'2_'.strtoupper($this->bean->object_name).'_offset'])) {//if you click the pagination button, it will populate the search criteria here
            if (!empty($_REQUEST['current_query_by_page'])) {//The code support multi browser tabs pagination
                $blockVariables = array('mass', 'uid', 'massupdate', 'delete', 'merge', 'selectCount', 'request_data', 'current_query_by_page', $module.'2_'.strtoupper($this->bean->object_name).'_ORDER_BY');
                if (isset($_REQUEST['lvso'])) {
                    $blockVariables[] = 'lvso';
                }

                $current_query_by_page = json_decode(html_entity_decode((string) $_REQUEST['current_query_by_page']), true, 512, JSON_THROW_ON_ERROR);
                foreach ($current_query_by_page as $search_key=>$search_value) {
                    if ($search_key != $module.'2_'.strtoupper($this->bean->object_name).'_offset' && !in_array($search_key, $blockVariables)) {
                        if (!is_array($search_value)) {
                            $_REQUEST[$search_key] = DBManagerFactory::getInstance()->quote($search_value);
                        } else {
                            foreach ($search_value as $key=>&$val) {
                                $val = DBManagerFactory::getInstance()->quote($val);
                            }
                            $_REQUEST[$search_key] = $search_value;
                        }
                    }
                }
            }
        }

        if (!empty($_REQUEST['saved_search_select']) && $_REQUEST['saved_search_select']!='_none') {
            if (empty($_REQUEST['button']) && (empty($_REQUEST['clear_query']) || $_REQUEST['clear_query']!='true')) {
                $this->saved_search = loadBean('SavedSearch');
                $this->saved_search->retrieveSavedSearch($_REQUEST['saved_search_select']);
                $this->saved_search->populateRequest();
            } elseif (!empty($_REQUEST['button'])) { // click the search button, after retrieving from saved_search
                $_SESSION['LastSavedView'][$_REQUEST['module']] = '';
                unset($_REQUEST['saved_search_select']);
                unset($_REQUEST['saved_search_select_name']);
            }
        }

        $lv = new ListViewSmarty();
        $displayColumns = array();
        if (!empty($_REQUEST['displayColumns'])) {
            foreach (explode('|', $_REQUEST['displayColumns']) as $num => $col) {
                if (!empty($listViewDefs[$module][$col])) {
                    $displayColumns[$col] = $listViewDefs[$module][$col];
                }
            }
        } else {
            foreach ($listViewDefs[$module] as $col => $params) {
                if (!empty($params['default']) && $params['default']) {
                    $displayColumns[$col] = $params;
                }
            }
        }

        $params = array( 'massupdate' => true, 'export' => true);

        if (!empty($_REQUEST['orderBy'])) {
            $params['orderBy'] = $_REQUEST['orderBy'];
            $params['overrideOrder'] = true;
            if (!empty($_REQUEST['sortOrder'])) {
                $params['sortOrder'] = $_REQUEST['sortOrder'];
            }
        }
        $lv->displayColumns = $displayColumns;

        $this->seed = $seed;
        $this->module = $module;

        $searchForm = null;
        $storeQuery = new StoreQuery();
        if (!isset($_REQUEST['query'])) {
            $storeQuery->loadQuery($this->module);
            $storeQuery->populateRequest();
        } else {
            $storeQuery->saveFromRequest($this->module);
        }

        //search
        $view = 'basic_search';
        if (!empty($_REQUEST['search_form_view'])) {
            $view = $_REQUEST['search_form_view'];
        }
        $headers = true;
        if (!empty($_REQUEST['search_form_only']) && $_REQUEST['search_form_only']) {
            $headers = false;
        } elseif (!isset($_REQUEST['search_form']) || $_REQUEST['search_form'] != 'false') {
            if (isset($_REQUEST['searchFormTab']) && $_REQUEST['searchFormTab'] == 'advanced_search') {
                $view = 'advanced_search';
            } else {
                $view = 'basic_search';
            }
        }

        $use_old_search = true;
        if (file_exists('modules/'.$this->module.'/SearchForm.html')) {
            require_once('include/SearchForm/SearchForm.php');
            $searchForm = new SearchForm($this->module, $this->seed);
        } else {
            $use_old_search = false;
            require_once('include/SearchForm/SearchForm2.php');


            if (file_exists('custom/modules/'.$this->module.'/metadata/searchdefs.php')) {
                require_once('custom/modules/'.$this->module.'/metadata/searchdefs.php');
            } elseif (!empty($metafiles[$this->module]['searchdefs'])) {
                require_once($metafiles[$this->module]['searchdefs']);
            } elseif (file_exists('modules/'.$this->module.'/metadata/searchdefs.php')) {
                require_once('modules/'.$this->module.'/metadata/searchdefs.php');
            }


            if (!empty($metafiles[$this->module]['searchfields'])) {
                require($metafiles[$this->module]['searchfields']);
            } elseif (file_exists('modules/'.$this->module.'/metadata/SearchFields.php')) {
                require('modules/'.$this->module.'/metadata/SearchFields.php');
            }


            $searchForm = new SearchForm($this->seed, $this->module, $this->action);
            $searchForm->setup($searchdefs, $searchFields, 'SearchFormGeneric.tpl', $view, $listViewDefs);
            $searchForm->lv = $lv;
        }

        if (isset($this->options['show_title']) && $this->options['show_title']) {
            $moduleName = isset($this->seed->module_dir) ? $this->seed->module_dir : $GLOBALS['mod_strings']['LBL_MODULE_NAME'];
            echo getClassicModuleTitle($moduleName, array($GLOBALS['mod_strings']['LBL_MODULE_TITLE']), false);
        }

        $where = '';
        if (isset($_REQUEST['query'])) {
            // we have a query
            if (!empty($_SERVER['HTTP_REFERER']) && preg_match('/action=EditView/', (string) $_SERVER['HTTP_REFERER'])) { // from EditView cancel
                   $searchForm->populateFromArray($storeQuery->query);
            } else {
                $searchForm->populateFromRequest();
            }
            $where_clauses = $searchForm->generateSearchWhere(true, $this->seed->module_dir);
            if ((is_countable($where_clauses) ? count($where_clauses) : 0) > 0) {
                $where = '('. implode(' ) AND ( ', $where_clauses) . ')';
            }
            $GLOBALS['log']->info("List View Where Clause: $where");
        }
        if ($use_old_search) {
            switch ($view) {
                case 'basic_search':
                    $searchForm->setup();
                    $searchForm->displayBasic($headers);
                    break;
                 case 'advanced_search':
                     $searchForm->setup();
                    $searchForm->displayAdvanced($headers);
                    break;
                 case 'saved_views':
                     echo $searchForm->displaySavedViews($listViewDefs, $lv, $headers);
                   break;
            }
        } else {
            echo $searchForm->display($headers);
        }
        if (!$headers) {
            return;
        }
        /*
        * Bug 50575 - related search columns not inluded in query in a proper way
        */
        $lv->searchColumns = $searchForm->searchColumns;

        if (empty($_REQUEST['search_form_only']) || $_REQUEST['search_form_only'] == false) {
            $lv->ss->assign('savedSearchData', $searchForm->getSavedSearchData());
            //Bug 58841 - mass update form was not displayed for non-admin users that should have access
            if (ACLController::checkAccess($module, 'massupdate') || ACLController::checkAccess($module, 'export')) {
                $lv->setup($seed, 'include/ListView/ListViewGeneric.tpl', $where, $params);
            } else {
                $lv->setup($seed, 'include/ListView/ListViewNoMassUpdate.tpl', $where, $params);
            }

            echo $lv->display();
        }
    }
}
