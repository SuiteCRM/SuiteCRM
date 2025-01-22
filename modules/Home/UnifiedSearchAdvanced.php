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

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

/**
 * @deprecated since v7.12.0
 * Class UnifiedSearchAdvanced
 */
#[\AllowDynamicProperties]
class UnifiedSearchAdvanced
{
    public $query_string = '';

    /* path to search form */
    public $searchFormPath = 'include/SearchForm/SearchForm2.php';

    /*search form class name*/
    public $searchFormClass = 'SearchForm';

    /**
     * @deprecated since v7.12.0
     * UnifiedSearchAdvanced constructor.
     */
    public function __construct()
    {
        LoggerManager::getLogger()->deprecated(
            'UnifiedSearchAdvanced is deprecated since v7.12.0, please use Search.php instead'
        );

        if (!empty($_REQUEST['query_string'])) {
            $query_string = trim($_REQUEST['query_string']);
            if (!empty($query_string)) {
                $this->query_string = $query_string;
            }
        }
        $this->cache_search = sugar_cached('modules/unified_search_modules.php');
        $this->cache_display = sugar_cached('modules/unified_search_modules_display.php');
    }

    /**
     * @deprecated since v7.12.0
     * @param string $tpl
     * @return string
     */
    public function getDropDownDiv($tpl = 'modules/Home/UnifiedSearchAdvanced.tpl')
    {
        global $app_list_strings, $app_strings;

        if (!file_exists($this->cache_search)) {
            $this->buildCache();
        }

        $unified_search_modules_display = $this->getUnifiedSearchModulesDisplay();

        global $mod_strings, $modListHeader, $app_list_strings, $current_user, $app_strings, $beanList;
        $users_modules = $current_user->getPreference('globalSearch', 'search');

        // preferences are empty, select all
        if (empty($users_modules)) {
            $users_modules = array();
            foreach ($unified_search_modules_display as $module=>$data) {
                if (!empty($data['visible'])) {
                    $users_modules[$module] = $beanList[$module];
                }
            }
            $current_user->setPreference('globalSearch', $users_modules, 0, 'search');
        }

        $sugar_smarty = new Sugar_Smarty();

        $modules_to_search = array();

        foreach ($users_modules as $key=>$module) {
            if (ACLController::checkAccess($key, 'list', true)) {
                $modules_to_search[$key]['checked'] = true;
            }
        }

        if (!empty($this->query_string)) {
            $sugar_smarty->assign('query_string', securexss($this->query_string));
        } else {
            $sugar_smarty->assign('query_string', '');
        }

        $sugar_smarty->assign('MOD', return_module_language($GLOBALS['current_language'], 'Administration'));
        $sugar_smarty->assign('APP', $app_strings);
        $sugar_smarty->assign('USE_SEARCH_GIF', 0);
        $sugar_smarty->assign('LBL_SEARCH_BUTTON_LABEL', $app_strings['LBL_SEARCH_BUTTON_LABEL']);
        $sugar_smarty->assign('LBL_SEARCH_BUTTON_TITLE', $app_strings['LBL_SEARCH_BUTTON_TITLE']);
        $sugar_smarty->assign('LBL_SEARCH', $app_strings['LBL_SEARCH']);

        $json_enabled = array();
        $json_disabled = array();

        //Now add the rest of the modules that are searchable via Global Search settings
        foreach ($unified_search_modules_display as $module=>$data) {
            if (!isset($modules_to_search[$module]) && $data['visible'] && ACLController::checkAccess($module, 'list', true)) {
                $modules_to_search[$module]['checked'] = false;
            } else {
                if (isset($modules_to_search[$module]) && !$data['visible']) {
                    unset($modules_to_search[$module]);
                }
            }
        }

        //Create the two lists (doing it this way preserves the user's ordering choice for enabled modules)
        foreach ($modules_to_search as $module=>$data) {
            $label = isset($app_list_strings['moduleList'][$module]) ? $app_list_strings['moduleList'][$module] : $module;
            if (!empty($data['checked'])) {
                $json_enabled[] = array("module" => $module, 'label' => $label);
            } else {
                $json_disabled[] = array("module" => $module, 'label' => $label);
            }
        }

        $sugar_smarty->assign('enabled_modules', json_encode($json_enabled));
        $sugar_smarty->assign('disabled_modules', json_encode($json_disabled));

        $showDiv = $current_user->getPreference('showGSDiv', 'search');
        if (!isset($showDiv)) {
            $showDiv = 'no';
        }

        $sugar_smarty->assign('SHOWGSDIV', $showDiv);
        $sugar_smarty->debugging = false;
        return $sugar_smarty->fetch($tpl);
    }


    /**
     * @deprecated since v7.12.0
     * search
     *
     * Search function run when user goes to Show All and runs a search again.  This outputs the search results
     * calling upon the various listview display functions for each module searched on.
     *
     * Todo: Sync this up with SugarSpot.php search method.
     *
     *
     */
    public function search()
    {
        $unified_search_modules = $this->getUnifiedSearchModules();
        $unified_search_modules_display = $this->getUnifiedSearchModulesDisplay();


        require_once 'include/ListView/ListViewSmarty.php';

        global $modListHeader, $beanList, $beanFiles, $current_language, $app_strings, $current_user, $mod_strings;
        $home_mod_strings = return_module_language($current_language, 'Home');

        $this->query_string = securexss(from_html(clean_string($this->query_string, 'UNIFIED_SEARCH')));

        if (!empty($_REQUEST['advanced']) && $_REQUEST['advanced'] != 'false') {
            $modules_to_search = array();
            if (!empty($_REQUEST['search_modules'])) {
                foreach (explode(',', $_REQUEST['search_modules']) as $key) {
                    if (isset($unified_search_modules_display[$key]) && !empty($unified_search_modules_display[$key]['visible'])) {
                        $modules_to_search[$key] = $beanList[$key];
                    }
                }
            }

            $current_user->setPreference('showGSDiv', isset($_REQUEST['showGSDiv']) ? $_REQUEST['showGSDiv'] : 'no', 0, 'search');
            $current_user->setPreference('globalSearch', $modules_to_search, 0, 'search'); // save selections to user preference
        } else {
            $users_modules = $current_user->getPreference('globalSearch', 'search');
            $modules_to_search = array();

            if (!empty($users_modules)) {
                // use user's previous selections
                foreach ($users_modules as $key => $value) {
                    if (isset($unified_search_modules_display[$key]) && !empty($unified_search_modules_display[$key]['visible'])) {
                        $modules_to_search[$key] = $beanList[$key];
                    }
                }
            } else {
                foreach ($unified_search_modules_display as $module=>$data) {
                    if (!empty($data['visible'])) {
                        $modules_to_search[$module] = $beanList[$module];
                    }
                }
            }
            $current_user->setPreference('globalSearch', $modules_to_search, 'search');
        }


        $templateFile = 'modules/Home/UnifiedSearchAdvancedForm.tpl';
        if (file_exists('custom/' . $templateFile)) {
            $templateFile = 'custom/'.$templateFile;
        }

        echo $this->getDropDownDiv($templateFile);

        $module_results = array();
        $module_counts = array();
        $has_results = false;

        if (!empty($this->query_string)) {
            foreach ($modules_to_search as $moduleName => $beanName) {
                require_once $beanFiles[$beanName] ;
                $seed = new $beanName();

                $lv = new ListViewSmarty();
                $lv->lvd->additionalDetails = false;
                $mod_strings = return_module_language($current_language, $seed->module_dir);

                //retrieve the original list view defs and store for processing in case of custom layout changes
                require('modules/'.$seed->module_dir.'/metadata/listviewdefs.php');
                $orig_listViewDefs = $listViewDefs;

                if (file_exists('custom/modules/'.$seed->module_dir.'/metadata/listviewdefs.php')) {
                    require('custom/modules/'.$seed->module_dir.'/metadata/listviewdefs.php');
                }

                if (!isset($listViewDefs) || !isset($listViewDefs[$seed->module_dir])) {
                    continue;
                }

                $unifiedSearchFields = array() ;
                $innerJoins = array();
                foreach ($unified_search_modules[ $moduleName ]['fields'] as $field=>$def) {
                    $listViewCheckField = strtoupper($field);
                    //check to see if the field is in listview defs
                    if (empty($listViewDefs[$seed->module_dir][$listViewCheckField]['default'])) {
                        //check to see if field is in original list view defs (in case we are using custom layout defs)
                        if (!empty($orig_listViewDefs[$seed->module_dir][$listViewCheckField]['default'])) {
                            //if we are here then the layout has been customized, but the field is still needed for query creation
                            $listViewDefs[$seed->module_dir][$listViewCheckField] = $orig_listViewDefs[$seed->module_dir][$listViewCheckField];
                        }
                    }

                    //bug: 34125 we might want to try to use the LEFT JOIN operator instead of the INNER JOIN in the case we are
                    //joining against a field that has not been populated.
                    if (!empty($def['innerjoin'])) {
                        if (empty($def['db_field'])) {
                            continue;
                        }
                        $innerJoins[$field] = $def;
                        $def['innerjoin'] = str_replace('INNER', 'LEFT', (string) $def['innerjoin']);
                    }

                    if (isset($seed->field_defs[$field]['type'])) {
                        $type = $seed->field_defs[$field]['type'];
                        if ($type == 'int' && !is_numeric($this->query_string)) {
                            continue;
                        }
                    }

                    $unifiedSearchFields[ $moduleName ] [ $field ] = $def ;
                    $unifiedSearchFields[ $moduleName ] [ $field ][ 'value' ] = $this->query_string;
                }

                /*
                 * Use searchForm2->generateSearchWhere() to create the search query, as it can generate SQL for the full set of comparisons required
                 * generateSearchWhere() expects to find the search conditions for a field in the 'value' parameter of the searchFields entry for that field
                 */
                require_once $beanFiles[$beanName] ;
                $seed = new $beanName();

                require_once $this->searchFormPath;
                $searchForm = new $this->searchFormClass($seed, $moduleName) ;

                $searchForm->setup(array( $moduleName => array() ), $unifiedSearchFields, '', 'saved_views' /* hack to avoid setup doing further unwanted processing */) ;
                $where_clauses = $searchForm->generateSearchWhere() ;
                //add inner joins back into the where clause
                $params = array('custom_select' => "");
                foreach ($innerJoins as $field=>$def) {
                    if (isset($def['db_field'])) {
                        foreach ($def['db_field'] as $dbfield) {
                            $where_clauses[] = $dbfield . " LIKE '" . DBManagerFactory::getInstance()->quote($this->query_string) . "%'";
                        }
                        $params['custom_select'] .= ", $dbfield";
                        $params['distinct'] = true;
                        //$filterFields[$dbfield] = $dbfield;
                    }
                }

                if ((is_countable($where_clauses) ? count($where_clauses) : 0) > 0) {
                    $where = '(('. implode(' ) OR ( ', $where_clauses) . '))';
                } else {
                    /* Clear $where from prev. module
                       if in current module $where_clauses */
                    $where = '';
                }
                $displayColumns = array();
                foreach ($listViewDefs[$seed->module_dir] as $colName => $param) {
                    if (!empty($param['default']) && $param['default'] == true) {
                        $param['url_sort'] = true;//bug 27933
                        $displayColumns[$colName] = $param;
                    }
                }

                if ((is_countable($displayColumns) ? count($displayColumns) : 0) > 0) {
                    $lv->displayColumns = $displayColumns;
                } else {
                    $lv->displayColumns = $listViewDefs[$seed->module_dir];
                }

                $lv->export = false;
                $lv->mergeduplicates = false;
                $lv->multiSelect = false;
                $lv->delete = false;
                $lv->select = false;
                $lv->showMassupdateFields = false;
                $lv->email = false;

                $lv->setup($seed, 'include/ListView/ListViewNoMassUpdate.tpl', $where, $params, 0, 10);
                $lv->ss->assign('showFilterIcon', 0);
                $lv->ss->assign('hideColumnFilter', 1);

                $module_results[$moduleName] = '<br /><br />' . get_form_header($GLOBALS['app_list_strings']['moduleList'][$seed->module_dir] . ' (' . $lv->data['pageData']['offsets']['total'] . ')', '', false);
                $module_counts[$moduleName] = $lv->data['pageData']['offsets']['total'];

                if ($lv->data['pageData']['offsets']['total'] == 0) {
                    //$module_results[$moduleName] .= "<li class='noBullet' id='whole_subpanel_{$moduleName}'><div id='div_{$moduleName}'><h2>" . $home_mod_strings['LBL_NO_RESULTS_IN_MODULE'] . '</h2></div></li>';
                    $module_results[$moduleName] .= '<h2>' . $home_mod_strings['LBL_NO_RESULTS_IN_MODULE'] . '</h2>';
                } else {
                    $has_results = true;
                    //$module_results[$moduleName] .= "<li class='noBullet' id='whole_subpanel_{$moduleName}'><div id='div_{$moduleName}'>" . $lv->display(false, false) . '</div></li>';
                    $module_results[$moduleName] .= $lv->display(false, false);
                }
            }
        }

        if ($has_results) {
            foreach ($module_counts as $name=>$value) {
                echo $module_results[$name];
            }
        } else {
            if (empty($_REQUEST['form_only'])) {
                echo $home_mod_strings['LBL_NO_RESULTS'];
                echo $home_mod_strings['LBL_NO_RESULTS_TIPS'];
            }
        }
    }

    /**
     * @deprecated since v7.12.0
     */
    public function buildCache()
    {
        global $beanList, $beanFiles, $dictionary;

        $supported_modules = array();

        foreach ($beanList as $moduleName=>$beanName) {
            if (!isset($beanFiles[$beanName])) {
                continue;
            }

            $beanName = BeanFactory::getObjectName($moduleName);
            $manager = new VardefManager();
            $manager->loadVardef($moduleName, $beanName) ;

            // obtain the field definitions used by generateSearchWhere (duplicate code in view.list.php)
            if (file_exists('custom/modules/'.$moduleName.'/metadata/metafiles.php')) {
                require('custom/modules/'.$moduleName.'/metadata/metafiles.php');
            } elseif (file_exists('modules/'.$moduleName.'/metadata/metafiles.php')) {
                require('modules/'.$moduleName.'/metadata/metafiles.php');
            }


            if (!empty($metafiles[$moduleName]['searchfields'])) {
                require $metafiles[$moduleName]['searchfields'] ;
            } else {
                if (file_exists("modules/{$moduleName}/metadata/SearchFields.php")) {
                    require "modules/{$moduleName}/metadata/SearchFields.php" ;
                }
            }

            //Load custom SearchFields.php if it exists
            if (file_exists("custom/modules/{$moduleName}/metadata/SearchFields.php")) {
                require "custom/modules/{$moduleName}/metadata/SearchFields.php" ;
            }

            //If there are $searchFields are empty, just continue, there are no search fields defined for the module
            if (empty($searchFields[$moduleName])) {
                continue;
            }

            $isCustomModule = preg_match('/^([a-z0-9]{1,5})_([a-z0-9_]+)$/i', (string) $moduleName);

            //If the bean supports unified search or if it's a custom module bean and unified search is not defined
            if (!empty($dictionary[$beanName]['unified_search']) || $isCustomModule) {
                $fields = array();
                foreach ($dictionary [ $beanName ][ 'fields' ] as $field => $def) {
                    // We cannot enable or disable unified_search for email in the vardefs as we don't actually have a vardef entry for 'email'
                    // the searchFields entry for 'email' doesn't correspond to any vardef entry. Instead it contains SQL to directly perform the search.
                    // So as a proxy we allow any field in the vardefs that has a name starting with 'email...' to be tagged with the 'unified_search' parameter

                    if (strpos((string) $field, 'email') !== false) {
                        $field = 'email' ;
                    }

                    //bug: 38139 - allow phone to be searched through Global Search
                    if (strpos((string) $field, 'phone') !== false) {
                        $field = 'phone' ;
                    }

                    if (!empty($def['unified_search']) && isset($searchFields [ $moduleName ] [ $field ])) {
                        $fields [ $field ] = $searchFields [ $moduleName ] [ $field ] ;
                    }
                }

                foreach ($searchFields[$moduleName] as $field => $def) {
                    if (
                        isset($def['force_unifiedsearch']) && $def['force_unifiedsearch']
                    ) {
                        $fields[$field] = $def;
                    }
                }

                if (count($fields) > 0) {
                    $supported_modules [$moduleName] ['fields'] = $fields;
                    if (isset($dictionary[$beanName]['unified_search_default_enabled']) && $dictionary[$beanName]['unified_search_default_enabled'] === true) {
                        $supported_modules [$moduleName]['default'] = true;
                    } else {
                        $supported_modules [$moduleName]['default'] = false;
                    }
                }
            }
        }

        ksort($supported_modules);
        write_array_to_file('unified_search_modules', $supported_modules, $this->cache_search);
    }

    /**
     * @deprecated since v7.12.0
     * Retrieve the enabled and disabled modules used for global search.
     *
     * @return array
     */
    public function retrieveEnabledAndDisabledModules()
    {
        global $app_list_strings;

        $unified_search_modules_display = $this->getUnifiedSearchModulesDisplay();
        //Add the translated attribute for display label
        $json_enabled = array();
        $json_disabled = array();
        foreach ($unified_search_modules_display as $module=>$data) {
            $label = isset($app_list_strings['moduleList'][$module]) ? $app_list_strings['moduleList'][$module] : $module;
            if ($data['visible'] === true) {
                $json_enabled[] = array("module" => $module, 'label' => $label);
            } else {
                $json_disabled[] = array("module" => $module, 'label' => $label);
            }
        }

        //If the file doesn't exist
        if (!file_exists($this->cache_search)) {
            $this->buildCache();
        }

        include($this->cache_search);

        //Now add any new modules that may have since been added to unified_search_modules.php
        foreach ($unified_search_modules as $module=>$data) {
            if (!isset($unified_search_modules_display[$module])) {
                $label = isset($app_list_strings['moduleList'][$module]) ? $app_list_strings['moduleList'][$module] : $module;
                if ($data['default']) {
                    $json_enabled[] = array("module" => $module, 'label' => $label);
                } else {
                    $json_disabled[] = array("module" => $module, 'label' => $label);
                }
            }
        }

        return array('enabled' => $json_enabled, 'disabled' => $json_disabled);
    }


    /**
     * @deprecated since v7.12.0
     * saveGlobalSearchSettings
     * This method handles the administrator's request to save the searchable modules selected and stores
     * the results in the unified_search_modules_display.php file
     *
     */
    public function saveGlobalSearchSettings()
    {
        if (isset($_REQUEST['enabled_modules'])) {
            $unified_search_modules_display = $this->getUnifiedSearchModulesDisplay();

            $new_unified_search_modules_display = array();

            foreach (explode(',', $_REQUEST['enabled_modules']) as $module) {
                $new_unified_search_modules_display[$module]['visible'] = true;
            }

            foreach ($unified_search_modules_display as $module=>$data) {
                if (!isset($new_unified_search_modules_display[$module])) {
                    $new_unified_search_modules_display[$module]['visible'] = false;
                }
            }

            $this->writeUnifiedSearchModulesDisplayFile($new_unified_search_modules_display);
        }
    }

    /**
     * @deprecated since v7.12.0
     */
    public static function unlinkUnifiedSearchModulesFile()
    {
        //clear the unified_search_module.php file
        $cache_search = sugar_cached('modules/unified_search_modules.php');
        if (file_exists($cache_search)) {
            $GLOBALS['log']->info("unlink {$cache_search}");
            unlink($cache_search);
        }
    }


    /**
     * @deprecated since v7.12.0
     * getUnifiedSearchModules
     *
     * Returns the value of the $unified_search_modules variable based on the module's vardefs.php file
     * and which fields are marked with the unified_search attribute.
     *
     * @return $unified_search_modules Array of metadata module definitions along with their fields
     */
    public function getUnifiedSearchModules()
    {
        $unified_search_modules = [];
        //Make directory if it doesn't exist
        $cachedir = sugar_cached('modules');
        if (!file_exists($cachedir)) {
            mkdir_recursive($cachedir);
        }

        //Load unified_search_modules.php file
        $cachedFile = sugar_cached('modules/unified_search_modules.php');
        if (!file_exists($cachedFile)) {
            $this->buildCache();
        }

        include $cachedFile;
        return $unified_search_modules;
    }


    /**
     * @deprecated since v7.12.0
     * getUnifiedSearchModulesDisplay
     *
     * Returns the value of the $unified_search_modules_display variable which is based on the $unified_search_modules
     * entries that have been selected to be allowed for searching.
     *
     * @return $unified_search_modules_display Array value of modules that have enabled for searching
     */
    public function getUnifiedSearchModulesDisplay()
    {
        $unified_search_modules_display = [];
        if (!file_exists('custom/modules/unified_search_modules_display.php')) {
            $unified_search_modules = $this->getUnifiedSearchModules();

            $unified_search_modules_display = array();

            if (!empty($unified_search_modules)) {
                foreach ($unified_search_modules as $module=>$data) {
                    $unified_search_modules_display[$module]['visible'] = (isset($data['default']) && $data['default']) ? true : false;
                }
            }

            $this->writeUnifiedSearchModulesDisplayFile($unified_search_modules_display);
        }

        include('custom/modules/unified_search_modules_display.php');
        return $unified_search_modules_display;
    }

    /**
     * @deprecated since v7.12.0
     * writeUnifiedSearchModulesDisplayFile
     * Private method to handle writing the unified_search_modules_display value to file
     *
     * @param mixed The array of the unified search modules and their display attributes
     * @return boolean value indication whether or not file was successfully written
     * @throws Exception Thrown if the file write operation fails
     */
    private function writeUnifiedSearchModulesDisplayFile($unified_search_modules_display)
    {
        if (is_null($unified_search_modules_display) || empty($unified_search_modules_display)) {
            return false;
        }

        if (!write_array_to_file("unified_search_modules_display", $unified_search_modules_display, 'custom/modules/unified_search_modules_display.php')) {
            //Log error message and throw Exception
            global $app_strings;
            $msg = string_format($app_strings['ERR_FILE_WRITE'], array('custom/modules/unified_search_modules_display.php'));
            $GLOBALS['log']->error($msg);
            throw new Exception($msg);
        }

        return true;
    }
}

/**
 * @deprecated since v7.12.0
 * @param $a
 * @param $b
 * @return int
 */
function unified_search_modules_cmp($a, $b)
{
    if (!isset($a['translated'], $b['translated'])) {
        return 0;
    }

    $name1 = strtolower($a['translated']);
    $name2 = strtolower($b['translated']);

    return $name1 < $name2 ? -1 : 1;
}
