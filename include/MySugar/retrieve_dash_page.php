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

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

global $current_user, $sugar_version, $sugar_config, $beanFiles;


require_once('include/MySugar/MySugar.php');

// build dashlet cache file if not found
if (!is_file($cachefile = sugar_cached('dashlets/dashlets.php'))) {
    require_once('include/Dashlets/DashletCacheBuilder.php');

    $dc = new DashletCacheBuilder();
    $dc->buildCache();
}
require_once $cachefile;

require('modules/Home/dashlets.php');

$pages = $current_user->getPreference('pages', 'Home');
$dashlets = $current_user->getPreference('dashlets', 'Home');

$defaultHomepage = false;
// BEGIN fill in with default homepage and dashlet selections

$hasUserPreferences = (!isset($pages) || empty($pages) || !isset($dashlets) || empty($dashlets)) ? false : true;

if (!$hasUserPreferences) {
    $dashlets = array();

    //list of preferences to move over and to where
    $prefstomove = array(
        'mypbss_date_start' => 'MyPipelineBySalesStageDashlet',
        'mypbss_date_end' => 'MyPipelineBySalesStageDashlet',
        'mypbss_sales_stages' => 'MyPipelineBySalesStageDashlet',
        'mypbss_chart_type' => 'MyPipelineBySalesStageDashlet',
        'lsbo_lead_sources' => 'OpportunitiesByLeadSourceByOutcomeDashlet',
        'lsbo_ids' => 'OpportunitiesByLeadSourceByOutcomeDashlet',
        'pbls_lead_sources' => 'OpportunitiesByLeadSourceDashlet',
        'pbls_ids' => 'OpportunitiesByLeadSourceDashlet',
        'pbss_date_start' => 'PipelineBySalesStageDashlet',
        'pbss_date_end' => 'PipelineBySalesStageDashlet',
        'pbss_sales_stages' => 'PipelineBySalesStageDashlet',
        'pbss_chart_type' => 'PipelineBySalesStageDashlet',
        'obm_date_start' => 'OutcomeByMonthDashlet',
        'obm_date_end' => 'OutcomeByMonthDashlet',
        'obm_ids' => 'OutcomeByMonthDashlet');

    //upgrading from pre-5.0 homepage
    $old_columns = $current_user->getPreference('columns', 'home');
    $old_dashlets = $current_user->getPreference('dashlets', 'home');

    if (isset($old_columns) && !empty($old_columns) && isset($old_dashlets) && !empty($old_dashlets)) {
        $columns = $old_columns;
        $dashlets = $old_dashlets;

        // resetting old columns and dashlets to have no preference and data
        $old_columns = array();
        $old_dashlets = array();
        $current_user->setPreference('columns', $old_columns, 0, 'home');
        $current_user->setPreference('dashlets', $old_dashlets, 0, 'home');
    } else {
        // This is here to get Sugar dashlets added above the rest
        $dashlets[create_guid()] = array('className' => 'iFrameDashlet',
            'module' => 'Home',
            'forceColumn' => 0,
            'fileLocation' => $dashletsFiles['iFrameDashlet']['file'],
            'options' => array('titleLabel' => 'LBL_DASHLET_DISCOVER_SUITE',
                'url' => 'https://suitecrm.com/',
                'height' => 315,
            ));

        $dashlets[create_guid()] = array('className' => 'SugarFeedDashlet',
            'module' => 'SugarFeed',
            'forceColumn' => 1,
            'fileLocation' => $dashletsFiles['SugarFeedDashlet']['file'],
        );

        $dashlets[create_guid()] = array('className' => 'iFrameDashlet',
            'module' => 'Home',
            'forceColumn' => 1,
            'fileLocation' => $dashletsFiles['iFrameDashlet']['file'],
            'options' => array('titleLabel' => 'LBL_DASHLET_SUITE_NEWS',
                'url' => 'https://suitecrm.com/about/newsroom/news/',
                'height' => 315,
            ));

        foreach ($defaultDashlets as $dashletName=>$module) {
            // clint - fixes bug #20398
            // only display dashlets that are from visibile modules and that the user has permission to list
            $myDashlet = new MySugar($module);
            $displayDashlet = $myDashlet->checkDashletDisplay();
            if (isset($dashletsFiles[$dashletName]) && $displayDashlet) {
                $options = array();
                $prefsforthisdashlet = array_keys($prefstomove, $dashletName);
                foreach ($prefsforthisdashlet as $pref) {
                    $options[$pref] = $current_user->getPreference($pref);
                }
                $dashlets[create_guid()] = array('className' => $dashletName,
                    'module' => $module,
                    'forceColumn' => 0,
                    'fileLocation' => $dashletsFiles[$dashletName]['file'],
                    'options' => $options);
            }
        }

        $count = 0;
        $columns = array();
        $columns[0] = array();
        $columns[0]['width'] = '60%';
        $columns[0]['dashlets'] = array();
        $columns[1] = array();
        $columns[1]['width'] = '40%';
        $columns[1]['dashlets'] = array();

        foreach ($dashlets as $guid=>$dashlet) {
            if ($dashlet['forceColumn'] == 0) {
                array_push($columns[0]['dashlets'], $guid);
            } else {
                array_push($columns[1]['dashlets'], $guid);
            }
            $count++;
        }
    }




    $current_user->setPreference('dashlets', $dashlets, 0, 'Home');
}

// handles upgrading from versions that had the 'Dashboard' module; move those items over to the Home page
$pagesDashboard = $current_user->getPreference('pages', 'Dashboard');
$dashletsDashboard = $current_user->getPreference('dashlets', 'Dashboard');
if (!empty($pagesDashboard)) {
    // move dashlets from the dashboard to be at the end of the home screen dashlets
    foreach ($pagesDashboard[0]['columns'] as $dashboardColumnKey => $dashboardColumn) {
        foreach ($dashboardColumn['dashlets'] as $dashletItem) {
            $pages[0]['columns'][$dashboardColumnKey]['dashlets'][] = $dashletItem;
        }
    }
    $pages = array_merge($pages, $pagesDashboard);
    $current_user->setPreference('pages', $pages, 0, 'Home');
}
if (!empty($dashletsDashboard)) {
    $dashlets = array_merge($dashlets, $dashletsDashboard);
    $current_user->setPreference('dashlets', $dashlets, 0, 'Home');
}
if (!empty($pagesDashboard) || !empty($dashletsDashboard)) {
    $current_user->resetPreferences('Dashboard');
}

if (empty($pages)) {
    $pages = array();
    $pageIndex = 0;
    $pages[0]['columns'] = $columns;
    $pages[0]['numColumns'] = '2';
    $pages[0]['pageTitleLabel'] = 'LBL_HOME_PAGE_1_NAME';	// "My Sugar"
    $pageIndex++;
    $current_user->setPreference('pages', $pages, 0, 'Home');
    $activePage = 0;
}

$sugar_smarty = new Sugar_Smarty();

$activePage = isset($_REQUEST['page_id']) && $_REQUEST['page_id'] ? $_REQUEST['page_id'] : 0;
$activePage = (int)$activePage;

$divPages[] = $activePage;

$numCols = (int)$pages[$activePage]['numColumns'];


$count = 0;
$dashletIds = array(); // collect ids to pass to javascript
$display = array();

foreach ($pages[$activePage]['columns'] as $colNum => $column) {
    if ($colNum == $numCols) {
        break;
    }
    $display[$colNum]['width'] = $column['width'];
    $display[$colNum]['dashlets'] = array();
    foreach ($column['dashlets'] as $num => $id) {
        // clint - fixes bug #20398
        // only display dashlets that are from visibile modules and that the user has permission to list
        if (!empty($id) && isset($dashlets[$id]) && is_file($dashlets[$id]['fileLocation'])) {
            $module = 'Home';
            if (!empty($dashletsFiles[$dashlets[$id]['className']]['module'])) {
                $module = $dashletsFiles[$dashlets[$id]['className']]['module'];
            }
            // Bug 24772 - Look into the user preference for the module the dashlet is a part of in case
            //             of the Report Chart dashlets.
            elseif (!empty($dashlets[$id]['module'])) {
                $module = $dashlets[$id]['module'];
            }

            $myDashlet = new MySugar($module);

            if ($myDashlet->checkDashletDisplay()) {
                require_once($dashlets[$id]['fileLocation']);
                $dashlet = new $dashlets[$id]['className']($id, (isset($dashlets[$id]['options']) ? $dashlets[$id]['options'] : array()));
                // Need to add support to dynamically display/hide dashlets
                // If it has a method 'shouldDisplay' we will call it to see if we should display it or not
                if (method_exists($dashlet, 'shouldDisplay')) {
                    if (!$dashlet->shouldDisplay()) {
                        // This dashlet doesn't want us to show it, skip it.
                        continue;
                    }
                }

                array_push($dashletIds, $id);

                $dashlets = $current_user->getPreference('dashlets', 'Home'); // Using hardcoded 'Home' because DynamicAction.php $_REQUEST['module'] value is always Home

                if (!empty($_REQUEST['id']) && $_REQUEST['id'] == $id) {
                    $sortOrder = '';
                    $orderBy = '';
                    foreach ($_REQUEST as $k => $v) {
                        if ($k == 'lvso') {
                            $sortOrder = $v;
                        } else {
                            if (preg_match('/Home2_.+_ORDER_BY/', $k)) {
                                $orderBy = $v;
                            }
                        }
                    }
                    if (!empty($sortOrder) && !empty($orderBy)) {
                        $dashlets[$id]['sort_options'] = array('sortOrder' => $sortOrder, 'orderBy' => $orderBy);
                        $current_user->setPreference('dashlets', $dashlets, 0, 'Home');
                    }
                }

                $lvsParams = array();
                if (!empty($dashlets[$id]['sort_options'])) {
                    $lvsParams = $dashlets[$id]['sort_options'];
                }

                $dashlet->process($lvsParams, $id);
                try {
                    $display[$colNum]['dashlets'][$id]['display'] = $dashlet->display();
                    $display[$colNum]['dashlets'][$id]['displayHeader'] = $dashlet->getHeader();
                    $display[$colNum]['dashlets'][$id]['displayFooter'] = $dashlet->getFooter();
                    if ($dashlet->hasScript) {
                        $display[$colNum]['dashlets'][$id]['script'] = $dashlet->displayScript();
                    }
                } catch (Exception $ex) {
                    $display[$colNum]['dashlets'][$id]['display'] = $ex->getMessage();
                    $display[$colNum]['dashlets'][$id]['displayHeader'] = $dashlet->getHeader();
                    $display[$colNum]['dashlets'][$id]['displayFooter'] = $dashlet->getFooter();
                }
            }
        }
    }
}

$_SESSION['current_tab'] = $activePage;


if (!empty($sugar_config['lock_homepage']) && $sugar_config['lock_homepage'] == true) {
    $sugar_smarty->assign('lock_homepage', true);
}

$serverUniqueKey = $GLOBALS['server_unique_key'] ?? '';

$sugar_smarty->assign('colNum', (int)$numCols);
$sugar_smarty->assign('sugarVersion', $sugar_version);
$sugar_smarty->assign('currentLanguage', $GLOBALS['current_language']);
$sugar_smarty->assign('serverUniqueKey', $serverUniqueKey);
$sugar_smarty->assign('imagePath', $GLOBALS['image_path']);

$sugar_smarty->assign('maxCount', empty($sugar_config['max_dashlets_homepage']) ? 15 : $sugar_config['max_dashlets_homepage']);
$sugar_smarty->assign('dashletCount', $count);
$sugar_smarty->assign('dashletIds', '["' . implode('","', $dashletIds) . '"]');
$sugar_smarty->assign('columns', $display);

global $theme;
$sugar_smarty->assign('theme', $theme);

$sugar_smarty->assign('divPages', $divPages);
$sugar_smarty->assign('activePage', $activePage);

$sugar_smarty->assign('current_user', $current_user->id);

$sugar_smarty->assign('lblAdd', $GLOBALS['app_strings']['LBL_ADD_BUTTON']);
$sugar_smarty->assign('lblAddDashlets', $GLOBALS['app_strings']['LBL_ADD_DASHLETS']);
$sugar_smarty->assign('lblLnkHelp', $GLOBALS['app_strings']['LNK_HELP']);

$sugar_smarty->assign('mod', return_module_language($GLOBALS['current_language'], 'Home'));
$sugar_smarty->assign('app', $GLOBALS['app_strings']);
$sugar_smarty->assign('module', 'Home');
//custom chart code
require_once('include/SugarCharts/SugarChartFactory.php');
$sugarChart = SugarChartFactory::getInstance();
if ($sugarChart) {
    $resources = $sugarChart->getChartResources();
    $mySugarResources = $sugarChart->getMySugarChartResources();
    $sugar_smarty->assign('chartResources', $resources);
    $sugar_smarty->assign('mySugarChartResources', $mySugarResources);
}
if (file_exists("custom/include/MySugar/tpls/MySugar2.tpl")) {
    echo $sugar_smarty->fetch('custom/include/MySugar/tpls/MySugar2.tpl');
} else {
    echo $sugar_smarty->fetch('include/MySugar/tpls/MySugar2.tpl');
}


//init the quickEdit listeners after the dashlets have loaded on home page the first time
echo"<script>if(typeof(qe_init) != 'undefined'){qe_init();}</script>";
