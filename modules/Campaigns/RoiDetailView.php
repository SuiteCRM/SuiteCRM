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






require_once('include/DetailView/DetailView.php');
require_once('modules/Campaigns/Charts.php');


global $mod_strings;
global $app_strings;
global $app_list_strings;
global $sugar_version, $sugar_config;

$focus = BeanFactory::newBean('Campaigns');

$detailView = new DetailView();
$offset = 0;
$offset=0;
if (isset($_REQUEST['offset']) || isset($_REQUEST['record'])) {
    $result = $detailView->processSugarBean("CAMPAIGN", $focus, $offset);
    if ($result == null) {
        sugar_die($app_strings['ERROR_NO_RECORD']);
    }
    $focus=$result;
} else {
    $header_URL = "Location: index.php?module=Accounts&action=index";
    SugarApplication::headerRedirect($header_URL);
}

// For all campaigns show the same ROI interface
// ..else default to legacy detail view
/*
if(!$focus->campaign_type == "NewsLetter"){
    include ('modules/Campaigns/NewsLetterTrackDetailView.php');
} else{

*/
    echo getClassicModuleTitle($mod_strings['LBL_MODULE_NAME'], array($mod_strings['LBL_MODULE_NAME'],$focus->name), true);
    
    $GLOBALS['log']->info("Campaign detail view");
    
    $smarty = new Sugar_Smarty();
    $smarty->assign("MOD", $mod_strings);
    $smarty->assign("APP", $app_strings);
    
    $smarty->assign("THEME", $theme);
    $smarty->assign("GRIDLINE", $gridline);
    $smarty->assign("PRINT_URL", "index.php?".$GLOBALS['request_string']);
    $smarty->assign("ID", $focus->id);
    $smarty->assign("ASSIGNED_TO", $focus->assigned_user_name);
    $smarty->assign("STATUS", $app_list_strings['campaign_status_dom'][$focus->status]);
    $smarty->assign("NAME", $focus->name);
    $smarty->assign("TYPE", $app_list_strings['campaign_type_dom'][$focus->campaign_type]);
    $smarty->assign("START_DATE", $focus->start_date);
    $smarty->assign("END_DATE", $focus->end_date);
    
    $smarty->assign("BUDGET", $focus->budget);
    $smarty->assign("ACTUAL_COST", $focus->actual_cost);
    $smarty->assign("EXPECTED_COST", $focus->expected_cost);
    $smarty->assign("EXPECTED_REVENUE", $focus->expected_revenue);
    
    
    $smarty->assign("OBJECTIVE", nl2br($focus->objective));
    $smarty->assign("CONTENT", nl2br($focus->content));
    $smarty->assign("DATE_MODIFIED", $focus->date_modified);
    $smarty->assign("DATE_ENTERED", $focus->date_entered);
    
    $smarty->assign("CREATED_BY", $focus->created_by_name);
    $smarty->assign("MODIFIED_BY", $focus->modified_by_name);
    $smarty->assign("TRACKER_URL", $sugar_config['site_url'] . '/campaign_tracker.php?track=' . $focus->tracker_key);
    $smarty->assign("TRACKER_COUNT", (int)$focus->tracker_count);
    $smarty->assign("TRACKER_TEXT", $focus->tracker_text);
    $smarty->assign("REFER_URL", $focus->refer_url);
    $smarty->assign("IMPRESSIONS", $focus->impressions);
   $roi_vals = array();
   $roi_vals['budget']= $focus->budget;
   $roi_vals['actual_cost']= $focus->actual_cost;
   $roi_vals['Expected_Revenue']= $focus->expected_revenue;
   $roi_vals['Expected_Cost']= $focus->expected_cost;
   
//Query for opportunities won, clickthroughs
$campaign_id = $focus->id;
            $opp_query1  = "select camp.name, count(*) opp_count,SUM(opp.amount) as Revenue, SUM(camp.actual_cost) as Investment, 
                            ROUND((SUM(opp.amount) - SUM(camp.actual_cost))/(SUM(camp.actual_cost)), 2)*100 as ROI";
            $opp_query1 .= " from opportunities opp";
            $opp_query1 .= " right join campaigns camp on camp.id = opp.campaign_id";
            $opp_query1 .= " where opp.sales_stage = 'Closed Won' and camp.id='$campaign_id'";
            $opp_query1 .= " and opp.deleted=0";
            $opp_query1 .= " group by camp.name";
            $opp_result1=$focus->db->query($opp_query1);
            $opp_data1=$focus->db->fetchByAssoc($opp_result1);
      if (empty($opp_data1['opp_count'])) {
          $opp_data1['opp_count']=0;
      }

     $smarty->assign("OPPORTUNITIES_WON", $opp_data1['opp_count']);
          
            $camp_query1  = "select camp.name, count(*) click_thru_link";
            $camp_query1 .= " from campaign_log camp_log";
            $camp_query1 .= " right join campaigns camp on camp.id = camp_log.campaign_id";
            $camp_query1 .= " where camp_log.activity_type = 'link' and camp.id='$campaign_id'";
            $camp_query1 .= " group by camp.name";
            $opp_query1 .= " and deleted=0";
            $camp_result1=$focus->db->query($camp_query1);
            $camp_data1=$focus->db->fetchByAssoc($camp_result1);
            
   if (unformat_number($focus->impressions) > 0) {
       $cost_per_impression= unformat_number($focus->actual_cost)/unformat_number($focus->impressions);
   } else {
       $cost_per_impression = format_number(0);
   }
   $smarty->assign("COST_PER_IMPRESSION", currency_format_number($cost_per_impression));
   if (empty($camp_data1['click_thru_link'])) {
       $camp_data1['click_thru_link']=0;
   }
   $click_thru_links = $camp_data1['click_thru_link'];
   
   if ($click_thru_links >0) {
       $cost_per_click_thru= unformat_number($focus->actual_cost)/unformat_number($click_thru_links);
   } else {
       $cost_per_click_thru = format_number(0);
   }
   $smarty->assign("COST_PER_CLICK_THROUGH", currency_format_number($cost_per_click_thru));
    
    
        $currency  = BeanFactory::newBean('Currencies');
    if (isset($focus->currency_id) && !empty($focus->currency_id)) {
        $currency->retrieve($focus->currency_id);
        if ($currency->deleted != 1) {
            $smarty->assign("CURRENCY", $currency->iso4217 .' '.$currency->symbol);
        } else {
            $smarty->assign("CURRENCY", $currency->getDefaultISO4217() .' '.$currency->getDefaultCurrencySymbol());
        }
    } else {
        $smarty->assign("CURRENCY", $currency->getDefaultISO4217() .' '.$currency->getDefaultCurrencySymbol());
    }
    global $current_user;
    if (is_admin($current_user) && $_REQUEST['module'] != 'DynamicLayout' && !empty($_SESSION['editinplace'])) {
        $smarty->assign("ADMIN_EDIT", "<a href='index.php?action=index&module=DynamicLayout&from_action=".$_REQUEST['action'] ."&from_module=".$_REQUEST['module'] ."&record=".$_REQUEST['record']. "'>".SugarThemeRegistry::current()->getImage("EditLayout", "border='0' align='bottom'", null, null, '.gif', $mod_strings['LBL_EDIT_LAYOUT'])."</a>");
    }
    global $xtpl;
    $detailView->processListNavigation($xtpl, "CAMPAIGN", $offset, $focus->is_AuditEnabled());
    // adding custom fields:
    $xtpl = $smarty;
    require_once('modules/DynamicFields/templates/Files/DetailView.php');
    

    
    
    
    //add chart
    $seps				= array("-", "/");
    $dates				= array(date($GLOBALS['timedate']->dbDayFormat), $GLOBALS['timedate']->dbDayFormat);
    $dateFileNameSafe	= str_replace($seps, "_", $dates);
    //$cache_file_name	= $current_user->getUserPrivGuid()."_campaign_response_by_activity_type_".$dateFileNameSafe[0]."_".$dateFileNameSafe[1].".xml";
    $cache_file_name_roi	= $current_user->getUserPrivGuid()."_campaign_response_by_roi_".$dateFileNameSafe[0]."_".$dateFileNameSafe[1].".xml";
    $chart= new campaign_charts();
    $smarty->assign("MY_CHART_ROI", $chart->campaign_response_roi($app_list_strings['roi_type_dom'], $app_list_strings['roi_type_dom'], $focus->id, true, true));
    //end chart
    //custom chart code
    require_once('include/SugarCharts/SugarChartFactory.php');
    $sugarChart = SugarChartFactory::getInstance();
    if ($sugarChart) {
        $resources = $sugarChart->getChartResources();
        $smarty->assign('chartResources', $resources);
    }

echo $smarty->fetch('modules/Campaigns/RoiDetailView.tpl');
