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

global $theme;




$GLOBALS['log']->info("Campaign detail view");

$xtpl=new XTemplate('modules/Campaigns/PopupCampaignRoi.html');

$campaign = BeanFactory::newBean('Campaigns');
$campaign_id = $campaign->db->quote($_REQUEST['id']);
$opp_query1  = "select camp.name, camp.actual_cost,camp.budget,camp.expected_revenue,count(*) opp_count,SUM(opp.amount) as Revenue, SUM(camp.actual_cost) as Investment,
                            ROUND((SUM(opp.amount) - SUM(camp.actual_cost))/(SUM(camp.actual_cost)), 2)*100 as ROI";
            $opp_query1 .= " from opportunities opp";
            $opp_query1 .= " right join campaigns camp on camp.id = opp.campaign_id";
            $opp_query1 .= " where opp.sales_stage = 'Closed Won' and camp.id='$campaign_id'";
            $opp_query1 .= " group by camp.name";
            //$opp_query1 .= " and deleted=0";
            $opp_result1=$campaign->db->query($opp_query1);
            $opp_data1=$campaign->db->fetchByAssoc($opp_result1);
 //get the click-throughs
 $query_click = "SELECT count(*) hits ";
            $query_click.= " FROM campaign_log ";
            $query_click.= " WHERE campaign_id = '$campaign_id' AND activity_type='link' AND related_type='CampaignTrackers' AND archived=0 AND deleted=0";

            //if $marketing id is specified, then lets filter the chart by the value
            if (!empty($marketing_id)) {
                $query_click.= " AND marketing_id ='$marketing_id'";
            }

            $query_click.= " GROUP BY  activity_type, target_type";
            $query_click.= " ORDER BY  activity_type, target_type";
            $result = $campaign->db->query($query_click);


  $xtpl->assign("OPP_COUNT", $opp_data1['opp_count']);
  $xtpl->assign("ACTUAL_COST", $opp_data1['actual_cost']);
  $xtpl->assign("PLANNED_BUDGET", $opp_data1['budget']);
  $xtpl->assign("EXPECTED_REVENUE", $opp_data1['expected_revenue']);




    $currency  = BeanFactory::newBean('Currencies');
if (isset($focus->currency_id) && !empty($focus->currency_id)) {
    $currency->retrieve($focus->currency_id);
    if ($currency->deleted != 1) {
        $xtpl->assign("CURRENCY", $currency->iso4217 .' '.$currency->symbol);
    } else {
        $xtpl->assign("CURRENCY", $currency->getDefaultISO4217() .' '.$currency->getDefaultCurrencySymbol());
    }
} else {
    $xtpl->assign("CURRENCY", $currency->getDefaultISO4217() .' '.$currency->getDefaultCurrencySymbol());
}

global $current_user;
if (is_admin($current_user) && $_REQUEST['module'] != 'DynamicLayout' && !empty($_SESSION['editinplace'])) {
    $xtpl->assign("ADMIN_EDIT", "<a href='index.php?action=index&module=DynamicLayout&from_action=".$_REQUEST['action'] ."&from_module=".$_REQUEST['module'] ."&record=".$_REQUEST['record']. "'>".SugarThemeRegistry::current()->getImage("EditLayout", "border='0' align='bottom'", null, null, '.gif', $mod_strings['LBL_EDIT_LAYOUT'])."</a>");
}

//$detailView->processListNavigation($xtpl, "CAMPAIGN", $offset, $focus->is_AuditEnabled());
// adding custom fields:
//require_once('modules/DynamicFields/templates/Files/DetailView.php');

/* we need to build the dropdown of related marketing values
    $latest_marketing_id = '';
    $selected_marketing_id = '';
    if(isset($_REQUEST['mkt_id'])) $selected_marketing_id = $_REQUEST['mkt_id'];
    $options_str = '<option value="all">--None--</option>';
    //query for all email marketing records related to this campaign
    $latest_marketing_query = "select id, name, date_modified from email_marketing where campaign_id = '$focus->id' order by date_modified desc";

    //build string with value(s) retrieved
    $result =$campaign->db->query($latest_marketing_query);
    if ($row = $campaign->db->fetchByAssoc($result)){
        //first, populated the latest marketing id variable, as this
        // variable will be used to build chart and subpanels
        $latest_marketing_id = $row['id'];
        //fill in first option value
        $options_str .= '<option value="'. $row['id'] .'"';
        // if the marketing id is same as selected marketing id, set this option to render as "selected"
        if (!empty($selected_marketing_id) && $selected_marketing_id == $row['id']) {
            $options_str .=' selected>'. $row['name'] .'</option>';
        // if the marketing id is empty then set this first option to render as "selected"
        }elseif(empty($selected_marketing_id)){
            $options_str .=' selected>'. $row['name'] .'</option>';
        // if the marketing is not empty, but not same as selected marketing id, then..
        //.. do not set this option to render as "selected"
        }else{
            $options_str .='>'. $row['name'] .'</option>';
        }
    }
    //process rest of records, if they exist
    while ($row = $campaign->db->fetchByAssoc($result)){
        //add to list of option values
        $options_str .= '<option value="'. $row['id'] .'"';
        //if the marketing id is same as selected marketing id, then set this option to render as "selected"
        if (!empty($selected_marketing_id) && $selected_marketing_id == $row['id']) {
            $options_str .=' selected>'. $row['name'] .'</option>';
        }else{
            $options_str .=' >'. $row['name'] .'</option>';
        }
     }
    //populate the dropdown
    $xtpl->assign("MKT_DROP_DOWN",$options_str);

  */

//add chart
$seps				= array("-", "/");
$dates				= array(date($GLOBALS['timedate']->dbDayFormat), $GLOBALS['timedate']->dbDayFormat);
$dateFileNameSafe	= str_replace($seps, "_", $dates);
$cache_file_name_roi	= $current_user->getUserPrivGuid()."_campaign_response_by_roi_".$dateFileNameSafe[0]."_".$dateFileNameSafe[1].".xml";
$chart= new campaign_charts();

//ob_start();

    //if marketing id has been selected, then set "latest_marketing_id" to the selected value
    //latest marketing id will be passed in to filter the charts and subpanels

    if (!empty($selected_marketing_id)) {
        $latest_marketing_id = $selected_marketing_id;
    }
    if (empty($latest_marketing_id) ||  $latest_marketing_id === 'all') {
        $xtpl->assign("MY_CHART_ROI", $chart->campaign_response_roi_popup($app_list_strings['roi_type_dom'], $app_list_strings['roi_type_dom'], $campaign_id, sugar_cached("xml/") . $cache_file_name_roi, true));
    } else {
        $xtpl->assign("MY_CHART_ROI", $chart->campaign_response_roi_popup($app_list_strings['roi_type_dom'], $app_list_strings['roi_type_dom'], $campaign_id, sugar_cached("xml/") .$cache_file_name_roi, true));
    }

//$output_html .= ob_get_contents();
//ob_end_clean();


//end chart

$xtpl->parse("main");
$xtpl->out("main");
