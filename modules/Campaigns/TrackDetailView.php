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

// if campaign type is set to newsletter, then include newsletter detail view..
// ..else default to legacy detail view

//    include ('modules/Campaigns/NewsLetterTrackDetailView.php');

if (isset($focus->campaign_type) && $focus->campaign_type == "NewsLetter") {
    echo getClassicModuleTitle($mod_strings['LBL_MODULE_NAME'], array($mod_strings['LBL_NEWSLETTER'],$focus->name), true);
} else {
    echo getClassicModuleTitle($mod_strings['LBL_MODULE_NAME'], array($mod_strings['LBL_MODULE_NAME'],$focus->name), true);
}

    $GLOBALS['log']->info("Campaign detail view");
    $smarty = new Sugar_Smarty();
    $smarty->assign("MOD", $mod_strings);
    $smarty->assign("APP", $app_strings);

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

    if (isset($focus->campaign_type) && $focus->campaign_type == "Email" || $focus->campaign_type == "NewsLetter") {
        $smarty->assign("TRACK_DELETE_BUTTON", "<input title=\"{$mod_strings['LBL_TRACK_DELETE_BUTTON_TITLE']}\" class=\"button\" onclick=\"this.form.module.value='Campaigns'; this.form.action.value='Delete';this.form.return_module.value='Campaigns'; this.form.return_action.value='TrackDetailView';this.form.mode.value='Test';return confirm('{$mod_strings['LBL_TRACK_DELETE_CONFIRM']}');\" type=\"submit\" name=\"button\" value=\"  {$mod_strings['LBL_TRACK_DELETE_BUTTON_LABEL']}  \">");
    }

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
    $xtpl = $smarty;

    $detailView->processListNavigation($xtpl, "CAMPAIGN", $offset, $focus->is_AuditEnabled());
    // adding custom fields:
    require_once('modules/DynamicFields/templates/Files/DetailView.php');


    //if this is a newsletter, we need to build dropdown
    $selected_marketing_id = '';
    if (isset($focus->campaign_type)) {
        //we need to build the dropdown of related marketing values
        $options_str = "<select onchange= \"this.form.module.value='Campaigns';this.form.action.value='TrackDetailView'; submit()\" name='mkt_id'>";
        $latest_marketing_id = '';
        if (isset($_REQUEST['mkt_id'])) {
            $selected_marketing_id = $_REQUEST['mkt_id'];
        }

        $options_str .= '<option value="all">'.$app_strings["LBL_CAMPAIGN_NONE"].'</option>';
        //query for all email marketing records related to this campaign
        $latest_marketing_query = "select id, name, date_modified from email_marketing where campaign_id = '$focus->id' order by date_modified desc";

        //build string with value(s) retrieved
        $result =$focus->db->query($latest_marketing_query);
        if ($row = $focus->db->fetchByAssoc($result)) {
            //first, populated the latest marketing id variable, as this
            // variable will be used to build chart and subpanels
            if ($focus->campaign_type == 'NewsLetter') {
                $latest_marketing_id = $row['id'];
            }

            //fill in first option value
            $options_str .= '<option value="'. $row['id'] .'"';
            // if the marketing id is same as selected marketing id, set this option to render as "selected"
            if (!empty($selected_marketing_id) && $selected_marketing_id == $row['id']) {
                $options_str .=' selected>'. $row['name'] .'</option>';
            // if the marketing id is empty then set this first option to render as "selected"
            } elseif (empty($selected_marketing_id) && $focus->campaign_type == 'NewsLetter') {
                $options_str .=' selected>'. $row['name'] .'</option>';
            // if the marketing is not empty, but not same as selected marketing id, then..
            //.. do not set this option to render as "selected"
            } else {
                $options_str .='>'. $row['name'] .'</option>';
            }
        }
        //process rest of records, if they exist
        while ($row = $focus->db->fetchByAssoc($result)) {
            //add to list of option values
            $options_str .= '<option value="'. $row['id'] .'"';
            //if the marketing id is same as selected marketing id, then set this option to render as "selected"
            if (!empty($selected_marketing_id) && $selected_marketing_id == $row['id']) {
                $options_str .=' selected>'. $row['name'] .'</option>';
            } else {
                $options_str .=' >'. $row['name'] .'</option>';
            }
        }
        $options_str .="</select>";
        //populate the dropdown
        $smarty->assign("FILTER_LABEL", $mod_strings['LBL_FILTER_CHART_BY']);
        $smarty->assign("MKT_DROP_DOWN", $options_str);
    }
//add chart
$seps               = array("-", "/");
$dates              = array(date($GLOBALS['timedate']->dbDayFormat), $GLOBALS['timedate']->dbDayFormat);
$dateFileNameSafe   = str_replace($seps, "_", $dates);
$cache_file_name    = $current_user->getUserPrivGuid()."_campaign_response_by_activity_type_".$dateFileNameSafe[0]."_".$dateFileNameSafe[1].".xml";
$cache_file_name_roi    = $current_user->getUserPrivGuid()."_campaign_response_by_roi_".$dateFileNameSafe[0]."_".$dateFileNameSafe[1].".xml";
$chart= new campaign_charts();

    //if marketing id has been selected, then set "latest_marketing_id" to the selected value
    //latest marketing id will be passed in to filter the charts and subpanels

    if (!empty($selected_marketing_id)) {
        $latest_marketing_id = $selected_marketing_id;
    }
    if (empty($latest_marketing_id) ||  $latest_marketing_id === 'all') {
        $smarty->assign("MY_CHART", $chart->campaign_response_by_activity_type($app_list_strings['campainglog_activity_type_dom'], $app_list_strings['campainglog_target_type_dom'], $focus->id, sugar_cached("xml/$cache_file_name"), true));
    } else {
        $smarty->assign("MY_CHART", $chart->campaign_response_by_activity_type($app_list_strings['campainglog_activity_type_dom'], $app_list_strings['campainglog_target_type_dom'], $focus->id, sugar_cached("xml/$cache_file_name"), true, $latest_marketing_id));
    }

//end chart
//custom chart code
    require_once('include/SugarCharts/SugarChartFactory.php');
    $sugarChart = SugarChartFactory::getInstance();
    if ($sugarChart) {
        $resources = $sugarChart->getChartResources();
        $smarty->assign('chartResources', $resources);
    }

echo $smarty->fetch('modules/Campaigns/TrackDetailView.tpl');

require_once('include/SubPanel/SubPanelTiles.php');
$subpanel = new SubPanelTiles($focus, 'Campaigns');
    //if latest marketing id is empty, or if it is set to 'all'', then do no filtering, otherwise filter..
    //.. out the chart and subpanels by marketing id
    if (empty($latest_marketing_id) || $latest_marketing_id === 'all') {
        //do nothing, no filtering is needed
    } else {

        // assign selected marketing ID back to request in order to let ListView use it as a part of subpanel base URL
        $_GET['mkt_id'] = $latest_marketing_id;

        //get array of layout defs
        $layoutDefsArr= $subpanel->subpanel_definitions->layout_defs;

        //iterate through layout defs for processing of subpanels.  If a marketing Id is specified, then we need to...
        //.. filter the subpanels by it so they match the chart rendered in code above.
        foreach ($layoutDefsArr as $subpanels_name => $subpanels) {

            //process each subpanel definition
            foreach ($subpanels as $subpane_key => $subpane) {

                    //see if "function_parameters" key exists in subpanel properties array
                if (isset($subpane['function_parameters'])) {
                    //if a function_parameters property key exists, then process further
                          $functionParamsArr = $subpane['function_parameters'];//$panelProperty;

                            //Check the array of function parameters and see if
                    //one exists for market value id.
                    if (isset($functionParamsArr['EMAIL_MARKETING_ID_VALUE'])) {
                        //We found the property, lets fill in the marketing id value...
                        //.. into the subpanel object, using the keys of the array that..
                        //.. we used to get to thi property
                        $subpanel->subpanel_definitions->layout_defs[$subpanels_name][$subpane_key]['function_parameters']['EMAIL_MARKETING_ID_VALUE'] = $latest_marketing_id;
                    }
                }//end if (isset($subpane['function_parameters'])){
            }//end foreach($subpanels as $subpane_key => $subpane){
        }
    }//end else

$deletedCampaignLogLeadsCount = $focus->getDeletedCampaignLogLeadsCount();
if ($deletedCampaignLogLeadsCount > 0) {
    $subpanel->subpanel_definitions->layout_defs['subpanel_setup']['lead']['top_buttons'][] = array(
        'widget_class' => 'SubPanelTopMessage',
        'message' => string_format($mod_strings['LBL_LEADS_DELETED_SINCE_CREATED'], array($deletedCampaignLogLeadsCount)),
    );
}

$alltabs=$subpanel->subpanel_definitions->get_available_tabs();
if (!empty($alltabs)) {
    foreach ($alltabs as $name) {
        if ($name == 'prospectlists' || $name=='emailmarketing' || $name == 'tracked_urls') {
            $subpanel->subpanel_definitions->exclude_tab($name);
        }
    }
}
echo $subpanel->display();
