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



/************** general UI Stuff *************/


require_once('modules/Campaigns/utils.php');



$focus = BeanFactory::newBean('Campaigns');
if (isset($_REQUEST['record']) &&  !empty($_REQUEST['record'])) {
    $focus->retrieve($_REQUEST['record']);

    /*
    $marketingLink = "index.php?action=WizardMarketing&module=Campaigns&return_module=Campaigns&return_action=WizardHome";
    $marketingLink .= "&return_id=".$focus->id."&campaign_id=".$focus->id."&jump=3";
    SugarApplication::redirect($marketingLink);
    return;
    */

    global $mod_strings;
    global $app_list_strings;
    global $app_strings;
    global $current_user;


    //if (!is_admin($current_user)) sugar_die("Unauthorized access to administration.");
    //account for use within wizards
    if ($focus->campaign_type == 'NewsLetter') {
        echo getClassicModuleTitle($mod_strings['LBL_MODULE_NAME'], array($mod_strings['LBL_NEWSLETTER_WIZARD_START_TITLE'].$focus->name), true, false);
    } else {
        echo getClassicModuleTitle($mod_strings['LBL_MODULE_NAME'], array($mod_strings['LBL_CAMPAIGN_WIZARD_START_TITLE'].$focus->name), true, false);
    }

    global $theme;
    global $currentModule;






    $ss = new Sugar_Smarty();
    $ss->assign("MOD", $mod_strings);
    $ss->assign("APP", $app_strings);

    //if this page has been refreshed as a result of sending emails, then display status
    if (isset($_REQUEST['from'])) {
        $mess = $mod_strings['LBL_TEST_EMAILS_SENT'];
        if ($_REQUEST['from']=='send') {
            $mess = $mod_strings['LBL_EMAILS_SCHEDULED'];
        }
        $confirm_msg = "var ajaxWizStatus = new SUGAR.ajaxStatusClass(); ";
        $confirm_msg .= "window.setTimeout(\"ajaxWizStatus.showStatus('".$mess."')\",1000); ";
        $confirm_msg .= "window.setTimeout('ajaxWizStatus.hideStatus()', 1500); ";
        $confirm_msg .= "window.setTimeout(\"ajaxWizStatus.showStatus('".$mess."')\",2000); ";
        $confirm_msg .= "window.setTimeout('ajaxWizStatus.hideStatus()', 5000); ";
        $ss->assign("MSG_SCRIPT", $confirm_msg);
    }

    if (isset($_REQUEST['return_module'])) {
        $ss->assign("RETURN_MODULE", $_REQUEST['return_module']);
    }
    if (isset($_REQUEST['return_action'])) {
        $ss->assign("RETURN_ACTION", $_REQUEST['return_action']);
    }
    if (isset($_REQUEST['return_id'])) {
        $ss->assign("RETURN_ID", $_REQUEST['return_id']);
    }
    if (isset($_REQUEST['record'])) {
        $ss->assign("ID", $_REQUEST['record']);
    }
    // handle Create $module then Cancel
    if (empty($_REQUEST['return_id'])) {
        $ss->assign("RETURN_ACTION", 'index');
    }



    $ss->assign("CAMPAIGN_TBL", create_campaign_summary($focus));
    $ss->assign("TARGETS_TBL", create_target_summary($focus));
    if ($focus->campaign_type =='NewsLetter' || $focus->campaign_type =='Email') {
        $ss->assign("MARKETING_TBL", create_marketing_summary($focus));
        $ss->assign("TRACKERS_TBL", create_tracker_summary($focus));
    }

    $camp_url = "index.php?action=WizardNewsletter&module=Campaigns&return_module=Campaigns&return_action=WizardHome";
    $camp_url .= "&return_id=".$focus->id."&record=".$focus->id."&direct_step=";
    $ss->assign("CAMP_WIZ_URL", $camp_url);

    $mrkt_string = $mod_strings['LBL_NAVIGATION_MENU_MARKETING'];
    if (!empty($focus->id)) {
        $mrkt_url = "<a  href='index.php?action=WizardMarketing&module=Campaigns&return_module=Campaigns&return_action=WizardHome";
        $mrkt_url .= "&return_id=".$focus->id."&campaign_id=".$focus->id;
        $mrkt_url .= "'>". $mrkt_string."</a>";
        $mrkt_string = $mrkt_url;
    }

    $mrkt_url = "<a  href='index.php?action=WizardMarketing&module=Campaigns&return_module=Campaigns&return_action=WizardHome";
    $mrkt_url .= "&return_id=".$focus->id."&campaign_id=".$focus->id;
    $mrkt_url .= "'>". $mod_strings['LBL_NAVIGATION_MENU_MARKETING']."</a>";
    $ss->assign("MRKT_WIZ_URL", $mrkt_url);

    $summ_url = "<a  href='index.php?action=WizardHome&module=Campaigns";
    $summ_url .= "&return_id=".$focus->id."&record=".$focus->id;
    $summ_url .= "'> ". $mod_strings['LBL_NAVIGATION_MENU_SUMMARY']."</a>";


    //Create the html to fill in the wizard steps
    if ($focus->campaign_type == 'NewsLetter') {
        $ss->assign('NAV_ITEMS', create_wiz_menu_items('newsletter', $mrkt_string, $camp_url, $summ_url));
        $ss->assign("CAMPAIGN_DIAGNOSTIC_LINK", diagnose());
    } elseif ($focus->campaign_type == 'Email') {
        $ss->assign('NAV_ITEMS', create_wiz_menu_items('email', $mrkt_string, $camp_url, $summ_url));
        $ss->assign("CAMPAIGN_DIAGNOSTIC_LINK", diagnose());
    } else {
        $ss->assign('NAV_ITEMS', create_wiz_menu_items('general', $mrkt_string, $camp_url, $summ_url));
    }


    /********** FINAL END OF PAGE UI Stuff ********/
    $ss->display(file_exists('custom/modules/Campaigns/WizardHome.html') ? 'custom/modules/Campaigns/WizardHome.html' : 'modules/Campaigns/WizardHome.html');

    function isWizardSummary()
    {
        return $_REQUEST['action'] == 'WizardHome';
    }

    function getFirstMarketingId($campaignId)
    {
        $db = DBManagerFactory::getInstance();
        $campaignId = $db->quote($campaignId);
        $emailMarketings = BeanFactory::getBean('EmailMarketing')->get_full_list("", "campaign_id = '$campaignId'");
        $firstEmailMarketing = $emailMarketings[0] ?? null;
        if (!is_object($firstEmailMarketing)) {
            $ret = null;
            LoggerManager::getLogger()->warn('Campaign Wisard Home trying to get first marketing id but the email marketings does not contains it');
        } else {
            $ret = $firstEmailMarketing->id;
        }
        return $ret;
    }

    function getMarketingId()
    {
        $campaignId = isset($_REQUEST['campaign_id']) && $_REQUEST['campaign_id'] ? $_REQUEST['campaign_id'] : $_REQUEST['record'];
        $ret = isset($_POST['marketing_id']) && $_POST['marketing_id'] ? $_POST['marketing_id'] : (
            isset($_REQUEST['marketing_id']) && $_REQUEST['marketing_id'] ? $_REQUEST['marketing_id'] : getFirstMarketingId($campaignId)
        );
        return $ret;
    }

    if ((isset($_REQUEST['WizardMarketingSave']) && $_REQUEST['WizardMarketingSave']) || isWizardSummary()) {
        $campaign_id = $focus->id;
        $marketing_id = getMarketingId();
        if (!$marketing_id && $_SERVER['REQUEST_METHOD'] == 'POST' && $_POST['action'] == 'WizardHome') {
            $header_URL = "Location: index.php?module=Campaigns&offset=1&return_module=Campaigns&action=DetailView&record=" . $campaign_id;
        } else {
            $header_URL = "Location: index.php?action=WizardMarketing&module=Campaigns&return_module=Campaigns&return_action=WizardHome&return_id=" . $campaign_id . "&campaign_id=" . $campaign_id . "&jump=3&show_wizard_marketing=1&marketing_id=" . $marketing_id . "&record=" . $marketing_id . '&campaign_type=' . $focus->campaign_type . (isset($_REQUEST['template_id']) && $_REQUEST['template_id'] ? '&template_id=' . $_REQUEST['template_id'] : '');
        }
        SugarApplication::headerRedirect($header_URL);
    }
} else {
    //there is no record to retrieve, so ask which type of campaign wizard to launch
    /*    $header_URL = "Location: index.php?module=Campaigns&action=index";
        $GLOBALS['log']->debug("about to post header URL of: $header_URL");
        header($header_URL);
    */
    global $mod_strings;
    global $app_list_strings;
    global $app_strings;
    global $current_user;

    //if (!is_admin($current_user)) sugar_die("Unauthorized access to administration.");
    //account for use within wizards
    echo getClassicModuleTitle($mod_strings['LBL_MODULE_NAME'], array($mod_strings['LBL_CAMPAIGN_WIZARD'].$focus->name), true, false);


    $ss = new Sugar_Smarty();
    $ss->assign("MOD", $mod_strings);
    $ss->assign("APP", $app_strings);
    unset($_SESSION['campaignWizard'][isset($campaign_id) ? $campaign_id : null]['defaultSelectedTemplateId']);
    $ss->display(file_exists('custom/modules/Campaigns/tpls/WizardHomeStart.tpl') ? 'custom/modules/Campaigns/tpls/WizardHomeStart.tpl' : 'modules/Campaigns/tpls/WizardHomeStart.tpl');
}




function create_campaign_summary($focus)
{
    global $mod_strings,$app_strings;
    $fields = array();
    $fields[] = 'name';
    $fields[] = 'assigned_user_name';
    $fields[] = 'status';
    $fields[] = 'team_name';
    $fields[] = 'start_date';
    $fields[] = 'end_date';
    if ($focus->campaign_type=='NewsLetter') {
        $fields[] = 'frequency';
    }
    $fields[] = 'content';
    $fields[] = 'budget';
    $fields[] = 'actual_cost';
    $fields[] = 'expected_revenue';
    $fields[] = 'expected_cost';
    $fields[] = 'impressions';
    $fields[] = 'objective';

    //create edit view status and input buttons
    $cmp_input = '';

    //create edit campaign button
    $cmp_input =  "<input id='wiz_next_button' name='SUBMIT'  ";
    $cmp_input.= "onclick=\"this.form.return_module.value='Campaigns';";
    $cmp_input.= "this.form.module.value='Campaigns';";
    $cmp_input.= "this.form.action.value='WizardNewsletter';";
    $cmp_input.= "this.form.return_action.value='WizardHome';";
    $cmp_input.= "this.form.direct_step.value='1';";
    $cmp_input.= "this.form.record.value='".$focus->id."';";
    $cmp_input.= "this.form.return_id.value='".$focus->id."';\" ";
    $cmp_input.= "class='button' value='".$mod_strings['LBL_EDIT_EXISTING']."' type='submit'> ";

    //create view status button
    if (($focus->campaign_type == 'NewsLetter') || ($focus->campaign_type == 'Email')) {
        $cmp_input .=  " <input id='wiz_status_button' name='SUBMIT'  ";
        $cmp_input.= "onclick=\"this.form.return_module.value='Campaigns';";
        $cmp_input.= "this.form.module.value='Campaigns';";
        $cmp_input.= "this.form.action.value='TrackDetailView';";
        $cmp_input.= "this.form.return_action.value='WizardHome';";
        $cmp_input.= "this.form.record.value='".$focus->id."';";
        $cmp_input.= "this.form.return_id.value='".$focus->id."';\" ";
        $cmp_input.= "class='button' value='".$mod_strings['LBL_TRACK_BUTTON_TITLE']."' type='submit'>";
    }
    //create view roi button
    $cmp_input .=  " <input id='wiz_status_button' name='SUBMIT'  ";
    $cmp_input.= "onclick=\"this.form.return_module.value='Campaigns';";
    $cmp_input.= "this.form.module.value='Campaigns';";
    $cmp_input.= "this.form.action.value='RoiDetailView';";
    $cmp_input.= "this.form.return_action.value='WizardHome';";
    $cmp_input.= "this.form.record.value='".$focus->id."';";
    $cmp_input.= "this.form.return_id.value='".$focus->id."';\" ";
    $cmp_input.= "class='button' value='".$mod_strings['LBL_TRACK_ROI_BUTTON_LABEL']."' type='submit'>";

    //Create Campaign Header
    $cmpgn_tbl = "<p><table class='edit view' width='100%' border='0' cellspacing='0' cellpadding='0'>";
    $cmpgn_tbl .= "<tr><td class='dataField' align='left'><h4 class='dataLabel'> ".$mod_strings['LBL_LIST_CAMPAIGN_NAME'].'  '. $mod_strings['LBL_WIZ_NEWSLETTER_TITLE_SUMMARY']." </h4></td>";
    $cmpgn_tbl .= "<td align='right'>$cmp_input</td></tr>";
    $colorclass = '';
    foreach ($fields as $key) {
        if (!empty($focus->$key) && !empty($mod_strings[$focus->field_name_map[$key]['vname']])) {
            $cmpgn_tbl .= "<tr><td scope='row' width='15%'>".$mod_strings[$focus->field_name_map[$key]['vname']]."</td>\n";
            if ($key == 'team_name') {
                require_once('modules/Teams/TeamSetManager.php');
                $cmpgn_tbl .= "<td scope='row'>".TeamSetManager::getCommaDelimitedTeams($focus->team_set_id, $focus->team_id, true)."</td></tr>\n";
            } else {
                $cmpgn_tbl .= "<td scope='row'>".$focus->$key."</td></tr>\n";
            }
        }
    }
    $cmpgn_tbl .= "</table></p>";

    return $cmpgn_tbl ;
}

function create_marketing_summary($focus)
{
    global $mod_strings,$app_strings;
    $colorclass = '';

    //create new marketing button input
    $new_mrkt_input =  "<input id='wiz_new_mrkt_button' name='SUBMIT' ";
    $new_mrkt_input .= "onclick=\"this.form.return_module.value='Campaigns';";
    $new_mrkt_input .= "this.form.module.value='Campaigns';";
    $new_mrkt_input .= "this.form.record.value='';";
    $new_mrkt_input .= "this.form.return_module.value='Campaigns';";
    $new_mrkt_input .= "this.form.action.value='WizardMarketing';";
    $new_mrkt_input .= "this.form.return_action.value='WizardHome';";
    $new_mrkt_input .= "this.form.direct_step.value='1';";
    $new_mrkt_input .= "this.form.campaign_id.value='".$focus->id."';";
    $new_mrkt_input .= "this.form.return_id.value='".$focus->id."';\" ";
    $new_mrkt_input .= "class='button' value='".$mod_strings['LBL_CREATE_NEW_MARKETING_EMAIL']."' type='submit'>";

    //create marketing email table
    $mrkt_tbl='';

    $focus->load_relationship('emailmarketing');
    $mrkt_lists = $focus->emailmarketing->get();


    $mrkt_tbl = "<p><table  class='list view' width='100%' border='0' cellspacing='1' cellpadding='1'>";
    $mrkt_tbl .= "<tr class='detail view'><td colspan='3'><h4> ".$mod_strings['LBL_WIZ_MARKETING_TITLE']." </h4></td>" .
                 "<td colspan=2 align='right'>$new_mrkt_input</td></tr>";
    $mrkt_tbl .= "<tr  class='listViewHRS1'><td scope='col' width='15%'><b>".$mod_strings['LBL_MRKT_NAME']."</b></td><td width='15%' scope='col'><b>".$mod_strings['LBL_FROM_MAILBOX_NAME']."</b></td><td width='15%' scope='col'><b>".$mod_strings['LBL_STATUS_TEXT']."</b></td><td scope='col' colspan=2>&nbsp;</td></tr>";

    if ((is_countable($mrkt_lists) ? count($mrkt_lists) : 0)>0) {
        $mrkt_focus = BeanFactory::newBean('EmailMarketing');
        foreach ($mrkt_lists as $mrkt_id) {
            $mrkt_focus->retrieve($mrkt_id);

            //create send test marketing button input
            $test_mrkt_input =  "<input id='wiz_new_mrkt_button' name='SUBMIT'  ";
            $test_mrkt_input .= "onclick=\"this.form.return_module.value='Campaigns'; ";
            $test_mrkt_input .= "this.form.module.value='Campaigns'; ";
            $test_mrkt_input .= "this.form.record.value='';";
            $test_mrkt_input .= "this.form.return_module.value='Campaigns'; ";
            $test_mrkt_input .= "this.form.action.value='QueueCampaign'; ";
            $test_mrkt_input .= "this.form.return_action.value='WizardHome'; ";
            $test_mrkt_input .= "this.form.wiz_mass.value='".$mrkt_focus->id."'; ";
            $test_mrkt_input .= "this.form.mode.value='test'; ";
            $test_mrkt_input .= "this.form.direct_step.value='1'; ";
            $test_mrkt_input .= "this.form.record.value='".$focus->id."'; ";
            $test_mrkt_input .= "this.form.return_id.value='".$focus->id."';\" ";
            $test_mrkt_input .= "class='button' value='".$mod_strings['LBL_TEST_BUTTON_LABEL']."' type='submit'>";

            //create send marketing button input
            $send_mrkt_input =  "<input id='wiz_new_mrkt_button' name='SUBMIT'  ";
            $send_mrkt_input .= "onclick=\"this.form.return_module.value='Campaigns'; ";
            $send_mrkt_input .= "this.form.module.value='Campaigns'; ";
            $send_mrkt_input .= "this.form.record.value='';";
            $send_mrkt_input .= "this.form.return_module.value='Campaigns'; ";
            $send_mrkt_input .= "this.form.action.value='QueueCampaign'; ";
            $send_mrkt_input .= "this.form.return_action.value='WizardHome'; ";
            $send_mrkt_input .= "this.form.wiz_mass.value='".$mrkt_focus->id."'; ";
            $send_mrkt_input .= "this.form.mode.value='send'; ";
            $send_mrkt_input .= "this.form.direct_step.value='1'; ";
            $send_mrkt_input .= "this.form.record.value='".$focus->id."'; ";
            $send_mrkt_input .= "this.form.return_id.value='".$focus->id."';\" ";
            $send_mrkt_input .= "class='button' value='".$mod_strings['LBL_SEND_EMAIL']."' type='submit'>";



            if ($colorclass== "class='evenListRowS1'") {
                $colorclass= "class='oddListRowS1'";
            } else {
                $colorclass= "class='evenListRowS1'";
            }

            if (isset($mrkt_focus->name) && !empty($mrkt_focus->name)) {
                $mrkt_tbl  .= "<tr $colorclass>";
                $mrkt_tbl  .= "<td scope='row' width='40%'><a href='index.php?action=WizardMarketing&module=Campaigns&return_module=Campaigns&return_action=WizardHome";
                $mrkt_tbl  .= "&return_id=" .$focus->id. "&campaign_id=" .$focus->id."&record=".$mrkt_focus->id."'>".$mrkt_focus->name."</a></td>";
                $mrkt_tbl  .= "<td scope='row' width='25%'>".$mrkt_focus->from_name."</td>";
                $mrkt_tbl  .= "<td scope='row' width='15%'>".$mrkt_focus->status."</td>";
                $mrkt_tbl  .= "<td scope='row' width='10%'>$test_mrkt_input</td>";
                $mrkt_tbl  .= "<td scope='row' width='10%'>$send_mrkt_input</td>";
                $mrkt_tbl  .= "</tr>";
            }
        }
    } else {
        $mrkt_tbl  .= "<tr><td colspan='3'>".$mod_strings['LBL_NONE']."</td></tr>";
    }
    $mrkt_tbl .= "</table></p>";
    return $mrkt_tbl ;
}

function create_target_summary($focus)
{
    global $mod_strings,$app_strings,$app_list_strings;
    $colorclass = '';
    $camp_type = $focus->campaign_type;


    //create schedule table
    $pltbl='';
    //set the title based on campaign type
    $target_title = $mod_strings['LBL_TARGET_LISTS'];
    if ($camp_type=='NewsLetter') {
        $target_title = $mod_strings['LBL_NAVIGATION_MENU_SUBSCRIPTIONS'];
    }


    $focus->load_relationship('prospectlists');
    $pl_lists = $focus->prospectlists->get();

    $pl_tbl = "<p><table align='center' class='list view' width='100%' border='0' cellspacing='1' cellpadding='1'>";
    $pl_tbl .= "<tr class='detail view'><td colspan='4'><h4> ".$target_title." </h4></td></tr>";
    $pl_tbl .= "<tr class='listViewHRS1'><td width='50%' scope='col'><b>".$mod_strings['LBL_LIST_NAME']."</b></td><td width='30%' scope='col'><b>".$mod_strings['LBL_LIST_TYPE']."</b></td>";
    $pl_tbl .= "<td width='15%' scope='col'><b>".$mod_strings['LBL_TOTAL_ENTRIES']."</b></td><td width='5%' scope='col'>&nbsp;</td></tr>";

    if ((is_countable($pl_lists) ? count($pl_lists) : 0)>0) {
        $pl_focus = BeanFactory::newBean('ProspectLists');
        foreach ($pl_lists as $pl_id) {
            if ($colorclass== "class='evenListRowS1'") {
                $colorclass= "class='oddListRowS1'";
            } else {
                $colorclass= "class='evenListRowS1'";
            }

            $pl_focus->retrieve($pl_id);
            //set the list type if this is a newsletter
            $type=$pl_focus->list_type;
            if ($camp_type=='NewsLetter') {
                if (($pl_focus->list_type == 'default') || ($pl_focus->list_type == 'seed')) {
                    $type = $mod_strings['LBL_SUBSCRIPTION_TYPE_NAME'];
                }
                if ($pl_focus->list_type == 'exempt') {
                    $type = $mod_strings['LBL_UNSUBSCRIPTION_TYPE_NAME'];
                }
                if ($pl_focus->list_type == 'test') {
                    $type = $mod_strings['LBL_TEST_TYPE_NAME'];
                }
            } else {
                $type = $app_list_strings['prospect_list_type_dom'][$pl_focus->list_type];
            }
            if (isset($pl_focus->id) && !empty($pl_focus->id)) {
                $pl_tbl  .= "<tr $colorclass>";
                $pl_tbl  .= "<td scope='row' width='50%'><a href='index.php?action=DetailView&module=ProspectLists&return_module=Campaigns&return_action=WizardHome&return_id=" .$focus->id. "&record=".$pl_focus->id."'>";
                $pl_tbl  .=  $pl_focus->name."</a></td>";
                $pl_tbl  .= "<td scope='row' width='30%'>$type</td>";
                $pl_tbl  .= "<td scope='row' width='15%'>".$pl_focus->get_entry_count()."</td>";
                $pl_tbl  .= "<td scope='row' width='5%' align='right'><a href='index.php?action=EditView&module=ProspectLists&return_module=Campaigns&return_action=WizardHome&return_id=" .$focus->id. "&record=".$pl_focus->id."'>";
                $pl_tbl  .= SugarThemeRegistry::current()->getImage('edit_inline', 'border=0', null, null, ".gif", $mod_strings['LBL_EDIT_INLINE']) . "</a>&nbsp;";


                $pl_tbl  .= "<a href='index.php?action=DetailView&module=ProspectLists&return_module=Campaigns&return_action=WizardHome&return_id=" .$focus->id. "&record=".$pl_focus->id."'>";
                $pl_tbl  .= SugarThemeRegistry::current()->getImage('view_inline', 'border=0', null, null, ".gif", $mod_strings['LBL_VIEW_INLINE'])."</a></td>";
            }
        }
    } else {
        $pl_tbl .= "<tr><td class='$colorclass' scope='row' colspan='2'>".$mod_strings['LBL_NONE']."</td></tr>";
    }

    $pl_tbl .= "</table></p>";
    return $pl_tbl;
}

function create_tracker_summary($focus)
{
    global $mod_strings,$app_strings;
    $colorclass = '';
    $trkr_tbl='';
    //create tracker table
    $focus->load_relationship('tracked_urls');
    $trkr_lists = $focus->tracked_urls->get();

    $trkr_tbl = "<p><table align='center' class='list view' width='100%' border='0' cellspacing='1' cellpadding='1'>";
    $trkr_tbl .= "<tr class='detail view'><td colspan='6'><h4> ".$mod_strings['LBL_NAVIGATION_MENU_TRACKERS']." </h4></td></tr>";
    $trkr_tbl .= "<tr class='listViewHRS1'><td width='15%' scope='col'><b>".$mod_strings['LBL_EDIT_TRACKER_NAME']."</b></td><td width='15%' scope='col'><b>".$mod_strings['LBL_EDIT_TRACKER_URL']."</b></td><td width='15%' scope='col'><b>".$mod_strings['LBL_EDIT_OPT_OUT']."</b></td></tr>";

    if ((is_countable($trkr_lists) ? count($trkr_lists) : 0)>0) {
        foreach ($trkr_lists as $trkr_id) {
            if ($colorclass== "class='evenListRowS1'") {
                $colorclass= "class='oddListRowS1'";
            } else {
                $colorclass= "class='evenListRowS1'";
            }


            $ct_focus = BeanFactory::newBean('CampaignTrackers');
            $ct_focus->retrieve($trkr_id);
            if (isset($ct_focus->tracker_name) && !empty($ct_focus->tracker_name)) {
                if ($ct_focus->is_optout) {
                    $opt = 'checked';
                } else {
                    $opt = '';
                }
                $trkr_tbl  .= "<tr $colorclass>";
                $trkr_tbl  .= "<td scope='row' ><a href='index.php?action=DetailView&module=CampaignTrackers&return_module=Campaigns&return_action=WizardHome&return_id=" .$focus->id. "&record=".$ct_focus->id."'>";
                $trkr_tbl  .= $ct_focus->tracker_name."</a></td>";
                $trkr_tbl  .= "<td scope='row' width='15%'>".$ct_focus->tracker_url."</td>";
                $trkr_tbl  .= "<td scope='row' width='15%'>&nbsp;&nbsp;<input type='checkbox' class='checkbox' $opt disabled></td>";
                $trkr_tbl  .= "</tr>";
            }
        }
    } else {
        $trkr_tbl  .= "<tr ><td colspan='3'>".$mod_strings['LBL_NONE']."</td>";
    }
    $trkr_tbl .= "</table></p>";
    return $trkr_tbl ;
}


function create_wiz_menu_items($type, $mrkt_string, $camp_url, $summ_url)
{

    global $mod_strings;

    $steps = [];
    $steps[$mod_strings['LBL_NAVIGATION_MENU_GEN1']]          = file_exists('custom/modules/Campaigns/tpls/WizardCampaignHeader.tpl') ? 'custom/modules/Campaigns/tpls/WizardCampaignHeader.tpl' : 'modules/Campaigns/tpls/WizardCampaignHeader.tpl';
    $steps[$mod_strings['LBL_NAVIGATION_MENU_GEN2']]          = file_exists('custom/modules/Campaigns/tpls/WizardCampaignBudget.tpl') ? 'custom/modules/Campaigns/tpls/WizardCampaignBudget.tpl' : 'modules/Campaigns/tpls/WizardCampaignBudget.tpl';

    //do not show tracker step for general campaigns (only for newsletter/email)
    if ($type != 'general') {
        $steps[$mod_strings['LBL_NAVIGATION_MENU_TRACKERS']]      = file_exists('custom/modules/Campaigns/tpls/WizardCampaignTracker.tpl') ? 'custom/modules/Campaigns/tpls/WizardCampaignTracker.tpl' : 'modules/Campaigns/tpls/WizardCampaignTracker.tpl';
    }

    if ($type == 'newsletter') {
        $steps[$mod_strings['LBL_NAVIGATION_MENU_SUBSCRIPTIONS']] = file_exists('custom/modules/Campaigns/tpls/WizardCampaignTargetList.tpl') ? 'custom/modules/Campaigns/tpls/WizardCampaignTargetList.tpl' : 'modules/Campaigns/tpls/WizardCampaignTargetList.tpl';
    } else {
        $steps[$mod_strings['LBL_TARGET_LISTS']]                  = file_exists('custom/modules/Campaigns/tpls/WizardCampaignTargetListForNonNewsLetter.tpl') ? 'custom/modules/Campaigns/tpls/WizardCampaignTargetListForNonNewsLetter.tpl' : 'modules/Campaigns/tpls/WizardCampaignTargetListForNonNewsLetter.tpl';
    }

    $nav_html = '<table border="0" cellspacing="0" cellpadding="0" width="100%" >';
    if (isset($steps)  && !empty($steps)) {
        $i=1;
        foreach ($steps as $name=>$step) {
            $nav_html .= "<tr><td scope='row' nowrap><div id='nav_step$i'><a href='".$camp_url.$i."'>$name</a></div></td></tr>";
            $i=$i+1;
        }
    }

    if ($type == 'newsletter'  ||  $type == 'email') {
        $nav_html .= "<td scope='row' nowrap><div id='nav_step'".($i+1).">$mrkt_string</div></td></tr>";
        $nav_html .= "<td scope='row' nowrap><div id='nav_step'".($i+2).">".$mod_strings['LBL_NAVIGATION_MENU_SEND_EMAIL']."</div></td></tr>";
        $nav_html .= "<td scope='row' nowrap><div id='nav_step'".($i+3).">".$summ_url."</div></td></tr>";
    } else {
        $nav_html .= "<td scope='row' nowrap><div id='nav_step'".($i+1).">".$summ_url."</div></td></tr>";
    }

    $nav_html .= '</table>';

    return $nav_html;
}
