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



/**************************** general UI Stuff *******************/



require_once('modules/Campaigns/utils.php');


global $app_strings;
global $timedate;
global $app_list_strings;
global $mod_strings;
global $current_user;
global $sugar_version, $sugar_config;


/**************************** GENERAL SETUP WORK*******************/
$campaign_focus = BeanFactory::newBean('Campaigns');
if (isset($_REQUEST['campaign_id']) && !empty($_REQUEST['campaign_id'])) {
    $campaign_focus->retrieve($_REQUEST['campaign_id']);
} else {
    sugar_die($app_strings['ERROR_NO_RECORD']);
}

global $theme;



$json = getJSONobj();

$GLOBALS['log']->info("Wizard Continue Create Wizard");
if ($campaign_focus->campaign_type=='NewsLetter') {
    echo getClassicModuleTitle($mod_strings['LBL_MODULE_NAME'], array($mod_strings['LBL_NEWSLETTER WIZARD_TITLE'].' '.$campaign_focus->name), true);
} else {
    echo getClassicModuleTitle($mod_strings['LBL_MODULE_NAME'], array($mod_strings['LBL_CAMPAIGN'].' '.$campaign_focus->name), true);
}

$ss = new Sugar_Smarty();
$ss->assign("MOD", $mod_strings);
$ss->assign("APP", $app_strings);
if (isset($_REQUEST['return_module'])) {
    $ss->assign("RETURN_MODULE", $_REQUEST['return_module']);
}
if (isset($_REQUEST['return_action'])) {
    $ss->assign("RETURN_ACTION", $_REQUEST['return_action']);
}
if (isset($_REQUEST['return_id'])) {
    $ss->assign("RETURN_ID", $_REQUEST['return_id']);
}
// handle Create $module then Cancel
$ss->assign('CAMPAIGN_ID', $campaign_focus->id);

$seps = get_number_separators();
$ss->assign("NUM_GRP_SEP", $seps[0]);
$ss->assign("DEC_SEP", $seps[1]);


/**************************** MARKETING UI DIV Stuff *******************/
//$campaign_focus->load_relationship('emailmarketing');
//$mrkt_ids = $campaign_focus->emailmarketing->get();

$mrkt_lists = [];
$mrkt_focus = BeanFactory::newBean('EmailMarketing');

//override marketing by session stored selection earlier..

if (isset($_REQUEST['func']) && $_REQUEST['func'] == 'createEmailMarketing') {
    unset($_SESSION['campaignWizard'][$campaign_focus->id]['defaultSelectedMarketingId']);
} else {
    //check to see if this campaign has an email marketing already attached, and if so, create duplicate
    $campaign_focus->load_relationship('emailmarketing');
    $mrkt_lists = $campaign_focus->emailmarketing->get();
}

if (!empty($_SESSION['campaignWizard'][$campaign_focus->id]['defaultSelectedMarketingId']) && !in_array($_SESSION['campaignWizard'][$campaign_focus->id]['defaultSelectedMarketingId'], $mrkt_lists)) {
    unset($_SESSION['campaignWizard'][$campaign_focus->id]['defaultSelectedMarketingId']);
}

if (!empty($_SESSION['campaignWizard'][$campaign_focus->id]['defaultSelectedMarketingId'])) {
    if (!empty($_REQUEST['record']) && in_array($_SESSION['campaignWizard'][$campaign_focus->id]['defaultSelectedMarketingId'], $mrkt_lists)) {
        $_REQUEST['record'] = $_SESSION['campaignWizard'][$campaign_focus->id]['defaultSelectedMarketingId'];
    }
    if (!empty($_REQUEST['marketing_id']) && in_array($_SESSION['campaignWizard'][$campaign_focus->id]['defaultSelectedMarketingId'], $mrkt_lists)) {
        if (!empty($_REQUEST['func']) && $_REQUEST['func'] == 'editEmailMarketing') {
            $_SESSION['campaignWizard'][$campaign_focus->id]['defaultSelectedMarketingId'] = $_REQUEST['marketing_id'];
        } else {
            $_REQUEST['marketing_id'] = $_SESSION['campaignWizard'][$campaign_focus->id]['defaultSelectedMarketingId'];
        }
    }
}

$mrktListsCount = is_countable($mrkt_lists) ? count($mrkt_lists) : 0;

//if record param exists and it is not empty, then retrieve this bean
if (isset($_REQUEST['record']) && !empty($_REQUEST['record'])) {
    $mrkt_focus->retrieve($_REQUEST['record']);
    $_SESSION['campaignWizard'][$campaign_focus->id]['defaultSelectedMarketingId'] = $mrkt_focus->id;
} else {
    if (isset($_REQUEST['marketing_id']) && !empty($_REQUEST['marketing_id'])) {
        $mrkt_focus->retrieve($_REQUEST['marketing_id']);
        $_SESSION['campaignWizard'][$campaign_focus->id]['defaultSelectedMarketingId'] = $mrkt_focus->id;
    } else {
        if (!isset($mrkt_lists) || !$mrkt_lists) {
            unset($_SESSION['campaignWizard'][$campaign_focus->id]['defaultSelectedMarketingId']);
        } elseif ($mrktListsCount == 1) {
            if (empty($_REQUEST['func']) || (isset($_REQUEST['func']) && $_REQUEST['func'] != 'createEmailMarketing')) {
                $mrkt_focus->retrieve($mrkt_lists[0]);
                $_SESSION['campaignWizard'][$campaign_focus->id]['defaultSelectedMarketingId'] = $mrkt_lists[0];
            } else {
                // if user clicks create from the email marking sub panel
                $mrkt_focus->retrieve($mrkt_lists[0]);
                $mrkt_focus->id = create_guid();
                $mrkt_focus->name = '';
                // clone
                $_SESSION['campaignWizard'][$campaign_focus->id]['defaultSelectedMarketingId'] = $mrkt_focus->id;
            }
        } else {
            if ($mrktListsCount > 1) {
                if (!empty($_SESSION['campaignWizard'][$campaign_focus->id]['defaultSelectedMarketingId']) && in_array($_SESSION['campaignWizard'][$campaign_focus->id]['defaultSelectedMarketingId'], $mrkt_lists)) {
                    if (!isset($_REQUEST['func']) || (empty($_REQUEST['func']) && $_REQUEST['func'] != 'createEmailMarketing')) {
                        $mrkt_focus->retrieve($_SESSION['campaignWizard'][$campaign_focus->id]['defaultSelectedMarketingId']);
                    } else {
                        // if user clicks create from the email marking sub panel
                        $mrkt_focus->retrieve($_SESSION['campaignWizard'][$campaign_focus->id]['defaultSelectedMarketingId']);
                        $mrkt_focus->id = create_guid();
                        $mrkt_focus->name = '';
                        // clone
                        $_SESSION['campaignWizard'][$campaign_focus->id]['defaultSelectedMarketingId'] = $mrkt_focus->id;
                    }
                } else {
                    unset($_SESSION['campaignWizard'][$campaign_focus->id]['defaultSelectedMarketingId']);
                }




                //        if(!empty($mrkt_lists)){
        //            //reverse array so we always use the most recent one:
        //            $mrkt_lists = array_reverse($mrkt_lists);
        //            $mrkt_focus->retrieve($mrkt_lists[0]);
        //            $mrkt_focus->id = '';
        //            //$mrkt_focus->name = $mod_strings['LBL_COPY_OF'] . ' '. $mrkt_focus->name;
        //        }
            } else {
                unset($_SESSION['campaignWizard'][$campaign_focus->id]['defaultSelectedMarketingId']);
                //throw new Exception('illegal related marketing list');
            }
        }
    }
}


$ss->assign("CALENDAR_LANG", "en");
$ss->assign("USER_DATEFORMAT", '('. $timedate->get_user_date_format().')');
$ss->assign("CALENDAR_DATEFORMAT", $timedate->get_cal_date_format());
$ss->assign("TIME_MERIDIEM", $timedate->AMPMMenu('', $mrkt_focus->time_start));
$ss->assign("MRKT_ID", $mrkt_focus->id);
$ss->assign("MRKT_NAME", $mrkt_focus->name);
$ss->assign("MRKT_FROM_NAME", $mrkt_focus->from_name);
$ss->assign("MRKT_FROM_ADDR", $mrkt_focus->from_addr);
$def = $mrkt_focus->getFieldDefinition('from_name');
$ss->assign("MRKT_FROM_NAME_LEN", $def['len']);

//jc: bug 15498
// assigning the length of the reply name from the var defs to the template to be used
// as the max length for the input field
$def = $mrkt_focus->getFieldDefinition('reply_to_name');
$ss->assign("MRKT_REPLY_NAME_LEN", $def['len']);
$ss->assign("MRKT_REPLY_NAME", $mrkt_focus->reply_to_name);
$def = $mrkt_focus->getFieldDefinition('reply_to_addr');
$ss->assign("MRKT_REPLY_ADDR_LEN", $def['len']);
// end bug 15498
$ss->assign("MRKT_REPLY_ADDR", $mrkt_focus->reply_to_addr);
$ss->assign("MRKT_DATE_START", $mrkt_focus->date_start);
$ss->assign("MRKT_TIME_START", $mrkt_focus->time_start);
//$_REQUEST['mass'] = $mrkt_focus->id;
$ss->assign("MRKT_ID", $mrkt_focus->id);
$emails=array();
$mailboxes=get_campaign_mailboxes($emails);

/*
 * get full array of stored options
 */
$IEStoredOptions = get_campaign_mailboxes_with_stored_options();
$IEStoredOptionsJSON = (!empty($IEStoredOptions)) ? $json->encode($IEStoredOptions, false) : 'new Object()';
$ss->assign("IEStoredOptions", $IEStoredOptionsJSON);

$OEStoredOptions = get_campaign_mailboxes_with_stored_options_outbound();
$OEStoredOptionsJSON = (!empty($OEStoredOptions)) ? $json->encode($OEStoredOptions, false) : 'new Object()';
$ss->assign("OEStoredOptions", $OEStoredOptionsJSON);

//add empty options.
$emails['']='nobody@example.com';
$mailboxes['']='';

//inbound_email_id
$default_email_address='nobody@example.com';
$from_emails = '';
foreach ($mailboxes as $id=>$name) {
    if (!empty($from_emails)) {
        $from_emails.=',';
    }
    if ($id=='') {
        $from_emails.="'EMPTY','$name','$emails[$id]'";
    } else {
        $from_emails.="'$id','$name','$emails[$id]'";
    }
}
$ss->assign("FROM_EMAILS", $from_emails);
$ss->assign("DEFAULT_FROM_EMAIL", $default_email_address);
$ss->assign("STATUS_OPTIONS", get_select_options_with_id($app_list_strings['email_marketing_status_dom'], $mrkt_focus->status ? $mrkt_focus->status : 'active'));
if (empty($mrkt_focus->inbound_email_id)) {
    $defaultMailboxId = '';
    $mailboxIds = array();
    foreach ($mailboxes as $mailboxId => $mailboxName) {
        if ($mailboxId) {
            $mailboxIds[] = $mailboxId;
        }
    }
    if (count($mailboxIds) == 1) {
        $defaultMailboxId = $mailboxIds[0];
    }
    $ss->assign("MAILBOXES", get_select_options_with_id($mailboxes, $defaultMailboxId));
    $ss->assign("MAILBOXES_DEAULT", $defaultMailboxId);
} else {
    $ss->assign("MAILBOXES", get_select_options_with_id($mailboxes, $mrkt_focus->inbound_email_id));
}

$outboundEmailAccountLabels = array();
$outboundEmailLabels = array();
$outboundEmailAccounts = BeanFactory::getBean('OutboundEmailAccounts')->get_full_list();
if ($outboundEmailAccounts) {
    foreach ($outboundEmailAccounts as $outboundEmailAccount) {
        $outboundEmailLabels[$outboundEmailAccount->id] = $outboundEmailAccount->name;
    }
} else {
    $GLOBALS['log']->warn('There are no outbound email accounts available.');
    $GLOBALS['log']->info('Please ensure that the email settings are configured correctly');
}

$ss->assign('OUTBOUND_MAILBOXES', get_select_options_with_id($outboundEmailLabels, $mrkt_focus->outbound_email_id));

$ss->assign("TIME_MERIDIEM", $timedate->AMPMMenu('', $mrkt_focus->time_start));
$ss->assign("TIME_FORMAT", '('. $timedate->get_user_time_format().')');

$email_templates_arr = get_bean_select_array(true, 'EmailTemplate', 'name', '', 'name');
if ($mrkt_focus->template_id) {
    $ss->assign("TEMPLATE_ID", $mrkt_focus->template_id);
    $templateId = $mrkt_focus->template_id;
    if (!$templateId && !empty($_SESSION['campaignWizard'][$campaign_focus->id]['defaultSelectedTemplateId'])) {
        $templateId = $_SESSION['campaignWizard'][$campaign_focus->id]['defaultSelectedTemplateId'];
    }
    $ss->assign("EMAIL_TEMPLATE_OPTIONS", get_select_options_with_id($email_templates_arr, $templateId));
    $ss->assign("EDIT_TEMPLATE", "visibility:inline");
    $ss->assign('email_template_already_selected', $mrkt_focus->template_id);
} else {
    $templateId = isset($_REQUEST['template_id']) && $_REQUEST['template_id'] ? $_REQUEST['template_id'] : "";
    if (!$templateId && !empty($_SESSION['campaignWizard'][$campaign_focus->id]['defaultSelectedTemplateId'])) {
        $templateId = $_SESSION['campaignWizard'][$campaign_focus->id]['defaultSelectedTemplateId'];
    }
    $ss->assign("EMAIL_TEMPLATE_OPTIONS", get_select_options_with_id($email_templates_arr, isset($_REQUEST['func']) && $_REQUEST['func'] == 'createEmailMarketing' ? null : $templateId));
    $ss->assign("EDIT_TEMPLATE", "visibility:hidden");
}


$scope_options=get_message_scope_dom($campaign_focus->id, $campaign_focus->name, $mrkt_focus->db);
$prospectlists=array();
if (isset($mrkt_focus->all_prospect_lists) && $mrkt_focus->all_prospect_lists==1) {
    $ss->assign("ALL_PROSPECT_LISTS_CHECKED", "checked");
    $ss->assign("MESSAGE_FOR_DISABLED", "disabled");
} else {
    //get select prospect list.
    if (!empty($mrkt_focus->id)) {
        $mrkt_focus->load_relationship('prospectlists');
        $prospectlists=$mrkt_focus->prospectlists->get();
    } else {
        $ss->assign("ALL_PROSPECT_LISTS_CHECKED", "checked");
        $ss->assign("MESSAGE_FOR_DISABLED", "disabled");
    };
}

// force to check all prospect list by default..
$ss->assign("ALL_PROSPECT_LISTS_CHECKED", "checked");
$ss->assign("MESSAGE_FOR_DISABLED", "disabled");

if (empty($prospectlists)) {
    $prospectlists=array();
}
if (empty($scope_options)) {
    $scope_options=array();
}
$ss->assign("SCOPE_OPTIONS", get_select_options_with_id($scope_options, $prospectlists));
$ss->assign("SAVE_CONFIRM_MESSAGE", $mod_strings['LBL_CONFIRM_SEND_SAVE']);



$javascript = new javascript();
$javascript->setFormName('wizform');
$javascript->setSugarBean($mrkt_focus);
$javascript->addAllFields('');
echo $javascript->getScript();

/**************************** Final Step UI DIV *******************/

//Grab the prospect list of type default
$default_pl_focus = ' ';
$campaign_focus->load_relationship('prospectlists');
$prospectlists=$campaign_focus->prospectlists->get();


$pl_count = 0;
$pl_lists = 0;
if (!empty($prospectlists)) {
    foreach ($prospectlists as $prospect_id) {
        $pl_focus = BeanFactory::newBean('ProspectLists');
        $pl_focus->retrieve($prospect_id);

        if (($pl_focus->list_type == 'default') || ($pl_focus->list_type == 'seed')) {
            $default_pl_focus= $pl_focus;
            // get count of all attached target types
            $pl_count = $default_pl_focus->get_entry_count();
        }
        $pl_lists = $pl_lists+1;
    }
}
//if count is 0, then hide inputs and and print warning message
$pl_diabled_test_too = true;
if ($pl_count==0) {
    if ($pl_lists==0) {
        //print no target list warning
        if ($campaign_focus->campaign_type != "Email" || $campaign_focus->campaign_type != "NewsLetter") {
            $ss->assign("WARNING_MESSAGE", $mod_strings['LBL_NO_TARGETS_WARNING_NON_EMAIL']);
            $ss->assign('error_on_target_list', $mod_strings['LBL_NO_TARGETS_WARNING_NON_EMAIL']);
        } else {
            $ss->assign("WARNING_MESSAGE", $mod_strings['LBL_NO_TARGETS_WARNING']);
            $ss->assign('error_on_target_list', $mod_strings['LBL_NO_TARGETS_WARNING']);
        }
    } else {
        //print no entries warning
        if ($campaign_focus->campaign_type=='NewsLetter' || $campaign_focus->campaign_type=='Survey') {
            $ss->assign("WARNING_MESSAGE", $mod_strings['LBL_NO_SUBS_ENTRIES_WARNING']);
            $ss->assign('error_on_target_list', $mod_strings['LBL_NO_SUBS_ENTRIES_WARNING']);
            $pl_diabled_test_too = false;
        } elseif ($campaign_focus->campaign_type=='Email') {
            $ss->assign("WARNING_MESSAGE", $mod_strings['LBL_NO_TARGET_ENTRIES_WARNING']);
            $ss->assign('error_on_target_list', $mod_strings['LBL_NO_TARGET_ENTRIES_WARNING']);
        } else {
            $ss->assign("WARNING_MESSAGE", $mod_strings['LBL_NO_TARGET_ENTRIES_WARNING_NON_EMAIL']);
            $ss->assign('error_on_target_list', $mod_strings['LBL_NO_TARGET_ENTRIES_WARNING_NON_EMAIL']);
        }
    }
    //disable the send email options
    $ss->assign("PL_DISABLED", 'disabled');
    $ss->assign("PL_DISABLED_TEST", $pl_diabled_test_too ? 'disabled' : false);
} else {
    //show inputs and assign type to be radio
}

if (!$list = BeanFactory::getBean('EmailMarketing')->get_full_list("", "campaign_id = '{$campaign_focus->id}' AND template_id IS NOT NULL AND template_id != ''")) {
    $ss->assign('error_on_templates', $mod_strings['LBL_NO_TEMPLATE_SELECTED']);
}



/**************************** WIZARD UI DIV Stuff *******************/

$additionalParams = '';
if (isset($_REQUEST['template_id']) && $_REQUEST['template_id']) {
    $additionalParams .= '&template_id=' . $_REQUEST['template_id'];
}
if (isset($_REQUEST['marketing_id']) && $_REQUEST['marketing_id']) {
    $additionalParams .= '&marketing_id=' . $_REQUEST['marketing_id'];
}

$camp_url = "index.php?action=WizardNewsletter&module=Campaigns&return_module=Campaigns&return_action=WizardHome";
$camp_url .= "&return_id=".$campaign_focus->id."&record=".$campaign_focus->id . $additionalParams ."&direct_step=";
$ss->assign("CAMP_WIZ_URL", $camp_url);
$summ_url = $mod_strings['LBL_NAVIGATION_MENU_SUMMARY'];
if (!empty($focus->id)) {
    $summ_url = "<a href='index.php?action=WizardHome&module=Campaigns";
    $summ_url .= "&return_id=".$focus->id."&record=".$focus->id;
    $summ_url .= "'> ". $mod_strings['LBL_NAVIGATION_MENU_SUMMARY']."</a>";
}
$summ_url = $mod_strings['LBL_NAVIGATION_MENU_SUMMARY'];
if (!empty($focus->id)) {
    $summ_url = "index.php?action=WizardHome&module=Campaigns&return_id=".$focus->id."&record=".$focus->id;
}
$ss->assign("SUMM_URL", $summ_url);

//  this is the wizard control script that resides in page
$divScript = <<<EOQ

 <script type="text/javascript" language="javascript">
    /*
     * this is the custom validation script that will call the right validation for each div
     */
    function validate_wiz_form(step){
        switch (step){
            case 'step1':
                if (!validate_step1()) {
                    check_form('wizform')
                    return false;
                }
                clear_all_errors();
                break;
            case 'step2':
            return check_form('wizform');
            break;
            default://no additional validation needed
        }
        return true;

    }

    function validate_step1() {
        if(!$('#template_id').val()) return false;
        return true;
    }

    showfirst('marketing')
</script>
EOQ;

//$ss->assign("WIZ_JAVASCRIPT", print_wizard_jscript());
$ss->assign("DIV_JAVASCRIPT", $divScript);





/**************************** FINAL END OF PAGE UI Stuff *******************/


if ($campaign_focus->campaign_type != 'Telesales' && (!isset($_REQUEST['campaign_type']) || $_REQUEST['campaign_type'] != 'Telesales')) {
    //$templateURLForProgressBar = '#';
    $templateURLForProgressBar = "index.php?action=WizardMarketing&module=Campaigns&return_module=Campaigns&return_action=WizardHome&return_id={$campaign_focus->id}&campaign_id={$campaign_focus->id}&jump=1&campaign_type=Email";
    if (isset($campaign_focus->id) && $campaign_focus->id && isset($mrkt_focus->id) && $mrkt_focus->id && isset($mrkt_focus->template_id) && $mrkt_focus->template_id) {
        $templateURLForProgressBar = "index.php?action=WizardMarketing&module=Campaigns&return_module=Campaigns&return_action=WizardHome&return_id={$campaign_focus->id}&campaign_id={$campaign_focus->id}&jump=1&marketing_id={$mrkt_focus->id}&record={$mrkt_focus->id}&campaign_type=Email&template_id={$mrkt_focus->template_id}";
    }
    if (isset($campaign_focus->id) && $campaign_focus->id && isset($mrkt_focus->template_id) && $mrkt_focus->template_id) {
        $templateURLForProgressBar = "index.php?action=WizardMarketing&module=Campaigns&return_module=Campaigns&return_action=WizardHome&return_id={$campaign_focus->id}&campaign_id={$campaign_focus->id}&jump=1&campaign_type=Email&template_id={$mrkt_focus->template_id}";
    }

    $marketingURLForProgressBar = false;
    if (isset($campaign_focus->id) && $campaign_focus->id && isset($mrkt_focus->id) && $mrkt_focus->id && isset($mrkt_focus->template_id) && $mrkt_focus->template_id) {
        $marketingURLForProgressBar = "index.php?action=WizardMarketing&module=Campaigns&return_module=Campaigns&return_action=WizardHome&return_id={$campaign_focus->id}&campaign_id={$campaign_focus->id}&jump=2&show_wizard_marketing=1&marketing_id={$mrkt_focus->id}&record={$mrkt_focus->id}&campaign_type=Email&template_id={$mrkt_focus->template_id}";
    }
}

$summaryURLForProgressBar = '#';
if (isset($campaign_focus->id) && $campaign_focus->id && isset($mrkt_focus->id) && $mrkt_focus->id && isset($mrkt_focus->template_id) && $mrkt_focus->template_id) {
    $summaryURLForProgressBar = "index.php?action=WizardMarketing&module=Campaigns&return_module=Campaigns&return_action=WizardHome&return_id={$campaign_focus->id}&campaign_id={$campaign_focus->id}&jump=3&show_wizard_marketing=1&marketing_id={$mrkt_focus->id}&record={$mrkt_focus->id}&campaign_type=Email&template_id={$mrkt_focus->template_id}";
}

$steps = array();
$steps[$mod_strings['LBL_NAVIGATION_MENU_GEN1']] = $camp_url.'1';
if ($campaign_focus->campaign_type == 'Telesales' || (isset($_REQUEST['campaign_type']) && $_REQUEST['campaign_type'] == 'Telesales')) {
    $steps[$mod_strings['LBL_NAVIGATION_MENU_GEN2']] = 'index.php?action=WizardNewsletter&module=Campaigns&return_module=Campaigns&return_action=WizardHome&return_id=' . $campaign_focus->id . '&record=' . $campaign_focus->id . '&direct_step=2';
    $steps[$mod_strings['LBL_TARGET_LIST']] = $camp_url.'2&show_target_list=1';
} else {
    $steps[$mod_strings['LBL_TARGET_LIST']] = $camp_url . '2';
}
if ($campaign_focus->campaign_type != 'Telesales' && (!isset($_REQUEST['campaign_type']) || $_REQUEST['campaign_type'] != 'Telesales')) {
    $steps[$mod_strings['LBL_SELECT_TEMPLATE']] = $templateURLForProgressBar;
    if (!$marketingURLForProgressBar) {
        $marketingURLForProgressBar = "index.php?action=WizardMarketing&module=Campaigns&return_module=Campaigns&return_action=WizardHome&return_id={$campaign_focus->id}&campaign_id={$campaign_focus->id}&jump=2&show_wizard_marketing=1&marketing_id={$mrkt_focus->id}&record={$mrkt_focus->id}&campaign_type=Email&template_id={$mrkt_focus->template_id}";
    }
    $steps[$mod_strings['LBL_NAVIGATION_MENU_MARKETING']] = $marketingURLForProgressBar;

    if ($summaryURLForProgressBar == '#') {
        $summaryURLForProgressBar = 'javascript:$(\'#wiz_cancel_button\').click();';
    }
    $steps[$mod_strings['LBL_NAVIGATION_MENU_SEND_EMAIL_AND_SUMMARY']] = $summaryURLForProgressBar;
} else {
    if ($summaryURLForProgressBar == '#') {
        $summaryURLForProgressBar = 'javascript:$("#wiz_cancel_button").click();';
    }
    $steps[$mod_strings['LBL_NAVIGATION_MENU_SUMMARY']] = $summaryURLForProgressBar;
}

include_once('modules/Campaigns/DotListWizardMenu.php');
$dotListWizardMenu = new DotListWizardMenu($mod_strings, $steps, true);
//    array(
//        $mod_strings['LBL_NAVIGATION_MENU_GEN1'] => $camp_url.'1',
//        $mod_strings['LBL_TARGET_LIST'] => $camp_url.'2',
//        //$mod_strings['LBL_NAVIGATION_MENU_GEN2'] => $camp_url.'2',
//        //$mod_strings['LBL_NAVIGATION_MENU_TRACKERS'] => $camp_url.'3',
//        $mod_strings['LBL_SELECT_TEMPLATE'] => $templateURLForProgressBar,
//        $mod_strings['LBL_NAVIGATION_MENU_MARKETING'] => $marketingURLForProgressBar, //$camp_url.'3',
//        $mod_strings['LBL_NAVIGATION_MENU_SEND_EMAIL_AND_SUMMARY'] => $summaryURLForProgressBar,
//        //$mod_strings['LBL_NAVIGATION_MENU_SUMMARY'] => false,
//    )
//    , true);


if (isset($_REQUEST['redirectToTargetList']) && $_REQUEST['redirectToTargetList']) {
    $ss->assign('hideScreen', true);
    $dotListWizardMenu .= <<<JS
<script type="text/javascript">
$(function(){
    document.location.href = $('#nav_step2 a').first().attr('href');
});
</script>
JS;
}

$ss->assign('WIZMENU', $dotListWizardMenu);

$diagnose = diagnose($errors, $links);

$ss->assign('diagnose', $diagnose);

// validate sender details
if ($mrkt_focus->id) {
    foreach ($marketingErrorResults = $mrkt_focus->validate() as $errorKey => $errorMsg) {
        $errors['marketing'] = $mod_strings['LBL_ERROR_ON_MARKETING'];
        $errors['marketing_' . $errorKey] = $errorMsg;
    }
}

foreach ($errors as $error => $msg) {
    if ($msg) {
        $ss->assign('error_on_' . $error, $msg);
    }
}


foreach ($links as $link => $url) {
    if ($url) {
        $ss->assign('link_to_' . $link, $url);
    }
}

$ss->assign('link_to_campaign_header', $camp_url.'1');

if ($campaign_focus->campaign_type == 'Telesales') {
    $stepValues = array_values($steps);
    $ss->assign('link_to_target_list', $stepValues[2]);
} else {
    $ss->assign('link_to_target_list', $camp_url.'2');
}

$ss->assign('link_to_choose_template', 'index.php?return_module=Campaigns&module=Campaigns&action=WizardMarketing&campaign_id=' . $campaign_focus->id);
$ss->assign('link_to_sender_details', 'index.php?return_module=Campaigns&module=Campaigns&action=WizardMarketing&campaign_id=' . $campaign_focus->id . '&jump=2');


// ---------------------------------
// ------------ EDITOR -------------
// ---------------------------------


require_once 'include/SuiteEditor/SuiteEditorConnector.php';
$templateWidth = 600;
$ss->assign('template_width', $templateWidth);
$ss->assign('BODY_EDITOR', SuiteEditorConnector::getHtml(SuiteEditorConnector::getSuiteSettings(isset($focus->body_html) ? html_entity_decode((string) $focus->body_html) : '', $templateWidth)));
$ss->assign('hide_width_set', $current_user->getEditorType() != 'mozaik');

// ---------------------------------
// ---------------------------------
// ---------------------------------



if (!empty($_SESSION['campaignWizard'][$campaign_focus->id]['defaultSelectedMarketingId'])) {
    $ss->assign('EmailMarketingId', $_SESSION['campaignWizard'][$campaign_focus->id]['defaultSelectedMarketingId']);
} else {
    if (isset($mrkt_lists[0])) {
        $_SESSION['campaignWizard'][$campaign_focus->id]['defaultSelectedMarketingId'] = $mrkt_lists[0];
        $ss->assign('EmailMarketingId', $mrkt_lists[0]);
    }
}



//if campaign_id is passed then we assume this is being invoked from the campaign module and in a popup.
$has_campaign = true;
$inboundEmail = true;
if (!isset($_REQUEST['campaign_id']) || empty($_REQUEST['campaign_id'])) {
    $has_campaign = false;
}
if (!isset($_REQUEST['inboundEmail']) || empty($_REQUEST['inboundEmail'])) {
    $inboundEmail = false;
}

// todo : its for testing, remove this!
//$has_campaign = false;

include_once 'modules/EmailTemplates/templateFields.php';
$ss->assign("FIELD_DEFS_JS", generateFieldDefsJS2());

///////////////////////////////////////
////	CAMPAIGNS
if ($has_campaign || $inboundEmail) {
    //$ss->assign("INPOPUPWINDOW", 'true');
    $ss->assign("INSERT_URL_ONCLICK", "insert_variable_html_link(document.wizform.tracker_url.value)");

    $get_campaign_urls = function ($campaign_id) {
        $return_array=array();

        if (!empty($campaign_id)) {
            $db = DBManagerFactory::getInstance();

            $campaign_id = $db->quote($campaign_id);

            $query1="select * from campaign_trkrs where campaign_id='$campaign_id' and deleted=0";
            $current=$db->query($query1);
            while (($row=$db->fetchByAssoc($current)) != null) {
                $return_array['{'.$row['tracker_name'].'}'] = array(
                    'text' => $row['tracker_name'] . ' : ' . $row['tracker_url'],
                    'url' => $row['tracker_url'],
                    'id' => $row['id']
                );
            }
        }
        return $return_array;
    };
    if ($has_campaign) {
        $campaign_urls = $get_campaign_urls($_REQUEST['campaign_id']);
    }
    if (!empty($campaign_urls)) {
        $ss->assign("DEFAULT_URL_TEXT", key($campaign_urls));
    }
    if ($has_campaign) {
        $get_tracker_options = function ($label_list, $key_list, $selected_key, $massupdate = false) {
            global $app_strings;
            $select_options = '';

            //for setting null selection values to human readable --None--
            $pattern = "/'0?'></";
            $replacement = "''>".$app_strings['LBL_NONE'].'<';
            if ($massupdate) {
                $replacement .= "/OPTION>\n<OPTION value='__SugarMassUpdateClearField__'><"; // Giving the user the option to unset a drop down list. I.e. none means that it won't get updated
            }

            if (empty($key_list)) {
                $key_list = array();
            }
            //create the type dropdown domain and set the selected value if $opp value already exists
            foreach ($key_list as $option_key => $option_value) {
                $select_options .= '<OPTION value="'.$option_key.'" data-id="'.$label_list[$option_key]['id'].'" data-url="'.$label_list[$option_key]['url'].'">'.$label_list[$option_key]['text'].'</OPTION>';
            }
            $select_options = preg_replace($pattern, $replacement, $select_options);

            return $select_options;
        };

        $ss->assign("TRACKER_KEY_OPTIONS", $get_tracker_options($campaign_urls, $campaign_urls, null));
        //$ss->parse("main.NoInbound.tracker_url");

        // create tracker URL fields
        $campaignTracker = BeanFactory::newBean('CampaignTrackers');
        if (isset($_REQUEST['campaign_tracker_id']) && $_REQUEST['campaign_tracker_id']) {
            $campaignTracker->retrieve((int) $_REQUEST['campaign_tracker_id']);
        }
        // todo: hide tracker select if it has no trackers
        $ss->assign("TRACKER_NAME", isset($focus) ? $focus->tracker_name : null);
        $ss->assign("TRACKER_URL", isset($focus) ? $focus->tracker_url : null);
        if (!empty($focus->is_optout) && $focus->is_optout == 1) {
            $ss->assign("IS_OPTOUT_CHECKED", "checked");
            $ss->assign("TRACKER_URL_DISABLED", "disabled");
        }
    }
}
// create option of "Contact/Lead/Task" from corresponding module
// translations
$lblContactAndOthers = implode('/', array(
    isset($app_list_strings['moduleListSingular']['Contacts']) ? $app_list_strings['moduleListSingular']['Contacts'] : 'Contact',
    isset($app_list_strings['moduleListSingular']['Leads']) ? $app_list_strings['moduleListSingular']['Leads'] : 'Lead',
    isset($app_list_strings['moduleListSingular']['Prospects']) ? $app_list_strings['moduleListSingular']['Prospects'] : 'Target',
));

// The insert variable drodown should be conditionally displayed.
// If it's campaign then hide the Account.
if ($has_campaign) {
    $dropdown = "<option value='Contacts'>
						" . $lblContactAndOthers . "
			       </option>";
    if ($campaign_focus->campaign_type == 'Survey') {
        $dropdown .= "<option value='Surveys'>".translate('Surveys')."</option>";
    }
    $ss->assign("DROPDOWN", $dropdown);
    $ss->assign("DEFAULT_MODULE", 'Contacts');
//$xtpl->assign("CAMPAIGN_POPUP_JS", '<script type="text/javascript" src="include/javascript/sugar_3.js"></script>');
} else {
    $ss->assign("DROPDOWN", genDropDownJS2());
    $ss->assign("DEFAULT_MODULE", 'Accounts');
}

$ss->assign("INSERT_VARIABLE_ONCLICK", "insert_variable(document.wizform.variable_text.value, \"email_template_editor\")");


///////////////////////////////////////
////    ATTACHMENTS
$attachments = '';
if (!empty($mrkt_focus->id)) {
    $etid = $mrkt_focus->id;
} elseif (!empty($old_id)) {
    $ss->assign('OLD_ID', $old_id);
    $etid = $old_id;
}
if (!empty($etid)) {
    $note = BeanFactory::newBean('Notes');
    $where = "notes.parent_id='{$etid}' AND notes.filename IS NOT NULL";
    $notes_list = $note->get_full_list("", $where, true);

    if (!isset($notes_list)) {
        $notes_list = array();
    }
    $notes_listCount = is_countable($notes_list) ? count($notes_list): 0;
    for ($i = 0; $i < $notes_listCount; $i++) {
        $the_note = $notes_list[$i];
        if (empty($the_note->filename)) {
            continue;
        }
        $secureLink = 'index.php?entryPoint=download&id=' . $the_note->id . '&type=Notes';
        $attachments .= '<input type="checkbox" name="remove_attachment[]" value="' . $the_note->id . '"> ' . $app_strings['LNK_REMOVE'] . '&nbsp;&nbsp;';
        $attachments .= '<a href="' . $secureLink . '" target="_blank">' . $the_note->filename . '</a><br>';
    }
}
$attJs = '<script type="text/javascript">';
$attJs .= 'var lnk_remove = "' . $app_strings['LNK_REMOVE'] . '";';
$attJs .= '</script>';
$ss->assign('ATTACHMENTS', $attachments);
$ss->assign('ATTACHMENTS_JAVASCRIPT', $attJs);

////    END ATTACHMENTS
///////////////////////////////////////

$ss->assign('campaign_type', isset($_REQUEST['campaign_type']) && $_REQUEST['campaign_type'] ? $_REQUEST['campaign_type'] : $campaign_focus->campaign_type);


$ss->assign('fields', array(
    'date_start' => array(
        'name' => 'date_start',
        'value' => $mrkt_focus->date_start . ' ' . $mrkt_focus->time_start,
    )
));

if (isset($_SESSION['msg']) && $_SESSION['msg']) {
    $ss->assign('msg', $mod_strings[$_SESSION['msg']]);
    unset($_SESSION['msg']);
}

if (!empty($_REQUEST['func'])) {
    echo '<input type="hidden" id="func" value="'.$_REQUEST['func'].'">';
}
      $ss->display('modules/Campaigns/WizardMarketing.html');
