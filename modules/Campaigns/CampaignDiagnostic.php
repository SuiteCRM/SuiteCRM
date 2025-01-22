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




global $mod_strings;
global $app_list_strings;
global $app_strings;
global $current_user;

//if (!is_admin($current_user)) sugar_die("Unauthorized access to administration.");
//account for use within wizards
if (!isset($_REQUEST['inline']) || $_REQUEST['inline'] != 'inline') {
    $params = array();
    $params[] = "<a href='index.php?module=Campaigns&action=index'>{$mod_strings['LBL_MODULE_NAME']}</a>";
    $params[] = $mod_strings['LBL_CAMPAIGN_DIAGNOSTICS'];

    echo getClassicModuleTitle('Campaigns', $params, true);
}

global $theme;
global $currentModule;




if (isset($_REQUEST['inline']) && $_REQUEST['inline'] == 'inline') {
    {

}
} else {
    //use html if not inline
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
    if (empty($_REQUEST['return_id'])) {
        $ss->assign("RETURN_ACTION", 'index');
    }
}

/************  EMAIL COMPONENTS *************/
//monitored mailbox section
$focus = BeanFactory::newBean('Administration');
$focus->retrieveSettings(); //retrieve all admin settings.


//run query for mail boxes of type 'bounce'
$email_health = 0;
$email_components = 2;
$mbox_qry = "select * from inbound_email where deleted ='0' and mailbox_type = 'bounce'";
$mbox_res = $focus->db->query($mbox_qry);
$mboxTable = "<table border ='0' width='100%'  class='detail view' cellpadding='0' cellspacing='0'>";
//put all rows returned into an array
$mbox = array();
while ($mbox_row = $focus->db->fetchByAssoc($mbox_res)) {
    $mbox[] = $mbox_row;
}
    $mbox_msg = ' ';
//if the array is not empty, then set "good" message
if (isset($mbox) && count($mbox)>0) {
    $mboxTable .= "<tr><td colspan='5' style='text-align: left;'><b>" .count($mbox) ." ". $mod_strings['LBL_MAILBOX_CHECK1_GOOD']." </b>.</td></tr>";
    $mboxTable .= "<tr><th scope='col' width='20%'><b>".$mod_strings['LBL_MAILBOX_NAME']."</b></th>"
                   .  " <th scope='col' width='20%'><b>".$mod_strings['LBL_LOGIN']."</b></th>"
                   .  " <th scope='col' width='20%'><b>".$mod_strings['LBL_MAILBOX']."</b></th>"
                   .  " <th scope='col' width='20%'><b>".$mod_strings['LBL_SERVER_URL']."</b></th>"
                   .  " <th scope='col' width='20%'><b>".$mod_strings['LBL_LIST_STATUS']."</b></th></tr>";

    foreach ($mbox as $details) {
        $mboxTable .= "<tr><td>".$details['name']."</td>";
        $mboxTable .= "<td>".$details['email_user']."</td>";
        $mboxTable .= "<td>".$details['mailbox']."</td>";
        $mboxTable .= "<td>".$details['server_url']."</td>";
        $mboxTable .= "<td>".$details['status']."</td></tr>";
    }
} else {
    //if array is empty, then set "bad" message and increment health counter
    $mboxTable .=  "<tr><td colspan='5'><b class='error'>". $mod_strings['LBL_MAILBOX_CHECK1_BAD']."</b></td></tr>";
    $email_health =$email_health +1;
}

$mboxTable.= '</table>' ;



$ss->assign("MAILBOXES_DETECTED_MESSAGE", $mboxTable);

//email settings configured
$conf_msg="<table border='0' width='100%' class='detail view' cellpadding='0' cellspacing='0'>";
if (strstr((string) $focus->settings['notify_fromaddress'], 'example.com')) {
    //if from address is the default, then set "bad" message and increment health counter
    $conf_msg .= "<tr><td colspan = '5'><b class='error'> ".$mod_strings['LBL_MAILBOX_CHECK2_BAD']." </b></td></td>";
    $email_health =$email_health +1;
} else {
    $conf_msg .= "<tr><td colspan = '5'><b> ".$mod_strings['LBL_MAILBOX_CHECK2_GOOD']."</b></td></tr>";
    $conf_msg .= "<tr><th scope='col' width='20%'><b>".$mod_strings['LBL_WIZ_FROM_NAME']."</b></th>"
               .  " <th scope='col' width='20%'><b>".$mod_strings['LBL_WIZ_FROM_ADDRESS']."</b></th>"
               .  " <th scope='col' width='20%'><b>".$mod_strings['LBL_MAIL_SENDTYPE']."</b></th>";
    if (strtolower(isSmtp($focus->settings['mail_sendtype'] ?? ''))) {
        $conf_msg .= " <th scope='col' width='20%'><b>".$mod_strings['LBL_MAIL_SMTPSERVER']."</b></th>"
               .  " <th scope='col' width='20%'><b>".$mod_strings['LBL_MAIL_SMTPUSER']."</b></th></tr>";
    } else {
        $conf_msg .= "</tr>";
    }



    $conf_msg .= "<tr><td>".$focus->settings['notify_fromname']."</td>";
    $conf_msg .= "<td>".$focus->settings['notify_fromaddress']."</td>";
    $conf_msg .= "<td>".$focus->settings['mail_sendtype']."</td>";
    if (isSmtp($focus->settings['mail_sendtype'] ?? '')) {
        $conf_msg .= "<td>".$focus->settings['mail_smtpserver']."</td>";
        $conf_msg .= "<td>".$focus->settings['mail_smtpuser']."</td></tr>";
    } else {
        $conf_msg .= "</tr>";
    }
}

$conf_msg .= '</table>';
$ss->assign("EMAIL_SETTINGS_CONFIGURED_MESSAGE", $conf_msg);
$email_setup_wiz_link='';
if ($email_health>0) {
    if (is_admin($current_user)) {
        $email_setup_wiz_link="<a href='index.php?module=Campaigns&action=WizardEmailSetup'>".$mod_strings['LBL_EMAIL_SETUP_WIZ']."</a>";
    } else {
        $email_setup_wiz_link=$mod_strings['LBL_NON_ADMIN_ERROR_MSG'];
    }
}

$ss->assign("EMAIL_SETUP_WIZ_LINK", $email_setup_wiz_link);
$ss->assign('EMAIL_IMAGE', define_image($email_health, 2));
$ss->assign('EMAIL_COMPONENTS', $mod_strings['LBL_EMAIL_COMPONENTS']);
$ss->assign('SCHEDULER_COMPONENTS', $mod_strings['LBL_SCHEDULER_COMPONENTS']);
$ss->assign('RECHECK_BTN', $mod_strings['LBL_RECHECK_BTN']);

/************* SCHEDULER COMPONENTS ************/

//create and run the scheduler queries
$sched_qry = "select job, name, status from schedulers where deleted = 0 and status = 'Active'";
$sched_res = $focus->db->query($sched_qry);
$sched_health = 0;
$sched = array();
$check_sched1 = 'function::runMassEmailCampaign';
$check_sched2 = 'function::pollMonitoredInboxesForBouncedCampaignEmails';
$sched_mes = '';
$sched_mes_body = '';

$scheds = array();
//build the table rows for scheduler display
while ($sched_row = $focus->db->fetchByAssoc($sched_res)) {
    $scheds[] = $sched_row;
}
foreach ($scheds as $funct) {
    if (($funct['job']==$check_sched1)  ||   ($funct['job']==$check_sched2)) {
        $sched_mes = 'use';
        $sched_mes_body .= "<tr><td style='text-align: left;'>".$funct['name']."</td>";
        $sched_mes_body .= "<td style='text-align: left;'>".$funct['status']."</td></tr>";
        if ($funct['job']==$check_sched1) {
            $check_sched1 ="found";
        } else {
            $check_sched2 ="found";
        }
    }
}

//determine which table header to use, based on whether or not schedulers were found
$show_admin_link = false;
if ($sched_mes == 'use') {
    $sched_mes = "<h5>".$mod_strings['LBL_SCHEDULER_CHECK_GOOD']."</h5><br><table class='other view' cellspacing='1'>";
    $sched_mes .= "<tr><th scope='col' width='40%'><b>".$mod_strings['LBL_SCHEDULER_NAME']."</b></tH>"
               .  " <th scope='col' width='60%'><b>".$mod_strings['LBL_SCHEDULER_STATUS']."</b></tH></tr>";
} else {
    $sched_mes = "<table class='other view' cellspacing='1'>";
    $sched_mes  .= "<tr><td colspan ='3'><font color='red'><b> ".$mod_strings['LBL_SCHEDULER_CHECK_BAD']."</b></font></td></tr>";
    $show_admin_link = true;
}

//determine if error messages need to be displayed for schedulers
if ($check_sched2 != 'found') {
    $sched_health =$sched_health +1;
    $sched_mes_body  .= "<tr><td colspan ='3'><font color='red'> ".$mod_strings['LBL_SCHEDULER_CHECK1_BAD']."</font></td></tr>";
}
if ($check_sched1 != 'found') {
    $sched_health =$sched_health +1;
    $sched_mes_body  .= "<tr><td colspan ='3' scope='row'><font color='red'>".$mod_strings['LBL_SCHEDULER_CHECK2_BAD']."</font></td></tr>";
}
$admin_sched_link='';
if ($sched_health>0) {
    if (is_admin($current_user)) {
        $admin_sched_link="<a href='index.php?module=Schedulers&action=index'>".$mod_strings['LBL_SCHEDULER_LINK']."</a>";
    } else {
        $admin_sched_link=$mod_strings['LBL_NON_ADMIN_ERROR_MSG'];
    }
}

//put table html together and display
    $final_sched_msg = $sched_mes . $sched_mes_body . '</table>' . $admin_sched_link;
    $ss->assign("SCHEDULER_EMAILS_MESSAGE", $final_sched_msg);
    $ss->assign('SCHEDULE_IMAGE', define_image($sched_health, 2));


/********** FINAL END OF PAGE UI Stuff ********/
if (!isset($_REQUEST['inline']) || $_REQUEST['inline'] != 'inline') {
    $ss->display('modules/Campaigns/CampaignDiagnostic.html');
}

/**
 * This function takes in 3 parameters and determines the appropriate image source.
 *
 * @param  int $num parameter is the "health" parameter being tracked whenever there is something wrong.  (higher number =bad)
 * @param  int $total Parameter is the total number things being checked.
 * @return string HTML img tag
 */
function define_image($num, $total)
{
    global $mod_strings;
    //if health number is equal to total number then all checks failed, set red image
    if ($num == $total) {
        //red
        return SugarThemeRegistry::current()->getImage('red_camp', "align='absmiddle'", null, null, ".gif", $mod_strings['LBL_INVALID']);
    } elseif ($num == 0) {
        //if health number is zero, then all checks passed, set green image
        //green
        return SugarThemeRegistry::current()->getImage('green_camp', "align='absmiddle'", null, null, ".gif", $mod_strings['LBL_VALID']);
    } else {
        //if health number is between total and num params, then some checks failed but not all, set yellow image
        //yellow
        return SugarThemeRegistry::current()->getImage('yellow_camp', "align='absmiddle'", null, null, ".gif", $mod_strings['LBL_ALERT']);
    }
}
