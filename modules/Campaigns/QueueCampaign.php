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

 * Description: Schedules email for delivery. emailman table holds emails for delivery.
 * A cron job polls the emailman table and delivers emails when intended send date time is reached.
 * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
 * All Rights Reserved.
 * Contributor(s): ______________________________________..
 ********************************************************************************/





global $timedate;
global $current_user;
global $mod_strings;

$campaign = BeanFactory::newBean('Campaigns');
$campaign->retrieve($_REQUEST['record']);
$err_messages=array();

$test=false;
if (isset($_REQUEST['mode']) && $_REQUEST['mode'] =='test') {
    $test=true;
}

//this is to account for the case of sending directly from summary page in wizards
$from_wiz =false;
if (isset($_REQUEST['wiz_mass'])) {
    $mass[] = $_REQUEST['wiz_mass'];
    $_POST['mass'] = $mass;
    $from_wiz =true;
}
if (isset($_REQUEST['from_wiz'])) {
    $from_wiz =true;
}

//if campaign status is 'sending' disallow this step.
if (!empty($campaign->status) && $campaign->status == 'sending') {
    $err_messages[]=$mod_strings['ERR_SENDING_NOW'];
}
$current_date = $campaign->db->now();

//start scheduling now.....
foreach ($_POST['mass'] as $message_id) {

    //fetch email marketing definition.
    if (!class_exists('EmailMarketing')) {
        require_once('modules/EmailMarketing/EmailMarketing.php');
    }


    $marketing = BeanFactory::newBean('EmailMarketing');
    $marketing->retrieve($message_id);

    //make sure that the marketing message has a mailbox.
    //
    if (empty($marketing->inbound_email_id)) {
        echo "<p>";
        echo "<h4 class=\"header-4\">{$mod_strings['ERR_NO_MAILBOX']}</h4>";
        echo "<BR><a href='index.php?module=EmailMarketing&action=EditView&record={$marketing->id}'>$marketing->name</a>";
        echo "</p>";
        sugar_die('');
    }


    global $timedate;
    $mergedvalue=$timedate->merge_date_time($marketing->date_start, $marketing->time_start);
    if ($test) {
        $send_date_time = $timedate->getNow()->get("-60 seconds")->asDb();
    } else {
        $send_date_time = $timedate->to_db($mergedvalue);
    }
    $send_date_time = $campaign->db->convert($campaign->db->quoted($send_date_time), "datetime");

    //find all prospect lists associated with this email marketing message.
    if ($marketing->all_prospect_lists == 1) {
        $query="SELECT prospect_lists.id prospect_list_id from prospect_lists ";
        $query.=" INNER JOIN prospect_list_campaigns plc ON plc.prospect_list_id = prospect_lists.id";
        $query.=" WHERE plc.campaign_id='{$campaign->id}'";
        $query.=" AND prospect_lists.deleted=0";
        $query.=" AND plc.deleted=0";
        if ($test) {
            $query.=" AND prospect_lists.list_type='test'";
        } else {
            $query.=" AND prospect_lists.list_type!='test' AND prospect_lists.list_type not like 'exempt%'";
        }
    } else {
        $query="select email_marketing_prospect_lists.* FROM email_marketing_prospect_lists ";
        $query.=" inner join prospect_lists on prospect_lists.id = email_marketing_prospect_lists.prospect_list_id";
        $query.=" WHERE prospect_lists.deleted=0 and email_marketing_id = '$message_id' and email_marketing_prospect_lists.deleted=0";

        if ($test) {
            $query.=" AND prospect_lists.list_type='test'";
        } else {
            $query.=" AND prospect_lists.list_type!='test' AND prospect_lists.list_type not like 'exempt%'";
        }
    }
    $result=$campaign->db->query($query);
    while (($row=$campaign->db->fetchByAssoc($result))!=null) {
        $prospect_list_id=$row['prospect_list_id'];

        //delete all messages for the current campaign and current email marketing message.
        $delete_emailman_query="delete from emailman where campaign_id='{$campaign->id}' and marketing_id='{$message_id}' and list_id='{$prospect_list_id}'";
        $campaign->db->query($delete_emailman_query);
        $auto = $campaign->db->getAutoIncrementSQL("emailman", "id");

        $insert_query= "INSERT INTO emailman (date_entered, user_id, campaign_id, marketing_id,list_id, related_id, related_type, send_date_time";
        $insert_query.= empty($auto)?"":",id";
        $insert_query.=')';
        $insert_query.= " SELECT $current_date,'{$current_user->id}',plc.campaign_id,'{$message_id}',plp.prospect_list_id, plp.related_id, plp.related_type,{$send_date_time}";
        $insert_query.= empty($auto)?"":",$auto";
        $insert_query.= " FROM prospect_lists_prospects plp ";
        $insert_query.= "INNER JOIN prospect_list_campaigns plc ON plc.prospect_list_id = plp.prospect_list_id ";
        $insert_query.= "WHERE plp.prospect_list_id = '{$prospect_list_id}' ";
        $insert_query.= "AND plp.deleted=0 ";
        $insert_query.= "AND plc.deleted=0 ";
        $insert_query.= "AND plc.campaign_id='{$campaign->id}'";

        $campaign->db->query($insert_query);
    }
}

//delete all entries from the emailman table that belong to the exempt list.
//TODO:SM: may want to move this to query clause above instead
if (!$test) {
    $delete_query =  "
    DELETE FROM emailman WHERE id IN (
        SELECT em.id FROM (
            SELECT emailman.id id
            FROM emailman 
            INNER JOIN prospect_lists_prospects plp
            ON emailman.related_id =  plp.related_id AND emailman.related_type =  plp.related_type
            INNER JOIN prospect_lists pl 
            ON pl.id = plp.prospect_list_id 
            INNER JOIN prospect_list_campaigns plc 
            ON plp.prospect_list_id = plc.prospect_list_id 
            WHERE plp.deleted = 0 AND plc.deleted = 0
            AND pl.deleted = 0 AND pl.list_type = 'exempt'
            AND plc.campaign_id = '{$campaign->id}'
            AND emailman.campaign_id = '{$campaign->id}') em    
    )";
    $campaign->db->query($delete_query);
}

$return_module=isset($_REQUEST['return_module'])?$_REQUEST['return_module']:'Campaigns';
$return_action=isset($_REQUEST['return_action'])?$_REQUEST['return_action']:'DetailView';
$return_id=$_REQUEST['record'];

if ($test) {
    //navigate to EmailManDelivery..
    $header_URL = "Location: index.php?action=EmailManDelivery&module=EmailMan&campaign_id={$_REQUEST['record']}&return_module={$return_module}&return_action={$return_action}&return_id={$return_id}&mode=test";
    if ($from_wiz) {
        $header_URL .= "&from_wiz=true";
    }
} else {
    //navigate back to campaign detail view...
    $header_URL = "Location: index.php?action={$return_action}&module={$return_module}&record={$return_id}";
    if ($from_wiz) {
        $header_URL .= "&from=send";
    }
}
if ($action=='WizardMarketingSave') {
    $header_URL .= '&WizardMarketingSave=1&marketing_id=' . (isset($_POST['marketing_id']) && $_POST['marketing_id'] ?
            $_POST['marketing_id'] : $_REQUEST['wiz_mass']);
}
$GLOBALS['log']->debug("about to post header URL of: $header_URL");

if (preg_match('/\s*Location:\s*(.*)$/', $header_URL, $matches)) {
    $href = $matches[1];
    SugarApplication::redirect($href);
} else {
    header($header_URL);
}
