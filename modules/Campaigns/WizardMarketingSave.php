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




global $timedate;
global $current_user;

$master = 'save';
if (isset($_REQUEST['wiz_home_next_step']) && !empty($_REQUEST['wiz_home_next_step'])) {
    if ($_REQUEST['wiz_home_next_step']==3) {
        //user has chosen to save and schedule this campaign for email
        $master = 'send';
    } elseif ($_REQUEST['wiz_home_next_step']==2) {
        //user has chosen to save and send this campaign in test mode
        $master = 'test';
    } else {
        //user has chosen to simply save
        $master  = 'save';
    }
} else {
    //default to just saving and exiting wizard
    $master = 'save';
}




$prefix = 'wiz_step3_';
$marketing = BeanFactory::newBean('EmailMarketing');
if (isset($_REQUEST['record']) && !empty($_REQUEST['record'])) {
    $marketing->retrieve($_REQUEST['record']);
} else {
    if (!empty($_SESSION['campaignWizard'][$_REQUEST['campaign_id']]['defaultSelectedMarketingId'])) {
        $marketing->retrieve($_SESSION['campaignWizard'][$_REQUEST['campaign_id']]['defaultSelectedMarketingId']);
    }
}
if (!$marketing->ACLAccess('Save')) {
    ACLController::displayNoAccess(true);
    sugar_cleanup(true);
}

if (!empty($_REQUEST['assigned_user_id']) && ($marketing->assigned_user_id != $_REQUEST['assigned_user_id']) && ($_POST['assigned_user_id'] != $current_user->id)) {
    $check_notify = true;
} else {
    $check_notify = false;
}

    foreach ($_REQUEST as $key => $val) {
        if ((strstr($key, $prefix)) && (strpos($key, $prefix)== 0)) {
            $newkey  =substr($key, strlen($prefix)) ;
            $_REQUEST[$newkey] = $val;
        }
    }

    foreach ($_REQUEST as $key => $val) {
        if ((strstr($key, $prefix)) && (strpos($key, $prefix)== 0)) {
            $newkey  =substr($key, strlen($prefix)) ;
            $_REQUEST[$newkey] = $val;
        }
    }

if (!empty($_REQUEST['meridiem'])) {
    $_REQUEST['time_start'] = $timedate->merge_time_meridiem($_REQUEST['time_start'], $timedate->get_time_format(), $_REQUEST['meridiem']);
}

if (empty($_REQUEST['time_start'])) {
    $_REQUEST['date_start'] = $_REQUEST['date_start'] . ' 00:00';
} else {
    $_REQUEST['date_start'] = $_REQUEST['date_start'] . ' ' . $_REQUEST['time_start'];
}

foreach ($marketing->column_fields as $field) {
    if ($field == 'all_prospect_lists') {
        if (isset($_REQUEST[$field]) && $_REQUEST[$field]='on') {
            $marketing->$field = 1;
        } else {
            $marketing->$field = 0;
        }
    } else {
        if (isset($_REQUEST[$field])) {
            $value = $_REQUEST[$field];
            $marketing->$field = trim($value);
        }
    }
}

foreach ($marketing->additional_column_fields as $field) {
    if (isset($_REQUEST[$field])) {
        $value = $_REQUEST[$field];
        $marketing->$field = $value;
    }
}

$marketing->campaign_id = $_REQUEST['campaign_id'];
$marketing->save($check_notify);

//add prospect lists to campaign.
$marketing->load_relationship('prospectlists');
$prospectlists=$marketing->prospectlists->get();
if ($marketing->all_prospect_lists==1) {
    //remove all related prospect lists.
    if (!empty($prospectlists)) {
        $marketing->prospectlists->delete($marketing->id);
    }
} else {
    if (isset($_REQUEST['message_for']) && is_array($_REQUEST['message_for'])) {
        foreach ($_REQUEST['message_for'] as $prospect_list_id) {
            $key=array_search($prospect_list_id, $prospectlists, true);
            if ($key === null || $key === false) {
                $marketing->prospectlists->add($prospect_list_id);
            } else {
                unset($prospectlists[$key]);
            }
        }
        if ((is_countable($prospectlists) ? count($prospectlists) : 0) != 0) {
            foreach ($prospectlists as $key=>$list_id) {
                $marketing->prospectlists->delete($marketing->id, $list_id);
            }
        }
    }
}

//populate an array with marketing email id to use
$mass[] = $marketing->id;
//if sending an email was chosen, set all the needed variables for queuing campaign

if ($master !='save') {
    $_REQUEST['mass']= $mass;
    $_POST['mass']=$mass;
    $_REQUEST['record'] =$marketing->campaign_id;
    $_POST['record']=$marketing->campaign_id;
    $_REQUEST['mode'] = $master;
    $_POST['mode'] = $master;
    $_REQUEST['from_wiz']= 'true';
    require_once('modules/Campaigns/QueueCampaign.php');
}

if (isset($_REQUEST['show_wizard_summary']) && $_REQUEST['show_wizard_summary']) {
    if ((isset($_REQUEST['sendMarketingEmailTest']) && $_REQUEST['sendMarketingEmailTest']) ||
        (isset($_REQUEST['sendMarketingEmailSchedule']) && $_REQUEST['sendMarketingEmailSchedule'])) {
        // set correct post variables..
        $_POST['module'] = 'Campaigns';
        $_POST['action'] = 'QueueCampaign';
        $_POST['process_form'] = false;
        $_POST['return_module'] = 'Campaigns';
        $_POST['return_id'] = $marketing->campaign_id;
        $_POST['return_action'] = 'WizardHome';
        $_POST['record'] = $marketing->campaign_id;
        $_POST['direct_step'] = '1';
        //$_POST['campaign_id'] = '';
        $_POST['wiz_mass'] = $marketing->id;
        if (isset($_REQUEST['sendMarketingEmailTest']) && $_REQUEST['sendMarketingEmailTest']) {
            $_POST['mode'] = 'test';
        } else {
            if (isset($_REQUEST['sendMarketingEmailSchedule']) && $_REQUEST['sendMarketingEmailSchedule']) {
                $_POST['mode'] = 'send';
                $_SESSION['msg'] = 'LBL_EMAILS_SCHEDULED';
            } else {
                throw new Exception('request error');
            }
        }

        //$_POST['SUBMIT'] = 'Send Test';
        $_REQUEST = array_merge($_REQUEST, $_POST);
        include 'modules/Campaigns/QueueCampaign.php';
        exit;
    }

    $header_URL = "Location: index.php?action=WizardMarketing&module=Campaigns&return_module=Campaigns&return_action=WizardHome&return_id=" . $marketing->campaign_id . "&campaign_id=" . $marketing->campaign_id . "&jump=3&marketing_id=" . $marketing->id;
} else {
    $header_URL = "Location: index.php?action=WizardHome&module=Campaigns&record=".$marketing->campaign_id;
}

$GLOBALS['log']->debug("about to post header URL of: $header_URL");
SugarApplication::headerRedirect($header_URL);
