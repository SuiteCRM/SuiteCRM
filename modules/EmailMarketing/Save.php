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
if (!empty($_POST['meridiem'])) {
    $time_start = isset($_POST['time_start']) ? $_POST['time_start'] : $_POST['wiz_step3_time_start'];
    $_POST['time_start'] = $timedate->merge_time_meridiem($time_start, $timedate->get_time_format(), $_POST['meridiem']);
}

if (empty($_REQUEST['time_start'])) {
    if (!empty($_REQUEST['date_start'])) {
        $_REQUEST['date_start'] = $_REQUEST['date_start'];// . ' 00:00';
        $_POST['date_start'] = $_POST['date_start'];// . ' 00:00';
    }
} else {
    if (!empty($_REQUEST['date_start'])) {
        $_REQUEST['date_start'] = $_REQUEST['date_start'] . ' ' . $_REQUEST['time_start'];
        $_POST['date_start'] = $_POST['date_start'] . ' ' . $_POST['time_start'];
    }
}

$marketing = BeanFactory::newBean('EmailMarketing');
if (isset($_POST['record']) && !empty($_POST['record'])) {
    $marketing->retrieve($_POST['record']);
}
if (!$marketing->ACLAccess('Save')) {
    ACLController::displayNoAccess(true);
    sugar_cleanup(true);
}

if (!empty($_POST['assigned_user_id']) && ($marketing->assigned_user_id != $_POST['assigned_user_id']) && ($_POST['assigned_user_id'] != $current_user->id)) {
    $check_notify = true;
} else {
    $check_notify = false;
}
foreach ($marketing->column_fields as $field) {
    if ($field == 'all_prospect_lists') {
        if (isset($_POST[$field]) && $_POST[$field]=='on') {
            $marketing->$field = 1;
        } else {
            $marketing->$field = 0;
        }
    } else {
        if (isset($_POST[$field])) {
            $value = $_POST[$field];
            $marketing->$field = $value;
        }
    }
}

foreach ($marketing->additional_column_fields as $field) {
    if (isset($_POST[$field])) {
        $value = $_POST[$field];
        $marketing->$field = $value;
    }
}

$marketing->campaign_id = $_REQUEST['campaign_id'];

if (isset($_REQUEST['func']) && $_REQUEST['func'] == 'wizardUpdate') {
    foreach ($_POST as $key => $value) {
        if (preg_match('/^wiz_step3_(.*)$/', $key, $match)) {
            $field = $match[1];
            $marketing->$field = $value;
            if ($field=='time_start') {
                $marketing->date_start .= ' ' . $value . (isset($_REQUEST['meridiem']) ? $_REQUEST['meridiem'] : '');
            }
        }
    }
}

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
    if (is_array($_REQUEST['message_for'])) {
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
if ($_REQUEST['action'] != 'WizardMarketingSave' && (!isset($_REQUEST['func']) || $_REQUEST['func'] != 'wizardUpdate')) {
    $header_URL = "Location: index.php?action=DetailView&module=Campaigns&record={$_REQUEST['campaign_id']}";
    $GLOBALS['log']->debug("about to post header URL of: $header_URL");
    header($header_URL);
}

if (isset($_REQUEST['func']) && $_REQUEST['func'] == 'wizardUpdate') {
    $resp = array();
    $resp['error'] = false;
    $resp['data'] = json_encode(array('id' => $marketing->id));
    $resp = json_encode($resp);
    echo $resp;
}
