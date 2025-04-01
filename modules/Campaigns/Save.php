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




$focus = BeanFactory::newBean('Campaigns');

$focus->retrieve($_POST['record']);
if (!$focus->ACLAccess('Save')) {
    ACLController::displayNoAccess(true);
    sugar_cleanup(true);
}
if (!empty($_POST['assigned_user_id']) && ($focus->assigned_user_id != $_POST['assigned_user_id']) && ($_POST['assigned_user_id'] != $current_user->id)) {
    $check_notify = true;
} else {
    $check_notify = false;
}

require_once('include/formbase.php');
$focus = populateFromPost('', $focus);

//store preformatted dates for 2nd save
$preformat_start_date = $focus->start_date;
$preformat_end_date = $focus->end_date;

$focus->save($check_notify);
$return_id = $focus->id;

$GLOBALS['log']->debug("Saved record with id of ".$return_id);

//copy compaign targets on duplicate
if (!empty($_REQUEST['duplicateSave']) &&  !empty($_REQUEST['duplicateId'])) {
    $copyFromCompaign = BeanFactory::newBean('Campaigns');
    $copyFromCompaign->retrieve($_REQUEST['duplicateId']);
    $copyFromCompaign->load_relationship('prospectlists');

    $focus->load_relationship('prospectlists');
    $target_lists = $copyFromCompaign->prospectlists->get();
    if ((is_countable($target_lists) ? count($target_lists) : 0)>0) {
        foreach ($target_lists as $prospect_list_id) {
            $focus->prospectlists->add($prospect_list_id);
        }
    }

    $focus->save();
}


//if type is set to newsletter then make sure there are prospect lists attached
if ($focus->campaign_type =='NewsLetter') {
    //if this is a duplicate, and the "relate_to" and "relate_id" elements are not cleared out,
    //then prospect lists will get related to the original campaign on save of the prospect list, and then
    //will get related to the new newsletter campaign, meaning the same (un)subscription list will belong to
    //two campaigns, which is wrong
    if ((isset($_REQUEST['duplicateSave']) && $_REQUEST['duplicateSave']) || (isset($_REQUEST['isDuplicate']) && $_REQUEST['isDuplicate'])) {
        $_REQUEST['relate_to'] = '';
        $_REQUEST['relate_id'] = '';
    }

    //add preformatted dates for 2nd save, to avoid formatting conversion errors
    $focus->start_date = $preformat_start_date ;
    $focus->end_date = $preformat_end_date ;

    $focus->load_relationship('prospectlists');
    $target_lists = $focus->prospectlists->get();
    if ((is_countable($target_lists) ? count($target_lists) : 0)<1) {
        global $current_user;
        global $mod_strings;
        //if no prospect lists are attached, then lets create a subscription and unsubscription
        //default prospect lists as these are required for newsletters.

        //create subscription list
        $subs = BeanFactory::newBean('ProspectLists');
        $subs->name = $focus->name.' '.$mod_strings['LBL_SUBSCRIPTION_LIST'];
        $subs->assigned_user_id= $current_user->id;
        $subs->list_type = "default";
        $subs->save();
        $focus->prospectlists->add($subs->id);

        //create unsubscription list
        $unsubs = BeanFactory::newBean('ProspectLists');
        $unsubs->name = $focus->name.' '.$mod_strings['LBL_UNSUBSCRIPTION_LIST'];
        $unsubs->assigned_user_id= $current_user->id;
        $unsubs->list_type = "exempt";
        $unsubs->save();
        $focus->prospectlists->add($unsubs->id);

        //create unsubscription list
        $test_subs = BeanFactory::newBean('ProspectLists');
        $test_subs->name = $focus->name.' '.$mod_strings['LBL_TEST_LIST'];
        $test_subs->assigned_user_id= $current_user->id;
        $test_subs->list_type = "test";
        $test_subs->save();
        $focus->prospectlists->add($test_subs->id);
    }
    //save new relationships
    $focus->save();
}//finish newsletter processing

handleRedirect($focus->id, 'Campaigns');
