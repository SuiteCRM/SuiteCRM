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


require_once('include/formbase.php');

global $mod_strings;

    //create new campaign bean and populate
    $campaign_focus = BeanFactory::newBean('Campaigns');
    if (isset($_REQUEST['record']) && $_REQUEST['record'] && !(isset($_REQUEST['campaign_id']) && $_REQUEST['campaign_id'])) {
        $campaign_focus->retrieve($_REQUEST['record']);
    } else {
        if (isset($_REQUEST['campaign_id']) && $_REQUEST['campaign_id']) {
            $campaign_focus->retrieve($_REQUEST['campaign_id']);
        }
    }

    $camp_steps[] = 'wiz_step1_';
    $camp_steps[] = 'wiz_step2_';

    $campaign_focus = populateFromPost('', $campaign_focus);

    foreach ($camp_steps as $step) {
        $campaign_focus =  populate_wizard_bean_from_request($campaign_focus, $step);
    }

    switch ($_REQUEST['currentstep']) {
        case 1:
            //save here so we can link relationships
            $campaign_focus->save();
            $GLOBALS['log']->debug("Saved record with id of ".$campaign_focus->id);
            echo json_encode(array('record'=>$campaign_focus->id));
            break;
        case 2:
            //process subscription lists if this is a newsletter
            if ($campaign_focus->campaign_type =='NewsLetter') {
                $pl_list = process_subscriptions_from_request($campaign_focus->name);

                $campaign_focus->load_relationship('prospectlists');
                $existing_pls = $campaign_focus->prospectlists->get();
                $ui_ids = array();

                //for each list returned, add the list to the relationship
                foreach ($pl_list as $pl) {
                    $campaign_focus->prospectlists->add($pl->id);
                    //populate array with id's from UI'
                    $ui_ids[] = $pl->id;
                }

                //now remove the lists that may have existed before, but were not specified in UI.
                //this will enforce that Newsletters only have 3 available target lists.
                foreach ($existing_pls as $pl_del) {
                    if (!in_array($pl_del, $ui_ids)) {
                        $campaign_focus->prospectlists->delete($campaign_focus->id, $pl_del);
                    }
                }
            } else {
                //process target lists if this is not a newsletter
                //remove Target Lists if defined

                if (isset($_REQUEST['wiz_remove_target_list'])) {
                    $remove_target_strings = explode(",", $_REQUEST['wiz_remove_target_list']);
                    foreach ($remove_target_strings as $remove_trgt_string) {
                        if (!empty($remove_trgt_string)) {
                            //load relationship and add to the list
                            $campaign_focus->load_relationship('prospectlists');
                            $campaign_focus->prospectlists->delete($campaign_focus->id, $remove_trgt_string);
                        }
                    }
                }


                //create new campaign tracker and save if defined
                if (isset($_REQUEST['wiz_list_of_targets'])) {
                    $target_strings = explode(",", $_REQUEST['wiz_list_of_targets']);
                    foreach ($target_strings as $trgt_string) {
                        $target_values = explode("@@", $trgt_string);
                        if (count($target_values) == 3) {
                            if (!empty($target_values[0])) {
                                //this is a selected target, as the id is already populated, retrieve and link
                                $trgt_focus = BeanFactory::newBean('ProspectLists');
                                $trgt_focus->retrieve($target_values[0]);

                                //load relationship and add to the list
                                $campaign_focus->load_relationship('prospectlists');
                                $campaign_focus->prospectlists->add($trgt_focus->id);
                            } else {

                                //this is a new target, as the id is not populated, need to create and link
                                $trgt_focus = BeanFactory::newBean('ProspectLists');
                                $trgt_focus->name = $target_values[1];
                                $trgt_focus->list_type = $target_values[2];
                                $trgt_focus->save();

                                //load relationship and add to the list
                                $campaign_focus->load_relationship('prospectlists');
                                $campaign_focus->prospectlists->add($trgt_focus->id);
                            }
                        }
                    }
                }
            }



            //remove campaign trackers if defined
            if (isset($_REQUEST['wiz_remove_tracker_list'])) {
                $remove_tracker_strings = explode(",", $_REQUEST['wiz_remove_tracker_list']);
                foreach ($remove_tracker_strings as $remove_trkr_string) {
                    if (!empty($remove_trkr_string)) {
                        //load relationship and add to the list
                        $campaign_focus->load_relationship('tracked_urls');
                        $campaign_focus->tracked_urls->delete($campaign_focus->id, $remove_trkr_string);
                    }
                }
            }


            //save  campaign trackers and save if defined
            if (isset($_REQUEST['wiz_list_of_existing_trackers'])) {
                $tracker_strings = explode(",", $_REQUEST['wiz_list_of_existing_trackers']);
                foreach ($tracker_strings as $trkr_string) {
                    $tracker_values = explode("@@", $trkr_string);
                    $ct_focus = BeanFactory::newBean('CampaignTrackers');
                    $ct_focus->retrieve($tracker_values[0]);
                    if (!empty($ct_focus->tracker_name)) {
                        $ct_focus->tracker_name = $tracker_values[1];
                        $ct_focus->is_optout = $tracker_values[2];
                        $ct_focus->tracker_url = $tracker_values[3];
                        $ct_focus->save();

                        //load relationship and add to the list
                        $campaign_focus->load_relationship('tracked_urls');
                        $campaign_focus->tracked_urls->add($ct_focus->id);
                    }
                }
            }


            //create new campaign tracker and save if defined
            if (isset($_REQUEST['wiz_list_of_trackers'])) {
                $tracker_strings = explode(",", $_REQUEST['wiz_list_of_trackers']);
                foreach ($tracker_strings as $trkr_string) {
                    $tracker_values = explode("@@", $trkr_string);
                    if (count($tracker_values)==3) {
                        $ct_focus = BeanFactory::newBean('CampaignTrackers');
                        $ct_focus->tracker_name = $tracker_values[0];
                        $ct_focus->is_optout = $tracker_values[1];
                        $ct_focus->tracker_url = $tracker_values[2];
                        $ct_focus->save();

                        //load relationship and add to the list
                        $campaign_focus->load_relationship('tracked_urls');
                        $campaign_focus->tracked_urls->add($ct_focus->id);
                        // save campaign_trkrs after populating campaign id
                        $ct_focus->save();
                    }
                }
            }

            //set navigation details
            $_REQUEST['return_id'] = $campaign_focus->id;
            $_REQUEST['return_module'] = $campaign_focus->module_dir;
            $_REQUEST['return_action'] = "WizardNewsLetter";
            $_REQUEST['action'] = "WizardMarketing";
            $_REQUEST['record'] = $campaign_focus->id;

            $action = '';
            $redirectToTargetList = '';
            if (isset($_REQUEST['wiz_direction'])) {
                switch ($_REQUEST['wiz_direction']) {
                    case 'continue':
                    $action = 'WizardMarketing';
                        break;
                    case 'continue_targetList':
                        $action = 'WizardMarketing';
                        $redirectToTargetList = '&redirectToTargetList=1';
                        break;
                     case 'exit':
                         $action = 'WizardMarketing';
                        break;
                }
            } else {
                $action = 'WizardHome&record='.$campaign_focus->id;
            }
            //require_once('modules/Campaigns/WizardMarketing.php');
            $header_URL = "Location: index.php?return_module=Campaigns&module=Campaigns&action=".$action.$redirectToTargetList."&campaign_id=".$campaign_focus->id."&return_action=WizardNewsLetter&return_id=".$campaign_focus->id;
            $GLOBALS['log']->debug("about to post header URL of: $header_URL");
            SugarApplication::headerRedirect($header_URL);
            break;
        case 3:
            //process subscription lists if this is a newsletter
            if ($campaign_focus->campaign_type =='NewsLetter') {
                $pl_list = process_subscriptions_from_request($campaign_focus->name);

                $campaign_focus->load_relationship('prospectlists');
                $existing_pls = $campaign_focus->prospectlists->get();
                $ui_ids = array();

                //for each list returned, add the list to the relationship
                foreach ($pl_list as $pl) {
                    $campaign_focus->prospectlists->add($pl->id);
                    //populate array with id's from UI'
                    $ui_ids[] = $pl->id;
                }

                //now remove the lists that may have existed before, but were not specified in UI.
                //this will enforce that Newsletters only have 3 available target lists.
                foreach ($existing_pls as $pl_del) {
                    if (!in_array($pl_del, $ui_ids)) {
                        $campaign_focus->prospectlists->delete($campaign_focus->id, $pl_del);
                    }
                }
            } else {
                //process target lists if this is not a newsletter
                //remove Target Lists if defined

                if (isset($_REQUEST['wiz_remove_target_list'])) {
                    $remove_target_strings = explode(",", $_REQUEST['wiz_remove_target_list']);
                    foreach ($remove_target_strings as $remove_trgt_string) {
                        if (!empty($remove_trgt_string)) {
                            //load relationship and add to the list
                            $campaign_focus->load_relationship('prospectlists');
                            $campaign_focus->prospectlists->delete($campaign_focus->id, $remove_trgt_string);
                        }
                    }
                }


                //create new campaign tracker and save if defined
                if (isset($_REQUEST['wiz_list_of_targets'])) {
                    $target_strings = explode(",", $_REQUEST['wiz_list_of_targets']);
                    foreach ($target_strings as $trgt_string) {
                        $target_values = explode("@@", $trgt_string);
                        if (count($target_values) == 3) {
                            if (!empty($target_values[0])) {
                                //this is a selected target, as the id is already populated, retrieve and link
                                $trgt_focus = BeanFactory::newBean('ProspectLists');
                                $trgt_focus->retrieve($target_values[0]);

                                //load relationship and add to the list
                                $campaign_focus->load_relationship('prospectlists');
                                $campaign_focus->prospectlists->add($trgt_focus->id);
                            } else {

                                //this is a new target, as the id is not populated, need to create and link
                                $trgt_focus = BeanFactory::newBean('ProspectLists');
                                $trgt_focus->name = $target_values[1];
                                $trgt_focus->list_type = $target_values[2];
                                $trgt_focus->save();

                                //load relationship and add to the list
                                $campaign_focus->load_relationship('prospectlists');
                                $campaign_focus->prospectlists->add($trgt_focus->id);
                            }
                        }
                    }
                }
            }



            //remove campaign trackers if defined
            if (isset($_REQUEST['wiz_remove_tracker_list'])) {
                $remove_tracker_strings = explode(",", $_REQUEST['wiz_remove_tracker_list']);
                foreach ($remove_tracker_strings as $remove_trkr_string) {
                    if (!empty($remove_trkr_string)) {
                        //load relationship and add to the list
                        $campaign_focus->load_relationship('tracked_urls');
                        $campaign_focus->tracked_urls->delete($campaign_focus->id, $remove_trkr_string);
                    }
                }
            }


            //save  campaign trackers and save if defined
            if (isset($_REQUEST['wiz_list_of_existing_trackers'])) {
                $tracker_strings = explode(",", $_REQUEST['wiz_list_of_existing_trackers']);
                foreach ($tracker_strings as $trkr_string) {
                    $tracker_values = explode("@@", $trkr_string);
                    $ct_focus = BeanFactory::newBean('CampaignTrackers');
                    $ct_focus->retrieve($tracker_values[0]);
                    if (!empty($ct_focus->tracker_name)) {
                        $ct_focus->tracker_name = $tracker_values[1];
                        $ct_focus->is_optout = $tracker_values[2];
                        $ct_focus->tracker_url = $tracker_values[3];
                        $ct_focus->save();

                        //load relationship and add to the list
                        $campaign_focus->load_relationship('tracked_urls');
                        $campaign_focus->tracked_urls->add($ct_focus->id);
                    }
                }
            }


            //create new campaign tracker and save if defined
            if (isset($_REQUEST['wiz_list_of_trackers'])) {
                $tracker_strings = explode(",", $_REQUEST['wiz_list_of_trackers']);
                foreach ($tracker_strings as $trkr_string) {
                    $tracker_values = explode("@@", $trkr_string);
                    if (count($tracker_values)==3) {
                        $ct_focus = BeanFactory::newBean('CampaignTrackers');
                        $ct_focus->tracker_name = $tracker_values[0];
                        $ct_focus->is_optout = $tracker_values[1];
                        $ct_focus->tracker_url = $tracker_values[2];
                        $ct_focus->save();

                        //load relationship and add to the list
                        $campaign_focus->load_relationship('tracked_urls');
                        $campaign_focus->tracked_urls->add($ct_focus->id);
                        // save campaign_trkrs after populating campaign id
                        $ct_focus->save();
                    }
                }
            }

            //set navigation details
            $_REQUEST['return_id'] = $campaign_focus->id;
            $_REQUEST['return_module'] = $campaign_focus->module_dir;
            $_REQUEST['return_action'] = "WizardNewsLetter";
            $_REQUEST['action'] = "WizardMarketing";
            $_REQUEST['record'] = $campaign_focus->id;

            $action = '';
            $redirectToTargetList = '';
            if (isset($_REQUEST['wiz_direction'])) {
                switch ($_REQUEST['wiz_direction']) {
                    case 'continue':
                        $action = 'WizardMarketing';
                        break;
                    case 'continue_targetList':
                        $action = 'WizardMarketing';
                        $redirectToTargetList = '&redirectToTargetList=1';
                        break;
                    case 'exit':
                        $action = 'WizardHome';
                        break;
                }
            } else {
                $action = 'WizardHome&record='.$campaign_focus->id;
            }
            //require_once('modules/Campaigns/WizardMarketing.php');
            $header_URL = "Location: index.php?return_module=Campaigns&module=Campaigns&action=".$action.$redirectToTargetList."&campaign_id=".$campaign_focus->id."&record=".$campaign_focus->id;
            $GLOBALS['log']->debug("about to post header URL of: $header_URL");
            SugarApplication::headerRedirect($header_URL);
            break;
    }


/*
 * This function will populate the passed in bean with the post variables
 * that contain the specified prefix
 */
function populate_wizard_bean_from_request($bean, $prefix)
{
    foreach ($_REQUEST as $key=> $val) {
        $key = trim($key);
        if ((strstr($key, (string) $prefix)) && (strpos($key, (string) $prefix)== 0)) {
            $field  =substr($key, strlen((string) $prefix)) ;
            if (isset($_REQUEST[$key]) && !empty($_REQUEST[$key])) {
                //echo "prefix is $prefix, field is $field,    key is $key,   and value is $val<br>";
                $value = $_REQUEST[$key];
                $bean->$field = $value;
            }
        }
    }

    return $bean;
}


/*
 * This function will process any specified prospect lists and attach them to current campaign
 * If no prospect lists have been specified, then it will create one for you.  A total of 3 prospect lists
 * will be created for you (Subscription, Unsubscription, and test)
 */
function process_subscriptions_from_request($campaign_name)
{
    global $mod_strings;
    $pl_list = array();

    //process default target list
    $create_new = true;
    $pl_subs = new ProspectList($campaign_name);
    if (!empty($_REQUEST['wiz_step3_subscription_list_id'])) {
        //if subscription list is specified then attach
        $pl_subs->retrieve($_REQUEST['wiz_step3_subscription_list_id']);
        //check to see name matches the bean, if not, then the user has chosen to create new bean
        if ($pl_subs->name == $_REQUEST['wiz_step3_subscription_name']) {
            $pl_list[] = $pl_subs;
            $create_new = false;
        }
    }
    //create new bio if one was not retrieved successfully
    if ($create_new) {
        //use default name if one has not been specified
        $name = $campaign_name . " ".$mod_strings['LBL_SUBSCRIPTION_LIST'];
        if (isset($_REQUEST['wiz_step3_subscription_name']) && !empty($_REQUEST['wiz_step3_subscription_name'])) {
            $name = $_REQUEST['wiz_step3_subscription_name'];
        }
        //if subscription list is not specified then create and attach default one
        $pl_subs->name = $name;
        $pl_subs->list_type = 'default';
        $pl_subs->assigned_user_id= $GLOBALS['current_user']->id;
        $pl_subs->save();
        $pl_list[] = $pl_subs;
    }

    //process exempt target list
    $create_new = true;
    $pl_un_subs = BeanFactory::newBean('ProspectLists');
    if (!empty($_REQUEST['wiz_step3_unsubscription_list_id'])) {
        //if unsubscription list is specified then attach
        $pl_un_subs->retrieve($_REQUEST['wiz_step3_unsubscription_list_id']);
        //check to see name matches the bean, if not, then the user has chosen to create new bean
        if ($pl_un_subs->name == $_REQUEST['wiz_step3_unsubscription_name']) {
            $pl_list[] = $pl_un_subs;
            $create_new = false;
        }
    }
    //create new bean if one was not retrieved successfully
    if ($create_new) {
        //use default name if one has not been specified
        $name = $campaign_name . " ".$mod_strings['LBL_UNSUBSCRIPTION_LIST'];
        if (isset($_REQUEST['wiz_step3_unsubscription_name']) && !empty($_REQUEST['wiz_step3_unsubscription_name'])) {
            $name = $_REQUEST['wiz_step3_unsubscription_name'];
        }
        //if unsubscription list is not specified then create and attach default one
        $pl_un_subs->name = $name;
        $pl_un_subs->list_type = 'exempt';
        $pl_un_subs->assigned_user_id= $GLOBALS['current_user']->id;
        $pl_un_subs->save();
        $pl_list[] = $pl_un_subs;
    }

    //process test target list
    $pl_test = BeanFactory::newBean('ProspectLists');
    $create_new = true;
    if (!empty($_REQUEST['wiz_step3_test_list_id'])) {
        //if test list is specified then attach
        $pl_test->retrieve($_REQUEST['wiz_step3_test_list_id']);
        //check to see name matches the bean, if not, then the user has chosen to create new bean
        if ($pl_test->name == $_REQUEST['wiz_step3_test_name']) {
            $pl_list[] = $pl_test;
            $create_new = false;
        }
    }
    //create new bio if one was not retrieved successfully
    if ($create_new) {
        //use default name if one has not been specified
        $name = $campaign_name . " ".$mod_strings['LBL_TEST_LIST'];
        if (isset($_REQUEST['wiz_step3_test_name']) && !empty($_REQUEST['wiz_step3_test_name'])) {
            $name = $_REQUEST['wiz_step3_test_name'];
        }
        //if test list is not specified then create and attach default one
        $pl_test->name = $name;
        $pl_test->list_type = 'test';
        $pl_test->assigned_user_id= $GLOBALS['current_user']->id;
        $pl_test->save();
        $pl_list[] = $pl_test;
    }

    return $pl_list;
}
