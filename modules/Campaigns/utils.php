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

 * Description:  Defines the English language pack for the base application.
 * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
 * All Rights Reserved.
 * Contributor(s): ______________________________________..
 ********************************************************************************/

/*
 *returns a list of objects a message can be scoped by, the list contacts the current campaign
 *name and list of all prospects associated with this campaign..
 *
 */
function get_message_scope_dom($campaign_id, $campaign_name, $db = null, $mod_strings = array())
{

    if (empty($db)) {
        $db = DBManagerFactory::getInstance();
    }
    if (empty($mod_strings) || !isset($mod_strings['LBL_DEFAULT'])) {
        global $current_language;
        $mod_strings = return_module_language($current_language, 'Campaigns');
    }

    //find prospect list attached to this campaign..
    $query = "SELECT prospect_list_id, prospect_lists.name ";
    $query .= "FROM prospect_list_campaigns ";
    $query .= "INNER join prospect_lists on prospect_lists.id = prospect_list_campaigns.prospect_list_id ";
    $query .= "WHERE prospect_lists.deleted = 0 ";
    $query .= "AND prospect_list_campaigns.deleted=0 ";
    $query .= "AND campaign_id='". $db->quote($campaign_id)."'";
    $query.=" and prospect_lists.list_type not like 'exempt%'";

    //add campaign to the result array.
    //$return_array[$campaign_id]= $campaign_name . ' (' . $mod_strings['LBL_DEFAULT'] . ')';

    $return_array = [];

    $result=$db->query($query);
    while (($row=$db->fetchByAssoc($result))!= null) {
        $return_array[$row['prospect_list_id']]=$row['name'];
    }
    if (empty($return_array)) {
        $return_array=array();
    } else {
        return $return_array;
    }
}
/**
 * Return bounce handling mailboxes for campaign.
 *
 * @param unknown_type $emails
 * @param unknown_type $get_box_name, Set it to false if want to get "From Name" other than the InboundEmail Name.
 * @return $get_name=true, bounce handling mailboxes' name; $get_name=false, bounce handling mailboxes' from name.
 */
function get_campaign_mailboxes(&$emails, $get_name=true)
{
    if (!class_exists('InboundEmail')) {
        require('modules/InboundEmail/InboundEmail.php');
    }
    $query =  "select id,name,stored_options from inbound_email where mailbox_type='bounce' and status='Active' and deleted='0'";
    $db = DBManagerFactory::getInstance();
    $result=$db->query($query);
    $return_array = [];
    while (($row=$db->fetchByAssoc($result))!= null) {
        if ($get_name) {
            $return_array[$row['id']] = $row['name'];
        } else {
            $return_array[$row['id']]= InboundEmail::get_stored_options_static('from_name', $row['name'], $row['stored_options']);
        }
        $emails[$row['id']]=InboundEmail::get_stored_options_static('from_addr', 'nobody@example.com', $row['stored_options']);
    }

    if (empty($return_array)) {
        $return_array=array(''=>'');
    }
    return $return_array;
}

function get_campaign_mailboxes_with_stored_options()
{
    $ret = array();

    if (!class_exists('InboundEmail')) {
        require('modules/InboundEmail/InboundEmail.php');
    }

    $q = "SELECT id, name, stored_options FROM inbound_email WHERE mailbox_type='bounce' AND status='Active' AND deleted='0'";

    $db = DBManagerFactory::getInstance();

    $r = $db->query($q);

    while ($a = $db->fetchByAssoc($r)) {
        $ret[$a['id']] = unserialize(base64_decode($a['stored_options']), ['allowed_classes' => false]);
    }
    return $ret;
}

function get_campaign_mailboxes_with_stored_options_outbound()
{
    $ret = array();

    if (!class_exists('OutboundEmail')) {
        require('modules/OutboundEmail/OutboundEmail.php');
    }

    $q = "SELECT * FROM outbound_email WHERE deleted='0'";

    $db = DBManagerFactory::getInstance();

    $r = $db->query($q);

    while ($a = $db->fetchByAssoc($r)) {
        $ret[$a['id']] = $a;
    }
    return $ret;
}

function log_campaign_activity($identifier, $activity, $update = true, $clicked_url_key = null)
{

    global $sugar_config;

    $data = [];
    $return_array = [];

    $db = DBManagerFactory::getInstance();

    //check to see if the identifier has been replaced with Banner string
    if ($identifier == 'BANNER' && isset($clicked_url_key) && !empty($clicked_url_key)) {
        // create md5 encrypted string using the client ip, this will be used for tracker id purposes
        $enc_id = 'BNR' . md5($_SERVER['REMOTE_ADDR']);

        //default the identifier to ip address
        $identifier = $enc_id;

        //if user has chosen to not use this mode of id generation, then replace identifier with plain guid.
        //difference is that guid will generate a new campaign log for EACH CLICK!!
        //encrypted generation will generate 1 campaign log and update the hit counter for each click
        if (isset($sugar_config['campaign_banner_id_generation']) && $sugar_config['campaign_banner_id_generation'] != 'md5') {
            $identifier = create_guid();
        }

        //retrieve campaign log.
        // quote variable first
        $identifierQuoted = $db->quote($identifier);
        $clickedUrlKeyQuoted = $db->quote($clicked_url_key);
        $trkr_query = "select * from campaign_log where target_tracker_key='$identifierQuoted' and related_id = '$clickedUrlKeyQuoted'";
        $current_trkr = $db->query($trkr_query);
        $row = $db->fetchByAssoc($current_trkr);

        //if campaign log is not retrieved (this is a new ip address or we have chosen to create
        //unique entries for each click
        if ($row == null || empty($row)) {


            //retrieve campaign id
            $clickedUrlKeyQuoted = $db->quote($clicked_url_key);
            $trkr_query = "select ct.campaign_id from campaign_trkrs ct, campaigns c where c.id = ct.campaign_id and ct.id = '$clickedUrlKeyQuoted'";
            $current_trkr = $db->query($trkr_query);
            $row = $db->fetchByAssoc($current_trkr);


            //create new campaign log with minimal info.  Note that we are creating new unique id
            //as target id, since we do not link banner/web campaigns to any users

            $data['target_id'] = "'" . create_guid() . "'";
            $data['target_type'] = "'Prospects'";
            $data['id'] = "'" . create_guid() . "'";
            $data['campaign_id'] = $db->quoted($row['campaign_id']);
            $data['target_tracker_key'] = $db->quoted($identifier);
            $data['activity_type'] = $db->quoted($activity);
            $data['activity_date'] = "'" . TimeDate::getInstance()->nowDb() . "'";
            $data['hits'] = 1;
            $data['deleted'] = 0;
            if (!empty($clicked_url_key)) {
                $data['related_id'] = $db->quoted($clicked_url_key);
                $data['related_type'] = "'" . 'CampaignTrackers' . "'";
            }

            //values for return array..
            $return_array['target_id'] = $data['target_id'];
            $return_array['target_type'] = $data['target_type'];

            //create insert query for new campaign log
            // quote variable first
            $dataArrayKeys = array_keys($data);
            $dataArrayKeysQuoted = array();
            foreach ($dataArrayKeys as $dataArrayKey) {
                $dataArrayKeysQuoted[] = $db->quote($dataArrayKey);
            }
            $dataArrayKeysQuotedImplode = implode(', ', $dataArrayKeysQuoted);

            $insert_query = "INSERT into campaign_log (" . $dataArrayKeysQuotedImplode . ")";

            $dataArrayValuesQuotedImplode = implode(', ', array_values($data));

            $insert_query .= " VALUES  (" . $dataArrayValuesQuotedImplode . ")";

            $db->query($insert_query);
        } else {

            //campaign log already exists, so just set the return array and update hits column
            $return_array['target_id'] = $row['target_id'];
            $return_array['target_type'] = $row['target_type'];

            // quote variable first
            $rowIdQuoted = $db->quote($row['id']);
            $query1 = "update campaign_log set hits=hits+1 where id='$rowIdQuoted'";

            $current = $db->query($query1);
        }

        //return array and exit
        return $return_array;
    }



    // quote variable first
    $identifierQuoted = $db->quote($identifier);
    $activityQuoted = $db->quote($activity);
    $query1 = "select * from campaign_log where target_tracker_key='$identifierQuoted' and activity_type='$activityQuoted'";
    if (!empty($clicked_url_key)) {
        // quote variable first
        $clickedUrlKeyQuoted = $db->quote($clicked_url_key);
        $query1 .= " AND related_id='$clickedUrlKeyQuoted'";
    }
    $current = $db->query($query1);
    $row = $db->fetchByAssoc($current);

    if ($row == null) {
        // quote variable first
        $identifierQuoted = $db->quote($identifier);
        $query = "select * from campaign_log where target_tracker_key='$identifierQuoted' and activity_type='targeted'";
        $targeted = $db->query($query);
        $row = $db->fetchByAssoc($targeted);

        //if activity is removed and target type is users, then a user is trying to opt out
        //of emails.  This is not possible as Users Table does not have opt out column.
        if ($row && (strtolower($row['target_type']) == 'users' && $activity == 'removed')) {
            $return_array['target_id'] = $row['target_id'];
            $return_array['target_type'] = $row['target_type'];
            return $return_array;
        } elseif ($row) {
            $data['id'] = "'" . create_guid() . "'";
            $data['campaign_id'] = $db->quoted($row['campaign_id']);
            $data['target_tracker_key'] = $db->quoted($identifier);
            $data['target_id'] = $db->quoted($row['target_id']);
            $data['target_type'] = $db->quoted($row['target_type']);
            $data['activity_type'] = $db->quoted($activity);
            $data['activity_date'] = "'" . TimeDate::getInstance()->nowDb() . "'";
            $data['list_id'] = $db->quoted($row['list_id']);
            $data['marketing_id'] = $db->quoted($row['marketing_id']);
            $data['hits'] = 1;
            $data['deleted'] = 0;
            if (!empty($clicked_url_key)) {
                $data['related_id'] = $db->quoted($clicked_url_key);
                $data['related_type'] = "'" . 'CampaignTrackers' . "'";
            }
            //values for return array..
            $return_array['target_id'] = $row['target_id'];
            $return_array['target_type'] = $row['target_type'];

            // quote variable first
            $dataArrayKeys = array_keys($data);
            $dataArrayKeysQuoted = array();
            foreach ($dataArrayKeys as $dataArrayKey) {
                $dataArrayKeysQuoted[] = $db->quote($dataArrayKey);
            }
            $dataArrayKeysQuotedImplode = implode(', ', $dataArrayKeysQuoted);

            $insert_query = "INSERT into campaign_log (" . $dataArrayKeysQuotedImplode . ")";

            $dataArrayValuesQuotedImplode = implode(', ', array_values($data));

            $insert_query .= " VALUES  (" . $dataArrayValuesQuotedImplode . ")";

            $db->query($insert_query);
        }
    } else {
        $return_array['target_id'] = $row['target_id'];
        $return_array['target_type'] = $row['target_type'];

        // quote variable first
        $rowIdQuoted = $db->quote($row['id']);
        $query1 = "update campaign_log set hits=hits+1 where id='$rowIdQuoted'";
        $current = $db->query($query1);
    }
    //check to see if this is a removal action
    if ($row && $activity == 'removed') {
        //retrieve campaign and check it's type, we are looking for newsletter Campaigns
        //
        // quote variable first
        $rowCampaignIdQuoted = $db->quote($row['campaign_id']);
        $query = "SELECT campaigns.* FROM campaigns WHERE campaigns.id = '" . $rowCampaignIdQuoted . "' ";
        $result = $db->query($query);

        if (!empty($result)) {
            $c_row = $db->fetchByAssoc($result);

            //if type is newsletter, then add campaign id to return_array for further processing.
            if (isset($c_row['campaign_type']) && $c_row['campaign_type'] == 'NewsLetter') {
                $return_array['campaign_id'] = $c_row['id'];
            }
        }
    }
    return $return_array;
}

/**
     *
     * This method is deprecated
     * @deprecated 62_Joneses - June 24, 2011
     * @see campaign_log_lead_or_contact_entry()
     */
function campaign_log_lead_entry($campaign_id, $parent_bean, $child_bean, $activity_type)
{
    campaign_log_lead_or_contact_entry($campaign_id, $parent_bean, $child_bean, $activity_type);
}


function campaign_log_lead_or_contact_entry($campaign_id, $parent_bean, $child_bean, $activity_type)
{
    global $timedate;

    //create campaign tracker id and retrieve related bio bean
    $tracker_id = create_guid();
    //create new campaign log record.
    $campaign_log = BeanFactory::newBean('CampaignLog');
    $campaign_log->campaign_id = $campaign_id;
    $campaign_log->target_tracker_key = $tracker_id;
    $campaign_log->related_id = $parent_bean->id;
    $campaign_log->related_type = $parent_bean->module_dir;
    $campaign_log->target_id = $child_bean->id;
    $campaign_log->target_type = $child_bean->module_dir;
    $campaign_log->activity_date = $timedate->now();
    $campaign_log->activity_type = $activity_type;
    //save the campaign log entry
    $campaign_log->save();
}


function get_campaign_urls($campaign_id)
{
    $return_array=array();

    if (!empty($campaign_id)) {
        $db = DBManagerFactory::getInstance();

        $campaign_id = $db->quote($campaign_id);

        $query1="select * from campaign_trkrs where campaign_id='$campaign_id' and deleted=0";
        $current=$db->query($query1);
        while (($row=$db->fetchByAssoc($current)) != null) {
            $return_array['{'.$row['tracker_name'].'}']=$row['tracker_name'] . ' : ' . $row['tracker_url'];
        }
    }
    return $return_array;
}

/**
 * Queries for the list
 */
function get_subscription_lists_query($focus, $additional_fields = null)
{
    //get all prospect lists belonging to Campaigns of type newsletter
    $all_news_type_pl_query = "select c.name, pl.list_type, plc.campaign_id, plc.prospect_list_id";
    if (is_array($additional_fields) && !empty($additional_fields)) {
        $all_news_type_pl_query .= ', ' . implode(', ', $additional_fields);
    }
    $all_news_type_pl_query .= " from prospect_list_campaigns plc , prospect_lists pl, campaigns c ";


    $all_news_type_pl_query .= "where plc.campaign_id = c.id ";
    $all_news_type_pl_query .= "and plc.prospect_list_id = pl.id ";
    $all_news_type_pl_query .= "and c.campaign_type = 'NewsLetter'  and pl.deleted = 0 and c.deleted=0 and plc.deleted=0 ";
    $all_news_type_pl_query .= "and (pl.list_type like 'exempt%' or pl.list_type ='default') ";

    $campaign = BeanFactory::newBean('Campaigns');
    $campaign->table_name = 'c';
    $accessWhere = $campaign->buildAccessWhere('list');
    if (!empty($accessWhere)) {
        $all_news_type_pl_query .= ' AND ' . $accessWhere;
    }

    $all_news_type_list =$focus->db->query($all_news_type_pl_query);

    //build array of all newsletter campaigns
    $news_type_list_arr = array();
    while ($row = $focus->db->fetchByAssoc($all_news_type_list)) {
        $news_type_list_arr[] = $row;
    }

    //now get all the campaigns that the current user is assigned to
    $all_plp_current = "select prospect_list_id from prospect_lists_prospects where related_id = '$focus->id' and deleted = 0 ";

    //build array of prospect lists that this user belongs to
    $current_plp =$focus->db->query($all_plp_current);
    $current_plp_arr = array();
    while ($row = $focus->db->fetchByAssoc($current_plp)) {
        $current_plp_arr[] = $row;
    }

    return array('current_plp_arr' => $current_plp_arr, 'news_type_list_arr' => $news_type_list_arr);
}
/*
 * This function takes in a bean from a lead, prospect, or contact and returns an array containing
 * all subscription lists that the bean is a part of, and all the subscriptions that the bean is not
 * a part of.  The array elements have the key names of "subscribed" and "unsusbscribed".  These elements contain an array
 * of the corresponding list.  In other words, the "subscribed" element holds another array that holds the subscription information.
 *
 * The subscription information is a concatenated string that holds the prospect list id and the campaign id, separated by at "@" character.
 * To parse these information string into something more usable, use the "process subscriptions()" function
 *
 * */
function get_subscription_lists($focus, $descriptions = false)
{
    $return_array = [];
    $subs_arr = array();
    $unsubs_arr = array();

    $results = get_subscription_lists_query($focus, $descriptions);

    $news_type_list_arr = $results['news_type_list_arr'];
    $current_plp_arr = $results['current_plp_arr'];

    //For each  prospect list of type 'NewsLetter', check to see if current user is already in list,
    foreach ($news_type_list_arr as $news_list) {
        $match = 'false';

        //perform this check against each prospect list this user belongs to
        foreach ($current_plp_arr as $current_list_key => $current_list) {//echo " new entry from current lists user is subscribed to-------------";
            //compare current user list id against newsletter id
            if ($news_list['prospect_list_id'] == $current_list['prospect_list_id']) {
                //if id's match, user is subscribed to this list, check to see if this is an exempt list,
                if (strpos((string) $news_list['list_type'], 'exempt')!== false) {
                    //this is an exempt list, so process
                    if (array_key_exists($news_list['name'], $subs_arr)) {
                        //first, add to unsubscribed array
                        $unsubs_arr[$news_list['name']] = $subs_arr[$news_list['name']];
                        //now remove from exempt subscription list
                        unset($subs_arr[$news_list['name']]);
                    } else {
                        //we know this is an exempt list the user belongs to, but the
                        //non exempt list has not been processed yet, so just add to exempt array
                        $unsubs_arr[$news_list['name']] = "prospect_list@".$news_list['prospect_list_id']."@campaign@".$news_list['campaign_id'];
                    }
                    $match = 'false';//although match is false, this is an exempt array, so
                    //it will not be added a second time down below
                } else {
                    //this list is not exempt, and user is subscribed, so add to subscribed array, and unset from the unsubs_arr
                    //as long as this list is not in exempt array
                    if (!array_key_exists($news_list['name'], $unsubs_arr)) {
                        $subs_arr[$news_list['name']] = "prospect_list@".$news_list['prospect_list_id']."@campaign@".$news_list['campaign_id'];
                        $match = 'true';
                        unset($unsubs_arr[$news_list['name']]);
                    }
                }
            } else {
                //do nothing, there is no match
            }
        }
        //if this newsletter id never matched a user subscription..
        //..then add to available(unsubscribed) NewsLetters if list is not of type exempt
        if (($match == 'false') && (strpos((string) $news_list['list_type'], 'exempt') === false) && (!array_key_exists($news_list['name'], $subs_arr))) {
            $unsubs_arr[$news_list['name']] = "prospect_list@".$news_list['prospect_list_id']."@campaign@".$news_list['campaign_id'];
        }
    }
    $return_array['unsubscribed'] = $unsubs_arr;
    $return_array['subscribed'] = $subs_arr;
    return $return_array;
}

/**
 * same function as get_subscription_lists, but with the data separated in an associated array
 */
function get_subscription_lists_keyed($focus)
{
    $return_array = [];
    $subs_arr = array();
    $unsubs_arr = array();

    $results = get_subscription_lists_query($focus, array('c.content', 'c.frequency'));

    $news_type_list_arr = $results['news_type_list_arr'];
    $current_plp_arr = $results['current_plp_arr'];

    //For each  prospect list of type 'NewsLetter', check to see if current user is already in list,
    foreach ($news_type_list_arr as $news_list) {
        $match = false;

        $news_list_data = array('prospect_list_id' => $news_list['prospect_list_id'],
                                'campaign_id'      => $news_list['campaign_id'],
                                'description'      => $news_list['content'],
                                'frequency'        => $news_list['frequency']);

        //perform this check against each prospect list this user belongs to
        foreach ($current_plp_arr as $current_list_key => $current_list) {//echo " new entry from current lists user is subscribed to-------------";
            //compare current user list id against newsletter id
            if ($news_list['prospect_list_id'] == $current_list['prospect_list_id']) {
                //if id's match, user is subscribed to this list, check to see if this is an exempt list,

                if ($news_list['list_type'] == 'exempt') {
                    //this is an exempt list, so process
                    if (array_key_exists($news_list['name'], $subs_arr)) {
                        //first, add to unsubscribed array
                        $unsubs_arr[$news_list['name']] = $subs_arr[$news_list['name']];
                        //now remove from exempt subscription list
                        unset($subs_arr[$news_list['name']]);
                    } else {
                        //we know this is an exempt list the user belongs to, but the
                        //non exempt list has not been processed yet, so just add to exempt array
                        $unsubs_arr[$news_list['name']] = $news_list_data;
                    }
                    $match = false;//although match is false, this is an exempt array, so
                    //it will not be added a second time down below
                } else {
                    //this list is not exempt, and user is subscribed, so add to subscribed array
                    //as long as this list is not in exempt array
                    if (!array_key_exists($news_list['name'], $unsubs_arr)) {
                        $subs_arr[$news_list['name']] = $news_list_data;
                        $match = 'true';
                    }
                }
            } else {
                //do nothing, there is no match
            }
        }
        //if this newsletter id never matched a user subscription..
        //..then add to available(unsubscribed) NewsLetters if list is not of type exempt
        if (($match == false) && ($news_list['list_type'] != 'exempt')) {
            $unsubs_arr[$news_list['name']] = $news_list_data;
        }
    }

    $return_array['unsubscribed'] = $unsubs_arr;
    $return_array['subscribed'] = $subs_arr;
    return $return_array;
}



/*
 * This function will take an array of strings that have been created by the "get_subscription_lists()" method
 * and parses it into an array.  The returned array has it's key's labeled in a specific fashion.
 *
 * Each string produces a campaign and a prospect id.  The keys are appended with a number specifying the order
 * it was process in.  So an input array containing 3 strings will have the following key values:
 * "prospect_list0", "campaign0"
 * "prospect_list1", "campaign1"
 * "prospect_list2", "campaign2"
 *
 * */
function process_subscriptions($subscription_string_to_parse)
{
    $subs_change = array();

    //parse through and build list of id's'.  We are retrieving the campaign_id and
    //the prospect_list id from the selected subscriptions
    $i = 0;
    foreach ($subscription_string_to_parse as $subs_changes) {
        $subs_changes = trim($subs_changes);
        if (!empty($subs_changes)) {
            $ids_arr = explode("@", $subs_changes);
            $subs_change[$ids_arr[0].$i] = $ids_arr[1];
            $subs_change[$ids_arr[2].$i] = $ids_arr[3];
            $i = $i+1;
        }
    }
    return $subs_change;
}


    /*This function is used by the Manage Subscriptions page in order to add the user
     * to the default prospect lists of the passed in campaign
     * Takes in campaign and prospect list id's we are subscribing to.
     * It also takes in a bean of the user (lead,target,prospect) we are subscribing
     * */
    function subscribe($campaign, $prospect_list, $focus, $default_list = false)
    {

        $relationship = strtolower($focus->getObjectName()).'s';

        //--grab all the lists for the passed in campaign id
        $pl_qry ="select id, list_type from prospect_lists where id in (select prospect_list_id from prospect_list_campaigns ";
        $pl_qry .= "where campaign_id = " . $focus->db->quoted($campaign) . ") and deleted = 0 ";
        $GLOBALS['log']->debug("In Campaigns Util: subscribe function, about to run query: ".$pl_qry);
        $pl_qry_result = $focus->db->query($pl_qry);

        //build the array of all prospect_lists
        $pl_arr = array();
        while ($row = $focus->db->fetchByAssoc($pl_qry_result)) {
            $pl_arr[] = $row;
        }

        //--grab all the prospect_lists this user belongs to
        $curr_pl_qry ="select prospect_list_id, related_id  from prospect_lists_prospects ";
        $curr_pl_qry .="where related_id = " . $focus->db->quoted($focus->id) . " and deleted = 0 ";
        $GLOBALS['log']->debug("In Campaigns Util: subscribe function, about to run query: ".$curr_pl_qry);
        $curr_pl_qry_result = $focus->db->query($curr_pl_qry);

        //build the array of all prospect lists that this current user belongs to
        $curr_pl_arr = array();
        while ($row = $focus->db->fetchByAssoc($curr_pl_qry_result)) {
            $curr_pl_arr[] = $row;
        }

        //search through prospect lists for this campaign and identifiy the "unsubscription list"
        $exempt_id = '';
        foreach ($pl_arr as $subscription_list) {
            if (strpos((string) $subscription_list['list_type'], 'exempt')!== false) {
                $exempt_id = $subscription_list['id'];
            }

            if ($subscription_list['list_type'] == 'default' && $default_list) {
                $prospect_list = $subscription_list['id'];
            }
        }

        $exempt_array = [];
        //now that we have exempt (unsubscription) list id, compare against user list id's
        if (!empty($exempt_id)) {
            $exempt_array['exempt_id'] = $exempt_id;

            foreach ($curr_pl_arr as $curr_subscription_list) {
                if ($curr_subscription_list['prospect_list_id'] == $exempt_id) {
                    //--if we are in here then user is subscribing to a list in which they are exempt.
                    // we need to remove the user from this unsubscription list.
                    //Begin by retrieving unsubscription prospect list
                    $exempt_subscription_list = BeanFactory::newBean('ProspectLists');


                    $exempt_result = $exempt_subscription_list->retrieve($exempt_id);
                    if ($exempt_result == null) {//error happened while retrieving this list
                        return;
                    }
                    //load realationships and delete user from unsubscription list
                    $exempt_subscription_list->load_relationship($relationship);
                    $exempt_subscription_list->$relationship->delete($exempt_id, $focus->id);
                }
            }
        }

        //Now we need to check if user is already in subscription list
        $already_here = 'false';
        //for each list user is subscribed to, compare id's with current list id'
        foreach ($curr_pl_arr as $user_list) {
            if (in_array($prospect_list, $user_list)) {
                //if user already exists, then set flag to true
                $already_here = 'true';
            }
        }
        if ($already_here ==='true') {
            //do nothing, user is already subscribed
        } else {
            //user is not subscribed already, so add to subscription list
            $subscription_list = BeanFactory::newBean('ProspectLists');
            $subs_result = $subscription_list->retrieve($prospect_list);
            if ($subs_result == null) {//error happened while retrieving this list, iterate and continue
                return;
            }
            //load subscription list and add this user
            $GLOBALS['log']->debug("In Campaigns Util, loading relationship: ".$relationship);
            $subscription_list->load_relationship($relationship);
            $subscription_list->$relationship->add($focus->id);
        }
    }


    /*This function is used by the Manage Subscriptions page in order to add the user
     * to the exempt prospect lists of the passed in campaign
     * Takes in campaign and focus parameters.
     * */
    function unsubscribe($campaign, $focus)
    {

        $relationship = strtolower($focus->getObjectName()).'s';
        //--grab all the list for this campaign id
        $pl_qry ="select id, list_type from prospect_lists where id in (select prospect_list_id from prospect_list_campaigns ";
        $pl_qry .= "where campaign_id = " . $focus->db->quoted($campaign) . ") and deleted = 0 ";
        $pl_qry_result = $focus->db->query($pl_qry);
        //build the array with list information
        $pl_arr = array();
        $GLOBALS['log']->debug("In Campaigns Util, about to run query: ".$pl_qry);
        while ($row = $focus->db->fetchByAssoc($pl_qry_result)) {
            $pl_arr[] = $row;
        }

        //retrieve lists that this user belongs to
        $curr_pl_qry ="select prospect_list_id, related_id  from prospect_lists_prospects ";
        $curr_pl_qry .="where related_id = '$focus->id'  and deleted = 0 ";
        $GLOBALS['log']->debug("In Campaigns Util, unsubscribe function about to run query: ".$curr_pl_qry);
        $curr_pl_qry_result = $focus->db->query($curr_pl_qry);

        //build the array with current user list information
        $curr_pl_arr = array();
        while ($row = $focus->db->fetchByAssoc($curr_pl_qry_result)) {
            $curr_pl_arr[] = $row;
        }
        //check to see if user is already there in prospect list
        $already_here = 'false';
        $exempt_id = '';

        foreach ($curr_pl_arr as $user_list) {
            foreach ($pl_arr as $v) {
                //if list is exempt list
                if ($v['list_type'] == 'exempt') {
                    //save the exempt list id for later use
                    $exempt_id = $v['id'];
                    //check to see if user is already in this exempt list
                    if (in_array($v['id'], $user_list)) {
                        $already_here = 'true';
                    }

                    break 2;
                }
            }
        }

        $exempt_list = null;

        //unsubscribe subscripted newsletter
        foreach ($pl_arr as $subscription_list) {
            //create a new instance of the prospect list
            $exempt_list = BeanFactory::newBean('ProspectLists');
            $exempt_list->retrieve($subscription_list['id']);
            $exempt_list->load_relationship($relationship);
            //if list type is default, then delete the relationship
            //if list type is exempt, then add the relationship to unsubscription list
            if ($subscription_list['list_type'] == 'exempt') {
                $exempt_list->$relationship->add($focus->id);
            } elseif ($subscription_list['list_type'] == 'default' || $subscription_list['list_type'] == 'test') {
                //if list type is default or test, then delete the relationship
                //$exempt_list->$relationship->delete($subscription_list['id'],$focus->id);
            }
        }

        if ($already_here =='true') {
            //do nothing, user is already exempted
        } else {
            //user is not exempted yet , so add to unsubscription list


            $exempt_result = $exempt_list->retrieve($exempt_id);
            if ($exempt_result == null) {//error happened while retrieving this list
                return;
            }
            $GLOBALS['log']->debug("In Campaigns Util, loading relationship: ".$relationship);
            $exempt_list->load_relationship($relationship);
            $exempt_list->$relationship->add($focus->id);
        }
    }


    /*
     *This function will return a string to the newsletter wizard if campaign check
     *does not return 100% healthy.
     */
    function diagnose(&$errors = array(), &$links = array())
    {
        global $mod_strings;
        global $current_user;

        $errors = array(
            'mailbox1' => false,
            'mailbox2' => false,
            'admin' => false,
            'scheduler1' => false,
            'scheduler2' => false,
        );

        $links = array(
            'scheduler' => false,
            'email' => false,
        );

        $msg = " <table class='diagnose_messages detail view small' width='100%'><tr><td> ".$mod_strings['LNK_CAMPAIGN_DIGNOSTIC_LINK']."</td></tr>";

        //Start with email components
        //monitored mailbox section
        $focus = BeanFactory::newBean('Administration');
        $focus->retrieveSettings(); //retrieve all admin settings.


        //run query for mail boxes of type 'bounce'
        $email_health = 0;
        $email_components = 2;
        $mbox_qry = "select * from inbound_email where deleted ='0' and mailbox_type = 'bounce'";
        $mbox_res = $focus->db->query($mbox_qry);

        $mbox = array();
        while ($mbox_row = $focus->db->fetchByAssoc($mbox_res)) {
            $mbox[] = $mbox_row;
        }
        //if the array is not empty, then set "good" message
        if (isset($mbox) && count($mbox)>0) {
            //everything is ok, do nothing
        } else {
            //if array is empty, then increment health counter
            $email_health =$email_health +1;
            $msg  .=  "<tr><td ><font color='red'><b>". $mod_strings['LBL_MAILBOX_CHECK1_BAD']."</b></font></td></tr>";
            $errors['mailbox1'] = $mod_strings['LBL_MAILBOX_CHECK1_BAD'];
        }


        if (strstr((string) $focus->settings['notify_fromaddress'], 'example.com')) {
            //if "from_address" is the default, then set "bad" message and increment health counter
            $email_health =$email_health +1;
            $msg .= "<tr><td ><font color='red'><b> ".$mod_strings['LBL_MAILBOX_CHECK2_BAD']." </b></font></td></tr>";
            $errors['mailbox2'] = $mod_strings['LBL_MAILBOX_CHECK2_BAD'];
        } else {
            //do nothing, address has been changed
        }
        //do nothing, address has been changed

        //if health counter is above 1, then show admin link
        if ($email_health>0) {
            if (is_admin($current_user)) {
                $lnk = 'index.php?module=Campaigns&action=WizardEmailSetup';
                $msg.="<tr><td ><a href='";
                if (isset($_REQUEST['return_module'])) {
                    $lnk .="&return_module=".$_REQUEST['return_module'];
                }
                if (isset($_REQUEST['return_action'])) {
                    $lnk .="&return_action=".$_REQUEST['return_action'];
                }
                $msg .= $lnk;
                $links['email'] = $lnk;
                $msg.="'>".$mod_strings['LBL_EMAIL_SETUP_WIZ']."</a></td></tr>";
            } else {
                $msg.="<tr><td >".$mod_strings['LBL_NON_ADMIN_ERROR_MSG']."</td></tr>";
                $errors['admin'] = $mod_strings['LBL_NON_ADMIN_ERROR_MSG'];
            }
        }


        // proceed with scheduler components

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

        while ($sched_row = $focus->db->fetchByAssoc($sched_res)) {
            $scheds[] = $sched_row;
        }
        //iterate through and see which jobs were found
        foreach ($scheds as $funct) {
            if (($funct['job']==$check_sched1)  ||   ($funct['job']==$check_sched2)) {
                if ($funct['job']==$check_sched1) {
                    $check_sched1 ="found";
                } else {
                    $check_sched2 ="found";
                }
            }
        }
        //determine if error messages need to be displayed for schedulers
        if ($check_sched2 != 'found') {
            $sched_health =$sched_health +1;
            $msg.= "<tr><td><font color='red'><b>".$mod_strings['LBL_SCHEDULER_CHECK1_BAD']."</b></font></td></tr>";
            $errors['scheduler1'] = $mod_strings['LBL_SCHEDULER_CHECK1_BAD'];
        }
        if ($check_sched1 != 'found') {
            $sched_health =$sched_health +1;
            $msg.= "<tr><td><font color='red'><b>".$mod_strings['LBL_SCHEDULER_CHECK2_BAD']."</b></font></td></tr>";
            $errors['scheduler2'] = $mod_strings['LBL_SCHEDULER_CHECK2_BAD'];
        }
        //if health counter is above 1, then show admin link
        if ($sched_health>0) {
            global $current_user;
            if (is_admin($current_user)) {
                $link = 'index.php?module=Schedulers&action=index';
                $msg.="<tr><td ><a href='$link'>".$mod_strings['LBL_SCHEDULER_LINK']."</a></td></tr>";
                $links['scheduler'] = $link;
            } else {
                $msg.="<tr><td >".$mod_strings['LBL_NON_ADMIN_ERROR_MSG']."</td></tr>";
                $errors['admin'] = $mod_strings['LBL_NON_ADMIN_ERROR_MSG'];
            }
        }

        //determine whether message should be returned
        if (($sched_health + $email_health)>0) {
            $msg  .= "</table> ";
        } else {
            $msg = '';
        }
        return $msg;
    }


/**
 * Handle campaign log entry creation for mail-merge activity. The function will be called by the soap component.
 *
 * @param String campaign_id Primary key of the campaign
 * @param array targets List of keys for entries from prospect_lists_prosects table
 */
 function campaign_log_mail_merge($campaign_id, $targets)
 {
     $campaign= BeanFactory::newBean('Campaigns');
     $campaign->retrieve($campaign_id);

     if (empty($campaign->id)) {
         $GLOBALS['log']->debug('set_campaign_merge: Invalid campaign id'. $campaign_id);
     } else {
         foreach ($targets as $target_list_id) {
             $pl_query = "select * from prospect_lists_prospects where id='".DBManagerFactory::getInstance()->quote($target_list_id)."'";
             $result=DBManagerFactory::getInstance()->query($pl_query);
             $row=DBManagerFactory::getInstance()->fetchByAssoc($result);
             if (!empty($row)) {
                 write_mail_merge_log_entry($campaign_id, $row);
             }
         }
     }
 }
/**
 * Function creates a campaign_log entry for campaigns processesed using the mail-merge feature. If any entry
 * exist the hit counter is updated. target_tracker_key is used to locate duplicate entries.
 * @param string campaign_id Primary key of the campaign
 * @param array $pl_row A row of data from prospect_lists_prospects table.
 */
function write_mail_merge_log_entry($campaign_id, $pl_row)
{

    //Update the log entry if it exists.
    $update="update campaign_log set hits=hits+1 where campaign_id='".DBManagerFactory::getInstance()->quote($campaign_id)."' and target_tracker_key='" . DBManagerFactory::getInstance()->quote($pl_row['id']) . "'";
    $result=DBManagerFactory::getInstance()->query($update);

    //get affected row count...
    $count=DBManagerFactory::getInstance()->getAffectedRowCount();
    if ($count==0) {
        $data=array();

        $data['id']="'" . create_guid() . "'";
        $data['campaign_id']="'" . DBManagerFactory::getInstance()->quote($campaign_id) . "'";
        $data['target_tracker_key']="'" . DBManagerFactory::getInstance()->quote($pl_row['id']) . "'";
        $data['target_id']="'" .  DBManagerFactory::getInstance()->quote($pl_row['related_id']) . "'";
        $data['target_type']="'" .  DBManagerFactory::getInstance()->quote($pl_row['related_type']) . "'";
        $data['activity_type']="'targeted'";
        $data['activity_date']="'" . TimeDate::getInstance()->nowDb() . "'";
        $data['list_id']="'" .  DBManagerFactory::getInstance()->quote($pl_row['prospect_list_id']) . "'";
        $data['hits']=1;
        $data['deleted']=0;
        $insert_query="INSERT into campaign_log (" . implode(",", array_keys($data)) . ")";
        $insert_query.=" VALUES  (" . implode(",", array_values($data)) . ")";
        DBManagerFactory::getInstance()->query($insert_query);
    }
}

    function track_campaign_prospects($focus)
    {
        $campaign_id = DBManagerFactory::getInstance()->quote($focus->id);
        $delete_query="delete from campaign_log where campaign_id='".$campaign_id."' and activity_type='targeted'";
        $focus->db->query($delete_query);

        $current_date = $focus->db->now();
        $guidSQL = $focus->db->getGuidSQL();

        $insert_query= "INSERT INTO campaign_log (id,activity_date, campaign_id, target_tracker_key,list_id, target_id, target_type, activity_type, deleted";
        $insert_query.=')';
        $insert_query.="SELECT {$guidSQL}, $current_date, plc.campaign_id,{$guidSQL},plp.prospect_list_id, plp.related_id, plp.related_type,'targeted',0 ";
        $insert_query.="FROM prospect_lists INNER JOIN prospect_lists_prospects plp ON plp.prospect_list_id = prospect_lists.id";
        $insert_query.=" INNER JOIN prospect_list_campaigns plc ON plc.prospect_list_id = prospect_lists.id";
        $insert_query.=" WHERE plc.campaign_id='".DBManagerFactory::getInstance()->quote($focus->id)."'";
        $insert_query.=" AND prospect_lists.deleted=0";
        $insert_query.=" AND plc.deleted=0";
        $insert_query.=" AND plp.deleted=0";
        $insert_query.=" AND prospect_lists.list_type!='test' AND prospect_lists.list_type not like 'exempt%'";
        $focus->db->query($insert_query);

        global $mod_strings;
        //return success message
        return $mod_strings['LBL_DEFAULT_LIST_ENTRIES_WERE_PROCESSED'];
    }

    function create_campaign_log_entry($campaign_id, $focus, $rel_name, $rel_bean, $target_id = '')
    {
        global $timedate;

        $target_ids = array();
        //check if this is specified for one target/contact/prospect/lead (from contact/lead detail subpanel)
        if (!empty($target_id)) {
            $target_ids[] = $target_id;
        } else {
            //this is specified for all, so load target/prospect relationships (mark as sent button)
            $focus->load_relationship($rel_name);
            $target_ids = $focus->$rel_name->get();
        }
        if ((is_countable($target_ids) ? count($target_ids) : 0)>0) {


            //retrieve the target beans and create campaign log entry
            foreach ($target_ids as $id) {
                //perform duplicate check
                $dup_query = "select id from campaign_log where campaign_id = '$campaign_id' and target_id = '$id'";
                $dup_result = $focus->db->query($dup_query);
                $row = $focus->db->fetchByAssoc($dup_result);

                //process if this is not a duplicate campaign log entry
                if (empty($row)) {
                    //create campaign tracker id and retrieve related bio bean
                    $tracker_id = create_guid();
                    $rel_bean->retrieve($id);

                    //create new campaign log record.
                    $campaign_log = BeanFactory::newBean('CampaignLog');
                    $campaign_log->campaign_id = $campaign_id;
                    $campaign_log->target_tracker_key = $tracker_id;
                    $campaign_log->target_id = $rel_bean->id;
                    $campaign_log->target_type = $rel_bean->module_dir;
                    $campaign_log->activity_type = 'targeted';
                    $campaign_log->activity_date=$timedate->now();
                    //save the campaign log entry
                    $campaign_log->save();
                }
            }
        }
    }

    /*
     * This function will return an array that has been formatted to work as a Quick Search Object for prospect lists
     */
    function getProspectListQSObjects($source = '', $return_field_name='name', $return_field_id='id')
    {
        global $app_strings;
        //if source has not been specified, then search across all prospect lists
        if (empty($source)) {
            $qsProspectList = array('method' => 'query',
                                'modules'=> array('ProspectLists'),
                                'group' => 'and',
                                'field_list' => array('name', 'id'),
                                'populate_list' => array('prospect_list_name', 'prospect_list_id'),
                                'conditions' => array( array('name'=>'name','op'=>'like_custom','end'=>'%','value'=>'') ),
                                'order' => 'name',
                                'limit' => '30',
                                'no_match_text' => $app_strings['ERR_SQS_NO_MATCH']);
        } else {
            //source has been specified use it to tell quicksearch.js which html input to use to get filter value
            $qsProspectList = array('method' => 'query',
                                'modules'=> array('ProspectLists'),
                                'group' => 'and',
                                'field_list' => array('name', 'id'),
                                'populate_list' => array($return_field_name, $return_field_id),
                                'conditions' => array(
                                                    array('name'=>'name','op'=>'like_custom','end'=>'%','value'=>''),
                                                    //this condition has the source parameter defined, meaning the query will take the value specified below
                                                    array('name'=>'list_type', 'op'=>'like_custom', 'end'=>'%','value'=>'', 'source' => $source)
                                ),
                                'order' => 'name',
                                'limit' => '30',
                                'no_match_text' => $app_strings['ERR_SQS_NO_MATCH']);
        }

        return $qsProspectList;
    }


function filterFieldsFromBeans($beans)
{
    global $app_strings;
    $formattedBeans = array();
    foreach ($beans as $b) {
        $formattedFields = array();
        //bug: 47574 - make sure, that webtolead_email1 field has same required attribute as email1 field
        if (isset($b->field_defs['webtolead_email1']) && isset($b->field_defs['email1']) && isset($b->field_defs['email1']['required'])) {
            $b->field_defs['webtolead_email1']['required'] = $b->field_defs['email1']['required'];
        }

        foreach ($b->field_defs as $field_def) {
            $email_fields = false;
            if ($field_def['name']== 'email1' || $field_def['name']== 'email2') {
                $email_fields = true;
            }
            if ($field_def['name']!= 'account_name') {
                if (($field_def['type'] == 'relate' && empty($field_def['custom_type']))
                    || $field_def['type'] == 'assigned_user_name' || $field_def['type'] =='link' || $field_def['type'] =='function'
                    || (isset($field_def['source'])  && $field_def['source']=='non-db' && !$email_fields) || $field_def['type'] == 'id') {
                    continue;
                }
            }
            if ($field_def['name']== 'deleted' || $field_def['name']=='converted' || $field_def['name']=='date_entered'
                || $field_def['name']== 'date_modified' || $field_def['name']=='modified_user_id'
                || $field_def['name']=='assigned_user_id' || $field_def['name']=='created_by'
                || $field_def['name']=='team_id') {
                continue;
            }

            //If the field is hidden in the studio settings, then do not show
            if (isset($field_def['studio']) && isset($field_def['studio']['editview']) && $field_def['studio']['editview']=== false) {
                continue;
            }


            $field_def['vname'] = preg_replace('/:$/', '', (string) translate($field_def['vname'], $b->module_dir));

            //$cols_name = "{'".$field_def['vname']."'}";
            $col_arr = array();
            if ((isset($field_def['required']) && $field_def['required'] != null && $field_def['required'] != 0)
                || $field_def['name']=='last_name'
            ) {
                $cols_name=$field_def['vname'].' '.$app_strings['LBL_REQUIRED_SYMBOL'];
                $col_arr[0]=$cols_name;
                $col_arr[1]=$field_def['name'];
                $col_arr[2]=true;
            } else {
                $cols_name=$field_def['vname'];
                $col_arr[0]=$cols_name;
                $col_arr[1]=$field_def['name'];
            }
            if (! in_array($cols_name, $formattedFields)) {
                array_push($formattedFields, $col_arr);
            }
        }

        $holder = new stdClass();
        $holder->name = $b->object_name;
        $holder->fields = $formattedFields;
        $holder->moduleKnownAs = translate($b->module_name, 'LBL_MODULE_NAME');
        $holder->moduleDir = $b->module_dir;
        $holder->moduleName = $b->module_name;
        $formattedBeans[] = $holder;
    }
    return $formattedBeans;
}

/**
 * Get valid web to person modules
 * @return array
 */
function getValidWebToPersonModules(): array
{
    $superclass = 'Person';
    $modules = [];
    foreach ($GLOBALS['moduleList'] as $mod) {
        $item = BeanFactory::getBean($mod);
        if ($item && is_subclass_of($item, $superclass)) {
            $modules[] = $item->module_name;
        }
    }

    return $modules;
}

/**
 * Check if it is a valid WebToPerson module
 * @param string $module
 * @return bool
 */
function isValidWebToPersonModule(string $module): bool
{
    $validModules = getValidWebToPersonModules();

    return in_array($module, $validModules, true);
}
