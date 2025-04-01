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








$focus = BeanFactory::newBean('ProspectLists');

$focus->retrieve($_POST['record']);

if (!empty($_POST['assigned_user_id']) && ($focus->assigned_user_id != $_POST['assigned_user_id']) && ($_POST['assigned_user_id'] != $current_user->id)) {
    $check_notify = true;
} else {
    $check_notify = false;
}

require_once('include/formbase.php');
$focus = populateFromPost('', $focus);

$focus->save($check_notify);
$return_id = $focus->id;


//Bug 33675 Duplicate target list
if (!empty($_REQUEST['duplicateId'])) {
    $copyFromProspectList = BeanFactory::newBean('ProspectLists');
    $copyFromProspectList->retrieve($_REQUEST['duplicateId']);
    $relations = $copyFromProspectList->retrieve_relationships('prospect_lists_prospects', array('prospect_list_id'=>$_REQUEST['duplicateId']), 'related_id, related_type');
    if ((is_countable($relations) ? count($relations) : 0)>0) {
        foreach ($relations as $rel) {
            $rel['prospect_list_id']=$return_id;
            $focus->set_relationship('prospect_lists_prospects', $rel, true);
        }
    }
}



if (isset($_POST['return_module']) && $_POST['return_module'] != "") {
    $return_module = $_POST['return_module'];
} else {
    $return_module = "ProspectLists";
}
if (isset($_POST['return_action']) && $_POST['return_action'] != "") {
    $return_action = $_POST['return_action'];
} else {
    $return_action = "DetailView";
}
if (isset($_POST['return_id']) && $_POST['return_id'] != "") {
    $return_id = $_POST['return_id'];
}

if ($return_action == "SaveCampaignProspectListRelationshipNew") {
    $prospect_list_id = $focus->id;
    handleRedirect($return_id, $return_module, array("prospect_list_id" => $prospect_list_id));
} else {
    //eggsurplus Bug 23816: maintain VCR after an edit/save. If it is a duplicate then don't worry about it. The offset is now worthless.
    $redirect_url = "Location: index.php?action=$return_action&module=$return_module&record=$return_id";
    if (isset($_REQUEST['offset']) && empty($_REQUEST['duplicateSave'])) {
        $redirect_url .= "&offset=".$_REQUEST['offset'];
    }
    $GLOBALS['log']->debug("Saved record with id of ".$return_id);
    handleRedirect($return_id, $return_module);
}
