<?php
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

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

/*
 Removes Relationships, input is a form POST

ARGS:
 $_REQUEST['module']; : the module associated with this Bean instance (will be used to get the class name)
 $_REQUEST['record']; : the id of the Bean instance
 $_REQUEST['linked_field']; : the linked field name of the Parent Bean
 $_REQUEST['linked_id']; : the id of the Related Bean instance to

 $_REQUEST['return_url']; : the URL to redirect to
  or use:
  1) $_REQUEST['return_id']; :
  2) $_REQUEST['return_module']; :
  3) $_REQUEST['return_action']; :
*/


require_once('include/formbase.php');

 global $beanFiles,$beanList, $db;
 $bean_name = $beanList[$_REQUEST['module']];
 require_once($beanFiles[$bean_name]);
 $focus = new $bean_name();
 if (empty($_REQUEST['linked_id']) || empty($_REQUEST['linked_field'])  || empty($_REQUEST['record'])) {
     die("need linked_field, linked_id and record fields");
 }
 $linked_field = $db->quote($_REQUEST['linked_field']);
 $record = $db->quote($_REQUEST['record']);
 $linked_id = $db->quote($_REQUEST['linked_id']);
 if ($linked_field === 'aclroles') {
     if (!ACLController::checkAccess($bean_name, 'edit', true)) {
         ACLController::displayNoAccess();
         sugar_cleanup(true);
     }
 } if ($linked_field === 'aclroles') {
     if (!ACLController::checkAccess($bean_name, 'edit', true)) {
         ACLController::displayNoAccess();
         sugar_cleanup(true);
     }
 }

$focus->retrieve($record);
if ($bean_name === 'Team') {
    $focus->remove_user_from_team($linked_id);
} else {

    // cut it off:
    $focus->load_relationship($linked_field);
    if ($focus->$linked_field->_relationship->relationship_name === 'quotes_contacts_shipto') {
        unset($focus->$linked_field->_relationship->relationship_role_column);
    }
    $focus->$linked_field->delete($record, $linked_id);
}
 if ($bean_name === 'Campaign' && $linked_field==='prospectlists') {
     $query = "SELECT email_marketing_prospect_lists.id from email_marketing_prospect_lists ";
     $query .= " left join email_marketing on email_marketing.id=email_marketing_prospect_lists.email_marketing_id";
     $query .= " where email_marketing.campaign_id='$record'";
     $query .= " and email_marketing_prospect_lists.prospect_list_id='$linked_id'";

     $result = $focus->db->query($query);
     while (($row = $focus->db->fetchByAssoc($result)) != null) {
         $del_query = " update email_marketing_prospect_lists set email_marketing_prospect_lists.deleted=1, email_marketing_prospect_lists.date_modified=" . $focus->db->convert(
             "'" . TimeDate::getInstance()->nowDb() . "'",
             'datetime'
         );
         $del_query .= " WHERE  email_marketing_prospect_lists.id='{$row['id']}'";
         $focus->db->query($del_query);
     }
     $focus->db->query($query);
 }
if ($bean_name === "Account" && $linked_field === 'leads') {
    // for Accounts-Leads non-standard relationship, after clearing account_id form Lead's bean, clear also account_name
    $focus->retrieve($record);
    $lead = BeanFactory::newBean('Leads');
    $lead->retrieve($linked_id);
    if ($focus->name === $lead->account_name) {
        $lead->account_name = '';
    }
    $lead->save();
    unset($lead);
}

if ($bean_name === "Meeting") {
    $focus->retrieve($record);
    $user = BeanFactory::newBean('Users');
    $user->retrieve($linked_id);
    if (!empty($user->id)) {  //make sure that record exists. we may have a contact on our hands.

        if ($focus->update_vcal) {
            vCal::cache_sugar_vcal($user);
        }
    }
}
if ($bean_name === "User" && $linked_field === 'eapm') {
    $eapm = BeanFactory::newBean('EAPM');
    $eapm->mark_deleted($linked_id);
}

if (!empty($_REQUEST['return_url'])) {
    $_REQUEST['return_url'] = urldecode($_REQUEST['return_url']);
}
$GLOBALS['log']->debug("deleted relationship: bean: $bean_name, linked_field: $linked_field, linked_id:$linked_id");
if (empty($_REQUEST['refresh_page'])) {
    handleRedirect();
}


exit;
