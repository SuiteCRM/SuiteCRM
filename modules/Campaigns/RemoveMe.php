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



require_once('modules/Campaigns/utils.php');

if (!empty($_REQUEST['remove'])) {
    clean_string($_REQUEST['remove'], "STANDARD");
}
if (!empty($_REQUEST['from'])) {
    clean_string($_REQUEST['from'], "STANDARD");
}

/*if the request is a POST request, and parameters are as expected, then it is a one click unsubscribe request.
 *
 * Must handle this POST format:
POST /index.php?entryPoint=removeme&identifier=UUID HTTP/1.1
   Host: crm.domain.tld
   Content-Type: application/x-www-form-urlencoded
   Content-Length: 26

   List-Unsubscribe=One-Click
 */
function isValidUUID( $uuid ) {
    // UUID is a 36-character string,  8-4-4-4-12 hex digits, e.g.:  550e8400-e29b-41d4-a716-446655440000
    return (preg_match('/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/', $uuid) === 1);
}
$isContentType =  (!empty($_SERVER['CONTENT_TYPE']) && $_SERVER['CONTENT_TYPE'] == 'application/x-www-form-urlencoded');
$isOneClickUnsubscribe = (($_SERVER['REQUEST_METHOD'] === 'POST') && !empty($_REQUEST['List-Unsubscribe']) && ($_REQUEST['List-Unsubscribe'] == 'One-Click'));
$isRemoveMe = ((!empty($_REQUEST['entryPoint']) && ($_REQUEST['entryPoint']) == 'removeme'));
$isIdentifier = ((!empty($_REQUEST['identifier'])) && isValidUUID($_REQUEST['identifier']));
$isPOST = ($_SERVER['REQUEST_METHOD'] === 'POST');

// Handle a typical "POST removeme Identifier OneClickUnsubscribe" opt-out.
if ( $isOneClickUnsubscribe && $isRemoveMe && $isIdentifier ) {
    global $beanFiles, $beanList, $current_user, $log, $current_language;

    //user is most likely not defined, retrieve admin user so that team queries are bypassed
    if (empty($current_user) || empty($current_user->id)) {
        $current_user = BeanFactory::newBean('Users');
        $current_user->retrieve('1');
    }
    
    $keys=log_campaign_activity($_REQUEST['identifier'], 'removed');
    $mod_strings = return_module_language($current_language, 'Campaigns');

    if (!empty($keys) && $keys['target_type'] == 'Users') {
        //Users cannot opt out of receiving emails, print out warning message.
        echo $mod_strings['LBL_USERS_CANNOT_OPTOUT'];
    } elseif (!empty($keys) && isset($keys['campaign_id']) && !empty($keys['campaign_id'])) {
        //we need to unsubscribe the user from this particular campaign
        $beantype = $beanList[$keys['target_type']];
        require_once($beanFiles[$beantype]);
        $focus = new $beantype();
        $focus->retrieve($keys['target_id']);
        unsubscribe($keys['campaign_id'], $focus);
    } elseif (!empty($keys)) {
        $id = $keys['target_id'];
        $module = trim($keys['target_type']);
        $class = $beanList[$module];
        require_once($beanFiles[$class]);
        $mod = new $class();
        $db = DBManagerFactory::getInstance();

        $id = $db->quote($id);

        //no opt out for users.
        if (preg_match('/^[0-9A-Za-z\-]*$/', (string) $id) && $module != 'Users') {
            //record this activity in the campaign log table..
            $query = "UPDATE email_addresses SET email_addresses.opt_out = 1 WHERE EXISTS(SELECT 1 FROM email_addr_bean_rel ear WHERE ear.bean_id = '$id' AND ear.deleted=0 AND email_addresses.id = ear.email_address_id)";
            $status=$db->query($query);
            if ($status) {
                echo "*";
            }
        }
    }
    //Print Confirmation Message.
    echo $mod_strings['LBL_ELECTED_TO_OPTOUT'];
}

if (!$isPOST &&  $isRemoveMe && $isIdentifier)
{
    // output simple html page containing an Unsubscribe button, which sends the Unsubscribe request back to here, the  "removeme" entryPoint.
    $identifier = $_REQUEST['identifier'];
    $UnsubscribePOSTURL = $_SERVER['APP_URL'].$_SERVER['REQUEST_URI']; // request URL should contain the query parameters ?entryPoint=removeme&identifier=xxxxxxxxxxx
    // TODO: localize Title and Unsubscribe Label on button.
    $OptOutOfEmails = "Opt Out of receiving Emails from this Campaign or Newsletter?";
    $unsubscribe = "Unsubscribe";   // button label.
    // TODO: If possible, display the public facing name of the Campaign or Newsletter on the page.
    // TODO: Use a .tpl file (and the organization-uploaded graphic logo) instead of hardcoding the HTML.
    echo("<html xml:lang='en' lang='en'>" .
        "<head>" .
        "<meta charset='UTF-8'>" .
        "<title>$OptOutOfEmails</title>" .
        "</head>" .
        "<body>" .
        "<h1>" . $OptOutOfEmails . "</h1><br/>" .
        "<form method='POST' action='$UnsubscribePOSTURL'>" .
        "<input type='hidden' name='entryPoint' value='removeme'>" .
        "<input type='hidden' name='identifier' value='$identifier'>" .
        "<input type='hidden' name='List-Unsubscribe' value='One-Click'>" .
        "<input type='submit' value='$unsubscribe'>" .
        "</form>" .
        "</body>" .
        "</html>");
}
sugar_cleanup();
