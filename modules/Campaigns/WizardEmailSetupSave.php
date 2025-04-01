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


//get new administration bean for setup
$focus = BeanFactory::newBean('Administration');
$camp_steps[] = 'wiz_step_';
$camp_steps[] = 'wiz_step1_';
$camp_steps[] = 'wiz_step2_';

    //name is used as key in post, it is also used in creation of summary page for wizard,
    //so let's clean up the posting so we can reuse the save functionality for inbound emails and
    //from existing save.php's
    foreach ($camp_steps as $step) {
        clean_up_post($step);
    }
/**************************** Save general Email Setup  *****************************/

//we do not need to track location if location type is not set
if (isset($_POST['tracking_entities_location_type'])) {
    if ($_POST['tracking_entities_location_type'] != '2') {
        unset($_POST['tracking_entities_location']);
        unset($_POST['tracking_entities_location_type']);
    }
}
//if the check box is empty, then set it to 0
if (!isset($_POST['mail_smtpauth_req'])) {
    $_POST['mail_smtpauth_req'] = 0;
}
//default ssl use to false
if (!isset($_POST['mail_smtpssl'])) {
    $_POST['mail_smtpssl'] = 0;
}
//reuse existing saveconfig functionality
$focus->saveConfig();



/**************************** Add New Monitored Box  *****************************/
//perform this if the option to create new mail box has been checked
if (isset($_REQUEST['wiz_new_mbox']) && ($_REQUEST['wiz_new_mbox']=='1')) {
    
   //Populate the Request variables that inboundemail expects
    $_REQUEST['mark_read'] = 1;
    $_REQUEST['only_since'] = 1;
    $_REQUEST['mailbox_type'] = 'bounce';
    $_REQUEST['from_name'] = $_REQUEST['name'];
    $_REQUEST['group_id'] = 'new';
//    $_REQUEST['from_addr'] = $_REQUEST['wiz_step1_notify_fromaddress'];
    //reuse save functionality for inbound email
    require_once('modules/InboundEmail/Save.php');
}
    if (!empty($_REQUEST['error'])) {
        //an error was found during inbound save.  This means the save was allowed but the inbound box had problems, return user to wizard
        //and display error message
        $header_URL = "Location: index.php?action=WizardEmailSetup&module=Campaigns&error=true";
        SugarApplication::headerRedirect($header_URL);
    } else {
        //set navigation details
        $header_URL = "Location: index.php?action=index&module=Campaigns";
        SugarApplication::headerRedirect($header_URL);
    }

/*
 * This function will re-add the post variables that exist with the specified prefix.
 * It will add them minus the specified prefix.  This is needed in order to reuse the save functionality,
 * which does not expect the prefix, and still use the generic create summary functionality in wizard, which
 * does expect the prefix.
 */
function clean_up_post($prefix)
{
    foreach ($_REQUEST as $key => $val) {
        if ((strstr($key, (string) $prefix)) && (strpos($key, (string) $prefix)== 0)) {
            $newkey  =substr($key, strlen((string) $prefix)) ;
            $_REQUEST[$newkey] = $val;
        }
    }

    foreach ($_POST as $key => $val) {
        if ((strstr($key, (string) $prefix)) && (strpos($key, (string) $prefix)== 0)) {
            $newkey  =substr($key, strlen((string) $prefix)) ;
            $_POST[$newkey] = $val;
        }
    }
}
