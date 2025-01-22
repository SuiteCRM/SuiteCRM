<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2019 SalesAgility Ltd.
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
       die('Not A Valid Entry Point');
}

global $mod_strings,$app_list_strings,$app_strings,$current_user;

require_once __DIR__ . "/../../include/Imap/ImapHandlerFactory.php";

if (!is_admin($current_user)&& !is_admin_for_module($GLOBALS['current_user'], 'Campaigns')) {
    sugar_die("Unauthorized access to administration.");
}

$params = array();
$params[] = "<a href='index.php?module=Campaigns&action=index'>{$mod_strings['LBL_MODULE_NAME']}</a>";
$params[] = $mod_strings['LBL_EMAIL_SETUP_WIZARD_TITLE'];

echo getClassicModuleTitle('Campaigns', $params, true);


global $theme, $currentModule, $sugar_config;





//get administration bean for email setup
$focus = BeanFactory::newBean('Administration');
$focus->retrieveSettings(); //retrieve all admin settings.
$GLOBALS['log']->info("Mass Emailer(EmailMan) ConfigureSettings view");
$email = BeanFactory::newBean('Emails');
$ss = new Sugar_Smarty();
$ss->assign("MOD", $mod_strings);
$ss->assign("APP", $app_strings);
if (isset($_REQUEST['return_module'])) {
    $ss->assign("RETURN_MODULE", $_REQUEST['return_module']);
}
if (isset($_REQUEST['return_action'])) {
    $ss->assign("RETURN_ACTION", $_REQUEST['return_action']);
}
if (isset($_REQUEST['return_id'])) {
    $ss->assign("RETURN_ID", $_REQUEST['return_id']);
}



/******** Email Setup UI DIV Stuff **********/
//get Settings if they exist
$ss->assign("notify_fromaddress", $focus->settings['notify_fromaddress']);
$ss->assign("notify_send_from_assigning_user", ($focus->settings['notify_send_from_assigning_user']) ? "checked='checked'" : "");
$ss->assign("notify_on", ($focus->settings['notify_on']) ? "checked='checked'" : "");
$ss->assign("notify_fromname", $focus->settings['notify_fromname']);
$ss->assign("mail_smtpserver", $focus->settings['mail_smtpserver']);
$ss->assign("mail_smtpport", $focus->settings['mail_smtpport']);
$ss->assign("mail_sendtype_options", get_select_options_with_id($app_list_strings['notifymail_sendtype'], $focus->settings['mail_sendtype']));
$ss->assign("mail_smtpuser", $focus->settings['mail_smtpuser']);
$ss->assign("mail_smtppass", $focus->settings['mail_smtppass']);
$ss->assign("mail_smtpauth_req", ($focus->settings['mail_smtpauth_req']) ? "checked='checked'" : "");

$protocol = filterInboundEmailPopSelection($app_list_strings['dom_email_server_type']);
$ss->assign('PROTOCOL', get_select_options_with_id($protocol, ''));
if (isset($focus->settings['massemailer_campaign_emails_per_run']) && !empty($focus->settings['massemailer_campaign_emails_per_run'])) {
    $ss->assign("EMAILS_PER_RUN", $focus->settings['massemailer_campaign_emails_per_run']);
} else {
    $ss->assign("EMAILS_PER_RUN", 500);
}

if (!isset($focus->settings['massemailer_tracking_entities_location_type']) || empty($focus->settings['massemailer_tracking_entities_location_type']) || $focus->settings['massemailer_tracking_entities_location_type']=='1') {
    $ss->assign("DEFAULT_CHECKED", "checked");
    $ss->assign("TRACKING_ENTRIES_LOCATION_STATE", "disabled");
    $ss->assign("TRACKING_ENTRIES_LOCATION", $mod_strings['TRACKING_ENTRIES_LOCATION_DEFAULT_VALUE']);
} else {
    $ss->assign("USERDEFINED_CHECKED", "checked");
    $ss->assign("TRACKING_ENTRIES_LOCATION", $focus->settings["massemailer_tracking_entities_location"]);
}

$ss->assign("SITEURL", $sugar_config['site_url']);

// Change the default campaign to not store a copy of each message.
if (!empty($focus->settings['massemailer_email_copy']) && $focus->settings['massemailer_email_copy']=='1') {
    $ss->assign("YES_CHECKED", "checked='checked'");
} else {
    $ss->assign("NO_CHECKED", "checked='checked'");
}

$ss->assign("MAIL_SSL_OPTIONS", get_select_options_with_id($app_list_strings['email_settings_for_ssl'], $focus->settings['mail_smtpssl']));


/*********** New Mail Box UI DIV Stuff ****************/
$mbox_qry = "select * from inbound_email where deleted ='0' and mailbox_type = 'bounce'";
$mbox_res = $focus->db->query($mbox_qry);
$mbox = [];
while ($mbox_row = $focus->db->fetchByAssoc($mbox_res)) {
    $mbox[] = $mbox_row;
}
$mbox_msg = ' ';
$need_mbox = '';

$mboxTable = "<table class='list view' width='100%' border='0' cellspacing='1' cellpadding='1'>";
$mboxCount =  is_countable($mbox) ? count($mbox) : 0;
if (isset($mbox) && $mboxCount>0) {
    $mboxTable .= "<tr><td colspan='5'><b>" . $mboxCount ." ". $mod_strings['LBL_MAILBOX_CHECK_WIZ_GOOD']." </b>.</td></tr>";
    $mboxTable .= "<tr class='listViewHRS1'><td width='20%'><b>".$mod_strings['LBL_MAILBOX_NAME']."</b></td>"
                   .  " <td width='20%'><b>".$mod_strings['LBL_LOGIN']."</b></td>"
                   .  " <td width='20%'><b>".$mod_strings['LBL_MAILBOX']."</b></td>"
                   .  " <td width='20%'><b>".$mod_strings['LBL_SERVER_URL']."</b></td>"
                   .  " <td width='20%'><b>".$mod_strings['LBL_LIST_STATUS']."</b></td></tr>";
    $colorclass=' ';
    foreach ($mbox as $details) {
        if ($colorclass == "class='evenListRowS1'") {
            $colorclass= "class='oddListRowS1'";
        } else {
            $colorclass= "class='evenListRowS1'";
        }

        $mboxTable .= "<tr $colorclass>";
        $mboxTable .= "<td>".$details['name']."</td>";
        $mboxTable .= "<td>".$details['email_user']."</td>";
        $mboxTable .= "<td>".$details['mailbox']."</td>";
        $mboxTable .= "<td>".$details['server_url']."</td>";
        $mboxTable .= "<td>".$details['status']."</td></tr>";
    }
} else {
    $need_mbox = 'checked';
    $mboxTable .= "<tr><td colspan='5'><b>".$mod_strings['LBL_MAILBOX_CHECK_WIZ_BAD']." </b>.</td></tr>";
}
$mboxTable .= "</table>";
$ss->assign("MAILBOXES_DETECTED_MESSAGE", $mboxTable);
$ss->assign("MBOX_NEEDED", $need_mbox);
$ss->assign('ROLLOVER', $email->rolloverStyle);



$imapFactory = new ImapHandlerFactory();
if (!$imapFactory->areAllHandlersAvailable()) {
    $ss->assign('IE_DISABLED', 'DISABLED');
}
/**************************** SUMMARY UI DIV Stuff *******************/

/**************************** WIZARD UI DIV Stuff *******************/

//  this is the wizard control script that resides in page
 $divScript = <<<EOQ
 <script type="text/javascript" language="javascript">

    //this function toggles visibility of fields based on selected options
    function notify_setrequired() {
        f = document.getElementById("wizform");
        document.getElementById("smtp_settings").style.display = (f.mail_sendtype.value == "SMTP") ? "inline" : "none";
        document.getElementById("smtp_settings").style.visibility = (f.mail_sendtype.value == "SMTP") ? "visible" : "hidden";
        document.getElementById("smtp_auth").style.display = (document.getElementById('mail_smtpauth_req').checked) ? "inline" : "none";
        document.getElementById("smtp_auth").style.visibility = (document.getElementById('mail_smtpauth_req').checked) ? "visible" : "hidden";
        document.getElementById("new_mbox").style.display = (document.getElementById('create_mbox').checked) ? "inline" : "none";
        document.getElementById("new_mbox").style.visibility = (document.getElementById('create_mbox').checked) ? "visible" : "hidden";
        document.getElementById("wiz_new_mbox").value = (document.getElementById('create_mbox').checked) ? "1" : "0";
        return true;
    }

    //this function will copy as much information as possible from the first step in wizard
    //onto the second step in wizard
    function copy_down() {
        document.getElementById("name").value = document.getElementById("notify_fromname").value;
        document.getElementById("email_user").value = document.getElementById("notify_fromaddress").value;
        document.getElementById("from_addr").value = document.getElementById("notify_fromaddress").value;
        if(document.getElementById("mail_sendtype").value=='SMTP'){
            document.getElementById("protocol").value = "SMTP";
            document.getElementById("server_url").value = document.getElementById("mail_smtpserver").value;
            if(document.getElementById('mail_smtpauth_req').checked){
                document.getElementById("email_user").value = document.getElementById("mail_smtpuser").value;
                document.getElementById("email_password").value = document.getElementById("mail_smtppass").value;
            }

        }
        return true;
    }

    //this calls the validation functions for each step that needs validation
    function validate_wiz_form(step){
        switch (step){
            case 'step1':
            if(!validate_step1()){return false;}
              copy_down();
            break;
            case 'step2':
           if(!validate_step2()){return false;}
            break;
            default://no additional validation needed
        }
        return true;

    }

    //this function will add validation to step1
    function validate_step1(){
        requiredTxt = SUGAR.language.get('app_strings', 'ERR_MISSING_REQUIRED_FIELDS');
        var haserrors = 0;
        var fields = new Array();

        //create list of fields that need validation, based on selected options
        fields[0] = 'notify_fromname';
        fields[1] = 'notify_fromaddress';
        fields[2] = 'massemailer_campaign_emails_per_run';
        fields[3] = 'massemailer_tracking_entities_location';
        if(document.getElementById("mail_sendtype").value=='SMTP'){
            fields[4] = 'mail_smtpserver';
            fields[5] = 'mail_smtpport';
                if(document.getElementById('mail_smtpauth_req').checked){
                    fields[6] = 'mail_smtpuser';
                    fields[7] = 'mail_smtppass';
                }
        }

        var field_value = '';
        //iterate through required fields and set empty string values ('  '') to null, this will cause failure later on
        for (i=0; i < fields.length; i++){
            elem = document.getElementById(fields[i]);
            field_value = trim(elem.value);
            if(field_value.length<1){
                elem.value = '';
            }
        }
        //add to generic validation and call function to calidate
        if(validate['wizform']!='undefined'){delete validate['wizform']};
        addToValidate('wizform', 'notify_fromaddress', 'email', true,  document.getElementById('notify_fromaddress').title);
        addToValidate('wizform', 'notify_fromname', 'alphanumeric', true,  document.getElementById('notify_fromname').title);
        addToValidate('wizform', 'massemailer_campaign_emails_per_run', 'int', true,  document.getElementById('massemailer_campaign_emails_per_run').title);
        addToValidate('wizform', 'massemailer_tracking_entities_location', 'alphanumeric', true,  document.getElementById('massemailer_tracking_entities_location').title);
        if(document.getElementById("mail_sendtype").value=='SMTP'){
            addToValidate('wizform', 'mail_smtpserver', 'alphanumeric', true,  document.getElementById('mail_smtpserver').title);
            addToValidate('wizform', 'mail_smtpport', 'int', true,  document.getElementById('mail_smtpport').title);
                if(document.getElementById('mail_smtpauth_req').checked){
                    addToValidate('wizform', 'mail_smtpuser', 'alphanumeric', true,  document.getElementById('mail_smtpuser').title);
                    addToValidate('wizform', 'mail_smtppass', 'alphanumeric', true,  document.getElementById('mail_smtppass').title);
                }
        }


        return check_form('wizform');
    }



    function validate_step2(){
        requiredTxt = SUGAR.language.get('app_strings', 'ERR_MISSING_REQUIRED_FIELDS');
        //validate only if the create mailbox form input has been selected
        if(document.getElementById("wiz_new_mbox").value == "0"){
         //this form is not checked, do not validate
         return true;
        }
        var haserrors = 0;
        var wiz_message = document.getElementById('wiz_message');

        //create list of fields that need validation, based on selected options
        var fields = new Array();
        fields[0] = 'name';
        fields[1] = 'server_url';
        fields[2] = 'email_user';
        fields[3] = 'protocol';
        fields[4] = 'email_password';
        fields[5] = 'mailbox';
        fields[6] = 'port';

        //iterate through required fields and set empty string values ('  '') to null, this will cause failure later on
        var field_value = '';
        for (i=0; i < fields.length; i++){
            field_value = trim(document.getElementById(fields[i]).value);
            if(field_value.length<1){
                add_error_style('wizform', fields[i], requiredTxt +' ' +document.getElementById(fields[i]).title );
                haserrors = 1;
            }
        }

        //add to generic validation and call function to calidate
        if(validate['wizform']!='undefined'){delete validate['wizform']};
        addToValidate('wizform', 'name', 'alphanumeric', true,  document.getElementById('name').title);
        addToValidate('wizform', 'server_url', 'alphanumeric', true,  document.getElementById('server_url').title);
        addToValidate('wizform', 'email_user', 'alphanumeric', true,  document.getElementById('email_user').title);
        addToValidate('wizform', 'email_password', 'alphanumeric', true,  document.getElementById('email_password').title);
        addToValidate('wizform', 'mailbox', 'alphanumeric', true,  document.getElementById('mailbox').title);
        addToValidate('wizform', 'protocol', 'alphanumeric', true,  document.getElementById('protocol').title);
        addToValidate('wizform', 'port', 'int', true,  document.getElementById('port').title);

        if(haserrors == 1){
            return false;
        }
        return check_form('wizform');


    }

    /*
     * The generic create summary will not work for this wizard, as we have a step that only gets
     * displayed if a check box is marked, and we also have certain inputs we do not want displayed(ie. password)
     * so this function will override the genereic version
     */
    function create_summary(){
        var current_step = document.getElementById('wiz_current_step');
        var currentValue = parseInt(current_step.value);
        var temp_elem = '';

        //  alert(test.title);alert(test.name);alert(test.id);
        var fields = new Array();
        //create the list of fields to create summary table
        fields[0] = 'notify_fromname';
        fields[1] = 'notify_fromaddress';
        fields[2] = 'massemailer_campaign_emails_per_run';
        fields[3] = 'massemailer_tracking_entities_location';

         if(document.getElementById("mail_sendtype").value=='SMTP'){
              fields[4] = 'mail_smtpserver';
              fields[5] = 'mail_smtpport';
                 if(document.getElementById('mail_smtpauth_req').checked){
                      fields[6] = 'mail_smtpuser';
                 }
              fields[7] = 'mail_smtpssl';
          }

          if(document.getElementById("wiz_new_mbox").value != "0"){
                fields[8] = 'name';
                fields[9] = 'server_url';
                fields[10] = 'email_user';
                fields[11] = 'from_addr';
                fields[12] = 'protocol';
                fields[13] = 'mailbox';
                fields[14] = 'port';
                fields[15] = 'ssl';
          }

        //iterate through list and create table
        var summhtml = "<table class='detail view' width='100%' border='0' cellspacing='1' cellpadding='1'>";
        var colorclass = 'tabDetailViewDF2';
        var elem ='';
        for (var i=0; i<fields.length; i++)
          {elem = document.getElementById(fields[i]);
            if(elem!=null){
                if(elem.type.indexOf('select') >= 0 ){
                    var selInd = elem.selectedIndex;
                    if(selInd<0){selInd =0;}
                    summhtml = summhtml+ "<tr  class='"+colorclass+"' ><td scope='row'  width='15%'><b>" +elem.title+ "</b></td><td class='tabDetailViewDF'  width='30%'>" + YAHOO.lang.escapeHTML(elem.options[selInd].text)+ "&nbsp;</td></tr>";
                }else if(elem.type == 'checked'){
                    summhtml = summhtml+ "<tr  class='"+colorclass+"' ><td scope='row'  width='15%'><b>" +elem.title+ "</b></td><td class='tabDetailViewDF'  width='30%'>" + YAHOO.lang.escapeHTML(elem.value) + "&nbsp;</td></tr>";
                }else if(elem.type == 'checkbox'){
                    if(elem.checked){
                        summhtml = summhtml+ "<tr  class='"+colorclass+"' ><td scope='row'  width='15%'><b>" +elem.title+ "</b></td><td class='tabDetailViewDF'  width='30%'><input type='checkbox' class='checkbox' disabled checked>&nbsp;</td></tr>";
                    }else{
                        summhtml = summhtml+ "<tr  class='"+colorclass+"' ><td scope='row'  width='15%'><b>" +elem.title+ "</b></td><td class='tabDetailViewDF'  width='30%'><input type='checkbox' class='checkbox' disabled >&nbsp;</td></tr>";
                    }
                }else{
                    summhtml = summhtml+ "<tr  class='"+colorclass+"' ><td scope='row'  width='15%'><b>" +elem.title+ "</b></td><td class='tabDetailViewDF'  width='30%'>" + YAHOO.lang.escapeHTML(elem.value) + "&nbsp;</td></tr>";
                }
            }
            if( colorclass== 'tabDetailViewDL2'){
                colorclass= 'tabDetailViewDF2';
            }else{
                colorclass= 'tabDetailViewDL2'
            }
          }

        summhtml = summhtml+ "</table>";
        temp_elem = document.getElementById('wiz_summ');
        temp_elem.innerHTML = summhtml;

    }

    showfirst('email');
    notify_setrequired();




</script>
EOQ;

if (isset($_REQUEST['error'])) {
    //if there is an error flagged, then we are coming here after a save where there was an error detected
    //on an inbound email save.  Display error to user so they are aware.
    $errorString = "<div class='error'>".$mod_strings['ERR_NO_OPTS_SAVED']."  <a href='index.php?module=InboundEmail&action=index'>".$mod_strings['ERR_REVIEW_EMAIL_SETTINGS']."</a></div>";
    $ss->assign('ERROR', $errorString);
    //navigate to inbound email page by default
    $divScript .=" <script>navigate('next');</script>";
}

$ss->assign("DIV_JAVASCRIPT", $divScript);


/**************************** FINAL END OF PAGE UI Stuff *******************/

//$ss->assign("JAVASCRIPT", get_validate_record_js());

$ss->display('modules/Campaigns/WizardEmailSetup.html');
