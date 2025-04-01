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

require_once 'include/SugarPHPMailer.php';

global $sugar_config;

$configurator = new Configurator();
$confirmOptInEnabled = $configurator->isConfirmOptInEnabled();

$test = false;
if (isset($_REQUEST['mode']) && $_REQUEST['mode'] == 'test') {
    $test = true;
}
if (isset($_REQUEST['send_all']) && $_REQUEST['send_all']) {
    $send_all = true;
} else {
    $send_all = false; //if set to true email delivery will continue..to run until all email have been delivered.
}

if (!isset($GLOBALS['log'])) {
    $GLOBALS['log'] = LoggerManager::getLogger();
}

$mail = new SugarPHPMailer();
$admin = BeanFactory::newBean('Administration');
$admin->retrieveSettings();
if (isset($admin->settings['massemailer_campaign_emails_per_run'])) {
    $max_emails_per_run = $admin->settings['massemailer_campaign_emails_per_run'];
}
if (empty($max_emails_per_run)) {
    $max_emails_per_run = 500; //default
}
//save email copies?
$massemailer_email_copy = 0;  //default: save copies of the email.
if (isset($admin->settings['massemailer_email_copy'])) {
    $massemailer_email_copy = $admin->settings['massemailer_email_copy'];
}

$emailsPerSecond = 10;

$mail->setMailerForSystem();
$mail->From = "no-reply@example.com";
$mail->FromName = "no-reply";
$mail->ContentType = "text/html";

$campaign_id = null;
if (isset($_REQUEST['campaign_id']) && !empty($_REQUEST['campaign_id'])) {
    $campaign_id = $_REQUEST['campaign_id'];
}

$db = DBManagerFactory::getInstance();
$timedate = TimeDate::getInstance();
$emailman = BeanFactory::newBean('EmailMan');

if ($test) {
    //if this is in test mode, then
    //find all the message that meet the following criteria.
    //1. scheduled send date time is now
    //2. campaign matches the current campaign
    //3. recipient belongs to a prospect list of type test, attached to this campaign

    $select_query = " SELECT em.* FROM emailman em";
    $select_query .= " join prospect_list_campaigns plc on em.campaign_id = plc.campaign_id";
    $select_query .= " join prospect_lists pl on pl.id = plc.prospect_list_id ";
    $select_query .= " WHERE em.list_id = pl.id and pl.list_type = 'test'";
    $select_query .= " AND pl.deleted = 0 AND plc.deleted = 0 AND em.deleted = 0";
    $select_query .= " AND em.send_date_time <= " . $db->now();
    $select_query .= " AND (em.in_queue ='0' OR em.in_queue IS NULL OR (em.in_queue ='1' AND em.in_queue_date <= " . $db->convert($db->quoted($timedate->fromString("-1 day")->asDb()), "datetime") . "))";
    $select_query .= " AND em.campaign_id='{$campaign_id}'";
    $select_query .= " ORDER BY em.send_date_time ASC, em.user_id, em.list_id";
} else {
    //this is not a test..
    //find all the message that meet the following criteria.
    //1. scheduled send date time is now
    //2. were never processed or last attempt was 24 hours ago
    $select_query = " SELECT *";
    $select_query .= " FROM $emailman->table_name";
    $select_query .= " WHERE send_date_time <= " . $db->now();
    $select_query .= " AND deleted = 0";
    $select_query .= " AND (in_queue ='0' OR in_queue IS NULL OR ( in_queue ='1' AND in_queue_date <= " . $db->convert($db->quoted($timedate->fromString("-1 day")->asDb()), "datetime") . ")) " . ($confirmOptInEnabled ? ' OR related_confirm_opt_in = 1 ' : ' AND related_confirm_opt_in = 0');

    if (!empty($campaign_id)) {
        $select_query .= " AND campaign_id='{$campaign_id}'";
    }
    $select_query .= " ORDER BY send_date_time ASC,user_id, list_id";
}

//bug 26926 fix start
DBManager::setQueryLimit(0);
//end bug fix

do {
    $no_items_in_queue = true;

    $result = $db->limitQuery($select_query, 0, $max_emails_per_run);
    global $current_user;
    if (isset($current_user)) {
        $temp_user = $current_user;
    }
    $current_user = BeanFactory::newBean('Users');
    $startTime = microtime(true);


    // for testing
    $count = 0;
    while ($row = $db->fetchByAssoc($result)) {
        //verify the queue item before further processing.
        //we have found cases where users have taken away access to email templates while them message is in queue.
        if ((empty($row['related_confirm_opt_in']) || $row['related_confirm_opt_in'] == '0') && empty($row['campaign_id'])) {
            $GLOBALS['log']->fatal('Skipping emailman entry with empty campaign id' . print_r($row, true));
            continue;
        }
        if ((empty($row['related_confirm_opt_in']) || $row['related_confirm_opt_in'] == '0') && empty($row['marketing_id'])) {
            $GLOBALS['log']->fatal('Skipping emailman entry with empty marketing id' . print_r($row, true));
            continue;  //do not process this row .
        }

        //fetch user that scheduled the campaign.
        if (empty($current_user) || $row['user_id'] != $current_user->id) {
            $current_user->retrieve($row['user_id']);
        }

        if ((empty($row['related_confirm_opt_in']) || $row['related_confirm_opt_in'] == '0') && !$emailman->verify_campaign($row['marketing_id'])) {
            $GLOBALS['log']->fatal('Error verifying templates for the campaign, exiting');
            continue;
        }

        //verify the email template too..
        //find the template associated with marketing message. make sure that template has a subject and
        //a non-empty body
        if (!isset($template_status[$row['marketing_id']])) {
            $current_emailmarketing = BeanFactory::newBean('EmailMarketing');
            $current_emailmarketing->retrieve($row['marketing_id']);

            $current_emailtemplate = BeanFactory::newBean('EmailTemplates');
            $current_emailtemplate->retrieve($current_emailmarketing->template_id);
        }

        $no_items_in_queue = false;

        foreach ($row as $name => $value) {
            $emailman->$name = $value;
        }

        //for the campaign process the suppression lists.
        if (!isset($current_campaign_id) || empty($current_campaign_id) || $current_campaign_id != $row['campaign_id']) {
            $current_campaign_id = $row['campaign_id'];

            //is this email address suppressed?
            $plc_query = " SELECT prospect_list_id, prospect_lists.list_type,prospect_lists.domain_name FROM prospect_list_campaigns ";
            $plc_query .= " LEFT JOIN prospect_lists on prospect_lists.id = prospect_list_campaigns.prospect_list_id";
            $plc_query .= " WHERE ";
            $plc_query .= " campaign_id='{$current_campaign_id}' ";
            $plc_query .= " AND prospect_lists.list_type in ('exempt_address','exempt_domain')";
            $plc_query .= " AND prospect_list_campaigns.deleted=0";
            $plc_query .= " AND prospect_lists.deleted=0";

            $emailman->restricted_domains = array();
            $emailman->restricted_addresses = array();

            $result1 = $db->query($plc_query);
            while ($row1 = $db->fetchByAssoc($result1)) {
                if ($row1['list_type'] == 'exempt_domain') {
                    $emailman->restricted_domains[strtolower($row1['domain_name'])] = 1;
                } else {
                    //find email address of targets in this prospect list.
                    $email_query = "SELECT email_address FROM email_addresses ea JOIN email_addr_bean_rel eabr ON ea.id = eabr.email_address_id JOIN prospect_lists_prospects plp ON eabr.bean_id = plp.related_id AND eabr.bean_module = plp.related_type AND plp.prospect_list_id = '{$row1['prospect_list_id']}' and plp.deleted = 0";
                    $email_query_result = $db->query($email_query);

                    while ($email_address = $db->fetchByAssoc($email_query_result)) {
                        if (!empty($email_address['email_address'])) {
                            $emailman->restricted_addresses[strtolower($email_address['email_address'])] = 1;
                        }
                    }
                }
            }
        }

        // if user want to use an other outbound email account to sending...
        if ($current_emailmarketing->outbound_email_id) {
            $outboundEmailAccount = BeanFactory::getBean('OutboundEmailAccounts', $current_emailmarketing->outbound_email_id);

            if (strtolower($outboundEmailAccount->mail_sendtype) === 'smtp') {
                $mail->Mailer = 'smtp';
                $mail->Host = $outboundEmailAccount->mail_smtpserver;
                $mail->Port = $outboundEmailAccount->mail_smtpport;
                if ($outboundEmailAccount->mail_smtpssl == 1) {
                    $mail->SMTPSecure = 'ssl';
                } elseif ($outboundEmailAccount->mail_smtpssl == 2) {
                    $mail->SMTPSecure = 'tls';
                } else {
                    $mail->SMTPSecure = '';
                }
                if ($outboundEmailAccount->mail_smtpauth_req) {
                    $mail->SMTPAuth = true;
                    $mail->Username = $outboundEmailAccount->mail_smtpuser;
                    $mail->Password = $outboundEmailAccount->mail_smtppass;
                } else {
                    $mail->SMTPAuth = false;
                    $mail->Username = '';
                    $mail->Password = '';
                }
            } else {
                $mail->Mailer = 'sendmail';
            }

            $mail->oe->mail_smtpauth_req = $outboundEmailAccount->mail_smtpauth_req;
            $mail->oe->mail_smtpuser = $outboundEmailAccount->mail_smtpuser;
            $mail->oe->mail_smtppass = $outboundEmailAccount->mail_smtppass;
            $mail->oe->mail_smtpserver = $outboundEmailAccount->mail_smtpserver;
            $mail->oe->mail_smtpport = $outboundEmailAccount->mail_smtpport;
            $mail->oe->mail_smtpssl = $outboundEmailAccount->mail_smtpssl;
        }

        if ((empty($row['related_confirm_opt_in']) || $row['related_confirm_opt_in'] == '0')) {
            if (!$emailman->sendEmail($mail, $massemailer_email_copy, $test)) {
                $GLOBALS['log']->fatal("Email delivery FAILURE:" . print_r($row, true));
            } else {
                $GLOBALS['log']->debug("Email delivery SUCCESS:" . print_r($row, true));
            }
        } else {
            if ($confirmOptInEnabled) {
                $emailAddress = BeanFactory::newBean('EmailAddresses');
                $emailAddress->email_address = $emailAddress->getAddressesByGUID($row['related_id'], $row['related_type']);

                $now = TimeDate::getInstance()->nowDb();

                if (!$emailman->sendOptInEmail($emailAddress, $row['related_type'], $row['related_id'])) {
                    $GLOBALS['log']->fatal("Confirm Opt In Email delivery FAILURE:" . print_r($row, true));
                    $emailAddress->confirm_opt_in_fail_date = $now;
                } else {
                    $GLOBALS['log']->debug("Confirm Opt In Email delivery SUCCESS:" . print_r($row, true));

                    if (is_string($emailAddress->email_address)) {
                        $emailAddressString = $emailAddress->email_address;
                    } elseif (is_array($emailAddress->email_address) && is_string($emailAddress->email_address[0]['email_address'])) {
                        $emailAddressString = $emailAddress->email_address[0]['email_address'];
                    } else {
                        $log->fatal('Incorrect Email Address');
                        return false;
                    }

                    $emailAddress->retrieve($emailAddressString);
                    $emailAddress->confirm_opt_in_sent_date = $now;
                    $emailAddress->save();
                    $emailman->retrieve_by_string_fields(array(
                        'related_id' => $emailman->related_id,
                        'related_confirm_opt_in' => '1',
                    ));
                    $emailman->mark_deleted($emailman->id);
                }
            } else {
                $log->warn('Confirm Opt In email in queue but Confirm Opt In is disabled.');
            }
        }

        if ($mail->isError()) {
            $GLOBALS['log']->fatal("Email delivery error:" . print_r($row, true) . $mail->ErrorInfo);
        }
        $count++;
    }

    $send_all = $send_all ? !$no_items_in_queue : $send_all;
} while ($send_all);

if (isSmtp($admin->settings['mail_sendtype'] ?? '')) {
    $mail->SMTPClose();
}
if (isset($temp_user)) {
    $current_user = $temp_user;
}
if (isset($_REQUEST['return_module']) && isset($_REQUEST['return_action']) && isset($_REQUEST['return_id'])) {
    $from_wiz = ' ';
    if (isset($_REQUEST['from_wiz']) && $_REQUEST['from_wiz']) {
        if (isset($_REQUEST['WizardMarketingSave']) && $_REQUEST['WizardMarketingSave']) {
            $header_URL = "Location: index.php?action=WizardMarketing&module=Campaigns&return_module=Campaigns&return_action=Wi" .
                    "zardMarketing&return_id=" . $_REQUEST['campaign_id'] . "&campaign_id=" . $_REQUEST['campaign_id'] .
                    "&show_wizard_marketing&jump=3&marketing_id=" . (isset($_POST['marketing_id']) && $_POST['marketing_id'] ? $_POST['marketing_id'] : $_REQUEST['marketing_id']) . "&record=" . (isset($_POST['marketing_id']) && $_POST['marketing_id'] ? $_POST['marketing_id'] : $_REQUEST['marketing_id']);
            header($header_URL);
        } else {
            header("Location: index.php?module={$_REQUEST['return_module']}&action={$_REQUEST['return_action']}&record={$_REQUEST['return_id']}&from=test");
        }
    } else {
        header("Location: index.php?module={$_REQUEST['return_module']}&action={$_REQUEST['return_action']}&record={$_REQUEST['return_id']}");
    }
} else {
    /* this will be triggered when manually sending off Email campaigns from the
     * Mass Email Queue Manager.
     */
    if (isset($_POST['manual'])) {
        header("Location: index.php?module=EmailMan&action=index");
    }
}
