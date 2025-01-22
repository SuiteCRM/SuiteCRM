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

/**

 * Description:
 */
//find all mailboxes of type bounce.

/**
 * Retrieve the attached error report for a bounced email if it exists.
 *
 * @param Email $email
 * @return string
 */
function retrieveErrorReportAttachment(Email $email)
{
    $contents = "";

    $email->getNotes($email->id);
    foreach ($email->attachments as $note) {
        if (stripos((string) $note->file_mime_type, 'rfc822') !== false) {
            $note_content = $note->getAttachmentContent();
            if ($note_content !== false) {
                // XXX: we don't know the encoding of the attached email, but
                // assume it's quoted-printable.
                $contents .= quoted_printable_decode($note_content);
            }
        } elseif (stripos((string) $note->file_mime_type, 'delivery-status') !== false) {
            $note_content = $note->getAttachmentContent();
            if ($note_content !== false) {
                $contents .= $note_content;
            }
        }
    }

    return $contents;
}

/**
 * Create a bounced log campaign entry
 *
 * @param array $row
 * @param Email $email
 * @param string $email_description
 * @return string
 */
function createBouncedCampaignLogEntry($row, $email, $email_description)
{
    $GLOBALS['log']->debug("Creating bounced email campaign log");
    $bounce = BeanFactory::newBean('CampaignLog');
    $bounce->campaign_id=$row['campaign_id'];
    $bounce->target_tracker_key=$row['target_tracker_key'];
    $bounce->target_id= $row['target_id'];
    $bounce->target_type=$row['target_type'];
    $bounce->list_id=$row['list_id'];
    $bounce->marketing_id=$row['marketing_id'];

    $bounce->activity_date=$email->date_created;
    $bounce->related_type='Emails';
    $bounce->related_id= $email->id;

    if (checkBouncedEmailInvalid($email_description)) {
        $bounce->activity_type='invalid email';
        markBounceEmailAddressInvalid($bounce);
    } else {
        $bounce->activity_type='send error';
    }
        
    $return_id=$bounce->save();
    return $return_id;
}

/**
 * Given an bounce entry, mark the related email address as invalid.
 *
 * @param CampaignLog $bounce
 */
function markBounceEmailAddressInvalid(CampaignLog $bounce)
{
    $sea = new SugarEmailAddress();
    $email_address = $sea->getPrimaryAddress(false, $bounce->target_id, $bounce->target_type);
    if (empty($email_address)) {
        return;
    }

    LoggerManager::getLogger()->info("Marking email address as invalid: ". $email_address);
    markEmailAddressInvalid($email_address);
}

/**
 * Given the email description returns whether the email should be marked invalid.
 *
 * @param string $email_description
 * @return bool
 */
function checkBouncedEmailInvalid($email_description)
{
    /* Consider as invalid if we get a permanent error status (5.X.X)
     * and in addition we get an smtp error 550.
     * https://tools.ietf.org/html/rfc3464#section-2.3.4
     * https://tools.ietf.org/html/rfc3464#section-2.3.6
     * https://www.usps.org/info/smtp_codes.html
     * Example:
     *  Status: 5.0.0
     *  Diagnostic-Code: smtp; 550 #5.1.0 Address rejected.
     * Example:
     *  Status: 5.5.0
     *  Diagnostic-Code: smtp; 550 5.5.0 Requested action not taken: mailbox unavailable
     * Example:
     *  Status: 5.1.1
     *  Diagnostic-Code: smtp; 554 5.1.1
     */
    if (preg_match('/^Status:\s*([0-9]+)\.([0-9]+)\.([0-9]+)/m', $email_description, $match)) {
        // 5.1.1 (permanent) Bad destination mailbox address
        if ($match[1] == '5' && $match[2] == '1' && $match[3] == '1') {
            return true;
        }

        // Permanent error with smtp error code for non-existent email address
        if ($match[1] == '5' && preg_match('/^Diagnostic-Code:\s*smtp\s*;.*550/m', $email_description)) {
            return true;
        }
    }

    return false;
}

/**
 * Given an email address, mark it as invalid.
 *
 * @param $email_address
 */
function markEmailAddressInvalid($email_address)
{
    if (empty($email_address)) {
        return;
    }
    $sea = new SugarEmailAddress();
    $rs = $sea->retrieve_by_string_fields(array('email_address_caps' => trim(strtoupper($email_address))));
    if ($rs != null) {
        $sea->AddUpdateEmailAddress($email_address, 1, 0, $rs->id);
    }
}

/**
 * Get the existing campaign log entry by tracker key.
 *
 * @param string Target Key
 * @return array Campaign Log Row
 */
function getExistingCampaignLogEntry($identifier)
{
    $row = false;
    $targeted = BeanFactory::newBean('CampaignLog');
    $where="campaign_log.activity_type='targeted' and campaign_log.target_tracker_key='{$identifier}'";
    $query=$targeted->create_new_list_query('', $where);
    $result=$targeted->db->query($query);
    $row=$targeted->db->fetchByAssoc($result);
    
    return $row;
}

/**
 * Scan the bounced email searching for a valid target identifier.
 *
 * @param string Email Description
 * @return array Results including matches and identifier
 */
function checkBouncedEmailForIdentifier($email_description)
{
    $matches = array();
    $identifiers = array();
    $found = false;
    //Check if the identifier is present in the header.
    if (preg_match('/X-CampTrackID: [a-z0-9\-]*/i', (string) $email_description, $matches)) {
        $identifiers = preg_split('/X-CampTrackID: /i', $matches[0], -1, PREG_SPLIT_NO_EMPTY);
        $found = true;
        $GLOBALS['log']->debug("Found campaign identifier in header of email");
    } else {
        if (preg_match('/index.php\?entryPoint=removeme&identifier=[a-z0-9\-]*/', (string) $email_description, $matches)) {
            $identifiers = preg_split('/index.php\?entryPoint=removeme&identifier=/', $matches[0], -1, PREG_SPLIT_NO_EMPTY);
            $found = true;
            $GLOBALS['log']->debug("Found campaign identifier in body of email");
        }
    }
    
    return array('found' => $found, 'matches' => $matches, 'identifiers' => $identifiers);
}

function campaign_process_bounced_emails(&$email, &$email_header)
{
    global $sugar_config;
    $emailFromAddress = $email_header->fromaddress;
    $email_description = $email->raw_source;

    $email_description .= retrieveErrorReportAttachment($email);

    if (preg_match('/MAILER-DAEMON|POSTMASTER/i', (string) $emailFromAddress)) {
        $matches=array();

        //do we have the identifier tag in the email?
        $identifierScanResults = checkBouncedEmailForIdentifier($email_description);

        if ($identifierScanResults['found']) {
            $matches = $identifierScanResults['matches'];
            $identifiers = $identifierScanResults['identifiers'];

            if (!empty($identifiers)) {
                //array should have only one element in it.
                $identifier = trim($identifiers[0]);
                $row = getExistingCampaignLogEntry($identifier);

                //Found entry
                if (!empty($row)) {
                    //do not create another campaign_log record is we already have an
                    //invalid email or send error entry for this tracker key.
                    $query_log = "select * from campaign_log where target_tracker_key='{$row['target_tracker_key']}'";
                    $query_log .=" and (activity_type='invalid email' or activity_type='send error')";
                    $targeted = BeanFactory::newBean('CampaignLog');
                    $result_log=$targeted->db->query($query_log);
                    $row_log=$targeted->db->fetchByAssoc($result_log);

                    if (empty($row_log)) {
                        $return_id = createBouncedCampaignLogEntry($row, $email, $email_description);
                        return true;
                    } else {
                        $GLOBALS['log']->debug("Warning: campaign log entry already exists for identifier $identifier");
                        return false;
                    }
                } else {
                    $GLOBALS['log']->info("Warning: skipping bounced email with this tracker_key(identifier) in the message body: ".$identifier);
                    return false;
                }
            } else {
                $GLOBALS['log']->info("Warning: Empty identifier for campaign log.");
                return false;
            }
        } else {
            $GLOBALS['log']->info("Warning: skipping bounced email because it does not have the removeme link.");
            return false;
        }
    } else {
        $GLOBALS['log']->info("Warning: skipping bounced email because the sender is not MAILER-DAEMON.");
        return false;
    }
}
