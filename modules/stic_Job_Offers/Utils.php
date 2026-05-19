<?php
/**
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation.
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */

class stic_Job_OffersUtils
{
    /**
     * Recalculate and store Job Applications counts for an offer
     *
     * @param string $offerId
     * @return void
     */
    public static function updateApplicationsCounts($offerId)
    {
        if (empty($offerId)) {
            return;
        }

        $db = DBManagerFactory::getInstance();
        $offerId = $db->quote($offerId);

        $statusPrefix = 'job_applications_';
        $statuses = array();
        $offerBean = BeanFactory::newBean('stic_Job_Offers');
        $fieldDefs = $offerBean->field_defs ?? array();
        foreach (array_keys($fieldDefs) as $fieldName) {
            if (strpos($fieldName, $statusPrefix) !== 0) {
                continue;
            }
            if ($fieldName === $statusPrefix . 'total') {
                continue;
            }
            $statusKey = substr($fieldName, strlen($statusPrefix));
            $statuses[$statusKey] = true;
        }
        $statusKeys = array_keys($statuses);
        $counts = array_fill_keys($statusKeys, 0);

        $query = "SELECT
                ja.status,
                COUNT(*) as total
            FROM stic_job_applications_stic_job_offers_c rel
            INNER JOIN stic_job_applications ja
                ON rel.stic_job_applications_stic_job_offersstic_job_applications_idb = ja.id
            WHERE rel.stic_job_applications_stic_job_offersstic_job_offers_ida = '{$offerId}'
              AND rel.deleted = 0
              AND ja.deleted = 0
            GROUP BY ja.status";

        $result = $db->query($query);
        if ($result === false) {
            $GLOBALS['log']->error(__METHOD__ . ": Error executing counts query for offer {$offerId}.");
            return;
        }

        $totalCount = 0;
        while ($row = $db->fetchByAssoc($result)) {
            $status = $row['status'];
            $count = (int)$row['total'];
            $totalCount += $count;
            if (!empty($status) && array_key_exists($status, $counts)) {
                $counts[$status] = $count;
            }
        }

        $fields = array(
            'job_applications_total' => $totalCount,
        );
        foreach ($counts as $status => $count) {
            $fields['job_applications_' . $status] = $count;
        }

        $setParts = array();
        foreach ($fields as $field => $value) {
            $setParts[] = $field . ' = ' . $db->quote($value);
        }

        if (empty($setParts)) {
            return;
        }

        $update = "UPDATE stic_job_offers SET " . implode(', ', $setParts) . " WHERE id = '{$offerId}'";
        $updateResult = $db->query($update);
        if ($updateResult === false) {
            $GLOBALS['log']->error(__METHOD__ . ": Error updating counts for offer {$offerId}.");
        }
    }

    /**
     * Notify on offer status change with LPO and Notification campaign
     *
     * @param SugarBean $bean
     * @return void
     */
    public static function notifyStatusChange($bean)
    {
        if (empty($bean->id) || empty($bean->status)) {
            return;
        }

        // Get previous status from the bean property set by before_save hook
        $previousStatus = $bean->_previous_status ?? null;

        // Only notify if status actually changed
        if (empty($previousStatus) || $previousStatus === $bean->status) {
            return;
        }

        // Check if notifications are enabled for this offer
        if (!self::areStatusNotificationsEnabled($bean)) {
            $GLOBALS['log']->info(__METHOD__ . ': notifications disabled for this offer.');
            return;
        }

        // Create notification campaign for the new status
        $newStatus = $bean->status;
        
        self::createNotificationCampaign(
            'stic_Job_Offers',
            $bean,
            $newStatus
        );
    }

    /**
     * Check if status change notifications are enabled for the offer
     *
     * @param SugarBean $bean
     * @return bool
     */
    protected static function areStatusNotificationsEnabled($bean)
    {
        if (empty($bean)) {
            return false;
        }

        return !empty($bean->notification_status);
    }

    /**
     * Create a Notification campaign for a status change
     *
     * @param string $parentType Parent module name
     * @param SugarBean $parentBean Parent bean
     * @param string $newStatus New status value
     * @return void
     */
    protected static function createNotificationCampaign($parentType, $parentBean, $newStatus)
    {
        global $current_user, $timedate;

        $candidateTemplateId = self::getNotificationTemplateId('job_offers_candidates', $parentBean);
        $assignedUserTemplateId = self::getNotificationTemplateId('job_offers_assigned_user', $parentBean);
        $interlocutorTemplateId = self::getNotificationTemplateId('job_offers_interlocutor', $parentBean);
        $outboundEmail = self::getDefaultOutboundEmailAccount();
        $inboundEmailId = self::getDefaultInboundEmailId();
        $assignedUserId = $parentBean->assigned_user_id ?? $current_user->id;
        $assignedUserName = $parentBean->assigned_user_name ?? $current_user->user_name;

        if (empty($outboundEmail)) {
            $GLOBALS['log']->error(
                "Notification campaign not created for {$parentType} {$parentBean->id}: missing outbound configuration."
            );
            return;
        }

        $startDate = $timedate->nowDbDate();

        $notificationSent = false;

        $closedStatuses = array('closed_covered', 'closed_partially_covered', 'closed_not_covered');
        $openStatuses = array('open', 'selection_process', 'reopened');

        // Send direct email to assigned user
        if(in_array($newStatus, $closedStatuses)){
            if (!empty($assignedUserTemplateId) && !empty($assignedUserId)) {
                $assignedUserBean = BeanFactory::getBean('Users', $assignedUserId);
                if (empty($assignedUserBean) || empty($assignedUserBean->id)) {
                    $GLOBALS['log']->error(
                        "Notification email (assigned user) not sent for {$parentType} {$parentBean->id}: could not load assigned user bean."
                    );
                } else {
                    if($assignedUserId != $parentBean->modified_user_id){
                        if (self::sendNotificationEmail($parentBean, $assignedUserBean, $assignedUserTemplateId, $outboundEmail, 'assigned user')) {
                            $notificationSent = true;
                        }
                    }
                }
            }

            // Create candidates campaign
            if (!empty($candidateTemplateId)) {
                $candidateLpoBean = self::getOrCreateNotificationLpo(
                    $parentBean->id,
                    $parentBean->name,
                    $assignedUserId,
                    $assignedUserName,
                    'LBL_STIC_JOB_APPLICATIONS_STIC_JOB_OFFERS_FROM_STIC_JOB_APPLICATIONS_TITLE'
                );

                if (!empty($candidateLpoBean) && !empty($candidateLpoBean->id)) {
                    $contactIds = self::getRelatedApplicantsContactIds($parentBean->id);
                    self::updateLpoContacts($candidateLpoBean, $contactIds);

                    $candidateCampaign = self::getOrCreateNotificationCampaign(
                        $parentType,
                        $parentBean,
                        $candidateLpoBean,
                        $candidateTemplateId,
                        $outboundEmail,
                        $inboundEmailId,
                        $startDate,
                        $newStatus,
                        'LBL_STIC_JOB_APPLICATIONS_STIC_JOB_OFFERS_FROM_STIC_JOB_APPLICATIONS_TITLE'
                    );

                    if (!empty($candidateCampaign) && !empty($candidateCampaign->id)) {
                        $notificationSent = true;
                    } else {
                        $GLOBALS['log']->error(
                            "Notification campaign (candidates) could not be created or updated for {$parentType} {$parentBean->id}."
                        );
                    }
                } else {
                    $GLOBALS['log']->error(
                        "Notification campaign not created for {$parentType} {$parentBean->id}: could not get or create candidates prospect list."
                    );
                }
            }

            if (!$notificationSent) {
                $GLOBALS['log']->error(
                    "Notification not sent for {$parentType} {$parentBean->id}: no valid templates or recipients."
                );
                return;
            }
        }

        if ($parentBean->assigned_user_id === $parentBean->modified_user_id) {
            if (in_array($newStatus, $closedStatuses) || in_array($newStatus, $openStatuses)) {
                $interlocutorId = $parentBean->contact_id_c ?? '';
                if (!empty($interlocutorId) && !empty($interlocutorTemplateId)) {
                    $interlocutorBean = BeanFactory::getBean('Contacts', $interlocutorId);
                    if (empty($interlocutorBean) || empty($interlocutorBean->id)) {
                        $GLOBALS['log']->error(
                            "Notification email (interlocutor) not sent for {$parentType} {$parentBean->id}: could not load interlocutor bean."
                        );
                    } else {
                        if (self::sendNotificationEmail($parentBean, $interlocutorBean, $interlocutorTemplateId, $outboundEmail, 'interlocutor')) {
                            $notificationSent = true;
                        }
                    }
                }

                if (!$notificationSent) {
                    $GLOBALS['log']->error(
                        "Notification not sent for {$parentType} {$parentBean->id}: no valid interlocutor template or recipient."
                    );
                }
            }
            return;
        }

        $GLOBALS['log']->info("Notification process completed for {$parentType} {$parentBean->id}.");
    }

    /**
     * Send a direct notification email to one recipient bean
     *
     * @param SugarBean $offerBean
     * @param SugarBean $recipientBean
     * @param string $templateId
     * @param SugarBean $outboundEmail
     * @param string $recipientType
     * @return bool
     */
    protected static function sendNotificationEmail($offerBean, $recipientBean, $templateId, $outboundEmail, $recipientType)
    {
        // Validate required inputs
        if (empty($offerBean) || empty($recipientBean) || empty($templateId) || empty($outboundEmail)) {
            return false;
        }

        $destAddress = '';
        // Resolve recipient primary email
        if (!empty($recipientBean->emailAddress)) {
            $destAddress = $recipientBean->emailAddress->getPrimaryAddress($recipientBean);
        }
        if (empty($destAddress) && !empty($recipientBean->email1)) {
            $destAddress = $recipientBean->email1;
        }

        if (empty($destAddress)) {
            $GLOBALS['log']->error(
                "Notification email ({$recipientType}) not sent for offer {$offerBean->id}: no recipient email address."
            );
            return false;
        }

        require_once 'SticInclude/Utils.php';
        $parsedMailArray = SticUtils::parseEmailTemplate($templateId, array(
            $offerBean,
            $recipientBean,
        ));

        $subject = $parsedMailArray['subject'] ?? '';
        $bodyHtml = $parsedMailArray['body_html'] ?? '';
        $bodyText = $parsedMailArray['body'] ?? '';

        if (empty($bodyHtml) && empty($bodyText)) {
            $GLOBALS['log']->error(
                "Notification email ({$recipientType}) not sent for offer {$offerBean->id}: parsed template is empty."
            );
            return false;
        }

        require_once 'include/SugarPHPMailer.php';
        require_once 'modules/Emails/Email.php';
        $emailObj = new Email();
        $defaults = $emailObj->getSystemDefaultEmail();

        $mail = new SugarPHPMailer();
        $mail->setMailerForSystem();
        $mail->From = !empty($outboundEmail->smtp_from_addr) ? $outboundEmail->smtp_from_addr : ($defaults['email'] ?? '');
        $mail->FromName = !empty($outboundEmail->smtp_from_name) ? $outboundEmail->smtp_from_name : ($defaults['name'] ?? '');
        $mail->AddAddress($destAddress);
        $mail->Subject = $subject;
        if (!empty($bodyHtml)) {
            $mail->Body = from_html($bodyHtml);
            $mail->isHtml(true);
            $mail->AltBody = from_html($bodyText);
        } else {
            $mail->Body = $bodyText;
            $mail->isHtml(false);
        }
        $mail->prepForOutbound();

        if (!$mail->Send()) {
            $GLOBALS['log']->error(
                "Notification email ({$recipientType}) send error for offer {$offerBean->id}: " . $mail->ErrorInfo
            );
            return false;
        }

        $GLOBALS['log']->info("Notification email ({$recipientType}) sent for offer {$offerBean->id} to {$destAddress}.");
        return true;
    }

    /**
     * Create a new notification campaign for a status change
     *
     * @param string $parentType
     * @param SugarBean $parentBean
     * @param SugarBean $lpoBean
     * @param string $templateId
     * @param SugarBean $outboundEmail
     * @param string $inboundEmailId
     * @param string $startDate
     * @return SugarBean|null
     */
    protected static function getOrCreateNotificationCampaign($parentType, $parentBean, $lpoBean, $templateId, $outboundEmail, $inboundEmailId, $startDate, $newStatus, $recipientType = '')
    {
        global $app_list_strings, $mod_strings;

        $recipientLabelText = self::getRecipientLabel($recipientType);
        $recipientLabel = empty($recipientLabelText) ? '' : " - {$recipientLabelText}";
        $campaignName = "{$app_list_strings['emailTemplates_type_list']['notification']} {$mod_strings['LBL_STATUS']} {$app_list_strings['moduleListSingular'][$parentType]} '{$parentBean->name}' - {$app_list_strings['stic_job_offers_status_list'][$newStatus]}{$recipientLabel} - {$startDate}";

        $lpoIdFormatted = "^{$lpoBean->id}^";

        // Create a new campaign for each status change
        $campaign = BeanFactory::newBean('Campaigns');
        $campaign->campaign_type = 'Notification';
        $campaign->parent_type = $parentType;
        $campaign->parent_id = $parentBean->id;
        $campaign->parent_name = $parentBean->name;
        $campaign->assigned_user_id = $parentBean->assigned_user_id;
        $campaign->assigned_user_name = $parentBean->assigned_user_name;
        $campaign->start_date = $startDate;
        $campaign->name = $campaignName;

        $campaign->notification_prospect_list_ids = $lpoIdFormatted;
        $campaign->notification_template_id = $templateId;
        $campaign->notification_outbound_email_id = $outboundEmail->id;
        if (!empty($inboundEmailId)) {
            $campaign->notification_inbound_email_id = $inboundEmailId;
        }
        $campaign->notification_from_name = $outboundEmail->smtp_from_name;
        $campaign->notification_from_addr = $outboundEmail->smtp_from_addr;
        $campaign->notification_reply_to_name = $outboundEmail->reply_to_name;
        $campaign->notification_reply_to_addr = $outboundEmail->reply_to_addr;

        // Avoid QueueCampaign redirect when campaign is created inside a logic hook flow.
        $hadNoRedirectFlag = array_key_exists('stic_no_queue_redirect', $GLOBALS);
        $previousNoRedirect = $hadNoRedirectFlag ? $GLOBALS['stic_no_queue_redirect'] : null;
        $GLOBALS['stic_no_queue_redirect'] = true;

        try {
            $campaign->save();
        } catch (\Throwable $e) {
            $GLOBALS['log']->error(
                "Notification campaign save error for {$parentType} {$parentBean->id}: " . $e->getMessage()
            );
            return null;
        } finally {
            if (!$hadNoRedirectFlag) {
                unset($GLOBALS['stic_no_queue_redirect']);
            } else {
                $GLOBALS['stic_no_queue_redirect'] = $previousNoRedirect;
            }
        }

        return !empty($campaign->id) ? $campaign : null;
    }

    /**
     * Get or create a unique prospect list for offer notifications
     *
     * @param string $offerId
     * @param string $offerName
     * @param string $assignedUserId
     * @param string $assignedUserName
     * @return SugarBean|null
     */
    protected static function getOrCreateNotificationLpo($offerId, $offerName, $assignedUserId, $assignedUserName, $suffix = '')
    {
        global $app_list_strings;

        if (empty($offerId)) {
            return null;
        }

        $db = DBManagerFactory::getInstance();
        $offerIdQuoted = $db->quote($offerId);

        // Use offer name in LPO for better identification, but ensure it's unique by including offer ID and recipient suffix
        $offerNameDecoded = html_entity_decode((string)$offerName, ENT_QUOTES);
        $suffixText = self::getRecipientLabel($suffix);
        $suffixPart = empty($suffixText) ? '' : " - {$suffixText}";
        $lpoName = "LPO {$app_list_strings['moduleListSingular']['stic_Job_Offers']} '{$offerNameDecoded}' - ({$offerId}){$suffixPart}";

        // Try to find existing LPO by stable parts of the name, so renaming offers does not create duplicates
        $query = "SELECT id
            FROM prospect_lists
            WHERE deleted = 0
              AND name LIKE '%({$offerIdQuoted})%'";

        if (!empty($suffixText)) {
            $suffixTextQuoted = $db->quote($suffixText);
            $query .= " AND name LIKE '%{$suffixTextQuoted}%'";
        }

        $query .= " ORDER BY date_modified DESC LIMIT 1";

        $result = $db->query($query);
        if ($row = $db->fetchByAssoc($result)) {
            return BeanFactory::getBean('ProspectLists', $row['id']);
        }

        // Create new LPO if it doesn't exist
        $lpoBean = BeanFactory::newBean('ProspectLists');
        $lpoBean->name = $lpoName;
        $lpoBean->list_type = 'default';
        $lpoBean->assigned_user_id = $assignedUserId;
        $lpoBean->assigned_user_name = $assignedUserName;
        $lpoBean->save();

        return !empty($lpoBean->id) ? $lpoBean : null;
    }

    /**
     * Update LPO contacts, adding new ones and removing old ones
     *
     * @param SugarBean $lpoBean
     * @param array $contactIds
     * @return void
     */
    protected static function updateLpoContacts($lpoBean, $contactIds)
    {
        if (empty($lpoBean) || empty($lpoBean->id)) {
            return;
        }

        // Get current contacts in LPO
        if (!$lpoBean->load_relationship('contacts')) {
            return;
        }

        $currentContactIds = $lpoBean->contacts->get();
        $currentContactIds = is_array($currentContactIds) ? $currentContactIds : array();

        // Remove contacts no longer in the offer
        $toRemove = array_diff($currentContactIds, $contactIds);
        foreach ($toRemove as $contactId) {
            $lpoBean->contacts->delete($lpoBean->id, $contactId);
        }

        // Add new contacts
        $toAdd = array_diff($contactIds, $currentContactIds);
        foreach ($toAdd as $contactId) {
            $lpoBean->contacts->add($contactId);
        }

    }

    /**
     * Translate recipient type key to a visible label
     *
     * @param string $recipientType
     * @return string
     */
    protected static function getRecipientLabel($recipientLabelKey)
    {
        if (empty($recipientLabelKey)) {
            return '';
        }

        return translate($recipientLabelKey, 'stic_Job_Offers');
    }

    /**
    * Get contact ids for applications related to the offer
     *
     * @param string $offerId
     * @return array
     */
    protected static function getRelatedApplicantsContactIds($offerId)
    {
        if (empty($offerId)) {
            return array();
        }

        $db = DBManagerFactory::getInstance();
        $offerId = $db->quote($offerId);

        $rejectedStatus = $db->quote('rejected_closed');

        $query = "SELECT DISTINCT c.id
            FROM stic_job_applications_stic_job_offers_c rel
            INNER JOIN stic_job_applications ja ON rel.stic_job_applications_stic_job_offersstic_job_applications_idb = ja.id
            INNER JOIN stic_job_applications_contacts_c jac ON jac.stic_job_applications_contactsstic_job_applications_idb = ja.id
            INNER JOIN contacts c ON c.id = jac.stic_job_applications_contactscontacts_ida
            WHERE rel.deleted = 0
              AND jac.deleted = 0
              AND ja.deleted = 0
              AND c.deleted = 0
              AND (ja.status IS NULL OR ja.status <> '{$rejectedStatus}')
              AND rel.stic_job_applications_stic_job_offersstic_job_offers_ida = '{$offerId}'";

        $result = $db->query($query);
        $contactIds = array();
        while ($row = $db->fetchByAssoc($result)) {
            if (!empty($row['id'])) {
                $contactIds[] = $row['id'];
            }
        }
        return array_values(array_unique($contactIds));
    }


    /**
     * Get first available Notification email template id
     *
     * @return string|null
     */
    protected static function getNotificationTemplateId($templateKey, $parentBean = null)
    {
        $requestedTemplateId = null;
        if (!empty($parentBean)) {
            switch ($templateKey) {
                case 'job_offers_assigned_user':
                    $requestedTemplateId = $parentBean->emailtemplate_assigned_user_id ?? null;
                    break;
                case 'job_offers_candidates':
                    $requestedTemplateId = $parentBean->emailtemplate_candidates_id ?? null;
                    break;
                case 'job_offers_interlocutor':
                    $requestedTemplateId = $parentBean->emailtemplate_interlocutor_id ?? null;
                    break;
            }
        }

        $validTemplateId = self::getValidNotificationTemplateId($requestedTemplateId);
        if (!empty($validTemplateId)) {
            return $validTemplateId;
        }

        $templateIds = array(
            'job_offers_assigned_user' => '1a2d6f10-8e32-4f8a-9a11-3b1b7c2a0010',
            'job_offers_candidates' => '1a2d6f10-8e32-4f8a-9a11-3b1b7c2a0011',
            'job_offers_interlocutor' => '1a2d6f10-8e32-4f8a-9a11-3b1b7c2a0012',
        );
        $defaultTemplateId = $templateIds[$templateKey] ?? null;

        return self::getValidNotificationTemplateId($defaultTemplateId);
    }

    /**
     * Validate notification email template id
     *
     * @param string|null $templateId
     * @return string|null
     */
    protected static function getValidNotificationTemplateId($templateId)
    {
        if (empty($templateId)) {
            return null;
        }

        $templateBean = BeanFactory::getBean('EmailTemplates', $templateId);
        if (!empty($templateBean) && !empty($templateBean->id) && empty($templateBean->deleted)) {
            return $templateBean->id;
        }

        return null;
    }

    /**
     * Get first available outbound email account (system)
     *
     * @return OutboundEmailAccounts|null
     */
    protected static function getDefaultOutboundEmailAccount()
    {
        if (!class_exists('InboundEmail')) {
            require_once 'modules/InboundEmail/InboundEmail.php';
        }
        $ie = new InboundEmail();
        $user = $GLOBALS['current_user'] ?? null;
        $defaultOutboundId = $user ? $ie->getUsersDefaultOutboundServerId($user) : '';

        if (!empty($defaultOutboundId)) {
            $defaultOutbound = BeanFactory::getBean('OutboundEmailAccounts', $defaultOutboundId);
            if (!empty($defaultOutbound) && !empty($defaultOutbound->id)) {
                return $defaultOutbound;
            }
        }

        $outboundEmailsFocus = BeanFactory::newBean('OutboundEmailAccounts');
        $outboundEmails = $outboundEmailsFocus->get_full_list("", "type != 'user'");
        if (!empty($outboundEmails)) {
            return array_shift($outboundEmails);
        }
        return null;
    }

    /**
     * Get first available inbound bounce mailbox id for campaign bounce handling
     *
     * @return string|null
     */
    protected static function getDefaultInboundEmailId()
    {
        $db = DBManagerFactory::getInstance();
        
        // Try to get any active bounce mailbox
        $query = "SELECT id FROM inbound_email 
                  WHERE mailbox_type='bounce' AND status='Active' AND deleted='0' 
                  LIMIT 1";
        $result = $db->query($query);
        if ($row = $db->fetchByAssoc($result)) {
            return $row['id'];
        }

        // Try to get any active inbound email
        $query = "SELECT id FROM inbound_email 
                  WHERE status='Active' AND deleted='0' 
                  LIMIT 1";
        $result = $db->query($query);
        if ($row = $db->fetchByAssoc($result)) {
            return $row['id'];
        }

        return null;
    }

    /**
     * Generate a target list (LPO) with all applicants of a job offer
     *
     * @param string $offerId
     * @param string $label
     * @return array|false
     */
    public static function generateApplicantsLpo($offerId, $label, $type = '')
    {
        global $current_user;

        if (empty($offerId)) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': Offer ID is empty.');
            return false;
        }

        $offerBean = BeanFactory::getBean('stic_Job_Offers', $offerId);
        if (empty($offerBean) || empty($offerBean->id)) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': Offer bean not found for ID ' . $offerId);
            return false;
        }

        $db = DBManagerFactory::getInstance();
        $offerIdQuoted = $db->quote($offerId);

        $sqlForContacts = "SELECT DISTINCT sjacc.stic_job_applications_contactscontacts_ida AS contact_id
            FROM stic_job_applications_stic_job_offers_c sjajoc
            INNER JOIN stic_job_applications ja
                ON ja.id = sjajoc.stic_job_applications_stic_job_offersstic_job_applications_idb
            INNER JOIN stic_job_applications_contacts_c sjacc
                ON sjacc.stic_job_applications_contactsstic_job_applications_idb = ja.id
            INNER JOIN contacts c
                ON c.id = sjacc.stic_job_applications_contactscontacts_ida
            WHERE sjajoc.deleted = 0
              AND ja.deleted = 0
              AND sjacc.deleted = 0
              AND c.deleted = 0
              AND sjajoc.stic_job_applications_stic_job_offersstic_job_offers_ida = '{$offerIdQuoted}'";

        if ($type === 'stic_Job_Offers__active_applicants') {
            $rejectedStatus = $db->quote('rejected_closed');
            $sqlForContacts .= " AND (ja.status IS NULL OR ja.status <> '{$rejectedStatus}')";
        }

        $contactsTargets = $db->query($sqlForContacts);

        // Build a stable LPO name including offer ID so it can be reused
        $labelQuoted = $db->quote($label);
        $offerNameDecoded = html_entity_decode((string)$offerBean->name, ENT_QUOTES);
        $offerNameQuoted = $db->quote($offerNameDecoded);
        $lpoName = "LPO {$GLOBALS['app_list_strings']['moduleListSingular']['stic_Job_Offers']} '{$offerNameDecoded}' - ({$offerId}) - {$label}";

        // Reuse LPO with offer ID or without ID
        $lpoList = BeanFactory::getBean('ProspectLists')->get_full_list(
            'prospect_lists.date_modified DESC',
            "prospect_lists.deleted = 0
            AND (
                (
                    prospect_lists.name LIKE '%({$offerIdQuoted})%'
                    AND prospect_lists.name LIKE '%{$labelQuoted}%'
                )
                OR prospect_lists.name LIKE '{$offerNameQuoted} - ({$labelQuoted}) - %'
            )"
        );

        if (!empty($lpoList)) {
            $lpoBean = array_shift($lpoList);

            // If LPO name does not match exactly, update it to ensure it includes offer ID and label for better identification
            if ($lpoBean->name !== $lpoName) {
                $lpoBean->name = $lpoName;
                $lpoBean->save();
            }
        } else {
            $lpoBean = BeanFactory::newBean('ProspectLists');
            $lpoBean->name = $lpoName;
            $lpoBean->list_type = 'default';
            $lpoBean->assigned_user_id = $offerBean->assigned_user_id ?? $current_user->id;
            $lpoBean->assigned_user_name = $offerBean->assigned_user_name ?? $current_user->user_name;
            $lpoBean->save();
        }

        $lpoBean->load_relationship('contacts');
        while ($contactTarget = $db->fetchByAssoc($contactsTargets)) {
            if (!empty($contactTarget['contact_id'])) {
                $lpoBean->contacts->add($contactTarget['contact_id']);
            }
        }

        return ['status' => 'success', 'lpoId' => $lpoBean->id, 'lpoName' => $lpoBean->name];
    }

}
