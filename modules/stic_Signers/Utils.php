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

/**
 * Utility class for handling operations related to stic_Signers module.
 * This includes functionality for sending signature requests via email,
 * sending One-Time Passwords (OTP), managing subpanel queries, and checking status.
 */
class stic_SignersUtils
{
    /**
     * Sends a signature request email to the specified signer.
     * This function retrieves signer and signature details, constructs an email with a unique
     * signing link, parses the email template, and sends the request using SugarCRM's mailer.
     *
     * @param string $signerId The ID of the signer to whom the email should be sent.
     * @throws Exception If a required bean is not found, the destination email is empty, or the email sending fails.
     * @return void
     */
    public static function sendToSign($signerId)
    {
        global $current_user, $mod_strings, $app_strings;

        // Validate signer ID
        if (empty($signerId)) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . "Signer ID cannot be empty.");
            return;
        }

        // Retrieve the signer bean
        $signerBean = BeanFactory::getBean('stic_Signers', $signerId);

        if (!$signerBean) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . "Signer with ID {$signerId} not found.");
            return;
        }

        // Retrieve the related signature bean
        require_once 'SticInclude/Utils.php';
        $signatureBean = SticUtils::getRelatedBeanObject($signerBean, 'stic_signatures_stic_signers');
        if (!$signatureBean) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . "Related signature for signer ID {$signerId} not found.");
            return;
        }

        // Get the email template ID from the signature, or use a default if not set
        // The default ID '000005f1-2e4e-3b11-051f-68e3c9e70330' is hardcoded here.
        $templateId = empty($signatureBean->emailtemplate_id_c) ? '000005f1-2e4e-3b11-051f-68e3c9e70330' : $signatureBean->emailtemplate_id_c;

        // Get the destination email address for the signer
        $destAddress = $signerBean->email_address ?? '';

        if (empty($destAddress)) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": No email address available for signer ID {$signerId}.");
            SugarApplication::appendErrorMessage("<p class='label label-warning'>{$mod_strings['LBL_SIGNER_NO_EMAIL']} ({$signerBean->name}) </p>");
            return;
        }

        // Prepare mailer
        require_once 'include/SugarPHPMailer.php';
        $emailObj = new Email();
        $defaults = $emailObj->getSystemDefaultEmail();
        $mail = new SugarPHPMailer();
        $mail->setMailerForSystem();

        // Set From and FromName using current user or system defaults
        $fromEmail = $current_user->email1 ?: $defaults['email'];
        $mail->From = $fromEmail;

        $fromName = $current_user->name ?: $defaults['name'];
        $mail->FromName = $fromName;

        // Add recipient
        if (empty($destAddress)) {
            // If no destination address, return false (or handle error appropriately)
            ob_clean();
            echo json_encode(false);
            die();
        }
        $mail->AddAddress($destAddress);

        // Cargar beans relacionados: signature, contact/user (si aplica) y el signer ya cargado
        $relatedContactOrUserBean = null;
        if (!empty($signerBean->parent_type) && !empty($signerBean->parent_id)) {
            $relatedContactOrUserBean = BeanFactory::getBean($signerBean->parent_type, $signerBean->parent_id);
        }

        require_once 'SticInclude/Utils.php';
        // Parse the email template using all relevant beans
        $parsedMailArray = SticUtils::parseEmailTemplate($templateId, [
            $signerBean,
            $signatureBean,
            $relatedContactOrUserBean,
        ]);

        $body_html = $parsedMailArray['body_html'] ?? '';
        $subject_parsed = $parsedMailArray['subject'] ?? '';

        // Validate final parsed body
        if (empty($body_html)) {
            throw new Exception("Parsed email body is empty after applying template '{$templateId}'.");
        }

        $mailBodyHtml = $body_html;
        // Use parsed subject, falling back to template subject or module string
        $mailSubject = $subject_parsed ?? '';

        // Assign subject and body to the mailer
        $mail->Subject = $mailSubject;
        $mail->Body = from_html($mailBodyHtml);
        $mail->isHtml(true);
        $mail->prepForOutbound();

        // Attempt to send the email and log the result
        if (!$mail->Send()) {
            $msg = "There was an error sending the email to {$destAddress}. Mailer Error: " . $mail->ErrorInfo;
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": " . $msg);
            SugarApplication::appendErrorMessage("<p class='label label-warning'>Error: {$app_strings['LBL_EMAIL_INVALID_SYSTEM_OUTBOUND']} </p>");
            SugarApplication::redirect('index.php?module=stic_Signers&action=DetailView&record=' . $signerId);

            throw new Exception($msg);
        } else {
            // On success: display message, log debug, and log the action
            SugarApplication::appendSuccessMessage("<p class='label label-success'> {$mod_strings['LBL_SIGNER_EMAIL_SUCCESS']} ({$signerBean->name})</p>");
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Email sent successfully to {$destAddress}.");
            require_once 'modules/stic_Signature_Log/Utils.php';
            stic_SignatureLogUtils::logSignatureAction('EMAIL_SENT', $signerId, 'SIGNER', $destAddress);
        }
    }

    /**
     * Sends a One-Time Password (OTP) to the signer via email.
     * This function uses the provided OTP code to construct and send a simple
     * verification email using SugarCRM's mailer.
     *
     * @param object $signerBean The bean object of the signer to whom the OTP should be sent.
     * @param string $otpCode The one-time password to include in the email body.
     * @throws Exception If the signer ID is empty or the destination email address is invalid.
     * @return bool True if the email was sent successfully, false otherwise.
     */
    public static function sendOtpEmailToSigner($signerBean, $otpCode)
    {
        global $current_user;
        require_once 'SticInclude/Utils.php';

        $signerId = $signerBean->id;
        $signerStrings = return_module_language($GLOBALS['current_language'], 'stic_Signers');

        if (empty($signerBean) || empty($signerBean->id)) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . " Invalid signer bean provided.");
            return false;
        }

        $signatureBean = SticUtils::getRelatedBeanObject($signerBean, 'stic_signatures_stic_signers');
        if (empty($signatureBean) || empty($signatureBean->id)) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . " Related signature for signer ID {$signerId} not found.");
            return false;
        }

        $userOrContactsBean = BeanFactory::getBean($signerBean->parent_type, $signerBean->parent_id);
        if (empty($userOrContactsBean) || empty($userOrContactsBean->id)) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . " Related user/contact not found for Signer ID: {$signerId}");
            return false;
        }

        // Get the email template ID from the signature, or use a default if not set
        $templateId = empty($signatureBean->emailtemplateotp_id_c) ? '000005f1-2e4e-3b11-051f-68e3c9e70332' : $signatureBean->emailtemplateotp_id_c;
        $parsedMailArray = SticUtils::parseEmailTemplate($templateId, [
            $signerBean,
            $signatureBean,
            $userOrContactsBean,
        ]);

        // Validate signer ID
        if (empty($signerId)) {
            throw new Exception("Signer ID cannot be empty.");
        }

        // Get the destination email address for the signer
        $destAddress = $signerBean->email_address ?? '';

        // Prepare mailer
        require_once 'include/SugarPHPMailer.php';
        $emailObj = new Email();
        $defaults = $emailObj->getSystemDefaultEmail();
        $mail = new SugarPHPMailer();
        $mail->setMailerForSystem();

        // Set From and FromName
        $fromEmail = $current_user->email1 ?: $defaults['email'];
        $mail->From = $fromEmail;

        $fromName = $current_user->name ?: $defaults['name'];
        $mail->FromName = $fromName;

        // Add recipient
        if (empty($destAddress)) {
            // If no destination address, return false
            return false;
            die();
        }
        $mail->AddAddress($destAddress);

        // Use parsed subject, falling back to module string
        $mail->Subject = $parsedMailArray['subject'] ?? $signerStrings['LBL_SIGNER_OTP_EMAIL_SUBJECT'];

        // Prepare the complete HTML body of the email
        $completeHTML = $parsedMailArray['body_html'];

        $mail->Body = from_html($completeHTML);
        $mail->isHtml(true);
        $mail->prepForOutbound();

        // Attempt to send the email and log the result
        if (!$mail->Send()) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": There was an error sending OTP the email to {$destAddress}. Mailer Error: " . $mail->ErrorInfo);
            return false;
        } else {
            // On success: log debug, log the action, and update session tracking
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": OTP Email sent successfully to {$destAddress}.");
            require_once 'modules/stic_Signature_Log/Utils.php';
            stic_SignatureLogUtils::logSignatureAction('OTP_SENT_EMAIL', $signerId, 'SIGNER', $destAddress);
            $_SESSION['otp_signed_sent'][$signerId] = [
                'method' => 'email',
                'time' => time(),
                'email' => $destAddress,
            ];
            return true;
        }
    }

    /**
     * Sends a One-Time Password (OTP) to the signer via phone (SMS).
     * This function uses the provided OTP code to construct an SMS message
     * and sends it using the configured SMS gateway via the stic_Messages module.
     *
     * @param object $signerBean The bean object of the signer to whom the OTP should be sent.
     * @param string $otpCode The one-time password to include in the SMS body.
     * @throws Exception If the signer ID is empty or the destination phone number is invalid.
     * @return bool True if the SMS was sent successfully, false otherwise.
     */
    public static function sendOtpPhoneMessageToSigner($signerBean, $otpCode)
    {
        require_once 'SticInclude/Utils.php';

        $signerId = $signerBean->id;
        $destPhone = $signerBean->phone ?? '';

        if (empty($destPhone)) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": No phone number available for signer ID {$signerId}.");
            return false;
        }

        if (empty($signerBean) || empty($signerBean->id)) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . " Invalid signer bean provided.");
            return false;
        }

        $signatureBean = SticUtils::getRelatedBeanObject($signerBean, 'stic_signatures_stic_signers');
        if (empty($signatureBean) || empty($signatureBean->id)) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . " Related signature for signer ID {$signerId} not found.");
            return false;
        }

        $userOrContactsBean = BeanFactory::getBean($signerBean->parent_type, $signerBean->parent_id);
        if (empty($userOrContactsBean) || empty($userOrContactsBean->id)) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . " Related user/contact not found for Signer ID: {$signerId}");
            return false;
        }

        $templateId = empty($signatureBean->emailtemplateotpsms_id_c) ? '000005f1-2e4e-3b11-051f-68e3c9e70333' : $signatureBean->emailtemplateotpsms_id_c;
        $parsedTemplateArray = SticUtils::parseEmailTemplate($templateId, [
            $signerBean,
            $signatureBean,
            $userOrContactsBean,
        ]);

        $messageText = $parsedTemplateArray['body'] ?? '';
        if (empty($messageText)) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": Parsed SMS body is empty for signer ID {$signerId}.");
            return false;
        }

        // Retrieve the SMS sender setting
        require_once 'modules/stic_Settings/Utils.php';
        $sender = stic_SettingsUtils::getSetting('MESSAGES_SENDER') ?? 'SinergiaCRM Signature Portal';

        // Determine the placeholder for the signer's name
        $templateMark = $signerBean->parent_type == 'Contacts' ? '$contact_first_name' : '$contact_user_first_name';

        // Create a new stic_Messages bean to record and send the SMS
        $messageBean = BeanFactory::newBean('stic_Messages');
        $messageBean->phone = $signerBean->phone;
        $messageBean->parent_type = $signerBean->parent_type;
        $messageBean->parent_id = $signerBean->parent_id;
        $messageBean->message = $messageText;
        $messageBean->sender = $sender;
        $messageBean->status = 'sent'; // Assuming 'sent' is set upon creation/attempt
        $messageBean->type = 'sms';
        $messageBean->save();

        // Check if the message record was successfully created/sent
        if (!is_string($messageBean->id) || $messageBean->status !== 'sent') {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": Failed to create message record for OTP via phone to signer ID {$signerId}.");
            return false;
        }

        // Log the OTP sending action and update session tracking
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": OTP sent via phone to {$destPhone} for signer ID {$signerId}.");

        require_once 'modules/stic_Signature_Log/Utils.php';
        stic_SignatureLogUtils::logSignatureAction('OTP_SENT_PHONE', $signerId, 'SIGNER', $destPhone);

        $_SESSION['otp_signed_sent'][$signerId] = [
            'method' => 'phone',
            'time' => time(),
            'phone' => $destPhone,
        ];

        return true;
    }

    /**
     * Retrieves the stic_Signers records associated with a specific Contact for use in a subpanel.
     * It filters for 'pending' or 'signed' statuses and orders by modification date descending.
     *
     * @return string The SQL query string to fetch the required stic_Signers records.
     */
    public static function getSticSignersForContacts()
    {
        global $app_list_strings;

        $contact_id = $_REQUEST['record'];
        if (empty($contact_id)) {
            return '';
        }

        // Construct the SQL query
        $query = "
            SELECT * FROM (
                SELECT
                    stic_signers.id,
                    stic_signers.name,
                    stic_signers.date_entered,
                    stic_signers.date_modified,
                    stic_signers.modified_user_id,
                    stic_signers.created_by,
                    stic_signers.description,
                    stic_signers.deleted,
                    stic_signers.assigned_user_id,
                    stic_signers.status,
                    stic_signers.signature_date,
                    stic_signers.parent_type,
                    stic_signers.parent_id,
                    stic_signers.record_name,
                    stic_signers.record_type,
                    stic_signers.record_id,
                    CONCAT_WS(' ', c1.first_name, c1.last_name) as on_behalf_of_id
                FROM stic_signers
                LEFT JOIN contacts c1 ON c1.id = stic_signers.contact_id_c -- to get on_behalf_of_id
                WHERE parent_type = 'Contacts'
                    AND (stic_signers.parent_id = '{$contact_id}' OR stic_signers.contact_id_c = '{$contact_id}')
                    AND stic_signers.status in ('pending','signed')
                    AND stic_signers.deleted = 0
                ) AS stic_signers
        ";
        return $query;
    }

    /**
     * Retrieves the stic_Signers records associated with a specific User for use in a subpanel.
     * It filters for 'pending' or 'signed' statuses and orders by modification date descending.
     *
     * @return string The SQL query string to fetch the required stic_Signers records.
     */
    public static function getSticSignersForUsers()
    {
        $user_id = $_REQUEST['record'];
        if (empty($user_id)) {
            return '';
        }

        // Construct the SQL query
        $query = "
            SELECT * FROM (
                SELECT
                    stic_signers.id,
                    stic_signers.name,
                    stic_signers.date_entered,
                    stic_signers.date_modified,
                    stic_signers.modified_user_id,
                    stic_signers.created_by,
                    stic_signers.description,
                    stic_signers.deleted,
                    stic_signers.assigned_user_id,
                    stic_signers.status,
                    stic_signers.signature_date,
                    stic_signers.parent_type,
                    stic_signers.parent_id,
                    stic_signers.record_name,
                    stic_signers.record_type,
                    stic_signers.record_id
                FROM stic_signers
                WHERE parent_type = 'Users'
                    AND parent_id = '{$user_id}'
                    AND status in ('pending','signed')
                    AND deleted = 0
                ) AS stic_signers
        ";

        return $query;
    }



    /**
     * Checks if there are any pending signers for the signature associated with the given signer.
     * If no pending signers are found and the signature is currently 'open',
     * it updates the signature status to 'completed'.
     *
     * @param object $signerBean The bean object of the signer to check.
     * @return void
     */
    public static function setSignatureCompletedIfNoPendingSigners($signerBean)
    {
        require_once 'SticInclude/Utils.php';
        $signatureBean = SticUtils::getRelatedBeanObject($signerBean, 'stic_signatures_stic_signers');

        // Only proceed if the signature is currently 'open'
        if ($signatureBean->status != 'open') {
            return;
        }

        // Check if there are any pending signers for this signature
        $pendingSigners = $signatureBean->get_linked_beans(
            'stic_signatures_stic_signers',
            'stic_Signers',
            '',
            0,
            0,
            0,
            " stic_signers.status = 'pending' "
        );

        // If no pending signers, set signature status to 'completed'
        if (empty($pendingSigners)) {
            $signatureBean->status = 'completed';
            $signatureBean->save();
            $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . "Signature {$signatureBean->id} marked as completed as there are no pending signers.");

            require_once 'modules/stic_Signature_Log/Utils.php';
            stic_SignatureLogUtils::logSignatureAction('SIGNATURE_COMPLETED', $signatureBean->id, 'SIGNATURE', '');
        }
    }

    /**
     * Deactivates other 'pending' signers for the same signature when one signer
     * has successfully completed the signature process (status changed to 'signed'),
     * specifically for signatures with the 'on behalf of' flag enabled.
     *
     * @param object $signerBean The bean object of the signer whose status has just changed.
     * @return void
     */
    public static function deactivateOtherSignersForSameSignature($signerBean)
    {
        // Only proceed if the signer's status has just changed from 'pending' to 'signed'
        if (!isset($signerBean->fetched_row['status']) || $signerBean->fetched_row['status'] != 'pending' || $signerBean->status != 'signed') {
            return;
        }

        require_once 'SticInclude/Utils.php';
        $signatureBean = SticUtils::getRelatedBeanObject($signerBean, 'stic_signatures_stic_signers');

        // Only run logic if 'on_behalf_of' is enabled on the signature
        if (isset($signatureBean->on_behalf_of) && $signatureBean->on_behalf_of == 1) {
            global $mod_strings;

            // Find other pending signers linked to the same signature AND the same on_behalf_of_id
            $otherSigners = $signatureBean->get_linked_beans(
                'stic_signatures_stic_signers',
                'stic_Signers',
                '',
                0,
                0,
                0,
                " stic_signers.id <> '{$signerBean->id}' AND stic_signers.status = 'pending' AND stic_signers.contact_id_c = '{$signerBean->contact_id_c}' "
            );

            // Deactivate and log
            foreach ($otherSigners as $otherSigner) {
                $otherSigner->status = 'unnecessary';
                $otherSigner->save();
                $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . "Deactivated signer {$otherSigner->id} for signature {$signatureBean->id} because signature was completed by $signerBean->name.");

                require_once 'modules/stic_Signature_Log/Utils.php';
                stic_SignatureLogUtils::logSignatureAction('SIGNATURE_NOT_NEEDED', $otherSigner->id, 'SIGNER', "{$mod_strings['LBL_SIGNATURE_COMPLETED_BY']} {$signerBean->parent_name}.");
            }
        }
    }

    /**
     * Checks if the signature associated with a signer has expired based on its expiration date.
     *
     * @param object $signatureBean The bean object of the associated signature.
     * @return bool True if the signature has expired, false otherwise.
     */
    public static function checkExpiredStatus($signatureBean)
    {
        if (empty($signatureBean->expiration_date)) {
            return false;
        }

        global $timedate;

        // Convert user-facing expiration date to internal DateTime object
        $expirationDate = $timedate->fromUser($signatureBean->expiration_date);
        // Get current time in DB format and convert to DateTime object
        $currentDate = $timedate->fromDb(gmdate('Y-m-d H:i:s'));

        // Compare current date with expiration date
        if ($currentDate > $expirationDate) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Checks if the signature associated with a signer is active based on its activation date.
     *
     * @param object $signatureBean The bean object of the associated signature.
     * @return bool True if the signature is active (current date is greater than or equal to activation date), false otherwise.
     */
    public static function checkActivatedStatus($signatureBean)
    {
        global $timedate;

        // Convert user-facing activation date to internal DateTime object
        $startDate = $timedate->fromUser($signatureBean->activation_date);
        // Get current time in DB format and convert to DateTime object
        $currentDate = $timedate->fromDb(gmdate('Y-m-d H:i:s'));

        // Compare current date with activation date
        if ($currentDate < $startDate) {
            return false;
        } else {
            return true;
        }
    }

    /**
     * Generates a sanitized document name for a signer with the appropriate prefix.
     *
     * @param object $signerBean The signer bean object.
     * @param string $prefix Optional prefix to add to the filename (e.g., "[BORRADOR]").
     * @return string The sanitized document name with .pdf extension.
     */
    public static function getDocumentName($signerBean)
    {
        global $app_list_strings;
        
        // Calculate prefix based on signer status
        $prefix = '';
        if (!empty($signerBean->status) && isset($app_list_strings['stic_signers_status_list'][$signerBean->status])) {
            $prefix = '[' . $app_list_strings['stic_signers_status_list'][$signerBean->status] . '] ';
        }
        
        // Sanitize filename: remove problematic characters but keep accented characters
        // Remove only: / \ : * ? " < > | and other control characters
        $sanitizedName = preg_replace('/[\/\\\:*?"<>|\x00-\x1F\x7F]/', '_', $signerBean->name);
        $sanitizedName = preg_replace('/\s+/', ' ', $sanitizedName);
        $sanitizedName = trim($sanitizedName);
        
        return $prefix . $sanitizedName . '.pdf';
    }

    /**
     * Generates and initiates the download of document PDFs for one or multiple signers.
     * For a single signer, the PDF is sent directly to the browser.
     * For multiple signers, all PDFs are packaged into a ZIP file.
     * Uses existing signed documents when available (status='signed'), otherwise generates drafts.
     *
     * @param string|array $signerIds The ID of the signer (or array of IDs) for whom documents should be downloaded.
     * @return void
     */
    public static function downloadDocuments($signerIds)
    {
        global $mod_strings, $current_user;

        require_once 'modules/stic_Signatures/sticGenerateSignedPdf.php';
        require_once 'modules/stic_Signature_Log/Utils.php';

        // Normalize to array for uniform processing
        $isArray = is_array($signerIds);
        $signerIdsArray = $isArray ? $signerIds : [$signerIds];

        // Handle single signer - direct download
        if (count($signerIdsArray) === 1) {
            $signerId = $signerIdsArray[0];
            $signerBean = BeanFactory::getBean('stic_Signers', $signerId);

            if (!$signerBean) {
                $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . "Signer with ID {$signerId} not found.");
                return;
            }

            $_REQUEST['signerId'] = $signerId;

            // Check if signed document exists
            $signedDocPath = "upload/{$signerId}_signed.pdf";
            $documentPath = '';
            $shouldRemoveTemp = false;

            if ($signerBean->status === 'signed' && file_exists($signedDocPath)) {
                $documentPath = $signedDocPath;
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . "Using existing signed document at {$documentPath} for signer ID {$signerBean->id}.");
            } else {
                $documentPath = sticGenerateSignedPdf::generateSignaturePdf('unsigned');
                $shouldRemoveTemp = true;
            }

            if (!file_exists($documentPath)) {
                $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . "Document file not found at path {$documentPath} for signer ID {$signerBean->id}.");
                return;
            }

            // Send file to browser for download
            header('Content-Description: File Transfer');
            header('Content-Type: application/pdf');
            header('Content-Disposition: attachment; filename="' . self::getDocumentName($signerBean) . '"');
            header('Expires: 0');
            header('Cache-Control: must-revalidate');
            header('Pragma: public');
            header('Content-Length: ' . filesize($documentPath));
            readfile($documentPath);

            // Remove temporary file if it was generated
            if ($shouldRemoveTemp && file_exists($documentPath)) {
                unlink($documentPath);
            }

            // Log the download action
            stic_SignatureLogUtils::logSignatureAction('CRM_PDF_DOWNLOADED', $signerBean->id, 'SIGNER', $mod_strings['LBL_SIGNER_DOWNLOADED_BY'] . ' ' . $current_user->full_name);

            exit;
        }

        // Handle multiple signers - ZIP download
        $tempFiles = [];
        $zipFileName = "PDF_{$mod_strings['LBL_MODULE_NAME']}_" . date('YmdHis') . '.zip';
        $zipPath = sys_get_temp_dir() . '/' . $zipFileName;

        $zip = new ZipArchive();
        if ($zip->open($zipPath, ZipArchive::CREATE | ZipArchive::OVERWRITE) !== true) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . "Failed to create ZIP archive at {$zipPath}.");
            return;
        }

        $successCount = 0;
        foreach ($signerIdsArray as $signerId) {
            $signerBean = BeanFactory::getBean('stic_Signers', $signerId);

            if (!$signerBean) {
                $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . "Signer with ID {$signerId} not found.");
                continue;
            }

            $_REQUEST['signerId'] = $signerId;

            // Check if signed document exists
            $signedDocPath = "upload/{$signerId}_signed.pdf";
            $documentPath = '';
            $shouldRemoveTemp = false;

            if ($signerBean->status === 'signed' && file_exists($signedDocPath)) {
                $documentPath = $signedDocPath;
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . "Using existing signed document at {$documentPath} for signer ID {$signerBean->id}.");
            } else {
                $documentPath = sticGenerateSignedPdf::generateSignaturePdf('unsigned');
                $shouldRemoveTemp = true;
            }

            if (!file_exists($documentPath)) {
                $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . "Document file not found at path {$documentPath} for signer ID {$signerBean->id}.");
                continue;
            }

            // Add file to ZIP with unique name
            $pdfFileName = self::getDocumentName($signerBean);
            $counter = 1;
            $uniqueFileName = $pdfFileName;
            $baseFileName = pathinfo($pdfFileName, PATHINFO_FILENAME);
            $extension = pathinfo($pdfFileName, PATHINFO_EXTENSION);

            while ($zip->locateName($uniqueFileName) !== false) {
                $uniqueFileName = $baseFileName . '_' . $counter . '.' . $extension;
                $counter++;
            }

            $zip->addFile($documentPath, $uniqueFileName);

            if ($shouldRemoveTemp) {
                $tempFiles[] = $documentPath;
            }

            // Log the download action
            stic_SignatureLogUtils::logSignatureAction('CRM_PDF_DOWNLOADED', $signerBean->id, 'SIGNER', $mod_strings['LBL_SIGNER_DOWNLOADED_BY'] . ' ' . $current_user->full_name);

            $successCount++;
        }

        $zip->close();

        // Clean up temporary PDF files
        foreach ($tempFiles as $tempFile) {
            if (file_exists($tempFile)) {
                unlink($tempFile);
            }
        }

        if ($successCount === 0) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . "No valid documents were generated.");
            if (file_exists($zipPath)) {
                unlink($zipPath);
            }
            return;
        }

        // Send ZIP file to browser for download
        header('Content-Description: File Transfer');
        header('Content-Type: application/zip');
        header('Content-Disposition: attachment; filename="' . $zipFileName . '"');
        header('Expires: 0');
        header('Cache-Control: must-revalidate');
        header('Pragma: public');
        header('Content-Length: ' . filesize($zipPath));
        readfile($zipPath);

        // Remove the ZIP file from the server
        if (file_exists($zipPath)) {
            unlink($zipPath);
        }

        exit;
    }

    


}
