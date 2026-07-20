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

// Twilio WhatsApp Helper class to send WhatsApp messages through Twilio provider.

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once('modules/stic_Messages/Helpers/stic_MessagesHelper.php');

class TwilioWhatsAppHelper extends stic_MessagesHelper {
    
    protected ?string $sid = null;
    protected ?string $token = null;
    protected ?string $twilioNumber = null;
    private string $apiUrl = 'https://api.twilio.com/2010-04-01';

    /**
     * Returns the provider name identifier.
     */
    protected function getProviderName(): string {
        return 'whatsapp';
    }

    /**
     * Returns the helper type matching the DB stic_messages_type_list key.
     */
    public function getHelperType(): string {
        return 'whatsapp';
    }

    /**
     * Returns the list of required setting keys.
     */
    protected function getRequiredSettings(): array {
        return ['twilio_sid', 'twilio_token', 'twilio_number'];
    }

    /**
     * WhatsApp passes template body to provider for server-side substitution.
     */
    public function passesTemplateBodyToProvider(): bool {
        return true;
    }

    /**
     * Constructor - loads configuration and initializes provider.
     */
    public function __construct() {
        $this->loadConfig();
        
        $this->sid = $this->config['twilio_sid'] ?? '';
        $this->token = $this->config['twilio_token'] ?? '';
        $this->twilioNumber = $this->config['twilio_number'] ?? '';
        $this->active = $this->isConfigured();
    }

    /**
     * Builds send parameters with WhatsApp-specific data.
     * 
     * @param string|null $from
     * @param string $text Message body or template SID
     * @param string $to Phone number
     * @param mixed ...$args Additional: templateSid, beans, mediaUrl
     * @return array
     */
    protected function buildSendParams(?string $from, string $text, string $to, ...$args): array {
        $templateSid = $args[0] ?? null;
        $beans = $args[1] ?? [];
        $mediaUrl = $args[2] ?? null;

        return [
            'from' => $from,
            'text' => $text,
            'to' => $to,
            'templateSid' => $templateSid,
            'beans' => $beans,
            'mediaUrl' => $mediaUrl,
        ];
    }

    /**
     * Resolve media attachment for WhatsApp messages.
     * Builds signed URL for Twilio to fetch the file.
     */
    public function resolveMedia(stic_Messages $bean): void {
        if (empty($bean->media_note_id)) {
            $bean->media_note_id = $_REQUEST['media_note_id'] ?? '';
        }

        if (empty($bean->media_note_id) && !empty($bean->id)) {
            $note = BeanFactory::getBean('Notes');
            $notes = $note->get_full_list('date_entered', "parent_id = '{$bean->id}' AND parent_type = 'stic_Messages' AND deleted = 0", 0, 1);
            if ($notes) {
                $bean->media_note_id = $notes[0]->id;
            }
        }

        if (empty($bean->media_note_id) && !empty($bean->template_id)) {
            require_once('modules/EmailTemplates/EmailTemplate.php');
            $template = BeanFactory::getBean('EmailTemplates', $bean->template_id);
            $attachments = $template->getAttachments();
            if ($attachments) {
                $bean->media_note_id = $attachments[0]->id;
            }
        }

        // Build signed URL for Twilio
        if (!empty($bean->media_note_id)) {
            $bean->media_url = $bean->buildSignedMediaUrl($bean->media_note_id);
        }
    }

    /**
     * Performs the API call to Twilio to send WhatsApp message.
     * 
     * @param array $params Must contain 'from', 'text', 'to', plus optional WhatsApp params
     * @return array Result with 'code', 'message', and optional 'twilio_sid', 'status'
     */
    protected function performApiCall(array $params): array {
        $from = $params['from'] ?? null;
        $message = $params['text'] ?? '';
        $phone = $params['to'] ?? '';
        $templateSid = $params['templateSid'] ?? null;
        $beans = $params['beans'] ?? [];
        $mediaUrl = $params['mediaUrl'] ?? null;

        $phone = $this->formatPhoneNumber($phone);

        if (empty($phone)) {
            return $this->buildError(translate('LBL_TWILIO_INVALID_PHONE', 'stic_Messages'));
        }

        $result = $this->callTwilioApi($from, $message, $phone, $templateSid, $beans, $mediaUrl);
        
        $resultArray = json_decode($result, true);
        
        if (!isset($resultArray['success']) || !$resultArray['success']) {
            return $this->buildError($result);
        }
        
        $twilioStatus = $resultArray['data']['status'] ?? '';
        $isFailedStatus = in_array($twilioStatus, ['failed', 'undelivered']);
        $hasErrorCode = !empty($resultArray['data']['error_code']);

        if (isset($resultArray['data']['sid']) && !$hasErrorCode && !$isFailedStatus) {
            $this->logInfo('WhatsApp message accepted. SID: ' . $resultArray['data']['sid'] . ' Twilio status: ' . $twilioStatus);
            return $this->buildSuccess(
                translate('LBL_MESSAGE_SENT', 'stic_Messages'),
                [
                    'twilio_sid' => $resultArray['data']['sid'],
                    'status' => $twilioStatus,
                ]
            );
        }

        $errorMessage = $resultArray['data']['error_message']
            ?? $resultArray['data']['message']
            ?? '';
        if (empty($errorMessage)) {
            $errorMessage = $isFailedStatus
                ? translate('LBL_TWILIO_STATUS_FAILED', 'stic_Messages')
                : translate('LBL_TWILIO_UNKNOWN_ERROR', 'stic_Messages');
        }
        $this->logError('WhatsApp send failed. SID: ' . ($resultArray['data']['sid'] ?? 'none') . ' - ' . $errorMessage);
        return $this->buildError($errorMessage);
    }

    // -------------------------------------------------------------------------
    // Private/Protected methods specific to WhatsApp/Twilio
    // -------------------------------------------------------------------------

    /**
     * Calls the Twilio API to send the WhatsApp message.
     */
    private function callTwilioApi(
        ?string $sender, 
        string $message, 
        string $phone, 
        ?string $templateSid = null, 
        array $beans = [], 
        ?string $mediaUrl = null
    ): string {
        if (!$this->isConfigured()) {
            return json_encode([
                'success' => false,
                'message' => translate('LBL_TWILIO_CONFIG_INCOMPLETE', 'stic_Messages')
            ]);
        }

        if (empty($phone) || (empty($message) && empty($templateSid) && empty($mediaUrl))) {
            return json_encode([
                'success' => false,
                'message' => translate('LBL_TWILIO_EMPTY_PHONE_OR_MESSAGE', 'stic_Messages')
            ]);
        }

        $from = 'whatsapp:' . $this->twilioNumber;
        $to = 'whatsapp:' . $phone;

        $postData = [
            'From' => $from,
            'To' => $to
        ];

        if (!empty($mediaUrl)) {
            $postData['MediaUrl'] = $mediaUrl;
        }

        if (!empty($templateSid)) {
            $postData['ContentSid'] = $templateSid;
            $vars = $this->buildTwilioContentVariables($message, $beans);
            $postData['ContentVariables'] = json_encode((object) $vars);
        } else {
            if (strpos($message, 'HX') === 0) {
                $postData['ContentSid'] = $message;
            } else {
                $postData['Body'] = $message;
            }
        }

        $url = $this->apiUrl . '/Accounts/' . $this->sid . '/Messages.json';
        
        $ch = curl_init();
        curl_setopt($ch, CURLOPT_URL, $url);
        curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
        curl_setopt($ch, CURLOPT_POST, true);
        curl_setopt($ch, CURLOPT_POSTFIELDS, http_build_query($postData));
        curl_setopt($ch, CURLOPT_USERPWD, $this->sid . ':' . $this->token);
        curl_setopt($ch, CURLOPT_HTTPHEADER, ['Content-Type: application/x-www-form-urlencoded']);
        curl_setopt($ch, CURLOPT_TIMEOUT, 7500);

        $response = curl_exec($ch);
        $httpCode = curl_getinfo($ch, CURLINFO_HTTP_CODE);

        if ($response === false) {
            $errorNumber = curl_errno($ch);
            $errorMessage = curl_error($ch);
            curl_close($ch);
            
            $this->logFatal('Error sending WhatsApp ' . __METHOD__ . ' ' . __LINE__ . ' - ' . $errorNumber . ' - ' . $errorMessage);
            $errorMsg = $errorNumber . '-' . $errorMessage;
            return json_encode([
                'success' => false, 
                'message' => $errorMsg
            ]);
        }

        curl_close($ch);

        $responseData = json_decode($response, true);

        if ($httpCode >= 200 && $httpCode < 300) {
            return json_encode([
                'success' => true, 
                'data' => $responseData
            ]);
        }

        $errorMessage = $responseData['message'] ?? translate('LBL_TWILIO_UNKNOWN_ERROR', 'stic_Messages');
        return json_encode([
            'success' => false,
            'message' => $errorMessage
        ]);
    }

    /**
     * Formats phone number to international format (+34XXXXXXXXX).
     */
    private function formatPhoneNumber($phone): string {
        $phone = trim($phone);
        if (strpos($phone, '+') === 0) {
            return '+' . preg_replace('/[^0-9]/', '', substr($phone, 1));
        }
        $digits = preg_replace('/[^0-9]/', '', $phone);
        if (strlen($digits) === 9) {
            return '+34' . $digits;
        }
        if (strlen($digits) === 10 && substr($digits, 0, 2) === '34') {
            return '+' . $digits;
        }
        return '';
    }

    /**
     * Checks if all required configuration is present.
     */
    public function isConfigured(): bool {
        return !empty($this->sid) && !empty($this->token) && !empty($this->twilioNumber);
    }

    /**
     * Validates configuration and returns list of errors.
     */
    public function validateConfig(): array {
        $errors = [];
        if (empty($this->sid)) $errors[] = translate('LBL_TWILIO_SID_MISSING', 'stic_Messages');
        if (empty($this->token)) $errors[] = translate('LBL_TWILIO_TOKEN_MISSING', 'stic_Messages');
        if (empty($this->twilioNumber)) $errors[] = translate('LBL_TWILIO_NUMBER_MISSING', 'stic_Messages');
        return $errors;
    }

    /**
     * Extracts $variable placeholders from the template body, resolves them
     * against the provided beans, and returns a Twilio contentVariables array.
     * 
     * @param string $templateBody Raw template body with $variable placeholders
     * @param array $beans Ordered list of SugarBean objects to resolve against
     * @return array ["1" => "value1", "2" => "value2", ...]
     */
    protected function buildTwilioContentVariables(string $templateBody, array $beans): array {
        preg_match_all('/\$([a-zA-Z_][a-zA-Z0-9_]*)/', $templateBody, $matches);
        $placeholders = array_unique($matches[1] ?? []);

        if (empty($placeholders)) {
            return [];
        }

        $contentVariables = [];
        $index = 1;
        foreach ($placeholders as $placeholder) {
            foreach ($beans as $bean) {
                $testText = '$' . $placeholder;
                $result = stic_Messages::replaceTemplateVariables($testText, $bean);
                if ($result !== $testText && $result !== '') {
                    $contentVariables[(string)$index] = $result;
                    $index++;
                    break;
                }
            }
        }

        return $contentVariables;
    }

    /**
     * {@inheritdoc}
     */
    protected function getSpecificUIConfig(): array {
        return [
            'lockSender' => true,
            'lockMessageOnTemplate' => true,
        ];
    }
}
