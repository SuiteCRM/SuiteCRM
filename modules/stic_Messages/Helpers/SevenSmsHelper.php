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

// SMS Helper class to send SMS messages through Seven provider.
// Info about API can be found at: https://docs.seven.io/en/rest-api/endpoints/sms

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once('modules/stic_Messages/Helpers/stic_MessagesHelper.php');

class SevenSMSHelper extends stic_MessagesHelper {

    protected ?string $apiKey = null;
    protected ?string $sender = null;

    /**
     * Returns the provider name identifier.
     */
    protected function getProviderName(): string {
        return 'sms';
    }

    /**
     * Returns the helper type matching the channel.
     */
    public function getHelperType(): string {
        return 'sms';
    }

    /**
     * Returns the list of required setting keys.
     */
    protected function getRequiredSettings(): array {
        return ['seven_active', 'seven_api_key', 'messages_sender'];
    }

    /**
     * Constructor - loads configuration and initializes provider.
     */
    public function __construct() {
        $this->loadConfig();
        
        $this->active = ('1' === ($this->config['seven_active'] ?? '0'));
        $this->apiKey = $this->config['seven_api_key'] ?? null;
        $this->sender = $this->config['messages_sender'] ?? null;
    }

    /**
     * Performs the API call to Seven.io to send SMS.
     * 
     * @param array $params Must contain 'from', 'text', 'to'
     * @return array Result with 'code' and 'message'
     */
    protected function performApiCall(array $params): array {
        $from = $params['from'] ?? null;
        $text = $params['text'] ?? '';
        $to = $params['to'] ?? '';

        // Remove non-numeric values from phone
        $to = preg_replace('~[^\d,]~', '', $to);

        if (!$this->isActive()) {
            return $this->buildError('Module not active');
        }

        $curlOpts = [
            CURLOPT_HTTPHEADER => [
                'Accept: application/json',
                'Content-type: application/json',
                'SentWith: SuiteCRM',
                'X-Api-Key: ' . $this->apiKey,
            ],
            CURLOPT_POSTFIELDS => json_encode(compact('from', 'text', 'to')),
            CURLOPT_RETURNTRANSFER => true,
            CURLOPT_TIMEOUT => 7500,
        ];

        $curl = curl_init('https://gateway.seven.io/api/sms');
        curl_setopt_array($curl, $curlOpts);
        $response = curl_exec($curl);

        if ($response === false) {
            $errorNumber = curl_errno($curl);
            $errorMessage = curl_error($curl);
            $this->logFatal('Error sending SMS ' . __METHOD__ . ' ' . __LINE__ . ' - ' . $errorNumber . ' - ' . $errorMessage);
            curl_close($curl);
            return $this->buildError($errorNumber . '-' . $errorMessage);
        }

        curl_close($curl);

        $resultArray = json_decode($response, true);

        if (!isset($resultArray['success']) || $resultArray['success'] != 100) {
            return $this->buildError($response);
        }

        if (isset($resultArray['messages'][0]['success']) && $resultArray['messages'][0]['success']) {
            $this->logInfo('SMS message sent successfully');
            return $this->buildSuccess('Message sent');
        }

        return $this->buildError($response);
    }

    /**
     * {@inheritdoc}
     */
    protected function getSpecificUIConfig(): array {
        return [];
    }
}
