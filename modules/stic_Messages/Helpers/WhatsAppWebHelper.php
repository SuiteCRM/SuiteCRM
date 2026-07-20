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

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once('modules/stic_Messages/Helpers/stic_MessagesHelper.php');

/**
 * Helper for WhatsAppWeb messages (redirect to WhatsApp Web).
 *
 * This type doesn't send via API — it opens WhatsApp Web in the browser.
 * Messages are immutable after creation and cannot be retried.
 */
class WhatsAppWebHelper extends stic_MessagesHelper {

    /**
     * {@inheritdoc}
     */
    protected function getProviderName(): string {
        return 'whatsapp_web';
    }

    public function getHelperType(): string {
        return 'whatsapp_web';
    }

    /**
     * {@inheritdoc}
     */
    protected function getRequiredSettings(): array {
        return [];
    }

    /**
     * {@inheritdoc}
     */
    protected function performApiCall(array $params): array {
        return $this->buildError('WhatsAppWeb does not use API calls');
    }

    /**
     * {@inheritdoc}
     */
    protected function getSpecificUIConfig(): array {
        return [
            'lockSender' => true,
            'fixedStatus' => 'redirected',
            'canRetry' => false,
            'allowedStatus' => ['sent', 'redirected'],
        ];
    }

    /**
     * {@inheritdoc}
     */
    public function isEditableAfterCreate(): bool {
        return false;
    }

    /**
     * {@inheritdoc}
     */
    public function isRetryable(): bool {
        return false;
    }

    /**
     * {@inheritdoc}
     */
    public function shouldSkipApiCall(): bool {
        return true;
    }

    /**
     * {@inheritdoc}
     */
    public function prepareBeanBeforeSave(stic_Messages $bean): void {
        global $current_user;

        // Set sender to current user name
        if (empty($bean->sender)) {
            $bean->sender = $current_user->name;
        }

        // WhatsAppWeb messages are always sent immediately, never saved as draft
        if ($bean->status === 'draft') {
            $bean->status = 'sent';
        }
    }

    /**
     * {@inheritdoc}
     */
    public function processSuccessfulSend(stic_Messages $bean): void {
        global $timedate;

        // Mark as redirected because user is redirected to WhatsApp Web
        $bean->status = 'redirected';
        $bean->response = 'Redirected to WhatsApp Web';
        $bean->sent_date = $timedate->nowDb();
    }

    /**
     * {@inheritdoc}
     */
    public function getSaveResponseData(stic_Messages $bean): array {
        $phone = !empty($bean->phone) ? preg_replace('/\D+/', '', $bean->phone) : '';
        $text = $bean->message ?? '';

        // Resolve template variables if parent exists
        if (!empty($bean->parent_type) && !empty($bean->parent_id)) {
            $parentBean = BeanFactory::getBean($bean->parent_type, $bean->parent_id);
            $text = stic_Messages::replaceTemplateVariables($text, $parentBean);
        }

        return [
            'type' => 'whatsapp_web',
            'phone' => $phone,
            'text' => $text,
            'id' => $bean->id,
        ];
    }

    /**
     * {@inheritdoc}
     */
    public function getMassSaveResponseData(array $phones, string $message, array $idsArray = []): array {
        $openData = [];

        foreach ($phones as $index => $p) {
            $p = trim($p);
            if ($p === '') continue;

            $phoneClean = preg_replace('/\D+/', '', $p);
            if ($phoneClean === '') continue;

            $processedText = $message;
            if (!empty($idsArray[$index])) {
                $parentBean = BeanFactory::getBean($_REQUEST['return_module'] ?? '', $idsArray[$index]);
                $processedText = stic_Messages::replaceTemplateVariables($message, $parentBean);
            }

            $openData[] = ['phone' => $phoneClean, 'text' => $processedText];
        }

        return [
            'type' => 'whatsapp_web',
            'open_data' => $openData,
        ];
    }
}
