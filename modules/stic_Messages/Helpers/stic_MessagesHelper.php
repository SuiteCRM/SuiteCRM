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

/**
 * Abstract base class for message helpers.
 * New providers must extend this class and implement the abstract methods.
 */
abstract class stic_MessagesHelper {

    protected bool $active = false;
    protected array $config = [];

    // -------------------------------------------------------------------------
    // Abstract methods
    // -------------------------------------------------------------------------

    /** Provider name identifier (e.g., 'sms', 'whatsapp') */
    abstract protected function getProviderName(): string;

    /** Required setting keys for loadConfig() */
    abstract protected function getRequiredSettings(): array;

    /** Performs the API call to send the message */
    abstract protected function performApiCall(array $params): array;

    /** UI behavior configuration for JavaScript (final - do not override) */
    abstract protected function getSpecificUIConfig(): array;

    // -------------------------------------------------------------------------
    // UI Config
    // -------------------------------------------------------------------------

    /**
     * Default UI configuration values.
     * If in the future you add a new parameter here, it will be automatically
     * available to all child classes.
     */
    protected function getDefaultUIConfig(): array {
        return [
            'lockSender' => false,
            'lockMessageOnTemplate' => false,
            'fixedStatus' => null,
            'canRetry' => true,
            'hideAttachment' => false,
            'allowedStatus' => ['sent', 'error', 'draft'],
        ];
    }

    /**
     * Public method called by the application.
     * Final so no child class can break it.
     */
    final public function getUIConfig(): array {
        return array_merge(
            $this->getDefaultUIConfig(),
            $this->getSpecificUIConfig()
        );
    }

    // -------------------------------------------------------------------------
    // Concrete methods
    // -------------------------------------------------------------------------

    /**
     * Returns the helper type identifier.
     * Defaults to provider name, can be overridden if needed.
     */
    public function getHelperType(): string {
        return $this->getProviderName();
    }

    /**
     * Returns the template type used by this helper.
     * Defaults to provider name, can be overridden if needed.
     */
    public function getTemplateType(): string {
        return $this->getProviderName();
    }

    /**
     * Returns true if this helper passes template body to the external provider.
     * Override in subclasses that need this behavior (e.g., WhatsApp with Twilio templates).
     */
    public function passesTemplateBodyToProvider(): bool {
        return false;
    }

    /**
     * Sends a message through the provider.
     * 
     * @param string|null $from Sender identifier
     * @param string $text Message body
     * @param string $to Recipient phone number
     * @param mixed ...$args Additional provider-specific arguments
     * 
     * @return array Result with 'code' and 'message' keys
     */
    public function sendMessage(?string $from, string $text, string $to, ...$args): array {
        if (!$this->isActive()) {
            return $this->buildError(translate('LBL_HELPER_MODULE_NOT_ACTIVE', 'stic_Messages'));
        }
        $params = $this->buildSendParams($from, $text, $to, ...$args);
        return $this->performApiCall($params);
    }

    // -------------------------------------------------------------------------
    // Lifecycle hooks - override in subclasses as needed
    // -------------------------------------------------------------------------

    /** Prepare the bean before saving (sender, status, etc.) */
    public function prepareBeanBeforeSave(stic_Messages $bean): void {}

    /** Resolve media attachment for the message (e.g., build signed URL for Twilio) */
    public function resolveMedia(stic_Messages $bean): void {}

    /** Return true to skip external API call (e.g., redirect, internal message) */
    public function shouldSkipApiCall(): bool {
        return false;
    }

    /** Process bean after successful send or skip */
    public function processSuccessfulSend(stic_Messages $bean): void {
        global $timedate;
        $bean->status = 'sent';
        $bean->response = 'Message sent';
        $bean->sent_date = $timedate->nowDb();
    }

    /** Process bean after failed send */
    public function processFailedSend(stic_Messages $bean, string $errorMessage): void {
        $bean->status = 'error';
        $bean->response = $errorMessage;
    }

    /** Whether this message type can be edited after creation */
    public function isEditableAfterCreate(): bool {
        return true;
    }

    /** Whether failed messages can be retried */
    public function isRetryable(): bool {
        return true;
    }

    /** Custom response data for single-save controller action */
    public function getSaveResponseData(stic_Messages $bean): array {
        return [];
    }

    /** Custom response data for mass-save controller action */
    public function getMassSaveResponseData(array $phones, string $message, array $idsArray = []): array {
        return [];
    }

    /** Post-save processing (e.g., M:M relationships) */
    public function processAfterSave(stic_Messages $bean): void {}

    // -------------------------------------------------------------------------
    // Protected utilities
    // -------------------------------------------------------------------------

    /**
     * Loads configuration from stic_Settings based on getRequiredSettings().
     */
    protected function loadConfig(): void {
        require_once('modules/stic_Settings/Utils.php');
        $this->config = [];
        foreach ($this->getRequiredSettings() as $settingKey) {
            $this->config[$settingKey] = stic_SettingsUtils::getSetting($settingKey);
        }
    }

    /**
     * Checks if this helper is active and properly configured.
     */
    protected function isActive(): bool {
        return $this->active;
    }

    /**
     * Builds the parameters array for performApiCall().
     * Override in subclasses to add provider-specific parameters.
     * 
     * @param string|null $from
     * @param string $text
     * @param string $to
     * @param mixed ...$args
     * @return array
     */
    protected function buildSendParams(?string $from, string $text, string $to, ...$args): array {
        return ['from' => $from, 'text' => $text, 'to' => $to];
    }

    protected function buildError(string $message): array {
        return ['code' => stic_Messages::ERROR_NOT_SENT, 'message' => $message];
    }

    protected function buildSuccess(string $message, array $extra = []): array {
        return array_merge(['code' => stic_Messages::OK, 'message' => $message], $extra);
    }

    protected function logInfo(string $message): void {
        $GLOBALS['log']->info($message);
    }

    protected function logError(string $message): void {
        $GLOBALS['log']->error($message);
    }

    protected function logFatal(string $message): void {
        $GLOBALS['log']->fatal($message);
    }
}
