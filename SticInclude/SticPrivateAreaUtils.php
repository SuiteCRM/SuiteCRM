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

class SticPrivateAreaUtils
{
    /**
     * Prepare portal credentials when Portal is enabled
     *
     * @param SugarBean $bean
     * @return void
     */
    public static function processBeforeSave($bean)
    {
        // When Portal is being disabled, keep credentials intact
        if (!self::isPortalEnabledNow($bean) && self::wasPortalEnabled($bean)) {
            $storedPassword = self::getStoredPortalPassword($bean);
            $fetchedPassword = (string)($bean->fetched_row['stic_pa_password_c'] ?? '');
            $bean->_stic_plain_pa_password = '';
            if ($fetchedPassword !== '') {
                $bean->stic_pa_password_c = $fetchedPassword;
            } elseif ($storedPassword !== '') {
                $bean->stic_pa_password_c = $storedPassword;
            }
            return;
        }

        // Skip credential logic when Portal is disabled
        if (!self::isPortalEnabledNow($bean)) {
            $storedPassword = self::getStoredPortalPassword($bean);
            $fetchedPassword = (string)($bean->fetched_row['stic_pa_password_c'] ?? '');
            $bean->_stic_plain_pa_password = '';
            if ($fetchedPassword !== '') {
                $bean->stic_pa_password_c = $fetchedPassword;
            } elseif ($storedPassword !== '') {
                $bean->stic_pa_password_c = $storedPassword;
            }
            return;
        }

        // Default username to primary email when empty
        if (empty($bean->stic_pa_username_c)) {
            $primaryEmail = self::getPrimaryEmail($bean);
            if (!empty($primaryEmail)) {
                $bean->stic_pa_username_c = $primaryEmail;
            }
        }

        $isBeingEnabled = self::isPortalBeingEnabled($bean);
        $submittedPassword = (string)($_REQUEST['stic_pa_password_c'] ?? '');
        $hasSubmittedPassword = (array_key_exists('stic_pa_password_c', $_REQUEST) && $submittedPassword !== '');
        $storedPassword = self::getStoredPortalPassword($bean);
        $fetchedPassword = (string)($bean->fetched_row['stic_pa_password_c'] ?? '');

        if ($isBeingEnabled && !$hasSubmittedPassword) {
            // On enable, generate only when no existing password is stored
            if ($fetchedPassword !== '') {
                $bean->stic_pa_password_c = $fetchedPassword;
                $bean->_stic_plain_pa_password = '';
            } elseif ($storedPassword !== '') {
                $bean->stic_pa_password_c = $storedPassword;
                $bean->_stic_plain_pa_password = '';
            } else {
                $generatedPassword = self::generateRandomPassword();
                $bean->_stic_plain_pa_password = $generatedPassword;
                $bean->stic_pa_password_c = $generatedPassword;
            }
        } elseif (!$hasSubmittedPassword) {
            // Keep existing password when it was not changed in the form
            $bean->_stic_plain_pa_password = '';
            if ($fetchedPassword !== '') {
                $bean->stic_pa_password_c = $fetchedPassword;
            } elseif ($storedPassword !== '') {
                $bean->stic_pa_password_c = $storedPassword;
            }
        } else {
            // If submitted value is the hidden UI mask/fetched value, keep stored value
            if ($fetchedPassword !== '' && $submittedPassword === $fetchedPassword && $storedPassword !== '') {
                $bean->stic_pa_password_c = $storedPassword;
                $bean->_stic_plain_pa_password = '';
            } else {
                // Admin manually entered a new password in plain text
                $bean->_stic_plain_pa_password = $submittedPassword;
                $bean->stic_pa_password_c = $submittedPassword;
            }
        }

        // Prevent duplicate Portal usernames across Accounts and Contacts
        // TODO: Encrypt password functionality
        // self::assertUniquePortalUsername($bean);
    }

    /**
     * Send credentials email after enabling portal
     *
     * @param SugarBean $bean
     * @return void
     */
    public static function processAfterSave($bean)
    {
        // Send only when Portal has just been enabled (0 -> 1)
        if (!self::isPortalBeingEnabled($bean)) {
            return;
        }

        require_once 'modules/stic_Settings/Utils.php';

        // Check if sending credentials email is enabled in settings
        $sendSettingValue = (string)stic_SettingsUtils::getSetting('PRIVATEAREA_SEND_CREDENTIALS_ON_ENABLE');
        if ($sendSettingValue === '') {
            $GLOBALS['log']->error(__METHOD__ . ': Setting PRIVATEAREA_SEND_CREDENTIALS_ON_ENABLE is empty. Using default value 1.');
            $isEnabled = true;
        } else {
            $isEnabled = ($sendSettingValue === '1');
        }

        if (!$isEnabled) {
            $GLOBALS['log']->error(__METHOD__ . ': Credentials email disabled by setting PRIVATEAREA_SEND_CREDENTIALS_ON_ENABLE=' . $sendSettingValue);
            return;
        }

        // Resolve and validate template
        $templateId = self::getTemplateIdForModule($bean->module_dir ?? '');
        if (empty($templateId)) {
            $GLOBALS['log']->error(__METHOD__ . ': Template setting is empty. Skipping credentials email.');
            return;
        }

        $templateBean = BeanFactory::getBean('EmailTemplates', $templateId);
        if (empty($templateBean) || empty($templateBean->id) || !empty($templateBean->deleted)) {
            $GLOBALS['log']->error(__METHOD__ . ': Invalid credentials template id: ' . $templateId);
            return;
        }

        // Resolve destination address
        $destAddress = self::getPrimaryEmail($bean);
        if (empty($destAddress)) {
            $GLOBALS['log']->error(__METHOD__ . ': Destination email is empty. Skipping credentials email.');
            return;
        }

        $parsedMailArray = self::parsePortalTemplate($templateBean, $bean);

        $subject = $parsedMailArray['subject'] ?? '';
        $bodyHtml = $parsedMailArray['body_html'] ?? '';
        $bodyText = $parsedMailArray['body'] ?? '';

        if (empty($bodyHtml) && empty($bodyText)) {
            $GLOBALS['log']->error(__METHOD__ . ': Parsed credentials template is empty.');
            return;
        }

        require_once 'include/SugarPHPMailer.php';
        require_once 'modules/Emails/Email.php';

        $emailObj = new Email();
        $defaults = $emailObj->getSystemDefaultEmail();

        $mail = new SugarPHPMailer();
        $mail->setMailerForSystem();
        $mail->From = $defaults['email'] ?? '';
        $mail->FromName = $defaults['name'] ?? '';
        $mail->AddAddress($destAddress, self::getRecipientName($bean));
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
            $GLOBALS['log']->error(__METHOD__ . ': Error sending credentials email: ' . $mail->ErrorInfo);
            return;
        }

        $GLOBALS['log']->info(__METHOD__ . ': Credentials email sent to ' . $destAddress);
    }

    /**
     * Parse credentials template supporting Portal placeholders
     *
     * @param SugarBean $templateBean
     * @param SugarBean $bean
     * @return array
     */
    protected static function parsePortalTemplate($templateBean, $bean)
    {
        global $sugar_config;

        $parsedTemplate = [
            'subject' => $templateBean->subject ?? '',
            'body' => $templateBean->body ?? '',
            'body_html' => $templateBean->body_html ?? '',
        ];

        $modulePrefixes = self::getModuleTemplatePrefixes($bean->module_dir ?? '');
        $replacementsText = [];
        $replacementsHtml = [];

        foreach ($bean->field_defs as $fieldName => $def) {
            $rawValue = $bean->$fieldName ?? '';
            if (!is_scalar($rawValue)) {
                $rawValue = '';
            }

            $textValue = (string)$rawValue;
            $htmlValue = $textValue;

            if (isset($def['type']) && $def['type'] === 'text') {
                $htmlValue = nl2br($htmlValue);
            }

            foreach ($modulePrefixes as $prefix) {
                $placeholder = '$' . $prefix . '_' . $fieldName;
                $replacementsText[$placeholder] = $textValue;
                $replacementsHtml[$placeholder] = $htmlValue;
            }
        }

        $name = self::getRecipientName($bean);
        foreach ($modulePrefixes as $prefix) {
            $namePlaceholder = '$' . $prefix . '_name';
            $replacementsText[$namePlaceholder] = $name;
            $replacementsHtml[$namePlaceholder] = $name;
        }

        $specialSubstitutions = [
            '$sugarurl' => $sugar_config['site_url'] ?? '',
            '$portal_address' => $sugar_config['site_url'] ?? '',
        ];

        foreach ($specialSubstitutions as $placeholder => $value) {
            if (!is_scalar($value)) {
                $value = '';
            }
            $textValue = (string)$value;
            $htmlValue = $textValue;
            $replacementsText[$placeholder] = $textValue;
            $replacementsHtml[$placeholder] = $htmlValue;
        }

        uksort($replacementsText, function ($a, $b) {
            return strlen($b) - strlen($a);
        });
        uksort($replacementsHtml, function ($a, $b) {
            return strlen($b) - strlen($a);
        });

        $parsedTemplate['subject'] = str_replace(array_keys($replacementsText), array_values($replacementsText), $parsedTemplate['subject']);
        $parsedTemplate['body'] = str_replace(array_keys($replacementsText), array_values($replacementsText), $parsedTemplate['body']);
        $parsedTemplate['body_html'] = str_replace(array_keys($replacementsHtml), array_values($replacementsHtml), $parsedTemplate['body_html']);

        $cleanupPattern = '/\$[a-zA-Z_][a-zA-Z0-9_]*/';
        $parsedTemplate['subject'] = preg_replace($cleanupPattern, '', $parsedTemplate['subject']);
        $parsedTemplate['body'] = preg_replace($cleanupPattern, '', $parsedTemplate['body']);
        $parsedTemplate['body_html'] = preg_replace($cleanupPattern, '', $parsedTemplate['body_html']);

        return $parsedTemplate;
    }

    /**
     * Return accepted placeholder prefixes by module
     *
     * @param string $module
     * @return array
     */
    protected static function getModuleTemplatePrefixes($module)
    {
        if ($module === 'Contacts') {
            return ['contact', 'contacts'];
        }

        if ($module === 'Accounts') {
            return ['contact_account', 'accounts'];
        }

        return [strtolower((string)$module)];
    }

    /**
     * Detect transition stic_pa_enable_c from false to true
     *
     * @param SugarBean $bean
     * @return bool
     */
    protected static function isPortalBeingEnabled($bean)
    {
        $current = (bool)($bean->stic_pa_enable_c ?? false);
        $previous = (bool)($bean->fetched_row['stic_pa_enable_c'] ?? false);

        return $current && !$previous;
    }

    /**
     * Check if Portal was enabled in the previously stored values
     *
     * @param SugarBean $bean
     * @return bool
     */
    protected static function wasPortalEnabled($bean)
    {
        return (bool)($bean->fetched_row['stic_pa_enable_c'] ?? false);
    }

    /**
     * Check if Portal is enabled in current bean values
     *
     * @param SugarBean $bean
     * @return bool
     */
    protected static function isPortalEnabledNow($bean)
    {
        return (bool)($bean->stic_pa_enable_c ?? false);
    }

    /**
     * Ensure Portal username is unique across Contacts and Accounts
     *
     * @param SugarBean $bean
     * @return void
     * @throws RuntimeException
     * TODO: Encrypt password functionality
     */
    protected static function assertUniquePortalUsername($bean)
    {
        $username = trim((string)($bean->stic_pa_username_c ?? ''));
        if (empty($username)) {
            return;
        }

        if (!self::hasPortalUsernameDuplicate($username, $bean->id ?? '')) {
            return;
        }

        $message = "Portal username '{$username}' is already in use by another enabled record. Please use a unique username.";
        $GLOBALS['log']->error(__METHOD__ . ': ' . $message);
        throw new \RuntimeException($message);
    }

    /**
     * Check username collisions in enabled Contacts and Accounts
     *
     * @param string $username
     * @param string $currentId
     * @return bool
     * TODO: Encrypt password functionality
     */
    protected static function hasPortalUsernameDuplicate($username, $currentId = '')
    {
        global $db;

        $usernameQuoted = $db->quote($username);
        $currentIdQuoted = $db->quote((string)$currentId);

        $sql = "
            SELECT 1
            FROM (
                SELECT c.id AS id
                FROM contacts c
                INNER JOIN contacts_cstm cc ON cc.id_c = c.id
                WHERE c.deleted = 0
                  AND cc.stic_pa_enable_c = 1
                  AND cc.stic_pa_username_c = '{$usernameQuoted}'

                UNION ALL

                SELECT a.id AS id
                FROM accounts a
                INNER JOIN accounts_cstm ac ON ac.id_c = a.id
                WHERE a.deleted = 0
                  AND ac.stic_pa_enable_c = 1
                  AND ac.stic_pa_username_c = '{$usernameQuoted}'
            ) x
            WHERE x.id <> '{$currentIdQuoted}'
            LIMIT 1
        ";

        return !empty($db->getOne($sql));
    }

    /**
     * Get credentials template setting depending on module
     *
     * @param string $module
     * @return string|null
     */
    protected static function getTemplateIdForModule($module)
    {
        require_once 'modules/stic_Settings/Utils.php';

        if ($module === 'Contacts') {
            return stic_SettingsUtils::getSetting('PRIVATEAREA_CREDENTIALS_TEMPLATE_CONTACTS') ?: null;
        }

        if ($module === 'Accounts') {
            return stic_SettingsUtils::getSetting('PRIVATEAREA_CREDENTIALS_TEMPLATE_ACCOUNTS') ?: null;
        }

        return null;
    }

    /**
     * Get primary email address from bean
     *
     * @param SugarBean $bean
     * @return string
     */
    protected static function getPrimaryEmail($bean)
    {
        if (!empty($bean->email1)) {
            return $bean->email1;
        }

        if (!empty($bean->emailAddress)) {
            $email = $bean->emailAddress->getPrimaryAddress($bean);
            if (!empty($email)) {
                return $email;
            }
        }

        return '';
    }

    /**
    * Get currently stored Portal password (plain text)
     *
     * @param SugarBean $bean
     * @return string
     */
    protected static function getStoredPortalPassword($bean)
    {
        $module = $bean->module_dir ?? '';
        $id = $bean->id ?? '';

        if (empty($module) || empty($id)) {
            return '';
        }

        $storedBean = BeanFactory::getBean($module, $id, ['disable_row_level_security' => true]);
        if (empty($storedBean) || empty($storedBean->id)) {
            return '';
        }

        $password = $storedBean->stic_pa_password_c ?? '';
        return is_scalar($password) ? (string)$password : '';
    }

    /**
     * Get recipient display name
     *
     * @param SugarBean $bean
     * @return string
     */
    protected static function getRecipientName($bean)
    {
        if (($bean->module_dir ?? '') === 'Contacts') {
            $fullName = trim(($bean->first_name ?? '') . ' ' . ($bean->last_name ?? ''));
            return !empty($fullName) ? $fullName : ($bean->name ?? '');
        }

        return $bean->name ?? '';
    }

    /**
     * Generate random password
     *
     * @return string
     */
    protected static function generateRandomPassword()
    {
        try {
            return bin2hex(random_bytes(6));
        } catch (\Exception $e) {
            return substr(md5(uniqid((string)mt_rand(), true)), 0, 12);
        }
    }
}
