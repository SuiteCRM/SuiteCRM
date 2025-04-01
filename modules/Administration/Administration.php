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

require_once('data/SugarBean.php');
require_once('include/OutboundEmail/OutboundEmail.php');

#[\AllowDynamicProperties]
class Administration extends SugarBean
{
    public $settings = array();
    public $table_name = "config";
    public $object_name = "Administration";
    public $new_schema = true;
    public $module_dir = 'Administration';
    public $config_categories = array(
        // 'mail', // cn: moved to include/OutboundEmail
        'disclosure', // appended to all outbound emails
        'notify',
        'system',
        'portal',
        'proxy',
        'massemailer',
        'ldap',
        'captcha',
        'sugarpdf',
    );
    public $disable_custom_fields = true;
    public $checkbox_fields = array("notify_send_by_default", "mail_smtpauth_req", "notify_on", 'portal_on', 'system_mailmerge_on', 'proxy_auth', 'proxy_on', 'system_ldap_enabled', 'captcha_on');

    public function __construct()
    {
        parent::__construct();

        $this->setupCustomFields('Administration');
    }

    public function checkSmtpError($displayWarning = true)
    {
        global $sugar_config;

        $smtp_error = false;
        $this->retrieveSettings();

        if (!$sugar_config['email_warning_notifications']) {
            $displayWarning = false;
        }

        //If sendmail has been configured by setting the config variable ignore this warning
        $sendMailEnabled = isset($sugar_config['allow_sendmail_outbound']) && $sugar_config['allow_sendmail_outbound'];

        // remove php notice from installer
        if (!array_key_exists('mail_smtpserver', $this->settings)) {
            $this->settings['mail_smtpserver'] = '';
        }

        if (trim($this->settings['mail_smtpserver']) == '' && !$sendMailEnabled) {
            if (isset($this->settings['notify_on']) && $this->settings['notify_on']) {
                $smtp_error = true;
            }
        }

        if ($displayWarning && $smtp_error) {
            displayAdminError(translate('WARN_NO_SMTP_SERVER_AVAILABLE_ERROR', 'Administration'));
        }


        return $smtp_error;
    }

    public function retrieveSettings($category = false, $clean = false)
    {
        $categoryQuoted = $this->db->quote($category);

        // declare a cache for all settings
        $settings_cache = sugar_cache_retrieve('admin_settings_cache');

        if ($clean) {
            $settings_cache = array();
        }

        // Check for a cache hit
        if (!empty($settings_cache)) {
            $this->settings = $settings_cache;
            if (!empty($this->settings[$category])) {
                return $this;
            }
        }

        if (!empty($category)) {
            $query = "SELECT category, name, value FROM {$this->table_name} WHERE category = '$categoryQuoted'";
        } else {
            $query = "SELECT category, name, value FROM {$this->table_name}";
        }

        $result = $this->db->query($query, true, "Unable to retrieve system settings");

        if (empty($result)) {
            return null;
        }

        while ($row = $this->db->fetchByAssoc($result)) {
            if ($row['category'] . "_" . $row['name'] == 'ldap_admin_password' || $row['category'] . "_" . $row['name'] == 'proxy_password') {
                $this->settings[$row['category'] . "_" . $row['name']] = $this->decrypt_after_retrieve($row['value']);
            } else {
                $this->settings[$row['category'] . "_" . $row['name']] = $row['value'];
            }
            $this->settings[$row['category']] = true;
        }
        $this->settings[$category] = true;

        if (!isset($this->settings["mail_sendtype"])) {
            // outbound email settings
            $oe = new OutboundEmail();
            $oe->getSystemMailerSettings();

            foreach ($oe->field_defs as $def) {
                // fixes installer php notice
                if (!array_key_exists($def, $this->settings)) {
                    $this->settings[$def] = '';
                }

                if (strpos((string) $def, "mail_") !== false) {
                    $this->settings[$def] = $oe->$def;
                }
                if (strpos((string) $def, "smtp") !== false) {
                    $this->settings[$def] = $oe->$def;
                }
            }
        }

        // At this point, we have built a new array that should be cached.
        sugar_cache_put('admin_settings_cache', $this->settings);
        return $this;
    }

    public function saveConfig()
    {


        // outbound email settings
        $oe = new OutboundEmail();

        foreach ($_POST as $key => $val) {
            $prefix = $this->get_config_prefix($key);
            if (in_array($prefix[0], $this->config_categories)) {
                if (is_array($val)) {
                    $val = implode(",", $val);
                }
                $this->saveSetting($prefix[0], $prefix[1], trim($val));
            }
            if (strpos($key, "mail_") !== false) {
                if (in_array($key, $oe->field_defs)) {
                    $oe->$key = trim($val);
                }
            }
        }

        //saving outbound email from here is probably redundant, adding a check to make sure
        //smtpserver name is set.
        if (!empty($oe->mail_smtpserver)) {
            $oe->saveSystem();
        }

        $this->retrieveSettings(false, true);
    }

    /**
     * @param string $category
     * @param string $key
     * @param string $value
     * @return int
     */
    public function saveSetting($category, $key, $value)
    {
        $categoryQuoted = $this->db->quote($category);
        $keyQuoted = $this->db->quote($key);
        $valueQuoted = $this->db->quote($value);

        $result = $this->db->query("SELECT count(*) AS the_count FROM config WHERE category = '$categoryQuoted' AND name = '$keyQuoted'");
        $row = $this->db->fetchByAssoc($result);
        $row_count = $row['the_count'];

        if ($category . "_" . $keyQuoted === 'ldap_admin_password' || $categoryQuoted . "_" . $keyQuoted === 'proxy_password') {
            $valueQuoted = $this->encrpyt_before_save($value);
        }

        if ($row_count == 0) {
            $result = $this->db->query("INSERT INTO config (value, category, name) VALUES ('$valueQuoted','$categoryQuoted', '$keyQuoted')");
        } else {
            $result = $this->db->query("UPDATE config SET value = '$valueQuoted' WHERE category = '$categoryQuoted' AND name = '$keyQuoted'");
        }
        sugar_cache_clear('admin_settings_cache');

        return $this->db->getAffectedRowCount($result);
    }

    public function get_config_prefix($str)
    {
        return $str ? array(substr((string) $str, 0, strpos((string) $str, "_")), substr((string) $str, strpos((string) $str, "_") + 1)) : array(false, false);
    }
}
