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



require_once('include/MVC/View/SugarView.php');
require_once('modules/EmailMan/Forms.php');

#[\AllowDynamicProperties]
class ViewConfig extends SugarView
{
    /**
     * @see SugarView::_getModuleTitleParams()
     */
    protected function _getModuleTitleParams($browserTitle = false)
    {
        global $mod_strings;

        return array(
           "<a href='index.php?module=Administration&action=index'>".translate('LBL_MODULE_NAME', 'Administration')."</a>",
           translate('LBL_MASS_EMAIL_CONFIG_TITLE', 'Administration'),
           );
    }

    /**
     * @see SugarView::preDisplay()
     */
    public function preDisplay()
    {
        global $current_user;

        if (!is_admin($current_user)
                && !is_admin_for_module($GLOBALS['current_user'], 'Emails')
                && !is_admin_for_module($GLOBALS['current_user'], 'Campaigns')) {
            sugar_die("Unauthorized access to administration.");
        }
    }

    /**
     * @see SugarView::display()
     */
    public function display()
    {
        global $mod_strings;
        global $app_list_strings;
        global $app_strings;
        global $current_user;
        global $sugar_config;

        echo $this->getModuleTitle(false);
        global $currentModule;

        $focus = BeanFactory::newBean('Administration');
        $focus->retrieveSettings(); //retrieve all admin settings.
        $GLOBALS['log']->info("Mass Emailer(EmailMan) ConfigureSettings view");


        $useLegacyEmailConfig = $sugar_config['legacy_email_behaviour'] ?? false;

        $this->ss->assign("legacyEmailConfigEnabled", $useLegacyEmailConfig);

        $this->ss->assign("MOD", $mod_strings);
        $this->ss->assign("APP", $app_strings);

        $this->ss->assign("RETURN_MODULE", "Administration");
        $this->ss->assign("RETURN_ACTION", "index");

        $this->ss->assign("MODULE", $currentModule);
        $this->ss->assign("PRINT_URL", "index.php?".$GLOBALS['request_string']);
        $this->ss->assign("HEADER", get_module_title("EmailMan", "{MOD.LBL_CONFIGURE_SETTINGS}", true));
        $this->ss->assign("notify_fromaddress", $focus->settings['notify_fromaddress']);
        $this->ss->assign("notify_send_from_assigning_user", (isset($focus->settings['notify_send_from_assigning_user']) && !empty($focus->settings['notify_send_from_assigning_user'])) ? "checked='checked'" : "");
        $this->ss->assign("notify_on", ($focus->settings['notify_on']) ? "checked='checked'" : "");
        if ($sugar_config['email_warning_notifications']) {
            $this->ss->assign("email_warning_notifications", "checked='checked'");
        } else {
            $this->ss->assign("email_warning_notifications", '');
        }
        $this->ss->assign("notify_fromname", $focus->settings['notify_fromname']);
        $this->ss->assign("notify_allow_default_outbound_on", (!empty($focus->settings['notify_allow_default_outbound']) && $focus->settings['notify_allow_default_outbound']) ? "checked='checked'" : "");

        $mailSmtpType = null;
        if (isset($focus->settings['mail_smtptype'])) {
            $mailSmtpType = $focus->settings['mail_smtptype'];
        } else {
            LoggerManager::getLogger()->error('EmailMan view display error: mail smtp type is not set for focus');
        }

        $mailSmtpServer = null;
        if (isset($focus->settings['mail_smtpserver'])) {
            $mailSmtpServer = $focus->settings['mail_smtpserver'];
        } else {
            LoggerManager::getLogger()->error('EmailMan view display error: mail smtp type is not set for focus');
        }

        $mailSmtpPort = null;
        if (isset($focus->settings['mail_smtpport'])) {
            $mailSmtpPort = $focus->settings['mail_smtpport'];
        } else {
            LoggerManager::getLogger()->error('EmailMan view display error: mail smtp port is not set for focus');
        }

        $mailSmtpUser = null;
        if (isset($focus->settings['mail_smtpuser'])) {
            $mailSmtpUser = $focus->settings['mail_smtpuser'];
        } else {
            LoggerManager::getLogger()->error('EmailMan view display error: mail smtp user is not set for focus');
        }

        $mailSmtpAuthReq = null;
        if (isset($focus->settings['mail_smtpauth_req'])) {
            $mailSmtpAuthReq = $focus->settings['mail_smtpauth_req'];
        } else {
            LoggerManager::getLogger()->error('EmailMan view display error: mail smtp auth req is not set for focus');
        }

        $mailSmtpSsl = null;
        if (isset($focus->settings['mail_smtpssl'])) {
            $mailSmtpSsl = $focus->settings['mail_smtpssl'];
        } else {
            LoggerManager::getLogger()->error('EmailMan view display error: mail smtp pass is not set for focus');
        }

        $mailSmtpPass = null;
        if (isset($focus->settings['mail_smtppass'])) {
            $mailSmtpPass = $focus->settings['mail_smtppass'];
        } else {
            LoggerManager::getLogger()->error('EmailMan view display error: mail smtp pass is not set for focus');
        }

        $mailSendType = null;
        if (isset($focus->settings['mail_sendtype'])) {
            $mailSendType = $focus->settings['mail_sendtype'];
        } else {
            LoggerManager::getLogger()->error('EmailMan view display error: mail send type is not set for focus');
        }

        $mailAllowUserSend = null;
        if (isset($sugar_config['email_allow_send_as_user'])) {
            $mailAllowUserSend = $sugar_config['email_allow_send_as_user'];
        } else {
            LoggerManager::getLogger()->warn('EmailMan view display error: mail allow user send is not set for focus');
        }

        $oe = new OutboundEmail();
        $oe = $oe->getSystemEmail();

        $this->ss->assign("system_outbound_email_id", $oe->id);
        $this->ss->assign("system_outbound_email_name", $oe->name);

        $this->ss->assign("mail_smtpserver", $mailSmtpServer);
        $this->ss->assign("mail_smtpport", $mailSmtpPort);
        $this->ss->assign("mail_smtpuser", $mailSmtpUser);
        $this->ss->assign("mail_smtpauth_req", ($mailSmtpAuthReq) ? "checked='checked'" : "");
        $this->ss->assign("mail_haspass", empty($mailSmtpPass)?0:1);
        $this->ss->assign("MAIL_SSL_OPTIONS", get_select_options_with_id($app_list_strings['email_settings_for_ssl'], $mailSmtpSsl));
        $this->ss->assign("mail_allow_user_send", ($mailAllowUserSend) ? "checked='checked'" : "");

        //Assign the current users email for the test send dialogue.
        $this->ss->assign("CURRENT_USER_EMAIL", $current_user->email1);

        $showSendMail = false;
        $outboundSendTypeCSSClass = "yui-hidden";
        if (isset($sugar_config['allow_sendmail_outbound']) && $sugar_config['allow_sendmail_outbound']) {
            $showSendMail = true;
            $app_list_strings['notifymail_sendtype']['sendmail'] = 'sendmail';
            $outboundSendTypeCSSClass = "";
        }

        $this->ss->assign("OUTBOUND_TYPE_CLASS", $outboundSendTypeCSSClass);
        $this->ss->assign("mail_sendtype_options", get_select_options_with_id($app_list_strings['notifymail_sendtype'], $mailSendType));

        $configurator = new Configurator();

        $email_templates_arr = get_bean_select_array(true, 'EmailTemplate', 'name', '', 'name', true);
        if (empty($email_templates_arr)) {
            throw new RuntimeException('System Email templates are missing');
        }

        $email_templates_options =  get_select_options_with_id($email_templates_arr, $configurator->config['email_confirm_opt_in_email_template_id']);
        $this->ss->assign('EMAIL_OPT_IN_TEMPLATES', $email_templates_options);

        if (!isset($configurator->config['email_enable_auto_send_opt_in'])) {
            throw new RuntimeException('email_enable_auto_send_opt_in is missing in the config. Please repair config.');
        }

        $isEmailEnableAutoSendConfirmOptIn = isset($configurator->config['email_enable_auto_send_opt_in']) ?
            $configurator->config['email_enable_auto_send_opt_in'] :
            false;

        $emailEnableAutoSendConfirmOptIn = $isEmailEnableAutoSendConfirmOptIn ? 'checked' : '';

        $this->ss->assign('EMAIL_ENABLE_AUTO_SEND_OPT_IN', $emailEnableAutoSendConfirmOptIn);

        ///////////////////////////////////////////////////////////////////////////////
        ////	USER EMAIL DEFAULTS
        // editors
        $editors = $app_list_strings['dom_email_editor_option'];
        $newEditors = array();
        foreach ($editors as $k => $v) {
            if ($k != "") {
                $newEditors[$k] = $v;
            }
        }

        // preserve attachments
        $preserveAttachments = '';
        if (isset($sugar_config['email_default_delete_attachments']) && $sugar_config['email_default_delete_attachments'] == true) {
            $preserveAttachments = 'CHECKED';
        }
        $this->ss->assign('DEFAULT_EMAIL_DELETE_ATTACHMENTS', $preserveAttachments);

        $emailNotifications = '';
        if (isset($sugar_config['email_warning_notifications']) && $sugar_config['email_warning_notifications'] === true) {
            $emailNotifications = 'CHECKED';
        }
        $this->ss->assign('LBL_EMAIL_WARNING_NOTIFICATIONS', $emailNotifications);

        $emailEnableConfirmOptIn = isset($configurator->config['email_enable_confirm_opt_in']) ? $configurator->config['email_enable_confirm_opt_in'] : '';

        $this->ss->assign(
            'EMAIL_ENABLE_CONFIRM_OPT_IN',
            get_select_options_with_id(
                $app_list_strings['email_settings_opt_in_dom'],
                $emailEnableConfirmOptIn
            )
        );

        ////	END USER EMAIL DEFAULTS
        ///////////////////////////////////////////////////////////////////////////////


        //setting to manage.
        //emails_per_run
        //tracking_entities_location_type default or custom
        //tracking_entities_location http://www.sugarcrm.com/track/

        //////////////////////////////////////////////////////////////////////////////
        ////	EMAIL SECURITY
        if (!isset($sugar_config['email_xss']) || empty($sugar_config['email_xss'])) {
            $sugar_config['email_xss'] = getDefaultXssTags();
        }

        foreach (unserialize(base64_decode($sugar_config['email_xss']), ['allowed_classes' => false]) as $k => $v) {
            $this->ss->assign($k."Checked", 'CHECKED');
        }

        ////	END EMAIL SECURITY
        ///////////////////////////////////////////////////////////////////////////////

        require_once('modules/Emails/Email.php');
        $email = BeanFactory::newBean('Emails');
        $this->ss->assign('ROLLOVER', $email->rolloverStyle);
        $this->ss->assign('THEME', $GLOBALS['theme']);

        $this->ss->assign("JAVASCRIPT", get_validate_record_js());
        $this->ss->display('modules/EmailMan/tpls/config.tpl');
    }
}
