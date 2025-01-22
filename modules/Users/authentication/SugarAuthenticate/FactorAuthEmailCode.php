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


include_once get_custom_file_if_exists('modules/Users/authentication/SugarAuthenticate/FactorAuthInterface.php');
include_once __DIR__ . '/../../../../include/SugarPHPMailer.php';

#[\AllowDynamicProperties]
class FactorAuthEmailCode implements FactorAuthInterface
{

    /**
     * Shows an input form for email code authentication
     * (like login page for pwd)
     * @throws \RuntimeException
     */
    public function showTokenInput()
    {
        global $app_strings;

        $ss = new Sugar_Smarty();

        $theme = SugarThemeRegistry::current();

        $cssPath = $theme->getCSSPath();
        $css = $theme->getCSS();
        $favicon = $theme->getImageURL('sugar_icon.ico', false);

        $ss->assign('APP', $app_strings);
        $ss->assign('cssPath', $cssPath);
        $ss->assign('css', $css);
        $ss->assign('favicon', getJSPath($favicon));

        $factorMessage = SugarAuthenticate::getFactorMessages();
        $ss->assign('factor_message', $factorMessage);

        $ss->display(__DIR__ . '/FactorAuthEmailCode.tpl');
    }

    /**
     * Send a Token Code to User for more factor Authentication,
     * Using selected Email Template on Password Settings page.
     * Template can use $code template variable to formatting Emails.
     * Returns true if success,
     * otherwise false.
     *
     * @param string $token
     * @return bool
     * @throws \phpmailerException
     */
    public function sendToken($token)
    {
        global $current_user, $sugar_config, $mod_strings;

        if (!$this->validateTokenMessage()) {
            $msg = 'Factor Authentication mail is invalid';
            $GLOBALS['log']->warn($msg);
            SugarApplication::appendErrorMessage($mod_strings['ERR_FACTOR_VALIDATION']);
        }

        $ret = true;

        $emailTemplate = BeanFactory::newBean('EmailTemplates');
        $emailTemplateId = $sugar_config['passwordsetting']['factoremailtmpl'];
        $emailTemplate->retrieve($emailTemplateId);

        $mailer = new SugarPHPMailer();
        $mailer->setMailerForSystem();

        $emailObj = BeanFactory::newBean('Emails');
        $defaults = $emailObj->getSystemDefaultEmail();

        $mailer->From = $defaults['email'];
        isValidEmailAddress($mailer->From);
        $mailer->FromName = $defaults['name'];

        $mailer->Subject = from_html($emailTemplate->subject);

        $mailer->Body = from_html($emailTemplate->body_html);
        $mailer->Body_html = from_html($emailTemplate->body_html);
        $mailer->AltBody = from_html($emailTemplate->body);

        $mailer->addAddress($current_user->email1, $current_user->full_name);

        $mailer->replace('code', $token);

        if (!$mailer->send()) {
            $ret = false;
            $GLOBALS['log']->fatal(
                'Email sending for two factor email authentication via Email Code failed. Mailer Error Info: ' .
                    $mailer->ErrorInfo
            );
        } else {
            $GLOBALS['log']->debug(
                'FACTOR AUTH: token sent to user: ' .
                    $current_user->id . ', token: ' . '{$token}' . ' so we store it in the session'
            );
        }

        return $ret;
    }

    public function validateTokenMessage()
    {
        global $sugar_config, $mod_strings;
        $msg = '';
        $emailTpl = false;
        if (isset($sugar_config['passwordsetting']['factoremailtmpl']) && $sugar_config['passwordsetting']['factoremailtmpl']) {
            $emailTpl = BeanFactory::getBean('EmailTemplates', $sugar_config['passwordsetting']['factoremailtmpl']);
        }
        if (!isset($sugar_config['passwordsetting']['factoremailtmpl']) || !$sugar_config['passwordsetting']['factoremailtmpl'] || !$emailTpl) {
            $msg .= 'Two factor email template is not set, change settings on password management page.';
            $GLOBALS['log']->warn($msg);
            SugarApplication::appendErrorMessage($mod_strings['ERR_NO_2FACTOR_EMAIL_TMPL']);
            return false;
        } elseif ($emailTpl && !preg_match('/\$code\b/', (string) $emailTpl->body_html)) {
            $msg .= 'Two factor email template should contains a $code at least.';
            $GLOBALS['log']->warn($msg);
            SugarApplication::appendErrorMessage($mod_strings['ERR_NO_2FACTOR_EMAIL_TMPL_CODE']);
            return false;
        }
        return true;
    }
}
