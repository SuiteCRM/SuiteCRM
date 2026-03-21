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



include_once get_custom_file_if_exists('modules/Users/authentication/SugarAuthenticate/FactorAuthFactory.php');

/**
 * This file is where the user authentication occurs. No redirection should happen in this file.
 *
 */
#[\AllowDynamicProperties]
class SugarAuthenticateUser
{

    /**
     * Does the actual authentication of the user and returns an id that will be used
     * to load the current user (loadUserOnSession)
     *
     * @param STRING $name
     * @param STRING $password
     * @param STRING $fallback - is this authentication a fallback from a failed authentication
     * @return STRING id - used for loading the user
     */
    public function authenticateUser($name, $password, $fallback = false)
    {
        $row = User::findUserPassword($name, $password, "(portal_only IS NULL OR portal_only !='1') AND (is_group IS NULL OR is_group !='1') AND status !='Inactive'");

        // set the ID in the seed user.  This can be used for retrieving the full user record later
        //if it's falling back on Sugar Authentication after the login failed on an external authentication return empty if the user has external_auth_disabled for them
        if (empty($row) || !empty($row['external_auth_only'])) {
            return '';
        } else {
            return $row['id'];
        }
    }

    /**
     * Checks if a user is a sugarLogin user
     * which implies they should use the sugar authentication to login
     *
     * @param STRING $name
     * @param STRIUNG $password
     * @return boolean
     */
    public function isSugarLogin($name, $password)
    {
        $row = User::findUserPassword($name, $password, "(portal_only IS NULL OR portal_only !='1') AND (is_group IS NULL OR is_group !='1') AND status !='Inactive' AND sugar_login=1");
        return !empty($row);
    }

    /**
     * this is called when a user logs in
     *
     * @param STRING $name
     * @param STRING $password
     * @param STRING $fallback - is this authentication a fallback from a failed authentication
     * @return boolean
     */
    public function loadUserOnLogin($name, $password, $fallback = false, $PARAMS = array())
    {
        global $login_error;

        $GLOBALS['log']->debug("Starting user load for " . $name);
        if (empty($name) || empty($password)) {
            return false;
        }
        $input_hash = $password;
        $passwordEncrypted = false;
        if (!empty($PARAMS) && isset($PARAMS['passwordEncrypted']) && $PARAMS['passwordEncrypted']) {
            $passwordEncrypted = true;
        }// if
        if (!$passwordEncrypted) {
            $input_hash = SugarAuthenticate::encodePassword($password);
        } // if
        $user_id = $this->authenticateUser($name, $input_hash, $fallback);
        if (empty($user_id)) {
            $GLOBALS['log']->fatal('SECURITY: User authentication for ' . $name . ' failed');
            return false;
        }
        $this->loadUserOnSession($user_id);
        return true;
    }

    /**
     * Loads the current user bassed on the given user_id
     *
     * @param STRING $user_id
     * @return boolean
     */
    public function loadUserOnSession($user_id = '')
    {
        if (!empty($user_id)) {
            $_SESSION['authenticated_user_id'] = $user_id;
        }

        if (!empty($_SESSION['authenticated_user_id']) || !empty($user_id)) {
            $GLOBALS['current_user'] = BeanFactory::newBean('Users');
            if ($GLOBALS['current_user']->retrieve($_SESSION['authenticated_user_id'])) {
                if ($GLOBALS['current_user']->isEnabled()) {
                    return true;
                } else {
                    $GLOBALS['log']->security('Load user failed: user ' . $GLOBALS['current_user']->user_name . ' is not active');
                    return false;
                }
            }
        }
        return false;
    }

    /**
     * @return bool
     */
    public function isUserNeedFactorAuthentication()
    {
        global $current_user;
        $ret = false;
        if ($current_user->factor_auth) {
            $ret = true;
        }
        return $ret;
    }

    /**
     * @return bool
     */
    public function isUserFactorAuthenticated()
    {
        $ret = true;
        if (!isset($_SESSION['user_factor_authenticated']) || !$_SESSION['user_factor_authenticated']) {
            $ret = false;
        }
        return $ret;
    }

    /**
     * @return bool
     */
    public function isUserFactorTokenReceived()
    {
        $ret = false;
        if (isset($_REQUEST['factor_token'])) {
            $ret = true;
        }
        return $ret;
    }

    public function factorAuthenticateCheck()
    {
        if ($_SESSION['user_factor_authenticated'] || $_REQUEST['factor_token'] == $_SESSION['factor_token']) {
            $_SESSION['user_factor_authenticated'] = true;
        } else {
            $_SESSION['user_factor_authenticated'] = false;
        }
        return $_SESSION['user_factor_authenticated'];
    }

    /**
     * @global User $current_user
     */
    public function showFactorTokenInput()
    {
        global $current_user;

        $GLOBALS['log']->debug('Redirect to factor token input.....');
        
        $factory = new FactorAuthFactory();
        $factorAuth = $factory->getFactorAuth();
        if (!$factorAuth->validateTokenMessage()) {
            $msg = 'Factor Authentication message is invalid.';
            $GLOBALS['log']->warn($msg);
            global $app_strings;
            SugarApplication::appendErrorMessage($app_strings['ERR_FACTOR_TPL_INVALID']);
            SugarAuthenticate::addFactorMessage($app_strings['ERR_FACTOR_TPL_INVALID']);
            $factorAuth->showTokenInput();
        } else {
            $factorAuth->showTokenInput();
        }

        die();
    }

    /**
     * @return bool
     */
    public function isFactorTokenSent()
    {
        $ret = false;
        if (isset($_SESSION['factor_token']) && $_SESSION['factor_token']) {
            $ret = true;
        }
        return $ret;
    }

    /**
     * @return bool
     */
    public function sendFactorTokenToUser()
    {
        global $current_user, $sugar_config;

        $ret = true;

        $min = 10000;
        $max = 99999;

        if (function_exists('random_int')) {
            $token = random_int($min, $max);
        } else {
            $token = mt_rand($min, $max);
        }

        $emailTemplate = BeanFactory::newBean('EmailTemplates');
        $emailTemplateId = $sugar_config['passwordsetting']['factoremailtmpl'];
        $emailTemplate->retrieve($emailTemplateId);

        include_once __DIR__ . '/../../../../include/SugarPHPMailer.php';
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
            $ret = true;
            $GLOBALS['log']->debug(
                'Token sent to user: ' .
                    $current_user->id . ', token: ' . $token . ' so we store it in the session'
            );

            $_SESSION['user_factor_authenticated'] = false;
            $_SESSION['factor_token'] = $token;
        }
        return $ret;
    }

    /**
     *
     */
    public function redirectToLogout()
    {
        $GLOBALS['log']->debug('Session destroy and redirect to logout.....');
        session_destroy();
        header('Location: index.php?action=Logout&module=Users');
        sugar_cleanup(true);
        die();
    }

    /**
     * @return bool
     */
    public function isUserLogoutRequest()
    {
        $logout = false;
        if (
                isset($_REQUEST['module']) && $_REQUEST['module'] == 'Users' &&
                isset($_REQUEST['action']) && $_REQUEST['action'] == 'Logout'
        ) {
            $logout = true;
        }
        return $logout;
    }

    /**
     * Has user has requested to resend the multi/2 factor token?
     * @return bool true === yes, false === no
     */
    public function isUserRequestedResendToken()
    {
        return isset($_REQUEST['action']) && $_REQUEST['action'] === 'Resend';
    }
}
