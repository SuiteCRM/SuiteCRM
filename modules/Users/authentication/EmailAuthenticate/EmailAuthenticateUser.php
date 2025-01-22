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




/**
 * This file is where the user authentication occurs. No redirection should happen in this file.
 *
 */
require_once('modules/Users/authentication/SugarAuthenticate/SugarAuthenticateUser.php');
class EmailAuthenticateUser extends SugarAuthenticateUser
{
    public $passwordLength = 4;


    /**
     * this is called when a user logs in
     *
     * @param STRING $name
     * @param STRING $password
     * @return boolean
     */
    public function loadUserOnLogin($name, $password, $fallback = \false, $PARAMS = array())
    {
        global $login_error;

        $GLOBALS['log']->debug("Starting user load for ". $name);
        if (empty($name) || empty($password)) {
            return false;
        }

        if (empty($_SESSION['lastUserId'])) {
            $input_hash = SugarAuthenticate::encodePassword($password);
            $user_id = $this->authenticateUser($name, $input_hash);
            if (empty($user_id)) {
                $GLOBALS['log']->fatal('SECURITY: User authentication for '.$name.' failed');
                return false;
            }
        }

        if (empty($_SESSION['emailAuthToken'])) {
            $_SESSION['lastUserId'] = $user_id;
            $_SESSION['lastUserName'] = $name;
            $_SESSION['emailAuthToken'] = '';
            for ($i = 0; $i < $this->passwordLength; $i++) {
                $_SESSION['emailAuthToken'] .= chr(mt_rand(48, 90));
            }
            $_SESSION['emailAuthToken']  =  str_replace(array('<', '>'), array('#', '@'), $_SESSION['emailAuthToken']);
            $_SESSION['login_error'] = 'Please Enter Your User Name and Emailed Session Token';
            $this->sendEmailPassword($user_id, $_SESSION['emailAuthToken']);
            return false;
        } else {
            if (strcmp($name, $_SESSION['lastUserName']) == 0 && strcmp($password, $_SESSION['emailAuthToken']) == 0) {
                $this->loadUserOnSession($_SESSION['lastUserId']);
                unset($_SESSION['lastUserId']);
                unset($_SESSION['lastUserName']);
                unset($_SESSION['emailAuthToken']);
                return true;
            }
        }
        if (strcmp($name, $_SESSION['lastUserName']) == 0 && strcmp($password, $_SESSION['emailAuthToken']) == 0) {
            $this->loadUserOnSession($_SESSION['lastUserId']);
            unset($_SESSION['lastUserId']);
            unset($_SESSION['lastUserName']);
            unset($_SESSION['emailAuthToken']);
            return true;
        }


        $_SESSION['login_error'] = 'Please Enter Your User Name and Emailed Session Token';
        return false;
    }


    /**
     * Sends the users password to the email address or sends
     *
     * @param unknown_type $user_id
     * @param unknown_type $password
     */
    public function sendEmailPassword($user_id, $password)
    {
        $result = DBManagerFactory::getInstance()->query("SELECT email1, email2, first_name, last_name FROM users WHERE id='$user_id'");
        $row = DBManagerFactory::getInstance()->fetchByAssoc($result);

        global $sugar_config;
        if (empty($row['email1']) && empty($row['email2'])) {
            $_SESSION['login_error'] = 'Please contact an administrator to setup up your email address associated to this account';
            return;
        }

        require_once("include/SugarPHPMailer.php");
        global $locale;
        $OBCharset = $locale->getPrecedentPreference('default_email_charset');
        $notify_mail = new SugarPHPMailer();
        $notify_mail->CharSet = $sugar_config['default_charset'];
        $notify_mail->AddAddress(((!empty($row['email1']))?$row['email1']: $row['email2']), $locale->translateCharsetMIME(trim($row['first_name'] . ' ' . $row['last_name']), 'UTF-8', $OBCharset));

        $notify_mail->Subject = 'Sugar Token';
        $notify_mail->Body = 'Your sugar session authentication token  is: ' . $password;
        $notify_mail->setMailerForSystem();
        $notify_mail->From = 'no-reply@sugarcrm.com';
        $notify_mail->FromName = 'Sugar Authentication';

        if (!$notify_mail->Send()) {
            $GLOBALS['log']->warn("Notifications: error sending e-mail (method: {$notify_mail->Mailer}), (error: {$notify_mail->ErrorInfo})");
        } else {
            $GLOBALS['log']->info("Notifications: e-mail successfully sent");
        }
    }
}
