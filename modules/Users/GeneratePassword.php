<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2021 SalesAgility Ltd.
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

require_once __DIR__ . '/../../include/entryPoint.php';
require_once __DIR__ . '/../../modules/Users/language/en_us.lang.php';

global $app_strings, $sugar_config, $new_pwd, $current_user;

    $mod_strings=return_module_language('', 'Users');
    $res=$GLOBALS['sugar_config']['passwordsetting'];
    $regexmail = "/^\w+(['\.\-\+]?\w+)*@\w+([\.-]?\w+)*(\.\w{2,})+\$/";

///////////////////////////////////////////////////
///////  Retrieve user
$username = '';
$useremail = '';
if (isset($_POST['user_name'])) {
    $username = $_POST['user_name'];
} else {
    if (isset($_POST['username'])) {
        $username = $_POST['username'];
    }
}

if (isset($_POST['Users0emailAddress0'])) {
    $useremail = $_POST['Users0emailAddress0'];
} else {
    if (isset($_POST['user_email'])) {
        $useremail = $_POST['user_email'];
    }
}

    $usr= new user();
    if (isset($username) && $username != '' && isset($useremail) && $useremail != '') {
        if ($username != '' && $useremail != '') {
            $usr_id=$usr->retrieve_user_id($username);
            $usr->retrieve($usr_id);
            if (!$usr->isPrimaryEmail($useremail)) {
                echo $mod_strings['LBL_PROVIDE_USERNAME_AND_EMAIL'];
                return;
            }

            if ($usr->portal_only || $usr->is_group) {
                echo $mod_strings['LBL_PROVIDE_USERNAME_AND_EMAIL'];
                return;
            }
        } else {
            echo  $mod_strings['LBL_PROVIDE_USERNAME_AND_EMAIL'];
            return;
        }
    } else {
        if (isset($_POST['userId']) && $_POST['userId'] != '') {
            $usr->retrieve($_POST['userId']);
        } else {
            if (!empty($_POST['sugar_user_name'])) {
                $usr_id=$usr->retrieve_user_id($_POST['sugar_user_name']);
                $usr->retrieve($usr_id);
            } else {
                echo  $mod_strings['LBL_PROVIDE_USERNAME_AND_EMAIL'];
                return;
            }
        }

        // Check if current_user is admin or the same user
        if (empty($current_user->id) || empty($usr->id) || ($usr->id != $current_user->id && !$current_user->is_admin)) {
            echo  $mod_strings['LBL_PROVIDE_USERNAME_AND_EMAIL'];
            return;
        }
    }

///////
///////////////////////////////////////////////////

///////////////////////////////////////////////////
///////  Check email address

    if (!preg_match($regexmail, (string) $usr->emailAddress->getPrimaryAddress($usr))) {
        echo $mod_strings['ERR_EMAIL_INCORRECT'];
        return;
    }

///////
///////////////////////////////////////////////////
    $isLink = isset($_POST['link']) && $_POST['link'] == '1';
    // if i need to generate a password (not a link)
    $password = $isLink ? '' : User::generatePassword();

$isPasswordGenerationActive = $res['SystemGeneratedPasswordON'] ?? false;
if(!$isLink && empty($isPasswordGenerationActive)) {
    echo 'Access Denied';
    return;
}

// Create URL
if ($isLink) {
    global $timedate;
    $guid = create_guid();
    $key = create_guid();
    $hashedKey = User::getPasswordHash($key);
    $url = $GLOBALS['sugar_config']['site_url'] . "/index.php?entryPoint=Changenewpassword&guid=$guid&key=$key";
    $time_now = TimeDate::getInstance()->nowDb();
    $userID = $usr->retrieve_user_id($username);
    $q = "INSERT INTO users_password_link (id, keyhash, username, date_generated, user_id) VALUES('" .
        $guid . "','" .
        $hashedKey . "','" .
        $username . "','" .
        $time_now . "','" .
        $userID . "') ";
    $usr->db->query($q);
}

///////  Email creation
    if ($isLink) {
        $emailTemp_id = $res['lostpasswordtmpl'];
    } else {
        $emailTemp_id = $res['generatepasswordtmpl'];
    }

    $additionalData = array(
        'link' => $isLink,
        'password' => $password
    );
    if (isset($url)) {
        $additionalData['url'] = $url;
    }
    $result = $usr->sendEmailForPassword($emailTemp_id, $additionalData);
    if ($result['status'] == false && $result['message'] != '') {
        echo $result['message'];
        $new_pwd = '4';
        return;
    }

    if ($result['status'] == true) {
        echo '1';
    } else {
        $new_pwd='4';
        if ($current_user->is_admin) {
            $email_errors=$mod_strings['ERR_EMAIL_NOT_SENT_ADMIN'];
            $email_errors.="\n-".$mod_strings['ERR_RECIPIENT_EMAIL'];
            $email_errors.="\n-".$mod_strings['ERR_SERVER_STATUS'];
            echo $email_errors;
        } else {
            echo $mod_strings['LBL_EMAIL_NOT_SENT'];
        }
    }
    return;
