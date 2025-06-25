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

if (!defined('SUITE_PHPUNIT_RUNNER')) {
    session_regenerate_id(false);
}
global $mod_strings;
$login_vars = $GLOBALS['app']->getLoginVars(false);

$user_name = isset($_REQUEST['user_name'])
    ? $_REQUEST['user_name'] : '';

$password = isset($_REQUEST['username_password'])
    ? $_REQUEST['username_password'] : '';

$authController->login($user_name, $password);
// authController will set the authenticated_user_id session variable
if (isset($_SESSION['authenticated_user_id'])) {
    // Login is successful
    if ($_SESSION['hasExpiredPassword'] == '1' && $_REQUEST['action'] != 'Save') {
        $GLOBALS['module'] = 'Users';
        $GLOBALS['action'] = 'ChangePassword';
        ob_clean();
        header("Location: index.php?module=Users&action=ChangePassword");
        sugar_cleanup(true);
    }
    global $record;
    global $current_user;
    global $sugar_config;

    global $current_user;

    if (isset($current_user)  && empty($login_vars)) {
        if (!empty($GLOBALS['sugar_config']['default_module']) && !empty($GLOBALS['sugar_config']['default_action'])) {
            $url = "index.php?module={$GLOBALS['sugar_config']['default_module']}&action={$GLOBALS['sugar_config']['default_action']}";
        } else {
            $modListHeader = query_module_access_list($current_user);
            //try to get the user's tabs
            $tempList = $modListHeader;
            $idx = array_shift($tempList);
            if (!empty($modListHeader[$idx])) {
                $url = "index.php?module={$modListHeader[$idx]}&action=index";
            }
        }
    } else {
        $url = $GLOBALS['app']->getLoginRedirect();
    }

    // STIC-Custom 20241014 ART - Tracker Module
    // https://github.com/SinergiaTIC/SinergiaCRM/pull/211
    // Track the login of the current user
    if ($action === 'Authenticate') {
    // Get the instance of the TrackerManager
    $trackerManager = TrackerManager::getInstance();

    // Get the tracker monitor
    $monitor = $trackerManager->getMonitor('tracker');

    // If the monitor exists, set its values
    if ($monitor) {
        // Set the date and time of the login
        $monitor->setValue('date_modified', $GLOBALS['timedate']->nowDb());

        // Set the user ID, assigned user ID, module name, action, item ID, item summary, visibility and session ID
        $monitor->setValue('user_id', $current_user->id);
        $monitor->setValue('assigned_user_id', $current_user->id);
        $monitor->setValue('module_name', 'Users');
        $monitor->setValue('action', 'login_ok');
        $monitor->setValue('item_id', $current_user->id);
        $monitor->setValue('item_summary', $current_user->full_name .' - Login');
        $monitor->setValue('visible', true);
        $monitor->setValue('session_id', $monitor->getSessionId());

        // Save the monitor to the database
        $trackerManager->saveMonitor($monitor, true, true);
    }
}
    // END STIC Custom
} else {
    // Login has failed
    if (isset($_POST['login_language']) && !empty($_POST['login_language'])) {
        $url ="index.php?module=Users&action=Login&login_language=". $_POST['login_language'];
    } else {
        $url ="index.php?module=Users&action=Login";
    }

    if (!empty($login_vars)) {
        $url .= '&' . http_build_query($login_vars);
    }
}

// construct redirect url
$url = 'Location: '.$url;

//adding this for bug: 21712.
if (!empty($GLOBALS['app'])) {
    $GLOBALS['app']->headerDisplayed = true;
}
if (!defined('SUITE_PHPUNIT_RUNNER')) {
    sugar_cleanup();
    header($url);
}
