<?php
 if (!defined('sugarEntry')) {
     define('sugarEntry', true);
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

//change directories to where this file is located.
//this is to make sure it can find dce_config.php
chdir(__DIR__);

require_once('include/entryPoint.php');

$sapi_type = php_sapi_name();
if (substr($sapi_type, 0, 3) != 'cli') {
    sugar_die("cron.php is CLI only.");
}

if (!is_windows()) {
    require_once 'include/utils.php';
    $cronUser = getRunningUser();
 
    if ($cronUser == '') {
        $GLOBALS['log']->warn('cron.php: can\'t determine running user. No cron user checks will occur.');
    } elseif (array_key_exists('cron', $sugar_config) && array_key_exists('allowed_cron_users', $sugar_config['cron'])) {
        if (!in_array($cronUser, $sugar_config['cron']['allowed_cron_users'])) {
            $GLOBALS['log']->fatal("cron.php: running as $cronUser is not allowed in allowed_cron_users ".
                                   "in config.php. Exiting.");
            if ($cronUser == 'root') {
                // Additional advice so that people running as root aren't led to adding root as an allowed user:
                $GLOBALS['log']->fatal('cron.php: root\'s crontab should not be used for cron.php. ' .
                                       'Use your web server user\'s crontab instead.');
            }
            sugar_die('cron.php running with user that is not in allowed_cron_users in config.php');
        }
    } else {
        $GLOBALS['log']->warn('cron.php: missing expected allowed_cron_users entry in config.php. ' .
                                 'No cron user checks will occur.');
    }
}

if (empty($current_language)) {
    $current_language = $sugar_config['default_language'];
}

$app_list_strings = return_app_list_strings_language($current_language);
$app_strings = return_application_language($current_language);

global $current_user;
$current_user = BeanFactory::newBean('Users');
$current_user->getSystemUser();

$GLOBALS['log']->debug('--------------------------------------------> at cron.php <--------------------------------------------');
$cron_driver = !empty($sugar_config['cron_class'])?$sugar_config['cron_class']:'SugarCronJobs';
$GLOBALS['log']->debug("Using $cron_driver as CRON driver");

if (file_exists("custom/include/SugarQueue/$cron_driver.php")) {
    require_once "custom/include/SugarQueue/$cron_driver.php";
} else {
    require_once "include/SugarQueue/$cron_driver.php";
}

$jobq = new $cron_driver();
$jobq->runCycle();

$exit_on_cleanup = true;

sugar_cleanup(false);
// some jobs have annoying habit of calling sugar_cleanup(), and it can be called only once
// but job results can be written to DB after job is finished, so we have to disconnect here again
// just in case we couldn't call cleanup
if (class_exists('DBManagerFactory')) {
    $db = DBManagerFactory::getInstance();
    $db->disconnect();
}

// If we have a session left over, destroy it
if (session_id()) {
    session_destroy();
}

if ($exit_on_cleanup) {
    exit($jobq->runOk()?0:1);
}
