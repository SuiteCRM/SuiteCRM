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

// This cron script, based on standard cron.php file, is used to run a single scheduler from SuiteCRM.
// The scheduler can be specified using the parameter scheduler_id.
// If no scheduler_id is provided, the script will run the scheduler "Run Nightly Mass Email Campaign".
// Changes to original cron.php file are marked in the code.
// STIC#619

if (!defined('sugarEntry')) {
    define('sugarEntry', true);
}

// STIC CUSTOM - "Sleep 3 seconds to priorize regular cron execution"
// https://github.com/SinergiaTIC/SinergiaCRM/pull/534
sleep(3);
// End STIC CUSTOM

// STIC Custom - "Run Nightly Mass Email Campaign" scheduler id
if (!$schedulerId = $_REQUEST['scheduler_id'] ?? false) {
    $schedulerId = '72ed826e-bd84-758a-d742-5e830d2ce892';
}
// End STIC Custom

chdir(dirname(__FILE__));

require_once 'include/entryPoint.php';

$sapi_type = php_sapi_name();

if (!is_windows()) {
    require_once 'include/utils.php';
    $cronUser = getRunningUser();

    if ($cronUser == '') {
        $GLOBALS['log']->warn('cron.php: can\'t determine running user. No cron user checks will occur.');
    } elseif (array_key_exists('cron', $sugar_config) && array_key_exists('allowed_cron_users', $sugar_config['cron'])) {
        if (!in_array($cronUser, $sugar_config['cron']['allowed_cron_users'])) {
            $GLOBALS['log']->fatal("cron.php: running as $cronUser is not allowed in allowed_cron_users " .
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

// STIC Custom - Run STIC cron driver to run a single scheduler
require_once "custom/include/SugarQueue/SticCronJobs.php";
$jobq = new SticCronJobs();
$jobq->runCycle($schedulerId);
// End STIC Custom

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
    exit($jobq->runOk() ? 0 : 1);
}
