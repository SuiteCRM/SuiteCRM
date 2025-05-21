<?php
/**
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
 *
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 * 
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo, "Supercharged by SuiteCRM" logo and “Nonprofitized by SinergiaCRM” logo. 
 * If the display of the logos is not reasonably feasible for technical reasons, 
 * the Appropriate Legal Notices must display the words "Powered by SugarCRM", 
 * "Supercharged by SuiteCRM" and “Nonprofitized by SinergiaCRM”. 
 */

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

$GLOBALS['starttTime'] = microtime(true);

set_include_path(
    __DIR__.'/..'.PATH_SEPARATOR.
    get_include_path()
);

if (!defined('PHP_VERSION_ID')) {
    $version_array = explode('.', phpversion());
    define('PHP_VERSION_ID', ($version_array[0] * 10000 + $version_array[1] * 100 + $version_array[2]));
}

if (empty($GLOBALS['installing']) && !file_exists('config.php')) {
    header('Location: install.php');
    throw new Exception('SuiteCRM is not installed. Entry point needs an installed SuiteCRM, please install first.');
}

$BASE_DIR = realpath(dirname(__DIR__));
$autoloader = $BASE_DIR.'/vendor/autoload.php';
if (file_exists($autoloader)) {
    require_once $autoloader;
} else {
    die('Composer autoloader not found. please run "composer install"');
}

// config|_override.php
if (is_file('config.php')) {
    require_once 'config.php'; // provides $sugar_config
}

// load up the config_override.php file.  This is used to provide default user settings
if (is_file('config_override.php')) {
    require_once 'config_override.php';
}
if (empty($GLOBALS['installing']) && empty($sugar_config['dbconfig']['db_name'])) {
    header('Location: install.php');
    exit();
}

// make sure SugarConfig object is available
$GLOBALS['sugar_config'] = !empty($sugar_config) ? $sugar_config : [];
require_once 'include/SugarObjects/SugarConfig.php';

///////////////////////////////////////////////////////////////////////////////
////	DATA SECURITY MEASURES

require_once 'include/utils.php';
require_once 'include/clean.php';
clean_special_arguments();

// STIC Custom 20220312 JCH - Skip clean_incoming_data() for exceptions defined in config.php
// Certain uses of clean_incoming_data() cause errors over some CRM functionality, specially when dealing with HTML code.
// While looking for a better way of ensuring both security and functionality, let's manage some exceptions.
// STIC#633
// STIC#699
// clean_incoming_data();
$skipAntiXSS = false;
// See if the current module is set as an exception 
foreach ($GLOBALS['sugar_config']['anti_xss_data_exceptions'] as $key => $xssException) {
    // STIC Custom 20250205 JBL - Avoid Undefined array key Warning
    // https://github.com/SinergiaTIC/SinergiaCRM/pull/477
    // if ($xssException['module'] == $_REQUEST['module'] && $skipAntiXSS !== true ) {
    if (($xssException['module'] ?? null) === ($_REQUEST['module'] ?? null) && $skipAntiXSS !== true) {
    // End STIC Custom
        // Sort config exception array (further than module, may contain other params like action or step)
        ksort($xssException);
        // Create a subarray from $_REQUEST containing the elements with keys defined in the prior exception
        $requestFiltered = array_intersect_key($_REQUEST, $xssException);
        // Sort the new array in order to have its elements in the same order of the config exception array 
        ksort($requestFiltered);
        // If both arrays are equal (ie, there is a match not only in module param but in all of them: action, step, etc.)
        // clean_incoming_data() should be avoided
        if (join($xssException) == join($requestFiltered)) {
            $skipAntiXSS = true;
            break;
        } 
    }
}

// Use clean_incoming_data() only when it won't cause any "damage"
if ($skipAntiXSS !== true) {
    clean_incoming_data();
}
// END STIC

////	END DATA SECURITY MEASURES
///////////////////////////////////////////////////////////////////////////////

// cn: set php.ini settings at entry points
setPhpIniSettings();

require_once 'sugar_version.php'; // provides $sugar_version, $sugar_db_version, $sugar_flavor
require_once 'include/database/DBManagerFactory.php';
require_once 'include/dir_inc.php';

require_once 'include/Localization/Localization.php';
require_once 'include/javascript/jsAlerts.php';
require_once 'include/TimeDate.php';
require_once 'include/modules.php'; // provides $moduleList, $beanList, $beanFiles, $modInvisList, $adminOnlyList, $modInvisListActivities

require_once 'include/utils/autoloader.php';
spl_autoload_register(array('SugarAutoLoader', 'autoload'));
require_once 'data/SugarBean.php';
require_once 'include/utils/mvc_utils.php';
require 'include/SugarObjects/LanguageManager.php';
require 'include/SugarObjects/VardefManager.php';

require 'modules/DynamicFields/templates/Fields/TemplateText.php';

require_once 'include/utils/file_utils.php';
require_once 'include/SugarEmailAddress/SugarEmailAddress.php';
require_once 'include/SugarLogger/LoggerManager.php';
require_once 'modules/Trackers/BreadCrumbStack.php';
require_once 'modules/Trackers/Tracker.php';
require_once 'modules/Trackers/TrackerManager.php';
require_once 'modules/ACL/ACLController.php';
require_once 'modules/Administration/Administration.php';
require_once 'modules/Administration/updater_utils.php';
require_once 'modules/Users/User.php';
require_once 'modules/Users/authentication/AuthenticationController.php';
require_once 'include/utils/LogicHook.php';
require_once 'include/SugarTheme/SugarTheme.php';
require_once 'include/MVC/SugarModule.php';
require_once 'include/SugarCache/SugarCache.php';
require 'modules/Currencies/Currency.php';
require_once 'include/MVC/SugarApplication.php';

require_once 'include/upload_file.php';
UploadStream::register();
//
//SugarApplication::startSession();

///////////////////////////////////////////////////////////////////////////////
////    Handle loading and instantiation of various Sugar* class
if (!defined('SUGAR_PATH')) {
    define('SUGAR_PATH', realpath(__DIR__.'/..'));
}
require_once 'include/SugarObjects/SugarRegistry.php';

if (empty($GLOBALS['installing'])) {
    ///////////////////////////////////////////////////////////////////////////////
    ////	SETTING DEFAULT VAR VALUES
    $GLOBALS['log'] = LoggerManager::getLogger();
    $error_notice = '';
    $use_current_user_login = false;

    // Allow for the session information to be passed via the URL for printing.
    if (isset($_GET['PHPSESSID'])) {
        if (!empty($_COOKIE['PHPSESSID']) && strcmp($_GET['PHPSESSID'], $_COOKIE['PHPSESSID']) == 0) {
            session_id($_REQUEST['PHPSESSID']);
        } else {
            unset($_GET['PHPSESSID']);
        }
    }

    $sessionGCConfig = $sugar_config['session_gc'] ?? [];
    if (!isset($sessionGCConfig['enable']) || isTrue($sessionGCConfig['enable'])) {
        $gcProbability = $sessionGCConfig['gc_probability'] ?? 1;
        $gcDivisor = $sessionGCConfig['gc_divisor'] ?? 100;

        ini_set('session.gc_probability', $gcProbability);
        ini_set('session.gc_divisor', $gcDivisor);
    }

    if (!empty($sugar_config['session_dir'])) {
        session_save_path($sugar_config['session_dir']);
    }

    SugarApplication::preLoadLanguages();

    $timedate = TimeDate::getInstance();

    $GLOBALS['sugar_version'] = $sugar_version;
    $GLOBALS['sugar_flavor'] = $sugar_flavor;
    $GLOBALS['timedate'] = $timedate;
    $GLOBALS['js_version_key'] = md5($GLOBALS['sugar_config']['unique_key'].$GLOBALS['sugar_version'].$GLOBALS['sugar_flavor']);

    $db = DBManagerFactory::getInstance();
    $db->resetQueryCount();
    $GLOBALS['db'] = $db;
    $locale = new Localization();
    $GLOBALS['locale'] = $locale;

    // Emails uses the REQUEST_URI later to construct dynamic URLs.
    // IIS does not pass this field to prevent an error, if it is not set, we will assign it to ''.
    if (!isset($_SERVER['REQUEST_URI'])) {
        $_SERVER['REQUEST_URI'] = '';
    }

    $current_user = BeanFactory::newBean('Users');
    $GLOBALS['current_user'] = $current_user;
    $current_entity = null;
    $system_config = BeanFactory::newBean('Administration');
    $system_config->retrieveSettings();

    LogicHook::initialize()->call_custom_logic('', 'after_entry_point');
}

////	END SETTING DEFAULT VAR VALUES
///////////////////////////////////////////////////////////////////////////////

//It does a check to see if the host is valid
check_trusted_hosts();
