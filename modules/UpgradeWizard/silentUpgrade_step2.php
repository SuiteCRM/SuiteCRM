<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2019 SalesAgility Ltd.
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
 *
 * This is a stand alone file that can be run from the command prompt for upgrading a
 * SuiteCRM Instance. Three parameters are required to be defined in order to execute this file.
 * php.exe -f silentUpgrade.php [Path to Upgrade Package zip] [Path to Log file] [Path to Instance]
 * See below the Usage for more details.
 */

//	UTILITIES THAT MUST BE LOCAL :(
ini_set('memory_limit', -1);

//Bug 24890, 24892. default_permissions not written to config.php. Following function checks and if
//no found then adds default_permissions to the config file.
function checkConfigForPermissions()
{
    if (file_exists(getcwd() . '/config.php')) {
        require getcwd() . '/config.php';
    }
    global $sugar_config;
    if (!isset($sugar_config['default_permissions'])) {
        $sugar_config['default_permissions'] = [
            'dir_mode' => 02770,
            'file_mode' => 0660,
            'user' => '',
            'group' => '',
        ];
        ksort($sugar_config);
        if (is_writable('config.php')) {
            write_array_to_file("sugar_config", $sugar_config, 'config.php');
        }
    }
}

function checkLoggerSettings()
{
    if (file_exists(getcwd() . '/config.php')) {
        require getcwd() . '/config.php';
    }
    global $sugar_config;
    if (!isset($sugar_config['logger'])) {
        $sugar_config['logger'] = array(
            'level' => 'fatal',
            'file' =>
                array(
                    'ext' => '.log',
                    'name' => 'suitecrm',
                    'dateFormat' => '%c',
                    'maxSize' => '10MB',
                    'maxLogs' => 10,
                    'suffix' => '', // bug51583, change default suffix to blank for backwards comptability
                ),
        );
        ksort($sugar_config);
        if (is_writable('config.php')) {
            write_array_to_file('sugar_config', $sugar_config, 'config.php');
        }
    }
}

function checkResourceSettings()
{
    if (file_exists(getcwd() . '/config.php')) {
        require getcwd() . '/config.php';
    }
    global $sugar_config;
    if (!isset($sugar_config['resource_management'])) {
        $sugar_config['resource_management'] =
            [
                'special_query_limit' => 50000,
                'special_query_modules' =>
                    [
                        0 => 'AOR_Reports',
                        1 => 'Export',
                        2 => 'Import',
                        3 => 'Administration',
                        4 => 'Sync',
                    ],
                'default_limit' => 1000,
            ];
        ksort($sugar_config);
        if (is_writable('config.php')) {
            write_array_to_file('sugar_config', $sugar_config, 'config.php');
        }
    }
}

/**
 * @param $argv
 * @param $usage_regular
 * @return mixed
 */
function verifyArguments($argv, $usage_regular)
{
    $cwd = getcwd(); // default to current, assumed to be in a valid SuiteCRM root dir.
    if (isset($argv[3])) {
        if (is_dir($argv[3])) {
            $cwd = $argv[3];
            chdir($cwd);
        } else {
            echo "*******************************************************************************\n";
            echo "*** ERROR: 3rd parameter must be a valid directory.  Tried to cd to [ {$argv[3]} ].\n";
            exit(1);
        }
    }

    //check if this is an instance
    if (is_file("{$cwd}/include/entryPoint.php")) {
        //this should be a regular sugar install
        $upgradeType = constant('SUGARCRM_INSTALL');
        //check if this is a valid zip file
        if (!is_file($argv[1])) { // valid zip?
            echo "*******************************************************************************\n";
            echo "*** ERROR: First argument must be a full path to the patch file. Got [ {$argv[1]} ].\n";
            echo $usage_regular;
            echo "FAILURE\n";
            exit(1);
        }
        if ((is_countable($argv) ? count($argv) : 0) < 5) {
            echo "*******************************************************************************\n";
            echo '*** ERROR: Missing required parameters.  Received ' . (is_countable($argv) ? count($argv) : 0) . " argument(s), require 5.\n";
            echo $usage_regular;
            echo "FAILURE\n";
            exit(1);
        }
    } else {
        //this should be a regular sugar install
        echo "*******************************************************************************\n";
        echo "*** ERROR: Tried to execute in a non-SuiteCRM root directory.\n";
        exit(1);
    }

    if (isset($argv[7]) && file_exists($argv[7] . 'SugarTemplateUtilties.php')) {
        require_once $argv[7] . 'SugarTemplateUtilties.php';
    }

    return $upgradeType;
}

// END UTILITIES THAT MUST BE LOCAL :(


function rebuildRelations($pre_path = '')
{
    $_REQUEST['silent'] = true;
    include $pre_path . 'modules/Administration/RebuildRelationship.php';
    $_REQUEST['upgradeWizard'] = true;
    include $pre_path . 'modules/ACL/install_actions.php';
}

//Bug 52872. Dies if the request does not come from CLI.
$sapi_type = PHP_SAPI;
if (strpos($sapi_type, 'cli') !== 0) {
    die('This is command-line only script');
}
//End of #52872

// only run from command line
if (isset($_SERVER['HTTP_USER_AGENT'])) {
    fwrite(STDERR, 'This utility may only be run from the command line or command prompt.');
    exit(1);
}
//Clean_string cleans out any file  passed in as a parameter
$_SERVER['PHP_SELF'] = 'silentUpgrade.php';


// USAGE
$usage_regular = <<<eoq2
Usage: php.exe -f silentUpgrade.php [upgradeZipFile] [logFile] [pathToSuiteCRMInstance] [admin-user]

On Command Prompt Change directory to where silentUpgrade.php resides. Then type path to
php.exe followed by -f silentUpgrade.php and the arguments.

Example:
    [path-to-PHP/]php.exe -f silentUpgrade.php [path-to-upgrade-package/]SugarEnt-Upgrade-5.2.0-to-5.5.0.zip [path-to-log-file/]silentupgrade.log  [path-to-sugar-instance/] admin

Arguments:
    upgradeZipFile                       : Upgrade package file.
    logFile                              : Silent Upgarde log file.
    pathToSuiteCRMInstance                  : Suite Instance instance being upgraded.
    admin-user                           : admin user performing the upgrade
eoq2;
// END USAGE


// STANDARD REQUIRED SUGAR INCLUDES AND PRESETS
if (!defined('sugarEntry')) {
    define('sugarEntry', true);
}

$_SESSION = [];
$_SESSION['schema_change'] = 'sugar'; // we force-run all SQL
$_SESSION['silent_upgrade'] = true;
$_SESSION['step'] = 'silent'; // flag to NOT try redirect to 4.5.x upgrade wizard

$_REQUEST = [];
$_REQUEST['addTaskReminder'] = 'remind';


define('SUGARCRM_INSTALL', 'SugarCRM_Install');
define('DCE_INSTANCE', 'DCE_Instance');

global $cwd;
$cwd = getcwd(); // default to current, assumed to be in a valid SuiteCRM root dir.

$upgradeType = verifyArguments($argv, $usage_regular);

$path = $argv[2]; // custom log file, if blank will use ./upgradeWizard.log
$subdirs = ['full', 'langpack', 'module', 'patch', 'theme', 'temp'];

require_once('include/entryPoint.php');
require_once('modules/UpgradeWizard/uw_utils.php');
require_once('include/utils/php_zip_utils.php');
require_once('include/utils/sugar_file_utils.php');
require_once('include/SugarObjects/SugarConfig.php');
global $sugar_config;
$isDCEInstance = false;
$errors = [];

require('config.php');
if (isset($argv[3]) && is_dir($argv[3])) {
    $cwd = $argv[3];
    chdir($cwd);
}

require_once "{$cwd}/sugar_version.php"; // provides $sugar_version & $sugar_flavor

global $sugar_config;
$configOptions = $sugar_config['dbconfig'];

$GLOBALS['log'] = LoggerManager::getLogger();
$patchName = basename($argv[1]);
$zip_from_dir = substr($patchName, 0, -4); // patch folder name (minus ".zip")
$path = $argv[2]; // custom log file, if blank will use ./upgradeWizard.log
$db = DBManagerFactory::getInstance();
$UWstrings = return_module_language('en_us', 'UpgradeWizard', true);
$adminStrings = return_module_language('en_us', 'Administration', true);
$app_list_strings = return_app_list_strings_language('en_us');
$mod_strings = array_merge($adminStrings, $UWstrings);
$subdirs = ['full', 'langpack', 'module', 'patch', 'theme', 'temp'];
global $unzip_dir;
$license_accepted = false;
if (isset($argv[5]) && (strtolower($argv[5]) === 'yes' || strtolower($argv[5]) === 'y')) {
    $license_accepted = true;
}

//Adding admin user to the silent upgrade

$current_user = BeanFactory::newBean('Users');
if (isset($argv[4])) {
    //if being used for internal upgrades avoid admin user verification
    $user_name = $argv[4];
    $q = "select id from users where user_name = '" . $user_name . "' and is_admin=1";
    $result = DBManagerFactory::getInstance()->query($q, false);
    $logged_user = DBManagerFactory::getInstance()->fetchByAssoc($result);
    if (isset($logged_user['id']) && $logged_user['id'] != null) {
        //do nothing
        $current_user->retrieve($logged_user['id']);
    } else {
        echo "Not an admin user in users table. Please provide an admin user\n";
        exit(1);
    }
} else {
    echo "*******************************************************************************\n";
    echo "*** ERROR: 4th parameter must be a valid admin user.\n";
    echo $usage_regular;
    echo "FAILURE\n";
    exit(1);
}

$unzip_dir = sugar_cached('upgrades/temp');

$install_file = $sugar_config['upload_dir'] . '/upgrades/patch/' . basename($argv[1]);
sugar_mkdir($sugar_config['upload_dir'] . '/upgrades/patch', 0775, true);

$_SESSION['unzip_dir'] = $unzip_dir;
$_SESSION['install_file'] = $install_file;
$_SESSION['zip_from_dir'] = $zip_from_dir;

mkdir_recursive($unzip_dir);
if (!is_dir($unzip_dir)) {
    fwrite(STDERR, "\n{$unzip_dir} is not an available directory\nFAILURE\n");
    exit(1);
}
unzip($argv[1], $unzip_dir);
// mimic standard UW by copy patch zip to appropriate dir
copy($argv[1], $install_file);
//	END UPGRADE PREP


if (function_exists('set_upgrade_vars')) {
    set_upgrade_vars();
}

// RUN SILENT UPGRADE
ob_start();
set_time_limit(0);

// RELOAD NEW DEFINITIONS
global $ACLActions, $beanList, $beanFiles;

require_once 'modules/Trackers/TrackerManager.php';
$trackerManager = TrackerManager::getInstance();
$trackerManager->pause();
$trackerManager->unsetMonitors();

include 'modules/ACLActions/actiondefs.php';
include 'include/modules.php';

require_once 'modules/Administration/upgrade_custom_relationships.php';
upgrade_custom_relationships();

logThis('Upgrading user preferences start .', $path);
if (function_exists('upgradeUserPreferences')) {
    upgradeUserPreferences();
}
logThis('Upgrading user preferences finish .', $path);

// clear out the theme cache
if (is_dir($GLOBALS['sugar_config']['cache_dir'] . 'themes')) {
    $allModFiles = [];
    $allModFiles = findAllFiles($GLOBALS['sugar_config']['cache_dir'] . 'themes', $allModFiles);
    foreach ($allModFiles as $file) {
        if (file_exists($file)) {
            unlink($file);
        }
    }
}

// re-minify the JS source files
$_REQUEST['root_directory'] = getcwd();
$_REQUEST['js_rebuild_concat'] = 'rebuild';
require_once 'jssource/minify.php';

//Add the cache cleaning here.
if (function_exists('deleteCache')) {
    logThis('Call deleteCache', $path);
    @deleteCache();
}

//First repair the databse to ensure it is up to date with the new vardefs/tabledefs
logThis('About to repair the database.', $path);
//Use Repair and rebuild to update the database.
global $dictionary;
require_once 'modules/Administration/QuickRepairAndRebuild.php';
$rac = new RepairAndClear();
$rac->clearVardefs();
$rac->rebuildExtensions();
//bug: 44431 - defensive check to ensure the method exists since upgrades to 6.2.0 may not have this method define yet.
if (method_exists($rac, 'clearExternalAPICache')) {
    $rac->clearExternalAPICache();
}

$repairedTables = [];
foreach ($beanFiles as $bean => $file) {
    if (file_exists($file)) {
        unset($GLOBALS['dictionary'][$bean]);
        require_once $file;
        $focus = new $bean();
        if (empty($focus->table_name) || isset($repairedTables[$focus->table_name])) {
            continue;
        }

        if ($focus instanceof SugarBean) {
            if (!isset($repairedTables[$focus->table_name])) {
                $sql = DBManagerFactory::getInstance()->repairTable($focus, true);
                if (trim($sql) != '') {
                    logThis('Running sql:' . $sql, $path);
                }
                $repairedTables[$focus->table_name] = true;
            }

            //Check to see if we need to create the audit table
            if ($focus->is_AuditEnabled() && !$focus->db->tableExists($focus->get_audit_table_name())) {
                logThis('Creating audit table:' . $focus->get_audit_table_name(), $path);
                $focus->create_audit_table();
            }
        }
    }
}

unset($dictionary);
include "{$argv[3]}/modules/TableDictionary.php";
foreach ($dictionary as $meta) {
    $tablename = $meta['table'];

    if (isset($repairedTables[$tablename])) {
        continue;
    }

    $fielddefs = $meta['fields'];
    $indices = $meta['indices'];
    $sql = DBManagerFactory::getInstance()->repairTableParams($tablename, $fielddefs, $indices, true);
    if (!empty($sql)) {
        logThis($sql, $path);
        $repairedTables[$tablename] = true;
    }
}

logThis('database repaired', $path);

logThis('Start rebuild relationships.', $path);
@rebuildRelations();
logThis('End rebuild relationships.', $path);

$sugar_version = getSilentUpgradeVar('origVersion');
if (!$sugar_version) {
    global $silent_upgrade_vars_loaded;
    logThis("Error retrieving silent upgrade var for sugar_version: cache dir is {$GLOBALS['sugar_config']['cache_dir']} -- full cache for \$silent_upgrade_vars_loaded is " . var_export($silent_upgrade_vars_loaded,
            true), $path);
}

//bug: 37214 - merge config_si.php settings if available
logThis('Begin merge_config_si_settings', $path);
merge_config_si_settings(true, '', '', $path);
logThis('End merge_config_si_settings', $path);

//Upgrade connectors
logThis('Begin upgrade_connectors', $path);
upgrade_connectors();
logThis('End upgrade_connectors', $path);


//Unlink files that have been removed
if (function_exists('unlinkUpgradeFiles')) {
    unlinkUpgradeFiles($sugar_version);
}

if (function_exists('rebuildSprites') && function_exists('imagecreatetruecolor')) {
    rebuildSprites(true);
}

//Run repairUpgradeHistoryTable
if (version_compare($sugar_version, '6.5.0', '<') && function_exists('repairUpgradeHistoryTable')) {
    repairUpgradeHistoryTable();
}


//TAKE OUT TRASH
if (empty($errors)) {
    set_upgrade_progress('end', 'in_progress', 'unlinkingfiles', 'in_progress');
    logThis('Taking out the trash, unlinking temp files.', $path);
    unlinkUWTempFiles();
    removeSilentUpgradeVarsCache();
    logThis('Taking out the trash, done.', $path);
}

// Clear language cache
$repair = new RepairAndClear();
$repair->clearJsLangFiles();
$repair->clearLanguageCache();

// Rebuild .htaccess
require_once __DIR__ . '/../../install/install_utils.php';
handleHtaccess();

///////////////////////////////////////////////////////////////////////////////
////	RECORD ERRORS

$phpErrors = ob_get_clean();
logThis("**** Potential PHP generated error messages: {$phpErrors}", $path);

if (count($errors) > 0) {
    foreach ($errors as $error) {
        logThis("****** SilentUpgrade ERROR: {$error}", $path);
    }
    echo "FAILED\n";
} else {
    logThis("***** SilentUpgrade completed successfully.", $path);
    echo "********************************************************************\n";
    echo "*************************** SUCCESS*********************************\n";
    echo "********************************************************************\n";
    echo "******** If your pre-upgrade Leads data is not showing  ************\n";
    echo "******** Or you see errors in detailview subpanels  ****************\n";
    echo "************* In order to resolve them  ****************************\n";
    echo "******** Log into application as Administrator  ********************\n";
    echo "******** Go to Admin panel  ****************************************\n";
    echo "******** Run Repair -> Rebuild Relationships  **********************\n";
    echo "********************************************************************\n";
}
