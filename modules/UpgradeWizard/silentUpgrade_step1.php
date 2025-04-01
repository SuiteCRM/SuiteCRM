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

function prepSystemForUpgradeSilent()
{
    global $subdirs, $sugar_config;

    // make sure dirs exist
    foreach ($subdirs as $subdir) {
        if (!is_dir($sugar_config['upload_dir'] . "/upgrades/{$subdir}")) {
            mkdir_recursive($sugar_config['upload_dir'] . "/upgrades/{$subdir}");
        }
    }
}

/**
 * local function for clearing cache
 * @param $thedir
 * @param $extension
 */
function clearCacheSU($thedir, $extension)
{
    if ($current = @opendir($thedir)) {
        while (false !== ($children = readdir($current))) {
            if ($children !== '.' && $children !== '..') {
                if (is_dir($thedir . '/' . $children)) {
                    clearCacheSU($thedir . '/' . $children, $extension);
                } elseif (is_file($thedir . '/' . $children) && substr_count($children, (string) $extension)) {
                    unlink($thedir . '/' . $children);
                }
            }
        }
    }
}

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
        $sugar_config['logger'] = [
            'level' => 'fatal',
            'file' =>
                [
                    'ext' => '.log',
                    'name' => 'suitecrm',
                    'dateFormat' => '%c',
                    'maxSize' => '10MB',
                    'maxLogs' => 10,
                    'suffix' => '', // bug51583, change default suffix to blank for backwards comptability
                ],
        ];
        ksort($sugar_config);
        if (is_writable('config.php')) {
            write_array_to_file("sugar_config", $sugar_config, 'config.php');
        }
    }
}

function checkLeadConversionSettings()
{
    if (file_exists(getcwd() . '/config.php')) {
        require getcwd() . '/config.php';
    }
    global $sugar_config;
    if (!isset($sugar_config['lead_conv_activity_opt'])) {
        $sugar_config['lead_conv_activity_opt'] = 'copy';
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


function createMissingRels()
{
    $relForObjects = ['leads' => 'Leads', 'campaigns' => 'Campaigns', 'prospects' => 'Prospects'];
    foreach ($relForObjects as $relObjName => $relModName) {
        //assigned_user
        $guid = create_guid();
        $query = "SELECT id FROM relationships WHERE relationship_name = '{$relObjName}_assigned_user'";
        $result = DBManagerFactory::getInstance()->query($query, true);
        $a = null;
        $a = DBManagerFactory::getInstance()->fetchByAssoc($result);
        if (!isset($a['id']) && empty($a['id'])) {
            $qRel = "INSERT INTO relationships (id,relationship_name, lhs_module, lhs_table, lhs_key, rhs_module, rhs_table, rhs_key, join_table, join_key_lhs, join_key_rhs, relationship_type, relationship_role_column, relationship_role_column_value, reverse, deleted)
						VALUES ('{$guid}', '{$relObjName}_assigned_user','Users','users','id','{$relModName}','{$relObjName}','assigned_user_id',NULL,NULL,NULL,'one-to-many',NULL,NULL,'0','0')";
            DBManagerFactory::getInstance()->query($qRel);
        }
        //modified_user
        $guid = create_guid();
        $query = "SELECT id FROM relationships WHERE relationship_name = '{$relObjName}_modified_user'";
        $result = DBManagerFactory::getInstance()->query($query, true);
        $a = null;
        $a = DBManagerFactory::getInstance()->fetchByAssoc($result);
        if (!isset($a['id']) && empty($a['id'])) {
            $qRel = "INSERT INTO relationships (id,relationship_name, lhs_module, lhs_table, lhs_key, rhs_module, rhs_table, rhs_key, join_table, join_key_lhs, join_key_rhs, relationship_type, relationship_role_column, relationship_role_column_value, reverse, deleted)
						VALUES ('{$guid}', '{$relObjName}_modified_user','Users','users','id','{$relModName}','{$relObjName}','modified_user_id',NULL,NULL,NULL,'one-to-many',NULL,NULL,'0','0')";
            DBManagerFactory::getInstance()->query($qRel);
        }
        //created_by
        $guid = create_guid();
        $query = "SELECT id FROM relationships WHERE relationship_name = '{$relObjName}_created_by'";
        $result = DBManagerFactory::getInstance()->query($query, true);
        $a = null;
        $a = DBManagerFactory::getInstance()->fetchByAssoc($result);
        if (!isset($a['id']) && empty($a['id'])) {
            $qRel = "INSERT INTO relationships (id,relationship_name, lhs_module, lhs_table, lhs_key, rhs_module, rhs_table, rhs_key, join_table, join_key_lhs, join_key_rhs, relationship_type, relationship_role_column, relationship_role_column_value, reverse, deleted)
						VALUES ('{$guid}', '{$relObjName}_created_by','Users','users','id','{$relModName}','{$relObjName}','created_by',NULL,NULL,NULL,'one-to-many',NULL,NULL,'0','0')";
            DBManagerFactory::getInstance()->query($qRel);
        }
        $guid = create_guid();
        $query = "SELECT id FROM relationships WHERE relationship_name = '{$relObjName}_team'";
        $result = DBManagerFactory::getInstance()->query($query, true);
        $a = null;
        $a = DBManagerFactory::getInstance()->fetchByAssoc($result);
        if (!isset($a['id']) && empty($a['id'])) {
            $qRel = "INSERT INTO relationships (id,relationship_name, lhs_module, lhs_table, lhs_key, rhs_module, rhs_table, rhs_key, join_table, join_key_lhs, join_key_rhs, relationship_type, relationship_role_column, relationship_role_column_value, reverse, deleted)
							VALUES ('{$guid}', '{$relObjName}_team','Teams','teams','id','{$relModName}','{$relObjName}','team_id',NULL,NULL,NULL,'one-to-many',NULL,NULL,'0','0')";
            DBManagerFactory::getInstance()->query($qRel);
        }
    }
    //Also add tracker perf relationship
    $guid = create_guid();
    $query = "SELECT id FROM relationships WHERE relationship_name = 'tracker_monitor_id'";
    $result = DBManagerFactory::getInstance()->query($query, true);
    $a = null;
    $a = DBManagerFactory::getInstance()->fetchByAssoc($result);
    if (!isset($a['id']) && empty($a['id'])) {
        $qRel = "INSERT INTO relationships (id,relationship_name, lhs_module, lhs_table, lhs_key, rhs_module, rhs_table, rhs_key, join_table, join_key_lhs, join_key_rhs, relationship_type, relationship_role_column, relationship_role_column_value, reverse, deleted)
					VALUES ('{$guid}', 'tracker_monitor_id','TrackerPerfs','tracker_perf','monitor_id','Trackers','tracker','monitor_id',NULL,NULL,NULL,'one-to-many',NULL,NULL,'0','0')";
        DBManagerFactory::getInstance()->query($qRel);
    }
}


/**
 * This function will merge password default settings into config file
 * @param   $sugar_config
 * @param   $sugar_version
 * @return  bool true if successful
 */
function merge_passwordsetting($sugar_config, $sugar_version)
{
    $passwordsetting_defaults = [
        'passwordsetting' => [
            'minpwdlength' => '',
            'maxpwdlength' => '',
            'oneupper' => '',
            'onelower' => '',
            'onenumber' => '',
            'onespecial' => '',
            'SystemGeneratedPasswordON' => '',
            'generatepasswordtmpl' => '',
            'lostpasswordtmpl' => '',
            'customregex' => '',
            'regexcomment' => '',
            'forgotpasswordON' => false,
            'linkexpiration' => '1',
            'linkexpirationtime' => '30',
            'linkexpirationtype' => '1',
            'userexpiration' => '0',
            'userexpirationtime' => '',
            'userexpirationtype' => '1',
            'userexpirationlogin' => '',
            'systexpiration' => '0',
            'systexpirationtime' => '',
            'systexpirationtype' => '0',
            'systexpirationlogin' => '',
            'lockoutexpiration' => '0',
            'lockoutexpirationtime' => '',
            'lockoutexpirationtype' => '1',
            'lockoutexpirationlogin' => '',
            'factoremailtmpl' => '',
        ],
    ];

    $sugar_config = sugarArrayMerge($passwordsetting_defaults, $sugar_config);

    // need to override version with default no matter what
    $sugar_config['suitecrm_version'] = $suitecrm_version;

    ksort($sugar_config);

    if (write_array_to_file('sugar_config', $sugar_config, 'config.php')) {
        return true;
    }

    return false;
}

/**
 * @param array $defaultRoles
 */
function addDefaultModuleRoles($defaultRoles = [])
{
    foreach ($defaultRoles as $roleName => $role) {
        foreach ($role as $category => $actions) {
            foreach ($actions as $name => $access_override) {
                $query = "SELECT * FROM acl_actions WHERE name='$name' AND category = '$category' AND acltype='$roleName' AND deleted=0 ";
                $result = DBManagerFactory::getInstance()->query($query);
                //only add if an action with that name and category don't exist
                $row = DBManagerFactory::getInstance()->fetchByAssoc($result);
                if ($row === null) {
                    $guid = create_guid();
                    $currdate = gmdate('Y-m-d H:i:s');
                    $query = "INSERT INTO acl_actions (id,date_entered,date_modified,modified_user_id,name,category,acltype,aclaccess,deleted ) VALUES ('$guid','$currdate','$currdate','1','$name','$category','$roleName','$access_override','0')";
                    DBManagerFactory::getInstance()->query($query);
                }
            }
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


function threeWayMerge()
{
    //using threeway merge apis
}

// END UTILITIES THAT MUST BE LOCAL :(

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

// Verify that all the arguments are appropriately placed

$path = $argv[2]; // custom log file, if blank will use ./upgradeWizard.log
$subdirs = ['full', 'langpack', 'module', 'patch', 'theme', 'temp'];


define('SUGARCRM_PRE_INSTALL_FILE', 'scripts/pre_install.php');
define('SUGARCRM_POST_INSTALL_FILE', 'scripts/post_install.php');
define('SUGARCRM_PRE_UNINSTALL_FILE', 'scripts/pre_uninstall.php');
define('SUGARCRM_POST_UNINSTALL_FILE', 'scripts/post_uninstall.php');


echo "\n";
echo "********************************************************************\n";
echo "***************This Upgrade process may take sometime***************\n";
echo "********************************************************************\n";
echo "\n";

global $sugar_config;
$isDCEInstance = false;
$errors = [];


if ($upgradeType !== constant('DCE_INSTANCE')) {
    ini_set('error_reporting', 1);
    require_once('include/entryPoint.php');
    require_once('include/SugarLogger/SugarLogger.php');
    require_once('include/utils/php_zip_utils.php');


    if (!function_exists('sugar_cached')) {
        /**
         * sugar_cached
         *
         * @param $file string The path to retrieve cache lookup information for
         * @return string The cached path according to $GLOBALS['sugar_config']['cache_dir'] or just appended with cache if not defined
         */
        function sugar_cached($file)
        {
            static $cdir = null;
            if (empty($cdir) && !empty($GLOBALS['sugar_config']['cache_dir'])) {
                $cdir = rtrim($GLOBALS['sugar_config']['cache_dir'], '/\\');
            }
            if (empty($cdir)) {
                $cdir = 'cache';
            }

            return "$cdir/$file";
        }
    }

    require 'config.php';
    //require_once('modules/UpgradeWizard/uw_utils.php'); // must upgrade UW first
    if (isset($argv[3]) && is_dir($argv[3])) {
        $cwd = $argv[3];
        chdir($cwd);
    }

    require_once "{$cwd}/suitecrm_version.php";
    require_once "{$cwd}/sugar_version.php"; // provides $sugar_version & $sugar_flavor

    $GLOBALS['log'] = LoggerManager::getLogger();
    $patchName = basename($argv[1]);
    $zip_from_dir = substr($patchName, 0, -4); // patch folder name (minus ".zip")
    $path = $argv[2]; // custom log file, if blank will use ./upgradeWizard.log

    $db = &DBManagerFactory::getInstance();
    $UWstrings = return_module_language('en_us', 'UpgradeWizard');
    $adminStrings = return_module_language('en_us', 'Administration');
    $app_list_strings = return_app_list_strings_language('en_us');
    $mod_strings = array_merge($adminStrings, $UWstrings);
    $subdirs = ['full', 'langpack', 'module', 'patch', 'theme', 'temp'];
    global $unzip_dir;
    $license_accepted = false;
    if (isset($argv[5]) && (strtolower($argv[5]) == 'yes' || strtolower($argv[5]) == 'y')) {
        $license_accepted = true;
    }

    // Adding admin user to the silent upgrade

    $current_user = BeanFactory::newBean('Users');
    if (isset($argv[4])) {
        //if being used for internal upgrades avoid admin user verification
        $user_name = $argv[4];
        $q = "select id from users where user_name = '" . $user_name . "' and is_admin=1";
        $result = DBManagerFactory::getInstance()->query($q, false);
        $logged_user = DBManagerFactory::getInstance()->fetchByAssoc($result);
        if (isset($logged_user['id']) && $logged_user['id'] !== null) {
            //do nothing
            $current_user->retrieve($logged_user['id']);
        } else {
            echo "FAILURE: Not an admin user in users table. Please provide an admin user\n";
            exit(1);
        }
    } else {
        echo "*******************************************************************************\n";
        echo "*** ERROR: 4th parameter must be a valid admin user.\n";
        echo $usage_regular;
        echo "FAILURE\n";
        exit(1);
    }


    global $sugar_config;
    $configOptions = $sugar_config['dbconfig'];


    // UPGRADE PREP
    prepSystemForUpgradeSilent();

    //repair tabledictionary.ext.php file if needed
    repairTableDictionaryExtFile();

    $unzip_dir = sugar_cached('upgrades/temp');

    $install_file = $sugar_config['upload_dir'] . '/upgrades/patch/' . basename($argv[1]);
    sugar_mkdir($sugar_config['upload_dir'] . '/upgrades/patch', 0775, true);

    if (isset($manifest['copy_files']['from_dir']) && $manifest['copy_files']['from_dir'] !== "") {
        $zip_from_dir = $manifest['copy_files']['from_dir'];
    }


    $install_file = $sugar_config['upload_dir'] . '/upgrades/patch/' . basename($argv[1]);

    $_SESSION['unzip_dir'] = $unzip_dir;
    $_SESSION['install_file'] = $install_file;
    $_SESSION['zip_from_dir'] = $zip_from_dir;
    if (is_dir($unzip_dir . '/scripts')) {
        rmdir_recursive($unzip_dir . '/scripts');
    }
    if (is_file($unzip_dir . '/manifest.php')) {
        rmdir_recursive($unzip_dir . '/manifest.php');
    }
    mkdir_recursive($unzip_dir);
    if (!is_dir($unzip_dir)) {
        echo "\n{$unzip_dir} is not an available directory\nFAILURE\n";
        fwrite(STDERR, "\n{$unzip_dir} is not an available directory\nFAILURE\n");
        exit(1);
    }

    unzip($argv[1], $unzip_dir);
    // mimic standard UW by copy patch zip to appropriate dir
    copy($argv[1], $install_file);
    // END UPGRADE PREP


    // UPGRADE UPGRADEWIZARD
    $zipBasePath = "$unzip_dir/{$zip_from_dir}";
    $uwFiles = findAllFiles("{$zipBasePath}/modules/UpgradeWizard", []);
    $destFiles = [];

    foreach ($uwFiles as $uwFile) {
        $destFile = str_replace($zipBasePath . "/", '', (string) $uwFile);
        copy($uwFile, $destFile);
    }
    require_once 'modules/UpgradeWizard/uw_utils.php'; // must upgrade UW first
    removeSilentUpgradeVarsCache(); // Clear the silent upgrade vars - Note: Any calls to these functions within this file are removed here
    logThis('*** SILENT UPGRADE INITIATED.', $path);
    logThis('*** UpgradeWizard Upgraded  ', $path);

    if (function_exists('set_upgrade_vars')) {
        set_upgrade_vars();
    }

    if ($configOptions['db_type'] === 'mysql') {
        //Change the db wait_timeout for this session
        $now_timeout = $db->getOne('select @@wait_timeout');
        logThis('Wait Timeout before change ***** ' . $now_timeout, $path);
        $db->query('set wait_timeout=28800');
        $now_timeout = $db->getOne('select @@wait_timeout');
        logThis('Wait Timeout after change ***** ' . $now_timeout, $path);
    }

    // END UPGRADE UPGRADEWIZARD


    // MAKE SURE PATCH IS COMPATIBLE
    if (is_file("$unzip_dir/manifest.php")) {
        include "$unzip_dir/manifest.php";
        if (!isset($manifest)) {
            fwrite(STDERR, "\nThe patch did not contain a proper manifest.php file.  Cannot continue.\n\n");
            exit(1);
        }
        copy("$unzip_dir/manifest.php", $sugar_config['upload_dir'] . "/upgrades/patch/{$zip_from_dir}-manifest.php");

        $error = validate_manifest($manifest);
        if (!empty($error)) {
            $error = strip_tags(br2nl($error));
            fwrite(STDERR, "\n{$error}\n\nFAILURE\n");
            exit(1);
        }
    } else {
        fwrite(STDERR, "\nThe patch did not contain a proper manifest.php file.  Cannot continue.\n\n");
        exit(1);
    }

    $_SESSION['upgrade_from_flavor'] = $manifest['name'];

    global $sugar_config, $sugar_version, $sugar_flavor;

    //END MAKE SURE PATCH IS COMPATIBLE


    // RUN SILENT UPGRADE
    ob_start();
    set_time_limit(0);
    if (file_exists('ModuleInstall/PackageManager/PackageManagerDisplay.php')) {
        require_once 'ModuleInstall/PackageManager/PackageManagerDisplay.php';
    }


    //copy minimum required files including sugar_file_utils.php
    if (file_exists("{$zipBasePath}/include/utils/sugar_file_utils.php")) {
        $destFile = clean_path(str_replace($zipBasePath, $cwd, "{$zipBasePath}/include/utils/sugar_file_utils.php"));
        copy("{$zipBasePath}/include/utils/sugar_file_utils.php", $destFile);
    }
    if (file_exists('include/utils/sugar_file_utils.php')) {
        require_once 'include/utils/sugar_file_utils.php';
    }

    //If version less than 500 then look for modules to be upgraded
    if (function_exists('set_upgrade_vars')) {
        set_upgrade_vars();
    }
    //Initialize the session variables. If upgrade_progress.php is already created
    //look for session vars there and restore them
    if (function_exists('initialize_session_vars')) {
        initialize_session_vars();
    }

    if (!didThisStepRunBefore('preflight')) {
        set_upgrade_progress('preflight', 'in_progress');
        //Quickcreatedefs on the basis of editviewdefs
        updateQuickCreateDefs();
        set_upgrade_progress('preflight', 'done');
    }
    // COMMIT PROCESS BEGINS
    // MAKE BACKUPS OF TARGET FILES

    if (!didThisStepRunBefore('commit')) {
        set_upgrade_progress('commit', 'in_progress', 'commit', 'in_progress');
        if (!didThisStepRunBefore('commit', 'commitMakeBackupFiles')) {
            set_upgrade_progress('commit', 'in_progress', 'commitMakeBackupFiles', 'in_progress');
            $errors = commitMakeBackupFiles($rest_dir, $install_file, $unzip_dir, $zip_from_dir, []);
            set_upgrade_progress('commit', 'in_progress', 'commitMakeBackupFiles', 'done');
        }

        //Need to make sure we have the matching copy of SetValueAction for static/instance method matching
        if (file_exists('include/Expressions/Actions/SetValueAction.php')) {
            require_once "include/Expressions/Actions/SetValueAction.php";
        }

        ///////////////////////////////////////////////////////////////////////////////
        ////	HANDLE PREINSTALL SCRIPTS
        if (empty($errors)) {
            $file = "{$unzip_dir}/" . constant('SUGARCRM_PRE_INSTALL_FILE');

            if (is_file($file)) {
                include $file;
                if (!didThisStepRunBefore('commit', 'pre_install')) {
                    set_upgrade_progress('commit', 'in_progress', 'pre_install', 'in_progress');
                    pre_install();
                    set_upgrade_progress('commit', 'in_progress', 'pre_install', 'done');
                }
            }
        }

        //Clean smarty from cache
        $cachedir = sugar_cached('smarty');
        if (is_dir($cachedir)) {
            $allModFiles = [];
            $allModFiles = findAllFiles($cachedir, $allModFiles);
            foreach ($allModFiles as $file) {
                if (file_exists($file)) {
                    unlink($file);
                }
            }
        }

        //Also add the three-way merge here. The idea is after the 451 html files have
        //been converted run the 3-way merge. If 500 then just run the 3-way merge
        if (file_exists('modules/UpgradeWizard/SugarMerge/SugarMerge.php')) {
            set_upgrade_progress('end', 'in_progress', 'threewaymerge', 'in_progress');
            require_once 'modules/UpgradeWizard/SugarMerge/SugarMerge.php';
            $merger = new SugarMerge($zipBasePath);
            $merger->mergeAll();
            set_upgrade_progress('end', 'in_progress', 'threewaymerge', 'done');
        }

        // COPY NEW FILES INTO TARGET INSTANCE
        $skippedFiles = '';

        if (!didThisStepRunBefore('commit', 'commitCopyNewFiles')) {
            set_upgrade_progress('commit', 'in_progress', 'commitCopyNewFiles', 'in_progress');
            $split = commitCopyNewFiles($unzip_dir, $zip_from_dir);
            $copiedFiles = $split['copiedFiles'];
            $skippedFiles = $split['skippedFiles'];
            set_upgrade_progress('commit', 'in_progress', 'commitCopyNewFiles', 'done');
        }
        require_once clean_path($unzip_dir . '/scripts/upgrade_utils.php');
        $new_sugar_version = getUpgradeVersion();
        $siv_varset_1 = setSilentUpgradeVar('origVersion', $sugar_version);
        $siv_varset_2 = setSilentUpgradeVar('destVersion', $new_sugar_version);
        $siv_write = writeSilentUpgradeVars();
        if (!$siv_varset_1 || !$siv_varset_2 || !$siv_write) {
            logThis("Error with silent upgrade variables: origVersion write success is ({$siv_varset_1}) " .
                "-- destVersion write success is ({$siv_varset_2}) -- " .
                "writeSilentUpgradeVars success is ({$siv_write}) -- " .
                "path to cache dir is ({$GLOBALS['sugar_config']['cache_dir']})", $path);
        }
        require_once 'modules/DynamicFields/templates/Fields/TemplateText.php';

        // RELOAD NEW DEFINITIONS
        global $ACLActions, $beanList, $beanFiles;
        include 'modules/ACLActions/actiondefs.php';
        include 'include/modules.php';


        //HANDLE POSTINSTALL SCRIPTS
        if (empty($errors)) {
            logThis('Starting post_install()...', $path);

            $trackerManager = TrackerManager::getInstance();
            $trackerManager->pause();
            $trackerManager->unsetMonitors();

            if (!didThisStepRunBefore('commit', 'post_install')) {
                $file = "$unzip_dir/" . constant('SUGARCRM_POST_INSTALL_FILE');
                if (is_file($file)) {
                    $progArray['post_install'] = 'in_progress';
                    post_install_progress($progArray, 'set');
                    global $moduleList;
                    include $file;
                    post_install();
                    // cn: only run conversion if admin selects "Sugar runs SQL"
                    if (!empty($_SESSION['allTables']) && $_SESSION['schema_change'] === 'sugar') {
                        executeConvertTablesSql($_SESSION['allTables']);
                    }
                    //set process to done
                    $progArray['post_install'] = 'done';
                    post_install_progress($progArray, 'set');
                }
            }
            //clean vardefs
            logThis('Performing UWrebuild()...', $path);
            ob_start();
            @UWrebuild();
            ob_end_clean();
            logThis('UWrebuild() done.', $path);

            logThis('begin check default permissions .', $path);
            checkConfigForPermissions();
            logThis('end check default permissions .', $path);

            logThis('begin check logger settings .', $path);
            checkLoggerSettings();
            logThis('begin check logger settings .', $path);

            logThis('begin check lead conversion settings .', $path);
            checkLeadConversionSettings();
            logThis('end check lead conversion settings .', $path);

            logThis('begin check resource settings .', $path);
            checkResourceSettings();
            logThis('begin check resource settings .', $path);


            require "sugar_version.php";
            require 'config.php';
            global $sugar_config;

            if (!write_array_to_file('sugar_config', $sugar_config, 'config.php')) {
                logThis('*** ERROR: could not write config.php! - upgrade will fail!', $path);
                $errors[] = 'Could not write config.php!';
            }

            if (!write_array_to_file('sugar_config', $sugar_config, 'config.php')) {
                logThis('*** ERROR: could not write config.php! - upgrade will fail!', $path);
                $errors[] = 'Could not write config.php!';
            }

            if (version_compare($new_sugar_version, $sugar_version, '=')) {
                require 'config.php';
            }
            //upgrade the sugar version prior to writing config file.
            logThis('Upgrade the sugar_version', $path);
            $sugar_config['sugar_version'] = $sugar_version;

            if (!write_array_to_file('sugar_config', $sugar_config, 'config.php')) {
                logThis('*** ERROR: could not write config.php! - upgrade will fail!', $path);
                $errors[] = 'Could not write config.php!';
            }

            logThis('post_install() done.', $path);
        }

        ///////////////////////////////////////////////////////////////////////////////
        ////	REGISTER UPGRADE
        if (empty($errors)) {
            logThis('Registering upgrade with UpgradeHistory', $path);
            if (!didThisStepRunBefore('commit', 'upgradeHistory')) {
                set_upgrade_progress('commit', 'in_progress', 'upgradeHistory', 'in_progress');
                $file_action = 'copied';
                // if error was encountered, script should have died before now
                $new_upgrade = new UpgradeHistory();
                $new_upgrade->filename = $install_file;
                $new_upgrade->md5sum = md5_file($install_file);
                $new_upgrade->name = $zip_from_dir;
                $new_upgrade->description = $manifest['description'];
                $new_upgrade->type = 'patch';
                $new_upgrade->version = $suitecrm_version;
                $new_upgrade->status = 'installed';
                $new_upgrade->manifest = (!empty($_SESSION['install_manifest']) ? $_SESSION['install_manifest'] : '');

                if ($new_upgrade->description === null) {
                    $new_upgrade->description = 'Silent Upgrade was used to upgrade the instance';
                } else {
                    $new_upgrade->description .= ' Silent Upgrade was used to upgrade the instance.';
                }

                // Running db insert query as bean save will throw logic hook errors due to dependencies that are not set yet
                $customID = create_guid();
                $new_upgrade->date_entered = $GLOBALS['timedate']->nowDb();

                $customIDQuoted = $db->quoted($customID);
                $fileNameQuoted = $db->quoted($new_upgrade->filename);
                $md5Quoted = $db->quoted($new_upgrade->md5sum);
                $typeQuoted = $db->quoted($new_upgrade->type);
                $statusQuoted = $db->quoted($new_upgrade->status);
                $versionQuoted = $db->quoted($new_upgrade->version);
                $nameQuoted = $db->quoted($new_upgrade->name);
                $descriptionQuoted = $db->quoted($new_upgrade->description);
                $manifestQuoted = $db->quoted($new_upgrade->manifest);
                $dateQuoted = $db->quoted($new_upgrade->date_entered);

                $upgradeHistoryInsert = "INSERT INTO upgrade_history (id, filename, md5sum, type, status, version, name, description, id_name, manifest, date_entered, enabled) 
                                                     VALUES ($customIDQuoted, $fileNameQuoted, $md5Quoted, $typeQuoted, $statusQuoted, $versionQuoted, $nameQuoted, $descriptionQuoted, NULL, $manifestQuoted, $dateQuoted, '1')";
                $result = $db->query($upgradeHistoryInsert, true, "Error writing upgrade history");

                set_upgrade_progress('commit', 'in_progress', 'upgradeHistory', 'done');
                set_upgrade_progress('commit', 'done', 'commit', 'done');
            }
        }

        //Clean modules from cache
        $cachedir = sugar_cached('smarty');
        if (is_dir($cachedir)) {
            $allModFiles = array();
            $allModFiles = findAllFiles($cachedir, $allModFiles);
            foreach ($allModFiles as $file) {
                if (file_exists($file)) {
                    unlink($file);
                }
            }
        }
        //delete cache/modules before rebuilding the relations
        //Clean modules from cache
        $cachedir = sugar_cached('modules');
        if (is_dir($cachedir)) {
            $allModFiles = [];
            $allModFiles = findAllFiles($cachedir, $allModFiles);
            foreach ($allModFiles as $file) {
                if (file_exists($file)) {
                    unlink($file);
                }
            }
        }

        //delete cache/themes
        $cachedir = sugar_cached('themes');
        if (is_dir($cachedir)) {
            $allModFiles = [];
            $allModFiles = findAllFiles($cachedir, $allModFiles);
            foreach ($allModFiles as $file) {
                if (file_exists($file)) {
                    unlink($file);
                }
            }
        }
        ob_start();
        $_REQUEST['silent'] = true;

        @createMissingRels();
        ob_end_clean();
    }

    set_upgrade_progress('end', 'in_progress', 'end', 'in_progress');
    // Old Logger settings

    if (function_exists('deleteCache')) {
        set_upgrade_progress('end', 'in_progress', 'deleteCache', 'in_progress');
        @deleteCache();
        set_upgrade_progress('end', 'in_progress', 'deleteCache', 'done');
    }

    // HANDLE REMINDERS
    if (empty($errors)) {
        commitHandleReminders($skippedFiles, $path);
    }

    require_once 'modules/Administration/Administration.php';
    $admin = BeanFactory::newBean('Administration');
    $admin->saveSetting('system', 'adminwizard', 1);

    if (isset($_SESSION['current_db_version'], $_SESSION['target_db_version']) && version_compare($_SESSION['current_db_version'],
            $_SESSION['target_db_version'], '=')) {
        $_REQUEST['upgradeWizard'] = true;
        ob_start();
        include 'include/Smarty/internals/core.write_file.php';
        ob_end_clean();
        $db =& DBManagerFactory::getInstance();
    }

    $phpErrors = ob_get_clean();
    logThis("**** Potential PHP generated error messages: {$phpErrors}", $path);

    if (count($errors) > 0) {
        foreach ($errors as $error) {
            logThis("****** SilentUpgrade ERROR: {$error}", $path);
        }
        echo "FAILED\n";
    }
}


/**
 * repairTableDictionaryExtFile
 *
 * There were some scenarios in 6.0.x whereby the files loaded in the extension tabledictionary.ext.php file
 * did not exist.  This would cause warnings to appear during the upgrade.  As a result, this
 * function scans the contents of tabledictionary.ext.php and then remove entries where the file does exist.
 */
function repairTableDictionaryExtFile()
{
    $tableDictionaryExtDirs = [
        'custom/Extension/application/Ext/TableDictionary',
        'custom/application/Ext/TableDictionary'
    ];

    foreach ($tableDictionaryExtDirs as $tableDictionaryExt) {
        if (is_dir($tableDictionaryExt) && is_writable($tableDictionaryExt)) {
            $dir = dir($tableDictionaryExt);
            while (($entry = $dir->read()) !== false) {
                $entry = $tableDictionaryExt . '/' . $entry;
                if (is_file($entry) && preg_match('/\.php$/i', $entry) && is_writable($entry)) {
                    if (function_exists('sugar_fopen')) {
                        $fp = @sugar_fopen($entry, 'r');
                    } else {
                        $fp = fopen($entry, 'rb');
                    }

                    $altered = false;
                    $contents = '';

                    if ($fp) {
                        while ($line = fgets($fp)) {
                            if (preg_match('/\s*include\s*\(\s*[\'|\"](.*?)[\"|\']\s*\)\s*;/', $line,
                                    $match) && !file_exists($match[1])) {
                                $altered = true;
                            } else {
                                $contents .= $line;
                            }
                        }

                        fclose($fp);
                    }


                    if ($altered) {
                        if (function_exists('sugar_file_put_contents')) {
                            @sugar_file_put_contents($entry, $contents);
                        } else {
                            file_put_contents($entry, $contents);
                        }
                    }
                }
            }
        }
    }
}
