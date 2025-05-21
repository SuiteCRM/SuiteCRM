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


logThis('[At end.php]');
global $unzip_dir;
global $path;
global $sugar_config;

if ($unzip_dir == null) {
    $unzip_dir = isset($_SESSION['unzip_dir']) ? $_SESSION['unzip_dir'] : null;
}
//First repair the databse to ensure it is up to date with the new vardefs/tabledefs
logThis('About to repair the database.', $path);
//Use Repair and rebuild to update the database.
global $dictionary, $beanFiles;

require_once 'modules/Trackers/TrackerManager.php';
$trackerManager = TrackerManager::getInstance();
$trackerManager->pause();
$trackerManager->unsetMonitors();

require_once 'modules/Administration/QuickRepairAndRebuild.php';
$rac = new RepairAndClear();
$rac->clearVardefs();
$rac->rebuildExtensions();
//bug: 44431 - defensive check to ensure the method exists since upgrades to 6.2.0 may not have this method define yet.
if (method_exists($rac, 'clearExternalAPICache')) {
    $rac->clearExternalAPICache();
}

$repairedTables = array();

foreach ($beanFiles as $bean => $file) {
    if (file_exists($file)) {
        require_once $file;
        unset($GLOBALS['dictionary'][$bean]);
        $focus = new $bean();
        if (($focus instanceof SugarBean)) {
            if (!isset($repairedTables[$focus->table_name])) {
                $sql = DBManagerFactory::getInstance()->repairTable($focus, true);
                if (trim($sql) != '') {
                    logThis('Running sql:'.$sql, $path);
                }
                $repairedTables[$focus->table_name] = true;
            }

            //Check to see if we need to create the audit table
            if ($focus->is_AuditEnabled() && !$focus->db->tableExists($focus->get_audit_table_name())) {
                logThis('Creating audit table:'.$focus->get_audit_table_name(), $path);
                $focus->create_audit_table();
            }
        }
    }
}

// add suite version into upgrade pack!
if (isset($repairedTables['reminders']) && $repairedTables['reminders'] && isset($_SESSION['suitecrm_version_before_upgrade']) && version_compare($_SESSION['suitecrm_version_before_upgrade'], Reminder::UPGRADE_VERSION, '<')) {
    Reminder::upgrade();
    unset($_SESSION['suitecrm_version_before_upgrade']);
}

$olddictionary = $dictionary;

unset($dictionary);
include 'modules/TableDictionary.php';
foreach ($dictionary as $meta) {
    $tablename = $meta['table'];
    if (isset($repairedTables[$tablename])) {
        continue;
    }
    $fielddefs = $meta['fields'];
    $indices = $meta['indices'];
    $sql = DBManagerFactory::getInstance()->repairTableParams($tablename, $fielddefs, $indices, true);
    if (trim($sql) != '') {
        logThis('Running sql:'.$sql, $path);
    }
    $repairedTables[$tablename] = true;
}

$dictionary = $olddictionary;

logThis('database repaired', $path);

$ce_to_pro_ent = isset($_SESSION['upgrade_from_flavor']) && ($_SESSION['upgrade_from_flavor'] == 'SugarCE to SugarPro' || $_SESSION['upgrade_from_flavor'] == 'SugarCE to SugarEnt' || $_SESSION['upgrade_from_flavor'] == 'SugarCE to SugarCorp' || $_SESSION['upgrade_from_flavor'] == 'SugarCE to SugarUlt');

logThis(' Start Rebuilding the config file again', $path);

//check and set the logger before rebuilding config
if (!isset($sugar_config['logger'])) {
    $sugar_config['logger'] = array(
        'level' => 'fatal',
        'file' => array(
                'ext' => '.log',
                'name' => 'suitecrm',
                'dateFormat' => '%c',
                'maxSize' => '10MB',
                'maxLogs' => 10,
                'suffix' => '', // bug51583, change default suffix to blank for backwards comptability
            ),
    );
}
//for upgraded version, set default lead conversion activity option to 'copy'
if (!isset($sugar_config['lead_conv_activity_opt'])) {
    $sugar_config['lead_conv_activity_opt'] = 'copy';
}

if (!rebuildConfigFile($sugar_config, $sugar_version)) {
    logThis('*** WARNING: could not write config.php!', $path);
}
logThis(' Finish Rebuilding the config file again', $path);

set_upgrade_progress('end', 'in_progress');

if (isset($_SESSION['current_db_version']) && isset($_SESSION['target_db_version'])) {
    if (version_compare($_SESSION['current_db_version'], $_SESSION['target_db_version'], '!=')) {
    }

    //keeping separate. making easily visible and readable
    if (version_compare($_SESSION['current_db_version'], $_SESSION['target_db_version'], '=')) {
        $_REQUEST['upgradeWizard'] = true;
        ob_start();
        include 'modules/ACL/install_actions.php';
        include_once 'include/Smarty/internals/core.write_file.php';
        ob_end_clean();
        $db = &DBManagerFactory::getInstance();
        if ($ce_to_pro_ent) {
            //Also set license information
            $admin = BeanFactory::newBean('Administration');
            $category = 'license';
            $value = '0';
            $admin->saveSetting($category, 'users', $value);
            $key = array('num_lic_oc', 'key', 'expire_date');
            $value = '';
            foreach ($key as $k) {
                $admin->saveSetting($category, $k, $value);
            }
        }
    }
}

// Mark the instance as having gone thru the admin wizard
$admin = BeanFactory::newBean('Administration');
$admin->saveSetting('system', 'adminwizard', 1);

//Upgrade connectors
logThis('Begin upgrade_connectors', $path);
upgrade_connectors();
logThis('End upgrade_connectors', $path);

//Unlink files that have been removed
if (function_exists('unlinkUpgradeFiles')) {
    unlinkUpgradeFiles($_SESSION['current_db_version']);
}

if (function_exists('rebuildSprites') && function_exists('imagecreatetruecolor')) {
    if (!empty($sugar_config['use_sprites']) && $sugar_config['use_sprites']) {
        rebuildSprites(true);
    }
}

//Run repairUpgradeHistoryTable
if (version_compare($_SESSION['current_db_version'], '6.5.0', '<') && function_exists('repairUpgradeHistoryTable')) {
    repairUpgradeHistoryTable();
}

require_once 'modules/Administration/upgrade_custom_relationships.php';
upgrade_custom_relationships();

require_once 'modules/UpgradeWizard/uw_utils.php';

// guarantee the cron user check mechanism works for people upgrading from systems installed before the check existed:
require_once 'include/utils.php';
addCronAllowedUser(getRunningUser());

set_upgrade_progress('end', 'done');

logThis('Cleaning up the session.  Goodbye.');
unlinkUWTempFiles();
logThis('Cleaning up the session.  Goodbye.');
resetUwSession();
// flag to say upgrade has completed
$_SESSION['upgrade_complete'] = true;

//Clear any third party caches
sugar_cache_reset_full();

//add the clean vardefs here
VardefManager::clearVardef();

require_once 'include/TemplateHandler/TemplateHandler.php';
TemplateHandler::clearAll();

//also add the cache cleaning here.
if (function_exists('deleteCache')) {
    deleteCache();
}

global $mod_strings;
global $current_language;

if (!isset($current_language) || ($current_language == null)) {
    $current_language = 'en_us';
}
if (isset($GLOBALS['current_language']) && ($GLOBALS['current_language'] != null)) {
    $current_language = $GLOBALS['current_language'];
}
$mod_strings = return_module_language($current_language, 'UpgradeWizard');
$stop = false;

$httpHost = $_SERVER['HTTP_HOST'];  // cn: 8472 - HTTP_HOST includes port in some cases
if ($colon = strpos((string) $httpHost, ':')) {
    $httpHost = substr((string) $httpHost, 0, $colon);
}
$parsedSiteUrl = parse_url((string) $sugar_config['site_url']);
$host = ($parsedSiteUrl['host'] != $httpHost) ? $httpHost : $parsedSiteUrl['host'];

// aw: 9747 - use SERVER_PORT for users who don't plug in the site_url at install correctly
if ($_SERVER['SERVER_PORT'] != 80) {
    $port = ':'.$_SERVER['SERVER_PORT'];
} elseif (isset($parsedSiteUrl['port']) && $parsedSiteUrl['port'] != 80) {
    $port = ':'.$parsedSiteUrl['port'];
} else {
    $port = '';
}
$path = $parsedSiteUrl['path'];
$cleanUrl = "{$parsedSiteUrl['scheme']}://{$host}{$port}{$path}/index.php";

/*ob_start();
check_now(get_sugarbeat());
ob_end_clean();*/

include 'PasswordExpirationService.php';
$expirationMessage = (new PasswordExpirationService())->getExpirationMessage();

$uwMain = <<<eoq
<table cellpadding="3" cellspacing="0" border="0">

	<tr>
		<td align="left">
			<p>
			<br>
			{$mod_strings['LBL_UW_END_LOGOUT_PRE2']}
			<br>
			<br>
            <b>{$mod_strings['LBL_UW_END_LOGOUT_PRE']}</b> {$mod_strings['LBL_UW_END_LOGOUT']}
			</p>
			<br>
			<p>{$expirationMessage}</p>
		</td>
	</tr>
</table>

<script>
 function deleteCacheAjax(){
	//AJAX call for checking the file size and comparing with php.ini settings.
	var callback = {
		 success:function(r) {
		     //alert(r.responseText);
		 }
	}
	postData = '&module=UpgradeWizard&action=deleteCache&to_pdf=1';
	YAHOO.util.Connect.asyncRequest('POST', 'index.php', callback, postData);
}
</script>
eoq;

$showBack = false;
$showCancel = false;
$showRecheck = false;
$showNext = false;
$showDone = true;

$stepBack = 0;
$stepNext = 0;
$stepCancel = 0;
$stepRecheck = 0;

$_SESSION['step'][$steps['files'][$_REQUEST['step']]] = ($stop) ? 'failed' : 'success';
unset($_SESSION['current_db_version']);
unset($_SESSION['target_db_version']);


ob_start();
include __DIR__ . '/../Administration/UpgradeAccess.php';
echo $cnt = ob_get_contents();
ob_get_clean();
