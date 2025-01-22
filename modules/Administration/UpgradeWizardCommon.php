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


if (!is_admin($GLOBALS['current_user'])) {
    sugar_die($GLOBALS['app_strings']['ERR_NOT_ADMIN']);
}

require_once('include/utils/db_utils.php');
require_once('include/utils/php_zip_utils.php');

// increase the cuttoff time to 1 hour
ini_set("max_execution_time", "3600");

if (isset($_REQUEST['view']) && ($_REQUEST['view'] != "")) {
    $view = $_REQUEST['view'];
    if ($view != "default" && $view != "module") {
        die($mod_strings['ERR_UW_INVALID_VIEW']);
    }
} else {
    die($mod_strings['ERR_UW_NO_VIEW']);
}
$form_action = "index.php?module=Administration&view=" . $view . "&action=UpgradeWizard";


$base_upgrade_dir       = "upload://upgrades";
$base_tmp_upgrade_dir   = sugar_cached('upgrades/temp');

$GLOBALS['subdirs'] = array('full', 'langpack', 'module', 'patch', 'theme');
// array of special scripts that are executed during (un)installation-- key is type of script, value is filename

if (!defined('SUGARCRM_PRE_INSTALL_FILE')) {
    define('SUGARCRM_PRE_INSTALL_FILE', 'scripts/pre_install.php');
    define('SUGARCRM_POST_INSTALL_FILE', 'scripts/post_install.php');
    define('SUGARCRM_PRE_UNINSTALL_FILE', 'scripts/pre_uninstall.php');
    define('SUGARCRM_POST_UNINSTALL_FILE', 'scripts/post_uninstall.php');
}
$script_files = array(
    "pre-install" => constant('SUGARCRM_PRE_INSTALL_FILE'),
    "post-install" => constant('SUGARCRM_POST_INSTALL_FILE'),
    "pre-uninstall" => constant('SUGARCRM_PRE_UNINSTALL_FILE'),
    "post-uninstall" => constant('SUGARCRM_POST_UNINSTALL_FILE'),
);



function extractFile($zip_file, $file_in_zip)
{
    global $base_tmp_upgrade_dir;
    if (empty($base_tmp_upgrade_dir)) {
        $base_tmp_upgrade_dir   = sugar_cached("upgrades/temp");
    }
    $my_zip_dir = mk_temp_dir($base_tmp_upgrade_dir);
    register_shutdown_function('rmdir_recursive', $my_zip_dir);
    unzip_file($zip_file, $file_in_zip, $my_zip_dir);
    return("$my_zip_dir/$file_in_zip");
}

function extractManifest($zip_file)
{
    return(extractFile($zip_file, "manifest.php"));
}

function getInstallType($type_string)
{
    // detect file type
    global $subdirs;

    foreach ($subdirs as $subdir) {
        if (preg_match("#/$subdir/#", (string) $type_string)) {
            return($subdir);
        }
    }
    // return empty if no match
    return("");
}

function getImageForType($type)
{
    global $mod_strings;

    $mod_strings = $mod_strings ?? [];

    $icon = "";
    switch ($type) {
        case "full":
            $icon = SugarThemeRegistry::current()->getImage("Upgrade", "", null, null, '.gif', $mod_strings['LBL_DST_UPGRADE']);
            break;
        case "langpack":
            $icon = SugarThemeRegistry::current()->getImage("LanguagePacks", "", null, null, '.gif', $mod_strings['LBL_LANGUAGE_PACKS']);
            break;
        case "module":
            $icon = SugarThemeRegistry::current()->getImage("ModuleLoader", "", null, null, '.gif', $mod_strings['LBL_MODULE_LOADER_TITLE']);
            break;
        case "patch":
            $icon = SugarThemeRegistry::current()->getImage("PatchUpgrades", "", null, null, '.gif', $mod_strings['LBL_PATCH_UPGRADES']);
            break;
        case "theme":
            $icon = SugarThemeRegistry::current()->getImage("Themes", "", null, null, '.gif', $mod_strings['LBL_THEME_SETTINGS']);
            break;
        default:
            break;
    }
    return($icon);
}

function getLanguagePackName($the_file)
{
    global $app_list_strings;
    require_once((string)$the_file);
    if (isset($app_list_strings["language_pack_name"])) {
        return($app_list_strings["language_pack_name"]);
    }
    return("");
}

function getUITextForType($type)
{
    $type = 'LBL_UW_TYPE_'.strtoupper($type);
    global $mod_strings;
    return $mod_strings[$type] ?? '';
}

function getUITextForMode($mode)
{
    $mode = 'LBL_UW_MODE_'.strtoupper($mode);
    global $mod_strings;
    return $mod_strings[$mode] ?? '';
}

function validate_manifest($manifest)
{
    require_once('ModuleInstall/PackageManager/PackageManager.php');
    $pm = new PackageManager();
    $pm->validate_manifest($manifest);
}

function getDiffFiles($unzip_dir, $install_file, $is_install = true, $previous_version = '')
{
    //require_once($unzip_dir . '/manifest.php');
    global $installdefs;
    if (!empty($previous_version)) {
        //check if the upgrade path exists
        if (!empty($upgrade_manifest)) {
            if (!empty($upgrade_manifest['upgrade_paths'])) {
                if (!empty($upgrade_manifest['upgrade_paths'][$previous_version])) {
                    $installdefs = 	$upgrade_manifest['upgrade_paths'][$previous_version];
                }
            }//fi
        }//fi
    }//fi
    $modified_files = array();
    if (!empty($installdefs['copy'])) {
        foreach ($installdefs['copy'] as $cp) {
            $cp['to'] = clean_path(str_replace('<basepath>', $unzip_dir, (string) $cp['to']));
            $restore_path = remove_file_extension(urldecode($install_file))."-restore/";
            $backup_path = clean_path($restore_path.$cp['to']);
            //check if this file exists in the -restore directory
            if (file_exists($backup_path)) {
                //since the file exists, then we want do an md5 of the install version and the file system version
                $from = $backup_path;
                $needle = $restore_path;
                if (!$is_install) {
                    $from = str_replace('<basepath>', $unzip_dir, (string) $cp['from']);
                    $needle = $unzip_dir;
                }
                $files_found = md5DirCompare($from.'/', $cp['to'].'/', array('.svn'), false);
                if (is_countable($files_found > 0) ? count($files_found > 0) : 0) {
                    foreach ($files_found as $key=>$value) {
                        $modified_files[] = str_replace($needle, '', (string) $key);
                    }
                }
            }//fi
        }//rof
    }//fi
    return $modified_files;
}
