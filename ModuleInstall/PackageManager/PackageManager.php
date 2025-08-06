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


define("CREDENTIAL_CATEGORY", "ml");
define("CREDENTIAL_USERNAME", "username");
define("CREDENTIAL_PASSWORD", "password");

require_once('include/nusoap/nusoap.php'); // TODO: is it needed?
require_once('include/utils/php_zip_utils.php');
require_once('ModuleInstall/PackageManager/PackageManagerDisplay.php');
require_once('ModuleInstall/ModuleInstaller.php');
require_once('include/entryPoint.php');
require_once('ModuleInstall/PackageManager/PackageManagerComm.php');

#[\AllowDynamicProperties]
class PackageManager
{
    public $soap_client;

    /**
     * Constructor: In this method we will initialize the nusoap client to point to the hearbeat server
     */
    public function __construct()
    {
        $this->db = DBManagerFactory::getInstance();
        $this->upload_dir = empty($GLOBALS['sugar_config']['upload_dir']) ? 'upload' : rtrim($GLOBALS['sugar_config']['upload_dir'], '/\\');
    }




    public function initializeComm()
    {
    }

    /**
     * Obtain a promotion from SugarDepot
     * @return string   the string from the promotion
     */
    public static function getPromotion()
    {
        $name_value_list = PackageManagerComm::getPromotion();
        if (!empty($name_value_list)) {
            $name_value_list = PackageManager::fromNameValueList($name_value_list);
            return $name_value_list['description'];
        } else {
            return '';
        }
    }

    /**
     * Obtain a list of category/packages/releases for use within the module loader
     */
    public static function getModuleLoaderCategoryPackages($category_id = '')
    {
        $filter = array();
        $filter = array('type' => "'module', 'theme', 'langpack'");
        $filter = PackageManager::toNameValueList($filter);
        return PackageManager::getCategoryPackages($category_id, $filter);
    }

    /**
     * Obtain the list of category_packages from SugarDepot
     * @return category_packages
     */
    public static function getCategoryPackages($category_id = '', $filter = array())
    {
        $results = PackageManagerComm::getCategoryPackages($category_id, $filter);
        PackageManagerComm::errorCheck();
        $nodes = array();

        $nodes[$category_id]['packages'] = array();
        if (!empty($results['categories'])) {
            foreach ($results['categories'] as $category) {
                $mycat = self::fromNameValueList($category);
                $nodes[$mycat['id']] = array('id' => $mycat['id'], 'label' => $mycat['name'], 'description' => $mycat['description'], 'type' => 'cat', 'parent' => $mycat['parent_id']);
                $nodes[$mycat['id']]['packages'] = array();
            }
        }
        if (!empty($results['packages'])) {
            $uh = new UpgradeHistory();
            foreach ($results['packages'] as $package) {
                $mypack = self::fromNameValueList($package);
                $nodes[$mypack['category_id']]['packages'][$mypack['id']] = array('id' => $mypack['id'], 'label' => $mypack['name'], 'description' => $mypack['description'], 'category_id' => $mypack['category_id'], 'type' => 'package');
                $releases = self::getReleases($category_id, $mypack['id'], $filter);
                $arr_releases = array();
                $nodes[$mypack['category_id']]['packages'][$mypack['id']]['releases'] = array();
                if (!empty($releases['packages'])) {
                    foreach ($releases['packages'] as $release) {
                        $myrelease = self::fromNameValueList($release);
                        //check to see if we already this one installed
                        $result = $uh->determineIfUpgrade($myrelease['id_name'], $myrelease['version']);
                        $enable = false;
                        if ($result == true || is_array($result)) {
                            $enable = true;
                        }
                        $nodes[$mypack['category_id']]['packages'][$mypack['id']]['releases'][$myrelease['id']] = array('id' => $myrelease['id'], 'version' => $myrelease['version'], 'label' => $myrelease['description'], 'category_id' => $mypack['category_id'], 'package_id' => $mypack['id'], 'type' => 'release', 'enable' => $enable);
                    }
                }
                //array_push($nodes[$mypack['category_id']]['packages'], $package_arr);
            }
        }
        LoggerManager::getLogger()->debug("NODES". var_export($nodes, true));
        return $nodes;
    }

    /**
     * Get a list of categories from the SugarDepot
     * @param category_id   the category id of parent to obtain
     * @param filter        an array of filters to pass to limit the query
     * @return array        an array of categories for display on the client
     */
    public static function getCategories($category_id, $filter = array())
    {
        $nodes = array();
        $results = PackageManagerComm::getCategories($category_id, $filter);
        PackageManagerComm::errorCheck();
        if (!empty($results['categories'])) {
            foreach ($results['categories'] as $category) {
                $mycat = self::fromNameValueList($category);
                $nodes[] = array('id' => $mycat['id'], 'label' => $mycat['name'], 'description' => $mycat['description'], 'type' => 'cat', 'parent' => $mycat['parent_id']);
            }
        }
        return $nodes;
    }

    public static function getPackages($category_id, $filter = array())
    {
        $nodes = array();
        $results = PackageManagerComm::getPackages($category_id, $filter);
        PackageManagerComm::errorCheck();
        $packages = array();
        //$xml = '';
        //$xml .= '<packages>';
        if (!empty($results['packages'])) {
            foreach ($results['packages'] as $package) {
                $mypack = self::fromNameValueList($package);
                $packages[$mypack['id']] = array('package_id' => $mypack['id'], 'name' => $mypack['name'], 'description' => $mypack['description'], 'category_id' => $mypack['category_id']);
                $releases = self::getReleases($category_id, $mypack['id']);
                $arr_releases = array();
                foreach ($releases['packages'] as $release) {
                    $myrelease = self::fromNameValueList($release);
                    $arr_releases[$myrelease['id']]  = array('release_id' => $myrelease['id'], 'version' => $myrelease['version'], 'description' => $myrelease['description'], 'category_id' => $mypack['category_id'], 'package_id' => $mypack['id']);
                }
                $packages[$mypack['id']]['releases'] = $arr_releases;
            }
        }
        return $packages;
    }

    public static function getReleases($category_id, $package_id, $filter = array())
    {
        $releases = PackageManagerComm::getReleases($category_id, $package_id, $filter);
        PackageManagerComm::errorCheck();
        return $releases;
    }

    /**
     * Retrieve the package as specified by the $id from the heartbeat server
     *
     * @param category_id   the category_id to which the release belongs
     * @param package_id    the package_id to which the release belongs
     * @param release_id    the release_id to download
     * @return filename - the path to which the zip file was saved
     */
    public function download($category_id, $package_id, $release_id)
    {
        LoggerManager::getLogger()->debug('RELEASE _ID: '.$release_id);
        if (!empty($release_id)) {
            $filename = PackageManagerComm::addDownload($category_id, $package_id, $release_id);
            if ($filename) {
                LoggerManager::getLogger()->debug('RESULT: '.$filename);
                PackageManagerComm::errorCheck();
                $filepath = PackageManagerComm::performDownload($filename);
                return $filepath;
            }
        } else {
            return null;
        }
    }

    /**
     * Given the Mambo username, password, and download key attempt to authenticate, if
     * successful then store these credentials
     *
     * @param username      Mambo username
     * @param password      Mambo password
     * @param systemname   the user's download key
     * @return              true if successful, false otherwise
     */
    public static function authenticate($username, $password, $systemname='', $terms_checked = true)
    {
        self::setCredentials($username, $password, $systemname);
        PackageManagerComm::clearSession();
        $result = PackageManagerComm::login($terms_checked);
        if (is_array($result)) {
            return $result;
        } else {
            return true;
        }
    }

    public static function setCredentials($username, $password, $systemname)
    {
        $admin = BeanFactory::newBean('Administration');
        $admin->retrieveSettings();
        $admin->saveSetting(CREDENTIAL_CATEGORY, CREDENTIAL_USERNAME, $username);
        $admin->saveSetting(CREDENTIAL_CATEGORY, CREDENTIAL_PASSWORD, $password);
        if (!empty($systemname)) {
            $admin->saveSetting('system', 'name', $systemname);
        }
    }

    public static function getCredentials()
    {
        $admin = BeanFactory::newBean('Administration');
        $admin->retrieveSettings(CREDENTIAL_CATEGORY, true);
        $credentials = array();
        $credentials['username'] = '';
        $credentials['password'] = '';
        $credentials['system_name'] = '';
        if (!empty($admin->settings[CREDENTIAL_CATEGORY.'_'.CREDENTIAL_USERNAME])) {
            $credentials['username'] = $admin->settings[CREDENTIAL_CATEGORY.'_'.CREDENTIAL_USERNAME];
        }
        if (!empty($admin->settings[CREDENTIAL_CATEGORY.'_'.CREDENTIAL_USERNAME])) {
            $credentials['password'] = $admin->settings[CREDENTIAL_CATEGORY.'_'.CREDENTIAL_PASSWORD];
        }
        if (!empty($admin->settings['system_name'])) {
            $credentials['system_name'] = $admin->settings['system_name'];
        }
        return $credentials;
    }

    public static function getTermsAndConditions()
    {
        return PackageManagerComm::getTermsAndConditions();
    }

    /**
     * Retrieve documentation for the given release or package
     *
     * @param package_id	the specified package to retrieve documentation
     * @param release_id	the specified release to retrieve documentation
     *
     * @return documents
     */
    public static function getDocumentation($package_id, $release_id)
    {
        if (!empty($release_id) || !empty($package_id)) {
            $documents = PackageManagerComm::getDocumentation($package_id, $release_id);
            return $documents;
        } else {
            return null;
        }
    }

    /**
     * Grab the list of installed modules and send that list to the depot.
     * The depot will then send back a list of modules that need to be updated
     */
    public function checkForUpdates()
    {
        $lists = $this->buildInstalledReleases(array('module'), true);
        $updates = array();
        if (!empty($lists)) {
            $updates = PackageManagerComm::checkForUpdates($lists);
        }//fi
        return $updates;
    }

    ////////////////////////////////////////////////////////
    /////////// HELPER FUNCTIONS
    public static function toNameValueList($array)
    {
        $list = array();
        foreach ($array as $name=>$value) {
            $list[] = array('name'=>$name, 'value'=>$value);
        }
        return $list;
    }

    public function toNameValueLists($arrays)
    {
        $lists = array();
        foreach ($arrays as $array) {
            $lists[] = self::toNameValueList($array);
        }
        return $lists;
    }

    public static function fromNameValueList($nvl)
    {
        $array = array();
        foreach ($nvl as $list) {
            $array[$list['name']] = $list['value'];
        }
        return $array;
    }

    public function buildInstalledReleases($types = array('module'))
    {
        //1) get list of installed modules
        $installeds = $this->getInstalled($types);
        $releases = array();
        foreach ($installeds as $installed) {
            $releases[] = array('name' => $installed->name, 'id_name' => $installed->id_name, 'version' => $installed->version, 'filename' => $installed->filename, 'type' => $installed->type);
        }

        $lists = array();
        $name_value_list = array();
        if (!empty($releases)) {
            $lists = $this->toNameValueLists($releases);
        }//fi
        return $lists;
    }

    public function buildPackageXML($package, $releases = array())
    {
        $xml = '<package>';
        $xml .= '<package_id>'.$package['id'].'</package_id>';
        $xml .= '<name>'.$package['name'].'</name>';
        $xml .= '<description>'.$package['description'].'</description>';
        if (!empty($releases)) {
            $xml .= '<releases>';
            foreach ($releases['packages'] as $release) {
                $myrelease = self::fromNameValueList($release);
                $xml .= '<release>';
                $xml .= '<release_id>'.$myrelease['id'].'</release_id>';
                $xml .= '<version>'.$myrelease['version'].'</version>';
                $xml .= '<description>'.$myrelease['description'].'</description>';
                $xml .= '<package_id>'.$package['id'].'</package_id>';
                $xml .= '<category_id>'.$package['category_id'].'</category_id>';
                $xml .= '</release>';
            }
            $xml .= '</releases>';
        }
        $xml .= '</package>';
        return $xml;
    }

    private $cleanUpDirs = array();

    private function addToCleanup($dir)
    {
        if (empty($this->cleanUpDirs)) {
            register_shutdown_function(array($this, "cleanUpTempDir"));
        }
        $this->cleanUpDirs[] = $dir;
    }

    public function cleanUpTempDir()
    {
        foreach ($this->cleanUpDirs as $dir) {
            rmdir_recursive($dir);
        }
    }

    //////////////////////////////////////////////////////////////////////
    /////////// INSTALL SECTION
    public function extractFile($zip_file, $file_in_zip, $base_tmp_upgrade_dir)
    {
        $my_zip_dir = mk_temp_dir($base_tmp_upgrade_dir);
        $this->addToCleanup($my_zip_dir);
        unzip_file($zip_file, $file_in_zip, $my_zip_dir);
        return("$my_zip_dir/$file_in_zip");
    }

    public function extractManifest($zip_file, $base_tmp_upgrade_dir)
    {
        global $sugar_config;
        $base_upgrade_dir       = $this->upload_dir."/upgrades";
        $base_tmp_upgrade_dir   = "$base_upgrade_dir/temp";
        return $this->extractFile($zip_file, "manifest.php", $base_tmp_upgrade_dir);
    }

    /**
     * @param string $type
     * @return bool
     */
    private function validateManifestType($type)
    {
        global $mod_strings;

        if (!isset($type)) {
            echo($mod_strings['ERROR_MANIFEST_TYPE']);

            return false;
        }
        LoggerManager::getLogger()->debug('Getting InstallType');
        if (empty($this->getInstallType("/$type/"))) {
            LoggerManager::getLogger()->debug('Error with InstallType' . $type);
            echo($mod_strings['ERROR_PACKAGE_TYPE'] . ": '" . $type . "'.");

            return false;
        }
        LoggerManager::getLogger()->debug('Passed with InstallType');

        return true;
    }

    /**
     * @param $versions
     * @param $key
     * @return bool
     */
    private function validateManifestVersion($versions, $key)
    {
        global $mod_strings, $sugar_version, $suitecrm_version;

        $checkedVersion = $suitecrm_version;
        if ($key === 'acceptable_sugar_versions') {
            $checkedVersion = $sugar_version;
        }

        if (!empty($versions)) {
            LoggerManager::getLogger()->debug("Getting $key");
            $matchesEmpty = true;
            if (isset($versions['exact_matches'])) {
                $matchesEmpty = false;
                foreach ($versions['exact_matches'] as $match) {
                    if ($match == $checkedVersion) {
                        LoggerManager::getLogger()->debug("Passed $key");

                        return true;
                    }
                }
            }
            if (isset($versions['regex_matches'])) {
                $matchesEmpty = false;
                foreach ($versions['regex_matches'] as $match) {
                    if (preg_match("/$match/", (string) $checkedVersion)) {
                        LoggerManager::getLogger()->debug("Passed $key");

                        return true;
                    }
                }
            }

            if (!$matchesEmpty) {
                LoggerManager::getLogger()->error("Error with $key");
                echo($mod_strings['ERROR_VERSION_INCOMPATIBLE'] . $suitecrm_version);

                return false;
            }
        }

        return true;
    }

    /**
     * @param array $manifest
     */
    public function validate_manifest($manifest)
    {
        if (!$this->validateManifestType($manifest['type'])) {
            exit();
        }

        $versionSugarOk = $this->validateManifestVersion($manifest['acceptable_sugar_versions'] ?? '', 'acceptable_sugar_versions');
        $versionSuiteOk = $this->validateManifestVersion($manifest['acceptable_suitecrm_versions'] ?? '', 'acceptable_suitecrm_versions');

        if (!$versionSugarOk || !$versionSuiteOk) {
            exit();
        }
    }

    public function getInstallType($type_string)
    {
        // detect file type
        global $subdirs;
        $subdirs = array('full', 'langpack', 'module', 'patch', 'theme', 'temp');


        foreach ($subdirs as $subdir) {
            if (preg_match("#/$subdir/#", (string) $type_string)) {
                return($subdir);
            }
        }
        // return empty if no match
        return("");
    }

    public function performSetup($tempFile, $view = 'module', $display_messages = true)
    {
        global $sugar_config,$mod_strings;
        $base_filename = urldecode($tempFile);
        LoggerManager::getLogger()->debug("BaseFileName: ".$base_filename);
        $base_upgrade_dir       = $this->upload_dir.'/upgrades';
        $base_tmp_upgrade_dir   = "$base_upgrade_dir/temp";
        $manifest_file = $this->extractManifest($base_filename, $base_tmp_upgrade_dir);
        LoggerManager::getLogger()->debug("Manifest: ".$manifest_file);
        if ($view == 'module') {
            $license_file = $this->extractFile($base_filename, 'LICENSE.txt', $base_tmp_upgrade_dir);
        }
        $manifest = [];
        if (is_file($manifest_file)) {
            LoggerManager::getLogger()->debug("VALIDATING MANIFEST". $manifest_file);
            require_once($manifest_file);
            $this->validate_manifest($manifest);
            $upgrade_zip_type = $manifest['type'];
            LoggerManager::getLogger()->debug("VALIDATED MANIFEST");
            // exclude the bad permutations
            if ($view == "module") {
                if ($upgrade_zip_type != "module" && $upgrade_zip_type != "theme" && $upgrade_zip_type != "langpack") {
                    $this->unlinkTempFiles();
                    if ($display_messages) {
                        die($mod_strings['ERR_UW_NOT_ACCEPTIBLE_TYPE']);
                    }
                }
            } elseif ($view == "default") {
                if ($upgrade_zip_type != "patch") {
                    $this->unlinkTempFiles();
                    if ($display_messages) {
                        die($mod_strings['ERR_UW_ONLY_PATCHES']);
                    }
                }
            }

            $base_filename = preg_replace("#\\\\#", "/", $base_filename);
            $base_filename = basename($base_filename);
            mkdir_recursive("$base_upgrade_dir/$upgrade_zip_type");
            $target_path = "$base_upgrade_dir/$upgrade_zip_type/$base_filename";
            $target_manifest = remove_file_extension($target_path) . "-manifest.php";

            if (isset($manifest['icon']) && $manifest['icon'] != "") {
                $icon_location = $this->extractFile($tempFile, $manifest['icon'], $base_tmp_upgrade_dir);
                $path_parts = pathinfo((string) $icon_location);
                copy($icon_location, remove_file_extension($target_path) . "-icon." . $path_parts['extension']);
            }

            if (copy($tempFile, $target_path)) {
                copy($manifest_file, $target_manifest);
                if ($display_messages) {
                    $messages = '<script>ajaxStatus.flashStatus("' .$base_filename.$mod_strings['LBL_UW_UPLOAD_SUCCESS'] . ', 5000");</script>';
                }
            } else {
                if ($display_messages) {
                    $messages = '<script>ajaxStatus.flashStatus("' .$mod_strings['ERR_UW_UPLOAD_ERROR'] . ', 5000");</script>';
                }
            }
        }//fi
        else {
            $this->unlinkTempFiles();
            if ($display_messages) {
                die($mod_strings['ERR_UW_NO_MANIFEST']);
            }
        }
        if (isset($messages)) {
            return $messages;
        }
    }

    public function unlinkTempFiles()
    {
        global $sugar_config;
        @unlink($_FILES['upgrade_zip']['tmp_name']);
        @unlink("upload://".$_FILES['upgrade_zip']['name']);
    }

    public function performInstall($file, $silent=true)
    {
        global $sugar_config;
        global $mod_strings;
        global $current_language;
        $base_upgrade_dir       = $this->upload_dir.'/upgrades';
        $base_tmp_upgrade_dir   = "$base_upgrade_dir/temp";
        if (!file_exists($base_tmp_upgrade_dir)) {
            mkdir_recursive($base_tmp_upgrade_dir, true);
        }

        LoggerManager::getLogger()->debug("INSTALLING: ".$file);
        $mi = new ModuleInstaller();
        $mi->silent = $silent;
        $mod_strings = return_module_language($current_language, "Administration");
        LoggerManager::getLogger()->debug("ABOUT TO INSTALL: ".$file);
        if (preg_match("#.*\.zip\$#", (string) $file)) {
            LoggerManager::getLogger()->debug("1: ".$file);
            // handle manifest.php
            $target_manifest = remove_file_extension($file) . '-manifest.php';
            $manifest = [];
            $installdefs = [];
            include($target_manifest);
            LoggerManager::getLogger()->debug("2: ".$file);
            $unzip_dir = mk_temp_dir($base_tmp_upgrade_dir);
            $this->addToCleanup($unzip_dir);
            unzip($file, $unzip_dir);
            LoggerManager::getLogger()->debug("3: ".$unzip_dir);
            $id_name = $installdefs['id'];
            $version = $manifest['version'];
            $uh = new UpgradeHistory();
            $previous_install = array();
            if (!empty($id_name) & !empty($version)) {
                $previous_install = $uh->determineIfUpgrade($id_name, $version);
            }
            $previous_version = (empty($previous_install['version'])) ? '' : $previous_install['version'];
            $previous_id = (empty($previous_install['id'])) ? '' : $previous_install['id'];

            if (!empty($previous_version)) {
                $mi->install($unzip_dir, true, $previous_version);
            } else {
                $mi->install($unzip_dir);
            }
            LoggerManager::getLogger()->debug("INSTALLED: ".$file);
            $new_upgrade = new UpgradeHistory();
            $new_upgrade->filename      = $file;
            $new_upgrade->md5sum        = md5_file($file);
            $new_upgrade->type          = $manifest['type'];
            $new_upgrade->version       = $manifest['version'];
            $new_upgrade->status        = "installed";
            //$new_upgrade->author        = $manifest['author'];
            $new_upgrade->name          = $manifest['name'];
            $new_upgrade->description   = $manifest['description'];
            $new_upgrade->id_name		= $id_name;
            $serial_manifest = array();
            $serial_manifest['manifest'] = (isset($manifest) ? $manifest : '');
            $serial_manifest['installdefs'] = (isset($installdefs) ? $installdefs : '');
            $serial_manifest['upgrade_manifest'] = (isset($upgrade_manifest) ? $upgrade_manifest : '');
            $new_upgrade->manifest		= base64_encode(serialize($serial_manifest));
            //$new_upgrade->unique_key    = (isset($manifest['unique_key'])) ? $manifest['unique_key'] : '';
            $new_upgrade->save();
            //unlink($file);
        }//fi
    }

    public function performUninstall($name)
    {
        $uh = new UpgradeHistory();
        $uh->name = $name;
        $uh->id_name = $name;
        $found = $uh->checkForExisting($uh);
        if ($found != null) {
            global $sugar_config;
            global $mod_strings;
            global $current_language;
            $base_upgrade_dir       = $this->upload_dir.'/upgrades';
            $base_tmp_upgrade_dir   = "$base_upgrade_dir/temp";
            if (is_file($found->filename)) {
                if (!isset($GLOBALS['mi_remove_tables'])) {
                    $GLOBALS['mi_remove_tables'] = true;
                }
                $unzip_dir = mk_temp_dir($base_tmp_upgrade_dir);
                unzip($found->filename, $unzip_dir);
                $mi = new ModuleInstaller();
                $mi->silent = true;
                $mi->uninstall((string)$unzip_dir);
                $found->delete();
                unlink(remove_file_extension($found->filename) . '-manifest.php');
                unlink($found->filename);
            } else {
                //file(s_ have been deleted or are not found in the directory, allow database delete to happen but no need to change filesystem
                $found->delete();
            }
        }
    }

    public function getUITextForType($type)
    {
        if ($type == "full") {
            return("Full Upgrade");
        }
        if ($type == "langpack") {
            return("Language Pack");
        }
        if ($type == "module") {
            return("Module");
        }
        if ($type == "patch") {
            return("Patch");
        }
        if ($type == "theme") {
            return("Theme");
        }
    }

    public function getImageForType($type)
    {
        $icon = "";
        switch ($type) {
            case "full":
                $icon = SugarThemeRegistry::current()->getImage("Upgrade", "", null, null, '.gif', "Upgrade");

                break;
            case "langpack":
                $icon = SugarThemeRegistry::current()->getImage("LanguagePacks", "", null, null, '.gif', "Language Packs");

                break;
            case "module":
                $icon = SugarThemeRegistry::current()->getImage("ModuleLoader", "", null, null, '.gif', "Module Loader");

                break;
            case "patch":
                $icon = SugarThemeRegistry::current()->getImage("PatchUpgrades", "", null, null, '.gif', "Patch Upgrades");

                break;
            case "theme":
                $icon = SugarThemeRegistry::current()->getImage("Themes", "", null, null, '.gif', "Themes");

                break;
            default:
                break;
        }
        return($icon);
    }

    public function getPackagesInStaging($view = 'module')
    {
        global $sugar_config;
        global $current_language;
        $uh = new UpgradeHistory();
        $base_upgrade_dir       = "upload://upgrades";
        $uContent = findAllFiles($base_upgrade_dir, array(), false, 'zip');
        $upgrade_contents = array();
        $content_values = array_values($uContent);
        $alreadyProcessed = array();
        foreach ($content_values as $val) {
            if (empty($alreadyProcessed[$val])) {
                $upgrade_contents[] = $val;
                $alreadyProcessed[$val] = true;
            }
        }

        $upgrades_available = 0;
        $packages = array();
        $mod_strings = return_module_language($current_language, "Administration");
        foreach ($upgrade_contents as $upgrade_content) {
            if (!preg_match('#.*\.zip$#', strtolower($upgrade_content)) || preg_match("#.*./zips/.*#", strtolower($upgrade_content))) {
                continue;
            }

            $the_base = basename((string) $upgrade_content);
            $the_md5 = md5_file($upgrade_content);
            $md5_matches = $uh->findByMd5($the_md5);
            $file_install = $upgrade_content;
            if (empty($md5_matches)) {
                $target_manifest = remove_file_extension($upgrade_content) . '-manifest.php';
                if (file_exists($target_manifest)) {
                    require_once($target_manifest);

                    $name = empty($manifest['name']) ? $upgrade_content : $manifest['name'];
                    $version = empty($manifest['version']) ? '' : $manifest['version'];
                    $published_date = empty($manifest['published_date']) ? '' : $manifest['published_date'];
                    $icon = '';
                    $description = empty($manifest['description']) ? 'None' : $manifest['description'];
                    $uninstallable = empty($manifest['is_uninstallable']) ? 'No' : 'Yes';
                    $type = $this->getUITextForType($manifest['type']);
                    $manifest_type = $manifest['type'];
                    $dependencies = array();
                    if (isset($manifest['dependencies'])) {
                        $dependencies    = $manifest['dependencies'];
                    }
                }

                //check dependencies first
                if (!empty($dependencies)) {
                    $uh = new UpgradeHistory();
                    $not_found = $uh->checkDependencies($dependencies);
                    if (!empty($not_found) && (is_countable($not_found) ? count($not_found) : 0) > 0) {
                        $file_install = 'errors_'.$mod_strings['ERR_UW_NO_DEPENDENCY']."[".implode(',', $not_found)."]";
                    }
                }

                if ($view == 'default' && $manifest_type != 'patch') {
                    continue;
                }

                if ($view == 'module'
                    && $manifest_type != 'module' && $manifest_type != 'theme' && $manifest_type != 'langpack') {
                    continue;
                }

                if (empty($manifest['icon'])) {
                    $icon = $this->getImageForType($manifest['type']);
                } else {
                    $path_parts = pathinfo((string) $manifest['icon']);
                    $icon = "<img src=\"" . remove_file_extension($upgrade_content) . "-icon." . $path_parts['extension'] . "\">";
                }

                $upgrades_available++;

                $packages[] = array('name' => $name, 'version' => $version, 'published_date' => $published_date,
                    'description' => $description, 'uninstallable' =>$uninstallable, 'type' => $type,
                    'file' => fileToHash($upgrade_content), 'file_install' => fileToHash($upgrade_content), 'unFile' => fileToHash($upgrade_content));
            }//fi
        }//rof
        return $packages;
    }

    public function getLicenseFromFile($file)
    {
        global $sugar_config;
        $base_upgrade_dir       = $this->upload_dir.'/upgrades';
        $base_tmp_upgrade_dir   = "$base_upgrade_dir/temp";
        $license_file = $this->extractFile($file, 'LICENSE.txt', $base_tmp_upgrade_dir);
        if (is_file($license_file)) {
            $contents = file_get_contents($license_file);
            return $contents;
        } else {
            return null;
        }
    }

    /**
     * Run the query to obtain the list of installed types as specified by the type param
     *
     * @param type	an array of types you would like to search for
     * 				type options include (theme, langpack, module, patch)
     *
     * @return an array of installed upgrade_history objects
     */
    public function getInstalled($types = array('module'))
    {
        $uh = new UpgradeHistory();
        $in = "";
        $typesCount = count($types);
        for ($i = 0; $i < $typesCount; $i++) {
            $in .= "'".$types[$i]."'";
            if (($i+1) < (is_countable($types) ? count($types) : 0)) {
                $in .= ",";
            }
        }
        $query = "SELECT * FROM ".$uh->table_name."	 WHERE type IN (".$in.")";
        return $uh->getList($query);
    }

    public function getinstalledPackages($types = array('module', 'langpack'))
    {
        global $sugar_config;
        $installeds = $this->getInstalled($types);
        $packages = array();
        $upgrades_installed = 0;
        $uh = new UpgradeHistory();
        $base_upgrade_dir       = $this->upload_dir.'/upgrades';
        $base_tmp_upgrade_dir   = "$base_upgrade_dir/temp";
        foreach ($installeds as $installed) {
            $populate = false;
            $filename = from_html($installed->filename);
            $date_entered = $installed->date_entered;
            $type = $installed->type;
            $version = $installed->version;
            $uninstallable = false;
            $link = "";
            $description = $installed->description;
            $name = $installed->name;
            $enabled = true;
            $enabled_string = 'ENABLED';
            //if the name is empty then we should try to pull from manifest and populate upgrade_history_table
            if (empty($name)) {
                $populate = true;
            }
            $upgrades_installed++;
            switch ($type) {
                case "theme":
                case "langpack":
                case "module":
                case "patch":
                    if ($populate) {
                        $manifest_file = $this->extractManifest($filename, $base_tmp_upgrade_dir);
                        require_once($manifest_file);
                        LoggerManager::getLogger()->info("Filling in upgrade_history table");
                        $populate = false;
                        if (isset($manifest['name'])) {
                            $name = $manifest['name'];
                            $installed->name = $name;
                        }
                        if (isset($manifest['description'])) {
                            $description = $manifest['description'];
                            $installed->description = $description;
                        }
                        if (isset($installdefs) && isset($installdefs['id'])) {
                            $id_name  = $installdefs['id'];
                            $installed->id_name = $id_name;
                        }

                        $serial_manifest = array();
                        $serial_manifest['manifest'] = (isset($manifest) ? $manifest : '');
                        $serial_manifest['installdefs'] = (isset($installdefs) ? $installdefs : '');
                        $serial_manifest['upgrade_manifest'] = (isset($upgrade_manifest) ? $upgrade_manifest : '');
                        $installed->manifest = base64_encode(serialize($serial_manifest));
                        $installed->save();
                    } else {
                        $serial_manifest = unserialize(base64_decode($installed->manifest), ['allowed_classes' => false]);
                        $manifest = $serial_manifest['manifest'];
                    }
                    if (($upgrades_installed==0 || $uh->UninstallAvailable($installeds, $installed))
                        && is_file($filename) && !empty($manifest['is_uninstallable'])) {
                        $uninstallable = true;
                    }
                    $enabled = $installed->enabled;
                    if (!$enabled) {
                        $enabled_string = 'DISABLED';
                    }
                    $file_uninstall = $filename;
                    if (!$uninstallable) {
                        $file_uninstall = 'UNINSTALLABLE';
                        $enabled_string = 'UNINSTALLABLE';
                    } else {
                        $file_uninstall = fileToHash($file_uninstall);
                    }

                $packages[] = array(
                    'name' => $name,
                    'version' => $version,
                    'type' => $type,
                    'published_date' => $date_entered,
                    'description' => $description,
                    'uninstallable' =>$uninstallable,
                    'file_install' =>  $file_uninstall ,
                    'file' =>  fileToHash($filename),
                    'enabled' => $enabled_string
                );
                break;
                default:
                break;
            }
        }//rof
        return $packages;
    }
}
