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

 require_once('ModuleInstall/PackageManager/PackageManagerDisplay.php');
 require_once('ModuleInstall/PackageManager/PackageManager.php');
 #[\AllowDynamicProperties]
 class PackageController
 {
     public $_pm;

     /**
      * Constructor: this class is called from the ajax call and handles invoking the correct
      * functionality on the server.
      */
     public function __construct()
     {
         $this->_pm = new PackageManager();
     }

     public function performBasicSearch()
     {
         $json = getJSONobj();
         $search_term = '';
         $node_id = '';
         if (isset($_REQUEST['search_term'])) {
             $search_term = nl2br($_REQUEST['search_term']);
         }
         if (isset($_REQUEST['node_id'])) {
             $node_id = nl2br($_REQUEST['node_id']);
         }
         $xml = PackageManager::getPackages($node_id);
         echo 'result = ' . $json->encode(array('packages' => $xml));
     }

     /**
      * Retrieve a list of packages which belong to the corresponding category
      *
      * @param category_id   this is passed via POST and is the category id of packages
      *                      we wish to retrieve
      * @return packages     xml string consisting of the packages and releases which belong to
      *                      the category
      */
     public function getPackages()
     {
         $json = getJSONobj();
         $category_id = '';

         if (isset($_REQUEST['category_id'])) {
             $category_id = nl2br($_REQUEST['category_id']);
         }
         $xml = PackageManager::getPackages($category_id);
         echo 'result = ' . $json->encode(array('package_output' => $xml));
     }

     /**
      * Obtain a list of releases from the server.  This function is currently used for generating the patches/langpacks for upgrade wizard
      * as well as during installation
      */
     public function getReleases()
     {
         $json = getJSONobj();
         $category_id = '';
         $package_id = '';
         $types = '';
         if (isset($_REQUEST['category_id'])) {
             $category_id = nl2br($_REQUEST['category_id']);
         }
         if (isset($_REQUEST['package_id'])) {
             $package_id = nl2br($_REQUEST['package_id']);
         }
         if (isset($_REQUEST['types'])) {
             $types = nl2br($_REQUEST['types']);
         }
         $types = explode(',', $types);

         $filter = array();
         $count = count($types);
         $index = 1;
         $type_str = '';
         foreach ($types as $type) {
             $type_str .= "'".$type."'";
             if ($index < $count) {
                 $type_str .= ",";
             }
             $index++;
         }

         $filter = array('type' => $type_str);
         $filter = PackageManager::toNameValueList($filter);
         $releases = PackageManager::getReleases($category_id, $package_id, $filter);
         $nodes = array();
         $release_map = array();
         foreach ($releases['packages'] as $release) {
             $release = PackageManager::fromNameValueList($release);
             $nodes[] = array('description' => $release['description'], 'version' => $release['version'], 'build_number' => $release['build_number'], 'id' => $release['id']);
             $release_map[$release['id']] = array('package_id' => $release['package_id'], 'category_id' => $release['category_id']);
         }
         $_SESSION['ML_PATCHES'] = $release_map;
         echo 'result = ' . $json->encode(array('releases' => $nodes));
     }

     /**
      * Obtain a promotion from the depot
      */
     public function getPromotion()
     {
         $json = getJSONobj();

         $header = PackageManager::getPromotion();

         echo 'result = ' . $json->encode(array('promotion' => $header));
     }

     /**
      * Download the given release
      *
      * @param category_id   this is passed via POST and is the category id of the release we wish to download
      * @param package_id   this is passed via POST and is the package id of the release we wish to download
      * @param release_id   this is passed via POST and is the release id of the release we wish to download
      * @return bool         true is successful in downloading, false otherwise
      */
     public function download()
     {
         global $sugar_config;
         $json = getJSONobj();
         $package_id = '';
         $category_id  = '';
         $release_id = '';
         if (isset($_REQUEST['package_id'])) {
             $package_id = nl2br($_REQUEST['package_id']);
         }
         if (isset($_REQUEST['category_id'])) {
             $category_id = nl2br($_REQUEST['category_id']);
         }
         if (isset($_REQUEST['release_id'])) {
             $release_id = nl2br($_REQUEST['release_id']);
         }
         $GLOBALS['log']->debug("PACKAGE ID: ".$package_id);
         $GLOBALS['log']->debug("CATEGORY ID: ".$category_id);
         $GLOBALS['log']->debug("RELEASE ID: ".$release_id);
         $result = $this->_pm->download($category_id, $package_id, $release_id);
         $GLOBALS['log']->debug("RESULT: ".print_r($result, true));
         $success = 'false';
         if ($result != null) {
             $GLOBALS['log']->debug("Performing Setup");
             $this->_pm->performSetup($result, 'module', false);
             $GLOBALS['log']->debug("Complete Setup");
             $success = 'true';
         }
         echo 'result = ' . $json->encode(array('success' => $success));
     }

     /**
     * Retrieve a list of categories that are subcategories to the selected category
     *
     * @param id - the id of the parent_category, -1 if this is the root
     * @return array - a list of categories/nodes which are underneath this node
     */
     public function getCategories()
     {
         $json = getJSONobj();
         $node_id = '';
         if (isset($_REQUEST['category_id'])) {
             $node_id = nl2br($_REQUEST['category_id']);
         }
         $GLOBALS['log']->debug("NODE ID: ".$node_id);
         $nodes = PackageManager::getCategories($node_id);
         echo 'result = ' . $json->encode(array('nodes' => $nodes));
     }

     public function getNodes()
     {
         $json = getJSONobj();
         $category_id = '';
         if (isset($_REQUEST['category_id'])) {
             $category_id = nl2br($_REQUEST['category_id']);
         }
         $GLOBALS['log']->debug("CATEGORY ID: ".$category_id);
         $nodes = PackageManager::getModuleLoaderCategoryPackages($category_id);
         $GLOBALS['log']->debug(var_export($nodes, true));
         echo 'result = ' . $json->encode(array('nodes' => $nodes));
     }

     /**
      * Check the SugarDepot for updates for the given type as passed in via POST
      * @param type      the type to check for
      * @return array    return an array of releases for each given installed object if an update is found
      */
     public function checkForUpdates()
     {
         $json = getJSONobj();
         $type = '';
         if (isset($_REQUEST['type'])) {
             $type = nl2br($_REQUEST['type']);
         }
         $pm = new PackageManager();
         $updates = $pm->checkForUpdates();
         $nodes = array();
         $release_map = array();
         if (!empty($updates)) {
             foreach ($updates as $update) {
                 $update = PackageManager::fromNameValueList($update);
                 $nodes[] = array('label' => $update['name'], 'description' => $update['description'], 'version' => $update['version'], 'build_number' => $update['build_number'], 'id' => $update['id'], 'type' => $update['type']);
                 $release_map[$update['id']] = array('package_id' => $update['package_id'], 'category_id' => $update['category_id'], 'type' => $update['type']);
             }
         }
         //patches
         $filter = array(array('name' => 'type', 'value' => "'patch'"));
         $releases = PackageManager::getReleases('', '', $filter);
         if (!empty($releases['packages'])) {
             foreach ($releases['packages'] as $update) {
                 $update = PackageManager::fromNameValueList($update);
                 $nodes[] = array('label' => $update['name'], 'description' => $update['description'], 'version' => $update['version'], 'build_number' => $update['build_number'], 'id' => $update['id'], 'type' => $update['type']);
                 $release_map[$update['id']] = array('package_id' => $update['package_id'], 'category_id' => $update['category_id'], 'type' => $update['type']);
             }
         }
         $_SESSION['ML_PATCHES'] = $release_map;
         echo 'result = ' . $json->encode(array('updates' => $nodes));
     }

     public function getLicenseText()
     {
         $json = getJSONobj();
         $file = '';
         if (isset($_REQUEST['file'])) {
             $file = hashToFile($_REQUEST['file']);
         }
         $GLOBALS['log']->debug("FILE : ".$file);
         echo 'result = ' . $json->encode(array('license_display' => PackageManagerDisplay::buildLicenseOutput($file)));
     }

     /**
      *  build the list of modules that are currently in the staging area waiting to be installed
      */
     public function getPackagesInStaging()
     {
         $packages = $this->_pm->getPackagesInStaging('module');
         $json = getJSONobj();

         echo 'result = ' . $json->encode(array('packages' => $packages));
     }

     /**
      *  build the list of modules that are currently in the staging area waiting to be installed
      */
     public function performInstall()
     {
         $file = '';
         if (isset($_REQUEST['file'])) {
             $file = hashToFile($_REQUEST['file']);
         }
         if (!empty($file)) {
             $this->_pm->performInstall($file);
         }
         $json = getJSONobj();

         echo 'result = ' . $json->encode(array('result' => 'success'));
     }

     public function authenticate()
     {
         $json = getJSONobj();
         $username = '';
         $password = '';
         $servername = '';
         $terms_checked = '';
         if (isset($_REQUEST['username'])) {
             $username = nl2br($_REQUEST['username']);
         }
         if (isset($_REQUEST['password'])) {
             $password = nl2br($_REQUEST['password']);
         }
         if (isset($_REQUEST['servername'])) {
             $servername = $_REQUEST['servername'];
         }
         if (isset($_REQUEST['terms_checked'])) {
             $terms_checked = $_REQUEST['terms_checked'];
             if ($terms_checked == 'on') {
                 $terms_checked = true;
             }
         }

         if (!empty($username) && !empty($password)) {
             $password = md5($password);
             $result = PackageManager::authenticate($username, $password, $servername, $terms_checked);
             if (!is_array($result) && $result === true) {
                 $status  = 'success';
             } else {
                 $status  = $result['faultstring'];
             }
         } else {
             $status  = 'failed';
         }

         echo 'result = ' . $json->encode(array('status' => $status));
     }

     public function getDocumentation()
     {
         $json = getJSONobj();
         $package_id = '';
         $release_id = '';

         if (isset($_REQUEST['package_id'])) {
             $package_id = nl2br($_REQUEST['package_id']);
         }
         if (isset($_REQUEST['release_id'])) {
             $release_id = nl2br($_REQUEST['release_id']);
         }

         $documents = PackageManager::getDocumentation($package_id, $release_id);
         $GLOBALS['log']->debug("DOCUMENTS: ".var_export($documents, true));
         echo 'result = ' . $json->encode(array('documents' => $documents));
     }

     public function downloadedDocumentation()
     {
         $json = getJSONobj();
         $document_id = '';

         if (isset($_REQUEST['document_id'])) {
             $document_id = nl2br($_REQUEST['document_id']);
         }
         $GLOBALS['log']->debug("Downloading Document: ".$document_id);
         PackageManagerComm::downloadedDocumentation($document_id);
         echo 'result = ' . $json->encode(array('result' => 'true'));
     }

     /**
      * Remove metadata files such as foo-manifest
      * Enter description here ...
      * @param unknown_type $file
      * @param unknown_type $meta
      */
     protected function rmMetaFile($file, $meta)
     {
         $metafile = pathinfo($file, PATHINFO_DIRNAME)."/". pathinfo($file, PATHINFO_FILENAME)."-$meta.php";
         if (file_exists($metafile)) {
             unlink($metafile);
         }
     }

     public function remove()
     {
         $json = getJSONobj();
         $file = '';

         if (isset($_REQUEST['file'])) {
             $file = urldecode(hashToFile($_REQUEST['file']));
         }
         $GLOBALS['log']->debug("FILE TO REMOVE: ".$file);
         if (!empty($file)) {
             unlink($file);
             foreach (array("manifest", "icon") as $meta) {
                 $this->rmMetaFile($file, $meta);
             }
         }
         echo 'result = ' . $json->encode(array('result' => 'true'));
     }
 }
