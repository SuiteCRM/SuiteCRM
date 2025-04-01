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

require_once 'include/utils/sugar_file_utils.php';

#[\AllowDynamicProperties]
class UpgradeMetaHelper
{
    public $upgrade_dir;
    public $debug_mode;
    public $upgrade_modules;
    public $customized_modules;
    public $source_dir;
    public $dest_dir ;
    public $evparser;
    public $dvparser;
    public $path_to_master_copy;
    /**
     * UpgradeMetaHelper
     * This is the constructor for the UpgradeMetaHelper class
     * @param $dir The root upgrade directory (where to copy working files)
     * @param $masterCopyDirectory The root master directory (where SugarCRM 5.x files reside)
     * @param $debugMode Debug mode, default is false
     *
     */
    public function __construct($dir='upgrade', $masterCopyDirecotry='modules_50', $debugMode = false)
    {
        $this->upgrade_dir = $dir;
        $this->debug_mode = $debugMode;
        $this->upgrade_modules = $this->getModifiedModules();

        if ((is_countable($this->upgrade_modules) ? count($this->upgrade_modules) : 0) > 0) {
            $_SESSION['Upgraded_Modules'] = $this->upgrade_modules;
            $this->create_upgrade_directory();
            $this->path_to_master_copy = $masterCopyDirecotry;
            $this->runParser();
        }

        $this->customized_modules = $this->getAllCustomizedModulesBeyondStudio();
        if ((is_countable($this->customized_modules) ? count($this->customized_modules) : 0) > 0) {
            $_SESSION['Customized_Modules'] = $this->customized_modules;
        }
    }

    /**
     * getModifiedModules
     * This method returns a two-dimensional Array of Studio enabled
     * modules that have been modified.  The second Array index is an
     * Array of files that do not match the md5 checksum for the module
     * @return $return_array Two-dimensional Array of [module][modified file(s) Array]
     */
    public function getModifiedModules()
    {
        $md5_string = array();
        if (file_exists(clean_path(getcwd().'/files.md5'))) {
            require(clean_path(getcwd().'/files.md5'));
        }

        $return_array = array();
        $modules = $this->loadStudioModules();
        foreach ($modules as $mod) {
            $editView = "modules/$mod/EditView.html";
            $detailView = "modules/$mod/DetailView.html";
            $searchForm = "modules/$mod/SearchForm.html";
            if (file_exists($editView) && isset($md5_string['./' . $editView])) {
                $fileContents = file_get_contents($editView);
                if (md5($fileContents) != $md5_string['./' . $editView]) {
                    $return_array[$mod][] = $editView;
                }
            }

            if (file_exists($detailView) && isset($md5_string['./' . $detailView])) {
                $fileContents = file_get_contents($detailView);
                if (md5($fileContents) != $md5_string['./' . $detailView]) {
                    $return_array[$mod][] = $detailView;
                }
            }

            if (file_exists($searchForm) && isset($md5_string['./' . $searchForm])) {
                $fileContents = file_get_contents($searchForm);
                if (md5($fileContents) != $md5_string['./' . $searchForm]) {
                    $return_array[$mod][] = $searchForm;
                }
            }
        } //foreach

        return $return_array;
    }

    public function saveMatchingFilesQueries($currStep, $value)
    {
        $file_queries = [];
        $upgrade_progress_dir = sugar_cached('upgrades/temp');
        if (!is_dir($upgrade_progress_dir) && !mkdir($upgrade_progress_dir) && !is_dir($upgrade_progress_dir)) {
            throw new \RuntimeException(sprintf('Directory "%s" was not created', $upgrade_progress_dir));
        }
        $file_queries_file = $upgrade_progress_dir.'/files_queries.php';
        if (file_exists($file_queries_file)) {
            include($file_queries_file);
        } else {
            if (function_exists('sugar_fopen')) {
                sugar_fopen($file_queries_file, 'w+');
            } else {
                fopen($file_queries_file, 'wb+');
            }
        }
        if (!isset($files_queries) || $files_queries == null) {
            $files_queries = array();
        }
        $files_queries[$currStep]=$value;
        if (is_writable($file_queries_file) && write_array_to_file(
            "file_queries",
            $file_queries,
            $file_queries_file
        )) {
            //writing to the file
        }
    }

    public function getAllCustomizedModulesBeyondStudio()
    {
        require_once('modules/UpgradeWizard/uw_utils.php');
        $md5_string = array();
        if (file_exists(clean_path(getcwd().'/files.md5'))) {
            require(clean_path(getcwd().'/files.md5'));
        }
        $return_array = array();
        $modules = $this->loadStudioModules();
        $modulesAll = getAllModules(); //keep all modules as well
        $allOtherModules = array_diff($modulesAll, $modules);
        foreach ($modules as $mod) {
            if (!is_dir('modules/'.$mod)) {
                continue;
            }
            $editView = "modules/$mod/EditView.html";
            $detailView ="modules/$mod/DetailView.html";
            $exclude_files[]=$editView;
            $exclude_files[]=$detailView;
            $allModFiles = array();
            $allModFiles = findAllFiles('modules/'.$mod, $allModFiles);
            foreach ($allModFiles as $file) {
                //$file_md5_ref = str_replace(clean_path(getcwd()),'',$file);
                if (file_exists($file) && !in_array($file, $exclude_files)) {
                    if (isset($md5_string['./'.$file])) {
                        $fileContents = file_get_contents($file);
                        if (md5($fileContents) != $md5_string['./'.$file]) {
                            $return_array[$mod][] = $file;
                        } else {
                            //keep in the array to be deleted later
                            $_SESSION['removeMd5MatchingFiles'][] = $file;
                        }
                    }
                }
            }
        } //foreach
        //also check out other non-studio modules by taking the difference between
        //allMOdules and
        foreach ($allOtherModules as $mod) {
            if (!is_dir('modules/'.$mod)) {
                continue;
            }
            $allModFiles = array();
            $allModFiles = findAllFiles('modules/'.$mod, $allModFiles);
            foreach ($allModFiles as $file) {
                //$file_md5_ref = str_replace(clean_path(getcwd()),'',$file);
                if (file_exists($file)) {
                    if (isset($md5_string['./'.$file])) {
                        $fileContents = file_get_contents($file);
                        if (md5($fileContents) == $md5_string['./'.$file]) {
                            $_SESSION['removeMd5MatchingFiles'][] = $file;
                        }
                    }
                }
            }
        }
        //Also store in a file
        //saveMatchingFilesQueries('removeMd5MatchingFiles',$_SESSION['removeMd5MatchingFiles']);
        return $return_array;
    }



    /**
     * Get all the customized modules. Compare the file md5s with the base md5s
     * If a file has been modified then put the module in the list of customized
     * modules. Show the list in the preflight check UI.
     */
    public function getAllCustomizedModules()
    {
        require_once('files.md5');

        $return_array = array();
        $modules = getAllModules();
        foreach ($modules as $mod) {
            //find all files in each module if the files have been modified
            //as compared to the base version then add the module to the
            //customized modules array
            $modFiles = findAllFiles(clean_path(getcwd())."/modules/$mod", array());
            foreach ($modFiles as $file) {
                $fileContents = file_get_contents($file);
                $file = str_replace(clean_path(getcwd()), '', (string) $file);
                if ($md5_string['./' . $file]) {
                    if (md5($fileContents) != $md5_string['./' . $file]) {
                        //A file has been customized in the module. Put the module into the
                        // customized modules array.
                        echo 'Changed File'.$file;
                        $return_array[$mod];
                        break;
                    }
                } else {
                    // This is a new file in user's version and indicates that module has been
                    //customized. Put the module in the customized array.
                    echo 'New File'.$file;
                    $return_array[$mod];
                    break;
                }
            }
        } //foreach

        return $return_array;
    }

    /**
     * loadStudioModules
     * This method returns an Array of all modules where a studio.php file is
     * present in the metadata directory of the module
     * @return $modules Array of modules that are studio enabled
     */
    public function loadStudioModules()
    {
        $modules = array();
        $d = dir('modules');
        while ($e = $d->read()) {
            if (substr($e, 0, 1) == '.' || !is_dir('modules/' . $e)) {
                continue;
            }
            if (file_exists('modules/' . $e . '/metadata/studio.php')) {
                $modules[] = $e;
            }
        }
        return $modules;
    }


    /**
     * runParser
     * This method creates the EditView and DetailView parser intances
     * and runs the parsing for the modules to upgrade
     *
     */
    public function runParser()
    {
        require_once('include/SugarFields/Parsers/EditViewMetaParser.php');
        require_once('include/SugarFields/Parsers/DetailViewMetaParser.php');
        require_once('include/SugarFields/Parsers/SearchFormMetaParser.php');

        $this->evparser = new EditViewMetaParser();
        $this->dvparser = new DetailViewMetaParser();
        $this->svparser = new SearchFormMetaParser();

        foreach ($this->upgrade_modules as $module_name=>$files) {
            $this->parseFile($module_name, $files);
        } //foreach
    }


    /**
     * parseFile
     * Hanldes parsing the files for given module where Studio or manual
     * changes have been detected.
     * @param $module_name The module to parse
     * @param $files Array of files found to parse
     *
     */
    public function parseFile($module_name, $files)
    {
        global $beanList, $dictionary;
        foreach ($files as $file) {
            if (preg_match('/(EditView|DetailView|SearchForm|QuickCreate)(\.html|\.tpl)$/s', (string) $file, $matches)) {
                $view = $matches[1];

                switch ($view) {
                       case 'EditView': $parser = $this->evparser; break;
                       case 'DetailView': $parser = $this->dvparser; break;
                       case 'SearchForm': $parser = $this->svparser; break;
                    }

                $lowerCaseView = $view == 'SearchForm' ? 'search' : strtolower($view);

                include('modules/'.$module_name.'/vardefs.php');
                $bean_name = $beanList[$module_name];
                $newFile = $this->upgrade_dir.'/modules/'.$module_name.'/metadata/'.$lowerCaseView.'defs.php';

                $bean_name = $bean_name == 'aCase' ? 'Case' : $bean_name;
                sugar_file_put_contents($newFile, $parser->parse(
                    $file,
                    $dictionary[$bean_name]['fields'],
                    $module_name,
                    true,
                    $this->path_to_master_copy.'/modules/'.$module_name.'/metadata/'.$lowerCaseView.'defs.php'
                ));
            } //if
        } //foreach
    }


    /**
     * create_upgrade_directory
     * Creates a directory called upgrade to house all the modules that are studio enabled.
     * Also creates subdirectories for all studio enabled modules.
     *
     */
    public function create_upgrade_directory()
    {
        $dir = $this->upgrade_dir . '/modules';
        if (!file_exists($this->upgrade_dir) && !mkdir($concurrentDirectory = $this->upgrade_dir,
                0755) && !is_dir($concurrentDirectory)) {
            throw new \RuntimeException(sprintf('Directory "%s" was not created', $concurrentDirectory));
        }
        if (!file_exists($dir) && !mkdir($dir, 0755) && !is_dir($dir)) {
            throw new \RuntimeException(sprintf('Directory "%s" was not created', $dir));
        }

        foreach ($this->upgrade_modules as $module => $files) {
            if (!file_exists($dir . '/' . $module) && !mkdir($concurrentDirectory = $dir . '/' . $module,
                    0755) && !is_dir($concurrentDirectory)) {
                throw new \RuntimeException(sprintf('Directory "%s" was not created', $concurrentDirectory));
            }
            if (!file_exists($dir . '/' . $module . '/metadata') && !mkdir($concurrentDirectory = $dir . '/' . $module . '/metadata',
                    0755) && !is_dir($concurrentDirectory)) {
                throw new \RuntimeException(sprintf('Directory "%s" was not created', $concurrentDirectory));
            }

            foreach ($files as $file) {
                if (file_exists($file) && is_file($file)) {
                    copy($file, $this->upgrade_dir . '/' . $file);
                } //if
            } //foreach
        }
    }


    /**
     * verifyMetaData
     * This function does some quick checks to make sure the metadataFile at
     * least has an Array panel
     *
     * @param $metadataFile The path to the metadata file to verify
     * @param $module The module to verify
     * @param $view The view (EditView or DetailView)
     * @return boolean true if verification check is okay; false otherwise
     */
    public function verifyMetaData($metadataFile, $module, $view)
    {
        if (!file_exists($metadataFile) || !is_file($metadataFile)) {
            return false;
        }

        include($metadataFile);

        if (isset($viewdefs) && isset($viewdefs[$module][$view]['panels']) && is_array($viewdefs[$module][$view]['panels'])) {
            return true;
        }

        if (isset($searchdefs) && isset($searchdefs[$module]) &&  is_array($searchdefs[$module])) {
            return true;
        }

        return false;
    }
}
