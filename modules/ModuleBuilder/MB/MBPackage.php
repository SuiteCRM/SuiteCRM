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

require_once 'modules/ModuleBuilder/MB/MBModule.php';

/**
 * Class MBPackage
 */
#[\AllowDynamicProperties]
class MBPackage
{
    public $name;
    public $is_uninstallable = true;
    public $description = '';
    public $has_images = true;
    public $modules = array();
    public $date_modified = '';
    public $author = '';
    public $key = '';
    public $readme = '';

    /**
     * MBPackage constructor.
     *
     * @param $name
     */
    public function __construct($name)
    {
        $this->name = $name;
        $this->load();
    }

    /**
     * @param bool $force
     */
    public function loadModules($force = false)
    {
        if (!file_exists(MB_PACKAGE_PATH . '/' . $this->name . '/modules')) {
            return;
        }
        $d = dir(MB_PACKAGE_PATH . '/' . $this->name . '/modules');
        while ($e = $d->read()) {
            if (substr($e, 0, 1) !== '.' && is_dir(MB_PACKAGE_PATH . '/' . $this->name . '/modules/' . $e)) {
                $this->getModule($e, $force);
            }
        }
    }

    /**
     * Loads the translated module titles from the selected language into.
     * Will override currently loaded string to reflect undeployed label changes.
     * $app_list_strings
     *
     * @param $language String language identifier
     */
    public function loadModuleTitles($language = '')
    {
        if (empty($language)) {
            $language = $GLOBALS['current_language'];
        }
        global $app_list_strings;
        $packLangFilePath = $this->getPackageDir() . '/language/application/' . $language . '.lang.php';
        if (file_exists($packLangFilePath)) {
            require($packLangFilePath);
        }
    }

    /**
     * @param $name
     * @param bool $force
     *
     * @return MBModule
     */
    public function getModule($name, $force = true)
    {
        if (!$force && !empty($this->modules[$name])) {
            return $this->modules[$name];
        }

        $path = $this->getPackageDir();
        $this->modules[$name] = new MBModule($name, $path, $this->name, $this->key);

        return $this->modules[$name];
    }

    /**
     * Returns an MBModule by the given full name (package key + module name)
     * if it exists in this package
     *
     * @param string $name
     *
     * @return MBModule
     */
    public function getModuleByFullName($name)
    {
        foreach ($this->modules as $mname => $module) {
            if ($this->key . '_' . $mname === $name) {
                return $module;
            }
        }
    }

    /**
     * @param $name
     */
    public function deleteModule($name)
    {
        $this->modules[$name]->delete();
        unset($this->modules[$name]);
    }

    /**
     * @param bool $version_specific
     * @param bool $for_export
     *
     * @return string
     */
    public function getManifest($version_specific = false, $for_export = false)
    {
        //If we are exporting the package, we must ensure a different install key
        $pre = $for_export ? MB_EXPORTPREPEND : '';
        $date = TimeDate::getInstance()->nowDb();
        $time = time();
        $this->description = to_html($this->description);
        $isUninstallable = ($this->is_uninstallable ? true : false);
        
        if ($GLOBALS['sugar_flavor'] === 'CE') {
            $flavors = array('CE');
        } else {
            $flavors = array($GLOBALS['sugar_flavor']);
        }
        $version = (!empty($version_specific)) ? $GLOBALS['sugar_version'] : '';

        // Build an array and use var_export to build this file
        $manifest = array(
            array('acceptable_sugar_versions' => array($version)),
            array('acceptable_sugar_flavors' => $flavors),
            'readme' => $this->readme,
            'key' => $this->key,
            'author' => $this->author,
            'description' => $this->description,
            'icon' => '',
            'is_uninstallable' => $isUninstallable,
            'name' => $pre . $this->name,
            'published_date' => $date,
            'type' => 'module',
            'version' => $time,
            'remove_tables' => 'prompt',
        );

        $header = file_get_contents('modules/ModuleBuilder/MB/header.php');

        return $header .
               "\n// THIS CONTENT IS GENERATED BY MBPackage.php\n" .
               '$manifest = ' .
               var_export_helper($manifest) .
               ";\n\n";
    }

    /**
     * @param $path
     *
     * @return string
     */
    public function buildInstall($path)
    {
        $installdefs = array(
            'id' => $this->name,
            'beans' => array(),
            'layoutdefs' => array(),
            'relationships' => array(),
        );
        if ($this->has_images) {
            $installdefs['image_dir'] = '<basepath>/icons';
        }
        foreach (array_keys($this->modules) as $module) {
            $this->modules[$module]->build($path);
            $this->modules[$module]->addInstallDefs($installdefs);
        }
        $this->path = $this->getPackageDir();
        if (file_exists($this->path . '/language')) {
            $d = dir($this->path . '/language');
            while ($e = $d->read()) {
                $lang_path = $this->path . '/language/' . $e;
                if (substr($e, 0, 1) !== '.' && is_dir($lang_path)) {
                    $f = dir($lang_path);
                    while ($g = $f->read()) {
                        if (substr($g, 0, 1) !== '.' && is_file($lang_path . '/' . $g)) {
                            $lang = substr($g, 0, strpos($g, '.'));
                            $installdefs['language'][] = array(
                                'from' => '<basepath>/SugarModules/language/' . $e . '/' . $g,
                                'to_module' => $e,
                                'language' => $lang
                            );
                        }
                    }
                }
            }

            copy_recursive($this->path . '/language/', $path . '/language/');
            $icon_path = $path . '/../icons/default/images/';
            mkdir_recursive($icon_path);
            copy_recursive($this->path . '/icons/', $icon_path);
        }

        return "\n" . '$installdefs = ' . var_export_helper($installdefs) . ';';
    }

    /**
     * @return string
     */
    public function getPackageDir()
    {
        return MB_PACKAGE_PATH . '/' . $this->name;
    }

    /**
     * @return string
     */
    public function getBuildDir()
    {
        return MB_PACKAGE_BUILD . DIRECTORY_SEPARATOR . $this->name;
    }

    /**
     * @return string
     */
    public function getZipDir()
    {
        return $this->getPackageDir() . '/zips';
    }

    public function load()
    {
        $path = $this->getPackageDir();
        if (file_exists($path . '/manifest.php')) {
            require($path . '/manifest.php');
            if (!empty($manifest)) {
                $this->date_modified = $manifest['published_date'];
                $this->is_uninstallable = $manifest['is_uninstallable'];
                $this->author = $manifest['author'];
                $this->key = $manifest['key'];
                $this->description = $manifest['description'];
                if (!empty($manifest['readme'])) {
                    $this->readme = $manifest['readme'];
                }
            }
        }
        $this->loadModules(true);
    }

    public function save()
    {
        $path = $this->getPackageDir();
        if (mkdir_recursive($path)) {
            //Save all the modules when we save a package
            $this->updateModulesMetaData(true);
            sugar_file_put_contents_atomic($path . '/manifest.php', $this->getManifest());
        }
    }

    /**
     * @param bool $export
     * @param bool $clean
     *
     * @return array
     */
    public function build($export = true, $clean = false)
    {
        $this->loadModules();
        require_once 'include/utils/php_zip_utils.php';
        $path = $this->getBuildDir() . '/SugarModules';
        if ($clean && file_exists($path)) {
            rmdir_recursive($path);
        }
        if (mkdir_recursive($path)) {
            $manifest = $this->getManifest() . $this->buildInstall($path);
            sugar_file_put_contents($this->getBuildDir() . '/manifest.php', $manifest);
        }
        if (file_exists('modules/ModuleBuilder/MB/LICENSE.txt')) {
            copy('modules/ModuleBuilder/MB/LICENSE.txt', $this->getBuildDir() . '/LICENSE.txt');
        } else {
            if (file_exists('LICENSE.txt')) {
                copy('LICENSE.txt', $this->getBuildDir() . '/LICENSE.txt');
            }
        }
        $date = date('Y_m_d_His');
        $zipDir = $this->getZipDir();
        if (!file_exists($zipDir)) {
            mkdir_recursive($zipDir);
        }
        $cwd = getcwd();
        chdir($this->getBuildDir());
        zip_dir('.', $cwd . '/' . $zipDir . '/' . $this->name . $date . '.zip');
        chdir($cwd);
        if ($export) {
            header('Location:' . $zipDir . '/' . $this->name . $date . '.zip');
        }

        return array(
            'zip' => $zipDir . '/' . $this->name . $date . '.zip',
            'manifest' => $this->getBuildDir() . '/manifest.php',
            'name' => $this->name . $date,
        );
    }

    /**
     * @return array
     */
    public function getNodes()
    {
        $this->loadModules();
        $node = array(
            'name' => $this->name,
            'action' => 'module=ModuleBuilder&action=package&package=' . $this->name,
            'children' => array()
        );
        foreach (array_keys($this->modules) as $module) {
            $node['children'][] = $this->modules[$module]->getNodes();
        }

        return $node;
    }

    public function populateFromPost()
    {
        $this->description = trim($_REQUEST['description']);
        $this->author = trim($_REQUEST['author']);
        $this->key = trim($_REQUEST['key']);
        $this->readme = trim($_REQUEST['readme']);
    }

    /**
     * @param string $new_name
     *
     * @return bool
     */
    public function rename($new_name)
    {
        $old = $this->getPackageDir();
        $this->name = $new_name;
        $new = $this->getPackageDir();
        if (file_exists($new)) {
            return false;
        }
        if (rename($old, $new)) {
            return true;
        }

        return false;
    }

    /**
     * @param bool $save
     */
    public function updateModulesMetaData($save = false)
    {
        foreach (array_keys($this->modules) as $module) {
            $old_name = $this->modules[$module]->key_name;
            $this->modules[$module]->key_name = $this->key . '_' . $this->modules[$module]->name;
            $this->modules[$module]->renameMetaData($this->modules[$module]->getModuleDir(), $old_name);
            $this->modules[$module]->renameLanguageFiles($this->modules[$module]->getModuleDir());
            if ($save) {
                $this->modules[$module]->save();
            }
        }
    }

    /**
     * @param string $new_name
     *
     * @return bool
     */
    public function copy($new_name)
    {
        $old = $this->getPackageDir();

        $count = 0;
        $this->name = $new_name;
        $new = $this->getPackageDir();
        while (file_exists($new)) {
            $count++;
            $this->name = $new_name . $count;
            $new = $this->getPackageDir();
        }

        $new = $this->getPackageDir();
        if (copy_recursive($old, $new)) {
            $this->updateModulesMetaData();

            return true;
        }

        return false;
    }

    /**
     * @return bool
     */
    public function delete()
    {
        return rmdir_recursive($this->getPackageDir());
    }

    /**
     * creation of the installdefs[] array for the manifest when exporting customizations
     *
     * @param $modules
     * @param $path
     *
     * @return string
     */
    public function customBuildInstall($modules, $path)
    {
        $installdefs = array('id' => $this->name, 'relationships' => array());
        $include_path = "$path/SugarModules/include/language";
        if (file_exists($include_path) && is_dir($include_path)) {
            $dd = dir($include_path);
            while ($gg = $dd->read()) {
                if (substr($gg, 0, 1) !== '.' && is_file($include_path . '/' . $gg)) {
                    $lang = substr($gg, 0, strpos($gg, '.'));
                    $installdefs['language'][] = array(
                        'from' => '<basepath>/SugarModules/include/language/' . $gg,
                        'to_module' => 'application',
                        'language' => $lang
                    );
                }
            }
        }

        foreach ($modules as $value) {
            $custom_module = $this->getCustomModules($value);
            foreach ($custom_module as $va) {
                if ($va === 'language') {
                    $this->getLanguageManifestForModule($value, $installdefs);
                    $this->getCustomFieldsManifestForModule($value, $installdefs);
                }//fi
                if ($va === 'metadata') {
                    $this->getCustomMetadataManifestForModule($value, $installdefs);
                }//fi
            }//foreach
            $relationshipsMetaFiles = $this->getCustomRelationshipsMetaFilesByModuleName($value, true, true, $modules);
            if ($relationshipsMetaFiles) {
                foreach ($relationshipsMetaFiles as $file) {
                    $installdefs['relationships'][] = array('meta_data' => str_replace('custom', '<basepath>', (string) $file));
                }
            }
        }//foreach
        if (is_dir($path . DIRECTORY_SEPARATOR . 'Extension')) {
            $this->getExtensionsManifestForPackage($path, $installdefs);
        }

        return "\n" . '$installdefs = ' . var_export_helper($installdefs) . ';';
    }

    /**
     * @param $module
     * @param $installdefs
     */
    private function getLanguageManifestForModule($module, &$installdefs)
    {
        $lang_path =
            'custom' .
            DIRECTORY_SEPARATOR .
            'modules' .
            DIRECTORY_SEPARATOR .
            $module .
            DIRECTORY_SEPARATOR .
            'language';
        foreach (scandir($lang_path) as $langFile) {
            if (substr((string) $langFile, 0, 1) !== '.' && is_file($lang_path . DIRECTORY_SEPARATOR . $langFile)) {
                $lang = substr((string) $langFile, 0, strpos((string) $langFile, '.'));
                $installdefs['language'][] = array(
                    'from' => '<basepath>/SugarModules/modules/' . $module . '/language/' . $langFile,
                    'to_module' => $module,
                    'language' => $lang
                );
            }
        }
    }

    /**
     * @param $module
     * @param $installdefs
     */
    private function getCustomFieldsManifestForModule($module, &$installdefs)
    {
        $db = DBManagerFactory::getInstance();
        $result = $db->query("SELECT *  FROM fields_meta_data where custom_module='$module'");
        while ($row = $db->fetchByAssoc($result)) {
            $name = $row['id'];
            foreach ($row as $col => $res) {
                switch ($col) {
                    case 'custom_module':
                        $installdefs['custom_fields'][$name]['module'] = $res;
                        break;
                    case 'vname':
                        $installdefs['custom_fields'][$name]['label'] = $res;
                        break;
                    case 'required':
                        $installdefs['custom_fields'][$name]['require_option'] = $res;
                        break;
                    case 'massupdate':
                        $installdefs['custom_fields'][$name]['mass_update'] = $res;
                        break;
                    case 'comments':
                        $installdefs['custom_fields'][$name]['comments'] = $res;
                        break;
                    case 'help':
                        $installdefs['custom_fields'][$name]['help'] = $res;
                        break;
                    case 'len':
                        $installdefs['custom_fields'][$name]['max_size'] = $res;
                        break;
                    default:
                        $installdefs['custom_fields'][$name][$col] = $res;
                }//switch
            }//foreach
        }//while
    }

    /**
     * @param $module
     * @param $installdefs
     */
    private function getCustomMetadataManifestForModule($module, &$installdefs)
    {
        $meta_path = 'custom/modules/' . $module . '/metadata';
        $working_array = [
            'editviewdefs.php',
            'detailviewdefs.php',
            'quickcreatedefs.php'
        ];
        foreach (scandir($meta_path) as $meta_file) {
            if (substr((string) $meta_file, 0, 1) !== '.' && is_file($meta_path . '/' . $meta_file)) {
                $installdefs['copy'][] = array(
                    'from' => '<basepath>/SugarModules/modules/' . $module . '/metadata/' . $meta_file,
                    'to' => 'custom/modules/' . $module . '/metadata/' . $meta_file,
                );
                if (in_array($meta_file, $working_array, true)) {
                    $installdefs['copy'][] = array(
                        'from' => '<basepath>/SugarModules/modules/' . $module . '/metadata/' . $meta_file,
                        'to' => 'custom/working/modules/' . $module . '/metadata/' . $meta_file,
                    );
                }
            }
        }
    }

    /**
     *
     * @param string $path
     * @param array $installdefs link
     */
    protected function getExtensionsManifestForPackage($path, &$installdefs)
    {
        if (empty($installdefs['copy'])) {
            $installdefs['copy'] = array();
        }
        $generalPath = DIRECTORY_SEPARATOR . 'Extension' . DIRECTORY_SEPARATOR . 'modules';

        //do not process if path is not a valid directory, or recursiveIterator will break.
        if (!is_dir($path . $generalPath)) {
            return;
        }

        $recursiveIterator = new RecursiveIteratorIterator(
            new RecursiveDirectoryIterator($path . $generalPath),
            RecursiveIteratorIterator::SELF_FIRST
        );

        /* @var $fInfo SplFileInfo */
        foreach (new RegexIterator($recursiveIterator, "/\.php$/i") as $fInfo) {
            $newPath = substr((string) $fInfo->getPathname(), strrpos((string) $fInfo->getPathname(), $generalPath));

            $installdefs['copy'][] = array(
                'from' => '<basepath>' . $newPath,
                'to' => 'custom' . $newPath
            );
        }
    }

    /**
     * return an array which contain the name of fields_meta_data table's columns
     *
     * @return array
     */
    public function getColumnsName()
    {
        $meta = BeanFactory::newBean('EditCustomFields');
        $arr = array();
        foreach ($meta->getFieldDefinitions() as $key => $value) {
            $arr[] = $key;
        }

        return $arr;
    }

    /**
     * creation of the custom fields ZIP file (use getmanifest() and customBuildInstall() )
     *
     * @param $modules
     * @param bool $export
     * @param bool $clean
     *
     * @return string
     */
    public function exportCustom($modules, $export = true, $clean = true)
    {
        $path = $this->getBuildDir();
        if ($clean && file_exists($path)) {
            rmdir_recursive($path);
        }
        //Copy the custom files to the build dir
        foreach ($modules as $module) {
            $pathmod = "$path/SugarModules/modules/$module";
            if (mkdir_recursive($pathmod)) {
                if (file_exists("custom/modules/$module")) {
                    copy_recursive("custom/modules/$module", (string)$pathmod);
                    //Don't include cached extension files
                    if (is_dir("$pathmod/Ext")) {
                        rmdir_recursive("$pathmod/Ext");
                    }
                }
                //Convert modstring files to extension compatible arrays
                $this->convertLangFilesToExtensions("$pathmod/language");
            }

            $extensions = $this->getExtensionsList($module, $modules);
            $relMetaFiles = $this->getCustomRelationshipsMetaFilesByModuleName($module, true, false, $modules);
            $extensions = array_merge($extensions, $relMetaFiles);

            foreach ($extensions as $file) {
                $fileInfo = new SplFileInfo($file);
                $trimmedPath = ltrim($fileInfo->getPath(), 'custom');

                sugar_mkdir($path . $trimmedPath, null, true);
                copy($file, $path . $trimmedPath . '/' . $fileInfo->getFilename());
            }
        }

        $this->copyCustomDropdownValuesForModules($modules, $path);
        if (file_exists($path)) {
            $manifest = $this->getManifest(true) . $this->customBuildInstall($modules, $path);
            sugar_file_put_contents($path . '/manifest.php', $manifest);
        }
        if (file_exists('modules/ModuleBuilder/MB/LICENSE.txt')) {
            copy('modules/ModuleBuilder/MB/LICENSE.txt', $path . '/LICENSE.txt');
        } else {
            if (file_exists('LICENSE.txt')) {
                copy('LICENSE.txt', $path . '/LICENSE.txt');
            }
        }
        require_once 'include/utils/php_zip_utils.php';
        $date = date('Y_m_d_His');
        $zipDir = $this->getZipDir();
        if (!file_exists($zipDir)) {
            mkdir_recursive($zipDir);
        }
        $cwd = getcwd();
        chdir($this->getBuildDir());
        zip_dir('.', $cwd . '/' . $zipDir . '/' . $this->name . $date . '.zip');
        chdir($cwd);
        if ($clean && file_exists($this->getBuildDir())) {
            rmdir_recursive($this->getBuildDir());
        }
        if ($export) {
            header('Location:' . $zipDir . '/' . $this->name . $date . '.zip');
        }

        return $zipDir . '/' . $this->name . $date . '.zip';
    }

    /**
     * @param $langDir
     */
    private function convertLangFilesToExtensions($langDir)
    {
        if (is_dir($langDir)) {
            foreach (scandir($langDir) as $langFile) {
                $mod_strings = array();
                if (strcasecmp(substr((string) $langFile, -4), '.php') !== 0) {
                    continue;
                }
                include("$langDir/$langFile");
                $out = "<?php \n // created: " . date('Y-m-d H:i:s') . "\n";
                foreach ($mod_strings as $lbl_key => $lbl_val) {
                    $out .= override_value_to_string('mod_strings', $lbl_key, $lbl_val) . "\n";
                }
                $out .= "\n?>\n";
                sugar_file_put_contents("$langDir/$langFile", $out);
            }
        }
    }

    /**
     * @param $modules
     * @param $path
     */
    private function copyCustomDropdownValuesForModules($modules, $path)
    {
        if (file_exists('custom/include/language')) {
            if (mkdir_recursive("$path/SugarModules/include")) {
                global $app_list_strings;
                $backStrings = $app_list_strings;
                mkdir_recursive("$path/SugarModules/include/language/");
                foreach (scandir('custom/include/language') as $langFile) {
                    $app_list_strings = array();
                    if (strcasecmp(substr((string) $langFile, -4), '.php') !== 0) {
                        continue;
                    }
                    include "custom/include/language/$langFile";
                    $out = "<?php \n";
                    $lang = substr((string) $langFile, 0, -9);
                    $options = $this->getCustomDropDownStringsForModules($modules, $app_list_strings);
                    foreach ($options as $name => $arr) {
                        $out .= override_value_to_string('app_list_strings', $name, $arr);
                    }
                    sugar_file_put_contents("$path/SugarModules/include/language/$lang.$this->name.php", $out);
                }
                $app_list_strings = $backStrings;
            }
        }
    }

    /**
     * @param $modules
     * @param $list_strings
     *
     * @return array
     */
    public function getCustomDropDownStringsForModules($modules, $list_strings)
    {
        global $beanList, $beanFiles;
        $options = array();
        foreach ($modules as $module) {
            if (!empty($beanList[$module])) {
                require_once($beanFiles[$beanList[$module]]);
                $bean = new $beanList[$module]();
                foreach ($bean->field_defs as $def) {
                    if (isset($def['options']) && isset($list_strings[$def['options']])) {
                        $options[$def['options']] = $list_strings[$def['options']];
                    }
                }
            }
        }

        return $options;
    }

    /**
     * @param bool $module
     *
     * @return array
     */
    public function getCustomModules($module = false)
    {
        global $mod_strings;
        $path = 'custom/modules/';
        if (!file_exists($path) || !is_dir($path)) {
            return array($mod_strings['LBL_EC_NOCUSTOM'] => '');
        }

        if ($module !== false) {
            $path = $path . $module . '/';
        }
        $scanlisting = scandir($path, SCANDIR_SORT_ASCENDING);
        $dirlisting = array();
        foreach ($scanlisting as $value) {
            if ($value !== '.' && $value !== '..' && is_dir($path . $value) === true) {
                $dirlisting[] = $value;
            }
        }
        if (empty($dirlisting)) {
            return array($mod_strings['LBL_EC_NOCUSTOM'] => '');
        }
        if (!$module) {
            $return = array();
            foreach ($dirlisting as $value) {
                if (!file_exists('modules/' . $value . '/metadata/studio.php')) {
                    continue;
                }
                $custommodules[$value] = $this->getCustomModules($value);
                foreach ($custommodules[$value] as $va) {
                    switch ($va) {
                        case 'language':
                            $return[$value][$va] = $mod_strings['LBL_EC_CUSTOMFIELD'];
                            break;
                        case 'metadata':
                            $return[$value][$va] = $mod_strings['LBL_EC_CUSTOMLAYOUT'];
                            break;
                        case 'Ext':
                            $return[$value][$va] = $mod_strings['LBL_EC_CUSTOMFIELD'];
                            break;
                        case '':
                            $return[$value][$va] = $mod_strings['LBL_EC_EMPTYCUSTOM'];
                            break;
                        case 'views':
                            $return[$value][$va] = $mod_strings['LBL_EC_VIEWS'];
                            break;
                        case 'SugarFeeds':
                            $return[$value][$va] = $mod_strings['LBL_EC_SUITEFEEDS'];
                            break;
                        case 'Dashlets':
                            $return[$value][$va] = $mod_strings['LBL_EC_DASHLETS'];
                            break;
                        case 'css':
                            $return[$value][$va] = $mod_strings['LBL_EC_CSS'];
                            break;
                        case 'tpls':
                            $return[$value][$va] = $mod_strings['LBL_EC_TPLS'];
                            break;
                        case 'images':
                            $return[$value][$va] = $mod_strings['LBL_EC_IMAGES'];
                            break;
                        case 'js':
                            $return[$value][$va] = $mod_strings['LBL_EC_JS'];
                            break;
                        case 'qtip':
                            $return[$value][$va] = $mod_strings['LBL_EC_QTIP'];
                            break;
                        default:
                            $return[$value][$va] = $mod_strings['LBL_UNDEFINED'];
                    }
                }
            }

            return $return;
        }

        return $dirlisting;
    }

    /**
     * Get _custom_ extensions for module.
     * Default path - custom/Extension/modules/$module/Ext.
     *
     * @param array $module Name.
     * @param mixed $includeRelationships ARRAY - relationships files between $module and names in array;
     * TRUE - with all relationships files;
     *
     * @return array Paths.
     */
    protected function getExtensionsList($module, $includeRelationships = true)
    {
        if (BeanFactory::getBeanName($module) === false) {
            return array();
        }

        $result = array();
        $includeMask = false;
        $extPath = sprintf('custom%1$sExtension%1$smodules%1$s' . $module . '%1$sExt', DIRECTORY_SEPARATOR);

        //do not process if path is not a valid directory, or recursiveIterator will break.
        if (!is_dir($extPath)) {
            return $result;
        }

        if (is_array($includeRelationships)) {
            $includeMask = array();
            $customRels = $this->getCustomRelationshipsByModuleName($module);

            $includeRelationships[] = $module;

            foreach ($customRels as $k => $v) {
                if (in_array($v->getLhsModule(), $includeRelationships) &&
                    in_array($v->getRhsModule(), $includeRelationships)
                ) {
                    $includeMask[] = $k;
                }
            }
        }

        $recursiveIterator = new RecursiveIteratorIterator(
            new RecursiveDirectoryIterator($extPath),
            RecursiveIteratorIterator::SELF_FIRST
        );

        /* @var $fileInfo SplFileInfo */
        foreach ($recursiveIterator as $fileInfo) {
            if ($fileInfo->isFile() && !in_array($fileInfo->getPathname(), $result)) {
                //get the filename in lowercase for easier comparison
                $fn = $fileInfo->getFilename();
                if (!empty($fn)) {
                    $fn = strtolower($fn);
                }

                if ($this->filterExportedRelationshipFile($fn, $module, $includeRelationships)) {
                    $result[] = $fileInfo->getPathname();
                }
            }
        }

        return $result;
    }

    /**
     * Processes the name of the file and compares against the passed in module names to
     * evaluate whether the file should be included for export.  Returns true or false
     *
     * @param $fn (file name that is being evaluated)
     * @param $module (name of current module being evaluated)
     * @param $includeRelationships
     *
     * @return bool true or false
     * @internal param $includeRelationship (list of related modules that are also being exported, to be used as filters)
     */
    public function filterExportedRelationshipFile($fn, $module, $includeRelationships)
    {
        $shouldExport = false;
        if (empty($fn) || !is_array($includeRelationships) || empty($includeRelationships)) {
            return $shouldExport;
        }

        //if file name does not contain the current module name then it is not a relationship file,
        //or if the module has the current module name twice separated with an underscore, then this is a relationship within itself
        //in both cases set the shouldExport flag to true
        $lc_mod = strtolower($module);
        $fn = strtolower($fn);
        if ((strpos($fn, $lc_mod) === false) || (strpos($fn, $lc_mod . '_' . $lc_mod) !== false)) {
            $shouldExport = true;
        } else {

            //grab only rels that have both modules in the export list
            foreach ($includeRelationships as $relatedModule) {
                //skip if the related module is empty
                if (empty($relatedModule)) {
                    continue;
                }

                //if the filename also has the related module name, then add the relationship file
                //strip the current module,as we have already checked for existance of module name and dont want any false positives
                $fn = str_replace($lc_mod, '', $fn);
                if (strpos($fn, strtolower($relatedModule)) !== false) {
                    //both modules exist in the filename lets include in the results array
                    $shouldExport = true;
                    break;
                }
            }
        }

        return $shouldExport;
    }

    /**
     * Returns a set of field defs for fields that will exist when this package is deployed
     * based on the relationships in all of its modules.
     *
     * @param $moduleName (module must be from whithin this package)
     *
     * @return array Field defs
     */
    public function getRelationshipsForModule($moduleName)
    {
        $ret = array();
        if (isset($this->modules[$moduleName])) {
            $keyName = $this->modules[$moduleName]->key_name;
            foreach ($this->modules as $module) {
                $rels = $module->getRelationships();
                $relList = $rels->getRelationshipList();
                foreach ($relList as $rName) {
                    $rel = $rels->get($rName);
                    if ($rel->lhs_module === $keyName || $rel->rhs_module === $keyName) {
                        $ret[$rName] = $rel;
                    }
                }
            }
        }

        return $ret;
    }

    /**
     * @param $package
     * @param $for_export
     *
     * @return string
     */
    public function exportProjectInstall($package, $for_export)
    {
        $pre = $for_export ? MB_EXPORTPREPEND : '';
        $installdefs = array('id' => $pre . $this->name);
        $installdefs['copy'][] = array(
            'from' => '<basepath>/' . $this->name,
            'to' => 'custom/modulebuilder/packages/' . $this->name,
        );

        return "\n" . '$installdefs = ' . var_export_helper($installdefs) . ';';
    }

    /**
     * @param $package
     * @param bool $export
     * @param bool $clean
     *
     * @return string
     */
    public function exportProject($package = '', $export = true, $clean = true)
    {
        $tmppath = 'custom/modulebuilder/projectTMP/';
        if (file_exists($this->getPackageDir())) {
            if (mkdir_recursive($tmppath)) {
                copy_recursive($this->getPackageDir(), $tmppath . '/' . $this->name);
                $manifest = $this->getManifest(true, $export) . $this->exportProjectInstall($package, $export);
                sugar_file_put_contents($tmppath . '/manifest.php', $manifest);
                if (file_exists('modules/ModuleBuilder/MB/LICENSE.txt')) {
                    copy('modules/ModuleBuilder/MB/LICENSE.txt', $tmppath . '/LICENSE.txt');
                } else {
                    if (file_exists('LICENSE.txt')) {
                        copy('LICENSE.txt', $tmppath . '/LICENSE.txt');
                    }
                }
                $readme_contents = $this->readme;
                sugar_file_put_contents($tmppath . '/README.txt', $readme_contents);
            }
        }
        require_once 'include/utils/php_zip_utils.php';
        $date = date('Y_m_d_His');
        $zipDir = 'custom/modulebuilder/packages/ExportProjectZips';
        if (!file_exists($zipDir)) {
            mkdir_recursive($zipDir);
        }
        $cwd = getcwd();
        chdir($tmppath);
        zip_dir('.', $cwd . '/' . $zipDir . '/project_' . $this->name . $date . '.zip');
        chdir($cwd);
        if ($clean && file_exists($tmppath)) {
            rmdir_recursive($tmppath);
        }
        if ($export) {
            header('Location:' . $zipDir . '/project_' . $this->name . $date . '.zip');
        }

        return $zipDir . '/project_' . $this->name . $date . '.zip';
    }

    /**
     * This returns an UNFILTERED list of custom relationships by module name.  You will have to filter the
     * relationships by the modules being exported after calling this method
     *
     * @param string $moduleName
     * @param bool $lhs Return relationships where $moduleName - left module in join.
     *
     * @return mixed Array or false when module name is wrong.
     */
    protected function getCustomRelationshipsByModuleName($moduleName, $lhs = false)
    {
        if (BeanFactory::getBeanName($moduleName) === false) {
            return false;
        }

        $result = array();
        $relation = null;
        $module = new StudioModule($moduleName);

        /* @var $rel DeployedRelationships */
        $rel = $module->getRelationships();

        $relList = $rel->getRelationshipList();

        foreach ($relList as $relationshipName) {
            $relation = $rel->get($relationshipName);

            if ($relation->getFromStudio()) {
                if ($lhs && $relation->getLhsModule() !== $moduleName) {
                    continue;
                }
                $result[$relationshipName] = $relation;
            }
        }

        return $result;
    }

    /**
     * @param string $moduleName
     * @param bool $lhs Return relationships where $moduleName - left module in join.
     * @param bool $metadataOnly Return only relationships metadata file.
     * @param array $exportedModulesFilter
     *
     * @return array
     * @internal param $includeRelationship (list of related modules that are also being exported)
     */
    protected function getCustomRelationshipsMetaFilesByModuleName(
        $moduleName,
        $lhs = false,
        $metadataOnly = false,
        $exportedModulesFilter = array()
    ) {
        $path =
            $metadataOnly ? 'custom' . DIRECTORY_SEPARATOR . 'metadata' . DIRECTORY_SEPARATOR :
                'custom' . DIRECTORY_SEPARATOR;
        $result = array();

        //do not process if path is not a valid directory, or recursiveIterator will break.
        if (!is_dir($path)) {
            return $result;
        }

        $relationships = $this->getCustomRelationshipsByModuleName($moduleName, $lhs);

        if (!$relationships) {
            return array();
        }

        $recursiveIterator = new RecursiveIteratorIterator(
            new RecursiveDirectoryIterator($path),
            RecursiveIteratorIterator::SELF_FIRST
        );

        /**
         * @var $fileInfo SplFileInfo
         */
        foreach ($recursiveIterator as $fileInfo) {
            if ($fileInfo->isFile() && !in_array($fileInfo->getPathname(), $result)) {
                foreach ($relationships as $k => $v) {
                    if (strpos((string) $fileInfo->getFilename(), (string) $k) !== false) {   //filter by modules being exported
                        if ($this->filterExportedRelationshipFile(
                            $fileInfo->getFilename(),
                            $moduleName,
                            $exportedModulesFilter
                        )
                        ) {
                            $result[] = $fileInfo->getPathname();
                            break;
                        }
                    }
                }
            }
        }

        return $result;
    }

    /**
     * @return bool
     */
    public function deleteBuild()
    {
        return rmdir_recursive($this->getBuildDir());
    }
}
