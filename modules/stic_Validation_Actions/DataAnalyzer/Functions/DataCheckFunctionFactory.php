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

require_once 'modules/stic_Validation_Actions/DataAnalyzer/Functions/DataCheckFunction.php';

/**
 * Class that allows loading the different checking functions
 */
class DataCheckFunctionFactory {

    /* Constant indicating the path of the include directory */
    public const FUNCTION_PATH = 'modules/stic_Validation_Actions/DataAnalyzer/Functions';
    public const CUSTOM_FUNCTION_PATH = 'custom/modules/stic_Validation_Actions/DataAnalyzer/Functions';
    public const INCLUDE_DIR = 'include';
    public const INCLUDE_PATH = 'modules/stic_Validation_Actions/DataAnalyzer/Functions/include';

    /* Array with the list of available function objects */
    private static $functionList = array();

    /* Array with the list of names of available functions */
    private static $functionNameList = array();

    /* Array with the list of common language tags */
    private static $commonLanguageLabels = array();

    /**
     * Returns an object of the specified identifier
     * @param String id Function identifier
     * @return Object|Null
     */
    public static function getFunctionObject($id) {
        if (empty(self::$functionList)) {
            self::loadFunctionList(true);
        }
        return clone self::$functionList[$id]; // Returns a copy of the object, not the object itself.
    }

    /**
     * Returns the list of names of available functions
     * @param $lang Tag language
     * @param $forceLoad Indicates whether the list of names should be reloaded
     * @return Array (ID => LABEL)
     */
    public static function getFunctionListStrings($lang = null, $forceLoad = false) {
        if (empty($lang)) {
            $lang = $GLOBALS['current_language'];
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Received empty lang, using the default language [{$lang}]");
        }
        if (empty(self::$functionNameList[$lang]) || $forceLoad) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Empty function list for the language [{$lang}] or forced load [{$forceLoad}]");
            self::_loadFunctionNames($lang); // Force reload of function names
        }
        return self::$functionNameList[$lang];
    }

    /**
     * Load the list of available functions
     * @param $forceLoad Allows you to force reload functions
     **/
    public static function loadFunctionList($forceLoad = false) {

        // If it has not been loaded before or the load is forced, the list of available functions is loaded
        if (empty(self::$functionList) || $forceLoad) {
            self::$functionList = array(); // Reset the function list
            self::$commonLanguageLabels = self::_loadLanguageFiles(self::INCLUDE_PATH); // Reset the list of common language tags

            // Tour the predefined functions
            $defaultFuncs = self::_loadFunctionPath(self::FUNCTION_PATH);

            // And custom functions
            $customFuncs = self::_loadFunctionPath(self::CUSTOM_FUNCTION_PATH);

            // We build the list of functions from the union of the two arrays
            self::$functionList = array_merge($defaultFuncs, $customFuncs);
            self::$functionList = $defaultFuncs;
        }

        return self::$functionList;
    }

    /**
     * Load the functions of a directory
     * @return array
     */
    private static function _loadFunctionPath($path) {
        $ret = array();
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Retrieving directory functions [{$path}]...");
        if (!is_dir($path) || !($d = @dir($path))) {
            $GLOBALS['log']->warn('Line ' . __LINE__ . ': ' . __METHOD__ . ": {$path} does not exist or is not accessible.");
        } else {
            while (false !== ($e = $d->read())) {
                // If the entry is the directory itself or is not a directory we do nothing
                if (substr($e, 0, 1) == '.' || !is_dir("{$path}/{$e}") || "{$e}" == self::INCLUDE_DIR) {
                    continue;
                }

                // If it is a directory we try to load the function definition file
                try {
                    $function = self::_loadFunctionFromFile("{$path}/{$e}");
                    $name = $function->getName();
                    $id = $function->id;
                    $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Function [{$name}] loaded with id [{$id}]");
                    $ret[$function->id] = $function;
                } catch (\Exception $ex) {
                    $GLOBALS['log']->warn('Line ' . __LINE__ . ': ' . __METHOD__ . ": {$path}/{$e} does not exist or is not accessible.");
                }
            }
        }
        return $ret;
    }

    /* Load the data from a configuration file */
    private static function _loadFunctionFromFile($path) {
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": loading function from [{$path}]...");
        $functionDef = null;
        if(!file_exists($path . '/FunctionDef.php')){
            throw new Exception();
        } else {
            require $path . '/FunctionDef.php';
        }
        if(!file_exists($path . '/' . $functionDef['classFile'])){
            throw new Exception();
        } else {
            require_once $path . '/' . $functionDef['classFile']; // Load the class file
        }
        $className = $functionDef['class'];
        $obj = new $className($functionDef);
        $obj->setLang(self::_loadLanguageFiles($path));
        return $obj;
    }

    /* Load the language labels of the function */
    private static function _loadLanguageFiles($path) {
        $langPath = $path . '/language';
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": loading language files from [{$langPath}]...");
        $langs = self::$commonLanguageLabels; // Common tags are always loaded
        $d = @dir($langPath);
        if (!$d) {
            $GLOBALS['log']->warn('Line ' . __LINE__ . ': ' . __METHOD__ . ": {$langPath} does not exist or is not accessible.");
        } else {
            while ($e = $d->read()) {

                // If the entry is a directory we do nothing
                if (is_dir("{$langPath}/{$e}")) {
                    continue;
                }

                // If it is a file we try to load the language files
                include "{$langPath}/{$e}";
                $langId = basename("{$langPath}/{$e}", ".lang.php"); // Save the name of the language file
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": file [{$e}] loaded with id [{$langId}].");

                // At this point in $ langs should be the general tags.
                // If the $ langId language has been initialized on these labels, mix the vectors by overwriting the duplicate values.
                // Otherwise simply initialize the $ langId in the tag vector
                if (isset($langs[$langId]) && is_array($langs[$langId])) {
                    $langs[$langId] = array_replace($langs[$langId], $func_strings);
                } else {
                    $langs[$langId] = $func_strings;
                }
            }
        }
        return $langs;
    }

    /* Load the list of function names */
    private static function _loadFunctionNames($lang) {
        self::$functionNameList[$lang] = array();
        if (empty(self::$functionList)) {
            self::loadFunctionList(true); // If the function list is not loaded, force reload
        }
        foreach (self::$functionList as $id => $function) {
            self::$functionNameList[$lang][$id] = $function->getName($lang);
        }

        // Sort the function list
        natcasesort(self::$functionNameList[$lang]);
    }

}
