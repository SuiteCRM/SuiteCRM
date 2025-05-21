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

/*
 * Implementation class (following a Bridge Pattern) for handling loading and saving deployed module metadata
 * For example, listview or editview viewdefs
 */

require_once 'modules/ModuleBuilder/parsers/views/AbstractMetaDataImplementation.php';
require_once 'modules/ModuleBuilder/parsers/views/MetaDataImplementationInterface.php';
require_once 'modules/ModuleBuilder/parsers/views/ListLayoutMetaDataParser.php';
require_once 'modules/ModuleBuilder/parsers/views/GridLayoutMetaDataParser.php';
require_once 'modules/ModuleBuilder/parsers/views/PopupMetaDataParser.php';
require_once 'modules/ModuleBuilder/Module/StudioModuleFactory.php';
require_once 'modules/ModuleBuilder/parsers/constants.php';

/**
 * Class DeployedMetaDataImplementation
 */
#[\AllowDynamicProperties]
class DeployedMetaDataImplementation extends AbstractMetaDataImplementation implements MetaDataImplementationInterface
{
    /**
     * @var string $_module_dir
     */
    protected $module_dir;

    /**
     * @var History $_history
     */
    protected $_history;


    /**
     * Constructor
     * @param string $view
     * @param string $moduleName
     * @throws Exception Thrown if the provided view doesn't exist for this module
     */
    public function __construct($view, $moduleName)
    {
        // BEGIN ASSERTIONS
        if (!isset($GLOBALS ['beanList'] [$moduleName])) {
            sugar_die(get_class($this) . ': $moduleName '. $moduleName .' is not a Deployed Module');
        }
        // END ASSERTIONS

        $this->_view = strtolower($view);
        $this->_moduleName = $moduleName;

        $module = StudioModuleFactory::getStudioModule($moduleName);
        $this->module_dir = $module->seed->module_dir;
        $fieldDefinitions = $module->getFields();

        //Load any custom views
        $sm = StudioModuleFactory::getStudioModule($moduleName);
        foreach ($sm->sources as $file => $def) {
            if (!empty($def['view'])) {
                $viewVar = 'viewdefs';
                if (!empty($def['type']) && !empty($this->_fileVariables[$def['type']])) {
                    $viewVar = $this->_fileVariables[$def['type']];
                }
                $this->_fileVariables[$def['view']] = $viewVar;
            }
        }

        $loaded = null;
        foreach (array(
                     MB_BASEMETADATALOCATION,
                     MB_CUSTOMMETADATALOCATION,
                     MB_WORKINGMETADATALOCATION,
                     MB_HISTORYMETADATALOCATION
                 ) as $type) {
            $this->_sourceFilename = $this->getFileName($view, $moduleName, null, $type);
            if ($view == MB_POPUPSEARCH || $view == MB_POPUPLIST) {
                global $current_language;
                $mod = return_module_language($current_language, $moduleName);
                $layout = $this->_loadFromPopupFile($this->_sourceFilename, $mod, $view);
            } else {
                $layout = $this->_loadFromFile($this->_sourceFilename);
            }
            if (null !== $layout) {
                // merge in the fieldDefinitions from this layout
                $this->_mergeFielddefs($fieldDefinitions, $layout);
                $loaded = $layout;
            }
        }

        if ($loaded === null) {
            switch ($view) {
                case MB_QUICKCREATE:
                    // Special handling for QuickCreates - if we don't have a QuickCreate definition in the usual places, then use an EditView

                    $loaded = $this->_loadFromFile($this->getFileName(
                        MB_EDITVIEW,
                        $this->_moduleName,
                        null,
                        MB_BASEMETADATALOCATION
                    ));

                    if ($loaded === null) {
                        throw new Exception(get_class($this) . ": cannot convert from EditView to QuickCreate for Module $this->_moduleName - definitions for EditView are missing");
                    }

                    // Now change the array index
                    $temp = $loaded [GridLayoutMetaDataParser::$variableMap [MB_EDITVIEW]];
                    unset($loaded [GridLayoutMetaDataParser::$variableMap [MB_EDITVIEW]]);
                    $loaded [GridLayoutMetaDataParser::$variableMap [MB_QUICKCREATE]] = $temp;
                    // finally, save out our new definition so that we have a base record for the history to work from
                    $this->_sourceFilename = $this->getFileName(
                        MB_QUICKCREATE,
                        $this->_moduleName,
                        null,
                        MB_CUSTOMMETADATALOCATION
                    );
                    $this->_saveToFile($this->_sourceFilename, $loaded);
                    $this->_mergeFielddefs($fieldDefinitions, $loaded);
                    break;

                case MB_DASHLETSEARCH:
                case MB_DASHLET:
                    $type = $module->getType();
                    $this->_sourceFilename = $this->getFileName($view, $moduleName, null, MB_CUSTOMMETADATALOCATION);
                    $needSave = false;
                    if (file_exists("custom/modules/{$moduleName}/metadata/" . basename($this->_sourceFilename))) {
                        $loaded = $this->_loadFromFile("custom/modules/{$moduleName}/metadata/" . basename($this->_sourceFilename));
                    } elseif (file_exists(
                        "modules/{$moduleName}/Dashlets/My{$moduleName}Dashlet/My{$moduleName}Dashlet.data.php"
                    )) {
                        $loaded = $this->_loadFromFile("modules/{$moduleName}/Dashlets/My{$moduleName}Dashlet/My{$moduleName}Dashlet.data.php");
                    } else {
                        $loaded = $this->_loadFromFile("include/SugarObjects/templates/$type/metadata/" . basename($this->_sourceFilename));
                        $needSave = true;
                    }
                    if ($loaded === null) {
                        throw new Exception(get_class($this) . ": cannot create dashlet view for module $moduleName - definitions for $view are missing in the SugarObject template for type $type");
                    }
                    $loaded = $this->replaceVariables($loaded, $module);
                    $temp = $this->_moduleName;
                    if ($needSave) {
                        $this->_moduleName = $this->_moduleName . 'Dashlet';
                        $this->_saveToFile(
                            $this->_sourceFilename,
                            $loaded,
                            false
                        ); // write out without the placeholder module_name and object
                        $this->_moduleName = $temp;
                        unset($temp);
                    }
                    $this->_mergeFielddefs($fieldDefinitions, $loaded);
                    break;
                case MB_POPUPLIST:
                case MB_POPUPSEARCH:
                    $type = $module->getType();
                    $this->_sourceFilename = $this->getFileName($view, $moduleName, null, MB_CUSTOMMETADATALOCATION);

                    global $current_language;
                    $mod = return_module_language($current_language, $moduleName);
                    $loadedForWrite = $this->_loadFromPopupFile(
                        "include/SugarObjects/templates/$type/metadata/" . basename($this->_sourceFilename),
                        $mod,
                        $view,
                        true
                    );
                    if ($loadedForWrite === null) {
                        throw new Exception(get_class($this) . ": cannot create popup view for module $moduleName - definitions for $view are missing in the SugarObject template for type $type");
                    }
                    $loadedForWrite = $this->replaceVariables($loadedForWrite, $module);
                    $this->_saveToFile(
                        $this->_sourceFilename,
                        $loadedForWrite,
                        false,
                        true
                    ); // write out without the placeholder module_name and object
                    $loaded = $this->_loadFromPopupFile(
                        "include/SugarObjects/templates/$type/metadata/" . basename($this->_sourceFilename),
                        $mod,
                        $view
                    );
                    $this->_mergeFielddefs($fieldDefinitions, $loaded);
                    break;
                default:

            }
            if ($loaded === null) {
                throw new Exception(get_class($this) . ": view definitions for View $this->_view and Module $this->_moduleName are missing");
            }
        }

        $this->_viewdefs = $loaded;
        // Set the original Viewdefs - required to ensure we don't lose fields from the base layout
        // Check the base location first, then if nothing is there (which for example, will be the case for some QuickCreates, and some mobile layouts - see above)
        // we need to check the custom location where the derived layouts will be
        foreach (array(MB_BASEMETADATALOCATION, MB_CUSTOMMETADATALOCATION) as $type) {
            $sourceFilename = $this->getFileName($view, $moduleName, null, $type);
            if ($view == MB_POPUPSEARCH || $view == MB_POPUPLIST) {
                global $current_language;
                $mod = return_module_language($current_language, $moduleName);
                $layout = $this->_loadFromPopupFile($sourceFilename, $mod, $view);
            } else {
                $layout = $this->_loadFromFile($sourceFilename);
            }
            if (null !== ($layout)) {
                $this->_originalViewdefs = $layout;
                break;
            }
        }
        //For quick create viewdefs, if there is no quickcreatedefs.php under MB_BASEMETADATALOCATION, the original defs is editview defs.
        if ($view == MB_QUICKCREATE) {
            foreach (array(MB_QUICKCREATE, MB_EDITVIEW) as $v) {
                $sourceFilename = $this->getFileName($v, $moduleName, null, MB_BASEMETADATALOCATION);
                if (file_exists($sourceFilename)) {
                    $layout = $this->_loadFromFile($sourceFilename);
                    if (null !== $layout && isset($layout[GridLayoutMetaDataParser::$variableMap[$v]])) {
                        $layout = array(GridLayoutMetaDataParser::$variableMap[MB_QUICKCREATE] => $layout[GridLayoutMetaDataParser::$variableMap[$v]]);
                        break;
                    }
                }
            }

            if (null === $layout) {
                $sourceFilename = $this->getFileName($view, $moduleName, null, MB_CUSTOMMETADATALOCATION);
                $layout = $this->_loadFromFile($sourceFilename);
            }

            if (null !== $layout) {
                $this->_originalViewdefs = $layout;
            }
        }

        $this->_fielddefs = $fieldDefinitions;
        $this->_history = new History($this->getFileName($view, $moduleName, null, MB_HISTORYMETADATALOCATION));
    }

    /**
     * @return string module name
     */
    public function getLanguage()
    {
        return $this->_moduleName;
    }


    /**
     * Save a draft layout
     * @param array $layoutDefinitions Layout definition in the same format as received by the constructor
     */
    public function save($layoutDefinitions)
    {
        //If we are pulling from the History Location, that means we did a restore, and we need to save the history for the previous file.
        if ($this->_sourceFilename === $this->getFileName(
            $this->_view,
            $this->_moduleName,
            null,
            MB_HISTORYMETADATALOCATION
        )
        ) {
            foreach (array(MB_WORKINGMETADATALOCATION, MB_CUSTOMMETADATALOCATION, MB_BASEMETADATALOCATION) as $type) {
                if (file_exists($this->getFileName($this->_view, $this->_moduleName, null, $type))) {
                    $this->_history->append($this->getFileName($this->_view, $this->_moduleName, null, $type));
                    break;
                }
            }
        } else {
            $this->_history->append($this->_sourceFilename);
        }

        $GLOBALS ['log']->debug(get_class($this) . "->save(): writing to " . $this->getFileName(
            $this->_view,
            $this->_moduleName,
            null,
            MB_WORKINGMETADATALOCATION
        ));
        $this->_saveToFile($this->getFileName($this->_view, $this->_moduleName, null, MB_WORKINGMETADATALOCATION), $layoutDefinitions);
    }

    /**
     * Deploy a layout
     * @param array $layoutDefinitions Layout definition in the same format as received by the constructor
     */
    public function deploy($layoutDefinitions)
    {
        if ($this->_sourceFilename === $this->getFileName(
            $this->_view,
            $this->_moduleName,
            null,
            MB_HISTORYMETADATALOCATION
        )
        ) {
            foreach (array(MB_WORKINGMETADATALOCATION, MB_CUSTOMMETADATALOCATION, MB_BASEMETADATALOCATION) as $type) {
                if (file_exists($this->getFileName($this->_view, $this->_moduleName, null, $type))) {
                    $this->_history->append($this->getFileName($this->_view, $this->_moduleName, null, $type));
                    break;
                }
            }
        } else {
            $this->_history->append($this->_sourceFilename);
        }
        // when we deploy get rid of the working file; we have the changes in the MB_CUSTOMMETADATALOCATION so no need for a redundant copy in MB_WORKINGMETADATALOCATION
        // this also simplifies manual editing of layouts. You can now switch back and forth between Studio and manual changes without having to keep these two locations in sync
        $workingFilename = $this->getFileName($this->_view, $this->_moduleName, null, MB_WORKINGMETADATALOCATION);

        if (file_exists($workingFilename)) {
            unlink($this->getFileName($this->_view, $this->_moduleName, null, MB_WORKINGMETADATALOCATION));
        }
        $filename = $this->getFileName($this->_view, $this->_moduleName, null, MB_CUSTOMMETADATALOCATION);
        $GLOBALS ['log']->debug(get_class($this) . "->deploy(): writing to " . $filename);
        $this->_saveToFile($filename, $layoutDefinitions);

        // now clear the cache so that the results are immediately visible
        include_once('include/TemplateHandler/TemplateHandler.php');
        TemplateHandler::clearCache($this->_moduleName);
    }

    /**
     * Construct a full pathname for the requested metadata
     * Can be called statically
     * @param string $view The view type, that is, EditView, DetailView etc
     * @param string $moduleName The name of the module that will use this layout
     * @param string $packageName
     * @param string $type
     * @return string
     */
    public function getFileName($view, $moduleName, $packageName, $type = MB_CUSTOMMETADATALOCATION)
    {
        $pathMap = array(
            MB_BASEMETADATALOCATION => '',
            MB_CUSTOMMETADATALOCATION => 'custom/',
            MB_WORKINGMETADATALOCATION => 'custom/working/',
            MB_HISTORYMETADATALOCATION => 'custom/history/'
        );
        $type = strtolower($type);

        $filenames = array(
            MB_DASHLETSEARCH => 'dashletviewdefs',
            MB_DASHLET => 'dashletviewdefs',
            MB_POPUPSEARCH => 'popupdefs',
            MB_POPUPLIST => 'popupdefs',
            MB_LISTVIEW => 'listviewdefs',
            MB_BASICSEARCH => 'searchdefs',
            MB_ADVANCEDSEARCH => 'searchdefs',
            MB_EDITVIEW => 'editviewdefs',
            MB_DETAILVIEW => 'detailviewdefs',
            MB_QUICKCREATE => 'quickcreatedefs',
        );

        //In a deployed module, we can check for a studio module with file name overrides.
        $sm = StudioModuleFactory::getStudioModule($moduleName);
        foreach ($sm->sources as $file => $def) {
            if (!empty($def['view'])) {
                $filenames[$def['view']] = substr((string) $file, 0, strlen((string) $file) - 4);
            }
        }

        // BEGIN ASSERTIONS
        if (!isset($pathMap [$type])) {
            sugar_die("DeployedMetaDataImplementation->getFileName(): Type $type is not recognized");
        }
        if (!isset($filenames [$view])) {
            sugar_die("DeployedMetaDataImplementation->getFileName(): View $view is not recognized");
        }
        // END ASSERTIONS

        // Construct filename
        return $pathMap [$type] . 'modules/' . $moduleName . '/metadata/' . $filenames [$view] . '.php';
    }

    /**
     * @param $defs
     * @param $module
     * @return array
     */
    private function replaceVariables($defs, $module)
    {
        $var_values = array(
            "<object_name>" => $module->seed->object_name,
            "<_object_name>" => strtolower($module->seed->object_name),
            "<OBJECT_NAME>" => strtoupper($module->seed->object_name),
            "<module_name>" => $module->seed->module_dir,
            '<_module_name>' => strtolower($module->seed->module_dir)
        );

        return $this->recursiveVariableReplace($defs, $module, $var_values);
    }

    /**
     * @return string
     */
    public function getModuleDir()
    {
        return $this->module_dir;
    }

    /**
     * @param array $arr
     * @param $module
     * @param $replacements
     * @return array
     */
    private function recursiveVariableReplace($arr, $module, $replacements)
    {
        $ret = array();
        foreach ($arr as $key => $val) {
            if (is_array($val)) {
                $newkey = $key;
                $val = $this->recursiveVariableReplace($val, $module, $replacements);
                foreach ($replacements as $var => $rep) {
                    $newkey = str_replace($var, $rep, $newkey);
                }
                $ret[$newkey] = $val;
            } else {
                $newkey = $key;
                $newval = $val;
                if (is_string($val)) {
                    foreach ($replacements as $var => $rep) {
                        $newkey = str_replace($var, $rep, $newkey);
                        $newval = str_replace($var, $rep, (string) $newval);
                    }
                }
                $ret[$newkey] = $newval;
            }
        }

        return $ret;
    }

    /**
     * This is just a wrapper to the private method _saveToFile
     * @param  $file    the file name to save to
     * @param  $defs    the defs to save to the file
     * @return void
     */
    public function saveToFile($file, $defs)
    {
        $this->_saveToFile($file, $defs);
    }
}
