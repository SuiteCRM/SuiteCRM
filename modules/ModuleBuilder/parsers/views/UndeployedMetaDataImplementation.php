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


require_once 'modules/ModuleBuilder/MB/ModuleBuilder.php' ;
require_once 'modules/ModuleBuilder/parsers/views/AbstractMetaDataImplementation.php' ;
require_once 'modules/ModuleBuilder/parsers/views/MetaDataImplementationInterface.php' ;
require_once 'modules/ModuleBuilder/parsers/views/ListLayoutMetaDataParser.php' ;
require_once 'modules/ModuleBuilder/parsers/views/GridLayoutMetaDataParser.php' ;
require_once 'modules/ModuleBuilder/parsers/constants.php' ;

#[\AllowDynamicProperties]
class UndeployedMetaDataImplementation extends AbstractMetaDataImplementation implements MetaDataImplementationInterface
{
    protected $_packageName ;

    /*
      * Constructor
      * @param string $view
      * @param string $moduleName
      * @throws Exception Thrown if the provided view doesn't exist for this module
      */

    public function __construct($view, $moduleName, $packageName)
    {

        // BEGIN ASSERTIONS
        if (! isset($this->_fileVariables [ $view ])) {
            sugar_die(get_class($this) . ": View $view is not supported") ;
        }
        // END ASSERTIONS

        $this->_view = strtolower($view) ;
        $this->_moduleName = $moduleName ;
        $this->_packageName = $packageName ;

        //get the bean from ModuleBuilder
        $mb = new ModuleBuilder() ;
        $this->module = $module = & $mb->getPackageModule($packageName, $moduleName) ;
        $pak = $mb->getPackage($packageName);
        $module->mbvardefs->updateVardefs() ;

        // Set the list of fields associated with this module
        $fielddefs = array_change_key_case($module->mbvardefs->vardefs [ 'fields' ]) ;

        // Set the global mod_strings directly as Sugar does not automatically load the language files for undeployed modules (how could it?)
        $selected_lang = 'en_us';
        if (isset($GLOBALS['current_language']) &&!empty($GLOBALS['current_language'])) {
            $selected_lang = $GLOBALS['current_language'];
        }
        $GLOBALS [ 'mod_strings' ] = array_merge($GLOBALS [ 'mod_strings' ], $module->getModStrings($selected_lang)) ;

        //Load relationshhip based fields and labels
        $moduleRels = $pak->getRelationshipsForModule($moduleName);
        foreach ($moduleRels as $rName => $rel) {
            $varDefsSet = $rel->buildVardefs();
            if (!empty($varDefsSet[$module->key_name])) {
                foreach ($varDefsSet[$module->key_name] as $def) {
                    $fielddefs[$def['name']] = $def;
                }
            }
            $labels = $rel->buildLabels();
            foreach ($labels as $def) {
                if ($def['module'] == $module->key_name) {
                    $GLOBALS [ 'mod_strings' ][$def['system_label']] = $def['display_label'];
                }
            }
        }

        $loaded = null ;
        foreach (array( MB_BASEMETADATALOCATION , MB_HISTORYMETADATALOCATION ) as $type) {
            $this->_sourceFilename = $this->getFileNameInPackage($view, $moduleName, $packageName, $type) ;
            if ($view == MB_POPUPSEARCH || $view == MB_POPUPLIST) {
                $layout = $this->_loadFromPopupFile($this->_sourceFilename, null, $view);
            } else {
                $layout = $this->_loadFromFile($this->_sourceFilename);
            }
            if (null !== $layout) {
                // merge in the fielddefs from this layout
                $this->_mergeFielddefs($fielddefs, $layout) ;
                $loaded = $layout ;
            }
        }

        if ($loaded === null) {
            throw new Exception(get_class($this) . ": view definitions for View $this->_view and Module $this->_moduleName are missing");
        }

        $this->_viewdefs = $loaded ;
        $sourceFilename = $this->getFileNameInPackage($view, $moduleName, $packageName, MB_BASEMETADATALOCATION);
        if ($view == MB_POPUPSEARCH || $view == MB_POPUPLIST) {
            $layout = $this->_loadFromPopupFile($sourceFilename, null, $view);
        } else {
            $layout = $this->_loadFromFile($sourceFilename) ;
        }
        $this->_originalViewdefs = $layout ;
        $this->_fielddefs = $fielddefs ;
        $this->_history = new History($this->getFileNameInPackage($view, $moduleName, $packageName, MB_HISTORYMETADATALOCATION)) ;
    }

    public function getLanguage()
    {
        return $this->_packageName . $this->_moduleName ;
    }

    /*
     * Deploy a layout
     * @param array defs    Layout definition in the same format as received by the constructor
     */
    public function deploy($defs)
    {
        //If we are pulling from the History Location, that means we did a restore, and we need to save the history for the previous file.
        if ($this->_sourceFilename === $this->getFileName($this->_view, $this->_moduleName, MB_HISTORYMETADATALOCATION)
        && file_exists($this->getFileName($this->_view, $this->_moduleName, MB_BASEMETADATALOCATION))) {
            $this->_history->append($this->getFileName($this->_view, $this->_moduleName, MB_BASEMETADATALOCATION));
        } else {
            $this->_history->append($this->_sourceFilename) ;
        }
        $filename = $this->getFileName($this->_view, $this->_moduleName, MB_BASEMETADATALOCATION);
        $GLOBALS ['log']->debug(get_class($this) . "->deploy(): writing to " . $filename);
        $this->_saveToFile($filename, $defs);
    }

    /**
     * Construct a full pathname for the requested metadata
     *
     * @param string $view           The view type, that is, EditView, DetailView etc
     * @param string $moduleName     The name of the module that will use this layout
     * @param string $packageName
     * @param string $type
     * @return string               The file name
     */
    public function getFileName($view, $moduleName, $packageName, $type = MB_BASEMETADATALOCATION)
    {
        return $this->getFileNameInPackage($view, $moduleName, $this->_packageName, $type);
    }

    /**
     * Construct a full pathname for the requested metadata, in a specific package
     *
     * @param string $view           The view type, that is, EditView, DetailView etc
     * @param string $moduleName     The name of the module that will use this layout
     * @param string $packageName    The name of the package to use
     * @param string $type
     * @return string               The file name
     */
    public function getFileNameInPackage($view, $moduleName, $packageName, $type = MB_BASEMETADATALOCATION)
    {
        $type = strtolower($type) ;

        // BEGIN ASSERTIONS
        if ($type != MB_BASEMETADATALOCATION && $type != MB_HISTORYMETADATALOCATION) {
            // just warn rather than die
            $GLOBALS [ 'log' ]->warning("UndeployedMetaDataImplementation->getFileName(): view type $type is not recognized") ;
        }
        // END ASSERTIONS

        $filenames = array(  	MB_DASHLETSEARCH => 'dashletviewdefs',
                                MB_DASHLET => 'dashletviewdefs',
                                MB_LISTVIEW => 'listviewdefs' ,
                                MB_BASICSEARCH => 'searchdefs' ,
                                MB_ADVANCEDSEARCH => 'searchdefs' ,
                                MB_EDITVIEW => 'editviewdefs' ,
                                MB_DETAILVIEW => 'detailviewdefs' ,
                                MB_QUICKCREATE => 'quickcreatedefs',
                                MB_POPUPSEARCH => 'popupdefs',
                                MB_POPUPLIST => 'popupdefs',
                                ) ;

        switch ($type) {
            case MB_HISTORYMETADATALOCATION:
                return 'custom/history/modulebuilder/packages/' . $packageName . '/modules/' . $moduleName . '/metadata/' . $filenames [ $view ] . '.php' ;
            default:
                // get the module again, all so we can call this method statically without relying on the module stored in the class variables
                $mb = new ModuleBuilder() ;
                $module = & $mb->getPackageModule($packageName, $moduleName) ;
                return $module->getModuleDir() . '/metadata/' . $filenames [ $view ] . '.php' ;
        }
    }

    public function getModuleDir()
    {
        return $this->module->key_name;
    }
}
