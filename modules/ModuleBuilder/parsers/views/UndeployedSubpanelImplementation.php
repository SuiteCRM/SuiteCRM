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

require_once 'modules/ModuleBuilder/parsers/views/MetaDataImplementationInterface.php' ;
require_once 'modules/ModuleBuilder/parsers/views/AbstractMetaDataImplementation.php' ;
require_once 'modules/ModuleBuilder/parsers/constants.php' ;

/**
 * Class UndeployedSubpanelImplementation
 *
 * Changes to AbstractSubpanelImplementation for DeployedSubpanels
 * The main differences are in the load and save of the definitions
 * For subpanels we must make use of the SubPanelDefinitions class to do this; this also means that the history mechanism,
 * which tracks files, not objects, needs us to create an intermediate file representation of the definition that it can manage and restore
 */
#[\AllowDynamicProperties]
class UndeployedSubpanelImplementation extends AbstractMetaDataImplementation implements MetaDataImplementationInterface
{
    public const HISTORYFILENAME = 'restored.php' ;
    public const HISTORYVARIABLENAME = 'layout_defs' ;

    /**
     * Constructor
     * @param string $subpanelName   The name of this subpanel
     * @param string $moduleName     The name of the module to which this subpanel belongs
     * @param string $packageName    If not empty, the name of the package to which this subpanel belongs
     */
    public function __construct($subpanelName, $moduleName, $packageName)
    {
        $this->_subpanelName = $subpanelName ;
        $this->_moduleName = $moduleName ;

        // TODO: history
        $this->historyPathname = 'custom/history/modulebuilder/packages/' . $packageName . '/modules/' . $moduleName . '/metadata/' . self::HISTORYFILENAME ;
        $this->_history = new History($this->historyPathname) ;

        //get the bean from ModuleBuilder
        $mb = new ModuleBuilder() ;
        $this->module = & $mb->getPackageModule($packageName, $moduleName) ;
        $this->module->mbvardefs->updateVardefs() ;
        $this->_fielddefs = & $this->module->mbvardefs->vardefs [ 'fields' ] ;

        $templates = & $this->module->config['templates'];
        $template_def="";
        foreach ($templates as $template => $a) {
            if ($a===1) {
                $template_def = $template;
            }
        }

        $subpanel_layout = [];
        $template_subpanel_def = 'include/SugarObjects/templates/'.$template_def. '/metadata/subpanels/default.php';
        if (file_exists($template_subpanel_def)) {
            include($template_subpanel_def);
            if (!empty($subpanel_layout['list_fields'])) {
                $this->_mergeFielddefs($this->_fielddefs, $subpanel_layout['list_fields']);
            }
        }

        $subpanel_layout = $this->module->getAvailibleSubpanelDef($this->_subpanelName) ;
        $this->_viewdefs = & $subpanel_layout [ 'list_fields' ] ;
        $this->_mergeFielddefs($this->_fielddefs, $this->_viewdefs);

        // Set the global mod_strings directly as Sugar does not automatically load the language files for undeployed modules (how could it?)
        $selected_lang = 'en_us';
        if (isset($GLOBALS['current_language']) &&!empty($GLOBALS['current_language'])) {
            $selected_lang = $GLOBALS['current_language'];
        }
        $GLOBALS [ 'mod_strings' ] = array_merge($GLOBALS [ 'mod_strings' ], $this->module->getModStrings($selected_lang)) ;
    }

    /**
     * @return string
     */
    public function getLanguage()
    {
        return ""; // '' is the signal to translate() to use the global mod_strings
    }

    /**
     * Save a subpanel
     * @param array $layoutDefinitions    Layout definition in the same format as received by the constructor
     */
    public function deploy($layoutDefinitions)
    {
        $outputDefs = $this->module->getAvailibleSubpanelDef($this->_subpanelName) ;
        // first sort out the historical record...
        // copy the definition to a temporary file then let the history object add it
        write_array_to_file(self::HISTORYVARIABLENAME, $outputDefs, $this->historyPathname, 'w', '') ;
        $this->_history->append($this->historyPathname) ;
        // no need to unlink the temporary file as being handled by in history->append()
        //unlink ( $this->historyPathname ) ;

        $outputDefs [ 'list_fields' ] = $layoutDefinitions ;
        $this->_viewdefs = $layoutDefinitions ;
        $this->module->saveAvailibleSubpanelDef($this->_subpanelName, $outputDefs) ;
    }

    public function getFileName($view, $moduleName, $packageName, $type = MB_CUSTOMMETADATALOCATION)
    {
        // TODO: Implement getFileName() method.
    }
}
