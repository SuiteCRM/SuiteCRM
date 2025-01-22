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



#[\AllowDynamicProperties]
class ModuleBuilderParser
{
    public $_defMap; // private - mapping from view to variable name inside the viewdef file
    public $_variables = array(); // private - set of additional variables (other than the viewdefs) found in the viewdef file that need to be added to the file again when it is saved - used by ModuleBuilder

    public function __construct()
    {
        $this->_defMap = array('listview'=>'listViewDefs','searchview'=>'searchdefs','editview'=>'viewdefs','detailview'=>'viewdefs','quickcreate'=>'viewdefs');
    }



    /*
     * Initialize this parser
     */
    public function init()
    {
    }

    /*
     * Dummy function used to ease the transition to the new parser structure
     */
    public function populateFromPost()
    {
    }

    public function _loadFromFile($view, $file, $moduleName)
    {
        $variables = array();
        if (! file_exists($file)) {
            $this->_fatalError("ModuleBuilderParser: required viewdef file {$file} does not exist");
        }
        $GLOBALS['log']->info('ModuleBuilderParser->_loadFromFile(): file='.$file);
        require($file); // loads in a $viewdefs

        // Check to see if we have the module name set as a variable rather than embedded in the $viewdef array
        // If we do, then we have to preserve the module variable when we write the file back out
        // This is a format used by ModuleBuilder templated modules to speed the renaming of modules
        // Traditional Sugar modules don't use this format
        // We must do this in ParserModifyLayout (rather than just in ParserBuildLayout) because we might be editing the layout of a MB created module in Studio after it has been deployed
        $moduleVariables = array('module_name','_module_name', 'OBJECT_NAME', '_object_name');
        foreach ($moduleVariables as $name) {
            if (isset(${$name})) {
                $variables[$name] = ${$name};
            }
        }
        $viewVariable = $this->_defMap[strtolower($view)];
        // Now tidy up the module name in the viewdef array
        // MB created definitions store the defs under packagename_modulename and later methods that expect to find them under modulename will fail
        $defs = ${$viewVariable};

        if (isset($variables['module_name'])) {
            $mbName = $variables['module_name'];
            if ($mbName != $moduleName) {
                $GLOBALS['log']->debug('ModuleBuilderParser->_loadFromFile(): tidying module names from '.$mbName.' to '.$moduleName);
                $defs[$moduleName] = $defs[$mbName];
                unset($defs[$mbName]);
            }
        }
        //	    $GLOBALS['log']->debug('ModuleBuilderParser->_loadFromFile(): '.print_r($defs,true));
        return (array('viewdefs' => $defs, 'variables' => $variables));
    }

    public function handleSave($file, $view, $moduleName, $defs)
    {
    }


    /*
     * Save the new layout
     */
    public function _writeToFile($file, $view, $moduleName, $defs, $variables)
    {
        if (file_exists($file)) {
            unlink($file);
        }

        mkdir_recursive(dirname((string) $file)) ;
        $GLOBALS['log']->debug("ModuleBuilderParser->_writeFile(): file=".$file);
        $useVariables = ((is_countable($variables) ? count($variables) : 0)>0);
        if ($fh = @sugar_fopen($file, 'w')) {
            $out = "<?php\n";
            if ($useVariables) {
                // write out the $<variable>=<modulename> lines
                foreach ($variables as $key=>$value) {
                    $out .= "\$$key = '".$value."';\n";
                }
            }

            // write out the defs array itself
            switch (strtolower($view)) {
                    case 'editview':
                    case 'detailview':
                    case 'quickcreate':
                        $defs = array($view => $defs);
                        break;
                    default:
                        break;
                }
            $viewVariable = $this->_defMap[strtolower($view)];
            $out .= "\$$viewVariable = ";
            $out .= ($useVariables) ? "array (\n\$module_name =>\n".var_export_helper($defs) : var_export_helper(array($moduleName => $defs));

            // tidy up the parenthesis
            if ($useVariables) {
                $out .= "\n)";
            }
            $out .= ";\n?>\n";

//           $GLOBALS['log']->debug("parser.modifylayout.php->_writeFile(): out=".print_r($out,true));
            fwrite($fh, $out);
            sugar_fclose($fh);
        } else {
            $GLOBALS['log']->fatal("ModuleBuilderParser->_writeFile() Could not write new viewdef file ".$file);
        }
    }


    public function _fatalError($msg)
    {
        $GLOBALS ['log']->fatal($msg);
        echo $msg;
        sugar_cleanup();
        die();
    }
}
