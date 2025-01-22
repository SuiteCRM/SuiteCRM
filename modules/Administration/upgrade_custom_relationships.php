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


/**
 * Searches through the installed relationships to find broken self referencing one-to-many relationships
 * (wrong field used in the subpanel, and the left link not marked as left)
 */
function upgrade_custom_relationships($modules = array())
{
    global $current_user, $moduleList;
    if (!is_admin($current_user)) {
        sugar_die($GLOBALS['app_strings']['ERR_NOT_ADMIN']);
    }
    
    require_once("modules/ModuleBuilder/parsers/relationships/DeployedRelationships.php");
    require_once("modules/ModuleBuilder/parsers/relationships/OneToManyRelationship.php");
    
    if (empty($modules)) {
        $modules = $moduleList;
    }
    
    foreach ($modules as $module) {
        $depRels = new DeployedRelationships($module);
        $relList = $depRels->getRelationshipList();
        foreach ($relList as $relName) {
            $relObject = $depRels->get($relName);
            $def = $relObject->getDefinition();
            //We only need to fix self referencing one to many relationships
            if ($def['lhs_module'] == $def['rhs_module'] && $def['is_custom'] && $def['relationship_type'] == "one-to-many") {
                $layout_defs = array();
                if (!is_dir("custom/Extension/modules/$module/Ext/Layoutdefs") || !is_dir("custom/Extension/modules/$module/Ext/Vardefs")) {
                    continue;
                }
                //Find the extension file containing the vardefs for this relationship
                foreach (scandir("custom/Extension/modules/$module/Ext/Vardefs") as $file) {
                    if (substr((string) $file, 0, 1) != "." && strtolower(substr((string) $file, -4)) == ".php") {
                        $dictionary = array($module => array("fields" => array()));
                        $filePath = "custom/Extension/modules/$module/Ext/Vardefs/$file";
                        include($filePath);
                        if (isset($dictionary[$module]["fields"][$relName])) {
                            $rhsDef = $dictionary[$module]["fields"][$relName];
                            //Update the vardef for the left side link field
                            if (!isset($rhsDef['side']) || $rhsDef['side'] != 'left') {
                                $rhsDef['side'] = 'left';
                                $fileContents = file_get_contents($filePath);
                                $out = preg_replace(
                                    '/\$dictionary[\w"\'\[\]]*?' . $relName . '["\'\[\]]*?\s*?=\s*?array\s*?\(.*?\);/s',
                                    '$dictionary["' . $module . '"]["fields"]["' . $relName . '"]=' . var_export_helper($rhsDef) . ";",
                                    $fileContents
                                );
                                sugar_file_put_contents($filePath, $out);
                            }
                        }
                    }
                }
                //Find the extension file containing the subpanel definition for this relationship
                foreach (scandir("custom/Extension/modules/$module/Ext/Layoutdefs") as $file) {
                    if (substr((string) $file, 0, 1) != "." && strtolower(substr((string) $file, -4)) == ".php") {
                        $layout_defs = array($module => array("subpanel_setup" => array()));
                        $filePath = "custom/Extension/modules/$module/Ext/Layoutdefs/$file";
                        include($filePath);
                        foreach ($layout_defs[$module]["subpanel_setup"] as $key => $subDef) {
                            if ($layout_defs[$module]["subpanel_setup"][$key]['get_subpanel_data'] == $relName) {
                                $fileContents = file_get_contents($filePath);
                                $out = preg_replace(
                                    '/[\'"]get_subpanel_data[\'"]\s*=>\s*[\'"]' . $relName . '[\'"],/s',
                                    "'get_subpanel_data' => '{$def["join_key_lhs"]}',",
                                    $fileContents
                                );
                                sugar_file_put_contents($filePath, $out);
                            }
                        }
                    }
                }
            }
        }
    }
}

if (isset($_REQUEST['execute']) && $_REQUEST['execute']) {
    upgrade_custom_relationships();
}
