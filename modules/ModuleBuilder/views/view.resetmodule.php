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

require_once('modules/ModuleBuilder/MB/AjaxCompose.php') ;
require_once('modules/ModuleBuilder/MB/ModuleBuilder.php') ;
require_once('modules/ModuleBuilder/Module/StudioModule.php') ;
require_once('modules/ModuleBuilder/Module/StudioBrowser.php') ;
require_once('modules/DynamicFields/DynamicField.php') ;
require_once 'modules/ModuleBuilder/Module/StudioModuleFactory.php' ;
require_once 'modules/ModuleBuilder/parsers/views/DeployedMetaDataImplementation.php';

#[\AllowDynamicProperties]
class ViewResetmodule extends SugarView
{
    public static array $exceptionFields = [
        'jjwg_maps_geocode_status_c',
        'jjwg_maps_address_c',
        'jjwg_maps_lng_c',
        'jjwg_maps_lat_c'
    ];
    /**
     * @see SugarView::_getModuleTitleParams()
     */
    protected function _getModuleTitleParams($browserTitle = false)
    {
        global $mod_strings;

        return array(
           translate('LBL_MODULE_NAME', 'Administration'),
           ModuleBuilderController::getModuleTitle(),
           );
    }

    public function display()
    {
        $moduleName = $this->module = $_REQUEST['view_module'];
        if (isset($_REQUEST['handle']) && $_REQUEST['handle'] == "execute") {
            return $this->handleSave();
        }

        $ajax = new AjaxCompose() ;
        $ajax->addCrumb(translate('LBL_STUDIO'), 'ModuleBuilder.main("studio")') ;
        $ajax->addCrumb(translate($moduleName), 'ModuleBuilder.getContent("module=ModuleBuilder&action=wizard&view_module=' . $moduleName . '")') ;
        $ajax->addCrumb(translate('LBL_RESET') . " " . translate($moduleName), '') ;

        $smarty = new Sugar_Smarty() ;
        $smarty->assign("module", $moduleName);
        $smarty->assign("actions", array(
            array("name" => "relationships", "label" => translate("LBL_CLEAR_RELATIONSHIPS")),
            array("name" => "fields", "label" => translate("LBL_REMOVE_FIELDS")),
            array("name" => "layouts", "label" => translate("LBL_RESET_LAYOUTS")),
            array("name" => "labels", "label" => translate("LBL_RESET_LABELS")),
            array("name" => "extensions", "label" => translate("LBL_CLEAR_EXTENSIONS")),
        ));

        $ajax->addSection(
            'center',
            "Reset ". translate($moduleName),
            $smarty->fetch('modules/ModuleBuilder/tpls/resetModule.tpl') //"This works now"
        ) ;

        echo $ajax->getJavascript() ;
    }

    public function handleSave()
    {
        $out = "<script>ajaxStatus.flashStatus(SUGAR.language.get('app_strings', 'LBL_REQUEST_PROCESSED'), 2000);</script>";

        if (!empty($_REQUEST['relationships'])) {
            $out .= $this->removeCustomRelationships();
        }

        if (!empty($_REQUEST['fields'])) {
            $out .= $this->removeCustomFields();
        }

        if (!empty($_REQUEST['layouts'])) {
            $out .= $this->removeCustomLayouts();
        }

        if (!empty($_REQUEST['labels'])) {
            $out .= $this->removeCustomLabels();
        }

        if (!empty($_REQUEST['extensions'])) {
            $out .= $this->removeCustomExtensions();
        }


        $out .= "Complete!";

        $ajax = new AjaxCompose() ;

        $ajax->addCrumb(translate('LBL_STUDIO'), 'ModuleBuilder.main("studio")') ;
        $ajax->addCrumb(translate($this->module), 'ModuleBuilder.getContent("module=ModuleBuilder&action=wizard&view_module=' . $this->module . '")') ;
        $ajax->addCrumb("Reset ". translate($this->module), '') ;


        $ajax->addSection(
            'center',
            "Reset ". translate($this->module),
            $out
        ) ;

        echo $ajax->getJavascript() ;
    }

    /**
     * Removes all custom fields created in studio
     *
     * @return html output record of the field deleted
     */
    public function removeCustomFields()
    {
        $moduleName = $this->module;
        $class_name = $GLOBALS [ 'beanList' ] [ $moduleName ] ;
        require_once($GLOBALS [ 'beanFiles' ] [ $class_name ]) ;
        $seed = new $class_name() ;
        $df = new DynamicField($moduleName) ;
        $df->setup($seed) ;


        $module = StudioModuleFactory::getStudioModule($moduleName) ;
        $customFields = array();
        foreach ($seed->field_defs as $def) {
            if (isset($def['source']) && $def['source'] == 'custom_fields'
                && (!in_array($def['name'], ViewResetmodule::$exceptionFields))
                || (isTrue($def['resetFieldInStudio'] ?? false))) {
                $field = $df->getFieldWidget($moduleName, $def['name']);
                $field->delete($df) ;

                $module->removeFieldFromLayouts($def['name']);
                $customFields[] = $def['name'];
            }
        }
        $out = "";
        foreach ($customFields as $field) {
            $out .= "Removed field $field<br/>";
        }
        return ($out);
    }

    /**
     * Removes the metadata files for all known studio layouts.
     *
     * @return html output record of the files deleted
     */
    public function removeCustomLayouts()
    {
        $module = StudioModuleFactory::getStudioModule($this->module) ;
        $sources = $module->getViewMetadataSources();

        $out = "";
        foreach ($sources as $view) {
            $deployedMetaDataImplementation = new DeployedMetaDataImplementation($view['type'], $this->module);
            $file = $deployedMetaDataImplementation->getFileName($view['type'], $this->module, null);
            if (file_exists($file)) {
                unlink($file);
                $out .= "Removed layout {$view['type']}.php<br/>";
            }
        }

        // now clear the cache
        include_once('include/TemplateHandler/TemplateHandler.php') ;
        TemplateHandler::clearCache($this->module) ;

        return $out;
    }

    /**
     * Removes all custom relationships containing this module
     *
     * @return html output record of the files deleted
     */
    public function removeCustomRelationships()
    {
        require_once 'modules/ModuleBuilder/parsers/relationships/DeployedRelationships.php' ;
        $out = "";
        $madeChanges = false;
        $relationships = new DeployedRelationships($this->module) ;

        foreach ($relationships->getRelationshipList() as $relationshipName) {
            $rel = $relationships->get($relationshipName)->getDefinition() ;
            if ($rel [ 'is_custom' ] || (isset($rel [ 'from_studio' ]) && $rel [ 'from_studio' ])) {
                $relationships->delete($relationshipName);
                $out .= "Removed relationship $relationshipName<br/>";
            }
        }
        if ($madeChanges) {
            $relationships->save() ;
        }

        return $out;
    }

    public function removeCustomLabels()
    {
        $out = "";
        $languageDir = "custom/modules/{$this->module}/language";
        if (is_dir($languageDir)) {
            $files = scandir($languageDir);
            foreach ($files as $langFile) {
                if (substr((string) $langFile, 0, 1) == '.') {
                    continue;
                }
                $language = substr((string) $langFile, 0, strlen((string) $langFile) - 9);
                unlink($languageDir . "/" . $langFile);

                LanguageManager::clearLanguageCache($this->module, $language) ;
                $out .= "Removed language file $langFile<br/>";
            }
        }

        return $out;
    }

    public function removeCustomExtensions()
    {
        $out = "";
        $extDir = "custom/Extension/modules/{$this->module}";
        if (is_dir($extDir)) {
            rmdir_recursive($extDir);
            require_once('modules/Administration/QuickRepairAndRebuild.php') ;
            $rac = new RepairAndClear() ;
            $rac->repairAndClearAll(array( 'clearAll' ), array( $this->module ), true, false) ;
            $rac->rebuildExtensions();
            $out .= "Cleared extensions for {$this->module}<br/>";
        }

        return $out;
    }
}
