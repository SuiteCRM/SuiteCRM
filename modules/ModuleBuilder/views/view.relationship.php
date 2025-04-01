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
require_once('modules/ModuleBuilder/Module/StudioModuleFactory.php') ;
require_once('modules/ModuleBuilder/Module/StudioBrowser.php') ;
require_once('modules/ModuleBuilder/parsers/constants.php') ;
require_once 'modules/ModuleBuilder/parsers/relationships/RelationshipFactory.php' ;

#[\AllowDynamicProperties]
class ViewRelationship extends SugarView
{
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

    public function overrideDefinitionFromPOST(
        $definition
        ) {
        require_once 'modules/ModuleBuilder/parsers/relationships/AbstractRelationship.php' ;
        foreach (AbstractRelationship::$definitionKeys as $key) {
            if (!empty($_REQUEST ['ajaxLoad']) && in_array($key, array('label', 'rhs_label', 'lhs_label'))) {
                continue;
            }
            if (! empty($_REQUEST [ $key ])) {
                $definition [ $key ] = $_REQUEST [ $key ] ;
            }
        }
        return $definition ;
    }

    public function display()
    {
        $selected_lang = (!empty($_REQUEST['relationship_lang'])?$_REQUEST['relationship_lang']:$_SESSION['authenticated_user_language']);
        $this->smarty = new Sugar_Smarty() ;
        $ac = new AjaxCompose() ;
        $this->fromModuleBuilder = isset($_REQUEST [ 'MB' ]) || (!empty($_REQUEST [ 'view_package' ]) && $_REQUEST [ 'view_package' ] != 'studio') ;
        $this->smarty->assign('fromModuleBuilder', $this->fromModuleBuilder) ;

        if (!$this->fromModuleBuilder) {
            $module = StudioModuleFactory::getStudioModule($_REQUEST [ 'view_module' ]) ;
            $moduleName = $_REQUEST [ 'view_module' ] ;
            $fields = $module->fields ;
            require_once 'modules/ModuleBuilder/parsers/relationships/DeployedRelationships.php' ;
            $relatableModules = DeployedRelationships::findRelatableModules() ;
            $appStrings = return_app_list_strings_language($selected_lang) ;
            $modStrings = return_module_language($selected_lang, $_REQUEST [ 'view_module' ], true) ;
            $appStrings = $appStrings['moduleList'];
        } else {
            $mb = new ModuleBuilder() ;
            $mb->getPackages();
            //display the latest module name rather than what is in or not in the loaded app_list_strings.
            $mb->getPackage($_REQUEST ['view_package'])->loadModuleTitles();
            $module = $mb->getPackageModule($_REQUEST [ 'view_package' ], $_REQUEST [ 'view_module' ]) ;
            $moduleName = empty($module->key_name) ? $module->getModuleName() : $module->key_name;
            $this->smarty->assign('view_package', $_REQUEST [ 'view_package' ]) ;
            $mbvardefs = $module->getVardefs() ;
            $fields = $mbvardefs [ 'fields' ] ;
            require_once 'modules/ModuleBuilder/parsers/relationships/UndeployedRelationships.php' ;
            $relatableModules = UndeployedRelationships::findRelatableModules() ;
            $appStrings = $module->getModStrings($selected_lang) ;
        }

        ksort($relatableModules) ;
        $lhs_subpanels = $module->getProvidedSubpanels() ;
        // Fix to re-add sorting of the subpanel names so that the 'default' subpanel always appears first in the list.
        // This assumes that subpanels are usually named ForXYZ which is the case currently, and hence 'default' will be sorted first.
        //I f this assumption is incorrect, then a better solution would be to remove 'default' from the subpanel list, then sort, and finally array_unshift it back on.
        natcasesort($lhs_subpanels);

        $cardinality = array( MB_ONETOONE => translate('LBL_ONETOONE') , MB_ONETOMANY => translate('LBL_ONETOMANY') , MB_MANYTOONE=> translate('LBL_MANYTOONE'), MB_MANYTOMANY => translate('LBL_MANYTOMANY'), ) ;

        if (!$this->fromModuleBuilder) {
            unset($cardinality[MB_MANYTOONE]);
        }

        $relationships = $module->getRelationships() ;

        // if a description for this relationship already exists, then load it so it can be modified
        if (! empty($_REQUEST [ 'relationship_name' ])) {
            $relationship = $relationships->get($_REQUEST [ 'relationship_name' ]) ;
            $relationship->setName($_REQUEST [ 'relationship_name' ]);
            $definition = $relationship->getDefinition();
            if (!$this->fromModuleBuilder) {
                $modStrings = return_module_language($selected_lang, $relationship->rhs_module, true) ;
                $definition['lhs_label'] = isset($modStrings[$relationship->getTitleKey()])?$modStrings[$relationship->getTitleKey()] : $relationship->lhs_module;
                $modStrings = return_module_language($selected_lang, $relationship->lhs_module, true) ;
                $definition['rhs_label'] = isset($modStrings[$relationship->getTitleKey(true)])?$modStrings[$relationship->getTitleKey(true)] : $relationship->rhs_module ;
            } else {
                #30624
                if (!empty($_REQUEST['rhs_module'])) {
                    $definition['rhs_label'] = $_REQUEST['rhs_module'];
                }
            }
        } else {
            $definition = array( ) ;
            $firstModuleDefinition = [key($relatableModules), current($relatableModules)];
            $definition [ 'rhs_module' ] = $firstModuleDefinition[0];
            $definition [ 'lhs_module' ] = $moduleName ;
            $definition [ 'lhs_label' ] = translate($moduleName);
            $definition [ 'relationship_type' ] = MB_MANYTOMANY ;
        }
        // load the relationship from post - required as we can call view.relationship.php from Ajax when changing the rhs_module for example
        $definition = $this->overrideDefinitionFromPOST($definition) ;

        if (empty($definition ['rhs_label'])) {
            $definition ['rhs_label'] = translate($definition [ 'rhs_module' ]);
        }
        if (empty($definition ['lhs_label'])) {
            $definition ['lhs_label'] = translate($definition [ 'lhs_module' ]);
        }
        $relationship = RelationshipFactory::newRelationship($definition) ;

        $rhs_subpanels = $relatableModules [ $relationship->rhs_module ] ;
        // Fix to re-add sorting of the subpanel names so that the 'default' subpanel always appears first in the list. This assumes that subpanels are usually named ForXYZ which is the case currently, and hence 'default' will be sorted first. If this assumption is incorrect, then a better solution would be to remove 'default' from the subpanel list, then sort, and finally array_unshift it back on.
        natcasesort($rhs_subpanels);

        if (empty($_REQUEST [ 'relationship_name' ])) {
            // tidy up the options for the view based on the modules participating in the relationship and the cardinality
            // some modules (e.g., Knowledge Base/KBDocuments) lack subpanels. That means they can't be the lhs of a 1-many or many-many, or the rhs of a many-many for example

            // fix up the available cardinality options
            $relationship_type = $relationship->getType() ;
            if ((is_countable($lhs_subpanels) ? count($lhs_subpanels) : 0) == 0 || (is_countable($rhs_subpanels) ? count($rhs_subpanels) : 0) == 0) {
                unset($cardinality [ MB_MANYTOMANY ]) ;
            }
            if ((is_countable($rhs_subpanels) ? count($rhs_subpanels) : 0) == 0) {
                unset($cardinality [ MB_ONETOMANY ]) ;
            }

            if (isset($definition['rhs_module']) && $definition['rhs_module'] == 'Activities') {
                $cardinality = array( MB_ONETOMANY => translate('LBL_ONETOMANY'));
            }
            //Bug 23139, Campaigns module current cannot display custom subpanels, so we need to ban it from any
            //relationships that would require a new subpanel to be shown in Campaigns.
            if (isset($definition['lhs_module']) && $definition['lhs_module'] == 'Campaigns') {
                unset($cardinality [ MB_MANYTOMANY ]) ;
                unset($cardinality [ MB_ONETOMANY ]) ;
            }
            if (isset($definition['rhs_module']) && $definition['rhs_module'] == 'Campaigns' && isset($cardinality [ MB_MANYTOMANY ])) {
                unset($cardinality [ MB_MANYTOMANY ]) ;
                unset($cardinality [ MB_MANYTOONE ]);
            }
            if (! isset($cardinality[$relationship->getType()])) {
                end($cardinality) ;
                $definition [ 'relationship_type' ] = key($cardinality) ;
                $relationship = RelationshipFactory::newRelationship($definition) ;
            }

            $this->smarty->assign('is_new', true) ;
        } else {
            $this->smarty->assign('is_new', false) ;
        }

        //Remove Activities if one-to-many is not availible
        if (!isset($cardinality [ MB_ONETOMANY ]) && isset($relatableModules['Activities'])) {
            unset($relatableModules['Activities']);
        }


        // now enforce the relationship_only requirement - that is, only construct the underlying relationship and link fields, and not the UI, if the subpanel code will have troubles displaying the UI
        $relationships->enforceRelationshipOnly($relationship) ;
        $this->smarty->assign('view_module', $_REQUEST['view_module']) ;
        $this->smarty->assign('rel', $relationship->getDefinition()) ;
        $this->smarty->assign('mod_strings', $GLOBALS [ 'mod_strings' ]) ;
        $this->smarty->assign('module_key', $relationship->lhs_module) ;
        $this->smarty->assign('cardinality', array_keys($cardinality)) ;
        $this->smarty->assign('translated_cardinality', $cardinality) ;
        $this->smarty->assign('selected_cardinality', translate($relationship->getType())) ;

        $relatable = array( ) ;
        foreach ($relatableModules as $name => $dummy) {
            $relatable [ $name ] = translate($name) ;
        }
        unset($relatable [ 'KBDocuments' ]) ;
        natcasesort($relatable) ;
        $this->smarty->assign('relatable', array_keys($relatable)) ;
        $this->smarty->assign('translated_relatable', $relatable) ;
        $this->smarty->assign('rhspanels', $rhs_subpanels) ;
        $this->smarty->assign('lhspanels', $lhs_subpanels) ;
        $this->smarty->assign('selected_lang', $selected_lang);
        $this->smarty->assign('available_languages', get_languages());

        switch ($relationship->relationship_type) {
            case MB_ONETOONE:
                break ;

            case MB_ONETOMANY:
                if (empty($relationship->relationship_column_name)) {
                    $validRoleColumnFields = array( ) ;
                    foreach ($fields as $field) {
                        $validRoleColumnFields [] = $field ;
                    }
                    $this->smarty->assign('relationship_role_column_enum', $validRoleColumnFields) ;
                }
                if (! empty($relationship->relationship_role_column_value)) {
                    $this->smarty->assign('relationship_role_column_value', $relationship->relationship_role_column_value) ;
                }
                break ;
            case MB_MANYTOMANY:
                if (! empty($relationship->relationship_role_column_value)) {
                    $this->smarty->assign('relationship_role_column_value', $relationship->relationship_role_column_value) ;
                }
                break ;
        }

        //see if we use the new system
        if (isset($_REQUEST [ 'json' ]) && $_REQUEST [ 'json' ] == 'false') {
            echo $this->smarty->fetch('modules/ModuleBuilder/tpls/studioRelationship.tpl') ;
        } else {
            $ac->addSection('east', $module->name . ' ' . $GLOBALS [ 'mod_strings' ] [ 'LBL_RELATIONSHIPS' ], $this->smarty->fetch('modules/ModuleBuilder/tpls/studioRelationship.tpl')) ;
            echo $ac->getJavascript() ;
        }
    }
}
