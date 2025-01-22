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


require_once 'modules/ModuleBuilder/parsers/relationships/AbstractRelationships.php' ;
require_once 'modules/ModuleBuilder/parsers/relationships/RelationshipsInterface.php' ;
require_once 'modules/ModuleBuilder/parsers/relationships/RelationshipFactory.php' ;


#[\AllowDynamicProperties]
class DeployedRelationships extends AbstractRelationships implements RelationshipsInterface
{
    /**
     * @var bool
     */
    public $activitiesToAdd;
    public function __construct($moduleName)
    {
        $this->moduleName = $moduleName ;
        $this->load() ;
    }

    public static function findRelatableModules($includeActivitiesSubmodules = true)
    {
        return parent::findRelatableModules(true) ;
    }

    /*
     * Load the set of relationships for this module - the set is the combination of that held in the working file plus all of the relevant deployed relationships for the module
     * Note that deployed relationships are readonly and cannot be modified - getDeployedRelationships() takes care of marking them as such
     * Assumes that only called for modules which exist in $beansList - otherwise get_module_info will break
     * This means that load() cannot be called for Activities, only Tasks, Notes, etc
     *
     * Note that we may need to adjust the cardinality for any custom relationships that we do not have entries for in the working directory
     * These relationships might have been loaded from an installation package by ModuleInstaller, or the custom/working directory might have been cleared at some point
     * The cardinality in the installed relationship is not necessarily correct for custom relationships, which currently are all built as many-to-many relationships
     * Instead we must obtain the true cardinality from a property we added to the relationship metadata when we created the relationship
     * This relationship metadata is accessed through the Table Dictionary
     */
    public function load()
    {
        $relationships = $this->getDeployedRelationships() ;

        if (! empty($relationships)) {
            // load the relationship definitions for all installed custom relationships into $dictionary
            $dictionary = array( ) ;
            if (file_exists('custom/application/Ext/TableDictionary/tabledictionary.ext.php')) {
                include('custom/application/Ext/TableDictionary/tabledictionary.ext.php') ;
            }

            $invalidModules = array();
            $validModules = array_keys(self::findRelatableModules()) ;

            // now convert the relationships array into an array of AbstractRelationship objects
            foreach ($relationships as $name => $definition) {
                if (($definition [ 'lhs_module' ] == $this->moduleName) || ($definition [ 'rhs_module' ] == $this->moduleName)) {
                    if (in_array($definition [ 'lhs_module' ], $validModules) && in_array($definition [ 'rhs_module' ], $validModules)
                        && ! in_array($definition [ 'lhs_module' ], $invalidModules) && ! in_array($definition [ 'rhs_module' ], $invalidModules)) {
                        // identify the subpanels for this relationship - TODO: optimize this - currently does m x n scans through the subpanel list...
                        $definition [ 'rhs_subpanel' ] = self::identifySubpanel($definition [ 'lhs_module' ], $definition [ 'rhs_module' ]) ;
                        $definition [ 'lhs_subpanel' ] = self::identifySubpanel($definition [ 'rhs_module' ], $definition [ 'lhs_module' ]) ;

                        // now adjust the cardinality with the true cardinality found in the relationships metadata (see method comment above)


                        if (! empty($dictionary) && ! empty($dictionary [ $name ])) {
                            if (! empty($dictionary [ $name ] [ 'true_relationship_type' ])) {
                                $definition [ 'relationship_type' ] = $dictionary [ $name ] [ 'true_relationship_type' ] ;
                            }
                            if (! empty($dictionary [ $name ] [ 'from_studio' ])) {
                                $definition [ 'from_studio' ] = $dictionary [ $name ] [ 'from_studio' ] ;
                            }
                            $definition [ 'is_custom' ] = true;
                        }


                        $this->relationships [ $name ] = RelationshipFactory::newRelationship($definition) ;
                    }
                }
            }
        }

        /*        // Now override with any definitions from the working directory
            // must do this to capture one-to-ones that we have created as these don't show up in the relationship table that is the source for getDeployedRelationships()
            $overrides = parent::_load ( "custom/working/modules/{$this->moduleName}" ) ;
            foreach ( $overrides as $name => $relationship )
            {
                $this->relationships [ $name ] = $relationship ;
            }*/
    }

    /*
     * Save this modules relationship definitions out to a working file
     */
    public function save()
    {
        parent::_save($this->relationships, "custom/working/modules/{$this->moduleName}") ;
    }

    /*
     * Update pre-5.1 relationships to the 5.1 relationship definition
     * There is nothing to do for Deployed relationships as these were only introduced in 5.1
     * @param array definition  The 5.0 relationship definition
     * @return array            The definition updated to 5.1 format
     */
    protected function _updateRelationshipDefinition($definition)
    {
        return $definition ;
    }

    /*
     * Use the module Loader to delete the relationship from the instance.
     */
    public function delete($rel_name)
    {
        //Remove any fields from layouts
        $rel = $this->get($rel_name);
        if (!empty($rel)) {
            $this->removeFieldsFromDeployedLayout($rel);
        }
        require_once("ModuleInstall/ModuleInstaller.php");
        require_once('modules/Administration/QuickRepairAndRebuild.php') ;
        $mi = new ModuleInstaller();
        $mi->silent = true;
        $mi->id_name = 'custom' . $rel_name; // provide the moduleinstaller with a unique name for this relationship - normally this value is set to the package key...
        $mi->uninstall_relationship("custom/metadata/{$rel_name}MetaData.php");
        $mi->uninstallLabels('custom/Extension/modules/relationships/language/', $rel->buildLabels());
        $mi->uninstallExtLabels($rel->buildLabels());

        // now clear all caches so that our changes are visible
        Relationship::delete_cache();
        $mi->rebuild_tabledictionary();

        $MBmodStrings = $GLOBALS [ 'mod_strings' ];
        $GLOBALS [ 'mod_strings' ] = return_module_language('', 'Administration') ;
        $rac = new RepairAndClear() ;
        $rac->repairAndClearAll(array( 'clearAll', 'rebuildExtensions',  ), array( $GLOBALS [ 'mod_strings' ] [ 'LBL_ALL_MODULES' ] ), true, false) ;
        $GLOBALS [ 'mod_strings' ] = $MBmodStrings;

        //Bug 41070, supercedes the previous 40941 fix in this section
        if (isset($this->relationships[$rel_name])) {
            unset($this->relationships[$rel_name]);
        }
    }

    /*
     * Return the set of all known relevant relationships for a deployed module
     * The set is made up of the relationships held in this class, plus all those already deployed in the application
     * @return array Set of all relevant relationships
     */
    protected function getAllRelationships()
    {
        return array_merge($this->relationships, parent::getDeployedRelationships()) ;
    }

    /*
     * Return the name of the first (currently only) subpanel displayed in the DetailView of $thisModuleName provided by $sourceModuleName
     * We can assume that both sides of the relationship are deployed modules as this is only called within the context of DeployedRelationships
     * @param string $thisModuleName    Name of the related module
     * @param string $sourceModuleName  Name of the primary module
     * @return string Name of the subpanel if found; null otherwise
     */
    private static function identifySubpanel($thisModuleName, $sourceModuleName)
    {
        $module = get_module_info($thisModuleName) ;
        require_once('include/SubPanel/SubPanelDefinitions.php') ;
        $spd = new SubPanelDefinitions($module) ;
        $subpanelNames = $spd->get_available_tabs() ; // actually these are the displayed subpanels

        foreach ($subpanelNames as $key => $name) {
            $GLOBALS [ 'log' ]->debug($thisModuleName . " " . $name) ;

            $subPanel = $spd->load_subpanel($name) ;
            if ($subPanel && ! isset($subPanel->_instance_properties [ 'collection_list' ])) {
                if ($sourceModuleName == $subPanel->_instance_properties [ 'module' ]) {
                    return $subPanel->_instance_properties [ 'subpanel_name' ] ;
                }
            }
        }

        return null ;
    }

    /*
     * Return the name of the first (currently only) relate field of $thisModuleName sourced from by $sourceModuleName
     * We can assume that both sides of the relationship are deployed modules as this is only called within the context of DeployedRelationships
     * @param string $thisModuleName    Name of the related module
     * @param string $sourceModuleName  Name of the primary module
     * @return string Name of the relate field, if found; null otherwise
     */

    private static function identifyRelateField($thisModuleName, $sourceModuleName)
    {
        $module = get_module_info($thisModuleName) ;

        foreach ($module->field_defs as $field) {
            if ($field [ 'type' ] == 'relate' && isset($field [ 'module' ]) && $field [ 'module' ] == $sourceModuleName) {
                return $field [ 'name' ] ;
            }
        }
        return null ;
    }

    /*
     * As of SugarCRM 5.1 the subpanel code and the widgets have difficulty handling multiple subpanels or relate fields from the same module
     * Until this is fixed, we new relationships which will trigger this problem must be flagged as "relationship_only" and built without a UI component
     * This function is called from the view when constructing a new relationship
     * We can assume that both sides of the relationship are deployed modules as this is only called within the context of DeployedRelationships
     * @param AbstractRelationship $relationship The relationship to be enforced
     */
    public function enforceRelationshipOnly($relationship)
    {
        $lhs = $relationship->lhs_module ;
        $rhs = $relationship->rhs_module ;
        // if the lhs_module already has a subpanel or relate field sourced from the rhs_module,
    // or the rhs_module already has a subpanel or relate field sourced from the lhs_module,
    // then set "relationship_only" in the relationship


    //        if (($relationship->getType() != MB_ONETOONE && ! is_null ( self::identifySubpanel ( $lhs, $rhs ) )) || ($relationship->getType() == MB_MANYTOMANY && ! is_null ( self::identifySubpanel ( $rhs, $lhs ) )) || ($relationship->getType() == MB_ONETOONE && ! is_null ( self::identifyRelateField ( $rhs, $lhs ) )) || ($relationship->getType() != MB_MANYTOMANY && ! is_null ( self::identifyRelateField ( $lhs, $rhs ) )))
    //            $relationship->setRelationship_only () ;
    }

    /*
     * BUILD FUNCTIONS
     */

    /*
     * Implement all of the Relationships in this set of relationships
     * This is more general than it needs to be given that deployed relationships are built immediately - there should only be one relationship to build here...
     * We use the Extension mechanism to do this for DeployedRelationships
     * All metadata is placed in the modules Ext directory, and then Rebuild is called to activate them
     */
    public function build($basepath = null, $installDefPrefix = null, $relationships = null)
    {
        $basepath = "custom/Extension/modules" ;

        $this->activitiesToAdd = false ;

        // and mark all as built so that the next time we come through we'll know and won't build again
        foreach ($this->relationships as $name => $relationship) {
            $definition = $relationship->getDefinition() ;
            // activities will always appear on the rhs only - lhs will be always be this module in MB
            if (strtolower($definition [ 'rhs_module' ]) == 'activities') {
                $this->activitiesToAdd = true ;
                $relationshipName = $definition [ 'relationship_name' ] ;
                foreach (self::$activities as $activitiesSubModuleLower => $activitiesSubModuleName) {
                    $definition [ 'rhs_module' ] = $activitiesSubModuleName ;
                    $definition [ 'for_activities' ] = true ;
                    $definition [ 'relationship_name' ] = $relationshipName . '_' . $activitiesSubModuleLower ;
                    $this->relationships [ $definition [ 'relationship_name' ] ] = RelationshipFactory::newRelationship($definition) ;
                }
                unset($this->relationships [ $name ]) ;
            }
        }

        $GLOBALS [ 'log' ]->info(get_class($this) . "->build(): installing relationships") ;

        $MBModStrings = $GLOBALS [ 'mod_strings' ] ;
        $adminModStrings = return_module_language('', 'Administration') ; // required by ModuleInstaller

        foreach ($this->relationships as $name => $relationship) {
            $relationship->setFromStudio();
            $GLOBALS [ 'mod_strings' ] = $MBModStrings ;
            $installDefs = parent::build($basepath, "<basepath>", array($name => $relationship )) ;

            // and mark as built so that the next time we come through we'll know and won't build again
            $relationship->setReadonly() ;
            $this->relationships [ $name ] = $relationship ;

            // now install the relationship - ModuleInstaller normally only does this as part of a package load where it installs the relationships defined in the manifest. However, we don't have a manifest or a package, so...

            // If we were to chose to just use the Extension mechanism, without using the ModuleInstaller install_...() methods, we must :
            // 1)   place the information for each side of the relationship in the appropriate Ext directory for the module, which means specific $this->save...() methods for DeployedRelationships, and
            // 2)   we must also manually add the relationship into the custom/application/Ext/TableDictionary/tabledictionary.ext.php file as install_relationship doesn't handle that (install_relationships which requires the manifest however does)
            //      Relationships must be in tabledictionary.ext.php for the Admin command Rebuild Relationships to reliably work:
            //      Rebuild Relationships looks for relationships in the modules vardefs.php, in custom/modules/<modulename>/Ext/vardefs/vardefs.ext.php, and in modules/TableDictionary.php and custom/application/Ext/TableDictionary/tabledictionary.ext.php
            //      if the relationship is not defined in one of those four places it could be deleted during a rebuilt, or during a module installation (when RebuildRelationships.php deletes all entries in the Relationships table)
            // So instead of doing this, we use common save...() methods between DeployedRelationships and UndeployedRelationships that will produce installDefs,
            // and rather than building a full manifest file to carry them, we manually add these installDefs to the ModuleInstaller, and then
            // individually call the appropriate ModuleInstaller->install_...() methods to take our relationship out of our staging area and expand it out to the individual module Ext areas

            $GLOBALS [ 'mod_strings' ] = $adminModStrings ;
            require_once 'ModuleInstall/ModuleInstaller.php' ;
            $mi = new ModuleInstaller() ;

            $mi->id_name = 'custom' . $name ; // provide the moduleinstaller with a unique name for this relationship - normally this value is set to the package key...
            $mi->installdefs = $installDefs ;
            $mi->base_dir = $basepath ;
            $mi->silent = true ;


            VardefManager::clearVardef() ;

            $mi->install_relationships() ;
            $mi->install_languages() ;
            $mi->install_vardefs() ;
            $mi->install_layoutdefs() ;
            $mi->install_extensions();
        }

        // Run through the module installer to rebuild the relationships once after everything is done.
        require_once 'ModuleInstall/ModuleInstaller.php' ;
        $mi = new ModuleInstaller() ;
        $mi->silent = true;
        $mi->rebuild_relationships();

        // now clear all caches so that our changes are visible
        require_once('modules/Administration/QuickRepairAndRebuild.php') ;
        $rac = new RepairAndClear() ;
        $rac->repairAndClearAll(array( 'clearAll' ), array( $GLOBALS [ 'mod_strings' ] [ 'LBL_ALL_MODULES' ] ), true, false) ;

        $GLOBALS [ 'mod_strings' ] = $MBModStrings ; // finally, restore the ModuleBuilder mod_strings

        // save out the updated definitions so that we keep track of the change in built status
        $this->save() ;

        $GLOBALS [ 'log' ]->info(get_class($this) . "->build(): finished relationship installation") ;
    }

    /*
     * Add any fields to the DetailView and EditView of the appropriate modules
     * @param string $basepath              Basepath location for this module (not used)
     * @param string $relationshipName      Name of this relationship (for uniqueness)
     * @param array $layoutAdditions  An array of module => fieldname
     * return null
     */
    protected function saveFieldsToLayouts($basepath, $dummy, $relationshipName, $layoutAdditions)
    {
        require_once 'modules/ModuleBuilder/parsers/views/GridLayoutMetaDataParser.php' ;

        // these modules either lack editviews/detailviews or use custom mechanisms for the editview/detailview. In either case, we don't want to attempt to add a relate field to them
        // would be better if GridLayoutMetaDataParser could handle this gracefully, so we don't have to maintain this list here
        $invalidModules = array( 'emails' , 'kbdocuments' ) ;

        foreach ($layoutAdditions as $deployedModuleName => $fieldName) {
            if (! in_array(strtolower($deployedModuleName), $invalidModules)) {
                foreach (array( MB_EDITVIEW , MB_DETAILVIEW ) as $view) {
                    $GLOBALS [ 'log' ]->info(get_class($this) . ": adding $fieldName to $view layout for module $deployedModuleName") ;
                    $parser = new GridLayoutMetaDataParser($view, $deployedModuleName) ;
                    $parser->addField(array( 'name' => $fieldName )) ;
                    $parser->handleSave(false) ;
                }
            }
        }
    }

    /**
     * Added for bug #40941
     * Deletes the field from DetailView and editView of the appropriate module
     * after the relatioship is deleted in delete() function above.
     * @param $relationship    The relationship that is getting deleted
     * return null
     */
    private function removeFieldsFromDeployedLayout($relationship)
    {

        // many-to-many relationships don't have fields so if we have a many-to-many we can just skip this...
        if ($relationship->getType() == MB_MANYTOMANY) {
            return false ;
        }

        $successful = true ;
        $layoutAdditions = $relationship->buildFieldsToLayouts() ;

        require_once 'modules/ModuleBuilder/parsers/views/GridLayoutMetaDataParser.php' ;
        foreach ($layoutAdditions as $deployedModuleName => $fieldName) {
            foreach (array( MB_EDITVIEW , MB_DETAILVIEW ) as $view) {
                $parser = new GridLayoutMetaDataParser($view, $deployedModuleName) ;
                $parser->removeField($fieldName);
                $parser->handleSave(false) ;
            }
        }

        return $successful ;
    }
}
