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
class UndeployedRelationships extends AbstractRelationships implements RelationshipsInterface
{
    public $packageKey;
    public $installDefs;
    protected $basepath ; // Base directory for the lhs_module
    protected $packageName ;
    private $activitiesToAdd ; // if we need to add in the composite Activities and History subpanels to the module during the build


    /*
     * Constructor
     * Automatically loads in any saved relationships
     * @param string $path  The pathname of the base module directory
     */
    public function __construct($path)
    {
        $this->basepath = $path ;
        // pull the module and package names out of the path
        $this->moduleName = basename((string) $path, "/") ; // just in case there are any trailing /
        $this->packageName = basename(dirname((string) $path, 2)) ; // simpler than explode :)
        require_once 'modules/ModuleBuilder/MB/ModuleBuilder.php' ;
        $mb = new ModuleBuilder() ;
        $this->packageKey = $mb->getPackageKey($this->packageName) ;

        $this->load() ;
    }

    /*
     * Find all modules, deployed and undeployed, that can participate in a relationship
     * @return array    Array of [$module][$subpanel]
     */
    public static function findRelatableModules($includeActivitiesSubmodules = true)
    {
        // first find all deployed modules that we might participate in a relationship
        $relatableModules = parent::findRelatableModules($includeActivitiesSubmodules) ;

        // now add in the undeployed modules - those in custom/modulebuilder
        // note that if a module exists in both deployed and undeployed forms, the subpanels from the undeployed form are used...

        require_once 'modules/ModuleBuilder/MB/ModuleBuilder.php' ;
        $mb = new ModuleBuilder() ;
        $mb->getPackages() ;
        foreach ($mb->getPackageList() as $packageName) {
            $package = $mb->packages [ $packageName ] ;
            foreach ($package->modules as $module) {
                $relatableModules [ $package->key . "_" . $module->name ] = $module->getProvidedSubpanels() ;
            }
        }

        return $relatableModules ;
    }

    /*
     * Add a relationship to the set
     * For undeployed relationships we always make the fields in the relationship visible in the layouts now, rather than waiting until build time, so
     * that the admin may move them around or otherwise edit them before the module is deployed
     * @param AbstractRelationship $relationship    The relationship to add
     */
    public function add($relationship)
    {
        parent::add($relationship) ;
        $this->addFieldsToUndeployedLayouts($relationship) ; // must come after parent::add as we need the relationship_name in the relationships getFieldsToLayouts() which is called by addFieldsToUndeployedLayouts()
    }

    /*
     * Delete a relationship by name
     * In future, if we need to actually track deleted relationships then just call $relationship->delete() instead
     * @param string $relationshipName  The unique name for this relationship, as returned by $relationship->getName()
     */
    public function delete($relationshipName)
    {
        if ($relationship = $this->get($relationshipName)) {
            $this->removeFieldsFromUndeployedLayouts($relationship) ;
            unset($this->relationships [ $relationshipName ]) ;
        }
    }

    /*
     * Load the saved relationship definitions for this module
     */
    public function load()
    {
        $this->relationships = parent::_load($this->basepath) ;
    }

    /*
     * Save this modules relationship definitions out to a working file
     */
    public function save()
    {
        parent::_save($this->relationships, $this->basepath) ;
    }

    /*
     * Update pre-5.1 relationships to the 5.1 relationship definition
     * @param array definition  The 5.0 relationship definition
     * @return array            The definition updated to 5.1 format
     */
    protected function _updateRelationshipDefinition($definition)
    {
        if (isset($definition [ 'relate' ])) {
            $newDefinition = array( ) ;
            foreach (array( 'relate' => 'rhs_module' , 'rsub' => 'rhs_subpanel' , 'msub' => 'lhs_subpanel' , 'label' => 'label' ) as $oldParameter => $newParameter) {
                if (isset($definition [ $oldParameter ])) {
                    $definition [ $newParameter ] = $definition [ $oldParameter ] ;
                    unset($definition [ $oldParameter ]) ;
                }
            }
            $definition [ 'lhs_module' ] = "{$this->packageKey}_{$this->moduleName}" ;
            // finally update the relationship name
            unset($definition [ 'name' ]) ; // clear the oldstyle name
        }
        return $definition ;
    }

    /*
     * Implementation of getAllRelationships() for Undeployed modules
     * The set of all relevant relationships for undeployed modules is the superset of that for deployed modules and all of the relationships known to ModuleBuilder
     * @return array Set of all relevant relationships
     */
    protected function getAllRelationships()
    {
        // start with the set of relationships known to this module plus those already deployed
        $allRelationships = array_merge($this->relationships, parent::getDeployedRelationships()) ;

        // add in the relationships known to ModuleBuilder
        require_once 'modules/ModuleBuilder/MB/ModuleBuilder.php' ;
        $mb = new ModuleBuilder() ;
        $mb->getPackages() ;
        foreach ($mb->getPackageList() as $packageName) {
            $package = $mb->packages [ $packageName ] ;
            foreach ($package->modules as $module) {
                foreach ($module->relationships->getRelationshipList() as $relationshipName) {
                    $relationship = $module->relationships->get($relationshipName) ;
                    $allRelationships [ $relationship->getName() ] = $relationship->getDefinition() ;
                }
            }
        }

        return $allRelationships ;
    }

    /*
     * As of SugarCRM 5.1 the subpanel code and the widgets have difficulty handling multiple subpanels or relate fields from the same module
     * Until this is fixed, we new relationships which will trigger this problem must be flagged as "relationship_only" and built without a UI component
     * This function is called from the view when constructing a new relationship
     * @param AbstractRelationship $relationship The relationship to be enforced
     */
    public function enforceRelationshipOnly($relationship)
    {
        // if we already have a relationship between this lhs_module and this rhs_module then set RelationshipOnly flag
        foreach ($this->relationships as $rel) {
            if ($rel->lhs_module == $relationship->lhs_module && $rel->rhs_module == $relationship->rhs_module) {
                $rel->setRelationship_only() ;
                break ;
            }
        }
    }

    /*
     * BUILD FUNCTIONS
     */

    /*
     * Translate the set of relationship objects into files that the Module Loader can work with
     * @param $basepath string Pathname of the directory to contain the build
     */
    public function build($basepath = null, $installDefPrefix = null, $relationships = null)
    {

        // first expand out any reference to Activities to its submodules
        // we do this here rather than in the subcomponents of the build as most of those subcomponents make use of elements of the definition, such
        // as the relationship name, that must be unique
        // the only special case is the subpanel for Activities, which is a composite, and is applied only once for all the submodules - this is handled in saveSubpanelDefinitions() for Undeployed modules

        $relationships = array( ) ;
        $this->activitiesToAdd = false ;
        foreach ($this->relationships as $relationshipName => $relationship) {
            $definition = $relationship->getDefinition() ;
            // activities will always appear on the rhs only - lhs will be always be this module in MB
            if (strtolower($definition [ 'rhs_module' ]) == 'activities') {
                $this->activitiesToAdd = true ;
                $relationshipName = $definition [ 'relationship_name' ] ;
                foreach (self::$activities as $activitiesSubModuleLower => $activitiesSubModuleName) {
                    $definition [ 'rhs_module' ] = $activitiesSubModuleName ;
                    $definition [ 'for_activities' ] = true ;
                    $definition [ 'relationship_name' ] = $relationshipName . '_' . $activitiesSubModuleLower ;
                    $relationships [ $definition [ 'relationship_name' ] ] = RelationshipFactory::newRelationship($definition) ;
                }
            } else {
                $relationships [ $definition [ 'relationship_name' ] ] = $relationship ;
            }
        }

        require_once 'modules/ModuleBuilder/MB/ModuleBuilder.php' ;
        $mb = new ModuleBuilder() ;
        $module = $mb->getPackageModule($this->packageName, $this->moduleName) ;
        if ($this->activitiesToAdd) {
            $appStrings = $module->getAppListStrings() ;
            foreach (getTypeDisplayList() as $typeDisplay) {
                $appStrings[$typeDisplay][$module->key_name] = $module->getlabel('en_us', 'LBL_MODULE_TITLE') ;
            }
            $module->setAppListStrings('en_us', $appStrings) ;
            $module->save() ;
        } else {
            //Bug42170================================
            $appStrings = $module->getAppListStrings() ;
            foreach (getTypeDisplayList() as $typeDisplay) {
                if (isset($appStrings[$typeDisplay][$module->key_name])) {
                    unset($appStrings[$typeDisplay][$module->key_name]);
                }
            }
            $module->setAppListStrings('en_us', $appStrings) ;
            $module->save() ;
            //Bug42170================================
        }

        // use an installDefPrefix of <basepath>/SugarModules for compatibility with the rest of ModuleBuilder
        $this->installDefs = parent::build($basepath, "<basepath>/SugarModules", $relationships) ;
    }

    /*
     * Add the installDefs for this relationship to the definitions in the parameter
     * Required by MBModule
     * @param reference installDef  Reference to the set of installDefs to which this relationship's installDefs should be added
     */
    public function addInstallDefs(&$installDef)
    {
        foreach ($this->installDefs as $name => $def) {
            if (! empty($def)) {
                foreach ($def as $val) {
                    $installDef [ $name ] [] = $val ;
                }
            }
        }
    }

    private function addFieldsToUndeployedLayouts($relationship)
    {
        return $this->updateUndeployedLayout($relationship, true) ;
    }

    private function removeFieldsFromUndeployedLayouts($relationship)
    {
        return $this->updateUndeployedLayout($relationship, false) ;
    }

    /**
     * @param AbstractRelationship $relationship
     * @return void
     */
    private function removeAppLangStrings($relationship)
    {
        $def = $relationship->getDefinition();
        if (strtolower($def [ 'rhs_module' ]) == 'activities' && !empty($_REQUEST [ 'view_package' ]) && !empty($_REQUEST [ 'view_module' ])) {
            $mb = new ModuleBuilder() ;
            $module = $mb->getPackageModule($_REQUEST [ 'view_package' ], $_REQUEST [ 'view_module' ]) ;
            $appStrings = $module->getAppListStrings() ;
            foreach (getTypeDisplayList() as $key) {
                if (isset($appStrings[$key][ $module->key_name ])) {
                    unset($appStrings[$key][ $module->key_name ]);
                }
            }
            $module->setAppListStrings('en_us', $appStrings) ;
            $module->save() ;
        }
    }

    /*
     * Add any relate fields to the DetailView and EditView of the appropriate module immediately (don't wait for a build)
     * @param AbstractRelationship $relationship The relationship whose fields we are to add or remove
     * @param boolean $actionAdd True if we are to add; false if to remove
     * return null
     */
    private function updateUndeployedLayout($relationship, $actionAdd = true)
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
                $parsedName = AbstractRelationships::parseDeployedModuleName($deployedModuleName) ;
                if (isset($parsedName [ 'packageName' ])) {
                    $GLOBALS [ 'log' ]->debug(get_class($this) . ": " . (($actionAdd) ? "adding" : "removing") . " $fieldName on $view layout for undeployed module {$parsedName [ 'moduleName' ]} in package {$parsedName [ 'packageName' ]}") ;
                    $parser = new GridLayoutMetaDataParser($view, $parsedName [ 'moduleName' ], $parsedName [ 'packageName' ]) ;

                    if (($actionAdd) ? $parser->addField(array( 'name' => $fieldName )) : $parser->removeField($fieldName)) {
                        $parser->handleSave(false) ;
                    } else {
                        $GLOBALS [ 'log' ]->debug(get_class($this) . ": couldn't " . (($actionAdd) ? "add" : "remove") . " $fieldName on $view layout for undeployed module $deployedModuleName") ;
                        $successful = false ;
                    }
                }
            }
        }

        return $successful ;
    }

    /*
     * Add any fields to the DetailView and EditView of the appropriate modules
     * Only add into deployed modules, as addFieldsToUndeployedLayouts has done this already for undeployed modules (and the admin might have edited the layouts already)
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

        $fieldsToAdd = array();
        foreach ($layoutAdditions as $deployedModuleName => $fieldName) {
            if (! in_array(strtolower($deployedModuleName), $invalidModules)) {
                foreach (array( MB_EDITVIEW , MB_DETAILVIEW ) as $view) {
                    $GLOBALS [ 'log' ]->debug(get_class($this) . ": adding $fieldName to $view layout for module $deployedModuleName") ;
                    $parsedName = self::parseDeployedModuleName($deployedModuleName) ;
                    if (! isset($parsedName [ 'packageName' ])) {
                        $fieldsToAdd [$parsedName [ 'moduleName' ]] = $fieldName;
                    }
                    //Bug 22348: We should add in the field for custom modules not in this package, if they have been deployed.
                    else {
                        if ($parsedName [ 'packageName' ] != $this->packageName
                            && isset($GLOBALS [ 'beanList' ] [ $deployedModuleName ])) {
                            $fieldsToAdd [$deployedModuleName] = $fieldName;
                        }
                    }
                }
            }
        }
        return array(array('additional_fields' => $fieldsToAdd));
    }
}
