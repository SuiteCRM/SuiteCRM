<?php
if (! defined('sugarEntry') || ! sugarEntry) {
    die('Not A Valid Entry Point') ;
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


/*
 * Abstract class for managing a set of Relationships
 * The Relationships we're managing consist of metadata about relationships, rather than relationship implementations used by the application
 * Relationships defined here are implemented by the build() method to become a relationship that the application can use
 * Note that the modules/Relationships/Relationship.php contains some methods that look similar; remember though that the methods in that file are acting on implemented relationships, not the metadata that we deal with here
 */
#[\AllowDynamicProperties]
class AbstractRelationships
{
    public static $methods = array(
        'Labels' => 'language' ,
        'RelationshipMetaData' => 'relationships' ,
        'SubpanelDefinitions' => 'layoutdefs' ,
        'Vardefs' => 'vardefs' ,
        'FieldsToLayouts' => 'layoutfields',
    ) ;
    public static $activities = array( 'calls' => 'Calls' , 'meetings' => 'Meetings' , 'notes' => 'Notes' , 'tasks' => 'Tasks' , 'emails' => 'Emails' ) ;

    protected $relationships = array( ) ; // array containing all the AbstractRelationship objects that are in this set of relationships
    protected $moduleName ;

    // bug33522 - the following relationship names that you would find in $dictionary[ <relationshipName> ]
    // have different actual relationship names other than <relationshipName>
    // e.g $dictionary[ 'quotes_accounts' ] has two relationships: quotes_billto_accounts, quotes_shipto_accounts
    protected $specialCaseBaseNames = array( 'quotes_accounts',
                                             'quotes_contacts',
                                             'emails_beans',
                                             'linked_documents',
                                             'project_relation',
                                             'prospect_lists_prospects',
                                             'queues_beans',
                                             'queues_queue',
                                             'tracker_sessions'
                                          );
    /*
     * Find all deployed modules that can participate in a relationship
     * Return a list of modules with associated subpanels
     * @param boolean $includeActivitiesSubmodules True if the list should include Calls, Meetings etc; false if they should be replaced by the parent, Activities
     * @return array    Array of [$module][$subpanel]
     */
    public static function findRelatableModules($includeActivitiesSubmodules = true)
    {
        $relatableModules = array( ) ;

        // add in activities automatically if required
        $relatableModules [ 'Activities' ] [ 'default' ] = translate('LBL_DEFAULT') ;

        // find all deployed modules
        require_once 'modules/ModuleBuilder/Module/StudioBrowser.php' ;
        $browser = new StudioBrowser() ;
        $browser->loadRelatableModules();
        reset($browser->modules) ;

        foreach ($browser->modules as $moduleName => $module) {
            // do not include the submodules of Activities as already have the parent...
            if (! $includeActivitiesSubmodules && in_array($module->module, self::$activities)) {
                continue ;
            }
            $providedSubpanels = $module->getProvidedSubpanels();
            if ($providedSubpanels !== false) {
                $relatableModules [ $module->module ] = $providedSubpanels;
            }
        }

        return $relatableModules ;
    }

    public static function validSubpanel($filename)
    {
        if (! file_exists($filename)) {
            return false ;
        }

        include $filename ;
        return (isset($subpanel_layout) && (isset($subpanel_layout [ 'top_buttons' ]) && isset($subpanel_layout [ 'list_fields' ]))) ;
    }

    /*
     * Get a list of all relationships (which have not been deleted)
     * @return array    Array of relationship names, ready for use in get()
     */
    public function getRelationshipList()
    {
        $list = array( ) ;
        foreach ($this->relationships as $name => $relationship) {
            if (! $relationship->deleted()) {
                $list [ $name ] = $name ;
            }
        }
        return $list ;
    }

    /*
     * Get a relationship by name
     * @param string $relationshipName  The unique name for this relationship, as returned by $relationship->getName()
     * @return AbstractRelationship or false if $relationshipName is not in this set of relationships
     */
    public function get($relationshipName)
    {
        if (isset($this->relationships [ $relationshipName ])) {
            return $this->relationships [ $relationshipName ] ;
        }
        return false ;
    }

    /*
     * Construct a relationship from the information in the $_REQUEST array
     * If a relationship_name is provided, and that relationship is not read only, then modify the existing relationship, overriding the definition with any AbstractRelationship::$definitionkeys entries set in the $_REQUEST
     * Otherwise, create and add a new relationship with the information in the $_REQUEST
     * @return AbstractRelationship
     */
    public function addFromPost()
    {
        $definition = array( ) ;

        require_once 'modules/ModuleBuilder/parsers/relationships/AbstractRelationship.php' ;
        foreach (AbstractRelationship::$definitionKeys as $key) {
            if (! empty($_REQUEST [ $key ])) {
                $definition [ $key ] = ($key == 'relationship_type') ? AbstractRelationship::parseRelationshipType($_REQUEST [ $key ]) : $_REQUEST [ $key ] ;
            }
        }

        // if this is a change to an existing relationship, and it is not readonly, then delete the old one
        if (! empty($_REQUEST [ 'relationship_name' ])) {
            if ($relationship = $this->get($_REQUEST [ 'relationship_name' ])) {
                unset($definition[ 'relationship_name' ]) ; // in case the related modules have changed; this name is probably no longer appropriate
                if (! $relationship->readonly()) {
                    $this->delete($_REQUEST [ 'relationship_name' ]) ;
                }
            }
        }

        $newRelationship = RelationshipFactory::newRelationship($definition) ;
        // TODO: error handling in case we get a badly formed definition and hence relationship
        $this->add($newRelationship) ;
        return $newRelationship ;
    }

    /*
     * Add a relationship to the set
     * @param AbstractRelationship $relationship    The relationship to add
     */
    public function add($relationship)
    {
        $name = $this->getUniqueName($relationship) ;
        $relationship->setName($name) ;
        $this->relationships [ $name ] = $relationship ;
    }

    /*
     * Load a set of relationships from a file
     * The saved relationships are stored as AbstractRelationship objects, which isn't the same as the old MBRelationships definition
     * @param string $basepath  Base directory in which to store the relationships information
     * @return Array of AbstractRelationship objects
     */
    protected function _load($basepath)
    {
        $GLOBALS [ 'log' ]->info(get_class($this) . ": loading relationships from " . $basepath . '/relationships.php') ;
        $objects = array( ) ;
        if (file_exists($basepath . '/relationships.php')) {
            include($basepath . '/relationships.php') ;
            foreach ($relationships as $name => $definition) {
                // update any pre-5.1 relationships to the new definitions
                // we do this here, rather than when upgrading from 5.0 to 5.1, as modules exported from MB in 5.0 may be loaded into 5.1 at any time
                // note also that since these definitions are only found in the relationships.php working file they only occur for deployed or exported modules, not published then loaded modules
                $definition = $this->_updateRelationshipDefinition($definition) ;
                $relationship = RelationshipFactory::newRelationship($definition) ;
                // make sure it has a unique name
                if (! isset($definition [ 'relationship_name' ])) {
                    $name = $this->getUniqueName($relationship) ;
                    $relationship->setName($name) ;
                }
                $objects [ $name ] = $relationship ;
            }
        }
        return $objects ;
    }

    /*
     * Save the set of relationships to a file
     * @param string $basepath  Base directory in which to store the relationships information
     */
    protected function _save($relationships, $basepath)
    {
        $GLOBALS [ 'log' ]->info(get_class($this) . ": saving relationships to " . $basepath . '/relationships.php') ;
        $header = file_get_contents('modules/ModuleBuilder/MB/header.php') ;

        $definitions = array( ) ;

        foreach ($relationships as $relationship) {
            // if (! $relationship->readonly ())
            $definitions [ $relationship->getName() ] = $relationship->getDefinition() ;
        }

        mkdir_recursive($basepath) ;
        // replace any existing relationships.php
        write_array_to_file('relationships', $definitions, $basepath . '/relationships.php', 'w', $header) ;
    }

    /*
     * Return all known deployed relationships
     * All are set to read-only - the assumption for now is that we can't directly modify a deployed relationship
     * However, if it was created through this AbstractRelationships class a modifiable version will be held in the relationships working file,
     * and that one will override the readonly version in load()
     *
     * TODO: currently we ignore the value of the 'reverse' field in the relationships definition. This is safe to do as only one
     * relationship (products-products) uses it (and there it makes no difference from our POV) and we don't use it when creating new ones
     * @return array Array of $relationshipName => $relationshipDefinition as an array
     */
    protected function getDeployedRelationships()
    {
        $relationships = [];
        $db = DBManagerFactory::getInstance() ;
        $query = "SELECT * FROM relationships WHERE deleted = 0" ;
        $result = $db->query($query) ;
        while ($row = $db->fetchByAssoc($result)) {
            // set this relationship to readonly
            $row [ 'readonly' ] = true ;
            $relationships [ $row [ 'relationship_name' ] ] = $row ;
        }

        return $relationships ;
    }

    /*
     * Get a name for this relationship that is unique across all of the relationships we are aware of
     * We make the name unique by simply adding on a suffix until we achieve uniqueness
     * @param AbstractRelationship The relationship object
     * @return string A globally unique relationship name
     */
    protected function getUniqueName($relationship)
    {
        $allRelationships = $this->getRelationshipList() ;
        $basename = $relationship->getName() ;

        if (empty($basename)) {
            // start off with the proposed name being simply lhs_module_rhs_module
            $definition = $relationship->getDefinition() ;
            $basename = strtolower($definition [ 'lhs_module' ] . '_' . $definition [ 'rhs_module' ]) ;
        }

        // Bug #49024 : Relationships Created in Earlier Versions Cause Conflicts and AJAX Errors After Upgrade
        // ...all custom relationships created via Studio should always have a numeric identifier attached.
        if ($this instanceof DeployedRelationships) {
            $name = $basename . '_1' ;
            $suffix = 2 ;
        } else {
            $name = $basename ;
            $suffix = 1 ;
        }

        while (isset($allRelationships [ $name ])) {
            $name = $basename . "_" . ( string ) ($suffix ++) ;
        }

        // bug33522 - if our relationship basename is in the special cases
        if (in_array($name, $this->specialCaseBaseNames)) {
            //add a _1 (or _suffix#) and check to see if it already exists
            $name = $name . "_" . ( string ) ($suffix ++);
            while (isset($allRelationships [ $name ])) {
                // if it does exist, strip off the _1 previously added and try again
                $name = substr($name, 0, -2) . "_" . ( string ) ($suffix ++);
            }
        }

        return $name ;
    }

    /*
     * Translate the set of relationship objects into files that the Module Loader can work with
     * @param string $basepath          Pathname of the directory to contain the build
     * @param string $installDefPrefix  Pathname prefix for the installdefs, for example for ModuleBuilder use "<basepath>/SugarModules"
     * @param array $relationships      Relationships to implement
     */
    protected function build($basepath, $installDefPrefix, $relationships)
    {
        global $sugar_config;
        // keep the relationships data separate from any other build data by ading /relationships to the basepath
        $basepath .= '/relationships' ;

        $installDefs = array( ) ;
        $compositeAdded = false ;
        foreach (self::$methods as $method => $key) {
            $buildMethod = 'build' . $method ;
            $saveMethod = 'save' . $method ;

            foreach ($relationships as $name => $relationship) {
                if (! ($relationship->readonly() || $relationship->deleted())) {
                    if (method_exists($relationship, $buildMethod) && method_exists($this, $saveMethod)) {
                        $metadata = $relationship->$buildMethod() ;

                        if ((is_countable($metadata) ? count($metadata) : 0) > 0) { // don't clutter up the filesystem with empty files...
                            $GLOBALS [ 'log' ]->debug(get_class($this) . ": BUILD is running METHOD $saveMethod") ;
                            $installDef = $this->$saveMethod($basepath, $installDefPrefix, $name, $metadata) ;

                            // some save methods (e.g., saveRelateFieldDefinition) handle the installDefs internally and so return null


                            if (! is_null($installDef)) {
                                foreach ($installDef as $moduleName => $def) {
                                    $installDefs [ $key ] [ ] = $def ;
                                }
                            }
                        }
                    }
                }
            }
        }

        return $installDefs ;
    }

    /*
     * SAVE methods called during the build to translate the metadata provided by each relationship into files for the module installer
     * Note that the installer expects only one file for each module in each section of the manifest - multiple files result in only the last one being implemented!
     */

    /*
     * Add a set of labels to the module
     * @param string $basepath              Basepath location for this module
     * @param $installDefPrefix             Pathname prefix for the installdefs, for example for ModuleBuilder use "<basepath>/SugarModules"
     * @param string $relationshipName      Name of this relationship (for uniqueness)
     * @param array $labelDefinitions       Array of System label => Display label pairs
     * @return null Nothing to be added to the installdefs for an undeployed module
     */
    protected function saveLabels($basepath, $installDefPrefix, $relationshipName, $labelDefinitions)
    {
        global $sugar_config;

        mkdir_recursive("$basepath/language") ;

        $headerString = "<?php\n//THIS FILE IS AUTO GENERATED, DO NOT MODIFY\n" ;
        $installDefs = array( ) ;
        foreach ($labelDefinitions as $definition) {
            $mod_strings = array();
            $app_list_strings = array();

            $out = $headerString;

            $filename = "{$basepath}/language/{$definition['module']}.php" ;

            if (file_exists($filename)) {
                include($filename);
            }


            //Check for app strings
            $GLOBALS [ 'log' ]->debug(get_class($this) . "->saveLabels(): saving the following to {$filename}"
                                      . print_r($definition, true)) ;
            if ($definition['module'] == 'application') {
                $app_list_strings[$definition [ 'system_label' ]] = $definition [ 'display_label' ];
                foreach ($app_list_strings as $key => $val) {
                    $out .= override_value_to_string_recursive2('app_list_strings', $key, $val);
                }
            } else {
                $mod_strings[ $definition [ 'system_label' ]] = $definition [ 'display_label' ];
                foreach ($mod_strings as $key => $val) {
                    $out .= override_value_to_string_recursive2('mod_strings', $key, $val);
                }
            }

            $fh = fopen($filename, 'wb') ;
            fwrite($fh, $out, strlen($out)) ;
            sugar_fclose($fh) ;


            foreach ($sugar_config['languages'] as $lk => $lv) {
                $installDefs [ $definition [ 'module' ] . "_$lk" ] = array(
                    'from' => "{$installDefPrefix}/relationships/language/{$definition [ 'module' ]}.php" ,
                    'to_module' => $definition [ 'module' ] ,
                    'language' => $lk
                ) ;
            }

            /* do not use the following write_array_to_file method to write the label file -
             * module installer appends each of the label files together (as it does for all files)
             * into a combined label file and so the last $mod_strings is the only one received by the application */
            // write_array_to_file ( 'mod_strings', array ( $definition [ 'system_label' ] => $definition [ 'display_label' ] ), $filename, "a" ) ;
        }

        return $installDefs ;
    }

    /*
     * Translate a set of relationship metadata definitions into files for the Module Loader
     * @param string $basepath              Basepath location for this module
     * @param $installDefPrefix             Pathname prefix for the installdefs, for example for ModuleBuilder use "<basepath>/SugarModules"
     * @param string $relationshipName      Name of this relationship (for uniqueness)
     * @param array $relationshipMetaData   Set of metadata definitions in the form $relationshipMetaData[$relationshipName]
     * @return array $installDefs           Set of new installDefs
     */
    protected function saveRelationshipMetaData($basepath, $installDefPrefix, $relationshipName, $relationshipMetaData)
    {
        mkdir_recursive("$basepath/relationships") ;

        $installDefs = array( ) ;
        $rhs_module = key($relationshipMetaData);
        $properties = current($relationshipMetaData);

        $filename = "$basepath/relationships/{$relationshipName}MetaData.php" ;
        $GLOBALS [ 'log' ]->debug(get_class($this) . "->saveRelationshipMetaData(): saving the following to {$filename}" . print_r($properties, true)) ;
        write_array_to_file('dictionary["' . $relationshipName . '"]', $properties, $filename, 'w') ;
        $installDefs [ $relationshipName ] = array( /*'module' => $rhs_module , 'module_vardefs' => "<basepath>/Vardefs/{$relationshipName}.php" ,*/ 'meta_data' => "{$installDefPrefix}/relationships/relationships/{$relationshipName}MetaData.php" ) ;

        return $installDefs ;
    }

    /*
     * Translate a set of subpanelDefinitions into files for the Module Loader
     * @param string $basepath              Basepath location for this module
     * @param $installDefPrefix             Pathname prefix for the installdefs, for example for ModuleBuilder use "<basepath>/SugarModules"
     * @param array $subpanelDefinitions    Set of subpanel definitions in the form $subpanelDefinitions[$for_module][]
     * @return array $installDefs           Set of new installDefs
     */
    protected function saveSubpanelDefinitions($basepath, $installDefPrefix, $relationshipName, $subpanelDefinitions)
    {
        $installDefs = [];
        mkdir_recursive("$basepath/layoutdefs/") ;

        foreach ($subpanelDefinitions as $moduleName => $definitions) {
            $filename = "$basepath/layoutdefs/{$relationshipName}_{$moduleName}.php" ;
            $subpanelVarname = 'layout_defs["' . $moduleName . '"]["subpanel_setup"]';
            $out = "";
            foreach ($definitions as $definition) {
                $GLOBALS [ 'log' ]->debug(get_class($this) . "->saveSubpanelDefinitions(): saving the following to {$filename}" . print_r($definition, true)) ;
                if (empty($definition ['get_subpanel_data']) || $definition ['subpanel_name'] == 'history' || $definition ['subpanel_name'] == 'activities') {
                    $definition ['get_subpanel_data'] = $definition ['subpanel_name'];
                }
                $out .= override_value_to_string($subpanelVarname, strtolower($definition [ 'get_subpanel_data' ]), $definition) . "\n";
            }
            if (!empty($out)) {
                $out = "<?php\n // created: " . date('Y-m-d H:i:s') . "\n" . $out;
                sugar_file_put_contents($filename, $out);
            }

            $installDefs [ $moduleName ] = array( 'from' => "{$installDefPrefix}/relationships/layoutdefs/{$relationshipName}_{$moduleName}.php" , 'to_module' => $moduleName ) ;
        }
        return $installDefs ;
    }


    /*
     * Translate a set of linkFieldDefinitions into files for the Module Loader
     * Note that the Module Loader will only accept one entry in the vardef section of the Manifest for each module
     * This means that we cannot simply build a file for each relationship as relationships that involve the same module will end up overwriting each other when installed
     * So we have to append the vardefs for each relationship to a single file for each module
     * @param string $basepath              Basepath location for this module
     * @param $installDefPrefix             Pathname prefix for the installdefs, for example for ModuleBuilder use "<basepath>/SugarModules"
     * @param string $relationshipName      Name of this relationship (for uniqueness)
     * @param array $linkFieldDefinitions   Set of link field definitions in the form $linkFieldDefinitions[$for_module]
     * @return array $installDefs           Set of new installDefs
     */
    protected function saveVardefs($basepath, $installDefPrefix, $relationshipName, $vardefs)
    {
        $installDefs = [];
        mkdir_recursive("$basepath/vardefs/") ;
        $GLOBALS [ 'log' ]->debug(get_class($this) . "->saveVardefs(): vardefs =" . print_r($vardefs, true)) ;

        foreach ($vardefs as $moduleName => $definitions) {
            // find this module's Object name - the object name, not the module name, is used as the key in the vardefs...
            if (isset($GLOBALS [ 'beanList' ] [ $moduleName ])) {
                $module = get_module_info($moduleName) ;
                $object = $module->object_name ;
            } else {
                $object = $moduleName ;
            }

            $relName = $moduleName;
            foreach ($definitions as $definition) {
                if (!empty($definition['relationship'])) {
                    $relName = $definition['relationship'];
                    break;
                }
            }

            $filename = "$basepath/vardefs/{$relName}_{$moduleName}.php" ;

            $out =  "<?php\n// created: " . date('Y-m-d H:i:s') . "\n";
            foreach ($definitions as $definition) {
                $GLOBALS [ 'log' ]->debug(get_class($this) . "->saveVardefs(): saving the following to {$filename}" . print_r($definition, true)) ;
                $out .= '$dictionary["' . $object . '"]["fields"]["' . $definition [ 'name' ] . '"] = '
                         . var_export_helper($definition) . ";\n";
            }
            sugar_file_put_contents($filename, $out);

            $installDefs [ $moduleName ] = array(
                'from' => "{$installDefPrefix}/relationships/vardefs/{$relName}_{$moduleName}.php" ,
                'to_module' => $moduleName
            ) ;
        }

        $GLOBALS [ 'log' ]->debug(get_class($this) . "->saveVardefs(): installDefs =" . print_r($installDefs, true)) ;

        return $installDefs ;
    }

    /*
     * Determine if we're dealing with a deployed or undeployed module based on the name
     * Undeployed modules are those known to ModuleBuilder; the twist is that the deployed names of modulebuilder modules are keyname_modulename not packagename_modulename
     * and ModuleBuilder doesn't have any accessor methods based around keys, so we must convert keynames to packagenames
     * @param $deployedName Name of the module in the deployed form - that is, keyname_modulename or modulename
     * @return array ('moduleName'=>name, 'packageName'=>package) if undeployed, ('moduleName'=>name) if deployed
     */
    public static function parseDeployedModuleName($deployedName)
    {
        require_once 'modules/ModuleBuilder/MB/ModuleBuilder.php' ;
        $mb = new ModuleBuilder() ;

        $packageName = '' ;
        $moduleName = $deployedName ;

        foreach ($mb->getPackageList() as $name) {
            // convert the keyName into a packageName, needed for checking to see if this is really an undeployed module, or just a module with a _ in the name...
            $package = $mb->getPackage($name) ; // seem to need to call getPackage twice to get the key correctly... TODO: figure out why...
            $key = $mb->getPackage($name)->key ;
            if (strlen((string) $key) < strlen((string) $deployedName)) {
                $position = stripos((string) $deployedName, (string) $key) ;
                $moduleName = trim(substr((string) $deployedName, strlen((string) $key)), '_'); //use trim rather than just assuming that _ is between packageName and moduleName in the deployedName
                if ($position !== false && $position == 0 && (isset($mb->packages [ $name ]->modules [ $moduleName ]))) {
                    $packageName = $name ;
                    break ;
                }
            }
        }

        if (! empty($packageName)) {
            return array( 'moduleName' => $moduleName , 'packageName' => $packageName ) ;
        } else {
            return array( 'moduleName' => $deployedName ) ;
        }
    }
}
