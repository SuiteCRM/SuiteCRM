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


require_once 'modules/ModuleBuilder/parsers/relationships/UndeployedRelationships.php' ;
require_once 'modules/ModuleBuilder/parsers/relationships/AbstractRelationships.php' ;
require_once 'modules/ModuleBuilder/parsers/relationships/AbstractRelationship.php' ;
require_once 'modules/ModuleBuilder/parsers/relationships/ManyToManyRelationship.php' ;

/*
 * This is an Adapter for the new UndeployedRelationships Class to allow ModuleBuilder to use the new class without change
 * As ModuleBuilder is updated, references to this MBRelationship class should be replaced by direct references to UndeployedRelationships
 */

#[\AllowDynamicProperties]
class MBRelationship
{
    public $relatableModules = array( ) ; // required by MBModule
    public $relationships = array( ) ; // required by view.relationships.php; must be kept in sync with the implementation


    /*
     * Constructor
     * @param string $name      The name of this module (not used)
     * @param string $path      The base path of the module directory within the ModuleBuilder package directory
     * @param string $key_name  The Fully Qualified Name for this module - that is, $packageName_$name
     */
    public function __construct($name, $path, $key_name)
    {
        $this->implementation = new UndeployedRelationships($path) ;
        $this->moduleName = $key_name ;
        $this->path = $path ;
        $this->updateRelationshipVariable();
    }

    public function findRelatableModules()
    {
        // do not call findRelatableModules in the constructor as it leads to an infinite loop if the implementation calls getPackage() which loads the packages which loads the module which findsRelatableModules which...
        $this->relatableModules = $this->implementation->findRelatableModules() ;
    }

    /*
     * Originally in 5.0 this method expected $_POST variables keyed in the "old" format - lhs_module, relate, msub, rsub etc
     * At 5.1 this has been changed to the "new" format of lhs_module, rhs_module, lhs_subpanel, rhs_subpanel, label
     * @return AbstractRelationship
     */
    public function addFromPost()
    {
        return $this->implementation->addFromPost() ;
    }

    /*
     * New function to replace the old MBModule subpanel property - now we obtain the 'subpanels' (actually related modules) from the relationships object
     */
    public function getRelationshipList()
    {
        return $this->implementation->getRelationshipList() ;
    }

    public function get($relationshipName)
    {
        return $this->implementation->get($relationshipName) ;
    }

    /*
     * Deprecated
     * Add a relationship to this set
     * Original MBRelationships could only support one relationship between this module and any other
     */
    /*
    function addRelationship ($name , $relatedTo , $relatedSubpanel = 'default' , $mysubpanel = 'default' , $type)
    {
        $this->implementation->add ( new ManyToManyRelationship ( $name, $this->moduleName, $relatedTo, $mysubpanel, $relatedSubpanel ) ) ;
        $this->updateRelationshipVariable () ;
    }
*/

    /* Add a relationship to this set
     * Original MBRelationships could only support one relationship between this module and any other
     * @param array $rel    Relationship definition in the old format (defined by self::oldFormatKeys)
     */
    public function add($rel)
    {
        // convert old format definition to new format
        if (! isset($rel [ 'lhs_module' ])) {
            $rel [ 'lhs_module' ] = $this->moduleName ;
        }
        $definition = AbstractRelationships::convertFromOldFormat($rel) ;
        if (! isset($definition ['relationship_type'])) {
            $definition ['relationship_type'] = 'many-to-many';
        }
        // get relationship object from RelationshipFactory
        $relationship = RelationshipFactory::newRelationship($definition) ;
        // add relationship to the set of relationships
        $this->implementation->add($relationship) ;
        $this->updateRelationshipVariable() ;
        return $relationship;
    }

    public function delete($name)
    {
        $this->implementation->delete($name) ;
        $this->updateRelationshipVariable() ;
    }

    public function save()
    {
        $this->implementation->save() ;
    }

    public function build($path)
    {
        $this->implementation->build() ;
    }

    public function addInstallDefs(&$installDef)
    {
        $this->implementation->addInstallDefs($installDef) ;
    }

    /*
    function load ()
    {
        $this->implementation->load () ;
        $this->updateRelationshipVariable () ;
    }
*/
    /*
     * Transitional function to keep the public relationship variable in sync with the implementation master copy
     * We have to do this as various things refer directly to MBRelationship->relationships...
     */

    private function updateRelationshipVariable()
    {
        foreach ($this->implementation->getRelationshipList() as $relationshipName) {
            $rel = $this->implementation->getOldFormat($relationshipName) ;
            $this->relationships [ $relationshipName ] = $rel ;
        }
    }
}
