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


require_once 'modules/ModuleBuilder/parsers/relationships/AbstractRelationship.php' ;
require_once 'modules/ModuleBuilder/parsers/relationships/OneToManyRelationship.php' ;
require_once 'modules/ModuleBuilder/parsers/constants.php' ;

/*
 * Class to manage the metadata for a many-To-one Relationship
 * Exactly the same as a one-to-many relationship except lhs and rhs modules have been reversed.
 */

class ManyToOneRelationship extends AbstractRelationship
{
    

    public $one_to_many;
    public $relationship_only;
    public $lhs_module;
    public $relationship_name;
    /*
     * Constructor
     * @param array $definition Parameters passed in as array defined in parent::$definitionKeys
     * The lhs_module value is for the One side; the rhs_module value is for the Many
     */
    public function __construct($definition)
    {
        parent::__construct($definition) ;
        $onetomanyDef = array_merge($definition, array(
            'rhs_label'    => isset($definition['lhs_label'])    ? $definition['lhs_label']    : null,
            'lhs_label'    => isset($definition['rhs_label'])    ? $definition['rhs_label']    : null,
            'lhs_subpanel' => isset($definition['rhs_subpanel']) ? $definition['rhs_subpanel'] : null,
            'rhs_subpanel' => isset($definition['lhs_subpanel']) ? $definition['lhs_subpanel'] : null,
            'lhs_module'   => isset($definition['rhs_module'])   ? $definition['rhs_module']   : null,
            'lhs_table'    => isset($definition['rhs_table'])    ? $definition['rhs_table']    : null,
            'lhs_key'      => isset($definition['rhs_key'])      ? $definition['rhs_key']      : null,
            'rhs_module'   => isset($definition['lhs_module'])   ? $definition['lhs_module']   : null,
            'rhs_table'    => isset($definition['lhs_table'])    ? $definition['lhs_table']    : null,
            'rhs_key'      => isset($definition['lhs_key'])      ? $definition['lhs_key']      : null,
            'join_key_lhs' => isset($definition['join_key_rhs']) ? $definition['join_key_rhs'] : null,
            'join_key_rhs' => isset($definition['join_key_lhs']) ? $definition['join_key_lhs'] : null,
            'relationship_type' => MB_ONETOMANY,
        ));
        $this->one_to_many = new OneToManyRelationship($onetomanyDef);
    }

    /*
     * BUILD methods called during the build
     */
    
    public function buildLabels($update = false)
    {
        return $this->one_to_many->buildLabels();
    }
    
    /*
     * Construct subpanel definitions
     * The format is that of TO_MODULE => relationship, FROM_MODULE, FROM_MODULES_SUBPANEL, mimicking the format in the layoutdefs.php
     * @return array    An array of subpanel definitions, keyed by the module
     */
    public function buildSubpanelDefinitions()
    {
        return $this->one_to_many->buildSubpanelDefinitions();
    }


    /*
     * @return array    An array of field definitions, ready for the vardefs, keyed by module
     */
    public function buildVardefs()
    {
        return $this->one_to_many->buildVardefs();
    }
    
    /*
     * Define what fields to add to which modules layouts
     * @return array    An array of module => fieldname
     */
    public function buildFieldsToLayouts()
    {
        if ($this->relationship_only) {
            return array() ;
        }

        return array( $this->lhs_module => static::getValidDBName($this->relationship_name . "_name") ) ; // this must match the name of the relate field from buildVardefs
    }
       
    /*
     * @return array    An array of relationship metadata definitions
     */
    public function buildRelationshipMetaData()
    {
        return $this->one_to_many->buildRelationshipMetaData();
    }
    
    public function setName($relationshipName)
    {
        parent::setName($relationshipName);
        $this->one_to_many->setname($relationshipName);
    }
    
    public function setReadonly($set = true)
    {
        parent::setReadonly($set);
        $this->one_to_many->setReadonly();
    }
    
    public function delete()
    {
        parent::delete();
        $this->one_to_many->delete();
    }
    
    public function setRelationship_only()
    {
        parent::setRelationship_only();
        $this->one_to_many->setRelationship_only();
    }
}
