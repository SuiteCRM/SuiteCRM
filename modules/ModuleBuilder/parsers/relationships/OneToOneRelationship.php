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

/*
 * Class to manage the metadata for a One-To-One Relationship
 * The LHS module will receive a new relate field to point back to the RHS
 * The RHS module will receive a new relate field to point back to the LHS
 *
 * OOB modules implement One-To-One relationships as:
 * A Relationship of type one-to-one in one modules vardefs.php
 * A single link field in the same vardefs.php with 'relationship'= the relationship name, and 'link-type'='one', 'Module'=other side, and 'source'='non-db'
 * These are not common - examples are in InboundEmail and Schedulers, both pre-5.0 modules
 * InboundEmail:
 *      'created_by_link' => array (
            'name' => 'created_by_link',
            'type' => 'link',
            'relationship' => 'inbound_email_created_by',
            'vname' => 'LBL_CREATED_BY_USER',
            'link_type' => 'one',
            'module' => 'Users',
            'bean_name' => 'User',
            'source' => 'non-db',
        ),

 */

class OneToOneRelationship extends AbstractRelationship
{

    public $lhs_module;
    public $rhs_module;
    public $relationship_name;
    public $relationship_only;
    /*
     * Constructor
     * @param array $definition Parameters passed in as array with keys defined in parent::keys
     */
    public function __construct($definition)
    {
        parent::__construct($definition) ;
    }
    
    /*
     * BUILD methods called during the build
     */
    
    /*
     * @return array    An array of relationship metadata definitions
     */
    public function buildRelationshipMetaData()
    {
        return array( $this->lhs_module => $this->getRelationshipMetaData(MB_ONETOONE) ) ;
    }

    /* Build a set of Link Field definitions for this relationship
     * @return array    An array of field definitions, ready for the vardefs, keyed by module
     */
    public function buildVardefs()
    {
        $vardefs = array( ) ;
        $vardefs [ $this->rhs_module ] [] = $this->getLinkFieldDefinition(
            $this->lhs_module,
            $this->relationship_name,
            false,
            'LBL_' . strtoupper($this->relationship_name . '_FROM_' . $this->getLeftModuleSystemLabel()) . '_TITLE',
            $this->relationship_only ? false : $this->getIDName($this->lhs_module)
        ) ;
        $vardefs [ $this->lhs_module ] [] = $this->getLinkFieldDefinition(
            $this->rhs_module,
            $this->relationship_name,
            false,
            'LBL_' . strtoupper($this->relationship_name . '_FROM_' . $this->getRightModuleSystemLabel()) . '_TITLE',
            $this->relationship_only ? false : $this->getIDName($this->rhs_module)
        ) ;
        
        if (!$this->relationship_only) {
            $vardefs [ $this->lhs_module ] [] = $this->getRelateFieldDefinition($this->rhs_module, $this->relationship_name, $this->getRightModuleSystemLabel()) ;
            $vardefs [ $this->rhs_module ] [] = $this->getRelateFieldDefinition($this->lhs_module, $this->relationship_name, $this->getLeftModuleSystemLabel()) ;
            $vardefs [ $this->lhs_module ] [] = $this->getLink2FieldDefinition(
                $this->rhs_module,
                $this->relationship_name,
                false,
                'LBL_' . strtoupper($this->relationship_name . '_FROM_' . $this->getRightModuleSystemLabel()) . '_TITLE'
            ) ;
            $vardefs [ $this->rhs_module ] [] = $this->getLink2FieldDefinition(
                $this->lhs_module,
                $this->relationship_name,
                false,
                'LBL_' . strtoupper($this->relationship_name . '_FROM_' . $this->getLeftModuleSystemLabel()) . '_TITLE'
            ) ;
        }
        
        return $vardefs ;
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
 
        if ($this->lhs_module == $this->rhs_module) { // don't add in two fields on recursive relationships
            return array( $this->lhs_module => static::getValidDBName($this->relationship_name . "_name") );
        } else {
            return array(
                $this->lhs_module => static::getValidDBName($this->relationship_name . "_name") ,
                $this->rhs_module => static::getValidDBName($this->relationship_name . "_name")
            ) ;
        }
    }
}
