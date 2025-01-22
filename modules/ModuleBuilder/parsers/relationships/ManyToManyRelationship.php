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
 * Class to manage the metadata for a Many-To-Many Relationship
 * The LHS (One) module will receive a new subpanel for the RHS module
 * The RHS (Many) module will receive a new subpanel for the RHS module
 * The subpanels get their data ('get_subpanel_data') from two link fields (one each) that reference a new Relationship
 *
 * In OOB modules it's done the same way (e.g. cases_bugs)
 */

class ManyToManyRelationship extends AbstractRelationship
{

    public $relationship_only;
    public $rhs_module;
    public $relationship_name;
    public $lhs_module;
    public $lhs_subpanel;
    public $rhs_subpanel;
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
     * Construct subpanel definitions
     * The format is that of TO_MODULE => relationship, FROM_MODULE, FROM_MODULES_SUBPANEL, mimicking the format in the layoutdefs.php
     * @return array    An array of subpanel definitions, keyed by module
     */
    public function buildSubpanelDefinitions()
    {
        $subpanelDefinitions = array( ) ;
        if (!$this->relationship_only) {
            $subpanelDefinitions [ $this->rhs_module ] = $this->getSubpanelDefinition($this->relationship_name, $this->lhs_module, $this->lhs_subpanel, $this->getLeftModuleSystemLabel()) ;
            $subpanelDefinitions [ $this->lhs_module ] = $this->getSubpanelDefinition($this->relationship_name, $this->rhs_module, $this->rhs_subpanel, $this->getRightModuleSystemLabel()) ;
        }
        return $subpanelDefinitions ;
    }


    /*
     * @return array    An array of field definitions, ready for the vardefs, keyed by module
     */
    public function buildVardefs()
    {
        $vardefs = array( ) ;
        $vardefs [ $this->rhs_module ] [] = $this->getLinkFieldDefinition(
            $this->lhs_module,
            $this->relationship_name,
            false,
            'LBL_' . strtoupper($this->relationship_name . '_FROM_' . $this->getLeftModuleSystemLabel()) . '_TITLE'
        ) ;
        $vardefs [ $this->lhs_module ] [] = $this->getLinkFieldDefinition(
            $this->rhs_module,
            $this->relationship_name,
            false,
            'LBL_' . strtoupper($this->relationship_name . '_FROM_' . $this->getRightModuleSystemLabel()) . '_TITLE'
        ) ;
        return $vardefs ;
    }
    
    /*
     * @return array    An array of relationship metadata definitions
     */
    public function buildRelationshipMetaData()
    {
        return array( $this->lhs_module => $this->getRelationshipMetaData(MB_MANYTOMANY) ) ;
    }
}
