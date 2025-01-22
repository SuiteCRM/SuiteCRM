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


require_once 'modules/ModuleBuilder/parsers/relationships/OneToManyRelationship.php' ;

/*
 * Class to manage the metadata for a One-To-Many Relationship
 * The One-To-Many relationships created by this class are a combination of a subpanel and a custom relate field
 * The LHS (One) module will receive a new subpanel for the RHS module. The subpanel gets its data ('get_subpanel_data') from a link field that references a new Relationship
 * The RHS (Many) module will receive a new relate field to point back to the LHS
 *
 * OOB modules implement One-To-Many relationships as:
 *
 * On the LHS (One) side:
 * A Relationship of type one-to-many in the rhs modules vardefs.php
 * A link field in the same vardefs.php with 'relationship'= the relationship name and 'source'='non-db'
 * A subpanel which gets its data (get_subpanel_data) from the link field
 *
 * On the RHS (Many) side:
 * A Relate field in the vardefs, formatted as in this example, which references a link field:
 * 'name' => 'account_name',
 * 'rname' => 'name',
 * 'id_name' => 'account_id',
 * 'vname' => 'LBL_ACCOUNT_NAME',
 * 'join_name'=>'accounts',
 * 'type' => 'relate',
 * 'link' => 'accounts',
 * 'table' => 'accounts',
 * 'module' => 'Accounts',
 * 'source' => 'non-db'
 * A link field which references the shared Relationship
 */

#[\AllowDynamicProperties]
class ActivitiesRelationship extends OneToManyRelationship
{
    protected static $subpanelsAdded = array();
    protected static $labelsAdded = array();

    /*
     * Constructor
     * @param array $definition Parameters passed in as array defined in parent::$definitionKeys
     * The lhs_module value is for the One side; the rhs_module value is for the Many
     */
    public function __construct($definition)
    {
        parent::__construct($definition) ;
    }

    /*
     * BUILD methods called during the build
     */

    /*
     * Define the labels to be added to the module for the new relationships
     * @return array    An array of system value => display value
     */
    public function buildLabels($update = false)
    {
        $labelDefinitions = array( ) ;
        if (!$this->relationship_only) {
            if (!isset(ActivitiesRelationship::$labelsAdded[$this->lhs_module])) {
                foreach (getTypeDisplayList() as $typeDisplay) {
                    $labelDefinitions [] = array(
                        'module' => 'application',
                        'system_label' => $typeDisplay,
                        'display_label' => array(
                            $this->lhs_module => $this->lhs_label ? $this->lhs_label : ucfirst($this->lhs_module)
                        ),
                    );
                }
            }

            $rhs_display_label = '';
            if (!empty($this->rhs_label)) {
                $rhs_display_label .= $this->rhs_label . ':';
            }
            $rhs_display_label .= translate($this->rhs_module);

            $lhs_display_label = '';
            if (!empty($this->rhs_label)) {
                $lhs_display_label .= $this->rhs_label . ':';
            }
            $lhs_display_label .= translate($this->lhs_module);

            $labelDefinitions[] = array(
                'module' => $this->lhs_module ,
                'system_label' => 'LBL_' . strtoupper($this->relationship_name . '_FROM_' . $this->getRightModuleSystemLabel()) . '_TITLE',
                'display_label' => $rhs_display_label
            );
            $labelDefinitions[] = array(
                'module' => $this->rhs_module,
                'system_label' => 'LBL_' . strtoupper($this->relationship_name . '_FROM_' . $this->getLeftModuleSystemLabel()) . '_TITLE',
                'display_label' => $lhs_display_label
            );

            ActivitiesRelationship::$labelsAdded[$this->lhs_module] = true;
        }
        return $labelDefinitions ;
    }


    /*
     * @return array    An array of field definitions, ready for the vardefs, keyed by module
     */
    public function buildVardefs()
    {
        $vardefs = array( ) ;

        $vardefs [ $this->rhs_module ] [] = $this->getLinkFieldDefinition($this->lhs_module, $this->relationship_name) ;
        $vardefs [ $this->lhs_module ] [] = $this->getLinkFieldDefinition($this->rhs_module, $this->relationship_name) ;


        return $vardefs ;
    }

    protected function getLinkFieldDefinition($sourceModule, $relationshipName, $right_side = false, $vname = "", $id_name = false)
    {
        $vardef = array( ) ;
        $vardef [ 'name' ] = $relationshipName;
        $vardef [ 'type' ] = 'link' ;
        $vardef [ 'relationship' ] = $relationshipName ;
        $vardef [ 'source' ] = 'non-db' ;
        $vardef [ 'module' ] = $sourceModule ;
        $vardef [ 'bean_name' ] = BeanFactory::getObjectName($sourceModule) ;
        $vardef [ 'vname' ] = strtoupper("LBL_{$relationshipName}_FROM_{$sourceModule}_TITLE");
        return $vardef ;
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

        return array( $this->rhs_module => $this->relationship_name . "_name" ) ; // this must match the name of the relate field from buildVardefs
    }

    public function buildSubpanelDefinitions()
    {
        if ($this->relationship_only || isset(ActivitiesRelationship::$subpanelsAdded[$this->lhs_module])) {
            return array() ;
        }

        ActivitiesRelationship::$subpanelsAdded[$this->lhs_module] = true;
        $relationshipName = substr((string) $this->relationship_name, 0, strrpos((string) $this->relationship_name, '_'));
        return array( $this->lhs_module => array(
                      'activities' => $this->buildActivitiesSubpanelDefinition($relationshipName),
                      'history' => $this->buildHistorySubpanelDefinition($relationshipName) ,
                    ));
    }

    /*
     * @return array    An array of relationship metadata definitions
     */
    public function buildRelationshipMetaData()
    {
        $relationshipName = $this->definition [ 'relationship_name' ];
        $relMetadata = array( ) ;
        $relMetadata [ 'lhs_module' ] = $this->definition [ 'lhs_module' ] ;
        $relMetadata [ 'lhs_table' ] = $this->getTablename($this->definition [ 'lhs_module' ]) ;
        $relMetadata [ 'lhs_key' ] = 'id' ;
        $relMetadata [ 'rhs_module' ] = $this->definition [ 'rhs_module' ] ;
        $relMetadata [ 'rhs_table' ] = $this->getTablename($this->definition [ 'rhs_module' ]) ;
        $relMetadata ['rhs_key'] = 'parent_id';
        $relMetadata ['relationship_type'] = 'one-to-many';
        $relMetadata ['relationship_role_column'] = 'parent_type';
        $relMetadata ['relationship_role_column_value'] = $this->definition [ 'lhs_module' ] ;

        return array( $this->lhs_module => array(
            'relationships' => array($relationshipName => $relMetadata),
            'fields' => '', 'indices' => '', 'table' => '')
        ) ;
    }

    /*
         * Shortcut to construct an Activities collection subpanel
         * @param AbstractRelationship $relationship    Source relationship to Activities module
         */
    protected function buildActivitiesSubpanelDefinition($relationshipName)
    {
        return array(
            'order' => 10 ,
            'sort_order' => 'desc' ,
            'sort_by' => 'date_due' ,
            'title_key' => 'LBL_ACTIVITIES_SUBPANEL_TITLE' ,
            'type' => 'collection' ,
            'subpanel_name' => 'activities' , //this value is not associated with a physical file
            'module' => 'Activities' ,
            'top_buttons' => array(
                array( 'widget_class' => 'SubPanelTopCreateTaskButton' ) ,
                array( 'widget_class' => 'SubPanelTopScheduleMeetingButton' ) ,
                array( 'widget_class' => 'SubPanelTopScheduleCallButton' ) ,
                array( 'widget_class' => 'SubPanelTopComposeEmailButton' ) ) ,
                'collection_list' => array(
                    'meetings' => array(
                        'module' => 'Meetings' ,
                        'subpanel_name' => 'ForActivities' ,
                        'get_subpanel_data' => $relationshipName. '_meetings' ) ,
                    'tasks' => array(
                        'module' => 'Tasks' ,
                        'subpanel_name' => 'ForActivities' ,
                        'get_subpanel_data' => $relationshipName. '_tasks' ) ,
                    'calls' => array(
                        'module' => 'Calls' ,
                        'subpanel_name' => 'ForActivities' ,
                        'get_subpanel_data' => $relationshipName. '_calls' ) ) ) ;
    }

    /*
     * Shortcut to construct a History collection subpanel
     * @param AbstractRelationship $relationship    Source relationship to Activities module
     */
    protected function buildHistorySubpanelDefinition($relationshipName)
    {
        return array(
            'order' => 20 ,
            'sort_order' => 'desc' ,
            'sort_by' => 'date_modified' ,
            'title_key' => 'LBL_HISTORY' ,
            'type' => 'collection' ,
            'subpanel_name' => 'history' , //this values is not associated with a physical file.
            'module' => 'History' ,
            'top_buttons' => array(
                array( 'widget_class' => 'SubPanelTopCreateNoteButton' ) ,
                array( 'widget_class' => 'SubPanelTopArchiveEmailButton'),
                array( 'widget_class' => 'SubPanelTopSummaryButton' ) ) ,
                'collection_list' => array(
                    'meetings' => array(
                        'module' => 'Meetings' ,
                        'subpanel_name' => 'ForHistory' ,
                        'get_subpanel_data' => $relationshipName. '_meetings' ) ,
                    'tasks' => array(
                        'module' => 'Tasks' ,
                        'subpanel_name' => 'ForHistory' ,
                        'get_subpanel_data' => $relationshipName. '_tasks' ) ,
                    'calls' => array(
                        'module' => 'Calls' ,
                        'subpanel_name' => 'ForHistory' ,
                        'get_subpanel_data' => $relationshipName. '_calls' ) ,
                    'notes' => array(
                        'module' => 'Notes' ,
                        'subpanel_name' => 'ForHistory' ,
                        'get_subpanel_data' => $relationshipName. '_notes' ) ,
                    'emails' => array(
                        'module' => 'Emails' ,
                        'subpanel_name' => 'ForHistory' ,
                        'get_subpanel_data' => $relationshipName. '_emails' ) ) )  ;
    }
}
