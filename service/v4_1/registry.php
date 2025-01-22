<?php
if (!defined('sugarEntry')) {
    define('sugarEntry', true);
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


require_once('service/v4/registry.php');

#[\AllowDynamicProperties]
class registry_v4_1 extends registry_v4
{


    /**
     * registerFunction
     *
     * Registers all the functions on the service class
     *
     */
    protected function registerFunction()
    {
        $GLOBALS['log']->info('Begin: registry->registerFunction');
        parent::registerFunction();

        //Add get_relationships with "pagination" support
        $this->serviceClass->registerFunction(
            'get_relationships',
            array('session'=>'xsd:string', 'module_name'=>'xsd:string', 'module_id'=>'xsd:string', 'link_field_name'=>'xsd:string', 'related_module_query'=>'xsd:string', 'related_fields'=>'tns:select_fields', 'related_module_link_name_to_fields_array'=>'tns:link_names_to_fields_array', 'deleted'=>'xsd:int', 'order_by'=>'xsd:string', 'offset'=>'xsd:int' , 'limit'=>'xsd:int'),
            array('return'=>'tns:get_entry_result_version2')
        );


        //Add get_modified_relationship function
        $this->serviceClass->registerFunction(
            'get_modified_relationships',
            array('session'=>'xsd:string', 'module_name'=>'xsd:string','related_module'=>'xsd:string', 'from_date'=>'xsd:string', 'to_date'=>'xsd:string','offset'=>'xsd:int', 'max_results'=>'xsd:int','deleted'=>'xsd:int', 'module_user_id'=>'xsd:string', 'select_fields'=>'tns:select_fields', 'relationship_name'=>'xsd:string', 'deletion_date'=>'xsd:string'),
            array('return'=>'tns:modified_relationship_result')
        );
    }


    /**
     * This method registers all the complex types
     *
     */
    protected function registerTypes()
    {
        parent::registerTypes();

        $this->serviceClass->registerType(
            'error_value',
            'complexType',
            'struct',
            'all',
            '',
            array(
                   'number'=>array('name'=>'number', 'type'=>'xsd:string'),
                   'name'=>array('name'=>'name', 'type'=>'xsd:string'),
                   'description'=>array('name'=>'description', 'type'=>'xsd:string'),
               )
           );

        //modified_relationship_entry_list
        //This type holds the array of modified_relationship_entry types
        $this->serviceClass->registerType(
            'modified_relationship_entry_list',
            'complexType',
            'array',
            '',
            'SOAP-ENC:Array',
            array(),
            array(
                    array('ref'=>'SOAP-ENC:arrayType', 'wsdl:arrayType'=>'tns:modified_relationship_entry[]')
                ),
            'tns:modified_relationship_entry'
            );

        //modified_relationship_entry
        //This type consists of id, module_name and name_value_list type
        $this->serviceClass->registerType(
            'modified_relationship_entry',
            'complexType',
            'struct',
            'all',
            '',
            array(
                     'id' => array('name'=>'id', 'type'=>'xsd:string'),
                     'module_name' => array('name'=>'module_name', 'type'=>'xsd:string'),
                     'name_value_list' => array('name'=>'name_value_lists', 'type'=>'tns:name_value_list')
                 )
            );

        //modified_relationship_result
        //the top level result array
        $this->serviceClass->registerType(
            'modified_relationship_result',
            'complexType',
            'struct',
            'all',
            '',
            array(
                   'result_count' => array('name'=>'result_count', 'type'=>'xsd:int'),
                   'next_offset' => array('name'=>'next_offset', 'type'=>'xsd:int'),
                   'entry_list' => array('name'=>'entry_list', 'type'=>'tns:modified_relationship_entry_list'),
                   'error' => array('name' =>'error', 'type'=>'tns:error_value'),
                )
           );
    }
}
