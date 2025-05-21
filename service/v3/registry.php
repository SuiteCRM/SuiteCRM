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


require_once('service/v2/registry.php'); //Extend off of v2 registry

#[\AllowDynamicProperties]
class registry_v3 extends registry
{
    
    /**
     * This method registers all the functions on the service class
     *
     */
    protected function registerFunction()
    {
        $GLOBALS['log']->info('Begin: registry->registerFunction');
        parent::registerFunction();

        $this->serviceClass->registerFunction(
            'get_module_fields_md5',
            array('session'=>'xsd:string', 'module_names'=>'tns:select_fields'),
            array('return'=>'tns:md5_results')
        );

        $this->serviceClass->registerFunction(
            'get_available_modules',
            array('session'=>'xsd:string','filter'=>'xsd:string'),
            array('return'=>'tns:module_list')
        );

        $this->serviceClass->registerFunction(
            'get_last_viewed',
            array('session'=>'xsd:string','module_names'=>'tns:module_names'),
            array('return'=>'tns:last_viewed_list')
        );

        $this->serviceClass->registerFunction(
            'get_upcoming_activities',
            array('session'=>'xsd:string'),
            array('return'=>'tns:upcoming_activities_list')
        );

        $this->serviceClass->registerFunction(
            'search_by_module',
            array('session'=>'xsd:string','search_string'=>'xsd:string', 'modules'=>'tns:select_fields', 'offset'=>'xsd:int', 'max_results'=>'xsd:int','assigned_user_id' => 'xsd:string', 'select_fields'=>'tns:select_fields'),
            array('return'=>'tns:return_search_result')
        );

        $this->serviceClass->registerFunction(
            'get_relationships',
            array('session'=>'xsd:string', 'module_name'=>'xsd:string', 'module_id'=>'xsd:string', 'link_field_name'=>'xsd:string', 'related_module_query'=>'xsd:string', 'related_fields'=>'tns:select_fields', 'related_module_link_name_to_fields_array'=>'tns:link_names_to_fields_array', 'deleted'=>'xsd:int', 'order_by'=>'xsd:string',),
            array('return'=>'tns:get_entry_result_version2')
        );

        $GLOBALS['log']->info('END: registry->registerFunction');

        // END OF REGISTER FUNCTIONS
    }
    
    /**
     * This method registers all the complex types
     *
     */
    protected function registerTypes()
    {
        parent::registerTypes();
        
        $this->serviceClass->registerType(
            'md5_results',
            'complexType',
            'array',
            '',
            'SOAP-ENC:Array',
            array(),
            array(
               array('ref'=>'SOAP-ENC:arrayType', 'wsdl:arrayType'=>'xsd:string[]')
            ),
            'xsd:string'
        );
        
        $this->serviceClass->registerType(
            'module_names',
            'complexType',
            'array',
            '',
            'SOAP-ENC:Array',
            array(),
            array(
               array('ref'=>'SOAP-ENC:arrayType', 'wsdl:arrayType'=>'xsd:string[]')
            ),
            'xsd:string'
        );
        
        $this->serviceClass->registerType(
            'upcoming_activities_list',
            'complexType',
            'array',
            '',
            'SOAP-ENC:Array',
            array(),
            array(
                array('ref'=>'SOAP-ENC:arrayType','wsdl:arrayType'=>'tns:upcoming_activity_entry[]')
            ),
            'tns:upcoming_activity_entry'
        );
        
        $this->serviceClass->registerType(
            'upcoming_activity_entry',
            'complexType',
            'struct',
            'all',
            '',
            array(
                "id" => array('name'=>"id",'type'=>'xsd:string'),
                "module" => array('name'=>"module",'type'=>'xsd:string'),
                "date_due" => array('name'=>"date_due",'type'=>'xsd:string'),
                "summary" => array('name'=>"summary",'type'=>'xsd:string'),
            )
        );
        
        $this->serviceClass->registerType(
            'last_viewed_list',
            'complexType',
            'array',
            '',
            'SOAP-ENC:Array',
            array(),
            array(
                array('ref'=>'SOAP-ENC:arrayType','wsdl:arrayType'=>'tns:last_viewed_entry[]')
            ),
            'tns:last_viewed_entry'
        );
        
        $this->serviceClass->registerType(
            'last_viewed_entry',
            'complexType',
            'struct',
            'all',
            '',
            array(
                "id" => array('name'=>"id",'type'=>'xsd:string'),
                "item_id" => array('name'=>"item_id",'type'=>'xsd:string'),
                "item_summary" => array('name'=>"item_summary",'type'=>'xsd:string'),
                "module_name" => array('name'=>"module_name",'type'=>'xsd:string'),
                "monitor_id" => array('name'=>"monitor_id",'type'=>'xsd:string'),
                "date_modified" => array('name'=>"date_modified",'type'=>'xsd:string')
            )
        );
        
        $this->serviceClass->registerType(
            'field',
            'complexType',
            'struct',
            'all',
            '',
            array(
                    'name'=>array('name'=>'name', 'type'=>'xsd:string'),
                    'type'=>array('name'=>'type', 'type'=>'xsd:string'),
                    'group'=>array('name'=>'group', 'type'=>'xsd:string'),
                    'label'=>array('name'=>'label', 'type'=>'xsd:string'),
                    'required'=>array('name'=>'required', 'type'=>'xsd:int'),
                    'options'=>array('name'=>'options', 'type'=>'tns:name_value_list'),
                    'default_value'=>array('name'=>'name', 'type'=>'xsd:string'),
                )
        );
    }
}
