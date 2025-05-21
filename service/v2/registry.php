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


/**
 * This class is responsible for providing all the registration of all the functions and complex types
 *
 */
#[\AllowDynamicProperties]
class registry
{
    protected $serviceClass = null;

    /**
     * Constructor.
     *
     * @param Class - $serviceClass
     */
    public function __construct($serviceClass)
    {
        $this->serviceClass = $serviceClass;
    } // fn

    /**
     * It registers all the functions and types by doign a call back method on service object
     *
     */
    public function register()
    {
        $this->registerFunction();
        $this->registerTypes();
    }

    /**
     * This mehtod registers all the functions on the service class
     *
     */
    protected function registerFunction()
    {
        // START OF REGISTER FUNCTIONS

        $GLOBALS['log']->info('Begin: registry->registerFunction');

        $this->serviceClass->registerFunction(
            'login',
            array('user_auth'=>'tns:user_auth', 'application_name'=>'xsd:string', 'name_value_list'=>'tns:name_value_list'),
            array('return'=>'tns:entry_value')
             );

        $this->serviceClass->registerFunction(
            'logout',
            array('session'=>'xsd:string'),
            array()
        );

        $this->serviceClass->registerFunction(
            'get_entry',
            array('session'=>'xsd:string', 'module_name'=>'xsd:string', 'id'=>'xsd:string', 'select_fields'=>'tns:select_fields','link_name_to_fields_array'=>'tns:link_names_to_fields_array'),
            array('return'=>'tns:get_entry_result_version2')
        );

        $this->serviceClass->registerFunction(
            'get_entries',
            array('session'=>'xsd:string', 'module_name'=>'xsd:string', 'ids'=>'tns:select_fields', 'select_fields'=>'tns:select_fields', 'link_name_to_fields_array'=>'tns:link_names_to_fields_array'),
            array('return'=>'tns:get_entry_result_version2')
        );

        $this->serviceClass->registerFunction(
            'get_entry_list',
            array('session'=>'xsd:string', 'module_name'=>'xsd:string', 'query'=>'xsd:string', 'order_by'=>'xsd:string','offset'=>'xsd:int', 'select_fields'=>'tns:select_fields', 'link_name_to_fields_array'=>'tns:link_names_to_fields_array', 'max_results'=>'xsd:int', 'deleted'=>'xsd:int'),
            array('return'=>'tns:get_entry_list_result_version2')
        );

        $this->serviceClass->registerFunction(
            'set_relationship',
            array('session'=>'xsd:string','module_name'=>'xsd:string','module_id'=>'xsd:string','link_field_name'=>'xsd:string', 'related_ids'=>'tns:select_fields', 'name_value_list'=>'tns:name_value_list', 'delete'=>'xsd:int'),
            array('return'=>'tns:new_set_relationship_list_result')
        );

        $this->serviceClass->registerFunction(
            'set_relationships',
            array('session'=>'xsd:string','module_names'=>'tns:select_fields','module_ids'=>'tns:select_fields','link_field_names'=>'tns:select_fields','related_ids'=>'tns:new_set_relationhip_ids', 'name_value_lists'=>'tns:name_value_lists', 'delete_array' => 'tns:deleted_array'),
            array('return'=>'tns:new_set_relationship_list_result')
        );

        $this->serviceClass->registerFunction(
            'get_relationships',
            array('session'=>'xsd:string', 'module_name'=>'xsd:string', 'module_id'=>'xsd:string', 'link_field_name'=>'xsd:string', 'related_module_query'=>'xsd:string', 'related_fields'=>'tns:select_fields', 'related_module_link_name_to_fields_array'=>'tns:link_names_to_fields_array', 'deleted'=>'xsd:int'),
            array('return'=>'tns:get_entry_result_version2')
        );

        $this->serviceClass->registerFunction(
            'set_entry',
            array('session'=>'xsd:string', 'module_name'=>'xsd:string',  'name_value_list'=>'tns:name_value_list'),
            array('return'=>'tns:new_set_entry_result')
        );

        $this->serviceClass->registerFunction(
            'set_entries',
            array('session'=>'xsd:string', 'module_name'=>'xsd:string',  'name_value_lists'=>'tns:name_value_lists'),
            array('return'=>'tns:new_set_entries_result')
        );

        $this->serviceClass->registerFunction(
            'get_server_info',
            array(),
            array('return'=>'tns:get_server_info_result')
        );

        $this->serviceClass->registerFunction(
            'get_user_id',
            array('session'=>'xsd:string'),
            array('return'=>'xsd:string')
        );

        $this->serviceClass->registerFunction(
            'get_module_fields',
            array('session'=>'xsd:string', 'module_name'=>'xsd:string', 'fields'=>'tns:select_fields'),
            array('return'=>'tns:new_module_fields')
        );

        $this->serviceClass->registerFunction(
            'seamless_login',
            array('session'=>'xsd:string'),
            array('return'=>'xsd:int')
        );

        $this->serviceClass->registerFunction(
            'set_note_attachment',
            array('session'=>'xsd:string','note'=>'tns:new_note_attachment'),
            array('return'=>'tns:new_set_entry_result')
        );

        $this->serviceClass->registerFunction(
            'get_note_attachment',
            array('session'=>'xsd:string', 'id'=>'xsd:string'),
            array('return'=>'tns:new_return_note_attachment')
        );

        $this->serviceClass->registerFunction(
            'set_document_revision',
            array('session'=>'xsd:string','note'=>'tns:document_revision'),
            array('return'=>'tns:new_set_entry_result')
        );

        $this->serviceClass->registerFunction(
            'get_document_revision',
            array('session'=>'xsd:string','i'=>'xsd:string'),
            array('return'=>'tns:new_return_document_revision')
        );

        $this->serviceClass->registerFunction(
            'search_by_module',
            array('session'=>'xsd:string','search_string'=>'xsd:string', 'modules'=>'tns:select_fields', 'offset'=>'xsd:int', 'max_results'=>'xsd:int'),
            array('return'=>'tns:return_search_result')
        );

        $this->serviceClass->registerFunction(
            'get_available_modules',
            array('session'=>'xsd:string'),
            array('return'=>'tns:module_list')
        );

        $this->serviceClass->registerFunction(
            'get_user_team_id',
            array('session'=>'xsd:string'),
            array('return'=>'xsd:string')
        );
        $this->serviceClass->registerFunction(
            'set_campaign_merge',
            array('session'=>'xsd:string', 'targets'=>'tns:select_fields', 'campaign_id'=>'xsd:string'),
            array()
        );
        $this->serviceClass->registerFunction(
            'get_entries_count',
            array('session'=>'xsd:string', 'module_name'=>'xsd:string', 'query'=>'xsd:string', 'deleted' => 'xsd:int'),
            array('return'=>'tns:get_entries_count_result')
        );


        $GLOBALS['log']->info('END: registry->registerFunction');

        // END OF REGISTER FUNCTIONS
    } // fn

    /**
     * This method registers all the complex types
     *
     */
    protected function registerTypes()
    {

        // START OF REGISTER COMPLEX TYPES

        $GLOBALS['log']->info('Begin: registry->registerTypes');

        $this->serviceClass->registerType(
            'new_note_attachment',
            'complexType',
            'struct',
            'all',
            '',
            array(
                "id" => array('name'=>"id",'type'=>'xsd:string'),
                "filename" => array('name'=>"filename",'type'=>'xsd:string'),
                "file" => array('name'=>"file",'type'=>'xsd:string'),
                "related_module_id" => array('name'=>"related_module_id",'type'=>'xsd:string'),
                "related_module_name" => array('name'=>"related_module_name",'type'=>'xsd:string'),
            )
        );

        $this->serviceClass->registerType(
            'new_return_note_attachment',
            'complexType',
            'struct',
            'all',
            '',
            array(
                "note_attachment"=>array('name'=>'note_attachment', 'type'=>'tns:new_note_attachment'),
            )
        );

        $this->serviceClass->registerType(
            'user_auth',
            'complexType',
            'struct',
            'all',
            '',
            array(
                'user_name'=>array('name'=>'user_name', 'type'=>'xsd:string'),
                'password' => array('name'=>'password', 'type'=>'xsd:string'),
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
                    'label'=>array('name'=>'label', 'type'=>'xsd:string'),
                    'required'=>array('name'=>'required', 'type'=>'xsd:int'),
                    'options'=>array('name'=>'options', 'type'=>'tns:name_value_list'),
                    'default_value'=>array('name'=>'name', 'type'=>'xsd:string'),
                )
        );

        $this->serviceClass->registerType(
            'field_list',
            'complexType',
            'array',
            '',
            'SOAP-ENC:Array',
            array(),
            array(
                array('ref'=>'SOAP-ENC:arrayType','wsdl:arrayType'=>'tns:field[]')
            ),
            'tns:field'
        );

        $this->serviceClass->registerType(
            'link_field',
            'complexType',
            'struct',
            'all',
            '',
            array(
                    'name'=>array('name'=>'name', 'type'=>'xsd:string'),
                    'type'=>array('name'=>'type', 'type'=>'xsd:string'),
                    'relationship'=>array('name'=>'relationship', 'type'=>'xsd:string'),
                    'module'=>array('name'=>'module', 'type'=>'xsd:string'),
                    'bean_name'=>array('name'=>'bean_name', 'type'=>'xsd:string'),
                )
        );

        $this->serviceClass->registerType(
            'link_field_list',
            'complexType',
            'array',
            '',
            'SOAP-ENC:Array',
            array(),
            array(
                array('ref'=>'SOAP-ENC:arrayType','wsdl:arrayType'=>'tns:link_field[]')
            ),
            'tns:link_field'
        );

        $this->serviceClass->registerType(
            'name_value',
            'complexType',
            'struct',
            'all',
            '',
            array(
                    'name'=>array('name'=>'name', 'type'=>'xsd:string'),
                    'value'=>array('name'=>'value', 'type'=>'xsd:string'),
                )
        );

        $this->serviceClass->registerType(
            'name_value_list',
            'complexType',
            'array',
            '',
            'SOAP-ENC:Array',
            array(),
            array(
                array('ref'=>'SOAP-ENC:arrayType', 'wsdl:arrayType'=>'tns:name_value[]')
            ),
            'tns:name_value'
        );

        $this->serviceClass->registerType(
            'name_value_lists',
            'complexType',
            'array',
            '',
            'SOAP-ENC:Array',
            array(),
            array(
                array('ref'=>'SOAP-ENC:arrayType', 'wsdl:arrayType'=>'tns:name_value_list[]')
            ),
            'tns:name_value_list'
        );

        $this->serviceClass->registerType(
            'select_fields',
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
            'deleted_array',
            'complexType',
            'array',
            '',
            'SOAP-ENC:Array',
            array(),
            array(
                array('ref'=>'SOAP-ENC:arrayType', 'wsdl:arrayType'=>'xsd:int[]')
            ),
            'xsd:string'
        );

        $this->serviceClass->registerType(
            'new_module_fields',
            'complexType',
            'struct',
            'all',
            '',
            array(
                    'module_name'=>array('name'=>'module_name', 'type'=>'xsd:string'),
                    'module_fields'=>array('name'=>'module_fields', 'type'=>'tns:field_list'),
                    'link_fields'=>array('name'=>'link_fields', 'type'=>'tns:link_field_list'),
                )
        );

        $this->serviceClass->registerType(
            'entry_value',
            'complexType',
            'struct',
            'all',
            '',
            array(
                    'id'=>array('name'=>'id', 'type'=>'xsd:string'),
                    'module_name'=>array('name'=>'module_name', 'type'=>'xsd:string'),
                    'name_value_list'=>array('name'=>'name_value_list', 'type'=>'tns:name_value_list'),
                )
        );

        $this->serviceClass->registerType(
            'entry_list',
            'complexType',
            'array',
            '',
            'SOAP-ENC:Array',
            array(),
            array(
                array('ref'=>'SOAP-ENC:arrayType', 'wsdl:arrayType'=>'tns:entry_value[]')
            ),
            'tns:entry_value'
        );

        $this->serviceClass->registerType(
            'set_entries_detail_result',
            'complexType',
            'struct',
            'all',
            '',
            array(
                'name_value_lists' => array('name'=>'name_value_lists', 'type'=>'tns:name_value_lists'),
            )
        );
        $this->serviceClass->registerType(
            'link_names_to_fields_array',
            'complexType',
            'array',
            '',
            'SOAP-ENC:Array',
            array(),
            array(
                array('ref'=>'SOAP-ENC:arrayType', 'wsdl:arrayType'=>'tns:link_name_to_fields_array[]')
            ),
            'tns:link_name_to_fields_array'
        );

        $this->serviceClass->registerType(
            'link_name_to_fields_array',
            'complexType',
            'struct',
            'all',
            '',
            array(
                        'name'=>array('name'=>'name', 'type'=>'xsd:string'),
                        'value'=>array('name'=>'value', 'type'=>'tns:select_fields'),
                )
        );

        $this->serviceClass->registerType(
            'link_value',
            'complexType',
            'array',
            '',
            'SOAP-ENC:Array',
            array(),
            array(
                array('ref'=>'SOAP-ENC:arrayType', 'wsdl:arrayType'=>'tns:name_value[]')
            ),
            'tns:name_value'
        );

        $this->serviceClass->registerType(
            'link_array_list',
            'complexType',
            'array',
            '',
            'SOAP-ENC:Array',
            array(),
            array(
                array('ref'=>'SOAP-ENC:arrayType', 'wsdl:arrayType'=>'tns:link_value[]')
            ),
            'tns:link_value'
        );

        $this->serviceClass->registerType(
            'link_name_value',
            'complexType',
            'struct',
            'all',
            '',
            array(
                    'name'=>array('name'=>'name', 'type'=>'xsd:string'),
                    'records'=>array('name'=>'records', 'type'=>'tns:link_array_list'),
                )
        );

        $this->serviceClass->registerType(
            'link_list',
            'complexType',
            'array',
            '',
            'SOAP-ENC:Array',
            array(),
            array(
                array('ref'=>'SOAP-ENC:arrayType', 'wsdl:arrayType'=>'tns:link_name_value[]')
            ),
            'tns:link_name_value'
        );

        $this->serviceClass->registerType(
            'link_lists',
            'complexType',
            'array',
            '',
            'SOAP-ENC:Array',
            array(),
            array(
                array('ref'=>'SOAP-ENC:arrayType', 'wsdl:arrayType'=>'tns:link_list[]')
            ),
            'tns:link_list'
        );

        $this->serviceClass->registerType(
            'get_entry_result_version2',
            'complexType',
            'struct',
            'all',
            '',
            array(
                'entry_list' => array('name' =>'entry_list', 'type'=>'tns:entry_list'),
                'relationship_list' => array('name' =>'relationship_list', 'type'=>'tns:link_lists'),
            )
        );

        $this->serviceClass->registerType(
            'return_search_result',
            'complexType',
            'struct',
            'all',
            '',
            array(
                'entry_list' => array('name' =>'entry_list', 'type'=>'tns:link_list'),
            )
        );

        $this->serviceClass->registerType(
            'get_entry_list_result_version2',
            'complexType',
            'struct',
            'all',
            '',
            array(
                'result_count' => array('name'=>'result_count', 'type'=>'xsd:int'),
                'next_offset' => array('name'=>'next_offset', 'type'=>'xsd:int'),
                'entry_list' => array('name' =>'entry_list', 'type'=>'tns:entry_list'),
                'relationship_list' => array('name' =>'relationship_list', 'type'=>'tns:link_lists'),
            )
        );

        $this->serviceClass->registerType(
            'get_server_info_result',
            'complexType',
            'struct',
            'all',
            '',
            array(
                'flavor' => array('name'=>'id', 'type'=>'xsd:string'),
                'version' => array('name'=>'id', 'type'=>'xsd:string'),
                'gmt_time' => array('name'=>'id', 'type'=>'xsd:string'),
            )
        );

        $this->serviceClass->registerType(
            'new_set_entry_result',
            'complexType',
            'struct',
            'all',
            '',
            array(
                'id' => array('name'=>'id', 'type'=>'xsd:string'),
            )
        );

        $this->serviceClass->registerType(
            'new_set_entries_result',
            'complexType',
            'struct',
            'all',
            '',
            array(
                'ids' => array('name'=>'ids', 'type'=>'tns:select_fields'),
            )
        );

        $this->serviceClass->registerType(
            'new_set_relationhip_ids',
            'complexType',
            'array',
            '',
            'SOAP-ENC:Array',
            array(),
            array(
                array('ref'=>'SOAP-ENC:arrayType', 'wsdl:arrayType'=>'tns:select_fields[]')
            ),
            'tns:select_fields'
        );

        $this->serviceClass->registerType(
            'new_set_relationship_list_result',
            'complexType',
            'struct',
            'all',
            '',
            array(
                'created' => array('name'=>'created', 'type'=>'xsd:int'),
                'failed' => array('name'=>'failed', 'type'=>'xsd:int'),
                'deleted' => array('name'=>'deleted', 'type'=>'xsd:int'),
            )
        );

        $this->serviceClass->registerType(
            'document_revision',
            'complexType',
            'struct',
            'all',
            '',
            array(
                "id" => array('name'=>"id",'type'=>'xsd:string'),
                "document_name" => array('name'=>"document_name",'type'=>'xsd:string'),
                "revision" => array('name' => "revision", 'type'=>'xsd:string'),
                "filename" => array('name' => "filename", 'type'=>'xsd:string'),
                "file" => array('name'=>"file",'type'=>'xsd:string'),
            )
        );

        $this->serviceClass->registerType(
            'new_return_document_revision',
            'complexType',
            'struct',
            'all',
            '',
            array(
                "document_revision"=>array('name'=>'document_revision', 'type'=>'tns:document_revision'),
            )
        );


        $this->serviceClass->registerType(
            'module_list',
            'complexType',
            'struct',
            'all',
            '',
            array(
                    'modules'=>array('name'=>'modules', 'type'=>'tns:select_fields'),
                )
        );

        $this->serviceClass->registerType(
            'get_entries_count_result',
            'complexType',
            'struct',
            'all',
            '',
            array(
                'result_count'=>array('name'=>'result_count', 'type'=>'xsd:int'),
            )
        );


        $GLOBALS['log']->info('End: registry->registerTypes');

        // END OF REGISTER COMPLEX TYPES
    } // fn
} // clazz
