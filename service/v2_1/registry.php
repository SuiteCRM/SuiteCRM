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
class registry_v2_1 extends registry
{
    
    /**
     * This method registers all the functions on the service class
     *
     */
    protected function registerFunction()
    {
        $GLOBALS['log']->info('Begin: registry->registerFunction');
        parent::registerFunction();

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
            'link_list2',
            'complexType',
            'struct',
            'all',
            '',
            array(
            'link_list'=>array('name'=>'link_list', 'type'=>'tns:link_list'),
            )
        );
        
        $this->serviceClass->registerType(
            'link_lists',
            'complexType',
            'array',
            '',
            'SOAP-ENC:Array',
            array(),
            array(
                array('ref'=>'SOAP-ENC:arrayType', 'wsdl:arrayType'=>'tns:link_list2[]')
            ),
            'tns:link_list2'
        );
        
        $this->serviceClass->registerType(
            'link_array_list',
            'complexType',
            'array',
            '',
            'SOAP-ENC:Array',
            array(),
            array(
                array('ref'=>'SOAP-ENC:arrayType', 'wsdl:arrayType'=>'tns:link_value2[]')
            ),
            'tns:link_value2'
        );
        
        $this->serviceClass->registerType(
            'link_value2',
            'complexType',
            'struct',
            'all',
            '',
            array(
            'link_value'=>array('name'=>'link_value', 'type'=>'tns:link_value'),
            )
        );
        $this->serviceClass->registerType(
            'field_list2',
            'complexType',
            'struct',
            'all',
            '',
            array(
            "field_list"=>array('name'=>'field_list', 'type'=>'tns:field_list'),
            )
        );
        $this->serviceClass->registerType(
            'entry_list2',
            'complexType',
            'struct',
            'all',
            '',
            array(
            "entry_list"=>array('name'=>'entry_list', 'type'=>'tns:entry_list'),
            )
        );
    }
}
