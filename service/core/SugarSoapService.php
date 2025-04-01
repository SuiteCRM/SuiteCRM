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
if (!defined('sugarEntry')) {
    define('sugarEntry', true);
}

require_once('service/core/SugarWebService.php');
require_once('service/core/SugarWebServiceImpl.php');

/**
 * This ia an abstract class for the soapservice. All the global fun
 *
 */
#[\AllowDynamicProperties]
abstract class SugarSoapService extends SugarWebService{
	protected $soap_version = '1.1';
    // Sugarcrm namespace is necessary for backwards compatibility with existing SOAP clients
	protected $namespace = 'http://www.sugarcrm.com/sugarcrm';
	protected $implementationClass = 'SugarWebServiceImpl';
	protected $registryClass = "";
	protected $soapURL = "";

  	/**
  	 * This is an abstract method. The implementation method should registers all the functions you want to expose as services.
  	 *
  	 * @param String $function - name of the function
  	 * @param Array $input - assoc array of input values: key = param name, value = param type
  	 * @param Array $output - assoc array of output values: key = param name, value = param type
	 * @access public
  	 */
	abstract public function registerFunction($function, $input, $output);

	/**
	 * This is an abstract method. This implementation method should register all the complex type	 
	 * 
	 * @param String $name - name of complex type
	 * @param String $typeClass - (complexType|simpleType|attribute)
	 * @param String $phpType - array or struct
	 * @param String $compositor - (all|sequence|choice)
	 * @param String $restrictionBase - SOAP-ENC:Array or empty
	 * @param Array $elements - array ( name => array(name=>'',type=>'') )
	 * @param Array $attrs - array(array('ref'=>'SOAP-ENC:arrayType','wsdl:arrayType'=>'xsd:string[]'))
	 * @param String $arrayType - arrayType: namespace:name (xsd:string)
	 * @access public
	 */	
	abstract public function registerType($name, $typeClass, $phpType, $compositor, $restrictionBase, $elements, $attrs=array(), $arrayType='');

	/**
	 * Constructor
	 *
	 */
	protected function __construct(){
		$this->setObservers();
	}

	/**
	 * This method sets the soap server object on all the observers
	 * @access public
	 */
	public function setObservers() {
		global $observers;
		if(!empty($observers)){
			foreach($observers as $observer) {
	   			if(method_exists($observer, 'set_soap_server')) {
	   	 			 $observer->set_soap_server($this->server);
	   			}
			}
		}
	} // fn

	/**
	 * This method returns the soapURL
	 *
	 * @return String - soapURL
	 * @access public
	 */
	public function getSoapURL(){
		return $this->soapURL;
	}

	public function getSoapVersion(){
		return $this->soap_version;
	}

	/**
	 * This method returns the namespace
	 *
	 * @return String - namespace
	 * @access public
	 */
	public function getNameSpace(){
		return $this->namespace;
	}

	/**
	 * This mehtod returns registered implementation class
	 *
	 * @return String - implementationClass
	 * @access public
	 */
	public function getRegisteredImplClass() {
		return $this->implementationClass;	
	}

	/**
	 * This mehtod returns registry class
	 *
	 * @return String - registryClass
	 * @access public
	 */
	public function getRegisteredClass() {
		return $this->registryClass;	
	}

	/**
	 * This mehtod returns server
	 *
	 * @return String -server
	 * @access public
	 */
	public function getServer() {
		return $this->server;	
	} // fn


} // class
