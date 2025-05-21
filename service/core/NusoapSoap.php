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

require('service/core/SugarSoapService.php');
require('include/nusoap/nusoap.php');

/**
 * This is an abstract class for the soap implementation for using NUSOAP. This class is responsible for making
 * all NUSOAP call by passing the client's request to NUSOAP server and seding response back to client
 * @api
 */
#[\AllowDynamicProperties]
abstract class NusoapSoap extends SugarSoapService{
	/**
	 * This is the constructor. It creates an instance of NUSOAP server.
	 *
	 * @param String $url - This is the soap URL
	 * @access public
	 */
	public function __construct($url){
		$GLOBALS['log']->info('Begin: NusoapSoap->__construct');
		$this->server = new soap_server();
		$this->soapURL = $url;
		$this->server->configureWSDL('sugarsoap', $this->getNameSpace(), $url);
		if(!isset($GLOBALS['HTTP_RAW_POST_DATA']))$GLOBALS['HTTP_RAW_POST_DATA'] = file_get_contents('php://input');
		parent::__construct();
		$GLOBALS['log']->info('End: NusoapSoap->__construct');
	} // ctor

	/**
	 * Fallback function to catch unexpected failure in SOAP
	 */
	public function shutdown()
	{
		if($this->in_service) {
			$out = ob_get_contents();
			ob_end_clean();
			$GLOBALS['log']->info('NusoapSoap->shutdown: service died unexpectedly');
			$this->server->fault(-1, "Unknown error in SOAP call: service died unexpectedly", '', $out);
			$this->server->send_response();
		}
	}

	/**
	 * It passes request data to NUSOAP server and sends response back to client
	 * @access public
	 */
	public function serve(){
		$GLOBALS['log']->info('Begin: NusoapSoap->serve');
		ob_clean();
		$this->in_service = true;
		register_shutdown_function(array($this, "shutdown"));
		ob_start();
		$this->server->service($GLOBALS['HTTP_RAW_POST_DATA']);
		$this->in_service = false;
		ob_end_flush();
		flush();
		$GLOBALS['log']->info('End: NusoapSoap->serve');
	} // fn

	/**
	 * This method registers all the complex type with NUSOAP server so that proper WSDL can be generated
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
	public function registerType($name, $typeClass, $phpType, $compositor, $restrictionBase, $elements, $attrs=array(), $arrayType=''){
		$this->server->wsdl->addComplexType($name, $typeClass, $phpType, $compositor, $restrictionBase, $elements, $attrs, $arrayType);
  	} // fn

  	/**
  	 * This method registers all the functions you want to expose as services with NUSOAP
  	 *
  	 * @param String $function - name of the function
  	 * @param Array $input - assoc array of input values: key = param name, value = param type
  	 * @param Array $output - assoc array of output values: key = param name, value = param type
	 * @access public
  	 */
	public function registerFunction($function, $input, $output){
		if(in_array($function, $this->excludeFunctions))return;
		$use = false;
		$style = false;
		if (isset($_REQUEST['use']) && ($_REQUEST['use'] == 'literal')) {
			$use = "literal";
		} // if
		if (isset($_REQUEST['style']) && ($_REQUEST['style'] == 'document')) {
			$style = "document";
		} // if
		$this->server->register($function, $input, $output, $this->getNameSpace(), '',$style, $use);
	} // fn

	/**
	 * This function registers implementation class name with NUSOAP so when NUSOAP makes a call to a funciton,
	 * it will be made on this class object
	 *
	 * @param String $implementationClass
	 * @access public
	 */
	public function registerImplClass($implementationClass){
		$GLOBALS['log']->info('Begin: NusoapSoap->registerImplClass');
		if (empty($implementationClass)) {
			$implementationClass = $this->implementationClass;
		} // if
		$this->server->register_class($implementationClass);
		$GLOBALS['log']->info('End: NusoapSoap->registerImplClass');
	} // fn

	/**
	 * Sets the name of the registry class
	 *
	 * @param String $registryClass
	 * @access public
	 */
	public function registerClass($registryClass){
		$GLOBALS['log']->info('Begin: NusoapSoap->registerClass');
		$this->registryClass = $registryClass;
		$GLOBALS['log']->info('End: NusoapSoap->registerClass');
	} // fn

	/**
	 * This function sets the fault object on the NUSOAP
	 *
	 * @param SoapError $errorObject - This is an object of type SoapError
	 * @access public
	 */
	public function error($errorObject){
		$GLOBALS['log']->info('Begin: NusoapSoap->error');
		$this->server->fault($errorObject->getFaultCode(), $errorObject->getName(), '', $errorObject->getDescription());
		$GLOBALS['log']->info('Begin: NusoapSoap->error');
	} // fn

} // clazz
