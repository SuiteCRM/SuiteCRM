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


require_once('service/core/SugarWebService.php');
require_once('service/core/SugarRestServiceImpl.php');

/**
 * Base class for rest services
 *
 */
#[\AllowDynamicProperties]
class SugarRestService extends SugarWebService{
	protected $implementationClass = 'SugarRestServiceImpl';
	protected $restURL = "";
	protected $registeredFunc = array();

	/**
	 * Get Sugar REST class name for input/return type
	 *
	 * @param string $name
	 * @return string
	 */
	protected function _getTypeName($name)
	{
		if(empty($name)) return 'SugarRest';

		$name = clean_string($name, 'ALPHANUM');
		$type = '';
		switch(strtolower($name)) {
			case 'json':
				$type = 'JSON';
				break;
			case 'rss':
				$type = 'RSS';
				break;
			case 'serialize':
				$type = 'Serialize';
				break;
		}
		$classname = "SugarRest$type";
		if(!file_exists('service/core/REST/' . $classname . '.php')) {
			return 'SugarRest';
		}
		return $classname;
	}

	/**
	 * Constructor.
	 *
	 * @param String $url - REST url
	 */
	public function __construct($url){
		$GLOBALS['log']->info('Begin: SugarRestService->__construct');
		$this->restURL = $url;

		$this->responseClass = $this->_getTypeName(@$_REQUEST['response_type']);
		$this->serverClass = $this->_getTypeName(@$_REQUEST['input_type']);
		$GLOBALS['log']->info('SugarRestService->__construct serverclass = ' . $this->serverClass);
		require_once('service/core/REST/'. $this->serverClass . '.php');
		$GLOBALS['log']->info('End: SugarRestService->__construct');
	} // ctor

	/**
	 * Its a no op method
	 *
	 * @access public
	 */
	public function registerType($name, $typeClass, $phpType, $compositor, $restrictionBase, $elements, $attrs, $arrayType){
  	} // fn

  	/**
  	 * This method registers all the functions you want to expose as services with REST
  	 *
  	 * @param String $function - name of the function
  	 * @param Array $input - assoc array of input values: key = param name, value = param type
  	 * @param Array $output - assoc array of output values: key = param name, value = param type
	 * @access public
  	 */
	public function registerFunction($function, $input, $output){
		if(in_array($function, $this->excludeFunctions))return;
		$this->registeredFunc[$function] = array('input'=> $input, 'output'=>$output);
	} // fn

	/**
	 * It passes request data to REST server and sends response back to client
	 * @access public
	 */
	public function serve(){
		$GLOBALS['log']->info('Begin: SugarRestService->serve');
		require_once('service/core/REST/'. $this->responseClass . '.php');
		$response  = $this->responseClass;

		$responseServer = new $response($this->implementation);
		$this->server->faultServer = $responseServer;
		$responseServer->faultServer = $responseServer;
		$responseServer->generateResponse($this->server->serve());
		$GLOBALS['log']->info('End: SugarRestService->serve');
	} // fn

	/**
	 * Enter description here...
	 *
	 * @param Array $excludeFunctions - All the functions you don't want to register
	 */
	public function register($excludeFunctions = array()){

	} // fn

	/**
	 * This mehtod returns registered implementation class
	 *
	 * @return String - implementationClass
	 * @access public
	 */
	public function getRegisteredImplClass() {
		return $this->implementationClass;
	} // fn

	/**
	 * This mehtod returns registry class
	 *
	 * @return String - registryClass
	 * @access public
	 */
	public function getRegisteredClass() {
		return $this->registryClass;
	} // fn

	/**
	 * Sets the name of the registry class
	 *
	 * @param String $registryClass
	 * @access public
	 */
	public function registerClass($registryClass){
		$this->registryClass = $registryClass;
	}

	/**
	 * This function registers implementation class name and creates an instance of rest implementation class
	 * it will be made on this class object
	 *
	 * @param String $implementationClass
	 * @access public
	 */
	public function registerImplClass($className){
		$GLOBALS['log']->info('Begin: SugarRestService->registerImplClass');
		$this->implementationClass = $className;
		$this->implementation = new $this->implementationClass();
		$this->server = new $this->serverClass($this->implementation);
		$this->server->registerd = $this->registeredFunc;
		$GLOBALS['log']->info('End: SugarRestService->registerImplClass');
	} // fn

	/**
	 * This function sets the fault object on the REST
	 *
	 * @param SoapError $errorObject - This is an object of type SoapError
	 * @access public
	 */
	public function error($errorObject){
		$GLOBALS['log']->info('Begin: SugarRestService->error');
		$this->server->fault($errorObject);
		$GLOBALS['log']->info('End: SugarRestService->error');
	} // fn

	/**
	 * This mehtod returns server
	 *
	 * @return String - server
	 * @access public
	 */
	public function getServer(){
		return $this->server;
	} // fn


}
