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

<?php
require('service/core/SugarSoapService.php');
require('include/nusoap/nusoap.php');

#[\AllowDynamicProperties]
abstract class PHP5Soap extends SugarSoapService{
	private $nusoap_server = null;
	public function __construct($url){
		$this->soapURL = $url;
		ini_set("soap.wsdl_cache_enabled", "0"); // disabling WSDL cache
		global $HTTP_RAW_POST_DATA;
		if(!isset($HTTP_RAW_POST_DATA)) {
			$HTTP_RAW_POST_DATA = file_get_contents('php://input');
		}
		parent::__construct();
	}

	public function setObservers() {

	} // fn

	/**
	 * Serves the Soap Request
	 * @return
	 */
	public function serve(){
        ob_clean();
		global $HTTP_RAW_POST_DATA;
		$GLOBALS['log']->debug("I am here1 ". $HTTP_RAW_POST_DATA);
		$qs = '';
		if (isset($_SERVER['QUERY_STRING'])) {
			$qs = $_SERVER['QUERY_STRING'];
		} elseif (isset($_SERVER['QUERY_STRING'])) {
			$qs = $_SERVER['QUERY_STRING'];
		} else {
			$qs = '';
		}

		if (stristr((string) $qs, 'wsdl') || $HTTP_RAW_POST_DATA == ''){
			$wsdlCacheFile = $this->getWSDLPath(false);
			if (stristr((string) $qs, 'wsdl')) {
			    $contents = @sugar_file_get_contents($wsdlCacheFile);
			    if($contents !== false) {
					header("Content-Type: text/xml; charset=ISO-8859-1\r\n");
					print $contents;
			    } // if
			} else {
				$this->nusoap_server->service($HTTP_RAW_POST_DATA);
			} // else
		} else {
			$this->server->handle();
		}
		ob_end_flush();
		flush();
	}

	private function generateSoapServer() {
		if ($this->server == null) {
			$soap_url = $this->getSoapURL() . "?wsdl";
			$this->server = new SoapServer($this->getWSDLPath(true), array('soap_version'=>SOAP_1_2, 'encoding'=>'ISO-8859-1'));
		}
	} // fn

	private function generateNuSoap() {
		if ($this->nusoap_server == null) {
			$this->nusoap_server = new soap_server();
			$this->nusoap_server->configureWSDL('sugarsoap', $this->getNameSpace(), "");
			$this->nusoap_server->register_class('SugarWebServiceImpl');
		} // if
	} // fn

	public function getWSDLPath($generateWSDL) {
		$wsdlURL = $this->getSoapURL().'?wsdl';
		$wsdlCacheFile = 'upload://wsdlcache-' . md5($wsdlURL);

		if ($generateWSDL) {
			$oldqs = $_SERVER['QUERY_STRING'];
			$_SERVER['QUERY_STRING'] = "wsdl";
			$this->nusoap_server->service($wsdlURL);
			$_SERVER['QUERY_STRING'] = $oldqs;
			file_put_contents($wsdlCacheFile, ob_get_contents());
			return $wsdlCacheFile;
		    //ob_clean();
		} else {
			return $wsdlCacheFile;
		}

	} // fn

	public function getNameSpace() {
		return $this->soapURL;
	} // fn

	/**
	 * This function allows specifying what version of PHP soap to use
	 * PHP soap supports version 1.1 and 1.2.
	 * @return
	 * @param $version String[optional]
	 */
	public function setSoapVersion($version='1.1'){
		//PHP SOAP supports 1.1 and 1.2 only currently
		$this->soap_version = ($version == '1.2')?'1.2':'1.1';
	}

	public function error($errorObject){
		$this->server->fault($errorObject->getFaultCode(), $errorObject->getName(), '', $errorObject->getDescription()); 	}

	public function registerImplClass($implementationClass){
		if (empty($implementationClass)) {
			$implementationClass = $this->implementationClass;
		} // if
		$this->generateSoapServer();
		$this->server->setClass($implementationClass);
		parent::setObservers();
	}

	public function registerClass($registryClass){
		$this->registryClass = $registryClass;
	}

	public function registerType($name, $typeClass, $phpType, $compositor, $restrictionBase, $elements, $attrs=array(), $arrayType=''){
		$this->nusoap_server->wsdl->addComplexType($name, $typeClass, $phpType, $compositor, $restrictionBase, $elements, $attrs, $arrayType);
  	}

	public function registerFunction($function, $input, $output){
		if(in_array($function, $this->excludeFunctions))return;
		if ($this->nusoap_server == null) {
			$this->generateNuSoap();
		} // if
		$this->nusoap_server->register($function, $input, $output, $this->getNameSpace());

	}

}
