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
 * This class is a base class implementation of REST protocol
 * @api
 */
 #[\AllowDynamicProperties]
 class SugarRest
 {

    /**
     * Constructor
     *
     * @param String $implementation - name of the implementation class
     */
     public function __construct($implementation)
     {
         $this->implementation = $implementation;
     } // fn

     /**
      * It will json encode version of the input object
      *
      * @param array $input - assoc array of input values: key = param name, value = param type
      * @return String - print's $input object
      */
     public function generateResponse($input)
     {
         print_r($input);
     } // fn

     /**
      * This method calls functions on the implementation class and returns the output or Fault object in case of error to client
      *
      * @return unknown
      */
     public function serve()
     {
         if (empty($_REQUEST['method']) || !method_exists($this->implementation, $_REQUEST['method'])) {
             if (empty($_REQUEST['method'])) {
                 echo '<pre>';
                 $reflect = new ReflectionClass(get_class($this->implementation));
                 $restWSDL = (string)$reflect;
                 $restWSDL = preg_replace('/@@.*/', "", $restWSDL);
                 echo $restWSDL;
             } else {
                 $er = new SoapError();
                 $er->set_error('invalid_call');
                 $this->fault($er);
             }
         } else {
             $method = $_REQUEST['method'];
             return  $this->implementation->$method();
         } // else
     } // fn

    /**
     * This function sends response to client containing error object
     *
     * @param SoapError $errorObject - This is an object of type SoapError
     * @access public
     */
     public function fault($errorObject)
     {
         $this->faultServer->generateFaultResponse($errorObject);
     } // fn

     public function generateFaultResponse($errorObject)
     {
         //ob_clean();
         $GLOBALS['log']->info('In SugarRest->fault. Setting fault object on response');
         header('HTTP/1.1 500 Internal Server Error');
         header('Content-Type: text/html; charset="ISO-8859-1"');
         echo '<br>500 Internal Server Error <br>';
         if (is_object($errorObject)) {
             $error = $errorObject->number . ': ' . $errorObject->name . '<br>' . $errorObject->description;
             $GLOBALS['log']->error($error);
             echo  $error;
         } else {
             $GLOBALS['log']->error(var_export($errorObject, true));
             print_r($errorObject);
         } // else
     }
 } // clazz
