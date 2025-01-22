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


require_once('include/resource/Observers/ResourceObserver.php');

class SoapResourceObserver extends ResourceObserver
{
    private $soapServer;

    public function __construct($module)
    {
        parent::__construct($module);
    }

    /**
     * set_soap_server
     * This method accepts an instance of the nusoap soap server so that a proper
     * response can be returned when the notify method is triggered.
     * @param $server The instance of the nusoap soap server
     */
    public function set_soap_server(& $server)
    {
        $this->soapServer = $server;
    }


    /**
     * notify
     * Soap implementation to notify the soap clients of a resource management error
     * @param msg String message to possibly display
     */
    public function notify($msg = '')
    {
        header($_SERVER['SERVER_PROTOCOL'] . ' 500 Internal Server Error');
        header('Content-Type: text/xml; charset="ISO-8859-1"');
        $error = new SoapError();
        $error->set_error('resource_management_error');
        //Override the description
        $error->description = $msg;
        $this->soapServer->methodreturn = array('result'=>$msg, 'error'=>$error->get_soap_array());
        $this->soapServer->serialize_return();
        $this->soapServer->send_response();
        sugar_cleanup(true);
    }
}
