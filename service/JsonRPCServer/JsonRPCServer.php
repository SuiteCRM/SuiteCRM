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

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}


require_once __DIR__ . '/../../soap/SoapHelperFunctions.php';
require_once __DIR__ . '/../../include/json_config.php';
require_once __DIR__ . '/../../include/utils.php';
require_once __DIR__ . '/JsonRPCServerUtils.php';
require_once __DIR__ . '/JsonRPCServerCalls.php';

#[\AllowDynamicProperties]
class JsonRPCServer
{
    /**
     * @var JSON $jsonParser
     */
    private $jsonParser;

    /**
     * @var JsonRPCServerUtils $jsonServerUtils
     */
    private $jsonServerUtils;

    /**
     * @var JsonRPCServerCalls
     */
    private $jsonServerCalls;

    /**
     * JsonRPCServer constructor.
     */
    public function __construct()
    {
        $this->jsonParser = getJSONobj();
        $this->jsonServerUtils = new JsonRPCServerUtils();
        $this->jsonServerCalls = new JsonRPCServerCalls();
    }

    /**
     * Run the JSON RPC Server
     */
    public function run()
    {
        global $sugar_config;
        global $log;

        $response = array();
        $jsonParser = $this->jsonParser;

        $log->debug('JSON_SERVER:');

        ob_start();
        insert_charset_header();

        if (!empty($sugar_config['session_dir'])) {
            session_save_path($sugar_config['session_dir']);
            $log->debug('JSON_SERVER:session_save_path:' . $sugar_config['session_dir']);
        }

        if(session_status() === PHP_SESSION_NONE){
            session_start();
            $log->debug('JSON_SERVER:session started');
        }

        $current_language = get_current_language();

        $log->debug('JSON_SERVER: current_language:' . $current_language);

        if (strtolower($_SERVER['REQUEST_METHOD']) === 'get') {
            $response['error'] = array('error_msg' => 'DEPRECATED API');
            $log->deprecated('JsonServer: Get Request Method is deprecated');
        } else {
            $response = $this->processRequest();
        }

        print $jsonParser::encode($response, true);
        ob_end_flush();
        sugar_cleanup();
    }

    /**
     * @return array json response
     */
    private function processRequest()
    {
        $response = [];
        $response['result'] = null;
        $response['id'] = '-1';
        $jsonParser = $this->jsonParser;
        $current_user = $this->jsonServerUtils->authenticate();

        if ($current_user === null) {
            $response['error'] = array('error_msg' => 'user not logged in');

            return $response;
        }

        // extract request
        if (isset($GLOBALS['HTTP_RAW_POST_DATA'])) {
            $request = $jsonParser::decode($GLOBALS['HTTP_RAW_POST_DATA'], true);
        } else {
            $request = $jsonParser::decode(file_get_contents('php://input'), true);
        }

        if (!is_array($request)) {
            $response['error'] = array('error_msg' => 'malformed request');

            return $response;
        }

        // make sure required RPC fields are set
        if (empty($request['method']) || empty($request['id'])) {
            $response['error'] = array('error_msg' => 'missing parameters');

            return $response;
        }

        $response['id'] = $request['id'];

        if (method_exists($this->jsonServerCalls, $request['method'])) {
            $response = call_user_func(array($this->jsonServerCalls, $request['method']), $request['id'],
                $request['params']);
            if (!empty($response)) {
                return $response;
            }
        }

        $response['error'] = array('error_msg' => 'method:' . $request['method'] . ' not supported');

        return $response;

    }
}
