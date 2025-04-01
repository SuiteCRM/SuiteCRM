<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
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


require_once 'modules/Calendar/Calendar.php';
require_once 'modules/iCals/iCal.php';
require_once 'include/HTTP_WebDAV_Server/Server.php';


/**
 * Calendar access using WebDAV
 *
 * @access public
 */
#[\AllowDynamicProperties]
class HTTP_WebDAV_Server_iCal extends HTTP_WebDAV_Server
{
    public $cal_encoding = "";
    public $cal_charset = "";
    public $http_spec = "";

    /**
     * Constructor for the WebDAV srver
     */
    public function __construct()
    {
        $this->vcal_focus = new iCal();
        $this->user_focus = BeanFactory::newBean('Users');
    }

    /**
     * Serve a webdav request
     *
     * @access public
     * @param  string
     */
    public function ServeICalRequest($base = false)
    {
        global $sugar_config;
        global $current_language;
        global $log;

        if (empty($_REQUEST['type'])) {
            $_REQUEST['type'] = 'ics';
        }

        if (empty($_REQUEST['encoding'])) {
            $this->cal_encoding = 'utf-8';
        } else {
            $this->cal_encoding = $_REQUEST['encoding'];
        }

        if (empty($_REQUEST['cal_charset'])) {
            $this->cal_charset = 'utf-8';
        } else {
            $this->cal_charset = $_REQUEST['cal_charset'];
        }

        if (empty($_REQUEST['http_spec'])) {
            $this->http_spec = '1.1';
        } else {
            $this->http_spec = $_REQUEST['http_spec'];
        }

        // check the HTTP auth headers for a user
        if (empty($_REQUEST['user_name']) && !empty($_SERVER['PHP_AUTH_USER'])) {
            $_REQUEST['user_name'] = $_SERVER['PHP_AUTH_USER'];
            $_REQUEST['password'] = $_SERVER['PHP_AUTH_PW'];
        }

        if (!empty($sugar_config['session_dir'])) {
            session_save_path($sugar_config['session_dir']);
        }

        session_start();

        $query_arr = array();
        // set path
        if (empty($_SERVER["PATH_INFO"])) {
            $this->path = "/";
            if (strtolower($_SERVER["REQUEST_METHOD"]) == 'get') {
                $query_arr = $_REQUEST;
            } else {
                parse_str($_REQUEST['parms'], $query_arr);
            }
        } else {
            $this->path = $this->_urldecode($_SERVER["PATH_INFO"]);

            $query_str = preg_replace('/^\//', '', (string) $this->path);
            $query_arr = array();
            parse_str($query_str, $query_arr);
        }


        if (!empty($query_arr['type'])) {
            $this->vcal_type = $query_arr['type'];
        } else {
            $this->vcal_type = 'vfb';
        }

        if (!empty($query_arr['source'])) {
            $this->source = $query_arr['source'];
        } else {
            $this->source = 'outlook';
        }

        if (!empty($query_arr['key'])) {
            $this->publish_key = $query_arr['key'];
        }


        // select user by email
        if (!empty($query_arr['user_id'])) {
            $this->user_focus->retrieve(clean_string($query_arr['user_id']));
            $this->user_focus->loadPreferences();
        } else {
            if (isset($query_arr['password'])) {
                $user = $this->user_focus;
                $user->retrieve_by_string_fields(['user_name' => $query_arr['user_name']]);
                if ($user->id === null
                    && !$user::findUserPassword($user->user_name, md5($query_arr['password']))
                ) {
                    $this->http_status("401 not authorized");
                    echo 'Invalid username or password';
                    return;
                }

                $this->user_focus->authenticated = true;
            } elseif (!empty($query_arr['email'])) {
                // clean the string!
                $query_arr['email'] = clean_string($query_arr['email']);
                //get user info
                $this->user_focus->retrieve_by_email_address($query_arr['email']);
            } else {
                if (!empty($query_arr['user_name'])) {
                    // clean the string!
                    $query_arr['user_name'] = clean_string($query_arr['user_name']);

                    //get user info
                    $arr = array('user_name' => $query_arr['user_name']);
                    $this->user_focus->retrieve_by_string_fields($arr);
                } else {
                    $errorMessage = 'iCal Server - Invalid request.';
                    $log->warning($errorMessage);
                    print $errorMessage;
                }
            }
        }

        parent::ServeRequest();
    }


    public function GET()
    {
        return true;
    }

    /**
     * GET method handler
     *
     * @param void
     * @returns void
     */
    public function http_GET()
    {
        if ($this->vcal_type == 'vfb') {
            $this->http_status("200 OK");
            ob_end_clean();
            echo $this->vcal_focus->get_vcal_freebusy($this->user_focus);
        } else {
            if ($this->vcal_type == 'ics') {
                // DO HTTP AUTHORIZATION for iCal:
                if (isset($this->publish_key)
                    && $this->publish_key === $this->user_focus->getPreference('calendar_publish_key')
                    || $this->user_focus->is_authenticated()
                ) {
                    $this->http_status("200 OK");
                    header('Content-Type: text/calendar; charset="' . $this->cal_charset . '"');
                    $result = mb_convert_encoding(html_entity_decode((string) $this->vcal_focus->getVcalIcal(
                        $this->user_focus,
                        $_REQUEST['num_months']
                    ), ENT_QUOTES, $this->cal_charset), $this->cal_encoding);
                    ob_end_clean();
                    echo $result;

                    return;
                }

                $this->http_status("401 not authorized");
                header('WWW-Authenticate: Basic realm="SugarCRM iCal"');
                echo 'Authorization required';
            } else {
                $this->http_status("404 Not Found");
                ob_end_clean();
            }
        }
    }

    /**
     * set HTTP return status and mirror it in a private header
     *
     * @param  string  status code and message
     * @return void
     */
    public function http_status($status)
    {
        // simplified success case
        if ($status === true) {
            $status = "200 OK";
        }

        // remember status
        $this->_http_status = $status;

        // generate HTTP status response
        header("HTTP/$this->http_spec $status");
        header("X-WebDAV-Status: $status", true);
    }
}
