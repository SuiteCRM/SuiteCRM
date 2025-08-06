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

require_once 'include/HTTP_WebDAV_Server/Server.php';


    /**
     * Filesystem access using WebDAV
     *
     * @access public
     */
    #[\AllowDynamicProperties]
    class HTTP_WebDAV_Server_vCal extends HTTP_WebDAV_Server
    {
        /**
         * Root directory for WebDAV access
         *
         * Defaults to webserver document root (set by ServeRequest)
         *
         * @access private
         * @var    string
         */
        public $base = "";
        public $vcal_focus;
        public $vcal_type = "";
        public $source = "";
        public $publish_key = "";

        public function __construct()
        {
            $this->vcal_focus = BeanFactory::newBean('vCals');
            $this->user_focus = BeanFactory::newBean('Users');
        }

        /**
         * Serve a webdav request
         *
         * @access public
         * @param  string
         */
        public function ServeRequest($base = false)
        {
            global $sugar_config;
            global $current_language;
            global $log;

            if (!empty($sugar_config['session_dir'])) {
                session_save_path($sugar_config['session_dir']);
            }

            session_start();
            $current_language = $sugar_config['default_language'];


            // check authentication
            if (!$this->_check_auth()) {
                $this->http_status('403 Forbidden');
                
                return;
            }

            // set root directory, defaults to webserver document root if not set
            if ($base) {
                $this->base = realpath($base); // TODO throw if not a directory
            } else {
                if (!$this->base) {
                    $this->base = $_SERVER['DOCUMENT_ROOT'];
                }
            }


            $query_arr =  array();
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
                $query_arr =  array();
                parse_str($query_str, $query_arr);
            }


            if (! empty($query_arr['type'])) {
                $this->vcal_type = $query_arr['type'];
            } else {
                $this->vcal_type = 'vfb';
            }

            if (! empty($query_arr['source'])) {
                $this->source = $query_arr['source'];
            } else {
                $this->source = 'outlook';
            }

            if (! empty($query_arr['key'])) {
                $this->publish_key = $query_arr['key'];
            }


            // select user by email
            if (! empty($query_arr['user_id'])) {
                $this->user_focus->retrieve(clean_string($query_arr['user_id']));
                $this->user_focus->loadPreferences();
            } else {
                if (! empty($query_arr['email'])) {
                    // clean the string!
                    $query_arr['email'] = clean_string($query_arr['email']);
                    //get user info
                    $this->user_focus->retrieve_by_email_address($query_arr['email']);
                } else {
                    if (! empty($query_arr['user_name'])) {
                        // clean the string!
                        $query_arr['user_name'] = clean_string($query_arr['user_name']);

                        //get user info
                        $arr = array('user_name' => $query_arr['user_name']);
                        $this->user_focus->retrieve_by_string_fields($arr);
                    } else {
                        $errorMessage = 'vCal Server - Invalid request.';
                        $log->warning($errorMessage);
                        print $errorMessage;
                    }
                }
            }

            /**
             * @var User|SugarBean|null $current_user
             */
            $current_user = BeanFactory::getBean('Users', $_SESSION['authenticated_user_id']);



            $userName = $query_arr['user_name'] ?? '';

            /**
             * Fake a response so that it is not different from when a user is found
             */
            if ($this->user_focus->id === null) {
                $this->user_focus->last_name = $userName;
            } elseif (
                !$current_user->isAdmin() &&
                $current_user->user_name !== $this->user_focus->user_name
            ) {
                $this->user_focus->last_name = $userName;
            }

            parent::ServeRequest();
        }

        /**
         * No authentication is needed here
         *
         * @access private
         * @param  string $type HTTP Authentication type (Basic, Digest, ...)
         * @param  string $user Username
         * @param  string $password Password
         * @return bool    true on successful authentication
         */
        public function check_auth($type, $user, $password)
        {
            if (isset($_SESSION['authenticated_user_id'])) {
                // allow logged in users access to freebusy info
                return true;
            }

            if (!empty($this->publish_key) && !empty($this->user_focus) && $this->user_focus->getPreference('calendar_publish_key') == $this->publish_key) {
                return true;
            }

            if ($type === 'basic' || $type === null) {
                $authController = new AuthenticationController();
                return $authController->authController->loginAuthenticate($user, $password);
            }

            return false;
        }


        public function GET()
        {
            return true;
        }

        // {{{ http_GET()

        /**
        * GET method handler
        *
        * @param void
        * @returns void
        */
        public function http_GET()
        {
            global $log;

            if ($this->vcal_type == 'vfb') {
                $this->http_status("200 OK");
                echo $this->vcal_focus->get_vcal_freebusy($this->user_focus);
            } else {
                $errorMessage = 'vCal Server - Invalid request.';
                $log->warning($errorMessage);
                print $errorMessage;
            }
        }
        // }}}


        // {{{ http_PUT()

        /**
        * PUT method handler
        *
        * @param  void
        * @return void
        */
        public function http_PUT()
        {
            $focus = null;
            $options = array();
            $options["path"] = $this->path;
            $options["content_length"] = $_SERVER["CONTENT_LENGTH"];

            // get the Content-type
            if (isset($_SERVER["CONTENT_TYPE"])) {
                // for now we do not support any sort of multipart requests
                if (!strncmp($_SERVER["CONTENT_TYPE"], "multipart/", 10)) {
                    $this->http_status("501 not implemented");
                    echo "The service does not support mulipart PUT requests";
                    return;
                }
                $options["content_type"] = $_SERVER["CONTENT_TYPE"];
            } else {
                // default content type if none given
                $options["content_type"] = "application/octet-stream";
            }

            /* RFC 2616 2.6 says: "The recipient of the entity MUST NOT
               ignore any Content-* (e.g. Content-Range) headers that it
               does not understand or implement and MUST return a 501
               (Not Implemented) response in such cases."
            */
            foreach ($_SERVER as $key => $val) {
                if (strncmp($key, "HTTP_CONTENT", 11)) {
                    continue;
                }
                switch ($key) {
                case 'HTTP_CONTENT_ENCODING': // RFC 2616 14.11
                    // TODO support this if ext/zlib filters are available
                    $this->http_status("501 not implemented");
                    echo "The service does not support '$val' content encoding";
                    return;

                case 'HTTP_CONTENT_LANGUAGE': // RFC 2616 14.12
                    // we assume it is not critical if this one is ignored
                    // in the actual PUT implementation ...
                    $options["content_language"] = $val;
                    break;

                case 'HTTP_CONTENT_LOCATION': // RFC 2616 14.14
                    /* The meaning of the Content-Location header in PUT
                       or POST requests is undefined; servers are free
                       to ignore it in those cases. */
                    break;

                case 'HTTP_CONTENT_RANGE':    // RFC 2616 14.16
                    // single byte range requests are NOT supported
                    // the header format is also specified in RFC 2616 14.16
                    // TODO we have to ensure that implementations support this or send 501 instead
                        $this->http_status("400 bad request");
                        echo "The service does only support single byte ranges";
                        return;

                case 'HTTP_CONTENT_MD5':      // RFC 2616 14.15
                    // TODO: maybe we can just pretend here?
                    $this->http_status("501 not implemented");
                    echo "The service does not support content MD5 checksum verification";
                    return;

                case 'HTTP_CONTENT_LENGTH': // RFC 2616 14.14
                    /* The meaning of the Content-Location header in PUT
                       or POST requests is undefined; servers are free
                       to ignore it in those cases. */
                    break;

                default:
                    // any other unknown Content-* headers
                    $this->http_status("501 not implemented");
                    echo "The service does not support '$key'";
                    return;
                }
            }

            // DO AUTHORIZATION for publishing Free/busy to Sugar:
            if (empty($this->publish_key) ||
                $this->publish_key != $this->user_focus->getPreference('calendar_publish_key')) {
                $this->http_status("401 not authorized");
                return;
            }

            // retrieve
            $arr = array('user_id'=>$this->user_focus->id,'type'=>'vfb','source'=>$this->source);
            $this->vcal_focus->retrieve_by_string_fields($arr);

            $isUpdate  = false;

            if (! empty($this->vcal_focus->user_id) &&
                $this->vcal_focus->user_id != -1) {
                $isUpdate  = true;
            }

            // open input stream
            $options["stream"] = fopen("php://input", 'rb');
            $content = '';

            // read in input stream
            while (!feof($options["stream"])) {
                $content .= fread($options["stream"], 4096);
            }

            // set freebusy members and save
            $this->vcal_focus->content = $content;
            $this->vcal_focus->type = 'vfb';
            $this->vcal_focus->source = $this->source;
            $focus->date_modified = null;
            $this->vcal_focus->user_id = $this->user_focus->id;
            $this->vcal_focus->save();

            if ($isUpdate) {
                $this->http_status("204 No Content");
            } else {
                $this->http_status("201 Created");
            }
        }

        /**
         * PUT method handler
         *
         * @param  array  parameter passing array
         * @return bool   true on success
         */
        public function PUT(&$options)
        {
        }

        /**
         * LOCK method handler
         *
         * @param  array  general parameter passing array
         * @return bool   true on success
         */
        public function lock(&$options)
        {
            $options["timeout"] = time()+300; // 5min. hardcoded
            return true;
        }

        /**
         * UNLOCK method handler
         *
         * @param  array  general parameter passing array
         * @return bool   true on success
         */
        public function unlock(&$options)
        {
            return "200 OK";
        }


        /**
         * checkLock() helper
         *
         * @param  string resource path to check for locks
         * @return bool   true on success
         */
        public function checkLock($path)
        {
            return false;
        }
    }
