<?php
/**
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation.
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */
require_once __DIR__ . '/WebFormDataBO.php';

/**
 * Class to manage the data entries from the web forms created.
 * It works as a Controller and needs the extension by each particular form for the definition of the abstract manage method
 */
class WebFormDataController
{
    // Response constants
    public const RESPONSE_STATUS_OK = 0; // Status finished OK
    public const RESPONSE_STATUS_PENDING = 1; // Pending status
    public const RESPONSE_STATUS_ERROR = 2; // Status terminated with error

    public const RESPONSE_TYPE_TXT = 0; // Text type response
    public const RESPONSE_TYPE_REDIRECTION = 1; // Redirect Response
    public const RESPONSE_TYPE_TEMPLATE = 2; // Template based response

    // Specific controller
    protected $controller = null;

    // Default error message
    protected $feedBackMsg = '';

    // Management Logic Object
    protected $bo = null;

    // Store the last error generated
    private $lastError = '';

    // Allows data exchange between controllers
    private $responseData = null;

    /**
     * Properties to manage the language of the messages
     */
    protected $lang = 'en_US'; // Language for user responses
    protected $mod_strings = null; // Module labels
    protected $app_strings = null; // Application labels
    protected $defaultModule = 'stic_Web_Forms'; // Default module to search for labels

    // Class constructor
    public function __construct()
    {
        $this->loadLanguage();
        $this->bo = new WebFormDataBO();
    }

    /**
     * Returns the last error
     */
    public function getLastError()
    {
        return $this->lastError;
    }

    public function returnCode($code = '')
    {
        return $this->lastError = $code;
    }

    /**
     * Detect the browser language and load the corresponding tags.
     */
    private function loadLanguage()
    {
        if (!empty($_REQUEST['language'])) {
            $this->lang = $_REQUEST['language'];
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Indicating language [{$this->lang}] from form.");
        } else {
            $_SERVER["HTTP_ACCEPT_LANGUAGE"] =  $_SERVER["HTTP_ACCEPT_LANGUAGE"] ?? '';
            $http_lang = substr($_SERVER["HTTP_ACCEPT_LANGUAGE"], 0, 2);
            switch ($http_lang) {
                case 'es':
                    $this->lang = 'es_ES';
                    break;
                case 'ca':
                    $this->lang = 'ca_ES';
                    break;
                case 'en':
                default:
                    $this->lang = 'en_us';
                    break;
            }
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  indicating language [{$_SERVER["HTTP_ACCEPT_LANGUAGE"]} -> {$http_lang} -> {$this->lang}] from browser parameter.");
        }

        $this->app_strings = return_application_language($this->lang); // Load application labels
        $this->mod_strings[$this->defaultModule] = return_module_language($this->lang, $this->defaultModule, false); // Load the module labels by default
    }

    /**
     * Returns the detected Lang
     */
    public function getLanguage()
    {
        return $this->lang;
    }

    /**
     * Retrieve the basic parameters of the form and populate with it the array fixedDefParams
     */
    private function getActionDefParams()
    {
        $this->bo->setActionDefParams($this->filterFields($this->bo->getActionDefFields()));
    }

    /**
     * Retrieve the form parameters and populate the formParams array
     */
    protected function getParams()
    {
        $this->bo->setFormParams($this->filterFields($this->bo->getFormFields()));
    }

    /**
     * Retrieve the form definition parameters and populate the defParams array
     */
    protected function getDefParams()
    {
        $defParams = $this->filterFields($this->bo->getDefFields());
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  DefParams: " . print_r($defParams, true));

        // If it has parameters encoded in JSON (field defParams) it decodes it
        if (!empty($defParams['defParams'])) {
            $jsonDefParams = json_decode(urldecode($defParams['defParams']), true);
            $defParams['decodedDefParams'] = $jsonDefParams;
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  DecodedDefParams: " . print_r($defParams, true));
        }

        $this->bo->setDefParams($defParams);
    }

    /**
     * Assign values to parameters from outside the Request
     */
    public function setNoRequestParams($params)
    {
        foreach ($params as $key => $param) {
            $this->bo->$key = $param;
        }
    }

    /**
     * Allows the controller to assign response data (apart from the Response)
     * Through these functions the exchange of data in delegated responses is facilitated
     */
    public function setResponseData($data)
    {
        $this->responseData = $data;
    }

    public function getResponseData()
    {
        return $this->responseData;
    }

    public function getObjectsCreated() {
        $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ' If this placeholder function is called, something web wrong ');
        return array();
    }


    /**
     *  Load the specific driver, if there is no specific driver, the class itself will be used
     */
    private function getSpecificController()
    {
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  : Loading specific driver...");
        $actionDefParams = $this->bo->getActionDefParams();

        if (!empty($actionDefParams['webFormClass'])) {
            $webFormClass = $actionDefParams['webFormClass'];
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Loading controller [{$webFormClass}]...");
            $controllerName = "{$webFormClass}Controller";
            require_once __DIR__ . "/$webFormClass/{$controllerName}.php";
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Instantiating [{$controllerName}]...");
            $this->controller = new $controllerName();
        } else {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  There is no specific driver, the default driver is loaded.");
            $this->controller = $this;
        }
        return $this->controller;
    }

    /**
     *  Generate error feedback with data from the last controller
     */
    public function feedBackError($controller)
    {
        $lastError = $controller->getLastError();
        
        // Errors that come from stic_Web_Forms_save entrypoint might be related with
        // errors in form creation or customisation, so an email notification should be sent
        // to form manager. Errors related to other entrypoints like stic_Web_Forms_tpv_response
        // can be ignored for notification purposes.
        
        if (isset($_REQUEST['stic_send_feedBackErrors']) && $_REQUEST['stic_send_feedBackErrors'] == 1) {
        // if ($_REQUEST['entryPoint'] == 'stic_Web_Forms_save') {
            
            $msg = array();
            $msg['subject'] = $this->getMsgString('LBL_' . $lastError);
            $msg['received_data_title'] = $this->getMsgString('LBL_PARAM_ERROR_EMAIL_RECEIVED_DATA_TITLE');
            $msg['form_params_title'] = $this->getMsgString('LBL_PARAM_ERROR_EMAIL_FORM_PARAMS_TITLE');
            $msg['information'] = $this->getMsgString('LBL_PARAM_ERROR_EMAIL_INFORMATION');
            $msg['required'] = $this->getMsgString('LBL_PARAM_ERROR_EMAIL_REQUIRED');

            require_once "Include/Mailer/WebFormMailer.php";

            $formData = array();
            $formParams = array();
            $reqIdFields = explode(";", $_REQUEST['req_id']);


            if (isset($_REQUEST['stic_origin_form']) && !empty($_REQUEST['stic_origin_form'])) {
                $formParams['URL'] = $_REQUEST['stic_origin_form'];
            }

            // Received fields
            foreach ($_REQUEST as $key => $value) {
                if (!strpos($key, '___') === false) {
                    if (strpos($_REQUEST['req_id'], (string) $key) === false) {
                        $formData[$key] = "<b>{$value}</b>";
                    } else {
                        $formData[$key . ' <span style="color:red;">(' . $msg['required'] . ')</span>'] = "<b>{$value}</b>";
                    }
                } else {
                    $formParams[$key] = "<b>{$value}</b>";
                }
            }

            // Required fields
            foreach ($reqIdFields as $key => $value) {
                if (!empty(trim($value)) && (empty($_REQUEST[$value]) || !isset($_REQUEST[$value]))) {
                    $formData[$value . ' <span style="color:red;">(' . $msg['required'] . ')</span>'] = '';
                }
            }
            ksort($formData, SORT_LOCALE_STRING);

            $actionDefParams = $controller->bo->getActionDefParams();
            $assignedUserId = $actionDefParams['assigned_user_id'];
            $notificationSent = WebFormMailer::sendErrorNotification($formData, $formParams, $assignedUserId, $msg);
         }

        // Redirect end user to KO URL
        return $this->createResponse(self::RESPONSE_STATUS_ERROR, self::RESPONSE_TYPE_REDIRECTION, $this->bo->getKOURL());
    }

    /**
     * Returns a template object from the file indicated.
     */
    public static function getNewTemplate($template)
    {
        return new XTemplate($template);
    }

    /**
     * Parameter verification functions received
     */
    public function checkActionDefParams()
    {
        return $this->returnCode($this->bo->checkActionDefParams());
    }

    /**
     * Parameter verification functions received
     */
    public function checkDefParams()
    {
        return $this->returnCode($this->bo->checkDefParams());
    }

    /**
     * Parameter verification functions received
     */
    public function checkParams()
    {
        return $this->returnCode($this->bo->checkParams());
    }

    /**
     * Manage the request
     * In case of error it shows a feedback with error.
     * Otherwise redirects to the redirect page or, if it does not exist, generates feedback with ok.
     * @param $delegate Indicates whether the response is executed directly or if the response is returned to the call method and its execution is delegated.
     */
    public function manage($delegate = false)
    {
        // Retrieves the necessary parameters for the controller
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Retrieving mandatory action definition parameters...");
        $this->getActionDefParams();
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Checking mandatory action definition parameters...");

        if ($this->checkActionDefParams()) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  There has been an error in the mandatory action definition parameters.");
            $response = $this->feedBackError($this);
        } else {
            // Load the request controller
            $controller = $this->getSpecificController();

            // Populate specific data with the parameters recovered
            $controller->bo->setActionDefParams($this->bo->getActionDefParams());

            // Retrieves the necessary parameters for the controller
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Retrieving request definition parameters...");
            $controller->getDefParams();
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Checking request definition parameters...");
            if ($controller->checkDefParams()) {
                $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ":  There was an error in the request definition parameters.");
                $response = $this->feedBackError($controller);
            } else {
                // Retrieve the parameters and check that they are correct
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Retrieving request parameters...");
                $controller->getParams();
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Checking request parameters...");
                if ($controller->checkParams()) {
                    $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ":  There was an error in the request parameters");
                    $response = $this->feedBackError($controller);
                } else {
                    // Perform the operation management
                    $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Managing the request..");
                    $response = $controller->doAction();
                    $response['objects'] = $controller->getObjectsCreated();
                }
            }
        }
        // Try the answer
        if ($delegate) {
            // If we have to delegate the execution, return the response
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Delegating type response [" . ($response['type'] ?? null) . "]...");
            return $response;
        } else {
            // If we have to deal with the answer, we deal with it.
            $response['type'] = $response['type'] ?? null;
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Managing type response [{$response['type']}]...");
            $response['data'] = $response['data'] ?? null;
            switch ($response['type']) {
                case self::
                        RESPONSE_TYPE_TXT:echo $response['data'];
                    break;
                case self::
                        RESPONSE_TYPE_REDIRECTION:$this->redirect($response['data']);
                    break;
                case self::
                        RESPONSE_TYPE_TEMPLATE:$response['data']->out('main');
                    break;
            }
        }
    }

    /**
     * Redirect the request to the indicated page
     * @param String $url Indicates the URL where to redirect
     */
    private function redirect($url)
    {
        if (headers_sent()) {
            echo '<html ' . get_language_header() . '><head><title>SinergiaCRM</title></head><body>';
            echo '<form name="redirect" action="' . $url . '" method="GET">';
            echo '</form><script language="javascript" type="text/javascript">document.redirect.submit();</script>';
            echo '</body></html>';
            die();
        } else {
            header("Location: {$url}");
            die();
        }
    }

    /**
     * THE SPECIFIC CONTROLLERS MUST, AT LEAST, OVERLOAD THIS METHOD
     * Execute the necessary operations to manage the operation.
     * Your call will be preceded by a call to getParams and checkParams
     * @return Array array (type => (TXT|REDIRECT), data => (mixed))
     */
    protected function doAction()
    {
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Using the default driver, this should not be happening..");
        $this->returnCode('FORM_ERROR');
        return $this->createResponse(self::RESPONSE_STATUS_ERROR, self::RESPONSE_TYPE_TXT, $this->feedBackError($this));
    }

    /**
     * Returns the text of a label in the user's language
     */
    protected function getMsgString($key, $mod = null)
    {
        // If it receives a module and is not loaded we load it
        if (!empty($mod) && empty($this->mod_strings[$mod])) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Loading module tags {$mod}...");
            $this->mod_strings[$mod] = return_module_language($this->lang, $mod);
        }

        // If we have not passed a module we use the default module
        if (empty($mod)) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Using the default module, {$this->defaultModule}");
            $mod = $this->defaultModule;
        }

        // We first look for the label in the module list, if it is not searched in the application and if it is not returned an empty string
        if (!empty($this->mod_strings[$mod][$key])) {
            return $this->mod_strings[$mod][$key];
        } else if (!empty($this->app_strings[$key])) {
            return $this->app_strings[$key];
        } else {
            return '';
        }
    }

    /**
     * Generate an answer array
     */
    protected function createResponse($status, $type, $data)
    {
        return array('status' => $status, 'type' => $type, 'data' => $data);
    }

    /**
     * Indicates if a url is empty or not
     * We may receive redirection parameters of the "http: //" style, in this case this function will return false
     * If the url contains a hostname, it will be considered valid without validating that the hostname exists
     */
    public static function isNotEmptyUrl($url)
    {
        $ret = parse_url($url);
        if ($ret !== false) {
            return !empty($ret['host']);
        }
    }

    /**
     * Filters the fields indicated in the array passed by parameter
     * Returns an array with the fields that existed in $_REQUEST and with the sanitized values
     * @param Array $fields array Fields to recover and filter
     * @param Array $defaultValues ​​(optional) array with default values
     * @param String $prefix (optional) Indicate the field prefix in the REQUEST
     * @param Boolean erasePrefix (optional, true by default) Indicates if, if there is a prefix, it must be removed by including it in the field array
     */
    protected function filterFields($fields, $defaultValues = null, $prefix = '', $erasePrefix = true)
    {
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  REQUEST -> " . print_r($_REQUEST, true));
        $ret = array();
        $default = !empty($defaultValues) && is_array($defaultValues) ? $defaultValues : array();
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  FIELDS -> " . print_r($fields, true));
        foreach ($fields as $field) {
            $prefixedName = "{$prefix}{$field}";
            // If the value received in $_REQUEST is an array is that it comes from a multiple field, in which case we process it to a valid string for this type of values
            if (is_array($_REQUEST[$prefixedName] ?? false)) {
                $_REQUEST[$prefixedName] = '^' . join('^,^', $_REQUEST[$prefixedName]) . '^';
            }
            $destName = ($erasePrefix ? $field : $prefixedName);

            if (isset($_REQUEST[$prefixedName])) {
                // Avoid use of deprecated constant FILTER_SANITIZE_STRING
                // $ret[$destName] = filter_var($_REQUEST[$prefixedName], FILTER_SANITIZE_STRING);
                $ret[$destName] = strip_tags($_REQUEST[$prefixedName] ?? '');
            } else if (isset($default[$prefixedName])) {
                $ret[$destName] = $default[$prefixedName];
            }
        }
        return $ret;
    }

    /**
     * Return the BO
     * @return WebFormDataBO
     */
    public function getBO()
    {
        return $this->bo;
    }
}
