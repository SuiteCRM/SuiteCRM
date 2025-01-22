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


require_once('include/externalAPI/Base/ExternalAPIPlugin.php');
require_once('include/externalAPI/Base/ExternalOAuthAPIPlugin.php');
require_once('include/connectors/sources/SourceFactory.php');

/**
 * Base implementation for external API
 * @api
 */
#[\AllowDynamicProperties]
abstract class ExternalAPIBase implements ExternalAPIPlugin
{
    public $account_name;
    public $account_password;
    public $authMethod = 'password';
    public $useAuth = true;
    public $requireAuth = true;

    public const APP_STRING_ERROR_PREFIX = 'ERR_EXTERNAL_API_';
    protected $_appStringErrorPrefix = self::APP_STRING_ERROR_PREFIX;

    /**
     * Authorization data
     * @var EAPM
     */
    protected $authData;

    /**
     * Load authorization data
     * @param EAPM $eapmBean
     * @see ExternalAPIPlugin::loadEAPM()
     */
    public function loadEAPM($eapmBean)
    {
        // FIXME: check if the bean is validated, if not, refuse it?
        $this->eapmBean = $eapmBean;
        if ($this->authMethod == 'password') {
            $this->account_name = $eapmBean->name;
            $this->account_password = $eapmBean->password;
        }
        return true;
    }

    /**
     * Check login
     * @param EAPM $eapmBean
     * @see ExternalAPIPlugin::checkLogin()
     */
    public function checkLogin($eapmBean = null)
    {
        if (!empty($eapmBean)) {
            $this->loadEAPM($eapmBean);
        }

        if (!isset($this->eapmBean)) {
            return array('success' => false);
        }

        return array('success' => true);
    }

    public function quickCheckLogin()
    {
        if (!isset($this->eapmBean)) {
            return array('success' => false, 'errorMessage' => translate('LBL_ERR_NO_AUTHINFO', 'EAPM'));
        }

        if ($this->eapmBean->validated==0) {
            return array('success' => false, 'errorMessage' => translate('LBL_ERR_NO_AUTHINFO', 'EAPM'));
        }

        return array('success' => true);
    }

    protected function getValue($value)
    {
        if (!empty($this->$value)) {
            return $this->$value;
        }
        return null;
    }

    public function logOff()
    {
        // Not sure if we should do anything.
        return true;
    }

    /**
     * Does API support this method?
     * @see ExternalAPIPlugin::supports()
     */
    public function supports($method = '')
    {
        return $method==$this->authMethod;
    }

    protected function postData($url, $postfields, $headers)
    {
        $proxy_settings = [];
        $ch = curl_init($url);
        curl_setopt($ch, CURLOPT_SSL_VERIFYPEER, false);
        curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);

        $proxy_config = SugarModule::get('Administration')->loadBean();
        $proxy_config->retrieveSettings('proxy');

        if (!empty($proxy_config) &&
            !empty($proxy_config->settings['proxy_on']) &&
            $proxy_config->settings['proxy_on'] == 1) {
            curl_setopt($ch, CURLOPT_PROXY, $proxy_config->settings['proxy_host']);
            curl_setopt($ch, CURLOPT_PROXYPORT, $proxy_config->settings['proxy_port']);
            if (!empty($proxy_settings['proxy_auth'])) {
                curl_setopt($ch, CURLOPT_PROXYUSERPWD, $proxy_settings['proxy_username'] . ':' . $proxy_settings['proxy_password']);
            }
        }

        if ((is_array($postfields) && count($postfields) == 0) ||
             empty($postfields)) {
            curl_setopt($ch, CURLOPT_POST, false);
        } else {
            curl_setopt($ch, CURLOPT_POST, true);
            curl_setopt($ch, CURLOPT_POSTFIELDS, $postfields);
        }
        curl_setopt($ch, CURLOPT_HTTPHEADER, $headers);
        curl_setopt($ch, CURLOPT_FOLLOWLOCATION, false);

        $GLOBALS['log']->debug("ExternalAPIBase->postData Where: ".$url);
        $GLOBALS['log']->debug("Headers:\n".print_r($headers, true));
        // $GLOBALS['log']->debug("Postfields:\n".print_r($postfields,true));
        $rawResponse = curl_exec($ch);
        $GLOBALS['log']->debug("Got:\n".print_r($rawResponse, true));

        return $rawResponse;
    }

    /**
     * Get connector for this API
     * @return source|null
     */
    public function getConnector()
    {
        if (isset($this->connector)) {
            if (empty($this->connector_source)) {
                $this->connector_source = SourceFactory::getSource($this->connector, false);
                $this->connector_source->setEAPM($this);
            }
            return $this->connector_source;
        }
        return null;
    }

    /**
     * Get parameter from source
     * @param string $name
     * @return mixed
     */
    public function getConnectorParam($name)
    {
        $connector =  $this->getConnector();
        if (empty($connector)) {
            return null;
        }
        return $connector->getProperty($name);
    }


    /**
     * formatCallbackURL
     *
     * This function takes a callback_url and checks the $_REQUEST variable to see if
     * additional parameters should be appended to the callback_url value.  The $_REQUEST variables
     * that are being checked deal with handling the behavior of closing/hiding windows/tabs that
     * are displayed when prompting for OAUTH validation
     *
     * @param string $callback_url value of callback URL
     * @return string value of URL with applicable formatting
     */
    protected function formatCallbackURL($callback_url)
    {
        // This is a tweak so that we can automatically close windows if requested by the external account system
        if (isset($_REQUEST['closeWhenDone']) && $_REQUEST['closeWhenDone'] == 1) {
            $callback_url .= '&closeWhenDone=1';
        }

        //Pass back the callbackFunction to call on the window.opener object
        if (!empty($_REQUEST['callbackFunction'])) {
            $callback_url .= '&callbackFunction=' . $_REQUEST['callbackFunction'];
        }

        //Pass back the id of the application that triggered this oauth login
        if (!empty($_REQUEST['application'])) {
            $callback_url .= '&application=' . $_REQUEST['application'];
        }

        //Pass back the id of the application that triggered this oauth login
        if (!empty($_REQUEST['refreshParentWindow'])) {
            $callback_url .= '&refreshParentWindow=' . $_REQUEST['refreshParentWindow'];
        }

        return $callback_url;
    }

    /**
     * Allow API clients to provide translated language strings for a given error code
     *
     * @param unknown_type $error_numb
     */
    protected function getErrorStringFromCode($error_numb)
    {
        $language_key = $this->_appStringErrorPrefix . $error_numb;
        if (isset($GLOBALS['app_strings'][$language_key])) {
            return $GLOBALS['app_strings'][$language_key];
        } else {
            return $GLOBALS['app_strings']['ERR_EXTERNAL_API_SAVE_FAIL'];
        }
    }

    /**
     * Determine if mime detection extensions are available.
     *
     * @return bool
     */
    public function isMimeDetectionAvailable()
    {
        return (function_exists('mime_content_type') || function_exists('ext2mime'));
    }
}
