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


require_once('include/externalAPI/Base/ExternalAPIBase.php');

/**
 * External API based on OAuth
 * @api
 */
class OAuthPluginBase extends ExternalAPIBase implements ExternalOAuthAPIPlugin
{
    public $oauth_token;
    public $oauth_secret;
    public $eapmBean;
    public $authMethod = 'oauth';
    protected $oauthParams = array();
    protected $oauth_keys_initialized = false;

    public function __construct()
    {
    }

    /**
     * Setup oauth parameters from connector
     */
    public function setupOauthKeys()
    {
        if ($this->oauth_keys_initialized) {
            return;
        }

        $connector = $this->getConnector();
        if (!empty($connector)) {
            $cons_key = $connector->getProperty('oauth_consumer_key');
            if (!empty($cons_key)) {
                $this->oauthParams['consumerKey'] = $cons_key;
            }
            $cons_secret = $connector->getProperty('oauth_consumer_secret');
            if (!empty($cons_secret)) {
                $this->oauthParams['consumerSecret'] = $cons_secret;
            }
        }
        $this->oauth_keys_initialized = true;
    }

    /**
     * Load data from EAPM bean
     * @see ExternalAPIBase::loadEAPM()
     */
    public function loadEAPM($eapmBean)
    {
        if (!parent::loadEAPM($eapmBean)) {
            return false;
        }

        $this->oauth_token = $eapmBean->oauth_token;
        $this->oauth_secret = $eapmBean->oauth_secret;

        return true;
    }

    /**
     * Check login
     * @param EAPM $eapmBean
     * @see ExternalAPIBase::checkLogin()
     */
    public function checkLogin($eapmBean = null)
    {
        $reply = parent::checkLogin($eapmBean);
        if (!$reply['success']) {
            return $reply;
        }

        if ($this->checkOauthLogin()) {
            return array('success' => true);
        }
    }

    public function quickCheckLogin()
    {
        $reply = parent::quickCheckLogin();

        if (!$reply['success']) {
            return $reply;
        }

        if (!empty($this->oauth_token) && !empty($this->oauth_secret)) {
            return array('success'=>true);
        } else {
            return array('success'=>false,'errorMessage'=>translate('LBL_ERR_NO_TOKEN', 'EAPM'));
        }
    }

    protected function checkOauthLogin()
    {
        if (empty($this->oauth_token) || empty($this->oauth_secret)) {
            return $this->oauthLogin();
        } else {
            return false;
        }
    }

    public function getOauthParams()
    {
        return $this->getValue("oauthParams");
    }

    public function getOauthRequestURL()
    {
        return $this->getValue("oauthReq");
    }

    public function getOauthAuthURL()
    {
        return $this->getValue("oauthAuth");
    }

    public function getOauthAccessURL()
    {
        return $this->getValue("oauthAccess");
    }

    /**
     * Get OAuth client
     * @return SugarOauth
     */
    public function getOauth()
    {
        $this->setupOauthKeys();
        $oauth = new SugarOAuth($this->oauthParams['consumerKey'], $this->oauthParams['consumerSecret'], $this->getOauthParams());

        if (isset($this->oauth_token) && !empty($this->oauth_token)) {
            $oauth->setToken($this->oauth_token, $this->oauth_secret);
        }

        return $oauth;
    }

    public function oauthLogin()
    {
        global $sugar_config;
        $oauth = $this->getOauth();
        if (isset($_SESSION['eapm_oauth_secret']) && isset($_SESSION['eapm_oauth_token']) && isset($_REQUEST['oauth_token']) && isset($_REQUEST['oauth_verifier'])) {
            $stage = 1;
        } else {
            $stage = 0;
        }
        if ($stage == 0) {
            $oauthReq = $this->getOauthRequestURL();
            $callback_url = $sugar_config['site_url'].'/index.php?module=EAPM&action=oauth&record='.$this->eapmBean->id;
            $callback_url = $this->formatCallbackURL($callback_url);

            $GLOBALS['log']->debug("OAuth request token: {$oauthReq} callback: $callback_url");

            $request_token_info = $oauth->getRequestTokenByUrl($oauthReq, $callback_url);

            $GLOBALS['log']->debug("OAuth token: ".var_export($request_token_info, true));

            if (empty($request_token_info['oauth_token_secret']) || empty($request_token_info['oauth_token'])) {
                return false;
            } else {
                // FIXME: error checking here
                $_SESSION['eapm_oauth_secret'] = $request_token_info['oauth_token_secret'];
                $_SESSION['eapm_oauth_token'] = $request_token_info['oauth_token'];
                $authReq = $this->getOauthAuthURL();
                SugarApplication::redirect("{$authReq}?oauth_token={$request_token_info['oauth_token']}");
            }
        } else {
            $accReq = $this->getOauthAccessURL();
            $oauth->setToken($_SESSION['eapm_oauth_token'], $_SESSION['eapm_oauth_secret']);
            $GLOBALS['log']->debug("OAuth access token: {$accReq}");
            $access_token_info = $oauth->getAccessToken($accReq);
            $GLOBALS['log']->debug("OAuth token: ".var_export($access_token_info, true));
            // FIXME: error checking here
            $this->oauth_token = $access_token_info['oauth_token'];
            $this->oauth_secret = $access_token_info['oauth_token_secret'];
            $this->eapmBean->oauth_token = $this->oauth_token;
            $this->eapmBean->oauth_secret = $this->oauth_secret;
            $oauth->setToken($this->oauth_token, $this->oauth_secret);
            $this->eapmBean->validated = 1;
            $this->eapmBean->save();
            unset($_SESSION['eapm_oauth_token']);
            unset($_SESSION['eapm_oauth_secret']);
            return true;
        }
        return false;
    }
}
