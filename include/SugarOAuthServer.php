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


require_once 'modules/OAuthTokens/OAuthToken.php';
require_once 'modules/OAuthKeys/OAuthKey.php';
/**
 * Sugar OAuth provider implementation
 * @api
 */
#[\AllowDynamicProperties]
class SugarOAuthServer
{
    /**
     * OAuth token
     * @var OAuthToken
     */
    protected $token;

    /**
     * Check if everything is OK
     * @throws OAuthException
     */
    protected function check()
    {
        if (!function_exists('mhash') && !function_exists('hash_hmac')) {
            // define exception class
            throw new OAuthException("MHash extension required for OAuth support");
        }
    }

    /**
     * Is this functionality enabled?
     */
    public static function enabled()
    {
        return function_exists('mhash') || function_exists('hash_hmac');
    }

    /**
     * Find consumer by key
     * @param $provider
     */
    public function lookupConsumer($provider)
    {
        // check $provider->consumer_key
        // on unknown: SuiteCRM\Zend_Oauth_Provider::CONSUMER_KEY_UNKNOWN
        // on bad key: SuiteCRM\Zend_Oauth_Provider::CONSUMER_KEY_REFUSED
        $GLOBALS['log']->debug("OAUTH: lookupConsumer, key={$provider->consumer_key}");
        $consumer = OAuthKey::fetchKey($provider->consumer_key);
        if (!$consumer) {
            return SuiteCRM\Zend_Oauth_Provider::CONSUMER_KEY_UNKNOWN;
        }
        $provider->consumer_secret = $consumer->c_secret;
        $this->consumer = $consumer;
        return SuiteCRM\Zend_Oauth_Provider::OK;
    }

    /**
     * Check timestamps & nonces
     * @param OAuthProvider $provider
     */
    public function timestampNonceChecker($provider)
    {
        // FIXME: add ts/nonce verification
        if (empty($provider->nonce)) {
            return SuiteCRM\Zend_Oauth_Provider::BAD_NONCE;
        }
        if (empty($provider->timestamp)) {
            return SuiteCRM\Zend_Oauth_Provider::BAD_TIMESTAMP;
        }
        return OAuthToken::checkNonce($provider->consumer_key, $provider->nonce, $provider->timestamp);
    }

    /**
     * Vefiry incoming token
     * @param OAuthProvider $provider
     */
    public function tokenHandler($provider)
    {
        $GLOBALS['log']->debug("OAUTH: tokenHandler, token={$provider->token}, verify={$provider->verifier}");

        $token = OAuthToken::load($provider->token);
        if (empty($token)) {
            return SuiteCRM\Zend_Oauth_Provider::TOKEN_REJECTED;
        }
        if ($token->consumer != $this->consumer->id) {
            return SuiteCRM\Zend_Oauth_Provider::TOKEN_REJECTED;
        }
        $GLOBALS['log']->debug("OAUTH: tokenHandler, found token=".var_export($token->id, true));
        if ($token->tstate == OAuthToken::REQUEST) {
            if (!empty($token->verify) && $provider->verifier == $token->verify) {
                $provider->token_secret = $token->secret;
                $this->token = $token;
                return SuiteCRM\Zend_Oauth_Provider::OK;
            }
            return SuiteCRM\Zend_Oauth_Provider::TOKEN_USED;
        }
        if ($token->tstate == OAuthToken::ACCESS) {
            $provider->token_secret = $token->secret;
            $this->token = $token;
            return SuiteCRM\Zend_Oauth_Provider::OK;
        }
        return SuiteCRM\Zend_Oauth_Provider::TOKEN_REJECTED;
    }

    /**
     * Decode POST/GET via from_html()
     * @return array decoded data
     */
    protected function decodePostGet()
    {
        $data = $_GET;
        $data = array_merge($data, $_POST);
        foreach ($data as $k => $v) {
            $data[$k] = from_html($v);
        }
        return $data;
    }

    /**
     * Create OAuth provider
     *
     * Checks current request for OAuth valitidy
     * @param bool $add_rest add REST endpoint as request path
     */
    public function __construct($req_path = '')
    {
        $GLOBALS['log']->debug("OAUTH: __construct($req_path): ".var_export($_REQUEST, true));
        $this->check();
        $this->provider = new SuiteCRM\Zend_Oauth_Provider();
        try {
            $this->provider->setConsumerHandler(array($this,'lookupConsumer'));
            $this->provider->setTimestampNonceHandler(array($this,'timestampNonceChecker'));
            $this->provider->setTokenHandler(array($this,'tokenHandler'));
            if (!empty($req_path)) {
                $this->provider->isRequestTokenEndpoint($req_path);  // No token needed for this end point
            }
            $this->provider->checkOAuthRequest(null, $this->decodePostGet());
            if (mt_rand() % 10 == 0) {
                // cleanup 1 in 10 times
                OAuthToken::cleanup();
            }
        } catch (Exception $e) {
            $GLOBALS['log']->debug($this->reportProblem($e));
            throw $e;
        }
    }

    /**
     * Generate request token string
     * @return string
     */
    public function requestToken()
    {
        $GLOBALS['log']->debug("OAUTH: requestToken");
        $token = OAuthToken::generate();
        $token->setConsumer($this->consumer);
        $params = $this->provider->getOAuthParams();
        if (!empty($params['oauth_callback']) && $params['oauth_callback'] != 'oob') {
            $token->setCallbackURL($params['oauth_callback']);
        }
        $token->save();
        return $token->queryString();
    }

    /**
     * Generate access token string - must have validated request token
     * @return string
     */
    public function accessToken()
    {
        $GLOBALS['log']->debug("OAUTH: accessToken");
        if (empty($this->token) || $this->token->tstate != OAuthToken::REQUEST) {
            return null;
        }
        $this->token->invalidate();
        $token = OAuthToken::generate();
        $token->setState(OAuthToken::ACCESS);
        $token->setConsumer($this->consumer);
        // transfer user data from request token
        $token->copyAuthData($this->token);
        $token->save();
        return $token->queryString();
    }

    /**
     * Return authorization URL
     * @return string
     */
    public function authUrl()
    {
        return urlencode(rtrim($GLOBALS['sugar_config']['site_url'], '/')."/index.php?module=OAuthTokens&action=authorize");
    }

    /**
     * Fetch current token if it is authorized
     * @return OAuthToken|null
     */
    public function authorizedToken()
    {
        if ($this->token->tstate == OAuthToken::ACCESS) {
            return $this->token;
        }
        return null;
    }

    /**
     * Fetch authorization data from current token
     * @return mixed Authorization data or null if none
     */
    public function authorization()
    {
        if ($this->token->tstate == OAuthToken::ACCESS) {
            return $this->token->authdata;
        }
        return null;
    }

    /**
     * Report OAuth problem as string
     */
    public function reportProblem(Exception $e)
    {
        return $this->provider->reportProblem($e);
    }
}

if (!class_exists('OAuthException')) {
    // we will use this in case oauth extension is not loaded
    #[\AllowDynamicProperties]
    class OAuthException extends Exception
    {
    }
}
