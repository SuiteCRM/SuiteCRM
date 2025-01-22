<?php
/**
 * Copyright 2011 Facebook, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License. You may obtain
 * a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations
 * under the License.
 */

if (!function_exists('curl_init')) {
    throw new Exception('Facebook needs the CURL PHP extension.');
}
if (!function_exists('json_decode')) {
    throw new Exception('Facebook needs the JSON PHP extension.');
}

/**
 * Thrown when an API call returns an exception.
 *
 * @author Naitik Shah <naitik@facebook.com>
 */
class FacebookApiException extends Exception
{
    /**
     * The result from the API server that represents the exception information.
     *
     * @var mixed
     */
    protected $result;

    /**
     * Make a new API Exception with the given result.
     *
     * @param array $result The result from the API server
     */
    public function __construct($result)
    {
        $this->result = $result;

        $code = 0;
        if (isset($result['error_code']) && is_int($result['error_code'])) {
            $code = $result['error_code'];
        }

        if (isset($result['error_description'])) {
            // OAuth 2.0 Draft 10 style
            $msg = $result['error_description'];
        } elseif (isset($result['error']) && is_array($result['error'])) {
            // OAuth 2.0 Draft 00 style
            $msg = $result['error']['message'];
        } elseif (isset($result['error_msg'])) {
            // Rest server style
            $msg = $result['error_msg'];
        } else {
            $msg = 'Unknown Error. Check getResult()';
        }

        parent::__construct($msg, $code);
    }

    /**
     * Return the associated result object returned by the API server.
     *
     * @return array The result from the API server
     */
    public function getResult()
    {
        return $this->result;
    }

    /**
     * Returns the associated type for the error. This will default to
     * 'Exception' when a type is not available.
     *
     * @return string
     */
    public function getType()
    {
        if (isset($this->result['error'])) {
            $error = $this->result['error'];
            if (is_string($error)) {
                // OAuth 2.0 Draft 10 style
                return $error;
            } elseif (is_array($error)) {
                // OAuth 2.0 Draft 00 style
                if (isset($error['type'])) {
                    return $error['type'];
                }
            }
        }

        return 'Exception';
    }

    /**
     * To make debugging easier.
     *
     * @return string The string representation of the error
     */
    public function __toString()
    {
        $str = $this->getType() . ': ';
        if ($this->code != 0) {
            $str .= $this->code . ': ';
        }
        return $str . $this->message;
    }
}

/**
 * Provides access to the Facebook Platform.  This class provides
 * a majority of the functionality needed, but the class is abstract
 * because it is designed to be sub-classed.  The subclass must
 * implement the four abstract methods listed at the bottom of
 * the file.
 *
 * @author Naitik Shah <naitik@facebook.com>
 */
abstract class BaseFacebook
{
    /**
     * Version.
     */
    const VERSION = '3.2.3';

    /**
     * Signed Request Algorithm.
     */
    const SIGNED_REQUEST_ALGORITHM = 'HMAC-SHA256';

    /**
     * Default options for curl.
     *
     * @var array
     */
    public static $CURL_OPTS = array(
    CURLOPT_CONNECTTIMEOUT => 10,
    CURLOPT_RETURNTRANSFER => true,
    CURLOPT_TIMEOUT        => 60,
    CURLOPT_USERAGENT      => 'facebook-php-3.2',
  );

    /**
     * List of query parameters that get automatically dropped when rebuilding
     * the current URL.
     *
     * @var array
     */
    protected static $DROP_QUERY_PARAMS = array(
    'code',
    'state',
    'signed_request',
  );

    /**
     * Maps aliases to Facebook domains.
     *
     * @var array
     */
    public static $DOMAIN_MAP = array(
    'api'         => 'https://api.facebook.com/',
    'api_video'   => 'https://api-video.facebook.com/',
    'api_read'    => 'https://api-read.facebook.com/',
    'graph'       => 'https://graph.facebook.com/',
    'graph_video' => 'https://graph-video.facebook.com/',
    'www'         => 'https://www.facebook.com/',
  );

    /**
     * The Application ID.
     *
     * @var string
     */
    protected $appId;

    /**
     * The Application App Secret.
     *
     * @var string
     */
    protected $appSecret;

    /**
     * The ID of the Facebook user, or 0 if the user is logged out.
     *
     * @var integer
     */
    protected $user;

    /**
     * The data from the signed_request token.
     *
     * @var string
     */
    protected $signedRequest;

    /**
     * A CSRF state variable to assist in the defense against CSRF attacks.
     *
     * @var string
     */
    protected $state;

    /**
     * The OAuth access token received in exchange for a valid authorization
     * code.  null means the access token has yet to be determined.
     *
     * @var string
     */
    protected $accessToken = null;

    /**
     * Indicates if the CURL based @ syntax for file uploads is enabled.
     *
     * @var boolean
     */
    protected $fileUploadSupport = false;

    /**
     * Indicates if we trust HTTP_X_FORWARDED_* headers.
     *
     * @var boolean
     */
    protected $trustForwarded = false;

    /**
     * Indicates if signed_request is allowed in query parameters.
     *
     * @var boolean
     */
    protected $allowSignedRequest = true;

    /**
     * Initialize a Facebook Application.
     *
     * The configuration:
     * - appId: the application ID
     * - secret: the application secret
     * - fileUpload: (optional) boolean indicating if file uploads are enabled
     * - allowSignedRequest: (optional) boolean indicating if signed_request is
     *                       allowed in query parameters or POST body.  Should be
     *                       false for non-canvas apps.  Defaults to true.
     *
     * @param array $config The application configuration
     */
    public function __construct($config)
    {
        $this->setAppId($config['appId']);
        $this->setAppSecret($config['secret']);
        if (isset($config['fileUpload'])) {
            $this->setFileUploadSupport($config['fileUpload']);
        }
        if (isset($config['trustForwarded']) && $config['trustForwarded']) {
            $this->trustForwarded = true;
        }
        if (isset($config['allowSignedRequest'])
        && !$config['allowSignedRequest']) {
            $this->allowSignedRequest = false;
        }
        $state = $this->getPersistentData('state');
        if (!empty($state)) {
            $this->state = $state;
        }
    }

    /**
     * Set the Application ID.
     *
     * @param string $appId The Application ID
     *
     * @return BaseFacebook
     */
    public function setAppId($appId)
    {
        $this->appId = $appId;
        return $this;
    }

    /**
     * Get the Application ID.
     *
     * @return string the Application ID
     */
    public function getAppId()
    {
        return $this->appId;
    }

    /**
     * Set the App Secret.
     *
     * @param string $apiSecret The App Secret
     *
     * @return BaseFacebook
     * @deprecated Use setAppSecret instead.
     * @see setAppSecret()
     */
    public function setApiSecret($apiSecret)
    {
        $this->setAppSecret($apiSecret);
        return $this;
    }

    /**
     * Set the App Secret.
     *
     * @param string $appSecret The App Secret
     *
     * @return BaseFacebook
     */
    public function setAppSecret($appSecret)
    {
        $this->appSecret = $appSecret;
        return $this;
    }

    /**
     * Get the App Secret.
     *
     * @return string the App Secret
     *
     * @deprecated Use getAppSecret instead.
     * @see getAppSecret()
     */
    public function getApiSecret()
    {
        return $this->getAppSecret();
    }

    /**
     * Get the App Secret.
     *
     * @return string the App Secret
     */
    public function getAppSecret()
    {
        return $this->appSecret;
    }

    /**
     * Set the file upload support status.
     *
     * @param boolean $fileUploadSupport The file upload support status.
     *
     * @return BaseFacebook
     */
    public function setFileUploadSupport($fileUploadSupport)
    {
        $this->fileUploadSupport = $fileUploadSupport;
        return $this;
    }

    /**
     * Get the file upload support status.
     *
     * @return boolean true if and only if the server supports file upload.
     */
    public function getFileUploadSupport()
    {
        return $this->fileUploadSupport;
    }

    /**
     * Get the file upload support status.
     *
     * @return boolean true if and only if the server supports file upload.
     *
     * @deprecated Use getFileUploadSupport instead.
     * @see getFileUploadSupport()
     */
    public function useFileUploadSupport()
    {
        return $this->getFileUploadSupport();
    }

    /**
     * Sets the access token for api calls.  Use this if you get
     * your access token by other means and just want the SDK
     * to use it.
     *
     * @param string $access_token an access token.
     *
     * @return BaseFacebook
     */
    public function setAccessToken($access_token)
    {
        $this->accessToken = $access_token;
        return $this;
    }

    /**
     * Extend an access token, while removing the short-lived token that might
     * have been generated via client-side flow. Thanks to http://bit.ly/b0Pt0H
     * for the workaround.
     */
    public function setExtendedAccessToken()
    {
        try {
            // need to circumvent json_decode by calling _oauthRequest
            // directly, since response isn't JSON format.
            $access_token_response = $this->_oauthRequest(
                $this->getUrl('graph', '/oauth/access_token'),
                $params = array(
          'client_id' => $this->getAppId(),
          'client_secret' => $this->getAppSecret(),
          'grant_type' => 'fb_exchange_token',
          'fb_exchange_token' => $this->getAccessToken(),
        )
      );
        } catch (FacebookApiException $e) {
            // most likely that user very recently revoked authorization.
            // In any event, we don't have an access token, so say so.
            return false;
        }

        if (empty($access_token_response)) {
            return false;
        }

        $response_params = array();
        parse_str($access_token_response, $response_params);

        if (!isset($response_params['access_token'])) {
            return false;
        }

        $this->destroySession();

        $this->setPersistentData(
            'access_token',
            $response_params['access_token']
    );
    }

    /**
     * Determines the access token that should be used for API calls.
     * The first time this is called, $this->accessToken is set equal
     * to either a valid user access token, or it's set to the application
     * access token if a valid user access token wasn't available.  Subsequent
     * calls return whatever the first call returned.
     *
     * @return string The access token
     */
    public function getAccessToken()
    {
        if ($this->accessToken !== null) {
            // we've done this already and cached it.  Just return.
            return $this->accessToken;
        }

        // first establish access token to be the application
        // access token, in case we navigate to the /oauth/access_token
        // endpoint, where SOME access token is required.
        $this->setAccessToken($this->getApplicationAccessToken());
        $user_access_token = $this->getUserAccessToken();
        if ($user_access_token) {
            $this->setAccessToken($user_access_token);
        }

        return $this->accessToken;
    }

    /**
     * Determines and returns the user access token, first using
     * the signed request if present, and then falling back on
     * the authorization code if present.  The intent is to
     * return a valid user access token, or false if one is determined
     * to not be available.
     *
     * @return string A valid user access token, or false if one
     *                could not be determined.
     */
    protected function getUserAccessToken()
    {
        // first, consider a signed request if it's supplied.
        // if there is a signed request, then it alone determines
        // the access token.
        $signed_request = $this->getSignedRequest();
        if ($signed_request) {
            // apps.facebook.com hands the access_token in the signed_request
            if (array_key_exists('oauth_token', $signed_request)) {
                $access_token = $signed_request['oauth_token'];
                $this->setPersistentData('access_token', $access_token);
                return $access_token;
            }

            // the JS SDK puts a code in with the redirect_uri of ''
            if (array_key_exists('code', $signed_request)) {
                $code = $signed_request['code'];
                if ($code && $code == $this->getPersistentData('code')) {
                    // short-circuit if the code we have is the same as the one presented
                    return $this->getPersistentData('access_token');
                }

                $access_token = $this->getAccessTokenFromCode($code, '');
                if ($access_token) {
                    $this->setPersistentData('code', $code);
                    $this->setPersistentData('access_token', $access_token);
                    return $access_token;
                }
            }

            // signed request states there's no access token, so anything
            // stored should be cleared.
            $this->clearAllPersistentData();
            return false; // respect the signed request's data, even
                    // if there's an authorization code or something else
        }

        $code = $this->getCode();
        if ($code && $code != $this->getPersistentData('code')) {
            $access_token = $this->getAccessTokenFromCode($code);
            if ($access_token) {
                $this->setPersistentData('code', $code);
                $this->setPersistentData('access_token', $access_token);
                return $access_token;
            }

            // code was bogus, so everything based on it should be invalidated.
            $this->clearAllPersistentData();
            return false;
        }

        // as a fallback, just return whatever is in the persistent
        // store, knowing nothing explicit (signed request, authorization
        // code, etc.) was present to shadow it (or we saw a code in $_REQUEST,
        // but it's the same as what's in the persistent store)
        return $this->getPersistentData('access_token');
    }

    /**
     * Retrieve the signed request, either from a request parameter or,
     * if not present, from a cookie.
     *
     * @return string the signed request, if available, or null otherwise.
     */
    public function getSignedRequest()
    {
        if (!$this->signedRequest) {
            if ($this->allowSignedRequest && !empty($_REQUEST['signed_request'])) {
                $this->signedRequest = $this->parseSignedRequest(
                    $_REQUEST['signed_request']
        );
            } elseif (!empty($_COOKIE[$this->getSignedRequestCookieName()])) {
                $this->signedRequest = $this->parseSignedRequest(
                    $_COOKIE[$this->getSignedRequestCookieName()]
                );
            }
        }
        return $this->signedRequest;
    }

    /**
     * Get the UID of the connected user, or 0
     * if the Facebook user is not connected.
     *
     * @return string the UID if available.
     */
    public function getUser()
    {
        if ($this->user !== null) {
            // we've already determined this and cached the value.
            return $this->user;
        }

        return $this->user = $this->getUserFromAvailableData();
    }

    /**
     * Determines the connected user by first examining any signed
     * requests, then considering an authorization code, and then
     * falling back to any persistent store storing the user.
     *
     * @return integer The id of the connected Facebook user,
     *                 or 0 if no such user exists.
     */
    protected function getUserFromAvailableData()
    {
        // if a signed request is supplied, then it solely determines
        // who the user is.
        $signed_request = $this->getSignedRequest();
        if ($signed_request) {
            if (array_key_exists('user_id', $signed_request)) {
                $user = $signed_request['user_id'];

                if ($user != $this->getPersistentData('user_id')) {
                    $this->clearAllPersistentData();
                }

                $this->setPersistentData('user_id', $signed_request['user_id']);
                return $user;
            }

            // if the signed request didn't present a user id, then invalidate
            // all entries in any persistent store.
            $this->clearAllPersistentData();
            return 0;
        }

        $user = $this->getPersistentData('user_id', $default = 0);
        $persisted_access_token = $this->getPersistentData('access_token');

        // use access_token to fetch user id if we have a user access_token, or if
        // the cached access token has changed.
        $access_token = $this->getAccessToken();
        if ($access_token &&
        $access_token != $this->getApplicationAccessToken() &&
        !($user && $persisted_access_token == $access_token)) {
            $user = $this->getUserFromAccessToken();
            if ($user) {
                $this->setPersistentData('user_id', $user);
            } else {
                $this->clearAllPersistentData();
            }
        }

        return $user;
    }

    /**
     * Get a Login URL for use with redirects. By default, full page redirect is
     * assumed. If you are using the generated URL with a window.open() call in
     * JavaScript, you can pass in display=popup as part of the $params.
     *
     * The parameters:
     * - redirect_uri: the url to go to after a successful login
     * - scope: comma separated list of requested extended perms
     *
     * @param array $params Provide custom parameters
     * @return string The URL for the login flow
     */
    public function getLoginUrl($params=array())
    {
        $this->establishCSRFTokenState();
        $currentUrl = $this->getCurrentUrl();

        // if 'scope' is passed as an array, convert to comma separated list
        $scopeParams = isset($params['scope']) ? $params['scope'] : null;
        if ($scopeParams && is_array($scopeParams)) {
            $params['scope'] = implode(',', $scopeParams);
        }

        return $this->getUrl(
            'www',
            'dialog/oauth',
            array_merge(
          array(
          'client_id' => $this->getAppId(),
          'redirect_uri' => $currentUrl, // possibly overwritten
          'state' => $this->state,
          'sdk' => 'php-sdk-'.self::VERSION
        ),
          $params
      )
        );
    }

    /**
     * Get a Logout URL suitable for use with redirects.
     *
     * The parameters:
     * - next: the url to go to after a successful logout
     *
     * @param array $params Provide custom parameters
     * @return string The URL for the logout flow
     */
    public function getLogoutUrl($params=array())
    {
        return $this->getUrl(
            'www',
            'logout.php',
            array_merge(array(
        'next' => $this->getCurrentUrl(),
        'access_token' => $this->getUserAccessToken(),
      ), $params)
    );
    }

    /**
     * Get a login status URL to fetch the status from Facebook.
     *
     * @param array $params Provide custom parameters
     * @return string The URL for the logout flow
     */
    public function getLoginStatusUrl($params=array())
    {
        return $this->getLoginUrl(
            array_merge(array(
        'response_type' => 'code',
        'display' => 'none',
      ), $params)
    );
    }

    /**
     * Make an API call.
     *
     * @return mixed The decoded response
     */
    public function api(/* polymorphic */)
    {
        $args = func_get_args();
        if (is_array($args[0])) {
            return $this->_restserver($args[0]);
        }
        return call_user_func_array(array($this, '_graph'), $args);
    }

    /**
     * Constructs and returns the name of the cookie that
     * potentially houses the signed request for the app user.
     * The cookie is not set by the BaseFacebook class, but
     * it may be set by the JavaScript SDK.
     *
     * @return string the name of the cookie that would house
     *         the signed request value.
     */
    protected function getSignedRequestCookieName()
    {
        return 'fbsr_'.$this->getAppId();
    }

    /**
     * Constructs and returns the name of the cookie that potentially contain
     * metadata. The cookie is not set by the BaseFacebook class, but it may be
     * set by the JavaScript SDK.
     *
     * @return string the name of the cookie that would house metadata.
     */
    protected function getMetadataCookieName()
    {
        return 'fbm_'.$this->getAppId();
    }

    /**
     * Get the authorization code from the query parameters, if it exists,
     * and otherwise return false to signal no authorization code was
     * discoverable.
     *
     * @return mixed The authorization code, or false if the authorization
     *               code could not be determined.
     */
    protected function getCode()
    {
        if (!isset($_REQUEST['code']) || !isset($_REQUEST['state'])) {
            return false;
        }
        if ($this->state === $_REQUEST['state']) {
            // CSRF state has done its job, so clear it
            $this->state = null;
            $this->clearPersistentData('state');
            return $_REQUEST['code'];
        }
        self::errorLog('CSRF state token does not match one provided.');

        return false;
    }

    /**
     * Retrieves the UID with the understanding that
     * $this->accessToken has already been set and is
     * seemingly legitimate.  It relies on Facebook's Graph API
     * to retrieve user information and then extract
     * the user ID.
     *
     * @return integer Returns the UID of the Facebook user, or 0
     *                 if the Facebook user could not be determined.
     */
    protected function getUserFromAccessToken()
    {
        try {
            $user_info = $this->api('/me');
            return $user_info['id'];
        } catch (FacebookApiException $e) {
            return 0;
        }
    }

    /**
     * Returns the access token that should be used for logged out
     * users when no authorization code is available.
     *
     * @return string The application access token, useful for gathering
     *                public information about users and applications.
     */
    public function getApplicationAccessToken()
    {
        return $this->appId.'|'.$this->appSecret;
    }

    /**
     * Lays down a CSRF state token for this process.
     *
     * @return void
     */
    protected function establishCSRFTokenState()
    {
        if ($this->state === null) {
            $this->state = md5(uniqid(mt_rand(), true));
            $this->setPersistentData('state', $this->state);
        }
    }

    /**
     * Retrieves an access token for the given authorization code
     * (previously generated from www.facebook.com on behalf of
     * a specific user).  The authorization code is sent to graph.facebook.com
     * and a legitimate access token is generated provided the access token
     * and the user for which it was generated all match, and the user is
     * either logged in to Facebook or has granted an offline access permission.
     *
     * @param string $code An authorization code.
     * @param string $redirect_uri Optional redirect URI. Default null
     *
     * @return mixed An access token exchanged for the authorization code, or
     *               false if an access token could not be generated.
     */
    protected function getAccessTokenFromCode($code, $redirect_uri = null)
    {
        if (empty($code)) {
            return false;
        }

        if ($redirect_uri === null) {
            $redirect_uri = $this->getCurrentUrl();
        }

        try {
            // need to circumvent json_decode by calling _oauthRequest
            // directly, since response isn't JSON format.
            $access_token_response =
        $this->_oauthRequest(
            $this->getUrl('graph', '/oauth/access_token'),
            $params = array('client_id' => $this->getAppId(),
                          'client_secret' => $this->getAppSecret(),
                          'redirect_uri' => $redirect_uri,
                          'code' => $code)
        );
        } catch (FacebookApiException $e) {
            // most likely that user very recently revoked authorization.
            // In any event, we don't have an access token, so say so.
            return false;
        }

        if (empty($access_token_response)) {
            return false;
        }

        $response_params = array();
        parse_str($access_token_response, $response_params);
        if (!isset($response_params['access_token'])) {
            return false;
        }

        return $response_params['access_token'];
    }

    /**
     * Invoke the old restserver.php endpoint.
     *
     * @param array $params Method call object
     *
     * @return mixed The decoded response object
     * @throws FacebookApiException
     */
    protected function _restserver($params)
    {
        // generic application level parameters
        $params['api_key'] = $this->getAppId();
        $params['format'] = 'json-strings';

        $result = json_decode($this->_oauthRequest(
            $this->getApiUrl($params['method']),
            $params
    ), true);

        // results are returned, errors are thrown
        if (is_array($result) && isset($result['error_code'])) {
            $this->throwAPIException($result);
            // @codeCoverageIgnoreStart
        }
        // @codeCoverageIgnoreEnd

        $method = strtolower($params['method']);
        if ($method === 'auth.expiresession' ||
        $method === 'auth.revokeauthorization') {
            $this->destroySession();
        }

        return $result;
    }

    /**
     * Return true if this is video post.
     *
     * @param string $path The path
     * @param string $method The http method (default 'GET')
     *
     * @return boolean true if this is video post
     */
    protected function isVideoPost($path, $method = 'GET')
    {
        if ($method == 'POST' && preg_match("/^(\/)(.+)(\/)(videos)$/", $path)) {
            return true;
        }
        return false;
    }

    /**
     * Invoke the Graph API.
     *
     * @param string $path The path (required)
     * @param string $method The http method (default 'GET')
     * @param array $params The query/post data
     *
     * @return mixed The decoded response object
     * @throws FacebookApiException
     */
    protected function _graph($path, $method = 'GET', $params = array())
    {
        if (is_array($method) && empty($params)) {
            $params = $method;
            $method = 'GET';
        }
        $params['method'] = $method; // method override as we always do a POST

        if ($this->isVideoPost($path, $method)) {
            $domainKey = 'graph_video';
        } else {
            $domainKey = 'graph';
        }

        $result = json_decode($this->_oauthRequest(
            $this->getUrl($domainKey, $path),
            $params
    ), true);

        // results are returned, errors are thrown
        if (is_array($result) && isset($result['error'])) {
            $this->throwAPIException($result);
            // @codeCoverageIgnoreStart
        }
        // @codeCoverageIgnoreEnd

        return $result;
    }

    /**
     * Make a OAuth Request.
     *
     * @param string $url The path (required)
     * @param array $params The query/post data
     *
     * @return string The decoded response object
     * @throws FacebookApiException
     */
    protected function _oauthRequest($url, $params)
    {
        if (!isset($params['access_token'])) {
            $params['access_token'] = $this->getAccessToken();
        }

        if (isset($params['access_token']) && !isset($params['appsecret_proof'])) {
            $params['appsecret_proof'] = $this->getAppSecretProof($params['access_token']);
        }

        // json_encode all params values that are not strings
        foreach ($params as $key => $value) {
            if (!is_string($value) && !($value instanceof CURLFile)) {
                $params[$key] = json_encode($value);
            }
        }

        return $this->makeRequest($url, $params);
    }

    /**
     * Generate a proof of App Secret
     * This is required for all API calls originating from a server
     * It is a sha256 hash of the access_token made using the app secret
     *
     * @param string $access_token The access_token to be hashed (required)
     *
     * @return string The sha256 hash of the access_token
     */
    protected function getAppSecretProof($access_token)
    {
        return hash_hmac('sha256', $access_token, $this->getAppSecret());
    }

    /**
     * Makes an HTTP request. This method can be overridden by subclasses if
     * developers want to do fancier things or use something other than curl to
     * make the request.
     *
     * @param string $url The URL to make the request to
     * @param array $params The parameters to use for the POST body
     * @param CurlHandler $ch Initialized curl handle
     *
     * @return string The response text
     */
    protected function makeRequest($url, $params, $ch=null)
    {
        if (!$ch) {
            $ch = curl_init();
        }

        $opts = self::$CURL_OPTS;
        if ($this->getFileUploadSupport()) {
            $opts[CURLOPT_POSTFIELDS] = $params;
        } else {
            $opts[CURLOPT_POSTFIELDS] = http_build_query($params, null, '&');
        }
        $opts[CURLOPT_URL] = $url;

        // disable the 'Expect: 100-continue' behaviour. This causes CURL to wait
        // for 2 seconds if the server does not support this header.
        if (isset($opts[CURLOPT_HTTPHEADER])) {
            $existing_headers = $opts[CURLOPT_HTTPHEADER];
            $existing_headers[] = 'Expect:';
            $opts[CURLOPT_HTTPHEADER] = $existing_headers;
        } else {
            $opts[CURLOPT_HTTPHEADER] = array('Expect:');
        }

        curl_setopt_array($ch, $opts);
        $result = curl_exec($ch);

        $errno = curl_errno($ch);
        // CURLE_SSL_CACERT || CURLE_SSL_CACERT_BADFILE
        if ($errno == 60 || $errno == 77) {
            self::errorLog('Invalid or no certificate authority found, '.
                     'using bundled information');
            curl_setopt(
                $ch,
                CURLOPT_CAINFO,
                dirname(__FILE__) . DIRECTORY_SEPARATOR . 'fb_ca_chain_bundle.crt'
            );
            $result = curl_exec($ch);
        }

        // With dual stacked DNS responses, it's possible for a server to
        // have IPv6 enabled but not have IPv6 connectivity.  If this is
        // the case, curl will try IPv4 first and if that fails, then it will
        // fall back to IPv6 and the error EHOSTUNREACH is returned by the
        // operating system.
        if ($result === false && empty($opts[CURLOPT_IPRESOLVE])) {
            $matches = array();
            $regex = '/Failed to connect to ([^:].*): Network is unreachable/';
            if (preg_match($regex, curl_error($ch), $matches)) {
                if (strlen(@inet_pton($matches[1])) === 16) {
                    self::errorLog('Invalid IPv6 configuration on server, '.
                           'Please disable or get native IPv6 on your server.');
                    self::$CURL_OPTS[CURLOPT_IPRESOLVE] = CURL_IPRESOLVE_V4;
                    curl_setopt($ch, CURLOPT_IPRESOLVE, CURL_IPRESOLVE_V4);
                    $result = curl_exec($ch);
                }
            }
        }

        if ($result === false) {
            $e = new FacebookApiException(array(
        'error_code' => curl_errno($ch),
        'error' => array(
        'message' => curl_error($ch),
        'type' => 'CurlException',
        ),
      ));
            curl_close($ch);
            throw $e;
        }
        curl_close($ch);
        return $result;
    }

    /**
     * Parses a signed_request and validates the signature.
     *
     * @param string $signed_request A signed token
     *
     * @return array The payload inside it or null if the sig is wrong
     */
    protected function parseSignedRequest($signed_request)
    {
        if (!$signed_request || strpos($signed_request, '.') === false) {
            self::errorLog('Signed request was invalid!');
            return null;
        }

        list($encoded_sig, $payload) = explode('.', $signed_request, 2);

        // decode the data
        $sig = self::base64UrlDecode($encoded_sig);
        $data = json_decode(self::base64UrlDecode($payload), true);

        if (!isset($data['algorithm'])
        || strtoupper($data['algorithm']) !==  self::SIGNED_REQUEST_ALGORITHM
    ) {
            self::errorLog(
                'Unknown algorithm. Expected ' . self::SIGNED_REQUEST_ALGORITHM
            );
            return null;
        }

        // check sig
        $expected_sig = hash_hmac(
            'sha256',
            $payload,
            $this->getAppSecret(),
            $raw = true
        );

        if (strlen($expected_sig) !== strlen($sig)) {
            self::errorLog('Bad Signed JSON signature!');
            return null;
        }

        $result = 0;
        for ($i = 0; $i < strlen($expected_sig); $i++) {
            $result |= ord($expected_sig[$i]) ^ ord($sig[$i]);
        }

        if ($result == 0) {
            return $data;
        }
        self::errorLog('Bad Signed JSON signature!');
        return null;
    }

    /**
     * Makes a signed_request blob using the given data.
     *
     * @param array $data The data array.
     *
     * @return string The signed request.
     */
    protected function makeSignedRequest($data)
    {
        if (!is_array($data)) {
            throw new InvalidArgumentException(
                'makeSignedRequest expects an array. Got: ' . print_r($data, true)
            );
        }
        $data['algorithm'] = self::SIGNED_REQUEST_ALGORITHM;
        $data['issued_at'] = time();
        $json = json_encode($data);
        $b64 = self::base64UrlEncode($json);
        $raw_sig = hash_hmac('sha256', $b64, $this->getAppSecret(), $raw = true);
        $sig = self::base64UrlEncode($raw_sig);
        return $sig.'.'.$b64;
    }

    /**
     * Build the URL for api given parameters.
     *
     * @param string $method The method name.
     *
     * @return string The URL for the given parameters
     */
    protected function getApiUrl($method)
    {
        static $READ_ONLY_CALLS =
      array('admin.getallocation' => 1,
            'admin.getappproperties' => 1,
            'admin.getbannedusers' => 1,
            'admin.getlivestreamvialink' => 1,
            'admin.getmetrics' => 1,
            'admin.getrestrictioninfo' => 1,
            'application.getpublicinfo' => 1,
            'auth.getapppublickey' => 1,
            'auth.getsession' => 1,
            'auth.getsignedpublicsessiondata' => 1,
            'comments.get' => 1,
            'connect.getunconnectedfriendscount' => 1,
            'dashboard.getactivity' => 1,
            'dashboard.getcount' => 1,
            'dashboard.getglobalnews' => 1,
            'dashboard.getnews' => 1,
            'dashboard.multigetcount' => 1,
            'dashboard.multigetnews' => 1,
            'data.getcookies' => 1,
            'events.get' => 1,
            'events.getmembers' => 1,
            'fbml.getcustomtags' => 1,
            'feed.getappfriendstories' => 1,
            'feed.getregisteredtemplatebundlebyid' => 1,
            'feed.getregisteredtemplatebundles' => 1,
            'fql.multiquery' => 1,
            'fql.query' => 1,
            'friends.arefriends' => 1,
            'friends.get' => 1,
            'friends.getappusers' => 1,
            'friends.getlists' => 1,
            'friends.getmutualfriends' => 1,
            'gifts.get' => 1,
            'groups.get' => 1,
            'groups.getmembers' => 1,
            'intl.gettranslations' => 1,
            'links.get' => 1,
            'notes.get' => 1,
            'notifications.get' => 1,
            'pages.getinfo' => 1,
            'pages.isadmin' => 1,
            'pages.isappadded' => 1,
            'pages.isfan' => 1,
            'permissions.checkavailableapiaccess' => 1,
            'permissions.checkgrantedapiaccess' => 1,
            'photos.get' => 1,
            'photos.getalbums' => 1,
            'photos.gettags' => 1,
            'profile.getinfo' => 1,
            'profile.getinfooptions' => 1,
            'stream.get' => 1,
            'stream.getcomments' => 1,
            'stream.getfilters' => 1,
            'users.getinfo' => 1,
            'users.getloggedinuser' => 1,
            'users.getstandardinfo' => 1,
            'users.hasapppermission' => 1,
            'users.isappuser' => 1,
            'users.isverified' => 1,
            'video.getuploadlimits' => 1);
        $name = 'api';
        if (isset($READ_ONLY_CALLS[strtolower($method)])) {
            $name = 'api_read';
        } elseif (strtolower($method) == 'video.upload') {
            $name = 'api_video';
        }
        return self::getUrl($name, 'restserver.php');
    }

    /**
     * Build the URL for given domain alias, path and parameters.
     *
     * @param string $name   The name of the domain
     * @param string $path   Optional path (without a leading slash)
     * @param array  $params Optional query parameters
     *
     * @return string The URL for the given parameters
     */
    protected function getUrl($name, $path='', $params=array())
    {
        $url = self::$DOMAIN_MAP[$name];
        if ($path) {
            if ($path[0] === '/') {
                $path = substr($path, 1);
            }
            $url .= $path;
        }
        if ($params) {
            $url .= '?' . http_build_query($params, null, '&');
        }

        return $url;
    }

    /**
     * Returns the HTTP Host
     *
     * @return string The HTTP Host
     */
    protected function getHttpHost()
    {
        if ($this->trustForwarded && isset($_SERVER['HTTP_X_FORWARDED_HOST'])) {
            $forwardProxies = explode(',', $_SERVER['HTTP_X_FORWARDED_HOST']);
            if (!empty($forwardProxies)) {
                return $forwardProxies[0];
            }
        }
        return $_SERVER['HTTP_HOST'];
    }

    /**
     * Returns the HTTP Protocol
     *
     * @return string The HTTP Protocol
     */
    protected function getHttpProtocol()
    {
        if ($this->trustForwarded && isset($_SERVER['HTTP_X_FORWARDED_PROTO'])) {
            if ($_SERVER['HTTP_X_FORWARDED_PROTO'] === 'https') {
                return 'https';
            }
            return 'http';
        }
        /*apache + variants specific way of checking for https*/
        if (isset($_SERVER['HTTPS']) &&
        ($_SERVER['HTTPS'] === 'on' || $_SERVER['HTTPS'] == 1)) {
            return 'https';
        }
        /*nginx way of checking for https*/
        if (isset($_SERVER['SERVER_PORT']) &&
        ($_SERVER['SERVER_PORT'] === '443')) {
            return 'https';
        }
        return 'http';
    }

    /**
     * Returns the base domain used for the cookie.
     *
     * @return string The base domain
     */
    protected function getBaseDomain()
    {
        // The base domain is stored in the metadata cookie if not we fallback
        // to the current hostname
        $metadata = $this->getMetadataCookie();
        if (array_key_exists('base_domain', $metadata) &&
        !empty($metadata['base_domain'])) {
            return trim($metadata['base_domain'], '.');
        }
        return $this->getHttpHost();
    }

    /**
     * Returns the Current URL, stripping it of known FB parameters that should
     * not persist.
     *
     * @return string The current URL
     */
    protected function getCurrentUrl()
    {
        $protocol = $this->getHttpProtocol() . '://';
        $host = $this->getHttpHost();
        $currentUrl = $protocol.$host.$_SERVER['REQUEST_URI'];
        $parts = parse_url($currentUrl);

        $query = '';
        if (!empty($parts['query'])) {
            // drop known fb params
            $params = explode('&', $parts['query']);
            $retained_params = array();
            foreach ($params as $param) {
                if ($this->shouldRetainParam($param)) {
                    $retained_params[] = $param;
                }
            }

            if (!empty($retained_params)) {
                $query = '?'.implode('&', $retained_params);
            }
        }

        // use port if non default
        $port =
      isset($parts['port']) &&
      (($protocol === 'http://' && $parts['port'] !== 80) ||
       ($protocol === 'https://' && $parts['port'] !== 443))
      ? ':' . $parts['port'] : '';

        // rebuild
        return $protocol . $parts['host'] . $port . $parts['path'] . $query;
    }

    /**
     * Returns true if and only if the key or key/value pair should
     * be retained as part of the query string.  This amounts to
     * a brute-force search of the very small list of Facebook-specific
     * params that should be stripped out.
     *
     * @param string $param A key or key/value pair within a URL's query (e.g.
     *                      'foo=a', 'foo=', or 'foo'.
     *
     * @return boolean
     */
    protected function shouldRetainParam($param)
    {
        foreach (self::$DROP_QUERY_PARAMS as $drop_query_param) {
            if ($param === $drop_query_param ||
          strpos($param, $drop_query_param.'=') === 0) {
                return false;
            }
        }

        return true;
    }

    /**
     * Analyzes the supplied result to see if it was thrown
     * because the access token is no longer valid.  If that is
     * the case, then we destroy the session.
     *
     * @param array $result A record storing the error message returned
     *                      by a failed API call.
     */
    protected function throwAPIException($result)
    {
        $e = new FacebookApiException($result);
        switch ($e->getType()) {
      // OAuth 2.0 Draft 00 style
      case 'OAuthException':
        // OAuth 2.0 Draft 10 style
      case 'invalid_token':
        // REST server errors are just Exceptions
      case 'Exception':
        $message = $e->getMessage();
        if ((strpos($message, 'Error validating access token') !== false) ||
            (strpos($message, 'Invalid OAuth access token') !== false) ||
            (strpos($message, 'An active access token must be used') !== false)
        ) {
            $this->destroySession();
        }
        break;
    }

        throw $e;
    }


    /**
     * Prints to the error log if you aren't in command line mode.
     *
     * @param string $msg Log message
     */
    protected static function errorLog($msg)
    {
        // disable error log if we are running in a CLI environment
        // @codeCoverageIgnoreStart
        if (php_sapi_name() != 'cli') {
            error_log($msg);
        }
        // uncomment this if you want to see the errors on the page
    // print 'error_log: '.$msg."\n";
    // @codeCoverageIgnoreEnd
    }

    /**
     * Base64 encoding that doesn't need to be urlencode()ed.
     * Exactly the same as base64_encode except it uses
     *   - instead of +
     *   _ instead of /
     *   No padded =
     *
     * @param string $input base64UrlEncoded input
     *
     * @return string The decoded string
     */
    protected static function base64UrlDecode($input)
    {
        return base64_decode(strtr($input, '-_', '+/'));
    }

    /**
     * Base64 encoding that doesn't need to be urlencode()ed.
     * Exactly the same as base64_encode except it uses
     *   - instead of +
     *   _ instead of /
     *
     * @param string $input The input to encode
     * @return string The base64Url encoded input, as a string.
     */
    protected static function base64UrlEncode($input)
    {
        $str = strtr(base64_encode($input), '+/', '-_');
        $str = str_replace('=', '', $str);
        return $str;
    }

    /**
     * Destroy the current session
     */
    public function destroySession()
    {
        $this->accessToken = null;
        $this->signedRequest = null;
        $this->user = null;
        $this->clearAllPersistentData();

        // Javascript sets a cookie that will be used in getSignedRequest that we
        // need to clear if we can
        $cookie_name = $this->getSignedRequestCookieName();
        if (array_key_exists($cookie_name, $_COOKIE)) {
            unset($_COOKIE[$cookie_name]);
            if (!headers_sent()) {
                $base_domain = $this->getBaseDomain();
                SugarApplication::setCookie($cookie_name, '', 1, '/', '.'.$base_domain, false, true);
            } else {
                // @codeCoverageIgnoreStart
                self::errorLog(
                    'There exists a cookie that we wanted to clear that we couldn\'t '.
          'clear because headers was already sent. Make sure to do the first '.
          'API call before outputing anything.'
        );
                // @codeCoverageIgnoreEnd
            }
        }
    }

    /**
     * Parses the metadata cookie that our Javascript API set
     *
     * @return array an array mapping key to value
     */
    protected function getMetadataCookie()
    {
        $cookie_name = $this->getMetadataCookieName();
        if (!array_key_exists($cookie_name, $_COOKIE)) {
            return array();
        }

        // The cookie value can be wrapped in "-characters so remove them
        $cookie_value = trim($_COOKIE[$cookie_name], '"');

        if (empty($cookie_value)) {
            return array();
        }

        $parts = explode('&', $cookie_value);
        $metadata = array();
        foreach ($parts as $part) {
            $pair = explode('=', $part, 2);
            if (!empty($pair[0])) {
                $metadata[urldecode($pair[0])] =
          (count($pair) > 1) ? urldecode($pair[1]) : '';
            }
        }

        return $metadata;
    }

    /**
     * Finds whether the given domain is allowed or not
     *
     * @param string $big   The value to be checked against $small
     * @param string $small The input string
     *
     * @return boolean Returns TRUE if $big matches $small
     */
    protected static function isAllowedDomain($big, $small)
    {
        if ($big === $small) {
            return true;
        }
        return self::endsWith($big, '.'.$small);
    }

    /**
     * Checks if $big string ends with $small string
     *
     * @param string $big   The value to be checked against $small
     * @param string $small The input string
     *
     * @return boolean TRUE if $big ends with $small
     */
    protected static function endsWith($big, $small)
    {
        $len = strlen($small);
        if ($len === 0) {
            return true;
        }
        return substr($big, -$len) === $small;
    }

    /**
     * Each of the following four methods should be overridden in
     * a concrete subclass, as they are in the provided Facebook class.
     * The Facebook class uses PHP sessions to provide a primitive
     * persistent store, but another subclass--one that you implement--
     * might use a database, memcache, or an in-memory cache.
     *
     * @see Facebook
     */

    /**
     * Stores the given ($key, $value) pair, so that future calls to
     * getPersistentData($key) return $value. This call may be in another request.
     *
     * @param string $key
     * @param array $value
     *
     * @return void
     */
    abstract protected function setPersistentData($key, $value);

    /**
     * Get the data for $key, persisted by BaseFacebook::setPersistentData()
     *
     * @param string $key The key of the data to retrieve
     * @param boolean $default The default value to return if $key is not found
     *
     * @return mixed
     */
    abstract protected function getPersistentData($key, $default = false);

    /**
     * Clear the data with $key from the persistent storage
     *
     * @param string $key
     *
     * @return void
     */
    abstract protected function clearPersistentData($key);

    /**
     * Clear all data from the persistent storage
     *
     * @return void
     */
    abstract protected function clearAllPersistentData();
}
