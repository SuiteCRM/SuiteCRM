<?php
/* https://github.com/smalyshev/Zend_OAuth_Provider/blob/master/Zend/Oauth/Provider.php */

namespace SuiteCRM;

use Zend_Oauth_Exception;
use Zend_Oauth_Http_Utility;
use Zend_Uri_Http;

/**
 *
 * Basic OAuth provider class
 */
#[\AllowDynamicProperties]
class Zend_Oauth_Provider
{
    /**
     * OAuth result statuses
     */
    public const OK = 0;
    public const BAD_NONCE = 1;
    public const BAD_TIMESTAMP = 2;
    public const CONSUMER_KEY_UNKNOWN = 3;
    public const CONSUMER_KEY_REFUSED = 4;
    public const INVALID_SIGNATURE = 5;
    public const TOKEN_USED = 6;
    public const TOKEN_EXPIRED = 7;
    public const TOKEN_REVOKED = 8;
    public const TOKEN_REJECTED = 9;
    public const PARAMETER_ABSENT = 10;
    public const SIGNATURE_METHOD_REJECTED = 11;
    public const OAUTH_VERIFIER_INVALID = 12;

    /**
     * Error names for error reporting
     * @var array
     */
    protected $errnames = [
        self::BAD_NONCE => 'nonce_used',
        self::BAD_TIMESTAMP => 'timestamp_refused',
        self::CONSUMER_KEY_UNKNOWN => 'consumer_key_unknown',
        self::CONSUMER_KEY_REFUSED => 'consumer_key_refused',
        self::INVALID_SIGNATURE => 'signature_invalid',
        self::TOKEN_USED => 'token_used',
        self::TOKEN_EXPIRED => 'token_expired',
        self::TOKEN_REVOKED => 'token_revoked',
        self::TOKEN_REJECTED => 'token_rejected',
        self::PARAMETER_ABSENT => 'parameter_absent',
        self::SIGNATURE_METHOD_REJECTED => 'signature_method_rejected',
        self::OAUTH_VERIFIER_INVALID => 'verifier_invalid',
    ];

    public $token;
    public $token_secret;
    public $consumer_key;
    public $consumer_secret;
    public $verifier;
    public $callback;

    protected $problem;

    protected $tokenHandler;
    protected $consumerHandler;
    protected $nonceHandler;
    protected $oauth_params;

    protected $requestPath;
    /**
     * Current URL
     * @var Zend_Uri_Http
     */
    protected $url;
    /**
     *
     * Required OAuth parameters
     * @var array
     */
    protected $required = [
        'oauth_consumer_key',
        'oauth_signature',
        'oauth_signature_method',
        'oauth_nonce',
        'oauth_timestamp'
    ];

    /**
     * Set consumer key handler
     * @param string $callback
     * @return Zend_Oauth_Provider
     */
    public function setConsumerHandler($callback)
    {
        $this->consumerHandler = $callback;

        return $this;
    }

    /**
     * Set nonce/ts handler
     * @param string $callback
     * @return Zend_Oauth_Provider
     */
    public function setTimestampNonceHandler($callback)
    {
        $this->nonceHandler = $callback;

        return $this;
    }

    /**
     * Set token handler
     * @param string $callback
     * @return Zend_Oauth_Provider
     */
    public function setTokenHandler($callback)
    {
        $this->tokenHandler = $callback;

        return $this;
    }

    /**
     * Set URL for requesting token (doesn't need token)
     * @param string $req_path
     * @return Zend_Oauth_Provider
     */
    public function setRequestTokenPath($req_path)
    {
        $this->requestPath = $req_path;

        return $this;
    }

    /**
     * Set this request as token endpoint
     * @param string $request
     * @return Zend_Oauth_Provider
     */
    public function isRequestTokenEndpoint($request)
    {
        $this->is_request = $request;

        return $this;
    }

    /**
     * Report problem in OAuth as string
     * @param Zend_Oauth_Exception $e
     * @return string
     */
    public function reportProblem(Zend_Oauth_Exception $e)
    {
        $code = $e->getCode();
        if ($code == self::PARAMETER_ABSENT) {
            return "oauth_problem=parameter_absent&oauth_parameters_absent={$this->problem}";
        }
        if ($code == self::INVALID_SIGNATURE) {
            return "oauth_problem=signature_invalid&debug_sbs={$this->problem}";
        }
        if (isset($this->errnames[$code])) {
            return 'oauth_problem=' . $this->errnames[$code];
        }

        return "oauth_problem=unknown_problem&code=$code";
    }

    /**
     * Check if this request needs token
     * Requests made to requestPath do not need a token
     * @return bool
     * @throws \Zend_Uri_Exception
     */
    protected function needsToken()
    {
        if (!empty($this->is_request)) {
            return false;
        }
        if (empty($this->requestPath)) {
            return true;
        }
        if ($this->requestPath[0] == '/') {
            return $this->url->getPath() != $this->requestPath;
        }

        return $this->url->getUri() != $this->requestPath;
    }

    /**
     * Check if all required parameters are there
     * @param array $params
     * @return bool
     * @throws Zend_Oauth_Exception
     * @throws \Zend_Uri_Exception
     */
    protected function checkRequiredParams($params)
    {
        foreach ($this->required as $param) {
            if (!isset($params[$param])) {
                $this->problem = $param;
                throw new Zend_Oauth_Exception("Missing parameter: $param", self::PARAMETER_ABSENT);
            }
        }
        if ($this->needsToken() && !isset($params['oauth_token'])) {
            $this->problem = 'oauth_token';
            throw new Zend_Oauth_Exception('Missing parameter: oauth_token', self::PARAMETER_ABSENT);
        }

        return true;
    }

    /**
     * Check if signature method is supported
     * @param string $signatureMethod
     * @throws Zend_Oauth_Exception
     */
    protected function checkSignatureMethod($signatureMethod)
    {
        $className = '';
        $hashAlgo = null;
        $parts = explode('-', $signatureMethod);
        if (count($parts) > 1) {
            $className = 'Zend_Oauth_Signature_' . ucfirst(strtolower($parts[0]));
        } else {
            $className = 'Zend_Oauth_Signature_' . ucfirst(strtolower($signatureMethod));
        }
        $filename = str_replace('_', '/', $className) . '.php';
        if (file_exists($filename)) {
            require_once $filename;
        }
        if (!class_exists($className)) {
            throw new Zend_Oauth_Exception('Invalid signature method', self::SIGNATURE_METHOD_REJECTED);
        }
    }

    /**
     * Collect request parameters from the environment
     * FIXME: uses GET/POST/SERVER, needs to be made injectable instead
     * @param string $method HTTP method being used
     * @param array $params Extra parameters
     * @return array List of all oauth params in the request
     */
    protected function assembleParams($method, $params = array())
    {
        $params = array_merge($_GET, $params);
        if ($method == 'POST') {
            $params = array_merge($_POST, $params);
        }
        $auth = null;
        if (function_exists('apache_request_headers')) {
            $headers = apache_request_headers();
            if (isset($headers['Authorization'])) {
                $auth = $headers['Authorization'];
            } elseif (isset($headers['authorization'])) {
                $auth = $headers['authorization'];
            }
        }
        if (empty($auth) && !empty($_SERVER['HTTP_AUTHORIZATION'])) {
            $auth = $_SERVER['HTTP_AUTHORIZATION'];
        }

        // import header data
        if (!empty($auth) && substr((string) $auth, 0,
                6) == 'OAuth ' && preg_match_all('/(oauth_[a-z_-]*)=(:?"([^"]*)"|([^,]*))/', (string) $auth, $matches)) {
            foreach ($matches[1] as $num => $header) {
                if ($header == 'realm') {
                    continue;
                }
                $params[$header] = urldecode(empty($matches[3][$num]) ? $matches[4][$num] : $matches[3][$num]);
            }
        }

        return $params;
    }

    /**
     * Get full current request URL
     * @return string
     * @throws \Zend_Uri_Exception
     */
    protected function getRequestUrl()
    {
        $proto = "http";
        if (empty($_SERVER['SERVER_PORT']) || empty($_SERVER['HTTP_HOST']) || empty($_SERVER['REQUEST_URI'])) {
            return Zend_Uri_Http::fromString("http://localhost/");
        }
        if ($_SERVER['SERVER_PORT'] == 443 || (!empty($_SERVER['HTTPS']) && $_SERVER['HTTPS'] == 'on') || (!empty($_SERVER['HTTP_HTTPS']) && $_SERVER['HTTP_HTTPS'] == 'on')) {
            $proto = 'https';
        }

        return Zend_Uri_Http::fromString("$proto://{$_SERVER['HTTP_HOST']}{$_SERVER['REQUEST_URI']}");
    }

    /**
     * Returns oauth parameters
     * @return array
     */
    public function getOAuthParams()
    {
        return $this->oauth_params;
    }

    /**
     * Validate OAuth request
     * @param Zend_Uri_Http|null $url Request URL, will use current if null
     * @param array $params Additional parameters
     * @return bool
     * @throws Zend_Oauth_Exception
     * @throws \Zend_Uri_Exception
     */
    public function checkOAuthRequest(Zend_Uri_Http $url = null, $params = array())
    {
        if ($url === null) {
            $this->url = $this->getRequestUrl();
        } else {
            $this->url = clone $url;
        }
        // We'll ignore query for the purpose of URL matching
        $this->url->setQuery('');
        // FIXME: make it injectable
        if (isset($_SERVER['REQUEST_METHOD'])) {
            $method = $_SERVER['REQUEST_METHOD'];
        } elseif (isset($_SERVER['HTTP_METHOD'])) {
            $method = $_SERVER['HTTP_METHOD'];
        } else {
            $method = 'GET';
        }
        $params = $this->assembleParams($method, $params);
        $this->oauth_params = $params;
        $this->checkSignatureMethod($params['oauth_signature_method']);
        $this->checkRequiredParams($params);

        $this->timestamp = $params['oauth_timestamp'];
        $this->nonce = $params['oauth_nonce'];
        $this->consumer_key = $params['oauth_consumer_key'];

        if (isset($params['oauth_callback'])) {
            $this->callback = $params['oauth_callback'];
        }

        if (!is_callable($this->nonceHandler)) {
            throw new Zend_Oauth_Exception('Nonce handler not callable', self::BAD_NONCE);
        }

        $res = call_user_func($this->nonceHandler, $this);
        if ($res != self::OK) {
            throw new Zend_Oauth_Exception('Invalid request', $res);
        }

        if (!is_callable($this->consumerHandler)) {
            throw new Zend_Oauth_Exception('Consumer handler not callable', self::CONSUMER_KEY_UNKNOWN);
        }

        $res = call_user_func($this->consumerHandler, $this);
        // this will set $this->consumer_secret if OK
        if ($res != self::OK) {
            throw new Zend_Oauth_Exception('Consumer key invalid', $res);
        }

        if ($this->needsToken()) {
            $this->token = $params['oauth_token'];
            if (isset($params['oauth_verifier'])) {
                $this->verifier = $params['oauth_verifier'];
            }
            if (!is_callable($this->tokenHandler)) {
                throw new Zend_Oauth_Exception('Token handler not callable', self::TOKEN_REJECTED);
            }
            $res = call_user_func($this->tokenHandler, $this);
            // this will set $this->token_secret if OK
            if ($res != self::OK) {
                throw new Zend_Oauth_Exception('Token invalid', $res);
            }
        }

        $util = new Zend_Oauth_Http_Utility();
        $req_sign = $params['oauth_signature'];
        unset($params['oauth_signature']);
        $our_sign = $util->sign($params, $params['oauth_signature_method'], $this->consumer_secret,
            $this->token_secret, $method, $this->url->getUri());
        if ($req_sign != $our_sign) {
            // TODO: think how to extract signature base string
            $this->problem = $our_sign;
            throw new Zend_Oauth_Exception('Invalid signature', self::INVALID_SIGNATURE);
        }

        return true;
    }

    /**
     * Generate new token
     * @param int $size How many characters?
     * @return false|string
     */
    public function generateToken($size)
    {
        $str = '';
        while (strlen($str) < $size) {
            $str .= md5(uniqid(mt_rand(), true), true);
        }

        return substr($str, 0, $size);
    }
}
