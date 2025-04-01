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

require_once 'modules/OAuthKeys/OAuthKey.php';

/**
 * OAuth token
 */
#[\AllowDynamicProperties]
class OAuthToken extends SugarBean
{
    public $module_dir = 'OAuthTokens';
    public $object_name = 'OAuthToken';
    public $table_name = 'oauth_tokens';
    public $disable_row_level_security = true;

    public $token;
    public $secret;
    public $tstate;
    public $token_ts;
    public $verify;
    public $consumer;
    public $assigned_user_id;
    public $consumer_obj;
    public $callback_url;
    // authdata is not preserved so far since we don't have any useful data yet
    // so it's an extension point for the future
    public $authdata;

    public const REQUEST = 1;
    public const ACCESS = 2;
    public const INVALID = 3;

    public function __construct($token='', $secret='')
    {
        parent::__construct();
        $this->token = $token;
        $this->secret = $secret;
        $this->setState(self::REQUEST);
    }

    /**
     * Set token state
     * @param int $s
     * @return OAuthToken
     */
    public function setState($s)
    {
        $this->tstate = $s;
        return $this;
    }

    /**
     * Associate the token with the consumer key
     * @param OAuthKey $consumer
     * @return OAuthToken
     */
    public function setConsumer($consumer)
    {
        $this->consumer = $consumer->id;
        $this->consumer_obj = $consumer;
        return $this;
    }

    /**
     * Set callback URL for request token
     * @param string $url
     * @return OAuthToken
     */
    public function setCallbackURL($url)
    {
        $this->callback_url = $url;
        return $this;
    }

    /**
     * Generate random token
     * @return string
     */
    protected static function randomValue()
    {
        $zop = new SuiteCRM\Zend_Oauth_Provider();
        return bin2hex($zop->generateToken(6));
    }

    /**
     * Generate random token/secret pair and create token
     * @return OAuthToken
     */
    public static function generate()
    {
        $t = self::randomValue();
        $s = self::randomValue();
        return new self($t, $s);
    }

    public function save($check_notify = false)
    {
        $this->token_ts = time();
        if (!isset($this->id)) {
            $this->new_with_id = true;
            $this->id = $this->token;
        }
        return parent::save();
    }

    /**
     * Load token by ID
     * @param string $token
     * @return OAuthToken
     */
    public static function load($token)
    {
        $ltoken = new self();
        $ltoken->retrieve($token);
        if (empty($ltoken->id)) {
            return null;
        }
        $ltoken->token = $ltoken->id;
        if (!empty($ltoken->consumer)) {
            $ltoken->consumer_obj = BeanFactory::getBean("OAuthKeys", $ltoken->consumer);
            if (empty($ltoken->consumer_obj->id)) {
                return null;
            }
        }
        return $ltoken;
    }

    /**
     * Invalidate token
     */
    public function invalidate()
    {
        $this->setState(self::INVALID);
        $this->verify = false;
        return $this->save();
    }

    /**
     * Create a new authorized token for specific user
     * This bypasses normal OAuth process and creates a ready-made access token
     * @param OAuthKey $consumer
     * @param User $user
     * @return OAuthToken
     */
    public static function createAuthorized($consumer, $user)
    {
        $token = self::generate();
        $token->setConsumer($consumer);
        $token->setState(self::ACCESS);
        $token->assigned_user_id = $user->id;
        $token->save();
        return $token;
    }

    /**
     * Authorize request token
     * @param mixed $authdata
     * @return string Validation token
     */
    public function authorize($authdata)
    {
        if ($this->tstate != self::REQUEST) {
            return false;
        }
        $this->verify = self::randomValue();
        $this->authdata = $authdata;
        if (isset($authdata['user'])) {
            $this->assigned_user_id = $authdata['user'];
        }
        $this->save();
        return $this->verify;
    }

    /**
     * Copy auth data between tokens
     * @param OAuthToken $token
     * @return OAuthToken
     */
    public function copyAuthData(OAuthToken $token)
    {
        $this->authdata = $token->authdata;
        $this->assigned_user_id = $token->assigned_user_id;
        return $this;
    }

    /**
     * Get query string for the token
     */
    public function queryString()
    {
        return "oauth_token={$this->token}&oauth_token_secret={$this->secret}";
    }

    /**
     * Clean up stale tokens
     */
    public static function cleanup()
    {
        $db = DBManagerFactory::getInstance();
        // delete invalidated tokens older than 1 day
        $db->query("DELETE FROM oauth_tokens WHERE tstate = ".self::INVALID." AND token_ts < ".(time()-60*60*24));
        // delete request tokens older than 1 day
        $db->query("DELETE FROM oauth_tokens WHERE tstate = ".self::REQUEST." AND token_ts < ".(time()-60*60*24));
    }

    /**
     * Check if the nonce is valid
     * @param string $key
     * @param string $nonce
     * @param string $ts
     */
    public static function checkNonce($key, $nonce, $ts)
    {
        $db = DBManagerFactory::getInstance();

        $res = $db->query(sprintf("SELECT * FROM oauth_nonce WHERE conskey='%s' AND nonce_ts > %d", $db->quote($key), $ts));
        if ($res && $db->fetchByAssoc($res)) {
            // we have later ts
            return SuiteCRM\Zend_Oauth_Provider::BAD_TIMESTAMP;
        }

        $res = $db->query(sprintf("SELECT * FROM oauth_nonce WHERE conskey='%s' AND nonce='%s' AND nonce_ts = %d", $db->quote($key), $db->quote($nonce), $ts));
        if ($res && $db->fetchByAssoc($res)) {
            // Already seen this one
            return SuiteCRM\Zend_Oauth_Provider::BAD_NONCE;
        }
        $db->query(sprintf("DELETE FROM oauth_nonce WHERE conskey='%s' AND nonce_ts < %d", $db->quote($key), $ts));
        $db->query(sprintf("INSERT INTO oauth_nonce(conskey, nonce, nonce_ts) VALUES('%s', '%s', %d)", $db->quote($key), $db->quote($nonce), $ts));
        return SuiteCRM\Zend_Oauth_Provider::OK;
    }

    /**
     * Delete token by ID
     * @param string id
     * @see SugarBean::mark_deleted($id)
     */
    public function mark_deleted($id)
    {
        $this->db->query("DELETE from {$this->table_name} WHERE id='".$this->db->quote($id)."'");
    }

    /**
     * Delete tokens by consumer ID
     * @param string $user
     */
    public static function deleteByConsumer($consumer_id)
    {
        $db = DBManagerFactory::getInstance();
        $db->query("DELETE FROM oauth_tokens WHERE consumer='".$db->quote($consumer_id) ."'");
    }

    /**
     * Delete tokens by user ID
     * @param string $user
     */
    public static function deleteByUser($user_id)
    {
        $db = DBManagerFactory::getInstance();
        $db->query("DELETE FROM oauth_tokens WHERE assigned_user_id='".$db->quote($user_id) ."'");
    }
}

function displayDateFromTs($focus, $field, $value, $view='ListView')
{
    $field = strtoupper($field);
    if (!isset($focus->$field)) {
        return '';
    }
    global $timedate;
    return $timedate->asUser($timedate->fromTimestamp($focus->$field));
}
