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


require_once('include/SugarCache/SugarCacheAbstract.php');

/**
 * Redis SugarCache backend, using the PHP Redis C library at http://github.com/nicolasff/phpredis
 */
class SugarCacheRedis extends SugarCacheAbstract
{
    /**
     * @var Redis server name string
     */
    protected $_host = 'localhost';
    
    /**
     * @var Redis server port int
     */
    protected $_port = 6379;
    
    /**
     * @var Redis object
     */
    protected $_redis = null;
    
    /**
     * @see SugarCacheAbstract::$_priority
     */
    protected $_priority = 920;
    
    /**
     * @see SugarCacheAbstract::useBackend()
     */
    public function useBackend()
    {
        if (!parent::useBackend()) {
            return false;
        }
        
        if (extension_loaded("redis")
                && empty($GLOBALS['sugar_config']['external_cache_disabled_redis'])
                && $this->_getRedisObject()) {
            return true;
        }
            
        return false;
    }
    
    /**
     * @see SugarCacheAbstract::__construct()
     */
    public function __construct()
    {
        parent::__construct();
    }
    
    /**
     * Get the memcache object; initialize if needed
     */
    protected function _getRedisObject()
    {
        try {
            if (!($this->_redis instanceof Redis)) {
                $this->_redis = new Redis();
                $this->_host = SugarConfig::getInstance()->get('external_cache.redis.host', $this->_host);
                $this->_port = SugarConfig::getInstance()->get('external_cache.redis.port', $this->_port);
                if (!$this->_redis->connect($this->_host, $this->_port)) {
                    return false;
                }
            }
        } catch (RedisException $e) {
            return false;
        }
        
        return $this->_redis;
    }
    
    /**
     * @see SugarCacheAbstract::_setExternal()
     */
    protected function _setExternal(
        $key,
        $value
        ) {
        $value = serialize($value);
        $key = $this->_fixKeyName($key);
        
        $this->_getRedisObject()->set($key, $value);
        $this->_getRedisObject()->expire($key, $this->_expireTimeout);
    }
    
    /**
     * @see SugarCacheAbstract::_getExternal()
     */
    protected function _getExternal(
        $key
        ) {
        $key = $this->_fixKeyName($key);
        $returnValue = $this->_getRedisObject()->get($key);
        // return null if we don't get a cache hit
        if ($returnValue === false) {
            return null;
        }
        
        return is_string($returnValue) ?
            unserialize($returnValue, ['allowed_classes' => false]) :
            $returnValue;
    }
    
    /**
     * @see SugarCacheAbstract::_clearExternal()
     */
    protected function _clearExternal(
        $key
        ) {
        $key = $this->_fixKeyName($key);
        $this->_getRedisObject()->del($key);
    }
    
    /**
     * @see SugarCacheAbstract::_resetExternal()
     */
    protected function _resetExternal()
    {
        $this->_getRedisObject()->flushAll();
    }
    
    /**
     * Fixed the key naming used so we don't have any spaces
     *
     * @param  string $key
     * @return string fixed key name
     */
    protected function _fixKeyName($key)
    {
        return str_replace(' ', '_', $key);
    }
}
