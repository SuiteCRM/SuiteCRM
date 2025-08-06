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

class SugarCacheFile extends SugarCacheAbstract
{
    /**
     * @var path and file which will store the cache used for this backend
     */
    protected $_cacheFileName = 'externalCache.php';

    /**
     * @var bool true if the cache has changed and needs written to disk
     */
    protected $_cacheChanged = false;

    /**
     * @see SugarCacheAbstract::$_priority
     */
    protected $_priority = 990;

    /**
     * @see SugarCacheAbstract::useBackend()
     */
    public function useBackend()
    {
        if (!parent::useBackend()) {
            return false;
        }

        if (!empty($GLOBALS['sugar_config']['external_cache_enabled_file'])) {
            return true;
        }

        return false;
    }

    /**
     * @see SugarCacheAbstract::__construct()
     *
     * For this backend, we'll read from the SugarCacheFile::_cacheFileName file into
     * the SugarCacheFile::$localCache array.
     */
    public function __construct()
    {
        parent::__construct();

        if (isset($GLOBALS['sugar_config']['external_cache_filename'])) {
            $this->_cacheFileName = $GLOBALS['sugar_config']['external_cache_filename'];
        }
    }

    /**
     * @see SugarCacheAbstract::__destruct()
     *
     * For this backend, we'll write the SugarCacheFile::$localCache array serialized out to a file
     */
    public function __destruct()
    {
        parent::__destruct();

        if ($this->_cacheChanged) {
            sugar_file_put_contents(sugar_cached($this->_cacheFileName), serialize($this->_localStore));
        }
    }

    /**
     * This is needed to prevent unserialize vulnerability
     */
    public function __wakeup()
    {
        // clean all properties
        foreach (get_object_vars($this) as $k => $v) {
            $this->$k = null;
        }
        throw new Exception("Not a serializable object");
    }

    /**
     * @see SugarCacheAbstract::_setExternal()
     *
     * Does nothing; we write to cache on destroy
     */
    protected function _setExternal(
        $key,
        $value
        ) {
        $this->_cacheChanged = true;
    }

    /**
     * @see SugarCacheAbstract::_getExternal()
     */
    protected function _getExternal(
        $key
        ) {
        // load up the external cache file
        if (is_file($cachedfile = sugar_cached($this->_cacheFileName))) {
            $this->localCache = unserialize(file_get_contents($cachedfile), ['allowed_classes' => false]);
        }

        if (isset($this->_localStore[$key])) {
            return $this->_localStore[$key];
        }

        return null;
    }

    /**
     * @see SugarCacheAbstract::_clearExternal()
     *
     * Does nothing; we write to cache on destroy
     */
    protected function _clearExternal(
        $key
        ) {
        $this->_cacheChanged = true;
    }

    /**
     * @see SugarCacheAbstract::_resetExternal()
     *
     * Does nothing; we write to cache on destroy
     */
    protected function _resetExternal()
    {
        $this->_cacheChanged = true;
    }
}
