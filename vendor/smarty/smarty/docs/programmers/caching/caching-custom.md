Custom Cache Implementation {#caching.custom}
===========================

As an alternative to using the default file-based caching mechanism, you
can specify a custom cache implementation that will be used to read,
write and clear cached files.

> **Note**
>
> In Smarty2 this used to be a callback function called
> `$cache_handler_func`. Smarty3 replaced this callback by the
> `Smarty_CacheResource` module.

With a custom cache implementation you\'re likely trying to achieve at
least one of the following goals: replace the slow filesystem by a
faster storage engine, centralize the cache to be accessible to multiple
servers.

Smarty allows CacheResource implementations to use one of the APIs
`Smarty_CacheResource_Custom` or `Smarty_CacheResource_KeyValueStore`.
`Smarty_CacheResource_Custom` is a simple API directing all read, write,
clear calls to your implementation. This API allows you to store
wherever and however you deem fit. The
`Smarty_CacheResource_KeyValueStore` API allows you to turn any \"dumb\"
KeyValue-Store (like APC, Memcache, ...) into a full-featured
CacheResource implementation. That is, everything around deep
cache-groups like \"a\|b\|c\" is being handled for you in way that
allows clearing the cache-group \"a\" and all nested groups are cleared
as well - even though KeyValue-Stores don\'t allow this kind of
hierarchy by nature.

Custom CacheResources may be put in a file `cacheresource.foobarxyz.php`
within your [`$plugins_dir`](#variable.plugins.dir), or registered on
runtime with [`registerCacheResource()`](#api.register.cacheresource).
In either case you need to set [`$caching_type`](#variable.caching.type)
to invoke your custom CacheResource implementation.


    <?php

    require_once 'libs/Smarty.class.php';
    $smarty = new Smarty();
    $smarty->caching_type = 'mysql';

    /**
     * MySQL CacheResource
     *
     * CacheResource Implementation based on the Custom API to use
     * MySQL as the storage resource for Smarty's output caching.
     *
     * Table definition:
     * <pre>CREATE TABLE IF NOT EXISTS `output_cache` (
     *   `id` CHAR(40) NOT NULL COMMENT 'sha1 hash',
     *   `name` VARCHAR(250) NOT NULL,
     *   `cache_id` VARCHAR(250) NULL DEFAULT NULL,
     *   `compile_id` VARCHAR(250) NULL DEFAULT NULL,
     *   `modified` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
     *   `content` LONGTEXT NOT NULL,
     *   PRIMARY KEY (`id`),
     *   INDEX(`name`),
     *   INDEX(`cache_id`),
     *   INDEX(`compile_id`),
     *   INDEX(`modified`)
     * ) ENGINE = InnoDB;</pre>
     *
     * @package CacheResource-examples
     * @author Rodney Rehm
     */
    class Smarty_CacheResource_Mysql extends Smarty_CacheResource_Custom {
        // PDO instance
        protected $db;
        protected $fetch;
        protected $fetchTimestamp;
        protected $save;
        
        public function __construct() {
            try {
                $this->db = new PDO("mysql:dbname=test;host=127.0.0.1", "smarty", "smarty");
            } catch (PDOException $e) {
                throw new SmartyException('Mysql Resource failed: ' . $e->getMessage());
            }
            $this->fetch = $this->db->prepare('SELECT modified, content FROM output_cache WHERE id = :id');
            $this->fetchTimestamp = $this->db->prepare('SELECT modified FROM output_cache WHERE id = :id');
            $this->save = $this->db->prepare('REPLACE INTO output_cache (id, name, cache_id, compile_id, content)
                VALUES  (:id, :name, :cache_id, :compile_id, :content)');
        }

        /**
         * fetch cached content and its modification time from data source
         *
         * @param string $id unique cache content identifier
         * @param string $name template name
         * @param string $cache_id cache id
         * @param string $compile_id compile id
         * @param string $content cached content
         * @param integer $mtime cache modification timestamp (epoch)
         * @return void
         */
        protected function fetch($id, $name, $cache_id, $compile_id, &$content, &$mtime)
        {
            $this->fetch->execute(array('id' => $id));
            $row = $this->fetch->fetch();
            $this->fetch->closeCursor();        
            if ($row) {
                $content = $row['content'];
                $mtime = strtotime($row['modified']);
            } else {
                $content = null;
                $mtime = null;
            }
        }
        
        /**
         * Fetch cached content's modification timestamp from data source
         *
         * @note implementing this method is optional. Only implement it if modification times can be accessed faster than loading the complete cached content.
         * @param string $id unique cache content identifier
         * @param string $name template name
         * @param string $cache_id cache id
         * @param string $compile_id compile id
         * @return integer|boolean timestamp (epoch) the template was modified, or false if not found
         */
        protected function fetchTimestamp($id, $name, $cache_id, $compile_id)
        {
            $this->fetchTimestamp->execute(array('id' => $id));
            $mtime = strtotime($this->fetchTimestamp->fetchColumn());
            $this->fetchTimestamp->closeCursor();
            return $mtime;
        }
        
        /**
         * Save content to cache
         *
         * @param string $id unique cache content identifier
         * @param string $name template name
         * @param string $cache_id cache id
         * @param string $compile_id compile id
         * @param integer|null $exp_time seconds till expiration time in seconds or null
         * @param string $content content to cache
         * @return boolean success
         */
        protected function save($id, $name, $cache_id, $compile_id, $exp_time, $content)
        {
            $this->save->execute(array(
                'id' => $id,
                'name' => $name,
                'cache_id' => $cache_id,
                'compile_id' => $compile_id,
                'content' => $content,
            ));
            return !!$this->save->rowCount();
        }
        
        /**
         * Delete content from cache
         *
         * @param string $name template name
         * @param string $cache_id cache id
         * @param string $compile_id compile id
         * @param integer|null $exp_time seconds till expiration or null
         * @return integer number of deleted caches
         */
        protected function delete($name, $cache_id, $compile_id, $exp_time)
        {
            // delete the whole cache
            if ($name === null && $cache_id === null && $compile_id === null && $exp_time === null) {
                // returning the number of deleted caches would require a second query to count them
                $query = $this->db->query('TRUNCATE TABLE output_cache');
                return -1;
            }
            // build the filter
            $where = array();
            // equal test name
            if ($name !== null) {
                $where[] = 'name = ' . $this->db->quote($name);
            }
            // equal test compile_id
            if ($compile_id !== null) {
                $where[] = 'compile_id = ' . $this->db->quote($compile_id);
            }
            // range test expiration time
            if ($exp_time !== null) {
                $where[] = 'modified < DATE_SUB(NOW(), INTERVAL ' . intval($exp_time) . ' SECOND)';
            }
            // equal test cache_id and match sub-groups
            if ($cache_id !== null) {
                $where[] = '(cache_id = '. $this->db->quote($cache_id)
                    . ' OR cache_id LIKE '. $this->db->quote($cache_id .'|%') .')';
            }
            // run delete query
            $query = $this->db->query('DELETE FROM output_cache WHERE ' . join(' AND ', $where));
            return $query->rowCount();
        }
    }

       


    <?php

    require_once 'libs/Smarty.class.php';
    $smarty = new Smarty();
    $smarty->caching_type = 'memcache';

    /**
     * Memcache CacheResource
     *
     * CacheResource Implementation based on the KeyValueStore API to use
     * memcache as the storage resource for Smarty's output caching.
     *
     * Note that memcache has a limitation of 256 characters per cache-key.
     * To avoid complications all cache-keys are translated to a sha1 hash.
     *
     * @package CacheResource-examples
     * @author Rodney Rehm
     */
    class Smarty_CacheResource_Memcache extends Smarty_CacheResource_KeyValueStore {
        /**
         * memcache instance
         * @var Memcache
         */
        protected $memcache = null;
        
        public function __construct()
        {
            $this->memcache = new Memcache();
            $this->memcache->addServer( '127.0.0.1', 11211 );
        }
        
        /**
         * Read values for a set of keys from cache
         *
         * @param array $keys list of keys to fetch
         * @return array list of values with the given keys used as indexes
         * @return boolean true on success, false on failure
         */
        protected function read(array $keys)
        {
            $_keys = $lookup = array();
            foreach ($keys as $k) {
                $_k = sha1($k);
                $_keys[] = $_k;
                $lookup[$_k] = $k;
            }
            $_res = array();
            $res = $this->memcache->get($_keys);
            foreach ($res as $k => $v) {
                $_res[$lookup[$k]] = $v;
            }
            return $_res;
        }
        
        /**
         * Save values for a set of keys to cache
         *
         * @param array $keys list of values to save
         * @param int $expire expiration time
         * @return boolean true on success, false on failure
         */
        protected function write(array $keys, $expire=null)
        {
            foreach ($keys as $k => $v) {
                $k = sha1($k);
                $this->memcache->set($k, $v, 0, $expire);
            }
            return true;
        }

        /**
         * Remove values from cache
         *
         * @param array $keys list of keys to delete
         * @return boolean true on success, false on failure
         */
        protected function delete(array $keys)
        {
            foreach ($keys as $k) {
                $k = sha1($k);
                $this->memcache->delete($k);
            }
            return true;
        }

        /**
         * Remove *all* values from cache
         *
         * @return boolean true on success, false on failure
         */
        protected function purge()
        {
            return $this->memcache->flush();
        }
    }


       
