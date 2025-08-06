<?php

/*

Modification information for LGPL compliance

r57813 - 2010-08-19 10:34:44 -0700 (Thu, 19 Aug 2010) - kjing - Author: John Mertic <jmertic@sugarcrm.com>
    Bug 39085 - When loading the opposite search panel via ajax on the ListViews, call the index action instead of the ListView action to avoid touching pre-MVC code by accident.

r56990 - 2010-06-16 13:05:36 -0700 (Wed, 16 Jun 2010) - kjing - snapshot "Mango" svn branch to a new one for GitHub sync

r56989 - 2010-06-16 13:01:33 -0700 (Wed, 16 Jun 2010) - kjing - defunt "Mango" svn dev branch before github cutover

r55980 - 2010-04-19 13:31:28 -0700 (Mon, 19 Apr 2010) - kjing - create Mango (6.1) based on windex

r51719 - 2009-10-22 10:18:00 -0700 (Thu, 22 Oct 2009) - mitani - Converted to Build 3  tags and updated the build system

r51634 - 2009-10-19 13:32:22 -0700 (Mon, 19 Oct 2009) - mitani - Windex is the branch for Sugar Sales 1.0 development

r50375 - 2009-08-24 18:07:43 -0700 (Mon, 24 Aug 2009) - dwong - branch kobe2 from tokyo r50372

r42807 - 2008-12-29 11:16:59 -0800 (Mon, 29 Dec 2008) - dwong - Branch from trunk/sugarcrm r42806 to branches/tokyo/sugarcrm

r13782 - 2006-06-06 10:58:55 -0700 (Tue, 06 Jun 2006) - majed - changes entry point code

r11115 - 2006-01-17 14:54:45 -0800 (Tue, 17 Jan 2006) - majed - add entry point validation

r8846 - 2005-10-31 11:01:12 -0800 (Mon, 31 Oct 2005) - majed - new version of nusoap

r5462 - 2005-05-25 13:50:11 -0700 (Wed, 25 May 2005) - majed - upgraded nusoap to .6.9

r573 - 2004-09-04 13:03:32 -0700 (Sat, 04 Sep 2004) - sugarclint - undoing copyrights added in inadvertantly.  --clint

r546 - 2004-09-03 11:49:38 -0700 (Fri, 03 Sep 2004) - sugarmsi - removed echo count

r354 - 2004-08-02 23:00:37 -0700 (Mon, 02 Aug 2004) - sugarjacob - Adding Soap


*/


if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

/*
The NuSOAP project home is:
http://sourceforge.net/projects/nusoap/

The primary support for NuSOAP is the mailing list:
nusoap-general@lists.sourceforge.net
*/

/**
* caches instances of the wsdl class
*
* @author   Scott Nichol <snichol@users.sourceforge.net>
* @author	Ingo Fischer <ingo@apollon.de>

* @access public
*/
class nusoap_wsdlcache
{
    /**
     *	@var resource
     *	@access private
     */
    public $fplock;
    /**
     *	@var integer
     *	@access private
     */
    public $cache_lifetime;
    /**
     *	@var string
     *	@access private
     */
    public $cache_dir;
    /**
     *	@var string
     *	@access public
     */
    public $debug_str = '';

	/**
	* constructor
	*
	* @param string $cache_dir directory for cache-files
	* @param integer $cache_lifetime lifetime for caching-files in seconds or 0 for unlimited
	* @access public
	*/
	function __construct($cache_dir='.', $cache_lifetime=0) {
		$this->fplock = array();
		$this->cache_dir = $cache_dir != '' ? $cache_dir : '.';
		$this->cache_lifetime = $cache_lifetime;
	}

    /**
    * creates the filename used to cache a wsdl instance
    *
    * @param string $wsdl The URL of the wsdl instance
    * @return string The filename used to cache the instance
    * @access private
    */
    public function createFilename($wsdl)
    {
        return $this->cache_dir.'/wsdlcache-' . md5($wsdl);
    }

    /**
    * adds debug data to the class level debug string
    *
    * @param    string $string debug data
    * @access   private
    */
    public function debug($string)
    {
        $this->debug_str .= get_class($this).": $string\n";
    }

    /**
    * gets a wsdl instance from the cache
    *
    * @param string $wsdl The URL of the wsdl instance
    * @return object wsdl The cached wsdl instance, null if the instance is not in the cache
    * @access public
    */
    public function get($wsdl)
    {
        $filename = $this->createFilename($wsdl);
        if ($this->obtainMutex($filename, "r")) {
            // check for expired WSDL that must be removed from the cache
            if ($this->cache_lifetime > 0) {
                if (file_exists($filename) && (time() - filemtime($filename) > $this->cache_lifetime)) {
                    unlink($filename);
                    $this->debug("Expired $wsdl ($filename) from cache");
                    $this->releaseMutex($filename);
                    return null;
                }
            }
            // see what there is to return
            if (!file_exists($filename)) {
                $this->debug("$wsdl ($filename) not in cache (1)");
                $this->releaseMutex($filename);
                return null;
            }
            $fp = @fopen($filename, 'rb');
            if ($fp) {
                $s = implode("", @file($filename));
                fclose($fp);
                $this->debug("Got $wsdl ($filename) from cache");
            } else {
                $s = null;
                $this->debug("$wsdl ($filename) not in cache (2)");
            }
            $this->releaseMutex($filename);
            return (!is_null($s)) ? unserialize($s, ['allowed_classes' => false]) : null;
        }
        $this->debug("Unable to obtain mutex for $filename in get");

        return null;
    }

    /**
    * obtains the local mutex
    *
    * @param string $filename The Filename of the Cache to lock
    * @param string $mode The open-mode ("r" or "w") or the file - affects lock-mode
    * @return boolean Lock successfully obtained ?!
    * @access private
    */
    public function obtainMutex($filename, $mode)
    {
        if (isset($this->fplock[md5($filename)])) {
            $this->debug("Lock for $filename already exists");
            return false;
        }
        $this->fplock[md5($filename)] = fopen($filename.".lock", 'wb');
        if ($mode == "r") {
            return flock($this->fplock[md5($filename)], LOCK_SH);
        }
        return flock($this->fplock[md5($filename)], LOCK_EX);
    }

    /**
    * adds a wsdl instance to the cache
    *
    * @param object wsdl $wsdl_instance The wsdl instance to add
    * @return boolean WSDL successfully cached
    * @access public
    */
    public function put($wsdl_instance)
    {
        $filename = $this->createFilename($wsdl_instance->wsdl);
        $s = serialize($wsdl_instance);
        if ($this->obtainMutex($filename, "w")) {
            $fp = fopen($filename, 'wb');
            if (! $fp) {
                $this->debug("Cannot write $wsdl_instance->wsdl ($filename) in cache");
                $this->releaseMutex($filename);
                return false;
            }
            fwrite($fp, $s);
            fclose($fp);
            $this->debug("Put $wsdl_instance->wsdl ($filename) in cache");
            $this->releaseMutex($filename);
            return true;
        }
        $this->debug("Unable to obtain mutex for $filename in put");

        return false;
    }

    /**
    * releases the local mutex
    *
    * @param string $filename The Filename of the Cache to lock
    * @return boolean Lock successfully released
    * @access private
    */
    public function releaseMutex($filename)
    {
        $ret = flock($this->fplock[md5($filename)], LOCK_UN);
        fclose($this->fplock[md5($filename)]);
        unset($this->fplock[md5($filename)]);
        if (! $ret) {
            $this->debug("Not able to release lock for $filename");
        }
        return $ret;
    }

    /**
    * removes a wsdl instance from the cache
    *
    * @param string $wsdl The URL of the wsdl instance
    * @return boolean Whether there was an instance to remove
    * @access public
    */
    public function remove($wsdl)
    {
        $filename = $this->createFilename($wsdl);
        if (!file_exists($filename)) {
            $this->debug("$wsdl ($filename) not in cache to be removed");
            return false;
        }
        // ignore errors obtaining mutex
        $this->obtainMutex($filename, "w");
        $ret = unlink($filename);
        $this->debug("Removed ($ret) $wsdl ($filename) from cache");
        $this->releaseMutex($filename);
        return $ret;
    }
}

/**
 * For backward compatibility
 */
class wsdlcache extends nusoap_wsdlcache
{
}
