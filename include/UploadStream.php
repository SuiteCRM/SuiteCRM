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

require_once('include/externalAPI/ExternalAPIFactory.php');

/**
 * @internal
 * Upload file stream handler
 */
#[\AllowDynamicProperties]
class UploadStream
{
    public const STREAM_NAME = "upload";
    protected static $upload_dir;

    /**
     * Method checks Suhosin restrictions to use streams in php
     *
     * @static
     * @return bool is allowed stream or not
     */
    public static function getSuhosinStatus(): bool
    {
        // looks like suhosin patch doesn't block protocols, only suhosin extension (tested on FreeBSD)
        // if suhosin is not installed it is okay for us
        if (extension_loaded('suhosin') == false) {
            return true;
        }
        $configuration = ini_get_all('suhosin', false);

        // suhosin simulation is okay for us
        if ($configuration['suhosin.simulation'] == true) {
            return true;
        }

        // checking that UploadStream::STREAM_NAME is allowed by white list
        $streams = $configuration['suhosin.executor.include.whitelist'];
        if ($streams != '') {
            $streams = explode(',', $streams);
            foreach ($streams as $stream) {
                $stream = explode('://', $stream, 2);
                if (count($stream) == 1) {
                    if ($stream[0] == UploadStream::STREAM_NAME) {
                        return true;
                    }
                } elseif ($stream[1] == '' && $stream[0] == UploadStream::STREAM_NAME) {
                    return true;
                }
            }

            $GLOBALS['log']->fatal('Stream ' . UploadStream::STREAM_NAME . ' is not listed in suhosin.executor.include.whitelist and blocked because of it');

            return false;
        }

        // checking that UploadStream::STREAM_NAME is not blocked by black list
        $streams = $configuration['suhosin.executor.include.blacklist'];
        if ($streams != '') {
            $streams = explode(',', $streams);
            foreach ($streams as $stream) {
                $stream = explode('://', $stream, 2);
                if ($stream[0] == UploadStream::STREAM_NAME) {
                    $GLOBALS['log']->fatal('Stream ' . UploadStream::STREAM_NAME . 'is listed in suhosin.executor.include.blacklist and blocked because of it');

                    return false;
                }
            }

            return true;
        }

        $GLOBALS['log']->fatal('Suhosin blocks all streams, please define ' . UploadStream::STREAM_NAME . ' stream in suhosin.executor.include.whitelist');

        return false;
    }

    /**
     * Get upload directory
     * @return string
     */
    public static function getDir(): string
    {
        if (empty(self::$upload_dir)) {
            self::$upload_dir = rtrim($GLOBALS['sugar_config']['upload_dir'], '/\\');
            if (empty(self::$upload_dir)) {
                self::$upload_dir = "upload";
            }
            if (!file_exists(self::$upload_dir)) {
                sugar_mkdir(self::$upload_dir, 0755, true);
            }
        }

        return self::$upload_dir;
    }

    /**
     * Check if upload dir is writable
     * @return bool
     */
    public static function writable(): bool
    {
        return is_writable(self::getDir());
    }

    /**
     * Register the stream
     */
    public static function register() : void
    {
        stream_wrapper_register(self::STREAM_NAME, self::class);
    }

    /**
     * Get real FS path of the upload stream file
     * @param string $path Upload stream path (with upload://)
     * @return string FS path
     */
    public static function path($path): ?string
    {
        $path = substr($path, strlen(self::STREAM_NAME) + 3); // cut off upload://
        $path = str_replace("\\", "/", $path); // canonicalize path
        if ($path == ".." || substr($path, 0, 3) == "../" || substr($path, -3, 3) == "/.." || strstr($path, "/../")) {
            return null;
        }

        return self::getDir() . "/" . $path;
    }

    /**
     * Ensure upload subdir exists
     * @param string $path Upload stream path (with upload://)
     * @param bool $writable
     * @return boolean
     */
    public static function ensureDir($path, $writable = true): bool
    {
        $path = self::path($path);
        if (!is_dir($path)) {
            return sugar_mkdir($path, 0755, true);
        }

        return true;
    }
    public function dir_opendir($path, $options): bool
    {
        $this->dirp = opendir(self::path($path));

        return !empty($this->dirp);
    }

    public function dir_readdir()
    {
        return readdir($this->dirp);
    }

    public function dir_rewinddir()
    {
        rewinddir($this->dirp);
    }

    public function mkdir($path, $mode, $options): bool
    {
        return mkdir(self::path($path), $mode, ($options & STREAM_MKDIR_RECURSIVE) != 0);
    }

    public function rename($path_from, $path_to): bool
    {
        return rename(self::path($path_from), self::path($path_to));
    }

    public function rmdir($path, $options): bool
    {
        return rmdir(self::path($path));
    }

    public function stream_cast($cast_as)
    {
        return $this->fp;
    }

    public function stream_close(): bool
    {
        fclose($this->fp);

        return true;
    }

    public function stream_eof(): bool
    {
        return feof($this->fp);
    }

    public function stream_flush(): bool
    {
        return fflush($this->fp);
    }

    public function stream_lock($operation): bool
    {
        return flock($this->fp, $operation);
    }

    public function stream_open($path, $mode): bool
    {
        $fullpath = self::path($path);
        if (empty($fullpath)) {
            return false;
        }
        if ($mode == 'r') {
            $this->fp = fopen($fullpath, $mode);
        } else {
            // if we will be writing, try to transparently create the directory
            $this->fp = @fopen($fullpath, $mode);
            if (!$this->fp && !file_exists(dirname($fullpath))) {
                if (!mkdir($concurrentDirectory = dirname($fullpath), 0755, true) && !is_dir($concurrentDirectory)) {
                    throw new \RuntimeException(sprintf('Directory "%s" was not created', $concurrentDirectory));
                }
                $this->fp = fopen($fullpath, $mode);
            }
        }

        return !empty($this->fp);
    }

    public function stream_read($count)
    {
        return fread($this->fp, $count);
    }

    public function stream_seek($offset, $whence = SEEK_SET): bool
    {
        return fseek($this->fp, $offset, $whence) == 0;
    }

    public function stream_set_option($option, $arg1, $arg2): bool
    {
        return true;
    }

    public function stream_stat()
    {
        return fstat($this->fp);
    }

    public function stream_tell()
    {
        return ftell($this->fp);
    }

    public function stream_write($data)
    {
        return fwrite($this->fp, $data);
    }

    public function unlink($path): bool
    {
        unlink(self::path($path));

        return true;
    }

    public function url_stat($path, $flags)
    {
        return @stat(self::path($path));
    }

    /**
    * Sets metadata on the stream.
    *
    * WARNING: Do not call this method directly! It will be called internally by
    * PHP itself when one of the following functions is called on a stream URL:
    *
    * @param string $uri
    *   A string containing the URI to the file to set metadata on.
    * @param int $option
    *   One of:
    *   - STREAM_META_TOUCH: The method was called in response to touch().
    *   - STREAM_META_OWNER_NAME: The method was called in response to chown()
    *     with string parameter.
    *   - STREAM_META_OWNER: The method was called in response to chown().
    *   - STREAM_META_GROUP_NAME: The method was called in response to chgrp().
    *   - STREAM_META_GROUP: The method was called in response to chgrp().
    *   - STREAM_META_ACCESS: The method was called in response to chmod().
    * @param mixed $value
    *   If option is:
    *   - STREAM_META_TOUCH: Array consisting of two arguments of the touch()
    *     function.
    *   - STREAM_META_OWNER_NAME or STREAM_META_GROUP_NAME: The name of the owner
    *     user/group as string.
    *   - STREAM_META_OWNER or STREAM_META_GROUP: The value of the owner
    *     user/group as integer.
    *   - STREAM_META_ACCESS: The argument of the chmod() as integer.
    *
    * @return bool
    *   Returns TRUE on success or FALSE on failure. If $option is not
    *   implemented, FALSE should be returned.
    *
    * @see touch()
    * @see chmod()
    * @see chown()
    * @see chgrp()
    * @link http://php.net/manual/streamwrapper.stream-metadata.php
    * @author Chris001 <chris NOSPAM at espacenetworks dot com>
    */
    public function stream_metadata(string $uri, int $option, $value): bool
    {
        $target = self::path($uri);
        $return = FALSE;
        switch ($option) {
            case STREAM_META_TOUCH:
                if (!empty($value)) {
                    $return = touch($target, $value[0], $value[1]);
                }
                else {
                    $return = touch($target);
                }
                break;

            case STREAM_META_OWNER_NAME:
            case STREAM_META_OWNER:
                $return = chown($target, $value);
                break;

            case STREAM_META_GROUP_NAME:
            case STREAM_META_GROUP:
                $return = chgrp($target, $value);
                break;

            case STREAM_META_ACCESS:
                $return = chmod($target, $value);
                break;
        }
        if ($return) {
            // For convenience clear the file status cache of the underlying file,
            // since metadata operations are often followed by file status checks.
            clearstatcache(TRUE, $target);
        }
        return $return;
    }

    public static function move_uploaded_file($upload, $path): bool
    {
        return move_uploaded_file($upload, self::path($path));
    }
}
