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

require_once(__DIR__.'/../SugarCache/SugarCache.php');

/**
 * sugar_mkdir
 * Call this function instead of mkdir to apply pre-configured permission
 * settings when creating the directory.  This method is basically
 * a wrapper to the PHP mkdir function except that it supports setting
 * the mode value by using the configuration file (if set).  The mode is
 * 0777 by default.
 *
 * @param string $pathname - String value of the directory to create
 * @param int $mode - The integer value of the permissions mode to set the created directory to
 * @param bool $recursive - boolean value indicating whether or not to create recursive directories if needed
 * @param resource $context
 *
 * @return bool - Returns true on success false on failure
 *
 * @throws Exception
 */
function sugar_mkdir($pathname, $mode = null, $recursive = false, $context = null)
{
    $mode = get_mode('dir_mode', $mode);

    if (is_dir($pathname)) {
        return true;
    }

    if (empty($mode)) {
        $mode = 0777;
    }
    if (empty($context)) {
        $result = @mkdir($pathname, $mode, $recursive);
    } else {
        $result = @mkdir($pathname, $mode, $recursive, $context);
    }

    if ($result) {
        if (!sugar_chmod($pathname, $mode)  && !is_writable($pathname)) {
            return false;
        }
        if (!empty($GLOBALS['sugar_config']['default_permissions']['user'])) {
            if (!sugar_chown($pathname)) {
                return false;
            }
        }
        if (!empty($GLOBALS['sugar_config']['default_permissions']['group'])) {
            if (!sugar_chgrp($pathname)) {
                return false;
            }
        }
    } else {
        $errorMessage = "Cannot create directory $pathname cannot be touched";
        if (is_null($GLOBALS['log'])) {
            throw new Exception("Error occurred but the system doesn't have logger. Error message: \"$errorMessage\"");
        }
        $GLOBALS['log']->error($errorMessage);
    }

    return $result;
}

/**
 * sugar_fopen
 * Call this function instead of fopen to apply pre-configured permission
 * settings when creating the file.  This method is basically
 * a wrapper to the PHP fopen function except that it supports setting
 * the mode value by using the configuration file (if set).  The mode is
 * 0777 by default.
 *
 * @param string $filename - String value of the file to create
 * @param int $mode - The integer value of the permissions mode to set the created file to
 * @param bool $use_include_path - boolean value indicating whether or not to search the included_path
 * @param resource $context
 *
 * @return resource - Returns a file pointer on success, false otherwise
 */
function sugar_fopen($filename, $mode, $use_include_path = false, $context = null)
{
    //check to see if the file exists, if not then use touch to create it.
    if (!file_exists($filename)) {
        sugar_touch($filename);
    }

    if (empty($context)) {
        return fopen($filename, $mode, $use_include_path);
    } else {
        return fopen($filename, $mode, $use_include_path, $context);
    }
}

/**
 * sugar_fclose
 * Call this function instead of fclose to make sure the closed file
 * is removed from caches
 *
 * @param resource $handle - Handle of the file to close
 *
 * @return bool - Returns true on success, false otherwise
 */
function sugar_fclose($handle)
{
    $filename = stream_get_meta_data($handle)['uri'];

    $result = fclose($handle);

    if ((new SplFileInfo($filename))->getExtension() == 'php') {
        SugarCache::cleanFile($filename);
    }

    return $result;
}

/**
 * sugar_file_put_contents
 * Call this function instead of file_put_contents to apply pre-configured permission
 * settings when creating the file.  This method is basically
 * a wrapper to the PHP file_put_contents function except that it supports setting
 * the mode value by using the configuration file (if set).  The mode is
 * 0777 by default.
 *
 * @param string $filename - String value of the file to create
 * @param mixed $data - The data to be written to the file
 * @param int $flags - int as specified by file_put_contents parameters
 * @param resource $context
 *
 * @return int - Returns the number of bytes written to the file, false otherwise.
 */
function sugar_file_put_contents($filename, $data, $flags = null, $context = null)
{
    //check to see if the file exists, if not then use touch to create it.
    if (!file_exists($filename)) {
        sugar_touch($filename);
    }

    if (!is_writable($filename)) {
        LoggerManager::getLogger()->error("File $filename cannot be written to");

        return false;
    }

    if ($flags === null){
        $flags = 0;
    }

    $result = file_put_contents($filename, $data, $flags, $context);
    if ((new SplFileInfo($filename))->getExtension() == 'php') {
        SugarCache::cleanFile($filename);
    }

    return $result;
}

/**
 * sugar_file_put_contents_atomic
 * This is an atomic version of sugar_file_put_contents. It attempts to circumvent the shortcomings of file_put_contents
 * by creating a temporary unique file and then doing an atomic rename operation.
 *
 * @param string $filename - String value of the file to create
 * @param mixed $data - The data to be written to the file
 * @param string $mode String value of the parameter to specify the type of access you require to the file stream
 *
 * @return bool - Returns true if $filename was created, false otherwise.
 */
function sugar_file_put_contents_atomic($filename, $data, $mode = 'wb')
{
    $dir = dirname($filename);
    $temp = tempnam($dir, 'temp');

    if (!($f = @fopen($temp, $mode))) {
        $temp = $dir . DIRECTORY_SEPARATOR . uniqid('temp');
        if (!($f = @fopen($temp, $mode))) {
            trigger_error("sugar_file_put_contents_atomic() : error writing temporary file '$temp'", E_USER_WARNING);

            return false;
        }
    }

    fwrite($f, $data);
    fclose($f);

    if (!@rename($temp, $filename)) {
        @unlink($filename);
        if (!@rename($temp, $filename)) {
            // cleaning up temp file to avoid filling up temp dir
            @unlink($temp);
            trigger_error(
                "sugar_file_put_contents_atomic() : fatal rename failure '$temp' -> '$filename'",
                E_USER_WARNING
            );
        }
    }

    if (file_exists($filename)) {
        if (!empty($GLOBALS['sugar_config']['default_permissions']['file_mode'])) {
            $result = sugar_chmod($filename, $GLOBALS['sugar_config']['default_permissions']['file_mode']);
        }else{
            $result = sugar_chmod($filename, 0755);
        }
        if ((new SplFileInfo($filename))->getExtension() == 'php') {
            SugarCache::cleanFile($filename);
        }
        return $result;
    }

    return false;
}

/**
 * sugar_file_get_contents.
 *
 * @param string $filename - String value of the file to create
 * @param bool $use_include_path - boolean value indicating whether or not to search the included_path
 * @param resource $context
 *
 * @return string|bool - Returns a file data on success, false otherwise
 */
function sugar_file_get_contents($filename, $use_include_path = false, $context = null)
{
    //check to see if the file exists, if not then use touch to create it.
    if (!file_exists($filename)) {
        sugar_touch($filename);
    }

    if (!is_readable($filename)) {
        $GLOBALS['log']->error("File $filename cannot be read");

        return false;
    }

    if (empty($context)) {
        return file_get_contents($filename, $use_include_path);
    } else {
        return file_get_contents($filename, $use_include_path, $context);
    }
}

/**
 * sugar_touch
 * Attempts to set the access and modification times of the file named in the filename
 * parameter to the value given in time . Note that the access time is always modified,
 * regardless of the number of parameters.  If the file does not exist it will be created.
 * This method is basically a wrapper to the PHP touch method except that created files
 * may be set with the permissions specified in the configuration file (if set).
 *
 * @param string $filename - The name of the file being touched.
 * @param int $time - The touch time. If time  is not supplied, the current system time is used.
 * @param $atime - If present, the access time of the given filename is set to the value of atime
 *
 * @return bool - Returns TRUE on success or FALSE on failure.
 */
function sugar_touch($filename, $time = null, $atime = null)
{
    if (!empty($atime) && !empty($time)) {
        $result = @touch($filename, $time, $atime);
    } elseif (!empty($time)) {
        $result = @touch($filename, $time);
    } else {
        $result = @touch($filename);
    }

    if (!$result) {
        $GLOBALS['log']->error("File $filename cannot be touched");

        return $result;
    }
    if (!empty($GLOBALS['sugar_config']['default_permissions']['file_mode'])) {
        sugar_chmod($filename, $GLOBALS['sugar_config']['default_permissions']['file_mode']);
    }
    if (!empty($GLOBALS['sugar_config']['default_permissions']['user'])) {
        sugar_chown($filename);
    }
    if (!empty($GLOBALS['sugar_config']['default_permissions']['group'])) {
        sugar_chgrp($filename);
    }

    return true;
}

/**
 * sugar_chmod
 * Attempts to change the permission of the specified filename to the mode value specified in the
 * default_permissions configuration; otherwise, it will use the mode value.
 *
 * @param  string $filename - Path to the file
 * @param int $mode The integer value of the permissions mode to set the created directory to
 *
 * @return bool Returns TRUE on success or FALSE on failure.
 */
function sugar_chmod($filename, $mode = null)
{
    if (!is_int($mode)) {
        $mode = (int)$mode;
    }
    if (!is_windows()) {
        if (!isset($mode)) {
            $mode = get_mode('file_mode', $mode);
        }
        if (isset($mode) && $mode > 0) {
            return @chmod($filename, $mode);
        } else {
            return false;
        }
    }

    return true;
}

/**
 * sugar_chown
 * Attempts to change the owner of the file filename to the user specified in the
 * default_permissions configuration; otherwise, it will use the user value.
 *
 * @param $filename - Path to the file
 * @param string $user - A user name or number
 *
 * @return bool - Returns TRUE on success or FALSE on failure.
 */
function sugar_chown($filename, $user = '')
{
    if (!is_windows()) {
        if (strlen($user)) {
            return chown($filename, $user);
        } else {
            if (strlen((string) $GLOBALS['sugar_config']['default_permissions']['user'])) {
                $user = $GLOBALS['sugar_config']['default_permissions']['user'];

                return chown($filename, $user);
            } else {
                return false;
            }
        }
    }

    return true;
}

/**
 * sugar_chgrp
 * Attempts to change the group of the file filename to the group specified in the
 * default_permissions configuration; otherwise it will use the group value.
 *
 * @param filename - Path to the file
 * @param string $group - A group name or number
 *
 * @return bool - Returns TRUE on success or FALSE on failure.
 */
function sugar_chgrp($filename, $group = '')
{
    if (!is_windows()) {
        if (!empty($group)) {
            return chgrp($filename, $group);
        } else {
            if (!empty($GLOBALS['sugar_config']['default_permissions']['group'])) {
                $group = $GLOBALS['sugar_config']['default_permissions']['group'];

                return chgrp($filename, $group);
            } else {
                return false;
            }
        }
    }

    return true;
}

/**
 * get_mode.
 *
 * Will check to see if there is a default mode defined in the config file, otherwise return the
 * $mode given as input
 *
 * @param string $key
 * @param int $mode - the mode being passed by the calling function. This value will be overridden by a value
 *                     defined in the config file.
 *
 * @return int - the mode either found in the config file or passed in via the input parameter
 */
function get_mode($key = 'dir_mode', $mode = null)
{
    if (!is_int($mode)) {
        $mode = (int)$mode;
    }
    if (!class_exists('SugarConfig', true)) {
        require 'include/SugarObjects/SugarConfig.php';
    }
    if (!is_windows()) {
        $conf_inst = SugarConfig::getInstance();
        $mode = $conf_inst->get('default_permissions.' . $key, $mode);
    }

    return $mode;
}

/**
 * Get filename in cache directory.
 *
 * @api
 *
 * @param string $file
 *
 * @return string
 */
function sugar_cached($file)
{
    static $cdir = null;
    if (empty($cdir) && !empty($GLOBALS['sugar_config']['cache_dir'])) {
        $cdir = rtrim($GLOBALS['sugar_config']['cache_dir'], '/\\');
    }
    if (empty($cdir)) {
        $cdir = 'cache';
    }

    return "$cdir/$file";
}

/**
 * @deprecated 7.8
 * @see is_dir
 *
 * @param $path
 * @return bool
 */
function sugar_is_dir($path)
{
    if (isset($GLOBALS['log'])) {
        $GLOBALS['log']->deprecated('sugar_file_utils.php: sugar_is_dir() is deprecated');
    }
    return is_dir($path);
}

/**
 * @deprecated 7.8
 * @see is_file
 *
 * @param $path
 * @return bool
 */
function sugar_is_file($path)
{
    if (isset($GLOBALS['log'])) {
        $GLOBALS['log']->deprecated('sugar_file_utils.php: sugar_is_file() is deprecated');
    }
    return is_file($path);
}
