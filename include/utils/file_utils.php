<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}
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


require_once('include/utils/array_utils.php');
require_once('include/utils/sugar_file_utils.php');

/**
 * Convert all \ to / in path, remove multiple '/'s and '/./'
 * @param string $path
 * @return string
 */
function clean_path($path)
{
    // clean directory/file path with a functional equivalent
    $appendpath = '';
    if (is_windows() && strlen($path) >= 2 && $path[0].$path[1] == "\\\\") {
        $path = substr($path, 2);
        $appendpath = "\\\\";
    }
    $path = str_replace("\\", "/", $path);
    $path = str_replace("//", "/", $path);
    $path = str_replace("/./", "/", $path);
    return($appendpath.$path);
}

function create_cache_directory($file)
{
    $paths = explode('/', $file);
    $dir = rtrim($GLOBALS['sugar_config']['cache_dir'], '/\\');
    if (!file_exists($dir)) {
        sugar_mkdir($dir, 0775);
    }
    $pathsCount = count($paths);
    for ($i = 0; $i < $pathsCount - 1; $i++) {
        $dir .= '/' . $paths[$i];
        if (!file_exists($dir)) {
            sugar_mkdir($dir, 0775);
        }
    }
    return $dir . '/'. $paths[count($paths) - 1];
}

function get_module_dir_list()
{
    $modules = array();
    $path = 'modules';
    $d = dir($path);
    while ($entry = $d->read()) {
        if ($entry != '..' && $entry != '.') {
            if (is_dir($path. '/'. $entry)) {
                $modules[$entry] = $entry;
            }
        }
    }
    return $modules;
}

function mk_temp_dir($base_dir, $prefix="")
{
    $temp_dir = tempnam($base_dir, $prefix);
    if (!$temp_dir || !unlink($temp_dir)) {
        return(false);
    }

    if (sugar_mkdir($temp_dir)) {
        return($temp_dir);
    }

    return(false);
}

function remove_file_extension($filename)
{
    return(substr((string) $filename, 0, strrpos((string) $filename, ".")));
}

function write_array_to_file($the_name, $the_array, $the_file, $mode="w", $header='')
{
    if (!empty($header) && ($mode !== 'a' || $mode !== 'ab' || !file_exists($the_file))) {
        $the_string = $header;
    } else {
        $the_string =   "<?php\n" .
                    '// created: ' . date('Y-m-d H:i:s') . "\n";
    }
    $the_string .=  "\$$the_name = " .
                    var_export_helper($the_array) .
                    ";";

    return sugar_file_put_contents($the_file, $the_string, LOCK_EX) !== false;
}

function write_override_label_to_file($the_name, $the_array, $the_file, $mode = 'w', $header = '')
{
    if (!empty($header) && ($mode !== 'a' || $mode !== 'ab' || !file_exists($the_file))) {
        $the_string = $header;
    } else {
        $the_string = "<?php\n" .
            '// created: ' . date('Y-m-d H:i:s') . "\n";
    }

    foreach ($the_array as $labelName => $labelValue) {
        $the_string .= '$' . "{$the_name}['" . addslashes($labelName) . "'] = '" . addslashes($labelValue) ."';\n";
    }

    return sugar_file_put_contents($the_file, $the_string, LOCK_EX) !== false;
}

function write_encoded_file($soap_result, $write_to_dir, $write_to_file="")
{
    // this function dies when encountering an error -- use with caution!
    // the path/file is returned upon success



    if ($write_to_file == "") {
        $write_to_file = $write_to_dir . "/" . $soap_result['filename'];
    }

    $file = $soap_result['data'];
    $write_to_file = str_replace("\\", "/", (string) $write_to_file);

    $dir_to_make = dirname($write_to_file);
    if (!is_dir($dir_to_make)) {
        mkdir_recursive($dir_to_make);
    }
    $fh = sugar_fopen($write_to_file, "wb");
    fwrite($fh, base64_decode($file));
    fclose($fh);

    if (md5_file($write_to_file) != $soap_result['md5']) {
        die("MD5 error after writing file $write_to_file");
    }
    return($write_to_file);
}

function create_custom_directory($file)
{
    $paths = explode('/', $file);
    $dir = 'custom';
    if (!file_exists($dir)) {
        sugar_mkdir($dir, 0755);
    }
    $pathsCount = count($paths);
    for ($i = 0; $i < $pathsCount - 1; $i++) {
        $dir .= '/' . $paths[$i];
        if (!file_exists($dir)) {
            sugar_mkdir($dir, 0755);
        }
    }
    return $dir . '/'. $paths[count($paths) - 1];
}

/**
 * This function will recursively generates md5s of files and returns an array of all md5s.
 * @param string $path The path of the root directory to scan - must end with '/'
 * @param array $ignore_dirs array of filenames/directory names to ignore running md5 on - default 'cache' and 'upload'
 * @return array
 */
function generateMD5array($path, $ignore_dirs = ['cache', 'upload', '.git', 'vendor', '.idea'])
{
    $current_dir_content = [];
    $dh = opendir($path);
    if (!$dh) {
        return [];
    }
    while (false !== ($filename = readdir($dh))) {
        $current_dir_content[] = $filename;
    }

    // removes the ignored directories
    $current_dir_content = array_diff($current_dir_content, $ignore_dirs);

    sort($current_dir_content);
    $md5_array = [];

    foreach ($current_dir_content as $file) {
        // make sure that it's not dir '.' or '..'
        if (strcmp($file, '.') && strcmp($file, '..')) {
            if (is_dir($path . $file)) {
                // For testing purposes - uncomment to see all files and md5s
                //echo "<BR>Dir:  ".$path.$file."<br>";
                //generateMD5array($path.$file."/");

                $md5_array += generateMD5array($path . $file . '/', $ignore_dirs);
            } else {
                // For testing purposes - uncomment to see all files and md5s
                //echo "   File: ".$path.$file."<br>";
                //echo md5_file($path.$file)."<BR>";

                $md5_array[$path . $file] = md5_file($path . $file);
            }
        }
    }

    return $md5_array;
}

/**
 * Function to compare two directory structures and return the items in path_a that didn't match in path_b
 *
 * @param	$path_a The path of the first root directory to scan - must end with '/'
 * @param	$path_b The path of the second root directory to scan - must end with '/'
 * @param	$ignore_dirs array of filenames/directory names to ignore running md5 on - default 'cache' and 'upload'
 * @result	array containing all the md5s of everything in $path_a that didn't have a match in $path_b
 */
function md5DirCompare($path_a, $path_b, $ignore_dirs = array('cache', 'upload'))
{
    $md5array_a = generateMD5array($path_a, $ignore_dirs);
    $md5array_b = generateMD5array($path_b, $ignore_dirs);

    $result = array_diff($md5array_a, $md5array_b);

    return $result;
}

/**
 * Function to retrieve all file names of matching pattern in a directory (and it's subdirectories)
 * example: getFiles($arr, './modules', '.+/EditView.php/'); // grabs all EditView.phps
 * @param array $arr return array to populate matches
 * @param string $dir directory to look in [ USE ./ in front of the $dir! ]
 * @param regex $pattern optional pattern to match against
 */
function getFiles(&$arr, $dir, $pattern = null)
{
    if (!is_dir($dir)) {
        return;
    }
    $d = dir($dir);
    while ($e =$d->read()) {
        if (substr($e, 0, 1) == '.') {
            continue;
        }
        $file = $dir . '/' . $e;
        if (is_dir($file)) {
            getFiles($arr, $file, $pattern);
        } else {
            if (empty($pattern)) {
                $arr[] = $file;
            } else {
                if (preg_match($pattern, $file)) {
                    $arr[] = $file;
                }
            }
        }
    }
}

/**
 * Function to split up large files for download
 * used in download.php
 * @param string $filename
 * @param int $retbytes
 */
function readfile_chunked($filename, $retbytes=true)
{
    $chunksize = 1*(1024*1024); // how many bytes per chunk
    $buffer = '';
    $cnt = 0;
    $handle = sugar_fopen($filename, 'rb');
    if ($handle === false) {
        return false;
    }
    while (!feof($handle)) {
        $buffer = fread($handle, $chunksize);
        echo $buffer;
        flush();
        if ($retbytes) {
            $cnt += strlen($buffer);
        }
    }
    $status = fclose($handle);
    if ($retbytes && $status) {
        return $cnt; // return num. bytes delivered like readfile() does.
    }
    return $status;
}
/**
 * Renames a file. If $new_file already exists, it will first unlink it and then rename it.
 * used in SugarLogger.php
 * @param string $old_filename
 * @param string $new_filename
 */
function sugar_rename($old_filename, $new_filename)
{
    if (empty($old_filename) || empty($new_filename)) {
        return false;
    }
    $success = false;
    if (file_exists($new_filename)) {
        unlink($new_filename);
        $success = rename($old_filename, $new_filename);
    } else {
        $success = rename($old_filename, $new_filename);
    }

    return $success;
}

function fileToHash($file)
{
    $hash = md5($file);
    $_SESSION['file2Hash'][$hash] = $file;
    return $hash;
}

function hashToFile($hash)
{
    if (!empty($_SESSION['file2Hash'][$hash])) {
        return $_SESSION['file2Hash'][$hash];
    }
    return false;
}



/**
 * get_file_extension
 * This function returns the file extension portion of a given filename
 *
 * @param $filename String of filename to return extension
 * @param $string_to_lower boolean value indicating whether or not to return value as lowercase, true by default
 *
 * @return extension String value, blank if no extension found
 */
function get_file_extension($filename, $string_to_lower=true)
{
    $ret = '';

    if (strpos((string) $filename, '.') !== false) {
        if ($string_to_lower) {
            $exp = explode('.', $filename);
            $pop = array_pop($exp);
            $ret = strtolower($pop);
        } else {
            $exp = explode('.', $filename);
            $ret = array_pop($exp);
        }
    }

    return $ret;
}


/**
 * get_mime_content_type_from_filename
 * This function is similar to mime_content_type, but does not require a real
 * file or path location.  Instead, the function merely checks the filename
 * extension and returns a best guess mime content type.
 *
 * @param $filename String of filename to return mime content type
 * @return mime content type as String value (defaults to 'application/octet-stream' for filenames with extension, empty otherwise)
 *
 */
function get_mime_content_type_from_filename($filename)
{
    if (strpos((string) $filename, '.') !== false) {
        $mime_types = array(
            'txt' => 'text/plain',
            'htm' => 'text/html',
            'html' => 'text/html',
            'php' => 'text/html',
            'css' => 'text/css',
            'js' => 'application/javascript',
            'json' => 'application/json',
            'xml' => 'application/xml',
            'swf' => 'application/x-shockwave-flash',
            'flv' => 'video/x-flv',

            // images
            'png' => 'image/png',
            'jpe' => 'image/jpeg',
            'jpeg' => 'image/jpeg',
            'jpg' => 'image/jpeg',
            'gif' => 'image/gif',
            'bmp' => 'image/bmp',
            'ico' => 'image/vnd.microsoft.icon',
            'tiff' => 'image/tiff',
            'tif' => 'image/tiff',
            'svg' => 'image/svg+xml',
            'svgz' => 'image/svg+xml',

            // archives
            'zip' => 'application/zip',
            'rar' => 'application/x-rar-compressed',
            'exe' => 'application/x-msdownload',
            'msi' => 'application/x-msdownload',
            'cab' => 'application/vnd.ms-cab-compressed',

            // audio/video
            'mp3' => 'audio/mpeg',
            'qt' => 'video/quicktime',
            'mov' => 'video/quicktime',

            // adobe
            'pdf' => 'application/pdf',
            'psd' => 'image/vnd.adobe.photoshop',
            'ai' => 'application/postscript',
            'eps' => 'application/postscript',
            'ps' => 'application/postscript',

            // ms office
            'doc' => 'application/msword',
            'rtf' => 'application/rtf',
            'xls' => 'application/vnd.ms-excel',
            'ppt' => 'application/vnd.ms-powerpoint',

            // open office
            'odt' => 'application/vnd.oasis.opendocument.text',
            'ods' => 'application/vnd.oasis.opendocument.spreadsheet',
        );

        $exp = explode('.', $filename);
        $pop = array_pop($exp);
        $ext = strtolower($pop);
        if (array_key_exists($ext, $mime_types)) {
            return $mime_types[$ext];
        }

        return 'application/octet-stream';
    }

    return '';
}
/*
function createFTSLogicHook($filePath = 'application/Ext/LogicHooks/logichooks.ext.php')
{
    $customFileLoc = create_custom_directory($filePath);
    $fp = sugar_fopen($customFileLoc, 'wb');
    $contents = <<<CIA
<?php
if (!isset(\$hook_array) || !is_array(\$hook_array)) {
    \$hook_array = array();
}
if (!isset(\$hook_array['after_save']) || !is_array(\$hook_array['after_save'])) {
    \$hook_array['after_save'] = array();
}
\$hook_array['after_save'][] = array(1, 'fts', 'include/SugarSearchEngine/SugarSearchEngineQueueManager.php', 'SugarSearchEngineQueueManager', 'populateIndexQueue');
CIA;

    fwrite($fp,$contents);
    fclose($fp);

}
*/
function cleanFileName($name)
{
    return preg_replace('/[^\w\-._]+/i', '', (string) $name);
}

/**
 * Filter dir name to not contain path components - no slashes, no .., etc.
 * @param string $name
 * @return string
 */
function cleanDirName($name)
{
    return str_replace(array("\\", "/", "."), "", $name);
}

/**
 * Check if has valid file name
 * @param string $fieldName
 * @param string $value
 * @return bool
 */
function hasValidFileName($fieldName, $value) {

    if (empty($value)){
        LoggerManager::getLogger()->error("Invalid filename for $fieldName : '$value'.");
        return false;
    }

    $isValid = preg_match('/^[\w\-.]+(\.\w+)?$/', $value);
    if ($isValid === false || $isValid < 1) {
        LoggerManager::getLogger()->error("Invalid filename for $fieldName : '$value'.");
        return false;
    }

    return true;
}
