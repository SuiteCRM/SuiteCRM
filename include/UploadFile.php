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

require_once __DIR__ . '/externalAPI/ExternalAPIFactory.php';
require_once __DIR__ . '/UploadStream.php';

use SuiteCRM\Exception\MalwareFoundException;
use SuiteCRM\Utility\AntiMalware\AntiMalwareTrait;

/**
 * @api
 * Manage uploaded files
 */
#[\AllowDynamicProperties]
class UploadFile
{
    use AntiMalwareTrait;

    public $field_name;
    public $stored_file_name;
    public $uploaded_file_name;
    public $original_file_name;
    public $temp_file_location;
    public $use_soap = false;
    public $file;
    public $file_ext;
    public $mime_type;
    protected static $url = "upload/";

    /**
     * Upload errors
     * @var array
     */
    protected static $filesError = array(
        UPLOAD_ERR_OK => 'UPLOAD_ERR_OK - There is no error, the file uploaded with success.',
        UPLOAD_ERR_INI_SIZE => 'UPLOAD_ERR_INI_SIZE - The uploaded file exceeds the upload_max_filesize directive in php.ini.',
        UPLOAD_ERR_FORM_SIZE => 'UPLOAD_ERR_FORM_SIZE - The uploaded file exceeds the MAX_FILE_SIZE directive that was specified in the HTML form.',
        UPLOAD_ERR_PARTIAL => 'UPLOAD_ERR_PARTIAL - The uploaded file was only partially uploaded.',
        UPLOAD_ERR_NO_FILE => 'UPLOAD_ERR_NO_FILE - No file was uploaded.',
        5 => 'UNKNOWN ERROR',
        UPLOAD_ERR_NO_TMP_DIR => 'UPLOAD_ERR_NO_TMP_DIR - Missing a temporary folder.',
        UPLOAD_ERR_CANT_WRITE => 'UPLOAD_ERR_CANT_WRITE - Failed to write file to disk.',
        UPLOAD_ERR_EXTENSION => 'UPLOAD_ERR_EXTENSION - A PHP extension stopped the file upload.',
    );

    /**
     * Create upload file handler
     * @param string $field_name Form field name
     */
    public function __construct($field_name = '')
    {
        // $field_name is the name of your passed file selector field in your form
        // i.e., for Emails, it is "email_attachmentX" where X is 0-9
        $this->field_name = $field_name;
    }

    /**
     * Setup for SOAP upload
     * @param string $filename Name for the file
     * @param string $file
     */
    public function set_for_soap($filename, $file)
    {
        $this->stored_file_name = $filename;
        $this->use_soap = true;
        $this->file = $file;
    }

    /**
     * Get URL for a document
     * @param string stored_file_name File name in filesystem
     * @param string bean_id note bean ID
     * @return string path with file name
     * @deprecated
     */
    public static function get_url($stored_file_name, $bean_id)
    {
        if (empty($bean_id) && empty($stored_file_name)) {
            return self::$url;
        }

        return self::$url . $bean_id;
    }

    /**
     * Get URL of the uploaded file related to the document
     * @param SugarBean $document
     * @param string $type Type of the document, if different from $document
     */
    public static function get_upload_url($document, $type = null)
    {
        if (empty($type)) {
            $type = $document->module_dir;
        }

        return "index.php?entryPoint=download&type=$type&id={$document->id}";
    }

    /**
     * Try renaming a file to bean_id name
     * @param string $filename
     * @param string $bean_id
     */
    protected static function tryRename($filename, $bean_id)
    {
        $fullname = "upload://$bean_id.$filename";
        if (file_exists($fullname)) {
            if (!rename($fullname, "upload://$bean_id")) {
                $GLOBALS['log']->fatal("unable to rename file: $fullname => $bean_id");
            }

            return true;
        }

        return false;
    }

    /**
     * builds a URL path for an anchor tag
     * @param string stored_file_name File name in filesystem
     * @param string bean_id note bean ID
     * @return string path with file name
     */
    public static function get_file_path($stored_file_name, $bean_id, $skip_rename = false)
    {
        global $locale;

        // if the parameters are empty strings, just return back the upload_dir
        if (empty($bean_id) && empty($stored_file_name)) {
            return "upload://";
        }

        if (!$skip_rename) {
            self::tryRename(rawurlencode($stored_file_name), $bean_id) ||
            self::tryRename(urlencode($stored_file_name), $bean_id) ||
            self::tryRename($stored_file_name, $bean_id) ||
            self::tryRename(
                $locale->translateCharset($stored_file_name, 'UTF-8', $locale->getExportCharset()),
                $bean_id
            );
        }

        return "upload://$bean_id";
    }

    /**
     * duplicates an already uploaded file in the filesystem.
     * @param string old_id ID of original note
     * @param string new_id ID of new (copied) note
     * @param string filename Filename of file (deprecated)
     * @return boolean TRUE = success, FALSE = failed
     */
    public static function duplicate_file($old_id, $new_id, $file_name)
    {
        global $sugar_config;

        // current file system (GUID)
        $source = "upload://$old_id";

        if (!file_exists($source)) {
            // old-style file system (GUID.filename.extension)
            $oldStyleSource = $source . $file_name;
            if (file_exists($oldStyleSource)) {
                // change to new style
                if (copy($oldStyleSource, $source)) {
                    // delete the old
                    if (!unlink($oldStyleSource)) {
                        $GLOBALS['log']->error("upload_file could not unlink [ {$oldStyleSource} ]");
                    }
                } else {
                    $GLOBALS['log']->error("upload_file could not copy [ {$oldStyleSource} ] to [ {$source} ]");
                }
            }
        }

        $destination = "upload://$new_id";

        if (is_dir($source)) {
            LoggerManager::getLogger()->warn('Upload File error: Argument cannot be a directory. Argument was: "' . $source . '"');
        } else {
            if (!copy($source, $destination)) {
                $GLOBALS['log']->error("upload_file could not copy [ {$source} ] to [ {$destination} ]");
            } else {
                return true;
            }
        }

        return false;
    }

    /**
     * Get upload error from system
     * @return string upload error
     */
    public function get_upload_error()
    {
        if (isset($this->field_name) && isset($_FILES[$this->field_name]['error'])) {
            return $_FILES[$this->field_name]['error'];
        }

        return false;
    }

    /**
     * standard PHP file-upload security measures. all variables accessed in a global context
     * @return bool True on success
     */
    public function confirm_upload()
    {
        global $sugar_config;

        if (empty($this->field_name) || !isset($_FILES[$this->field_name])) {
            return false;
        }

        //check to see if there are any errors from upload
        if ($_FILES[$this->field_name]['error'] != UPLOAD_ERR_OK) {
            if ($_FILES[$this->field_name]['error'] != UPLOAD_ERR_NO_FILE) {
                if ($_FILES[$this->field_name]['error'] == UPLOAD_ERR_INI_SIZE) {
                    //log the error, the string produced will read something like:
                    //ERROR: There was an error during upload. Error code: 1
                    // - UPLOAD_ERR_INI_SIZE - The uploaded file exceeds the upload_max_filesize directive in php.ini. upload_maxsize is 16
                    $errMess = string_format(
                        $GLOBALS['app_strings']['UPLOAD_ERROR_TEXT_SIZEINFO'],
                        array(
                            $_FILES['filename_file']['error'],
                            self::$filesError[$_FILES['filename_file']['error']],
                            $sugar_config['upload_maxsize']
                        )
                    );
                    $GLOBALS['log']->fatal($errMess);
                } else {
                    //log the error, the string produced will read something like:
                    //ERROR: There was an error during upload. Error code: 3
                    // - UPLOAD_ERR_PARTIAL - The uploaded file was only partially uploaded.
                    $errMess = string_format(
                        $GLOBALS['app_strings']['UPLOAD_ERROR_TEXT'],
                        array(
                            $_FILES['filename_file']['error'],
                            self::$filesError[$_FILES['filename_file']['error']]
                        )
                    );
                    $GLOBALS['log']->fatal($errMess);
                }
            }

            return false;
        }

        if (!is_uploaded_file($_FILES[$this->field_name]['tmp_name'])) {
            return false;
        } elseif ($_FILES[$this->field_name]['size'] > $sugar_config['upload_maxsize']) {
            $GLOBALS['log']->fatal("ERROR: uploaded file was too big: max filesize: {$sugar_config['upload_maxsize']}");

            return false;
        }

        if (!UploadStream::writable()) {
            $GLOBALS['log']->fatal("ERROR: cannot write to upload directory");

            return false;
        }

        try {
            $this->scanPathForMalware($_FILES[$this->field_name]['tmp_name']);
        } catch (MalwareFoundException $exception) {
            LoggerManager::getLogger()->security("Malware found, unable to save file: {$_FILES[$this->field_name]['name']}");

            return false;
        }


        $this->mime_type = $this->getMime($_FILES[$this->field_name]);
        $this->stored_file_name = $this->create_stored_filename();
        $this->temp_file_location = $_FILES[$this->field_name]['tmp_name'];
        $this->uploaded_file_name = $_FILES[$this->field_name]['name'];

        if (has_valid_image_mime_type($this->mime_type) && !verify_uploaded_image($this->temp_file_location)) {
            LoggerManager::getLogger()->security("Image Malware found, unable to save file: {$_FILES[$this->field_name]['name']}");
            return false;
        }

        return true;
    }

    /**
     * Guess MIME type for file
     * @param string $filename
     * @return string MIME type
     */
    public function getMimeSoap($filename)
    {
        if (function_exists('ext2mime')) {
            $mime = ext2mime($filename);
        } else {
            $mime = ' application/octet-stream';
        }

        return $mime;
    }

    /**
     * Get MIME type for uploaded file
     * @param array $_FILES_element $_FILES element required
     * @return string MIME type
     */
    public function getMime($_FILES_element)
    {
        $filename = $_FILES_element['name'];
        $filetype = isset($_FILES_element['type']) ? $_FILES_element['type'] : null;
        $file_ext = pathinfo((string) $filename, PATHINFO_EXTENSION);

        $is_image = strpos((string) $filetype, 'image/') === 0;
        // if it's an image, or no file extension is available and the mime is octet-stream
        // try to determine the mime type
        $recheckMime = $is_image || (empty($file_ext) && $filetype == 'application/octet-stream');

        $mime = 'application/octet-stream';
        if ($filetype && !$recheckMime) {
            $mime = $filetype;
        } elseif (function_exists('mime_content_type')) {
            $mime = mime_content_type($_FILES_element['tmp_name']);
        } elseif ($is_image) {
            $info = getimagesize($_FILES_element['tmp_name']);
            if ($info) {
                $mime = $info['mime'];
            }
        } elseif (function_exists('ext2mime')) {
            $mime = ext2mime($filename);
        }

        return $mime;
    }

    /**
     * gets note's filename
     * @return string
     */
    public function get_stored_file_name()
    {
        return $this->stored_file_name;
    }

    public function get_temp_file_location()
    {
        return $this->temp_file_location;
    }

    public function get_uploaded_file_name()
    {
        return $this->uploaded_file_name;
    }

    public function get_mime_type()
    {
        return $this->mime_type;
    }

    /**
     * Returns the contents of the uploaded file
     */
    public function get_file_contents()
    {

        // Need to call
        if (!isset($this->temp_file_location)) {
            $this->confirm_upload();
        }

        if (($data = @file_get_contents($this->temp_file_location)) === false) {
            return false;
        }

        return $data;
    }


    /**
     * creates a file's name for preparation for saving
     * @return string
     */
    public function create_stored_filename()
    {
        global $sugar_config;

        if (!$this->use_soap) {
            $stored_file_name = $_FILES[$this->field_name]['name'];
            $this->original_file_name = $stored_file_name;

            /**
             * cn: bug 8056 - windows filesystems and IIS do not like utf8.  we are forced to urlencode() to ensure that
             * the file is linkable from the browser.  this will stay broken until we move to a db-storage system
             */
            if (is_windows()) {
                // create a non UTF-8 name encoding
                // 176 + 36 char guid = windows' maximum filename length
                $end = (strlen((string) $stored_file_name) > 176) ? 176 : strlen((string) $stored_file_name);
                $stored_file_name = substr((string) $stored_file_name, 0, $end);
                $this->original_file_name = $_FILES[$this->field_name]['name'];
            }
            $stored_file_name = str_replace("\\", "", (string) $stored_file_name);
        } else {
            $stored_file_name = $this->stored_file_name;
            $this->original_file_name = $stored_file_name;
        }

        $this->file_ext = pathinfo((string) $stored_file_name, PATHINFO_EXTENSION);
        // cn: bug 6347 - fix file extension detection
        foreach ($sugar_config['upload_badext'] as $badExt) {
            if (strtolower($this->file_ext) === strtolower($badExt)) {
                $stored_file_name .= ".txt";
                $this->file_ext = "txt";
                break; // no need to look for more
            }
        }

        return $stored_file_name;
    }

    /**
     * moves uploaded temp file to permanent save location
     * @param string $bean_id ID of parent bean
     * @return bool True on success
     */
    public function final_move($bean_id)
    {
        global $log;

        $destination = $bean_id;
        if (substr($destination, 0, 9) != 'upload://') {
            $destination = 'upload://' . $bean_id;
        }

        if ($this->use_soap) {
            if (!file_put_contents($destination, $this->file)) {
                $log->fatal('Unable to save file to ' . $destination);
                return false;
            }
        } elseif (!UploadStream::move_uploaded_file($_FILES[$this->field_name]['tmp_name'], $destination)) {
            $log->fatal(
                'Unable to move move_uploaded_file to ' . $destination .
                ' You should try making the directory writable by the webserver'
            );

            return false;
        }

        return true;
    }

    /**
     * Upload document to external service
     * @param SugarBean $bean Related bean
     * @param string $bean_id
     * @param string $doc_type
     * @param string $file_name
     * @param string $mime_type
     */
    public function upload_doc($bean, $bean_id, $doc_type, $file_name, $mime_type)
    {
        $result = [];
        if (!empty($doc_type) && $doc_type != 'Sugar') {
            global $sugar_config;
            $destination = $this->get_upload_path($bean_id);
            sugar_rename($destination, str_replace($bean_id, $bean_id . '_' . $file_name, $destination));
            $new_destination = $this->get_upload_path($bean_id . '_' . $file_name);

            try {
                $this->api = ExternalAPIFactory::loadAPI($doc_type);

                if (isset($this->api) && $this->api !== false) {
                    $result = $this->api->uploadDoc(
                        $bean,
                        $new_destination,
                        $file_name,
                        $mime_type
                    );
                } else {
                    $result['success'] = false;
                    // FIXME: Translate
                    $GLOBALS['log']->error("Could not load the requested API (" . $doc_type . ")");
                    $result['errorMessage'] = 'Could not find a proper API';
                }
            } catch (Exception $e) {
                $result['success'] = false;
                $result['errorMessage'] = $e->getMessage();
                $GLOBALS['log']->error("Caught exception: (" . $e->getMessage() . ") ");
            }
            if (!$result['success']) {
                sugar_rename($new_destination, str_replace($bean_id . '_' . $file_name, $bean_id, $new_destination));
                $bean->doc_type = 'Sugar';
                // FIXME: Translate
                if (!is_array($_SESSION['user_error_message'])) {
                    $_SESSION['user_error_message'] = array();
                }

                $error_message = isset($result['errorMessage']) ? $result['errorMessage'] :
                    $GLOBALS['app_strings']['ERR_EXTERNAL_API_SAVE_FAIL'];
                $_SESSION['user_error_message'][] = $error_message;
            } else {
                unlink($new_destination);
            }
        }
    }

    /**
     * returns the path with file name to save an uploaded file
     * @param string bean_id ID of the parent bean
     * @return string
     */
    public function get_upload_path($bean_id)
    {
        $file_name = $bean_id;

        // cn: bug 8056 - mbcs filename in urlencoding > 212 chars in Windows fails
        $end = (strlen((string) $file_name) > 212) ? 212 : strlen((string) $file_name);
        $ret_file_name = substr((string) $file_name, 0, $end);

        return "upload://$ret_file_name";
    }

    /**
     * deletes a file
     * @param string bean_id ID of the parent bean
     * @param string file_name File's name
     */
    public static function unlink_file($bean_id, $file_name = '')
    {
        if (file_exists("upload://$bean_id$file_name")) {
            return unlink("upload://$bean_id$file_name");
        }
    }

    /**
     * Get upload file location prefix
     * @return string prefix
     */
    public function get_upload_dir()
    {
        return "upload://";
    }

    /**
     * Return real FS path of the file
     * @param string $path
     */
    public static function realpath($path)
    {
        if (substr($path, 0, 9) == "upload://") {
            $path = UploadStream::path($path);
        }
        $ret = realpath($path);

        return $ret ? $ret : $path;
    }

    /**
     * Return path of uploaded file relative to uploads dir
     * @param string $path
     */
    public static function relativeName($path)
    {
        if (substr($path, 0, 9) == "upload://") {
            $path = substr($path, 9);
        }

        return $path;
    }
}
