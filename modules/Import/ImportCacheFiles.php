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

/**

 * Description: Static class to that is used to get the filenames for the various
 * cache files used
 * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
 * All Rights Reserved.
 */

#[\AllowDynamicProperties]
class ImportCacheFiles
{
    /**#@+
     * Cache file names
     */
    public const FILE_MISCELLANEOUS      = 'misc';
    public const FILE_DUPLICATES         = 'dupes';
    public const FILE_DUPLICATES_DISPLAY = 'dupesdisplay';
    public const FILE_ERRORS             = 'error';
    public const FILE_ERROR_RECORDS      = 'errorrecords';
    public const FILE_ERROR_RECORDS_ONLY = 'errorrecordsonly';
    public const FILE_STATUS             = 'status';
    /**#@-*/

    /**
     * List of all cache file names
     *
     * @var array
     */
    protected static $all_files = array(
        self::FILE_MISCELLANEOUS,
        self::FILE_DUPLICATES,
        self::FILE_DUPLICATES_DISPLAY,
        self::FILE_ERRORS,
        self::FILE_ERROR_RECORDS,
        self::FILE_ERROR_RECORDS_ONLY,
        self::FILE_STATUS,
    );

    /**
     * Get import directory name
     * @return string
     */
    public static function getImportDir()
    {
        return "upload://import";
    }


    /**
     * Function generates a download link for the given import file
     *
     * @param string $fileName String value of the upload file name
     * @return string The converted URL of the file name
     */
    public static function convertFileNameToUrl($fileName)
    {
        $fileName = str_replace(self::getImportDir() . "/", "", $fileName);
        $fileName = "index.php?entryPoint=download&id=ImportErrors&type=import&tempName=" . $fileName . "&isTempFile=1";
        return $fileName;
    }


    /**
     * Returns the filename for a temporary file
     *
     * @param  string $type string to prepend to the filename, typically to indicate the file's use
     * @return string filename
     */
    private static function _createFileName($type = self::FILE_MISCELLANEOUS)
    {
        global $current_user;
        $importdir = self::getImportDir();
        // ensure dir exists and writable
        UploadStream::ensureDir($importdir, true);

        return "$importdir/{$type}_{$current_user->id}.csv";
    }

    /**
     * Ensure that all cache files are writable or can be created
     *
     * @return bool
     */
    public static function ensureWritable()
    {
        foreach (self::$all_files as $type) {
            $filename = self::_createFileName($type);
            if (file_exists($filename) && !is_writable($filename)) {
                return false;
            } elseif (!is_writable(dirname($filename))) {
                return false;
            }
        }
        return true;
    }

    /**
     * Returns the duplicates filename (the ones used to download to csv file
     *
     * @return string filename
     */
    public static function getDuplicateFileName()
    {
        return self::_createFileName(self::FILE_DUPLICATES);
    }

    /**
     * Returns the duplicates display filename (the one used for display in html)
     *
     * @return string filename
     */
    public static function getDuplicateFileDisplayName()
    {
        return self::_createFileName(self::FILE_DUPLICATES_DISPLAY);
    }

    /**
     * Returns the error filename
     *
     * @return string filename
     */
    public static function getErrorFileName()
    {
        return self::_createFileName(self::FILE_ERRORS);
    }

    /**
     * Returns the error records filename
     *
     * @return string filename
     */
    public static function getErrorRecordsFileName()
    {
        return self::_createFileName(self::FILE_ERROR_RECORDS);
    }

    /**
     * Returns the error records filename
     *
     * @return string filename
     */
    public static function getErrorRecordsWithoutErrorFileName()
    {
        return self::_createFileName(self::FILE_ERROR_RECORDS_ONLY);
    }

    /**
     * Returns the status filename
     *
     * @return string filename
     */
    public static function getStatusFileName()
    {
        return self::_createFileName(self::FILE_STATUS);
    }

    /**
     * Clears out all cache files in the import directory
     */
    public static function clearCacheFiles()
    {
        global $sugar_config;
        $importdir = self::getImportDir();
        if (is_dir($importdir)) {
            $files = dir($importdir);
            while (false !== ($file = $files->read())) {
                if (!is_dir($file) && stristr($file, '.csv')) {
                    unlink("$importdir/$file");
                }
            }
        }
    }
}
