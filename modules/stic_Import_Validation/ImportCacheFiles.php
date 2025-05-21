<?php
/**
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation.
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

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
    public const FILE_ERROR_RECORDS_ONLY = 'OriginalFileWithErrors';
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
        return "upload://stic_Import_Validation";
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
        $fileName = "index.php?entryPoint=download&id=ImportErrors&type=stic_Import_Validation&tempName=" . $fileName . "&isTempFile=1";
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

        // STIC-Code MHP - Build the file name with the current date and timestamp. 
        // return "$importdir/{$type}_{$current_user->id}.csv";
        if (!isset($_SESSION["stic_ImporValidation"]['errorFileName'])) {
            $date = date('Ymd');
            $timestamp = time();
            // STIC-Code MHP - Rename file error considering language
            if ($type = self::FILE_ERROR_RECORDS_ONLY) {
                $type = $GLOBALS['mod_strings']['LBL_ERROR_FILENAME'];
            }
            // END STIC-Code 
            $filename = "$importdir/{$type}_{$date}_{$timestamp}.csv";
            $_SESSION["stic_ImporValidation"]['errorFileName'] = $filename;
            return $filename;
        } else {
            return $_SESSION["stic_ImporValidation"]['errorFileName'];
        }
        // END STIC-Code
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
