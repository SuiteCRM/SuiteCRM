<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2019 SalesAgility Ltd.
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

require_once 'include/SugarCache/SugarCache.php';

/**
 * Unzip file to specified directory
 *
 * @param string $zip_archive
 * @param string $zip_dir
 * @return bool
 */
function unzip($zip_archive, $zip_dir)
{
    return unzip_file($zip_archive, null, $zip_dir);
}

/**
 * Unzip files to specified directory
 *
 * @param string $zip_archive
 * @param string|null $archive_file
 * @param string $zip_dir
 * @return bool
 */
function unzip_file($zip_archive, $archive_file, $zip_dir)
{
    if (!is_dir($zip_dir)) {
        LoggerManager::getLogger()->fatal('Specified directory for zip file extraction does not exist');
        if (defined('SUITE_PHPUNIT_RUNNER') || defined('SUGARCRM_INSTALL')) {
            return false;
        }
    }
    $zip = new ZipArchive;
    // We need realpath here for PHP streams support
    $res = $zip->open(UploadFile::realpath($zip_archive));

    if ($res !== true) {
        LoggerManager::getLogger()->fatal(sprintf('ZIP Error(%d): Status(%s)', $res, $zip->status));
        if (defined('SUITE_PHPUNIT_RUNNER') || defined('SUGARCRM_INSTALL')) {
            return false;
        }
    }

    if ($archive_file !== null) {
        try {
            $res = $zip->extractTo(UploadFile::realpath($zip_dir), $archive_file);
        } catch (ValueError $t) {
            if (file_exists($zip_archive)){
                LoggerManager::getLogger()->fatal(sprintf('ZIP Error(%d): Invalid file(%s). Deleting.', $res, $zip_archive));
                unlink($zip_archive);
            }
            throw $t;
        }

        if ((new SplFileInfo($archive_file))->getExtension() == 'php') {
            SugarCache::cleanFile(UploadFile::realpath($zip_dir).'/'.$archive_file);
        }
    } else {
        $res = $zip->extractTo(UploadFile::realpath($zip_dir));
        SugarCache::cleanDir(UploadFile::realpath($zip_dir));
    }

    if ($res !== true) {
        LoggerManager::getLogger()->fatal(sprintf('ZIP Error(%d): Status(%s)', $res, $zip->status));
        if (defined('SUITE_PHPUNIT_RUNNER') || defined('SUGARCRM_INSTALL')) {
            return false;
        }
    }

    return true;
}

/**
 * Zip specified directory
 *
 * @param $zip_dir
 * @param $zip_archive
 * @return bool
 */
function zip_dir($zip_dir, $zip_archive)
{
    if (!is_dir($zip_dir)) {
        LoggerManager::getLogger()->fatal('Specified directory for zip file extraction does not exist.');

        return false;
    }
    $zip = new ZipArchive();
    // We need realpath here for PHP streams support
    $zip->open(UploadFile::realpath($zip_archive),
        ZipArchive::CREATE | ZipArchive::OVERWRITE);
    $path = UploadFile::realpath($zip_dir);
    $chop = strlen((string) $path) + 1;
    $dir = new RecursiveDirectoryIterator($path);
    foreach (new RecursiveIteratorIterator($dir, RecursiveIteratorIterator::SELF_FIRST) as $fileinfo) {
        // Bug # 45143
        // ensure that . and .. are not zipped up, otherwise, the
        // CENT OS and others will fail when deploying module
        $fileName = $fileinfo->getFilename();
        if ($fileName === '.' || $fileName === '..') {
            continue;
        }
        $localname = str_replace("\\", '/', substr((string) $fileinfo->getPathname(), $chop));
        if ($fileinfo->isDir()) {
            $zip->addEmptyDir($localname . '/');
        } else {
            $zip->addFile($fileinfo->getPathname(), $localname);
        }
    }

    return true;
}

/**
 * Zip list of files, optionally stripping prefix
 *
 * @param string $zip_file
 * @param array $file_list
 * @param string $prefix
 * @return bool
 */
function zip_files_list($zip_file, $file_list, $prefix = '')
{
    $archive = new ZipArchive();
    // We need realpath here for PHP streams support
    $res = $archive->open(UploadFile::realpath($zip_file),
        ZipArchive::CREATE | ZipArchive::OVERWRITE);
    if ($res !== true) {
        LoggerManager::getLogger()->fatal("Unable to open zip file, check directory permissions: $zip_file");

        return false;
    }
    foreach ($file_list as $file) {
        if (!empty($prefix) && preg_match($prefix, (string) $file, $matches) > 0) {
            $zipname = substr((string) $file, strlen($matches[0]));
        } else {
            $zipname = $file;
        }
        $archive->addFile($file, $zipname);
    }

    return true;
}
