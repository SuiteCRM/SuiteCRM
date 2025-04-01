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
 * UpgradeRemoval.php
 *
 * This is the base class to support removing files during an upgrade process.
 * To support custom removal of files during an upgrade process take the following steps:
 *
 * 1) Extend this class and save the PHP file name to be the same as the class name
 * 2) Override the getFilesToRemove method to return an Array of files/directories to remove
 * 3) Place this PHP file in custom/scripts/files_to_remove directory of your SugarCRM install
 *
 * The UpgradeRemoval instance will be invoked from the unlinkUpgradeFiles method of uw_utils.php
 */
#[\AllowDynamicProperties]
class UpgradeRemoval
{

    /**
     * @var string minimal version for removal
     */
    public $version = '';

    /**
     * getFilesToRemove
     * Return array of files/directories to remove.  Default implementation returns empty array.
     *
     * @param int $version integer value of original version to be upgraded
     * @return mixed $files Array of files/directories to remove
     */
    public function getFilesToRemove($version)
    {
        return array();
    }

    /**
     * processFilesToRemove
     * This method handles removing the array of files/directories specified.
     *
     * @param mixed $files
     */
    public function processFilesToRemove($files=array())
    {
        if (empty($files) || !is_array($files)) {
            return;
        }
    
        require_once('include/dir_inc.php');
    
        if (!file_exists('custom/backup')) {
            mkdir_recursive('custom/backup');
        }
    
        foreach ($files as $file) {
            if (file_exists($file)) {
                $this->backup($file);
                if (is_dir($file)) {
                    rmdir_recursive($file);
                } else {
                    unlink($file);
                }
            }
        }
    }


    /**
     * backup
     * Private method to handle backing up the file to custom/backup directory
     *
     * @param $file File or directory to backup to custom/backup directory
     */
    protected function backup($file)
    {
        $basename = basename((string) $file);
        $basepath = str_replace($basename, '', (string) $file);

        if (!empty($basepath) && !file_exists('custom/backup/' . $basepath)) {
            mkdir_recursive('custom/backup/' . $basepath);
        }
    
        if (is_dir($file)) {
            copy_recursive($file, 'custom/backup/' . $file);
        } else {
            copy($file, 'custom/backup/' . $file);
        }
    }
}
