<?php
/**
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

namespace SuiteCRM\Search\Index;

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

use Carbon\Carbon;
use Monolog\Logger;

/**
 * Trait IndexingLockFileTrait allows to read and write lock files dynamically.
 *
 * This is intended to be use with an instance of AbstractIndexer.
 *
 * @property Logger $logger
 */
trait IndexingLockFileTrait
{
    /**
     * Reads the lock file.
     *
     * Returns a Carbon timestamp or `false` if the file could not be found.
     *
     * @return Carbon|false
     */
    private function readLockFile()
    {
        $filename = $this->getLockFile();

        $this->logger->debug("Reading lock file $filename");

        if (!$this->checkFile($filename)) {
            return false;
        }

        $data = file_get_contents($filename);
        $data = (int) $data;

        if (empty($data)) {
            $this->logger->warn('Failed to read lock file. Returning \'false\'.');
            return false;
        }

        $carbon = Carbon::createFromTimestamp($data);

        $this->logger->debug(sprintf("Last logged indexing performed on %s (%s)", $carbon->toDateTimeString(), $carbon->diffForHumans()));

        return $carbon;
    }

    /**
     * Gets the lock file name based on the class name.
     *
     * @return string
     */
    private function getLockFile()
    {
        $cacheFolder = 'cache';

        try {
            $name = (new \ReflectionClass($this))->getShortName();
        } catch (\ReflectionException $exception) {
            $name = str_replace('\\', '.', get_class($this));
        }

        $file = sprintf('%s/%s.lock', $cacheFolder, $name);

        return $file;
    }

    /**
     * Checks if the file is readable.
     *
     * @param string $filename
     *
     * @return bool
     */
    private function checkFile($filename)
    {
        if (!file_exists($filename)) {
            $this->logger->debug('Lock file not found');
            return false;
        }

        if (!is_readable($filename)) {
            $this->logger->error('Lock file not readable');
            return false;
        }

        return true;
    }

    /** Writes the lock file with the current timestamp to the default location */
    private function writeLockFile()
    {
        $filename = $this->getLockFile();

        $this->logger->debug('Writing lock file to ' . $filename);

        if (file_exists($filename) && !is_writable($filename)) {
            $this->logger->error('Lock file not writable');
            return;
        }

        try {
            $result = file_put_contents($filename, Carbon::now()->timestamp);

            if ($result === false) {
                throw new \RuntimeException('Failed to write lock file!');
            }
        } catch (\Exception $exception) {
            $this->logger->error('Error while writing lock file');
            $this->logger->error($exception);
        }
    }
}
