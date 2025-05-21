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
class ImportFileSplitter
{
    /**
     * Filename of file we are splitting
     */
    private $_sourceFile;

    /**
     * Count of files that we split the $_sourceFile into
     */
    private $_fileCount;

    /**
     * Count of records in $_sourceFile
     */
    private $_recordCount;

    /**
    * Maximum number of records per file
    */
    private $_recordThreshold;

    /**
     * Constructor
     *
     * @param string $source filename we are splitting
     */
    public function __construct(
        $source = null,
        $recordThreshold = 1000
        ) {
        // sanitize crazy values to the default value
        if (!is_int($recordThreshold) || $recordThreshold < 1) {
            //if this is not an int but is still a
            //string representation of a number, then cast to an int
            if (!is_int($recordThreshold) && is_numeric($recordThreshold)) {
                //cast the string to an int
                $recordThreshold = (int)$recordThreshold;
            } else {
                //if not a numeric string, or less than 1, then default to 100
                $recordThreshold = 100;
            }
        }
        $this->_recordThreshold = $recordThreshold;
        $this->_sourceFile      = $source;
    }

    /**
     * Returns true if the filename given exists and is readable
     *
     * @return bool
     */
    public function fileExists()
    {
        if (!is_file($this->_sourceFile) || !is_readable($this->_sourceFile)) {
            return false;
        }

        return true;
    }

    /**
     * Actually split the file into parts
     *
     * @param string $delimiter
     * @param string $enclosure
     * @param bool $has_header true if file has a header row
     */
    public function splitSourceFile(
        $delimiter = ',',
        $enclosure = '"',
        $has_header = false
        ) {
        if (!$this->fileExists()) {
            return false;
        }
        $importFile = new ImportFile($this->_sourceFile, $delimiter, $enclosure, false);
        $filecount = 0;
        $fw = sugar_fopen("{$this->_sourceFile}-{$filecount}", "w");
        $count = 0;
        // skip first row if we have a header row
        if ($has_header && $importFile->getNextRow()) {
            // mark as duplicate to stick header row in the dupes file
            // STIC-Code MHP
            // $importFile->markRowAsDuplicate();
            // same for error records file
            // $importFile->writeErrorRecord();
            $importFile->writeRecordWithError('', true);
            // END STIC-Code
        }
        while ($row = $importFile->getNextRow()) {
            // after $this->_recordThreshold rows, close this import file and goto the next one
            if ($count >= $this->_recordThreshold) {
                fclose($fw);
                $filecount++;
                $fw = sugar_fopen("{$this->_sourceFile}-{$filecount}", "w");
                $count = 0;
            }
            // Bug 25119: Trim the enclosure string to remove any blank spaces that may have been added.
            $enclosure = trim($enclosure);
            if (!empty($enclosure)) {
                foreach ($row as $key => $v) {
                    $row[$key] = str_replace($enclosure, $enclosure.$enclosure, (string) $v);
                }
            }
            $line = $enclosure.implode($enclosure.$delimiter.$enclosure, $row).$enclosure.PHP_EOL;
            //Would normally use fputcsv() here. But when enclosure character is used and the field value doesn't include delimiter, enclosure, escape character, "\n", "\r", "\t", or " ", php default function 'fputcsv' will not use enclosure for this string.
            fwrite($fw, $line);
            $count++;
        }

        fclose($fw);
        $this->_fileCount   = $filecount;
        $this->_recordCount = ($filecount * $this->_recordThreshold) + $count;
        // increment by one to get true count of files created
        ++$this->_fileCount;
    }

    /**
     * Return the count of records in the file, if it's been processed with splitSourceFile()
     *
     * @return int count of records in the file
     */
    public function getRecordCount()
    {
        if (!isset($this->_recordCount)) {
            return false;
        }

        return $this->_recordCount;
    }

    /**
     * Return the count of files created by the split, if it's been processed with splitSourceFile()
     *
     * @return int count of files created by the split
     */
    public function getFileCount()
    {
        if (!isset($this->_fileCount)) {
            return false;
        }

        return $this->_fileCount;
    }

    /**
     * Return file name of one of the split files
     *
     * @param int $filenumber which split file we want
     *
     * @return string filename
     */
    public function getSplitFileName(
        $filenumber = 0
        ) {
        if ($filenumber >= $this->getFileCount()) {
            return false;
        }

        return "{$this->_sourceFile}-{$filenumber}";
    }
}
