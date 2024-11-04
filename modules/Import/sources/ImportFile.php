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

 * Description: Class to handle processing an import file
 * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
 * All Rights Reserved.
 */

require_once('modules/Import/CsvAutoDetect.php');
require_once('modules/Import/sources/ImportDataSource.php');

#[\AllowDynamicProperties]
class ImportFile extends ImportDataSource
{
    /**
     * Stores whether or not we are deleting the import file in the destructor
     */
    private $_deleteFile;

    /**
     * File pointer returned from fopen() call
     */
    private $_fp = false;

    /**
     * True if the csv file has a header row.
     */
    private $_hasHeader = false;

    /**
     * True if the csv file has a header row.
     */
    private $_detector = null;

    /**
     * CSV date format
     */
    private $_date_format = false;

    /**
     * CSV time format
     */
    private $_time_format = false;

    /**
     * The import file map that this import file inherits properties from.
     */
    private $_importFile = null;

    /**
     * Delimiter string we are using (i.e. , or ;)
     */
    private $_delimiter;

    /**
     * Enclosure string we are using (i.e. ' or ")
     */
    private $_enclosure;

    /**
     * File encoding, used to translate the data into UTF-8 for display and import
     */
    private $_encoding;


    /**
     * Constructor
     *
     * @param string $filename
     * @param string $delimiter
     * @param string $enclosure
     * @param bool   $deleteFile
     */
    public function __construct($filename, $delimiter  = ',', $enclosure  = '', $deleteFile = true, $checkUploadPath = true)
    {
        if (!is_file($filename) || !is_readable($filename)) {
            return false;
        }

        if ($checkUploadPath && UploadStream::path($filename) == null) {
            $GLOBALS['log']->fatal("ImportFile detected attempt to access to the following file not within the sugar upload dir: $filename");
            return null;
        }


        $this->_fp         = sugar_fopen($filename, 'r');
        $this->_sourcename   = $filename;
        $this->_deleteFile = $deleteFile;
        $this->_delimiter  = (empty($delimiter) ? ',' : $delimiter);
        if ($this->_delimiter == '\t') {
            $this->_delimiter = "\t";
        }
        $this->_enclosure  = (empty($enclosure) ? '' : trim($enclosure));

        // Autodetect does setFpAfterBOM()
        $this->_encoding = $this->autoDetectCharacterSet();
    }

    /**
     * Remove the BOM (Byte Order Mark) from the beginning of the import row if it exists
     * @return void
     */
    private function setFpAfterBOM()
    {
        if ($this->_fp === false) {
            return;
        }

        rewind($this->_fp);
        $bomCheck = fread($this->_fp, 3);
        if ($bomCheck != pack("CCC", 0xef, 0xbb, 0xbf)) {
            rewind($this->_fp);
        }
    }
    /**
     * Destructor
     *
     * Deletes $_importFile if $_deleteFile is true
     */
    public function __destruct()
    {
        if ($this->_deleteFile && $this->fileExists()) {
            fclose($this->_fp);
            //Make sure the file exists before unlinking
            if (file_exists($this->_sourcename)) {
                unlink($this->_sourcename);
            }
        }

    }

    /**
     * This is needed to prevent unserialize vulnerability
     */
    public function __wakeup()
    {
        // clean all properties
        foreach (get_object_vars($this) as $k => $v) {
            $this->$k = null;
        }
        throw new Exception("Not a serializable object");
    }

    /**
     * Returns true if the filename given exists and is readable
     *
     * @return bool
     */
    public function fileExists()
    {
        return !$this->_fp ? false : true;
    }

    /**
     * Gets the next row from $_importFile
     *
     * @return array current row of file
     */
    public function getNextRow()
    {
        $this->_currentRow = false;

        if (!$this->fileExists()) {
            return false;
        }

        // explode on delimiter instead if enclosure is an empty string
        if (empty($this->_enclosure)) {
            $row = explode($this->_delimiter, rtrim(fgets($this->_fp, 8192), "\r\n"));
            if ($row !== false && !(count($row) == 1 && trim($row[0]) == '')) {
                $this->_currentRow = $row;
            } else {
                return false;
            }
        } else {
            $row = fgetcsv($this->_fp, 8192, $this->_delimiter, $this->_enclosure);
            if ($row !== false && $row != array(null)) {
                $this->_currentRow = $row;
            } else {
                return false;
            }
        }

        global $locale;
        foreach ($this->_currentRow as $key => $value) {
            // If encoding is set, convert all values from it
            if (!empty($this->_encoding)) {
                // Convert all values to UTF-8 for display and import purposes
                $this->_currentRow[$key] = $locale->translateCharset($value, $this->_encoding);
            }

            $this->_currentRow[$key] = securexss($value);

            // Convert all line endings to the same style as PHP_EOL
            // Use preg_replace instead of str_replace as str_replace may cause extra lines on Windows
            $this->_currentRow[$key] = preg_replace("[\r\n|\n|\r]", PHP_EOL, (string) $this->_currentRow[$key]);
        }

        $this->_rowsCount++;

        return $this->_currentRow;
    }

    /**
     * Returns the number of fields in the current row
     *
     * @return int count of fiels in the current row
     */
    public function getFieldCount()
    {
        return is_countable($this->_currentRow) ? count($this->_currentRow) : 0;
    }

    /**
     * Determine the number of lines in this file.
     *
     * @return int
     */
    public function getNumberOfLinesInfile()
    {
        $lineCount = 0;

        if ($this->_fp) {
            rewind($this->_fp);
            while (!feof($this->_fp)) {
                if (fgets($this->_fp) !== false) {
                    $lineCount++;
                }
            }
            //Reset the fp to after the bom if applicable.
            $this->setFpAfterBOM();
        }

        return $lineCount;
    }

    //TODO: Add auto detection for field delim and qualifier properteis.
    public function autoDetectCSVProperties()
    {
        // defaults
        $this->_delimiter  = ",";
        $this->_enclosure  = '"';

        $this->_detector = new CsvAutoDetect($this->_sourcename);

        $delimiter = $enclosure = false;

        $ret = $this->_detector->getCsvSettings($delimiter, $enclosure);
        if ($ret) {
            $this->_delimiter = $delimiter;
            $this->_enclosure = $enclosure;
            return true;
        } else {
            return false;
        }
    }

    public function getFieldDelimeter()
    {
        return $this->_delimiter;
    }

    public function getFieldEnclosure()
    {
        return $this->_enclosure;
    }

    public function autoDetectCharacterSet()
    {
        // If encoding is already detected, just return it
        if (!empty($this->_encoding)) {
            return $this->_encoding;
        }

        // Move file pointer to start
        $this->setFpAfterBOM();

        global $locale;
        $user_charset = $locale->getExportCharset();
        $system_charset = $locale->default_export_charset;
        $other_charsets = 'UTF-8, UTF-7, ASCII, CP1252, EUC-JP, SJIS, eucJP-win, SJIS-win, JIS, ISO-2022-JP';
        $detectable_charsets = "UTF-8, {$user_charset}, {$system_charset}, {$other_charsets}";

        // Bug 26824 - mb_detect_encoding() thinks CP1252 is IS0-8859-1, so use that instead in the encoding list passed to the function
        $detectable_charsets = str_replace('CP1252', 'ISO-8859-1', $detectable_charsets);

        // If we are able to detect encoding
        if (function_exists('mb_detect_encoding')) {
            // Retrieve a sample of data set
            $text = '';

            // Read 10 lines from the file and put them all together in a variable
            $i = 0;
            while ($i < 10 && $temp = fgets($this->_fp, 8192)) {
                $text .= $temp;
                $i++;
            }

            // If we picked any text, try to detect charset
            if (strlen($text) > 0) {
                $charset_for_import = mb_detect_encoding($text, $detectable_charsets);
            }
        }

        // If we couldn't detect the charset, set it to default export/import charset
        if (empty($charset_for_import)) {
            $charset_for_import = $locale->getExportCharset();
        }

        // Reset the fp to after the bom if applicable.
        $this->setFpAfterBOM();

        return $charset_for_import;
    }

    public function getDateFormat()
    {
        if ($this->_detector) {
            $this->_date_format = $this->_detector->getDateFormat();
        }

        return $this->_date_format;
    }

    public function getTimeFormat()
    {
        if ($this->_detector) {
            $this->_time_format = $this->_detector->getTimeFormat();
        }

        return $this->_time_format;
    }

    public function setHeaderRow($hasHeader)
    {
        $this->_hasHeader = $hasHeader;
    }

    public function hasHeaderRow($autoDetect = true)
    {
        if ($autoDetect) {
            if (!isset($_REQUEST['import_module'])) {
                return false;
            }

            $module = $_REQUEST['import_module'];

            $ret = false;
            $heading = false;

            if ($this->_detector) {
                $ret = $this->_detector->hasHeader($heading, $module, $this->_encoding);
            }

            if ($ret) {
                $this->_hasHeader = $heading;
            }
        }
        return $this->_hasHeader;
    }

    public function setImportFileMap($map)
    {
        $this->_importFile = $map;
        $importMapProperties = array('_delimiter' => 'delimiter','_enclosure' => 'enclosure', '_hasHeader' => 'has_header');
        //Inject properties from the import map
        foreach ($importMapProperties as $k => $v) {
            $this->$k = $map->$v;
        }
    }

    //Begin Implementation for SPL's Iterator interface
    public function key(): mixed
    {
        return $this->_rowsCount;
    }

    public function current(): mixed
    {
        return $this->_currentRow;
    }

    public function next(): void
    {
        $this->getNextRow();
    }

    public function valid(): bool
    {
        return $this->_currentRow !== false;
    }

    public function rewind(): void
    {
        $this->setFpAfterBOM();
        //Load our first row
        $this->getNextRow();
    }

    public function getTotalRecordCount()
    {
        $totalCount = $this->getNumberOfLinesInfile();
        if ($this->hasHeaderRow(false) && $totalCount > 0) {
            $totalCount--;
        }
        return $totalCount;
    }

    public function loadDataSet($totalItems = 0)
    {
        $currentLine = 0;
        $this->_dataSet = array();
        $this->rewind();
        //If there's a header don't include it.
        if ($this->hasHeaderRow(false)) {
            $this->next();
        }

        while ($this->valid() &&  $totalItems > (is_countable($this->_dataSet) ? count($this->_dataSet) : 0)) {
            if ($currentLine >= $this->_offset) {
                $this->_dataSet[] = $this->_currentRow;
            }
            $this->next();
            $currentLine++;
        }

        return $this;
    }

    public function getHeaderColumns()
    {
        $this->rewind();
        if ($this->hasHeaderRow(false)) {
            return $this->_currentRow;
        } else {
            return false;
        }
    }
}
