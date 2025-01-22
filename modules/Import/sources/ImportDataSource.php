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


require_once('modules/Import/ImportCacheFiles.php');



#[\AllowDynamicProperties]
abstract class ImportDataSource implements Iterator
{
    /**
     * The current offset the data set should start at
     */
    protected $_offset = 0;

    /**
     * Count of rows processed
     */
    protected $_rowsCount = 0;

    /**
     * True if the current row has already had an error it in, so we don't increase the $_errorCount
     */
    protected $_rowCountedForErrors = false;

    /**
     * Count of rows with errors
     */
    private $_errorCount = 0;

    /**
     * Count of duplicate rows
     */
    private $_dupeCount = 0;

    /**
     * Count of newly created rows
     */
    private $_createdCount = 0;

    /**
     * Count of updated rows
     */
    private $_updatedCount = 0;

    /**
     * Sourcename used as an identifier for this import
     */
    protected $_sourcename;

    /**
     * Array of the values in the current array we are in
     */
    protected $_currentRow = false;

    /**
     * Holds any locale settings needed for import.  These can be provided by the user
     * or explicitly set by the user.
     */
    protected $_localeSettings = array();

    /**
     * Stores a subset or entire portion of the data set requested.
     */
    protected $_dataSet = array();

    /**
     * Return a result set from the external source as an associative array with the key value equal to the
     * external field name and the rvalue equal to the actual value.
     *
     * @abstract
     * @param  int $startIndex
     * @param  int $maxResults
     * @return void
     */
    abstract public function loadDataSet($maxResults = 0);

    /**
     * Return the total count of records that will be imported.
     *
     * @abstract
     * @return int
     */
    abstract public function getTotalRecordCount();

    /**
     * @abstract
     * @return void
     */
    abstract public function getHeaderColumns();

    /**
     * Set the source name.
     *
     * @param  $sourceName
     * @return void
     */
    public function setSourceName($sourceName = '')
    {
        $this->_sourcename = $sourceName;
    }


    /**
     * Set the current offset.
     *
     * @param $offset
     * @return void
     */
    public function setCurrentOffset($offset)
    {
        $this->_offset = $offset;
    }

    /**
     * Return the current offset
     *
     * @return int
     */
    public function getCurrentOffset()
    {
        return $this->_offset;
    }

    /**
     * Return the current data set loaded.
     *
     * @return array
     */
    public function getDataSet()
    {
        return $this->_dataSet;
    }

    /**
     * Add this row to the UsersLastImport table
     *
     * @param string $import_module name of the module we are doing the import into
     * @param string $module        name of the bean we are creating for this import
     * @param string $id            id of the recorded created in the $module
     */
    public static function writeRowToLastImport($import_module, $module, $id)
    {
        // cache $last_import instance
        static $last_import;

        if (!($last_import instanceof UsersLastImport)) {
            $last_import = BeanFactory::newBean('Import_2');
        }

        $last_import->id = null;
        $last_import->deleted = null;
        $last_import->assigned_user_id = $GLOBALS['current_user']->id;
        $last_import->import_module = $import_module;
        if ($module == 'Case') {
            $module = 'aCase';
        }

        $last_import->bean_type = $module;
        $last_import->bean_id = $id;
        return $last_import->save();
    }


    /**
     * Writes the row out to the ImportCacheFiles::getErrorFileName() file
     *
     * @param $error string
     * @param $fieldName string
     * @param $fieldValue mixed
     */
    public function writeError($error, $fieldName, $fieldValue)
    {
        $fp = sugar_fopen(ImportCacheFiles::getErrorFileName(), 'a');
        fputcsv($fp, array($error,$fieldName,$fieldValue,$this->_rowsCount));
        fclose($fp);

        if (!$this->_rowCountedForErrors) {
            $this->_errorCount++;
            $this->_rowCountedForErrors = true;
            $this->writeErrorRecord($this->formatErrorMessage($error, $fieldName, $fieldValue));
        }
    }

    protected function formatErrorMessage($error, $fieldName, $fieldValue)
    {
        global $current_language;
        $mod_strings = return_module_language($current_language, 'Import');
        return "{$mod_strings['LBL_ERROR']}: $error. ".
               "- {$mod_strings['LBL_FIELD_NAME']}: $fieldName.  " .
               "- {$mod_strings['LBL_VALUE']}: $fieldValue";
    }
    public function resetRowErrorCounter()
    {
        $this->_rowCountedForErrors = false;
    }

    /**
     * Writes the totals and filename out to the ImportCacheFiles::getStatusFileName() file
     */
    public function writeStatus()
    {
        $fp = sugar_fopen(ImportCacheFiles::getStatusFileName(), 'a');
        $statusData = array($this->_rowsCount,$this->_errorCount,$this->_dupeCount,
                            $this->_createdCount,$this->_updatedCount,$this->_sourcename);
        fputcsv($fp, $statusData);
        fclose($fp);
    }

    /**
     * Writes the row out to the ImportCacheFiles::getDuplicateFileName() file
     */
    public function markRowAsDuplicate($field_names=array())
    {
        $fp = sugar_fopen(ImportCacheFiles::getDuplicateFileName(), 'a');
        fputcsv($fp, $this->_currentRow);
        fclose($fp);

        //if available, grab the column number based on passed in field_name
        if (!empty($field_names)) {
            $colkey = '';
            $colnums = array();

            //REQUEST should have the field names in order as they appear in the row to be written, get the key values
            //of passed in fields into an array
            foreach ($field_names as $fv) {
                $fv = trim($fv);
                if (empty($fv) || $fv == 'delete') {
                    continue;
                }
                $new_keys = array_keys($_REQUEST, $fv);
                $colnums = array_merge($colnums, $new_keys);
            }


            //if values were found, process for number position
            if (!empty($colnums)) {
                //foreach column, strip the 'colnum_' prefix to the get the column key value
                foreach ($colnums as $column_key) {
                    if (strpos($column_key, 'colnum_') === 0) {
                        $colkey = substr($column_key, 7);
                    }

                    //if we have the column key, then lets add a span tag with styling reference to the original value
                    if (!empty($colkey)) {
                        $hilited_val = $this->_currentRow[$colkey];
                        $this->_currentRow[$colkey]= '<span class=warn>'.$hilited_val.'</span>';
                    }
                }
            }
        }

        //add the row (with or without stylings) to the list view, this will get displayed to the user as a list of duplicates
        $fdp = sugar_fopen(ImportCacheFiles::getDuplicateFileDisplayName(), 'a');
        fputcsv($fdp, $this->_currentRow);
        fclose($fdp);

        //increment dupecount
        $this->_dupeCount++;
    }

    /**
     * Marks whether this row created a new record or not
     *
     * @param $createdRecord bool true if record is created, false if it is just updated
     */
    public function markRowAsImported($createdRecord = true)
    {
        if ($createdRecord) {
            ++$this->_createdCount;
        } else {
            ++$this->_updatedCount;
        }
    }

    /**
     * Writes the row out to the ImportCacheFiles::getErrorRecordsFileName() file
     */
    public function writeErrorRecord($errorMessage = '')
    {
        $rowData = !$this->_currentRow ? array() : $this->_currentRow;
        $fp = sugar_fopen(ImportCacheFiles::getErrorRecordsFileName(), 'a');
        $fpNoErrors = sugar_fopen(ImportCacheFiles::getErrorRecordsWithoutErrorFileName(), 'a');

        //Write records only for download without error message.
        fputcsv($fpNoErrors, $rowData);

        //Add the error message to the first column
        array_unshift($rowData, $errorMessage);
        fputcsv($fp, $rowData);

        fclose($fp);
        fclose($fpNoErrors);
    }

    public function __get($var)
    {
        if (isset($_REQUEST[$var])) {
            return $_REQUEST[$var];
        } elseif (isset($this->_localeSettings[$var])) {
            return $this->_localeSettings[$var];
        } elseif (isset($this->$var)) {
            return $this->$var;
        }
        return null;
    }
}
