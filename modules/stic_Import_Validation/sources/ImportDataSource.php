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


require_once('modules/stic_Import_Validation/ImportCacheFiles.php');



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
     * STIC-Code MHP - Add the $_previousRowCount variable
     * Stores the previous value of the $_rowsCount variable, which will help us to identify when a record in the file has more than one error
     */
    protected $_previousRowCount = 0;

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
     * STIC-Code MHP  
     * Method to set the value of the protected variable: $_currentRow
     *
     * @param  $row
     * @return void
     */
    public function setCurrentRow($row = '')
    {
        $this->_currentRow = $row;
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
        // Avoid Deprecated warning: the $escape parameter must be provided as its default value will change
        // fputcsv($fp, array($error,$fieldName,$fieldValue,$this->_rowsCount));
        fputcsv($fp, array($error,$fieldName,$fieldValue,$this->_rowsCount), ',', '"', '\\');
        fclose($fp);

        if (!$this->_rowCountedForErrors) {
            $this->_errorCount++;
            $this->_rowCountedForErrors = true;
            $this->writeErrorRecord($this->formatErrorMessage($error, $fieldName, $fieldValue));
        }
    }

    
    /**
     * STIC-Code MHP - Adapts the writeError function of the Import module
     * Writes the row out to the ImportCacheFiles::getErrorFileName() file
     *
     * @param $error string
     * @param $fieldName string
     * @param $fieldValue mixed
     */
    public function writeRecordAndErrors($error = "", $fieldName = "", $fieldValue = "", $id = "")
    {
        // STIC-Code MHP - Add condition to Write the error message if a row is detected to have more than one error
        if ((!$this->_rowCountedForErrors) || (($this->_rowsCount == $this->_previousRowCount))) 
        {
            // STIC-Code MHP - If it is the first error detected in a file record, increment the error counter by one
            if (($error != "") && ($fieldName != "") && (($this->_rowsCount != $this->_previousRowCount))) { 
                $this->_errorCount++;
                $_SESSION["stic_ImporValidation"]['modules'][$_REQUEST["import_module"]]['errorCount']++;
            }
            $this->_rowCountedForErrors = true;
            $errorMessage = ($error == "") ? "" : "$fieldName $error";
            $this->writeRecordWithError($errorMessage, false, $id);
        }
    }

    protected function formatErrorMessage($error, $fieldName, $fieldValue)
    {
        global $current_language;
        $mod_strings = return_module_language($current_language, 'stic_Import_Validation');
        return "<b>{$mod_strings['LBL_ERROR']}</b> $error <br/>".
               "<b>{$mod_strings['LBL_FIELD_NAME']}</b> $fieldName <br/>" .
               "<b>{$mod_strings['LBL_VALUE']}</b> $fieldValue <br/>";
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
        // Avoid Deprecated warning: the $escape parameter must be provided as its default value will change
        // fputcsv($fp, $statusData);
        fputcsv($fp, $statusData, ',', '"', '\\');
        fclose($fp);
    }

    /**
     * Writes the row out to the ImportCacheFiles::getDuplicateFileName() file
     */
    public function markRowAsDuplicate($field_names=array())
    {
        $fp = sugar_fopen(ImportCacheFiles::getDuplicateFileName(), 'a');
        // Avoid Deprecated warning: the $escape parameter must be provided as its default value will change
        // fputcsv($fp, $this->_currentRow);
        fputcsv($fp, $this->_currentRow, ',', '"', '\\');
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
        // Avoid Deprecated warning: the $escape parameter must be provided as its default value will change
        // fputcsv($fdp, $this->_currentRow);
        fputcsv($fdp, $this->_currentRow, ',', '"', '\\');
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
            // STIC-Code MHP - increment the correct records counter
            $_SESSION["stic_ImporValidation"]['modules'][$_REQUEST["import_module"]]['createdCount']++;
        } else {
            ++$this->_updatedCount;
        }
    }

    /**
     * Writes the row out to the ImportCacheFiles::getErrorRecordsFileName() file
     */
    // public function writeErrorRecord($errorMessage = '')
    // {
    //     $rowData = !$this->_currentRow ? array() : $this->_currentRow;
    //     $fp = sugar_fopen(ImportCacheFiles::getErrorRecordsFileName(), 'a');
    //     $fpNoErrors = sugar_fopen(ImportCacheFiles::getErrorRecordsWithoutErrorFileName(), 'a');

    //     //Write records only for download without error message.
    //     fputcsv($fpNoErrors, $rowData);

    //     //Add the error message to the first column
    //     array_unshift($rowData, $errorMessage);
    //     fputcsv($fp, $rowData);
        
    //     fclose($fp);
    //     fclose($fpNoErrors);
    // }


    /**
     * STIC-Code MHP
     * Writes the row out to the ImportCacheFiles::getErrorRecordsWithoutErrorFileName() file
     */
    public function writeRecordWithError($errorMessage = '', $header = false, $id = "")
    {
        global $current_language;
        $mod_strings = return_module_language($current_language, 'stic_Import_Validation');
        $rowData = !$this->_currentRow ? array() : $this->_currentRow;

        // Check if we are writing the header or a record
        if ($header) { 
            // Check if a new column needs to be created to add the autogenerated id
            if ($_REQUEST["generateID"] && $_REQUEST["positionForIdColumn"] == 0) {
                // Add ID column at the beginning
                array_push($rowData, translate('LBL_MODULE_NAME', $_REQUEST["import_module"]) . " - ID");
            }

            // Add ERROR column at the beginning
            array_push($rowData, translate('LBL_MODULE_NAME', $_REQUEST["import_module"]) . ": " . $mod_strings['LBL_ERROR_FIELD_VALUE']);
            // Reset _previousRowCount 
            $this->_previousRowCount = 0;
        } else {
            if ($_REQUEST["generateID"]) {
                // If a new column has been created to add the ID
                if ($_REQUEST["positionForIdColumn"] == 0) {
                    // Add the ID obtained from the database
                    if (isset($id) && $id != "") {
                        array_push($rowData, $id);
                    } else {
                        // Add a new ID value
                        $id = create_guid();
                        array_push($rowData, $id);
                    }
                } else {
                    // Check if value already exists in that row 
                    if (empty($rowData[$_REQUEST["positionForIdColumn"]-1])) {
                        $rowData[$_REQUEST["positionForIdColumn"]-1] = create_guid();
                    } 
                }
            }
            // Add ERROR value 
            array_push($rowData, $errorMessage);
        }
        
        // Open file
        $fpNoErrors = sugar_fopen(ImportCacheFiles::getErrorRecordsWithoutErrorFileName(), 'a');

        // Write records with the error message by file.
        if ($this->_rowsCount != $this->_previousRowCount){
            // Avoid Deprecated warning: the $escape parameter must be provided as its default value will change
            // fputcsv($fpNoErrors, $rowData, $_REQUEST["custom_delimiter"], html_entity_decode($_REQUEST["custom_enclosure"], ENT_QUOTES));
            fputcsv($fpNoErrors, $rowData, $_REQUEST["custom_delimiter"], html_entity_decode($_REQUEST["custom_enclosure"], ENT_QUOTES), '\\');
            $this->_previousRowCount = $this->_rowsCount;
        } else { // Being an error on a row already reported, we eliminate the last existing row to concatenate the errors
            // Read content of file
            $contentFile = file(ImportCacheFiles::getErrorRecordsWithoutErrorFileName());
            // Extract last line
            $lastRow = array_pop($contentFile);
            // Get line items using to the separator and delimitertador 
            $columns = str_getcsv($lastRow, $_REQUEST["custom_delimiter"], html_entity_decode($_REQUEST['custom_enclosure'], ENT_QUOTES));
            // Update file content
            file_put_contents(ImportCacheFiles::getErrorRecordsWithoutErrorFileName(), $contentFile);
            // If it is a DUPLICATE error we copy the ID obtained from the database
            global $mod_strings;
            if (strpos($errorMessage, (string) $mod_strings['LBL_ERROR_DUPLICATE_RECORD'])) {
                $idPosition = count($columns) - 2;
                $columns[$idPosition] = $rowData[$idPosition];
            }
            // Concatenate error messages in the same row
            $lastPosition = count($columns) - 1;
            $columns[$lastPosition] = $rowData[$lastPosition] . " && " . trim($columns[$lastPosition], '"');
            // Add error message
            // Avoid Deprecated warning: the $escape parameter must be provided as its default value will change
            // fputcsv($fpNoErrors, $columns, $_REQUEST["custom_delimiter"], html_entity_decode($_REQUEST["custom_enclosure"]));
            fputcsv($fpNoErrors, $columns, $_REQUEST["custom_delimiter"], html_entity_decode($_REQUEST["custom_enclosure"]), '\\');
        }
        
        // Close file
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
