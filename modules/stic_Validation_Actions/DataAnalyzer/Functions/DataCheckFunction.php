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

/**
 * Base class for all functions implemented for data verification processes.
 */
abstract class DataCheckFunction
{
    /* Selector Type Constants */
    public const SELECTOR_INCREMENTAL = 'INCREMENTAL'; // Selector incremental
    public const SELECTOR_SPECIFIC = 'SPECIFIC'; // Specific selector

    /* Constantes de tipos de acción */
    public const ACTION_UPDATE = 'UPDATE'; // Modification action
    public const ACTION_REPORT = 'REPORT'; // Information Action
    public const ACTION_UPDATE_REPORT = 'UPDATE_REPORT'; // Modification action and information

    /* Class Properties */
    /* Property => Required */
    protected $defFields = array('id' => true, // Function identifier
        'class' => true, // Name of the class that defines the function
        'classFile' => true, // Class File Path
        'action' => true, // Type of action of the 'UPDATE', 'REPORT' or 'UPDATE_REPORT' function
        'selector' => true, // Selector type 'INCREMENTAL' or 'SPECIFIC'
        'module' => false, // Main module on which the action is executed
    );

    protected $functionDef = array(); // Array with function definition values
    protected $lang = array(); // Function tags

    /**
     * ============================================================
     * Abstract methods to implement by function classes
     */

    /**
     * DoAction abstract function.
     * Perform the action defined in the function
     * @param $dbResult Result of the database query. It can be a recordset (SELECT) or a Boolean (UPDATE / INSERT)
     * @param $actionBean stic_Validation_Actions Bean of the action in which the function is being executed.
     * @return boolean It will return true in case of success and false in case of error.
     */
    abstract public function doAction($beans, stic_Validation_Actions $actionBean);

    /**
     * End of the abstract methods to be implemented by the function classes
     * ============================================================
     */

    /**
     * Receive an SQL proposal and modify it with the particularities necessary for the function.
     * Most functions should overwrite this method.
     * @param $actionBean Bean of the action in which the function is being executed.
     * @param $proposedSQL Array generated automatically (if possible) with the keys select, from, where and order_by.
     * @return string
     */
    public function prepareSQL(stic_Validation_Actions $actionBean, $proposedSQL)
    {
        $query = '';
        if (is_array($proposedSQL)) {
            if (!empty($proposedSQL['select'])) {$query .= $proposedSQL['select'];}
            if (!empty($proposedSQL['from'])) {$query .= $proposedSQL['from'];}
            if (!empty($proposedSQL['where'])) {$query .= $proposedSQL['where'];}
            if (!empty($proposedSQL['order_by'])) {$query .= $proposedSQL['order_by'];}
        } else {
            $query = $proposedSQL;
        }
        return $query;
    }

    /* Default class constructor */
    public function __construct($functionDef)
    {
        $this->functionDef = $functionDef;

        // Check that the definition fields have values
        foreach ($this->defFields as $field => $required) {
            if ($required && empty($this->functionDef[$field])) {
                throw new InvalidArgumentException("Empty required field [{$field}]");
            }
        }

        // Check that the values ​​of the Selector and Action fields are valid
        if (!self::isValidSelector($this->functionDef['selector'])) {
            throw new InvalidArgumentException("{$field} not valid");
        }
        if (!self::isValidAction($this->functionDef['action'])) {
            throw new InvalidArgumentException("{$field} not valid");
        }
    }

    /* Returns the name of the function */
    public function getName($lang = null)
    {return $this->getLabel('name', $lang);}

    /**
     * Returns the language tag
     * @param $label String     Label to recover
     * @param $lang optional If the language is indicated, the function label for that language is returned,
     *                        If nothing is specified $ GLOBALS ['current_language'] is used;
     * @return String|array
     */
    public function getLabel($label, $lang = null)
    {
        if (empty($lang)) {
            $lang = $GLOBALS['current_language'];
        }
        if ($lang != null) {
            return $this->lang[$lang][mb_strtoupper($label)] ?? null;
        } else {
            return null;
        }
    }

    /**
     * Populate the array of object language
     */
    public function setLang($lang)
    {
        $this->lang = $lang;
    }

    /* Returns the value of private properties */
    public function __get($name)
    {return $this->functionDef[$name];}

    /* Indicates whether a property exists or not */
    public function __isset($name)
    {
        return isset($this->functionDef[$name]);
    }

    /* "Magic" method to clone the object */
    public function __clone()
    {
        $this->defFields = self::dupArray($this->defFields);
        $this->functionDef = self::dupArray($this->functionDef);
        $this->lang = self::dupArray($this->lang);
    }

    /* Returns the list of fields in the class */
    public function getDefFields()
    {
        return array_keys($this->defFields);
    }

    /**
     * Returns a bean with the data from a row in the Database
     * @param array $row Associative array with database fields
     * @param string $module Optional Type of module from which the bean will be created.
     *                                If nothing is indicated, the definition file is used
     * @return mixed Bean|null Null in case of error
     */
    protected function loadBeanFromDBRow($row, $module = '')
    {
        if (empty($module)) {
            $module = $this->module;
        }
        $temp = BeanFactory::getBean($module, $row['id']);
        /*
        If the data in the field array is used, the format of the fields does not match.
        Therefore, it is considered safer to recover the bean from the database.
        $temp = BeanFactory::getBean($module, $row['id']);
        if (is_object($temp)) {
        $temp->setupCustomFields($module);
        $temp->loadFromRow($row);
        }*/
        return $temp;
    }

    /**
     * Returns the bean from the row data
     * @param SugarBean $bean Object that where to load the data. If you already have data and corresponds to those in the row, it will not be reloaded
     * @param array $row Array with bean data
     * @return SugarBean
     */
    protected function loadBean(&$bean, $row, $module = '')
    {
        if (empty($bean) || $bean->id != $row['id']) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Loading bean data ...");
            $bean = $this->loadBeanFromDBRow($row, $module);
        }
        return $bean;
    }

    /**
     * Create a validation result with the information contained in $data
     * @param Array $data Contains information on the validation record to create
     */
    protected function logValidationResult($data)
    {
        global $timedate;
        $validationResult = BeanFactory::newBean('stic_Validation_Results');
        $name = $data['name'] ?? '';
        $validationResult->name = $timedate->now() . ' - ' . $name;
        $validationResult->execution_date = $timedate->now();
        $validationResult->description = $data['description'] ?? '';
        $validationResult->log = $data['log'] ?? '';
        $validationResult->stic_validation_actions_id_c = $data['stic_validation_actions_id'] ?? '';
        $validationResult->schedulers_id_c = $GLOBALS['scheduler_id'] ?? '';
        $validationResult->parent_type = $data['parent_type'] ?? '';
        $validationResult->parent_id = $data['parent_id'] ?? '';
        $validationResult->reviewed = $data['reviewed'] ?? 'no';
        $validationResult->assigned_user_id = $data['assigned_user_id'] ?? '';

        $validationResult->save();
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Created the validation result:" . $validationResult->name);
    }

    /**
     * Remove the obsoletes validation results
     * @param $records Set of records on which the validation action is to be applied
     * @param DBManager $dbObject DBManager object used to perform the query. It allows the function if it is necessary to access error data, affected records, etc.
     * @param String $type Indicates whether the validation action is incremental or complete
     * @param String $validationActionID Validation action id
     */
    public function removeObsoleteValidationResults($records, DBManager $dbObject, String $type, String $validationActionID)
    {
        // Create the query to delete obsoletes registers
        // SPECIFIC actions only have the condition that the ID of the validation action is the one being processed
        $sqlString = "
        UPDATE stic_validation_results SET deleted = 1, date_modified = NOW()
        WHERE deleted = 0 AND stic_validation_actions_id_c = '$validationActionID'";

        // Set the conditions in case the action is incremental
        if ($type == self::SELECTOR_INCREMENTAL) 
        {
            // Create a string with the IDs of these beans
            $recordIds = ''; // Will be used with incremental actions
            while ($row = array_pop($records)) {
                $recordIds .= "'" . $row['id'] . "', ";
            }

            // Remove the last two characters
            if (strlen($recordIds) > 0) {
                $recordIds = substr($recordIds, 0, -2);
            }

            $sqlString .= "
            AND (   reviewed = 'yes' 
                 OR reviewed = 'not_necessary'";
            if (strlen($recordIds) > 0) {
                // // Remove the record if it will be handled in the validation below
                $sqlString .= " 
                OR parent_id IN ($recordIds)";
            }
            $sqlString .= "
            );";
        }

        // Run SQL query
        $dbResult = $dbObject->query($sqlString); 
        $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ': SQL Query to Remove obsoletes validation results: ' . $sqlString);
        if ($dbResult === false) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': Failed to remove obsoletes validation results from validation action whose ID is: ' . $validationActionID);
        } else {
            $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ': Removed obsoletes validation results from validation action whose ID is: ' . $validationActionID);
        }
    }


    /**
     * ============================================================
     * Static class methods
     **/

    /* Indicates whether the type of selector indicated is valid or not */
    public static function isValidSelector($selector)
    {
        return ($selector == self::SELECTOR_INCREMENTAL || $selector == self::SELECTOR_SPECIFIC);
    }

    /* Indicates whether the type of action indicated is valid or not */
    public static function isValidAction($action)
    {
        return ($action == self::ACTION_REPORT ||
            $action == self::ACTION_UPDATE ||
            $action == self::ACTION_UPDATE_REPORT);
    }

    /* Returns a copy of the past array */
    private static function dupArray($array)
    {
        $ref = new ArrayObject($array);
        return $ref->getArrayCopy();
    }

    /**
     * End of static class methods
     * ============================================================
     **/
}
