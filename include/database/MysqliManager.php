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

/*********************************************************************************
 * Description: This file handles the Data base functionality for the application.
 * It acts as the DB abstraction layer for the application. It depends on helper classes
 * which generate the necessary SQL. This sql is then passed to PEAR DB classes.
 * The helper class is chosen in DBManagerFactory, which is driven by 'db_type' in 'dbconfig' under config.php.
 *
 * All the functions in this class will work with any bean which implements the meta interface.
 * The passed bean is passed to helper class which uses these functions to generate correct sql.
 *
 * The meta interface has the following functions:
 * getTableName()                Returns table name of the object.
 * getFieldDefinitions()         Returns a collection of field definitions in order.
 * getFieldDefintion(name)       Return field definition for the field.
 * getFieldValue(name)           Returns the value of the field identified by name.
 *                               If the field is not set, the function will return boolean FALSE.
 * getPrimaryFieldDefinition()   Returns the field definition for primary key
 *
 * The field definition is an array with the following keys:
 *
 * name      This represents name of the field. This is a required field.
 * type      This represents type of the field. This is a required field and valid values are:
 *           �   int
 *           �   long
 *           �   varchar
 *           �   text
 *           �   date
 *           �   datetime
 *           �   double
 *           �   float
 *           �   uint
 *           �   ulong
 *           �   time
 *           �   short
 *           �   enum
 * length    This is used only when the type is varchar and denotes the length of the string.
 *           The max value is 255.
 * enumvals  This is a list of valid values for an enum separated by "|".
 *           It is used only if the type is �enum�;
 * required  This field dictates whether it is a required value.
 *           The default value is �FALSE�.
 * isPrimary This field identifies the primary key of the table.
 *           If none of the fields have this flag set to �TRUE�,
 *           the first field definition is assume to be the primary key.
 *           Default value for this field is �FALSE�.
 * default   This field sets the default value for the field definition.
 *
 *
 * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
 * All Rights Reserved.
 * Contributor(s): ______________________________________..
 ********************************************************************************/

require_once('include/database/MysqlManager.php');

/**
 * MySQL manager implementation for mysqli extension
 */
class MysqliManager extends MysqlManager
{
    /**
     * @see DBManager::$dbType
     */
    public $dbType = 'mysql';
    public $variant = 'mysqli';
    public $priority = 10;
    public $label = 'LBL_MYSQLI';

    /**
     * @see DBManager::$backendFunctions
     */
    protected $backendFunctions = array(
        'free_result' => 'mysqli_free_result',
        'close' => 'mysqli_close',
        'row_count' => 'mysqli_num_rows',
        'affected_row_count' => 'mysqli_affected_rows',
    );

    /**
     * @see MysqlManager::query()
     */
    public function query($sql, $dieOnError = false, $msg = '', $suppress = false, $keepResult = false)
    {
        $result = null;
        if (is_array($sql)) {
            return $this->queryArray($sql, $dieOnError, $msg, $suppress);
        }

        static $queryMD5 = array();

        parent::countQuery($sql);
        $GLOBALS['log']->info('Query:' . $sql);
        $this->checkConnection();
        $this->query_time = microtime(true);
        $this->lastsql = $sql;
        if (!empty($sql)) {
            if ($this->database instanceof mysqli) {
                $result = false;
                try {
                    $result = mysqli_query($this->database, $sql);
                } catch (Exception $e) {
                }

                if ($result === false && !$suppress) {
                    if (inDeveloperMode()) {
                        LoggerManager::getLogger()->debug('Mysqli_query failed, error was: ' . $this->lastDbError() . ', query was: ');
                    }
                    LoggerManager::getLogger()->fatal('Mysqli_query failed.');
                }
            } else {
                LoggerManager::getLogger()->fatal('Database error: Incorrect link');
            }
        } else {
            $GLOBALS['log']->fatal('MysqliManager: Empty query');
            $result = null;
        }
        $md5 = md5($sql);

        if (empty($queryMD5[$md5])) {
            $queryMD5[$md5] = true;
        }

        $this->query_time = microtime(true) - $this->query_time;
        $GLOBALS['log']->info('Query Execution Time:' . $this->query_time);
        $this->dump_slow_queries($sql);

        // This is some heavy duty debugging, leave commented out unless you need this:
        /*
        $bt = debug_backtrace();
        for ( $i = count($bt) ; $i-- ; $i > 0 ) {
            if ( strpos('MysqliManager.php',$bt[$i]['file']) === false ) {
                $line = $bt[$i];
            }
        }

        $GLOBALS['log']->fatal("${line['file']}:${line['line']} ${line['function']} \nQuery: $sql\n");
        */


        if ($keepResult) {
            $this->lastResult = $result;
        }
        $this->checkError($msg . ' Query Failed: ' . $sql, $dieOnError);

        return $result;
    }

    /**
     * Returns the number of rows affected by the last query
     *
     * @return int
     */
    public function getAffectedRowCount($result)
    {
        return mysqli_affected_rows($this->getDatabase());
    }

    /**
     * Returns the number of rows returned by the result
     *
     * This function can't be reliably implemented on most DB, do not use it.
     * @abstract
     * @deprecated
     * @param  resource $result
     * @return int
     */
    public function getRowCount($result)
    {
        return mysqli_num_rows($result);
    }


    /**
     * Disconnects from the database
     *
     * Also handles any cleanup needed
     */
    public function disconnect()
    {
        if (isset($GLOBALS['log']) && !is_null($GLOBALS['log'])) {
            $GLOBALS['log']->debug('Calling MySQLi::disconnect()');
        }
        if (!empty($this->database)) {
            $this->freeResult();
            if (!@mysqli_close($this->database)) {
                $GLOBALS['log']->fatal('mysqli_close() failed');
            }
            $this->database = null;
        }
    }

    /**
     * @see DBManager::freeDbResult()
     */
    protected function freeDbResult($dbResult)
    {
        if (!empty($dbResult)) {
            mysqli_free_result($dbResult);
        }
    }

    /**
     * @see DBManager::getFieldsArray()
     */
    public function getFieldsArray($result, $make_lower_case = false)
    {
        $field_array = array();

        if (!isset($result) || empty($result)) {
            return 0;
        }

        $i = 0;
        while ($i < mysqli_num_fields($result)) {
            $meta = mysqli_fetch_field_direct($result, $i);
            if (!$meta) {
                return 0;
            }

            if ($make_lower_case == true) {
                $meta->name = strtolower($meta->name);
            }

            $field_array[] = $meta->name;

            $i++;
        }

        return $field_array;
    }

    /**
     * @see DBManager::fetchRow()
     */
    public function fetchRow($result)
    {
        if (empty($result)) {
            return false;
        }

        $row = mysqli_fetch_assoc($result);
        if ($row == null) {
            $row = false;
        } //Make sure MySQLi driver results are consistent with other database drivers

        return $row;
    }

    /**
     * @see DBManager::quote()
     */
    public function quote($string)
    {
        return mysqli_real_escape_string($this->getDatabase(), $this->quoteInternal($string));
    }

    /**
     * @see DBManager::connect()
     */
    public function connect(array $configOptions = null, $dieOnError = false)
    {
        global $sugar_config;

        if (is_null($configOptions)) {
            $configOptions = $sugar_config['dbconfig'];
        }

        if (!isset($this->database)) {

            //mysqli connector has a separate parameter for port.. We need to separate it out from the host name
            $dbhost = $configOptions['db_host_name'];
            $dbport = isset($configOptions['db_port']) ? ($configOptions['db_port'] == '' ? null : $configOptions['db_port']) : null;

            $pos = strpos($configOptions['db_host_name'], ':');
            if ($pos !== false) {
                $dbhost = substr($configOptions['db_host_name'], 0, $pos);
                $dbport = substr($configOptions['db_host_name'], $pos + 1);
            }

            $this->database = @mysqli_connect(
                $dbhost,
                $configOptions['db_user_name'],
                $configOptions['db_password'],
                isset($configOptions['db_name']) ? $configOptions['db_name'] : '',
                $dbport
            );
            if (empty($this->database)) {
                $GLOBALS['log']->fatal("Could not connect to DB server " . $dbhost . " as " . $configOptions['db_user_name'] . ". port " . $dbport . ": " . mysqli_connect_error());
                if ($dieOnError) {
                    if (isset($GLOBALS['app_strings']['ERR_NO_DB'])) {
                        sugar_die($GLOBALS['app_strings']['ERR_NO_DB']);
                    } else {
                        sugar_die("Could not connect to the database. Please refer to suitecrm.log for details (2).");
                    }
                } else {
                    return false;
                }
            }
        }

        if (!empty($configOptions['db_name']) && !@mysqli_select_db($this->database, $configOptions['db_name'])) {
            $GLOBALS['log']->fatal("Unable to select database {$configOptions['db_name']}: " . mysqli_connect_error());
            if ($dieOnError) {
                if (isset($GLOBALS['app_strings']['ERR_NO_DB'])) {
                    sugar_die($GLOBALS['app_strings']['ERR_NO_DB']);
                } else {
                    sugar_die("Could not connect to the database. Please refer to suitecrm.log for details (2).");
                }
            } else {
                return false;
            }
        }

        $collation = $this->getOption('collation');
        $charset = $this->getCharset();

        if (!empty($collation) && !empty($charset)) {
            $names = 'SET NAMES ' . $this->quoted($charset) . ' COLLATE ' . $this->quoted($collation);
            mysqli_query($this->database, $names);
        }

        if (!empty($charset)) {
            mysqli_set_charset($this->database, $charset);
	    }

        // https://github.com/salesagility/SuiteCRM/issues/7107
        // MySQL 5.7 is stricter regarding missing values in SQL statements and makes some tests fail.
        // Remove STRICT_TRANS_TABLES from sql_mode so we get the old behaviour again.
        mysqli_query($this->database, "SET SESSION sql_mode=(SELECT REPLACE(@@sql_mode, 'STRICT_TRANS_TABLES', ''))");

        if ($this->checkError('Could Not Connect', $dieOnError)) {
            $GLOBALS['log']->info("connected to db");
        }

        $this->connectOptions = $configOptions;

        return true;
    }

    /**
     * (non-PHPdoc)
     * @see MysqlManager::lastDbError()
     */
    public function lastDbError()
    {
        if ($this->database) {
            if (mysqli_errno($this->database)) {
                return "MySQL error " . mysqli_errno($this->database) . ": " . mysqli_error($this->database);
            }
        } else {
            $err = mysqli_connect_error();
            if ($err) {
                return $err;
            }
        }

        return false;
    }

    public function getDbInfo()
    {
        $charsets = $this->getCharsetInfo();
        $charset_str = array();
        foreach ($charsets as $name => $value) {
            $charset_str[] = "$name = $value";
        }

        return array(
            "MySQLi Version" => @mysqli_get_client_info(),
            "MySQLi Host Info" => @mysqli_get_host_info($this->database),
            "MySQLi Server Info" => @mysqli_get_server_info($this->database),
            "MySQLi Client Encoding" => @mysqli_character_set_name($this->database),
            "MySQL Character Set Settings" => implode(", ", $charset_str),
        );
    }

    /**
     * Select database
     * @param string $dbname
     */
    protected function selectDb($dbname)
    {
        return mysqli_select_db($this->getDatabase(), $dbname);
    }

    /**
     * Check if this driver can be used
     * @return bool
     */
    public function valid()
    {
        return function_exists("mysqli_connect") && empty($GLOBALS['sugar_config']['mysqli_disabled']);
    }

    public function compareVarDefs($fielddef1, $fielddef2, $ignoreName = false)
    {
        /**
         * Int lengths are ignored in MySQL versions >= 8.0.19 so we need to ignore when comparing vardefs.
         */
        if($fielddef1['type'] == 'int') {
            $db_version = $this->version();
            if (!empty($db_version)
                && version_compare($db_version, '8.0.19') >= 0
                && strpos($db_version, "MariaDB") === false
            ) {
                unset($fielddef2['len']);
            }
        }
        return parent::compareVarDefs($fielddef1, $fielddef2, $ignoreName);
    }
}
