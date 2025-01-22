<?php
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
 * getFieldDefinitions()            Returns a collection of field definitions in order.
 * getFieldDefintion(name)        Return field definition for the field.
 * getFieldValue(name)            Returns the value of the field identified by name.
 *                            If the field is not set, the function will return boolean FALSE.
 * getPrimaryFieldDefinition()    Returns the field definition for primary key
 *
 * The field definition is an array with the following keys:
 *
 * name        This represents name of the field. This is a required field.
 * type        This represents type of the field. This is a required field and valid values are:
 *            int
 *            long
 *            varchar
 *            text
 *            date
 *            datetime
 *            double
 *            float
 *            uint
 *            ulong
 *            time
 *            short
 *            enum
 * length    This is used only when the type is varchar and denotes the length of the string.
 *            The max value is 255.
 * enumvals  This is a list of valid values for an enum separated by "|".
 *            It is used only if the type is ?enum?;
 * required    This field dictates whether it is a required value.
 *            The default value is ?FALSE?.
 * isPrimary    This field identifies the primary key of the table.
 *            If none of the fields have this flag set to ?TRUE?,
 *            the first field definition is assume to be the primary key.
 *            Default value for this field is ?FALSE?.
 * default    This field sets the default value for the field definition.
 *
 *
 * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
 * All Rights Reserved.
 * Contributor(s): ______________________________________..
 ********************************************************************************/

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

/**
 * SQL Server (mssql) manager
 */
class MssqlManager extends DBManager
{
    /**
     * @see DBManager::$dbType
     */
    public $dbType = 'mssql';
    public $dbName = 'MsSQL';
    public $variant = 'mssql';
    public $label = 'LBL_MSSQL';

    protected $capabilities = array(
        'affected_rows' => true,
        'select_rows' => true,
        'fulltext' => true,
        'limit_subquery' => true,
        'fix:expandDatabase' => true, // Support expandDatabase fix
        'create_user' => true,
        'create_db' => true,
    );

    /**
     * Maximum length of identifiers
     */
    protected $maxNameLengths = array(
        'table' => 128,
        'column' => 128,
        'index' => 128,
        'alias' => 128
    );

    protected $type_map = array(
        'int' => 'int',
        'double' => 'float',
        'float' => 'float',
        'uint' => 'int',
        'ulong' => 'int',
        'long' => 'bigint',
        'short' => 'smallint',
        'varchar' => 'varchar',
        'text' => 'text',
        'longtext' => 'text',
        'date' => 'datetime',
        'enum' => 'varchar',
        'relate' => 'varchar',
        'multienum' => 'text',
        'html' => 'text',
        'emailbody' => 'nvarchar(max)',
        'longhtml' => 'text',
        'datetime' => 'datetime',
        'datetimecombo' => 'datetime',
        'time' => 'datetime',
        'bool' => 'bit',
        'tinyint' => 'tinyint',
        'char' => 'char',
        'blob' => 'image',
        'longblob' => 'image',
        'currency' => 'decimal(26,6)',
        'decimal' => 'decimal',
        'decimal2' => 'decimal',
        'id' => 'varchar(36)',
        'url' => 'varchar',
        'encrypt' => 'varchar',
        'file' => 'varchar',
        'decimal_tpl' => 'decimal(%d, %d)',
    );

    protected $connectOptions;

    /**
     * @see DBManager::connect()
     */
    public function connect(array $configOptions = null, $dieOnError = false)
    {
        global $sugar_config;

        if (is_null($configOptions)) {
            $configOptions = $sugar_config['dbconfig'];
        }

        //SET DATEFORMAT to 'YYYY-MM-DD''
        ini_set('mssql.datetimeconvert', '0');

        //set the text size and textlimit to max number so that blob columns are not truncated
        ini_set('mssql.textlimit', '2147483647');
        ini_set('mssql.textsize', '2147483647');
        ini_set('mssql.charset', 'UTF-8');

        if (!empty($configOptions['db_host_instance'])) {
            $configOptions['db_host_instance'] = trim($configOptions['db_host_instance']);
        }
        //set the connections parameters
        if (empty($configOptions['db_host_instance'])) {
            $connect_param = $configOptions['db_host_name'];
        } else {
            $connect_param = $configOptions['db_host_name'] . "\\" . $configOptions['db_host_instance'];
        }

        //create persistent connection
        if ($this->getOption('persistent')) {
            $this->database = @mssql_pconnect(
                $connect_param,
                $configOptions['db_user_name'],
                $configOptions['db_password']
            );
        }
        //if no persistent connection created, then create regular connection
        if (!$this->database) {
            $this->database = mssql_connect(
                $connect_param,
                $configOptions['db_user_name'],
                $configOptions['db_password']
            );
            if (!$this->database) {
                $GLOBALS['log']->fatal('Could not connect to server ' . $configOptions['db_host_name'] .
                    ' as ' . $configOptions['db_user_name'] . '.');
                if ($dieOnError) {
                    sugar_die($GLOBALS['app_strings']['ERR_NO_DB']);
                } else {
                    return false;
                }
            }
            if ($this->database && $this->getOption('persistent')) {
                $_SESSION['administrator_error'] = '<B>Severe Performance Degradation: Persistent Database Connections '
                    . "not working.  Please set \$sugar_config['dbconfigoption']['persistent'] to false in your "
                    . 'config.php file</B>';
            }
        }
        //make sure connection exists
        if (!$this->database) {
            if ($dieOnError) {
                sugar_die($GLOBALS['app_strings']['ERR_NO_DB']);
            } else {
                return false;
            }
        }

        //select database

        //Adding sleep and retry for mssql connection. We have come across scenarios when
        //an error is thrown.' Unable to select database'. Following will try to connect to
        //mssql db maximum number of 5 times at the interval of .2 second. If can not connect
        //it will throw an Unable to select database message.

        if (!empty($configOptions['db_name']) && !@mssql_select_db($configOptions['db_name'], $this->database)) {
            $connected = false;
            for ($i = 0; $i < 5; $i++) {
                usleep(200000);
                if (@mssql_select_db($configOptions['db_name'], $this->database)) {
                    $connected = true;
                    break;
                }
            }
            if (!$connected) {
                $GLOBALS['log']->fatal("Unable to select database {$configOptions['db_name']}");
                if ($dieOnError) {
                    if (isset($GLOBALS['app_strings']['ERR_NO_DB'])) {
                        sugar_die($GLOBALS['app_strings']['ERR_NO_DB']);
                    } else {
                        sugar_die('Could not connect to the database. Please refer to suitecrm.log for details.');
                    }
                } else {
                    return false;
                }
            }
        }

        if (!$this->checkError('Could Not Connect', $dieOnError)) {
            $GLOBALS['log']->info('connected to db');
        }

        $this->connectOptions = $configOptions;

        $GLOBALS['log']->info('Connect:' . $this->database);

        return true;
    }

    /**
     * @see DBManager::version()
     */
    public function version()
    {
        return $this->getOne('SELECT @@VERSION as version');
    }

    /**
     * @see DBManager::query()
     */
    public function query($sql, $dieOnError = false, $msg = '', $suppress = false, $keepResult = false)
    {
        if (is_array($sql)) {
            return $this->queryArray($sql, $dieOnError, $msg, $suppress);
        }
        // Flag if there are odd number of single quotes
        if (substr_count($sql, "'") & 1) {
            $GLOBALS['log']->error('SQL statement[' . $sql . '] has odd number of single quotes.');
        }

        $sql = $this->_appendN($sql);

        $GLOBALS['log']->info('Query:' . $sql);
        $this->checkConnection();
        $this->countQuery($sql);
        $this->query_time = microtime(true);

        // Bug 34892 - Clear out previous error message by checking the @@ERROR global variable
        @mssql_query('SELECT @@ERROR', $this->database);

        $result = $suppress ? @mssql_query($sql, $this->database) : mssql_query($sql, $this->database);

        if (!$result) {
            $sqlmsg = mssql_get_last_message();
            $sqlpos = strpos((string) $sqlmsg, 'Changed database context to');
            $sqlpos2 = strpos((string) $sqlmsg, 'Warning:');
            $sqlpos3 = strpos((string) $sqlmsg, 'Checking identity information:');

            // if sqlmsg has 'Changed database context to', just log it
            if ($sqlpos !== false || $sqlpos2 !== false || $sqlpos3 !== false) {
                $GLOBALS['log']->debug($sqlmsg . ': ' . $sql);
            } else {
                $GLOBALS['log']->fatal($sqlmsg . ': ' . $sql);
                if ($dieOnError) {
                    sugar_die('SQL Error : ' . $sqlmsg);
                } else {
                    echo 'SQL Error : ' . $sqlmsg;
                }
            }
        }

        $this->query_time = microtime(true) - $this->query_time;
        $GLOBALS['log']->info('Query Execution Time:' . $this->query_time);


        $this->checkError($msg . ' Query Failed: ' . $sql, $dieOnError);

        return $result;
    }

    /**
     * This function take in the sql for a union query, the start and offset,
     * and wraps it around an "mssql friendly" limit query
     *
     * @param  string $sql
     * @param  int $start record to start at
     * @param  int $count number of records to retrieve
     * @return string SQL statement
     */
    private function handleUnionLimitQuery($sql, $start, $count)
    {
        //set the start to 0, no negs
        if ($start < 0) {
            $start = 0;
        }

        $GLOBALS['log']->debug(print_r(func_get_args(), true));

        $this->lastsql = $sql;

        //change the casing to lower for easier string comparison, and trim whitespaces
        $sql = strtolower(trim($sql));

        //set default sql
        $order_by_str = 'order by';

        //make array of order by's.  substring approach was proving too inconsistent
        $orderByArray = explode($order_by_str, $sql);

        //count the number of array elements
        $unionOrderByCount = count($orderByArray);
        $arr_count = 0;

        //process if there are elements
        if ($unionOrderByCount) {
            //we really want the last order by, so reconstruct string
            //adding a 1 to count, as we dont wish to process the last element
            $unionsql = '';
            while ($unionOrderByCount > $arr_count + 1) {
                $unionsql .= $orderByArray[$arr_count];
                ++$arr_count;
                //add an "order by" string back if we are coming into loop again
                //remember they were taken out when array was created
                if ($unionOrderByCount > $arr_count + 1) {
                    $unionsql .= 'order by';
                }
            }
            //grab the last order by element, set both order by's'
            $unionOrderBy = $orderByArray[$arr_count];
            $rowNumOrderBy = $unionOrderBy;

            //if last element contains a "select", then this is part of the union query,
            //and there is no order by to use
            if (strpos($unionOrderBy, 'select')) {
                $unionsql = $sql;
                //with no guidance on what to use for required order by in rownumber function,
                //resort to using name column.
                $rowNumOrderBy = 'id';
                $unionOrderBy = '';
            }
        } else {
            //there are no order by elements, so just pass back string
            $unionsql = $sql;
            //with no guidance on what to use for required order by in rownumber function,
            //resort to using name column.
            $rowNumOrderBy = 'id';
            $unionOrderBy = '';
        }
        //Unions need the column name being sorted on to match across all queries in Union statement
        //so we do not want to strip the alias like in other queries.  Just add the "order by" string and
        //pass column name as is
        if ($unionOrderBy != '') {
            $unionOrderBy = ' order by ' . $unionOrderBy;
        }

        //Bug 56560, use top query in conjunction with rownumber() function
        //to create limit query when paging is needed. Otherwise,
        //it shows duplicates when paging on activities subpanel.
        //If not for paging, no need to use rownumber() function
        if ($count == 1 && $start == 0) {
            $limitUnionSQL = 'SELECT TOP (@topCount) * FROM (' . $unionsql . ') AS top_count ' . $unionOrderBy;
        } else {
            $limitUnionSQL = 'SELECT TOP (@topCount) * FROM( select ROW_NUMBER() OVER ( order by '
                . $rowNumOrderBy . ') AS row_number, * FROM ('
                . $unionsql . ') As numbered) '
                . "As top_count_limit WHERE row_number > $start "
                . $unionOrderBy;
        }

        return $limitUnionSQL;
    }

    /**
     * @see DBManager::limitQuery()
     */
    public function limitQuery($sql, $start, $count, $dieOnError = false, $msg = '', $execute = true)
    {
        $dist_str = 'distinct';
        $start = $start;
        $count = $count;
        $countVar = '(@topCount)';
        $newSQL = $sql;
        $distinctSQLARRAY = array();
        if (strpos($sql, 'UNION') && !preg_match("/(')(UNION).?(')/i", $sql)) {
            $newSQL = $this->handleUnionLimitQuery($sql, $start, $count);
        } else {
            if ($start < 0) {
                $start = 0;
            }
            $GLOBALS['log']->debug(print_r(func_get_args(), true));
            $this->lastsql = $sql;
            $matches = array();
            preg_match('/^(.*SELECT\b)(.*?\bFROM\b.*\bWHERE\b)(.*)$/isU', $sql, $matches);
            if (!empty($matches[3])) {
                if ($start == 0) {
                    $match_two = strtolower($matches[2]);
                    if (!strpos($match_two, 'distinct') > 0 && strpos($match_two, 'distinct') !== 0) {
                        $orderByMatch = array();
                        preg_match('/^(.*)(\bORDER BY\b)(.*)$/is', $matches[3], $orderByMatch);
                        if (!empty($orderByMatch[3])) {
                            $selectPart = array();
                            preg_match('/^(.*)(\bFROM\b.*)$/isU', $matches[2], $selectPart);
                            $newSQL = "SELECT TOP $countVar * FROM
                                (
                                    " . $matches[1] . $selectPart[1] . ', ROW_NUMBER()
                                    OVER (ORDER BY ' . $this->returnOrderBy($sql, $orderByMatch[3]) . ') AS row_number
                                    ' . $selectPart[2] . $orderByMatch[1] . "
                                ) AS a
                                WHERE row_number > $start";
                        } else {
                            $newSQL = $matches[1] . " TOP $countVar " . $matches[2] . $matches[3];
                        }
                    } else {
                        $distinct_o = strpos($match_two, 'distinct');
                        $up_to_distinct_str = substr($match_two, 0, $distinct_o);
                        //check to see if the distinct is within a function, if so, then proceed as normal
                        if (strpos($up_to_distinct_str, '(')) {
                            //proceed as normal
                            $newSQL = $matches[1] . " TOP $countVar " . $matches[2] . $matches[3];
                        } else {
                            //if distinct is not within a function, then parse
                            //string contains distinct clause, "TOP needs to come after Distinct"
                            //get position of distinct
                            $match_zero = strtolower($matches[0]);
                            $distinct_pos = strpos($match_zero, 'distinct');
                            //get position of where
                            $where_pos = strpos($match_zero, 'where');
                            //parse through string
                            $beg = substr($matches[0], 0, $distinct_pos + 9);
                            $mid = substr($matches[0], strlen($beg), ($where_pos + 5) - strlen($beg));
                            $end = substr($matches[0], strlen($beg) + strlen($mid));
                            //repopulate matches array
                            $matches[1] = $beg;
                            $matches[2] = $mid;
                            $matches[3] = $end;

                            $newSQL = $matches[1] . " TOP $countVar " . $matches[2] . $matches[3];
                        }
                    }
                } else {
                    $orderByMatch = array();
                    preg_match('/^(.*)(\bORDER BY\b)(.*)$/is', $matches[3], $orderByMatch);

                    //if there is a distinct clause, parse sql string as we will have to insert the rownumber
                    //for paging, AFTER the distinct clause
                    $grpByStr = [];
                    $hasDistinct = strpos(strtolower($matches[0]), 'distinct');
                    $hasGroupBy = strpos(strtolower($matches[0]), 'group by');

                    require_once 'include/php-sql-parser.php';
                    $parser = new PHPSQLParser();
                    $sqlArray = $parser->parse($sql);

                    if ($hasDistinct) {
                        $matches_sql = strtolower($matches[0]);
                        //remove reference to distinct and select keywords, as we will use a group by instead
                        //we need to use group by because we are introducing
                        // rownumber column which would make every row unique

                        //take out the select and distinct from string so we can reuse in group by
                        $dist_str = 'distinct';
                        preg_match('/\b' . $dist_str . '\b/simU', $matches_sql, $matchesPartSQL, PREG_OFFSET_CAPTURE);
                        $matches_sql = trim(substr($matches_sql, $matchesPartSQL[0][1] + strlen($dist_str)));
                        //get the position of where and from for further processing
                        preg_match('/\bfrom\b/simU', $matches_sql, $matchesPartSQL, PREG_OFFSET_CAPTURE);
                        $from_pos = $matchesPartSQL[0][1];
                        preg_match('/\where\b/simU', $matches_sql, $matchesPartSQL, PREG_OFFSET_CAPTURE);
                        $where_pos = $matchesPartSQL[0][1];
                        //split the sql into a string before and after the from clause
                        //we will use the columns being selected to construct the group by clause
                        if ($from_pos > 0) {
                            $distinctSQLARRAY[0] = substr($matches_sql, 0, $from_pos);
                            $distinctSQLARRAY[1] = substr($matches_sql, $from_pos);
                            //get position of order by (if it exists) so we can strip it from the string
                            $ob_pos = strpos($distinctSQLARRAY[1], 'order by');
                            if ($ob_pos) {
                                $distinctSQLARRAY[1] = substr($distinctSQLARRAY[1], 0, $ob_pos);
                            }

                            // strip off last closing parentheses from the where clause
                            $distinctSQLARRAY[1] = preg_replace('/\)\s$/', ' ', $distinctSQLARRAY[1]);
                        }

                        $grpByStr = array();
                        foreach ($sqlArray['SELECT'] as $record) {
                            if ($record['expr_type'] === 'const') {
                                continue;
                            }
                            $grpByStr[] = trim($record['base_expr']);
                        }
                        $grpByStr = implode(', ', $grpByStr);
                    } elseif ($hasGroupBy) {
                        $groupBy = explode('group by', strtolower($matches[0]));
                        $groupByVars = explode(',', $groupBy[1]);
                        $grpByStr = $groupByVars[0];
                    }

                    if (!empty($orderByMatch[3])) {
                        //if there is a distinct clause, form query with rownumber after distinct
                        if ($hasDistinct) {
                            $newSQL = "SELECT TOP $countVar * FROM
                                        (
                                            SELECT ROW_NUMBER()
                                                OVER (ORDER BY " . preg_replace(
                                '/^' . $dist_str . '\s+/',
                                '',
                                $this->returnOrderBy($sql, $orderByMatch[3])
                            ) . ') AS row_number,
                                                count(*) counter, ' . $distinctSQLARRAY[0] . '
                                                ' . $distinctSQLARRAY[1] . '
                                                group by ' . $grpByStr . "
                                        ) AS a
                                        WHERE row_number > $start";
                        } else {
                            $newSQL = "SELECT TOP $countVar * FROM
                                    (
                                        " . $matches[1] . ' ROW_NUMBER()
                                        OVER (ORDER BY ' . $this->returnOrderBy($sql, $orderByMatch[3]) . ') AS row_number,
                                        ' . $matches[2] . $orderByMatch[1] . "
                                    ) AS a
                                    WHERE row_number > $start";
                        }
                    } else {
                        //if there is a distinct clause, form query with rownumber after distinct
                        if ($hasDistinct) {
                            $newSQL = "SELECT TOP $countVar * FROM
                                            (
                            SELECT ROW_NUMBER() OVER (ORDER BY " . $grpByStr . ') AS row_number, count(*) counter, ' . $distinctSQLARRAY[0] . '
                                                        ' . $distinctSQLARRAY[1] . '
                                                    group by ' . $grpByStr . "
                                            )
                                            AS a
                                            WHERE row_number > $start";
                        } elseif ($hasGroupBy) {
                            $newSQL = "SELECT TOP $countVar * FROM
                                           (
                                  " . $matches[1] . ' ROW_NUMBER() OVER (ORDER BY ' . $grpByStr . ') AS row_number, ' . $matches[2] . $matches[3] . "
                                           )
                                           AS a
                                           WHERE row_number > $start";
                        } else {
                            $newSQL = "SELECT TOP $countVar * FROM
                                           (
                                  " . $matches[1] . ' ROW_NUMBER() OVER (ORDER BY ' . $sqlArray['FROM'][0]['alias'] . '.id) AS row_number, ' . $matches[2] . $matches[3] . "
                                           )
                                           AS a
                                           WHERE row_number > $start";
                        }
                    }
                }
            }
        }

        $newSQL = "DECLARE @topCount INT SET @topCount = $count " . $newSQL;

        $GLOBALS['log']->debug('Limit Query: ' . $newSQL);
        if ($execute) {
            $result = $this->query($newSQL, $dieOnError, $msg);
            $this->dump_slow_queries($newSQL);

            return $result;
        }

        return $newSQL;
    }


    /**
     * Searches for begginning and ending characters.  It places contents into
     * an array and replaces contents in original string.  This is used to account for use of
     * nested functions while aliasing column names
     *
     * @param  string $p_sql SQL statement
     * @param  string $strip_beg Beginning character
     * @param  string $strip_end Ending character
     * @param  string $patt Optional, pattern to
     * @return array
     */
    private function removePatternFromSQL($p_sql, $strip_beg, $strip_end, $patt = 'patt')
    {
        //strip all single quotes out
        $count = substr_count($p_sql, $strip_beg);
        $increment = 1;
        if ($strip_beg != $strip_end) {
            $increment = 2;
        }

        $i = 0;
        $offset = 0;
        $strip_array = array();
        while ($i < $count && $offset < strlen($p_sql)) {
            if ($offset > strlen($p_sql)) {
                break;
            }

            $beg_sin = strpos($p_sql, $strip_beg, $offset);
            if (!$beg_sin) {
                break;
            }
            $sec_sin = strpos($p_sql, $strip_end, $beg_sin + 1);
            $strip_array[$patt . $i] = substr($p_sql, $beg_sin, $sec_sin - $beg_sin + 1);
            if ($increment > 1) {
                //we are in here because beginning and end patterns are not identical, so search for nesting
                $exists = strpos($strip_array[$patt . $i], $strip_beg);
                if ($exists >= 0) {
                    $nested_pos = strrpos($strip_array[$patt . $i], $strip_beg);
                    $strip_array[$patt . $i] = substr(
                        $p_sql,
                        $nested_pos + $beg_sin,
                        $sec_sin - ($nested_pos + $beg_sin) + 1
                    );
                    $p_sql = substr($p_sql, 0, $nested_pos + $beg_sin) . ' ##' . $patt . $i . '## ' . substr(
                        $p_sql,
                        $sec_sin + 1
                    );
                    ++$i;
                    continue;
                }
            }
            $p_len = strlen('##' . $patt . $i . '##');
            $p_sql = substr($p_sql, 0, $beg_sin) . ' ##' . $patt . $i . '## ' . substr($p_sql, $sec_sin + 1);
            //move the marker up
            $offset = ($sec_sin - ($sec_sin - $beg_sin)) + $p_len + 1; // Adjusting the starting point of the marker

            ++$i;
        }
        $strip_array['sql_string'] = $p_sql;

        return $strip_array;
    }

    /**
     * adds a pattern
     *
     * @param  string $token
     * @param  array $pattern_array
     * @return string
     */
    private function addPatternToSQL($token, array $pattern_array)
    {
        //strip all single quotes out
        $pattern_array = array_reverse($pattern_array);

        foreach ($pattern_array as $key => $replace) {
            $token = str_replace(' ##' . $key . '## ', $replace, $token);
        }

        return $token;
    }

    /**
     * gets an alias from the sql statement
     *
     * @param  string $sql
     * @param  string $alias
     * @return string
     */
    private function getAliasFromSQL($sql, $alias)
    {
        $matches = array();
        preg_match('/^(.*SELECT)(.*?FROM.*WHERE)(.*)$/isU', $sql, $matches);
        //parse all single and double  quotes out of array
        $sin_array = $this->removePatternFromSQL($matches[2], "'", "'", 'sin_');
        $new_sql = array_pop($sin_array);
        $dub_array = $this->removePatternFromSQL($new_sql, "\"", "\"", 'dub_');
        $new_sql = array_pop($dub_array);

        //search for parenthesis
        $paren_array = $this->removePatternFromSQL($new_sql, '(', ')', 'par_');
        $new_sql = array_pop($paren_array);

        //all functions should be removed now, so split the array on commas
        $mstr_sql_array = explode(',', $new_sql);
        foreach ($mstr_sql_array as $token) {
            if (strpos($token, $alias)) {
                //found token, add back comments
                $token = $this->addPatternToSQL($token, $paren_array);
                $token = $this->addPatternToSQL($token, $dub_array);
                $token = $this->addPatternToSQL($token, $sin_array);

                //log and break out of this function
                return $token;
            }
        }

        return null;
    }


    /**
     * Finds the alias of the order by column, and then return the preceding column name
     *
     * @param  string $sql
     * @param  string $orderMatch
     * @return string
     */
    private function findColumnByAlias($sql, $orderMatch)
    {
        //change case to lowercase
        $sql = strtolower($sql);
        $patt = '/\s+' . trim($orderMatch) . '\s*(,|from)/';

        //check for the alias, it should contain comma, may contain space, \n, or \t
        $matches = array();
        preg_match($patt, $sql, $matches, PREG_OFFSET_CAPTURE);
        $found_in_sql = isset($matches[0][1]) ? $matches[0][1] : false;


        //set default for found variable
        $found = $found_in_sql;

        //if still no match found, then we need to parse through the string
        if (!$found_in_sql) {
            //get count of how many times the match exists in string
            $found_count = substr_count($sql, $orderMatch);
            $i = 0;
            $first_ = 0;
            $len = strlen($orderMatch);
            //loop through string as many times as there is a match
            while ($found_count > $i) {
                //get the first match
                $found_in_sql = strpos($sql, $orderMatch, $first_);
                //make sure there was a match
                if ($found_in_sql) {
                    //grab the next 2 individual characters
                    $str_plusone = substr($sql, $found_in_sql + $len, 1);
                    $str_plustwo = substr($sql, $found_in_sql + $len + 1, 1);
                    //if one of those characters is a comma, then we have our alias
                    if ($str_plusone === ',' || $str_plustwo === ',') {
                        //keep track of this position
                        $found = $found_in_sql;
                    }
                }
                //set the offset and increase the iteration counter
                $first_ = $found_in_sql + $len;
                ++$i;
            }
        }

        //return $found, defaults have been set, so if no match was found it will be a negative number
        return $found;
    }


    /**
     * Return the order by string to use in case the column has been aliased
     *
     * @param  string $sql
     * @param  string $orig_order_match
     * @return string
     */
    private function returnOrderBy($sql, $orig_order_match)
    {
        $sql = strtolower($sql);
        $orig_order_match = trim($orig_order_match);
        //this has a tablename defined, pass in the order match
        if (strpos($orig_order_match, '.') != 0) {
            return $orig_order_match;
        }

        // If there is no ordering direction (ASC/DESC), use ASC by default
        if (strpos($orig_order_match, ' ') === false) {
            $orig_order_match .= ' ASC';
        }

        //grab first space in order by
        $firstSpace = strpos($orig_order_match, ' ');

        //split order by into column name and ascending/descending
        $orderMatch = ' ' . strtolower(substr($orig_order_match, 0, $firstSpace));
        $asc_desc = trim(substr($orig_order_match, $firstSpace));

        //look for column name as an alias in sql string
        $found_in_sql = $this->findColumnByAlias($sql, $orderMatch);

        if (!$found_in_sql) {
            //check if this column needs the tablename prefixed to it
            $orderMatch = '.' . trim($orderMatch);
            $colMatchPos = strpos($sql, $orderMatch);
            if ($colMatchPos !== false) {
                //grab sub string up to column name
                $containsColStr = substr($sql, 0, $colMatchPos);
                //get position of first space, so we can grab table name
                $lastSpacePos = strrpos($containsColStr, ' ');
                //use positions of column name, space before name, and length of column to find the correct column name
                $col_name = substr($sql, $lastSpacePos, $colMatchPos - $lastSpacePos + strlen($orderMatch));

                $containsCommaPos = strpos($col_name, ',');
                if ($containsCommaPos !== false) {
                    $col_name = substr($col_name, $containsCommaPos + 1);
                }
                //add the "asc/desc" order back
                $col_name = $col_name . ' ' . $asc_desc;

                //return column name
                return $col_name;
            }
            //break out of here, log this
            $GLOBALS['log']->debug("No match was found for order by, pass string back untouched as: $orig_order_match");

            return $orig_order_match;
        } else {
            //if found, then parse and return
            //grab string up to the aliased column
            $GLOBALS['log']->debug('order by found, process sql string');

            $psql = trim($this->getAliasFromSQL($sql, $orderMatch));
            if (empty($psql)) {
                $psql = trim(substr($sql, 0, $found_in_sql));
            }

            //grab the last comma before the alias
            preg_match('/\s+' . trim($orderMatch) . '/', $psql, $match, PREG_OFFSET_CAPTURE);
            $comma_pos = $match[0][1];
            //substring between the comma and the alias to find the joined_table alias and column name
            $col_name = substr($psql, 0, $comma_pos);

            //make sure the string does not have an end parenthesis
            //and is not part of a function (i.e. "ISNULL(leads.last_name,'') as name"  )
            //this is especially true for unified search from home screen

            $alias_beg_pos = 0;
            if (strpos($psql, ' as ')) {
                $alias_beg_pos = strpos($psql, ' as ');
            }

            // Bug # 44923 - This breaks the query and does not properly filter isnull
            // as there are other functions such as ltrim and rtrim.
            /* elseif (strncasecmp($psql, 'isnull', 6) != 0)
                $alias_beg_pos = strpos($psql, " "); */

            if ($alias_beg_pos > 0) {
                $col_name = substr($psql, 0, $alias_beg_pos);
            }
            //add the "asc/desc" order back
            $col_name = $col_name . ' ' . $asc_desc;

            //pass in new order by
            $GLOBALS['log']->debug('order by being returned is ' . $col_name);

            return $col_name;
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
        while ($i < mssql_num_fields($result)) {
            $meta = mssql_fetch_field($result, $i);
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
     * @see DBManager::getAffectedRowCount()
     */
    public function getAffectedRowCount($result)
    {
        return $this->getOne('SELECT @@ROWCOUNT');
    }

    /**
     * @see DBManager::fetchRow()
     */
    public function fetchRow($result)
    {
        if (empty($result)) {
            return false;
        }

        $row = mssql_fetch_assoc($result);
        //MSSQL returns a space " " when a varchar column is empty ("") and not null.
        //We need to iterate through the returned row array and strip empty spaces
        if (!empty($row)) {
            foreach ($row as $key => $column) {
                //notice we only strip if one space is returned.  we do not want to strip
                //strings with intentional spaces (" foo ")
                if (!empty($column) && $column == ' ') {
                    $row[$key] = '';
                }
            }
        }

        return $row;
    }

    /**
     * @see DBManager::quote()
     */
    public function quote($string)
    {
        if (is_array($string)) {
            return $this->arrayQuote($string);
        }

        return str_replace("'", "''", $this->quoteInternal($string));
    }

    /**
     * @see DBManager::quoteIdentifier()
     */
    public function quoteIdentifier($string)
    {
        return '[' . $string . ']';
    }

    /**
     * @see DBManager::tableExists()
     */
    public function tableExists($tableName)
    {
        $GLOBALS['log']->info("tableExists: $tableName");

        $this->checkConnection();
        $result = $this->getOne(
            "SELECT * FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_TYPE='BASE TABLE' AND TABLE_NAME=" . $this->quoted($tableName)
        );

        return !empty($result);
    }

    /**
     * Get tables like expression
     * @param string $like
     * @return array
     */
    public function tablesLike($like)
    {
        if ($this->getDatabase()) {
            $tables = array();
            $r = $this->query('SELECT TABLE_NAME tn FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_TYPE=\'BASE TABLE\' AND TABLE_NAME LIKE ' . $this->quoted($like));
            if (!empty($r)) {
                while ($a = $this->fetchByAssoc($r)) {
                    $row = array_values($a);
                    $tables[] = $row[0];
                }

                return $tables;
            }
        }

        return false;
    }

    /**
     * @see DBManager::getTablesArray()
     */
    public function getTablesArray()
    {
        $GLOBALS['log']->debug('MSSQL fetching table list');

        if ($this->getDatabase()) {
            $tables = array();
            $r = $this->query('SELECT TABLE_NAME FROM INFORMATION_SCHEMA.TABLES');
            if (is_resource($r)) {
                while ($a = $this->fetchByAssoc($r)) {
                    $tables[] = $a['TABLE_NAME'];
                }

                return $tables;
            }
        }

        return false; // no database available
    }


    /**
     * This call is meant to be used during install, when Full Text Search is enabled
     * Indexing would always occur after a fresh sql server install, so this code creates
     * a catalog and table with full text index.
     */
    public function full_text_indexing_setup()
    {
        $FTSqry = [];
        $GLOBALS['log']->debug('MSSQL about to wakeup FTS');

        if ($this->getDatabase()) {
            //create wakeup catalog
            $FTSqry[] = "if not exists(  select * from sys.fulltext_catalogs where name ='wakeup_catalog' )
                CREATE FULLTEXT CATALOG wakeup_catalog
                ";

            //drop wakeup table if it exists
            $FTSqry[] = "IF EXISTS(SELECT 'fts_wakeup' FROM sysobjects WHERE name = 'fts_wakeup' AND xtype='U')
                    DROP TABLE fts_wakeup
                ";
            //create wakeup table
            $FTSqry[] = 'CREATE TABLE fts_wakeup(
                    id VARCHAR(36) NOT NULL CONSTRAINT pk_fts_wakeup_id PRIMARY KEY CLUSTERED (id ASC ),
                    body TEXT NULL,
                    kb_index INT IDENTITY(1,1) NOT NULL CONSTRAINT wakeup_fts_unique_idx UNIQUE NONCLUSTERED
                )
                ';
            //create full text index
            $FTSqry[] = 'CREATE FULLTEXT INDEX ON fts_wakeup
                (
                    body
                    Language 0X0
                )
                KEY INDEX wakeup_fts_unique_idx ON wakeup_catalog
                WITH CHANGE_TRACKING AUTO
                ';

            //insert dummy data
            $FTSqry[] = "INSERT INTO fts_wakeup (id ,body)
                VALUES ('" . create_guid() . "', 'SugarCRM Rocks' )";


            //create queries to stop and restart indexing
            $FTSqry[] = 'ALTER FULLTEXT INDEX ON fts_wakeup STOP POPULATION';
            $FTSqry[] = 'ALTER FULLTEXT INDEX ON fts_wakeup DISABLE';
            $FTSqry[] = 'ALTER FULLTEXT INDEX ON fts_wakeup ENABLE';
            $FTSqry[] = 'ALTER FULLTEXT INDEX ON fts_wakeup SET CHANGE_TRACKING MANUAL';
            $FTSqry[] = 'ALTER FULLTEXT INDEX ON fts_wakeup START FULL POPULATION';
            $FTSqry[] = 'ALTER FULLTEXT INDEX ON fts_wakeup SET CHANGE_TRACKING AUTO';

            foreach ($FTSqry as $q) {
                sleep(3);
                $this->query($q);
            }
            $this->create_default_full_text_catalog();
        }

        return false; // no database available
    }

    protected $date_formats = array(
        '%Y-%m-%d' => 10,
        '%Y-%m' => 7,
        '%Y' => 4,
    );

    /**
     * @see DBManager::convert()
     */
    public function convert($string, $type, array $additional_parameters = array())
    {
        // convert the parameters array into a comma delimited string
        if (!empty($additional_parameters)) {
            $additional_parameters_string = ',' . implode(',', $additional_parameters);
        } else {
            $additional_parameters_string = '';
        }
        $all_parameters = $additional_parameters;
        if (is_array($string)) {
            $all_parameters = array_merge($string, $all_parameters);
        } elseif (!is_null($string)) {
            array_unshift($all_parameters, $string);
        }

        switch (strtolower($type)) {
            case 'today':
                return 'GETDATE()';
            case 'left':
                return "LEFT($string$additional_parameters_string)";
            case 'date_format':
                if (!empty($additional_parameters[0]) && $additional_parameters[0][0] == "'") {
                    $additional_parameters[0] = trim($additional_parameters[0], "'");
                }
                if (!empty($additional_parameters) && isset($this->date_formats[$additional_parameters[0]])) {
                    $len = $this->date_formats[$additional_parameters[0]];

                    return "LEFT(CONVERT(varchar($len)," . $string . ",120),$len)";
                } else {
                    return 'LEFT(CONVERT(varchar(10),' . $string . ',120),10)';
                }
                // no break
            case 'ifnull':
                if (empty($additional_parameters_string)) {
                    $additional_parameters_string = ",''";
                }

                return "ISNULL($string$additional_parameters_string)";
            case 'concat':
                return implode('+', $all_parameters);
            case 'text2char':
                return "CAST($string AS varchar(8000))";
            case 'quarter':
                return "DATENAME(quarter, $string)";
            case 'length':
                return "LEN($string)";
            case 'month':
                return "MONTH($string)";
            case 'add_date':
                return "DATEADD({$additional_parameters[1]},{$additional_parameters[0]},$string)";
            case 'add_time':
                return "DATEADD(hh, {$additional_parameters[0]}, DATEADD(mi, {$additional_parameters[1]}, $string))";
            case 'add_tz_offset':
                $getUserUTCOffset = $GLOBALS['timedate']->getUserUTCOffset();
                $operation = $getUserUTCOffset < 0 ? '-' : '+';

                return 'DATEADD(minute, ' . $operation . abs($getUserUTCOffset) . ', ' . $string . ')';
            case 'avg':
                return "avg($string)";
            case 'now':
                return 'getutcdate()';
        }

        return (string)$string;
    }

    /**
     * @see DBManager::fromConvert()
     */
    public function fromConvert($string, $type)
    {
        switch ($type) {
            case 'datetimecombo':
            case 'datetime':
                return substr($string, 0, 19);
            case 'date':
                return substr($string, 0, 10);
            case 'time':
                return substr($string, 11);
        }

        return $string;
    }

    /**
     * @see DBManager::createTableSQLParams()
     */
    public function createTableSQLParams($tablename, $fieldDefs, $indices)
    {
        if (empty($tablename) || empty($fieldDefs)) {
            return '';
        }

        $columns = $this->columnSQLRep($fieldDefs, false, $tablename);
        if (empty($columns)) {
            return '';
        }

        return "CREATE TABLE $tablename ($columns)";
    }

    /**
     * Does this type represent text (i.e., non-varchar) value?
     * @param string $type
     * @return bool
     */
    public function isTextType($type)
    {
        $type = strtolower($type);
        if (!isset($this->type_map[$type])) {
            return false;
        }

        return in_array($this->type_map[$type], array('ntext', 'text', 'image', 'nvarchar(max)'));
    }

    /**
     * Return representation of an empty value depending on type
     * @param string $type
     * @return mixed|string
     */
    public function emptyValue($type)
    {
        $ctype = $this->getColumnType($type);
        if ($ctype == 'datetime') {
            return 'NULL';
        }
        if ($ctype == 'date') {
            return 'NULL';
        }
        if ($ctype == 'time') {
            return 'NULL';
        }

        return parent::emptyValue($type);
    }

    /**
     * @param string $tablename
     * @param string $column
     * @param string $newname
     * @return string
     */
    public function renameColumnSQL($tablename, $column, $newname)
    {
        return "SP_RENAME '$tablename.$column', '$newname', 'COLUMN'";
    }

    /**
     * Returns the SQL Alter table statment
     *
     * MSSQL has a quirky T-SQL alter table syntax. Pay special attention to the
     * modify operation
     * @param string $action
     * @param array $def
     * @param bool $ignorRequired
     * @param string $tablename
     * @return string
     */
    protected function alterSQLRep($action, array $def, $ignoreRequired, $tablename)
    {
        switch ($action) {
            case 'add':
                $f_def = $this->oneColumnSQLRep($def, $ignoreRequired, $tablename, false);

                return 'ADD ' . $f_def;
                break;
            case 'drop':
                return 'DROP COLUMN ' . $def['name'];
                break;
            case 'modify':
                //You cannot specify a default value for a column for MSSQL
                $f_def = $this->oneColumnSQLRep($def, $ignoreRequired, $tablename, true);
                $f_stmt = 'ALTER COLUMN ' . $f_def['name'] . ' ' . $f_def['colType'] . ' ' .
                    $f_def['required'] . ' ' . $f_def['auto_increment'] . "\n";
                if (!empty($f_def['default'])) {
                    $f_stmt .= ' ALTER TABLE ' . $tablename . ' ADD  ' . $f_def['default'] . ' FOR ' . $def['name'];
                }

                return $f_stmt;
                break;
            default:
                return '';
        }
    }

    /**
     * @see DBManager::changeColumnSQL()
     *
     * MSSQL uses a different syntax than MySQL for table altering that is
     * not quite as simplistic to implement...
     */
    protected function changeColumnSQL($tablename, $fieldDefs, $action, $ignoreRequired = false)
    {
        $sql = $sql2 = '';
        $constraints = $this->getFieldDefaultConstraintName($tablename);
        $columns = array();
        if ($this->isFieldArray($fieldDefs)) {
            foreach ($fieldDefs as $def) {
                //if the column is being modified drop the default value
                //constraint if it exists. alterSQLRep will add the constraint back
                if (!empty($constraints[$def['name']])) {
                    $sql .= ' ALTER TABLE ' . $tablename . ' DROP CONSTRAINT ' . $constraints[$def['name']];
                }
                //check to see if we need to drop related indexes before the alter
                $indices = $this->get_indices($tablename);
                foreach ($indices as $index) {
                    if (in_array($def['name'], $index['fields'])) {
                        $sql .= ' ' . $this->add_drop_constraint($tablename, $index, true) . ' ';
                        $sql2 .= ' ' . $this->add_drop_constraint($tablename, $index, false) . ' ';
                    }
                }

                $columns[] = $this->alterSQLRep($action, $def, $ignoreRequired, $tablename);
            }
        } else {
            //if the column is being modified drop the default value
            //constraint if it exists. alterSQLRep will add the constraint back
            if (!empty($constraints[$fieldDefs['name']])) {
                $sql .= ' ALTER TABLE ' . $tablename . ' DROP CONSTRAINT ' . $constraints[$fieldDefs['name']];
            }
            //check to see if we need to drop related indexes before the alter
            $indices = $this->get_indices($tablename);
            foreach ($indices as $index) {
                if (in_array($fieldDefs['name'], $index['fields'])) {
                    $sql .= ' ' . $this->add_drop_constraint($tablename, $index, true) . ' ';
                    $sql2 .= ' ' . $this->add_drop_constraint($tablename, $index, false) . ' ';
                }
            }


            $columns[] = $this->alterSQLRep($action, $fieldDefs, $ignoreRequired, $tablename);
        }

        $columns = implode(', ', $columns);
        $sql .= " ALTER TABLE $tablename $columns " . $sql2;

        return $sql;
    }

    /**
     * @param string $table
     * @param string $field_name
     * @return string
     */
    protected function setAutoIncrement($table, $field_name)
    {
        return 'identity(1,1)';
    }

    /**
     * @see DBManager::setAutoIncrementStart()
     */
    public function setAutoIncrementStart($table, $field_name, $start_value)
    {
        if ($start_value > 1) {
            --$start_value;
        }
        $this->query("DBCC CHECKIDENT ('$table', RESEED, $start_value) WITH NO_INFOMSGS");

        return true;
    }

    /**
     * @see DBManager::getAutoIncrement()
     */
    public function getAutoIncrement($table, $field_name)
    {
        $result = $this->getOne("select IDENT_CURRENT('$table') + IDENT_INCR ( '$table' ) as 'Auto_increment'");

        return $result;
    }

    /**
     * @see DBManager::get_indices()
     */
    public function get_indices($tableName)
    {
        //find all unique indexes and primary keys.
        $query = <<<EOSQL
SELECT sys.tables.object_id, sys.tables.name as table_name, sys.columns.name as column_name,
                sys.indexes.name as index_name, sys.indexes.is_unique, sys.indexes.is_primary_key
            FROM sys.tables, sys.indexes, sys.index_columns, sys.columns
            WHERE (sys.tables.object_id = sys.indexes.object_id
                    AND sys.tables.object_id = sys.index_columns.object_id
                    AND sys.tables.object_id = sys.columns.object_id
                    AND sys.indexes.index_id = sys.index_columns.index_id
                    AND sys.index_columns.column_id = sys.columns.column_id)
                AND sys.tables.name = '$tableName'
EOSQL;
        $result = $this->query($query);

        $indices = array();
        while (($row = $this->fetchByAssoc($result)) != null) {
            $index_type = 'index';
            if ($row['is_primary_key'] == '1') {
                $index_type = 'primary';
            } elseif ($row['is_unique'] == 1) {
                $index_type = 'unique';
            }
            $name = strtolower($row['index_name']);
            $indices[$name]['name'] = $name;
            $indices[$name]['type'] = $index_type;
            $indices[$name]['fields'][] = strtolower($row['column_name']);
        }

        return $indices;
    }

    /**
     * @see DBManager::get_columns()
     */
    public function get_columns($tablename)
    {
        //find all unique indexes and primary keys.
        $result = $this->query("sp_columns $tablename");

        $columns = array();
        while (($row = $this->fetchByAssoc($result)) != null) {
            $column_name = strtolower($row['COLUMN_NAME']);
            $columns[$column_name]['name'] = $column_name;
            $columns[$column_name]['type'] = strtolower($row['TYPE_NAME']);
            if ($row['TYPE_NAME'] == 'decimal') {
                $columns[$column_name]['len'] = strtolower($row['PRECISION']);
                $columns[$column_name]['len'] .= ',' . strtolower($row['SCALE']);
            } elseif (in_array($row['TYPE_NAME'], array('nchar', 'nvarchar'))) {
                $columns[$column_name]['len'] = strtolower($row['PRECISION']);
            } elseif (!in_array($row['TYPE_NAME'], array('datetime', 'text'))) {
                $columns[$column_name]['len'] = strtolower($row['LENGTH']);
            }
            if (stristr((string) $row['TYPE_NAME'], 'identity')) {
                $columns[$column_name]['auto_increment'] = '1';
                $columns[$column_name]['type'] = str_replace(' identity', '', strtolower($row['TYPE_NAME']));
            }

            if (
                !empty($row['IS_NULLABLE']) && $row['IS_NULLABLE'] == 'NO'
                && (empty($row['KEY']) || !stristr((string) $row['KEY'], 'PRI'))
            ) {
                $columns[strtolower($row['COLUMN_NAME'])]['required'] = 'true';
            }

            $column_def = 1;
            if (strtolower($tablename) == 'relationships') {
                $column_def = $this->getOne("select cdefault from syscolumns where id = object_id('relationships') and name = '$column_name'");
            }
            // NOTE Not using !empty as an empty string may be a viable default value.
            if ($column_def != 0 && ($row['COLUMN_DEF'] != null)) {
                $matches = array();
                $row['COLUMN_DEF'] = html_entity_decode((string) $row['COLUMN_DEF'], ENT_QUOTES);
                if (preg_match('/\([\(|\'](.*)[\)|\']\)/i', $row['COLUMN_DEF'], $matches)) {
                    $columns[$column_name]['default'] = $matches[1];
                } elseif (preg_match('/\(N\'(.*)\'\)/i', $row['COLUMN_DEF'], $matches)) {
                    $columns[$column_name]['default'] = $matches[1];
                } else {
                    $columns[$column_name]['default'] = $row['COLUMN_DEF'];
                }
            }
        }

        return $columns;
    }


    /**
     * Get FTS catalog name for current DB
     */
    protected function ftsCatalogName()
    {
        if (isset($this->connectOptions['db_name'])) {
            return $this->connectOptions['db_name'] . '_fts_catalog';
        }

        return 'sugar_fts_catalog';
    }

    /**
     * @see DBManager::add_drop_constraint()
     */
    public function add_drop_constraint($table, $definition, $drop = false)
    {
        $type = $definition['type'];
        $fields = is_array($definition['fields']) ? implode(',', $definition['fields']) : $definition['fields'];

        $fields = $this->removeIndexLimit($fields);

        $name = $definition['name'];
        $sql = '';

        switch ($type) {
            // generic indices
            case 'index':
            case 'alternate_key':
                if ($drop) {
                    $sql = "DROP INDEX {$name} ON {$table}";
                } else {
                    $sql = "CREATE INDEX {$name} ON {$table} ({$fields})";
                }
                break;
            case 'clustered':
                if ($drop) {
                    $sql = "DROP INDEX {$name} ON {$table}";
                } else {
                    $sql = "CREATE CLUSTERED INDEX $name ON $table ($fields)";
                }
                break;
            // constraints as indices
            case 'unique':
                if ($drop) {
                    $sql = "ALTER TABLE {$table} DROP CONSTRAINT $name";
                } else {
                    $sql = "ALTER TABLE {$table} ADD CONSTRAINT {$name} UNIQUE ({$fields})";
                }
                break;
            case 'primary':
                if ($drop) {
                    $sql = "ALTER TABLE {$table} DROP CONSTRAINT {$name}";
                } else {
                    $sql = "ALTER TABLE {$table} ADD CONSTRAINT {$name} PRIMARY KEY ({$fields})";
                }
                break;
            case 'foreign':
                if ($drop) {
                    $sql = "ALTER TABLE {$table} DROP FOREIGN KEY ({$fields})";
                } else {
                    $sql = "ALTER TABLE {$table} ADD CONSTRAINT {$name}  FOREIGN KEY ({$fields}) REFERENCES {$definition['foreignTable']}({$definition['foreignFields']})";
                }
                break;
            case 'fulltext':
                if ($this->full_text_indexing_enabled() && $drop) {
                    $sql = "DROP FULLTEXT INDEX ON {$table}";
                } elseif ($this->full_text_indexing_enabled()) {
                    $catalog_name = $this->ftsCatalogName();
                    if (isset($definition['catalog_name']) && $definition['catalog_name'] != 'default') {
                        $catalog_name = $definition['catalog_name'];
                    }

                    $language = 'Language 1033';
                    if (isset($definition['language']) && !empty($definition['language'])) {
                        $language = 'Language ' . $definition['language'];
                    }

                    $key_index = $definition['key_index'];

                    $change_tracking = 'auto';
                    if (isset($definition['change_tracking']) && !empty($definition['change_tracking'])) {
                        $change_tracking = $definition['change_tracking'];
                    }

                    $sql = " CREATE FULLTEXT INDEX ON $table ($fields $language) KEY INDEX $key_index ON $catalog_name WITH CHANGE_TRACKING $change_tracking";
                }
                break;
        }

        return $sql;
    }

    /**
     * Returns true if Full Text Search is installed
     *
     * @return bool
     */
    public function full_text_indexing_installed()
    {
        $ftsChckRes = $this->getOne("SELECT FULLTEXTSERVICEPROPERTY('IsFulltextInstalled') as fts");

        return !empty($ftsChckRes);
    }

    /**
     * @see DBManager::full_text_indexing_enabled()
     */
    protected function full_text_indexing_enabled($dbname = null)
    {
        // check to see if we already have install setting in session
        if (!isset($_SESSION['IsFulltextInstalled'])) {
            $_SESSION['IsFulltextInstalled'] = $this->full_text_indexing_installed();
        }

        // check to see if FTS Indexing service is installed
        if (empty($_SESSION['IsFulltextInstalled'])) {
            return false;
        }

        // grab the dbname if it was not passed through
        if (empty($dbname)) {
            global $sugar_config;
            $dbname = $sugar_config['dbconfig']['db_name'];
        }
        //we already know that Indexing service is installed, now check
        //to see if it is enabled
        $res = $this->getOne("SELECT DATABASEPROPERTY('$dbname', 'IsFulltextEnabled') ftext");

        return !empty($res);
    }

    /**
     * Creates default full text catalog
     */
    protected function create_default_full_text_catalog()
    {
        if ($this->full_text_indexing_enabled()) {
            $catalog = $this->ftsCatalogName();
            $GLOBALS['log']->debug("Creating the default catalog for full-text indexing, $catalog");

            //drop catalog if exists.
            $ret = $this->query("
                if not exists(
                    select *
                        from sys.fulltext_catalogs
                        where name ='$catalog'
                        )
                CREATE FULLTEXT CATALOG $catalog");

            if (empty($ret)) {
                $GLOBALS['log']->error("Error creating default full-text catalog, $catalog");
            }
        }
    }

    /**
     * Function returns name of the constraint automatically generated by sql-server.
     * We request this for default, primary key, required
     *
     * @param  string $table
     * @param  string $column
     * @return string
     */
    private function getFieldDefaultConstraintName($table, $column = null)
    {
        static $results = array();

        if (empty($column) && isset($results[$table])) {
            return $results[$table];
        }

        $query = <<<EOQ
select s.name, o.name, c.name dtrt, d.name ctrt
    from sys.default_constraints as d
        join sys.objects as o
            on o.object_id = d.parent_object_id
        join sys.columns as c
            on c.object_id = o.object_id and c.column_id = d.parent_column_id
        join sys.schemas as s
            on s.schema_id = o.schema_id
    where o.name = '$table'
EOQ;
        if (!empty($column)) {
            $query .= " and c.name = '$column'";
        }
        $res = $this->query($query);
        if (!empty($column)) {
            $row = $this->fetchByAssoc($res);
            if (!empty($row)) {
                return $row['ctrt'];
            }
        } else {
            $returnResult = array();
            while ($row = $this->fetchByAssoc($res)) {
                $returnResult[$row['dtrt']] = $row['ctrt'];
            }
            $results[$table] = $returnResult;

            return $returnResult;
        }

        return null;
    }

    /**
     * @see DBManager::massageFieldDef()
     */
    public function massageFieldDef(&$fieldDef, $tablename)
    {
        parent::massageFieldDef($fieldDef, $tablename);

        if ($fieldDef['type'] == 'int') {
            $fieldDef['len'] = '4';
        }

        if (empty($fieldDef['len'])) {
            switch ($fieldDef['type']) {
                case 'bit':
                case 'bool':
                    $fieldDef['len'] = '1';
                    break;
                case 'smallint':
                    $fieldDef['len'] = '2';
                    break;
                case 'float':
                    $fieldDef['len'] = '8';
                    break;
                case 'varchar':
                case 'nvarchar':
                    $fieldDef['len'] = $this->isTextType($fieldDef['dbType']) ? 'max' : '255';
                    break;
                case 'image':
                    $fieldDef['len'] = '2147483647';
                    break;
                case 'ntext':
                    $fieldDef['len'] = '2147483646';
                    break;   // Note: this is from legacy code, don't know if this is correct
            }
        }
        if ($fieldDef['type'] == 'decimal'
            && empty($fieldDef['precision'])
            && !strpos((string) $fieldDef['len'], ',')
        ) {
            $fieldDef['len'] .= ',0'; // Adding 0 precision if it is not specified
        }

        if (empty($fieldDef['default'])
            && in_array($fieldDef['type'], array('bit', 'bool'))
        ) {
            $fieldDef['default'] = '0';
        }
        if (isset($fieldDef['required']) && $fieldDef['required'] && !isset($fieldDef['default'])) {
            $fieldDef['default'] = '';
        }
    }

    /**
     * @see DBManager::oneColumnSQLRep()
     */
    protected function oneColumnSQLRep($fieldDef, $ignoreRequired = false, $table = '', $return_as_array = false)
    {
        //Bug 25814
        if (isset($fieldDef['name'])) {
            $colType = $this->getFieldType($fieldDef);
            if (stristr($this->getFieldType($fieldDef), 'decimal') && isset($fieldDef['len'])) {
                $fieldDef['len'] = min($fieldDef['len'], 38);
            }
            //bug: 39690 float(8) is interpreted as real and this generates a diff when doing repair
            if (stristr($colType, 'float') && isset($fieldDef['len']) && $fieldDef['len'] == 8) {
                unset($fieldDef['len']);
            }
        }

        // always return as array for post-processing
        $ref = parent::oneColumnSQLRep($fieldDef, $ignoreRequired, $table, true);

        // Quote the column name (fixes problems with names like 'open', as found in aobh_businesshours)
        $ref['name'] = $this->quoteIdentifier($ref['name']);

        // Bug 24307 - Don't add precision for float fields.
        if (stristr((string) $ref['colType'], 'float')) {
            $ref['colType'] = preg_replace('/(,\d+)/', '', (string) $ref['colType']);
        }

        if ($return_as_array) {
            return $ref;
        } else {
            return "{$ref['name']} {$ref['colType']} {$ref['default']} {$ref['required']} {$ref['auto_increment']}";
        }
    }

    /**
     * Saves changes to module's audit table
     *
     * @param Sugarbean $bean
     * @param array $changes changes
     * @return bool
     */
    public function save_audit_records(SugarBean $bean, $changes)
    {
        //Bug 25078 fixed by Martin Hu: sqlserver haven't 'date' type, trim extra "00:00:00"
        if ($changes['data_type'] == 'date') {
            $changes['before'] = str_replace(' 00:00:00', '', (string) $changes['before']);
        }

        return parent::save_audit_records($bean, $changes);
    }

    /**
     * Disconnects from the database
     *
     * Also handles any cleanup needed
     */
    public function disconnect()
    {
        $GLOBALS['log']->debug('Calling Mssql::disconnect()');
        if (!empty($this->database)) {
            $this->freeResult();
            mssql_close($this->database);
            $this->database = null;
        }
    }

    /**
     * @see DBManager::freeDbResult()
     */
    protected function freeDbResult($dbResult)
    {
        if (!empty($dbResult)) {
            mssql_free_result($dbResult);
        }
    }

    /**
     * (non-PHPdoc)
     * @see DBManager::lastDbError()
     */
    public function lastDbError()
    {
        $sqlmsg = mssql_get_last_message();
        if (empty($sqlmsg)) {
            return false;
        }
        global $app_strings;
        if (empty($app_strings)
            || !isset($app_strings['ERR_MSSQL_DB_CONTEXT'])
            || !isset($app_strings['ERR_MSSQL_WARNING'])
        ) {
            //ignore the message from sql-server if $app_strings array is empty. This will happen
            //only if connection if made before language is set.
            return false;
        }

        $sqlpos = strpos((string) $sqlmsg, 'Changed database context to');
        $sqlpos2 = strpos((string) $sqlmsg, 'Warning:');
        $sqlpos3 = strpos((string) $sqlmsg, 'Checking identity information:');
        if ($sqlpos !== false || $sqlpos2 !== false || $sqlpos3 !== false) {
            return false;
        } else {
            global $app_strings;
            //ERR_MSSQL_DB_CONTEXT: localized version of 'Changed database context to' message
            if (empty($app_strings) || !isset($app_strings['ERR_MSSQL_DB_CONTEXT'])) {
                //ignore the message from sql-server if $app_strings array is empty. This will happen
                //only if connection if made before languge is set.
                $GLOBALS['log']->debug('Ignoring this database message: ' . $sqlmsg);

                return false;
            } else {
                $sqlpos = strpos((string) $sqlmsg, (string) $app_strings['ERR_MSSQL_DB_CONTEXT']);
                if ($sqlpos !== false) {
                    return false;
                }
            }
        }

        if (strlen((string) $sqlmsg) > 2) {
            return 'SQL Server error: ' . $sqlmsg;
        }

        return false;
    }

    /**
     * (non-PHPdoc)
     * @see DBManager::getDbInfo()
     */
    public function getDbInfo()
    {
        return array('version' => $this->version());
    }

    /**
     * (non-PHPdoc)
     * @see DBManager::validateQuery()
     */
    public function validateQuery($query)
    {
        if (!$this->isSelect($query)) {
            return false;
        }
        $this->query('SET SHOWPLAN_TEXT ON');
        $res = $this->getOne($query);
        $this->query('SET SHOWPLAN_TEXT OFF');

        return !empty($res);
    }

    /**
     * This is a utility function to prepend the "N" character in front of SQL values that are
     * surrounded by single quotes.
     *
     * @param  $sql string SQL statement
     * @return string SQL statement with single quote values prepended with "N" character for nvarchar columns
     */
    protected function _appendN($sql)
    {
        // If there are no single quotes, don't bother, will just assume there is no character data
        if (strpos((string) $sql, "'") === false) {
            return $sql;
        }

        // Flag if there are odd number of single quotes, just continue without trying to append N
        if (substr_count((string) $sql, "'") & 1) {
            $GLOBALS['log']->error('SQL statement[' . $sql . '] has odd number of single quotes.');

            return $sql;
        }

        //The only location of three subsequent ' will be at the beginning or end of a value.
        $sql = preg_replace('/(?<!\')(\'{3})(?!\')/', "'<@#@#@PAIR@#@#@>", (string) $sql);

        // Remove any remaining '' and do not parse... replace later (hopefully we don't even have any)
        $pairs = array();
        $regexp = '/(\'{2})/';
        $pair_matches = array();
        preg_match_all($regexp, $sql, $pair_matches);
        if ($pair_matches) {
            foreach (array_unique($pair_matches[0]) as $key => $value) {
                $pairs['<@PAIR-' . $key . '@>'] = $value;
            }
            if (!empty($pairs)) {
                $sql = str_replace($pairs, array_keys($pairs), $sql);
            }
        }

        $regexp = "/(N?'.+?')/is";
        $matches = array();
        preg_match_all($regexp, $sql, $matches);
        $replace = array();
        if (!empty($matches)) {
            foreach ($matches[0] as $value) {
                // We are assuming that all nvarchar columns are no more than 200 characters in length
                // One problem we face is the image column type in reports which cannot accept nvarchar data
                if (
                    !empty($value)
                    && !is_numeric(trim(str_replace(array("'", ','), '', (string) $value)))
                    && !preg_match('/^\'[\,]\'$/', (string) $value)
                ) {
                    $replace[$value] = 'N' . trim($value, 'N');
                }
            }
        }

        if (!empty($replace)) {
            $sql = str_replace(array_keys($replace), $replace, $sql);
        }

        if (!empty($pairs)) {
            $sql = str_replace(array_keys($pairs), $pairs, $sql);
        }

        if (strpos($sql, '<@#@#@PAIR@#@#@>')) {
            $sql = str_replace(array('<@#@#@PAIR@#@#@>'), array("''"), $sql);
        }

        return $sql;
    }

    /**
     * Quote SQL Server search term
     * @param string $term
     * @return string
     */
    protected function quoteTerm($term)
    {
        $term = str_replace('%', '*', $term); // Mssql wildcard is *

        return '"' . str_replace('"', '', $term) . '"';
    }

    /**
     * Generate fulltext query from set of terms
     * @param string $field Field to search against
     * @param array $terms Search terms that may be or not be in the result
     * @param array $must_terms Search terms that have to be in the result
     * @param array $exclude_terms Search terms that have to be not in the result
     * @return string
     */
    public function getFulltextQuery($field, $terms, $must_terms = array(), $exclude_terms = array())
    {
        $condition = $or_condition = array();
        foreach ($must_terms as $term) {
            $condition[] = $this->quoteTerm($term);
        }

        foreach ($terms as $term) {
            $or_condition[] = $this->quoteTerm($term);
        }

        if (!empty($or_condition)) {
            $condition[] = '(' . implode(' | ', $or_condition) . ')';
        }

        foreach ($exclude_terms as $term) {
            $condition[] = ' NOT ' . $this->quoteTerm($term);
        }
        $condition = $this->quoted(implode(' AND ', $condition));

        return "CONTAINS($field, $condition)";
    }

    /**
     * Check if certain database exists
     * @param string $dbname
     * @return bool
     */
    public function dbExists($dbname)
    {
        $db = $this->getOne('SELECT name FROM master..sysdatabases WHERE name = N' . $this->quoted($dbname));

        return !empty($db);
    }

    /**
     * Select database
     * @param string $dbname
     * @return bool
     */
    protected function selectDb($dbname)
    {
        return mssql_select_db($dbname);
    }

    /**
     * Check if certain DB user exists
     * @param string $username
     * @return bool
     */
    public function userExists($username)
    {
        $this->selectDb('master');
        $user = $this->getOne('SELECT count(*) FROM sys.sql_logins WHERE name =' . $this->quoted($username));

        // FIXME: go back to the original DB
        return !empty($user);
    }

    /**
     * Create DB user
     * @param string $database_name
     * @param string $host_name
     * @param string $user
     * @param string $password
     */
    public function createDbUser($database_name, $host_name, $user, $password)
    {
        $qpassword = $this->quote($password);
        $this->selectDb($database_name);
        $this->query("CREATE LOGIN $user WITH PASSWORD = '$qpassword'", true);
        $this->query("CREATE USER $user FOR LOGIN $user", true);
        $this->query("EXEC sp_addRoleMember 'db_ddladmin ', '$user'", true);
        $this->query("EXEC sp_addRoleMember 'db_datareader','$user'", true);
        $this->query("EXEC sp_addRoleMember 'db_datawriter','$user'", true);
    }

    /**
     * Create a database
     * @param string $dbname
     * @return bool|resource
     */
    public function createDatabase($dbname)
    {
        return $this->query("CREATE DATABASE $dbname", true);
    }

    /**
     * Drop a database
     * @param string $dbname
     * @return bool|mixed|resource
     */
    public function dropDatabase($dbname)
    {
        return $this->query("DROP DATABASE $dbname", true);
    }

    /**
     * Check if this driver can be used
     * @return bool
     */
    public function valid()
    {
        return function_exists('mssql_connect');
    }

    /**
     * Check if this DB name is valid
     *
     * @param string $name
     * @return bool
     */
    public function isDatabaseNameValid($name)
    {
        // No funny chars, does not begin with number
        return preg_match('/^[0-9#@]+|[\"\'\*\/\\?\:\\<\>\-\ \&\!\(\)\[\]\{\}\;\,\.\`\~\|\\\\]+/', $name) == 0;
    }

    /**
     * @return array
     */
    public function installConfig()
    {
        return array(
            'LBL_DBCONFIG_MSG3' => array(
                'setup_db_database_name' => array('label' => 'LBL_DBCONF_DB_NAME', 'required' => true),
            ),
            'LBL_DBCONFIG_MSG2' => array(
                'setup_db_host_name' => array('label' => 'LBL_DBCONF_HOST_NAME', 'required' => true),
                'setup_db_host_instance' => array('label' => 'LBL_DBCONF_HOST_INSTANCE'),
            ),
            'LBL_DBCONF_TITLE_USER_INFO' => array(),
            'LBL_DBCONFIG_B_MSG1' => array(
                'setup_db_admin_user_name' => array('label' => 'LBL_DBCONF_DB_ADMIN_USER', 'required' => true),
                'setup_db_admin_password' => array('label' => 'LBL_DBCONF_DB_ADMIN_PASSWORD', 'type' => 'password'),
            )
        );
    }

    /**
     * Returns a DB specific FROM clause which can be used to select against functions.
     * Note that depending on the database that this may also be an empty string.
     * @return string
     */
    public function getFromDummyTable()
    {
        return '';
    }

    /**
     * Returns a DB specific piece of SQL which will generate GUID (UUID)
     * This string can be used in dynamic SQL to do multiple inserts with a single query.
     * I.e. generate a unique Sugar id in a sub select of an insert statement.
     * @return string
     */

    public function getGuidSQL()
    {
        return 'NEWID()';
    }

    /**
     * Remove unsupported index limit
     * @param $fields
     * @return string|string[]|null
     */
    protected function removeIndexLimit($fields)
    {
        return preg_replace('/(\s?\(\d+\))/', '', (string) $fields);
    }
}
