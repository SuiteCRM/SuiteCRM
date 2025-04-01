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



//to_string methods to get strings for values

 // var_export gets rid of the empty values that we use to display None
 // thishelper function fixes that
 // *This is no longer the case in php 5. var_export will now preserve empty keys.
function var_export_helper($tempArray)
{
    return var_export($tempArray, true);
}



/*
 * this function is used to overide a value in an array and returns the string code to write
 * @params : $array_name - a String containing the name of an array.
 * @params : $value_name - a String containing the name of a variable in the array.
 * @params : $value      - a String containing the associated value with $value_name.
 *
 * @returns: String. Example - override_value_to_string($name, 'b', 1) = '$name['b'] = 1;'
 */

function override_value_to_string($array_name, $value_name, $value)
{
    $string = "\${$array_name}[". var_export($value_name, true). "] = ";
    $string .= var_export_helper($value, true);
    return $string . ";";
}

function add_blank_option($options)
{
    if (is_array($options)) {
        if (!isset($options['']) && !isset($options['0'])) {
            $options = array_merge(array(''=>''), $options);
        }
    } else {
        $options = array(''=>'');
    }
    return $options;
}

/**
 * Exports array to string
 *
 * @param array $key_names array of keys
 * @param string $array_name  name of the array
 * @param mixed $value value of the array
 * @param bool $eval evaluates the generated string if true, note that the array name must be in the global space!
 * @return mixed|string string $array_name['a']['b']['c'][.] = 'hello'
 */
function override_value_to_string_recursive($key_names, $array_name, $value, $eval = false)
{
    global $log;
    if ($eval) {
        $log->deprecated('$eval parameter is deprecated');
    }

    return "\${$array_name}". override_recursive_helper($key_names, $array_name, $value);
}

function override_recursive_helper($key_names, $array_name, $value)
{
    if (empty($key_names)) {
        return "=".var_export_helper($value, true).";";
    } else {
        $key = array_shift($key_names);
        return "[".var_export($key, true)."]". override_recursive_helper($key_names, $array_name, $value);
    }
}

function override_value_to_string_recursive2($array_name, $value_name, $value, $save_empty = true)
{
    $quoted_vname = var_export($value_name, true);
    if (is_array($value)) {
        $str = '';
        $newArrayName = $array_name . "[$quoted_vname]";
        foreach ($value as $key=>$val) {
            $str.= override_value_to_string_recursive2($newArrayName, $key, $val, $save_empty);
        }
        return $str;
    } else {
        if (!$save_empty && empty($value)) {
            return;
        } else {
            return "\$$array_name" . "[$quoted_vname] = " . var_export($value, true) . ";\n";
        }
    }
}

/**
 * This function will attempt to convert an object to an array.
 * Loops are not checked for so this function should be used with caution.
 *
 * @param $obj
 * @return array representation of $obj
 */
function object_to_array_recursive($obj)
{
    if (!is_object($obj)) {
        return $obj;
    }

    $ret = get_object_vars($obj);
    foreach ($ret as $key => $val) {
        if (is_object($val)) {
            $ret[$key] = object_to_array_recursive($val);
        }
    }
    return $ret;
}
/**
     * This function returns an array of all the key=>value pairs in $array1
     * that are wither not present, or different in $array2.
     * If a key exists in $array2 but not $array1, it will not be reported.
     * Values which are arrays are traced further and reported only if thier is a difference
     * in one or more of thier children.
     *
     * @param array $array1, the array which contains all the key=>values you wish to check againts
     * @param array $array2, the array which
     * @param array $allowEmpty, will return the value if it is empty in $array1 and not in $array2,
     * otherwise empty values in $array1 are ignored.
     * @return array containing the differences between the two arrays
     */
    function deepArrayDiff($array1, $array2, $allowEmpty = false)
    {
        $diff = array();
        foreach ($array1 as $key=>$value) {
            if (is_array($value)) {
                if ((!isset($array2[$key]) || !is_array($array2[$key])) && (isset($value) || $allowEmpty)) {
                    $diff[$key] = $value;
                } else {
                    $value = deepArrayDiff($array1[$key], $array2[$key], $allowEmpty);
                    if (!empty($value) || $allowEmpty) {
                        $diff[$key] = $value;
                    }
                }
            } else {
                if ((!isset($array2[$key]) || $value != $array2[$key]) && (isset($value) || $allowEmpty)) {
                    $diff[$key] = $value;
                }
            }
        }
        return $diff;
    }
    
    /**
     * Recursivly set a value in an array, creating sub arrays as necessary
     *
     * @param unknown_type $array
     * @param unknown_type $key
     */
    function setDeepArrayValue(&$array, $key, $value)
    {
        //if _ is at position zero, that is invalid.
        if (strrpos($key, "_")) {
            list($key, $remkey) = explode('_', $key, 2);
            if (!isset($array[$key]) || !is_array($array[$key])) {
                $array[$key] = array();
            }
            setDeepArrayValue($array[$key], $remkey, $value);
        } else {
            $array[$key] = $value;
        }
    }


// This function iterates through the given arrays and combines the values of each key, to form one array
// Returns FALSE if number of elements in the arrays do not match; otherwise, returns merged array
// Example: array("a", "b", "c") and array("x", "y", "z") are passed in; array("ax", "by", "cz") is returned
function array_merge_values($arr1, $arr2)
{
    if (count($arr1) !== count($arr2)) {
        return false;
    }
    $arr1Count = count($arr1);

    for ($i = 0; $i < $arr1Count; $i++) {
        $arr1[$i] .= $arr2[$i];
    }

    return $arr1;
}

/**
 * Search an array for a given value ignorning case sensitivity
 *
 * @param unknown_type $key
 * @param unknown_type $haystack
 */
function array_search_insensitive($key, $haystack)
{
    if (!is_array($haystack)) {
        return false;
    }

    $found = false;
    foreach ($haystack as $k => $v) {
        if (strtolower($v) === strtolower($key)) {
            $found = true;
            break;
        }
    }

    return $found;
}


/**
 * This function is useful to format properly indices definitions before
 * compare them to decide if this index should be created in database or not.
 *
 * Example:
 * If an index definition in vardefs.php contains a field like this:
 *   => 'last_name  (  30 ) ',
 *
 * This function formats this string like this:
 *  => 'last_name (30)',
 *
 * The function replaces
 *  - one o more whitespace by only one whitespace.
 *  - trim the string.
 *  - whitespace after '('
 *  - whitespace before ')'
 * @param  array $indexArray an index definition
 * @return array $indexArray an index definition
 */
function fixIndexArrayFormat($indexArray)
{
    foreach ($indexArray as $key => $value) {
        $indexArray[$key] = preg_replace("/\s+/u", " ", (string) $value);
        $indexArray[$key] = trim($indexArray[$key]);
        $indexArray[$key] = str_replace(['( ', ' )'], ['(', ')'], $indexArray[$key]);
    }
    return $indexArray;
}


/**
 * Wrapper around PHP's ArrayObject class that provides dot-notation recursive searching
 * for multi-dimensional arrays
 */
#[\AllowDynamicProperties]
class SugarArray extends ArrayObject
{
    /**
     * Return the value matching $key if exists, otherwise $default value
     *
     * This method uses dot notation to look through multi-dimensional arrays
     *
     * @param string $key key to look up
     * @param mixed $default value to return if $key does not exist
     * @return mixed
     */
    public function get($key, $default = null)
    {
        return $this->_getFromSource($key, $default);
    }

    /**
     * Provided as a convinience method for fetching a value within an existing
     * array without instantiating a SugarArray
     *
     * NOTE: This should only used where refactoring an array into a SugarArray
     *       is unfeasible.  This operation is more expensive than a direct
     *       SugarArray as each time it creates and throws away a new instance
     *
     * @param array $haystack haystack
     * @param string $needle needle
     * @param mixed $default default value to return
     * @return mixed
     */
    public static function staticGet($haystack, $needle, $default = null)
    {
        if (empty($haystack)) {
            return $default;
        }
        $array = new self($haystack);
        return $array->get($needle, $default);
    }

    private function _getFromSource($key, $default)
    {
        if (strpos((string) $key, '.') === false) {
            return isset($this[$key]) ? $this[$key] : $default;
        }

        $exploded = explode('.', $key);
        $current_key = array_shift($exploded);
        return $this->_getRecursive($this->_getFromSource($current_key, $default), $exploded, $default);
    }

    private function _getRecursive($raw_config, $children, $default)
    {
        if ($raw_config === $default) {
            return $default;
        } elseif ((is_countable($children) ? count($children) : 0) == 0) {
            return $raw_config;
        } else {
            $next_key = array_shift($children);
            return isset($raw_config[$next_key]) ?
                $this->_getRecursive($raw_config[$next_key], $children, $default) :
                $default;
        }
    }
}
