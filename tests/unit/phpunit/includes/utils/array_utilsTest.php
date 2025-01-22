<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2021 SalesAgility Ltd.
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

namespace SuiteCRM\Tests\Unit\includes\utils;

use SugarArray;
use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;
use TimeDate;

require_once __DIR__ . '/../../../../../include/utils/array_utils.php';

/**
 * Class array_utilsTest
 * @package SuiteCRM\Tests\Unit\utils
 */
class array_utilsTest extends SuitePHPUnitFrameworkTestCase
{
    public function testvar_export_helper(): void
    {
        //execute the method and test if it returns expected values
        $tempArray = array('Key1' => 'value1', 'Key2' => 'value2');

        $expected = "array (\n  'Key1' => 'value1',\n  'Key2' => 'value2',\n)";
        $actual = var_export_helper($tempArray);
        self::assertSame($expected, $actual);
    }

    public function testoverride_value_to_string(): void
    {
        //execute the method and test if it returns expected values
        $expected = "\$array_name['value_name'] = 'value';";
        $actual = override_value_to_string('array_name', 'value_name', 'value');
        self::assertSame($expected, $actual);
    }

    public function testadd_blank_option(): void
    {
        //execute the method with array not having any blank key value pair. function will return an array with blank key value pair added.
        $tempArray = array('Key1' => 'value1', 'Key2' => 'value2');
        $expected = array('' => '', 'Key1' => 'value1', 'Key2' => 'value2');

        $actual = add_blank_option($tempArray);
        self::assertSame($expected, $actual);

        //execute the method with array having a blank key value pair. function will return the same array back without any change.
        $tempArray = array('' => '', 'Key1' => 'value1', 'Key2' => 'value2');
        $expected = array('' => '', 'Key1' => 'value1', 'Key2' => 'value2');

        $actual = add_blank_option($tempArray);
        self::assertSame($expected, $actual);
    }

    public function testoverride_value_to_string_recursive(): void
    {
        //execute the method and test if it returns expected values

        //without keys
        $tempArray = array('Key1' => 'value1', 'Key2' => 'value2');
        $expected = "\$tempArray=array (\n  'Key1' => 'value1',\n  'Key2' => 'value2',\n);";
        $actual = override_value_to_string_recursive('', 'tempArray', $tempArray);
        self::assertSame($expected, $actual);

        //with keys
        $tempArray = array();
        $tempArray['Key1']['Key2'] = array('Key3' => 'value', 'Key4' => 'value');
        $expected = "\$tempArray['key1']['key2']=array (\n  'Key1' => \n  array (\n    'Key2' => \n    array (\n      'Key3' => 'value',\n      'Key4' => 'value',\n    ),\n  ),\n);";
        $actual = override_value_to_string_recursive(array('key1', 'key2'), 'tempArray', $tempArray);

        self::assertSame($expected, $actual);
    }

    public function testoverride_recursive_helper(): void
    {
        //execute the method and test if it returns expected values

        //without keys
        $tempArray = array('Key1' => 'value1', 'Key2' => 'value2');
        $expected = "=array (\n  'Key1' => 'value1',\n  'Key2' => 'value2',\n);";
        $actual = override_recursive_helper('', 'tempArray', $tempArray);
        self::assertSame($expected, $actual);

        //with keys
        $tempArray = array();
        $tempArray['Key1']['Key2'] = array('Key3' => 'value', 'Key4' => 'value');
        $expected = "['key1']['key2']=array (\n  'Key1' => \n  array (\n    'Key2' => \n    array (\n      'Key3' => 'value',\n      'Key4' => 'value',\n    ),\n  ),\n);";
        $actual = override_recursive_helper(array('key1', 'key2'), 'tempArray', $tempArray);
        self::assertSame($expected, $actual);
    }

    public function testoverride_value_to_string_recursive2(): void
    {
        //execute the method and test if it returns expected values

        //null array
        $expected = null;
        $actual = override_value_to_string_recursive2('tempArray', 'key1', '', false);
        self::assertSame($expected, $actual);

        //simple array
        $tempArray = array('Key1' => 'value1', 'Key2' => 'value2');
        $expected = "\$['tempArray']['Key1'] = 'value1';\n$['tempArray']['Key2'] = 'value2';\n";
        $actual = override_value_to_string_recursive2('', 'tempArray', $tempArray);
        self::assertSame($expected, $actual);

        //complex array
        $tempArray = array();
        $tempArray['Key1']['Key2'] = array('Key3' => 'value', 'Key4' => 'value');
        $expected = "\$tempArray['key1']['Key2']['Key3'] = 'value';\n\$tempArray['key1']['Key2']['Key4'] = 'value';\n";
        $actual = override_value_to_string_recursive2('tempArray', 'key1', $tempArray['Key1']);
        self::assertSame($expected, $actual);
    }

    public function testobject_to_array_recursive(): void
    {
        //execute the method and test if it returns expected values

        //test invalid input
        $obj = '';
        $expected = '';
        $actual = object_to_array_recursive($obj);
        self::assertSame($expected, $actual);

        //test with a valid object
        $obj = new TimeDate();
        $expected = array('dbDayFormat' => 'Y-m-d', 'dbTimeFormat' => 'H:i:s', 'allow_cache' => true);
        $actual = object_to_array_recursive($obj);

        self::assertSame($expected, $actual);
    }

    public function testdeepArrayDiff(): void
    {
        //execute the method and test if it returns expected values

        //same simple arrays
        $tempArray1 = array('Key1' => 'value1', 'Key2' => 'value2');
        $tempArray2 = array('Key1' => 'value1', 'Key2' => 'value2');
        $expected = array();
        $actual = deepArrayDiff($tempArray1, $tempArray2);
        self::assertSame($expected, $actual);

        //different simple arrays
        $tempArray1 = array('Key1' => 'value1', 'Key2' => 'value2');
        $tempArray2 = array('Key1' => 'value1', 'Key2' => 'value3');
        $expected = array('Key2' => 'value2');
        $actual = deepArrayDiff($tempArray1, $tempArray2);
        self::assertSame($expected, $actual);

        //same complex arrays
        $tempArray1 = array('Key1' => array('Key2' => 'value2', 'Key3' => 'value3'));
        $tempArray2 = array('Key1' => array('Key2' => 'value2', 'Key3' => 'value3'));
        $expected = array();
        $actual = deepArrayDiff($tempArray1, $tempArray2);
        self::assertSame($expected, $actual);

        //complex arrays with different root node
        $tempArray1 = array('Key1' => array('Key2' => 'value2', 'Key3' => 'value3'));
        $tempArray2 = array('Key2' => array('Key2' => 'value2', 'Key3' => 'value3'));
        $expected = array('Key1' => array('Key2' => 'value2', 'Key3' => 'value3'));
        $actual = deepArrayDiff($tempArray1, $tempArray2);
        self::assertSame($expected, $actual);

        //complex arrays with different child node
        $tempArray1 = array('Key1' => array('Key2' => 'value2', 'Key3' => 'value3'));
        $tempArray2 = array('Key1' => array('Key2' => 'value2', 'Key4' => 'value4'));
        $expected = array('Key1' => array('Key3' => 'value3'));
        $actual = deepArrayDiff($tempArray1, $tempArray2);
        self::assertSame($expected, $actual);
    }

    public function testsetDeepArrayValue(): void
    {
        //execute the method and test if it returns expected values

        //add to existing array
        $tempArray = array('Key1' => array('Key2' => 'value2', 'Key3' => 'value3'));
        $expected = array('Key1' => array('Key2' => 'value2', 'Key3' => 'value3'), 'key4' => 'value4');
        setDeepArrayValue($tempArray, 'key4', 'value4');
        self::assertSame($tempArray, $expected);

        //add to empty array
        $tempArray = array();
        $expected = array('key1' => array('key2' => array('key3' => 'value3')));
        setDeepArrayValue($tempArray, 'key1_key2_key3', 'value3');
        //var_dump($tempArray);
        self::assertSame($tempArray, $expected);
    }

    public function testarray_merge_values(): void
    {
        //execute the method and test if it returns expected values

        //try with two different length arrays
        $tempArray1 = array('v1', 'v2', 'v3');
        $tempArray2 = array('v4', 'v5');
        $actual = array_merge_values($tempArray1, $tempArray2);
        self::assertFalse($actual);

        //try with same length arrays.
        $tempArray1 = array('v1', 'v2', 'v3');
        $tempArray2 = array('v4', 'v5', 'v6');
        $expected = array('v1v4', 'v2v5', 'v3v6');
        $actual = array_merge_values($tempArray1, $tempArray2);
        self::assertSame($expected, $actual);
    }

    public function testarray_search_insensitive(): void
    {
        //execute the method and test if it returns expected value

        //test with invalid input
        $tempArray = '';
        $actual = array_search_insensitive('', $tempArray);
        self::assertFalse($actual);

        //test with invalid needle..
        $tempArray = array('Key1' => 'value1', 'Key2' => 'value2', 'Key3' => 'value3', 'key4' => 'value4');
        $actual = array_search_insensitive('', $tempArray);
        self::assertFalse($actual);

        //test with valid needle and haystack.
        $tempArray = array('Key1' => 'value1', 'Key2' => 'value2', 'Key3' => 'value3', 'key4' => 'value4');
        $actual = array_search_insensitive('value4', $tempArray);
        self::assertTrue($actual);
    }

    public function testget(): void
    {
        //execute the method and test if it returns expected values

        //test for a top level key
        $tempArray = new SugarArray(array('Key1' => array('Key2' => 'value2', 'Key3' => 'value3'), 'key4' => 'value4'));
        $expected = 'value4';
        $actual = $tempArray->get('key4');
        self::assertSame($expected, $actual);

        //test for a child level key with dot notation
        $tempArray = new SugarArray(array('key1' => array('key2' => 'value2', 'key3' => 'value3'), 'key4' => 'value4'));
        $expected = 'value3';
        $actual = $tempArray->get('key1.key3');
        self::assertSame($expected, $actual);
    }

    public function teststaticGet(): void
    {
        //execute the method and test if it returns expected values

        //test for a top level key
        $haystack = array('key1' => array('key2' => 'value2', 'key3' => 'value3'), 'key4' => 'value4');
        $expected = 'value4';
        $actual = SugarArray::staticGet($haystack, 'key4');
        self::assertSame($expected, $actual);

        //test for a child level key with dot notation
        $haystack = array('key1' => array('key2' => 'value2', 'key3' => 'value3'), 'key4' => 'value4');
        $expected = 'value3';
        $actual = SugarArray::staticGet($haystack, 'key1.key3');
        self::assertSame($expected, $actual);
    }

    /**
     * This function tests fixIndexArrayFormat() function.
     * The idea is that both arrays represents the same index definition, one defined in vardefs.php
     * and the other obtained with get_indices() function.
     * After applying fixIndexArrayFormat() to both arrays we compare it as compareVarDefs() function does:
     *
     */
    public function testfixIndexArrayFormat(): void
    {
        $index1 = [
            'user_name',
            'is_group',
            'status',
            'last_name (30)',
            'first_name (30)',
            'id'
        ];

        $index2 = [
            'user_name',
            'is_group',
            'status',
            'last_name    (  30 ) ',
            'first_name  ( 30  ) ',
            'id'
        ];

        $index1 = fixIndexArrayFormat($index1);
        $index2 = fixIndexArrayFormat($index2);
        self::assertEquals(array_map('strtolower', $index1), array_map('strtolower', $index2));

        $index3 = [
            'user_name',
            'is_group',
            'status',
            'last_name (30)',
            'first_name (30)',
            'id'
        ];

        $index4 = [
            'user_name',
            'is_group',
            'status',
            'last_name    (  30 )',
            'first_name  ( 50  )',
            'id'
        ];

        $index3 = fixIndexArrayFormat($index3);
        $index4 = fixIndexArrayFormat($index4);
        self::assertNotEquals(array_map('strtolower', $index3), array_map('strtolower', $index4));
    }
}
