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

namespace SuiteCRM\API\JsonApi\v1\Filters\Parsers;

use SuiteCRM\API\JsonApi\v1\Filters\Interfaces\OperatorInterface;
use Psr\Container\ContainerInterface;
use SuiteCRM\API\JsonApi\v1\Filters\Operators\FieldOperator;
use SuiteCRM\API\JsonApi\v1\Filters\Operators\Operator;
use SuiteCRM\API\JsonApi\v1\Filters\Validators\FieldValidator;
use SuiteCRM\API\JsonApi\v1\Filters\Validators\FilterValidator;
use SuiteCRM\API\v8\Exception\BadRequestException;
use SuiteCRM\Exception\Exception;
use SuiteCRM\Exception\InvalidArgumentException;

/**
 * Class FilterParser
 * @package SuiteCRM\API\JsonApi\v1\Filters\Parsers
 */
#[\AllowDynamicProperties]
class FilterParser
{
    /**
     * @var ContainerInterface
     */
    private $containers;

    /**
     * @var FieldOperator $fieldOperator
     */
    private static $fieldOperator;

    /**
     * @var \SuiteCRM\API\JsonApi\v1\Filters\Interfaces\OperatorInterface[] $filterOperators
     */
    private static $filterOperators;

    /**
     * @var \SuiteCRM\API\JsonApi\v1\Filters\Interfaces\OperatorInterface[]  $filterFieldOperators
     */
    private static $filterFieldOperators;

    /**
     * @var \SuiteCRM\API\JsonApi\v1\Filters\Interfaces\OperatorInterface[]  $filterFieldOperators
     */
    private static $filterSpecialOperators;


    /**
     * FilterParser constructor.
     * @param ContainerInterface $containers
     */
    public function __construct(ContainerInterface $containers)
    {
        $this->containers = $containers;

        if (self::$fieldOperator === null) {
            self::$fieldOperator = new FieldOperator($containers);
        }

        if (self::$filterOperators === null) {
            self::$filterOperators = $containers->get('FilterOperators');
        }

        if (self::$filterFieldOperators === null) {
            self::$filterFieldOperators = $containers->get('FilterFieldOperators');
        }

        if (self::$filterSpecialOperators === null) {
            self::$filterSpecialOperators = $containers->get('FilterSpecialOperators');
        }
    }


    /**
     * @param string $filterKey
     * @param string $filterValue
     * @return array
     */
    public function parseFilter($filterKey, $filterValue)
    {
        if (empty($filterKey)) {
            // predefined filter eg roi
            $response = array($filterValue);
        } else {
            $filterKeyArray = $this->parseFieldKey($filterKey);
            $filterValueArray = $this->parseFieldFilter($filterValue);
            $response = $this->mergeValueToFilterKey($filterKeyArray, $filterValueArray);
        }

        return $response;
    }

    /**
     * @param $fieldKey
     * @return array
     * @throws Exception
     */
    protected function parseFieldKey($fieldKey)
    {
        if (is_string($fieldKey) === false) {
            throw new Exception(
                '[JsonApi][v1][FilterParser][parseFieldKey][expected type to be string] $fieldKey'
            );
        }

        if (strpos($fieldKey, '.') !== false) {
            $parsedKey = $this->splitFieldKeys($fieldKey);
        } else {
            $parsedKey = array(
                self::$fieldOperator->toFilterTag($fieldKey) => array()
            );
        }

        return $parsedKey;
    }

    /**
     * Convert file into tree structure
     * eg 'Accounts.contacts.name' -> [ 'Accounts' => [ 'contacts' => 'name' => [] ] ]
     * @param string $fieldKey
     * @param string $delimiter
     * @return array
     * @throws BadRequestException
     * @throws Exception
     * @throws InvalidArgumentException
     */
    protected function splitFieldKeys($fieldKey, $delimiter = '.')
    {
        $response = array();
        //
        if (is_string($fieldKey) === false) {
            throw new InvalidArgumentException(
                '[JsonApi][v1][Filters][Parsers][FilterParser]' .
                '[splitFieldKeys][expected type to be string] $fieldKey'
            );
        }

        if (strpos($fieldKey, $delimiter) === false) {
            throw new Exception(
                '[JsonApi][v1][Filters][Parsers][FilterParser]' .
                '[splitFieldKeys][InvalidValue] expected period "' . $fieldKey . '"'
            );
        }

        $flatDataStructure = explode('.', $fieldKey);
        if (is_array($flatDataStructure)) {
            //
            // convert the flat data structure
            $treeDataStructure = array();
            $fieldValidator = new FieldValidator($this->containers);
            $nodeReference = &$flatDataStructure;

            foreach ($flatDataStructure as $index => $attribute) {
                $fieldAttribute = self::$fieldOperator->toFilterTag($attribute);
                // validate field name
                if ($fieldValidator->isValid($fieldAttribute) === false) {
                    throw new Exception('[JsonApi][v1][Filters][FilterParser][splitFieldKeys][InvalidValue] "' . $fieldAttribute . '"');
                }

                // set the root node
                if ($index === 0) {
                    $treeDataStructure[$fieldAttribute] = array();
                    $nodeReference = &$treeDataStructure[$fieldAttribute];
                    continue;
                }

                // set the child nodes
                $nodeReference[$fieldAttribute] = array();
                $nodeReference = &$nodeReference[$fieldAttribute];
            }

            $response = $treeDataStructure;
        } else {
            throw new BadRequestException('[JsonApi][v1][Filters][FilterParser][splitFieldKeys][unable to split value] "' . $fieldKey . '"');
        }


        return $response;
    }

    /**
     * @param string $filters
     * @return array
     * @throws Exception
     */
    protected function parseFieldFilter($filters)
    {
        $standardOperator = new Operator($this->containers);
        $fieldOperator = new FieldOperator($this->containers);
        $specialOperator = new Operator($this->containers);
        $filterValidator = new FilterValidator($this->containers);
        $parsedValues =  array();
        // Parse values handle single filter vs an array of filters
        if (strpos($filters, ',')) {
            $values = $this->splitValues($filters);
        } else {
            $values =  array(
                $filters
            );
        }

        /**
         *
         * Valid cases:
         *      operand
         *      [field]
         *      [[[special_operator]]]
         *      [[operator]]operand
         *      [[operator]]
         *      [[operator]][field]
         *      [[operator]][[[special_operator]]]
         */
        foreach ($values as $value) {
            if ($standardOperator->hasOperator($value)) {
                $operand = '';
                $operators = '';
                $operatorsMatches= array();
                $operatorsArray= array();
                if (preg_match_all('/\[+[A-Za-z0-9\_\-\.]+\]+/', (string) $value, $operatorsMatches) !== false) {
                    // split operators in from their operands
                    foreach ($operatorsMatches[0] as $operator) {
                        // Field Operators [field.fieldname]
                        if ($this->isInOperatorsArray($operator, self::$filterFieldOperators)) {
                            $operators .= $operator;
                            $operatorsArray[] = $operator;
                        } elseif ($this->isInOperatorsArray($operator, self::$filterSpecialOperators)) {
                            $operators .= $operator;
                            $operatorsArray[] = $operator;
                        } elseif ($this->isInOperatorsArray($operator, self::$filterOperators)) {
                            $operators .= $operator;
                            $operatorsArray[] = $operator;
                        } else {
                            throw new Exception(
                                '[JsonApi][v1][Filters][Parsers][FilterParser]' .
                               '[parserFieldFilters][operator not found] please ensure that an operator has been added to '.
                               'containers '
                           );
                        }
                    }
                }

                $parsedValues = $operatorsArray;
                $diff = $this->stringDifference($value, $operators);
                if (empty($diff) === false) {
                    // add operands to the end or data structure
                    $parsedValues[] = $diff;
                }
            } else {
                $parsedValues[] = $value;
            }
        }

        return $parsedValues;
    }

    /**
     * @param string $fieldKey
     * @param string $delimiter
     * @return array
     * @throws Exception
     * @throws InvalidArgumentException
     */
    protected function splitValues($fieldKey, $delimiter = ',')
    {
        if (is_string($fieldKey) === false) {
            throw new InvalidArgumentException(
                '[JsonApi][v1][Filters][Parsers][FilterParser]' .
                '[splitValues][expected type to be string] $fieldKey'
            );
        }

        if (strpos($fieldKey, $delimiter) === false) {
            throw new Exception(
                '[JsonApi][v1][Filters][Parsers][FilterParser]' .
                '[splitValues][InvalidValue] expected delimiter "' . $fieldKey . '"'
            );
        }

        return explode($delimiter, $fieldKey);
    }

    /**
     * @param string $operatorNeedle
     * @param OperatorInterface[] $operatorsHaystack
     * @return bool
     */
    protected function isInOperatorsArray($operatorNeedle, array $operatorsHaystack)
    {
        foreach ($operatorsHaystack as $operator) {
            /** @var OperatorInterface $operator */
            if ($operator->isOperator($operatorNeedle)) {
                return true;
            }
        }
        return false;
    }

    /**
     * @param string $a
     * @param string $b
     * @return string
     */
    private function stringDifference($a, $b)
    {
        $aArray = str_split($a);
        $bArray = str_split($b);
        $arrayDiff = array_diff($aArray, $bArray);
        return implode('', $arrayDiff);
    }

    /**
     * @param array $filterKeyArray
     * @param array $filterValueArray
     * @return array
     */
    private function mergeValueToFilterKey(array $filterKeyArray, array $filterValueArray)
    {
        $filterStructure = $filterKeyArray;
        if (empty($filterKeyArray)) {
            return $filterValueArray;
        }

        $nodeReference = &$filterStructure;
        $isLeaf = false;
        do {
            if (is_array(current($nodeReference))) {
                $nodeReference = & $nodeReference[key($nodeReference)];
            } else {
                $isLeaf = true;
                $nodeReference = $filterValueArray;
            }
        } while ($isLeaf === false);

        return $filterStructure;
    }
}
