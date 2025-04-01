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

namespace SuiteCRM\API\JsonApi\v1\Filters\Interpreters;

use Behat\Gherkin\Filter\FilterInterface;
use SuiteCRM\API\JsonApi\v1\Filters\Interfaces\OperatorInterface;
use SuiteCRM\API\JsonApi\v1\Filters\Interpreters\ByIdFilters\ByIdFilter;
use SuiteCRM\API\JsonApi\v1\Filters\Operators\FieldOperator;
use SuiteCRM\API\JsonApi\v1\Filters\Operators\Operator;
use SuiteCRM\API\JsonApi\v1\Filters\Validators\FieldValidator;
use SuiteCRM\API\v8\Exception\BadRequestException;
use SuiteCRM\Exception\Exception;
use SuiteCRM\Utility\StringValidator;

use Psr\Container\ContainerInterface;

/**
 * Class FilterInterpreter
 * @package SuiteCRM\API\JsonApi\v1\Filters\Interpreters
 */
#[\AllowDynamicProperties]
class FilterInterpreter
{
    /**
     * @var ContainerInterface $containers
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
     * FilterInterpreter constructor.
     * @param ContainerInterface $containers
     * @throws \Psr\Container\ContainerExceptionInterface
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
     * @param array $filterStructure
     * @return bool
     * @throws InvalidArgumentException
     */
    public function isFilterByPreMadeName(array $filterStructure)
    {
        if (is_array($filterStructure) === false) {
            throw new InvalidArgumentException('[JsonApi][v1][Filters][Interpreters][isFilterByPreMadeName][expected type to be array]');
        }

        return count($filterStructure) === 1 && is_array(current($filterStructure)) === false;
    }

    /**
     * @param array $filterStructure
     * @return bool
     * @throws InvalidArgumentException
     */
    public function isFilterById(array $filterStructure)
    {
        if (is_array($filterStructure) === false) {
            throw new InvalidArgumentException('[JsonApi][v1][Filters][Interpreters][isFilterById][expected type to be array]');
        }

        return count($filterStructure) === 1 &&
            array_keys($filterStructure)[0] === '[id]' &&
            is_array(current($filterStructure)) === true;
    }

    /**
     * @param array $filterStructure  [table => [field => [operator, operand, ... ], ...]
     * @return bool
     * @throws Exception
     */
    public function isFilterByAttributes(array $filterStructure)
    {
        if (is_array($filterStructure) === false) {
            throw new Exception('[JsonApi][v1][Filters][Interpreters][isFilterByAttributes][expected type to be array]');
        }

        $fieldValidator = new FieldValidator($this->containers);
        return count($filterStructure) >= 1 &&
            is_array(current($filterStructure)) === true &&
            array_keys($filterStructure)[0] !== '[id]' &&
        $fieldValidator->isValid(array_keys($filterStructure)[0]);
    }

    /**
     * Convert the filter structure for a parser into an SQL where clause
     * @param array $filterStructure
     * @return string
     * @throws Exception
     */
    public function getFilterByPreMadeName(array $filterStructure)
    {
        $filter = '';
        $filterName = current($filterStructure);
        $interpreters = $this->containers->get('ByPreMadeFilterInterpreters');

        /** @var  \SuiteCRM\API\JsonApi\v1\Filters\Interfaces\ByPreMadeFilterInterpreter $interpreter */
        foreach ($interpreters as $interpreter) {
            if ($interpreter->hasByPreMadeFilter($filterName)) {
                $filter = $interpreter->getByPreMadeFilter();
            }
        }

        if (empty($filter)) {
            throw new Exception('[JsonApi][v1][Filters][Interpreters][getFilterByPreMadeName][cannot find filter]');
        }

        return $filter;
    }

    /**
     * Convert the filter structure for a parser into an SQL where clause
     * @param array $filterStructure  [table => [field => [operator, operand, ... ], ...]
     * @return string|ByIdFilter
     * @throws Exception
     */
    public function getFilterById(array $filterStructure)
    {
        $filter = '';
        /** @var ByIdFilter $filter */
        $interpreter = $this->containers->get('ByIdFilterInterpreter');
        $filter = $interpreter->getByIdFilter($filterStructure);

        if (empty($filter)) {
            if (is_array($filterStructure) === false) {
                throw new Exception('[JsonApi][v1][Filters][Interpreters][getFilterById][cannot find filter]');
            }
        }

        return $filter;
    }

    /**
     * Convert the filter structure for a parser into an SQL where clause
     * @param array $filterStructure [table => [field => [operator, operand, ... ], ...]
     * @param array $args Route arguments
     * @return string
     * @throws BadRequestException
     */
    public function getFilterByAttributes(array $filterStructure, array $args)
    {
        $filter = '';
        $filterOperator = new FieldOperator($this->containers);
        $operator = new Operator($this->containers);
        foreach ($filterStructure as $beanType => $filterFields) {
            //
            if ($filterOperator->isValid($beanType) === false) {
                throw new BadRequestException('[getFilterByAttributes][invalid filter]');
            }

            /** @var \SugarBean $module */
            $module = \BeanFactory::newBean($filterOperator->stripFilterTag($beanType));
            $tableName = $module->table_name;

            // Process fields
            foreach ($filterFields as $field => $fieldOperations) {
                // Get next field
                if ($filterOperator->isValid($field) === false) {
                    throw new BadRequestException('[getFilterByAttributes][invalid field]');
                }
                $fieldName = $filterOperator->stripFilterTag($field);
                if (isset($module->field_defs[$fieldName]) === false) {
                    throw new BadRequestException('[getFilterByAttributes][field does not exist] "'.$fieldName.'"');
                }

                if (is_array($fieldOperations) === false) {
                    throw new BadRequestException('[getFilterByAttributes][operations does not exist]');
                }

                // Build the SQL Query for each operation [operator, [operand, ...] ...] in this filter
                // By iterating through $fieldOperations array
                $index = 0;
                $end = count($fieldOperations);
                $lastOperator = null;
                $operands = array();
                while ($index < $end) {
                    // Lets play: Is this element an operator or an operand?
                    if ($operator->hasOperator(current($fieldOperations))) {
                        // It's an operator
                        if ($lastOperator === null) {
                            // So this is the first operator
                            $lastOperator = $this->getOperator(current($fieldOperations));
                            // So lets keep going ...
                            // Meanwhile lets collect the operands
                            // Until we see an other operator or the end of the array
                            $index++;
                            next($fieldOperations);
                            // Next Operator or Operand
                            continue;
                        }

                        // Here's where the magic happens
                        $filter .= $this->toSqlFilter(
                            $tableName,
                            $filterOperator,
                            $lastOperator,
                            $field,
                            $operands,
                            $args
                        );

                        // Clear the operands for the next operator
                        $operands = array();

                        // We need to start again.
                        // So lets keep going ...
                        // Until we see an other operator or the end of the array
                        $lastOperator = $this->getOperator(current($fieldOperations));
                    } else {
                        // It's an Operand, let's keep looking for more operands
                        // Until we see an other operator or the end of the array
                        $operands[] = current($fieldOperations);
                    }

                    $index++;
                    next($fieldOperations);
                    // Next Operator or Operand
                }

                // Handle the last operator
                // Here's where the magic happens
                $filter .= $this->toSqlFilter(
                    $tableName,
                    $filterOperator,
                    $lastOperator,
                    $field,
                    $operands,
                    $args
                );
            }
        }

        return $filter;
    }

    /**
     * @param string $tableName
     * @param OperatorInterface|FilterInterface $filterOperator
     * @param OperatorInterface|FilterInterface $lastOperator
     * @param string $field
     * @param array $operands
     * @param array $args route arguments
     * @return string
     */
    private function toSqlFilter($tableName, $filterOperator, $lastOperator, $field, array $operands, array $args)
    {
        // detect custom field and change table to {table}_cstm
        if ($this->isCustomField($filterOperator->stripFilterTag($field), $args)) {
            $tableName = $this->toCustomTable($tableName);
        }

        // Lets build the last operation into a SQL Query
        $sqlField = implode('.', array($tableName, $filterOperator->stripFilterTag($field)));
        $sqlOperator = $lastOperator->toSqlOperator();
        $sqlOperands = $lastOperator->toSqlOperands($operands);

        // Here's where the real magic happens
        return implode(' ', array($sqlField, $sqlOperator, $sqlOperands));
    }


    /**
     * @param string $operator
     * @return OperatorInterface
     * @throws Exception
     */
    protected function getOperator($operator)
    {

        //
        $isInOperatorsArray = function ($operatorNeedle, $operatorsHaystack) {
            foreach ($operatorsHaystack as $operator) {
                /** @var OperatorInterface $operator */
                if ($operator->isOperator($operatorNeedle)) {
                    return $operator;
                }
            }
            return false;
        };

        //
        $isInFieldsOperatorsArray = $isInOperatorsArray($operator, self::$filterFieldOperators);
        $isInSpecialOperatorsArray = $isInOperatorsArray($operator, self::$filterSpecialOperators);
        $isInOperatorArray = $isInOperatorsArray($operator, self::$filterOperators);

        if ($isInFieldsOperatorsArray !== false) {
            return $isInFieldsOperatorsArray;
        } elseif ($isInSpecialOperatorsArray !== false) {
            return $isInSpecialOperatorsArray;
        } elseif ($isInOperatorArray !== false) {
            return $isInOperatorArray;
        }

        throw new Exception(
            '[JsonApi][v1][Filters][FilterInterpreter][getOperator]' .
            '[parserFieldFilters][operator not found] please ensure that an operator has been added to '.
            'containers '
        );
    }

    /**
     * @param string $field
     * @param array $args route arguments
     * @return bool
     */
    protected function isCustomField($field, array $args)
    {
        if (!is_string($field)) {
            throw new \InvalidArgumentException('isCustomField requires $field to be a string');
        }

        if (empty($args) || !isset($args['module'])) {
            return false;
        }

        $module = $args['module'];
        $bean = \BeanFactory::newBean($module);

        return $bean->custom_fields->fieldExists($field);
    }

    /**
     * @param string $table
     * @return string custom version of the table
     * @throws \InvalidArgumentException
     */
    protected function toCustomTable($table)
    {
        if (!is_string($table)) {
            throw new \InvalidArgumentException('toCustom requires $table to be a string');
        }

        if (StringValidator::endsWith($table, '_cstm')) {
            return $table;
        }

        return $table . '_cstm';
    }
}
