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

namespace SuiteCRM\API\JsonApi\v1\Filters\Operators;

use Psr\Container\ContainerInterface;
use SuiteCRM\API\v8\Exception\BadRequestException;
use SuiteCRM\Exception\InvalidArgumentException;

/**
 * Class Operator
 * @package SuiteCRM\API\JsonApi\v1\Filters\Operators
 */
#[\AllowDynamicProperties]
class Operator
{
    /**
     * string representation of what an operator looks like
     * @var string $tag
     */
    protected $tag = '[[operator]]';

    /**
     * Represents an the full operator format
     * @var string $operatorFormatRegex
     */
    protected $operatorFormatRegex = '\[\[[A-Za-z\_\-]+\]\]';


    /**
     * @var ContainerInterface $containers;
     */
    protected $containers;

    public function __construct(ContainerInterface $containers)
    {
        $this->containers = $containers;
    }

    /**
     * Convert string to operator tag
     * @param string $operator
     * @return string
     */
    public function toFilterTag($operator)
    {
        return str_replace('operator', $operator, $this->tag);
    }

    /**
     * Convert operator tag simple string
     * @param string $operator
     * @return string
     */
    public function stripFilterTag($operator)
    {
        $operatorAsArray = str_split($operator);
        $operatorTagAsArray = str_split($this->toFilterTag(' '));
        $arrayDiff = array_diff($operatorAsArray, $operatorTagAsArray);
        return implode('', $arrayDiff);
    }

    /**
     * @param string $operator
     * @return bool
     * @throws \SuiteCRM\Exception\InvalidArgumentException
     */
    public function isValid($operator)
    {
        if (!is_string($operator)) {
            throw new InvalidArgumentException(
                '[JsonApi][v1][Filters][Operators][Operator][isValid][expected type to be string] $operator'
            );
        }

        if (preg_match('/^'.$this->operatorFormatRegex.'$/', $operator, $matches) === 1) {
            return true;
        }

        return false;
    }

    /**
     * @param string $operator
     * @return bool
     */
    public function isOperator($operator)
    {
        return $this->toFilterOperator() === $operator;
    }

    /**
     * @param $filter
     * @return bool
     * @throws InvalidArgumentException
     */
    public function hasOperator($filter)
    {
        if (!is_string($filter)) {
            throw new InvalidArgumentException(
                '[JsonApi][v1][Filters][Operators][Operator][hasOperator][expected type to be string] $operator'
            );
        }

        if (preg_match('/'.$this->operatorFormatRegex.'/', $filter, $matches) === 1) {
            return true;
        }

        return false;
    }

    /**
     * @return int total number of operators or values after the operator
     */
    public function totalOperands()
    {
        return 1;
    }


    /**
     * General case
     * @param array $operands
     * @return string
     * @throws InvalidArgumentException
     * @throws BadRequestException
     */
    public function toSqlOperands(array $operands)
    {
        if (!is_array($operands)) {
            throw new InvalidArgumentException(
                '[JsonApi][v1][Filters][Operators][Operator][toSqlOperands][expected type to be array] $operands'
            );
        }

        /** @var \DBManager $db */
        $db = $this->containers->get('DatabaseManager');
        $db->checkConnection();

        foreach ($operands as $i => $operand) {
            if ($i >= $this->totalOperands()) {
                throw new BadRequestException('[JsonApi][v1][Filters][Operators][Operator][toSqlOperands][operand limit exceeded]');
            }

            if (is_numeric($operand)) {
                $operands[$i] = $db->quote($operand);
            } else {
                $operands[$i] = '"'. $db->quote($operand) .'"';
            }
        }

        return implode(',', $operands);
    }
}
