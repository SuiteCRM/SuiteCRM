<?php
namespace Api\V8\JsonApi\Repository;

#[\AllowDynamicProperties]
class Filter
{
    // operators so far
    public const OP_EQ = '=';
    public const OP_NEQ = '<>';
    public const OP_GT = '>';
    public const OP_GTE = '>=';
    public const OP_LT = '<';
    public const OP_LTE = '<=';
    public const OP_LIKE = 'LIKE';

    public const OP_IN = 'IN';
    public const OP_NOT_IN = 'NOT IN';

    public const OP_AND = 'AND';
    public const OP_OR = 'OR';

    /**
     * @var \DBManager
     */
    private $db;

    /**
     * @param \DBManager $db
     */
    public function __construct(\DBManager $db)
    {
        $this->db = $db;
    }

    /**
     * @param \SugarBean $bean
     * @param array $params
     *
     * @return string
     * @throws \InvalidArgumentException When field is not found or is not an array.
     */
    public function parseWhere(\SugarBean $bean, array $params)
    {
        $operator = self::OP_AND;
        if (isset($params['operator'])) {
            $this->checkOperator($params['operator']);
            $operator = strtoupper($params['operator']);
            unset($params['operator']);
        }

        $deleted = false;
        if (isset($params['deleted'])) {
            if (isset($params['deleted']['eq'])) {
                $deleted = ($params['deleted']['eq'] == 1);
            }
            
            unset($params['deleted']);
        }

        $where = [];
        foreach ($params as $field => $expr) {
            if (empty($bean->field_defs[$field])) {
                throw new \InvalidArgumentException(sprintf(
                    'Filter field %s in %s module is not found',
                    $field,
                    $bean->getObjectName()
                ));
            }

            if (!is_array($expr)) {
                throw new \InvalidArgumentException(sprintf('Filter field %s must be an array', $field));
            }

            $isCustom = isset($bean->field_defs[$field]['source']) && ($bean->field_defs[$field]['source'] == 'custom_fields');
            $tableName = $isCustom ? $bean->get_custom_table_name() : $bean->getTableName();

            foreach ($expr as $op => $value) {
                $this->checkOperator($op);
                $sqlOperator = constant(sprintf('%s::OP_%s', self::class, strtoupper($op)));
                if (in_array($sqlOperator, [self::OP_IN, self::OP_NOT_IN])) {
                    if (!is_string($value)) {
                        // If it's not a string, throw an exception.
                        throw new \InvalidArgumentException(sprintf(
                            'Value for %s operator on field %s must be a comma-separated string',
                            $sqlOperator,
                            $field
                        ));
                    }
                    $items = array_map('trim', explode(',', $value));
                    if (empty($items)) {
                        // For empty IN lists, return a condition that is always false to prevent
                        // querying all records (e.g., SELECT * FROM table WHERE 1=0)
                        $where[] = '1=0';
                    } else {
                        // Quote each value in the array and then implode them for the SQL string
                        $quotedValues = array_map(function ($item) {
                            if (is_numeric($item) && !is_string($item)) {
                                return $item; // Return raw number
                            }
                            if (is_bool($item)) {
                                return (int)$item; // Convert true/false to 1/0
                            }
                            return $this->db->quoted($item); // Quote strings
                        }, $items);

                        $where[] = sprintf(
                            '%s.%s %s (%s)',
                            $tableName,
                            $field,
                            $sqlOperator,
                            implode(', ', $quotedValues)
                        );
                    }
                } else {
                    if (is_numeric($value) && !is_string($value)) {
                        $formattedValue = $value; // Return raw number
                    } elseif (is_bool($value)) {
                        $formattedValue = (int)$value; // Convert true/false to 1/0
                    } else {
                        $formattedValue = $this->db->quoted($value); // Quote strings
                    }
                    
                    $where[] = sprintf(
                        '%s.%s %s %s',
                        $tableName,
                        $field,
                        $sqlOperator,
                        $formattedValue
                    );
                }
            }
        }

        if (empty($where)) {
            return sprintf(
                "%s.deleted = '%d'",
                $bean->getTableName(),
                $deleted
            );
        }

        return sprintf(
            "(%s) AND %s.deleted = '%d'",
            implode(sprintf(' %s ', $operator), $where),
            $bean->getTableName(),
            $deleted
        );
    }

    /**
     * Only return deleted records if they were explicitly requested
     * @deprecated
     * @param array $params
     * @return array
     */
    protected function addDeletedParameter(array $params)
    {
        if (!array_key_exists('deleted', $params)) {
            $params['deleted'] = [
                'eq' => 0
            ];
        }

        return $params;
    }

    /**
     * @param string $op
     *
     * @throws \InvalidArgumentException When the given operator is invalid.
     */
    private function checkOperator($op)
    {
        $operator = sprintf('%s::OP_%s', self::class, strtoupper($op));
        if (!defined($operator)) {
            throw new \InvalidArgumentException(
                sprintf('Filter operator %s is invalid', $op)
            );
        }
    }
}
