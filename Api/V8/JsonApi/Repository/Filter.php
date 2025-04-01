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
                $where[] = sprintf(
                    '%s.%s %s %s',
                    $tableName,
                    $field,
                    constant(sprintf('%s::OP_%s', self::class, strtoupper($op))),
                    $this->db->quoted($value)
                );
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
