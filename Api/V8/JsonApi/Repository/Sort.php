<?php
namespace Api\V8\JsonApi\Repository;

#[\AllowDynamicProperties]
class Sort
{
    public const ORDER_BY_ASC = 'ASC';
    public const ORDER_BY_DESC = 'DESC';

    /**
     * We don't support multiple sorting. for now.
     *
     * @param \SugarBean $bean
     * @param string $value
     *
     * @return string
     * @throws \InvalidArgumentException When sort field is not found in the bean.
     */
    public function parseOrderBy(\SugarBean $bean, $value)
    {
        $orderBy = self::ORDER_BY_ASC;
        if (strpos($value, '-') === 0) {
            $orderBy = self::ORDER_BY_DESC;
            $value = ltrim($value, '-');
        }

        if (empty($bean->field_defs[$value])) {
            throw new \InvalidArgumentException(sprintf(
                'Sort field %s in %s module is not found',
                $value,
                $bean->getObjectName()
            ));
        }

        return sprintf('%s %s', $value, $orderBy);
    }
}
