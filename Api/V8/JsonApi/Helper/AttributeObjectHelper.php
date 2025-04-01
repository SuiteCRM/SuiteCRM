<?php
namespace Api\V8\JsonApi\Helper;

use Api\V8\BeanDecorator\BeanManager;
use Api\V8\JsonApi\Response\AttributeResponse;

#[\AllowDynamicProperties]
class AttributeObjectHelper
{
    /**
     * @var BeanManager
     */
    protected $beanManager;

    /**
     * @param BeanManager $beanManager
     */
    public function __construct(BeanManager $beanManager)
    {
        $this->beanManager = $beanManager;
    }

    /**
     * @param \SugarBean $bean
     * @param array|null $fields
     *
     * @return AttributeResponse
     */
    public function getAttributes(\SugarBean $bean, $fields = null)
    {
        $bean->fixUpFormatting();

        $allowedField = [];

        $fieldsToParse = $fields;
        if (empty($fields)) {
            $fieldsToParse =  array_keys($bean->field_defs);
        }

        foreach ($fieldsToParse ?? [] as $index => $field) {
            $isSensitive = isTrue($bean->field_defs[$field]['sensitive'] ?? false);
            $notApiVisible = isFalse($bean->field_defs[$field]['api-visible'] ?? true);

            if ($isSensitive || $notApiVisible){
                continue;
            }

            $allowedField[$index] = $field;
        }

        // using the ISO 8601 format for dates
        $attributes = array_map(function ($value) {
            return is_string($value)
                ? (\DateTime::createFromFormat('Y-m-d H:i:s', $value)
                    ? date(\DateTime::ATOM, strtotime($value))
                    : $value)
                : $value;
        }, $bean->toArray());

        if ($allowedField !== null) {
            $attributes = array_intersect_key($attributes, array_flip($allowedField));
        }

        unset($attributes['id']);

        return new AttributeResponse($attributes);
    }
}
