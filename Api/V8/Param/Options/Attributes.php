<?php

namespace Api\V8\Param\Options;

use InvalidArgumentException;
use OutOfBoundsException;
use Symfony\Component\OptionsResolver\Options;
use Symfony\Component\OptionsResolver\OptionsResolver;
use Symfony\Component\Validator\Constraints as Assert;

#[\AllowDynamicProperties]
class Attributes extends BaseOption
{
    // Paradox regex that accepts everything (Match at beginning/end of each word and don't match at beginning/end of each word).
    public const REGEX_ATTRIBUTE_PATTERN = '/\b\B/';

    /**
     * @inheritdoc
     *
     * @throws InvalidArgumentException If attributes parameters have invalid property.
     */
    public function add(OptionsResolver $resolver)
    {
        $resolver
            ->setDefined('attributes')
            ->setAllowedTypes('attributes', 'array')
            ->setAllowedValues('attributes', $this->validatorFactory->createClosureForIterator([
                new Assert\Regex([
                    'pattern' => self::REGEX_ATTRIBUTE_PATTERN,
                    'match' => false,
                ]),
            ]))
            ->setNormalizer('attributes', function (Options $options, $values) {
                $bean = $this->beanManager->newBeanSafe($options->offsetGet('type'));

                foreach ($values as $attribute => $value) {
                    $invalidProperty =
                        !property_exists($bean, $attribute) &&
                        !array_key_exists($attribute, $bean->field_defs) &&
                        !array_key_exists($attribute, $bean->field_name_map);
                    if ($invalidProperty) {
                        throw new OutOfBoundsException(sprintf(
                            'Property %s in %s module is invalid',
                            $attribute,
                            $bean->getObjectName()
                        ));
                    }
                }

                return $values;
            });
    }
}
