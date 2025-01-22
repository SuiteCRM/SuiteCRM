<?php
namespace Api\V8\Param\Options;

use Api\V8\JsonApi\Repository\Sort as SortRepository;
use Symfony\Component\OptionsResolver\Options;
use Symfony\Component\OptionsResolver\OptionsResolver;
use Symfony\Component\Validator\Constraints as Assert;

#[\AllowDynamicProperties]
class Sort extends BaseOption
{
    public const REGEX_SORT_PATTERN = '/[^\w\-]/';

    /**
     * @inheritdoc
     */
    public function add(OptionsResolver $resolver)
    {
        $resolver
            ->setDefined('sort')
            ->setAllowedTypes('sort', 'string')
            ->setAllowedValues('sort', $this->validatorFactory->createClosure([
                new Assert\NotBlank(),
                new Assert\Regex([
                    'pattern' => self::REGEX_SORT_PATTERN,
                    'match' => false,
                ]),
            ], true))
            ->setNormalizer('sort', function (Options $options, $value) {
                if ($options->offsetExists('linkFieldName')) {
                    $bean = $this->beanManager->getLinkedFieldBean($options->offsetGet('sourceBean'), $options->offsetGet('linkFieldName'));
                } else {
                    $bean = $this->beanManager->newBeanSafe($options->offsetGet('moduleName'));
                }
                $sort = new SortRepository();

                return $sort->parseOrderBy($bean, $value);
            });
    }
}
