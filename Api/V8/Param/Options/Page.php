<?php
namespace Api\V8\Param\Options;

use Api\V8\Param\PageParams;
use Symfony\Component\OptionsResolver\Options;
use Symfony\Component\OptionsResolver\OptionsResolver;
use Symfony\Component\Validator\Constraints as Assert;

#[\AllowDynamicProperties]
class Page extends BaseOption
{
    public const REGEX_PAGE_PATTERN = '/[^\d]/';

    /**
     * @inheritdoc
     */
    public function add(OptionsResolver $resolver)
    {
        $resolver
            ->setDefined('page')
            ->setAllowedTypes('page', 'array')
            ->setAllowedValues('page', $this->validatorFactory->createClosureForIterator([
                new Assert\NotBlank(),
                new Assert\Regex([
                    'pattern' => self::REGEX_PAGE_PATTERN,
                    'match' => false,
                ]),
            ], true))
            ->setNormalizer('page', function (Options $options, $values) {
                $pageParams = new PageParams($this->validatorFactory, $this->beanManager);
                $pageParams->configure($values);

                return $pageParams;
            });
    }
}
