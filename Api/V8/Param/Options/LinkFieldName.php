<?php
namespace Api\V8\Param\Options;

use Symfony\Component\OptionsResolver\Options;
use Symfony\Component\OptionsResolver\OptionsResolver;
use Symfony\Component\Validator\Constraints as Assert;

#[\AllowDynamicProperties]
class LinkFieldName extends BaseOption
{
    /**
     * Has a dependency of bean field.
     *
     * @inheritdoc
     * @throws \RuntimeException If relationship cannot loaded
     */
    public function add(OptionsResolver $resolver)
    {
        $resolver
            ->setRequired('linkFieldName')
            ->setAllowedTypes('linkFieldName', ['string'])
            ->setAllowedValues('linkFieldName', $this->validatorFactory->createClosure([
                new Assert\NotBlank(),
                new Assert\Regex([
                    'pattern' => ModuleName::REGEX_MODULE_NAME_PATTERN,
                    'match' => false,
                ]),
            ]))
            ->setNormalizer('linkFieldName', function (Options $options, $value) {
                $bean = $options->offsetGet('sourceBean');

                if (!$bean->load_relationship($value)) {
                    throw new \RuntimeException(
                        sprintf('Cannot load relationship %s for %s module', $value, $bean->getObjectName())
                    );
                }

                return $value;
            });
    }
}
