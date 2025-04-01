<?php
namespace Api\V8\Param;

use Api\V8\Param\Options as ParamOption;
use Symfony\Component\OptionsResolver\Options;
use Symfony\Component\OptionsResolver\OptionsResolver;
use Symfony\Component\Validator\Constraints as Assert;

#[\AllowDynamicProperties]
class CreateRelationshipByLinkParams extends CreateRelationshipParams
{
    /**
     * @return string
     */
    public function getLinkedFieldName()
    {
        return $this->parameters['linkFieldName'];
    }

    /**
     * @inheritdoc
     */
    protected function configureParameters(OptionsResolver $resolver)
    {
        $this->setOptions(
            $resolver,
            [
                ParamOption\ModuleName::class,
                ParamOption\Id::class,
            ]
        );

        $resolver
            ->setRequired('data')
            ->setAllowedTypes('data', 'array')
            ->setAllowedValues('data', $this->validatorFactory->createClosureForIterator([
                new Assert\NotBlank(),
            ]))
            ->setNormalizer('data', function (Options $options, $value) {
                $dataParams = new GetRelationshipDataParams($this->validatorFactory, $this->beanManager);
                $dataParams->configure($value);

                return $dataParams;
            });

        $resolver
            ->setDefined('sourceBean')
            ->setDefault('sourceBean', function (Options $options) {
                return $this->beanManager->getBeanSafe(
                    $options->offsetGet('moduleName'),
                    $options->offsetGet('id')
                );
            })
            ->setAllowedTypes('sourceBean', \SugarBean::class);

        $resolver
            ->setDefined('relatedBean')
            ->setDefault('relatedBean', function (Options $options) {
                $dataParams = $options->offsetGet('data');

                return $this->beanManager->getBeanSafe(
                    $dataParams->getType(),
                    $dataParams->getId()
                );
            })
            ->setAllowedTypes('relatedBean', \SugarBean::class);

        $this->setOptions($resolver, [ParamOption\LinkFieldName::class]);
    }
}
