<?php
namespace Api\V8\Param;

use Symfony\Component\OptionsResolver\Options;
use Symfony\Component\OptionsResolver\OptionsResolver;
use Symfony\Component\Validator\Constraints as Assert;

#[\AllowDynamicProperties]
class UpdateModuleParams extends BaseParam
{
    /**
     * @return CreateModuleDataParams
     */
    public function getData()
    {
        return $this->parameters['data'];
    }

    /**
     * @inheritdoc
     */
    protected function configureParameters(OptionsResolver $resolver)
    {
        // need to make this more simple
        $resolver
            ->setRequired('data')
            ->setAllowedTypes('data', 'array')
            ->setAllowedValues('data', $this->validatorFactory->createClosureForIterator([
                new Assert\NotBlank(),
            ]))
            ->setNormalizer('data', function (Options $options, $values) {
                $dataParams = new UpdateModuleDataParams($this->validatorFactory, $this->beanManager);
                $dataParams->configure($values);

                return $dataParams;
            });
    }
}
