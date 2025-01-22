<?php
namespace Api\V8\Param;

use Api\V8\Param\Options as ParamOption;
use Symfony\Component\OptionsResolver\OptionsResolver;
use Symfony\Component\Validator\Constraints as Assert;

#[\AllowDynamicProperties]
class CreateModuleDataParams extends BaseParam
{
    /**
     * @return string
     */
    public function getType()
    {
        return $this->parameters['type'];
    }

    /**
     * @return string|null
     */
    public function getId()
    {
        return isset($this->parameters['id']) ? $this->parameters['id'] : null;
    }

    /**
     * @return array
     */
    public function getAttributes()
    {
        return isset($this->parameters['attributes']) ? $this->parameters['attributes'] : [];
    }

    /**
     * @inheritdoc
     */
    protected function configureParameters(OptionsResolver $resolver)
    {
        // need to make this more simple
        $resolver
            ->setDefined('id')
            ->setAllowedTypes('id', 'string')
            ->setAllowedValues('id', $this->validatorFactory->createClosure([
                new Assert\NotBlank(),
                new Assert\Uuid(['strict' => false]),
            ]));

        $this->setOptions(
            $resolver,
            [
                ParamOption\Type::class,
                ParamOption\Attributes::class,
            ]
        );
    }
}
