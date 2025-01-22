<?php
namespace Api\V8\Param;

use Api\V8\Param\Options as ParamOption;
use Symfony\Component\OptionsResolver\OptionsResolver;

#[\AllowDynamicProperties]
class UpdateModuleDataParams extends BaseParam
{
    /**
     * @return string
     */
    public function getType()
    {
        return $this->parameters['type'];
    }

    /**
     * @return string
     */
    public function getId()
    {
        return $this->parameters['id'];
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
        $this->setOptions(
            $resolver,
            [
                ParamOption\Type::class,
                ParamOption\Id::class,
                ParamOption\Attributes::class,
            ]
        );
    }
}
