<?php
namespace Api\V8\Param;

use Api\V8\BeanDecorator\BeanManager;
use Symfony\Component\OptionsResolver\OptionsResolver;
use Symfony\Component\Validator\Constraints as Assert;

#[\AllowDynamicProperties]
class PageParams extends BaseParam
{
    /**
     * @return integer
     */
    public function getSize()
    {
        return isset($this->parameters['size']) ? (int)$this->parameters['size'] : BeanManager::DEFAULT_ALL_RECORDS;
    }

    /**
     * @return integer
     */
    public function getNumber()
    {
        return isset($this->parameters['number']) ? (int)$this->parameters['number'] : BeanManager::DEFAULT_OFFSET;
    }

    /**
     * @inheritdoc
     */
    protected function configureParameters(OptionsResolver $resolver)
    {
        $resolver
            ->setDefined('size')
            ->setAllowedTypes('size', 'string')
            ->setAllowedValues('size', $this->validatorFactory->createClosure([
                new Assert\GreaterThan(0),
            ]));

        $resolver
            ->setDefined('number')
            ->setAllowedTypes('number', 'string')
            ->setAllowedValues('number', $this->validatorFactory->createClosure([
                new Assert\GreaterThan(0),
            ]));
    }
}
