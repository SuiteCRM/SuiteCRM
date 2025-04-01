<?php
namespace Api\V8\Param;

use Api\V8\BeanDecorator\BeanManager;
use Api\V8\Factory\ValidatorFactory;
use Api\V8\Param\Options\BaseOption;
use Symfony\Component\OptionsResolver\OptionsResolver;

#[\AllowDynamicProperties]
abstract class BaseParam implements \JsonSerializable
{
    /**
     * @var array
     */
    protected $parameters = [];

    /**
     * @var ValidatorFactory
     */
    protected $validatorFactory;

    /**
     * @var BeanManager
     */
    protected $beanManager;

    /**
     * @param ValidatorFactory $validatorFactory
     * @param BeanManager $beanManager
     */
    public function __construct(ValidatorFactory $validatorFactory, BeanManager $beanManager)
    {
        $this->validatorFactory = $validatorFactory;
        $this->beanManager = $beanManager;
    }

    /**
     * @param array $arguments
     *
     * @return self
     */
    final public function configure(array $arguments)
    {
        $optionsResolver = new OptionsResolver();
        $this->setDefined($optionsResolver, $arguments);
        $this->configureParameters($optionsResolver);
        $this->parameters = $optionsResolver->resolve($arguments);

        return $this;
    }

    /**
     * We can overwrite this method, if necessary
     *
     * @param OptionsResolver $resolver
     * @param array $arguments
     */
    public function setDefined(OptionsResolver $resolver, array $arguments)
    {
    }

    /**
     * Configure parameters.
     *
     * @param OptionsResolver $resolver
     *
     * @return void
     */
    abstract protected function configureParameters(OptionsResolver $resolver);

    /**
     * Configure already defined options.
     *
     * @param OptionsResolver $optionResolver
     * @param array $options
     *
     * @throws \InvalidArgumentException If option is not exist.
     */
    protected function setOptions(OptionsResolver $optionResolver, array $options)
    {
        foreach ($options as $key => $option) {
            if (!class_exists($option)) {
                throw new \InvalidArgumentException(sprintf('Option %s does not exist!', $option));
            }

            /** @var BaseOption $class */
            $class = new $option($this->validatorFactory, $this->beanManager);
            $class->add($optionResolver);
        }
    }

    /**
     * @inheritdoc
     */
    public function jsonSerialize()
    {
        return $this->parameters;
    }
}
