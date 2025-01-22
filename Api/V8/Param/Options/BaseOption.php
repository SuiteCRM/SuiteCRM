<?php
namespace Api\V8\Param\Options;

use Api\V8\BeanDecorator\BeanManager;
use Api\V8\Factory\ValidatorFactory;
use Symfony\Component\OptionsResolver\OptionsResolver;

#[\AllowDynamicProperties]
abstract class BaseOption
{
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
     * @param OptionsResolver $resolver
     *
     * @return void
     */
    abstract public function add(OptionsResolver $resolver);

    /**
     * @param string $class
     * @see https://github.com/rappasoft/laravel-helpers#class_basename
     *
     * @return string
     */
    protected function getOptionName($class)
    {
        return lcfirst(basename(str_replace('\\', '/', $class)));
    }
}
