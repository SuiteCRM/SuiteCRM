<?php
namespace Api\V8\Param;

use Api\V8\Param\Options as ParamOption;
use Symfony\Component\OptionsResolver\OptionsResolver;

#[\AllowDynamicProperties]
class GetModulesParams extends BaseParam
{
    /**
     * @return string
     */
    public function getModuleName()
    {
        return $this->parameters['moduleName'];
    }

    /**
     * @return array|null
     */
    public function getFields()
    {
        return isset($this->parameters['fields']) ? $this->parameters['fields'] : null;
    }

    /**
     * @return PageParams
     */
    public function getPage()
    {
        return isset($this->parameters['page'])
            ? $this->parameters['page']
            : new PageParams($this->validatorFactory, $this->beanManager);
    }

    /**
     * @return string
     */
    public function getSort()
    {
        return isset($this->parameters['sort']) ? $this->parameters['sort'] : '';
    }

    /**
     * @return string
     */
    public function getFilter()
    {
        return isset($this->parameters['filter']) ? $this->parameters['filter'] : '';
    }

    /**
     * @return bool
     */
    public function getDeleted()
    {
        return strpos($this->getFilter(), ".deleted = '1'") !== false;
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
                ParamOption\Fields::class,
                ParamOption\Page::class,
                ParamOption\Sort::class,
                ParamOption\Filter::class
            ]
        );
    }
}
