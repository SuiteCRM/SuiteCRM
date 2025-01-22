<?php
namespace Api\V8\Param;

use Api\V8\Param\Options as ParamOption;
use Symfony\Component\OptionsResolver\Options;
use Symfony\Component\OptionsResolver\OptionsResolver;

#[\AllowDynamicProperties]
class GetRelationshipParams extends BaseParam
{
    /**
     * @return string
     */
    public function getModuleName()
    {
        return $this->parameters['moduleName'];
    }

    /**
     * @return string
     */
    public function getId()
    {
        return $this->parameters['id'];
    }

    /**
     * @return string
     */
    public function getLinkedFieldName()
    {
        return $this->parameters['linkFieldName'];
    }

    /**
     * @return \SugarBean
     */
    public function getSourceBean()
    {
        return $this->parameters['sourceBean'];
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
            ->setDefined('sourceBean')
            ->setDefault('sourceBean', function (Options $options) {
                return $this->beanManager->getBeanSafe(
                    $options->offsetGet('moduleName'),
                    $options->offsetGet('id')
                );
            })
            ->setAllowedTypes('sourceBean', [\SugarBean::class]);

        // dependency on sourceBean field
        $this->setOptions($resolver, [ParamOption\LinkFieldName::class]);
        $this->setOptions(
            $resolver,
            [
                ParamOption\LinkFieldName::class,
                ParamOption\Page::class,
                ParamOption\Sort::class,
                ParamOption\Filter::class
            ]
        );
    }
}
