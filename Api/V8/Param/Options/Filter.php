<?php
namespace Api\V8\Param\Options;

use Api\V8\JsonApi\Repository\Filter as FilterRepository;
use Symfony\Component\OptionsResolver\Options;
use Symfony\Component\OptionsResolver\OptionsResolver;
use Symfony\Component\Validator\Constraints as Assert;

#[\AllowDynamicProperties]
class Filter extends BaseOption
{
    /**
     * @inheritdoc
     */
    public function add(OptionsResolver $resolver)
    {
        $resolver
            ->setDefaults(['filter' => [
                'deleted' => [
                    'eq' => 0
                ]]])
            ->setDefined('filter')
            ->setAllowedTypes('filter', 'array')
            ->setAllowedValues('filter', $this->validatorFactory->createClosure([
                new Assert\NotBlank(),
            ]))
            ->setNormalizer('filter', function (Options $options, $values) {
                // we don't support multiple level filtering. for now.
                if ($options->offsetExists('linkFieldName')) {
                    $bean = $this->beanManager->getLinkedFieldBean($options->offsetGet('sourceBean'), $options->offsetGet('linkFieldName'));
                } else {
                    $bean = $this->beanManager->newBeanSafe($options->offsetGet('moduleName'));
                }
                $filter = new FilterRepository($bean->db);

                return $filter->parseWhere($bean, $values);
            });
    }
}
