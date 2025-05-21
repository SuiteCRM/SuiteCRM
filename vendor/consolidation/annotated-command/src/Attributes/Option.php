<?php

namespace Consolidation\AnnotatedCommand\Attributes;

use Attribute;
use Consolidation\AnnotatedCommand\Parser\CommandInfo;

#[Attribute(Attribute::TARGET_METHOD | Attribute::IS_REPEATABLE)]
class Option
{
    /**
     * @param $name
     *   The name of the option.
     * @param $description
     *   A one line description.
     * @param $suggestedValues
     *   An array of suggestions or a Closure which gets them. See https://symfony.com/blog/new-in-symfony-6-1-improved-console-autocompletion#completion-values-in-input-definitions.
     */
    public function __construct(
        public string $name,
        public string $description,
        public array|\Closure $suggestedValues = []
    ) {
    }

    public static function handle(\ReflectionAttribute $attribute, CommandInfo $commandInfo)
    {
        $instance = $attribute->newInstance();
        $commandInfo->addOptionDescription($instance->name, $instance->description, $instance->suggestedValues);
    }
}
