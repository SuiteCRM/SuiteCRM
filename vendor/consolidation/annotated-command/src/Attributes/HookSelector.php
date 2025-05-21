<?php

namespace Consolidation\AnnotatedCommand\Attributes;

use Attribute;
use Consolidation\AnnotatedCommand\Parser\CommandInfo;

#[Attribute(Attribute::TARGET_METHOD  | \Attribute::IS_REPEATABLE)]
class HookSelector
{
    /**
     * @param $name
     *  The name of the hook selector that must be present for that hook to run.
     * @param $value
     *   A value which can may used by the hook.
     */
    public function __construct(
        public string $name,
        public mixed $value = true,
    ) {
    }

    public static function handle(\ReflectionAttribute $attribute, CommandInfo $commandInfo)
    {
        $instance = $attribute->newInstance();
        $commandInfo->addAnnotation($instance->name, $instance->value);
    }
}
