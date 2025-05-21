<?php

namespace Consolidation\AnnotatedCommand\Attributes;

use Attribute;
use Consolidation\AnnotatedCommand\Parser\CommandInfo;

#[Attribute(Attribute::TARGET_METHOD | Attribute::IS_REPEATABLE)]
class Usage
{
    /**
     * @param $name
     *   The example command.
     * @param $description
     *   A one line description.
     */
    public function __construct(
        public string $name,
        public ?string $description = null
    ) {
    }

    public static function handle(\ReflectionAttribute $attribute, CommandInfo $commandInfo)
    {
        $args = $attribute->getArguments();
        $commandInfo->setExampleUsage($args['name'], @$args['description']);
    }
}
