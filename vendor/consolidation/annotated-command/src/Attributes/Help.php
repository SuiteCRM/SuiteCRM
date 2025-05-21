<?php

namespace Consolidation\AnnotatedCommand\Attributes;

use Attribute;
use Consolidation\AnnotatedCommand\Parser\CommandInfo;

#[Attribute(Attribute::TARGET_METHOD)]
class Help
{
    /**
     * @param $description
     *   A one line description.
     * @param $synopsis
     *   A multi-line help text.
     * @param bool $hidden
     *   Hide the command from the help list.
     */
    public function __construct(
        public ?string $description = null,
        public ?string $synopsis = null,
        public bool $hidden = false
    ) {
    }

    public static function handle(\ReflectionAttribute $attribute, CommandInfo $commandInfo)
    {
        $instance = $attribute->newInstance();
        if ($instance->description) {
            $commandInfo->setDescription($instance->description);
        }
        if ($instance->synopsis) {
            $commandInfo->setHelp($instance->synopsis);
        }
        if ($instance->hidden) {
            $commandInfo->setHidden($instance->hidden);
        }
    }
}
