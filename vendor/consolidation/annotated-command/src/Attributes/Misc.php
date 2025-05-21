<?php

namespace Consolidation\AnnotatedCommand\Attributes;

use Attribute;
use Consolidation\AnnotatedCommand\Parser\CommandInfo;

/**
 * @deprecated Removed in 5.x
 */
#[Attribute(Attribute::TARGET_METHOD)]
class Misc
{
    /**
     * @param array $data
     *   An associative array containing arbitrary data.
     */
    public function __construct(
        public array $data,
    ) {
    }

    public static function handle(\ReflectionAttribute $attribute, CommandInfo $commandInfo)
    {
        $args = $attribute->getArguments();
        $commandInfo->AddAnnotation(key($args['data']), current($args['data']));
    }
}
