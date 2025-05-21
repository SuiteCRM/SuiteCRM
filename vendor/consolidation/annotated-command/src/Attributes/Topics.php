<?php

namespace Consolidation\AnnotatedCommand\Attributes;

use Attribute;
use Consolidation\AnnotatedCommand\Parser\CommandInfo;

#[Attribute(Attribute::TARGET_METHOD)]
class Topics
{
    /**
     * @param string[] $topics
     *   An array of topics that are related to this command.
     * @param $isTopic
     *   Mark this command as a topic itself.
     * @param $path
     *   The path to a markdown file, when this command is itself a topic.
     */
    public function __construct(
        public ?array $topics = [],
        public bool $isTopic = false,
        public ?string $path = null,
    ) {
    }

    public static function handle(\ReflectionAttribute $attribute, CommandInfo $commandInfo)
    {
        $instance = $attribute->newInstance();
        $commandInfo->addAnnotation('topics', $instance->topics);
        if ($instance->isTopic || $instance->path) {
            $commandInfo->addAnnotation('topic', $instance->path ?? true);
        }
    }
}
