<?php

namespace Consolidation\AnnotatedCommand\Attributes;

use Attribute;
use Consolidation\AnnotatedCommand\Hooks\HookManager;
use Consolidation\AnnotatedCommand\Parser\CommandInfo;
use JetBrains\PhpStorm\ExpectedValues;

#[Attribute(Attribute::TARGET_METHOD)]
class Hook
{
    /**
     * @param $type
     *  When during the command lifecycle this hook will be called (e.g. validate).
     * @param $target
     *   Specifies which command(s) the hook will be attached to.
     * @param $selector
     *   A 'tag' that requests that a hook operate on the current command.
     */
    public function __construct(
        #[ExpectedValues(valuesFromClass: HookManager::class)] public string $type,
        public ?string $target = null,
        public ?string $selector = null
    ) {
    }

    public static function handle(\ReflectionAttribute $attribute, CommandInfo $commandInfo)
    {
        $instance = $attribute->newInstance();
        if ($instance->selector && $instance->target) {
            throw new \Exception('Selector and Target may not be sent in the same Hook attribute.');
        }
        $value = null;
        if ($instance->selector) {
            if (strpos($instance->selector, '@') !== false) {
                throw new \Exception('Selector may not contain an \'@\'');
            }
            $value = '@' . $instance->selector;
        } elseif ($instance->target) {
            $value = $instance->target;
        }
        $commandInfo->setName($value);
        $commandInfo->addAnnotation('hook', $instance->type . ' ' . $value);
    }
}
