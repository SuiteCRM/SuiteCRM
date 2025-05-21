<?php
namespace Consolidation\AnnotatedCommand\Parser\Internal;

use Consolidation\AnnotatedCommand\Attributes\AttributeInterface;
use Consolidation\AnnotatedCommand\Parser\CommandInfo;

/**
 * Given a class and method name, let each attribute handle its own
 * properties, populating the CommandInfo object.
 */
class AttributesDocBlockParser
{
    protected $commandInfo;
    protected $reflection;
    protected $fqcnCache;

    public function __construct(CommandInfo $commandInfo, \ReflectionMethod $reflection, $fqcnCache = null)
    {
        $this->commandInfo = $commandInfo;
        $this->reflection = $reflection;
        // @todo Unused. Lets just remove from this class?
        $this->fqcnCache = $fqcnCache ?: new FullyQualifiedClassCache();
    }

    /**
     * Call the handle method of each attribute, which alters the CommandInfo object.
     */
    public function parse()
    {
        $attributes = $this->reflection->getAttributes();
        foreach ($attributes as $attribute) {
            if (method_exists($attribute->getName(), 'handle')) {
                call_user_func([$attribute->getName(), 'handle'], $attribute, $this->commandInfo);
            }
        }

        // If 'CLI\Help' is not defined, then get the help from the docblock comment
        if (!$this->commandInfo->hasHelp()) {
            $doc = $this->reflection->getDocComment();
            $doc = DocBlockUtils::stripLeadingCommentCharacters($doc);

            $lines = explode("\n", $doc);

            // Everything up to the first blank line goes in the description.
            $description = array_shift($lines);
            while (DocBlockUtils::nextLineIsNotEmpty($lines)) {
                $description .= ' ' . array_shift($lines);
            }

            // Everything else goes in the help, up to the first @annotation
            // (e.g. @param)
            $help = '';
            foreach ($lines as $line) {
                if (preg_match('#^[ \t]*@#', $line)) {
                    break;
                }
                $help .= $line . PHP_EOL;
            }

            $this->commandInfo->setDescription($description);
            $this->commandInfo->setHelp(trim($help));
        }
    }
}
