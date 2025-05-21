<?php
namespace Consolidation\AnnotatedCommand\Parser\Internal;

use Consolidation\AnnotatedCommand\Parser\CommandInfo;
use Consolidation\AnnotatedCommand\Parser\DefaultsWithDescriptions;

/**
 * Given a class and method name, parse the annotations in the
 * DocBlock comment, and provide accessor methods for all of
 * the elements that are needed to create an annotated Command.
 */
class BespokeDocBlockParser
{
    protected $fqcnCache;
    protected $commandInfo;
    protected $reflection;
    protected $optionParamName;

    /**
     * @var array
     */
    protected $tagProcessors = [
        'command' => 'processCommandTag',
        'name' => 'processCommandTag',
        'arg' => 'processArgumentTag',
        'param' => 'processParamTag',
        'return' => 'processReturnTag',
        'option' => 'processOptionTag',
        'default' => 'processDefaultTag',
        'aliases' => 'processAliases',
        'usage' => 'processUsageTag',
        'description' => 'processAlternateDescriptionTag',
        'desc' => 'processAlternateDescriptionTag',
    ];

    public function __construct(CommandInfo $commandInfo, \ReflectionMethod $reflection, $fqcnCache = null)
    {
        $this->commandInfo = $commandInfo;
        $this->reflection = $reflection;
        $this->fqcnCache = $fqcnCache ?: new FullyQualifiedClassCache();
    }

    /**
     * Parse the docBlock comment for this command, and set the
     * fields of this class with the data thereby obtained.
     */
    public function parse()
    {
        $doc = $this->reflection->getDocComment();
        $this->parseDocBlock($doc);
    }

    /**
     * Save any tag that we do not explicitly recognize in the
     * 'otherAnnotations' map.
     */
    protected function processGenericTag($tag)
    {
        $this->commandInfo->addAnnotation($tag->getTag(), $tag->getContent());
    }

    /**
     * Set the name of the command from a @command or @name annotation.
     */
    protected function processCommandTag($tag)
    {
        if (!$tag->hasWordAndDescription($matches)) {
            throw new \Exception('Could not determine command name from tag ' . (string)$tag);
        }
        $commandName = $matches['word'];
        $this->commandInfo->setName($commandName);
        // We also store the name in the 'other annotations' so that is is
        // possible to determine if the method had a @command annotation.
        $this->commandInfo->addAnnotation($tag->getTag(), $commandName);
    }

    /**
     * The @description and @desc annotations may be used in
     * place of the synopsis (which we call 'description').
     * This is discouraged.
     *
     * @deprecated
     */
    protected function processAlternateDescriptionTag($tag)
    {
        $this->commandInfo->setDescription($tag->getContent());
    }

    /**
     * Store the data from a @param annotation in our argument descriptions.
     */
    protected function processParamTag($tag)
    {
        if ($tag->hasTypeVariableAndDescription($matches)) {
            if ($this->ignoredParamType($matches['type'])) {
                return;
            }
        }
        return $this->processArgumentTag($tag);
    }

    protected function ignoredParamType($paramType)
    {
        // TODO: We should really only allow a couple of types here,
        // e.g. 'string', 'array', 'bool'. Blacklist things we do not
        // want for now to avoid breaking commands with weird types.
        // Fix in the next major version.
        //
        // This works:
        //   return !in_array($paramType, ['string', 'array', 'integer', 'bool']);
        return preg_match('#(InputInterface|OutputInterface)$#', $paramType);
    }

    /**
     * Store the data from a @arg annotation in our argument descriptions.
     */
    protected function processArgumentTag($tag)
    {
        if (!$tag->hasVariable($matches)) {
            throw new \Exception('Could not determine argument name from tag ' . (string)$tag);
        }
        if ($matches['variable'] == $this->optionParamName()) {
            return;
        }
        $this->commandInfo->addArgumentDescription($matches['variable'], static::removeLineBreaks($matches['description']));
    }

    /**
     * Store the data from an @option annotation in our option descriptions.
     */
    protected function processOptionTag($tag)
    {
        if (!$tag->hasVariable($matches)) {
            throw new \Exception('Could not determine option name from tag ' . (string)$tag);
        }
        $this->commandInfo->addOptionDescription($matches['variable'], static::removeLineBreaks($matches['description']));
    }

    // @deprecated No longer called, only here for backwards compatibility (no clients should use "internal" classes anyway)
    protected function addOptionOrArgumentTag($tag, DefaultsWithDescriptions $set, $name, $description)
    {
        $variableName = $this->commandInfo->findMatchingOption($name);
        $description = static::removeLineBreaks($description);
        list($description, $defaultValue) = $this->splitOutDefault($description);
        $set->add($variableName, $description);
        if ($defaultValue !== null) {
            $set->setDefaultValue($variableName, $defaultValue);
        }
    }

    // @deprecated No longer called, only here for backwards compatibility (no clients should use "internal" classes anyway)
    protected function splitOutDefault($description)
    {
        if (!preg_match('#(.*)(Default: *)(.*)#', trim($description), $matches)) {
            return [$description, null];
        }

        return [trim($matches[1]), $this->interpretDefaultValue(trim($matches[3]))];
    }

    /**
     * Store the data from a @default annotation in our argument or option store,
     * as appropriate.
     */
    protected function processDefaultTag($tag)
    {
        if (!$tag->hasVariable($matches)) {
            throw new \Exception('Could not determine parameter name for default value from tag ' . (string)$tag);
        }
        $variableName = $matches['variable'];
        $defaultValue = DefaultValueFromString::fromString($matches['description'])->value();
        if ($this->commandInfo->arguments()->exists($variableName)) {
            $this->commandInfo->arguments()->setDefaultValue($variableName, $defaultValue);
            return;
        }
        $variableName = $this->commandInfo->findMatchingOption($variableName);
        if ($this->commandInfo->options()->exists($variableName)) {
            $this->commandInfo->options()->setDefaultValue($variableName, $defaultValue);
        }
    }

    /**
     * Store the data from a @usage annotation in our example usage list.
     */
    protected function processUsageTag($tag)
    {
        $lines = explode("\n", $tag->getContent());
        $usage = trim(array_shift($lines));
        $description = static::removeLineBreaks(implode("\n", array_map(function ($line) {
            return trim($line);
        }, $lines)));

        $this->commandInfo->setExampleUsage($usage, $description);
    }

    /**
     * Process the comma-separated list of aliases
     */
    protected function processAliases($tag)
    {
        $this->commandInfo->setAliases((string)$tag->getContent());
    }

    /**
     * Store the data from a @return annotation in our argument descriptions.
     */
    protected function processReturnTag($tag)
    {
        // The return type might be a variable -- '$this'. It will
        // usually be a type, like RowsOfFields, or \Namespace\RowsOfFields.
        if (!$tag->hasVariableAndDescription($matches)) {
            throw new \Exception('Could not determine return type from tag ' . (string)$tag);
        }
        // Look at namespace and `use` statments to make returnType a fqdn
        $returnType = $matches['variable'];
        $returnType = $this->findFullyQualifiedClass($returnType);
        $this->commandInfo->setReturnType($returnType);
    }

    protected function findFullyQualifiedClass($className)
    {
        if (strpos($className, '\\') !== false) {
            return $className;
        }

        return $this->fqcnCache->qualify($this->reflection->getFileName(), $className);
    }


    private function parseDocBlock($doc)
    {
        // Remove the leading /** and the trailing */
        $doc = DocBlockUtils::stripLeadingCommentCharacters($doc);

        // Nothing left? Exit.
        if (empty($doc)) {
            return;
        }

        $tagFactory = new TagFactory();
        $lines = [];

        foreach (explode("\n", $doc) as $row) {
            // Remove trailing whitespace and leading space + '*'s
            $row = rtrim($row);

            if (!$tagFactory->parseLine($row)) {
                $lines[] = $row;
            }
        }

        $this->processDescriptionAndHelp($lines);
        $this->processAllTags($tagFactory->getTags());
    }

    protected function processDescriptionAndHelp($lines)
    {
        // Trim all of the lines individually.
        $lines =
            array_map(
                function ($line) {
                    return trim($line);
                },
                $lines
            );

        // Everything up to the first blank line goes in the description.
        $description = array_shift($lines);
        while (static::nextLineIsNotEmpty($lines)) {
            $description .= ' ' . array_shift($lines);
        }

        // Everything else goes in the help.
        $help = trim(implode("\n", $lines));

        $this->commandInfo->setDescription($description);
        $this->commandInfo->setHelp($help);
    }

    protected function nextLineIsNotEmpty($lines)
    {
        return DocBlockUtils::nextLineIsNotEmpty($lines);
    }

    protected function processAllTags($tags)
    {
        // Iterate over all of the tags, and process them as necessary.
        foreach ($tags as $tag) {
            $processFn = [$this, 'processGenericTag'];
            if (array_key_exists($tag->getTag(), $this->tagProcessors)) {
                $processFn = [$this, $this->tagProcessors[$tag->getTag()]];
            }
            $processFn($tag);
        }
    }

    protected function lastParameterName()
    {
        $params = $this->commandInfo->getParameters();
        $param = end($params);
        if (!$param) {
            return '';
        }
        return $param->name;
    }

    /**
     * Return the name of the last parameter if it holds the options.
     */
    public function optionParamName()
    {
        // Remember the name of the last parameter, if it holds the options.
        // We will use this information to ignore @param annotations for the options.
        if (!isset($this->optionParamName)) {
            $this->optionParamName = '';
            $options = $this->commandInfo->options();
            if (!$options->isEmpty()) {
                $this->optionParamName = $this->lastParameterName();
            }
        }

        return $this->optionParamName;
    }

    // @deprecated No longer called, only here for backwards compatibility (no clients should use "internal" classes anyway)
    protected function interpretDefaultValue($defaultValue)
    {
        $defaults = [
            'null' => null,
            'true' => true,
            'false' => false,
            "''" => '',
            '[]' => [],
        ];
        foreach ($defaults as $defaultName => $defaultTypedValue) {
            if ($defaultValue == $defaultName) {
                return $defaultTypedValue;
            }
        }
        return $defaultValue;
    }

    /**
     * Given a list that might be 'a b c' or 'a, b, c' or 'a,b,c',
     * convert the data into the last of these forms.
     */
    protected static function convertListToCommaSeparated($text)
    {
        return preg_replace('#[ \t\n\r,]+#', ',', $text);
    }

    /**
     * Take a multiline description and convert it into a single
     * long unbroken line.
     */
    protected static function removeLineBreaks($text)
    {
        return trim(preg_replace('#[ \t\n\r]+#', ' ', $text));
    }
}
