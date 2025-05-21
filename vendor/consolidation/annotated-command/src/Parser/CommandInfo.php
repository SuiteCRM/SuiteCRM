<?php
namespace Consolidation\AnnotatedCommand\Parser;

use Symfony\Component\Console\Input\InputOption;
use Consolidation\AnnotatedCommand\Parser\Internal\CommandDocBlockParser;
use Consolidation\AnnotatedCommand\Parser\Internal\CommandDocBlockParserFactory;
use Consolidation\AnnotatedCommand\Parser\Internal\DefaultValueFromString;
use Consolidation\AnnotatedCommand\AnnotationData;

/**
 * Given a class and method name, parse the annotations in the
 * DocBlock comment, and provide accessor methods for all of
 * the elements that are needed to create a Symfony Console Command.
 *
 * Note that the name of this class is now somewhat of a misnomer,
 * as we now use it to hold annotation data for hooks as well as commands.
 * It would probably be better to rename this to MethodInfo at some point.
 */
class CommandInfo
{
    /**
     * Serialization schema version. Incremented every time the serialization schema changes.
     */
    const SERIALIZATION_SCHEMA_VERSION = 4;

    /**
     * @var \ReflectionMethod
     */
    protected $reflection;

    /**
     * @var boolean
     * @var string
     */
    protected $docBlockIsParsed = false;

    /**
     * @var boolean
     * @var string
     */
    protected $parsingInProgress = false;

    /**
     * @var string
     */
    protected $name;

    /**
     * @var string
     */
    protected $description = '';

    /**
     * @var string
     */
    protected $help = '';

    /**
     * @var DefaultsWithDescriptions
     */
    protected $options;

    /**
     * @var DefaultsWithDescriptions
     */
    protected $arguments;

    /**
     * @var array
     */
    protected $exampleUsage = [];

    /**
     * @var AnnotationData
     */
    protected $otherAnnotations;

    /**
     * @var array
     */
    protected $aliases = [];

    /**
     * @var InputOption[]
     */
    protected $inputOptions;

    /**
     * @var string
     */
    protected $methodName;

    /**
     * @var string
     */
    protected $returnType;

    /**
     * @var string[]
     */
    protected $injectedClasses = [];

    /**
     * @var bool[]
     */
    protected $parameterMap = [];

    /**
     * @var bool
     */
    protected $simpleOptionParametersAllowed = false;

    /**
     * Create a new CommandInfo class for a particular method of a class.
     *
     * @param string|mixed $classNameOrInstance The name of a class, or an
     *   instance of it, or an array of cached data.
     * @param string $methodName The name of the method to get info about.
     * @param array $cache Cached data
     * @deprecated Use CommandInfo::create() or CommandInfo::deserialize()
     *   instead. In the future, this constructor will be protected.
     */
    public function __construct($classNameOrInstance, $methodName, $cache = [])
    {
        $this->reflection = new \ReflectionMethod($classNameOrInstance, $methodName);
        $this->methodName = $methodName;
        $this->arguments = new DefaultsWithDescriptions();
        $this->options = new DefaultsWithDescriptions();

        // If the cache came from a newer version, ignore it and
        // regenerate the cached information.
        if (!empty($cache) && CommandInfoDeserializer::isValidSerializedData($cache) && !$this->cachedFileIsModified($cache)) {
            $deserializer = new CommandInfoDeserializer();
            $deserializer->constructFromCache($this, $cache);
            $this->docBlockIsParsed = true;
        } else {
            $this->constructFromClassAndMethod($classNameOrInstance, $methodName);
        }
    }

    public static function create($classNameOrInstance, $methodName)
    {
        return new self($classNameOrInstance, $methodName);
    }

    public static function deserialize($cache)
    {
        $cache = (array)$cache;
        return new self($cache['class'], $cache['method_name'], $cache);
    }

    public function cachedFileIsModified($cache)
    {
        $path = $this->reflection->getFileName();
        return filemtime($path) != $cache['mtime'];
    }

    protected function constructFromClassAndMethod($classNameOrInstance, $methodName)
    {
        $this->otherAnnotations = new AnnotationData();
        // Set up a default name for the command from the method name.
        // This can be overridden via @command or @name annotations.
        $this->name = $this->convertName($methodName);

        // To start with, $this->options will contain the values from the final
        // `$options = ['name' => 'default'], and arguments will be everything else.
        // When we process the annotations / attributes, if we find an "option" which
        // appears in the 'arguments' section, then we will move it.
        $optionsFromParameters = $this->determineOptionsFromParameters();
        $this->simpleOptionParametersAllowed = empty($optionsFromParameters);
        $this->options = new DefaultsWithDescriptions($optionsFromParameters, false);
        $this->arguments = $this->determineAgumentClassifications();

        // Construct the object from docblock annotations or php attributes
        $this->parseDocBlock();
    }

    /**
     * Recover the method name provided to the constructor.
     *
     * @return string
     */
    public function getMethodName()
    {
        return $this->methodName;
    }

    /**
     * Return the primary name for this command.
     *
     * @return string
     */
    public function getName()
    {
        // getName() is the only attribute that may be used during parsing.
        return $this->name;
    }

    /**
     * Set the primary name for this command.
     *
     * @param string $name
     */
    public function setName($name)
    {
        $this->name = $name;
        return $this;
    }

    /**
     * Return whether or not this method represents a valid command
     * or hook.
     */
    public function valid()
    {
        return !empty($this->name);
    }

    /**
     * If higher-level code decides that this CommandInfo is not interesting
     * or useful (if it is not a command method or a hook method), then
     * we will mark it as invalid to prevent it from being created as a command.
     * We still cache a placeholder record for invalid methods, so that we
     * do not need to re-parse the method again later simply to determine that
     * it is invalid.
     */
    public function invalidate()
    {
        $this->name = '';
    }

    public function getParameterMap()
    {
        return $this->parameterMap;
    }

    public function getReturnType()
    {
        $this->requireConsistentState();
        return $this->returnType;
    }

    public function getInjectedClasses()
    {
        $this->requireConsistentState();
        return $this->injectedClasses;
    }

    public function setInjectedClasses($injectedClasses)
    {
        $this->injectedClasses = $injectedClasses;
        return $this;
    }

    public function setReturnType($returnType)
    {
        $this->returnType = $returnType;
        return $this;
    }

    /**
     * Get any annotations included in the docblock comment for the
     * implementation method of this command that are not already
     * handled by the primary methods of this class.
     *
     * @return AnnotationData
     */
    public function getRawAnnotations()
    {
        $this->requireConsistentState();
        return $this->otherAnnotations;
    }

    /**
     * Replace the annotation data.
     */
    public function replaceRawAnnotations($annotationData)
    {
        $this->otherAnnotations = new AnnotationData((array) $annotationData);
        return $this;
    }

    /**
     * Get any annotations included in the docblock comment,
     * also including default values such as @command.  We add
     * in the default @command annotation late, and only in a
     * copy of the annotation data because we use the existance
     * of a @command to indicate that this CommandInfo is
     * a command, and not a hook or anything else.
     *
     * @return AnnotationData
     */
    public function getAnnotations()
    {
        // Also provide the path to the commandfile that these annotations
        // were pulled from and the classname of that file.
        $path = $this->reflection->getFileName();
        $className = $this->reflection->getDeclaringClass()->getName();
        return new AnnotationData(
            $this->getRawAnnotations()->getArrayCopy() +
            [
                'command' => $this->getName(),
                '_path' => $path,
                '_classname' => $className,
            ]
        );
    }

    /**
     * Return a specific named annotation for this command as a list.
     *
     * @param string $name The name of the annotation.
     * @return array|null
     */
    public function getAnnotationList($name)
    {
        // hasAnnotation parses the docblock
        if (!$this->hasAnnotation($name)) {
            return null;
        }
        return $this->otherAnnotations->getList($name);
        ;
    }

    /**
     * Return a specific named annotation for this command as a string.
     *
     * @param string $name The name of the annotation.
     * @return string|null
     */
    public function getAnnotation($name)
    {
        // hasAnnotation parses the docblock
        if (!$this->hasAnnotation($name)) {
            return null;
        }
        return $this->otherAnnotations->get($name);
    }

    /**
     * Check to see if the specified annotation exists for this command.
     *
     * @param string $annotation The name of the annotation.
     * @return boolean
     */
    public function hasAnnotation($annotation)
    {
        $this->requireConsistentState();
        return isset($this->otherAnnotations[$annotation]);
    }

    /**
     * Save any tag that we do not explicitly recognize in the
     * 'otherAnnotations' map.
     */
    public function addAnnotation($name, $content)
    {
        // Convert to an array and merge if there are multiple
        // instances of the same annotation defined.
        if (isset($this->otherAnnotations[$name])) {
            $content = array_merge((array) $this->otherAnnotations[$name], (array)$content);
        }
        $this->otherAnnotations[$name] = $content;
    }

    /**
     * Remove an annotation that was previoudly set.
     */
    public function removeAnnotation($name)
    {
        unset($this->otherAnnotations[$name]);
    }

    /**
     * Get the synopsis of the command (~first line).
     *
     * @return string
     */
    public function getDescription()
    {
        $this->requireConsistentState();
        return $this->description;
    }

    /**
     * Set the command description.
     *
     * @param string $description The description to set.
     */
    public function setDescription($description)
    {
        $this->description = str_replace("\n", ' ', $description ?? '');
        return $this;
    }

    /**
     * Determine if help was provided for this command info
     */
    public function hasHelp()
    {
        return !empty($this->help) || !empty($this->description);
    }

    /**
     * Get the help text of the command (the description)
     */
    public function getHelp()
    {
        $this->requireConsistentState();
        return $this->help;
    }
    /**
     * Set the help text for this command.
     *
     * @param string $help The help text.
     */
    public function setHelp($help)
    {
        $this->help = $help;
        return $this;
    }

    /**
     * Return the list of aliases for this command.
     * @return string[]
     */
    public function getAliases()
    {
        $this->requireConsistentState();
        return $this->aliases;
    }

    /**
     * Set aliases that can be used in place of the command's primary name.
     *
     * @param string|string[] $aliases
     */
    public function setAliases($aliases)
    {
        if (is_string($aliases)) {
            $aliases = explode(',', static::convertListToCommaSeparated($aliases));
        }
        $this->aliases = array_filter($aliases);
        return $this;
    }

    /**
     * Get hidden status for the command.
     * @return bool
     */
    public function getHidden()
    {
        $this->requireConsistentState();
        return $this->hasAnnotation('hidden');
    }

    /**
     * Set hidden status. List command omits hidden commands.
     *
     * @param bool $hidden
     */
    public function setHidden($hidden)
    {
        $this->AddAnnotation('hidden', $hidden);
        return $this;
    }

    /**
     * Return the examples for this command. This is @usage instead of
     * @example because the later is defined by the phpdoc standard to
     * be example method calls.
     *
     * @return string[]
     */
    public function getExampleUsages()
    {
        $this->requireConsistentState();
        return $this->exampleUsage;
    }

    /**
     * Add an example usage for this command.
     *
     * @param string $usage An example of the command, including the command
     *   name and all of its example arguments and options.
     * @param string $description An explanation of what the example does.
     */
    public function setExampleUsage($usage, $description)
    {
        $this->exampleUsage[$usage] = $description;
        return $this;
    }

    /**
     * Overwrite all example usages
     */
    public function replaceExampleUsages($usages)
    {
        $this->exampleUsage = $usages;
        return $this;
    }

    /**
     * Return the topics for this command.
     *
     * @return string[]
     */
    public function getTopics()
    {
        if (!$this->hasAnnotation('topics')) {
            return [];
        }
        $topics = $this->getAnnotation('topics');
        return explode(',', trim($topics));
    }

    /**
     * Return the list of refleaction parameters.
     *
     * @return ReflectionParameter[]
     */
    public function getParameters()
    {
        return $this->reflection->getParameters();
    }

    /**
     * Descriptions of commandline arguements for this command.
     *
     * @return DefaultsWithDescriptions
     */
    public function arguments()
    {
        return $this->arguments;
    }

    /**
     * Descriptions of commandline options for this command.
     *
     * @return DefaultsWithDescriptions
     */
    public function options()
    {
        return $this->options;
    }

    /**
     * Get the inputOptions for the options associated with this CommandInfo
     * object, e.g. via @option annotations, or from
     * $options = ['someoption' => 'defaultvalue'] in the command method
     * parameter list.
     *
     * @return InputOption[]
     */
    public function inputOptions()
    {
        if (!isset($this->inputOptions)) {
            $this->inputOptions = $this->createInputOptions();
        }
        return $this->inputOptions;
    }

    protected function addImplicitNoOptions()
    {
        $opts = $this->options()->getValues();
        foreach ($opts as $name => $defaultValue) {
            if ($defaultValue === true) {
                $key = 'no-' . $name;
                if (!array_key_exists($key, $opts)) {
                    $description = "Negate --$name option.";
                    $this->options()->add($key, $description, false);
                }
            }
        }
    }

    protected function createInputOptions()
    {
        $explicitOptions = [];
        $this->addImplicitNoOptions();

        $opts = $this->options()->getValues();
        foreach ($opts as $name => $defaultValue) {
            $description = $this->options()->getDescription($name);
            $suggestedValues = $this->options()->getSuggestedValues($name);

            $fullName = $name;
            $shortcut = '';
            if (strpos($name, '|')) {
                list($fullName, $shortcut) = explode('|', $name, 2);
            }

            // Treat the following two cases identically:
            //   - 'foo' => InputOption::VALUE_OPTIONAL
            //   - 'foo' => null
            // The first form is preferred, but we will convert the value
            // to 'null' for storage as the option default value.
            if ($defaultValue === InputOption::VALUE_OPTIONAL) {
                $defaultValue = null;
            }

            if ($defaultValue === false) {
                $explicitOptions[$fullName] = new InputOption($fullName, $shortcut, InputOption::VALUE_NONE, $description);
            } elseif ($defaultValue === InputOption::VALUE_REQUIRED) {
                $explicitOptions[$fullName] = new InputOption($fullName, $shortcut, InputOption::VALUE_REQUIRED, $description, null, $suggestedValues);
            } elseif (is_array($defaultValue)) {
                $optionality = count($defaultValue) ? InputOption::VALUE_OPTIONAL : InputOption::VALUE_REQUIRED;
                $explicitOptions[$fullName] = new InputOption(
                    $fullName,
                    $shortcut,
                    InputOption::VALUE_IS_ARRAY | $optionality,
                    $description,
                    count($defaultValue) ? $defaultValue : null,
                    $suggestedValues
                );
            } else {
                $explicitOptions[$fullName] = new InputOption($fullName, $shortcut, InputOption::VALUE_OPTIONAL, $description, $defaultValue, $suggestedValues);
            }
        }

        return $explicitOptions;
    }

    /**
     * An option might have a name such as 'silent|s'. In this
     * instance, we will allow the @option or @default tag to
     * reference the option only by name (e.g. 'silent' or 's'
     * instead of 'silent|s').
     *
     * @param string $optionName
     * @return string
     */
    public function findMatchingOption($optionName)
    {
        // Exit fast if there's an exact match
        if ($this->options->exists($optionName)) {
            return $optionName;
        }
        $existingOptionName = $this->findExistingOption($optionName);
        if (isset($existingOptionName)) {
            return $existingOptionName;
        }
        return $this->findOptionAmongAlternatives($optionName);
    }

    public function addArgumentDescription($name, $description, $suggestedValues = [])
    {
        $this->addOptionOrArgumentDescription($this->arguments(), $name, $description, $suggestedValues);
    }

    public function addOption($name, $description, $suggestedValues = [], $defaultValue = null)
    {
        $this->addOptionOrArgumentDescription($this->options(), $name, $description, $suggestedValues, $defaultValue);
    }

    /**
     * @deprecated Use addOption() instead.
     */
    public function addOptionDescription($name, $description, $suggestedValues = [], $defaultValue = null)
    {
        $variableName = $this->findMatchingOption($name);
        $defaultFromParameter = null;
        $parameterName = $this->arguments()->approximatelyMatchingKey($variableName);
        if ($this->simpleOptionParametersAllowed && $parameterName) {
            $defaultFromParameter = $this->arguments()->removeMatching($variableName);
            // One of our parameters is an option, not an argument. Flag it so that we can inject the right value when needed.
            $this->parameterMap[$parameterName] = $variableName;
        }
        $this->addOptionOrArgumentDescription($this->options(), $variableName, $description, $suggestedValues, $defaultValue ?? $defaultFromParameter);
    }

    protected function addOptionOrArgumentDescription(DefaultsWithDescriptions $set, $variableName, $description, $suggestedValues = [], $defaultFromParameter = null)
    {
        list($description, $defaultValue) = $this->splitOutDefault($description);
        if (empty($defaultValue) && !empty($defaultFromParameter)) {
            $defaultValue = $defaultFromParameter;
        }
        // "Avoid cannot set a default value except for InputArgument::OPTIONAL mode." error.
        $set->add($variableName, $description, $defaultValue === [] ? null : $defaultValue, $suggestedValues);
        // Now set the defaultValue if we fudged it above. This is more permissive.
        // Note that there is no setSuggestions() method so it has to be set above.
        if ($defaultValue === []) {
            $set->setDefaultValue($variableName, $defaultValue);
        }
    }

    protected function splitOutDefault($description)
    {
        if (!preg_match('#(.*)(Default: *)(.*)#', trim($description), $matches)) {
            return [$description, null];
        }

        return [trim($matches[1]), DefaultValueFromString::fromString(trim($matches[3]))->value()];
    }

    /**
     * @param string $optionName
     * @return string
     */
    protected function findOptionAmongAlternatives($optionName)
    {
        // Check the other direction: if the annotation contains @silent|s
        // and the options array has 'silent|s'.
        $checkMatching = explode('|', $optionName);
        if (count($checkMatching) > 1) {
            foreach ($checkMatching as $checkName) {
                if ($this->options->exists($checkName)) {
                    $this->options->rename($checkName, $optionName);
                    return $optionName;
                }
            }
        }
        return $optionName;
    }

    /**
     * @param string $optionName
     * @return string|null
     */
    protected function findExistingOption($optionName)
    {
        // Check to see if we can find the option name in an existing option,
        // e.g. if the options array has 'silent|s' => false, and the annotation
        // is @silent.
        foreach ($this->options()->getValues() as $name => $default) {
            if (in_array($optionName, explode('|', $name))) {
                return $name;
            }
            if ($optionName == $this->convertArgumentName($name)) {
                return $name;
            }
        }
    }

    /**
     * Examine the parameters of the method for this command, and
     * build a list of commandline arguments for them.
     *
     * @return array
     */
    protected function determineAgumentClassifications()
    {
        $result = new DefaultsWithDescriptions();
        $params = $this->reflection->getParameters();
        $optionsFromParameters = $this->determineOptionsFromParameters();
        if ($this->lastParameterIsOptionsArray()) {
            array_pop($params);
        }
        while (!empty($params) && ($params[0]->getType() != null) && ($params[0]->getType() instanceof \ReflectionNamedType) && !($params[0]->getType()->isBuiltin())) {
            $param = array_shift($params);
            $injectedClass = $param->getType()->getName();
            array_unshift($this->injectedClasses, $injectedClass);
        }
        foreach ($params as $param) {
            $this->parameterMap[$param->name] = false;
            $this->addParameterToResult($result, $param);
        }
        return $result;
    }

    /**
     * Examine the provided parameter, and determine whether it
     * is a parameter that will be filled in with a positional
     * commandline argument.
     */
    protected function addParameterToResult($result, $param)
    {
        // Commandline arguments must be strings, so ignore any
        // parameter that is typehinted to any non-primitive class.
        if ($param->getType() && (!$param->getType() instanceof \ReflectionNamedType || !$param->getType()->isBuiltin())) {
            return;
        }
        $result->add($param->name);
        if ($param->isDefaultValueAvailable()) {
            $defaultValue = $param->getDefaultValue();
            if (!$this->isAssoc($defaultValue)) {
                $result->setDefaultValue($param->name, $defaultValue);
            }
        } elseif ($param->getType() && $param->getType()->getName() === 'array') {
            $result->setDefaultValue($param->name, []);
        }
    }

    /**
     * Examine the parameters of the method for this command, and determine
     * the disposition of the options from them.
     *
     * @return array
     */
    protected function determineOptionsFromParameters()
    {
        $params = $this->reflection->getParameters();
        if (empty($params)) {
            return [];
        }
        $param = end($params);
        if (!$param->isDefaultValueAvailable()) {
            return [];
        }
        if (!$this->isAssoc($param->getDefaultValue())) {
            return [];
        }
        return $param->getDefaultValue();
    }

    /**
     * Determine if the last argument contains $options.
     *
     * Two forms indicate options:
     * - $options = []
     * - $options = ['flag' => 'default-value']
     *
     * Any other form, including `array $foo`, is not options.
     */
    protected function lastParameterIsOptionsArray()
    {
        $params = $this->reflection->getParameters();
        if (empty($params)) {
            return [];
        }
        $param = end($params);
        if (!$param->isDefaultValueAvailable()) {
            return [];
        }
        return is_array($param->getDefaultValue());
    }

    /**
     * Helper; determine if an array is associative or not. An array
     * is not associative if its keys are numeric, and numbered sequentially
     * from zero. All other arrays are considered to be associative.
     *
     * @param array $arr The array
     * @return boolean
     */
    protected function isAssoc($arr)
    {
        if (!is_array($arr)) {
            return false;
        }
        return array_keys($arr) !== range(0, count($arr) - 1);
    }

    /**
     * Convert from a method name to the corresponding command name. A
     * method 'fooBar' will become 'foo:bar', and 'fooBarBazBoz' will
     * become 'foo:bar-baz-boz'.
     *
     * @param string $camel method name.
     * @return string
     */
    protected function convertName($camel)
    {
        $snake = $this->camelToSnake($camel, '-');
        return preg_replace("/-/", ':', $snake, 1);
    }

    /**
     * Convert an argument name from snake_case or camelCase
     * to a hyphenated-string.
     */
    protected function convertArgumentName($camel)
    {
        $snake = $this->camelToSnake($camel, '-');
        return strtr($snake, '_', '-');
    }

    /**
     * Convert a camelCase string to a snake_case string.
     */
    protected function camelToSnake($camel, $splitter = '_')
    {
        $camel=preg_replace('/(?!^)[[:upper:]][[:lower:]]/', '$0', preg_replace('/(?!^)[[:upper:]]+/', $splitter.'$0', $camel));
        return strtolower($camel);
    }

    /**
     * Guard against invalid usage of CommandInfo during parsing.
     */
    protected function requireConsistentState()
    {
        if ($this->parsingInProgress == true) {
            throw new \Exception("Cannot use CommandInfo object while it is in an inconsistant state!");
        }
    }

    /**
     * Parse the docBlock comment for this command, and set the
     * fields of this class with the data thereby obtained.
     */
    protected function parseDocBlock()
    {
        if (!$this->docBlockIsParsed) {
            $this->docBlockIsParsed = true;
            $this->parsingInProgress = true;
            // The parse function will insert data from the provided method
            // into this object, using our accessors.
            CommandDocBlockParserFactory::parse($this, $this->reflection);
            $this->parsingInProgress = false;
            // Use method's return type if @return is not present.
            if ($this->reflection->hasReturnType() && !$this->getReturnType()) {
                $type = $this->reflection->getReturnType();
                if ($type instanceof \ReflectionUnionType) {
                    // Use first declared type.
                    $type = current($type->getTypes());
                }
                $this->setReturnType($type->getName());
            }
        }
    }

    /**
     * Given a list that might be 'a b c' or 'a, b, c' or 'a,b,c',
     * convert the data into the last of these forms.
     */
    protected static function convertListToCommaSeparated($text)
    {
        return preg_replace('#[ \t\n\r,]+#', ',', $text);
    }
}
