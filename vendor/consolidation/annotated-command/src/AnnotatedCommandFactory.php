<?php
namespace Consolidation\AnnotatedCommand;

use Consolidation\AnnotatedCommand\Cache\CacheWrapper;
use Consolidation\AnnotatedCommand\Cache\NullCache;
use Consolidation\AnnotatedCommand\Cache\SimpleCacheInterface;
use Consolidation\AnnotatedCommand\Hooks\HookManager;
use Consolidation\AnnotatedCommand\Options\AutomaticOptionsProviderInterface;
use Consolidation\AnnotatedCommand\Parser\CommandInfo;
use Consolidation\AnnotatedCommand\Parser\CommandInfoDeserializer;
use Consolidation\AnnotatedCommand\Parser\CommandInfoSerializer;
use Consolidation\OutputFormatters\Options\FormatterOptions;
use Symfony\Component\Console\Command\Command;
use Symfony\Component\Console\Input\InputInterface;
use Symfony\Component\Console\Output\OutputInterface;

/**
 * The AnnotatedCommandFactory creates commands for your application.
 * Use with a Dependency Injection Container and the CommandFactory.
 * Alternately, use the CommandFileDiscovery to find commandfiles, and
 * then use AnnotatedCommandFactory::createCommandsFromClass() to create
 * commands.  See the README for more information.
 *
 * @package Consolidation\AnnotatedCommand
 */
class AnnotatedCommandFactory implements AutomaticOptionsProviderInterface
{
    /** var bool */
    protected static $ignoreCommandsInTraits = false;

    /** var bool */
    protected static $ignoreCommandsInParentClasses = false;

    /** var CommandProcessor */
    protected $commandProcessor;

    /** var CommandCreationListenerInterface[] */
    protected $listeners = [];

    /** var AutomaticOptionsProvider[] */
    protected $automaticOptionsProviderList = [];

    /** var boolean */
    protected $includeAllPublicMethods = true;

    /** var CommandInfoAltererInterface */
    protected $commandInfoAlterers = [];

    /** var SimpleCacheInterface */
    protected $dataStore;

    /** var string[] */
    protected $ignoredCommandsRegexps = [];

    /**
     * Typically, traits should not contain commands; however, some
     * applications make use of this feature to package commands in
     * libraries, so we must allow command files in traits to maintain
     * backwards compatibility. Call this method to skip the parsing
     * of trait files for a performance boost.
     *
     * In future versions, this property be removed, and commands will
     * not be parsed from traits. Use Robo plugins as the preferred method
     * of distributing shared commands.
     */
    public static function setIgnoreCommandsInTraits(bool $skipTraitFiles)
    {
        static::$ignoreCommandsInTraits = $skipTraitFiles;
    }

    /**
     * Typically, commands should not be inherited from parent classes;
     * however, some applications make use of this feature to package
     * commands in libraries, so we must allow command files in traits
     * to maintain backwards compatibility. Call this method to skip the
     * parsing of trait files for a performance boost.
     *
     * In future versions, this property be removed, and commands will
     * not be parsed from traits. Use Robo plugins as the preferred method
     * of distributing shared commands.
     */
    public static function setIgnoreCommandsInParentClasses(bool $ignoreCommandsInParentClasses)
    {
        static::$ignoreCommandsInParentClasses = $ignoreCommandsInParentClasses;
    }

    public function __construct()
    {
        $this->dataStore = new NullCache();
        $this->commandProcessor = new CommandProcessor(new HookManager());
        $this->addAutomaticOptionProvider($this);
    }

    public function setCommandProcessor(CommandProcessor $commandProcessor)
    {
        $this->commandProcessor = $commandProcessor;
        return $this;
    }

    /**
     * @return CommandProcessor
     */
    public function commandProcessor()
    {
        return $this->commandProcessor;
    }

    /**
     * Set the 'include all public methods flag'. If true (the default), then
     * every public method of each commandFile will be used to create commands.
     * If it is false, then only those public methods annotated with @command
     * or @name (deprecated) will be used to create commands.
     */
    public function setIncludeAllPublicMethods($includeAllPublicMethods)
    {
        $this->includeAllPublicMethods = $includeAllPublicMethods;
        return $this;
    }

    public function getIncludeAllPublicMethods()
    {
        return $this->includeAllPublicMethods;
    }

    /**
     * @return HookManager
     */
    public function hookManager()
    {
        return $this->commandProcessor()->hookManager();
    }

    /**
     * Add a listener that is notified immediately before the command
     * factory creates commands from a commandFile instance.  This
     * listener can use this opportunity to do more setup for the commandFile,
     * and so on.
     *
     * @param CommandCreationListenerInterface $listener
     */
    public function addListener(CommandCreationListenerInterface $listener)
    {
        $this->listeners[] = $listener;
        return $this;
    }

    /**
     * Add a listener that's just a simple 'callable'.
     * @param callable $listener
     */
    public function addListernerCallback(callable $listener)
    {
        $this->addListener(new CommandCreationListener($listener));
        return $this;
    }

    /**
     * Add a regular expresion used to match methods names
     * that will not be part of the final set of commands.
     *
     * @param string $regex
     */
    public function addIgnoredCommandsRegexp(string $regex)
    {
        $this->ignoredCommandsRegexps[] = $regex;
        return $this;
    }

    /**
     * Call all command creation listeners
     *
     * @param object $commandFileInstance
     */
    protected function notify($commandFileInstance)
    {
        foreach ($this->listeners as $listener) {
            $listener->notifyCommandFileAdded($commandFileInstance);
        }
    }

    public function addAutomaticOptionProvider(AutomaticOptionsProviderInterface $optionsProvider)
    {
        $this->automaticOptionsProviderList[] = $optionsProvider;
    }

    public function addCommandInfoAlterer(CommandInfoAltererInterface $alterer)
    {
        $this->commandInfoAlterers[] = $alterer;
    }

    /**
     * n.b. This registers all hooks from the commandfile instance as a side-effect.
     */
    public function createCommandsFromClass($commandFileInstance, $includeAllPublicMethods = null)
    {
        // Deprecated: avoid using the $includeAllPublicMethods in favor of the setIncludeAllPublicMethods() accessor.
        if (!isset($includeAllPublicMethods)) {
            $includeAllPublicMethods = $this->getIncludeAllPublicMethods();
        }
        $this->notify($commandFileInstance);
        $commandInfoList = $this->getCommandInfoListFromClass($commandFileInstance);
        $this->registerCommandHooksFromClassInfo($commandInfoList, $commandFileInstance);
        return $this->createCommandsFromClassInfo($commandInfoList, $commandFileInstance, $includeAllPublicMethods);
    }

    public function getCommandInfoListFromClass($commandFileInstance)
    {
        $cachedCommandInfoList = $this->getCommandInfoListFromCache($commandFileInstance);
        $commandInfoList = $this->createCommandInfoListFromClass($commandFileInstance, $cachedCommandInfoList);
        if (!empty($commandInfoList)) {
            $cachedCommandInfoList = array_merge($commandInfoList, $cachedCommandInfoList);
            $this->storeCommandInfoListInCache($commandFileInstance, $cachedCommandInfoList);
        }
        return $cachedCommandInfoList;
    }

    protected function storeCommandInfoListInCache($commandFileInstance, $commandInfoList)
    {
        if (!$this->hasDataStore()) {
            return;
        }
        $cache_data = [];
        $serializer = new CommandInfoSerializer();
        foreach ($commandInfoList as $i => $commandInfo) {
            $cache_data[$i] = $serializer->serialize($commandInfo);
        }
        $className = get_class($commandFileInstance);
        $this->getDataStore()->set($className, $cache_data);
    }

    /**
     * Get the command info list from the cache
     *
     * @param mixed $commandFileInstance
     * @return array
     */
    protected function getCommandInfoListFromCache($commandFileInstance)
    {
        $commandInfoList = [];
        if (!is_object($commandFileInstance)) {
            return [];
        }
        $className = get_class($commandFileInstance);
        if (!$this->getDataStore()->has($className)) {
            return [];
        }
        $deserializer = new CommandInfoDeserializer();

        $cache_data = $this->getDataStore()->get($className);
        foreach ($cache_data as $i => $data) {
            if (CommandInfoDeserializer::isValidSerializedData((array)$data)) {
                $commandInfoList[$i] = $deserializer->deserialize((array)$data);
            }
        }
        return $commandInfoList;
    }

    /**
     * Check to see if this factory has a cache datastore.
     * @return boolean
     */
    public function hasDataStore()
    {
        return !($this->dataStore instanceof NullCache);
    }

    /**
     * Set a cache datastore for this factory. Any object with 'set' and
     * 'get' methods is acceptable. The key is the classname being cached,
     * and the value is a nested associative array of strings.
     *
     * TODO: Typehint this to SimpleCacheInterface
     *
     * This is not done currently to allow clients to use a generic cache
     * store that does not itself depend on the annotated-command library.
     *
     * @param Mixed $dataStore
     * @return type
     */
    public function setDataStore($dataStore)
    {
        if (!($dataStore instanceof SimpleCacheInterface)) {
            $dataStore = new CacheWrapper($dataStore);
        }
        $this->dataStore = $dataStore;
        return $this;
    }

    /**
     * Get the data store attached to this factory.
     */
    public function getDataStore()
    {
        return $this->dataStore;
    }

    protected function createCommandInfoListFromClass($commandFileInstance, $cachedCommandInfoList)
    {
        $commandInfoList = [];

        // Ignore special functions, such as __construct and __call, which
        // can never be commands.
        $commandClass = get_class($commandFileInstance);
        $commandMethodNames = array_filter(
            get_class_methods($commandFileInstance) ?: [],
            function ($m) use ($commandFileInstance, $commandClass) {
                $reflectionMethod = new \ReflectionMethod($commandFileInstance, $m);
                $name = $reflectionMethod->getFileName();
                if (static::$ignoreCommandsInParentClasses && $reflectionMethod->getDeclaringClass()->getName() !== $commandClass) {
                    return false;
                }
                if ($reflectionMethod->isStatic() || preg_match('#^_#', $m)) {
                    return false;
                }
                if (!static::$ignoreCommandsInTraits) {
                    return true;
                }
                return basename($name) !== 'IO.php' && strpos($name, 'Trait') === false;
            }
        );

        foreach ($commandMethodNames as $commandMethodName) {
            if (!array_key_exists($commandMethodName, $cachedCommandInfoList)) {
                $commandInfo = CommandInfo::create($commandFileInstance, $commandMethodName);
                $this->alterCommandInfo($commandInfo, $commandFileInstance);
                if (!static::isCommandOrHookMethod($commandInfo, $this->getIncludeAllPublicMethods())) {
                    $commandInfo->invalidate();
                }
                $commandInfoList[$commandMethodName] =  $commandInfo;
            }
        }

        return $commandInfoList;
    }

    public function createCommandInfo($commandFileInstance, $commandMethodName)
    {
        $commandInfo = CommandInfo::create($commandFileInstance, $commandMethodName);
        $this->alterCommandInfo($commandInfo, $commandFileInstance);
        return $commandInfo;
    }

    public function createCommandsFromClassInfo($commandInfoList, $commandFileInstance, $includeAllPublicMethods = null)
    {
        // Deprecated: avoid using the $includeAllPublicMethods in favor of the setIncludeAllPublicMethods() accessor.
        if (!isset($includeAllPublicMethods)) {
            $includeAllPublicMethods = $this->getIncludeAllPublicMethods();
        }
        return $this->createSelectedCommandsFromClassInfo(
            $commandInfoList,
            $commandFileInstance,
            function ($commandInfo) use ($includeAllPublicMethods) {
                return $this->isMethodRecognizedAsCommand($commandInfo, $includeAllPublicMethods);
            }
        );
    }

    public function createSelectedCommandsFromClassInfo($commandInfoList, $commandFileInstance, callable $commandSelector)
    {
        $commandInfoList = $this->filterCommandInfoList($commandInfoList, $commandSelector);
        return array_map(
            function ($commandInfo) use ($commandFileInstance) {
                return $this->createCommand($commandInfo, $commandFileInstance);
            },
            $commandInfoList
        );
    }

    protected function isMethodRecognizedAsCommand($commandInfo, $includeAllPublicMethods)
    {
        // Ignore everything labeled @hook
        if ($this->isMethodRecognizedAsHook($commandInfo)) {
            return false;
        }
        // Ignore everything labeled @ignored-command
        if ($commandInfo->hasAnnotation('ignored-command')) {
            return false;
        }
        // Include everything labeled @command
        if ($commandInfo->hasAnnotation('command')) {
            return true;
        }
        // Skip anything that has a missing or invalid name.
        $commandName = $commandInfo->getName();
        if (empty($commandName) || preg_match('#[^a-zA-Z0-9:_-]#', $commandName)) {
            return false;
        }
        // Skip anything named like an accessor ('get' or 'set')
        if (preg_match('#^(get[A-Z]|set[A-Z])#', $commandInfo->getMethodName())) {
            return false;
        }

        // Skip based on the configured regular expresions
        foreach ($this->ignoredCommandsRegexps as $regex) {
            if (preg_match($regex, $commandInfo->getMethodName())) {
                return false;
            }
        }

        // Default to the setting of 'include all public methods'.
        return $includeAllPublicMethods;
    }

    protected function isMethodRecognizedAsHook($commandInfo)
    {
        return $commandInfo->hasAnnotation('hook');
    }

    protected function filterCommandInfoList($commandInfoList, callable $commandSelector)
    {
        return array_filter($commandInfoList, $commandSelector);
    }

    public static function isCommandOrHookMethod($commandInfo, $includeAllPublicMethods)
    {
        return static::isHookMethod($commandInfo) || static::isCommandMethod($commandInfo, $includeAllPublicMethods);
    }

    // Deprecated: avoid using the isHookMethod in favor of the protected non-static isMethodRecognizedAsHook
    public static function isHookMethod($commandInfo)
    {
        return $commandInfo->hasAnnotation('hook');
    }

    // Deprecated: avoid using the isCommandMethod in favor of the protected non-static isMethodRecognizedAsCommand
    public static function isCommandMethod($commandInfo, $includeAllPublicMethods)
    {
        // Ignore everything labeled @hook
        if (static::isHookMethod($commandInfo)) {
            return false;
        }
        // Ignore everything labeled @ignored-command
        if ($commandInfo->hasAnnotation('ignored-command')) {
            return false;
        }
        // Include everything labeled @command
        if ($commandInfo->hasAnnotation('command')) {
            return true;
        }
        // Skip anything that has a missing or invalid name.
        $commandName = $commandInfo->getName();
        if (empty($commandName) || preg_match('#[^a-zA-Z0-9:_-]#', $commandName)) {
            return false;
        }
        // Skip anything named like an accessor ('get' or 'set')
        if (preg_match('#^(get[A-Z]|set[A-Z])#', $commandInfo->getMethodName())) {
            return false;
        }

        // Default to the setting of 'include all public methods'.
        return $includeAllPublicMethods;
    }

    public function registerCommandHooksFromClassInfo($commandInfoList, $commandFileInstance)
    {
        foreach ($commandInfoList as $commandInfo) {
            if (static::isHookMethod($commandInfo)) {
                $this->registerCommandHook($commandInfo, $commandFileInstance);
            }
        }
    }

    /**
     * Register a command hook given the CommandInfo for a method.
     *
     * The hook format is:
     *
     *   @hook type name type
     *
     * For example, the pre-validate hook for the core:init command is:
     *
     *   @hook pre-validate core:init
     *
     * If no command name is provided, then this hook will affect every
     * command that is defined in the same file.
     *
     * If no hook is provided, then we will presume that ALTER_RESULT
     * is intended.
     *
     * @param CommandInfo $commandInfo Information about the command hook method.
     * @param object $commandFileInstance An instance of the CommandFile class.
     */
    public function registerCommandHook(CommandInfo $commandInfo, $commandFileInstance)
    {
        // Ignore if the command info has no @hook
        if (!static::isHookMethod($commandInfo)) {
            return;
        }
        $hookData = $commandInfo->getAnnotation('hook');
        $hook = $this->getNthWord($hookData, 0, HookManager::ALTER_RESULT);
        $commandName = $this->getNthWord($hookData, 1);

        // Register the hook
        $callback = [$commandFileInstance, $commandInfo->getMethodName()];
        $this->commandProcessor()->hookManager()->add($callback, $hook, $commandName);

        // If the hook has options, then also register the commandInfo
        // with the hook manager, so that we can add options and such to
        // the commands they hook.
        if (!$commandInfo->options()->isEmpty()) {
            $this->commandProcessor()->hookManager()->recordHookOptions($commandInfo, $commandName);
        }
    }

    protected function getNthWord($string, $n, $default = '', $delimiter = ' ')
    {
        $words = explode($delimiter, $string);
        if (!empty($words[$n])) {
            return $words[$n];
        }
        return $default;
    }

    public function createCommand(CommandInfo $commandInfo, $commandFileInstance)
    {
        $command = new AnnotatedCommand($commandInfo->getName());
        $commandCallback = [$commandFileInstance, $commandInfo->getMethodName()];
        $command->setCommandCallback($commandCallback);
        $completionCallback = null;
        if ($annotation = $commandInfo->getAnnotation('complete')) {
            $completionCallback = is_callable($annotation) ?: [$commandFileInstance, $annotation];
        }
        $command->setCompletionCallback($completionCallback);
        $command->setCommandProcessor($this->commandProcessor);
        $command->setCommandInfo($commandInfo);
        $automaticOptions = $this->callAutomaticOptionsProviders($commandInfo);
        $command->setCommandOptions($commandInfo, $automaticOptions);
        // Annotation commands are never bootstrap-aware, but for completeness
        // we will notify on every created command, as some clients may wish to
        // use this notification for some other purpose.
        $this->notify($command);
        return $command;
    }

    /**
     * Give plugins an opportunity to update the commandInfo
     */
    public function alterCommandInfo(CommandInfo $commandInfo, $commandFileInstance)
    {
        foreach ($this->commandInfoAlterers as $alterer) {
            $alterer->alterCommandInfo($commandInfo, $commandFileInstance);
        }
    }

    /**
     * Get the options that are implied by annotations, e.g. @fields implies
     * that there should be a --fields and a --format option.
     *
     * @return InputOption[]
     */
    public function callAutomaticOptionsProviders(CommandInfo $commandInfo)
    {
        $automaticOptions = [];
        foreach ($this->automaticOptionsProviderList as $automaticOptionsProvider) {
            $automaticOptions += $automaticOptionsProvider->automaticOptions($commandInfo);
        }
        return $automaticOptions;
    }

    /**
     * Get the options that are implied by annotations, e.g. @fields implies
     * that there should be a --fields and a --format option.
     *
     * @return InputOption[]
     */
    public function automaticOptions(CommandInfo $commandInfo)
    {
        $automaticOptions = [];
        $formatManager = $this->commandProcessor()->formatterManager();
        if ($formatManager) {
            $annotationData = $commandInfo->getAnnotations()->getArrayCopy();
            $formatterOptions = new FormatterOptions($annotationData);
            $dataType = $commandInfo->getReturnType();
            $automaticOptions = $formatManager->automaticOptions($formatterOptions, $dataType);
        }
        return $automaticOptions;
    }
}
