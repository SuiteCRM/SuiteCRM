<?php

namespace Robo;

use Composer\Autoload\ClassLoader;
use League\Container\Container;
use League\Container\ContainerAwareInterface;
use League\Container\Definition\DefinitionInterface;
use Psr\Container\ContainerInterface;
use Robo\Collection\CollectionBuilder;
use Robo\Common\ProcessExecutor;
use Robo\Contract\BuilderAwareInterface;
use Consolidation\Config\ConfigInterface;
use Consolidation\Config\Loader\ConfigProcessor;
use Consolidation\Config\Loader\YamlConfigLoader;
use Symfony\Component\Console\Input\StringInput;
use Symfony\Component\Console\Application as SymfonyApplication;
use Symfony\Component\Console\Command\Command;
use Symfony\Component\Process\Process;

/**
 * Manages the container reference and other static data.  Favor
 * using dependency injection wherever possible.  Avoid using
 * this class directly, unless setting up a custom DI container.
 */
class Robo
{
    const APPLICATION_NAME = 'Robo';
    const VERSION = '3.0.12';

    /**
     * The currently active container object, or NULL if not initialized yet.
     *
     * @var \Psr\Container\ContainerInterface|null
     */
    protected static $container;

    /**
     * Entrypoint for standalone Robo-based tools.  See docs/framework.md.
     *
     * @param string[] $argv
     * @param string $commandClasses
     * @param null|string $appName
     * @param null|string $appVersion
     * @param null|\Symfony\Component\Console\Output\OutputInterface $output
     * @param null|string $repository
     *
     * @return int
     */
    public static function run($argv, $commandClasses, $appName = null, $appVersion = null, $output = null, $repository = null)
    {
        $runner = new \Robo\Runner($commandClasses);
        $runner->setSelfUpdateRepository($repository);
        $statusCode = $runner->execute($argv, $appName, $appVersion, $output);
        return $statusCode;
    }

    /**
     * Sets a new global container.
     *
     * @param \Psr\Container\ContainerInterface $container
     *   A new container instance to replace the current.
     */
    public static function setContainer(ContainerInterface $container)
    {
        static::$container = $container;
    }

    /**
     * Unsets the global container.
     */
    public static function unsetContainer()
    {
        static::$container = null;
    }

    /**
     * Returns the currently active global container.
     *
     * @return \Psr\Container\ContainerInterface
     *
     * @throws \RuntimeException
     */
    public static function getContainer()
    {
        if (static::$container === null) {
            throw new \RuntimeException('container is not initialized yet. \Robo\Robo::setContainer() must be called with a real container.');
        }
        return static::$container;
    }

    /**
     * Returns TRUE if the container has been initialized, FALSE otherwise.
     *
     * @return bool
     */
    public static function hasContainer()
    {
        return static::$container !== null;
    }

    /**
     * Create a config object and load it from the provided paths.
     *
     * @param string[] $paths
     *
     * @return \Consolidation\Config\ConfigInterface
     */
    public static function createConfiguration($paths)
    {
        $config = new \Robo\Config\Config();
        static::loadConfiguration($paths, $config);
        return $config;
    }

    /**
     * Use a simple config loader to load configuration values from specified paths
     *
     * @param string[] $paths
     * @param null|\Consolidation\Config\ConfigInterface $config
     */
    public static function loadConfiguration($paths, $config = null)
    {
        if ($config == null) {
            $config = static::config();
        }
        $loader = new YamlConfigLoader();
        $processor = new ConfigProcessor();
        $processor->add($config->export());
        foreach ($paths as $path) {
            $processor->extend($loader->load($path));
        }
        $config->import($processor->export());
    }

    /**
     * Create a container for Robo application.
     *
     * After calling this method you may add any additional items you wish
     * to manage in your application. After you do that, you must call
     * Robo::finalizeContainer($container) to complete container initialization.
     *
     * @param null|\Robo\Application $app
     * @param null|\Consolidation\Config\ConfigInterface $config
     * @param null|\Composer\Autoload\ClassLoader $classLoader
     *
     * @return \Psr\Container\ContainerInterface
     */
    public static function createContainer($app = null, $config = null, $classLoader = null)
    {
        // Do not allow this function to be called more than once.
        if (static::hasContainer()) {
            return static::getContainer();
        }

        if (!$app) {
            $app = static::createDefaultApplication();
        }

        if (!$config) {
            $config = new \Robo\Config\Config();
        }

        // $input and $output will not be stored in the container at all in the future.
        $unusedInput = new StringInput('');
        $unusedOutput = new \Symfony\Component\Console\Output\NullOutput();

        // Set up our dependency injection container.
        $container = new Container();
        static::configureContainer($container, $app, $config, $unusedInput, $unusedOutput, $classLoader);

        return $container;
    }

    /**
     * Create a container and initiailze it.  If you wish to *change*
     * anything defined in the container, then you should call
     * Robo::createContainer() and Robo::finalizeContainer() instead of this function.
     *
     * @param null|\Symfony\Component\Console\Input\InputInterface $input
     * @param null|\Symfony\Component\Console\Output\OutputInterface $output
     * @param null|\Robo\Application $app
     * @param null|\Consolidation\Config\ConfigInterface $config
     * @param null|\Composer\Autoload\ClassLoader $classLoader
     *
     * @deprecated Use createContainer instead
     *
     * @return \Psr\Container\ContainerInterface
     */
    public static function createDefaultContainer($input = null, $output = null, $app = null, $config = null, $classLoader = null)
    {
        // Do not allow this function to be called more than once.
        if (static::hasContainer()) {
            return static::getContainer();
        }

        if (!$app) {
            $app = static::createDefaultApplication();
        }

        if (!$config) {
            $config = new \Robo\Config\Config();
        }

        // Set up our dependency injection container.
        $container = new Container();
        static::configureContainer($container, $app, $config, $input, $output, $classLoader);
        static::finalizeContainer($container);

        return $container;
    }

    /**
     * Do final initialization to the provided container. Make any necessary
     * modifications to the container before calling this method.
     *
     * @param ContainerInterface $container
     */
    public static function finalizeContainer(ContainerInterface $container)
    {
        $app = $container->get('application');

        // Set the application dispatcher
        $app->setDispatcher($container->get('eventDispatcher'));
    }

    /**
     * Adds a shared instance to the container. This is to support 3.x and 4.x of league/container.
     * @param \Psr\Container\ContainerInterface $container
     * @param string $id
     * @param mixed $concrete
     * @return \League\Container\Definition\DefinitionInterface
     */
    public static function addShared(ContainerInterface $container, string $id, $concrete)
    {
        if (method_exists($container, 'addShared')) {
            return $container->addShared($id, $concrete);
        } else {
            return $container->share($id, $concrete);
        }
    }

    /**
     * Initialize a container with all of the default Robo services.
     * IMPORTANT:  after calling this method, clients MUST call:
     *
     * Robo::finalizeContainer($container);
     *
     * Any modification to the container should be done prior to fetching
     * objects from it.
     *
     * It is recommended to use Robo::createContainer() instead.
     *
     * @param \Psr\Container\ContainerInterface $container
     * @param \Symfony\Component\Console\Application $app
     * @param \Consolidation\Config\ConfigInterface $config
     * @param null|\Symfony\Component\Console\Input\InputInterface $input
     * @param null|\Symfony\Component\Console\Output\OutputInterface $output
     * @param null|\Composer\Autoload\ClassLoader $classLoader
     */
    public static function configureContainer(ContainerInterface $container, SymfonyApplication $app, ConfigInterface $config, $input = null, $output = null, $classLoader = null)
    {
        // Self-referential container refernce for the inflector
        $container->add('container', $container);
        static::setContainer($container);

        // Create default input and output objects if they were not provided.
        // TODO: We would like to remove $input and $output from the container
        // (or always register StringInput('') and NullOutput()). There are
        // currently three shortcomings preventing this:
        //  1. The logger cannot be used (we could remove the logger from Robo)
        //  2. Commands that abort with an exception do not print a message (bug)
        //  3. The runner tests do not initialize taskIO correctly for all tests
        if (!$input) {
            $input = new StringInput('');
        }
        if (!$output) {
            $output = new \Symfony\Component\Console\Output\ConsoleOutput();
        }
        if (!$classLoader) {
            $classLoader = new ClassLoader();
        }
        $config->set(Config::DECORATED, $output->isDecorated());
        $config->set(Config::INTERACTIVE, $input->isInteractive());

        self::addShared($container, 'application', $app);
        self::addShared($container, 'config', $config);
        self::addShared($container, 'input', $input);
        self::addShared($container, 'output', $output);
        self::addShared($container, 'outputAdapter', \Robo\Common\OutputAdapter::class);
        self::addShared($container, 'classLoader', $classLoader);

        // Register logging and related services.
        self::addShared($container, 'logStyler', \Robo\Log\RoboLogStyle::class);
        self::addShared($container, 'logger', \Robo\Log\RoboLogger::class)
            ->addArgument('output')
            ->addMethodCall('setLogOutputStyler', ['logStyler']);
        $container->add('progressBar', \Symfony\Component\Console\Helper\ProgressBar::class)
            ->addArgument('output');
        self::addShared($container, 'progressIndicator', \Robo\Common\ProgressIndicator::class)
            ->addArgument('progressBar')
            ->addArgument('output');
        self::addShared($container, 'resultPrinter', \Robo\Log\ResultPrinter::class);
        $container->add('simulator', \Robo\Task\Simulator::class);
        self::addShared($container, 'globalOptionsEventListener', \Robo\GlobalOptionsEventListener::class)
            ->addMethodCall('setApplication', ['application']);
        self::addShared($container, 'injectConfigEventListener', \Consolidation\Config\Inject\ConfigForCommand::class)
            ->addArgument('config')
            ->addMethodCall('setApplication', ['application']);
        self::addShared($container, 'collectionProcessHook', \Robo\Collection\CollectionProcessHook::class);
        self::addShared($container, 'alterOptionsCommandEvent', \Consolidation\AnnotatedCommand\Options\AlterOptionsCommandEvent::class)
            ->addArgument('application');
        self::addShared($container, 'hookManager', \Consolidation\AnnotatedCommand\Hooks\HookManager::class)
            ->addMethodCall('addCommandEvent', ['alterOptionsCommandEvent'])
            ->addMethodCall('addCommandEvent', ['injectConfigEventListener'])
            ->addMethodCall('addCommandEvent', ['globalOptionsEventListener'])
            ->addMethodCall('addResultProcessor', ['collectionProcessHook', '*']);
        self::addShared($container, 'eventDispatcher', \Symfony\Component\EventDispatcher\EventDispatcher::class)
            ->addMethodCall('addSubscriber', ['hookManager']);
        self::addShared($container, 'formatterManager', \Consolidation\OutputFormatters\FormatterManager::class)
            ->addMethodCall('addDefaultFormatters', [])
            ->addMethodCall('addDefaultSimplifiers', []);
        self::addShared($container, 'prepareTerminalWidthOption', \Consolidation\AnnotatedCommand\Options\PrepareTerminalWidthOption::class)
            ->addMethodCall('setApplication', ['application']);
        self::addShared($container, 'symfonyStyleInjector', \Robo\Symfony\SymfonyStyleInjector::class);
        self::addShared($container, 'consoleIOInjector', \Robo\Symfony\ConsoleIOInjector::class);
        self::addShared($container, 'parameterInjection', \Consolidation\AnnotatedCommand\ParameterInjection::class)
            ->addMethodCall('register', ['Symfony\Component\Console\Style\SymfonyStyle', 'symfonyStyleInjector'])
            ->addMethodCall('register', ['Robo\Symfony\ConsoleIO', 'consoleIOInjector']);
        self::addShared($container, 'commandProcessor', \Consolidation\AnnotatedCommand\CommandProcessor::class)
            ->addArgument('hookManager')
            ->addMethodCall('setFormatterManager', ['formatterManager'])
            ->addMethodCall('addPrepareFormatter', ['prepareTerminalWidthOption'])
            ->addMethodCall('setParameterInjection', ['parameterInjection'])
            ->addMethodCall(
                'setDisplayErrorFunction',
                [
                    function ($output, $message) use ($container) {
                        $logger = $container->get('logger');
                        $logger->error($message);
                    }
                ]
            );
        self::addShared($container, 'stdinHandler', \Consolidation\AnnotatedCommand\Input\StdinHandler::class);
        self::addShared($container, 'commandFactory', \Consolidation\AnnotatedCommand\AnnotatedCommandFactory::class)
            ->addMethodCall('setCommandProcessor', ['commandProcessor'])
            // Public methods from the class Robo\Commo\IO that should not be
            // added as available commands.
            ->addMethodCall('addIgnoredCommandsRegexp', ['/^currentState$|^restoreState$/']);
        self::addShared($container, 'relativeNamespaceDiscovery', \Robo\ClassDiscovery\RelativeNamespaceDiscovery::class)
            ->addArgument('classLoader');

        // Deprecated: favor using collection builders to direct use of collections.
        $container->add('collection', \Robo\Collection\Collection::class);
        // Deprecated: use CollectionBuilder::create() instead -- or, better
        // yet, BuilderAwareInterface::collectionBuilder() if available.
        $container->add('collectionBuilder', \Robo\Collection\CollectionBuilder::class);

        static::addInflectors($container);

        // Make sure the application is appropriately initialized.
        $app->setAutoExit(false);
    }

    /**
     * @param null|string $appName
     * @param null|string $appVersion
     *
     * @return \Robo\Application
     */
    public static function createDefaultApplication($appName = null, $appVersion = null)
    {
        $appName = $appName ?: self::APPLICATION_NAME;
        $appVersion = $appVersion ?: self::VERSION;

        $app = new \Robo\Application($appName, $appVersion);
        $app->setAutoExit(false);
        return $app;
    }

    /**
     * Add the Robo League\Container inflectors to the container
     *
     * @param \Psr\Container\ContainerInterface $container
     */
    public static function addInflectors($container)
    {
        // Register our various inflectors.
        $container->inflector(\Robo\Contract\ConfigAwareInterface::class)
            ->invokeMethod('setConfig', ['config']);
        $container->inflector(\Psr\Log\LoggerAwareInterface::class)
            ->invokeMethod('setLogger', ['logger']);
        $container->inflector(\League\Container\ContainerAwareInterface::class)
            ->invokeMethod('setContainer', ['container']);
        $container->inflector(\Symfony\Component\Console\Input\InputAwareInterface::class)
            ->invokeMethod('setInput', ['input']);
        $container->inflector(\Robo\Contract\OutputAwareInterface::class)
            ->invokeMethod('setOutput', ['output']);
        $container->inflector(\Robo\Contract\ProgressIndicatorAwareInterface::class)
            ->invokeMethod('setProgressIndicator', ['progressIndicator']);
        $container->inflector(\Consolidation\AnnotatedCommand\Events\CustomEventAwareInterface::class)
            ->invokeMethod('setHookManager', ['hookManager']);
        $container->inflector(\Robo\Contract\VerbosityThresholdInterface::class)
            ->invokeMethod('setOutputAdapter', ['outputAdapter']);
        $container->inflector(\Consolidation\AnnotatedCommand\Input\StdinAwareInterface::class)
            ->invokeMethod('setStdinHandler', ['stdinHandler']);
    }

    /**
     * Retrieves a service from the container.
     *
     * Use this method if the desired service is not one of those with a dedicated
     * accessor method below. If it is listed below, those methods are preferred
     * as they can return useful type hints.
     *
     * @param string $id
     *   The ID of the service to retrieve.
     *
     * @return mixed
     *   The specified service.
     */
    public static function service($id)
    {
        return static::getContainer()->get($id);
    }

    /**
     * Indicates if a service is defined in the container.
     *
     * @param string $id
     *   The ID of the service to check.
     *
     * @return bool
     *   TRUE if the specified service exists, FALSE otherwise.
     */
    public static function hasService($id)
    {
        // Check hasContainer() first in order to always return a Boolean.
        return static::hasContainer() && static::getContainer()->has($id);
    }

    /**
     * Return the result printer object.
     *
     * @return \Robo\Log\ResultPrinter
     *
     * @deprecated
     */
    public static function resultPrinter()
    {
        return static::service('resultPrinter');
    }

    /**
     * @return \Consolidation\Config\ConfigInterface
     */
    public static function config()
    {
        return static::service('config');
    }

    /**
     * @return \Consolidation\Log\Logger
     */
    public static function logger()
    {
        return static::service('logger');
    }

    /**
     * @return \Robo\Application
     */
    public static function application()
    {
        return static::service('application');
    }

    /**
     * Return the output object.
     *
     * @return \Symfony\Component\Console\Output\OutputInterface
     */
    public static function output()
    {
        return static::service('output');
    }

    /**
     * Return the input object.
     *
     * @return \Symfony\Component\Console\Input\InputInterface
     */
    public static function input()
    {
        return static::service('input');
    }

    /**
     * @return \Robo\Common\ProcessExecutor
     */
    public static function process(Process $process)
    {
        return ProcessExecutor::create(static::getContainer(), $process);
    }

    /**
     * @param \Robo\Application $app
     * @param string|object $handler
     *
     * @return null|object
     */
    public static function register($app, $handler)
    {
        $container = Robo::getContainer();
        $instance = static::instantiate($handler);
        if (!$instance) {
            return;
        }

        // If the instance is a Symfony Command, register it with
        // the application
        if ($instance instanceof Command) {
            $app->add($instance);
            return $instance;
        }

        // Register commands for all of the public methods in the RoboFile.
        $commandFactory = $container->get('commandFactory');
        $commandList = $commandFactory->createCommandsFromClass($instance);
        foreach ($commandList as $command) {
            $app->add($command);
        }
        return $instance;
    }

    /**
     * @param string|object $handler
     *
     * @return null|object
     */
    protected static function instantiate($handler)
    {
        $container = Robo::getContainer();

        // Register the RoboFile with the container and then immediately
        // fetch it; this ensures that all of the inflectors will run.
        // If the command class is already an instantiated object, then
        // just use it exactly as it was provided to us.
        if (is_string($handler)) {
            if (!class_exists($handler)) {
                return;
            }
            $reflectionClass = new \ReflectionClass($handler);
            if ($reflectionClass->isAbstract()) {
                return;
            }

            $commandFileName = "{$handler}Commands";
            Robo::addShared($container, $commandFileName, $handler);
            $handler = $container->get($commandFileName);
        }
        // If the command class is a Builder Aware Interface, then
        // ensure that it has a builder.  Every command class needs
        // its own collection builder, as they have references to each other.
        if ($handler instanceof BuilderAwareInterface) {
            $builder = CollectionBuilder::create($container, $handler);
            $handler->setBuilder($builder);
        }
        if ($handler instanceof ContainerAwareInterface) {
            $handler->setContainer($container);
        }
        return $handler;
    }
}
