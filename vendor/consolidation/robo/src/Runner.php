<?php

namespace Robo;

use Composer\Autoload\ClassLoader;
use Symfony\Component\Console\Input\ArgvInput;
use Symfony\Component\Console\Input\StringInput;
use Robo\Contract\BuilderAwareInterface;
use Robo\Collection\CollectionBuilder;
use Robo\Common\IO;
use Robo\Exception\TaskExitException;
use League\Container\ContainerAwareInterface;
use League\Container\ContainerAwareTrait;
use League\Container\Exception\ContainerException;
use Consolidation\Config\Util\EnvConfig;
use Symfony\Component\Console\Output\NullOutput;

class Runner implements ContainerAwareInterface
{
    use IO;
    use ContainerAwareTrait;

    const ROBOCLASS = 'RoboFile';
    const ROBOFILE = 'RoboFile.php';

    /**
     * @var string
     */
    protected $roboClass;

    /**
     * @var string
     */
    protected $roboFile;

    /**
     * Working dir of Robo.
     *
     * @var string
     */
    protected $dir;

    /**
     * @var string[]
     */
    protected $errorConditions = [];

    /**
     * GitHub Repo for SelfUpdate.
     *
     * @var string
     */
    protected $selfUpdateRepository = null;

    /**
     * Filename to load configuration from (set to 'robo.yml' for RoboFiles).
     *
     * @var string
     */
    protected $configFilename = 'conf.yml';

    /**
     * @var string prefix for environment variable configuration overrides
     */
    protected $envConfigPrefix = false;

    /**
     * @var null|\Composer\Autoload\ClassLoader
     */
    protected $classLoader = null;

    /**
     * @var string
     */
    protected $relativePluginNamespace;

    /**
     * Class Constructor
     *
     * @param null|string $roboClass
     * @param null|string $roboFile
     */
    public function __construct($roboClass = null, $roboFile = null)
    {
        // set the const as class properties to allow overwriting in child classes
        $this->roboClass = $roboClass ? $roboClass : self::ROBOCLASS ;
        $this->roboFile  = $roboFile ? $roboFile : self::ROBOFILE;
        $this->dir = getcwd();
    }

    /**
     * @param string $msg
     * @param string $errorType
     */
    protected function errorCondition($msg, $errorType)
    {
        $this->errorConditions[$msg] = $errorType;
    }

    /**
     * @param \Symfony\Component\Console\Output\OutputInterface $output
     *
     * @return bool
     */
    protected function loadRoboFile($output)
    {
        // If we have not been provided an output object, make a temporary one.
        if (!$output) {
            $output = new \Symfony\Component\Console\Output\ConsoleOutput();
        }

        // If $this->roboClass is a single class that has not already
        // been loaded, then we will try to obtain it from $this->roboFile.
        // If $this->roboClass is an array, we presume all classes requested
        // are available via the autoloader.
        if (is_array($this->roboClass) || class_exists($this->roboClass)) {
            return true;
        }
        if (!file_exists($this->dir)) {
            $this->errorCondition("Path `{$this->dir}` is invalid; please provide a valid absolute path to the Robofile to load.", 'red');
            return false;
        }

        $realDir = realpath($this->dir);

        $roboFilePath = $realDir . DIRECTORY_SEPARATOR . $this->roboFile;
        if (!file_exists($roboFilePath)) {
            $requestedRoboFilePath = $this->dir . DIRECTORY_SEPARATOR . $this->roboFile;
            $this->errorCondition("Requested RoboFile `$requestedRoboFilePath` is invalid, please provide valid absolute path to load Robofile.", 'red');
            return false;
        }
        require_once $roboFilePath;

        if (!class_exists($this->roboClass)) {
            $this->errorCondition("Class {$this->roboClass} was not loaded.", 'red');
            return false;
        }
        return true;
    }

    /**
     * @param array $argv
     * @param null|string $appName
     * @param null|string $appVersion
     * @param null|\Symfony\Component\Console\Output\OutputInterface $output
     *
     * @return int
     */
    public function execute($argv, $appName = null, $appVersion = null, $output = null)
    {
        $argv = $this->shebang($argv);
        $argv = $this->processRoboOptions($argv);
        $app = null;
        if ($appName && $appVersion) {
            $app = Robo::createDefaultApplication($appName, $appVersion);
        }
        $commandFiles = $this->getRoboFileCommands($output);
        return $this->run($argv, $output, $app, $commandFiles, $this->classLoader);
    }

    /**
     * Return an initialized application loaded with specified commands and configuration.
     *
     * This should ONLY be used for testing purposes. Works well in conjunction with Symfony's CommandTester.
     *
     * @see https://symfony.com/doc/current/console.html#testing-commands
     * @see CommandTestertTest
     * @see CommandTesterTrait
     *
     * @param string|null $appName
     *   Name of the application.
     * @param string|null $appVersion
     *   Version of the application.
     * @param string|array|null $commandFile
     *   Name of the specific command file, or array of commands, that should be included with the application.
     * @param \Robo\Config\Config|null $config
     *   Robo configuration to be used with the application.
     * @param \Composer\Autoload\ClassLoader|null $classLoader
     *   Class loader to use.
     *
     * @return \Robo\Application
     *   Initialized application based on passed configuration and command classes.
     */
    public function getAppForTesting($appName = null, $appVersion = null, $commandFile = null, $config = null, $classLoader = null)
    {
        $app = Robo::createDefaultApplication($appName, $appVersion);
        $output = new NullOutput();
        $container = Robo::createDefaultContainer(null, $output, $app, $config, $classLoader);
        if (!is_null($commandFile) && (is_array($commandFile) || is_string($commandFile))) {
            if (is_string($commandFile)) {
                $commandFile = [$commandFile];
            }
            $this->registerCommandClasses($app, $commandFile);
        }
        return $app;
    }

    /**
     * Get a list of locations where config files may be loaded
     *
     * @param string $userConfig
     *
     * @return string[]
     */
    protected function getConfigFilePaths($userConfig)
    {
        // Look for application config at the root of the application.
        // Find the root relative to this file, considering that Robo itself
        // might be the application, or it might be in the `vendor` directory.
        $roboAppConfig = dirname(__DIR__) . '/' . basename($userConfig);
        if (basename(dirname(__DIR__, 3)) == 'vendor') {
            $roboAppConfig = dirname(__DIR__, 4) . '/' . basename($userConfig);
        }
        $configFiles = [$roboAppConfig, $userConfig];
        if (dirname($userConfig) != '.') {
            $configFiles[] = basename($userConfig);
        }
        return $configFiles;
    }

    /**
     * @param null|array|\Symfony\Component\Console\Input\InputInterface $input
     * @param null|\Symfony\Component\Console\Output\OutputInterface $output
     * @param null|\Robo\Application $app
     * @param array[] $commandFiles
     * @param null|ClassLoader $classLoader
     *
     * @return int
     */
    public function run($input = null, $output = null, $app = null, $commandFiles = [], $classLoader = null)
    {
        // Create default input and output objects if they were not provided
        if (!$input) {
            $input = new StringInput('');
        }
        if (is_array($input)) {
            $input = new ArgvInput($input);
        }
        if (!$output) {
            $output = new \Symfony\Component\Console\Output\ConsoleOutput();
        }
        $this->setInput($input);
        $this->setOutput($output);

        // If we were not provided a container, then create one
        try {
            $this->getContainer();
        } catch (ContainerException $e) {
            $configFiles = $this->getConfigFilePaths($this->configFilename);
            $config = Robo::createConfiguration($configFiles);
            if ($this->envConfigPrefix) {
                $envConfig = new EnvConfig($this->envConfigPrefix);
                $config->addContext('env', $envConfig);
            }
            $container = Robo::createDefaultContainer($input, $output, $app, $config, $classLoader);
            $this->setContainer($container);
            // Automatically register a shutdown function and
            // an error handler when we provide the container.
            $this->installRoboHandlers();
        }

        if (!$app) {
            $app = Robo::application();
        }
        if ($app instanceof \Robo\Application) {
            $app->addSelfUpdateCommand($this->getSelfUpdateRepository());
            if (!isset($commandFiles)) {
                $this->errorCondition("Robo is not initialized here. Please run `robo init` to create a new RoboFile.", 'yellow');
                $app->addInitRoboFileCommand($this->roboFile, $this->roboClass);
                $commandFiles = [];
            }
        }

        if (!empty($this->relativePluginNamespace)) {
            $commandClasses = $this->discoverCommandClasses($this->relativePluginNamespace);
            $commandFiles = array_merge((array)$commandFiles, $commandClasses);
        }

        $this->registerCommandClasses($app, $commandFiles);

        try {
            $statusCode = $app->run($input, $output);
        } catch (TaskExitException $e) {
            $statusCode = $e->getCode() ?: 1;
        }

        // If there were any error conditions in bootstrapping Robo,
        // print them only if the requested command did not complete
        // successfully.
        if ($statusCode) {
            foreach ($this->errorConditions as $msg => $color) {
                // TODO: This was 'yell'. Add styling?
                $output->writeln($msg); // used to wrap at 40 and write in $color
            }
        }
        return $statusCode;
    }

    /**
     * @param \Symfony\Component\Console\Output\OutputInterface $output
     *
     * @return null|string
     */
    protected function getRoboFileCommands($output)
    {
        if (!$this->loadRoboFile($output)) {
            return;
        }
        return $this->roboClass;
    }

    /**
     * @param \Robo\Application $app
     * @param array $commandClasses
     */
    public function registerCommandClasses($app, $commandClasses)
    {
        foreach ((array)$commandClasses as $commandClass) {
            Robo::register($app, $commandClass);
        }
    }

    /**
     * @param string $relativeNamespace
     *
     * @return string[]
     */
    protected function discoverCommandClasses($relativeNamespace)
    {
        /** @var \Robo\ClassDiscovery\RelativeNamespaceDiscovery $discovery */
        $discovery = Robo::service('relativeNamespaceDiscovery');
        $discovery->setRelativeNamespace($relativeNamespace . '\Commands')
            ->setSearchPattern('/.*Commands?\.php$/');
        return $discovery->getClasses();
    }

    /**
     * @param \Robo\Application $app
     * @param string|BuilderAwareInterface|ContainerAwareInterface $commandClass
     *
     * @deprecated Use Robo::register directly
     *
     * @return null|object
     */
    public function registerCommandClass($app, $commandClass)
    {
        return Robo::register($app, $commandClass);
    }

    /**
     * @deprecated Use Robo::instantiate directly
     */
    protected function instantiateCommandClass($commandClass)
    {
        return Robo::instantiate($commandClass);
    }

    public function installRoboHandlers()
    {
        register_shutdown_function(array($this, 'shutdown'));
        set_error_handler(array($this, 'handleError'));
    }

    /**
     * Process a shebang script, if one was used to launch this Runner.
     *
     * @param array $args
     *
     * @return array $args
     *   With shebang script removed.
     */
    protected function shebang($args)
    {
        // Option 1: Shebang line names Robo, but includes no parameters.
        // #!/bin/env robo
        // The robo class may contain multiple commands; the user may
        // select which one to run, or even get a list of commands or
        // run 'help' on any of the available commands as usual.
        if ((count($args) > 1) && $this->isShebangFile($args[1])) {
            return array_merge([$args[0]], array_slice($args, 2));
        }
        // Option 2: Shebang line stipulates which command to run.
        // #!/bin/env robo mycommand
        // The robo class must contain a public method named 'mycommand'.
        // This command will be executed every time.  Arguments and options
        // may be provided on the commandline as usual.
        if ((count($args) > 2) && $this->isShebangFile($args[2])) {
            return array_merge([$args[0]], explode(' ', $args[1]), array_slice($args, 3));
        }
        return $args;
    }

    /**
     * Determine if the specified argument is a path to a shebang script.
     * If so, load it.
     *
     * @param string $filepath
     *   File to check.
     *
     * @return bool
     *   Returns TRUE if shebang script was processed.
     */
    protected function isShebangFile($filepath)
    {
        // Avoid trying to call $filepath on remote URLs
        if ((strpos($filepath, '://') !== false) && (substr($filepath, 0, 7) != 'file://')) {
            return false;
        }
        if (!is_file($filepath)) {
            return false;
        }
        $fp = fopen($filepath, "r");
        if ($fp === false) {
            return false;
        }
        $line = fgets($fp);
        $result = $this->isShebangLine($line);
        if ($result) {
            while ($line = fgets($fp)) {
                $line = trim($line);
                if ($line == '<?php') {
                    $script = stream_get_contents($fp);
                    if (preg_match('#^class *([^ ]+)#m', $script, $matches)) {
                        $this->roboClass = $matches[1];
                        eval($script);
                        $result = true;
                    }
                }
            }
        }
        fclose($fp);

        return $result;
    }

    /**
     * Test to see if the provided line is a robo 'shebang' line.
     *
     * @param string $line
     *
     * @return bool
     */
    protected function isShebangLine($line)
    {
        return ((substr($line, 0, 2) == '#!') && (strstr($line, 'robo') !== false));
    }

    /**
     * Check for Robo-specific arguments such as --load-from, process them,
     * and remove them from the array.  We have to process --load-from before
     * we set up Symfony Console.
     *
     * @param array $argv
     *
     * @return array
     */
    protected function processRoboOptions($argv)
    {
        // loading from other directory
        $pos = $this->arraySearchBeginsWith('--load-from', $argv) ?: array_search('-f', $argv);
        if ($pos === false) {
            return $argv;
        }

        $passThru = array_search('--', $argv);
        if (($passThru !== false) && ($passThru < $pos)) {
            return $argv;
        }

        if (substr($argv[$pos], 0, 12) == '--load-from=') {
            $this->dir = substr($argv[$pos], 12);
        } elseif (isset($argv[$pos + 1])) {
            $this->dir = $argv[$pos + 1];
            unset($argv[$pos + 1]);
        }
        unset($argv[$pos]);
        // Make adjustments if '--load-from' points at a file.
        if (is_file($this->dir) || (substr($this->dir, -4) == '.php')) {
            $this->roboFile = basename($this->dir);
            $this->dir = dirname($this->dir);
            $className = basename($this->roboFile, '.php');
            if ($className != $this->roboFile) {
                $this->roboClass = $className;
            }
        }
        // Convert directory to a real path, but only if the
        // path exists. We do not want to lose the original
        // directory if the user supplied a bad value.
        $realDir = realpath($this->dir);
        if ($realDir) {
            chdir($realDir);
            $this->dir = $realDir;
        }

        return $argv;
    }

    /**
     * @param string $needle
     * @param string[] $haystack
     *
     * @return bool|int
     */
    protected function arraySearchBeginsWith($needle, $haystack)
    {
        for ($i = 0; $i < count($haystack); ++$i) {
            if (substr($haystack[$i], 0, strlen($needle)) == $needle) {
                return $i;
            }
        }
        return false;
    }

    public function shutdown()
    {
        $error = error_get_last();
        if (!is_array($error)) {
            return;
        }
        $this->writeln(sprintf("<error>ERROR: %s \nin %s:%d\n</error>", $error['message'], $error['file'], $error['line']));
    }

    /**
     * This is just a proxy error handler that checks the current error_reporting level.
     * In case error_reporting is disabled the error is marked as handled, otherwise
     * the normal internal error handling resumes.
     *
     * @return bool
     */
    public function handleError()
    {
        if (error_reporting() === 0) {
            return true;
        }
        return false;
    }

    /**
     * @return string
     */
    public function getSelfUpdateRepository()
    {
        return $this->selfUpdateRepository;
    }

    /**
     * @param $selfUpdateRepository
     *
     * @return $this
     */
    public function setSelfUpdateRepository($selfUpdateRepository)
    {
        $this->selfUpdateRepository = $selfUpdateRepository;
        return $this;
    }

    /**
     * @param string $configFilename
     *
     * @return $this
     */
    public function setConfigurationFilename($configFilename)
    {
        $this->configFilename = $configFilename;
        return $this;
    }

    /**
     * @param string $envConfigPrefix
     *
     * @return $this
     */
    public function setEnvConfigPrefix($envConfigPrefix)
    {
        $this->envConfigPrefix = $envConfigPrefix;
        return $this;
    }

    /**
     * @param \Composer\Autoload\ClassLoader $classLoader
     *
     * @return $this
     */
    public function setClassLoader(ClassLoader $classLoader)
    {
        $this->classLoader = $classLoader;
        return $this;
    }

    /**
     * @param string $relativeNamespace
     *
     * @return $this
     */
    public function setRelativePluginNamespace($relativeNamespace)
    {
        $this->relativePluginNamespace = $relativeNamespace;
        return $this;
    }
}
