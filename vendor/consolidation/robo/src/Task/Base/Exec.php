<?php

namespace Robo\Task\Base;

use Closure;
use Robo\Contract\CommandInterface;
use Robo\Contract\PrintedInterface;
use Robo\Contract\SimulatedInterface;
use Robo\Task\BaseTask;
use Symfony\Component\Process\Process;
use Robo\Result;
use Robo\Common\CommandReceiver;
use Robo\Common\ExecOneCommand;

/**
 * Executes shell script. Closes it when running in background mode.
 *
 * ``` php
 * <?php
 * $this->taskExec('compass')->arg('watch')->run();
 * // or use shortcut
 * $this->_exec('compass watch');
 *
 * $this->taskExec('compass watch')->background()->run();
 *
 * if ($this->taskExec('phpunit .')->run()->wasSuccessful()) {
 *  $this->say('tests passed');
 * }
 *
 * ?>
 * ```
 */
class Exec extends BaseTask implements CommandInterface, PrintedInterface, SimulatedInterface
{
    use CommandReceiver;
    use ExecOneCommand;

    /**
     * @var static[]
     */
    protected static $instances = [];

    /**
     * @var string|\Robo\Contract\CommandInterface
     */
    protected $command;

    private static $isSetupStopRunningJob = false;

    /**
     * @param string|\Robo\Contract\CommandInterface $command
     */
    public function __construct($command)
    {
        $this->command = $this->receiveCommand($command);

        $this->setupStopRunningJobs();
    }

    private function setupStopRunningJobs()
    {
        if (self::$isSetupStopRunningJob === true) {
            return;
        }

        $stopRunningJobs = Closure::fromCallable(self::class.'::stopRunningJobs');

        if (function_exists('pcntl_signal')) {
            pcntl_signal(SIGTERM, $stopRunningJobs);
        }

        register_shutdown_function($stopRunningJobs);

        self::$isSetupStopRunningJob = true;
    }

    public function __destruct()
    {
        $this->stop();
    }

    /**
     * Executes command in background mode (asynchronously)
     *
     * @param bool $arg
     *
     * @return $this
     */
    public function background($arg = true)
    {
        self::$instances[] = $this;
        $this->background = $arg;
        return $this;
    }

    /**
     * {@inheritdoc}
     */
    protected function getCommandDescription()
    {
        return $this->getCommand();
    }
    /**
     * {@inheritdoc}
     */
    public function getCommand()
    {
        return trim($this->command . $this->arguments);
    }

    /**
     * {@inheritdoc}
     */
    public function simulate($context)
    {
        $this->printAction($context);
    }

    public static function stopRunningJobs()
    {
        foreach (self::$instances as $instance) {
            if ($instance) {
                unset($instance);
            }
        }
    }

    /**
     * {@inheritdoc}
     */
    public function run()
    {
        $this->hideProgressIndicator();
        // TODO: Symfony 4 requires that we supply the working directory.
        $result_data = $this->execute(Process::fromShellCommandline($this->getCommand(), getcwd()));
        $result = new Result(
            $this,
            $result_data->getExitCode(),
            $result_data->getMessage(),
            $result_data->getData()
        );
        $this->showProgressIndicator();
        return $result;
    }
}
