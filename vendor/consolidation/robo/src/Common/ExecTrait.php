<?php

namespace Robo\Common;

use Robo\ResultData;
use Symfony\Component\Process\Process;

/**
 * Class ExecTrait
 * @package Robo\Common
 */
trait ExecTrait
{
    /**
     * @var bool
     */
    protected $background = false;

    /**
     * @var null|int
     */
    protected $timeout = null;

    /**
     * @var null|int
     */
    protected $idleTimeout = null;

    /**
     * @var null|array
     */
    protected $env = null;

    /**
     * @var Process
     */
    protected $process;

    /**
     * @var resource|string
     */
    protected $input;

    /**
     * @var boolean
     */
    protected $interactive = null;

    /**
     * @var bool
     */
    protected $isPrinted = true;

    /**
     * @var bool
     */
    protected $isMetadataPrinted = true;

    /**
     * @var string
     */
    protected $workingDirectory;

    /**
     * @return string
     */
    abstract public function getCommandDescription();

    /**
     * @see \Robo\Common\ProgressIndicatorAwareTrait
     * @see \Robo\Common\Timer
     */
    abstract protected function startTimer();

    /**
     * @see \Robo\Common\ProgressIndicatorAwareTrait
     * @see \Robo\Common\Timer
     */
    abstract protected function stopTimer();

    /**
     * @return null|float
     *
     * @see \Robo\Common\ProgressIndicatorAwareTrait
     * @see \Robo\Common\Timer
     */
    abstract protected function getExecutionTime();

    /**
     * @return bool
     *
     * @see \Robo\Common\TaskIO
     */
    abstract protected function hideTaskProgress();

    /**
     * @param bool $inProgress
     *
     * @see \Robo\Common\TaskIO
     */
    abstract protected function showTaskProgress($inProgress);

    /**
     * @param string $text
     * @param null|array $context
     *
     * @see \Robo\Common\TaskIO
     */
    abstract protected function printTaskInfo($text, $context = null);

    /**
     * @return bool
     *
     * @see \Robo\Common\VerbosityThresholdTrait
     */
    abstract public function verbosityMeetsThreshold();

    /**
     * @param string $message
     *
     * @see \Robo\Common\VerbosityThresholdTrait
     */
    abstract public function writeMessage($message);

    /**
     * Sets $this->interactive() based on posix_isatty().
     *
     * @return $this
     */
    public function detectInteractive()
    {
        // If the caller did not explicity set the 'interactive' mode,
        // and output should be produced by this task (verbosityMeetsThreshold),
        // then we will automatically set interactive mode based on whether
        // or not output was redirected when robo was executed.
        if (!isset($this->interactive) && function_exists('posix_isatty') && $this->verbosityMeetsThreshold()) {
            $this->interactive = posix_isatty(STDOUT);
        }

        return $this;
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
        $this->background = $arg;
        return $this;
    }

    /**
     * Stop command if it runs longer then $timeout in seconds
     *
     * @param int $timeout
     *
     * @return $this
     */
    public function timeout($timeout)
    {
        $this->timeout = $timeout;
        return $this;
    }

    /**
     * Stops command if it does not output something for a while
     *
     * @param int $timeout
     *
     * @return $this
     */
    public function idleTimeout($timeout)
    {
        $this->idleTimeout = $timeout;
        return $this;
    }

    /**
     * Set a single environment variable, or multiple.
     *
     * @param string|array $env
     * @param bool|string $value
     *
     * @return $this
     */
    public function env($env, $value = null)
    {
        if (!is_array($env)) {
            $env = [$env => ($value ? $value : true)];
        }
        return $this->envVars($env);
    }

    /**
     * Sets the environment variables for the command
     *
     * @param array $env
     *
     * @return $this
     */
    public function envVars(array $env)
    {
        $this->env = $this->env ? $env + $this->env : $env;
        return $this;
    }

    /**
     * Pass an input to the process. Can be resource created with fopen() or string
     *
     * @param resource|string $input
     *
     * @return $this
     */
    public function setProcessInput($input)
    {
        $this->input = $input;
        // A tty should not be allocated when the input is provided.
        $this->interactive(false);
        return $this;
    }

    /**
     * Pass an input to the process. Can be resource created with fopen() or string
     *
     * @param resource|string $input
     *
     * @return $this
     *
     * @deprecated
     */
    public function setInput($input)
    {
        trigger_error('setInput() is deprecated. Please use setProcessInput(().', E_USER_DEPRECATED);
        $this->input = $input;
        return $this;
    }

    /**
     * Attach tty to process for interactive input
     *
     * @param bool $interactive
     *
     * @return $this
     */
    public function interactive($interactive = true)
    {
        $this->interactive = $interactive;
        return $this;
    }


    /**
     * Is command printing its output to screen
     *
     * @return bool
     */
    public function getPrinted()
    {
        return $this->isPrinted;
    }

    /**
     * Changes working directory of command
     *
     * @param string $dir
     *
     * @return $this
     */
    public function dir($dir)
    {
        $this->workingDirectory = $dir;
        return $this;
    }

    /**
     * Shortcut for setting isPrinted() and isMetadataPrinted() to false.
     *
     * @param bool $arg
     *
     * @return $this
     */
    public function silent($arg)
    {
        if (is_bool($arg)) {
            $this->isPrinted = !$arg;
            $this->isMetadataPrinted = !$arg;
        }
        return $this;
    }

    /**
     * Should command output be printed
     *
     * @param bool $arg
     *
     * @return $this
     *
     * @deprecated
     */
    public function printed($arg)
    {
        trigger_error('printed() is deprecated. Please use printOutput().', E_USER_DEPRECATED);
        return $this->printOutput($arg);
    }

    /**
     * Should command output be printed
     *
     * @param bool $arg
     *
     * @return $this
     */
    public function printOutput($arg)
    {
        if (is_bool($arg)) {
            $this->isPrinted = $arg;
        }
        return $this;
    }

    /**
     * Should command metadata be printed. I,e., command and timer.
     *
     * @param bool $arg
     *
     * @return $this
     */
    public function printMetadata($arg)
    {
        if (is_bool($arg)) {
            $this->isMetadataPrinted = $arg;
        }
        return $this;
    }

    /**
     * @param \Symfony\Component\Process\Process $process
     * @param callable $output_callback
     *
     * @return \Robo\ResultData
     */
    protected function execute($process, $output_callback = null)
    {
        $this->process = $process;

        if (!$output_callback) {
            $output_callback = function ($type, $buffer) {
                $progressWasVisible = $this->hideTaskProgress();
                $this->writeMessage($buffer);
                $this->showTaskProgress($progressWasVisible);
            };
        }

        $this->detectInteractive();

        if ($this->isMetadataPrinted) {
            $this->printAction();
        }
        $this->process->setTimeout($this->timeout);
        $this->process->setIdleTimeout($this->idleTimeout);
        if ($this->workingDirectory) {
            $this->process->setWorkingDirectory($this->workingDirectory);
        }
        if ($this->input) {
            $this->process->setInput($this->input);
        }

        if ($this->interactive && $this->isPrinted) {
            $this->process->setTty(true);
        }

        if (isset($this->env)) {
            $this->process->setEnv($this->env);
        }

        if (!$this->background && !$this->isPrinted) {
            $this->startTimer();
            $this->process->run();
            $this->stopTimer();
            $output = rtrim($this->process->getOutput());
            $result = new ResultData(
                $this->process->getExitCode(),
                $output,
                $this->getResultData()
            );
            $result->provideOutputdata();
            return $result;
        }

        if (!$this->background && $this->isPrinted) {
            $this->startTimer();
            $this->process->run($output_callback);
            $this->stopTimer();
            return new ResultData(
                $this->process->getExitCode(),
                $this->process->getOutput(),
                $this->getResultData()
            );
        }

        try {
            $this->process->start();
        } catch (\Exception $e) {
            return new ResultData(
                $this->process->getExitCode(),
                $e->getMessage(),
                $this->getResultData()
            );
        }
        return new ResultData($this->process->getExitCode());
    }

    protected function stop()
    {
        if ($this->background && isset($this->process) && $this->process->isRunning()) {
            $this->process->stop();
            $this->printTaskInfo(
                "Stopped {command}",
                ['command' => $this->getCommandDescription()]
            );
        }
    }

    /**
     * @param array $context
     */
    protected function printAction($context = [])
    {
        $command = $this->getCommandDescription();
        $formatted_command = $this->formatCommandDisplay($command);

        $dir = $this->workingDirectory ? " in {dir}" : "";
        $this->printTaskInfo("Running {command}$dir", [
                'command' => $formatted_command,
                'dir' => $this->workingDirectory
            ] + $context);
    }

    /**
     * @param string $command
     *
     * @return string
     */
    protected function formatCommandDisplay($command)
    {
        $formatted_command = str_replace("&&", "&&\n", $command);
        $formatted_command = str_replace("||", "||\n", $formatted_command);

        return $formatted_command;
    }

    /**
     * Gets the data array to be passed to Result().
     *
     * @return array
     *   The data array passed to Result().
     */
    protected function getResultData()
    {
        if ($this->isMetadataPrinted) {
            return ['time' => $this->getExecutionTime()];
        }

        return [];
    }
}
