<?php
namespace Consolidation\AnnotatedCommand;

use Consolidation\AnnotatedCommand\Hooks\Dispatchers\ReplaceCommandHookDispatcher;
use Psr\Log\LoggerAwareInterface;
use Psr\Log\LoggerAwareTrait;
use Symfony\Component\Console\Input\InputInterface;
use Symfony\Component\Console\Output\OutputInterface;
use Symfony\Component\Console\Output\ConsoleOutputInterface;

use Consolidation\OutputFormatters\FormatterManager;
use Consolidation\OutputFormatters\Options\FormatterOptions;
use Consolidation\AnnotatedCommand\Hooks\HookManager;
use Consolidation\AnnotatedCommand\Options\PrepareFormatter;

use Consolidation\AnnotatedCommand\Hooks\Dispatchers\InitializeHookDispatcher;
use Consolidation\AnnotatedCommand\Hooks\Dispatchers\OptionsHookDispatcher;
use Consolidation\AnnotatedCommand\Hooks\Dispatchers\InteractHookDispatcher;
use Consolidation\AnnotatedCommand\Hooks\Dispatchers\ValidateHookDispatcher;
use Consolidation\AnnotatedCommand\Hooks\Dispatchers\ProcessResultHookDispatcher;
use Consolidation\AnnotatedCommand\Hooks\Dispatchers\StatusDeterminerHookDispatcher;
use Consolidation\AnnotatedCommand\Hooks\Dispatchers\ExtracterHookDispatcher;

/**
 * Process a command, including hooks and other callbacks.
 * There should only be one command processor per application.
 * Provide your command processor to the AnnotatedCommandFactory
 * via AnnotatedCommandFactory::setCommandProcessor().
 */
class CommandProcessor implements LoggerAwareInterface
{
    use LoggerAwareTrait;

    /** @var HookManager */
    protected $hookManager;
    /** @var FormatterManager */
    protected $formatterManager;
    /** @var PrepareFormatterOptions[] */
    protected $prepareOptionsList = [];
    /** @var boolean */
    protected $passExceptions;
    /** @var ResultWriter */
    protected $resultWriter;
    /** @var ParameterInjection */
    protected $parameterInjection;

    public function __construct(HookManager $hookManager)
    {
        $this->hookManager = $hookManager;
    }

    /**
     * Return the hook manager
     * @return HookManager
     */
    public function hookManager()
    {
        return $this->hookManager;
    }

    public function resultWriter()
    {
        if (!$this->resultWriter) {
            $this->setResultWriter(new ResultWriter());
        }
        return $this->resultWriter;
    }

    public function setResultWriter($resultWriter)
    {
        $this->resultWriter = $resultWriter;
    }

    public function parameterInjection()
    {
        if (!$this->parameterInjection) {
            $this->setParameterInjection(new ParameterInjection());
        }
        return $this->parameterInjection;
    }

    public function setParameterInjection($parameterInjection)
    {
        $this->parameterInjection = $parameterInjection;
    }

    public function addPrepareFormatter(PrepareFormatter $preparer)
    {
        $this->prepareOptionsList[] = $preparer;
    }

    public function setFormatterManager(FormatterManager $formatterManager)
    {
        $this->formatterManager = $formatterManager;
        $this->resultWriter()->setFormatterManager($formatterManager);
        return $this;
    }

    public function setDisplayErrorFunction(callable $fn)
    {
        $this->resultWriter()->setDisplayErrorFunction($fn);
    }

    /**
     * Set a mode to make the annotated command library re-throw
     * any exception that it catches while processing a command.
     *
     * The default behavior in the current (2.x) branch is to catch
     * the exception and replace it with a CommandError object that
     * may be processed by the normal output processing passthrough.
     *
     * In the 3.x branch, exceptions will never be caught; they will
     * be passed through, as if setPassExceptions(true) were called.
     * This is the recommended behavior.
     */
    public function setPassExceptions($passExceptions)
    {
        $this->passExceptions = $passExceptions;
        return $this;
    }

    public function commandErrorForException(\Exception $e)
    {
        if ($this->passExceptions) {
            throw $e;
        }
        return new CommandError($e->getMessage(), $e->getCode());
    }

    /**
     * Return the formatter manager
     * @return FormatterManager
     */
    public function formatterManager()
    {
        return $this->formatterManager;
    }

    public function initializeHook(
        InputInterface $input,
        $names,
        AnnotationData $annotationData
    ) {
        $initializeDispatcher = new InitializeHookDispatcher($this->hookManager(), $names);
        return $initializeDispatcher->initialize($input, $annotationData);
    }

    public function optionsHook(
        AnnotatedCommand $command,
        $names,
        AnnotationData $annotationData
    ) {
        $optionsDispatcher = new OptionsHookDispatcher($this->hookManager(), $names);
        $optionsDispatcher->getOptions($command, $annotationData);
    }

    public function interact(
        InputInterface $input,
        OutputInterface $output,
        $names,
        AnnotationData $annotationData
    ) {
        $interactDispatcher = new InteractHookDispatcher($this->hookManager(), $names);
        return $interactDispatcher->interact($input, $output, $annotationData);
    }

    public function process(
        OutputInterface $output,
        $names,
        $commandCallback,
        CommandData $commandData
    ) {
        $result = [];
        try {
            $result = $this->validateRunAndAlter(
                $names,
                $commandCallback,
                $commandData
            );
            return $this->handleResults($output, $names, $result, $commandData);
        } catch (\Exception $e) {
            $result = $this->commandErrorForException($e);
            return $this->handleResults($output, $names, $result, $commandData);
        }
    }

    public function validateRunAndAlter(
        $names,
        $commandCallback,
        CommandData $commandData
    ) {
        // Validators return any object to signal a validation error;
        // if the return an array, it replaces the arguments.
        $validateDispatcher = new ValidateHookDispatcher($this->hookManager(), $names);
        $validated = $validateDispatcher->validate($commandData);
        if (is_object($validated)) {
            return $validated;
        }

        // Once we have validated the options, update the formatter options.
        $this->updateFormatterOptions($commandData);

        $replaceDispatcher = new ReplaceCommandHookDispatcher($this->hookManager(), $names);
        if ($this->logger) {
            $replaceDispatcher->setLogger($this->logger);
        }
        if ($replaceDispatcher->hasReplaceCommandHook()) {
            $commandCallback = $replaceDispatcher->getReplacementCommand($commandData);
        }

        // Run the command, alter the results, and then handle output and status
        $result = $this->runCommandCallback($commandCallback, $commandData);
        return $this->processResults($names, $result, $commandData);
    }

    public function processResults($names, $result, CommandData $commandData)
    {
        $processDispatcher = new ProcessResultHookDispatcher($this->hookManager(), $names);
        return $processDispatcher->process($result, $commandData);
    }

    /**
     * Update the FormatterOptions object with validated command options.
     * Also runs the perparers.
     *
     * @param CommandData $commandData
     * @return FormatterOptions
     */
    protected function updateFormatterOptions($commandData)
    {
        // Update formatter options, in case anything was changed in a hook
        $formatterOptions = $commandData->formatterOptions();
        $formatterOptions->setConfigurationData($commandData->annotationData()->getArrayCopy());
        $formatterOptions->setOptions($commandData->input()->getOptions());

        // Run any prepare function, e.g. to prepare a format object to run
        foreach ($this->prepareOptionsList as $preparer) {
            $preparer->prepare($commandData, $formatterOptions);
        }

        // Call 'overrideOptions' in advance of calling the command, to
        // allow the format options to be as close as possible to their
        // final state. 'overrideOptions' will be called again right before
        // rendering, and may be altered, e.g. if the override depends on
        // the data in the command output.
        $format = $formatterOptions->getFormat();
        if ($format) {
            $placeholderStructuredOutput = [];
            try {
                $formatter = $this->formatterManager->getFormatter($format);
                $this->formatterManager->overrideOptions($formatter, $placeholderStructuredOutput, $formatterOptions);
            } catch (\Exception $e) {
            }
        }
    }

    /**
     * Handle the result output and status code calculation.
     */
    public function handleResults(OutputInterface $output, $names, $result, CommandData $commandData)
    {
        $statusCodeDispatcher = new StatusDeterminerHookDispatcher($this->hookManager(), $names);
        $extractDispatcher = new ExtracterHookDispatcher($this->hookManager(), $names);

        return $this->resultWriter()->handle($output, $result, $commandData, $statusCodeDispatcher, $extractDispatcher);
    }

    /**
     * Run the main command callback
     */
    protected function runCommandCallback($commandCallback, CommandData $commandData)
    {
        $result = false;
        try {
            $args = $this->parameterInjection()->args($commandData);
            $result = call_user_func_array($commandCallback, array_values($args));
        } catch (\Exception $e) {
            $result = $this->commandErrorForException($e);
        }
        return $result;
    }

    public function injectIntoCommandData($commandData, $injectedClasses)
    {
        $this->parameterInjection()->injectIntoCommandData($commandData, $injectedClasses);
    }
}
