<?php
namespace Consolidation\AnnotatedCommand;

use Consolidation\AnnotatedCommand\Help\HelpDocumentAlter;
use Consolidation\AnnotatedCommand\Help\HelpDocumentBuilder;
use Consolidation\AnnotatedCommand\Hooks\HookManager;
use Consolidation\AnnotatedCommand\Output\OutputAwareInterface;
use Consolidation\AnnotatedCommand\Parser\CommandInfo;
use Consolidation\AnnotatedCommand\State\State;
use Consolidation\AnnotatedCommand\State\StateHelper;
use Symfony\Component\Console\Command\Command;
use Symfony\Component\Console\Completion\CompletionInput;
use Symfony\Component\Console\Completion\CompletionSuggestions;
use Symfony\Component\Console\Input\InputArgument;
use Symfony\Component\Console\Input\InputAwareInterface;
use Symfony\Component\Console\Input\InputInterface;
use Symfony\Component\Console\Input\InputOption;
use Symfony\Component\Console\Output\OutputInterface;
use Consolidation\OutputFormatters\Options\FormatterOptions;

/**
 * AnnotatedCommands are created automatically by the
 * AnnotatedCommandFactory.  Each command method in a
 * command file will produce one AnnotatedCommand.  These
 * are then added to your Symfony Console Application object;
 * nothing else is needed.
 *
 * Optionally, though, you may extend AnnotatedCommand directly
 * to make a single command.  The usage pattern is the same
 * as for any other Symfony Console command, except that you may
 * omit the 'Confiure' method, and instead place your annotations
 * on the execute() method.
 *
 * @package Consolidation\AnnotatedCommand
 */
class AnnotatedCommand extends Command implements HelpDocumentAlter
{
    protected $commandCallback;
    protected $completionCallback;
    protected $commandProcessor;
    protected $annotationData;
    protected $examples = [];
    protected $topics = [];
    protected $returnType;
    protected $injectedClasses = [];
    protected $parameterMap = [];

    public function __construct($name = null)
    {
        $commandInfo = false;

        // If this is a subclass of AnnotatedCommand, check to see
        // if the 'execute' method is annotated.  We could do this
        // unconditionally; it is a performance optimization to skip
        // checking the annotations if $this is an instance of
        // AnnotatedCommand.  Alternately, we break out a new subclass.
        // The command factory instantiates the subclass.
        if (get_class($this) != 'Consolidation\AnnotatedCommand\AnnotatedCommand') {
            $commandInfo = CommandInfo::create($this, 'execute');
            if (!isset($name)) {
                $name = $commandInfo->getName();
            }
        }
        parent::__construct($name);
        if ($commandInfo && $commandInfo->hasAnnotation('command')) {
            $this->setCommandInfo($commandInfo);
            $this->setCommandOptions($commandInfo);
        }
    }

    public function setCommandCallback($commandCallback)
    {
        $this->commandCallback = $commandCallback;
        return $this;
    }

    public function setCompletionCallback($completionCallback)
    {
        $this->completionCallback = $completionCallback;
        return $this;
    }

    public function setCommandProcessor($commandProcessor)
    {
        $this->commandProcessor = $commandProcessor;
        return $this;
    }

    public function commandProcessor()
    {
        // If someone is using an AnnotatedCommand, and is NOT getting
        // it from an AnnotatedCommandFactory OR not correctly injecting
        // a command processor via setCommandProcessor() (ideally via the
        // DI container), then we'll just give each annotated command its
        // own command processor. This is not ideal; preferably, there would
        // only be one instance of the command processor in the application.
        if (!isset($this->commandProcessor)) {
            $this->commandProcessor = new CommandProcessor(new HookManager());
        }
        return $this->commandProcessor;
    }

    public function getReturnType()
    {
        return $this->returnType;
    }

    public function setReturnType($returnType)
    {
        $this->returnType = $returnType;
        return $this;
    }

    public function getAnnotationData()
    {
        return $this->annotationData;
    }

    public function setAnnotationData($annotationData)
    {
        $this->annotationData = $annotationData;
        return $this;
    }

    public function getTopics()
    {
        return $this->topics;
    }

    public function setTopics($topics)
    {
        $this->topics = $topics;
        return $this;
    }

    public function setCommandInfo($commandInfo)
    {
        $this->setDescription($commandInfo->getDescription() ?: '');
        $this->setHelp($commandInfo->getHelp() ?: '');
        $this->setAliases($commandInfo->getAliases());
        $this->setAnnotationData($commandInfo->getAnnotations());
        $this->setTopics($commandInfo->getTopics());
        foreach ($commandInfo->getExampleUsages() as $usage => $description) {
            $this->addUsageOrExample($usage, $description);
        }
        $this->setCommandArguments($commandInfo);
        $this->setReturnType($commandInfo->getReturnType());
        // Hidden commands available since Symfony 3.2
        // http://symfony.com/doc/current/console/hide_commands.html
        if (method_exists($this, 'setHidden')) {
            $this->setHidden($commandInfo->getHidden());
        }
        $this->parameterMap = $commandInfo->getParameterMap();
        return $this;
    }

    public function getExampleUsages()
    {
        return $this->examples;
    }

    public function addUsageOrExample($usage, $description)
    {
        $this->addUsage($usage);
        if (!empty($description)) {
            $this->examples[$usage] = $description;
        }
    }

    public function getCompletionCallback()
    {
        return $this->completionCallback;
    }

    public function helpAlter(\DomDocument $originalDom)
    {
        return HelpDocumentBuilder::alter($originalDom, $this);
    }

    protected function setCommandArguments($commandInfo)
    {
        $this->injectedClasses = $commandInfo->getInjectedClasses();
        $this->setCommandArgumentsFromParameters($commandInfo);
        return $this;
    }

    protected function setCommandArgumentsFromParameters($commandInfo)
    {
        $args = $commandInfo->arguments()->getValues();
        foreach ($args as $name => $defaultValue) {
            $description = $commandInfo->arguments()->getDescription($name);
            $hasDefault = $commandInfo->arguments()->hasDefault($name);
            $parameterMode = $this->getCommandArgumentMode($hasDefault, $defaultValue);
            $suggestedValues = $commandInfo->arguments()->getSuggestedValues($name);
            $this->addArgument($name, $parameterMode, $description, $defaultValue, $suggestedValues);
        }
        return $this;
    }

    protected function getCommandArgumentMode($hasDefault, $defaultValue)
    {
        if (!$hasDefault) {
            return InputArgument::REQUIRED;
        }
        if (is_array($defaultValue)) {
            return InputArgument::IS_ARRAY;
        }
        return InputArgument::OPTIONAL;
    }

    public function setCommandOptions($commandInfo, $automaticOptions = [])
    {
        $inputOptions = $commandInfo->inputOptions();

        $this->addOptions($inputOptions + $automaticOptions, $automaticOptions);
        return $this;
    }

    public function addOptions($inputOptions, $automaticOptions = [])
    {
        foreach ($inputOptions as $name => $inputOption) {
            $description = $inputOption->getDescription();

            if (empty($description) && isset($automaticOptions[$name])) {
                // Unfortunately, Console forces us too construct a new InputOption to set a description.
                $description = $automaticOptions[$name]->getDescription();
                $this->addInputOption($inputOption, $description);
            } else {
                if ($native = $this->getNativeDefinition()) {
                    $native->addOption($inputOption);
                }
                $this->getDefinition()->addOption($inputOption);
            }
        }
    }

    private function addInputOption($inputOption, $description = null)
    {
        $default = $inputOption->getDefault();
        // Recover the 'mode' value, because Symfony is stubborn
        $mode = 0;
        if ($inputOption->isValueRequired()) {
            $mode |= InputOption::VALUE_REQUIRED;
        }
        if ($inputOption->isValueOptional()) {
            $mode |= InputOption::VALUE_OPTIONAL;
        }
        if ($inputOption->isArray()) {
            $mode |= InputOption::VALUE_IS_ARRAY;
        }
        if (!$mode) {
            $mode = InputOption::VALUE_NONE;
            $default = null;
        }

        $suggestedValues = [];
        // Symfony 6.1+ feature https://symfony.com/blog/new-in-symfony-6-1-improved-console-autocompletion#completion-values-in-input-definitions
        if (property_exists($inputOption, 'suggestedValues')) {
            // Alas, Symfony provides no accessor.
            $class = new \ReflectionClass($inputOption);
            $property = $class->getProperty('suggestedValues');
            $property->setAccessible(true);
            $suggestedValues = $property->getValue($inputOption);
        }
        $this->addOption(
            $inputOption->getName(),
            $inputOption->getShortcut(),
            $mode,
            $description ?? $inputOption->getDescription(),
            $default,
            $suggestedValues
        );
    }

    /**
     * @deprecated since 4.5.0
     */
    protected static function inputOptionSetDescription($inputOption, $description)
    {
        @\trigger_error(
            'Since consolidation/annotated-command 4.5: ' .
            'AnnotatedCommand::inputOptionSetDescription method is deprecated and will be removed in 5.0',
            \E_USER_DEPRECATED
        );
        // Recover the 'mode' value, because Symfony is stubborn
        $mode = 0;
        if ($inputOption->isValueRequired()) {
            $mode |= InputOption::VALUE_REQUIRED;
        }
        if ($inputOption->isValueOptional()) {
            $mode |= InputOption::VALUE_OPTIONAL;
        }
        if ($inputOption->isArray()) {
            $mode |= InputOption::VALUE_IS_ARRAY;
        }
        if (!$mode) {
            $mode = InputOption::VALUE_NONE;
        }

        $inputOption = new InputOption(
            $inputOption->getName(),
            $inputOption->getShortcut(),
            $mode,
            $description,
            $inputOption->getDefault()
        );
        return $inputOption;
    }

    /**
     * Returns all of the hook names that may be called for this command.
     *
     * @return array
     */
    public function getNames()
    {
        return HookManager::getNames($this, $this->commandCallback);
    }

    /**
     * Add any options to this command that are defined by hook implementations
     */
    public function optionsHook()
    {
        $this->commandProcessor()->optionsHook(
            $this,
            $this->getNames(),
            $this->annotationData
        );
    }

    /**
     * Route a completion request to the specified Callable if available.
     */
    public function complete(CompletionInput $input, CompletionSuggestions $suggestions): void
    {
        parent::complete($input, $suggestions);
        if (is_callable($this->completionCallback)) {
            call_user_func($this->completionCallback, $input, $suggestions);
        }
    }

    public function optionsHookForHookAnnotations($commandInfoList)
    {
        foreach ($commandInfoList as $commandInfo) {
            $inputOptions = $commandInfo->inputOptions();
            $this->addOptions($inputOptions);
            foreach ($commandInfo->getExampleUsages() as $usage => $description) {
                if (!in_array($usage, $this->getUsages())) {
                    $this->addUsageOrExample($usage, $description);
                }
            }
        }
    }

    /**
     * {@inheritdoc}
     */
    protected function interact(InputInterface $input, OutputInterface $output)
    {
        $state = $this->injectIntoCommandfileInstance($input, $output);
        $this->commandProcessor()->interact(
            $input,
            $output,
            $this->getNames(),
            $this->annotationData
        );
        $state->restore();
    }

    protected function initialize(InputInterface $input, OutputInterface $output)
    {
        $state = $this->injectIntoCommandfileInstance($input, $output);
        // Allow the hook manager a chance to provide configuration values,
        // if there are any registered hooks to do that.
        $this->commandProcessor()->initializeHook($input, $this->getNames(), $this->annotationData);
        $state->restore();
    }

    /**
     * {@inheritdoc}
     */
    protected function execute(InputInterface $input, OutputInterface $output): int
    {
        $state = $this->injectIntoCommandfileInstance($input, $output);
        // Validate, run, process, alter, handle results.
        $result = $this->commandProcessor()->process(
            $output,
            $this->getNames(),
            $this->commandCallback,
            $this->createCommandData($input, $output)
        );
        $state->restore();
        return $result;
    }

    /**
     * This function is available for use by a class that may
     * wish to extend this class rather than use annotations to
     * define commands. Using this technique does allow for the
     * use of annotations to define hooks.
     */
    public function processResults(InputInterface $input, OutputInterface $output, $results)
    {
        $state = $this->injectIntoCommandfileInstance($input, $output);
        $commandData = $this->createCommandData($input, $output);
        $commandProcessor = $this->commandProcessor();
        $names = $this->getNames();
        $results = $commandProcessor->processResults(
            $names,
            $results,
            $commandData
        );
        $status = $commandProcessor->handleResults(
            $output,
            $names,
            $results,
            $commandData
        );
        $state->restore();
        return $status;
    }

    protected function createCommandData(InputInterface $input, OutputInterface $output)
    {
        $commandData = new CommandData(
            $this->annotationData,
            $input,
            $output,
            $this->parameterMap
        );

        $formatterOptions = new FormatterOptions($commandData->annotationData()->getArrayCopy(), $commandData->input()->getOptions());
        $commandData->setFormatterOptions($formatterOptions);

        // Fetch any classes (e.g. InputInterface / OutputInterface) that
        // this command's callback wants passed as a parameter and inject
        // it into the command data.
        $this->commandProcessor()->injectIntoCommandData($commandData, $this->injectedClasses);

        // Allow the commandData to cache the list of options with
        // special default values ('null' and 'true'), as these will
        // need special handling. @see CommandData::options().
        $commandData->cacheSpecialDefaults($this->getDefinition());

        return $commandData;
    }

    /**
     * Inject $input and $output into the command instance if it is set up to receive them.
     *
     * @param callable $commandCallback
     * @param CommandData $commandData
     * @return State
     */
    public function injectIntoCommandfileInstance(InputInterface $input, OutputInterface $output)
    {
        return StateHelper::injectIntoCallbackObject($this->commandCallback, $input, $output);
    }
}
