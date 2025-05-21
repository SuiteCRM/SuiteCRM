<?php
namespace Consolidation\AnnotatedCommand;

use Consolidation\OutputFormatters\Options\FormatterOptions;
use Symfony\Component\Console\Input\ArgvInput;
use Symfony\Component\Console\Input\InputInterface;
use Symfony\Component\Console\Output\OutputInterface;

class CommandData
{
    /** var AnnotationData */
    protected $annotationData;
    /** var InputInterface */
    protected $input;
    /** var OutputInterface */
    protected $output;
    /** var boolean */
    protected $includeOptionsInArgs;
    /** var array */
    protected $specialDefaults = [];
    /** @var string[] */
    protected $injectedInstances = [];
    /** @var FormatterOptions */
    protected $formatterOptions;
    /** @var bool[] */
    protected $parameterMap;

    public function __construct(
        AnnotationData $annotationData,
        InputInterface $input,
        OutputInterface $output,
        $parameterMap = []
    ) {
        $this->annotationData = $annotationData;
        $this->input = $input;
        $this->output = $output;
        $this->includeOptionsInArgs = true;
        $this->parameterMap = $parameterMap;
    }

    /**
     * For internal use only; inject an instance to be passed back
     * to the command callback as a parameter.
     */
    public function injectInstance($injectedInstance)
    {
        array_unshift($this->injectedInstances, $injectedInstance);
        return $this;
    }

    /**
     * Provide a reference to the instances that will be added to the
     * beginning of the parameter list when the command callback is invoked.
     */
    public function injectedInstances()
    {
        return $this->injectedInstances;
    }

    /**
     * For backwards-compatibility mode only: disable addition of
     * options on the end of the arguments list.
     */
    public function setIncludeOptionsInArgs($includeOptionsInArgs)
    {
        $this->includeOptionsInArgs = $includeOptionsInArgs;
        return $this;
    }

    public function annotationData()
    {
        return $this->annotationData;
    }

    public function formatterOptions()
    {
        return $this->formatterOptions;
    }

    public function setFormatterOptions($formatterOptions)
    {
        $this->formatterOptions = $formatterOptions;
    }

    public function input()
    {
        return $this->input;
    }

    public function output()
    {
        return $this->output;
    }

    public function arguments()
    {
        return $this->input->getArguments();
    }

    public function options()
    {
        // We cannot tell the difference between '--foo' (an option without
        // a value) and the absence of '--foo' when the option has an optional
        // value, and the current value of the option is 'null' using only
        // the public methods of InputInterface. We'll try to figure out
        // which is which by other means here.
        $options = $this->getAdjustedOptions();

        // Make two conversions here:
        // --foo=0 wil convert $value from '0' to 'false' for binary options.
        // --foo with $value of 'true' will be forced to 'false' if --no-foo exists.
        foreach ($options as $option => $value) {
            if ($this->shouldConvertOptionToFalse($options, $option, $value)) {
                $options[$option] = false;
            }
        }

        return $options;
    }

    /**
     * Use 'hasParameterOption()' to attempt to disambiguate option states.
     */
    protected function getAdjustedOptions()
    {
        $options = $this->input->getOptions();

        // If Input isn't an ArgvInput, then return the options as-is.
        if (!$this->input instanceof ArgvInput) {
            return $options;
        }

        // If we have an ArgvInput, then we can determine if options
        // are missing from the command line. If the option value is
        // missing from $input, then we will keep the value `null`.
        // If it is present, but has no explicit value, then change it its
        // value to `true`.
        foreach ($options as $option => $value) {
            if (($value === null) && ($this->input->hasParameterOption("--$option"))) {
                $options[$option] = true;
            }
        }

        return $options;
    }

    protected function shouldConvertOptionToFalse($options, $option, $value)
    {
        // If the value is 'true' (e.g. the option is '--foo'), then convert
        // it to false if there is also an option '--no-foo'. n.b. if the
        // commandline has '--foo=bar' then $value will not be 'true', and
        // --no-foo will be ignored.
        if ($value === true) {
            // Check if the --no-* option exists. Note that none of the other
            // alteration apply in the $value == true case, so we can exit early here.
            $negation_key = 'no-' . $option;
            return array_key_exists($negation_key, $options) && $options[$negation_key];
        }

        // If the option is '--foo=0', convert the '0' to 'false' when appropriate.
        if ($value !== '0') {
            return false;
        }

        // The '--foo=0' convertion is only applicable when the default value
        // is not in the special defaults list. i.e. you get a literal '0'
        // when your default is a string.
        return in_array($option, $this->specialDefaults);
    }

    public function cacheSpecialDefaults($definition)
    {
        foreach ($definition->getOptions() as $option => $inputOption) {
            $defaultValue = $inputOption->getDefault();
            if (($defaultValue === null) || ($defaultValue === true)) {
                $this->specialDefaults[] = $option;
            }
        }
    }

    public function getArgsWithoutAppName()
    {
        $args = $this->arguments();

        // When called via the Application, the first argument
        // will be the command name. The Application alters the
        // input definition to match, adding a 'command' argument
        // to the beginning.
        if ($this->input->hasArgument('command')) {
            array_shift($args);
        }

        return $args;
    }

    public function getArgsAndOptions()
    {
        // Get passthrough args, and add the options on the end.
        $args = $this->getArgsWithoutAppName();

        // If this command has a mix of named arguments and options in its
        // parameter list, then use the parameter map to insert the options
        // into the correct spot in the parameters list.
        if (!empty($this->parameterMap)) {
            $mappedArgs = [];
            foreach ($this->parameterMap as $name => $mappedName) {
                if ($mappedName) {
                    $mappedArgs[$name] = $this->input->getOption($mappedName);
                } else {
                    $mappedArgs[$name] = array_shift($args);
                }
            }
            $args = $mappedArgs;
        }

        if ($this->includeOptionsInArgs) {
            $args['options'] = $this->options();
        }

        return $args;
    }
}
