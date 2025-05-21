<?php
namespace Consolidation\AnnotatedCommand;

/**
 * Prepare parameter list for execurion. Handle injection of any
 * special values (e.g. $input and $output) into the parameter list.
 */
class ParameterInjection implements ParameterInjector
{
    protected $injectors = [];

    public function __construct()
    {
        $this->register('Symfony\Component\Console\Input\InputInterface', $this);
        $this->register('Symfony\Component\Console\Output\OutputInterface', $this);
        $this->register('Consolidation\AnnotatedCommand\AnnotationData', $this);
        $this->register('Consolidation\OutputFormatters\Options\FormatterOptions', $this);
    }

    public function register($interfaceName, ParameterInjector $injector)
    {
        $this->injectors[$interfaceName] = $injector;
    }

    public function args($commandData)
    {
        return array_merge(
            $commandData->injectedInstances(),
            $commandData->getArgsAndOptions()
        );
    }

    public function injectIntoCommandData($commandData, $injectedClasses)
    {
        foreach ($injectedClasses as $injectedClass) {
            $injectedInstance = $this->getInstanceToInject($commandData, $injectedClass);
            $commandData->injectInstance($injectedInstance);
        }
    }

    protected function getInstanceToInject(CommandData $commandData, $interfaceName)
    {
        if (!isset($this->injectors[$interfaceName])) {
            return null;
        }

        return $this->injectors[$interfaceName]->get($commandData, $interfaceName);
    }

    public function get(CommandData $commandData, $interfaceName)
    {
        switch ($interfaceName) {
            case 'Symfony\Component\Console\Input\InputInterface':
                return $commandData->input();
            case 'Symfony\Component\Console\Output\OutputInterface':
                return $commandData->output();
            case 'Consolidation\AnnotatedCommand\AnnotationData':
                return $commandData->annotationData();
            case 'Consolidation\OutputFormatters\Options\FormatterOptions':
                return $commandData->formatterOptions();
        }

        return null;
    }
}
