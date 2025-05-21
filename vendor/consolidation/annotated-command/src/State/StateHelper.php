<?php
namespace Consolidation\AnnotatedCommand\State;

use Consolidation\AnnotatedCommand\Output\OutputAwareInterface;
use Consolidation\AnnotatedCommand\State\SavableState;
use Consolidation\AnnotatedCommand\State\State;
use Symfony\Component\Console\Input\InputAwareInterface;
use Symfony\Component\Console\Input\InputInterface;
use Symfony\Component\Console\Output\OutputInterface;

class StateHelper
{
    /**
     * Inject $input and $output into the command instance if it is set up to receive them.
     *
     * @param Callable|object $callback
     * @param OutputInterface $output
     * @return State
     */
    public static function injectIntoCallbackObject($callback, InputInterface $input, ?OutputInterface $output = null)
    {
        return static::inject(static::recoverCallbackObject($callback), $input, $output);
    }

    /**
     * Inject $input and $output into the command instance if it is set up to receive them.
     *
     * @param Callable|object $callback
     * @param OutputInterface $output
     * @return State
     */
    public static function inject($target, InputInterface $input, ?OutputInterface $output = null)
    {
        // Do not allow injection unless the target can save its state
        if (!$target || !($target instanceof SavableState)) {
            return new class implements State {
                public function restore()
                {
                }
            };
        }

        $state = $target->currentState();

        if ($target instanceof InputAwareInterface) {
            $target->setInput($input);
        }
        if (isset($output) && $target instanceof OutputAwareInterface) {
            $target->setOutput($output);
        }

        return $state;
    }

    /**
     * If the command callback is a method of an object, return the object.
     *
     * @param Callable|object $callback
     * @return object|bool
     */
    protected static function recoverCallbackObject($callback)
    {
        if (is_object($callback)) {
            return $callback;
        }

        if (!is_array($callback)) {
            return false;
        }

        if (!is_object($callback[0])) {
            return false;
        }

        return $callback[0];
    }
}
