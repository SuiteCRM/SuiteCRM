<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser;

use Pimple\Container as PimpleContainer;
use Pimple\Exception\UnknownIdentifierException;
use ReflectionClass;
use ReflectionParameter;

/**
 * Automatically configures classes and dependencies.
 *
 * Sets up an automatic registration for classes when requested through
 * Pimple by looking up the class's constructor and arguments.
 *
 * @author Zaahid Bateson
 */
class Container extends PimpleContainer
{
    /**
     * Looks up the type of the passed ReflectionParameter and returns it as a
     * fully qualified class name as expected by the class's auto registration.
     *
     * Null is returned for built-in types.
     *
     */
    private function getParameterClass(ReflectionParameter $param) : ?string
    {
        if (\method_exists($param, 'getType')) {
            $type = $param->getType();
            if ($type && !$type->isBuiltin()) {
                return \method_exists($type, 'getName') ? $type->getName() : (string) $type;
            }
        } elseif ($param->getClass() !== null) {
            return $param->getClass()->getName();
        }
        return null;
    }

    /**
     * Returns a factory function for the passed class.
     *
     * The returned factory method looks up arguments and uses pimple to get an
     * instance of those types to pass them during construction.
     */
    public function autoRegister($class) : ?string
    {
        $fn = function($c) use ($class) {
            $ref = new ReflectionClass($class);
            $cargs = ($ref->getConstructor() !== null) ? $ref->getConstructor()->getParameters() : [];
            $ap = [];
            foreach ($cargs as $arg) {
                $name = $arg->getName();
                $argClass = $this->getParameterClass($arg);
                if (!empty($c[$name])) {
                    $ap[] = $c[$name];
                } elseif ($argClass !== null && !empty($c[$argClass])) {
                    $ap[] = $c[$argClass];
                } else {
                    $ap[] = 0;
                }
            }
            $ret = $ref->newInstanceArgs($ap);
            return $ret;
        };
        $this[$class] = $fn;
        return null;
    }

    /**
     * Overridden to see if the class can be auto-registered and return true if
     * it can.
     */
    public function offsetExists($id) : bool
    {
        $exists = parent::offsetExists($id);
        if (!$exists && \class_exists($id)) {
            $this->autoRegister($id);
            return true;
        }
        return $exists;
    }

    /**
     * Overridden to see if the class can be auto-registered and return an
     * instance if it can.
     *
     * @param string | int $id
     *
     * @return mixed The value of the parameter or an object
     *
     * @throws UnknownIdentifierException
     */
    #[\ReturnTypeWillChange]
    public function offsetGet($id)
    {
        try {
            return parent::offsetGet($id);
        } catch (UnknownIdentifierException $e) {
            if (\class_exists($id)) {
                $this->autoRegister($id);
                return parent::offsetGet($id);
            }
            throw $e;
        }
    }

    /**
     * Overridden to see if the class can be auto-registered first before
     * calling Pimple\Container::extend
     *
     * @param string $id
     * @param callable $callable
     * @return callable the wrapped $callable
     */
    public function extend($id, $callable)
    {
        $this->offsetExists($id);
        return parent::extend($id, $callable);
    }
}
