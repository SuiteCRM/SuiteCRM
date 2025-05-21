<?php

declare(strict_types=1);

namespace League\Container\Definition;

use League\Container\Argument\{
    ArgumentResolverInterface,
    ArgumentResolverTrait,
    ArgumentInterface,
    LiteralArgumentInterface
};
use League\Container\ContainerAwareTrait;
use League\Container\Exception\ContainerException;
use League\Container\Exception\NotFoundException;
use Psr\Container\ContainerInterface;
use ReflectionClass;

class Definition implements ArgumentResolverInterface, DefinitionInterface
{
    use ArgumentResolverTrait;
    use ContainerAwareTrait;

    /**
     * @var string
     */
    protected $alias;

    /**
     * @var mixed
     */
    protected $concrete;

    /**
     * @var boolean
     */
    protected $shared = false;

    /**
     * @var array
     */
    protected $tags = [];

    /**
     * @var array
     */
    protected $arguments = [];

    /**
     * @var array
     */
    protected $methods = [];

    /**
     * @var mixed
     */
    protected $resolved;

    /**
     * @var array
     */
    protected $recursiveCheck = [];

    /**
     * @param string     $id
     * @param mixed|null $concrete
     */
    public function __construct(string $id, $concrete = null)
    {
        $id = static::normaliseAlias($id);

        $concrete = $concrete ?? $id;
        $this->alias    = $id;
        $this->concrete = $concrete;
    }

    public function addTag(string $tag): DefinitionInterface
    {
        $this->tags[$tag] = true;
        return $this;
    }

    public function hasTag(string $tag): bool
    {
        return isset($this->tags[$tag]);
    }

    public function setAlias(string $id): DefinitionInterface
    {
        $id = static::normaliseAlias($id);

        $this->alias = $id;
        return $this;
    }

    public function getAlias(): string
    {
        return $this->alias;
    }

    public function setShared(bool $shared = true): DefinitionInterface
    {
        $this->shared = $shared;
        return $this;
    }

    public function isShared(): bool
    {
        return $this->shared;
    }

    public function getConcrete()
    {
        return $this->concrete;
    }

    public function setConcrete($concrete): DefinitionInterface
    {
        $this->concrete = $concrete;
        $this->resolved = null;
        return $this;
    }

    public function addArgument($arg): DefinitionInterface
    {
        $this->arguments[] = $arg;
        return $this;
    }

    public function addArguments(array $args): DefinitionInterface
    {
        foreach ($args as $arg) {
            $this->addArgument($arg);
        }

        return $this;
    }

    public function addMethodCall(string $method, array $args = []): DefinitionInterface
    {
        $this->methods[] = [
            'method'    => $method,
            'arguments' => $args
        ];

        return $this;
    }

    public function addMethodCalls(array $methods = []): DefinitionInterface
    {
        foreach ($methods as $method => $args) {
            $this->addMethodCall($method, $args);
        }

        return $this;
    }

    public function resolve()
    {
        if (null !== $this->resolved && $this->isShared()) {
            return $this->resolved;
        }

        return $this->resolveNew();
    }

    public function resolveNew()
    {
        $concrete = $this->concrete;

        if (is_callable($concrete)) {
            $concrete = $this->resolveCallable($concrete);
        }

        if ($concrete instanceof LiteralArgumentInterface) {
            $this->resolved = $concrete->getValue();
            return $concrete->getValue();
        }

        if ($concrete instanceof ArgumentInterface) {
            $concrete = $concrete->getValue();
        }

        if (is_string($concrete) && class_exists($concrete)) {
            $concrete = $this->resolveClass($concrete);
        }

        if (is_object($concrete)) {
            $concrete = $this->invokeMethods($concrete);
        }

        try {
            $container = $this->getContainer();
        } catch (ContainerException $e) {
            $container = null;
        }

        // stop recursive resolving
        if (is_string($concrete) && in_array($concrete, $this->recursiveCheck)) {
            $this->resolved = $concrete;
            return $concrete;
        }

        // if we still have a string, try to pull it from the container
        // this allows for `alias -> alias -> ... -> concrete
        if (is_string($concrete) && $container instanceof ContainerInterface && $container->has($concrete)) {
            $this->recursiveCheck[] = $concrete;
            $concrete = $container->get($concrete);
        }

        $this->resolved = $concrete;
        return $concrete;
    }

    /**
     * @param callable $concrete
     * @return mixed
     */
    protected function resolveCallable(callable $concrete)
    {
        $resolved = $this->resolveArguments($this->arguments);
        return call_user_func_array($concrete, $resolved);
    }

    protected function resolveClass(string $concrete): object
    {
        $resolved   = $this->resolveArguments($this->arguments);
        $reflection = new ReflectionClass($concrete);
        return $reflection->newInstanceArgs($resolved);
    }

    protected function invokeMethods(object $instance): object
    {
        foreach ($this->methods as $method) {
            $args = $this->resolveArguments($method['arguments']);
            $callable = [$instance, $method['method']];
            call_user_func_array($callable, $args);
        }

        return $instance;
    }

    public static function normaliseAlias(string $alias): string
    {
        if (strpos($alias, '\\') === 0) {
            return substr($alias, 1);
        }

        return $alias;
    }
}
