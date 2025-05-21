<?php

declare(strict_types=1);

namespace League\Container;

use League\Container\Definition\{DefinitionAggregate, DefinitionInterface, DefinitionAggregateInterface};
use League\Container\Exception\{NotFoundException, ContainerException};
use League\Container\Inflector\{InflectorAggregate, InflectorInterface, InflectorAggregateInterface};
use League\Container\ServiceProvider\{ServiceProviderAggregate,
    ServiceProviderAggregateInterface,
    ServiceProviderInterface};
use Psr\Container\ContainerInterface;

class Container implements DefinitionContainerInterface
{
    /**
     * @var boolean
     */
    protected $defaultToShared = false;

    /**
     * @var DefinitionAggregateInterface
     */
    protected $definitions;

    /**
     * @var ServiceProviderAggregateInterface
     */
    protected $providers;

    /**
     * @var InflectorAggregateInterface
     */
    protected $inflectors;

    /**
     * @var ContainerInterface[]
     */
    protected $delegates = [];

    public function __construct(
        ?DefinitionAggregateInterface $definitions = null,
        ?ServiceProviderAggregateInterface $providers = null,
        ?InflectorAggregateInterface $inflectors = null
    ) {
        $this->definitions = $definitions ?? new DefinitionAggregate();
        $this->providers   = $providers   ?? new ServiceProviderAggregate();
        $this->inflectors  = $inflectors  ?? new InflectorAggregate();

        if ($this->definitions instanceof ContainerAwareInterface) {
            $this->definitions->setContainer($this);
        }

        if ($this->providers instanceof ContainerAwareInterface) {
            $this->providers->setContainer($this);
        }

        if ($this->inflectors instanceof ContainerAwareInterface) {
            $this->inflectors->setContainer($this);
        }
    }

    public function add(string $id, $concrete = null): DefinitionInterface
    {
        $concrete = $concrete ?? $id;

        if (true === $this->defaultToShared) {
            return $this->addShared($id, $concrete);
        }

        return $this->definitions->add($id, $concrete);
    }

    public function addShared(string $id, $concrete = null): DefinitionInterface
    {
        $concrete = $concrete ?? $id;
        return $this->definitions->addShared($id, $concrete);
    }

    public function defaultToShared(bool $shared = true): ContainerInterface
    {
        $this->defaultToShared = $shared;
        return $this;
    }

    public function extend(string $id): DefinitionInterface
    {
        if ($this->providers->provides($id)) {
            $this->providers->register($id);
        }

        if ($this->definitions->has($id)) {
            return $this->definitions->getDefinition($id);
        }

        throw new NotFoundException(sprintf(
            'Unable to extend alias (%s) as it is not being managed as a definition',
            $id
        ));
    }

    public function addServiceProvider(ServiceProviderInterface $provider): DefinitionContainerInterface
    {
        $this->providers->add($provider);
        return $this;
    }

    /**
     * @template RequestedType
     *
     * @param class-string<RequestedType>|string $id
     *
     * @return RequestedType|mixed
     */
    public function get($id)
    {
        return $this->resolve($id);
    }

    /**
     * @template RequestedType
     *
     * @param class-string<RequestedType>|string $id
     *
     * @return RequestedType|mixed
     */
    public function getNew($id)
    {
        return $this->resolve($id, true);
    }

    public function has($id): bool
    {
        if ($this->definitions->has($id)) {
            return true;
        }

        if ($this->definitions->hasTag($id)) {
            return true;
        }

        if ($this->providers->provides($id)) {
            return true;
        }

        foreach ($this->delegates as $delegate) {
            if ($delegate->has($id)) {
                return true;
            }
        }

        return false;
    }

    public function inflector(string $type, ?callable $callback = null): InflectorInterface
    {
        return $this->inflectors->add($type, $callback);
    }

    public function delegate(ContainerInterface $container): self
    {
        $this->delegates[] = $container;

        if ($container instanceof ContainerAwareInterface) {
            $container->setContainer($this);
        }

        return $this;
    }

    protected function resolve($id, bool $new = false)
    {
        if ($this->definitions->has($id)) {
            $resolved = (true === $new) ? $this->definitions->resolveNew($id) : $this->definitions->resolve($id);
            return $this->inflectors->inflect($resolved);
        }

        if ($this->definitions->hasTag($id)) {
            $arrayOf = (true === $new)
                ? $this->definitions->resolveTaggedNew($id)
                : $this->definitions->resolveTagged($id);

            array_walk($arrayOf, function (&$resolved) {
                $resolved = $this->inflectors->inflect($resolved);
            });

            return $arrayOf;
        }

        if ($this->providers->provides($id)) {
            $this->providers->register($id);

            if (!$this->definitions->has($id) && !$this->definitions->hasTag($id)) {
                throw new ContainerException(sprintf('Service provider lied about providing (%s) service', $id));
            }

            return $this->resolve($id, $new);
        }

        foreach ($this->delegates as $delegate) {
            if ($delegate->has($id)) {
                $resolved = $delegate->get($id);
                return $this->inflectors->inflect($resolved);
            }
        }

        throw new NotFoundException(sprintf('Alias (%s) is not being managed by the container or delegates', $id));
    }
}
