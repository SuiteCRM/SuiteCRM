<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Message\Factory;

use ZBateson\MailMimeParser\Stream\StreamFactory;

/**
 * Abstract factory for subclasses of IMessagePart.
 *
 * @author Zaahid Bateson
 */
abstract class IMessagePartFactory
{
    /**
     * @var StreamFactory
     */
    protected $streamFactory;

    /**
     * @var PartStreamContainerFactory
     */
    protected $partStreamContainerFactory;

    public function __construct(
        StreamFactory $streamFactory,
        PartStreamContainerFactory $partStreamContainerFactory
    ) {
        $this->streamFactory = $streamFactory;
        $this->partStreamContainerFactory = $partStreamContainerFactory;
    }

    /**
     * Constructs a new IMessagePart object and returns it
     *
     * @return \ZBateson\MailMimeParser\Message\IMessagePart
     */
    abstract public function newInstance();
}
