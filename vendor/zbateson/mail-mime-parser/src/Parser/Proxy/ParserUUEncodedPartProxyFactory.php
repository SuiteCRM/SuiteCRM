<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Parser\Proxy;

use ZBateson\MailMimeParser\Message\UUEncodedPart;
use ZBateson\MailMimeParser\Parser\IParser;
use ZBateson\MailMimeParser\Parser\Part\ParserPartStreamContainerFactory;
use ZBateson\MailMimeParser\Parser\PartBuilder;
use ZBateson\MailMimeParser\Stream\StreamFactory;

/**
 * Responsible for creating proxied IUUEncodedPart instances wrapped in a
 * ParserUUEncodedPartProxy and used by NonMimeParser.
 *
 * @author Zaahid Bateson
 */
class ParserUUEncodedPartProxyFactory extends ParserPartProxyFactory
{
    /**
     * @var StreamFactory the StreamFactory instance
     */
    protected $streamFactory;

    /**
     * @var ParserPartStreamContainerFactory
     */
    protected $parserPartStreamContainerFactory;

    public function __construct(StreamFactory $sdf, ParserPartStreamContainerFactory $parserPartStreamContainerFactory)
    {
        $this->streamFactory = $sdf;
        $this->parserPartStreamContainerFactory = $parserPartStreamContainerFactory;
    }

    /**
     * Constructs a new ParserUUEncodedPartProxy wrapping an IUUEncoded object.
     *
     * @return ParserPartProxy
     */
    public function newInstance(PartBuilder $partBuilder, IParser $parser)
    {
        $parserProxy = new ParserUUEncodedPartProxy($partBuilder, $parser);
        $streamContainer = $this->parserPartStreamContainerFactory->newInstance($parserProxy);

        $part = new UUEncodedPart(
            $parserProxy->getUnixFileMode(),
            $parserProxy->getFileName(),
            $partBuilder->getParent()->getPart(),
            $streamContainer
        );
        $parserProxy->setPart($part);

        $streamContainer->setStream($this->streamFactory->newMessagePartStream($part));
        $part->attach($streamContainer);
        return $parserProxy;
    }
}
