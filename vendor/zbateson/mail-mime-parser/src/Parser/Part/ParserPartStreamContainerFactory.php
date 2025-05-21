<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Parser\Part;

use ZBateson\MailMimeParser\Parser\Proxy\ParserPartProxy;
use ZBateson\MailMimeParser\Stream\StreamFactory;

/**
 * Creates ParserPartStreamContainer instances.
 *
 * @author Zaahid Bateson
 */
class ParserPartStreamContainerFactory
{
    /**
     * @var StreamFactory
     */
    protected $streamFactory;

    public function __construct(StreamFactory $streamFactory)
    {
        $this->streamFactory = $streamFactory;
    }

    public function newInstance(ParserPartProxy $parserProxy)
    {
        return new ParserPartStreamContainer($this->streamFactory, $parserProxy);
    }
}
