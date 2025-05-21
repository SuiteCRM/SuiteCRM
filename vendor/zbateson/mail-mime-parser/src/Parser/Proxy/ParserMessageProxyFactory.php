<?php

/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Parser\Proxy;

use ZBateson\MailMimeParser\Message;
use ZBateson\MailMimeParser\Message\Factory\PartHeaderContainerFactory;
use ZBateson\MailMimeParser\Message\Helper\MultipartHelper;
use ZBateson\MailMimeParser\Message\Helper\PrivacyHelper;
use ZBateson\MailMimeParser\Parser\IParser;
use ZBateson\MailMimeParser\Parser\Part\ParserPartChildrenContainerFactory;
use ZBateson\MailMimeParser\Parser\Part\ParserPartStreamContainerFactory;
use ZBateson\MailMimeParser\Parser\PartBuilder;
use ZBateson\MailMimeParser\Stream\StreamFactory;

/**
 * Responsible for creating proxied IMessage instances wrapped in a
 * ParserMessageProxy.
 *
 * @author Zaahid Bateson
 */
class ParserMessageProxyFactory extends ParserMimePartProxyFactory
{
    /**
     * @var MultipartHelper
     */
    protected $multipartHelper;

    /**
     * @var PrivacyHelper
     */
    protected $privacyHelper;

    public function __construct(
        StreamFactory $sdf,
        PartHeaderContainerFactory $phcf,
        ParserPartStreamContainerFactory $pscf,
        ParserPartChildrenContainerFactory $ppccf,
        MultipartHelper $multipartHelper,
        PrivacyHelper $privacyHelper
    ) {
        parent::__construct($sdf, $phcf, $pscf, $ppccf);
        $this->multipartHelper = $multipartHelper;
        $this->privacyHelper = $privacyHelper;
    }

    /**
     * Constructs a new ParserMessageProxy wrapping an IMessage object that will
     * dynamically parse a message's content and parts as they're requested.
     *
     * @return ParserMessageProxy
     */
    public function newInstance(PartBuilder $partBuilder, IParser $parser)
    {
        $parserProxy = new ParserMessageProxy($partBuilder, $parser);

        $streamContainer = $this->parserPartStreamContainerFactory->newInstance($parserProxy);
        $headerContainer = $this->partHeaderContainerFactory->newInstance($parserProxy->getHeaderContainer());
        $childrenContainer = $this->parserPartChildrenContainerFactory->newInstance($parserProxy);

        $message = new Message(
            $streamContainer,
            $headerContainer,
            $childrenContainer,
            $this->multipartHelper,
            $this->privacyHelper
        );
        $parserProxy->setPart($message);

        $streamContainer->setStream($this->streamFactory->newMessagePartStream($message));
        $message->attach($streamContainer);
        return $parserProxy;
    }
}
