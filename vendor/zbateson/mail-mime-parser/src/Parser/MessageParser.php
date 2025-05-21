<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Parser;

use Psr\Http\Message\StreamInterface;
use ZBateson\MailMimeParser\Message\Factory\PartHeaderContainerFactory;

/**
 * Parses a mail mime message into its component parts.  To invoke, call
 * {@see MailMimeParser::parse()}.
 *
 * @author Zaahid Bateson
 */
class MessageParser
{
    /**
     * @var PartHeaderContainerFactory To create a container to read the
     *      message's headers into.
     */
    protected $partHeaderContainerFactory;

    /**
     * @var ParserManager To figure out what parser is responsible for parsing a
     *      message.
     */
    protected $parserManager;

    /**
     * @var PartBuilderFactory To create a PartBuilder representing this
     *      message, and to pass it to ParserManager.
     */
    protected $partBuilderFactory;

    /**
     * @var HeaderParser To parse the headers into a PartHeaderContainer.
     */
    protected $headerParser;

    public function __construct(
        PartBuilderFactory $pbf,
        PartHeaderContainerFactory $phcf,
        ParserManager $pm,
        HeaderParser $hp
    ) {
        $this->partBuilderFactory = $pbf;
        $this->partHeaderContainerFactory = $phcf;
        $this->parserManager = $pm;
        $this->headerParser = $hp;
    }

    /**
     * Convenience method to read a line of up to 4096 characters from the
     * passed resource handle.
     *
     * If the line is larger than 4096 characters, the remaining characters in
     * the line are read and discarded, and only the first 4096 characters are
     * returned.
     *
     * @param resource $handle
     * @return string|bool the read line or false on EOF or on error.
     */
    public static function readLine($handle)
    {
        $size = 4096;
        $ret = $line = \fgets($handle, $size);
        while (\strlen($line) === $size - 1 && \substr($line, -1) !== "\n") {
            $line = \fgets($handle, $size);
        }
        return $ret;
    }

    /**
     * Parses the passed stream into an {@see ZBateson\MailMimeParser\IMessage}
     * object and returns it.
     *
     * @param StreamInterface $stream the stream to parse the message from
     * @return \ZBateson\MailMimeParser\IMessage
     */
    public function parse(StreamInterface $stream)
    {
        $headerContainer = $this->partHeaderContainerFactory->newInstance();
        $partBuilder = $this->partBuilderFactory->newPartBuilder($headerContainer, $stream);
        $this->headerParser->parse(
            $partBuilder->getMessageResourceHandle(),
            $headerContainer
        );
        $proxy = $this->parserManager->createParserProxyFor($partBuilder);
        return $proxy->getPart();
    }
}
