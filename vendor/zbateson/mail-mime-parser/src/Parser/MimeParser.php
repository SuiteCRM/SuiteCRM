<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Parser;

use ZBateson\MailMimeParser\Message\Factory\PartHeaderContainerFactory;
use ZBateson\MailMimeParser\Message\PartHeaderContainer;
use ZBateson\MailMimeParser\Parser\Proxy\ParserMessageProxyFactory;
use ZBateson\MailMimeParser\Parser\Proxy\ParserMimePartProxy;
use ZBateson\MailMimeParser\Parser\Proxy\ParserMimePartProxyFactory;
use ZBateson\MailMimeParser\Parser\Proxy\ParserPartProxy;

/**
 * Parses content and children of MIME parts.
 *
 * @author Zaahid Bateson
 */
class MimeParser extends AbstractParser
{
    /**
     * @var PartHeaderContainerFactory Factory service for creating
     *      PartHeaderContainers for headers.
     */
    protected $partHeaderContainerFactory;

    /**
     * @var HeaderParser The HeaderParser service.
     */
    protected $headerParser;

    public function __construct(
        ParserMessageProxyFactory $parserMessageProxyFactory,
        ParserMimePartProxyFactory $parserMimePartProxyFactory,
        PartBuilderFactory $partBuilderFactory,
        PartHeaderContainerFactory $partHeaderContainerFactory,
        HeaderParser $headerParser
    ) {
        parent::__construct($parserMessageProxyFactory, $parserMimePartProxyFactory, $partBuilderFactory);
        $this->partHeaderContainerFactory = $partHeaderContainerFactory;
        $this->headerParser = $headerParser;
    }

    /**
     * Returns true if the passed PartBuilder::isMime() method returns true.
     *
     */
    public function canParse(PartBuilder $part) : bool
    {
        return $part->isMime();
    }

    /**
     * Reads up to 2048 bytes of input from the passed resource handle,
     * discarding portions of a line that are longer than that, and returning
     * the read portions of the line.
     *
     * The method also calls $proxy->setLastLineEndingLength which is used in
     * findContentBoundary() to set the exact end byte of a part.
     *
     * @param resource $handle
     */
    private function readBoundaryLine($handle, ParserMimePartProxy $proxy) : string
    {
        $size = 2048;
        $isCut = false;
        $line = \fgets($handle, $size);
        while (\strlen($line) === $size - 1 && \substr($line, -1) !== "\n") {
            $line = \fgets($handle, $size);
            $isCut = true;
        }
        $ret = \rtrim($line, "\r\n");
        $proxy->setLastLineEndingLength(\strlen($line) - \strlen($ret));
        return ($isCut) ? '' : $ret;
    }

    /**
     * Reads 2048-byte lines from the passed $handle, calling
     * $partBuilder->setEndBoundaryFound with the passed line until it returns
     * true or the stream is at EOF.
     *
     * setEndBoundaryFound returns true if the passed line matches a boundary
     * for the $partBuilder itself or any of its parents.
     *
     * Lines longer than 2048 bytes are returned as single lines of 2048 bytes,
     * the longer line is not returned separately but is simply discarded.
     *
     * Once a boundary is found, setStreamPartAndContentEndPos is called with
     * the passed $handle's read pos before the boundary and its line separator
     * were read.
     */
    private function findContentBoundary(ParserMimePartProxy $proxy) : self
    {
        $handle = $proxy->getMessageResourceHandle();
        // last separator before a boundary belongs to the boundary, and is not
        // part of the current part, if a part is immediately followed by a
        // boundary, this could result in a '-1' or '-2' content length
        while (!\feof($handle)) {
            $endPos = \ftell($handle) - $proxy->getLastLineEndingLength();
            $line = $this->readBoundaryLine($handle, $proxy);
            if (\substr($line, 0, 2) === '--' && $proxy->setEndBoundaryFound($line)) {
                $proxy->setStreamPartAndContentEndPos($endPos);
                return $this;
            }
        }
        $proxy->setStreamPartAndContentEndPos(\ftell($handle));
        $proxy->setEof();
        return $this;
    }

    /**
     * @return static
     */
    public function parseContent(ParserPartProxy $proxy)
    {
        $proxy->setStreamContentStartPos($proxy->getMessageResourceHandlePos());
        $this->findContentBoundary($proxy);
        return $this;
    }

    /**
     * Calls the header parser to fill the passed $headerContainer, then calls
     * $this->parserManager->createParserProxyFor($child);
     *
     * The method first checks though if the 'part' represents hidden content
     * past a MIME end boundary, which some messages like to include, for
     * instance:
     *
     * ```
     * --outer-boundary--
     * --boundary
     * content
     * --boundary--
     * some hidden information
     * --outer-boundary--
     * ```
     *
     * In this case, $this->parserPartProxyFactory is called directly to create
     * a part, $this->parseContent is called immediately to parse it and discard
     * it, and null is returned.
     *
     * @return ParserPartProxy|null
     */
    private function createPart(ParserMimePartProxy $parent, PartHeaderContainer $headerContainer, PartBuilder $child)
    {
        if (!$parent->isEndBoundaryFound()) {
            $this->headerParser->parse(
                $child->getMessageResourceHandle(),
                $headerContainer
            );
            $parserProxy = $this->parserManager->createParserProxyFor($child);
            return $parserProxy;
        }
        // reads content past an end boundary if there is any
        $parserProxy = $this->parserPartProxyFactory->newInstance($child, $this);
        $this->parseContent($parserProxy);
        return null;
    }

    public function parseNextChild(ParserMimePartProxy $proxy)
    {
        if ($proxy->isParentBoundaryFound()) {
            return null;
        }
        $headerContainer = $this->partHeaderContainerFactory->newInstance();
        $child = $this->partBuilderFactory->newChildPartBuilder($headerContainer, $proxy);
        return $this->createPart($proxy, $headerContainer, $child);
    }
}
