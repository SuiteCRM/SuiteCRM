<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Parser;

use GuzzleHttp\Psr7\StreamWrapper;
use Psr\Http\Message\StreamInterface;
use ZBateson\MailMimeParser\Header\HeaderConsts;
use ZBateson\MailMimeParser\Message\PartHeaderContainer;
use ZBateson\MailMimeParser\Parser\Proxy\ParserPartProxy;

/**
 * Holds generic/all purpose information about a part while it's being parsed.
 *
 * The class holds:
 *  - a HeaderContainer to hold headers
 *  - stream positions (part start/end positions, content start/end)
 *  - the message's psr7 stream and a resource handle created from it (held
 *    only for a top-level PartBuilder representing the message, child
 *    PartBuilders do not duplicate/hold a separate stream).
 *
 * More specific information a parser needs to keep about a message as it's
 * parsing it should be stored in its ParserPartProxy.
 *
 * @author Zaahid Bateson
 */
class PartBuilder
{
    /**
     * @var int The offset read start position for this part (beginning of
     * headers) in the message's stream.
     */
    private $streamPartStartPos = null;

    /**
     * @var int The offset read end position for this part.  If the part is a
     * multipart mime part, the end position is after all of this parts
     * children.
     */
    private $streamPartEndPos = null;

    /**
     * @var int The offset read start position in the message's stream for the
     * beginning of this part's content (body).
     */
    private $streamContentStartPos = null;

    /**
     * @var int The offset read end position in the message's stream for the
     * end of this part's content (body).
     */
    private $streamContentEndPos = null;

    /**
     * @var PartHeaderContainer The parsed part's headers.
     */
    private $headerContainer;

    /**
     * @var StreamInterface the raw message input stream for a message, or null
     *      for a child part.
     */
    private $messageStream = null;

    /**
     * @var resource the raw message input stream handle constructed from
     *      $messageStream or null for a child part
     */
    private $messageHandle = null;

    /**
     * @var ParserPartProxy The parent ParserPartProxy.
     */
    private $parent = null;

    public function __construct(PartHeaderContainer $headerContainer, ?StreamInterface $messageStream = null, ?ParserPartProxy $parent = null)
    {
        $this->headerContainer = $headerContainer;
        $this->messageStream = $messageStream;
        $this->parent = $parent;
        if ($messageStream !== null) {
            $this->messageHandle = StreamWrapper::getResource($messageStream);
        }
        $this->setStreamPartStartPos($this->getMessageResourceHandlePos());
    }

    public function __destruct()
    {
        if ($this->messageHandle) {
            \fclose($this->messageHandle);
        }
    }

    /**
     * The ParserPartProxy parent of this PartBuilder.
     *
     * @return ParserPartProxy
     */
    public function getParent()
    {
        return $this->parent;
    }

    /**
     * Returns this part's PartHeaderContainer.
     *
     * @return PartHeaderContainer the container
     */
    public function getHeaderContainer()
    {
        return $this->headerContainer;
    }

    /**
     * Returns the raw message StreamInterface for a message, getting it from
     * the parent part if this is a child part.
     *
     * @return StreamInterface
     */
    public function getStream()
    {
        return ($this->parent !== null) ?
            $this->parent->getStream() :
            $this->messageStream;
    }

    /**
     * Returns the resource handle for a the message's stream, getting it from
     * the parent part if this is a child part.
     *
     * @return resource
     */
    public function getMessageResourceHandle()
    {
        return ($this->parent !== null) ?
            $this->parent->getMessageResourceHandle() :
            $this->messageHandle;
    }

    /**
     * Shortcut for calling ftell($partBuilder->getMessageResourceHandle()).
     *
     */
    public function getMessageResourceHandlePos() : int
    {
        return \ftell($this->getMessageResourceHandle());
    }

    /**
     * Returns the byte offset start position for this part within the message
     * stream if it's been set, or null otherwise.
     */
    public function getStreamPartStartPos() : ?int
    {
        return $this->streamPartStartPos;
    }

    /**
     * Returns the number of raw bytes this part has.
     *
     * This method does not perform checks on whether the start pos and end pos
     * of this part have been set, and so could cause errors if called before
     * being set and are still null.
     *
     */
    public function getStreamPartLength() : int
    {
        return $this->streamPartEndPos - $this->streamPartStartPos;
    }

    /**
     * Returns the byte offset start position of the content of this part within
     * the main raw message stream, or null if not set.
     *
     */
    public function getStreamContentStartPos() : ?int
    {
        return $this->streamContentStartPos;
    }

    /**
     * Returns the length of this part's content stream.
     *
     * This method does not perform checks on whether the start pos and end pos
     * of this part's content have been set, and so could cause errors if called
     * before being set and are still null.
     *
     */
    public function getStreamContentLength() : int
    {
        return $this->streamContentEndPos - $this->streamContentStartPos;
    }

    /**
     * Sets the byte offset start position of the part in the raw message
     * stream.
     *
     * @return static
     */
    public function setStreamPartStartPos(int $streamPartStartPos)
    {
        $this->streamPartStartPos = $streamPartStartPos;
        return $this;
    }

    /**
     * Sets the byte offset end position of the part in the raw message stream,
     * and also calls its parent's setParentStreamPartEndPos to expand to parent
     * PartBuilders.
     *
     * @return static
     */
    public function setStreamPartEndPos(int $streamPartEndPos)
    {
        $this->streamPartEndPos = $streamPartEndPos;
        if ($this->parent !== null) {
            $this->parent->setStreamPartEndPos($streamPartEndPos);
        }
        return $this;
    }

    /**
     * Sets the byte offset start position of the content in the raw message
     * stream.
     *
     * @return static
     */
    public function setStreamContentStartPos(int $streamContentStartPos)
    {
        $this->streamContentStartPos = $streamContentStartPos;
        return $this;
    }

    /**
     * Sets the byte offset end position of the content and part in the raw
     * message stream.
     *
     * @return static
     */
    public function setStreamPartAndContentEndPos(int $streamContentEndPos)
    {
        $this->streamContentEndPos = $streamContentEndPos;
        $this->setStreamPartEndPos($streamContentEndPos);
        return $this;
    }

    /**
     * Returns true if the byte offset positions for this part's content have
     * been set.
     *
     * @return ?bool true if set.
     */
    public function isContentParsed() : ?bool
    {
        return ($this->streamContentEndPos !== null);
    }

    /**
     * Returns true if this part, or any parent, have a Content-Type or
     * MIME-Version header set.
     *
     * @return bool true if it's a mime message or child of a mime message.
     */
    public function isMime() : bool
    {
        if ($this->getParent() !== null) {
            return $this->getParent()->isMime();
        }
        return ($this->headerContainer->exists(HeaderConsts::CONTENT_TYPE) ||
            $this->headerContainer->exists(HeaderConsts::MIME_VERSION));
    }
}
