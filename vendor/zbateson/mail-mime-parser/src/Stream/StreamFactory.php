<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Stream;

use Psr\Http\Message\StreamInterface;
use ZBateson\MailMimeParser\Message\IMessagePart;
use ZBateson\MailMimeParser\Parser\PartBuilder;
use ZBateson\StreamDecorators\Base64Stream;
use ZBateson\StreamDecorators\CharsetStream;
use ZBateson\StreamDecorators\ChunkSplitStream;
use ZBateson\StreamDecorators\NonClosingStream;
use ZBateson\StreamDecorators\PregReplaceFilterStream;
use ZBateson\StreamDecorators\QuotedPrintableStream;
use ZBateson\StreamDecorators\SeekingLimitStream;
use ZBateson\StreamDecorators\UUStream;

/**
 * Factory class for Psr7 stream decorators used in MailMimeParser.
 *
 * @author Zaahid Bateson
 */
class StreamFactory
{
    /**
     * Returns a SeekingLimitStream using $part->getStreamPartLength() and
     * $part->getStreamPartStartPos()
     *
     * @return SeekingLimitStream
     */
    public function getLimitedPartStream(PartBuilder $part)
    {
        return $this->newLimitStream(
            $part->getStream(),
            $part->getStreamPartLength(),
            $part->getStreamPartStartPos()
        );
    }

    /**
     * Returns a SeekingLimitStream using $part->getStreamContentLength() and
     * $part->getStreamContentStartPos()
     *
     * @return ?SeekingLimitStream
     */
    public function getLimitedContentStream(PartBuilder $part)
    {
        $length = $part->getStreamContentLength();
        if ($length !== 0) {
            return $this->newLimitStream(
                $part->getStream(),
                $part->getStreamContentLength(),
                $part->getStreamContentStartPos()
            );
        }
        return null;
    }

    /**
     * Creates and returns a SeekingLimitedStream.
     *
     */
    private function newLimitStream(StreamInterface $stream, int $length, int $start) : SeekingLimitStream
    {
        return new SeekingLimitStream(
            $this->newNonClosingStream($stream),
            $length,
            $start
        );
    }

    /**
     * Creates a non-closing stream that doesn't close it's internal stream when
     * closing/detaching.
     *
     * @return NonClosingStream
     */
    public function newNonClosingStream(StreamInterface $stream)
    {
        return new NonClosingStream($stream);
    }

    /**
     * Creates a ChunkSplitStream.
     *
     * @return ChunkSplitStream
     */
    public function newChunkSplitStream(StreamInterface $stream)
    {
        return new ChunkSplitStream($stream);
    }

    /**
     * Creates and returns a Base64Stream with an internal
     * PregReplaceFilterStream that filters out non-base64 characters.
     *
     * @return Base64Stream
     */
    public function newBase64Stream(StreamInterface $stream)
    {
        return new Base64Stream(
            new PregReplaceFilterStream($stream, '/[^a-zA-Z0-9\/\+=]/', '')
        );
    }

    /**
     * Creates and returns a QuotedPrintableStream.
     *
     * @return QuotedPrintableStream
     */
    public function newQuotedPrintableStream(StreamInterface $stream)
    {
        return new QuotedPrintableStream($stream);
    }

    /**
     * Creates and returns a UUStream
     *
     * @return UUStream
     */
    public function newUUStream(StreamInterface $stream)
    {
        return new UUStream($stream);
    }

    /**
     * Creates and returns a CharsetStream
     *
     * @return CharsetStream
     */
    public function newCharsetStream(StreamInterface $stream, string $fromCharset, string $toCharset)
    {
        return new CharsetStream($stream, $fromCharset, $toCharset);
    }

    /**
     * Creates and returns a MessagePartStream
     *
     * @return MessagePartStream
     */
    public function newMessagePartStream(IMessagePart $part)
    {
        return new MessagePartStream($this, $part);
    }

    /**
     * Creates and returns a HeaderStream
     *
     * @return HeaderStream
     */
    public function newHeaderStream(IMessagePart $part)
    {
        return new HeaderStream($part);
    }
}
