<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Message;

use GuzzleHttp\Psr7\CachingStream;
use Psr\Http\Message\StreamInterface;
use ZBateson\MailMimeParser\Stream\StreamFactory;

/**
 * Holds the stream and content stream objects for a part.
 *
 * Note that streams are not explicitly closed or detached on destruction of the
 * PartSreamContainer by design: the passed StreamInterfaces will be closed on
 * their destruction when no references to them remain, which is useful when the
 * streams are passed around.
 *
 * In addition, all the streams passed to PartStreamContainer should be wrapping
 * a ZBateson\StreamDecorators\NonClosingStream unless attached to a part by a
 * user, this is because MMP uses a single seekable stream for content and wraps
 * it in ZBateson\StreamDecorators\SeekingLimitStream objects for each part.
 *
 * @author Zaahid Bateson
 */
class PartStreamContainer
{
    /**
     * @var StreamFactory used to apply psr7 stream decorators to the
     *      attached StreamInterface based on encoding.
     */
    protected $streamFactory;

    /**
     * @var StreamInterface stream containing the part's headers, content and
     *      children
     */
    protected $stream;

    /**
     * @var StreamInterface a stream containing this part's content
     */
    protected $contentStream;

    /**
     * @var StreamInterface the content stream after attaching transfer encoding
     *      streams to $contentStream.
     */
    protected $decodedStream;

    /**
     * @var StreamInterface attached charset stream to $decodedStream
     */
    protected $charsetStream;

    /**
     * @var bool true if the stream should be detached when this container is
     *      destroyed.
     */
    protected $detachParsedStream;

    /**
     * @var array<string, null> map of the active encoding filter on the current handle.
     */
    private $encoding = [
        'type' => null,
        'filter' => null
    ];

    /**
     * @var array<string, null> map of the active charset filter on the current handle.
     */
    private $charset = [
        'from' => null,
        'to' => null,
        'filter' => null
    ];

    public function __construct(StreamFactory $streamFactory)
    {
        $this->streamFactory = $streamFactory;
    }

    /**
     * Sets the part's stream containing the part's headers, content, and
     * children.
     *
     */
    public function setStream(StreamInterface $stream)
    {
        $this->stream = $stream;
    }

    /**
     * Returns the part's stream containing the part's headers, content, and
     * children.
     *
     * @return StreamInterface
     */
    public function getStream()
    {
        // error out if called before setStream, getStream should never return
        // null.
        $this->stream->rewind();
        return $this->stream;
    }

    /**
     * Returns true if there's a content stream associated with the part.
     *
     */
    public function hasContent() : bool
    {
        return ($this->contentStream !== null);
    }

    /**
     * Attaches the passed stream as the content portion of this
     * StreamContainer.
     *
     * The content stream would represent the content portion of $this->stream.
     *
     * If the content is overridden, $this->stream should point to a dynamic
     * {@see ZBateson\Stream\MessagePartStream} that dynamically creates the
     * RFC822 formatted message based on the IMessagePart this
     * PartStreamContainer belongs to.
     *
     * setContentStream can be called with 'null' to indicate the IMessagePart
     * does not contain any content.
     *
     * @param StreamInterface $contentStream
     */
    public function setContentStream(?StreamInterface $contentStream = null)
    {
        $this->contentStream = $contentStream;
        $this->decodedStream = null;
        $this->charsetStream = null;
    }

    /**
     * Returns true if the attached stream filter used for decoding the content
     * on the current handle is different from the one passed as an argument.
     *
     * @param string $transferEncoding
     */
    private function isTransferEncodingFilterChanged(?string $transferEncoding) : bool
    {
        return ($transferEncoding !== $this->encoding['type']);
    }

    /**
     * Returns true if the attached stream filter used for charset conversion on
     * the current handle is different from the one needed based on the passed
     * arguments.
     *
     */
    private function isCharsetFilterChanged(string $fromCharset, string $toCharset) : bool
    {
        return ($fromCharset !== $this->charset['from']
            || $toCharset !== $this->charset['to']);
    }

    /**
     * Attaches a decoding filter to the attached content handle, for the passed
     * $transferEncoding.
     *
     * @param string $transferEncoding
     */
    protected function attachTransferEncodingFilter(?string $transferEncoding) : self
    {
        if ($this->decodedStream !== null) {
            $this->encoding['type'] = $transferEncoding;
            $assign = null;
            switch ($transferEncoding) {
                case 'base64':
                    $assign = $this->streamFactory->newBase64Stream($this->decodedStream);
                    break;
                case 'x-uuencode':
                    $assign = $this->streamFactory->newUUStream($this->decodedStream);
                    break;
                case 'quoted-printable':
                    $assign = $this->streamFactory->newQuotedPrintableStream($this->decodedStream);
                    break;
            }
            if ($assign !== null) {
                $this->decodedStream = new CachingStream($assign);
            }
        }
        return $this;
    }

    /**
     * Attaches a charset conversion filter to the attached content handle, for
     * the passed arguments.
     *
     * @param string $fromCharset the character set the content is encoded in
     * @param string $toCharset the target encoding to return
     */
    protected function attachCharsetFilter(string $fromCharset, string $toCharset) : self
    {
        if ($this->charsetStream !== null) {
            $this->charsetStream = new CachingStream($this->streamFactory->newCharsetStream(
                $this->charsetStream,
                $fromCharset,
                $toCharset
            ));
            $this->charset['from'] = $fromCharset;
            $this->charset['to'] = $toCharset;
        }
        return $this;
    }

    /**
     * Resets just the charset stream, and rewinds the decodedStream.
     */
    private function resetCharsetStream() : self
    {
        $this->charset = [
            'from' => null,
            'to' => null,
            'filter' => null
        ];
        $this->decodedStream->rewind();
        $this->charsetStream = $this->decodedStream;
        return $this;
    }

    /**
     * Resets cached encoding and charset streams, and rewinds the stream.
     */
    public function reset()
    {
        $this->encoding = [
            'type' => null,
            'filter' => null
        ];
        $this->charset = [
            'from' => null,
            'to' => null,
            'filter' => null
        ];
        $this->contentStream->rewind();
        $this->decodedStream = $this->contentStream;
        $this->charsetStream = $this->contentStream;
    }

    /**
     * Checks what transfer-encoding decoder stream and charset conversion
     * stream are currently attached on the underlying contentStream, and resets
     * them if the requested arguments differ from the currently assigned ones.
     *
     * @param string $transferEncoding the transfer encoding
     * @param string $fromCharset the character set the content is encoded in
     * @param string $toCharset the target encoding to return
     * @return ?StreamInterface
     */
    public function getContentStream(?string $transferEncoding, ?string $fromCharset, ?string $toCharset)
    {
        if ($this->contentStream === null) {
            return null;
        }
        if (empty($fromCharset) || empty($toCharset)) {
            return $this->getBinaryContentStream($transferEncoding);
        }
        if ($this->charsetStream === null
            || $this->isTransferEncodingFilterChanged($transferEncoding)
            || $this->isCharsetFilterChanged($fromCharset, $toCharset)) {
            if ($this->charsetStream === null
                || $this->isTransferEncodingFilterChanged($transferEncoding)) {
                $this->reset();
                $this->attachTransferEncodingFilter($transferEncoding);
            }
            $this->resetCharsetStream();
            $this->attachCharsetFilter($fromCharset, $toCharset);
        }
        $this->charsetStream->rewind();
        return $this->charsetStream;
    }

    /**
     * Checks what transfer-encoding decoder stream is attached on the
     * underlying stream, and resets it if the requested arguments differ.
     *
     * @param string $transferEncoding
     * @return StreamInterface
     */
    public function getBinaryContentStream(?string $transferEncoding = null) : ?StreamInterface
    {
        if ($this->contentStream === null) {
            return null;
        }
        if ($this->decodedStream === null
            || $this->isTransferEncodingFilterChanged($transferEncoding)) {
            $this->reset();
            $this->attachTransferEncodingFilter($transferEncoding);
        }
        $this->decodedStream->rewind();
        return $this->decodedStream;
    }
}
