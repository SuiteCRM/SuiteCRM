<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Stream;

use ArrayIterator;
use GuzzleHttp\Psr7;
use GuzzleHttp\Psr7\StreamDecoratorTrait;
use Psr\Http\Message\StreamInterface;
use SplObserver;
use SplSubject;
use ZBateson\MailMimeParser\Header\HeaderConsts;
use ZBateson\MailMimeParser\Message\IMessagePart;
use ZBateson\MailMimeParser\Message\IMimePart;

/**
 * Psr7 stream decorator implementation providing a readable stream for a part's
 * headers.
 *
 * HeaderStream is only used by a MimePart parent.  It can accept any
 * MessagePart - for non-MimeParts, only type headers are generated based on
 * available information.
 *
 * @author Zaahid Bateson
 */
#[\AllowDynamicProperties]
class HeaderStream implements SplObserver, StreamInterface
{
    use StreamDecoratorTrait;

    /**
     * @var IMessagePart the part to read from.
     */
    protected $part;

    public function __construct(IMessagePart $part)
    {
        $this->part = $part;
        $part->attach($this);
    }

    public function __destruct()
    {
        if ($this->part !== null) {
            $this->part->detach($this);
        }
    }

    public function update(SplSubject $subject) : void
    {
        if ($this->stream !== null) {
            $this->stream = $this->createStream();
        }
    }

    /**
     * Returns a header array for the current part.
     *
     * If the part is not a MimePart, Content-Type, Content-Disposition and
     * Content-Transfer-Encoding headers are generated manually.
     *
     */
    private function getPartHeadersIterator() : \Iterator
    {
        if ($this->part instanceof IMimePart) {
            return $this->part->getRawHeaderIterator();
        } elseif ($this->part->getParent() !== null && $this->part->getParent()->isMime()) {
            return new ArrayIterator([
                [HeaderConsts::CONTENT_TYPE, $this->part->getContentType()],
                [HeaderConsts::CONTENT_DISPOSITION, $this->part->getContentDisposition()],
                [HeaderConsts::CONTENT_TRANSFER_ENCODING, $this->part->getContentTransferEncoding()]
            ]);
        }
        return new ArrayIterator();
    }

    /**
     * Writes out headers for $this->part and follows them with an empty line.
     */
    public function writePartHeadersTo(StreamInterface $stream) : self
    {
        foreach ($this->getPartHeadersIterator() as $header) {
            $stream->write("{$header[0]}: {$header[1]}\r\n");
        }
        $stream->write("\r\n");
        return $this;
    }

    /**
     * Creates the underlying stream lazily when required.
     *
     */
    protected function createStream() : StreamInterface
    {
        $stream = Psr7\Utils::streamFor();
        $this->writePartHeadersTo($stream);
        $stream->rewind();
        return $stream;
    }
}
