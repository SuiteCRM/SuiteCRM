<?php
/**
 * This file is part of the ZBateson\StreamDecorators project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\StreamDecorators;

use GuzzleHttp\Psr7\BufferStream;
use GuzzleHttp\Psr7\StreamDecoratorTrait;
use Psr\Http\Message\StreamInterface;
use RuntimeException;

/**
 * GuzzleHttp\Psr7 stream decoder extension for base64 streams.
 *
 * Note that it's expected the underlying stream will only contain valid base64
 * characters (normally the stream should be wrapped in a
 * PregReplaceFilterStream to filter out non-base64 characters for reading).
 *
 * ```
 * $f = fopen(...);
 * $stream = new Base64Stream(new PregReplaceFilterStream(
 *      Psr7\Utils::streamFor($f), '/[^a-zA-Z0-9\/\+=]/', ''
 * ));
 * //...
 * ```
 *
 * For writing, a ChunkSplitStream could come in handy so the output is split
 * into lines:
 *
 * ```
 * $f = fopen(...);
 * $stream = new Base64Stream(new ChunkSplitStream(new PregReplaceFilterStream(
 *      Psr7\Utils::streamFor($f), '/[^a-zA-Z0-9\/\+=]/', ''
 * )));
 * //...
 * ```
 *
 * @author Zaahid Bateson
 */
class Base64Stream implements StreamInterface
{
    use StreamDecoratorTrait;

    /**
     * @var BufferStream buffered bytes
     */
    private $buffer;

    /**
     * @var string remainder of write operation if the bytes didn't align to 3
     *      bytes
     */
    private $remainder = '';

    /**
     * @var int current number of read/written bytes (for tell())
     */
    private $position = 0;

    /**
     * @var StreamInterface $stream
     */
    private $stream;

    public function __construct(StreamInterface $stream)
    {
        $this->stream = $stream;
        $this->buffer = new BufferStream();
    }

    /**
     * Returns the current position of the file read/write pointer
     */
    public function tell() : int
    {
        return $this->position;
    }

    /**
     * Returns null, getSize isn't supported
     *
     * @return null
     */
    public function getSize() : ?int
    {
        return null;
    }

    /**
     * Not implemented (yet).
     *
     * Seek position can be calculated.
     *
     * @param int $offset
     * @param int $whence
     * @throws RuntimeException
     */
    public function seek($offset, $whence = SEEK_SET) : void
    {
        throw new RuntimeException('Cannot seek a Base64Stream');
    }

    /**
     * Overridden to return false
     */
    public function isSeekable() : bool
    {
        return false;
    }

    /**
     * Returns true if the end of stream has been reached.
     */
    public function eof() : bool
    {
        return ($this->buffer->eof() && $this->stream->eof());
    }

    /**
     * Fills the internal byte buffer after reading and decoding data from the
     * underlying stream.
     *
     * Note that it's expected the underlying stream will only contain valid
     * base64 characters (normally the stream should be wrapped in a
     * PregReplaceFilterStream to filter out non-base64 characters).
     */
    private function fillBuffer(int $length) : void
    {
        $fill = 8192;
        while ($this->buffer->getSize() < $length) {
            $read = $this->stream->read($fill);
            if ($read === '') {
                break;
            }
            $this->buffer->write(\base64_decode($read));
        }
    }

    /**
     * Attempts to read $length bytes after decoding them, and returns them.
     *
     * Note that reading and writing to the same stream may result in wrongly
     * encoded data and is not supported.
     *
     * @param int $length
     * @return string
     */
    public function read($length) : string
    {
        // let Guzzle decide what to do.
        if ($length <= 0 || $this->eof()) {
            return $this->stream->read($length);
        }
        $this->fillBuffer($length);
        $ret = $this->buffer->read($length);
        $this->position += \strlen($ret);
        return $ret;
    }

    /**
     * Writes the passed string to the underlying stream after encoding it to
     * base64.
     *
     * Base64Stream::close or detach must be called.  Failing to do so may
     * result in 1-2 bytes missing from the end of the stream if there's a
     * remainder.  Note that the default Stream destructor calls close as well.
     *
     * Note that reading and writing to the same stream may result in wrongly
     * encoded data and is not supported.
     *
     * @param string $string
     * @return int the number of bytes written
     */
    public function write($string) : int
    {
        $bytes = $this->remainder . $string;
        $len = \strlen($bytes);
        if (($len % 3) !== 0) {
            $this->remainder = \substr($bytes, -($len % 3));
            $bytes = \substr($bytes, 0, $len - ($len % 3));
        } else {
            $this->remainder = '';
        }
        $this->stream->write(\base64_encode($bytes));
        $written = \strlen($string);
        $this->position += $len;
        return $written;
    }

    /**
     * Writes out any remaining bytes at the end of the stream and closes.
     */
    private function beforeClose() : void
    {
        if ($this->isWritable() && $this->remainder !== '') {
            $this->stream->write(\base64_encode($this->remainder));
            $this->remainder = '';
        }
    }

    /**
     * @inheritDoc
     */
    public function close() : void
    {
        $this->beforeClose();
        $this->stream->close();
    }

    /**
     * @inheritDoc
     */
    public function detach()
    {
        $this->beforeClose();
        $this->stream->detach();

        return null;
    }
}
