<?php
/**
 * This file is part of the ZBateson\StreamDecorator project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\StreamDecorators;

use GuzzleHttp\Psr7\StreamDecoratorTrait;
use Psr\Http\Message\StreamInterface;
use RuntimeException;
use ZBateson\MbWrapper\MbWrapper;

/**
 * GuzzleHttp\Psr7 stream decoder extension for charset conversion.
 *
 * @author Zaahid Bateson
 */
class CharsetStream implements StreamInterface
{
    use StreamDecoratorTrait;

    /**
     * @var MbWrapper the charset converter
     */
    protected $converter = null;

    /**
     * @var string charset of the source stream
     */
    protected $streamCharset = 'ISO-8859-1';

    /**
     * @var string charset of strings passed in write operations, and returned
     *      in read operations.
     */
    protected $stringCharset = 'UTF-8';

    /**
     * @var int current read/write position
     */
    private $position = 0;

    /**
     * @var int number of $stringCharset characters in $buffer
     */
    private $bufferLength = 0;

    /**
     * @var string a buffer of characters read in the original $streamCharset
     *      encoding
     */
    private $buffer = '';

    /**
     * @var StreamInterface $stream
     */
    private $stream;

    /**
     * @param StreamInterface $stream Stream to decorate
     * @param string $streamCharset The underlying stream's charset
     * @param string $stringCharset The charset to encode strings to (or
     *        expected for write)
     */
    public function __construct(StreamInterface $stream, string $streamCharset = 'ISO-8859-1', string $stringCharset = 'UTF-8')
    {
        $this->stream = $stream;
        $this->converter = new MbWrapper();
        $this->streamCharset = $streamCharset;
        $this->stringCharset = $stringCharset;
    }

    /**
     * Overridden to return the position in the target encoding.
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
     * Not supported.
     *
     * @param int $offset
     * @param int $whence
     * @throws RuntimeException
     */
    public function seek($offset, $whence = SEEK_SET) : void
    {
        throw new RuntimeException('Cannot seek a CharsetStream');
    }

    /**
     * Overridden to return false
     */
    public function isSeekable() : bool
    {
        return false;
    }

    /**
     * Reads a minimum of $length characters from the underlying stream in its
     * encoding into $this->buffer.
     *
     * Aligning to 4 bytes seemed to solve an issue reading from UTF-16LE
     * streams and pass testReadUtf16LeToEof, although the buffered string
     * should've solved that on its own.
     */
    private function readRawCharsIntoBuffer(int $length) : void
    {
        $n = (int) \ceil(($length + 32) / 4.0) * 4;
        while ($this->bufferLength < $n) {
            $raw = $this->stream->read($n + 512);
            if ($raw === '') {
                return;
            }
            $this->buffer .= $raw;
            $this->bufferLength = $this->converter->getLength($this->buffer, $this->streamCharset);
        }
    }

    /**
     * Returns true if the end of stream has been reached.
     */
    public function eof() : bool
    {
        return ($this->bufferLength === 0 && $this->stream->eof());
    }

    /**
     * Reads up to $length decoded chars from the underlying stream and returns
     * them after converting to the target string charset.
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
        $this->readRawCharsIntoBuffer($length);
        $numChars = \min([$this->bufferLength, $length]);
        $chars = $this->converter->getSubstr($this->buffer, $this->streamCharset, 0, $numChars);

        $this->position += $numChars;
        $this->buffer = $this->converter->getSubstr($this->buffer, $this->streamCharset, $numChars);
        $this->bufferLength -= $numChars;

        return $this->converter->convert($chars, $this->streamCharset, $this->stringCharset);
    }

    /**
     * Writes the passed string to the underlying stream after converting it to
     * the target stream encoding.
     *
     * @param string $string
     * @return int the number of bytes written
     */
    public function write($string) : int
    {
        $converted = $this->converter->convert($string, $this->stringCharset, $this->streamCharset);
        $written = $this->converter->getLength($converted, $this->streamCharset);
        $this->position += $written;
        return $this->stream->write($converted);
    }
}
