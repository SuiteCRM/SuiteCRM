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
 * GuzzleHttp\Psr7 stream decoder extension for UU-Encoded streams.
 *
 * The size of the underlying stream and the position of bytes can't be
 * determined because the number of encoded bytes is indeterminate without
 * reading the entire stream.
 *
 * @author Zaahid Bateson
 */
class UUStream implements StreamInterface
{
    use StreamDecoratorTrait;

    /**
     * @var string name of the UUEncoded file
     */
    protected $filename = null;

    /**
     * @var BufferStream of read and decoded bytes
     */
    private $buffer;

    /**
     * @var string remainder of write operation if the bytes didn't align to 3
     *      bytes
     */
    private $remainder = '';

    /**
     * @var int read/write position
     */
    private $position = 0;

    /**
     * @var bool set to true when 'write' is called
     */
    private $isWriting = false;

    /**
     * @var StreamInterface $stream
     */
    private $stream;

    /**
     * @param StreamInterface $stream Stream to decorate
     * @param string $filename optional file name
     */
    public function __construct(StreamInterface $stream, ?string $filename = null)
    {
        $this->stream = $stream;
        $this->filename = $filename;
        $this->buffer = new BufferStream();
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
        throw new RuntimeException('Cannot seek a UUStream');
    }

    /**
     * Overridden to return false
     */
    public function isSeekable() : bool
    {
        return false;
    }

    /**
     * Finds the next end-of-line character to ensure a line isn't broken up
     * while buffering.
     */
    private function readToEndOfLine(int $length) : string
    {
        $str = $this->stream->read($length);
        if ($str === '') {
            return $str;
        }
        while (\substr($str, -1) !== "\n") {
            $chr = $this->stream->read(1);
            if ($chr === '') {
                break;
            }
            $str .= $chr;
        }
        return $str;
    }

    /**
     * Removes invalid characters from a uuencoded string, and 'BEGIN' and 'END'
     * line headers and footers from the passed string before returning it.
     */
    private function filterAndDecode(string $str) : string
    {
        $ret = \str_replace("\r", '', $str);
        $ret = \preg_replace('/[^\x21-\xf5`\n]/', '`', $ret);
        if ($this->position === 0) {
            $matches = [];
            if (\preg_match('/^\s*begin\s+[^\s+]\s+([^\r\n]+)\s*$/im', $ret, $matches)) {
                $this->filename = $matches[1];
            }
            $ret = \preg_replace('/^\s*begin[^\r\n]+\s*$/im', '', $ret);
        } else {
            $ret = \preg_replace('/^\s*end\s*$/im', '', $ret);
        }
        return \convert_uudecode(\trim($ret));
    }

    /**
     * Buffers bytes into $this->buffer, removing uuencoding headers and footers
     * and decoding them.
     */
    private function fillBuffer(int $length) : void
    {
        // 5040 = 63 * 80, seems to be good balance for buffering in benchmarks
        // testing with a simple 'if ($length < x)' and calculating a better
        // size reduces speeds by up to 4x
        while ($this->buffer->getSize() < $length) {
            $read = $this->readToEndOfLine(5040);
            if ($read === '') {
                break;
            }
            $this->buffer->write($this->filterAndDecode($read));
        }
    }

    /**
     * Returns true if the end of stream has been reached.
     */
    public function eof() : bool
    {
        return ($this->buffer->eof() && $this->stream->eof());
    }

    /**
     * Attempts to read $length bytes after decoding them, and returns them.
     *
     * @param int $length
     */
    public function read($length) : string
    {
        // let Guzzle decide what to do.
        if ($length <= 0 || $this->eof()) {
            return $this->stream->read($length);
        }
        $this->fillBuffer($length);
        $read = $this->buffer->read($length);
        $this->position += \strlen($read);
        return $read;
    }

    /**
     * Writes the 'begin' UU header line.
     */
    private function writeUUHeader() : void
    {
        $filename = (empty($this->filename)) ? 'null' : $this->filename;
        $this->stream->write("begin 666 $filename");
    }

    /**
     * Writes the '`' and 'end' UU footer lines.
     */
    private function writeUUFooter() : void
    {
        $this->stream->write("\r\n`\r\nend\r\n");
    }

    /**
     * Writes the passed bytes to the underlying stream after encoding them.
     */
    private function writeEncoded(string $bytes) : void
    {
        $encoded = \preg_replace('/\r\n|\r|\n/', "\r\n", \rtrim(\convert_uuencode($bytes)));
        // removes ending '`' line
        $this->stream->write("\r\n" . \rtrim(\substr($encoded, 0, -1)));
    }

    /**
     * Prepends any existing remainder to the passed string, then checks if the
     * string fits into a uuencoded line, and removes and keeps any remainder
     * from the string to write.  Full lines ready for writing are returned.
     */
    private function handleRemainder(string $string) : string
    {
        $write = $this->remainder . $string;
        $nRem = \strlen($write) % 45;
        $this->remainder = '';
        if ($nRem !== 0) {
            $this->remainder = \substr($write, -$nRem);
            $write = \substr($write, 0, -$nRem);
        }
        return $write;
    }

    /**
     * Writes the passed string to the underlying stream after encoding it.
     *
     * Note that reading and writing to the same stream without rewinding is not
     * supported.
     *
     * Also note that some bytes may not be written until close or detach are
     * called.  This happens if written data doesn't align to a complete
     * uuencoded 'line' of 45 bytes.  In addition, the UU footer is only written
     * when closing or detaching as well.
     *
     * @param string $string
     * @return int the number of bytes written
     */
    public function write($string) : int
    {
        $this->isWriting = true;
        if ($this->position === 0) {
            $this->writeUUHeader();
        }
        $write = $this->handleRemainder($string);
        if ($write !== '') {
            $this->writeEncoded($write);
        }
        $written = \strlen($string);
        $this->position += $written;
        return $written;
    }

    /**
     * Returns the filename set in the UUEncoded header (or null)
     */
    public function getFilename() : string
    {
        return $this->filename;
    }

    /**
     * Sets the UUEncoded header file name written in the 'begin' header line.
     */
    public function setFilename(string $filename) : void
    {
        $this->filename = $filename;
    }

    /**
     * Writes out any remaining bytes and the UU footer.
     */
    private function beforeClose() : void
    {
        if (!$this->isWriting) {
            return;
        }
        if ($this->remainder !== '') {
            $this->writeEncoded($this->remainder);
        }
        $this->remainder = '';
        $this->isWriting = false;
        $this->writeUUFooter();
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
