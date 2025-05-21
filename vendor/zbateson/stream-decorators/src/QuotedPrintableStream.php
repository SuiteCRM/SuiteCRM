<?php
/**
 * This file is part of the ZBateson\StreamDecorators project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\StreamDecorators;

use GuzzleHttp\Psr7\StreamDecoratorTrait;
use Psr\Http\Message\StreamInterface;
use RuntimeException;

/**
 * GuzzleHttp\Psr7 stream decoder decorator for quoted printable streams.
 *
 * @author Zaahid Bateson
 */
class QuotedPrintableStream implements StreamInterface
{
    use StreamDecoratorTrait;

    /**
     * @var int current read/write position
     */
    private $position = 0;

    /**
     * @var string Last line of written text (used to maintain good line-breaks)
     */
    private $lastLine = '';

    /**
     * @var StreamInterface $stream
     * @phpstan-ignore-next-line
     */
    private $stream;

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
        throw new RuntimeException('Cannot seek a QuotedPrintableStream');
    }

    /**
     * Overridden to return false
     */
    public function isSeekable() : bool
    {
        return false;
    }

    /**
     * Reads $length chars from the underlying stream, prepending the past $pre
     * to it first.
     *
     * If the characters read (including the prepended $pre) contain invalid
     * quoted-printable characters, the underlying stream is rewound by the
     * total number of characters ($length + strlen($pre)).
     *
     * The quoted-printable encoded characters are returned.  If the characters
     * read are invalid, '3D' is returned indicating an '=' character.
     */
    private function readEncodedChars(int $length, string $pre = '') : string
    {
        $str = $pre . $this->stream->read($length);
        $len = \strlen($str);
        if ($len > 0 && !\preg_match('/^[0-9a-f]{2}$|^[\r\n]{1,2}.?$/is', $str) && $this->stream->isSeekable()) {
            $this->stream->seek(-$len, SEEK_CUR);
            return '3D';    // '=' character
        }
        return $str;
    }

    /**
     * Decodes the passed $block of text.
     *
     * If the last or before last character is an '=' char, indicating the
     * beginning of a quoted-printable encoded char, 1 or 2 additional bytes are
     * read from the underlying stream respectively.
     *
     * @return string The decoded string
     */
    private function decodeBlock(string $block) : string
    {
        if (\substr($block, -1) === '=') {
            $block .= $this->readEncodedChars(2);
        } elseif (\substr($block, -2, 1) === '=') {
            $first = \substr($block, -1);
            $block = \substr($block, 0, -1);
            $block .= $this->readEncodedChars(1, $first);
        }
        return \quoted_printable_decode($block);
    }

    /**
     * Reads up to $length characters, appends them to the passed $str string,
     * and returns the total number of characters read.
     *
     * -1 is returned if there are no more bytes to read.
     */
    private function readRawDecodeAndAppend(int $length, string &$str) : int
    {
        $block = $this->stream->read($length);
        if ($block === '') {
            return -1;
        }
        $decoded = $this->decodeBlock($block);
        $count = \strlen($decoded);
        $str .= $decoded;
        return $count;
    }

    /**
     * Reads up to $length decoded bytes from the underlying quoted-printable
     * encoded stream and returns them.
     *
     * @param int $length
     */
    public function read($length) : string
    {
        // let Guzzle decide what to do.
        if ($length <= 0 || $this->eof()) {
            return $this->stream->read($length);
        }
        $count = 0;
        $bytes = '';
        while ($count < $length) {
            $nRead = $this->readRawDecodeAndAppend($length - $count, $bytes);
            if ($nRead === -1) {
                break;
            }
            $this->position += $nRead;
            $count += $nRead;
        }
        return $bytes;
    }

    /**
     * Writes the passed string to the underlying stream after encoding it as
     * quoted-printable.
     *
     * Note that reading and writing to the same stream without rewinding is not
     * supported.
     *
     * @param string $string
     *
     * @return int the number of bytes written
     */
    public function write($string) : int
    {
        $encodedLine = \quoted_printable_encode($this->lastLine);
        $lineAndString = \rtrim(\quoted_printable_encode($this->lastLine . $string), "\r\n");
        $write = \substr($lineAndString, \strlen($encodedLine));
        $this->stream->write($write);
        $written = \strlen($string);
        $this->position += $written;

        $lpos = \strrpos($lineAndString, "\n");
        $lastLine = $lineAndString;
        if ($lpos !== false) {
            $lastLine = \substr($lineAndString, $lpos + 1);
        }
        $this->lastLine = \quoted_printable_decode($lastLine);
        return $written;
    }

    /**
     * Writes out a final CRLF if the current line isn't empty.
     */
    private function beforeClose() : void
    {
        if ($this->isWritable() && $this->lastLine !== '') {
            $this->stream->write("\r\n");
            $this->lastLine = '';
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
