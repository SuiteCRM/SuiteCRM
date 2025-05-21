<?php
/**
 * This file is part of the ZBateson\StreamDecorators project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\StreamDecorators;

use GuzzleHttp\Psr7\StreamDecoratorTrait;
use Psr\Http\Message\StreamInterface;

/**
 * Maintains an internal 'read' position, and seeks to it before reading, then
 * seeks back to the original position of the underlying stream after reading if
 * the attached stream supports seeking.
 *
 * Although based on LimitStream, it's not inherited from it since $offset and
 * $limit are set to private on LimitStream, and most other functions are re-
 * implemented anyway.  This also decouples the implementation from upstream
 * changes.
 *
 * @author Zaahid Bateson
 */
class SeekingLimitStream implements StreamInterface
{
    use StreamDecoratorTrait;

    /** @var int Offset to start reading from */
    private $offset;

    /** @var int Limit the number of bytes that can be read */
    private $limit;

    /**
     * @var int Number of bytes written, and importantly, if non-zero, writes a
     *      final $lineEnding on close (and so maintained instead of using
     *      tell() directly)
     */
    private $position = 0;

    /**
     * @var StreamInterface $stream
     */
    private $stream;

    /**
     * @param StreamInterface $stream Stream to wrap
     * @param int             $limit  Total number of bytes to allow to be read
     *                                from the stream. Pass -1 for no limit.
     * @param int             $offset Position to seek to before reading (only
     *                                works on seekable streams).
     */
    public function __construct(StreamInterface $stream, int $limit = -1, int $offset = 0)
    {
        $this->stream = $stream;
        $this->setLimit($limit);
        $this->setOffset($offset);
    }

    /**
     * Returns the current relative read position of this stream subset.
     */
    public function tell() : int
    {
        return $this->position;
    }

    /**
     * Returns the size of the limited subset of data, or null if the wrapped
     * stream returns null for getSize.
     */
    public function getSize() : ?int
    {
        $size = $this->stream->getSize();
        if ($size === null) {
            // this shouldn't happen on a seekable stream I don't think...
            $pos = $this->stream->tell();
            $this->stream->seek(0, SEEK_END);
            $size = $this->stream->tell();
            $this->stream->seek($pos);
        }
        if ($this->limit === -1) {
            return $size - $this->offset;
        }

        return \min([$this->limit, $size - $this->offset]);
    }

    /**
     * Returns true if the current read position is at the end of the limited
     * stream
     */
    public function eof() : bool
    {
        $size = $this->limit;
        if ($size === -1) {
            $size = $this->getSize();
        }
        return ($this->position >= $size);
    }

    /**
     * Ensures the seek position specified is within the stream's bounds, and
     * sets the internal position pointer (doesn't actually seek).
     */
    private function doSeek(int $pos) : void
    {
        if ($this->limit !== -1) {
            $pos = \min([$pos, $this->limit]);
        }
        $this->position = \max([0, $pos]);
    }

    /**
     * Seeks to the passed position within the confines of the limited stream's
     * bounds.
     *
     * For SeekingLimitStream, no actual seek is performed on the underlying
     * wrapped stream.  Instead, an internal pointer is set, and the stream is
     * 'seeked' on read operations
     *
     * @param int $offset
     * @param int $whence
     */
    public function seek($offset, $whence = SEEK_SET) : void
    {
        $pos = $offset;
        switch ($whence) {
            case SEEK_CUR:
                $pos = $this->position + $offset;
                break;
            case SEEK_END:
                $pos = $this->limit + $offset;
                break;
            default:
                break;
        }
        $this->doSeek($pos);
    }

    /**
     * Sets the offset to start reading from the wrapped stream.
     */
    public function setOffset(int $offset) : void
    {
        $this->offset = $offset;
        $this->position = 0;
    }

    /**
     * Sets the length of the stream to the passed $limit.
     */
    public function setLimit(int $limit) : void
    {
        $this->limit = $limit;
    }

    /**
     * Seeks to the current position and reads up to $length bytes, or less if
     * it would result in reading past $this->limit
     */
    public function seekAndRead(int $length) : string
    {
        $this->stream->seek($this->offset + $this->position);
        if ($this->limit !== -1) {
            $length = \min($length, $this->limit - $this->position);
            if ($length <= 0) {
                return '';
            }
        }
        return $this->stream->read($length);
    }

    /**
     * Reads from the underlying stream after seeking to the position within the
     * bounds set for this limited stream.  After reading, the wrapped stream is
     * 'seeked' back to its position prior to the call to read().
     *
     * @param int $length
     * @return string
     */
    public function read($length) : string
    {
        $pos = $this->stream->tell();
        $ret = $this->seekAndRead($length);
        $this->position += \strlen($ret);
        $this->stream->seek($pos);
        if ($this->limit !== -1 && $this->position > $this->limit) {
            $ret = \substr($ret, 0, -($this->position - $this->limit));
            $this->position = $this->limit;
        }
        return $ret;
    }
}
