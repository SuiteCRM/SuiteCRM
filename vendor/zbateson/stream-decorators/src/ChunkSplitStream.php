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
 * Inserts line ending characters after the set number of characters have been
 * written to the underlying stream.
 *
 * @author Zaahid Bateson
 */
class ChunkSplitStream implements StreamInterface
{
    use StreamDecoratorTrait;

    /**
     * @var int Number of bytes written, and importantly, if non-zero, writes a
     *      final $lineEnding on close (and so maintained instead of using
     *      tell() directly)
     */
    private $position;

    /**
     * @var int The number of characters in a line before inserting $lineEnding.
     */
    private $lineLength;

    /**
     * @var string The line ending characters to insert.
     */
    private $lineEnding;

    /**
     * @var int The strlen() of $lineEnding
     */
    private $lineEndingLength;

    /**
     * @var StreamInterface $stream
     */
    private $stream;

    public function __construct(StreamInterface $stream, int $lineLength = 76, string $lineEnding = "\r\n")
    {
        $this->stream = $stream;
        $this->lineLength = $lineLength;
        $this->lineEnding = $lineEnding;
        $this->lineEndingLength = \strlen($this->lineEnding);
    }

    /**
     * Inserts the line ending character after each line length characters in
     * the passed string, making sure previously written bytes are taken into
     * account.
     */
    private function getChunkedString(string $string) : string
    {
        $firstLine = '';
        if ($this->tell() !== 0) {
            $next = $this->lineLength - ($this->position % ($this->lineLength + $this->lineEndingLength));
            if (\strlen($string) > $next) {
                $firstLine = \substr($string, 0, $next) . $this->lineEnding;
                $string = \substr($string, $next);
            }
        }
        // chunk_split always ends with the passed line ending
        $chunked = $firstLine . \chunk_split($string, $this->lineLength, $this->lineEnding);
        return \substr($chunked, 0, \strlen($chunked) - $this->lineEndingLength);
    }

    /**
     * Writes the passed string to the underlying stream, ensuring line endings
     * are inserted every "line length" characters in the string.
     *
     * @param string $string
     * @return int number of bytes written
     */
    public function write($string) : int
    {
        $chunked = $this->getChunkedString($string);
        $this->position += \strlen($chunked);
        return $this->stream->write($chunked);
    }

    /**
     * Inserts a final line ending character.
     */
    private function beforeClose() : void
    {
        if ($this->position !== 0) {
            $this->stream->write($this->lineEnding);
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
