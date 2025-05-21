<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Parser\Proxy;

/**
 * A bi-directional parser-to-part proxy for IMessage objects created by
 * NonMimeParser.
 *
 * @author Zaahid Bateson
 */
class ParserNonMimeMessageProxy extends ParserMessageProxy
{
    /**
     * @var int|null The next part's start position within the message's raw
     *      stream, or null if not set, not discovered, or there are no more
     *      parts.
     */
    protected $nextPartStart = null;

    /**
     * @var int The next part's unix file mode in a uu-encoded 'begin' line if
     *      exists, or null otherwise.
     */
    protected $nextPartMode = null;

    /**
     * @var string The next part's file name in a uu-encoded 'begin' line if
     *      exists, or null otherwise.
     */
    protected $nextPartFilename = null;

    /**
     * Returns the next part's start position within the message's raw stream,
     * or null if not set, not discovered, or there are no more parts under this
     * message.
     *
     * @return int|null The start position or null
     */
    public function getNextPartStart() : ?int
    {
        return $this->nextPartStart;
    }

    /**
     * Returns the next part's unix file mode in a uu-encoded 'begin' line if
     * one exists, or null otherwise.
     *
     * @return int|null The file mode or null
     */
    public function getNextPartMode() : ?int
    {
        return $this->nextPartMode;
    }

    /**
     * Returns the next part's filename in a uu-encoded 'begin' line if one
     * exists, or null otherwise.
     *
     * @return string|null The file name or null
     */
    public function getNextPartFilename() : ?string
    {
        return $this->nextPartFilename;
    }

    /**
     * Sets the next part's start position within the message's raw stream.
     *
     */
    public function setNextPartStart(int $nextPartStart) : self
    {
        $this->nextPartStart = $nextPartStart;
        return $this;
    }

    /**
     * Sets the next part's unix file mode from its 'begin' line.
     */
    public function setNextPartMode(int $nextPartMode) : self
    {
        $this->nextPartMode = $nextPartMode;
        return $this;
    }

    /**
     * Sets the next part's filename from its 'begin' line.
     *
     */
    public function setNextPartFilename(string $nextPartFilename) : self
    {
        $this->nextPartFilename = $nextPartFilename;
        return $this;
    }

    /**
     * Sets the next part start position, file mode, and filename to null
     */
    public function clearNextPart() : self
    {
        $this->nextPartStart = null;
        $this->nextPartMode = null;
        $this->nextPartFilename = null;
        return $this;
    }
}
