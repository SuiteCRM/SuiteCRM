<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Parser\Part;

use ZBateson\MailMimeParser\Message\PartHeaderContainer;

/**
 * Header container representing the start line of a uu-encoded part.
 *
 * The line may contain a unix file mode and a filename.
 *
 * @author Zaahid Bateson
 */
class UUEncodedPartHeaderContainer extends PartHeaderContainer
{
    /**
     * @var int the unix file permission
     */
    protected $mode = null;

    /**
     * @var string the name of the file in the uuencoding 'header'.
     */
    protected $filename = null;

    /**
     * Returns the file mode included in the uuencoded 'begin' line for this
     * part.
     */
    public function getUnixFileMode() : ?int
    {
        return $this->mode;
    }

    /**
     * Sets the unix file mode for the uuencoded 'begin' line.
     *
     * @return static
     */
    public function setUnixFileMode(int $mode)
    {
        $this->mode = $mode;
        return $this;
    }

    /**
     * Returns the filename included in the uuencoded 'begin' line for this
     * part.
     *
     * @return string
     */
    public function getFilename() : ?string
    {
        return $this->filename;
    }

    /**
     * Sets the filename included in the uuencoded 'begin' line.
     *
     * @return static
     */
    public function setFilename(string $filename)
    {
        $this->filename = $filename;
        return $this;
    }
}
