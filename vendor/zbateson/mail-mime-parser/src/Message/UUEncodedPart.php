<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Message;

use ZBateson\MailMimeParser\MailMimeParser;

/**
 * Implementation of a non-mime message's uuencoded attachment part.
 *
 * @author Zaahid Bateson
 */
class UUEncodedPart extends NonMimePart implements IUUEncodedPart
{
    /**
     * @var int the unix file permission
     */
    protected $mode = null;

    /**
     * @var string the name of the file in the uuencoding 'header'.
     */
    protected $filename = null;

    public function __construct(?int $mode = null, ?string $filename = null, ?IMimePart $parent = null, ?PartStreamContainer $streamContainer = null)
    {
        if ($streamContainer === null) {
            $di = MailMimeParser::getDependencyContainer();
            $streamContainer = $di[\ZBateson\MailMimeParser\Message\PartStreamContainer::class];
            $streamFactory = $di[\ZBateson\MailMimeParser\Stream\StreamFactory::class];
            $streamContainer->setStream($streamFactory->newMessagePartStream($this));
        }
        parent::__construct(
            $streamContainer,
            $parent
        );
        $this->mode = $mode;
        $this->filename = $filename;
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
     * @return static
     */
    public function setFilename(string $filename)
    {
        $this->filename = $filename;
        $this->notify();
        return $this;
    }

    /**
     * Returns false.
     *
     * Although the part may be plain text, there is no reliable way of
     * determining its type since uuencoded 'begin' lines only include a file
     * name and no mime type.  The file name's extension may be a hint.
     *
     * @return false
     */
    public function isTextPart() : bool
    {
        return false;
    }

    /**
     * Returns 'application/octet-stream'.
     *
     * @return string
     */
    public function getContentType(string $default = 'application/octet-stream') : ?string
    {
        return 'application/octet-stream';
    }

    /**
     * Returns null
     */
    public function getCharset() : ?string
    {
        return null;
    }

    /**
     * Returns 'attachment'.
     */
    public function getContentDisposition(?string $default = 'attachment') : ?string
    {
        return 'attachment';
    }

    /**
     * Returns 'x-uuencode'.
     */
    public function getContentTransferEncoding(?string $default = 'x-uuencode') : ?string
    {
        return 'x-uuencode';
    }

    public function getUnixFileMode() : ?int
    {
        return $this->mode;
    }

    /**
     * @return static
     */
    public function setUnixFileMode(int $mode)
    {
        $this->mode = $mode;
        $this->notify();
        return $this;
    }
}
