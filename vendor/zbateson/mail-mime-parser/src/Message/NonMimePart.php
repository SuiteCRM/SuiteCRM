<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Message;

/**
 * Represents part of a non-mime message.
 *
 * @author Zaahid Bateson
 */
abstract class NonMimePart extends MessagePart
{
    /**
     * Returns true.
     *
     */
    public function isTextPart() : bool
    {
        return true;
    }

    /**
     * Returns text/plain
     *
     * @return string
     */
    public function getContentType(string $default = 'text/plain') : ?string
    {
        return $default;
    }

    /**
     * Returns ISO-8859-1
     *
     * @return string
     */
    public function getCharset() : ?string
    {
        return 'ISO-8859-1';
    }

    /**
     * Returns 'inline'.
     *
     * @return string
     */
    public function getContentDisposition(?string $default = 'inline') : ?string
    {
        return 'inline';
    }

    /**
     * Returns '7bit'.
     *
     * @return string
     */
    public function getContentTransferEncoding(?string $default = '7bit') : ?string
    {
        return '7bit';
    }

    /**
     * Returns false.
     *
     */
    public function isMime() : bool
    {
        return false;
    }

    /**
     * Returns the Content ID of the part.
     *
     * NonMimeParts do not have a Content ID, and so this simply returns null.
     *
     */
    public function getContentId() : ?string
    {
        return null;
    }
}
