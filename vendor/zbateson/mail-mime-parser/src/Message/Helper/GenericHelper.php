<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Message\Helper;

use ZBateson\MailMimeParser\Header\HeaderConsts;
use ZBateson\MailMimeParser\Header\IHeader;
use ZBateson\MailMimeParser\IMessage;
use ZBateson\MailMimeParser\MailMimeParser;
use ZBateson\MailMimeParser\Message\IMimePart;

/**
 * Provides common Message helper routines for Message manipulation.
 *
 * @author Zaahid Bateson
 */
class GenericHelper extends AbstractHelper
{
    /**
     * @var string[] non mime content fields that are not related to the content
     *      of a part.
     */
    private static $nonMimeContentFields = ['contentreturn', 'contentidentifier'];

    /**
     * Returns true if the passed header's name is a Content-* header other than
     * one defined in the static $nonMimeContentFields
     *
     */
    private function isMimeContentField(IHeader $header, array $exceptions = []) : bool
    {
        return (\stripos($header->getName(), 'Content') === 0
            && !\in_array(\strtolower(\str_replace('-', '', $header->getName())), \array_merge(self::$nonMimeContentFields, $exceptions)));
    }

    /**
     * Copies the passed $header from $from, to $to or sets the header to
     * $default if it doesn't exist in $from.
     *
     * @param string $header
     * @param string $default
     */
    public function copyHeader(IMimePart $from, IMimePart $to, $header, $default = null)
    {
        $fromHeader = $from->getHeader($header);
        $set = ($fromHeader !== null) ? $fromHeader->getRawValue() : $default;
        if ($set !== null) {
            $to->setRawHeader($header, $set);
        }
    }

    /**
     * Removes Content-* headers from the passed part, then detaches its content
     * stream.
     *
     * An exception is made for the obsolete Content-Return header, which isn't
     * isn't a MIME content field and so isn't removed.
     */
    public function removeContentHeadersAndContent(IMimePart $part) : self
    {
        foreach ($part->getAllHeaders() as $header) {
            if ($this->isMimeContentField($header)) {
                $part->removeHeader($header->getName());
            }
        }
        $part->detachContentStream();
        return $this;
    }

    /**
     * Copies Content-* headers from the $from header into the $to header. If
     * the Content-Type header isn't defined in $from, defaults to text/plain
     * with utf-8 and quoted-printable as its Content-Transfer-Encoding.
     *
     * An exception is made for the obsolete Content-Return header, which isn't
     * isn't a MIME content field and so isn't copied.
     *
     * @param bool $move
     */
    public function copyContentHeadersAndContent(IMimePart $from, IMimePart $to, $move = false)
    {
        $this->copyHeader($from, $to, HeaderConsts::CONTENT_TYPE, 'text/plain; charset=utf-8');
        if ($from->getHeader(HeaderConsts::CONTENT_TYPE) === null) {
            $this->copyHeader($from, $to, HeaderConsts::CONTENT_TRANSFER_ENCODING, 'quoted-printable');
        } else {
            $this->copyHeader($from, $to, HeaderConsts::CONTENT_TRANSFER_ENCODING);
        }
        foreach ($from->getAllHeaders() as $header) {
            if ($this->isMimeContentField($header, ['contenttype', 'contenttransferencoding'])) {
                $this->copyHeader($from, $to, $header->getName());
            }
        }
        if ($from->hasContent()) {
            $to->attachContentStream($from->getContentStream(), MailMimeParser::DEFAULT_CHARSET);
        }
        if ($move) {
            $this->removeContentHeadersAndContent($from);
        }
    }

    /**
     * Creates a new content part from the passed part, allowing the part to be
     * used for something else (e.g. changing a non-mime message to a multipart
     * mime message).
     *
     * @return IMimePart the newly-created IMimePart
     */
    public function createNewContentPartFrom(IMimePart $part)
    {
        $mime = $this->mimePartFactory->newInstance();
        $this->copyContentHeadersAndContent($part, $mime, true);
        return $mime;
    }

    /**
     * Copies type headers (Content-Type, Content-Disposition,
     * Content-Transfer-Encoding) from the $from MimePart to $to.  Attaches the
     * content resource handle of $from to $to, and loops over child parts,
     * removing them from $from and adding them to $to.
     *
     */
    public function movePartContentAndChildren(IMimePart $from, IMimePart $to)
    {
        $this->copyContentHeadersAndContent($from, $to, true);
        if ($from->getChildCount() > 0) {
            foreach ($from->getChildIterator() as $child) {
                $from->removePart($child);
                $to->addChild($child);
            }
        }
    }

    /**
     * Replaces the $part IMimePart with $replacement.
     *
     * Essentially removes $part from its parent, and adds $replacement in its
     * same position.  If $part is the IMessage, then $part can't be removed and
     * replaced, and instead $replacement's type headers are copied to $message,
     * and any children below $replacement are added directly below $message.
     */
    public function replacePart(IMessage $message, IMimePart $part, IMimePart $replacement) : self
    {
        $position = $message->removePart($replacement);
        if ($part === $message) {
            $this->movePartContentAndChildren($replacement, $message);
            return $this;
        }
        $parent = $part->getParent();
        $parent->addChild($replacement, $position);
        $parent->removePart($part);

        return $this;
    }
}
