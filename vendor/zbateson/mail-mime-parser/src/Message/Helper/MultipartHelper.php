<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Message\Helper;

use ZBateson\MailMimeParser\Header\HeaderConsts;
use ZBateson\MailMimeParser\IMessage;
use ZBateson\MailMimeParser\Message\Factory\IMimePartFactory;
use ZBateson\MailMimeParser\Message\Factory\IUUEncodedPartFactory;
use ZBateson\MailMimeParser\Message\IMessagePart;
use ZBateson\MailMimeParser\Message\IMimePart;
use ZBateson\MailMimeParser\Message\PartFilter;

/**
 * Provides various routines to manipulate and create multipart messages from an
 * existing message (e.g. to make space for attachments in a message, or to
 * change a simple message to a multipart/alternative one, etc...)
 *
 * @author Zaahid Bateson
 */
class MultipartHelper extends AbstractHelper
{
    /**
     * @var GenericHelper a GenericHelper instance
     */
    private $genericHelper;

    public function __construct(IMimePartFactory $mimePartFactory, IUUEncodedPartFactory $uuEncodedPartFactory, GenericHelper $genericHelper)
    {
        parent::__construct($mimePartFactory, $uuEncodedPartFactory);
        $this->genericHelper = $genericHelper;
    }

    /**
     * Creates and returns a unique boundary.
     *
     * @param string $mimeType first 3 characters of a multipart type are used,
     *      e.g. REL for relative or ALT for alternative
     */
    public function getUniqueBoundary(string $mimeType) : string
    {
        $type = \ltrim(\strtoupper(\preg_replace('/^(multipart\/(.{3}).*|.*)$/i', '$2-', $mimeType)), '-');
        return \uniqid('----=MMP-' . $type . '-', true);
    }

    /**
     * Creates a unique mime boundary and assigns it to the passed part's
     * Content-Type header with the passed mime type.
     */
    public function setMimeHeaderBoundaryOnPart(IMimePart $part, string $mimeType) : self
    {
        $part->setRawHeader(
            HeaderConsts::CONTENT_TYPE,
            "$mimeType;\r\n\tboundary=\""
                . $this->getUniqueBoundary($mimeType) . '"'
        );
        $part->notify();
        return $this;
    }

    /**
     * Sets the passed message as multipart/mixed.
     *
     * If the message has content, a new part is created and added as a child of
     * the message.  The message's content and content headers are moved to the
     * new part.
     */
    public function setMessageAsMixed(IMessage $message) : self
    {
        if ($message->hasContent()) {
            $part = $this->genericHelper->createNewContentPartFrom($message);
            $message->addChild($part, 0);
        }
        $this->setMimeHeaderBoundaryOnPart($message, 'multipart/mixed');
        $atts = $message->getAllAttachmentParts();
        if (!empty($atts)) {
            foreach ($atts as $att) {
                $att->notify();
            }
        }
        return $this;
    }

    /**
     * Sets the passed message as multipart/alternative.
     *
     * If the message has content, a new part is created and added as a child of
     * the message.  The message's content and content headers are moved to the
     * new part.
     */
    public function setMessageAsAlternative(IMessage $message) : self
    {
        if ($message->hasContent()) {
            $part = $this->genericHelper->createNewContentPartFrom($message);
            $message->addChild($part, 0);
        }
        $this->setMimeHeaderBoundaryOnPart($message, 'multipart/alternative');
        return $this;
    }

    /**
     * Searches the passed $alternativePart for a part with the passed mime type
     * and returns its parent.
     *
     * Used for alternative mime types that have a multipart/mixed or
     * multipart/related child containing a content part of $mimeType, where
     * the whole mixed/related part should be removed.
     *
     * @param string $mimeType the content-type to find below $alternativePart
     * @param IMimePart $alternativePart The multipart/alternative part to look
     *        under
     * @return bool|IMimePart false if a part is not found
     */
    public function getContentPartContainerFromAlternative($mimeType, IMimePart $alternativePart)
    {
        $part = $alternativePart->getPart(0, PartFilter::fromInlineContentType($mimeType));
        $contPart = null;
        do {
            if ($part === null) {
                return false;
            }
            $contPart = $part;
            $part = $part->getParent();
        } while ($part !== $alternativePart);
        return $contPart;
    }

    /**
     * Removes all parts of $mimeType from $alternativePart.
     *
     * If $alternativePart contains a multipart/mixed or multipart/relative part
     * with other parts of different content-types, the multipart part is
     * removed, and parts of different content-types can optionally be moved to
     * the main message part.
     */
    public function removeAllContentPartsFromAlternative(IMessage $message, string $mimeType, IMimePart $alternativePart, bool $keepOtherContent) : bool
    {
        $rmPart = $this->getContentPartContainerFromAlternative($mimeType, $alternativePart);
        if ($rmPart === false) {
            return false;
        }
        if ($keepOtherContent && $rmPart->getChildCount() > 0) {
            $this->moveAllNonMultiPartsToMessageExcept($message, $rmPart, $mimeType);
            $alternativePart = $message->getPart(0, PartFilter::fromInlineContentType('multipart/alternative'));
        }
        $message->removePart($rmPart);
        if ($alternativePart !== null) {
            if ($alternativePart->getChildCount() === 1) {
                $this->genericHelper->replacePart($message, $alternativePart, $alternativePart->getChild(0));
            } elseif ($alternativePart->getChildCount() === 0) {
                $message->removePart($alternativePart);
            }
        }
        while ($message->getChildCount() === 1) {
            $this->genericHelper->replacePart($message, $message, $message->getChild(0));
        }
        return true;
    }

    /**
     * Creates a new mime part as a multipart/alternative and assigns the passed
     * $contentPart as a part below it before returning it.
     *
     * @return IMimePart the alternative part
     */
    public function createAlternativeContentPart(IMessage $message, IMessagePart $contentPart)
    {
        $altPart = $this->mimePartFactory->newInstance();
        $this->setMimeHeaderBoundaryOnPart($altPart, 'multipart/alternative');
        $message->removePart($contentPart);
        $message->addChild($altPart, 0);
        $altPart->addChild($contentPart, 0);
        return $altPart;
    }

    /**
     * Moves all parts under $from into this message except those with a
     * content-type equal to $exceptMimeType.  If the message is not a
     * multipart/mixed message, it is set to multipart/mixed first.
     */
    public function moveAllNonMultiPartsToMessageExcept(IMessage $message, IMimePart $from, string $exceptMimeType) : self
    {
        $parts = $from->getAllParts(function(IMessagePart $part) use ($exceptMimeType) {
            if ($part instanceof IMimePart && $part->isMultiPart()) {
                return false;
            }
            return \strcasecmp($part->getContentType(), $exceptMimeType) !== 0;
        });
        if (\strcasecmp($message->getContentType(), 'multipart/mixed') !== 0) {
            $this->setMessageAsMixed($message);
        }
        foreach ($parts as $key => $part) {
            $from->removePart($part);
            $message->addChild($part);
        }
        return $this;
    }

    /**
     * Enforces the message to be a mime message for a non-mime (e.g. uuencoded
     * or unspecified) message.  If the message has uuencoded attachments, sets
     * up the message as a multipart/mixed message and creates a separate
     * content part.
     */
    public function enforceMime(IMessage $message)
    {
        if (!$message->isMime()) {
            if ($message->getAttachmentCount()) {
                $this->setMessageAsMixed($message);
            } else {
                $message->setRawHeader(HeaderConsts::CONTENT_TYPE, "text/plain;\r\n\tcharset=\"iso-8859-1\"");
            }
            $message->setRawHeader(HeaderConsts::MIME_VERSION, '1.0');
        }
    }

    /**
     * Creates a multipart/related part out of 'inline' children of $parent and
     * returns it.
     *
     * @return IMimePart
     */
    public function createMultipartRelatedPartForInlineChildrenOf(IMimePart $parent)
    {
        $relatedPart = $this->mimePartFactory->newInstance();
        $this->setMimeHeaderBoundaryOnPart($relatedPart, 'multipart/related');
        foreach ($parent->getChildParts(PartFilter::fromDisposition('inline')) as $part) {
            $parent->removePart($part);
            $relatedPart->addChild($part);
        }
        $parent->addChild($relatedPart, 0);
        return $relatedPart;
    }

    /**
     * Finds an alternative inline part in the message and returns it if one
     * exists.
     *
     * If the passed $mimeType is text/plain, searches for a text/html part.
     * Otherwise searches for a text/plain part to return.
     *
     * @return IMimePart or null if not found
     */
    public function findOtherContentPartFor(IMessage $message, string $mimeType)
    {
        $altPart = $message->getPart(
            0,
            PartFilter::fromInlineContentType(($mimeType === 'text/plain') ? 'text/html' : 'text/plain')
        );
        if ($altPart !== null && $altPart->getParent() !== null && $altPart->getParent()->isMultiPart()) {
            $altPartParent = $altPart->getParent();
            if ($altPartParent->getChildCount(PartFilter::fromDisposition('inline')) !== 1) {
                $altPart = $this->createMultipartRelatedPartForInlineChildrenOf($altPartParent);
            }
        }
        return $altPart;
    }

    /**
     * Creates a new content part for the passed mimeType and charset, making
     * space by creating a multipart/alternative if needed
     *
     * @return \ZBateson\MailMimeParser\Message\IMimePart
     */
    public function createContentPartForMimeType(IMessage $message, string $mimeType, string $charset)
    {
        $mimePart = $this->mimePartFactory->newInstance();
        $mimePart->setRawHeader(HeaderConsts::CONTENT_TYPE, "$mimeType;\r\n\tcharset=\"$charset\"");
        $mimePart->setRawHeader(HeaderConsts::CONTENT_TRANSFER_ENCODING, 'quoted-printable');

        $this->enforceMime($message);
        $altPart = $this->findOtherContentPartFor($message, $mimeType);

        if ($altPart === $message) {
            $this->setMessageAsAlternative($message);
            $message->addChild($mimePart);
        } elseif ($altPart !== null) {
            $mimeAltPart = $this->createAlternativeContentPart($message, $altPart);
            $mimeAltPart->addChild($mimePart, 1);
        } else {
            $message->addChild($mimePart, 0);
        }

        return $mimePart;
    }

    /**
     * Creates and adds a IMimePart for the passed content and options as an
     * attachment.
     *
     * @param string|resource|\Psr\Http\Message\StreamInterface $resource
     */
    public function createAndAddPartForAttachment(IMessage $message, $resource, string $mimeType, string $disposition, ?string $filename = null, string $encoding = 'base64')
    {
        if ($filename === null) {
            $filename = 'file' . \uniqid();
        }

        $safe = \iconv('UTF-8', 'US-ASCII//translit//ignore', $filename);
        if ($message->isMime()) {
            $part = $this->mimePartFactory->newInstance();
            $part->setRawHeader(HeaderConsts::CONTENT_TRANSFER_ENCODING, $encoding);
            if (\strcasecmp($message->getContentType(), 'multipart/mixed') !== 0) {
                $this->setMessageAsMixed($message);
            }
            $part->setRawHeader(HeaderConsts::CONTENT_TYPE, "$mimeType;\r\n\tname=\"$safe\"");
            $part->setRawHeader(HeaderConsts::CONTENT_DISPOSITION, "$disposition;\r\n\tfilename=\"$safe\"");
        } else {
            $part = $this->uuEncodedPartFactory->newInstance();
            $part->setFilename($safe);
        }
        $part->setContent($resource);
        $message->addChild($part);
    }

    /**
     * Removes the content part of the message with the passed mime type.  If
     * there is a remaining content part and it is an alternative part of the
     * main message, the content part is moved to the message part.
     *
     * If the content part is part of an alternative part beneath the message,
     * the alternative part is replaced by the remaining content part,
     * optionally keeping other parts if $keepOtherContent is set to true.
     *
     * @return bool true on success
     */
    public function removeAllContentPartsByMimeType(IMessage $message, string $mimeType, bool $keepOtherContent = false) : bool
    {
        $alt = $message->getPart(0, PartFilter::fromInlineContentType('multipart/alternative'));
        if ($alt !== null) {
            return $this->removeAllContentPartsFromAlternative($message, $mimeType, $alt, $keepOtherContent);
        }
        $message->removeAllParts(PartFilter::fromInlineContentType($mimeType));
        return true;
    }

    /**
     * Removes the 'inline' part with the passed contentType, at the given index
     * defaulting to the first
     *
     * @return bool true on success
     */
    public function removePartByMimeType(IMessage $message, string $mimeType, int $index = 0) : bool
    {
        $parts = $message->getAllParts(PartFilter::fromInlineContentType($mimeType));
        $alt = $message->getPart(0, PartFilter::fromInlineContentType('multipart/alternative'));
        if ($parts === null || !isset($parts[$index])) {
            return false;
        } elseif (\count($parts) === 1) {
            return $this->removeAllContentPartsByMimeType($message, $mimeType, true);
        }
        $part = $parts[$index];
        $message->removePart($part);
        if ($alt !== null && $alt->getChildCount() === 1) {
            $this->genericHelper->replacePart($message, $alt, $alt->getChild(0));
        }
        return true;
    }

    /**
     * Either creates a mime part or sets the existing mime part with the passed
     * mimeType to $strongOrHandle.
     *
     * @param string|resource $stringOrHandle
     */
    public function setContentPartForMimeType(IMessage $message, string $mimeType, $stringOrHandle, string $charset) : self
    {
        $part = ($mimeType === 'text/html') ? $message->getHtmlPart() : $message->getTextPart();
        if ($part === null) {
            $part = $this->createContentPartForMimeType($message, $mimeType, $charset);
        } else {
            $contentType = $part->getContentType();
            $part->setRawHeader(HeaderConsts::CONTENT_TYPE, "$contentType;\r\n\tcharset=\"$charset\"");
        }
        $part->setContent($stringOrHandle);
        return $this;
    }
}
