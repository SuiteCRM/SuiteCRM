<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Message;

/**
 * Collection of static methods that return callables for common IMultiPart
 * child filters.
 *
 * @author Zaahid Bateson
 */
abstract class PartFilter
{
    /**
     * Provides an 'attachment' filter used by Message::getAttachmentPart.
     *
     * The method filters out the following types of parts:
     *  - text/plain and text/html parts that do not have an 'attachment'
     *    disposition
     *  - any part that returns true for isMultiPart()
     *  - any part that returns true for isSignaturePart()
     *
     * @return callable
     */
    public static function fromAttachmentFilter()
    {
        return function(IMessagePart $part) {
            $type = $part->getContentType();
            $disp = $part->getContentDisposition();
            if (\in_array($type, ['text/plain', 'text/html']) && $disp !== null && \strcasecmp($disp, 'inline') === 0) {
                return false;
            }
            return !(($part instanceof IMimePart)
                && ($part->isMultiPart() || $part->isSignaturePart()));
        };
    }

    /**
     * Provides a filter that keeps parts that contain a header of $name with a
     * value that matches $value (case insensitive).
     *
     * By default signed parts are excluded. Pass FALSE to the third parameter
     * to include them.
     *
     * @param string $name The header name to look up
     * @param string $value The value to match
     * @param bool $excludeSignedParts Optional signed parts exclusion (defaults
     *        to true).
     * @return callable
     */
    public static function fromHeaderValue($name, $value, $excludeSignedParts = true)
    {
        return function(IMessagePart $part) use ($name, $value, $excludeSignedParts) {
            if ($part instanceof IMimePart) {
                if ($excludeSignedParts && $part->isSignaturePart()) {
                    return false;
                }
                return (\strcasecmp($part->getHeaderValue($name, ''), $value) === 0);
            }
            return false;
        };
    }

    /**
     * Includes only parts that match the passed $mimeType in the return value
     * of a call to 'getContentType()'.
     *
     * @param string $mimeType Mime type of parts to find.
     * @return callable
     */
    public static function fromContentType($mimeType)
    {
        return function(IMessagePart $part) use ($mimeType) {
            return \strcasecmp($part->getContentType() ?: '', $mimeType) === 0;
        };
    }

    /**
     * Returns parts matching $mimeType that do not have a Content-Disposition
     * set to 'attachment'.
     *
     * @param string $mimeType Mime type of parts to find.
     * @return callable
     */
    public static function fromInlineContentType($mimeType)
    {
        return function(IMessagePart $part) use ($mimeType) {
            $disp = $part->getContentDisposition();
            return (\strcasecmp($part->getContentType() ?: '', $mimeType) === 0) && ($disp === null
                || \strcasecmp($disp, 'attachment') !== 0);
        };
    }

    /**
     * Finds parts with the passed disposition (matching against
     * IMessagePart::getContentDisposition()), optionally including
     * multipart parts and signed parts.
     *
     * @param string $disposition The disposition to find.
     * @param bool $includeMultipart Optionally include multipart parts by
     *        passing true (defaults to false).
     * @param bool $includeSignedParts Optionally include signed parts (defaults
     *        to false).
     * @return callable
     */
    public static function fromDisposition($disposition, $includeMultipart = false, $includeSignedParts = false)
    {
        return function(IMessagePart $part) use ($disposition, $includeMultipart, $includeSignedParts) {
            if (($part instanceof IMimePart) && ((!$includeMultipart && $part->isMultiPart()) || (!$includeSignedParts && $part->isSignaturePart()))) {
                return false;
            }
            $disp = $part->getContentDisposition();
            return ($disp !== null && \strcasecmp($disp, $disposition) === 0);
        };
    }
}
