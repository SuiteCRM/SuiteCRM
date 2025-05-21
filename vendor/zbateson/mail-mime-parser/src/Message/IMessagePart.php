<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Message;

use Psr\Http\Message\StreamInterface;
use SplSubject;
use ZBateson\MailMimeParser\MailMimeParser;

/**
 * An interface representing a single part of an email.
 *
 * The base type for a message or any child part of a message.  The part may
 * contain content, have a parent, and identify the type of content (e.g.
 * mime-type or charset) agnostically.
 *
 * The interface extends SplSubject -- any modifications to a message must
 * notify any attached observers.
 *
 * @author Zaahid Bateson
 */
interface IMessagePart extends SplSubject
{
    /**
     * Returns this part's parent.
     *
     * @return IMimePart the parent part
     */
    public function getParent();

    /**
     * Returns true if the part contains a 'body' (content).
     *
     */
    public function hasContent() : bool;

    /**
     * Returns true if the content of this part is plain text.
     *
     */
    public function isTextPart() : bool;

    /**
     * Returns the mime type of the content, or $default if one is not set.
     *
     * @param string $default Optional override for the default return value of
     *        'text/plain.
     * @return string the mime type
     */
    public function getContentType(string $default = 'text/plain') : ?string;

    /**
     * Returns the charset of the content, or null if not applicable/defined.
     *
     * @return string|null the charset
     */
    public function getCharset() : ?string;

    /**
     * Returns the content's disposition, or returns the value of $default if
     * not defined.
     *
     * @param string $default Optional default value to return if not
     *        applicable/defined
     * @return string|null the disposition.
     */
    public function getContentDisposition(?string $default = null) : ?string;

    /**
     * Returns the content transfer encoding used to encode the content on this
     * part, or the value of $default if not defined.
     *
     * @param $default Optional default value to return if not
     *        applicable/defined
     * @return string|null the transfer encoding defined for the part.
     */
    public function getContentTransferEncoding(?string $default = null) : ?string;

    /**
     * Returns the Content ID of the part, or null if not defined.
     *
     * @return string|null the content ID.
     */
    public function getContentId() : ?string;

    /**
     * Returns a filename for the part if one is defined, or null otherwise.
     *
     * @return string|null the file name
     */
    public function getFilename() : ?string;

    /**
     * Returns true if the current part is a mime part.
     *
     */
    public function isMime() : bool;

    /**
     * Overrides the default character set used for reading content from content
     * streams in cases where a user knows the source charset is not what is
     * specified.
     *
     * If set, the returned value from {@see IMessagePart::getCharset()} must be
     * ignored during subsequent read operations and streams created out of this
     * part's content.
     *
     * Note that setting an override on an
     * {@see \ZBateson\MailMimeParser\IMessage} and calling getTextStream,
     * getTextContent, getHtmlStream or getHtmlContent will not be applied to
     * those sub-parts, unless the text/html part is the IMessage itself.
     * Instead, {@see \ZBateson\MailMimeParser\IMessage::getTextPart()} should
     * be called, and setCharsetOverride called on the returned IMessagePart.
     *
     * @see IMessagePart::getContentStream() to get the content stream.
     * @param string $charsetOverride the actual charset of the content.
     * @param bool $onlyIfNoCharset if true, $charsetOverride is used only if
     *        getCharset returns null.
     */
    public function setCharsetOverride(string $charsetOverride, bool $onlyIfNoCharset = false);

    /**
     * Returns the StreamInterface for the part's content or null if the part
     * doesn't have a content section.
     *
     * To get a stream without charset conversion if you know the part's content
     * contains a binary stream, call {@see self::getBinaryContentStream()}
     * instead.
     *
     * The library automatically handles decoding and charset conversion (to the
     * target passed $charset) based on the part's transfer encoding as returned
     * by {@see IMessagePart::getContentTransferEncoding()} and the part's
     * charset as returned by {@see IMessagePart::getCharset()}.  The returned
     * stream is ready to be read from directly.
     *
     * Note that the returned Stream is a shared object.  If called multiple
     * times with the same $charset, and the value of the part's
     * Content-Transfer-Encoding header has not changed, the stream will be
     * rewound.  This would affect other existing variables referencing the
     * stream, for example:
     *
     * ```php
     * // assuming $part is a part containing the following
     * // string for its content: '12345678'
     * $stream = $part->getContentStream();
     * $someChars = $part->read(4);
     *
     * $stream2 = $part->getContentStream();
     * $moreChars = $part->read(4);
     * echo ($someChars === $moreChars);    //1
     * ```
     *
     * In this case the Stream was rewound, and $stream's second call to read 4
     * bytes reads the same first 4.
     *
     * @see IMessagePart::getBinaryContentStream() to get the content stream
     *      without any charset conversions.
     * @see IMessagePart::saveContent() to save the binary contents to file.
     * @see IMessagePart::setCharsetOverride() to override the charset of the
     *      content and ignore the charset returned from calling
     *      IMessagePart::getCharset() when reading.
     * @param string $charset Optional charset for the returned stream.
     * @return StreamInterface|null the stream
     */
    public function getContentStream(string $charset = MailMimeParser::DEFAULT_CHARSET);

    /**
     * Returns the raw data stream for the current part, if it exists, or null
     * if there's no content associated with the stream.
     *
     * This is basically the same as calling
     * {@see IMessagePart::getContentStream()}, except no automatic charset
     * conversion is done.  Note that for non-text streams, this doesn't have an
     * effect, as charset conversion is not performed in that case, and is
     * useful only when:
     *
     * - The charset defined is not correct, and the conversion produces errors;
     *   or
     * - You'd like to read the raw contents without conversion, for instance to
     *   save it to file or allow a user to download it as-is (in a download
     *   link for example).
     *
     * @see IMessagePart::getContentStream() to get the content stream with
     *      charset conversions applied.
     * @see IMessagePart::getBinaryContentResourceHandle() to get a resource
     *      handle instead.
     * @see IMessagePart::saveContent() to save the binary contents to file.
     * @return StreamInterface|null the stream
     */
    public function getBinaryContentStream();

    /**
     * Returns a resource handle for the content's raw data stream, or null if
     * the part doesn't have a content stream.
     *
     * The method wraps a call to {@see IMessagePart::getBinaryContentStream()}
     * and returns a resource handle for the returned Stream.
     *
     * @see IMessagePart::getBinaryContentStream() to get a stream instead.
     * @see IMessagePart::saveContent() to save the binary contents to file.
     * @return resource|null the resource
     */
    public function getBinaryContentResourceHandle();

    /**
     * Saves the binary content of the stream to the passed file, resource or
     * stream.
     *
     * Note that charset conversion is not performed in this case, and the
     * contents of the part are saved in their binary format as transmitted (but
     * after any content-transfer decoding is performed).  {@see
     * IMessagePart::getBinaryContentStream()} for a more detailed description
     * of the stream.
     *
     * If the passed parameter is a string, it's assumed to be a filename to
     * write to.  The file is opened in 'w+' mode, and closed before returning.
     *
     * When passing a resource or Psr7 Stream, the resource is not closed, nor
     * rewound.
     *
     * @see IMessagePart::getContentStream() to get the content stream with
     *      charset conversions applied.
     * @see IMessagePart::getBinaryContentStream() to get the content as a
     *      binary stream.
     * @see IMessagePart::getBinaryContentResourceHandle() to get the content as
     *      a resource handle.
     * @param string|resource|StreamInterface $filenameResourceOrStream
     */
    public function saveContent($filenameResourceOrStream);

    /**
     * Shortcut to reading stream content and assigning it to a string.  Returns
     * null if the part doesn't have a content stream.
     *
     * The returned string is encoded to the passed $charset character encoding.
     *
     * @see IMessagePart::getContentStream()
     * @param string $charset the target charset for the returned string
     * @return string|null the content
     */
    public function getContent(string $charset = MailMimeParser::DEFAULT_CHARSET) : ?string;

    /**
     * Attaches the stream or resource handle for the part's content.  The
     * stream is closed when another stream is attached, or the MimePart is
     * destroyed.
     *
     * @see IMessagePart::setContent() to pass a string as the content.
     * @see IMessagePart::getContentStream() to get the content stream.
     * @see IMessagePart::detachContentStream() to detach the content stream.
     * @param StreamInterface $stream the content
     * @param string $streamCharset the charset of $stream
     */
    public function attachContentStream(StreamInterface $stream, string $streamCharset = MailMimeParser::DEFAULT_CHARSET);

    /**
     * Detaches the content stream.
     *
     * @see IMessagePart::getContentStream() to get the content stream.
     * @see IMessagePart::attachContentStream() to attach a content stream.
     */
    public function detachContentStream();

    /**
     * Sets the content of the part to the passed string, resource, or stream.
     *
     * @see IMessagePart::getContentStream() to get the content stream.
     * @see IMessagePart::attachContentStream() to attach a content stream.
     * @see IMessagePart::detachContentStream() to detach the content stream.
     * @param string|resource|StreamInterface $resource the content.
     * @param string $resourceCharset the charset of the passed $resource.
     */
    public function setContent($resource, string $resourceCharset = MailMimeParser::DEFAULT_CHARSET);

    /**
     * Returns a resource handle for the string representation of this part,
     * containing its headers, content and children.  For an IMessage, this
     * would be the entire RFC822 (or greater) email.
     *
     * If the part has not been modified and represents a parsed part, the
     * original stream should be returned.  Otherwise a stream representation of
     * the part including its modifications should be returned.  This insures
     * that an unmodified, signed message could be passed on that way even after
     * parsing and reading.
     *
     * The returned stream is not guaranteed to be RFC822 (or greater) compliant
     * for the following reasons:
     *
     *  - The original email or part, if not modified, is returned as-is and may
     *    not be compliant.
     *  - Although certain parts may have been modified, an original unmodified
     *    header from the original email or part may not be compliant.
     *  - A user may set headers in a non-compliant format.
     *
     * @see IMessagePart::getStream() to get a Psr7 StreamInterface instead of a
     *      resource handle.
     * @see IMessagePart::__toString() to write the part to a string and return
     *      it.
     * @see IMessage::save() to write the part to a file, resource handle or
     *      Psr7 stream.
     * @return resource the resource handle containing the part.
     */
    public function getResourceHandle();

    /**
     * Returns a Psr7 StreamInterface for the string representation of this
     * part, containing its headers, content and children.
     *
     * If the part has not been modified and represents a parsed part, the
     * original stream should be returned.  Otherwise a stream representation of
     * the part including its modifications should be returned.  This insures
     * that an unmodified, signed message could be passed on that way even after
     * parsing and reading.
     *
     * The returned stream is not guaranteed to be RFC822 (or greater) compliant
     * for the following reasons:
     *
     *  - The original email or part, if not modified, is returned as-is and may
     *    not be compliant.
     *  - Although certain parts may have been modified, an original unmodified
     *    header from the original email or part may not be compliant.
     *  - A user may set headers in a non-compliant format.
     *
     * @see IMessagePart::getResourceHandle() to get a resource handle.
     * @see IMessagePart::__toString() to write the part to a string and return
     *      it.
     * @see IMessage::save() to write the part to a file, resource handle or
     *      Psr7 stream.
     * @return StreamInterface the stream containing the part.
     */
    public function getStream();

    /**
     * Writes a string representation of this part, including its headers,
     * content and children to the passed file, resource, or stream.
     *
     * If the part has not been modified and represents a parsed part, the
     * original stream should be written to the file.  Otherwise a stream
     * representation of the part including its modifications should be written.
     * This insures that an unmodified, signed message could be passed on this
     * way even after parsing and reading.
     *
     * The written stream is not guaranteed to be RFC822 (or greater) compliant
     * for the following reasons:
     *
     *  - The original email or part, if not modified, is returned as-is and may
     *    not be compliant.
     *  - Although certain parts may have been modified, an original unmodified
     *    header from the original email or part may not be compliant.
     *  - A user may set headers in a non-compliant format.
     *
     * If the passed $filenameResourceOrStream is a string, it's assumed to be a
     * filename to write to.
     *
     * When passing a resource or Psr7 Stream, the resource is not closed, nor
     * rewound after being written to.
     *
     * @see IMessagePart::getResourceHandle() to get a resource handle.
     * @see IMessagePart::__toString() to get the part in a string.
     * @see IMessage::save() to write the part to a file, resource handle or
     *      Psr7 stream.
     * @param string|resource|StreamInterface $filenameResourceOrStream the
     *        file, resource, or stream to write to.
     * @param string $filemode Optional filemode to open a file in (if
     *        $filenameResourceOrStream is a string)
     */
    public function save($filenameResourceOrStream, string $filemode = 'w+');

    /**
     * Returns the message/part as a string, containing its headers, content and
     * children.
     *
     * Convenience method for calling getContents() on
     * {@see IMessagePart::getStream()}.
     *
     * @see IMessagePart::getStream() to get a Psr7 StreamInterface instead of a
     *      string.
     * @see IMessagePart::getResourceHandle() to get a resource handle.
     * @see IMessage::save() to write the part to a file, resource handle or
     *      Psr7 stream.
     */
    public function __toString() : string;
}
