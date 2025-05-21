<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser;

use ZBateson\MailMimeParser\Message\IMimePart;

/**
 * An interface representing an email message.
 *
 * Defines an interface to retrieve content, attachments and other parts of an
 * email message.
 *
 * @author Zaahid Bateson
 */
interface IMessage extends IMimePart
{
    /**
     * Returns the inline text/plain IMessagePart for a message.
     *
     * If the message contains more than one text/plain 'inline' part, the
     * default behavior is to return the first part.  Additional parts can be
     * returned by passing a 0-based index.
     *
     * If there are no inline text/plain parts in this message, null is
     * returned.
     *
     * @see IMessage::getTextPartCount() to get a count of text parts.
     * @see IMessage::getTextStream() to get the text content stream directly.
     * @see IMessage::getTextContent() to get the text content in a string.
     * @see IMessage::getHtmlPart() to get the HTML part(s).
     * @see IMessage::getHtmlPartCount() to get a count of html parts.
     * @param int $index Optional index of part to return.
     * @return \ZBateson\MailMimeParser\Message\IMessagePart|null
     */
    public function getTextPart($index = 0);

    /**
     * Returns the number of inline text/plain parts this message contains.
     *
     * @see IMessage::getTextPart() to get the text part(s).
     * @see IMessage::getHtmlPart() to get the HTML part(s).
     * @see IMessage::getHtmlPartCount() to get a count of html parts.
     * @return int
     */
    public function getTextPartCount();

    /**
     * Returns the inline text/html IMessagePart for a message.
     *
     * If the message contains more than one text/html 'inline' part, the
     * default behavior is to return the first part.  Additional parts can be
     * returned by passing a 0-based index.
     *
     * If there are no inline text/plain parts in this message, null is
     * returned.
     *
     * @see IMessage::getHtmlStream() to get the html content stream directly.
     * @see IMessage::getHtmlStream() to get the html content in a string.
     * @see IMessage::getTextPart() to get the text part(s).
     * @see IMessage::getTextPartCount() to get a count of text parts.
     * @see IMessage::getHtmlPartCount() to get a count of html parts.
     * @param int $index Optional index of part to return.
     * @return \ZBateson\MailMimeParser\Message\IMessagePart|null
     */
    public function getHtmlPart($index = 0);

    /**
     * Returns the number of inline text/html parts this message contains.
     *
     * @see IMessage::getTextPart() to get the text part(s).
     * @see IMessage::getTextPartCount() to get a count of text parts.
     * @see IMessage::getHtmlPart() to get the HTML part(s).
     * @return int
     */
    public function getHtmlPartCount();

    /**
     * Returns a Psr7 Stream for the 'inline' text/plain content.
     *
     * If the message contains more than one text/plain 'inline' part, the
     * default behavior is to return the first part.  The streams for additional
     * parts can be returned by passing a 0-based index.
     *
     * If a part at the passed index doesn't exist, null is returned.
     *
     * @see IMessage::getTextPart() to get the text part(s).
     * @see IMessage::getTextContent() to get the text content in a string.
     * @param int $index Optional 0-based index of inline text part stream.
     * @param string $charset Optional charset to encode the stream with.
     * @return \Psr\Http\Message\StreamInterface|null
     */
    public function getTextStream($index = 0, $charset = MailMimeParser::DEFAULT_CHARSET);

    /**
     * Returns the content of the inline text/plain part as a string.
     *
     * If the message contains more than one text/plain 'inline' part, the
     * default behavior is to return the first part.  The content for additional
     * parts can be returned by passing a 0-based index.
     *
     * If a part at the passed index doesn't exist, null is returned.
     *
     * @see IMessage::getTextPart() to get the text part(s).
     * @see IMessage::getTextStream() to get the text content stream directly.
     * @param int $index Optional 0-based index of inline text part content.
     * @param string $charset Optional charset for the returned string to be
     *        encoded in.
     * @return string|null
     */
    public function getTextContent($index = 0, $charset = MailMimeParser::DEFAULT_CHARSET);

    /**
     * Returns a Psr7 Stream for the 'inline' text/html content.
     *
     * If the message contains more than one text/html 'inline' part, the
     * default behavior is to return the first part.  The streams for additional
     * parts can be returned by passing a 0-based index.
     *
     * If a part at the passed index doesn't exist, null is returned.
     *
     * @see IMessage::getHtmlPart() to get the html part(s).
     * @see IMessage::getHtmlContent() to get the html content in a string.
     * @param int $index Optional 0-based index of inline html part stream.
     * @param string $charset Optional charset to encode the stream with.
     * @return \Psr\Http\Message\StreamInterface|null
     */
    public function getHtmlStream($index = 0, $charset = MailMimeParser::DEFAULT_CHARSET);

    /**
     * Returns the content of the inline text/html part as a string.
     *
     * If the message contains more than one text/html 'inline' part, the
     * default behavior is to return the first part.  The content for additional
     * parts can be returned by passing a 0-based index.
     *
     * If a part at the passed index doesn't exist, null is returned.
     *
     * @see IMessage::getHtmlPart() to get the html part(s).
     * @see IMessage::getHtmlStream() to get the html content stream directly.
     * @param int $index Optional 0-based index of inline html part content.
     * @param string $charset Optional charset for the returned string to be
     *        encoded in.
     * @return string|null
     */
    public function getHtmlContent($index = 0, $charset = MailMimeParser::DEFAULT_CHARSET);

    /**
     * Sets the text/plain part of the message to the passed $resource, either
     * creating a new part if one doesn't exist for text/plain, or assigning the
     * value of $resource to an existing text/plain part.
     *
     * The optional $contentTypeCharset parameter is the charset for the
     * text/plain part's Content-Type, not the charset of the passed $resource.
     * $resource must be encoded in UTF-8 regardless of the target charset.
     *
     * @see IMessage::setHtmlPart() to set the html part
     * @see IMessage::removeTextPart() to remove a text part
     * @see IMessage::removeAllTextParts() to remove all text parts
     * @param string|resource|\Psr\Http\Message\StreamInterface $resource UTF-8
     *        encoded content.
     * @param string $contentTypeCharset the charset to use as the text/plain
     *        part's content-type header charset value.
     */
    public function setTextPart($resource, string $contentTypeCharset = 'UTF-8');

    /**
     * Sets the text/html part of the message to the passed $resource, either
     * creating a new part if one doesn't exist for text/html, or assigning the
     * value of $resource to an existing text/html part.
     *
     * The optional $contentTypeCharset parameter is the charset for the
     * text/html part's Content-Type, not the charset of the passed $resource.
     * $resource must be encoded in UTF-8 regardless of the target charset.
     *
     * @see IMessage::setTextPart() to set the text part
     * @see IMessage::removeHtmlPart() to remove an html part
     * @see IMessage::removeAllHtmlParts() to remove all html parts
     * @param string|resource|\Psr\Http\Message\StreamInterface $resource UTF-8
     *        encoded content.
     * @param string $contentTypeCharset the charset to use as the text/html
     *        part's content-type header charset value.
     */
    public function setHtmlPart($resource, string $contentTypeCharset = 'UTF-8');

    /**
     * Removes the text/plain part of the message at the passed index if one
     * exists (defaults to first part if an index isn't passed).
     *
     * Returns true if a part exists at the passed index and has been removed.
     *
     * @see IMessage::setTextPart() to set the text part
     * @see IMessage::removeHtmlPart() to remove an html part
     * @see IMessage::removeAllTextParts() to remove all text parts
     * @param int $index Optional 0-based index of inline text part to remove.
     * @return bool true on success
     */
    public function removeTextPart(int $index = 0) : bool;

    /**
     * Removes all text/plain inline parts in this message.
     *
     * If the message contains a multipart/alternative part, the text parts are
     * removed from below the alternative part only.  If there is only one
     * remaining part after that, it is moved up, replacing the
     * multipart/alternative part.
     *
     * If the multipart/alternative part further contains a multipart/related
     * (or mixed) part which holds an inline text part, only parts from that
     * child multipart are removed, and if the passed
     * $moveRelatedPartsBelowMessage is true, any non-text parts are moved to be
     * below the message directly (changing the message into a multipart/mixed
     * message if need be).
     *
     * For more control, call
     * {@see \ZBateson\MailMimeParser\Message\IMessagePart::removePart()} with
     * parts you wish to remove.
     *
     * @see IMessage::setTextPart() to set the text part
     * @see IMessage::removeTextPart() to remove a text part
     * @see IMessage::removeAllHtmlParts() to remove all html parts
     * @param bool $moveRelatedPartsBelowMessage Optionally pass false to remove
     *        related parts.
     * @return bool true on success
     */
    public function removeAllTextParts(bool $moveRelatedPartsBelowMessage = true) : bool;

    /**
     * Removes the text/html part of the message at the passed index if one
     * exists (defaults to first part if an index isn't passed).
     *
     * Returns true if a part exists at the passed index and has been removed.
     *
     * @see IMessage::setHtmlPart() to set the html part
     * @see IMessage::removeTextPart() to remove a text part
     * @see IMessage::removeAllHtmlParts() to remove all html parts
     * @param int $index Optional 0-based index of inline html part to remove.
     * @return bool true on success
     */
    public function removeHtmlPart(int $index = 0) : bool;

    /**
     * Removes all text/html inline parts in this message.
     *
     * If the message contains a multipart/alternative part, the html parts are
     * removed from below the alternative part only.  If there is only one
     * remaining part after that, it is moved up, replacing the
     * multipart/alternative part.
     *
     * If the multipart/alternative part further contains a multipart/related
     * (or mixed) part which holds an inline html part, only parts from that
     * child multipart are removed, and if the passed
     * $moveRelatedPartsBelowMessage is true, any non-html parts are moved to be
     * below the message directly (changing the message into a multipart/mixed
     * message if need be).
     *
     * For more control, call
     * {@see \ZBateson\MailMimeParser\Message\IMessagePart::removePart()} with
     * parts you wish to remove.
     *
     * @see IMessage::setHtmlPart() to set the html part
     * @see IMessage::removeHtmlPart() to remove an html part
     * @see IMessage::removeAllTextParts() to remove all html parts
     * @param bool $moveRelatedPartsBelowMessage Optionally pass false to remove
     *        related parts.
     * @return bool true on success
     */
    public function removeAllHtmlParts(bool $moveRelatedPartsBelowMessage = true) : bool;

    /**
     * Returns the attachment part at the given 0-based index, or null if none
     * is set.
     *
     * The method returns all parts other than the main content part for a
     * non-mime message, and all parts under a mime message except:
     *  - text/plain and text/html parts with a Content-Disposition not set to
     *    'attachment'
     *  - all multipart/* parts
     *  - any signature part
     *
     * @see IMessage::getAllAttachmentParts() to get an array of all parts.
     * @see IMessage::getAttachmentCount() to get the number of attachments.
     * @param int $index the 0-based index of the attachment part to return.
     * @return \ZBateson\MailMimeParser\Message\IMessagePart|null
     */
    public function getAttachmentPart(int $index);

    /**
     * Returns all attachment parts.
     *
     * The method returns all parts other than the main content part for a
     * non-mime message, and all parts under a mime message except:
     *  - text/plain and text/html parts with a Content-Disposition not set to
     *    'attachment'
     *  - all multipart/* parts
     *  - any signature part
     *
     * @see IMessage::getAllAttachmentPart() to get a single attachment.
     * @see IMessage::getAttachmentCount() to get the number of attachments.
     * @return \ZBateson\MailMimeParser\Message\IMessagePart[]
     */
    public function getAllAttachmentParts();

    /**
     * Returns the number of attachments available.
     *
     * @see IMessage::getAllAttachmentPart() to get a single attachment.
     * @see IMessage::getAllAttachmentParts() to get an array of all parts.
     * @return int
     */
    public function getAttachmentCount();

    /**
     * Adds an attachment part for the passed raw data string, handle, or stream
     * and given parameters.
     *
     * Note that $disposition must be one of 'inline' or 'attachment', and will
     * default to 'attachment' if a different value is passed.
     *
     * @param string|resource|\Psr\Http\Message\StreamInterface $resource the
     *        part's content
     * @param string $mimeType the mime-type of the attachment
     * @param string $filename Optional filename (to set relevant header params)
     * @param string $disposition Optional Content-Disposition value.
     * @param string $encoding defaults to 'base64', only applied for a mime
     *        email
     */
    public function addAttachmentPart($resource, string $mimeType, ?string $filename = null, string $disposition = 'attachment', string $encoding = 'base64');

    /**
     * Adds an attachment part using the passed file.
     *
     * Essentially creates a psr7 stream and calls
     * {@see IMessage::addAttachmentPart}.
     *
     * Note that $disposition must be one of 'inline' or 'attachment', and will
     * default to 'attachment' if a different value is passed.
     *
     * @param string $filePath file to attach
     * @param string $mimeType the mime-type of the attachment
     * @param string $filename Optional filename (to set relevant header params)
     * @param string $disposition Optional Content-Disposition value.
     * @param string $encoding defaults to 'base64', only applied for a mime
     *        email
     */
    public function addAttachmentPartFromFile($filePath, string $mimeType, ?string $filename = null, string $disposition = 'attachment', string $encoding = 'base64');

    /**
     * Removes the attachment at the given index.
     *
     * Attachments are considered to be all parts other than the main content
     * part for a non-mime message, and all parts under a mime message except:
     *  - text/plain and text/html parts with a Content-Disposition not set to
     *    'attachment'
     *  - all multipart/* parts
     *  - any signature part
     *
     */
    public function removeAttachmentPart(int $index);

    /**
     * Returns a stream that can be used to read the content part of a signed
     * message, which can be used to sign an email or verify a signature.
     *
     * The method simply returns the stream for the first child.  No
     * verification of whether the message is in fact a signed message is
     * performed.
     *
     * Note that unlike getSignedMessageAsString, getSignedMessageStream doesn't
     * replace new lines, and before calculating a signature, LFs not preceded
     * by CR should be replaced with CRLFs.
     *
     * @see IMessage::getSignedMessageAsString to get a string with CRLFs
     *      normalized
     * @return \Psr\Http\Message\StreamInterface or null if the message doesn't
     *         have any children
     */
    public function getSignedMessageStream();

    /**
     * Returns a string containing the entire body of a signed message for
     * verification or calculating a signature.
     *
     * Non-CRLF new lines are replaced to always be CRLF.
     *
     * @see IMessage::setAsMultipartSigned to make the message a
     *      multipart/signed message.
     * @return string or null if the message doesn't have any children
     */
    public function getSignedMessageAsString();

    /**
     * Returns the signature part of a multipart/signed message or null.
     *
     * The signature part is determined to always be the 2nd child of a
     * multipart/signed message, the first being the 'body'.
     *
     * Using the 'protocol' parameter of the Content-Type header is unreliable
     * in some instances (for instance a difference of x-pgp-signature versus
     * pgp-signature).
     *
     * @return IMimePart
     */
    public function getSignaturePart();

    /**
     * Turns the message into a multipart/signed message, moving the actual
     * message into a child part, sets the content-type of the main message to
     * multipart/signed and adds an empty signature part as well.
     *
     * After calling setAsMultipartSigned, call getSignedMessageAsString to
     * get the normalized string content to be used for calculated the message's
     * hash.
     *
     * @see IMessage::getSignedMessageAsString
     * @param string $micalg The Message Integrity Check algorithm being used
     * @param string $protocol The mime-type of the signature body
     */
    public function setAsMultipartSigned(string $micalg, string $protocol);

    /**
     * Sets the signature body of the message to the passed $body for a
     * multipart/signed message.
     *
     * @param string $body the message's hash
     */
    public function setSignature(string $body);
}
