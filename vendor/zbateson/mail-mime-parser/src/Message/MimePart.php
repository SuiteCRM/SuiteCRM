<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Message;

use ZBateson\MailMimeParser\Header\HeaderConsts;
use ZBateson\MailMimeParser\Header\IHeader;
use ZBateson\MailMimeParser\Header\ParameterHeader;
use ZBateson\MailMimeParser\IMessage;
use ZBateson\MailMimeParser\MailMimeParser;

/**
 * A mime email message part.
 *
 * A MIME part may contain any combination of headers, content and children.
 *
 * @author Zaahid Bateson
 */
class MimePart extends MultiPart implements IMimePart
{
    /**
     * @var PartHeaderContainer Container for this part's headers.
     */
    protected $headerContainer;

    public function __construct(
        ?IMimePart $parent = null,
        ?PartStreamContainer $streamContainer = null,
        ?PartHeaderContainer $headerContainer = null,
        ?PartChildrenContainer $partChildrenContainer = null
    ) {
        $setStream = false;
        $di = MailMimeParser::getDependencyContainer();
        if ($streamContainer === null || $headerContainer === null || $partChildrenContainer === null) {
            $headerContainer = $di[\ZBateson\MailMimeParser\Message\PartHeaderContainer::class];
            $streamContainer = $di[\ZBateson\MailMimeParser\Message\PartStreamContainer::class];
            $partChildrenContainer = $di[\ZBateson\MailMimeParser\Message\PartChildrenContainer::class];
            $setStream = true;
        }
        parent::__construct(
            $parent,
            $streamContainer,
            $partChildrenContainer
        );
        if ($setStream) {
            $streamFactory = $di[\ZBateson\MailMimeParser\Stream\StreamFactory::class];
            $streamContainer->setStream($streamFactory->newMessagePartStream($this));
        }
        $this->headerContainer = $headerContainer;
    }

    /**
     * Returns a filename for the part if one is defined, or null otherwise.
     *
     * Uses the 'filename' parameter of the Content-Disposition header if it
     * exists, or the 'name' parameter of the 'Content-Type' header if it
     * doesn't.
     *
     * @return string|null the file name of the part or null.
     */
    public function getFilename() : ?string
    {
        return $this->getHeaderParameter(
            HeaderConsts::CONTENT_DISPOSITION,
            'filename',
            $this->getHeaderParameter(
                HeaderConsts::CONTENT_TYPE,
                'name'
            )
        );
    }

    /**
     * Returns true.
     *
     */
    public function isMime() : bool
    {
        return true;
    }

    public function isMultiPart()
    {
        // casting to bool, preg_match returns 1 for true
        return (bool) (\preg_match(
            '~multipart/.*~i',
            $this->getContentType()
        ));
    }

    /**
     * Returns true if this part has a defined 'charset' on its Content-Type
     * header.
     *
     * This may result in some false positives if charset is set on a part that
     * is not plain text which has been seen.  If a part is known to be binary,
     * it's better to use {@see IMessagePart::getBinaryContentStream()} to
     * avoid issues, or to call {@see IMessagePart::saveContent()} directly if
     * saving a part's content.
     *
     */
    public function isTextPart() : bool
    {
        return ($this->getCharset() !== null);
    }

    /**
     * Returns the mime type of the content, or $default if one is not set.
     *
     * Looks at the part's Content-Type header and returns its value if set, or
     * defaults to 'text/plain'.
     *
     * Note that the returned value is converted to lower case, and may not be
     * identical to calling {@see MimePart::getHeaderValue('Content-Type')} in
     * some cases.
     *
     * @param string $default Optional default value to specify a default other
     *        than text/plain if needed.
     * @return string the mime type
     */
    public function getContentType(string $default = 'text/plain') : ?string
    {
        return \strtolower($this->getHeaderValue(HeaderConsts::CONTENT_TYPE, $default));
    }

    /**
     * Returns the charset of the content, or null if not applicable/defined.
     *
     * Looks for a 'charset' parameter under the 'Content-Type' header of this
     * part and returns it if set, defaulting to 'ISO-8859-1' if the
     * Content-Type header exists and is of type text/plain or text/html.
     *
     * Note that the returned value is also converted to upper case.
     *
     * @return string|null the charset
     */
    public function getCharset() : ?string
    {
        $charset = $this->getHeaderParameter(HeaderConsts::CONTENT_TYPE, 'charset');
        if ($charset === null || \strcasecmp($charset, 'binary') === 0) {
            $contentType = $this->getContentType();
            if ($contentType === 'text/plain' || $contentType === 'text/html') {
                return 'ISO-8859-1';
            }
            return null;
        }
        return \strtoupper($charset);
    }

    /**
     * Returns the content's disposition, or returns the value of $default if
     * not defined.
     *
     * Looks at the 'Content-Disposition' header, which should only contain
     * either 'inline' or 'attachment'.  If the header is not one of those
     * values, $default is returned, which defaults to 'inline' unless passed
     * something else.
     *
     * @param string $default Optional default value if not set or does not
     *        match 'inline' or 'attachment'.
     * @return string the content disposition
     */
    public function getContentDisposition(?string $default = 'inline') : ?string
    {
        $value = $this->getHeaderValue(HeaderConsts::CONTENT_DISPOSITION);
        if ($value === null || !\in_array($value, ['inline', 'attachment'])) {
            return $default;
        }
        return \strtolower($value);
    }

    /**
     * Returns the content transfer encoding used to encode the content on this
     * part, or the value of $default if not defined.
     *
     * Looks up and returns the value of the 'Content-Transfer-Encoding' header
     * if set, defaulting to '7bit' if an alternate $default param is not
     * passed.
     *
     * The returned value is always lowercase, and header values of 'x-uue',
     * 'uue' and 'uuencode' will return 'x-uuencode' instead.
     *
     * @param string $default Optional default value to return if the header
     *        isn't set.
     * @return string the content transfer encoding.
     */
    public function getContentTransferEncoding(?string $default = '7bit') : ?string
    {
        static $translated = [
            'x-uue' => 'x-uuencode',
            'uue' => 'x-uuencode',
            'uuencode' => 'x-uuencode'
        ];
        $type = \strtolower($this->getHeaderValue(HeaderConsts::CONTENT_TRANSFER_ENCODING, $default));
        if (isset($translated[$type])) {
            return $translated[$type];
        }
        return $type;
    }

    /**
     * Returns the Content ID of the part, or null if not defined.
     *
     * Looks up and returns the value of the 'Content-ID' header.
     *
     * @return string|null the content ID or null if not defined.
     */
    public function getContentId() : ?string
    {
        return $this->getHeaderValue(HeaderConsts::CONTENT_ID);
    }

    /**
     * Returns true if this part's parent is an IMessage, and is the same part
     * returned by {@see IMessage::getSignaturePart()}.
     *
     * @return bool
     */
    public function isSignaturePart()
    {
        if ($this->parent === null || !$this->parent instanceof IMessage) {
            return false;
        }
        return $this->parent->getSignaturePart() === $this;
    }

    public function getHeader($name, $offset = 0)
    {
        return $this->headerContainer->get($name, $offset);
    }

    public function getHeaderAs(string $name, string $iHeaderClass, int $offset = 0) : ?IHeader
    {
        return $this->headerContainer->getAs($name, $iHeaderClass, $offset);
    }

    public function getAllHeaders()
    {
        return $this->headerContainer->getHeaderObjects();
    }

    public function getAllHeadersByName($name)
    {
        return $this->headerContainer->getAll($name);
    }

    public function getRawHeaders()
    {
        return $this->headerContainer->getHeaders();
    }

    public function getRawHeaderIterator()
    {
        return $this->headerContainer->getIterator();
    }

    public function getHeaderValue($name, $defaultValue = null)
    {
        $header = $this->getHeader($name);
        if ($header !== null) {
            return $header->getValue() ?: $defaultValue;
        }
        return $defaultValue;
    }

    public function getHeaderParameter($header, $param, $defaultValue = null)
    {
        $obj = $this->getHeader($header);
        if ($obj && $obj instanceof ParameterHeader) {
            return $obj->getValueFor($param, $defaultValue);
        }
        return $defaultValue;
    }

    /**
     * @return static
     */
    public function setRawHeader(string $name, ?string $value, int $offset = 0)
    {
        $this->headerContainer->set($name, $value, $offset);
        $this->notify();
        return $this;
    }

    /**
     * @return static
     */
    public function addRawHeader(string $name, string $value)
    {
        $this->headerContainer->add($name, $value);
        $this->notify();
        return $this;
    }

    /**
     * @return static
     */
    public function removeHeader(string $name)
    {
        $this->headerContainer->removeAll($name);
        $this->notify();
        return $this;
    }

    /**
     * @return static
     */
    public function removeSingleHeader(string $name, int $offset = 0)
    {
        $this->headerContainer->remove($name, $offset);
        $this->notify();
        return $this;
    }
}
