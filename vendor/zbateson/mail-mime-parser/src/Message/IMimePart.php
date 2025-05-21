<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Message;

use ZBateson\MailMimeParser\Header\IHeader;

/**
 * An interface representation of any MIME email part.
 *
 * A MIME part may contain any combination of headers, content and children.
 *
 * @author Zaahid Bateson
 */
interface IMimePart extends IMultiPart
{
    /**
     * Returns true if this part's content type matches multipart/*
     *
     * @return bool
     */
    public function isMultiPart();

    /**
     * Returns true if this part is the 'signature' part of a signed message.
     *
     * @return bool
     */
    public function isSignaturePart();

    /**
     * Returns the IHeader object for the header with the given $name.
     *
     * If the optional $offset is passed, and multiple headers exist with the
     * same name, the one at the passed offset is returned.
     *
     * Note that mime header names aren't case sensitive, and the '-' character
     * is ignored, so ret
     *
     * If a header with the given $name and $offset doesn't exist, null is
     * returned.
     *
     * @see IMimePart::getHeaderAs() to parse a header into a provided IHeader
     *      type and return it.
     * @see IMimePart::getHeaderValue() to get the string value portion of a
     *      specific header only.
     * @see IMimePart::getHeaderParameter() to get the string value portion of a
     *      specific header's parameter only.
     * @see IMimePart::getAllHeaders() to retrieve an array of all header
     *      objects for this part.
     * @see IMimePart::getAllHeadersByName() to retrieve an array of all headers
     *      with a certain name.
     * @see IMimePart::getRawHeaders() to retrieve a two-dimensional string[][]
     *      array of raw headers in this part.
     * @see IMimePart::getRawHeaderIterator() to retrieve an iterator traversing
     *      a two-dimensional string[] array of raw headers.
     * @param string $name The name of the header to retrieve.
     * @param int $offset Optional offset if there are multiple headers with the
     *        given name.
     * @return \ZBateson\MailMimeParser\Header\IHeader|null the header object
     */
    public function getHeader($name, $offset = 0);

    /**
     * Returns the IHeader object for the header with the given $name, using the
     * passed $iHeaderClass to construct it.
     *
     * If the optional $offset is passed, and multiple headers exist with the
     * same name, the one at the passed offset is returned.
     *
     * Note that mime headers aren't case sensitive, and the '-' character is
     *
     * If a header with the given $name and $offset doesn't exist, null is
     * returned.
     *
     * @see IMimePart::getHeaderValue() to get the string value portion of a
     *      specific header only.
     * @see IMimePart::getHeaderParameter() to get the string value portion of a
     *      specific header's parameter only.
     * @see IMimePart::getAllHeaders() to retrieve an array of all header
     *      objects for this part.
     * @see IMimePart::getAllHeadersByName() to retrieve an array of all headers
     *      with a certain name.
     * @see IMimePart::getRawHeaders() to retrieve a two-dimensional string[][]
     *      array of raw headers in this part.
     * @see IMimePart::getRawHeaderIterator() to retrieve an iterator traversing
     *      a two-dimensional string[] array of raw headers.
     * @param string $name The name of the header to retrieve.
     * @param
     * @param int $offset Optional offset if there are multiple headers with the
     *        given name.
     * @return ?IHeader the header object
     */
    public function getHeaderAs(string $name, string $iHeaderClass, int $offset = 0) : ?IHeader;

    /**
     * Returns an array of all headers in this part.
     *
     * @see IMimePart::getHeader() to retrieve a single header object.
     * @see IMimePart::getHeaderValue() to get the string value portion of a
     *      specific header only.
     * @see IMimePart::getHeaderParameter() to get the string value portion of a
     *      specific header's parameter only.
     * @see IMimePart::getAllHeadersByName() to retrieve an array of all headers
     *      with a certain name.
     * @see IMimePart::getRawHeaders() to retrieve a two-dimensional string[][]
     *      array of raw headers in this part.
     * @see IMimePart::getRawHeaderIterator() to retrieve an iterator traversing
     *      a two-dimensional string[] array of raw headers.
     * @return \ZBateson\MailMimeParser\Header\IHeader[] an array of header
     *         objects
     */
    public function getAllHeaders();

    /**
     * Returns an array of headers that match the passed name.
     *
     * @see IMimePart::getHeader() to retrieve a single header object.
     * @see IMimePart::getHeaderValue() to get the string value portion of a
     *      specific header only.
     * @see IMimePart::getHeaderParameter() to get the string value portion of a
     *      specific header's parameter only.
     * @see IMimePart::getAllHeaders() to retrieve an array of all header
     *      objects for this part.
     * @see IMimePart::getRawHeaders() to retrieve a two-dimensional string[][]
     *      array of raw headers in this part.
     * @see IMimePart::getRawHeaderIterator() to retrieve an iterator traversing
     *      a two-dimensional string[] array of raw headers.
     * @param string $name
     * @return \ZBateson\MailMimeParser\Header\IHeader[] an array of header
     *         objects
     */
    public function getAllHeadersByName($name);

    /**
     * Returns a two dimensional string array of all headers for the mime part
     * with the first element holding the name, and the second its raw string
     * value:
     *
     * [ [ '1st-Header-Name', 'Header Value' ], [ '2nd-Header-Name', 'Header Value' ] ]
     *
     *
     * @see IMimePart::getHeader() to retrieve a single header object.
     * @see IMimePart::getHeaderValue() to get the string value portion of a
     *      specific header only.
     * @see IMimePart::getHeaderParameter() to get the string value portion of a
     *      specific header's parameter only.
     * @see IMimePart::getAllHeaders() to retrieve an array of all header
     *      objects for this part.
     * @see IMimePart::getAllHeadersByName() to retrieve an array of all headers
     *      with a certain name.
     * @see IMimePart::getRawHeaderIterator() to retrieve an iterator instead of
     *      the returned two-dimensional array
     * @return string[][] an array of raw headers
     */
    public function getRawHeaders();

    /**
     * Returns an iterator to all headers in this part.  Each returned element
     * is an array with its first element set to the header's name, and the
     * second to its raw value:
     *
     * [ 'Header-Name', 'Header Value' ]
     *
     * @see IMimePart::getHeader() to retrieve a single header object.
     * @see IMimePart::getHeaderValue() to get the string value portion of a
     *      specific header only.
     * @see IMimePart::getHeaderParameter() to get the string value portion of a
     *      specific header's parameter only.
     * @see IMimePart::getAllHeaders() to retrieve an array of all header
     *      objects for this part.
     * @see IMimePart::getAllHeadersByName() to retrieve an array of all headers
     *      with a certain name.
     * @see IMimePart::getRawHeaders() to retrieve the array the returned
     *      iterator iterates over.
     * @return \Iterator an iterator for raw headers
     */
    public function getRawHeaderIterator();

    /**
     * Returns the string value for the header with the given $name, or null if
     * the header doesn't exist and no alternative $defaultValue is passed.
     *
     * Note that mime headers aren't case sensitive.
     *
     * @see IMimePart::getHeader() to retrieve a single header object.
     * @see IMimePart::getHeaderParameter() to get the string value portion of a
     *      specific header's parameter only.
     * @see IMimePart::getAllHeaders() to retrieve an array of all header
     *      objects for this part.
     * @see IMimePart::getAllHeadersByName() to retrieve an array of all headers
     *      with a certain name.
     * @see IMimePart::getRawHeaders() to retrieve the array the returned
     *      iterator iterates over.
     * @see IMimePart::getRawHeaderIterator() to retrieve an iterator instead of
     *      the returned two-dimensional array
     * @param string $name The name of the header
     * @param string $defaultValue Optional default value to return if the
     *        header doesn't exist on this part.
     * @return string|null the value of the header
     */
    public function getHeaderValue($name, $defaultValue = null);

    /**
     * Returns the value of the parameter named $param on a header with the
     * passed $header name, or null if the parameter doesn't exist and a
     * $defaultValue isn't passed.
     *
     * Only headers of type
     * {@see \ZBateson\MailMimeParser\Header\ParameterHeader} have parameters.
     * Content-Type and Content-Disposition are examples of headers with
     * parameters. "Charset" is a common parameter of Content-Type.
     *
     * @see IMimePart::getHeader() to retrieve a single header object.
     * @see IMimePart::getHeaderValue() to get the string value portion of a
     *      specific header only.
     * @see IMimePart::getAllHeaders() to retrieve an array of all header
     *      objects for this part.
     * @see IMimePart::getAllHeadersByName() to retrieve an array of all headers
     *      with a certain name.
     * @see IMimePart::getRawHeaders() to retrieve the array the returned
     *      iterator iterates over.
     * @see IMimePart::getRawHeaderIterator() to retrieve an iterator instead of
     *      the returned two-dimensional array
     * @param string $header The name of the header.
     * @param string $param The name of the parameter.
     * @param string $defaultValue Optional default value to return if the
     *        parameter doesn't exist.
     * @return string|null The value of the parameter.
     */
    public function getHeaderParameter($header, $param, $defaultValue = null);

    /**
     * Adds a header with the given $name and $value.  An optional $offset may
     * be passed, which will overwrite a header if one exists with the given
     * name and offset only. Otherwise a new header is added.  The passed
     * $offset may be ignored in that case if it doesn't represent the next
     * insert position for the header with the passed name... instead it would
     * be 'pushed' on at the next position.
     *
     * ```
     * $part = $myMimePart;
     * $part->setRawHeader('New-Header', 'value');
     * echo $part->getHeaderValue('New-Header');        // 'value'
     *
     * $part->setRawHeader('New-Header', 'second', 4);
     * echo is_null($part->getHeader('New-Header', 4)); // '1' (true)
     * echo $part->getHeader('New-Header', 1)
     *      ->getValue();                               // 'second'
     * ```
     *
     * A new {@see \ZBateson\MailMimeParser\Header\IHeader} object is created
     * from the passed value.  No processing on the passed string is performed,
     * and so the passed name and value must be formatted correctly according to
     * related RFCs.  In particular, be careful to encode non-ascii data, to
     * keep lines under 998 characters in length, and to follow any special
     * formatting required for the type of header.
     *
     * @see IMimePart::addRawHeader() Adds a header to the part regardless of
     *      whether or not a header with that name already exists.
     * @see IMimePart::removeHeader() Removes all headers on this part with the
     *      passed name
     * @see IMimePart::removeSingleHeader() Removes a single header if more than
     *      one with the passed name exists.
     * @param string $name The name of the new header, e.g. 'Content-Type'.
     * @param ?string $value The raw value of the new header.
     * @param int $offset An optional offset, defaulting to '0' and therefore
     *        overriding the first header of the given $name if one exists.
     */
    public function setRawHeader(string $name, ?string $value, int $offset = 0);

    /**
     * Adds a header with the given $name and $value.
     *
     * Note: If a header with the passed name already exists, a new header is
     * created with the same name.  This should only be used when that is
     * intentional - in most cases {@see IMimePart::setRawHeader()} should be
     * called instead.
     *
     * A new {@see \ZBateson\MailMimeParser\Header\IHeader} object is created
     * from the passed value.  No processing on the passed string is performed,
     * and so the passed name and value must be formatted correctly according to
     * related RFCs.  In particular, be careful to encode non-ascii data, to
     * keep lines under 998 characters in length, and to follow any special
     * formatting required for the type of header.
     *
     * @see IMimePart::setRawHeader() Sets a header, potentially overwriting one
     *      if it already exists.
     * @see IMimePart::removeHeader() Removes all headers on this part with the
     *      passed name
     * @see IMimePart::removeSingleHeader() Removes a single header if more than
     *      one with the passed name exists.
     * @param string $name The name of the header
     * @param string $value The raw value of the header.
     */
    public function addRawHeader(string $name, string $value);

    /**
     * Removes all headers from this part with the passed name.
     *
     * @see IMimePart::addRawHeader() Adds a header to the part regardless of
     *      whether or not a header with that name already exists.
     * @see IMimePart::setRawHeader() Sets a header, potentially overwriting one
     *      if it already exists.
     * @see IMimePart::removeSingleHeader() Removes a single header if more than
     *      one with the passed name exists.
     * @param string $name The name of the header(s) to remove.
     */
    public function removeHeader(string $name);

    /**
     * Removes a single header with the passed name (in cases where more than
     * one may exist, and others should be preserved).
     *
     * @see IMimePart::addRawHeader() Adds a header to the part regardless of
     *      whether or not a header with that name already exists.
     * @see IMimePart::setRawHeader() Sets a header, potentially overwriting one
     *      if it already exists.
     * @see IMimePart::removeHeader() Removes all headers on this part with the
     *      passed name
     * @param string $name The name of the header to remove
     * @param int $offset Optional offset of the header to remove (defaults to
     *        0 -- the first header).
     */
    public function removeSingleHeader(string $name, int $offset = 0);
}
