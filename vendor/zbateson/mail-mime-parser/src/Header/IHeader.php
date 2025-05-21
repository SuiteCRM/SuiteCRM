<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Header;

/**
 * A mime email header line consisting of a name and value.
 *
 * The header object provides methods to access the header's name, raw value,
 * and also its parsed value.  The parsed value will depend on the type of
 * header and in some cases may be broken up into other parts (for example email
 * addresses in an address header, or parameters in a parameter header).
 *
 * @author Zaahid Bateson
 */
interface IHeader
{
    /**
     * Returns an array of IHeaderPart objects the header's value has been
     * parsed into.
     *
     * @return IHeaderPart[] The array of parts.
     */
    public function getParts() : array;

    /**
     * Returns the parsed 'value' of the header.
     *
     * For headers that contain multiple parts, like address headers (To, From)
     * or parameter headers (Content-Type), the 'value' is the value of the
     * first parsed part.
     *
     * @return string The value
     */
    public function getValue() : ?string;

    /**
     * Returns the raw value of the header.
     *
     * @return string The raw value.
     */
    public function getRawValue() : string;

    /**
     * Returns the name of the header.
     *
     * @return string The name.
     */
    public function getName() : string;

    /**
     * Returns the string representation of the header.
     *
     * i.e.: '<HeaderName>: <RawValue>'
     *
     * @return string The string representation.
     */
    public function __toString() : string;
}
