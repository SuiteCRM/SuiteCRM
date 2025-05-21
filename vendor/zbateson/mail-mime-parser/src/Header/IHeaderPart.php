<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Header;

/**
 * Represents a single parsed part of a header line's value.
 *
 * For header values with multiple parts, for instance a list of addresses, each
 * address would be parsed into a single part.
 *
 * @author Zaahid Bateson
 */
interface IHeaderPart
{
    /**
     * Returns the part's value.
     *
     * @return string The value of the part
     */
    public function getValue() : ?string;

    /**
     * Returns the value of the part (which is a string).
     *
     * @return string The value
     */
    public function __toString() : string;
}
