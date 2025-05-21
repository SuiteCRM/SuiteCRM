<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Header\Consumer;

/**
 * Represents a quoted part of a header value starting at a double quote, and
 * ending at the next double quote.
 *
 * A quoted-pair part in a header is a literal.  There are no sub-consumers for
 * it and a Part\LiteralPart is returned.
 *
 * Newline characters (CR and LF) are stripped entirely from the quoted part.
 * This is based on the example at:
 *
 * https://tools.ietf.org/html/rfc822#section-3.1.1
 *
 * And https://www.w3.org/Protocols/rfc1341/7_2_Multipart.html in section 7.2.1
 * splitting the boundary.
 *
 * @author Zaahid Bateson
 */
class QuotedStringConsumer extends GenericConsumer
{
    /**
     * QuotedStringConsumer doesn't have any sub-consumers.  This method returns
     * an empty array.
     *
     */
    public function getSubConsumers() : array
    {
        return [];
    }

    /**
     * Returns true if the token is a double quote.
     */
    protected function isStartToken(string $token) : bool
    {
        return ($token === '"');
    }

    /**
     * Returns true if the token is a double quote.
     */
    protected function isEndToken(string $token) : bool
    {
        return ($token === '"');
    }

    /**
     * Returns a single regex pattern for a double quote.
     *
     * @return string[]
     */
    protected function getTokenSeparators() : array
    {
        return ['\"'];
    }

    /**
     * No ignored spaces in a quoted part.  Returns the passed $parts param
     * as-is.
     *
     */
    protected function filterIgnoredSpaces(array $parts) : array
    {
        return $parts;
    }

    /**
     * Constructs a LiteralPart and returns it.
     *
     * @param bool $isLiteral not used - everything in a quoted string is a
     *        literal
     * @return \ZBateson\MailMimeParser\Header\IHeaderPart|null
     */
    protected function getPartForToken(string $token, bool $isLiteral)
    {
        return $this->partFactory->newLiteralPart($token);
    }
}
