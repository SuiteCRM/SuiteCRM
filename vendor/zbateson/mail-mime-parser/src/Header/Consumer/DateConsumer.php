<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Header\Consumer;

/**
 * Parses a date header into a Part\DatePart taking care of comment and quoted
 * parts as necessary.
 *
 * @author Zaahid Bateson
 */
class DateConsumer extends GenericConsumer
{
    /**
     * Returns a Part\LiteralPart for the current token
     *
     * @param string $token the token
     * @param bool $isLiteral set to true if the token represents a literal -
     *        e.g. an escaped token
     * @return \ZBateson\MailMimeParser\Header\IHeaderPart|null
     */
    protected function getPartForToken(string $token, bool $isLiteral)
    {
        return $this->partFactory->newLiteralPart($token);
    }

    /**
     * Concatenates the passed parts and constructs a single Part\DatePart,
     * returning it in an array with a single element.
     *
     * @param \ZBateson\MailMimeParser\Header\IHeaderPart[] $parts The parsed
     *        parts.
     * @return \ZBateson\MailMimeParser\Header\IHeaderPart[] Array of resulting
     *         final parts.
     */
    protected function processParts(array $parts) : array
    {
        $strValue = '';
        foreach ($parts as $part) {
            $strValue .= $part->getValue();
        }
        return [$this->partFactory->newDatePart($strValue)];
    }
}
