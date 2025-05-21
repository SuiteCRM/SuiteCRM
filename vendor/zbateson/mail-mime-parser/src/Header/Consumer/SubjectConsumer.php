<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Header\Consumer;

use Iterator;
use ZBateson\MailMimeParser\Header\Part\Token;

/**
 * Extends GenericConsumer to remove its sub consumers.
 *
 * Prior to this, subject headers were parsed using the GenericConsumer which
 * meant if the subject contained text within parentheses, it would not be
 * included as part of the returned value in a getHeaderValue.  Mime-encoded
 * parts within quotes would be ignored, and backslash characters denoted an
 * escaped character.
 *
 * From testing in ThunderBird and Outlook web mail it seems quoting parts
 * doesn't have an effect (e.g. quoting a "mime-literal" encoded part still
 * comes out decoded), and parts in parentheses (comments) are displayed
 * normally.
 *
 * @author Zaahid Bateson
 */
class SubjectConsumer extends GenericConsumer
{
    /**
     * Returns an empty array
     *
     * @return AbstractConsumer[] the sub-consumers
     */
    protected function getSubConsumers() : array
    {
        return [];
    }

    /**
     * Overridden to preserve whitespace.
     *
     * Whitespace between two words is preserved unless the whitespace begins
     * with a newline (\n or \r\n), in which case the entire string of
     * whitespace is discarded, and a single space ' ' character is used in its
     * place.
     *
     * @param string $token the token
     * @param bool $isLiteral set to true if the token represents a literal -
     *        e.g. an escaped token
     * @return \ZBateson\MailMimeParser\Header\Part\HeaderPart|null the
     *         constructed header part or null if the token should be ignored
     */
    protected function getPartForToken(string $token, bool $isLiteral)
    {
        if ($isLiteral) {
            return $this->partFactory->newLiteralPart($token);
        } elseif (\preg_match('/^\s+$/', $token)) {
            if (\preg_match('/^[\r\n]/', $token)) {
                return $this->partFactory->newToken(' ');
            }
            return $this->partFactory->newToken($token);
        }
        return $this->partFactory->newInstance($token);
    }

    /**
     * Returns an array of \ZBateson\MailMimeParser\Header\Part\HeaderPart for
     * the current token on the iterator.
     *
     * Overridden from AbstractConsumer to remove special filtering for
     * backslash escaping, which also seems to not apply to Subject headers at
     * least in ThunderBird's implementation.
     *
     * @return \ZBateson\MailMimeParser\Header\Part\HeaderPart[]|array
     */
    protected function getTokenParts(Iterator $tokens) : array
    {
        return $this->getConsumerTokenParts($tokens);
    }

    /**
     * Overridden to not split out backslash characters and its next character
     * as a special case defined in AbastractConsumer
     *
     * @return string the regex pattern
     */
    protected function getTokenSplitPattern() : string
    {
        $sChars = \implode('|', $this->getAllTokenSeparators());
        return '~(' . $sChars . ')~';
    }
}
