<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Header\Consumer;

use ZBateson\MailMimeParser\Header\Part\CommentPart;

/**
 * Serves as a base-consumer for ID headers (like Message-ID and Content-ID).
 *
 * IdBaseConsumer handles invalidly-formatted IDs not within '<' and '>'
 * characters.  Processing for validly-formatted IDs are passed on to its
 * sub-consumer, IdConsumer.
 *
 * @author Zaahid Bateson
 */
class IdBaseConsumer extends AbstractConsumer
{
    /**
     * Returns the following as sub-consumers:
     *  - {@see CommentConsumer}
     *  - {@see QuotedStringConsumer}
     *  - {@see IdConsumer}
     *
     * @return AbstractConsumer[] the sub-consumers
     */
    protected function getSubConsumers() : array
    {
        return [
            $this->consumerService->getCommentConsumer(),
            $this->consumerService->getQuotedStringConsumer(),
            $this->consumerService->getIdConsumer()
        ];
    }

    /**
     * Returns '\s+' as a whitespace separator.
     *
     * @return string[] an array of regex pattern matchers.
     */
    protected function getTokenSeparators() : array
    {
        return ['\s+'];
    }

    /**
     * IdBaseConsumer doesn't have start/end tokens, and so always returns
     * false.
     */
    protected function isEndToken(string $token) : bool
    {
        return false;
    }

    /**
     * IdBaseConsumer doesn't have start/end tokens, and so always returns
     * false.
     *
     * @codeCoverageIgnore
     */
    protected function isStartToken(string $token) : bool
    {
        return false;
    }

    /**
     * Returns null for whitespace, and LiteralPart for anything else.
     *
     * @param string $token the token
     * @param bool $isLiteral set to true if the token represents a literal -
     *        e.g. an escaped token
     * @return \ZBateson\MailMimeParser\Header\IHeaderPart|null the constructed
     *         header part or null if the token should be ignored
     */
    protected function getPartForToken(string $token, bool $isLiteral)
    {
        if (\preg_match('/^\s+$/', $token)) {
            return null;
        }
        return $this->partFactory->newLiteralPart($token);
    }

    /**
     * Overridden to filter out any found CommentPart objects.
     *
     * @param \ZBateson\MailMimeParser\Header\IHeaderPart[] $parts
     * @return \ZBateson\MailMimeParser\Header\IHeaderPart[]
     */
    protected function processParts(array $parts) : array
    {
        return \array_values(\array_filter($parts, function($part) {
            return !(empty($part) || $part instanceof CommentPart);
        }));
    }
}
