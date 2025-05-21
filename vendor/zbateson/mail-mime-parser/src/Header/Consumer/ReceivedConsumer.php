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
 * Parses a Received header into ReceivedParts, ReceivedDomainParts, a DatePart,
 * and CommentParts.
 *
 * Parts that don't correspond to any of the above are discarded.
 *
 * @author Zaahid Bateson
 */
class ReceivedConsumer extends AbstractConsumer
{
    /**
     * ReceivedConsumer doesn't have any token separators of its own.
     * Sub-Consumers will return separators matching 'part' word separators, for
     * example 'from' and 'by', and ';' for date, etc...
     *
     * @return string[] an array of regex pattern matchers
     */
    protected function getTokenSeparators() : array
    {
        return [];
    }

    /**
     * ReceivedConsumer doesn't have an end token, and so this just returns
     * false.
     */
    protected function isEndToken(string $token) : bool
    {
        return false;
    }

    /**
     * ReceivedConsumer doesn't start consuming at a specific token, it's the
     * base handler for the Received header, and so this always returns false.
     *
     * @codeCoverageIgnore
     */
    protected function isStartToken(string $token) : bool
    {
        return false;
    }

    /**
     * Returns sub-consumers for a received consumer.
     *
     * - 2 {@see Received/DomainConsumer} instances, with FROM and BY as part
     *   names.
     * - 4 {@see Received/GenericReceivedConsumer} instances for VIA, WITH, ID,
     *   and FOR part names.
     * - 1 {@see Received/ReceivedDateConsumer} for the date/time stamp.
     * - 1 {@see CommentConsumer} to consume any comments.
     *
     * @return AbstractConsumer[] the sub-consumers
     */
    protected function getSubConsumers() : array
    {
        return [
            $this->consumerService->getSubReceivedConsumer('from'),
            $this->consumerService->getSubReceivedConsumer('by'),
            $this->consumerService->getSubReceivedConsumer('via'),
            $this->consumerService->getSubReceivedConsumer('with'),
            $this->consumerService->getSubReceivedConsumer('id'),
            $this->consumerService->getSubReceivedConsumer('for'),
            $this->consumerService->getSubReceivedConsumer('date'),
            $this->consumerService->getCommentConsumer()
        ];
    }

    /**
     * Overridden to exclude the MimeLiteralPart pattern that comes by default
     * in AbstractConsumer.
     *
     * @return string the regex pattern
     */
    protected function getTokenSplitPattern() : string
    {
        $sChars = \implode('|', $this->getAllTokenSeparators());
        return '~(' . $sChars . ')~';
    }

    /**
     * Overridden to /not/ advance when the end token matches a start token for
     * a sub-consumer.
     *
     * @return static
     */
    protected function advanceToNextToken(Iterator $tokens, bool $isStartToken)
    {
        if ($isStartToken) {
            $tokens->next();
        } elseif ($tokens->valid() && !$this->isEndToken($tokens->current())) {
            foreach ($this->getSubConsumers() as $consumer) {
                if ($consumer->isStartToken($tokens->current())) {
                    return $this;
                }
            }
            $tokens->next();
        }
        return $this;
    }

    /**
     * Overridden to combine all part values into a single string and return it
     * as an array with a single element.
     *
     * @param \ZBateson\MailMimeParser\Header\IHeaderPart[] $parts
     * @return \ZBateson\MailMimeParser\Header\IHeaderPart[]
     */
    protected function processParts(array $parts) : array
    {
        $ret = [];
        foreach ($parts as $part) {
            if ($part instanceof Token) {
                continue;
            }
            $ret[] = $part;
        }
        return $ret;
    }
}
