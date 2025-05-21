<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Header\Consumer;

use ZBateson\MailMimeParser\Header\Part\CommentPart;
use ZBateson\MailMimeParser\Header\Part\LiteralPart;

/**
 * Parses the Address portion of an email address header, for an address part
 * that contains both a name and an email address, e.g. "name" <email@tld.com>.
 *
 * The address portion found within the '<' and '>' chars may contain comments
 * and quoted portions.
 *
 * @author Zaahid Bateson
 */
class AddressEmailConsumer extends AbstractConsumer
{
    /**
     * Returns the following as sub-consumers:
     *  - {@see AddressGroupConsumer}
     *  - {@see CommentConsumer}
     *  - {@see QuotedStringConsumer}
     *
     * @return AbstractConsumer[] the sub-consumers
     */
    protected function getSubConsumers() : array
    {
        return [
            $this->consumerService->getCommentConsumer(),
            $this->consumerService->getQuotedStringConsumer(),
        ];
    }

    /**
     * Overridden to return patterns matching the beginning/end part of an
     * address in a name/address part ("<" and ">" chars).
     *
     * @return string[] the patterns
     */
    public function getTokenSeparators() : array
    {
        return ['<', '>'];
    }

    /**
     * Returns true for the '>' char.
     */
    protected function isEndToken(string $token) : bool
    {
        return ($token === '>');
    }

    /**
     * Returns true for the '<' char.
     */
    protected function isStartToken(string $token) : bool
    {
        return ($token === '<');
    }

    /**
     * Returns a single AddressPart with its 'email' portion set, so an
     * AddressConsumer can identify it and create an AddressPart with both a
     * name and email set.
     *
     * @param \ZBateson\MailMimeParser\Header\IHeaderPart[] $parts
     * @return \ZBateson\MailMimeParser\Header\IHeaderPart[]|array
     */
    protected function processParts(array $parts) : array
    {
        $strEmail = '';
        foreach ($parts as $p) {
            $val = $p->getValue();
            if ((($p instanceof LiteralPart) && !($p instanceof CommentPart)) && $val !== '') {
                $val = '"' . \preg_replace('/(["\\\])/', '\\\$1', $val) . '"';
            } else {
                $val = \preg_replace('/\s+/', '', $val);
            }
            $strEmail .= $val;
        }
        return [$this->partFactory->newAddressPart('', $strEmail)];
    }
}
