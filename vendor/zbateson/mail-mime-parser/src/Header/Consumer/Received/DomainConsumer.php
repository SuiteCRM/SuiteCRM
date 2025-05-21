<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Header\Consumer\Received;

use ZBateson\MailMimeParser\Header\Part\CommentPart;

/**
 * Parses a so-called "extended-domain" (from and by) part of a Received header.
 *
 * Looks for and extracts the following fields from an extended-domain part:
 * Name, Hostname and Address.
 *
 * The Name part is always the portion of the extended-domain part existing on
 * its own, outside of the parenthesized hostname and address part.  This is
 * true regardless of whether an address is used as the name, as its assumed to
 * be the string used to identify the server, whatever it may be.
 *
 * The parenthesized part normally (but not necessarily) following a name must
 * "look like" a tcp-info section of an extended domain as defined by RFC5321.
 * The validation is very purposefully very loose to be accommodating to many
 * erroneous implementations.  The only restriction is the host part must
 * contain two characters, the first being alphanumeric, followed by any number
 * of more alphanumeric, '.', and '-' characters.  The address part must be
 * within square brackets, '[]'... although an address outside of square
 * brackets could be matched by the domain matcher if it exists alone within the
 * parentheses.  The address is any number of '.', numbers, ':' and letters a-f.
 * This allows it to match ipv6 addresses as well.  In addition, the address may
 * start with the string "ipv6", and may be followed by a port number as some
 * implementations seem to do.
 *
 * Strings in parentheses not matching the aforementioned 'domain/address'
 * pattern will be considered comments, and will be returned as a separate
 * CommentPart.
 *
 * @see https://tools.ietf.org/html/rfc5321#section-4.4
 * @see https://github.com/Te-k/pyreceived/blob/master/test.py
 * @author Zaahid Bateson
 * @author Mariusz Krzaczkowski
 */
class DomainConsumer extends GenericReceivedConsumer
{
    /**
     * Overridden to return true if the passed token is a closing parenthesis.
     */
    protected function isEndToken(string $token) : bool
    {
        if ($token === ')') {
            return true;
        }
        return parent::isEndToken($token);
    }

    /**
     * Attempts to match a parenthesized expression to find a hostname and an
     * address.  Returns true if the expression matched, and either hostname or
     * address were found.
     *
     * @param string $hostname
     * @param string $address
     */
    private function matchHostPart(string $value, ?string &$hostname, ?string &$address) : bool
    {
        $matches = [];
        $pattern = '~^(\[(IPv[64])?(?P<addr1>[a-f\d\.\:]+)\])?\s*(helo=)?(?P<name>[a-z0-9\-]+[a-z0-9\-\.]+)?\s*(\[(IPv[64])?(?P<addr2>[a-f\d\.\:]+)\])?$~i';
        if (\preg_match($pattern, $value, $matches)) {
            if (!empty($matches['name'])) {
                $hostname = $matches['name'];
            }
            if (!empty($matches['addr1'])) {
                $address = $matches['addr1'];
            }
            if (!empty($matches['addr2'])) {
                $address = $matches['addr2'];
            }
            return true;
        }
        return false;
    }

    /**
     * Creates a single ReceivedDomainPart out of matched parts.  If an
     * unmatched parenthesized expression was found, it's returned as a
     * CommentPart.
     *
     * @param \ZBateson\MailMimeParser\Header\Part\HeaderPart[] $parts
     * @return \ZBateson\MailMimeParser\Header\Part\ReceivedDomainPart[]|\ZBateson\MailMimeParser\Header\Part\CommentPart[]|\ZBateson\MailMimeParser\Header\Part\HeaderPart[]
     */
    protected function processParts(array $parts) : array
    {
        $ehloName = null;
        $hostname = null;
        $address = null;
        $commentPart = null;

        $filtered = $this->filterIgnoredSpaces($parts);
        foreach ($filtered as $part) {
            if ($part instanceof CommentPart) {
                $commentPart = $part;
                continue;
            }
            $ehloName .= $part->getValue();
        }

        $strValue = $ehloName;
        if ($commentPart !== null && $this->matchHostPart($commentPart->getComment(), $hostname, $address)) {
            $strValue .= ' (' . $commentPart->getComment() . ')';
            $commentPart = null;
        }

        $domainPart = $this->partFactory->newReceivedDomainPart(
            $this->getPartName(),
            $strValue,
            $ehloName,
            $hostname,
            $address
        );
        return \array_filter([$domainPart, $commentPart]);
    }
}
