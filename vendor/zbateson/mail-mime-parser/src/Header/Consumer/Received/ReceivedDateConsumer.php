<?php
/**
 * This file is part of the ZBateson\MailMimeParser project.
 *
 * @license http://opensource.org/licenses/bsd-license.php BSD
 */

namespace ZBateson\MailMimeParser\Header\Consumer\Received;

use ZBateson\MailMimeParser\Header\Consumer\DateConsumer;

/**
 * Parses the date portion of a Received header into a DatePart.
 *
 * The only difference between DateConsumer and ReceivedDateConsumer is the
 * addition of a start token, ';', and a token separator (also ';').
 *
 * @author Zaahid Bateson
 */
class ReceivedDateConsumer extends DateConsumer
{
    /**
     * Returns true if the token is a ';'
     */
    protected function isStartToken(string $token) : bool
    {
        return ($token === ';');
    }

    /**
     * Returns an array containing ';'.
     *
     * @return string[] an array of regex pattern matchers
     */
    protected function getTokenSeparators() : array
    {
        return [';'];
    }
}
