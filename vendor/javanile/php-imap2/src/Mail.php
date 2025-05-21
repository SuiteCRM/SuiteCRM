<?php

/*
 * This file is part of the PHP IMAP2 package.
 *
 * (c) Francesco Bianco <bianco@javanile.org>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

namespace Javanile\Imap2;

class Mail
{
    /**
     * Copy specified messages to a mailbox.
     *
     * @param $imap
     * @param $messageNums
     * @param $mailbox
     * @param $flags
     *
     * @return false|mixed
     */
    public static function copy($imap, $messageNums, $mailbox, $flags = 0)
    {
        if (!is_a($imap, Connection::class)) {
            return Errors::invalidImapConnection(debug_backtrace(), 1, false);
        }

        if ($flags & CP_MOVE) {
            return Mail::move($imap, $messageNums, $mailbox, $flags);
        }

        $client = $imap->getClient();

        if (!($flags & CP_UID)) {
            $messageNums = ImapHelpers::idToUid($imap, $messageNums);
        }

        $from = $imap->getMailboxName();
        $to = $mailbox;

        return $client->copy($messageNums, $from, $to);
    }

    /**
     * Move specified messages to a mailbox.
     *
     * @param $imap
     * @param $messageNums
     * @param $mailbox
     * @param $flags
     *
     * @return false|mixed
     */
    public static function move($imap, $messageNums, $mailbox, $flags = 0)
    {
        if (!is_a($imap, Connection::class)) {
            return Errors::invalidImapConnection(debug_backtrace(), 1, false);
        }

        $client = $imap->getClient();
        #$client->setDebug(true);

        if (!($flags & CP_UID)) {
            $messageNums = ImapHelpers::idToUid($imap, $messageNums);
        }

        return $client->move($messageNums, $imap->getMailboxName(), $mailbox);
    }

    /**
     * Send an email message.
     *
     * @param $to
     * @param $subject
     * @param $message
     * @param $additionalHeaders
     * @param $cc
     * @param $bcc
     * @param $returnPath
     *
     * @return false|mixed
     */
    public static function send($to, $subject, $message, $additionalHeaders = null, $cc = null, $bcc = null, $returnPath = null)
    {
        if (!is_a($imap, Connection::class)) {
            return Errors::invalidImapConnection(debug_backtrace(), 1, false);
        }

        $client = $imap->getClient();

        if (!($options & ST_UID)) {
            $messages = $client->fetch($imap->getMailboxName(), $sequence, false, ['UID']);

            $uid = [];
            foreach ($messages as $message) {
                $uid[] = $message->uid;
            }

            $sequence = implode(',', $uid);
        }

        $client->flag($imap->getMailboxName(), $sequence, strtoupper(substr($flag, 1)));

        return false;
    }
}
