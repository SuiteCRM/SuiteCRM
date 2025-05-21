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

class Message
{
    /**
     * Returns an array of messages matching the given search criteria.
     *
     * @param $imap
     * @param $criteria
     * @param $flags
     * @param $charset
     *
     * @return array|false|mixed
     */
    public static function search($imap, $criteria, $flags = SE_FREE, $charset = "")
    {
        if (!is_a($imap, Connection::class)) {
            return Errors::invalidImapConnection(debug_backtrace(), 1, false);
        }

        $client = $imap->getClient();
        #$client->setDebug(true);

        $result = $client->search($imap->getMailboxName(), $criteria, $flags & SE_UID);

        if (empty($result->count())) {
            return false;
        }

        $messages = $result->get();
        foreach ($messages as &$message) {
            $message = is_numeric($message) ? intval($message) : $message;
        }

        return $messages;
    }

    public static function sort($imap, $criteria, $reverse, $flags = 0, $searchCriteria = null, $charset = null)
    {
        if (is_a($imap, Connection::class)) {
            $client = $imap->getClient();
            #$client->setDebug(true);

            $result = $client->search($imap->getMailboxName(), $criteria, $flags & SE_UID);

            if (empty($result->count())) {
                return false;
            }

            $messages = $result->get();
            foreach ($messages as &$message) {
                $message = is_numeric($message) ? intval($message) : $message;
            }

            return $messages;
        }

        return imap_sort($imap, $criteria, $reverse, $flags, $searchCriteria, $charset);
    }

    public static function headerInfo($imap, $messageNum, $fromLength = 0, $subjectLength = 0, $defaultHost = null)
    {
        if (!is_a($imap, Connection::class)) {
            return Errors::invalidImapConnection(debug_backtrace(), 1, false);
        }

        $client = $imap->getClient();
        #$client->setDebug(true);

        $messages = $client->fetch($imap->getMailboxName(), $messageNum, false, [
            'BODY.PEEK[HEADER.FIELDS (SUBJECT FROM TO CC REPLY-TO DATE SIZE REFERENCES)]',
            'ENVELOPE',
            'INTERNALDATE',
            'UID',
            'FLAGS',
            'RFC822.SIZE',
            'RFC822.HEADER'
        ]);

        if (empty($messages)) {
            return false;
        }

        foreach ($messages as $message) {
            return HeaderInfo::fromMessage($message, $defaultHost);
        }
    }

    public static function headers($imap)
    {
        if (!is_a($imap, Connection::class)) {
            return Errors::invalidImapConnection(debug_backtrace(), 1, false);
        }

        $client = $imap->getClient();
        #$client->setDebug(true);

        $status = $client->status($imap->getMailboxName(), ['MESSAGES']);
        if (empty($status['MESSAGES'])) {
            return [];
        }

        $sequence = '1:'.intval($status['MESSAGES']);
        $messages = $client->fetch($imap->getMailboxName(), $sequence, false, [
            'BODY.PEEK[HEADER.FIELDS (SUBJECT FROM TO CC REPLYTO MESSAGEID DATE SIZE REFERENCES)]',
            #'UID',
            'FLAGS',
            'INTERNALDATE',
            'RFC822.SIZE',
            #'ENVELOPE',
            'RFC822.HEADER'
        ]);

        if (empty($messages)) {
            return [];
        }

        $headers = [];
        foreach ($messages as $message) {
            $from = ' ';
            if ($message->from != 'no_host') {
                $from = imap_rfc822_parse_adrlist($message->from, 'no_host');
                $from = isset($from[0]->personal) ? $from[0]->personal : $message->from;
            }

            $date = explode(' ', $message->internaldate);
            $subject = empty($message->subject) ? ' ' : $message->subject;
            $unseen = empty($message->flags['SEEN']) ? 'U' : ' ';
            $flagged = empty($message->flags['FLAGGED']) ? ' ' : 'F';
            $answered = empty($message->flags['ANSWERED']) ? ' ' : 'A';
            $draft = empty($message->flags['DRAFT']) ? ' ' : 'D';
            $deleted = empty($message->flags['DELETED']) ? ' ' : 'X';

            $header = ' ' . $unseen . $flagged . $answered . $draft . $deleted . ' '
                    . str_pad($message->id, 3, ' ', STR_PAD_LEFT) . ')' . $date[0] .' ' . str_pad($from, 20, ' ') . ' '
                    . substr($subject, 0, 25) . ' (' . $message->size . ' chars)';

            $headers[] = $header;
        }

        return $headers;
    }

    public static function body($imap, $messageNum, $flags = 0)
    {
        if (!is_a($imap, Connection::class)) {
            return Errors::invalidImapConnection(debug_backtrace(), 1, false);
        }

        $client = $imap->getClient();
        #$client->setDebug(true);

        $isUid = boolval($flags & FT_UID);

        $messages = $client->fetch($imap->getMailboxName(), $messageNum, $isUid, ['BODY[TEXT]']);

        return $messages[$messageNum]->bodypart['TEXT'];
    }

    public static function fetchBody($imap, $messageNum, $section, $flags = 0)
    {
        if (!is_a($imap, Connection::class)) {
            return Errors::invalidImapConnection(debug_backtrace(), 1, false);
        }

        $client = $imap->getClient();
        #$client->setDebug(true);

        $isUid = boolval($flags & FT_UID);
        $messages = $client->fetch($imap->getMailboxName(), $messageNum, $isUid, ['BODY['.$section.']']);

        if (empty($messages)) {
            trigger_error(Errors::badMessageNumber(debug_backtrace(), 1), E_USER_WARNING);

            return false;
        }

        if ($section) {
            return $messages[$messageNum]->bodypart[$section];
        }

        return $messages[$messageNum]->body;
    }

    public static function fetchMime($imap, $messageNum, $section, $flags = 0)
    {
        if (!is_a($imap, Connection::class)) {
            return Errors::invalidImapConnection(debug_backtrace(), 1, false);
        }

        if ($messageNum <= 0) {
            trigger_error(Errors::badMessageNumber(debug_backtrace(), 1), E_USER_WARNING);

            return false;
        }

        $client = $imap->getClient();
        #$client->setDebug(true);

        $isUid = boolval($flags & FT_UID);

        $sectionKey = $section.'.MIME';
        $messages = $client->fetch($imap->getMailboxName(), $messageNum, $isUid, ['BODY['.$sectionKey.']']);

        if (empty($messages)) {
            return "";
        }

        if ($section && isset($messages[$messageNum]->bodypart[$sectionKey])) {
            return $messages[$messageNum]->bodypart[$sectionKey];
        }

        return $messages[$messageNum]->body;
    }

    public static function saveBody($imap, $file, $messageNum, $section = "", $flags = 0)
    {
        if (is_a($imap, Connection::class)) {
            $client = $imap->getClient();
            #$client->setDebug(true);

            $messages = $client->fetch($imap->getMailboxName(), $messageNum, false, ['BODY['.$section.']']);

            $body = $section ? $messages[$messageNum]->bodypart[$section] : $messages[$messageNum]->body;

            return file_put_contents($file, $body);
        }

        return imap_savebody($imap, $file, $messageNum, $section, $flags);
    }

    public static function fetchStructure($imap, $messageNum, $flags = 0)
    {
        if (!is_a($imap, Connection::class)) {
            return Errors::invalidImapConnection(debug_backtrace(), 1, false);
        }

        $client = $imap->getClient();
        #$client->setDebug(true);

        $isUid = boolval($flags & FT_UID);

        $messages = $client->fetch($imap->getMailboxName(), $messageNum, $isUid, ['BODYSTRUCTURE']);

        if (empty($messages)) {
            return false;
        }

        foreach ($messages as $message) {
            return BodyStructure::fromMessage($message);
        }
    }

    public static function bodyStruct($imap, $messageNum, $flags = 0)
    {
        if (!is_a($imap, Connection::class)) {
            return Errors::invalidImapConnection(debug_backtrace(), 1, false);
        }

        $client = $imap->getClient();
        #$client->setDebug(true);

        $messages = $client->fetch($imap->getMailboxName(), $messageNum, false, ['BODY['.$section.']']);

        if ($section) {
            return $messages[$messageNum]->bodypart[$section];
        }

        return $messages[$messageNum]->body;
    }

    public static function fetchHeader($imap, $messageNum, $flags = 0)
    {
        if (!is_a($imap, Connection::class)) {
            return Errors::invalidImapConnection(debug_backtrace(), 1, false);
        }

        /*
         * FT_UID - The message_num argument is a UID
            FT_INTERNAL - The return string is in "internal" format, without any attempt to canonicalize to CRLF newlines
            FT_PREFETCHTEXT
         * */

        $client = $imap->getClient();
        #$client->setDebug(true);

        $isUid = boolval($flags & FT_UID);

        $messages = $client->fetch($imap->getMailboxName(), $messageNum, $isUid, ['BODY[HEADER]']);

        if (empty($messages)) {
            return false;
        }

        foreach ($messages as $message) {
            return $message->bodypart['HEADER'] ?? false;
        }
    }

    public static function fetchOverview($imap, $sequence, $flags = 0)
    {
        if (!is_a($imap, Connection::class)) {
            return Errors::invalidImapConnection(debug_backtrace(), 1, false);
        }

        $client = $imap->getClient();
        #$client->setDebug(true);

        $messages = $client->fetch($imap->getMailboxName(), $sequence, false, [
            'BODY[HEADER.FIELDS (SUBJECT FROM TO CC REPLYTO MESSAGEID DATE SIZE REFERENCES)]',
            'UID',
            'FLAGS',
            'INTERNALDATE',
            'RFC822.SIZE',
            'ENVELOPE',
            'RFC822.HEADER'
        ]);

        if ($sequence != '*' && count($messages) < Functions::expectedNumberOfMessages($sequence)) {
            return [];
        }

        $overview = [];
        foreach ($messages as $message) {
            #var_dump($message);
            #die();
            $messageEntry = (object) [
                'subject' => $message->envelope[1],
                'from' => Functions::writeAddressFromEnvelope($message->envelope[2]),
                'to' => $message->get('to'),
                'date' => $message->envelope[0],
                'message_id' => $message->envelope[9],
                'references' => $message->references,
                'in_reply_to' => $message->envelope[8],
                'size' => $message->size,
                'uid' => $message->uid,
                'msgno' => $message->id,
                'recent' => intval($message->flags['RECENT'] ?? 0),
                'flagged' => intval($message->flags['FLAGGED'] ?? 0),
                'answered' => intval($message->flags['ANSWERED'] ?? 0),
                'deleted' => intval($message->flags['DELETED'] ?? 0),
                'seen' => intval($message->flags['SEEN'] ?? 0),
                'draft' => intval($message->flags['DRAFT'] ?? 0),
                'udate' => strtotime($message->internaldate),
            ];

            if (empty($messageEntry->subject)) {
                unset($messageEntry->subject);
            }

            if (empty($messageEntry->references)) {
                unset($messageEntry->references);
            }

            if (empty($messageEntry->in_reply_to)) {
                unset($messageEntry->in_reply_to);
            }

            if (empty($messageEntry->to)) {
                unset($messageEntry->to);
            }

            $overview[] = $messageEntry;
        }

        return $overview;
    }

    public static function delete($imap, $messageNums, $flags = 0)
    {
        if (!is_a($imap, Connection::class)) {
            return Errors::invalidImapConnection(debug_backtrace(), 1, false);
        }

        $client = $imap->getClient();

        $messages = $client->fetch($imap->getMailboxName(), $messageNums, false, ['UID']);

        $uid = [];
        foreach ($messages as $message) {
            $uid[] = $message->uid;
        }

        $client->flag($imap->getMailboxName(), implode(',', $uid), $client->flags['DELETED']);

        return true;
    }

    public static function undelete($imap, $messageNums, $flags = 0)
    {
        if (is_a($imap, Connection::class)) {
            $client = $imap->getClient();
            #$client->setDebug(true);

            $messages = $client->fetch($imap->getMailboxName(), $messageNums, false, ['UID']);
            foreach ($messages as $message) {
                $client->unflag($imap->getMailboxName(), $message->uid, $client->flags['DELETED']);
            }

            return true;
        }

        return imap_undelete($imap, $messageNums, $flags);
    }

    public static function expunge($imap)
    {
        if (is_a($imap, Connection::class)) {
            $client = $imap->getClient();

            return $client->expunge($imap->getMailboxName());
        }

        return imap_expunge($imap);
    }

    /**
     * Sets flags on messages.
     *
     * @param $imap
     * @param $sequence
     * @param $flag
     * @param $options
     *
     * @return bool|void
     */
    public static function setFlagFull($imap, $sequence, $flag, $options = 0)
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

    /**
     * Clears flags on messages.
     *
     * @param $imap
     * @param $sequence
     * @param $flag
     * @param $options
     *
     * @return false|string
     */
    public static function clearFlagFull($imap, $sequence, $flag, $options = 0)
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

        $client->unflag($imap->getMailboxName(), $sequence, strtoupper(substr($flag, 1)));

        return false;
    }

    public static function msgno($imap, $messageUid)
    {
        if (!is_a($imap, Connection::class)) {
            return Errors::invalidImapConnection(debug_backtrace(), 1, false);
        }

        $client = $imap->getClient();

        $msgNo = ImapHelpers::uidToId($imap, $messageUid);

        return is_numeric($msgNo) ? intval($msgNo) : $msgNo;
    }

    public static function uid($imap, $messageNum)
    {
        if (!is_a($imap, Connection::class)) {
            return Errors::invalidImapConnection(debug_backtrace(), 1, false);
        }

        $uid = ImapHelpers::idToUid($imap, $messageNum);

        return is_numeric($uid) ? intval($uid) : $uid;
    }
}
