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

use Javanile\Imap2\Roundcube\ImapClient;

class Mailbox
{
    public static function check($imap)
    {
        if (is_a($imap, Connection::class)) {
            $imap->selectMailbox();

            $client = $imap->getClient();
            $status = $client->status($imap->getMailboxName(), ['MESSAGES', 'RECENT']);

            return (object) [
                'Date' => date('D, j M Y G:i:s').' +0000 (UTC)',
                'Driver' => 'imap',
                'Mailbox' => $imap->getMailbox(),
                'Nmsgs' => intval($status['MESSAGES']),
                'Recent' => intval($status['RECENT']),
            ];

        } elseif (IMAP2_RETROFIT_MODE && is_resource($imap) && get_resource_type($imap) == 'imap') {
            return imap_check($imap);
        }

        // STIC Custom 20250325 JBL - Fix Fatal Error: Too few arguments to function
        // https://github.com/SinergiaTIC/SinergiaCRM/pull/315
        // this error still occurs with the latest javanile version (0.1.10): https://github.com/javanile/php-imap2/blob/main/src/Mailbox.php
        // trigger_error(Errors::invalidImapConnection(debug_backtrace(), 1), E_USER_WARNING);
        trigger_error(Errors::invalidImapConnection(debug_backtrace(), 1, false), E_USER_WARNING);
        // END STIC Custom

        return false;
    }

    public static function numMsg($imap)
    {
        if (!is_a($imap, Connection::class)) {
            return Errors::invalidImapConnection(debug_backtrace(), 1, false);
        }

        $imap->selectMailbox();
        $client = $imap->getClient();

        $status = $client->status($imap->getMailboxName(), ['MESSAGES']);

        return intval($status['MESSAGES']);
    }

    public static function numRecent($imap)
    {
        if (is_a($imap, Connection::class)) {
            $client = $imap->getClient();
            $imap->selectMailbox();

            return (object) [
                'Driver' => 'imap',
                'Mailbox' => $imap->getMailbox(),
                'Nmsgs' => $client->data['EXISTS'],
                'Recent' => $client->data['RECENT'],
            ];
        }

        return imap_check($imap);
    }

    public static function status($imap, $mailbox, $flags)
    {
        if (!is_a($imap, Connection::class)) {
            return Errors::invalidImapConnection(debug_backtrace(), 1, false);
        }

        $mailboxName = Functions::getMailboxName($mailbox);

        $client = $imap->getClient();

        $items = [];

        $statusKeys = [
            'MESSAGES' => 'messages',
            'UNSEEN' => 'unseen',
            'RECENT' => 'recent',
            'UIDNEXT' => 'uidnext',
            'UIDVALIDITY' => 'uidvalidity',
        ];

        if ($flags & SA_MESSAGES || $flags & SA_ALL) {
            $items[] = 'MESSAGES';
        }
        if ($flags & SA_RECENT || $flags & SA_ALL) {
            $items[] = 'RECENT';
        }
        if ($flags & SA_UNSEEN || $flags & SA_ALL) {
            $items[] = 'UNSEEN';
        }
        if ($flags & SA_UIDNEXT || $flags & SA_ALL) {
            $items[] = 'UIDNEXT';
        }
        if ($flags & SA_UIDVALIDITY || $flags & SA_ALL) {
            $items[] = 'UIDVALIDITY';
        }

        $status = $client->status($mailboxName, $items);

        if (empty($status)) {
            return false;
        }

        $returnStatus = [];
        foreach ($status as $key => $value) {
            $returnStatus[$statusKeys[$key]] = is_numeric($value) ? intval($value) : $value;
        }

        return (object) $returnStatus;
    }

    public static function mailboxMsgInfo($imap)
    {
        if (!is_a($imap, Connection::class)) {
            return Errors::invalidImapConnection(debug_backtrace(), 1, false);
        }

        $client = $imap->getClient();
        #$client->setDebug(true);

        $imap->selectMailbox();
        $mailboxName = $imap->getMailboxName();

        $status = $client->status($mailboxName, [
            'MESSAGES',
            'UNSEEN',
            'RECENT',
            'UIDNEXT',
            'UIDVALIDITY'
        ]);

        $mailboxInfo = [
            'Unread' => intval($status['UNSEEN']),
            'Deleted' => 0,
            'Nmsgs' => intval($status['MESSAGES']),
            'Size' => 0,
            'Date' => date('D, j M Y G:i:s').' +0000 (UTC)',
            'Driver' => 'imap',
            'Mailbox' => $imap->getMailbox(),
            'Recent' => intval($status['RECENT'])
        ];

        return (object) $mailboxInfo;
    }

    public static function list($imap, $reference, $pattern)
    {
        if (is_a($imap, Connection::class)) {
            $referenceParts = explode('}', $reference);
            $client = $imap->getClient();
            $return = [];
            $mailboxes = $client->listMailboxes($referenceParts[1], $pattern);
            foreach ($mailboxes as $mailbox) {
                if (in_array('\\Noselect', $client->data['LIST'][$mailbox])) {
                    continue;
                }
                $return[] = $referenceParts[0].'}'.$mailbox;
            }

            return $return;
        }

        return imap_list($imap, $reference, $pattern);
    }

    public static function listScan($imap, $reference, $pattern)
    {
        if (is_a($imap, Connection::class)) {
            $referenceParts = explode('}', $reference);
            $client = $imap->getClient();
            $return = [];
            $mailboxes = $client->listMailboxes($referenceParts[1], $pattern);
            foreach ($mailboxes as $mailbox) {
                if (in_array('\\Noselect', $client->data['LIST'][$mailbox])) {
                    continue;
                }
                $return[] = $referenceParts[0].'}'.$mailbox;
            }

            return $return;
        }

        return imap_list($imap, $reference, $pattern);
    }

    public static function getMailboxes($imap, $reference, $pattern)
    {
        if (!is_a($imap, Connection::class)) {
            return Errors::invalidImapConnection(debug_backtrace(), 1, false);
        }

        $referenceParts = explode('}', $reference);
        $client = $imap->getClient();
        #$client->setDebug(true);
        $return = [];
        $delimiter = $client->getHierarchyDelimiter();
        $mailboxes = $client->listMailboxes($referenceParts[1], $pattern);
        foreach ($mailboxes as $mailbox) {
            $attributesValue = Functions::getListAttributesValue($client->data['LIST'][$mailbox]);
            if ($mailbox == '[Gmail]' && $imap->getHost() == 'imap.gmail.com') {
                $attributesValue = 34;
            }
            $return[] = (object) [
                'name' => $referenceParts[0].'}'.$mailbox,
                'attributes' => $attributesValue,
                'delimiter' => $delimiter,
            ];
        }

        return $return;
    }

    public static function createMailbox($imap, $mailbox)
    {
        if (!is_a($imap, Connection::class)) {
            return Errors::invalidImapConnection(debug_backtrace(), 1, false);
        }

        $client = $imap->getClient();

        if ($mailbox[0] == '{') {
            $mailbox = (string) \preg_replace('/^{.+}/', '', $mailbox);
        }

        $success = $client->createFolder($mailbox);

        if (!$success) {
            Errors::appendError($client->getRawLastLine());
        }

        return $success;
    }

    public static function renameMailbox($imap, $from, $to)
    {
        if (is_a($imap, Connection::class)) {
            $client = $imap->getClient();

            return $client->createFolder($mailbox);
        }

        return imap_createmailbox($imap, $mailbox);
    }

    public static function deleteMailbox($imap, $mailbox)
    {
        if (!is_a($imap, Connection::class)) {
            return Errors::invalidImapConnection(debug_backtrace(), 1, false);
        }

        $client = $imap->getClient();

        if ($mailbox[0] == '{') {
            $mailbox = (string) \preg_replace('/^{.+}/', '', $mailbox);
        }

        $result = $client->execute('DELETE', array($client->escape($mailbox)), ImapClient::COMMAND_RAW_LASTLINE);

        $success = $result[0] == ImapClient::ERROR_OK;

        if (!$success && $imap->getRegistryValue('mailbox', $mailbox, 'deleted')) {
            Errors::appendError($result[1]);
        } elseif (!$success) {
            Errors::appendError("Can't delete mailbox {$mailbox}: no such mailbox");
        } else {
            $imap->setRegistryValue('mailbox', $mailbox, 'deleted', true);
        }

        return $success;
    }

    /**
     * Append a string message to a specified mailbox.
     *
     * @param $imap
     * @param $folder
     * @param $message
     * @param $options
     * @param $internalDate
     *
     * @return bool
     */
    public static function append($imap, $folder, $message, $options = null, $internalDate = null)
    {
        if (!is_a($imap, Connection::class)) {
            return Errors::invalidImapConnection(debug_backtrace(), 1, false);
        }

        $folderParts = explode('}', $folder);
        $client = $imap->getClient();

        $mailbox = empty($folderParts[1]) ? 'INBOX' : $folderParts[1];

        $success = $client->append($mailbox, $message);

        return boolval($success);
    }

    public static function getSubscribed($imap, $mailbox)
    {
        if (is_a($imap, Connection::class)) {
            $client = $imap->getClient();

            return $client->deleteFolder($mailbox);
        }

        return imap_deletemailbox($imap, $mailbox);
    }

    public static function listSubscribed($imap, $mailbox)
    {
        if (is_a($imap, Connection::class)) {
            $client = $imap->getClient();

            return $client->deleteFolder($mailbox);
        }

        return imap_deletemailbox($imap, $mailbox);
    }

    public static function subscribe($imap, $mailbox)
    {
        if (is_a($imap, Connection::class)) {
            $client = $imap->getClient();

            return $client->deleteFolder($mailbox);
        }

        return imap_deletemailbox($imap, $mailbox);
    }

    public static function unsubscribe($imap, $mailbox)
    {
        if (is_a($imap, Connection::class)) {
            $client = $imap->getClient();

            return $client->deleteFolder($mailbox);
        }

        return imap_deletemailbox($imap, $mailbox);
    }
}
