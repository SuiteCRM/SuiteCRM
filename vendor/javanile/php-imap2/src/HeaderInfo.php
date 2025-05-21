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

class HeaderInfo
{

    public static function fromMessage($message, $defaultHost)
    {
        #file_put_contents('t3.json', json_encode($message, JSON_PRETTY_PRINT));

        $to = Functions::writeAddressFromEnvelope($message->envelope[5]);
        $cc = Functions::writeAddressFromEnvelope($message->envelope[6]);
        $from = Functions::writeAddressFromEnvelope($message->envelope[2]);
        $sender = Functions::writeAddressFromEnvelope($message->envelope[3]);

        if (empty($message->replyto)) {
            $replyTo = $from;
        } else {
            $replyTo = Functions::writeAddressFromEnvelope($message->envelope[4]);
        }

        $headerInfo = [
            'date' => $message->envelope[0],
            'Date' => $message->envelope[0],
            'subject' => $message->envelope[1],
            'Subject' => $message->envelope[1],
            'in_reply_to' => $message->envelope[8],
            'message_id' => $message->envelope[9],
            'references' => $message->references,
            'toaddress' => $to,
            'to' => self::parseAddressList($to, $defaultHost),
            'fromaddress' => $from,
            'from' => self::parseAddressList($from, $defaultHost),
            'ccaddress' => $cc,
            'cc' => self::parseAddressList($cc, $defaultHost),
            'reply_toaddress' => $replyTo,
            'reply_to' => self::parseAddressList($replyTo, $defaultHost),
            'senderaddress' => $sender,
            'sender' => self::parseAddressList($sender, $defaultHost),
            'Recent' => ' ',
            'Unseen' => isset($message->flags['SEEN']) && $message->flags['SEEN'] ? ' ' : 'U',
            'Flagged' => empty($message->flags['FLAGGED']) ? ' ' : 'F',
            'Answered' => empty($message->flags['ANSWERED']) ? ' ' : 'A',
            'Deleted' => empty($message->flags['DELETED']) ? ' ' : 'X',
            'Draft' => empty($message->flags['DRAFT']) ? ' ' : 'D',
            'Msgno' => str_pad($message->id, 4, ' ', STR_PAD_LEFT),
            'MailDate' => self::sanitizeMailDate($message->internaldate),
            'Size' => strval($message->size),
            'udate' => strtotime($message->internaldate)
        ];

        if (empty($headerInfo['subject'])) {
            unset($headerInfo['subject']);
            unset($headerInfo['Subject']);
        }

        if (empty($headerInfo['in_reply_to'])) {
            unset($headerInfo['in_reply_to']);
        }

        if (empty($headerInfo['references'])) {
            unset($headerInfo['references']);
        }

        if (empty($headerInfo['to'])) {
            unset($headerInfo['toaddress']);
            unset($headerInfo['to']);
        }

        if (empty($headerInfo['cc'])) {
            unset($headerInfo['ccaddress']);
            unset($headerInfo['cc']);
        }

        return (object) $headerInfo;
    }

    protected static function parseAddressList($address, $defaultHost)
    {
        $addressList = imap_rfc822_parse_adrlist($address, $defaultHost);
        $customAddressList = [];

        foreach ($addressList as $objectEntry) {
            $addressEntry = (object) [
                'personal' => $objectEntry->personal ?? null,
                'mailbox' => @$objectEntry->mailbox,
                'host' => @$objectEntry->host,
            ];

            if (empty($addressEntry->personal)) {
                unset($addressEntry->personal);
            }

            if (empty($addressEntry->host)) {
                unset($addressEntry->host);
            }

            if (empty($addressEntry->mailbox)) {
                continue;
            }

            $customAddressList[] = $addressEntry;
        }

        return $customAddressList;
    }

    public static function sanitizeMailDate($mailDate)
    {
        if ($mailDate[0] == '0') {
            $mailDate = ' '.substr($mailDate, 1);
        }

        return $mailDate;
    }
}

