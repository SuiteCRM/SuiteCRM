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

use ZBateson\MailMimeParser\Message;
use ZBateson\MailMimeParser\Header\HeaderConsts;

class Polyfill
{
    public static function convert8bit($string)
    {
        return $string;
    }

    public static function mimeHeaderDecode($string)
    {
        return $string;
    }

    public static function mutf7ToUtf8($string)
    {
        return $string;
    }

    public static function qPrint($string)
    {
        return $string;
    }

    public static function rfc822ParseAdrList($string, $defaultHost)
    {
        $message = Message::from('To: '.$string, false);

        return Functions::getAddressObjectList(
            $message->getHeader(HeaderConsts::TO)->getAddresses(),
            $defaultHost
        );
    }

    /**
     *
     * @param $headers
     * @param $defaultHostname
     *
     * @return mixed
     */
    public static function rfc822ParseHeaders($headers, $defaultHost = 'UNKNOWN')
    {
        $message = Message::from($headers, false);

        $date = $message->getHeaderValue(HeaderConsts::DATE);
        $subject = $message->getHeaderValue(HeaderConsts::SUBJECT);

        $hasReplyTo = $message->getHeader(HeaderConsts::REPLY_TO) !== null;
        $hasSender = $message->getHeader(HeaderConsts::SENDER) !== null;

        return (object) [
            'date' => $date,
            'Date' => $date,
            'subject' => $subject,
            'Subject' => $subject,
            'message_id' => '<'.$message->getHeaderValue(HeaderConsts::MESSAGE_ID).'>',
            'toaddress' => $message->getHeaderValue(HeaderConsts::TO),
            'to' => Functions::getAddressObjectList($message->getHeader(HeaderConsts::TO)->getAddresses()),
            'fromaddress' => $message->getHeaderValue(HeaderConsts::FROM),
            'from' => Functions::getAddressObjectList($message->getHeader(HeaderConsts::FROM)->getAddresses()),
            'reply_toaddress' => $message->getHeaderValue($hasReplyTo ? HeaderConsts::REPLY_TO : HeaderConsts::FROM),
            'reply_to' => Functions::getAddressObjectList($message->getHeader($hasReplyTo ? HeaderConsts::REPLY_TO : HeaderConsts::FROM)->getAddresses()),
            'senderaddress' => $message->getHeaderValue($hasSender ? HeaderConsts::SENDER : HeaderConsts::FROM),
            'sender' => Functions::getAddressObjectList($message->getHeader($hasSender ? HeaderConsts::SENDER : HeaderConsts::FROM)->getAddresses()),
        ];
    }

    public static function rfc822WriteHeaders($string)
    {
        return $string;
    }

    public static function utf7Decode($string)
    {
        return mb_convert_decoding($string, "UTF7-IMAP", "UTF-8");
    }

    public static function utf7Encode($string)
    {
        return mb_convert_encoding($string, "UTF-8", "UTF7-IMAP");
    }

    public static function utf8ToMutf7($string)
    {
        return $string;
    }

    public static function utf8($string)
    {
        return $string;
    }

    public static function mailCompose($envelope, $bodies)
    {
        return false;
    }
}
