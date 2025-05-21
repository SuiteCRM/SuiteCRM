<?php

/**
+-----------------------------------------------------------------------+
| This file is part of the Roundcube Webmail client                     |
|                                                                       |
| Copyright (C) The Roundcube Dev Team                                  |
| Copyright (C) Kolab Systems AG                                        |
|                                                                       |
| Licensed under the GNU General Public License version 3 or            |
| any later version with exceptions for skins & plugins.                |
| See the README file for a full license statement.                     |
|                                                                       |
| PURPOSE:                                                              |
|   E-mail message headers representation                               |
+-----------------------------------------------------------------------+
| Author: Aleksander Machniak <alec@alec.pl>                            |
+-----------------------------------------------------------------------+
 */

namespace Javanile\Imap2\Roundcube;

/**
 * Struct representing an e-mail message header
 *
 * @package    Framework
 * @subpackage Storage
 */
class MessageHeader
{
    /**
     * Message sequence number
     *
     * @var int
     */
    public $id;

    /**
     * Message unique identifier
     *
     * @var int
     */
    public $uid;

    /**
     * Message subject
     *
     * @var string
     */
    public $subject;

    /**
     * Message sender (From)
     *
     * @var string
     */
    public $from;

    /**
     * Message recipient (To)
     *
     * @var string
     */
    public $to;

    /**
     * Message additional recipients (Cc)
     *
     * @var string
     */
    public $cc;

    /**
     * Message Reply-To header
     *
     * @var string
     */
    public $replyto;

    /**
     * Message In-Reply-To header
     *
     * @var string
     */
    public $in_reply_to;

    /**
     * Message date (Date)
     *
     * @var string
     */
    public $date;

    /**
     * Message identifier (Message-ID)
     *
     * @var string
     */
    public $messageID;

    /**
     * Message size
     *
     * @var int
     */
    public $size;

    /**
     * Message encoding
     *
     * @var string
     */
    public $encoding;

    /**
     * Message charset
     *
     * @var string
     */
    public $charset;

    /**
     * Message Content-type
     *
     * @var string
     */
    public $ctype;

    /**
     * Message timestamp (based on message date)
     *
     * @var int
     */
    public $timestamp;

    /**
     * IMAP bodystructure string
     *
     * @var string
     */
    public $bodystructure;

    /**
     * IMAP internal date
     *
     * @var string
     */
    public $internaldate;

    /**
     * Message References header
     *
     * @var string
     */
    public $references;

    /**
     * Message priority (X-Priority)
     *
     * @var int
     */
    public $priority;

    /**
     * Message receipt recipient
     *
     * @var string
     */
    public $mdn_to;

    /**
     * IMAP folder this message is stored in
     *
     * @var string
     */
    public $folder;

    /**
     * Other message headers
     *
     * @var array
     */
    public $others = array();

    /**
     * Message flags
     *
     * @var array
     */
    public $flags = array();

    // map header to rcube_message_header object property
    private $obj_headers = array(
        'date'      => 'date',
        'from'      => 'from',
        'to'        => 'to',
        'subject'   => 'subject',
        'reply-to'  => 'replyto',
        'cc'        => 'cc',
        'bcc'       => 'bcc',
        'mbox'      => 'folder',
        'folder'    => 'folder',
        'content-transfer-encoding' => 'encoding',
        'in-reply-to'               => 'in_reply_to',
        'content-type'              => 'ctype',
        'charset'                   => 'charset',
        'references'                => 'references',
        'return-receipt-to'         => 'mdn_to',
        'disposition-notification-to' => 'mdn_to',
        'x-confirm-reading-to'      => 'mdn_to',
        'message-id'                => 'messageID',
        'x-priority'                => 'priority',
    );

    /**
     * Returns header value
     */
    public function get($name, $decode = true)
    {
        $name = strtolower($name);

        if (isset($this->obj_headers[$name])) {
            $value = $this->{$this->obj_headers[$name]};
        }
        else {
            $value = $this->others[$name];
        }

        if ($decode) {
            if (is_array($value)) {
                foreach ($value as $key => $val) {
                    $val         = Mime::decode_header($val, $this->charset);
                    $value[$key] = Charset::clean($val);
                }
            }
            else {
                $value = Mime::decode_header($value, $this->charset);
                $value = Charset::clean($value);
            }
        }

        return $value;
    }

    /**
     * Sets header value
     */
    public function set($name, $value)
    {
        $name = strtolower($name);

        if (isset($this->obj_headers[$name])) {
            $this->{$this->obj_headers[$name]} = $value;
        }
        else {
            $this->others[$name] = $value;
        }
    }


    /**
     * Factory method to instantiate headers from a data array
     *
     * @param array Hash array with header values
     * @return object rcube_message_header instance filled with headers values
     */
    public static function from_array($arr)
    {
        $obj = new MessageHeader;
        foreach ($arr as $k => $v)
            $obj->set($k, $v);

        return $obj;
    }
}

