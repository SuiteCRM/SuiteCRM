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

class Errors
{
    protected static $alerts = [];

    protected static $errors = [];

    protected static $lastError;

    public static function appendAlert($alert)
    {
        self::$alerts[] = $alert;
    }

    public static function appendError($error)
    {
        self::$lastError = $error;

        self::$errors[] = $error;
    }

    public static function alerts()
    {
        if (empty(self::$alerts)) {
            return false;
        }

        $return = self::$alerts;

        self::$alerts = [];

        return $return;
    }

    public static function errors()
    {
        if (empty(self::$errors)) {
            return false;
        }

        $return = self::$errors;

        self::$errors = [];

        return $return;
    }

    public static function lastError()
    {
        if (empty(self::$lastError)) {
            return false;
        }

        return self::$lastError;
    }

    public static function raiseWarning($warning, $backtrace, $depth)
    {
        $message = $warning . ' in '.$backtrace[$depth]['file']. ' on line '.$backtrace[$depth]['line'];

        trigger_error($message, E_USER_WARNING);
    }

    public static function invalidImapConnection($backtrace, $depth, $return)
    {
        $warning = 'Invalid IMAP connection parameter for '.$backtrace[$depth]['function'].'() '
                 . 'at '.$backtrace[$depth]['file']. ' on line '.$backtrace[$depth]['line'].'. Source code';

        trigger_error($warning, E_USER_WARNING);

        return $return;
    }

    public static function couldNotOpenStream($mailbox, $backtrace, $depth)
    {
        if (isset($backtrace[$depth + 1]['function']) && $backtrace[$depth + 1]['function'] == 'imap_open') {
            $depth++;
        }

        return $backtrace[$depth]['function'].'(): Couldn\'t open stream '.$mailbox
             . ' in '.$backtrace[$depth]['file']. ' on line '.$backtrace[$depth]['line'].'. Source code';
    }

    public static function badMessageNumber($backtrace, $depth)
    {
        if (Functions::isBackportCall($backtrace, $depth)) {
            $depth++;
        }

        return $backtrace[$depth]['function'].'(): Bad message number in '
             . $backtrace[$depth]['file']. ' on line '.$backtrace[$depth]['line'].'. Source code';
    }

    public static function appendErrorCanNotOpen($mailbox, $error)
    {
        if ($mailbox[0] == '{') {
            $error = preg_replace("/^AUTHENTICATE [A-Z]+\d*:\s/i", '', $error);
            //$error = preg_replace("/^([A-Z]+\d+ )(OK|NO|BAD|BYE|PREAUTH)?\s/i", '', $error);
            $error = 'Can not authenticate to IMAP server: '.$error;
        } else {
            $error = "Can't open mailbox {$mailbox}: no such mailbox";
        }

        self::appendError($error);
    }
}
