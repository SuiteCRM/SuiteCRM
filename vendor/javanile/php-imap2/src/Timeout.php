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

class Timeout
{
    protected static $timeout;

    public static function set($timeoutType, $timeout = -1)
    {
        if ($timeout == -1) {
            return self::get($timeoutType);
        }

        self::$timeout[$timeoutType] = $timeout;

        return true;
    }

    public static function get($timeoutType)
    {
        return self::$timeout[$timeoutType];
    }
}
