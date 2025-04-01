<?php
/**
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUGARCRM, SUGARCRM DISCLAIMS THE WARRANTY
 * OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License along with
 * this program; if not, see http://www.gnu.org/licenses or write to the Free
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 *
 * You can contact SugarCRM, Inc. headquarters at 10050 North Wolfe Road,
 * SW2-130, Cupertino, CA 95014, USA. or at email address contact@sugarcrm.com.
 *
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo and "Supercharged by SuiteCRM" logo. If the display of the logos is not
 * reasonably feasible for technical reasons, the Appropriate Legal Notices must
 * display the words "Powered by SugarCRM" and "Supercharged by SuiteCRM".
 */

namespace SuiteCRM\Log;

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

use Monolog\Formatter\FormatterInterface;
use Monolog\Logger;

/**
 * CliLoggerFormatter for CliLoggerHandler.
 */
#[\AllowDynamicProperties]
class CliLoggerFormatter implements FormatterInterface
{
    /**  @var array a list of the available colours for quicker usage */
    private $colors = [];
    /** @var string the format of the log line */
    private $format;
    /** @var string string used for padding new lines */
    private $padding;
    /** @var bool if set to false only warning with levels >= warn will be colour formatted */
    private $alwaysColourLine = true;

    /**
     * Creates the colours and initializes the format.
     */
    public function __construct()
    {
        $this->colors = $this->getColors();

        $color = &$this->colors;

        $this->format = "[{$color['bold']}%s{$color['reset']}][%s] {$color['bold']}%s" . PHP_EOL;

        $this->padding =
            PHP_EOL
            . $color['reset']
            . $color['gray-dark']
            . str_repeat('  ~', 4)
            . $color['reset']
            . str_repeat(' ', 2);
    }

    /**
     * Formats a set of log records.
     *
     * @param  array $records A set of records to format
     *
     * @return mixed The formatted set of records
     */
    public function formatBatch(array $records)
    {
        $formatted = [];
        foreach ($records as $record) {
            $formatted = $this->format($record);
        }
        return $formatted;
    }

    /**
     * Formats a log record.
     *
     * @param  array $record A record to format
     *
     * @return mixed The formatted record
     */
    public function format(array $record)
    {
        $level = $record['level'];
        $message = $record['message'];

        list($color, $code) = $this->getColourAndCode($level);

        if ($level >= Logger::WARNING || $this->alwaysColourLine) {
            $message = $color . $message . $this->colors['reset'];
        }

        $message = preg_replace("/\n\s*/", $this->padding . $color, (string) $message);

        $time = (new \DateTime())->format('H:i:s');

        return sprintf($this->format, $color . $code, $time, $message);
    }

    /**
     * Creates an array with the available colours.
     *
     * @return array
     */
    protected function getColors()
    {
        return [
            'white' => $this->code(97),
            'gray' => $this->code(37),
            'gray-dark' => $this->code(90),
            'cyan' => $this->code(36),
            'cyan-light' => $this->code(96),
            'blue' => $this->code(34),
            'blue-light' => $this->code(94),
            'green' => $this->code(32),
            'green-light' => $this->code(92),
            'yellow' => $this->code(33),
            'yellow-light' => $this->code(93),
            'purple' => $this->code(95),
            'red' => $this->code(31),
            'red-light' => $this->code(91),
            'bg-red' => $this->code(41),
            'bg-red-light' => $this->code(101),
            'bold' => $this->code(1), // this will also make colours lighter
            'reverse' => $this->code(7),
            'reset' => $this->code(0),
        ];
    }

    /**
     * Utility to make a unix terminal escape sequence given the code.
     *
     * @param int $code
     *
     * @return string
     */
    protected function code($code)
    {
        return "\e[{$code}m";
    }

    /**
     * Retrieves the right colour formatting and symbol to show in brackets.
     *
     * @param int $level
     *
     * @return string[]
     */
    protected function getColourAndCode($level)
    {
        switch ($level) {
            case Logger::INFO:
                $color = $this->colors['cyan'];
                $code = '=';
                break;
            case Logger::NOTICE:
                $color = $this->colors['green'];
                $code = '?';
                break;
            case Logger::WARNING:
                $color = $this->colors['yellow'];
                $code = '*';
                break;
            case Logger::ERROR:
                $color = $this->colors['red'];
                $code = '!';
                break;
            case Logger::CRITICAL:
                $color = $this->colors['bg-red-light'];
                $code = '!';
                break;
            case Logger::EMERGENCY:
                $color = $this->colors['bg-red'];
                $code = '!';
                break;
            case Logger::DEBUG:
            default:
                $color = $this->colors['blue'];
                $code = '@';
                break;
        }
        return [$color, $code];
    }
}
