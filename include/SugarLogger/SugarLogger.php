<?php

/**
 *
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

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once 'include/utils/file_utils.php';
require_once 'include/SugarLogger/LoggerManager.php';
require_once 'include/SugarLogger/LoggerTemplate.php';

/**
 * Default SugarCRM Logger
 * @api
 */
#[\AllowDynamicProperties]
class SugarLogger implements LoggerTemplate
{
    /**
     * properties for the SugarLogger
     */
    protected $logfile = 'suitecrm';
    protected $ext = '.log';
    protected $dateFormat = '%c';
    protected $logSize = '10MB';
    protected $maxLogs = 10;
    protected $filesuffix = "";
    protected $date_suffix = "";
    protected $log_dir = '.';
    protected $defaultPerms = 0664;


    /**
     * used for config screen
     */
    public static $filename_suffix = array(
        //bug#50265: Added none option for previous version users
        "" => "None",
        "%m_%Y"    => "Month_Year",
        "%d_%m"    => "Day_Month",
        "%m_%d_%y" => "Month_Day_Year",
        );

    /**
     * Let's us know if we've initialized the logger file
     */
    protected $initialized = false;

    /**
     * Logger file handle
     */
    protected $fp = false;

    public function __get(
        $key
        ) {
        return $this->$key;
    }

    /**
     * Used by the diagnostic tools to get SugarLogger log file information
     */
    public function getLogFileNameWithPath()
    {
        return $this->full_log_file;
    }

    /**
     * Used by the diagnostic tools to get SugarLogger log file information
     */
    public function getLogFileName()
    {
        return ltrim($this->full_log_file, "./");
    }

    /**
     * Constructor
     *
     * Reads the config file for logger settings
     */
    public function __construct()
    {
        $config = SugarConfig::getInstance();
        $this->ext = $config->get('logger.file.ext', $this->ext);
        $this->logfile = $config->get('logger.file.name', $this->logfile);
        $this->dateFormat = $config->get('logger.file.dateFormat', $this->dateFormat);
        $this->logSize = $config->get('logger.file.maxSize', $this->logSize);
        $this->maxLogs = $config->get('logger.file.maxLogs', $this->maxLogs);
        $this->filesuffix = $config->get('logger.file.suffix', $this->filesuffix);
        $this->defaultPerms = $config->get('logger.file.perms', $this->defaultPerms);
        $log_dir = $config->get('log_dir', $this->log_dir);
        $this->log_dir = $log_dir . (empty($log_dir)?'':'/');
        unset($config);
        $this->_doInitialization();
        LoggerManager::setLogger('default', 'SugarLogger');
    }

    /**
     * Handles the SugarLogger initialization
     */
    protected function _doInitialization()
    {
        if ($this->filesuffix && array_key_exists($this->filesuffix, self::$filename_suffix)) { //if the global config contains date-format suffix, it will create suffix by parsing datetime
            $this->date_suffix = "_" . date(str_replace("%", "", $this->filesuffix));
        }
        $this->full_log_file = $this->log_dir . $this->logfile . $this->date_suffix . $this->ext;
        $this->initialized = $this->_fileCanBeCreatedAndWrittenTo();
        $this->rollLog();
    }

    /**
     * Checks to see if the SugarLogger file can be created and written to
     * @noinspection PhpUndefinedFieldInspection
     * @return bool
     */
    protected function _fileCanBeCreatedAndWrittenTo()
    {
        if (is_writable($this->full_log_file)) {
            return true;
        }

        if (!is_file($this->full_log_file)) {
            @touch($this->full_log_file);
            if ($this->defaultPerms !== false) {
                @chmod($this->full_log_file, $this->defaultPerms);
            }

            return is_writable($this->full_log_file);
        }

        return false;
    }

    private function getArguments($className, $funcName) {
        // https://stackoverflow.com/a/42600677/1189711
        $f = null;
        try {
            $f = (new ReflectionFunction($funcName))->getParameters();
        } catch(ReflectionException $funcExc) {
            try {
                $f = (new ReflectionClass($className))->getMethod($funcName)->getParameters();
            } catch (ReflectionException $classExc) {
                return [];
            }
        }
        return array_map( function( $parameter ) { return $parameter->name; }, $f);
    }

    /**
     * A fancy replacement for PHP's print_r, allowing depth-level limits and other formatting options
     *
     * @param mixed $data The data to be printed into text
     * @param int $maxLevel The maximum depth level for recursion into arrays or objects [optional, defaults to 5]
     * @param string $eol Use either '' or PHP_EOL to control whether newlines get added [optional, defaults to PHP_EOL]
     * @param int $trimChars Not a strict char limit, just a performance enhancement; avoid recursion when generating excess chars [optional, defaults to 100]
     * @param int $currLevel the current depth level into an object or array, used for recursion [optional, defaults to 1]
     * @param int $numSpaces Total spaces to indent, will be increased by 2 in each recursion [optional, defaults to 2]
     *
     * @return  string  Text representation of $data input
     */
    private function extendedPrintR($data, $maxLevel = 5, $eol = PHP_EOL, $trimChars = 100, $currLevel = 1, $numSpaces = 2)
    {
        $type = gettype($data);
        $spaces = str_repeat(' ', $numSpaces + 1);
        $print = '';
        $elements = array();

        if (in_array($type, array('object', 'array'))) {
            if ($type === 'object') {
                $print = get_class($data) . ' ' . ucfirst($type);
                $ref = new \ReflectionObject($data);

                foreach ($ref->getProperties() as $property) {
                    $property->setAccessible(true);
                    $pType = $property->getName();
                    // the @ operator is to ignore all notices in getValue:
                    @$elements[$pType] = $property->getValue($data);
                }
            }
            elseif ($type === 'array') {
                $print = 'Array';
                $elements = $data;
            }

            if ($maxLevel === 0 || $currLevel < $maxLevel) {
                $print .= " ("; // start of obj or arr
                foreach ($elements as $key => $element) {
                    $print .= $eol.($eol==PHP_EOL? "  {$spaces}" : '')." [{$key}] => ";
                    if (strlen($print) < $trimChars) {
                        $print .= in_array(gettype($element), array('object', 'array')) ?
                            $this->extendedPrintR($element, $maxLevel, $eol, $trimChars, $currLevel + 1, $numSpaces + 2) :
                            (is_string($element) ? ("'" . $element . "'") : $element);
                    } else {
                        return $print . '…'; // excess chars, finish everything early
                    }
                }
            } else {
                $print .= '…'; // excess levels, no more recursion in depth, but continue working in breadth
            }
        } else {
            // non-complex types, end of recursion:
            $print = is_string($data) ? ("'".$data."'") : $data;
        }
        return $print;
    }

    private function getRequestOverviewAsString($eol = false, $trimChars = 100, $maxDepth = 3) {
        $eol = $eol ? PHP_EOL : '';

        $context['$_REQUEST'] =  $_REQUEST;
        // the @ operator is to ignore all missing array keys, no point in generating notices here:
        @$context['Selected $GLOBALS'] = [
            'SuiteCRM Version' => $GLOBALS['suitecrm_version'],
            'db' => (isset($GLOBALS['db']) ? $GLOBALS['db']->connectOptions['db_name'] : ''),
            'BASE_DIR' => $GLOBALS['BASE_DIR'],
        ];
        if (isset($GLOBALS['app']->controller)) {
            @$context['SugarController'] = [
                'entryPointFile' => $GLOBALS['app']->controller->breadcrumbs['entryPointFile'],
                'processTasksHistory' => $GLOBALS['app']->controller->breadcrumbs['processTasksHistory'],
                'handleActionTasksHistory' => $GLOBALS['app']->controller->breadcrumbs['handleActionTasksHistory'],
                'redirect_url' => $GLOBALS['app']->controller->redirect_url,
            ];
        }
        /** @noinspection PhpComposerExtensionStubsInspection */
        $text = 'Request Overview:' .
            mb_strimwidth(trim($this->extendedPrintR($context, 4, $eol, 2000)), 0, 2000, ' …)');

        return $text;
    }

   /**
    * Print an entire exception trace as a string
    *
    * This is an advanced function allowing multiple formatting configurations,
    * and combining two subtly different sources of information:
    * - function names and argument values come in the dynamic information of
    *   the Exception object itself (where we are in the execution at the moment)
    * - but since that doesn't include argument names,
    *   we use also Reflection to get those (static information about what is in the files)
    *
    * @param Exception $Exception The Exception object we want to print, containing the message, stack trace, etc
    * @param boolean $eol Use true to include newlines in output, false to keep each frame to just one line [optional, defaults to false]
    * @param integer $trimChars Char limit for text output of each frame [optional, defaults to 100]
    *
    * @return  string  Text representation of Exception stack trace
    */
    private function getExceptionTraceAsString($exception, $eol = false, $trimChars = 100, $maxDepth = 3, $source = -1) {
        $ret = '';
        $count = 0;
        $eol = $eol ? PHP_EOL : '';
        $dumpArgs = '';

        $frames = $exception->getTrace();
        foreach ($frames as $frame) {
            if ((isset($frame['class']) && ($frame['class'] === 'SugarLogger')) ||
                (isset($frame['function']) && $frame['function'] === 'phpShutdownHandler')) {
                continue; // skip repetitive entries common to everything logged
            }
            try {
                // since PHP 7.4, args will only be available if php.ini includes: zend.exception_ignore_args = Off
                if (isset($frame['args'])) {
                    $args = array();
                    $argNames = $this->getArguments(isset($frame['class']) ? $frame['class'] : null, $frame['function']);

                    foreach ($frame['args'] as $paramsCount => $arg) {
                        $argName = '';
                        if (isset($argNames[$paramsCount]) and strlen($argNames[$paramsCount]) !== 0) {
                            $argName = "$argNames[$paramsCount]";
                        }
                        /** @noinspection PhpComposerExtensionStubsInspection */
                        $text = mb_strimwidth(trim($this->extendedPrintR($arg, $maxDepth, $eol, $trimChars)), 0, $trimChars, ' …)');
                        $args[] = ($eol===PHP_EOL ? '     ' : '').(strlen($argName)>0 ? "$argName: " : '')."$text";
                    }
                    $dumpArgs = join(', '.$eol, $args);
                }
            } catch(Exception $e) {
                // if something fails when getting an arg while we're dumping a backtrace, we just skip it
                $dumpArgs .= "### Exception in reflection: $e ###";
            }

            $sourceText = '';
            if (isset($frame['file']) && isset($frame['line'])) {
                $sourceText = ($source != -1 && $eol !== '') ?
                    $eol . $this->getFileSource($frame['file'], $frame['line'], $source) . '   Called' :
                    '';
            }
            $ret .= sprintf("#%s %s(%s): %s %s(%s%s)".PHP_EOL,
                $count++,
                isset($frame['file']) ? str_replace(SUGAR_PATH, '', $frame['file']) : 'unknown file',
                isset($frame['line']) ? $frame['line'] : 'unknown line',
                $sourceText,
                (isset($frame['class'])) ? $frame['class'] . $frame['type'] . $frame['function'] : $frame['function'],
                strstr($dumpArgs, PHP_EOL) ? $eol : '',
                $dumpArgs);
            $dumpArgs= '';
        }
        return $ret;
    }

    /**
     * Get a segment of a file's source code at a specific line, optionally with some surrounding context
     *
     * @param $fileName              The name of the PHP source file
     * @param $startLine             The focus line you want to get
     * @param int $additionalContext Number of additional context lines to get, both before and after the focus line
     * @return string                The source code with context and some formatting
     */
    private function getFileSource($fileName, $startLine, $additionalContext = 0) {
        $ret = '';
        try {
            $source = file($fileName);
            if (false !== $source) {
                $start = max((int)($startLine - 1 - $additionalContext), 0);
                $end = min((int)($startLine - 1 + $additionalContext), count($source) - 1);
                for ($line = $start; $line <= $end; $line++) {
                    $ret .= (($line == $start + intdiv($end - $start, 2)) ? '   >> ' : '   >  ') . $source[$line];
                }
            }
        } catch (Exception $e) {
        }
        return $ret;
    }

    /**
     * Show log
     *
     * and show a backtrace information in log when
     * the 'show_log_trace' config variable is set and true, or a string value to match a message.
     * Further configuration possible with 'show_log_trace_with_eol' and 'show_log_trace_trim' config vars.
     * see LoggerTemplate::log()
     */
    public function log($level, $message) {
        global $sugar_config;

        if (!$this->initialized) {
            return;
        }

        // change to a string if there is just one entry
        if (is_array($message) && count($message) == 1) {
            $message = array_shift($message);
        }
        // change to a human-readable array output if it's any other array
        if (is_array($message)) {
            $message = print_r($message, true);
        }

        // if any of these 2 regexps contain syntax errors, they will effectively be ignored:
        if (isset($sugar_config['show_log_regexp']) &&
            (0 === preg_match($sugar_config['show_log_regexp'], $message))) {
            return;
        }
        if (isset($sugar_config['show_log_regexp_exclude']) &&
            (1 === preg_match($sugar_config['show_log_regexp_exclude'], $message))) {
            return;
        }

        //lets get the current user id or default to -none- if it is not set yet
        $userID = (!empty($GLOBALS['current_user']->id))?$GLOBALS['current_user']->id:'-none-';

        //if we haven't opened a file pointer yet let's do that
        if (! $this->fp) {
            $this->fp = fopen($this->full_log_file, 'ab');
        }

        //write out to the file including the time in the dateFormat the process id , the user id , and the log level as well as the message
        fwrite(
            $this->fp,
            strftime($this->dateFormat) . ' [' .
                getmypid () . '][' . $userID . '][' .
                substr(str_pad(strtoupper($level), 5), 0, 5) . '] ' .
                $message . PHP_EOL
            );

        if (($level !== 'PHP S') && // with XDEBUG you will still see a basic trace for these, as part of the $message
            isset($sugar_config['show_log_trace']) &&
            (($sugar_config['show_log_trace'] === true) ||
                (is_string($sugar_config['show_log_trace']) && (strpos($message, $sugar_config['show_log_trace'])!== false)))) {

            $eolConfig = isset($sugar_config['show_log_trace_with_eol']) && $sugar_config['show_log_trace_with_eol'];

            if (isset($sugar_config['show_log_trace_trim'])) {
                $trimConfig = (int)$sugar_config['show_log_trace_trim'];
            }
            $trimConfig = (isset($trimConfig) && is_int($trimConfig) && $trimConfig > 0) ? $trimConfig : 100;

            if (isset($sugar_config['show_log_trace_depth'])) {
                $depthConfig = (int)$sugar_config['show_log_trace_depth'];
            }
            $depthConfig = (isset($depthConfig) && is_int($depthConfig) && $depthConfig > 0) ? $depthConfig : 5;

            if (isset($sugar_config['show_log_trace_source'])) {
                $sourceConfig = (int)$sugar_config['show_log_trace_source'];
            }
            $sourceConfig = (isset($sourceConfig) && is_int($sourceConfig) && $sourceConfig >= -1) ? $sourceConfig : -1;

            $overviewConfig = isset($sugar_config['show_log_trace_overview']) && $sugar_config['show_log_trace_overview'];

            $e = new \Exception;
            fwrite(
                $this->fp,
                $this->getExceptionTraceAsString($e, $eolConfig, $trimConfig, $depthConfig, $sourceConfig) . PHP_EOL .
                ($overviewConfig ? ($this->getRequestOverviewAsString($eolConfig, $trimConfig, $depthConfig) . PHP_EOL . '      )' . PHP_EOL) : '')
            );
        }
    }


    /**
     * rolls the logger file to start using a new file
     */
    protected function rollLog(
        $force = false
        ) {
        if (!$this->initialized || empty($this->logSize)) {
            return;
        }
        // bug#50265: Parse the its unit string and get the size properly
        $units = array(
            'b' => 1,                   //Bytes
            'k' => 1024,                //KBytes
            'm' => 1024 * 1024,         //MBytes
            'g' => 1024 * 1024 * 1024,  //GBytes
        );
        if (preg_match('/^\s*([0-9]+\.[0-9]+|\.?[0-9]+)\s*(k|m|g|b)(b?ytes)?/i', $this->logSize, $match)) {
            $rollAt = ( int ) $match[1] * $units[strtolower($match[2])];
        }
        //check if our log file is greater than that or if we are forcing the log to roll if and only if roll size assigned the value correctly
        if ($force || ($rollAt && filesize($this->full_log_file) >= $rollAt)) {
            //now lets move the logs starting at the oldest and going to the newest
            for ($i = $this->maxLogs - 2; $i > 0; $i --) {
                if (file_exists($this->log_dir . $this->logfile . $this->date_suffix . '_'. $i . $this->ext)) {
                    $to = $i + 1;
                    $old_name = $this->log_dir . $this->logfile . $this->date_suffix . '_'. $i . $this->ext;
                    $new_name = $this->log_dir . $this->logfile . $this->date_suffix . '_'. $to . $this->ext;
                    //nsingh- Bug 22548  Win systems fail if new file name already exists. The fix below checks for that.
                    //if/else branch is necessary as suggested by someone on php-doc ( see rename function ).
                    sugar_rename($old_name, $new_name);

                    //rename ( $this->logfile . $i . $this->ext, $this->logfile . $to . $this->ext );
                }
            }
            //now lets move the current .log file
            sugar_rename($this->full_log_file, $this->log_dir . $this->logfile . $this->date_suffix . '_1' . $this->ext);
        }
    }

    /**
     * This is needed to prevent unserialize vulnerability
     */
    public function __wakeup()
    {
        // clean all properties
        foreach (get_object_vars($this) as $k => $v) {
            $this->$k = null;
        }
        throw new Exception("Not a serializable object");
    }

    /**
     * Destructor
     *
     * Closes the SugarLogger file handle
     */
    public function __destruct()
    {
        if ($this->fp) {
            fclose($this->fp);
            $this->fp = false;
        }
    }
}
