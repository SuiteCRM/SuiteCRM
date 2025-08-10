<?php

/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2020 SalesAgility Ltd.
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

namespace SuiteCRM\Utility;

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once 'include/SugarLogger/LoggerManager.php';

/**
 * Class SuiteHandlers
 *
 * Intercepts PHP errors so they can be logged together in suitecrm.log,
 * making use of the rich stack traces and other logging features.
 */
class SuiteHandlers implements \Serializable
{
    protected $typeStrings = Array(
        1 => 'E_ERROR',
        2 => 'E_WARNING',
        4 => 'E_PARSE',
        8 => 'E_NOTICE',
        16 => 'E_CORE_ERROR',
        32 => 'E_CORE_WARNING',
        64 => 'E_COMPILE_ERROR',
        128 => 'E_COMPILE_WARNING',
        256 => 'E_USER_ERROR',
        512 => 'E_USER_WARNING',
        1024 => 'E_USER_NOTICE',
        2048 => 'E_STRICT',
        4096 => 'E_RECOVERABLE_ERROR',
        8192 => 'E_DEPRECATED',
        16384 => 'E_USER_DEPRECATED',
    );

    protected $logger;

    /**
     * SuiteHandlers constructor.
     *
     * Should only be called when $sugar_config['show_log_with_php_messages'] === true;
     */
    public function __construct()
    {
        $this->logger = \LoggerManager::getLogger();
        register_shutdown_function(array($this, 'phpShutdownHandler'));
        $previousErrorHandler = set_error_handler(array($this, 'phpErrorsHandler'), E_ALL & ~E_NOTICE & ~E_USER_NOTICE);
    }

    /**
     * Checks for a PHP FATAL or PARSE error, which set_error_handler won't see.
     * Stack traces won't normally be possible here; often they are not needed, a simple file and
     * line number is always enough to fix a PARSE error, for example.
     * Systems with XDebug will sometimes get a simple stack trace included with the msg
     */
    public function phpShutdownHandler() {
        $error = error_get_last();
        if (null !== $error) {
            $msg = '[' . $this->typeStrings[$error['type']] . "] '" . $error['message'] . "' at " . $error['file'] . ' (' . $error['line'] . ')';
            $this->logger->__call('PHP S', str_replace('Stack trace:', 'XDebug Stack trace:', $msg));
        }
    }

    public function phpErrorsHandler($errno, $errstr, $errfile, $errline, $errcontext) {
        $msg = "[".$this->typeStrings[$errno]."] '$errstr' at $errfile ($errline)";
        // Note: $errcontext is an array of every variable that existed in the scope the error was triggered in.
        // It will be visible in the call stack if arguments are printed.

        $this->logger->__call('PHP E',  $msg);
        return true;
    }

    /**
     * @inheritDoc
     */
    public function serialize()
    {
        // this was needed to pass Travis tests...
        return '';
    }

    /**
     * @inheritDoc
     */
    public function unserialize($serialized)
    {
        // this was needed to pass Travis tests...
    }
}