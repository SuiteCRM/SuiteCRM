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

namespace SuiteCRM\Utility;

use Psr\Log\AbstractLogger;
use Psr\Log\InvalidArgumentException;
use Psr\Log\LogLevel;

/**
 * /**
 * PSR-3 Compliant logger
 * Class SuiteLogger
 * @package SuiteCRM\Utility
 * @see http://www.php-fig.org/psr/psr-3/
 */
#[\AllowDynamicProperties]
class SuiteLogger extends AbstractLogger
{
    /**
     * @param LogLevel|string $level
     * @param string $message eg 'hello {user}'
     * @param array $context eg array(user => 'joe')
     * @throws InvalidArgumentException
     */
    public function log($level, $message, array $context = [])
    {
        $log = \LoggerManager::getLogger();
        $message = $this->interpolate($message, $context);
        switch ($level) {
            case LogLevel::EMERGENCY:
                /** @noinspection PhpUndefinedMethodInspection */
                $log->fatal('[EMERGENCY] ' . $message);
                break;
            case LogLevel::ALERT:
                /** @noinspection PhpUndefinedMethodInspection */
                $log->fatal('[ALERT] ' . $message);
                break;
            case LogLevel::CRITICAL:
                /** @noinspection PhpUndefinedMethodInspection */
                $log->fatal('[CRITICAL] ' . $message);
                break;
            case LogLevel::ERROR:
                /** @noinspection PhpUndefinedMethodInspection */
                $log->fatal('[ERROR] ' . $message);
                break;
            case LogLevel::WARNING:
                /** @noinspection PhpUndefinedMethodInspection */
                $log->warn('[WARNING] ' . $message);
                break;
            case LogLevel::NOTICE:
                /** @noinspection PhpUndefinedMethodInspection */
                $log->warn('[NOTICE] ' . $message);
                break;
            case LogLevel::INFO:
                /** @noinspection PhpUndefinedMethodInspection */
                $log->info('[INFO] ' . $message);
                break;
            case LogLevel::DEBUG:
                /** @noinspection PhpUndefinedMethodInspection */
                $log->debug('[DEBUG] ' . $message);
                break;
            default:
                throw new InvalidArgumentException('Invalid log level type: ' . $level);
        }
    }

    /**
     * build a replacement array with braces around the context keys
     * @param $message
     * @param array $context
     * @return string
     */
    private function interpolate($message, array $context = [])
    {
        $replace = [];

        if (empty($context)) {
            return $message;
        }

        foreach ($context as $key => $val) {
            // check that the value can be casted to string
            if (!is_array($val) && (!is_object($val) || method_exists($val, '__toString'))) {
                $replace['{' . $key . '}'] = $val;
            }
        }

        return strtr($message, $replace);
    }
}
