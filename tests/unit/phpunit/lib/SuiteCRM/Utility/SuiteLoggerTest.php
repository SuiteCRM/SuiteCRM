<?php
/**
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2021 SalesAgility Ltd.
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

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;
use SuiteCRM\Utility\Paths;
use SuiteCRM\Utility\SuiteLogger;

class SuiteLoggerTest extends SuitePHPUnitFrameworkTestCase
{
    /**
     * @var SuiteLogger
     */
    private static $logger;

    /**
     * @var string
     */
    private static $oldLogLevel;

    protected function setUp(): void
    {
        global $sugar_config;

        parent::setUp();
        if (self::$logger === null) {
            self::$logger = new SuiteLogger();
        }

        $loggerManager = LoggerManager::getLogger();
        self::$oldLogLevel = $loggerManager::getLogLevel();

        $loggerManager::setLogLevel('debug');
        $sugar_config['show_log_trace'] = false;
    }

    protected function tearDown(): void
    {
        $loggerManager = LoggerManager::getLogger();
        $loggerManager::setLogLevel(self::$oldLogLevel);
        self::$logger = null;
        parent::tearDown();
    }

    public function testLogEmergency(): void
    {
        self::$logger->emergency('test');
        $lastLine = $this->getLastLogMessage();
        preg_match('/\[EMERGENCY\] test/', (string) $lastLine, $matches);
        self::assertNotEmpty($matches);
    }

    public function testLogAlert(): void
    {
        self::$logger->alert('test');
        $lastLine = $this->getLastLogMessage();
        preg_match('/\[ALERT\] test/', (string) $lastLine, $matches);
        self::assertNotEmpty($matches);
    }

    public function testLogCritical(): void
    {
        self::$logger->critical('test');
        $lastLine = $this->getLastLogMessage();
        preg_match('/\[CRITICAL\] test/', (string) $lastLine, $matches);
        self::assertNotEmpty($matches);
    }

    public function testLogError(): void
    {
        self::$logger->error('test');
        $lastLine = $this->getLastLogMessage();
        preg_match('/\[ERROR\] test/', (string) $lastLine, $matches);
        self::assertNotEmpty($matches);
    }

    public function testLogWarning(): void
    {
        self::$logger->warning('test');
        $lastLine = $this->getLastLogMessage();
        preg_match('/\[WARNING\] test/', (string) $lastLine, $matches);
        self::assertNotEmpty($matches);
    }

    public function testLogNotice(): void
    {
        self::$logger->notice('test');
        $lastLine = $this->getLastLogMessage();
        preg_match('/\[NOTICE\] test/', (string) $lastLine, $matches);
        self::assertNotEmpty($matches);
    }

    public function testLogInfo(): void
    {
        self::$logger->info('test');
        $lastLine = $this->getLastLogMessage();
        preg_match('/\[INFO\] test/', (string) $lastLine, $matches);
        self::assertNotEmpty($matches);
    }

    public function testLogDebug(): void
    {
        self::$logger->debug('test');
        $lastLine = $this->getLastLogMessage();
        preg_match('/\[DEBUG\] test/', (string) $lastLine, $matches);
        self::assertNotEmpty($matches);
    }

    public function testInterpolate(): void
    {
        self::$logger->error('test {a}', ['a' => 'apple']);
        $lastLine = $this->getLastLogMessage();
        preg_match('/\[ERROR\] test apple/', (string) $lastLine, $matches);
        self::assertNotEmpty($matches);
    }

    public function testInvalidLevel(): void
    {
        $this->expectException(InvalidArgumentException::class);
        self::$logger->log('invalid', 'test');
    }

    private function getLastLogMessage()
    {
        $paths = new Paths();
        $loggerPath = $paths->getProjectPath() . '/suitecrm.log';
        $data = file($loggerPath);

        if (empty($data)) {
            $line = '';
        } else {
            $line = $data[count($data) - 1];
        }

        return $line;
    }
}
