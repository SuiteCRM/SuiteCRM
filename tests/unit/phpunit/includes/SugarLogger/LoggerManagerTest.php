<?php

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

/**
 * Class LoggerManagerTest
 */
class LoggerManagerTest extends SuitePHPUnitFrameworkTestCase
{
    /**
     * @var LoggerManager
     */
    private static $loggerManager;

    protected function setUp(): void
    {
        parent::setUp();

        if (self::$loggerManager === null) {
            self::$loggerManager = LoggerManager::getLogger();
        }
    }

    protected function tearDown(): void
    {
        self::$loggerManager = null;
        parent::tearDown();
    }

    public function testLoggerLevels(): void
    {
        $loggerManager = self::$loggerManager;
        $loggerManager::setLevelMapping('test', 125);

        $loggerLevels = $loggerManager::getLoggerLevels();
        self::assertArrayHasKey('test', $loggerLevels);
    }

    public function testGetLogLevel(): void
    {
        $logLevel = self::$loggerManager::getLogLevel();
        self::assertEquals('fatal', $logLevel);
    }

    public function testGetAvailableLoggers(): void
    {
        $loggers = self::$loggerManager::getAvailableLoggers();

        self::assertContains('SugarLogger', $loggers);
    }

    public function testSetLoggerLevel(): void
    {
        $loggerManager = self::$loggerManager;
        $loggerManager->setLevel('debug');
        $logLevel = $loggerManager::getLogLevel();

        self::assertEquals('debug', $logLevel);
    }
}
