<?php

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once 'include/CalendarSync/CalendarSync.php';
require_once 'tests/unit/phpunit/include/CalendarSync/TestDoubles/InMemoryCalendarAccountRepository.php';
require_once 'tests/unit/phpunit/include/CalendarSync/TestDoubles/InMemoryCalendarAccountValidator.php';
require_once 'tests/unit/phpunit/include/CalendarSync/TestDoubles/FakeCalendarSyncOrchestrator.php';
require_once 'tests/unit/phpunit/include/CalendarSync/TestDoubles/InMemoryCalendarSyncConfig.php';
require_once 'tests/unit/phpunit/include/CalendarSync/TestDoubles/FakeCalendarProviderRegistry.php';
require_once 'tests/unit/phpunit/include/CalendarSync/TestDoubles/FakeCalendarSyncJobFactory.php';
require_once 'tests/unit/phpunit/include/CalendarSync/TestDoubles/FakeCalendarSyncJobCleaner.php';
require_once 'tests/unit/phpunit/include/CalendarSync/TestDoubles/FakeCalendarAccountEventFactory.php';

/**
 * Testable version of CalendarSync with dependency injection.
 *
 * Extends CalendarSync to allow injection of test doubles for all dependencies.
 * Used exclusively in unit tests to replace infrastructure dependencies
 * (database, jobs, providers) with in-memory test doubles.
 *
 * Key features:
 * - Static factory method for test instance creation
 * - Defaults to fake implementations for all dependencies
 * - Provides accessor methods for test verification
 * - Manages singleton state reset between tests
 *
 * @see CalendarSyncTest
 */
class TestableCalendarSync extends CalendarSync
{
    private static ?TestableCalendarSync $testInstance = null;

    public static function createTestInstance(
        ?CalendarAccountEventFactory $accountEventFactory = null,
        ?CalendarAccountRepository $calendarAccountRepository = null,
        ?CalendarAccountValidator $calendarAccountValidator = null,
        ?CalendarProviderRegistry $providerRegistry = null,
        ?CalendarSyncConfig $config = null,
        ?CalendarSyncJobCleaner $jobCleaner = null,
        ?CalendarSyncJobFactory $jobFactory = null,
        ?CalendarSyncOperationDiscovery $operationDiscovery = null,
        ?CalendarSyncOperationSerializer $operationSerializer = null,
        ?CalendarSyncOrchestrator $orchestrator = null
    ): TestableCalendarSync {
        self::$testInstance = new self(
            $accountEventFactory ?? new FakeCalendarAccountEventFactory(),
            $calendarAccountRepository ?? new InMemoryCalendarAccountRepository(),
            $calendarAccountValidator ?? new InMemoryCalendarAccountValidator(),
            $providerRegistry ?? new FakeCalendarProviderRegistry(),
            $config ?? new InMemoryCalendarSyncConfig(),
            $jobCleaner ?? new FakeCalendarSyncJobCleaner(),
            $jobFactory ?? new FakeCalendarSyncJobFactory(),
            $operationDiscovery ?? new CalendarSyncOperationDiscovery(),
            $operationSerializer ?? new CalendarSyncOperationSerializer(),
            $orchestrator ?? new FakeCalendarSyncOrchestrator()
        );

        return self::$testInstance;
    }

    public static function resetTestInstance(): void
    {
        self::$testInstance = null;

        $reflection = new ReflectionClass(CalendarSync::class);
        $instance = $reflection->getProperty('instance');
        $instance->setAccessible(true);
        $instance->setValue(null, null);
    }

    public function getRepository(): CalendarAccountRepository
    {
        return $this->calendarAccountRepository;
    }

    public function getValidator(): CalendarAccountValidator
    {
        return $this->calendarAccountValidator;
    }

    public function getOrchestrator(): CalendarSyncOrchestrator
    {
        return $this->orchestrator;
    }

    public function getConfig(): array
    {
        return $this->config->getAll();
    }

    public function getProviderRegistry(): CalendarProviderRegistry
    {
        return $this->providerRegistry;
    }
}
