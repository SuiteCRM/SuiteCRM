<?php
/**
 * In-memory spy for CalendarSyncOrchestrator.
 *
 * Records all sync operations and account sync calls for verification in tests.
 * Replaces the production orchestrator which coordinates with external calendar providers.
 *
 * Key differences from production:
 * - No actual sync with external calendars
 * - All operations recorded in-memory for verification
 * - Supports failure simulation via setShouldSucceed()
 * - Provides spy methods to verify sync behavior
 *
 * Spy methods available:
 * - wasSyncEventCalled() - Check if any event sync occurred
 * - wasSyncAccountCalled() - Check if any account sync occurred
 * - getLastOperation() - Get details of last event sync
 * - getLastAccount() - Get details of last account sync
 * - getOperationCount() - Count of event syncs
 * - getAccountSyncCount() - Count of account syncs
 *
 * Use this test double when testing code that triggers calendar sync operations
 * but you want to verify the sync was requested without actually syncing.
 */

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once 'include/CalendarSync/application/CalendarSyncOrchestrator.php';

class FakeCalendarSyncOrchestrator extends CalendarSyncOrchestrator
{
    private array $syncedOperations = [];
    private array $syncedAccounts = [];
    private bool $shouldSucceed = true;

    public function setShouldSucceed(bool $shouldSucceed): void
    {
        $this->shouldSucceed = $shouldSucceed;
    }

    public function syncAllCalendarAccounts(bool $async = false): void
    {
        if (!$this->shouldSucceed) {
            throw new RuntimeException('Orchestrator configured to fail');
        }

        $this->syncedAccounts[] = [
            'type' => 'all',
            'runAsync' => $async,
            'timestamp' => time(),
        ];
    }

    public function syncCalendarAccount(CalendarAccount $calendarAccount, ?bool $async = false): bool
    {
        if (!$this->shouldSucceed) {
            throw new RuntimeException('Orchestrator configured to fail');
        }

        if (empty($calendarAccount->id)) {
            throw new InvalidArgumentException('Calendar account must have an ID');
        }

        $this->syncedAccounts[] = [
            'type' => 'single',
            'accountId' => $calendarAccount->id,
            'userId' => $calendarAccount->calendar_user_id,
            'runAsync' => $async,
            'timestamp' => time(),
        ];

        return true;
    }

    public function syncEvent(CalendarSyncOperation $operation, ?bool $async = false): bool
    {
        if (!$this->shouldSucceed) {
            return false;
        }

        if (empty($operation->getUserId())) {
            throw new InvalidArgumentException('Operation must have a user ID');
        }

        $this->syncedOperations[] = [
            'userId' => $operation->getUserId(),
            'action' => $operation->getAction()->value,
            'location' => $operation->getLocation()->value,
            'runAsync' => $async,
            'timestamp' => time(),
        ];

        return true;
    }

    public function getSyncedOperations(): array
    {
        return $this->syncedOperations;
    }

    public function getSyncedAccounts(): array
    {
        return $this->syncedAccounts;
    }

    public function wasSyncCalled(): bool
    {
        return count($this->syncedOperations) > 0 || count($this->syncedAccounts) > 0;
    }

    public function wasSyncEventCalled(): bool
    {
        return count($this->syncedOperations) > 0;
    }

    public function wasSyncAccountCalled(): bool
    {
        return count($this->syncedAccounts) > 0;
    }

    public function getLastOperation(): ?array
    {
        if (empty($this->syncedOperations)) {
            return null;
        }

        return end($this->syncedOperations);
    }

    public function getLastAccount(): ?array
    {
        if (empty($this->syncedAccounts)) {
            return null;
        }

        return end($this->syncedAccounts);
    }

    public function getOperationCount(): int
    {
        return count($this->syncedOperations);
    }

    public function getAccountSyncCount(): int
    {
        return count($this->syncedAccounts);
    }

    public function clear(): void
    {
        $this->syncedOperations = [];
        $this->syncedAccounts = [];
        $this->shouldSucceed = true;
    }
}
