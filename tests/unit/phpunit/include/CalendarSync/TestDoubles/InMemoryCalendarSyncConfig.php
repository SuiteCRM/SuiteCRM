<?php
/**
 * In-memory fake for CalendarSyncConfig.
 *
 * Replaces database-backed configuration with in-memory storage.
 * Provides same configuration interface as production without database persistence.
 *
 * Key differences from production:
 * - Configuration stored in-memory instead of database
 * - Changes persist only during test execution
 * - Default values pre-populated for common test scenarios
 * - No database queries or writes
 *
 * Default configuration:
 * - calendar_sync_enabled: 1
 * - calendar_sync_run_async: 1
 * - calendar_sync_past_days: 30
 * - calendar_sync_future_days: 365
 * - calendar_sync_conflict_resolution: EXTERNAL_WINS
 * - calendar_sync_enable_logic_hooks: 1
 *
 * Use this test double when testing code that reads or writes calendar sync
 * configuration but you want to avoid database access.
 */

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once 'include/CalendarSync/domain/CalendarSyncConfig.php';

class InMemoryCalendarSyncConfig extends CalendarSyncConfig
{
    private array $configData = [
        'calendar_sync_enabled' => '1',
        'calendar_sync_run_async' => '1',
        'calendar_sync_past_days' => '30',
        'calendar_sync_future_days' => '365',
        'calendar_sync_conflict_resolution' => 'EXTERNAL_WINS',
        'calendar_sync_enable_logic_hooks' => '1',
    ];

    public function get(string $key): ?string
    {
        if (!$this->isValidKey($key)) {
            return null;
        }

        return $this->configData[$key] ?? null;
    }

    public function set(array $configValues): bool
    {
        foreach ($configValues as $key => $value) {
            if (!$this->isValidKey($key)) {
                continue;
            }

            $this->configData[$key] = $value;
        }

        return true;
    }

    public function getAll(): array
    {
        return $this->configData;
    }

    public function getKeys(): array
    {
        return array_keys($this->configData);
    }

    public function getRunAsyncValue(): bool
    {
        return $this->configData['calendar_sync_run_async'] === '1';
    }

    public function enableCalendarSyncLogicHooks(): bool
    {
        return $this->configData['calendar_sync_enable_logic_hooks'] === '1';
    }

    private function isValidKey(string $key): bool
    {
        return array_key_exists($key, $this->configData);
    }

    public function setConfigValue(string $key, string $value): void
    {
        if (!$this->isValidKey($key)) {
            throw new InvalidArgumentException("Invalid config key: {$key}");
        }

        $this->configData[$key] = $value;
    }

    public function clear(): void
    {
        $this->configData = [
            'calendar_sync_enabled' => '1',
            'calendar_sync_run_async' => '1',
            'calendar_sync_past_days' => '30',
            'calendar_sync_future_days' => '365',
            'calendar_sync_conflict_resolution' => 'EXTERNAL_WINS',
            'calendar_sync_enable_logic_hooks' => '1',
        ];
    }
}
