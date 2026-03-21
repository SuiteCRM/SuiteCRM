<?php
/**
 * SuiteCRM is a customer relationship management program developed by SuiteCRM Ltd.
 * Copyright (C) 2025 SuiteCRM Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUITECRM, SUITECRM DISCLAIMS THE
 * WARRANTY OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License
 * version 3, these Appropriate Legal Notices must retain the display of the
 * "Supercharged by SuiteCRM" logo. If the display of the logos is not reasonably
 * feasible for technical reasons, the Appropriate Legal Notices must display
 * the words "Supercharged by SuiteCRM".
 */

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once 'include/CalendarSync/domain/enums/ConflictResolution.php';
require_once 'include/CalendarSync/domain/CalendarSyncConfigInterface.php';

/**
 * Provides configuration management for Calendar Sync functionality.
 * Handles retrieval and persistence of configuration settings, ensuring
 * default values are applied where necessary.
 */
class CalendarSyncConfig implements CalendarSyncConfigInterface
{

    private const CONFIG_KEY = 'calendar_sync';

    private const DEFAULTS = [
        'run_async' => false,
        'max_accounts_per_sync' => 30,
        'max_operations_per_account' => 100,
        'sync_window_past_days' => 30,
        'sync_window_future_days' => 90,
        'conflict_resolution' => 'timestamp',
        'allow_internal_event_deletion' => true,
        'allow_external_event_deletion' => true,
        'enable_calendar_sync_logic_hooks' => false,
        'external_calendar_name' => 'SuiteCRM'
    ];

    /**
     * @inheritdoc
     */
    public function getKeys(): array
    {
        return array_keys($this->getAll());
    }

    /**
     * @inheritdoc
     */
    public function getAll(): array
    {
        return $this->getMergedConfig();
    }

    /**
     * Merges the default configuration with the loaded raw configuration.
     *
     * @return array The resulting merged configuration array.
     */
    protected function getMergedConfig(): array
    {
        $config = $this->loadRawConfig();
        return array_merge(self::DEFAULTS, $config);
    }

    /**
     * Loads the raw configuration data associated with the specified configuration key.
     *
     * @return array<string, mixed> The raw configuration array retrieved from the system configuration.
     */
    protected function loadRawConfig(): array
    {
        return SugarConfig::getInstance()?->get(self::CONFIG_KEY, []) ?? [];
    }

    /**
     * @inheritdoc
     */
    public function set(array $values): bool
    {
        try {
            require_once 'modules/Configurator/Configurator.php';
            $configurator = new Configurator();

            $stringValues = array_map(
                static fn($value) => (string)$value,
                $values
            );

            $configurator->config[self::CONFIG_KEY] = array_merge($this->loadRawConfig(), $stringValues);
            $configurator->saveConfig();

            SugarConfig::getInstance()?->clearCache();

            return true;
        } catch (Throwable $e) {
            $GLOBALS['log']->error('[CalendarSyncConfig][set] Failed to save: ' . $e->getMessage());
            return false;
        }
    }

    /**
     * @inheritdoc
     */
    public function getRunAsyncValue(): bool
    {
        $value = $this->getConfig('run_async');
        return $this->validateBoolean($value, self::DEFAULTS['run_async']);
    }

    /**
     * Retrieves a configuration value based on the specified key.
     *
     * @param string $key The configuration key to retrieve the value for.
     * @return mixed The value associated with the given key, or null if the key does not exist.
     */
    protected function getConfig(string $key): mixed
    {
        $merged = $this->getMergedConfig();
        return $merged[$key] ?? null;
    }

    /**
     * Validates and converts a value to boolean with fallback to default.
     *
     * @param mixed $value The value to validate
     * @param bool $default The default value if validation fails
     * @return bool The validated boolean value
     */
    protected function validateBoolean(mixed $value, bool $default): bool
    {
        if (is_bool($value)) {
            return $value;
        }

        $cast = filter_var($value, FILTER_VALIDATE_BOOLEAN, FILTER_NULL_ON_FAILURE);
        return $cast ?? $default;
    }

    /**
     * @inheritdoc
     */
    public function getMaxAccountsPerSync(): int
    {
        $value = $this->getConfig('max_accounts_per_sync');
        return $this->validateInt($value, self::DEFAULTS['max_accounts_per_sync']);
    }

    /**
     * Validates and converts a value to integer with fallback to default.
     *
     * @param mixed $value The value to validate
     * @param int $default The default value if validation fails
     * @return int The validated integer value
     */
    protected function validateInt(mixed $value, int $default): int
    {
        if (is_int($value)) {
            return $value;
        }

        $cast = filter_var($value, FILTER_VALIDATE_INT);
        return $cast !== false ? $cast : $default;
    }

    /**
     * @inheritdoc
     */
    public function getMaxOperationsPerAccount(): int
    {
        $value = $this->getConfig('max_operations_per_account');
        return $this->validateInt($value, self::DEFAULTS['max_operations_per_account']);
    }

    /**
     * @inheritdoc
     */
    public function getSyncWindowPastDays(): int
    {
        $value = $this->getConfig('sync_window_past_days');
        return $this->validateInt($value, self::DEFAULTS['sync_window_past_days']);
    }

    /**
     * @inheritdoc
     */
    public function getSyncWindowFutureDays(): int
    {
        $value = $this->getConfig('sync_window_future_days');
        return $this->validateInt($value, self::DEFAULTS['sync_window_future_days']);
    }

    /**
     * @inheritdoc
     */
    public function getConflictResolution(): string
    {
        $value = $this->getConfig('conflict_resolution');
        return ConflictResolution::tryFrom($value) ? $value : self::DEFAULTS['conflict_resolution'];
    }

    /**
     * @inheritdoc
     */
    public function allowInternalEventDeletion(): bool
    {
        $value = $this->getConfig('allow_internal_event_deletion');
        return $this->validateBoolean($value, self::DEFAULTS['allow_internal_event_deletion']);
    }

    /**
     * @inheritdoc
     */
    public function allowExternalEventDeletion(): bool
    {
        $value = $this->getConfig('allow_external_event_deletion');
        return $this->validateBoolean($value, self::DEFAULTS['allow_external_event_deletion']);
    }

    /**
     * @inheritdoc
     */
    public function enableCalendarSyncLogicHooks(): bool
    {
        $value = $this->getConfig('enable_calendar_sync_logic_hooks');
        return $this->validateBoolean($value, self::DEFAULTS['enable_calendar_sync_logic_hooks']);
    }

    /**
     * Gets the external calendar name for sync operations.
     *
     * @return string The external calendar name
     */
    public function getExternalCalendarName(): string
    {
        $value = $this->getConfig('external_calendar_name');
        return $this->validateString($value, self::DEFAULTS['external_calendar_name']);
    }

    /**
     * Validates and converts a value to string with fallback to default.
     *
     * @param mixed $value The value to validate
     * @param string $default The default value if validation fails
     * @return string The validated string value
     */
    protected function validateString(mixed $value, string $default): string
    {
        if (is_string($value) && $value !== '') {
            return $value;
        }

        return $default;
    }

    /**
     * @inheritdoc
     */
    public function getLastManualRunTime(): ?string
    {
        $value = $this->getConfig('last_manual_run_time');
        return $value !== null && $value !== '' ? (string)$value : null;
    }

    /**
     * @inheritdoc
     */
    public function setLastManualRunTime(string $timestamp): bool
    {
        $GLOBALS['log']->info('[CalendarSyncConfig][setLastManualRunTime] Saving last manual run time: ' . $timestamp);
        return $this->set(['last_manual_run_time' => $timestamp]);
    }

}