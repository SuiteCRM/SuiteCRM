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

/**
 * Interface for calendar sync configuration management.
 * Defines the contract for retrieving and persisting configuration settings.
 */
interface CalendarSyncConfigInterface
{

    /**
     * Get all configuration keys.
     *
     * @return array<string> Array of configuration keys
     */
    public function getKeys(): array;

    /**
     * Get all configuration values.
     *
     * @return array<string, mixed> Array of all configuration values
     */
    public function getAll(): array;

    /**
     * Set configuration values.
     *
     * @param array<string, mixed> $values Configuration values to set
     * @return bool True if values were saved successfully, false otherwise
     */
    public function set(array $values): bool;

    /**
     * Get whether synchronization should run asynchronously.
     *
     * @return bool True if async mode is enabled
     */
    public function getRunAsyncValue(): bool;

    /**
     * Get maximum number of calendar accounts to process per sync run.
     *
     * @return int Maximum number of accounts
     */
    public function getMaxAccountsPerSync(): int;

    /**
     * Get maximum number of meeting operations per account per sync run.
     *
     * @return int Maximum number of operations
     */
    public function getMaxOperationsPerAccount(): int;

    /**
     * Get number of days to synchronize into the past.
     *
     * @return int Number of past days
     */
    public function getSyncWindowPastDays(): int;

    /**
     * Get number of days to synchronize into the future.
     *
     * @return int Number of future days
     */
    public function getSyncWindowFutureDays(): int;

    /**
     * Get the conflict resolution strategy.
     *
     * @return string Conflict resolution strategy identifier
     */
    public function getConflictResolution(): string;

    /**
     * Check if internal event deletion is allowed.
     *
     * @return bool True if internal events can be deleted
     */
    public function allowInternalEventDeletion(): bool;

    /**
     * Check if external event deletion is allowed.
     *
     * @return bool True if external events can be deleted
     */
    public function allowExternalEventDeletion(): bool;

    /**
     * Check if calendar sync logic hooks should be executed.
     *
     * @return bool True if logic hooks should be executed
     */
    public function enableCalendarSyncLogicHooks(): bool;

    /**
     * Gets the external calendar name for sync operations.
     *
     * @return string The external calendar name
     */
    public function getExternalCalendarName(): string;

    /**
     * Gets the last manual run time of calendar sync.
     *
     * @return string|null The last manual run timestamp in datetime format, or null if never run
     */
    public function getLastManualRunTime(): ?string;

    /**
     * Sets the last manual run time of calendar sync.
     *
     * @param string $timestamp The timestamp in datetime format
     * @return bool True if saved successfully, false otherwise
     */
    public function setLastManualRunTime(string $timestamp): bool;

}