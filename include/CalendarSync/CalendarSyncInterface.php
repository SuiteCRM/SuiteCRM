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
 * CalendarSync Interface
 *
 * Contract for calendar synchronization functionality.
 * Defines methods for external integrations with proper structure.
 */
interface CalendarSyncInterface
{

    // =============================================================================
    // PUBLIC - JOB EXECUTION FUNCTIONS
    // =============================================================================

    /**
     * Synchronize all calendar accounts.
     *
     * @param bool $isManualRun Whether this is a manual run (default: false for scheduled)
     * @return bool True if synchronization succeeded, false otherwise
     */
    public function syncAllCalendarAccounts(bool $isManualRun = false): bool;

    /**
     * Schedule all meetings associated with a specific calendar account.
     *
     * @param string $calendarAccountId The identifier of the calendar account whose meetings should be scheduled
     * @return void
     */
    public function syncAllMeetingsOfCalendarAccount(string $calendarAccountId): void;

    /**
     * Synchronize a meeting with external calendar systems.
     *
     * @param Meeting $bean The meeting instance to be synchronized
     * @return void
     */
    public function syncMeeting(Meeting $bean): void;

    /**
     * Execute a calendar sync meeting job from scheduler data.
     * Deserializes the operation data and executes the meeting synchronization.
     *
     * @param string $decodedData
     * @return bool True if synchronization was successful
     */
    public function syncEvent(string $decodedData): bool;

    // =============================================================================
    // PUBLIC - PROVIDER MANAGEMENT & VALIDATION
    // =============================================================================

    /**
     * Get provider authentication method with validation.
     * Throws exceptions for error cases, returns auth method on success.
     *
     * @param string $source Calendar source identifier
     * @return string Authentication method
     * @throws InvalidArgumentException When source is empty
     * @throws RuntimeException When provider not found or not configured
     * @throws Exception For other errors
     */
    public function getProviderAuthMethodWithValidation(string $source): string;

    /**
     * Test calendar provider connection with complete validation and setup.
     * Handles account setup, authentication, and connection testing.
     * Returns the provider's test result object.
     *
     * @param CalendarAccount $calendarAccount Configured calendar account
     * @return CalendarConnectionTestResult Provider test connection result
     * @throws InvalidArgumentException When account source is empty
     * @throws RuntimeException When provider not found or connection test fails
     * @throws Exception For other errors
     */
    public function testProviderConnectionWithValidation(CalendarAccount $calendarAccount): CalendarConnectionTestResult;

    // =============================================================================
    // PUBLIC - UI & FORM UTILITIES
    // =============================================================================

    /**
     * Get fields that should be hidden based on the authentication method for a calendar source.
     *
     * @param string $source Calendar source identifier
     * @return array List of field names to hide
     */
    public function getFieldsToHide(string $source): array;

    /**
     * Get calendar source types array.
     *
     * @return array Array of calendar source types with provider keys and names
     */
    public function getCalendarSourceTypes(): array;

    // =============================================================================
    // PUBLIC - CONFIGURATION MANAGEMENT
    // =============================================================================

    /**
     * Save calendar sync configuration values.
     *
     * @param array $postData POST data containing configuration values
     * @return bool True if configuration was saved successfully
     */
    public function saveConfig(array $postData): bool;

    /**
     * Get all calendar sync configuration values.
     *
     * @return array All configuration values
     */
    public function getConfig(): array;

    /**
     * Get conflict resolution enum cases.
     *
     * @return array Conflict resolution cases
     */
    public function getConflictResolutionCases(): array;

    // =============================================================================
    // PUBLIC - SCHEDULING & JOB MANAGEMENT
    // =============================================================================

    /**
     * Get the calendar sync scheduler instance.
     *
     * @return Scheduler|null Scheduler instance or null if not found
     */
    public function getScheduler(): ?Scheduler;

    // =============================================================================
    // PUBLIC - ACCOUNT MANAGEMENT
    // =============================================================================

    /**
     * Get all active calendar accounts for a user
     *
     * @param string $userId The user ID to get accounts for
     * @return CalendarAccount[] Array of active calendar accounts for the user
     */
    public function getActiveCalendarAccountsForUser(string $userId): array;

    // =============================================================================
    // SINGLETON FACTORY
    // =============================================================================

    /**
     * Get singleton instance
     *
     * @return CalendarSync The singleton instance
     */
    public static function getInstance(): CalendarSync;

}