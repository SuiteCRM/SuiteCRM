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

require_once 'include/utils.php';
require_once 'include/CalendarSync/migrations/value-objects/SchedulerMigrationResult.php';
require_once 'include/CalendarSync/infrastructure/jobs/JobStatusHelper.php';

/**
 * Scheduler Migration Service
 *
 * Handles the migration of schedulers from the legacy GoogleSync system to the new
 * CalendarSync system. This includes updating scheduler jobs from the old
 * 'syncGoogleCalendar' function to the new unified 'calendarSyncJob' function.
 */
class SchedulerMigrationService
{

    /**
     * @var DBManager Database manager instance
     */
    protected DBManager $db;

    /**
     * @var LoggerManager Logger instance
     */
    protected LoggerManager $logger;

    /**
     * Constructor
     *
     * @throws RuntimeException If required dependencies cannot be initialized
     */
    public function __construct()
    {
        $dbManager = DBManagerFactory::getInstance();
        if (!($dbManager instanceof DBManager)) {
            throw new RuntimeException('Failed to initialize database manager');
        }
        $this->db = $dbManager;

        $logger = LoggerManager::getLogger();
        if (!$logger) {
            throw new RuntimeException('Failed to initialize logger');
        }
        $this->logger = $logger;
    }

    /**
     * Validate scheduler migration system requirements
     *
     * @return string[] Array of validation issues
     */
    public function validateSchedulerMigrationRequirements(): array
    {
        $issues = [];

        if (!$this->db->tableExists('schedulers')) {
            $issues[] = 'Schedulers table does not exist';
        }

        if (!class_exists('Scheduler')) {
            $issues[] = 'Scheduler bean class not available';
        }

        return $issues;
    }

    /**
     * Find existing legacy Google Calendar sync schedulers
     *
     * @return array Array of scheduler records that need migration
     */
    public function findLegacyGoogleSyncSchedulers(): array
    {
        $query = "
            SELECT *
            FROM schedulers
            WHERE deleted = '0'
            AND job = 'function::syncGoogleCalendar'
            AND status = 'Active'
        ";

        $result = $this->db->query($query);
        $schedulers = [];

        while ($row = $this->db->fetchByAssoc($result)) {
            $schedulers[] = $row;
        }

        return $schedulers;
    }

    /**
     * Check if new calendar sync scheduler already exists
     *
     * @return bool True if new scheduler exists
     */
    public function hasNewCalendarSyncScheduler(): bool
    {
        $schedulerJob = JobStatusHelper::SCHEDULER_JOB;

        $query = "
            SELECT id
            FROM schedulers
            WHERE deleted = '0'
            AND job = '$schedulerJob'
            AND status = 'Active'
            LIMIT 1
        ";

        $result = $this->db->query($query);
        return $this->db->fetchByAssoc($result) !== false;
    }

    /**
     * Migrate legacy Google Calendar sync scheduler to new Calendar sync scheduler
     *
     * @param bool $dryRun If true, only simulate the migration
     * @return SchedulerMigrationResult Migration result with success status and details
     */
    public function migrateScheduler(bool $dryRun = false): SchedulerMigrationResult
    {
        try {
            $legacySchedulers = $this->findLegacyGoogleSyncSchedulers();
            $schedulersFound = count($legacySchedulers);

            if (empty($legacySchedulers)) {
                return SchedulerMigrationResult::skip('No legacy Google Calendar sync schedulers found');
            }

            if ($this->hasNewCalendarSyncScheduler()) {
                $this->logger->info('New Calendar sync scheduler already exists, will update legacy schedulers');
            }

            if ($dryRun) {
                return SchedulerMigrationResult::success($schedulersFound, 0, true);
            }

            $mod_strings = return_module_language($GLOBALS['current_language'], 'Schedulers');

            $schedulerName = $mod_strings['LBL_OOTB_CAL_ACC_SYNC'] ?? 'Calendar Accounts Sync';
            $schedulerJob = JobStatusHelper::SCHEDULER_JOB;

            $updateCount = 0;
            foreach ($legacySchedulers as $scheduler) {
                $updateQuery = "
                    UPDATE schedulers
                    SET name = '$schedulerName',
                        job = '$schedulerJob',
                        date_modified = '" . date('Y-m-d H:i:s') . "'
                    WHERE id = '" . $this->db->quote($scheduler['id']) . "'
                ";

                if ($this->db->query($updateQuery)) {
                    $updateCount++;
                    $this->logger->info("Updated scheduler ID: {$scheduler['id']} from 'syncGoogleCalendar' to 'calendarSyncJob'");
                }
            }

            return SchedulerMigrationResult::success($schedulersFound, $updateCount, false);

        } catch (Exception $e) {
            $this->logger->error("Scheduler migration failed: " . $e->getMessage());
            return SchedulerMigrationResult::error('Scheduler migration failed: ' . $e->getMessage(), $e);
        }
    }

    /**
     * Get scheduler migration status summary
     *
     * @return array Status summary with counts and details
     */
    public function getSchedulerMigrationStatus(): array
    {
        $legacySchedulers = $this->findLegacyGoogleSyncSchedulers();
        $hasNewScheduler = $this->hasNewCalendarSyncScheduler();

        return [
            'legacy_schedulers_count' => count($legacySchedulers),
            'has_new_scheduler' => $hasNewScheduler,
            'migration_needed' => count($legacySchedulers) > 0,
            'legacy_schedulers' => $legacySchedulers
        ];
    }

}