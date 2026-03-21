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

require_once 'include/CalendarSync/migrations/value-objects/ValidationResult.php';
require_once 'include/CalendarSync/migrations/value-objects/UserMigrationStatus.php';
require_once 'include/CalendarSync/migrations/value-objects/LegacyUserData.php';
require_once 'include/CalendarSync/migrations/value-objects/UserMigrationStatsDetail.php';
require_once 'include/CalendarSync/migrations/enums/MigrationStatsDetailType.php';
require_once 'include/CalendarSync/migrations/Services/ProviderMigrationService.php';
require_once 'include/CalendarSync/migrations/Services/SchedulerMigrationService.php';
require_once 'include/CalendarSync/migrations/Services/UserMigrationService.php';
require_once 'include/CalendarSync/migrations/Services/MeetingMigrationService.php';

/**
 * Legacy Google Sync Migration Service
 *
 * Migrates users from the legacy GoogleSync system (User preferences)
 * to the new CalendarSync system (CalendarAccount + ExternalOAuthConnection).
 *
 * This service handles the migration of OAuth tokens and configurations
 * while preserving user calendar sync settings and maintaining data integrity.
 *
 * Additionally migrates meeting sync data from legacy gsync_id/gsync_lastsync
 * fields to the calendar_account_meetings table.
 */
class LegacyGoogleSyncMigrationService
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
     * @param UserMigrationStatus $migrationStats
     * @param ProviderMigrationService $providerMigrationService
     * @param SchedulerMigrationService $schedulerMigrationService
     * @param UserMigrationService $userMigrationService
     * @param MeetingMigrationService $meetingMigrationService
     */
    public function __construct(
        protected UserMigrationStatus $migrationStats = new UserMigrationStatus(),
        protected ProviderMigrationService $providerMigrationService = new ProviderMigrationService(),
        protected SchedulerMigrationService $schedulerMigrationService = new SchedulerMigrationService(),
        protected UserMigrationService $userMigrationService = new UserMigrationService(),
        protected MeetingMigrationService $meetingMigrationService = new MeetingMigrationService(),
    ) {
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

        $GLOBALS['current_user'] = BeanFactory::newBean('Users');
        $GLOBALS['current_user']->retrieve(1);
    }

    public function migrateLegacySchedulers(): UserMigrationStatus
    {
        $this->migrateSchedulerOrFail(false);
        return $this->migrationStats;
    }

    /**
     * Execute complete migration process
     *
     * @param bool $dryRun If true, only simulate the migration without making changes
     * @return UserMigrationStatus Migration results with statistics
     */
    public function executeMigration(bool $dryRun = false): UserMigrationStatus
    {
        $this->migrationStats->setDryRun($dryRun);

        $externalOauthProviderId = $this->getProviderOrFail($dryRun);
        if ($externalOauthProviderId === null) {
            return $this->migrationStats;
        }

        $legacyUsers = $this->findUsersOrFail();
        if ($legacyUsers === null) {
            return $this->migrationStats;
        }

        $this->migrateUsers($legacyUsers, $externalOauthProviderId, $dryRun);

        if ($this->migrationStats->isMigrated()) {
            $this->writeMigrationMarkerFile();
        }

        return $this->migrationStats;
    }

    protected function getProviderOrFail(bool $dryRun): ?string
    {
        try {
            return $this->providerMigrationService->createOrGetExternalOAuthProvider($dryRun);
        } catch (Throwable $e) {
            $this->migrationStats->addDetail(
                new UserMigrationStatsDetail(
                    type: MigrationStatsDetailType::ERROR,
                    message: 'Provider creation failed: ' . $e->getMessage()
                )
            );
            return null;
        }
    }

    protected function migrateSchedulerOrFail(bool $dryRun): bool
    {
        try {
            $schedulerResult = $this->schedulerMigrationService->migrateScheduler($dryRun);
            if (!$schedulerResult->isSuccess()) {
                $this->migrationStats->addDetail(
                    new UserMigrationStatsDetail(
                        type: MigrationStatsDetailType::ERROR,
                        message: 'Scheduler migration failed: ' . $schedulerResult->message
                    )
                );
                return false;
            }
            return true;
        } catch (Throwable $e) {
            $this->migrationStats->addDetail(
                new UserMigrationStatsDetail(
                    type: MigrationStatsDetailType::ERROR,
                    message: 'Scheduler migration failed: ' . $e->getMessage()
                )
            );
            return false;
        }
    }

    protected function findUsersOrFail(): ?array
    {
        try {
            return $this->userMigrationService->findAllUsersForMigration();
        } catch (Throwable $e) {
            $this->migrationStats->addDetail(
                new UserMigrationStatsDetail(
                    type: MigrationStatsDetailType::ERROR,
                    message: 'Error finding users for migration: ' . $e->getMessage()
                )
            );
            return null;
        }
    }

    protected function migrateUsers(array $legacyUsers, string $externalOauthProviderId, bool $dryRun): void
    {
        foreach ($legacyUsers as $userData) {
            $migrationDetail = $this->userMigrationService->migrateUser($userData, $externalOauthProviderId, $dryRun);
            $eventMigrationStats = $this->meetingMigrationService->migrateMeetingSyncData($userData, $dryRun);
            $migrationDetail->setEventMigrationDetail($eventMigrationStats);
            $this->migrationStats->addDetail($migrationDetail);
        }
    }

    protected function writeMigrationMarkerFile(): void
    {
        $markerFile = 'include/CalendarSync/migrations/.google_sync_migrated_checked';

        $directory = dirname($markerFile);
        if (!is_dir($directory)) {
            mkdir($directory, 0755, true);
            $this->logger->info("Created migration marker directory: $directory");
        }

        file_put_contents($markerFile, "1");

        $this->logger->info("Migration marker file created: $markerFile");
    }

    /**
     * Validate system requirements for migration
     *
     * @return ValidationResult Validation results
     */
    public function validateMigrationRequirements(): ValidationResult
    {
        $issues = [];

        try {
            $providerIssues = $this->providerMigrationService->validateProviderRequirements();
            $issues = array_merge($issues, $providerIssues);

            $schedulerIssues = $this->schedulerMigrationService->validateSchedulerMigrationRequirements();
            $issues = array_merge($issues, $schedulerIssues);

            $userIssues = $this->userMigrationService->validateUserMigrationRequirements();
            $issues = array_merge($issues, $userIssues);

            $meetingIssues = $this->meetingMigrationService->validateMeetingMigrationRequirements();
            $issues = array_merge($issues, $meetingIssues);

        } catch (Throwable $e) {
            $issues[] = ['Exception: ' . $e->getMessage()];
        }

        return new ValidationResult($issues);
    }

}
