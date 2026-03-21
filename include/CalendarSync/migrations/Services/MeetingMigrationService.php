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

require_once 'include/CalendarSync/migrations/value-objects/EventMigrationStatus.php';
require_once 'include/CalendarSync/migrations/value-objects/LegacyEventData.php';

/**
 * Meeting Migration Service
 *
 * Handles the migration of meeting sync data from legacy gsync_id/gsync_lastsync
 * fields to the calendar_account_meetings table. This service processes existing
 * meeting records that have Google sync data and creates corresponding entries
 * in the new calendar sync system.
 */
class MeetingMigrationService
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
     * Validate meeting migration system requirements
     *
     * @return string[] Array of validation issues
     */
    public function validateMeetingMigrationRequirements(): array
    {
        $issues = [];

        if (!$this->db->tableExists('calendar_account_meetings')) {
            $issues[] = 'calendar_account_meetings table does not exist';
        }

        if (!$this->db->tableExists('meetings')) {
            $issues[] = 'meetings table does not exist';
        }

        if (!$this->db->tableExists('calendar_accounts')) {
            $issues[] = 'calendar_accounts table does not exist';
        }

        return $issues;
    }

    /**
     * Migrate meeting synchronization data to a new format
     *
     * @param LegacyUserData $userData The user data associated with the meetings to be migrated
     * @param bool $dryRun Whether to simulate the migration process without making actual changes
     * @return EventMigrationStatus The status of the migration process, including details for successes, skips, and errors
     */
    public function migrateMeetingSyncData(LegacyUserData $userData, bool $dryRun = false): EventMigrationStatus
    {
        $migrationStatus = new EventMigrationStatus();

        try {
            $meetingsWithSync = $this->findMeetingsWithGoogleSync($userData);
        } catch (Throwable $e) {
            $migrationStatus->addDetail(
                new EventMigrationStatsDetail(
                    type: MigrationStatsDetailType::ERROR,
                    message: 'Error finding meetings with sync data: ' . $e->getMessage()
                )
            );

            return $migrationStatus;
        }

        try {
            $calendarAccountId = $this->findCalendarAccountForUser($userData->user_id);
        } catch (Throwable $e) {
            $migrationStatus->addDetail(
                new EventMigrationStatsDetail(
                    type: MigrationStatsDetailType::ERROR,
                    message: 'Error finding calendar account for user: ' . $e->getMessage()
                )
            );

            return $migrationStatus;
        }

        if (!$calendarAccountId) {
            $migrationStatus->addDetail(
                new EventMigrationStatsDetail(
                    type: MigrationStatsDetailType::SKIP,
                    message: "No calendar account found for user $userData->user_id"
                )
            );
            return $migrationStatus;
        }

        foreach ($meetingsWithSync as $meetingData) {
            if (!$meetingData instanceof LegacyEventData) {
                continue;
            }

            $meetingId = $meetingData->event_id;
            $meetingName = $meetingData->event_name;
            $migrationStatus->incrementProcessed();

            try {
                if ($this->hasExistingCalendarAccountMeeting($calendarAccountId, $meetingId)) {
                    $migrationStatus->addDetail(
                        new EventMigrationStatsDetail(
                            type: MigrationStatsDetailType::SKIP,
                            event_id: $meetingId,
                            event_name: $meetingName,
                            message: 'Record already exists'
                        )
                    );
                    continue;
                }

                $this->createCalendarAccountMeeting($meetingData, $calendarAccountId, $dryRun);
                $this->cleanupLegacyMeetingSync($meetingId, $dryRun);

                $migrationStatus->addDetail(
                    new EventMigrationStatsDetail(
                        type: MigrationStatsDetailType::SUCCESS,
                        event_id: $meetingId,
                        event_name: $meetingName,
                        message: "Migrated using $calendarAccountId"
                    )
                );
            } catch (Throwable $e) {
                $migrationStatus->addDetail(
                    new EventMigrationStatsDetail(
                        type: MigrationStatsDetailType::ERROR,
                        event_id: $meetingId,
                        event_name: $meetingName,
                        message: $e->getMessage()
                    )
                );
            }
        }

        return $migrationStatus;
    }

    /**
     * Find all meetings with legacy Google sync data
     *
     * @param LegacyUserData $userData
     * @return Generator<LegacyEventData> Generator yielding meeting data with gsync_id and gsync_lastsync
     * @throws Exception
     */
    protected function findMeetingsWithGoogleSync(LegacyUserData $userData): Generator
    {
        $query = "
            SELECT id, name, assigned_user_id, gsync_id, gsync_lastsync
            FROM meetings 
            WHERE deleted = '0'
                AND gsync_id IS NOT NULL 
                AND gsync_id != ''
                AND assigned_user_id = '$userData->user_id'
        ";

        $result = $this->db->query($query);

        while ($row = $this->db->fetchByAssoc($result)) {
            if (empty($row['id']) || empty($row['assigned_user_id']) || empty($row['gsync_id'])) {
                continue;
            }

            yield new LegacyEventData(
                event_id: $row['id'],
                event_name: $row['name'] ?? '',
                user_id: $row['assigned_user_id'],
                external_event_id: $row['gsync_id'],
                external_event_last_sync: $row['gsync_lastsync'] ? new DateTime("@{$row['gsync_lastsync']}") : null
            );
        }
    }

    /**
     * Find the calendar account ID for a given user
     *
     * @param string $userId User ID to find calendar account for
     * @return string|null Calendar account ID if found, null otherwise
     */
    protected function findCalendarAccountForUser(string $userId): ?string
    {
        $query = "
            SELECT id 
            FROM calendar_accounts 
            WHERE calendar_user_id = '{$this->db->quote($userId)}'
                AND source = 'google'
                AND deleted = '0'
            LIMIT 1
        ";

        $result = $this->db->query($query);
        $row = $this->db->fetchByAssoc($result);

        return $row['id'] ?? null;
    }

    /**
     * Check if a calendar account meeting record already exists
     *
     * @param string $calendarAccountId Calendar account ID
     * @param string $meetingId Meeting ID
     * @return bool True if record exists, false otherwise
     */
    protected function hasExistingCalendarAccountMeeting(string $calendarAccountId, string $meetingId): bool
    {
        $query = "
            SELECT id 
            FROM calendar_account_meetings 
            WHERE calendar_account_id = '{$this->db->quote($calendarAccountId)}'
                AND meeting_id = '{$this->db->quote($meetingId)}'
            LIMIT 1
        ";

        $result = $this->db->query($query);
        return $this->db->fetchByAssoc($result) !== false;
    }

    /**
     * Create a calendar_account_meetings record from legacy meeting sync data
     *
     * @param LegacyEventData $meetingData Meeting data with gsync_id and gsync_lastsync
     * @param string $calendarAccountId Calendar account ID to link to
     * @param bool $dryRun If true, only simulate the creation
     * @return void
     * @throws RuntimeException If creation fails
     */
    protected function createCalendarAccountMeeting(LegacyEventData $meetingData, string $calendarAccountId, bool $dryRun = false): void
    {
        $lastSyncDateTime = null;
        if ($meetingData->external_event_last_sync !== null) {
            $lastSyncDateTime = $meetingData->external_event_last_sync->format('Y-m-d H:i:s');
        }

        $data = [
            'id' => create_guid(),
            'calendar_account_id' => $calendarAccountId,
            'meeting_id' => $meetingData->event_id,
            'external_event_id' => $meetingData->external_event_id,
            'last_sync' => $lastSyncDateTime,
            'date_modified' => date('Y-m-d H:i:s'),
            'deleted' => '0',
            'calendar_account_source' => 'google'
        ];

        $quoteLastSync = $data['last_sync'] ? $this->db->quote($data['last_sync']) : 'NULL';

        $query = "
            INSERT INTO calendar_account_meetings (         
               id,
               calendar_account_id, 
               meeting_id, 
               external_event_id,
               last_sync,
               date_modified, 
               deleted,
               calendar_account_source
            ) VALUES (
                '{$this->db->quote($data['id'])}',
                '{$this->db->quote($data['calendar_account_id'])}',
                '{$this->db->quote($data['meeting_id'])}',
                '{$this->db->quote($data['external_event_id'])}',
                '$quoteLastSync',
                '{$this->db->quote($data['date_modified'])}',
                '{$this->db->quote($data['deleted'])}',
                '{$this->db->quote($data['calendar_account_source'])}'
            )
        ";

        if ($dryRun) {
            return;
        }
        $result = $this->db->query($query);
        if (!$result) {
            throw new RuntimeException("Failed to create calendar_account_meetings record for meeting {$meetingData->event_id}");
        }
    }

    /**
     * Clean up legacy meeting sync fields after successful migration
     *
     * @param string $meetingId Meeting ID to clean up
     * @param bool $dryRun If true, only simulate the cleanup
     * @return void
     */
    protected function cleanupLegacyMeetingSync(string $meetingId, bool $dryRun = false): void
    {
        if ($dryRun) {
            return;
        }

        $query = "
            UPDATE meetings 
            SET gsync_id = NULL, gsync_lastsync = NULL 
            WHERE id = '{$this->db->quote($meetingId)}'
        ";

        $result = $this->db->query($query);
        if (!$result) {
        }
    }

}