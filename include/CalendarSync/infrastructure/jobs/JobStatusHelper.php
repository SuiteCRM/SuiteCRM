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
 * Helper class for managing job statuses and constructing job-related information
 * within the context of scheduler jobs, particularly for calendar synchronization operations.
 */
class JobStatusHelper
{

    public const SCHEDULER_JOB = 'function::calendarSyncJob';
    public const ACCOUNT_JOB_TARGET = 'function::calendarSyncJob';
    public const MEETING_JOB_TARGET = 'function::calendarSyncJob';

    /**
     * Retrieves the condition to filter active job statuses in the job queue.
     *
     * @return string The condition string used to identify active jobs in the job queue.
     */
    public function getActiveJobStatusCondition(): string
    {
        $done = SchedulersJob::JOB_STATUS_DONE;
        return "job_queue.status != " . DBManagerFactory::getInstance()?->quoted($done) ?? $done . " AND job_queue.deleted = 0";
    }

    /**
     * Constructs a condition string to identify a meeting synchronization job.
     *
     * @param CalendarSyncOperation $operation The synchronization operation being performed.
     * @param SchedulersJob $job The scheduler job instance containing database context.
     * @param string $jobName The name of the job to match in the condition.
     * @return string The constructed condition string for the meeting synchronization job.
     */
    public function getMeetingJobCondition(CalendarSyncOperation $operation, SchedulersJob $job, string $jobName): string
    {
        return "name = " . $job->db->quoted($jobName) . " AND " .
            "target = " . $job->db->quoted(self::MEETING_JOB_TARGET);
    }

    /**
     * Generates a job name for a calendar account synchronization process.
     *
     * @param string $calendarAccountId The unique identifier of the calendar account.
     * @return string The generated job name for the calendar account.
     */
    public function generateAccountJobName(string $calendarAccountId): string
    {
        return "Calendar Sync - CalendarAccount: $calendarAccountId";
    }

    /**
     * Generates a standardized job name for a calendar sync meeting operation.
     *
     * @param CalendarSyncOperation $operation The operation containing details of the calendar sync, including account, location, and event information.
     * @return string A string representing the formatted job name for the calendar sync meeting operation.
     */
    public function generateMeetingJobName(CalendarSyncOperation $operation): string
    {
        $calendarAccountId = $operation->getCalendarAccountId();
        $location = $operation->getLocation()->value;
        $eventId = $operation->getSubjectId()
            ?: $operation->getPayload()?->getId()
                ?: $operation->getPayload()?->getLinkedEventId()
                    ?: 'unknown';
        return "Calendar Sync - CalendarAccount: $calendarAccountId, Location: $location, Event: $eventId";
    }

}