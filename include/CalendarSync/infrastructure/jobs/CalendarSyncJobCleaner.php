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

require_once 'include/CalendarSync/domain/entities/CalendarSyncOperation.php';
require_once 'include/CalendarSync/infrastructure/jobs/JobStatusHelper.php';

/**
 * Class responsible for cleaning up pending calendar synchronization jobs.
 *
 * The CalendarSyncJobCleaner class provides functionalities to identify and
 * resolve pending jobs that are no longer valid due to newer synchronization
 * operations or certain conditions in the job queue.
 */
class CalendarSyncJobCleaner
{

    /**
     * Cancels pending jobs related to a specific meeting synchronization operation.
     *
     * This method identifies pending jobs for a given calendar synchronization operation
     * and marks them as failed if they are overwritten by a newer operation.
     *
     * @param CalendarSyncOperation $operation The calendar synchronization operation
     *                                          for which pending jobs should be cancelled.
     * @return int The number of jobs that were successfully cancelled.
     */
    public function cancelPendingMeetingJobs(CalendarSyncOperation $operation): int
    {
        $cancelledCount = 0;
        try {
            /** @var SchedulersJob $job */
            $job = BeanFactory::newBean('SchedulersJobs');
            if (!$job) {
                throw new RuntimeException('Failed to create SchedulersJobs bean');
            }

            /** @var ?SchedulersJob[] $pendingJobs */
            $pendingJobs = $job->get_full_list(
                '',
                $this->getMeetingJobCondition($operation, $job) . " AND " . $this->getPendingJobStatusCondition(),
            );

            if (!empty($pendingJobs)) {
                foreach ($pendingJobs as $pendingJob) {
                    $pendingJob->resolveJob(SchedulersJob::JOB_FAILURE, 'Job overwritten by newer meeting sync operation');
                    $cancelledCount++;
                    $GLOBALS['log']->debug("[CalendarSyncJobCleaner][cancelPendingMeetingJobs] Resolved pending job ID: $pendingJob->id as overwritten by newer meeting operation");
                }
            }

            if ($cancelledCount > 0) {
                $GLOBALS['log']->info("[CalendarSyncJobCleaner][cancelPendingMeetingJobs] Successfully cancelled $cancelledCount pending jobs for meeting operation");
            }

            return $cancelledCount;

        } catch (Throwable $e) {
            $GLOBALS['log']->error("[CalendarSyncJobCleaner][cancelPendingMeetingJobs] Failed to cancel pending jobs for meeting operation: " . $e->getMessage());
            return 0;
        }
    }

    /**
     * Constructs a SQL condition string for meeting job validation based on the given operation and job.
     *
     * @param CalendarSyncOperation $operation The synchronization operation associated with the meeting job.
     * @param SchedulersJob $job The schedulers job instance used for database interaction.
     * @return string The constructed SQL condition string for meeting job validation.
     */
    protected function getMeetingJobCondition(CalendarSyncOperation $operation, SchedulersJob $job): string
    {
        $jobName = $this->generateMeetingJobName($operation);

        return "name = " . $job->db->quoted($jobName) . " AND " .
            "target = " . $job->db->quoted(JobStatusHelper::MEETING_JOB_TARGET);
    }

    /**
     * Generates a job name string for a meeting synchronization operation.
     *
     * @param CalendarSyncOperation $operation The calendar synchronization operation containing the relevant data.
     * @return string The generated job name including calendar account ID, location, and event details.
     */
    protected function generateMeetingJobName(CalendarSyncOperation $operation): string
    {
        $calendarAccountId = $operation->getCalendarAccountId();
        $location = $operation->getLocation()->value;
        $eventId = $operation->getSubjectId()
            ?: $operation->getPayload()?->getId()
                ?: $operation->getPayload()?->getLinkedEventId()
                    ?: 'unknown';
        return "Calendar Sync - CalendarAccount: $calendarAccountId, Location: $location, Event: $eventId";
    }

    /**
     * Builds and returns a condition string used to filter pending jobs in the job queue.
     *
     * @return string The condition string to filter pending jobs.
     */
    protected function getPendingJobStatusCondition(): string
    {
        $queued = SchedulersJob::JOB_STATUS_QUEUED;
        return "job_queue.status = " . DBManagerFactory::getInstance()?->quoted($queued) ?? $queued . " AND job_queue.deleted = 0";
    }

}