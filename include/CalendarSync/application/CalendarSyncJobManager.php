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

require_once 'include/CalendarSync/infrastructure/jobs/JobStatusHelper.php';

/**
 * Class responsible for managing calendar synchronization jobs.
 */
class CalendarSyncJobManager
{

    public function __construct(
        private readonly JobStatusHelper $jobStatusHelper = new JobStatusHelper()
    ) {
    }

    /**
     * Check if an account-level sync job is currently active.
     *
     * @param string $calendarAccountId The identifier of the calendar account to check for an active sync job.
     * @return bool True if an active account-level sync job exists, false otherwise.
     */
    public function accountJobIsActive(string $calendarAccountId): bool
    {
        $GLOBALS['log']->debug("[CalendarSyncJobManager][accountJobIsActive] Checking for active account jobs for account $calendarAccountId");
        $jobName = $this->jobStatusHelper->generateAccountJobName($calendarAccountId);

        try {
            /** @var SchedulersJob $job */
            $job = BeanFactory::newBean('SchedulersJobs');

            $activeJobs = $job->get_list(
                '',
                "name = " . $job->db->quoted($jobName) . " AND " .
                "target = " . $job->db->quoted(JobStatusHelper::ACCOUNT_JOB_TARGET) . " AND " .
                $this->jobStatusHelper->getActiveJobStatusCondition(),
                0,
                1
            );

            $isActive = !empty($activeJobs['list']);
            $GLOBALS['log']->debug("[CalendarSyncJobManager][accountJobIsActive] Account $calendarAccountId job status: " . ($isActive ? 'active' : 'inactive'));
            return $isActive;

        } catch (Throwable $e) {
            $GLOBALS['log']->error("[CalendarSyncJobManager][accountJobIsActive] Failed to check active account jobs for account $calendarAccountId: " . $e->getMessage());
            return false;
        }
    }

    /**
     * Retrieves the count of active meeting-level synchronization jobs for the given operation.
     *
     * @param CalendarSyncOperation $operation The operation for which to count active meeting sync jobs.
     * @return int The number of active meeting synchronization jobs.
     */
    public function getActiveMeetingJobCount(CalendarSyncOperation $operation): int
    {
        $GLOBALS['log']->debug("[CalendarSyncJobManager][getActiveMeetingJobCount] Counting active meeting jobs for operation: " . $operation->getSubjectId());
        try {
            /** @var SchedulersJob $job */
            $job = BeanFactory::newBean('SchedulersJobs');

            $activeJobs = $job->get_list(
                '',
                $this->jobStatusHelper->getMeetingJobCondition($operation, $job, $this->jobStatusHelper->generateMeetingJobName($operation)) . " AND " . $this->jobStatusHelper->getActiveJobStatusCondition()
            );

            $count = count($activeJobs['list'] ?? []);
            $GLOBALS['log']->debug("[CalendarSyncJobManager][getActiveMeetingJobCount] Found $count active meeting jobs for operation: " . $operation->getSubjectId());
            return $count;
        } catch (Throwable $e) {
            $GLOBALS['log']->error("[CalendarSyncJobManager][getActiveMeetingJobCount] Failed to check active jobs for operation: " . $e->getMessage());
            return 0;
        }
    }

    /**
     * Determines if the meeting job is currently active for the given calendar sync operation.
     *
     * @param CalendarSyncOperation $operation The calendar sync operation to check.
     * @return bool Returns true if the meeting job is active, false otherwise.
     */
    public function meetingJobIsActive(CalendarSyncOperation $operation): bool
    {
        return $this->getActiveMeetingJobCount($operation) > 0;
    }

}