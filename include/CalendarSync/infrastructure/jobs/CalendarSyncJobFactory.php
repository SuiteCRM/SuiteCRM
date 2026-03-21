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

require_once 'include/SugarQueue/SugarJobQueue.php';
require_once 'include/CalendarSync/domain/services/CalendarSyncOperationSerializer.php';
require_once 'include/CalendarSync/infrastructure/jobs/JobStatusHelper.php';

/**
 * A factory class responsible for creating and managing scheduler jobs related to calendar synchronization.
 */
class CalendarSyncJobFactory
{

    protected ?SugarJobQueue $queue = null;

    public function __construct(
        private readonly CalendarSyncOperationSerializer $serializer = new CalendarSyncOperationSerializer(),
        private readonly JobStatusHelper $jobStatusHelper = new JobStatusHelper()
    ) {
    }

    /**
     * Retrieves the scheduler instance that matches the specified conditions.
     *
     * @return Scheduler|null The scheduler instance if found, or null if it does not exist or an error occurs.
     */
    public function getScheduler(): ?Scheduler
    {
        try {
            /** @var Scheduler $scheduler */
            $scheduler = BeanFactory::newBean('Schedulers');

            /** @var Scheduler|null $existing */
            $existing = $scheduler->retrieve_by_string_fields(
                [
                    'job' => JobStatusHelper::SCHEDULER_JOB,
                    'deleted' => '0'
                ]
            );

            return $existing;

        } catch (Throwable $e) {
            $GLOBALS['log']->error("[CalendarSyncJobFactory][getScheduler] Failed to check scheduler status: " . $e->getMessage());
            return null;
        }
    }

    /**
     * Creates and submits a job for the specified calendar account.
     *
     * @param string $calendarAccountId The identifier of the calendar account for which the job is being created.
     * @return string|null Returns the job ID if the job was successfully created and submitted, or null if the creation failed.
     */
    public function createAccountJob(string $calendarAccountId): ?string
    {
        $GLOBALS['log']->debug("[CalendarSyncJobFactory][createAccountJob] Creating account job for calendar account: $calendarAccountId");
        $job = $this->createBaseJob();
        if (!$job) {
            return null;
        }

        /** @var CalendarAccount $calendarAccount */
        $calendarAccount = BeanFactory::getBean('CalendarAccount', $calendarAccountId);
        $userId = $calendarAccount?->calendar_user_id ?? '1';

        $job->name = $this->jobStatusHelper->generateAccountJobName($calendarAccountId);
        $job->target = JobStatusHelper::ACCOUNT_JOB_TARGET;
        $job->data = $calendarAccountId;
        $job->assigned_user_id = $userId;
        $job->scheduler = $this->getScheduler()->id;

        $GLOBALS['log']->debug("[CalendarSyncJobFactory][createAccountJob] Account job prepared for submission: {$job->name}");
        return $this->submitJob($job);
    }

    /**
     * Creates a meeting synchronization job based on the provided operation.
     *
     * @param CalendarSyncOperation $operation The calendar sync operation for which the meeting job is created.
     * @return string|null Returns the ID of the submitted job if the creation is successful, or null if the job could not be created or submitted.
     */
    public function createMeetingJob(CalendarSyncOperation $operation): ?string
    {
        $GLOBALS['log']->debug("[CalendarSyncJobFactory][createMeetingJob] Creating meeting job for operation: " . $operation->getSubjectId());
        $job = $this->createBaseJob();
        if (!$job) {
            return null;
        }

        $job->name = $this->jobStatusHelper->generateMeetingJobName($operation);
        $job->target = JobStatusHelper::MEETING_JOB_TARGET;
        $job->data = $this->serializer->serialize($operation);
        $job->assigned_user_id = $operation->getUserId();
        $job->scheduler = $this->getScheduler()->id;

        $GLOBALS['log']->debug("[CalendarSyncJobFactory][createMeetingJob] Meeting job prepared for submission: {$job->name}");
        return $this->submitJob($job);
    }

    /**
     * Creates and initializes a base scheduler job with default properties.
     *
     * @return SchedulersJob|null Returns the created scheduler job instance if successful, or null on failure.
     */
    protected function createBaseJob(): ?SchedulersJob
    {
        global $timedate;

        try {
            /** @var SchedulersJob|false $job */
            $job = BeanFactory::newBean('SchedulersJobs');
            if (!$job) {
                throw new RuntimeException('Failed to create SchedulersJob bean');
            }

            $job->assigned_user_id = '1';
            $job->execute_time = $timedate->nowDb();
            $job->status = SchedulersJob::JOB_STATUS_QUEUED;
            $job->resolution = SchedulersJob::JOB_PENDING;

            return $job;

        } catch (Throwable $e) {
            $GLOBALS['log']->error("[CalendarSyncJobFactory][createBaseJob] Failed to create base job: " . $e->getMessage());
            return null;
        }
    }

    /**
     * Submits a job to the job queue and returns its ID if successfully created.
     *
     * @param SchedulersJob $job The job instance to be submitted to the queue.
     *
     * @return string|null Returns the unique ID of the submitted job, or null if the submission fails.
     */
    protected function submitJob(SchedulersJob $job): ?string
    {
        try {
            if (!$this->queue) {
                $this->queue = new SugarJobQueue();
            }

            /** @var SugarJob $job */
            $jobId = $this->queue->submitJob($job);

            $GLOBALS['log']->info("[CalendarSyncJobFactory][submitJob] Created job '$job->name' with ID: $jobId");

            return $jobId;

        } catch (Throwable $e) {
            $GLOBALS['log']->error("[CalendarSyncJobFactory][submitJob] Failed to submit job '$job->name': " . $e->getMessage());
            return null;
        }
    }

}