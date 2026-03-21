<?php
/**
 * In-memory test double for CalendarSyncJobFactory.
 *
 * Replaces database-backed job factory with in-memory state to eliminate
 * database queries during unit testing.
 *
 * Key differences from production:
 * - getScheduler() returns null instead of querying database
 * - createAccountJob() returns fake job ID without database interaction
 * - createMeetingJob() returns fake job ID without database interaction
 * - All job creation is tracked in-memory for verification
 *
 * Use this test double when testing code that creates calendar sync jobs
 * but you want to avoid database access and job queue side effects.
 */

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once 'include/CalendarSync/infrastructure/jobs/CalendarSyncJobFactory.php';

class FakeCalendarSyncJobFactory extends CalendarSyncJobFactory
{
    private array $createdAccountJobs = [];
    private array $createdMeetingJobs = [];
    private ?Scheduler $fakeScheduler = null;

    public function getScheduler(): ?Scheduler
    {
        return $this->fakeScheduler;
    }

    public function setFakeScheduler(?Scheduler $scheduler): void
    {
        $this->fakeScheduler = $scheduler;
    }

    public function createAccountJob(string $calendarAccountId): ?string
    {
        if (empty($calendarAccountId)) {
            return null;
        }

        $jobId = 'fake_account_job_' . uniqid();

        $this->createdAccountJobs[] = [
            'jobId' => $jobId,
            'calendarAccountId' => $calendarAccountId,
            'timestamp' => time(),
        ];

        return $jobId;
    }

    public function createMeetingJob(CalendarSyncOperation $operation): ?string
    {
        if (empty($operation->userId)) {
            return null;
        }

        $jobId = 'fake_meeting_job_' . uniqid();

        $this->createdMeetingJobs[] = [
            'jobId' => $jobId,
            'userId' => $operation->userId,
            'action' => $operation->action->value,
            'timestamp' => time(),
        ];

        return $jobId;
    }

    public function getCreatedAccountJobs(): array
    {
        return $this->createdAccountJobs;
    }

    public function getCreatedMeetingJobs(): array
    {
        return $this->createdMeetingJobs;
    }

    public function wasAccountJobCreated(): bool
    {
        return count($this->createdAccountJobs) > 0;
    }

    public function wasMeetingJobCreated(): bool
    {
        return count($this->createdMeetingJobs) > 0;
    }

    public function clear(): void
    {
        $this->createdAccountJobs = [];
        $this->createdMeetingJobs = [];
        $this->fakeScheduler = null;
    }
}