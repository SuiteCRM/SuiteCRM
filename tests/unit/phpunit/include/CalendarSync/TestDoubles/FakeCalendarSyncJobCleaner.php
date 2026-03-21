<?php
/**
 * In-memory spy for CalendarSyncJobCleaner.
 *
 * Replaces database-backed job cleanup with in-memory tracking.
 * Records all job cancellation attempts without querying the database.
 *
 * Key differences from production:
 * - No database queries to job_queue table
 * - No BeanFactory calls to create SchedulersJobs
 * - All cancelled jobs tracked in-memory for verification
 * - Returns configurable cancellation count
 *
 * Spy methods available:
 * - wasCancelCalled() - Check if any cancellation occurred
 * - getCancelledOperations() - Get all operations that triggered cancellation
 * - getCancellationCount() - Total number of jobs cancelled
 * - setJobsToCancel() - Configure how many jobs to report as cancelled
 *
 * Use this test double when testing code that cancels pending jobs
 * but you want to avoid database access and verify cancellation behavior.
 */

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once 'include/CalendarSync/infrastructure/jobs/CalendarSyncJobCleaner.php';

class FakeCalendarSyncJobCleaner extends CalendarSyncJobCleaner
{
    private array $cancelledOperations = [];
    private int $jobsToCancel = 0;

    public function cancelPendingMeetingJobs(CalendarSyncOperation $operation): int
    {
        $this->cancelledOperations[] = [
            'userId' => $operation->userId,
            'calendarAccountId' => $operation->calendarAccountId,
            'action' => $operation->action->value,
            'location' => $operation->location->value,
            'targetEventId' => $operation->targetEventId,
            'timestamp' => time(),
        ];

        return $this->jobsToCancel;
    }

    public function setJobsToCancel(int $count): void
    {
        $this->jobsToCancel = $count;
    }

    public function wasCancelCalled(): bool
    {
        return count($this->cancelledOperations) > 0;
    }

    public function getCancelledOperations(): array
    {
        return $this->cancelledOperations;
    }

    public function getCancellationCount(): int
    {
        return count($this->cancelledOperations);
    }

    public function getLastCancelledOperation(): ?array
    {
        if (empty($this->cancelledOperations)) {
            return null;
        }

        return end($this->cancelledOperations);
    }

    public function clear(): void
    {
        $this->cancelledOperations = [];
        $this->jobsToCancel = 0;
    }
}
