<?php
/**
 * In-memory fake for CalendarAccountEventFactory.
 *
 * Replaces database-backed event creation with in-memory object construction.
 * Creates CalendarAccountEvent objects without querying relationship tables.
 *
 * Key differences from production:
 * - No database queries to calendar_account_meetings table
 * - No CalendarAccountRelationshipManager database calls
 * - Returns CalendarAccountEvent with minimal required fields
 * - No relationship data lookup (linked_event_id, last_sync always null)
 *
 * Default behavior:
 * - Uses bean properties directly without DB validation
 * - Sets linked_event_id to null (simulates no existing relationship)
 * - Sets last_sync to null
 * - Calculates date_end from duration if not provided
 *
 * Use this test double when testing code that creates CalendarAccountEvent
 * objects from beans but you want to avoid database access to relationship tables.
 */

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once 'include/CalendarSync/domain/services/factories/CalendarAccountEventFactory.php';

class FakeCalendarAccountEventFactory extends CalendarAccountEventFactory
{
    public function __construct()
    {
    }

    public function fromMeetingBean(Meeting $meeting, CalendarAccount $calendarAccount): CalendarAccountEvent
    {
        return $this->fromBean($meeting, $calendarAccount);
    }

    public function fromCallBean(Call $call, CalendarAccount $calendarAccount): CalendarAccountEvent
    {
        return $this->fromBean($call, $calendarAccount);
    }

    public function fromTaskBean(Task $task, CalendarAccount $calendarAccount): CalendarAccountEvent
    {
        return $this->fromBean($task, $calendarAccount);
    }

    public function fromBean(Meeting|Call|Task $bean, CalendarAccount $calendarAccount): CalendarAccountEvent
    {
        $date_end = '';

        if ($bean->object_name === 'Task') {
            $date_start = $bean->date_start ?? $bean->date_due ?? '';
            $date_end = $bean->date_due ?? '';
        } else {
            $date_start = $bean->date_start ?? '';
            if (!empty($bean->date_end)) {
                $date_end = $bean->date_end;
            } else {
                $hours = (int)($bean->duration_hours ?? 0);
                $minutes = (int)($bean->duration_minutes ?? 0);
                if ($hours > 0 || $minutes > 0) {
                    $startTime = strtotime($date_start);
                    if ($startTime !== false) {
                        $endTime = $startTime + ($hours * 3600) + ($minutes * 60);
                        $date_end = date('Y-m-d H:i:s', $endTime);
                    }
                }
            }
        }

        require_once 'include/CalendarSync/domain/enums/CalendarEventType.php';
        $type = match ($bean->object_name) {
            'Call' => CalendarEventType::CALL,
            'Task' => CalendarEventType::TASK,
            default => CalendarEventType::MEETING
        };

        require_once 'include/CalendarSync/domain/entities/CalendarAccountEvent.php';
        return new CalendarAccountEvent(
            id: $bean->id ?? '',
            name: $bean->name ?? '',
            description: $bean->description ?? '',
            location: $bean->location ?? '',
            date_start: $this->createDateTime($date_start) ?? new DateTime(),
            date_end: $this->createDateTime($date_end),
            assigned_user_id: $bean->assigned_user_id ?? '',
            type: $type,
            linked_event_id: null,
            last_sync: null,
            date_modified: $this->createDateTime($bean->date_modified ?? '') ?? new DateTime(),
            is_external: false
        );
    }

    public function fromSourceEvent(string $eventId, CalendarAccountEvent $sourceEvent): CalendarAccountEvent
    {
        require_once 'include/CalendarSync/domain/entities/CalendarAccountEvent.php';
        return new CalendarAccountEvent(
            id: $eventId,
            name: $sourceEvent->getName(),
            description: $sourceEvent->getDescription(),
            location: $sourceEvent->getLocation(),
            date_start: $sourceEvent->getDateStart(),
            date_end: $sourceEvent->getDateEnd(),
            assigned_user_id: $sourceEvent->getAssignedUserId(),
            type: $sourceEvent->getType(),
            linked_event_id: $sourceEvent->getId(),
            last_sync: $sourceEvent->getLastSync(),
            date_modified: $sourceEvent->getDateModified(),
            is_external: !$sourceEvent->isExternal()
        );
    }

    private function createDateTime(?string $dateString): ?DateTime
    {
        if (empty($dateString)) {
            return null;
        }

        try {
            return new DateTime($dateString);
        } catch (Throwable $e) {
            return null;
        }
    }
}
