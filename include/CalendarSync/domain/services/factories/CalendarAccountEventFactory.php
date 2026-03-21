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

require_once 'include/CalendarSync/domain/entities/CalendarAccountEvent.php';
require_once 'include/CalendarSync/domain/enums/CalendarEventType.php';
require_once 'include/CalendarSync/domain/services/CalendarEventSerializer.php';
require_once 'include/CalendarSync/domain/services/CalendarAccountRelationshipManager.php';

/**
 * Factory class for creating instances of CalendarAccountEvent from various data sources.
 */
class CalendarAccountEventFactory
{

    public function __construct(
        private readonly CalendarAccountRelationshipManager $relationshipManager = new CalendarAccountRelationshipManager()
    ) {
    }

    /**
     * Creates a CalendarAccountEvent object from a Meeting bean and a CalendarAccount.
     *
     * @param Meeting $meeting The Meeting object to convert.
     * @param CalendarAccount $calendarAccount The CalendarAccount associated with the event.
     * @return CalendarAccountEvent The resulting CalendarAccountEvent object.
     */
    public function fromMeetingBean(Meeting $meeting, CalendarAccount $calendarAccount): CalendarAccountEvent
    {
        return $this->fromBean($meeting, $calendarAccount);
    }

    /**
     * Converts a bean object into a CalendarAccountEvent.
     *
     * @param Meeting|Call|Task $bean The source bean object containing event data.
     * @param CalendarAccount $calendarAccount The calendar account associated with the event.
     * @return CalendarAccountEvent An instance of CalendarAccountEvent created from the provided bean and calendar account.
     */
    public function fromBean(Meeting|Call|Task $bean, CalendarAccount $calendarAccount): CalendarAccountEvent
    {
        global $timedate;

        $date_end = '';
        if ($bean->object_name === 'Task') {
            $date_start = $bean->date_start ?? $bean->date_due ?? '';
            $date_end = $bean->date_due ?? '';
        } else {
            $date_start = $bean->date_start ?? '';
        }

        $startDateTime = $this->parseDateTimeString($date_start) ?? $timedate->getNow();

        if (!empty($bean->date_end)) {
            $date_end = $bean->date_end;
        } else {
            $hours = (int)($bean->duration_hours ?? 0);
            $minutes = (int)($bean->duration_minutes ?? 0);
            if ($hours > 0 || $minutes > 0) {
                $startTime = $startDateTime->getTimestamp();
                $endTime = $startTime + ($hours * 3600) + ($minutes * 60);
                $endDateTime = $timedate->fromTimestamp($endTime);
                $date_end = $timedate->asDb($endDateTime);
            }
        }

        $endDateTime = $this->parseDateTimeString($date_end) ?? $startDateTime;

        $dateModifiedDateTime = $this->parseDateTimeString($bean->date_modified) ?? new DateTime();

        $type = match ($bean->object_name) {
            'Call' => CalendarEventType::CALL,
            'Task' => CalendarEventType::TASK,
            default => CalendarEventType::MEETING
        };

        $relationshipData = $this->relationshipManager->getRelationshipData($bean, $calendarAccount);

        return new CalendarAccountEvent(
            id: $bean->id ?? '',
            name: html_entity_decode($bean->name ?? ''),
            description: html_entity_decode($bean->description ?? ''),
            location: $bean->location ?? '',
            date_start: $startDateTime,
            date_end: $endDateTime,
            assigned_user_id: $bean->assigned_user_id ?? '',
            type: $type,
            linked_event_id: $relationshipData->linked_event_id,
            last_sync: $relationshipData->last_sync,
            date_modified: $dateModifiedDateTime,
            is_external: false
        );
    }

    /**
     * Creates a CalendarAccountEvent from a Call bean and CalendarAccount.
     *
     * @param Call $call The Call bean to be converted.
     * @param CalendarAccount $calendarAccount The associated calendar account.
     *
     * @return CalendarAccountEvent The created CalendarAccountEvent instance.
     */
    public function fromCallBean(Call $call, CalendarAccount $calendarAccount): CalendarAccountEvent
    {
        return $this->fromBean($call, $calendarAccount);
    }

    /**
     * Converts a Task bean into a CalendarAccountEvent object.
     *
     * @param Task $task The Task bean representing the source data.
     * @param CalendarAccount $calendarAccount The calendar account associated with the event.
     * @return CalendarAccountEvent The resulting CalendarAccountEvent object.
     */
    public function fromTaskBean(Task $task, CalendarAccount $calendarAccount): CalendarAccountEvent
    {
        return $this->fromBean($task, $calendarAccount);
    }

    /**
     * Creates a new CalendarAccountEvent instance using data from the provided source event and a specified event ID.
     *
     * @param string $eventId The unique identifier for the new event.
     * @param CalendarAccountEvent $sourceEvent The source event object to derive data from.
     * @return CalendarAccountEvent A new instance of CalendarAccountEvent populated with data from the source event.
     */
    public function fromSourceEvent(string $eventId, CalendarAccountEvent $sourceEvent): CalendarAccountEvent
    {
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
            is_external: !$sourceEvent->isExternal()
        );
    }

    /**
     * @param string $dateString
     * @return SugarDateTime|null
     */
    private function parseDateTimeString(string $dateString): ?SugarDateTime
    {
        global $timedate, $current_user;

        if (empty($dateString)) {
            return null;
        }

        if ($timedate->check_matching_format($dateString, TimeDate::DB_DATETIME_FORMAT)) {
            return $timedate->fromDb($dateString);
        }

        if ($timedate->check_matching_format($dateString, TimeDate::DB_DATE_FORMAT)) {
            return $timedate->fromDbDate($dateString);
        }

        return $timedate->fromUser($dateString, $current_user);
    }

}