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

require_once 'include/CalendarSync/infrastructure/providers/AbstractCalendarProvider.php';
require_once 'include/CalendarSync/domain/services/factories/CalendarAccountEventFactory.php';
require_once 'include/CalendarSync/domain/entities/CalendarAccountEvent.php';
require_once 'include/CalendarSync/domain/services/CalendarAccountRelationshipManager.php';
require_once 'include/CalendarSync/domain/valueObjects/CalendarConnectionTestResult.php';

/**
 * SuiteCRM Internal Calendar Provider
 *
 * This class provides an implementation of an internal calendar provider for SuiteCRM,
 * handling operations such as testing the connection to the internal calendar, fetching events,
 * creating new events, updating existing events, deleting events, and retrieving individual events.
 */
class SuiteCRMInternalCalendarProvider extends AbstractCalendarProvider
{

    protected const ENABLE_MEETINGS = true;
    protected const ENABLE_CALLS = false;
    protected const ENABLE_TASKS = false;

    private DBManager $db;

    public function __construct(
        ?CalendarAccountEventFactory $eventFactory = new CalendarAccountEventFactory(),
        ?CalendarAccountRelationshipManager $relationshipManager = new CalendarAccountRelationshipManager()
    ) {
        if (!$this->db = DBManagerFactory::getInstance()) {
            throw new RuntimeException('Failed to get DBManagerFactory instance');
        }
        parent::__construct($eventFactory, $relationshipManager);
    }

    /**
     * @inheritdoc
     */
    public function testCalendarConnection(): CalendarConnectionTestResult
    {
        global $log;

        try {
            /** @var Meeting $meeting */
            $meeting = BeanFactory::newBean('Meetings');
            if (!$meeting) {
                throw new RuntimeException('Failed to create Meetings bean - database may be unavailable');
            }

            $log->debug('SuiteCRMInternalCalendarProvider: Internal calendar connection test successful');

            return new CalendarConnectionTestResult(
                success: true,
                connection: $this->connection,
                authenticationStatus: 'internal'
            );

        } catch (Throwable $e) {
            $log->error("SuiteCRMInternalCalendarProvider: Connection test failed: " . $e->getMessage());
            return new CalendarConnectionTestResult(
                success: false,
                connection: $this->connection,
                errorMessage: $e->getMessage(),
                errorCode: (string)$e->getCode(),
                authenticationStatus: 'failed'
            );
        }
    }

    /**
     * @inheritdoc
     */
    public function getEvents(CalendarEventQuery $query): array
    {
        global $log, $disable_date_format;

        if (!$this->connection || empty($this->connection->calendar_user_id)) {
            throw new RuntimeException('No calendar connection or user ID set');
        }

        $userId = $this->connection->calendar_user_id;
        $log->debug("SuiteCRMInternalCalendarProvider: Getting internal events for user $userId with query: " . json_encode($query->toArray()));

        $events = [];
        $old_disable_date_format = $disable_date_format;
        $disable_date_format = true;
        try {
            require_once 'include/CalendarSync/domain/entities/CalendarAccountEvent.php';

            if (static::ENABLE_MEETINGS) {
                $events = array_merge($events, $this->getMeetings($query, $userId));
            }

            if (static::ENABLE_CALLS) {
                $events = array_merge($events, $this->getCalls($query, $userId));
            }

            if (static::ENABLE_TASKS) {
                $events = array_merge($events, $this->getTasks($query, $userId));
            }

            usort(
                $events,
                static fn($a, $b) => strcmp($a->getDateStartString(), $b->getDateStartString())
            );

            $log->debug("SuiteCRMInternalCalendarProvider: Found " . count($events) . " total internal events for user $userId");
        } catch (Throwable $e) {
            $log->error("SuiteCRMInternalCalendarProvider: Failed to get events for user $userId: " . $e->getMessage());
        }

        $disable_date_format = $old_disable_date_format;
        return $events;
    }

    /**
     * Retrieve a list of meetings for a specified user based on the given query.
     *
     * @param CalendarEventQuery $query Query object specifying filtering and limit criteria
     * @param string $userId ID of the user for whom to retrieve the meetings
     * @return CalendarAccountEvent[] List of meetings as transformed event objects
     * @throws RuntimeException If the Meetings bean creation fails
     */
    protected function getMeetings(CalendarEventQuery $query, string $userId): array
    {
        global $log;

        /** @var Meeting $meeting */
        $meeting = BeanFactory::newBean('Meetings');
        if (!$meeting) {
            throw new RuntimeException('Failed to create Meetings bean');
        }

        $whereConditions = $this->buildBaseWhereConditions($userId, 'meetings');
        $whereConditions = array_merge($whereConditions, $this->buildDateConditions($query, 'meetings', 'date_start'));

        $whereClause = implode(' AND ', $whereConditions);
        $log->debug("SuiteCRMInternalCalendarProvider: Meetings WHERE clause: $whereClause");

        $limit = $query->getLimit() ?? -99;

        $result = $meeting->get_list(
            order_by: 'meetings.date_start ASC',
            where: $whereClause,
            limit: $limit,
            max: $limit,
        );

        $events = [];
        if (!empty($result['list'])) {
            foreach ($result['list'] as $meetingBean) {
                $events[] = $this->eventFactory->fromMeetingBean($meetingBean, $this->connection);
            }
        }

        $log->debug("SuiteCRMInternalCalendarProvider: Found " . count($events) . " meetings for user $userId");
        return $events;
    }

    /**
     * Constructs an array of base WHERE conditions for a database query.
     *
     * @param string $userId The ID of the user to filter the records by.
     * @param string $tableName The name of the database table to query.
     * @return string[] An array of base WHERE conditions used for filtering records.
     */
    protected function buildBaseWhereConditions(string $userId, string $tableName): array
    {
        $quotedUserId = "'" . $this->db->quote($userId) . "'";

        return [
            "$tableName.assigned_user_id = $quotedUserId",
            "$tableName.deleted = 0"
        ];
    }

    /**
     * Builds an array of date-based WHERE conditions for a database query.
     *
     * @param CalendarEventQuery $query The query object containing date filter criteria.
     * @param string $tableName The name of the database table to query.
     * @param string $dateField The name of the date field in the table to apply conditions on.
     * @return string[] An array of date-based WHERE conditions used for filtering records.
     */
    protected function buildDateConditions(CalendarEventQuery $query, string $tableName, string $dateField): array
    {
        $conditions = [];

        if ($query->getStartDate()) {
            $startDate = $query->getStartDate()->format('Y-m-d H:i:s');
            $conditions[] = "$tableName.$dateField >= '" . $this->db->quote($startDate) . "'";
        }

        if ($query->getEndDate()) {
            $endDate = $query->getEndDate()->format('Y-m-d H:i:s');
            $conditions[] = "$tableName.$dateField <= '" . $this->db->quote($endDate) . "'";
        }

        return $conditions;
    }

    /**
     * Retrieves a list of call events for the specified user based on the query conditions.
     *
     * @param CalendarEventQuery $query An object representing the query parameters to filter events by.
     * @param string $userId The ID of the user whose calls are to be retrieved.
     * @return CalendarAccountEvent[] An array of call events matching the given query and user ID.
     * @throws RuntimeException If the creation of the Calls bean fails.
     */
    protected function getCalls(CalendarEventQuery $query, string $userId): array
    {
        global $log;

        $call = BeanFactory::newBean('Calls');
        if (!$call) {
            throw new RuntimeException('Failed to create Calls bean');
        }

        $whereConditions = $this->buildBaseWhereConditions($userId, 'calls');
        $whereConditions = array_merge($whereConditions, $this->buildDateConditions($query, 'calls', 'date_start'));

        $whereClause = implode(' AND ', $whereConditions);
        $log->debug("SuiteCRMInternalCalendarProvider: Calls WHERE clause: $whereClause");

        $result = $call->get_list(
            order_by: 'calls.date_start ASC',
            where: $whereClause,
            limit: $query->getLimit() ?? -1
        );

        $events = [];
        if (!empty($result['list'])) {
            foreach ($result['list'] as $callBean) {
                $events[] = $this->eventFactory->fromCallBean($callBean, $this->connection);
            }
        }

        $log->debug("SuiteCRMInternalCalendarProvider: Found " . count($events) . " calls for user $userId");
        return $events;
    }

    /**
     * Retrieves a list of tasks for a specified user based on the given query criteria.
     *
     * @param CalendarEventQuery $query The query containing search criteria, such as date range and limit.
     * @param string $userId The ID of the user for whom tasks should be retrieved.
     * @return CalendarAccountEvent[] An array of tasks matching the query criteria, structured as events.
     * @throws RuntimeException If the Tasks bean cannot be created.
     */
    protected function getTasks(CalendarEventQuery $query, string $userId): array
    {
        global $log;

        $task = BeanFactory::newBean('Tasks');
        if (!$task) {
            throw new RuntimeException('Failed to create Tasks bean');
        }

        $whereConditions = $this->buildBaseWhereConditions($userId, 'tasks');
        $whereConditions = array_merge($whereConditions, $this->buildDateConditions($query, 'tasks', 'date_due'));

        $whereConditions[] = "tasks.status != 'Completed'";

        $whereClause = implode(' AND ', $whereConditions);
        $log->debug("SuiteCRMInternalCalendarProvider: Tasks WHERE clause: $whereClause");

        $result = $task->get_list(
            order_by: 'tasks.date_due ASC',
            where: $whereClause,
            limit: $query->getLimit() ?? -1
        );

        $events = [];
        if (!empty($result['list'])) {
            foreach ($result['list'] as $taskBean) {
                $events[] = $this->eventFactory->fromTaskBean($taskBean, $this->connection);
            }
        }

        $log->debug("SuiteCRMInternalCalendarProvider: Found " . count($events) . " tasks for user $userId");
        return $events;
    }

    /**
     * @inheritdoc
     */
    protected function doCreateEvent(CalendarAccountEvent $targetEvent): string
    {
        global $log, $timedate;

        if (!$this->connection || empty($this->connection->calendar_user_id)) {
            throw new RuntimeException('No calendar connection or user ID set');
        }

        try {
            /** @var Meeting $meeting */
            $meeting = BeanFactory::newBean('Meetings');
            if (!$meeting) {
                throw new RuntimeException('Failed to create Meetings bean');
            }

            $calendarUserId = $targetEvent->getAssignedUserId() ?: $this->connection->calendar_user_id;

            $user = BeanFactory::getBean('Users', $calendarUserId);
            if (!$user) {
                throw new RuntimeException('Failed to retrieve user with ID ' . $calendarUserId);
            }

            $meeting->name = $targetEvent->getName() ?: 'New Meeting';
            $meeting->description = $targetEvent->getDescription();
            $meeting->location = $targetEvent->getLocation();
            $startDate = $targetEvent->getDateStart();
            $endDate = $targetEvent->getDateEnd();
            $meeting->date_start = $timedate->asDb($startDate);
            if ($endDate) {
                $meeting->date_end = $timedate->asDb($endDate);
                $durationSeconds = $endDate->getTimestamp() - $startDate->getTimestamp();
                $durationMinutes = (int)($durationSeconds / 60);
                $meeting->duration_hours = (int)($durationMinutes / 60);
                $meeting->duration_minutes = $durationMinutes % 60;
            } else {
                $meeting->duration_hours = '';
                $meeting->duration_minutes = '';
            }
            $meeting->assigned_user_id = $calendarUserId;
            $meeting->modified_user_id = $calendarUserId;
            $meeting->created_by = $calendarUserId;

            $meetingId = $meeting->save();

            if (!$meetingId) {
                throw new RuntimeException('Failed to save meeting to database');
            }

            $meeting->load_relationship('users');

            /** @var Link2 $link */
            $link = $meeting->users;
            $link->add([$user]);

            if ($this->connection->id) {
                $this->relationshipManager->upsertEventCalendarAccountRelationship($meeting, $this->connection, $targetEvent);
            }

            $log->info("SuiteCRMInternalCalendarProvider: Created internal meeting $meetingId");
            return $meetingId;

        } catch (Throwable $e) {
            $log->error("SuiteCRMInternalCalendarProvider: Failed to create meeting: " . $e->getMessage());
            throw $e;
        }
    }

    /**
     * @inheritdoc
     */
    protected function doUpdateEvent(CalendarAccountEvent $targetEvent): void
    {
        global $log;

        $targetId = $targetEvent->getId();

        try {
            /** @var Meeting | Call | Task $bean */
            ['bean' => $bean, 'type' => $eventType] = $this->findEventBean($targetId);

            if (!$bean || !$bean->id) {
                throw new RuntimeException("Event not found in Meetings, Calls, or Tasks: $targetId");
            }

            $bean->name = $targetEvent->getName() ?: $bean->name;
            $bean->description = $targetEvent->getDescription();
            $bean->skip_calendar_sync = true;

            if ($this->connection && $this->connection->id) {
                $this->relationshipManager->upsertEventCalendarAccountRelationship($bean, $this->connection, $targetEvent);
            }

            global $timedate;

            switch ($eventType) {
                case CalendarEventType::MEETING:
                    $bean->location = $targetEvent->getLocation();
                    $bean->date_start = $timedate->fromString($targetEvent->getDateStartString())?->asDb();
                    $bean->date_end = $timedate->fromString($targetEvent->getDateEndString())?->asDb();
                    break;
                case CalendarEventType::CALL:
                    $bean->date_start = $timedate->fromString($targetEvent->getDateStartString())?->asDb();
                    $bean->date_end = $timedate->fromString($targetEvent->getDateEndString())?->asDb();
                    break;
                case CalendarEventType::TASK:
                    $bean->date_due = $timedate->fromString($targetEvent->getDateStartString())?->asDb();
                    break;
            }

            $bean->save();

            $log->info("SuiteCRMInternalCalendarProvider: Updated internal $eventType->value $targetId with linking to {$targetEvent->getLinkedEventId()}");

        } catch (Throwable $e) {
            $log->error("SuiteCRMInternalCalendarProvider: Failed to update event $targetId: " . $e->getMessage());
            throw $e;
        }
    }

    /**
     * Finds an event bean based on the provided target ID and enabled event types.
     *
     * @param string $targetId The ID of the target record for which to locate an event bean.
     * @return array An associative array containing the following keys:
     *               - 'bean': The located event bean object, or null if none is found.
     *               - 'type': The type of the event bean, or null if none is found.
     */
    protected function findEventBean(string $targetId): array
    {
        $entityTypes = [];

        if (static::ENABLE_MEETINGS) {
            $entityTypes[] = CalendarEventType::MEETING;
        }

        if (static::ENABLE_CALLS) {
            $entityTypes[] = CalendarEventType::CALL;
        }

        if (static::ENABLE_TASKS) {
            $entityTypes[] = CalendarEventType::TASK;
        }

        foreach ($entityTypes as $entityType) {
            $testBean = BeanFactory::getBean($entityType->getBeanType(), $targetId);
            if (!$testBean || !$testBean->id) {
                continue;
            }
            return ['bean' => $testBean, 'type' => $entityType];
        }

        return ['bean' => null, 'type' => null];
    }

    /**
     * @inheritdoc
     */
    protected function doDeleteEvent(string $targetId): void
    {
        global $log;

        try {
            /** @var Meeting | Call | Task $bean */
            ['bean' => $bean, 'type' => $entityType] = $this->findEventBean($targetId);

            if (!$bean || !$bean->id) {
                throw new RuntimeException("Event not found in Meetings, Calls, or Tasks: $targetId");
            }

            $bean->mark_deleted($targetId);

            $log->info("SuiteCRMInternalCalendarProvider: Deleted internal $entityType->value $targetId");

        } catch (Throwable $e) {
            $log->error("SuiteCRMInternalCalendarProvider: Failed to delete event $targetId: " . $e->getMessage());
            throw $e;
        }
    }

    /**
     * @inheritdoc
     */
    public function getEvent(string $targetId): ?CalendarAccountEvent
    {
        global $log;

        if (empty($targetId)) {
            $log->debug('SuiteCRMInternalCalendarProvider: getEvent called with empty eventId');
            return null;
        }

        try {
            /** @var Meeting | Call | Task $bean */
            ['bean' => $bean, 'type' => $entityType] = $this->findEventBean($targetId);

            if (!$bean || !$bean->id) {
                $log->debug("SuiteCRMInternalCalendarProvider: Event not found in Meetings, Calls, or Tasks: $targetId");
                return null;
            }

            if ($bean->deleted === 1) {
                $log->debug("SuiteCRMInternalCalendarProvider: $entityType->value is deleted: $targetId");
                return null;
            }

            $calendarEvent = $this->getEventFromBean($entityType, $bean);

            $log->debug("SuiteCRMInternalCalendarProvider: Retrieved internal $entityType->value $targetId");
            return $calendarEvent;

        } catch (Throwable $e) {
            $log->error("SuiteCRMInternalCalendarProvider: Failed to retrieve event $targetId: " . $e->getMessage());
            throw $e;
        }
    }

    /**
     * Retrieves a CalendarAccountEvent instance based on the provided event type and bean.
     *
     * @param CalendarEventType|null $eventType The type of calendar event, which could be MEETING, CALL, or TASK.
     *                                          If null, an exception will be thrown.
     * @param Meeting|Call|Task $bean The bean corresponding to the specified event type.
     * @return CalendarAccountEvent A CalendarAccountEvent instance created based on the event type and bean.
     * @throws RuntimeException If the event type is invalid or not provided.
     */
    protected function getEventFromBean(?CalendarEventType $eventType, Meeting|Call|Task $bean): CalendarAccountEvent
    {
        return match ($eventType) {
            CalendarEventType::MEETING => $this->eventFactory->fromMeetingBean($bean, $this->connection),
            CalendarEventType::CALL => $this->eventFactory->fromCallBean($bean, $this->connection),
            CalendarEventType::TASK => $this->eventFactory->fromTaskBean($bean, $this->connection),
            default => throw new RuntimeException("Empty entity type")
        };
    }

}