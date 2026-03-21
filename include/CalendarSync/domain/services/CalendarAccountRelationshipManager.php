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

require_once 'include/CalendarSync/domain/enums/CalendarEventType.php';
require_once 'include/CalendarSync/domain/helpers/DateTimeHelper.php';

/**
 * This class manages the relationship between events (Meeting, Call, Task)
 * and calendar accounts. It provides methods to create, update, retrieve,
 * and enforce constraints on these relationships.
 */
class CalendarAccountRelationshipManager
{

    protected DBManager $db;

    public function __construct(
        private readonly DateTimeHelper $dateTimeHelper = new DateTimeHelper()
    ) {
        if (!$this->db = DBManagerFactory::getInstance()) {
            throw new RuntimeException('Failed to get DBManagerFactory instance');
        }
    }

    /**
     * Upsert (create or update) relationship between an event and a calendar account
     */
    public function upsertEventCalendarAccountRelationship(Meeting|Call|Task $eventBean, CalendarAccount $calendarAccount, CalendarAccountEvent $targetEvent): void
    {
        global $log, $timedate;

        $eventType = match ($eventBean->object_name) {
            'Call' => CalendarEventType::CALL,
            'Task' => CalendarEventType::TASK,
            default => CalendarEventType::MEETING
        };
        $relationshipName = 'calendar_account_meetings';

        if (!$calendarAccount->id || !$eventBean->id) {
            return;
        }

        try {
            // $this->ensureUniqueCalendarAccount($eventBean, $eventType, $calendarAccount);

            if (!$calendarAccount->load_relationship($relationshipName)) {
                throw new RuntimeException("Failed to load $relationshipName relationship for calendar account $calendarAccount->id");
            }

            $externalEventId = $targetEvent->getLinkedEventId();
            $lastSync = $targetEvent->getLastSync();

            if (!$externalEventId) {
                throw new RuntimeException("External event ID is missing for event $eventBean->id");
            }

            $sourceKey = 'calendar_account_source';
            $externalEventIdKey = 'external_event_id';
            $lastSyncKey = 'last_sync';
            $additionalValues = [
                $sourceKey => $calendarAccount->source,
                $externalEventIdKey => $externalEventId,
                $lastSyncKey => $timedate->asDb($lastSync),
            ];

            $tableName = 'calendar_account_meetings';
            $escapedCalendarAccountId = $this->db->quoted($calendarAccount->id);
            $escapedMeetingId = $this->db->quoted($eventBean->id);

            $sql = "SELECT id, external_event_id
                    FROM $tableName
                    WHERE calendar_account_id = $escapedCalendarAccountId
                      AND meeting_id = $escapedMeetingId
                    LIMIT 1";

            $result = $this->db->query($sql);
            $existingRelationship = $this->db->fetchByAssoc($result);

            /** @var Link2 $link */
            $link = $calendarAccount->$relationshipName;

            if ($existingRelationship) {
                $this->updateRelationshipFields($calendarAccount->id, $eventBean->id, $additionalValues);
                $log->info("[CalendarAccountRelationshipManager][upsertEventCalendarAccountRelationship] Updated relationship between calendar account $calendarAccount->id and $eventType->value $eventBean->id with external_event_id: $externalEventId");
            } else {
                $result = $link->add([$eventBean], $additionalValues);
                $action = 'Created';

                if ($result === true) {
                    $log->info("[CalendarAccountRelationshipManager][upsertEventCalendarAccountRelationship] $action relationship between calendar account $calendarAccount->id and $eventType->value $eventBean->id with external_event_id: $externalEventId");
                } else {
                    $log->warn("[CalendarAccountRelationshipManager][upsertEventCalendarAccountRelationship] Failed to $action relationship between calendar account $calendarAccount->id and $eventType->value $eventBean->id");
                }
            }
        } catch (Throwable $e) {
            $log->error("[CalendarAccountRelationshipManager][upsertEventCalendarAccountRelationship] Failed to create calendar account relationship: " . $e->getMessage());
            throw new RuntimeException("Failed to create calendar account relationship: " . $e->getMessage());
        }
    }

    /**
     * Update relationship fields in the middle table
     */
    protected function updateRelationshipFields(string $calendarAccountId, string $meetingId, array $fields): void
    {
        global $log;

        $tableName = 'calendar_account_meetings';
        $setClauses = [];

        foreach ($fields as $fieldName => $fieldValue) {
            $escapedValue = $this->db->quoted($fieldValue);
            $setClauses[] = "$fieldName = $escapedValue";
        }

        $setClauses[] = "date_modified = NOW()";
        $setClauses[] = "deleted = 0";
        $setClause = implode(', ', $setClauses);

        $escapedCalendarAccountId = $this->db->quoted($calendarAccountId);
        $escapedMeetingId = $this->db->quoted($meetingId);

        $sql = "UPDATE $tableName
            SET $setClause
            WHERE calendar_account_id = $escapedCalendarAccountId
                AND meeting_id = $escapedMeetingId;";

        $log->info("[CalendarAccountRelationshipManager][updateRelationshipFields] Executing: $sql");
        $this->db->query($sql, true);
    }

    /**
     * Get relationship data (external_event_id, last_sync, and source) for an event
     */
    public function getRelationshipData(Meeting|Call|Task $eventBean, CalendarAccount $calendarAccount): stdClass
    {
        global $log;

        $relationshipName = 'calendar_account_meetings';
        $calendarAccountIdKey = 'calendar_account_id';
        $externalEventIdKey = 'external_event_id';
        $lastSyncKey = 'last_sync';
        $sourceKey = 'calendar_account_source';
        $data = new stdClass();
        $data->linked_event_id = null;
        $data->last_sync = null;
        $data->source = null;

        if ($eventBean->object_name !== 'Meeting') {
            return $data;
        }

        try {
            if (!$eventBean->load_relationship($relationshipName)) {
                return $data;
            }

            /** @var Link2 $link */
            $link = $eventBean->$relationshipName;
            $link->load(
                [
                    'include_middle_table_fields' => true,
                ]
            );

            $rows = $link->rows ?? [];
            foreach ($rows as $row) {
                if (empty($row) || !is_array($row) || $row[$calendarAccountIdKey] !== $calendarAccount->id) {
                    continue;
                }
                if (isset($row['deleted']) && isTrue($row['deleted'])) {
                    continue;
                }
                $data->linked_event_id = $row[$externalEventIdKey] ?? null;
                $data->last_sync = $this->dateTimeHelper->createDateTime($row[$lastSyncKey] ?? null);
                $data->source = $row[$sourceKey] ?? null;
            }
        } catch (Throwable $e) {
            $log->debug("[CalendarAccountRelationshipManager][getRelationshipData] Failed to get relationship data for $eventBean->object_name $eventBean->id: " . $e->getMessage());
        }

        return $data;
    }

    /**
     * Enforce one-to-many constraint: ensure each event belongs to only one calendar account
     */
    protected function ensureUniqueCalendarAccount(Meeting|Call|Task $eventBean, CalendarEventType $eventType, CalendarAccount $newCalendarAccount): void
    {
        global $log;

        $eventRelationshipName = 'calendar_account_meetings';

        try {
            if (!$eventBean->load_relationship($eventRelationshipName)) {
                return;
            }

            /** @var Link2 $eventLink */
            $eventLink = $eventBean->$eventRelationshipName;
            $existingAccountIds = $eventLink->get();

            foreach ($existingAccountIds as $accountId) {
                if ($accountId === $newCalendarAccount->id) {
                    continue;
                }
                $eventLink->remove($accountId);
                $log->info("[CalendarAccountRelationshipManager][ensureUniqueCalendarAccount] Removed existing calendar account relationship: $eventType->value $eventBean->id from calendar account $accountId to enforce one-to-many constraint");
            }
        } catch (Throwable $e) {
            $log->warn("[CalendarAccountRelationshipManager][ensureUniqueCalendarAccount] Failed to enforce unique calendar account constraint for $eventType->value $eventBean->id: " . $e->getMessage());
        }
    }

}