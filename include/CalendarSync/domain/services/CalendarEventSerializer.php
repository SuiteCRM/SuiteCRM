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

/**
 * Handles serialization and deserialization of CalendarAccountEvent objects to and from JSON strings.
 */
class CalendarEventSerializer
{

    /**
     * Serialize a CalendarAccountEvent object into a JSON string.
     *
     * @param CalendarAccountEvent $event The event object to serialize.
     * @return string The JSON string representation of the event. Returns an empty string if serialization fails.
     */
    public function serializeEvent(CalendarAccountEvent $event): string
    {
        try {
            $result = json_encode(
                [
                    'id' => $event->getId(),
                    'name' => $event->getName(),
                    'description' => $event->getDescription(),
                    'location' => $event->getLocation(),
                    'date_start' => $event->getDateStartString(),
                    'date_end' => $event->getDateEndString(),
                    'assigned_user_id' => $event->getAssignedUserId(),
                    'type' => $event->getType()->value,
                    'linked_event_id' => $event->getLinkedEventId(),
                    'last_sync' => $event->getLastSyncString(),
                    'date_modified' => $event->getDateModifiedString(),
                    'is_external' => $event->isExternal()
                ], JSON_THROW_ON_ERROR
            );

            return $result !== false ? $result : '';
        } catch (JsonException $e) {
            return '';
        }
    }

    /**
     * Deserializes a JSON-encoded string into a CalendarAccountEvent object.
     *
     * @param string $serializedData The JSON-encoded string representing the event data.
     * @return CalendarAccountEvent|null The deserialized CalendarAccountEvent object, or null if deserialization fails.
     */
    public function deserializeEvent(string $serializedData): ?CalendarAccountEvent
    {
        try {
            $data = json_decode($serializedData, true, 512, JSON_THROW_ON_ERROR);

            if (!is_array($data) || !isset($data['id'])) {
                return null;
            }

            $type = CalendarEventType::MEETING;
            if (isset($data['type'])) {
                $type = CalendarEventType::tryFrom($data['type']) ?? CalendarEventType::MEETING;
            }

            return new CalendarAccountEvent(
                id: $data['id'],
                name: $data['name'] ?? '',
                description: $data['description'] ?? '',
                location: $data['location'] ?? '',
                date_start: $data['date_start'],
                date_end: $data['date_end'],
                assigned_user_id: $data['assigned_user_id'] ?? '',
                type: $type,
                linked_event_id: $data['linked_event_id'] ?? null,
                last_sync: $data['last_sync'],
                date_modified: $data['date_modified'],
                is_external: $data['is_external'] ?? false
            );
        } catch (JsonException $e) {
            return null;
        }
    }

}