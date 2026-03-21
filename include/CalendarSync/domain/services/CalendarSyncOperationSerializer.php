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
require_once 'include/CalendarSync/domain/entities/CalendarSyncOperation.php';
require_once 'include/CalendarSync/domain/enums/CalendarLocation.php';
require_once 'include/CalendarSync/domain/enums/CalendarSyncAction.php';
require_once 'include/CalendarSync/domain/services/CalendarEventSerializer.php';

/**
 * CalendarSyncOperationSerializer is responsible for serializing and deserializing CalendarSyncOperation objects.
 * It converts operation objects to JSON string representations and vice versa, ensuring proper handling of data and validation.
 */
class CalendarSyncOperationSerializer
{

    public function __construct(
        private readonly CalendarEventSerializer $serializer = new CalendarEventSerializer()
    ) {
    }

    /**
     * Serializes the given CalendarSyncOperation object into a JSON string representation.
     *
     * @param CalendarSyncOperation $operation The calendar synchronization operation to serialize.
     * @return string The serialized JSON string representation of the operation, or an empty string if serialization fails.
     */
    public function serialize(CalendarSyncOperation $operation): string
    {
        try {
            $data = [
                'user_id' => $operation->getUserId(),
                'subject_id' => $operation->getSubjectId(),
                'location' => $operation->getLocation()->value,
                'action' => $operation->getAction()->value,
                'calendar_account_id' => $operation->getCalendarAccountId(),
                'payload' => $operation->getPayload() ? $this->serializer->serializeEvent($operation->getPayload()) : null
            ];

            $result = json_encode($data, JSON_THROW_ON_ERROR);
            return $result !== false ? $result : '';
        } catch (JsonException $e) {
            return '';
        }
    }

    /**
     * Deserializes a JSON string representation into a CalendarSyncOperation object.
     *
     * @param string $serializedData The serialized JSON string containing the calendar synchronization data.
     * @return CalendarSyncOperation The deserialized CalendarSyncOperation object based on the provided JSON data.
     * @throws InvalidArgumentException If the serialized data is invalid, missing required fields, or contains invalid values.
     * @throws JsonException If the JSON decoding process encounters an error.
     */
    public function deserialize(string $serializedData): CalendarSyncOperation
    {
        $data = json_decode($serializedData, true, 512, JSON_THROW_ON_ERROR);

        if (!is_array($data) || !isset($data['subject_id'], $data['location'], $data['action'])) {
            throw new InvalidArgumentException('Invalid serialized data: missing required fields (subject_id, location, action)');
        }

        $user_id = $data['user_id'] ?? '';

        $location = CalendarLocation::tryFrom($data['location']);
        $action = CalendarSyncAction::tryFrom($data['action']);

        if (!$location || !$action) {
            throw new InvalidArgumentException('Invalid location or action values in serialized data');
        }

        $subject_id = $data['subject_id'];
        if (!is_string($subject_id)) {
            throw new InvalidArgumentException('Subject ID must be a string');
        }

        if (empty($subject_id) && $action !== CalendarSyncAction::CREATE) {
            throw new InvalidArgumentException('Subject ID is required for non-CREATE actions');
        }

        $payload = null;
        if (!empty($data['payload'])) {
            $payload = $this->serializer->deserializeEvent($data['payload']);
        }

        $calendar_account_id = $data['calendar_account_id'] ?? '';

        return new CalendarSyncOperation(
            user_id: $user_id,
            calendar_account_id: $calendar_account_id,
            subject_id: $subject_id,
            location: $location,
            action: $action,
            payload: $payload
        );
    }

}