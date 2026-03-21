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
require_once 'include/CalendarSync/domain/entities/CalendarAccountEvent.php';
require_once 'include/CalendarSync/domain/enums/CalendarEventType.php';
require_once 'include/CalendarSync/domain/valueObjects/CalendarConnectionTestResult.php';

/**
 * Provides functionality to interact with calendar events stored as JSON files.
 * This class extends the AbstractCalendarProvider and offers methods to
 * test a connection, retrieve events, initialize paths, and map JSON data
 * to domain-specific objects.
 */
class JsonFileCalendarProvider extends AbstractCalendarProvider
{

    private string $filePath;
    private array $events = [];

    /**
     * @inheritdoc
     */
    public function testCalendarConnection(): CalendarConnectionTestResult
    {
        $GLOBALS['log']->debug('[JsonFileCalendarProvider][testCalendarConnection] Starting connection test');
        $this->initializeFilePath();

        try {
            $this->loadEvents();

            if (!is_writable(dirname($this->filePath))) {
                throw new RuntimeException("Directory not writable: " . dirname($this->filePath));
            }

            $GLOBALS['log']->debug("[JsonFileCalendarProvider][testCalendarConnection] Connection test successful for file: $this->filePath");

            return new CalendarConnectionTestResult(
                success: true,
                connection: $this->connection,
                authenticationStatus: 'file_system',
                externalCalendarId: $this->filePath
            );

        } catch (Throwable $e) {
            $GLOBALS['log']->error("[JsonFileCalendarProvider][testCalendarConnection] Connection test failed: " . $e->getMessage());
            return new CalendarConnectionTestResult(
                success: false,
                connection: $this->connection,
                errorMessage: $e->getMessage(),
                errorCode: (string)$e->getCode(),
                authenticationStatus: 'file_system_error'
            );
        }
    }

    /**
     * Initializes the file path for storing calendar data based on the current connection
     * and user ID. Ensures the directory for storing calendar data exists and sets
     * the appropriate file path.
     *
     * @return void
     * @throws RuntimeException If no calendar connection or user ID is set.
     */
    protected function initializeFilePath(): void
    {
        if (!$this->connection || empty($this->connection->calendar_user_id)) {
            $GLOBALS['log']->error('[JsonFileCalendarProvider][initializeFilePath] No calendar connection or user ID set');
            throw new RuntimeException('No calendar connection or user ID set');
        }

        $connectionId = $this->connection->id;
        $userId = $this->connection->calendar_user_id;
        $dataDir = rtrim($GLOBALS['sugar_config']['cache_dir'] ?? 'cache', '/') . '/calendar_data';

        if (!is_dir($dataDir)) {
            $GLOBALS['log']->debug('[JsonFileCalendarProvider][initializeFilePath] Creating calendar data directory: ' . $dataDir);
            mkdir($dataDir, 0755, true);
        }

        $this->filePath = "$dataDir/user-$userId-calendar-$connectionId.json";
        $GLOBALS['log']->debug('[JsonFileCalendarProvider][initializeFilePath] File path initialized: ' . $this->filePath);
    }

    /**
     * Loads events from a JSON file specified by the file path. If the file does not exist
     * or contains no data, initializes an empty events array. If the file content cannot
     * be read or JSON parsing fails, an exception is thrown.
     *
     * @return void
     * @throws RuntimeException If the file cannot be read or if JSON parsing fails.
     */
    protected function loadEvents(): void
    {
        $GLOBALS['log']->debug('[JsonFileCalendarProvider][loadEvents] Loading events from file: ' . $this->filePath);

        if (!file_exists($this->filePath)) {
            $GLOBALS['log']->debug('[JsonFileCalendarProvider][loadEvents] File does not exist, initializing empty events array');
            $this->events = [];
            return;
        }

        $content = file_get_contents($this->filePath);
        if ($content === false) {
            $GLOBALS['log']->error('[JsonFileCalendarProvider][loadEvents] Failed to read file: ' . $this->filePath);
            throw new RuntimeException("Failed to read file: $this->filePath");
        }

        if (empty($content)) {
            $GLOBALS['log']->debug('[JsonFileCalendarProvider][loadEvents] File is empty, initializing empty events array');
            $this->events = [];
            return;
        }

        $data = json_decode($content, true);
        if ($data === null && json_last_error() !== JSON_ERROR_NONE) {
            $GLOBALS['log']->error('[JsonFileCalendarProvider][loadEvents] Failed to parse JSON file: ' . json_last_error_msg());
            throw new RuntimeException("Failed to parse JSON file: " . json_last_error_msg());
        }

        $this->events = $data ?? [];
        $GLOBALS['log']->debug('[JsonFileCalendarProvider][loadEvents] Successfully loaded ' . count($this->events) . ' events from file');
    }

    /**
     * @inheritdoc
     */
    public function getEvents(CalendarEventQuery $query): array
    {
        $GLOBALS['log']->debug('[JsonFileCalendarProvider][getEvents] Starting to retrieve events with query filters');
        $this->initializeFilePath();

        try {
            $this->loadEvents();

            $filteredEvents = [];
            foreach ($this->events as $eventId => $eventData) {
                $event = $this->mapToCalendarAccountEvent($eventId, $eventData);
                if (!$event) {
                    continue;
                }

                if ($this->matchesQuery($event, $query)) {
                    $filteredEvents[] = $event;
                }
            }

            $GLOBALS['log']->debug("[JsonFileCalendarProvider][getEvents] Found " . count($filteredEvents) . " events matching query out of " . count($this->events) . " total events");
            return $filteredEvents;

        } catch (Throwable $e) {
            $GLOBALS['log']->error("[JsonFileCalendarProvider][getEvents] Failed to get events: " . $e->getMessage());
            throw $e;
        }
    }

    /**
     * Maps the provided JSON event data to a CalendarAccountEvent object.
     * Extracts necessary details such as event metadata, title, description,
     * location, and timing, and creates a representation of the calendar event.
     * If mapping fails due to an exception, null is returned.
     *
     * @param string $eventId The unique identifier of the event to be mapped.
     * @param array $eventData The raw JSON event data containing event details.
     * @return CalendarAccountEvent|null A CalendarAccountEvent object if mapping is successful, or null on failure.
     */
    protected function mapToCalendarAccountEvent(string $eventId, array $eventData): ?CalendarAccountEvent
    {
        $GLOBALS['log']->debug('[JsonFileCalendarProvider][mapToCalendarAccountEvent] Mapping JSON data to CalendarAccountEvent: ' . $eventId);

        try {
            $type = CalendarEventType::MEETING;
            $eventMetadataType = $eventData['metadata']['type'] ?? null;
            if (isset($eventMetadataType)) {
                $type = CalendarEventType::tryFrom($eventMetadataType) ?? CalendarEventType::MEETING;
            }

            return new CalendarAccountEvent(
                id: $eventId,
                name: $eventData['title'] ?? '',
                description: $eventData['body'] ?? '',
                location: $eventData['venue'] ?? '',
                date_start: $eventData['start_time'] ?? '',
                date_end: $eventData['end_time'] ?? '',
                assigned_user_id: $eventData['owner_id'] ?? '',
                type: $type,
                linked_event_id: $eventData['linked_to'] ?? null,
                last_sync: $eventData['last_synced'] ?? null,
                date_modified: $eventData['modified_at'] ?? '',
                is_external: true
            );
        } catch (Throwable $e) {
            $GLOBALS['log']->error("[JsonFileCalendarProvider][mapToCalendarAccountEvent] Failed to map event data for $eventId: " . $e->getMessage());
            return null;
        }
    }

    /**
     * Determines whether a given calendar event matches the criteria specified in a query.
     * The method evaluates the event's start date against the query's start and end dates
     * to determine if the event falls within the required range.
     *
     * @param CalendarAccountEvent $event The calendar event to evaluate.
     * @param CalendarEventQuery $query The query containing filtering criteria such as start and end dates.
     *
     * @return bool True if the event matches the query criteria, false otherwise.
     */
    protected function matchesQuery(CalendarAccountEvent $event, CalendarEventQuery $query): bool
    {
        if ($query->getStartDate()) {
            $eventStart = $event->getStartDateTime();
            if ($eventStart < $query->getStartDate()) {
                $GLOBALS['log']->debug('[JsonFileCalendarProvider][matchesQuery] Event ' . $event->getId() . ' filtered out: starts before query start date');
                return false;
            }
        }

        if ($query->getEndDate()) {
            $eventStart = $event->getStartDateTime();
            if ($eventStart > $query->getEndDate()) {
                $GLOBALS['log']->debug('[JsonFileCalendarProvider][matchesQuery] Event ' . $event->getId() . ' filtered out: starts after query end date');
                return false;
            }
        }

        $GLOBALS['log']->debug('[JsonFileCalendarProvider][matchesQuery] Event ' . $event->getId() . ' matches query filters');
        return true;
    }

    /**
     * @inheritdoc
     */
    protected function doCreateEvent(CalendarAccountEvent $targetEvent): string
    {
        $GLOBALS['log']->debug('[JsonFileCalendarProvider][doCreateEvent] Starting to create new event: ' . $targetEvent->getName());
        $this->initializeFilePath();

        try {
            $this->loadEvents();

            $newEventId = $targetEvent->getId();

            $this->events[$newEventId] = $this->mapFromCalendarAccountEvent($targetEvent);
            $this->saveEvents();

            $GLOBALS['log']->info("[JsonFileCalendarProvider][doCreateEvent] Successfully created event: $newEventId");
            return $newEventId;

        } catch (Throwable $e) {
            $GLOBALS['log']->error("[JsonFileCalendarProvider][doCreateEvent] Failed to create event: " . $e->getMessage());
            throw $e;
        }
    }

    /**
     * Generates a unique event ID by combining a prefix, a unique identifier,
     * and the current timestamp. Logs the generated ID for debugging purposes.
     *
     * @return string A unique event ID.
     */
    protected function generateEventId(): string
    {
        $eventId = 'json_' . uniqid() . '_' . time();
        $GLOBALS['log']->debug('[JsonFileCalendarProvider][generateEventId] Generated new event ID: ' . $eventId);
        return $eventId;
    }

    /**
     * Maps a CalendarAccountEvent object to a formatted array suitable for JSON storage.
     * Extracts event details from the provided object and creates a structured representation.
     *
     * @param CalendarAccountEvent $event The calendar event to be mapped.
     * @return array An associative array containing mapped event data.
     */
    protected function mapFromCalendarAccountEvent(CalendarAccountEvent $event): array
    {
        $GLOBALS['log']->debug('[JsonFileCalendarProvider][mapFromCalendarAccountEvent] Mapping event to JSON format: ' . $event->getId());

        return [
            'uuid' => $event->getId(),
            'title' => $event->getName(),
            'body' => $event->getDescription(),
            'venue' => $event->getLocation(),
            'start_time' => $event->getDateStartString(),
            'end_time' => $event->getDateEndString(),
            'owner_id' => $event->getAssignedUserId(),
            'linked_to' => $event->getLinkedEventId(),
            'last_synced' => $event->getLastSyncString(),
            'modified_at' => $event->getDateModifiedString(),
            'external_source' => true,
            'metadata' => [
                'type' => $event->getType()->value,
                'provider' => 'json_file',
                'created_at' => date('Y-m-d H:i:s'),
                'version' => '1.0'
            ]
        ];
    }

    /**
     * Saves the current list of events to a JSON file at the specified file path.
     * Ensures the events are properly encoded and written to the file. Logs the
     * progress and handles any failures during the encoding or writing process.
     *
     * @return void
     * @throws RuntimeException If encoding the events to JSON fails or writing the file fails.
     */
    protected function saveEvents(): void
    {
        $GLOBALS['log']->debug('[JsonFileCalendarProvider][saveEvents] Saving ' . count($this->events) . ' events to file: ' . $this->filePath);

        $content = json_encode($this->events, JSON_PRETTY_PRINT | JSON_UNESCAPED_UNICODE);
        if ($content === false) {
            $GLOBALS['log']->error('[JsonFileCalendarProvider][saveEvents] Failed to encode events to JSON');
            throw new RuntimeException("Failed to encode events to JSON");
        }

        $result = file_put_contents($this->filePath, $content, LOCK_EX);
        if ($result === false) {
            $GLOBALS['log']->error('[JsonFileCalendarProvider][saveEvents] Failed to write file: ' . $this->filePath);
            throw new RuntimeException("Failed to write file: $this->filePath");
        }

        $GLOBALS['log']->debug('[JsonFileCalendarProvider][saveEvents] Successfully saved events to file');
    }

    /**
     * @inheritdoc
     */
    public function getEvent(string $targetId): ?CalendarAccountEvent
    {
        $GLOBALS['log']->debug('[JsonFileCalendarProvider][getEvent] Retrieving event: ' . $targetId);
        $this->initializeFilePath();

        try {
            $this->loadEvents();

            if (!isset($this->events[$targetId])) {
                $GLOBALS['log']->debug("[JsonFileCalendarProvider][getEvent] Event not found: $targetId");
                return null;
            }

            $event = $this->mapToCalendarAccountEvent($targetId, $this->events[$targetId]);
            $GLOBALS['log']->debug("[JsonFileCalendarProvider][getEvent] Successfully retrieved event: $targetId");
            return $event;

        } catch (Throwable $e) {
            $GLOBALS['log']->error("[JsonFileCalendarProvider][getEvent] Failed to retrieve event $targetId: " . $e->getMessage());
            throw $e;
        }
    }

    /**
     * @inheritdoc
     */
    protected function doUpdateEvent(CalendarAccountEvent $targetEvent): void
    {
        $targetEventId = $targetEvent->getId();
        $GLOBALS['log']->debug('[JsonFileCalendarProvider][doUpdateEvent] Starting to update event: ' . $targetEventId);
        $this->initializeFilePath();

        try {
            $this->loadEvents();

            if (!isset($this->events[$targetEventId])) {
                $GLOBALS['log']->warn("[JsonFileCalendarProvider][doUpdateEvent] Event not found for update: $targetEventId");
                throw new RuntimeException("Event not found: $targetEventId");
            }

            $this->events[$targetEventId] = $this->mapFromCalendarAccountEvent($targetEvent);
            $this->saveEvents();

            $GLOBALS['log']->info("[JsonFileCalendarProvider][doUpdateEvent] Successfully updated event: $targetEventId");

        } catch (Throwable $e) {
            $GLOBALS['log']->error("[JsonFileCalendarProvider][doUpdateEvent] Failed to update event $targetEventId: " . $e->getMessage());
            throw $e;
        }
    }

    /**
     * @inheritdoc
     */
    protected function doDeleteEvent(string $targetId): void
    {
        $GLOBALS['log']->debug('[JsonFileCalendarProvider][doDeleteEvent] Starting to delete event: ' . $targetId);
        $this->initializeFilePath();

        try {
            $this->loadEvents();

            if (!isset($this->events[$targetId])) {
                $GLOBALS['log']->warn("[JsonFileCalendarProvider][doDeleteEvent] Event not found for deletion: $targetId");
                throw new RuntimeException("Event not found: $targetId");
            }

            unset($this->events[$targetId]);
            $this->saveEvents();

            $GLOBALS['log']->info("[JsonFileCalendarProvider][doDeleteEvent] Successfully deleted event: $targetId");

        } catch (Throwable $e) {
            $GLOBALS['log']->error("[JsonFileCalendarProvider][doDeleteEvent] Failed to delete event $targetId: " . $e->getMessage());
            throw $e;
        }
    }

}