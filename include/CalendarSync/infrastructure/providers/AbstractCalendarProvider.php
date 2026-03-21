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
require_once 'include/CalendarSync/domain/services/CalendarEventQuery.php';
require_once 'include/CalendarSync/domain/services/factories/CalendarAccountEventFactory.php';
require_once 'include/CalendarSync/domain/valueObjects/CalendarConnectionTestResult.php';
require_once 'include/CalendarSync/domain/CalendarSyncConfig.php';

/**
 * Abstract class representing a provider for handling calendar operations.
 * This class defines the structure and methods required for managing calendar accounts,
 * events, and related operations. Extend this class to implement specific calendar provider functionality.
 */
abstract class AbstractCalendarProvider
{

    protected ?CalendarAccount $connection = null;
    protected ?CalendarAccountEventFactory $eventFactory = null;
    protected ?CalendarAccountRelationshipManager $relationshipManager = null;
    protected CalendarSyncConfig $config;

    public function __construct(
        ?CalendarAccountEventFactory $eventFactory = new CalendarAccountEventFactory(),
        ?CalendarAccountRelationshipManager $relationshipManager = new CalendarAccountRelationshipManager(),
        ?CalendarSyncConfig $config = null
    ) {
        $this->eventFactory = $eventFactory;
        $this->relationshipManager = $relationshipManager;
        $this->config = $config ?? new CalendarSyncConfig();
    }

    /**
     * Sets the connection for the calendar account.
     *
     * @param CalendarAccount $connection The calendar account connection to be set.
     * @return void
     */
    public function setConnection(CalendarAccount $connection): void
    {
        $this->connection = $connection;
    }

    /**
     * Tests the connection and retrieves the connection status or details.
     *
     * @return CalendarConnectionTestResult A value object containing the results of the connection test.
     */
    abstract public function testCalendarConnection(): CalendarConnectionTestResult;

    /**
     * Retrieves a list of events based on the specified query criteria.
     *
     * @param CalendarEventQuery $query The query object containing the criteria for fetching events.
     * @return CalendarAccountEvent[] An array of events matching the specified query.
     */
    abstract public function getEvents(CalendarEventQuery $query): array;

    /**
     * Creates an event in the target system based on the source event provided.
     *
     * @param CalendarAccountEvent $sourceEvent The source event object from which the new event will be generated.
     * @param DateTime $syncTime The time when the event synchronization is performed.
     * @return string The identifier of the created event in the target system.
     */
    final public function createEventFromSource(CalendarAccountEvent $sourceEvent, DateTime $syncTime): string
    {
        $targetEvent = $this->eventFactory->fromSourceEvent(
            eventId: $this->generateEventId(),
            sourceEvent: $sourceEvent
        );
        return $this->createEvent($targetEvent, $syncTime);
    }

    /**
     * Generates a unique event ID for new events.
     *
     * @return string A unique event identifier
     */
    protected function generateEventId(): string
    {
        return uniqid('suitecrm-') . '-' . date('Ymd-His');
    }

    /**
     * Creates an event in the target system using the specified target event object.
     *
     * @param CalendarAccountEvent $targetEvent The event object to be created in the target system.
     * @param DateTime $syncTime The timestamp indicating when the event was last synchronized.
     * @return string The identifier of the newly created event in the target system.
     */
    final protected function createEvent(CalendarAccountEvent $targetEvent, DateTime $syncTime): string
    {
        $targetEvent->setLastSync($syncTime);
        return $this->doCreateEvent($targetEvent);
    }

    /**
     * Performs the actual event creation with the target event.
     * Override this method to handle provider-specific event creation logic.
     *
     * @param CalendarAccountEvent $targetEvent The target event to create in the calendar provider.
     * @return string The unique identifier of the newly created event.
     */
    abstract protected function doCreateEvent(CalendarAccountEvent $targetEvent): string;

    /**
     * Retrieves an event based on the provided target ID.
     *
     * @param string $targetId The unique identifier of the event to be retrieved.
     * @return CalendarAccountEvent|null The event associated with the given target ID, or null if no event is found.
     */
    abstract public function getEvent(string $targetId): ?CalendarAccountEvent;

    /**
     * Updates an existing event in the target system using the specified source event data.
     *
     * @param string $targetId The identifier of the event to be updated in the target system.
     * @param CalendarAccountEvent $sourceEvent The source event object containing the updated data.
     * @param DateTime $syncTime The time when the event update synchronization is performed.
     * @return void No return value.
     */
    final public function updateEventFromSource(string $targetId, CalendarAccountEvent $sourceEvent, DateTime $syncTime): void
    {
        $targetEvent = $this->eventFactory->fromSourceEvent($targetId, $sourceEvent);
        $this->updateEvent($targetEvent, $syncTime);
    }

    /**
     * Updates an event in the target system based on the provided event object.
     *
     * @param CalendarAccountEvent $targetEvent The event object to be updated in the target system.
     * @param DateTime $syncTime The time when the event update synchronization is performed.
     * @return void
     */
    final protected function updateEvent(CalendarAccountEvent $targetEvent, DateTime $syncTime): void
    {
        $targetEvent->setLastSync($syncTime);
        $this->doUpdateEvent($targetEvent);
    }

    /**
     * Performs the actual event update with provider-specific logic.
     * Override this method to handle provider-specific event update logic.
     *
     * @param CalendarAccountEvent $targetEvent The event object containing updated details of the calendar event.
     * @return void
     */
    abstract protected function doUpdateEvent(CalendarAccountEvent $targetEvent): void;

    /**
     * Updates the source event with the details of the target event and performs synchronization.
     *
     * @param string $targetId The identifier of the target event in the system.
     * @param CalendarAccountEvent $sourceEvent The source event object to be updated with the target event reference.
     * @param DateTime $syncTime The time when the event synchronization is performed.
     * @return void
     */
    final public function updateSourceEvent(string $targetId, CalendarAccountEvent $sourceEvent, DateTime $syncTime): void
    {
        $updatedSourceEvent = $sourceEvent->setLinkedEventId($targetId);
        $this->updateEvent($updatedSourceEvent, $syncTime);
    }

    /**
     * Template method that performs common operations before deleting an event.
     * This method is final to ensure the template pattern is followed.
     *
     * @param string $targetId The unique identifier of the event to be deleted.
     * @return void
     */
    final public function deleteEvent(string $targetId): void
    {
        $this->doDeleteEvent($targetId);
    }

    /**
     * Performs the actual event deletion with provider-specific logic.
     * Override this method to handle provider-specific event deletion logic.
     *
     * @param string $targetId The unique identifier of the event to be deleted.
     * @return void
     */
    abstract protected function doDeleteEvent(string $targetId): void;

    /**
     * Gets the external calendar name from config with fallback to default.
     *
     * @return string The calendar name.
     */
    protected function getExternalCalendarName(): string
    {
        return $this->config->getExternalCalendarName();
    }

}