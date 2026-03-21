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

require_once 'include/CalendarSync/domain/entities/CalendarSyncOperation.php';
require_once 'include/CalendarSync/domain/services/CalendarEventConflictResolver.php';

/**
 * The CalendarSyncOperationDiscovery class is responsible for analyzing calendar events
 * and determining the necessary synchronization operations between source and target calendars.
 */
class CalendarSyncOperationDiscovery
{

    public function __construct(
        protected CalendarEventConflictResolver $conflictResolver = new CalendarEventConflictResolver(),
        protected CalendarSyncConfig $calendarSyncConfig = new CalendarSyncConfig(),
    ) {
    }

    /**
     * Discovers synchronization operations needed to align events between source and target calendars.
     *
     * This method compares events from the source calendar with events from the target calendar
     * and determines the necessary synchronization operations (create, update, or delete)
     * to reconcile differences between the two calendars.
     *
     * @param CalendarAccountEvent[] $sourceEvents List of events from the source calendar.
     * @param CalendarAccountEvent[] $targetEvents List of events from the target calendar.
     * @param CalendarLocation $targetLocation Target calendar location (internal or external).
     * @param bool $allowDeletion Indicates whether deletion of source events is allowed when unmatched in the target calendar.
     * @param string $userId ID of the user performing the synchronization.
     * @param string $calendarAccountId ID of the calendar account being synchronized.
     * @param AbstractCalendarProvider $targetProvider Calendar provider to fetch additional data for the target calendar, if needed.
     * @return CalendarSyncOperation[] List of synchronization operations to execute.
     */
    public function discoverSyncOperations(
        array $sourceEvents,
        array $targetEvents,
        CalendarLocation $targetLocation,
        bool $allowDeletion,
        string $userId,
        string $calendarAccountId,
        AbstractCalendarProvider $targetProvider
    ): array {
        $operations = [];
        $targetLocationName = $targetLocation->value;

        /** @var array<string, CalendarAccountEvent> $targetEventsLookup */
        $targetEventsLookup = [];
        /** @var array<string,CalendarAccountEvent> $reverseLinkedLookup */
        $reverseLinkedLookup = [];
        /** @var array<string,list<CalendarAccountEvent>> $titleLookup */
        $titleLookup = [];
        /** @var array<string,list<CalendarAccountEvent>> $timeWindowLookup */
        $timeWindowLookup = [];

        foreach ($targetEvents as $targetEvent) {
            if (!$targetEvent instanceof CalendarAccountEvent || empty($targetEvent->getId())) {
                continue;
            }

            $targetEventId = $targetEvent->getId();
            $targetEventsLookup[$targetEventId] = $targetEvent;

            $linkedEventId = $targetEvent->getLinkedEventId();
            if ($linkedEventId) {
                $reverseLinkedLookup[$linkedEventId] = $targetEvent;
            }
        }

        require_once 'include/CalendarSync/domain/enums/ConflictResolution.php';
        require_once 'include/CalendarSync/domain/CalendarSyncConfig.php';
        $conflictResolutionStrategy = $this->calendarSyncConfig->getConflictResolution();
        $conflictResolution = ConflictResolution::tryFrom($conflictResolutionStrategy) ?: ConflictResolution::TIMESTAMP;

        $GLOBALS['log']->debug("[CalendarSyncOperationDiscovery][UserJob] Discovering sync operations for $targetLocationName target location for user $userId");

        foreach ($sourceEvents as $sourceEvent) {
            if (!$sourceEvent instanceof CalendarAccountEvent || empty($sourceEvent->getId())) {
                continue;
            }

            $sourceEventId = $sourceEvent->getId();
            $linkedTargetEventId = $sourceEvent->getLinkedEventId();

            $sourceHasToBeDeleted = !empty($linkedTargetEventId) && !isset($targetEventsLookup[$linkedTargetEventId]) && $allowDeletion;
            if ($sourceHasToBeDeleted) {
                $sourceLocation = $targetLocation->getOpposite();

                $GLOBALS['log']->debug("[CalendarSyncOperationDiscovery][UserJob] $sourceLocation->value event $sourceEventId marked for deletion - linked $targetLocationName event $linkedTargetEventId no longer exists");

                $operations[] = $this->createSyncOperation(
                    userId: $userId,
                    calendarAccountId: $calendarAccountId,
                    action: CalendarSyncAction::DELETE,
                    location: $sourceLocation,
                    targetEventId: $sourceEventId,
                );

                continue;
            }

            $matchingTargetEvent = $targetEventsLookup[$linkedTargetEventId] ?? $reverseLinkedLookup[$sourceEventId] ?? null;

            if ($matchingTargetEvent === null) {
                $GLOBALS['log']->debug("[CalendarSyncOperationDiscovery][UserJob] Creating $targetLocationName event from source event $sourceEventId");

                $operations[] = $this->createSyncOperation(
                    userId: $userId,
                    calendarAccountId: $calendarAccountId,
                    action: CalendarSyncAction::CREATE,
                    location: $targetLocation,
                    payload: $sourceEvent,
                );
            } else {
                $targetEventId = $matchingTargetEvent->getId();
                $chosenEventVersion = $this->conflictResolver->determineWinningEvent($matchingTargetEvent, $sourceEvent, $conflictResolution);

                $targetNeedsUpdate = $chosenEventVersion->getId() === $sourceEventId;
                $targetNeedsLinkUpdate = $matchingTargetEvent->getLinkedEventId() !== $sourceEventId;

                if (!$targetNeedsUpdate && !$targetNeedsLinkUpdate) {
                    continue;
                }

                $reasons = [];
                if ($targetNeedsUpdate) {
                    $reasons[] = 'Source version is newer';
                }
                if ($targetNeedsLinkUpdate) {
                    $reasons[] = 'Correcting event linkage';
                }
                $reason = implode('. ', $reasons) . '.';
                $GLOBALS['log']->debug("[CalendarSyncOperationDiscovery][UserJob] Updating $targetLocationName event $targetEventId from source event $sourceEventId. $reason");
                $matchingTargetEvent->setLinkedEventId($sourceEventId);
                $operations[] = $this->createSyncOperation(
                    userId: $userId,
                    calendarAccountId: $calendarAccountId,
                    action: CalendarSyncAction::UPDATE,
                    location: $targetLocation,
                    targetEventId: $targetEventId,
                    payload: $sourceEvent,
                );
            }
        }

        return $operations;
    }

    /**
     * Creates a new synchronization operation for a calendar account.
     *
     * @param string $userId The unique identifier of the user.
     * @param string $calendarAccountId The unique identifier of the calendar account.
     * @param CalendarSyncAction $action The action to be performed, such as create, update, or delete.
     * @param CalendarLocation $location The calendar location to which the operation applies.
     * @param string|null $targetEventId The identifier of the target event, or null if not applicable.
     * @param CalendarAccountEvent|null $payload Optional event payload associated with the operation.
     *
     * @return CalendarSyncOperation Returns a new instance of CalendarSyncOperation configured with the provided parameters.
     */
    public function createSyncOperation(
        string $userId,
        string $calendarAccountId,
        CalendarSyncAction $action,
        CalendarLocation $location,
        ?string $targetEventId = null,
        ?CalendarAccountEvent $payload = null,
    ): CalendarSyncOperation {
        return new CalendarSyncOperation(
            user_id: $userId,
            calendar_account_id: $calendarAccountId,
            subject_id: $action === CalendarSyncAction::CREATE ? '' : $targetEventId,
            location: $location,
            action: $action,
            payload: $action === CalendarSyncAction::DELETE ? null : $payload
        );
    }

}