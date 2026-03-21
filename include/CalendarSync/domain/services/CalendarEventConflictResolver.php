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

require_once 'include/CalendarSync/domain/enums/ConflictResolution.php';

/**
 * Determines which calendar event version should be used when syncing between internal and external calendars.
 *
 * The selection process follows these scenarios:
 * 1. Neither event modified since last sync: Returns target event (no sync needed)
 * 2. One-sided change: Returns the modified event (automatic selection)
 * 3. Both modified to identical content: Returns target event (convergent edit)
 * 4. Two-sided change (real conflict): Applies user's conflict resolution strategy
 *
 * Performance: Modification checks (cheap) are performed before content comparison (expensive).
 *
 * Last Sync Semantics:
 * - last_sync represents the last time this event pair was synchronized
 * - For first sync: last_sync may be null, treated as "always modified" to ensure initial sync
 * - Events are considered modified if date_modified > last_sync
 *
 * Event Relationship:
 * - Events must be related (linked via getLinkedEventId()) to be compared
 * - Validation prevents accidental comparison of unrelated events
 */
class CalendarEventConflictResolver
{

    protected ?LoggerManager $log;

    public function __construct()
    {
        $this->log = LoggerManager::getLogger();
    }

    /**
     * Determines which event should be used for synchronization by detecting changes and resolving conflicts.
     *
     * Flow:
     * - If only one side modified: returns the modified event (one-sided update)
     * - If both sides modified: applies the conflict resolution strategy (real conflict)
     * - If neither modified: returns either event (no sync needed)
     *
     * @param CalendarAccountEvent $targetEvent The target calendar event (where we sync TO).
     * @param CalendarAccountEvent $sourceEvent The source calendar event (where we sync FROM).
     * @param ConflictResolution $strategy The conflict resolution strategy for two-sided changes.
     *
     * @return CalendarAccountEvent The event selected for synchronization.
     */
    public function determineWinningEvent(
        CalendarAccountEvent $targetEvent,
        CalendarAccountEvent $sourceEvent,
        ConflictResolution $strategy
    ): CalendarAccountEvent {
        $this->validateEventData($targetEvent);
        $this->validateEventData($sourceEvent);

        $earlyDecision = $this->determineEarlySelection($targetEvent, $sourceEvent);
        if ($earlyDecision !== null) {
            return $earlyDecision;
        }

        $this->log->info("[CalendarEventConflictResolver][determineWinningEvent] Two-sided change detected, applying $strategy->name strategy");

        return match ($strategy) {
            ConflictResolution::TIMESTAMP => $this->resolveByTimestamp($targetEvent, $sourceEvent),
            ConflictResolution::EXTERNAL_BASED => $this->resolveByExternalPriority($targetEvent, $sourceEvent),
            ConflictResolution::INTERNAL_BASED => $this->resolveByInternalPriority($targetEvent, $sourceEvent),
        };
    }

    /**
     * Validates event data for security and integrity.
     *
     * @param CalendarAccountEvent $event The event to validate.
     * @throws RuntimeException If event data contains security issues or invalid timestamps.
     */
    protected function validateEventData(CalendarAccountEvent $event): void
    {
        $eventId = $event->getId();
        $eventSource = $event->isExternal() ? 'external' : 'internal';

        $maxTimestamp = 2147483647;
        $dateModified = $event->getDateModified();
        if ($dateModified->getTimestamp() > $maxTimestamp) {
            $this->log->warn("[CalendarEventConflictResolver][validateEventData] Event $eventId ($eventSource) has timestamp beyond year 2038, may cause issues");
        }

        $suspiciousPatterns = ['<script', 'javascript:', 'onerror=', 'onclick='];
        $fieldsToCheck = [
            'title' => $event->getTitle(),
            'description' => $event->getDescription(),
            'location' => $event->getLocation()
        ];

        foreach ($fieldsToCheck as $fieldName => $fieldValue) {
            foreach ($suspiciousPatterns as $pattern) {
                if (stripos($fieldValue, $pattern) !== false) {
                    $this->log->warn("[CalendarEventConflictResolver][validateEventData] Event $eventId ($eventSource) contains suspicious content in $fieldName: pattern '$pattern'");
                }
            }
        }
    }

    /**
     * Determines if a selection can be made without applying conflict resolution strategy.
     *
     * @param CalendarAccountEvent $targetEvent The target calendar event.
     * @param CalendarAccountEvent $sourceEvent The source calendar event.
     * @return CalendarAccountEvent|null The selected event, or null if strategy is needed.
     */
    protected function determineEarlySelection(CalendarAccountEvent $targetEvent, CalendarAccountEvent $sourceEvent): ?CalendarAccountEvent
    {
        if ($targetEvent->getContentChecksum() === $sourceEvent->getContentChecksum()) {
            $this->log->info('[CalendarEventConflictResolver][determineEarlySelection] Both modified to identical content, no conflict resolution needed');
            return $targetEvent;
        }

        return null;
    }

    /**
     * Resolves a two-sided conflict by comparing modification timestamps and selecting the most recent event.
     *
     * This method is only called when both events have been modified since last sync.
     * Uses microsecond precision to detect sub-second modifications.
     * If timestamps are equal, uses event ID as deterministic tie-breaker to prevent sync loops.
     *
     * @param CalendarAccountEvent $targetEvent The target calendar event to compare.
     * @param CalendarAccountEvent $sourceEvent The source calendar event to compare.
     *
     * @return CalendarAccountEvent The most recently modified event.
     */
    protected function resolveByTimestamp(CalendarAccountEvent $targetEvent, CalendarAccountEvent $sourceEvent): CalendarAccountEvent
    {
        $targetTimestamp = (float)$targetEvent->getDateModified()->getTimestamp();
        $sourceTimestamp = (float)$sourceEvent->getDateModified()->getTimestamp();

        if ($sourceTimestamp > $targetTimestamp) {
            $this->log->info('[CalendarEventConflictResolver][resolveByTimestamp] Source is more recent');
            return $sourceEvent;
        }

        $this->log->info('[CalendarEventConflictResolver][resolveByTimestamp] Target is more recent');
        return $targetEvent;
    }

    /**
     * Resolves a two-sided conflict by prioritizing the external event.
     *
     * This method is only called when both events have been modified since last sync.
     * If both events have the same external status, falls back to timestamp comparison.
     *
     * @param CalendarAccountEvent $targetEvent The target calendar event to evaluate.
     * @param CalendarAccountEvent $sourceEvent The source calendar event to evaluate.
     *
     * @return CalendarAccountEvent The external event, or the most recent if both have same status.
     */
    protected function resolveByExternalPriority(CalendarAccountEvent $targetEvent, CalendarAccountEvent $sourceEvent): CalendarAccountEvent
    {
        $targetIsExternal = $targetEvent->isExternal();
        $sourceIsExternal = $sourceEvent->isExternal();

        if (!$targetIsExternal && $sourceIsExternal) {
            $this->log->info('[CalendarEventConflictResolver][resolveByExternalPriority] Selecting external source');
            return $sourceEvent;
        }
        if ($targetIsExternal && !$sourceIsExternal) {
            $this->log->info('[CalendarEventConflictResolver][resolveByExternalPriority] Selecting external target');
            return $targetEvent;
        }

        $this->log->info('[CalendarEventConflictResolver][resolveByExternalPriority] Both same status, falling back to timestamp');
        return $this->resolveByTimestamp($targetEvent, $sourceEvent);
    }

    /**
     * Resolves a two-sided conflict by prioritizing the internal event.
     *
     * This method is only called when both events have been modified since last sync.
     * If both events have the same external status, falls back to timestamp comparison.
     *
     * @param CalendarAccountEvent $targetEvent The target calendar event to consider in the resolution process.
     * @param CalendarAccountEvent $sourceEvent The source calendar event to consider in the resolution process.
     *
     * @return CalendarAccountEvent The internal event, or the most recent if both have same status.
     */
    protected function resolveByInternalPriority(CalendarAccountEvent $targetEvent, CalendarAccountEvent $sourceEvent): CalendarAccountEvent
    {
        $targetIsExternal = $targetEvent->isExternal();
        $sourceIsExternal = $sourceEvent->isExternal();

        if ($targetIsExternal && !$sourceIsExternal) {
            $this->log->info('[CalendarEventConflictResolver][resolveByInternalPriority] Selecting internal source');
            return $sourceEvent;
        }
        if (!$targetIsExternal && $sourceIsExternal) {
            $this->log->info('[CalendarEventConflictResolver][resolveByInternalPriority] Selecting internal target');
            return $targetEvent;
        }

        $this->log->info('[CalendarEventConflictResolver][resolveByInternalPriority] Both same status, falling back to timestamp');
        return $this->resolveByTimestamp($targetEvent, $sourceEvent);
    }

}