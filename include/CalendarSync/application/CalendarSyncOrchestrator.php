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

require_once 'include/CalendarSync/domain/services/CalendarAccountValidator.php';
require_once 'include/CalendarSync/domain/services/CalendarAccountRepository.php';
require_once 'include/CalendarSync/application/CalendarSyncJobManager.php';
require_once 'include/CalendarSync/application/CalendarSyncOperationDiscovery.php';
require_once 'include/CalendarSync/domain/CalendarSyncConfig.php';
require_once 'include/CalendarSync/domain/services/factories/CalendarEventQueryFactory.php';
require_once 'include/CalendarSync/domain/enums/CalendarLocation.php';
require_once 'include/CalendarSync/domain/enums/CalendarSyncAction.php';
require_once 'include/CalendarSync/domain/services/factories/CalendarAccountEventFactory.php';
require_once 'include/CalendarSync/infrastructure/jobs/CalendarSyncJobFactory.php';
require_once 'include/CalendarSync/infrastructure/registry/CalendarProviderRegistry.php';

/**
 * Class responsible for orchestrating the synchronization of calendar accounts and events.
 */
class CalendarSyncOrchestrator
{

    protected DateTime $syncTime;

    public function __construct(
        protected readonly CalendarAccountRepository $calendarAccountRepository = new CalendarAccountRepository(),
        protected readonly CalendarAccountValidator $calendarAccountValidator = new CalendarAccountValidator(),
        protected readonly CalendarAccountEventFactory $eventFactory = new CalendarAccountEventFactory(),
        protected readonly CalendarEventQueryFactory $eventQueryFactory = new CalendarEventQueryFactory(),
        protected readonly CalendarProviderRegistry $providerRegistry = new CalendarProviderRegistry(),
        protected readonly CalendarSyncConfig $config = new CalendarSyncConfig(),
        protected readonly CalendarSyncJobFactory $jobFactory = new CalendarSyncJobFactory(),
        protected readonly CalendarSyncJobManager $jobManager = new CalendarSyncJobManager(),
        protected readonly CalendarSyncOperationDiscovery $operationDiscovery = new CalendarSyncOperationDiscovery(),
    ) {
        $this->syncTime = new DateTime();
    }

    /**
     * Synchronizes all calendar accounts by creating sync jobs as needed.
     *
     * This method processes all available calendar accounts and attempts to create
     * synchronization jobs for each account, subject to a configured limit on the
     * maximum number of running sync jobs. If the limit is reached, additional job
     * creation stops. Logs are generated to give details about the processing
     * progress and any errors encountered.
     *
     * @param bool $async Determines if the sync operation should be processed asynchronously.
     *                    If set to true, jobs are created for asynchronous execution.
     *
     * @return void
     */
    public function syncAllCalendarAccounts(bool $async = false): void
    {
        $GLOBALS['log']->debug("[CalendarSyncOrchestrator][syncAllCalendarAccounts] Starting synchronization of all calendar accounts (async: " . ($async ? 'true' : 'false') . ")");

        $maxAccountsPerSync = $this->config->getMaxAccountsPerSync();

        $calendarAccounts = $this->getCalendarAccounts($maxAccountsPerSync);
        $discoveredCalendarAccounts = count($calendarAccounts);

        $GLOBALS['log']->debug("[CalendarSyncOrchestrator][syncAllCalendarAccounts] Processing $discoveredCalendarAccounts calendar accounts for sync job creation");

        $calendarAccountSyncJobsCount = 0;
        foreach ($calendarAccounts as $calendarAccount) {
            try {
                $calendarAccountSync = $this->syncCalendarAccount($calendarAccount, $async);
            } catch (Throwable $e) {
                $GLOBALS['log']->warn("[CalendarSyncOrchestrator][syncAllCalendarAccounts] Failed to create job for account $calendarAccount->id ($calendarAccount->name): " . $e->getMessage());
                continue;
            }

            if (!$calendarAccountSync) {
                continue;
            }

            $calendarAccountSyncJobsCount++;
        }

        $GLOBALS['log']->info("[CalendarSyncOrchestrator][syncAllCalendarAccounts] Calendar sync scheduler completed. Discovered $discoveredCalendarAccounts eligible accounts, $calendarAccountSyncJobsCount jobs currently active (limit: $maxAccountsPerSync)");
    }

    /**
     * Retrieves and validates a list of calendar accounts available for synchronization.
     *
     * This method queries the database to fetch calendar accounts while ensuring that
     * only valid accounts are returned after passing through a validation process. If
     * no accounts are found or an error occurs during the retrieval process, an empty
     * array is returned. Logging is performed to detail the operation results and any
     * encountered issues.
     *
     * @param int|null $limit Maximum number of accounts to return, or null for no limit
     * @return CalendarAccount[] An array of validated calendar accounts eligible for synchronization.
     */
    protected function getCalendarAccounts(?int $limit = null): array
    {
        $GLOBALS['log']->debug('[CalendarSyncOrchestrator][getCalendarAccounts] Starting calendar accounts retrieval and validation');

        $validatedAccounts = $this->calendarAccountRepository->getValidatedAccountsBatch($limit);

        if (empty($validatedAccounts)) {
            $GLOBALS['log']->debug('[CalendarSyncOrchestrator][getCalendarAccounts] No valid calendar accounts found');
            return [];
        }

        $GLOBALS['log']->debug("[CalendarSyncOrchestrator][getCalendarAccounts] Found " . count($validatedAccounts) . " validated calendar accounts able to sync");
        return $validatedAccounts;
    }

    /**
     * Synchronizes a specific calendar account, processing its events for synchronization.
     *
     * This method attempts to synchronize a given calendar account by identifying and processing
     * the necessary operations, either asynchronously or synchronously based on the parameters.
     * If executed asynchronously, it ensures that duplicate jobs for the same account are not created.
     * Logs detail the progress of synchronization, including any errors encountered.
     *
     * @param CalendarAccount $calendarAccount The calendar account object to synchronize.
     * @param bool|null $async Determines if the synchronization should be processed asynchronously.
     *                         If set to true, jobs are created for asynchronous execution.
     *                         Defaults to false.
     *
     * @return bool Returns true if synchronization is partially or fully completed successfully,
     *              or if asynchronous job creation is successful. Returns false if there are no
     *              events to synchronize.
     * @throws Throwable
     */
    public function syncCalendarAccount(CalendarAccount $calendarAccount, ?bool $async = false): bool
    {
        $calendarAccountId = $calendarAccount->id;
        $calendarAccountName = $calendarAccount->name;

        $GLOBALS['log']->debug("[CalendarSyncOrchestrator][syncCalendarAccount] Starting sync for account: $calendarAccountId ($calendarAccountName), async: " . ($async ? 'true' : 'false'));

        if ($async) {
            return $this->createAsyncAccountJob($calendarAccount);
        }

        $this->updateLastSyncAttemptDate($calendarAccount);

        try {
            [
                'externalProvider' => $externalProvider,
                'internalProvider' => $internalProvider,
                'query' => $query,
                'userId' => $userId,
                'accountId' => $accountId,
            ] = $this->prepareProvidersAndQuery($calendarAccount);

            [
                'external' => $externalEvents,
                'internal' => $internalEvents
            ] = $this->fetchAndEnrichEvents(
                $externalProvider,
                $internalProvider,
                $query,
                $accountId
            );

            $meetingAsync = $this->config->getRunAsyncValue();

            [
                'totalOperations' => $totalOperations,
                'executedOperations' => $executedOperations,
                'failedOperations' => $failedOperations,
            ] = $this->discoverAndExecuteOperations(
                $externalEvents,
                $internalEvents,
                $externalProvider,
                $internalProvider,
                $userId,
                $accountId,
                $meetingAsync
            );

            $this->finalizeSyncAccount($calendarAccount, $totalOperations, $executedOperations, $failedOperations);

            return true;
        } catch (Throwable $e) {
            $GLOBALS['log']->error("[CalendarSyncOrchestrator][syncCalendarAccount] Sync failed for account $calendarAccountId: " . $e->getMessage());
            $this->updateSyncAttemptError($calendarAccount);
            throw $e;
        }
    }

    /**
     * Create async job for calendar account sync
     *
     * @param CalendarAccount $calendarAccount Account to sync
     * @return bool True if job created successfully
     * @throws RuntimeException If job creation fails
     */
    protected function createAsyncAccountJob(CalendarAccount $calendarAccount): bool
    {
        $calendarAccountId = $calendarAccount->id;
        $calendarAccountName = $calendarAccount->name;

        if ($this->jobManager->accountJobIsActive($calendarAccountId)) {
            $GLOBALS['log']->debug("[CalendarSyncOrchestrator][createAsyncAccountJob] Skipping duplicate job for account: $calendarAccountId ($calendarAccountName) - job already active");
            return true;
        }

        $jobId = $this->jobFactory->createAccountJob($calendarAccountId);

        if (!$jobId) {
            $GLOBALS['log']->warn("[CalendarSyncOrchestrator][createAsyncAccountJob] Failed to create job for account $calendarAccountId ($calendarAccountName)");
            throw new RuntimeException("Failed to create job for account $calendarAccountId ($calendarAccountName)");
        }

        $GLOBALS['log']->debug("[CalendarSyncOrchestrator][createAsyncAccountJob] Created account job $jobId for calendar account $calendarAccountId ($calendarAccountName)");

        return true;
    }

    /**
     * Prepare providers, sync query, and context data
     *
     * @param CalendarAccount $calendarAccount Account to prepare for
     * @return array{
     *     externalProvider: AbstractCalendarProvider,
     *     internalProvider: AbstractCalendarProvider,
     *     query: CalendarEventQuery,
     *     userId: string,
     *     accountId: string,
     * }
     * @throws RuntimeException If provider not found
     */
    protected function prepareProvidersAndQuery(CalendarAccount $calendarAccount): array
    {
        $userId = $calendarAccount->calendar_user_id;
        $accountId = $calendarAccount->id;
        $accountName = $calendarAccount->name;

        $GLOBALS['log']->info("[CalendarSyncOrchestrator][prepareProvidersAndQuery] Processing calendar account sync for account: $accountId ($accountName), user: $userId");

        $provider = $this->providerRegistry->getProviderForAccount($calendarAccount);
        $internalProvider = $this->providerRegistry->getInternalProviderForAccount($calendarAccount);

        if (!$provider) {
            $errorMessage = "Calendar provider not found for account: $accountId";
            $GLOBALS['log']->error("[CalendarSyncOrchestrator][prepareProvidersAndQuery] $errorMessage");
            throw new RuntimeException($errorMessage);
        }

        $GLOBALS['log']->debug("[CalendarSyncOrchestrator][prepareProvidersAndQuery] Retrieved providers for account $accountId - External: " . get_class($provider) . ", Internal: " . get_class($internalProvider));

        $syncWindowPastDays = $this->config->getSyncWindowPastDays();
        $syncWindowFutureDays = $this->config->getSyncWindowFutureDays();
        $calendarEventQuery = $this->eventQueryFactory->forSyncWindows($syncWindowPastDays, $syncWindowFutureDays);

        $GLOBALS['log']->debug("[CalendarSyncOrchestrator][prepareProvidersAndQuery] Using sync window: past $syncWindowPastDays days, future $syncWindowFutureDays days for account $accountId");

        return [
            'externalProvider' => $provider,
            'internalProvider' => $internalProvider,
            'query' => $calendarEventQuery,
            'userId' => $userId,
            'accountId' => $accountId,
        ];
    }

    /**
     * Fetch events within sync window and enrich with linked events outside window
     *
     * @param AbstractCalendarProvider $externalProvider External calendar provider
     * @param AbstractCalendarProvider $internalProvider Internal calendar provider
     * @param CalendarEventQuery $query Sync window query
     * @param string $accountId Account ID for logging
     * @return array{external: CalendarAccountEvent[], internal: CalendarAccountEvent[]}
     * @throws RuntimeException If event fetching fails
     */
    protected function fetchAndEnrichEvents(
        AbstractCalendarProvider $externalProvider,
        AbstractCalendarProvider $internalProvider,
        CalendarEventQuery $query,
        string $accountId
    ): array {
        $GLOBALS['log']->debug("[CalendarSyncOrchestrator][fetchAndEnrichEvents] Retrieving external events for account $accountId");
        try {
            $externalEvents = $externalProvider->getEvents($query);
        } catch (Throwable $e) {
            $errorMessage = "Failed to retrieve external events for account $accountId: " . $e->getMessage();
            $GLOBALS['log']->error("[CalendarSyncOrchestrator][fetchAndEnrichEvents] $errorMessage");
            throw new RuntimeException($errorMessage, 0, $e);
        }

        $GLOBALS['log']->debug("[CalendarSyncOrchestrator][fetchAndEnrichEvents] Retrieving internal events for account $accountId");
        try {
            $internalEvents = $internalProvider->getEvents($query);
        } catch (Throwable $e) {
            $errorMessage = "Failed to retrieve internal events for account $accountId: " . $e->getMessage();
            $GLOBALS['log']->error("[CalendarSyncOrchestrator][fetchAndEnrichEvents] $errorMessage");
            throw new RuntimeException($errorMessage, 0, $e);
        }

        $externalEventsCount = count($externalEvents);
        $internalEventsCount = count($internalEvents);
        $GLOBALS['log']->debug("[CalendarSyncOrchestrator][fetchAndEnrichEvents] Retrieved events for account $accountId - External: $externalEventsCount, Internal: $internalEventsCount");

        $externalEventsLookup = array_flip(array_map(static fn($e) => $e->getId(), $externalEvents));
        $internalEventsLookup = array_flip(array_map(static fn($e) => $e->getId(), $internalEvents));

        $missingExternalIds = array_diff_key(
            array_flip(array_filter(array_map(static fn($e) => $e->getLinkedEventId(), $internalEvents))),
            $externalEventsLookup
        );

        $missingInternalIds = array_diff_key(
            array_flip(array_filter(array_map(static fn($e) => $e->getLinkedEventId(), $externalEvents))),
            $internalEventsLookup
        );

        if (!empty($missingExternalIds)) {
            $missingExternalIdsList = array_keys($missingExternalIds);
            $missingCount = count($missingExternalIdsList);
            $GLOBALS['log']->debug("[CalendarSyncOrchestrator][fetchAndEnrichEvents] Found $missingCount linked external events outside sync window, fetching them for account $accountId");

            foreach ($missingExternalIdsList as $missingId) {
                try {
                    $missingEvent = $externalProvider->getEvent($missingId);
                    if ($missingEvent) {
                        $externalEvents[] = $missingEvent;
                        $GLOBALS['log']->debug("[CalendarSyncOrchestrator][fetchAndEnrichEvents] Successfully fetched missing external event $missingId");
                    }
                } catch (Throwable $e) {
                    $GLOBALS['log']->debug("[CalendarSyncOrchestrator][fetchAndEnrichEvents] Could not fetch missing external event $missingId: " . $e->getMessage());
                }
            }
        }

        if (!empty($missingInternalIds)) {
            $missingInternalIdsList = array_keys($missingInternalIds);
            $missingCount = count($missingInternalIdsList);
            $GLOBALS['log']->debug("[CalendarSyncOrchestrator][fetchAndEnrichEvents] Found $missingCount linked internal events outside sync window, fetching them for account $accountId");

            foreach ($missingInternalIdsList as $missingId) {
                try {
                    $missingEvent = $internalProvider->getEvent($missingId);
                    if ($missingEvent) {
                        $internalEvents[] = $missingEvent;
                        $GLOBALS['log']->debug("[CalendarSyncOrchestrator][fetchAndEnrichEvents] Successfully fetched missing internal event $missingId");
                    }
                } catch (Throwable $e) {
                    $GLOBALS['log']->debug("[CalendarSyncOrchestrator][fetchAndEnrichEvents] Could not fetch missing internal event $missingId: " . $e->getMessage());
                }
            }
        }

        $externalEventsCount = count($externalEvents);
        $internalEventsCount = count($internalEvents);
        $GLOBALS['log']->debug("[CalendarSyncOrchestrator][fetchAndEnrichEvents] Total events after enrichment for account $accountId - External: $externalEventsCount, Internal: $internalEventsCount");

        return [
            'external' => $externalEvents,
            'internal' => $internalEvents
        ];
    }

    /**
     * Discover sync operations and execute them with throttling
     *
     * @param array $externalEvents External calendar events
     * @param array $internalEvents Internal calendar events
     * @param AbstractCalendarProvider $externalProvider External provider
     * @param AbstractCalendarProvider $internalProvider Internal provider
     * @param string $userId User ID
     * @param string $accountId Account ID for logging
     * @param bool $async Whether to run operations asynchronously
     * @return array{totalOperations: int, executedOperations: int, failedOperations: int}
     * @throws RuntimeException If operation discovery fails
     */
    protected function discoverAndExecuteOperations(
        array $externalEvents,
        array $internalEvents,
        AbstractCalendarProvider $externalProvider,
        AbstractCalendarProvider $internalProvider,
        string $userId,
        string $accountId,
        bool $async
    ): array {
        $allowExternalEventDeletion = $this->config->allowExternalEventDeletion();
        $allowInternalEventDeletion = $this->config->allowInternalEventDeletion();

        $GLOBALS['log']->debug("[CalendarSyncOrchestrator][discoverAndExecuteOperations] Deletion settings for account $accountId - External: " . ($allowExternalEventDeletion ? 'allowed' : 'disabled') . ", Internal: " . ($allowInternalEventDeletion ? 'allowed' : 'disabled'));

        try {
            $allOperations = array_merge(
                $this->operationDiscovery->discoverSyncOperations(
                    sourceEvents: $externalEvents,
                    targetEvents: $internalEvents,
                    targetLocation: CalendarLocation::INTERNAL,
                    allowDeletion: $allowExternalEventDeletion,
                    userId: $userId,
                    calendarAccountId: $accountId,
                    targetProvider: $internalProvider
                ),
                $this->operationDiscovery->discoverSyncOperations(
                    sourceEvents: $internalEvents,
                    targetEvents: $externalEvents,
                    targetLocation: CalendarLocation::EXTERNAL,
                    allowDeletion: $allowInternalEventDeletion,
                    userId: $userId,
                    calendarAccountId: $accountId,
                    targetProvider: $externalProvider
                )
            );
        } catch (Throwable $e) {
            $errorMessage = "Failed to discover sync operations for account $accountId: " . $e->getMessage();
            $GLOBALS['log']->error("[CalendarSyncOrchestrator][discoverAndExecuteOperations] $errorMessage");
            throw new RuntimeException($errorMessage, 0, $e);
        }

        $operationsCount = count($allOperations);
        $maxOperationsPerAccount = $this->config->getMaxOperationsPerAccount();

        $eventSyncJobsCount = 0;
        $failedOperationsCount = 0;
        foreach ($allOperations as $operation) {
            if (!$operation instanceof CalendarSyncOperation) {
                continue;
            }

            if ($eventSyncJobsCount >= $maxOperationsPerAccount) {
                $GLOBALS['log']->debug("[CalendarSyncOrchestrator][discoverAndExecuteOperations] Stopping meeting job creation - limits reached (pending: $eventSyncJobsCount/$maxOperationsPerAccount) for account $accountId");
                break;
            }

            try {
                $meetingSync = $this->syncEvent($operation, $async);
            } catch (Throwable $e) {
                $GLOBALS['log']->warn("[CalendarSyncOrchestrator][discoverAndExecuteOperations] Failed to create meeting job for operation: " . $operation->getSubjectId() . " - " . $e->getMessage());
                $failedOperationsCount++;
                continue;
            }

            if (!$meetingSync) {
                $failedOperationsCount++;
                continue;
            }

            $eventSyncJobsCount++;
        }

        return [
            'totalOperations' => $operationsCount,
            'executedOperations' => $eventSyncJobsCount,
            'failedOperations' => $failedOperationsCount,
        ];
    }

    /**
     * Finalize sync operation with logging and state update
     *
     * @param CalendarAccount $calendarAccount Account that was synced
     * @param int $totalOperations Total operations discovered
     * @param int $executedOperations Operations actually executed
     * @param int $failedOperations Operations that failed
     * @return void
     */
    protected function finalizeSyncAccount(
        CalendarAccount $calendarAccount,
        int $totalOperations,
        int $executedOperations,
        int $failedOperations = 0
    ): void {
        $accountId = $calendarAccount->id;
        $maxOperationsPerAccount = $this->config->getMaxOperationsPerAccount();

        $GLOBALS['log']->info("[CalendarSyncOrchestrator][finalizeSyncAccount] Calendar account sync completed for account $accountId. Discovered $totalOperations sync opportunities, $executedOperations jobs currently active (limit: $maxOperationsPerAccount), $failedOperations failed");

        $this->updateLastSyncDate($calendarAccount);
        $this->updateSyncAttemptResult($calendarAccount, $totalOperations, $executedOperations, $failedOperations);
    }

    /**
     * Synchronizes a specific calendar event based on the provided operation details.
     *
     * This method handles various operations (CREATE, UPDATE, DELETE) for a given
     * calendar event, allowing synchronization between internal and external calendar
     * providers. If asynchronous execution is requested, a job is created and queued
     * for later execution. Synchronous execution processes the event immediately.
     *
     * @param CalendarSyncOperation $operation The operation object containing details about the event
     *                                         to be synchronized, including its type and target data.
     * @param bool|null $async Optional. Determines if the sync operation should be processed asynchronously.
     *                         Defaults to false if not specified.
     *
     * @return bool Returns true if the sync operation is successfully processed or queued;
     *              false if the operation fails.
     */
    public function syncEvent(CalendarSyncOperation $operation, ?bool $async = false): bool
    {
        $targetId = $operation->getSubjectId();
        $action = $operation->getAction();
        $actionName = $action->value;
        $location = $operation->getLocation();
        $locationName = $location->value;

        $GLOBALS['log']->debug("[CalendarSyncOrchestrator][syncEvent] Starting event sync - Target: $targetId, Action: $actionName, Location: $locationName, Async: " . ($async ? 'true' : 'false'));

        if ($async) {
            if ($this->jobManager->meetingJobIsActive($operation)) {
                $GLOBALS['log']->debug("[CalendarSyncOrchestrator][syncEvent] Skipping duplicate job for operation: " . $operation->getSubjectId() . " - job already active");
                return true;
            }

            $jobId = $this->jobFactory->createMeetingJob($operation);
            if (!$jobId) {
                $GLOBALS['log']->error('[CalendarSyncOrchestrator][syncEvent] Failed to create meeting job for subject ID: ' . $operation->getSubjectId());
                throw new RuntimeException('Failed to create meeting job');
            }

            $GLOBALS['log']->info('[CalendarSyncOrchestrator][syncEvent] Successfully created meeting job with ID: ' . $jobId . ' for subject ID: ' . $operation->getSubjectId());
            return true;
        }

        $calendarAccountId = $operation->getCalendarAccountId();
        $GLOBALS['log']->info("[CalendarSyncOrchestrator][syncEvent] Processing meeting calendar sync for account: $calendarAccountId - Event: $targetId, Action: $actionName, Location: $locationName");

        try {
            if (!$calendarAccountId) {
                $GLOBALS['log']->error("[CalendarSyncOrchestrator][syncEvent] No calendar account ID found in operation");
                return false;
            }

            $calendarAccount = $this->calendarAccountValidator->validateCalendarAccount($calendarAccountId);

            $externalProvider = $this->providerRegistry->getProviderForAccount($calendarAccount);
            if ($externalProvider === null) {
                $GLOBALS['log']->error("[CalendarSyncOrchestrator][syncEvent] External calendar provider not found for account: $calendarAccountId");
                return false;
            }
            $internalProvider = $this->providerRegistry->getInternalProviderForAccount($calendarAccount);

            $GLOBALS['log']->debug("[CalendarSyncOrchestrator][syncEvent] Retrieved providers for account $calendarAccountId - External: " . get_class($externalProvider) . ", Internal: " . get_class($internalProvider));

            $targetProvider = $location === CalendarLocation::INTERNAL ? $internalProvider : $externalProvider;
            $sourceProvider = $location === CalendarLocation::INTERNAL ? $externalProvider : $internalProvider;

            $sourceEvent = $operation->getPayload();

            $GLOBALS['log']->debug("[CalendarSyncOrchestrator][syncEvent] Executing $actionName operation for event $targetId at $locationName");

            switch ($action) {
                case CalendarSyncAction::CREATE:
                    if (!$sourceEvent) {
                        $GLOBALS['log']->error("[CalendarSyncOrchestrator][syncEvent] No source event found for CREATE operation: $targetId");
                        return false;
                    }

                    $createdEventId = $targetProvider->createEventFromSource($sourceEvent, $this->syncTime);
                    $GLOBALS['log']->info("[CalendarSyncOrchestrator][syncEvent] Created event at $locationName: new_event=$createdEventId");

                    try {
                        $sourceProvider->updateSourceEvent($createdEventId, $sourceEvent, $this->syncTime);
                        $GLOBALS['log']->debug("[CalendarSyncOrchestrator][syncEvent] Updated source event linking at $locationName: linked_to=$createdEventId");
                    } catch (Throwable $linkingError) {
                        $GLOBALS['log']->warn("[CalendarSyncOrchestrator][syncEvent] Created event but failed to update source linking at $locationName: " . $linkingError->getMessage());
                    }
                    break;

                case CalendarSyncAction::UPDATE:
                    if (!$sourceEvent) {
                        $GLOBALS['log']->error("[CalendarSyncOrchestrator][syncEvent] No source event found for UPDATE operation: $targetId");
                        return false;
                    }

                    $targetProvider->updateEventFromSource($targetId, $sourceEvent, $this->syncTime);
                    $GLOBALS['log']->info("[CalendarSyncOrchestrator][syncEvent] Updated target event at $locationName: target_id=$targetId");

                    try {
                        $sourceProvider->updateSourceEvent($targetId, $sourceEvent, $this->syncTime);
                        $GLOBALS['log']->debug("[CalendarSyncOrchestrator][syncEvent] Updated source event sync timestamp at $locationName");
                    } catch (Throwable $linkingError) {
                        $GLOBALS['log']->warn("[CalendarSyncOrchestrator][syncEvent] Updated target event but failed to update source sync timestamp at $locationName: " . $linkingError->getMessage());
                    }
                    break;

                case CalendarSyncAction::DELETE:
                    $targetProvider->deleteEvent($targetId);
                    $GLOBALS['log']->info("[CalendarSyncOrchestrator][syncEvent] Deleted event at $locationName: target_id=$targetId");
                    break;
            }

            $GLOBALS['log']->info("[CalendarSyncOrchestrator][syncEvent] Successfully executed $action->value operation at $locationName for account: $calendarAccountId, event: $targetId");
            return true;

        } catch (Throwable $e) {
            $GLOBALS['log']->error("[CalendarSyncOrchestrator][syncEvent] Meeting calendar sync failed for account: $calendarAccountId - Event: $targetId, Action: $actionName, Error: " . $e->getMessage());
            return false;
        }
    }

    protected function updateLastSyncDate(CalendarAccount $calendarAccount): void
    {
        global $timedate;

        $accountId = $calendarAccount->id;
        $GLOBALS['log']->debug("[CalendarSyncOrchestrator][updateLastSyncDate] Updating last sync date for account $accountId");

        try {
            $calendarAccount->updateSyncMetadata([
                'last_sync_date' => $timedate->asDb($this->syncTime)
            ]);
            $GLOBALS['log']->info("[CalendarSyncOrchestrator][updateLastSyncDate] Updated last sync date for account $accountId");
        } catch (Throwable $e) {
            $GLOBALS['log']->warn("[CalendarSyncOrchestrator][updateLastSyncDate] Failed to update last sync date for account $accountId: " . $e->getMessage());
        }
    }

    protected function updateLastSyncAttemptDate(CalendarAccount $calendarAccount): void
    {
        global $timedate;

        $accountId = $calendarAccount->id;
        $GLOBALS['log']->debug("[CalendarSyncOrchestrator][updateLastSyncAttemptDate] Updating last sync attempt date for account $accountId");

        try {
            $calendarAccount->updateSyncMetadata([
                'last_sync_attempt_date' => $timedate->asDb($this->syncTime),
                'last_sync_attempt_status' => 'in_progress',
                'last_sync_attempt_message' => '',
            ]);
            $GLOBALS['log']->info("[CalendarSyncOrchestrator][updateLastSyncAttemptDate] Updated last sync attempt date for account $accountId");
        } catch (Throwable $e) {
            $GLOBALS['log']->warn("[CalendarSyncOrchestrator][updateLastSyncAttemptDate] Failed to update last sync attempt date for account $accountId: " . $e->getMessage());
        }
    }

    protected function updateSyncAttemptResult(
        CalendarAccount $calendarAccount,
        int $totalOperations,
        int $executedOperations,
        int $failedOperations
    ): void {
        $accountId = $calendarAccount->id;
        $processedOperations = $executedOperations + $failedOperations;

        if ($failedOperations > 0) {
            $status = 'warning';
            $message = 'meetings_failed';
            $GLOBALS['log']->debug("[CalendarSyncOrchestrator][updateSyncAttemptResult] Setting warning status for account $accountId: $failedOperations failed operations");
        } elseif ($totalOperations > $processedOperations) {
            $status = 'success';
            $message = 'sync_partial';
            $GLOBALS['log']->debug("[CalendarSyncOrchestrator][updateSyncAttemptResult] Setting success status for account $accountId: partial sync ($processedOperations/$totalOperations)");
        } elseif ($totalOperations === 0) {
            $status = 'success';
            $message = 'up_to_date';
            $GLOBALS['log']->debug("[CalendarSyncOrchestrator][updateSyncAttemptResult] Setting success status for account $accountId: already up to date");
        } else {
            $status = 'success';
            $message = 'sync_complete';
            $GLOBALS['log']->debug("[CalendarSyncOrchestrator][updateSyncAttemptResult] Setting success status for account $accountId: all synced");
        }

        try {
            $calendarAccount->updateSyncMetadata([
                'last_sync_attempt_status' => $status,
                'last_sync_attempt_message' => $message,
            ]);
            $GLOBALS['log']->info("[CalendarSyncOrchestrator][updateSyncAttemptResult] Updated sync attempt result for account $accountId: $status");
        } catch (Throwable $e) {
            $GLOBALS['log']->warn("[CalendarSyncOrchestrator][updateSyncAttemptResult] Failed to update sync attempt result for account $accountId: " . $e->getMessage());
        }
    }

    protected function updateSyncAttemptError(CalendarAccount $calendarAccount): void
    {
        $accountId = $calendarAccount->id;
        $GLOBALS['log']->debug("[CalendarSyncOrchestrator][updateSyncAttemptError] Setting error status for account $accountId");

        try {
            $calendarAccount->updateSyncMetadata([
                'last_sync_attempt_status' => 'error',
                'last_sync_attempt_message' => 'sync_failed',
            ]);
            $GLOBALS['log']->info("[CalendarSyncOrchestrator][updateSyncAttemptError] Updated sync attempt error for account $accountId");
        } catch (Throwable $e) {
            $GLOBALS['log']->warn("[CalendarSyncOrchestrator][updateSyncAttemptError] Failed to update sync attempt error for account $accountId: " . $e->getMessage());
        }
    }

}