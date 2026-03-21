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
require_once 'include/CalendarSync/application/CalendarSyncOrchestrator.php';
require_once 'include/CalendarSync/CalendarSyncInterface.php';
require_once 'include/CalendarSync/domain/entities/CalendarAccountEvent.php';
require_once 'include/CalendarSync/domain/entities/CalendarSyncOperation.php';
require_once 'include/CalendarSync/domain/services/factories/CalendarAccountEventFactory.php';
require_once 'include/CalendarSync/domain/services/CalendarSyncOperationSerializer.php';
require_once 'include/CalendarSync/infrastructure/jobs/CalendarSyncJobCleaner.php';
require_once 'include/CalendarSync/infrastructure/jobs/CalendarSyncJobFactory.php';
require_once 'include/CalendarSync/infrastructure/providers/AbstractCalendarProvider.php';
require_once 'include/CalendarSync/infrastructure/registry/CalendarProviderRegistry.php';
require_once 'include/CalendarSync/domain/valueObjects/CalendarConnectionTestResult.php';

/**
 * CalendarSync Facade
 *
 * Main entry point for calendar synchronization functionality.
 * Provides instantiable interface for external integrations while delegating
 * to properly structured internal layers.
 *
 * @see tests/unit/phpunit/include/CalendarSync/CalendarSyncTest.php
 */
class CalendarSync implements CalendarSyncInterface
{

    /**
     * Singleton instance
     * @var CalendarSync|null
     */
    protected static ?CalendarSync $instance = null;

    protected function __construct(
        protected readonly CalendarAccountEventFactory $accountEventFactory = new CalendarAccountEventFactory(),
        protected readonly CalendarAccountRepository $calendarAccountRepository = new CalendarAccountRepository(),
        protected readonly CalendarAccountValidator $calendarAccountValidator = new CalendarAccountValidator(),
        protected readonly CalendarProviderRegistry $providerRegistry = new CalendarProviderRegistry(),
        protected readonly CalendarSyncConfig $config = new CalendarSyncConfig(),
        protected readonly CalendarSyncJobCleaner $jobCleaner = new CalendarSyncJobCleaner(),
        protected readonly CalendarSyncJobFactory $jobFactory = new CalendarSyncJobFactory(),
        protected readonly CalendarSyncOperationDiscovery $operationDiscovery = new CalendarSyncOperationDiscovery(),
        protected readonly CalendarSyncOperationSerializer $operationSerializer = new CalendarSyncOperationSerializer(),
        protected readonly CalendarSyncOrchestrator $orchestrator = new CalendarSyncOrchestrator(),
    ) {
    }

    public static function getInstance(): CalendarSync
    {
        if (self::$instance === null) {
            $GLOBALS['log']->debug('[CalendarSync][getInstance] Creating new CalendarSync instance');
            self::$instance = new self();
        }
        return self::$instance;
    }

    /** @inheritDoc */
    public function syncAllCalendarAccounts(bool $isManualRun = false): bool
    {
        $runType = $isManualRun ? 'manual' : 'scheduled';
        $GLOBALS['log']->debug('[CalendarSync][syncAllCalendarAccounts] Method started - Run type: ' . $runType);

        try {
            $GLOBALS['log']->info('[CalendarSync][syncAllCalendarAccounts] Starting calendar sync ' . $runType . ' job');

            $runAsync = $this->config->getRunAsyncValue();

            $this->orchestrator->syncAllCalendarAccounts($runAsync);

            if ($isManualRun) {
                $currentTimestamp = $GLOBALS['timedate']->nowDb();
                $this->config->setLastManualRunTime($currentTimestamp);
            }

            $GLOBALS['log']->info('[CalendarSync][syncAllCalendarAccounts] Calendar sync ' . $runType . ' job completed successfully');
            return true;
        } catch (Throwable $e) {
            $GLOBALS['log']->error('[CalendarSync][syncAllCalendarAccounts] Failed to schedule calendar sync for all users: ' . $e->getMessage());
            return false;
        }
    }

    /** @inheritDoc */
    public function syncAllMeetingsOfCalendarAccount(string $calendarAccountId): void
    {
        $GLOBALS['log']->debug('[CalendarSync][syncAllMeetingsOfCalendarAccount] Method started with calendarAccountId: ' . $calendarAccountId);

        try {
            $calendarAccount = $this->calendarAccountValidator->validateCalendarAccount($calendarAccountId);

            $accountName = $calendarAccount->name;
            $userId = $calendarAccount->calendar_user_id;

            $GLOBALS['log']->info('[CalendarSync][syncAllMeetingsOfCalendarAccount] Starting sync all meetings for calendar account: ' . $calendarAccountId . ' (' . $accountName . ') - User: ' . $userId);

            $this->orchestrator->syncCalendarAccount($calendarAccount);

            $GLOBALS['log']->info('[CalendarSync][syncAllMeetingsOfCalendarAccount] Calendar account sync completed for account: ' . $calendarAccountId);
        } catch (Throwable $e) {
            $GLOBALS['log']->error('[CalendarSync][syncAllMeetingsOfCalendarAccount] Calendar account sync failed for account: ' . $calendarAccountId . ': ' . $e->getMessage());
            throw new RuntimeException($e->getMessage(), 0, $e);
        }
    }

    /** @inheritDoc */
    public function syncMeeting(Meeting $bean): void
    {
        require_once 'include/CalendarSync/domain/enums/CalendarSyncAction.php';

        $meetingId = $bean->id ?? 'unknown';
        $GLOBALS['log']->debug('[CalendarSync][syncMeeting] Method started for meeting: ' . $meetingId);

        if (!$this->config->enableCalendarSyncLogicHooks()) {
            $GLOBALS['log']->debug('[CalendarSync][syncMeeting] Calendar sync logic hooks are disabled, skipping sync for meeting: ' . $meetingId);
            return;
        }

        if ($bean->deleted === 1) {
            $action = CalendarSyncAction::DELETE;
        } elseif (empty($bean->fetched_row)) {
            $action = CalendarSyncAction::CREATE;
        } else {
            $action = CalendarSyncAction::UPDATE;
        }

        $actionString = $action->value;
        $meetingId = $bean->id;

        $userId = $bean->assigned_user_id;
        if (empty($userId)) {
            $GLOBALS['log']->debug('[CalendarSync][syncMeeting] No assigned user for meeting: ' . $meetingId . ', skipping sync');
            return;
        }

        $calendarAccount = $this->getActiveCalendarAccountForUser($userId);
        if (!$calendarAccount) {
            $GLOBALS['log']->debug('[CalendarSync][syncMeeting] No active calendar account found for user: ' . $userId . ', meeting: ' . $meetingId);
            return;
        }

        $sourceEvent = $this->accountEventFactory->fromMeetingBean($bean, $calendarAccount);

        $targetEventId = $sourceEvent->getLinkedEventId();
        $hasTargetEvent = !empty($targetEventId);

        $doesNotHaveTargetEvent = $action !== CalendarSyncAction::CREATE && !$hasTargetEvent;
        $skip_calendar_sync = $bean->skip_calendar_sync ?? false;
        if ($doesNotHaveTargetEvent || $skip_calendar_sync) {
            $GLOBALS['log']->debug('[CalendarSync][syncMeeting] Skipping sync for meeting: ' . $meetingId . ' - doesNotHaveTargetEvent: ' . ($doesNotHaveTargetEvent ? 'true' : 'false') . ', skip_calendar_sync: ' . ($skip_calendar_sync ? 'true' : 'false'));
            return;
        }

        $runAsync = $this->config->getRunAsyncValue();

        try {
            $operation = $this->operationDiscovery->createSyncOperation(
                userId: $userId,
                calendarAccountId: $calendarAccount->id,
                action: $action,
                location: CalendarLocation::EXTERNAL,
                targetEventId: $sourceEvent->getLinkedEventId() ?? '',
                payload: $sourceEvent
            );

            $cancelledCount = 0;
            if ($runAsync) {
                $cancelledCount = $this->jobCleaner->cancelPendingMeetingJobs($operation);
            }

            $this->orchestrator->syncEvent($operation, $runAsync);

            if ($cancelledCount > 0) {
                $GLOBALS['log']->info('[CalendarSync][syncMeeting] Cancelled ' . $cancelledCount . ' pending sync jobs for meeting: ' . $meetingId . ' to ensure priority');
            }

            $GLOBALS['log']->debug('[CalendarSync][syncMeeting] Scheduled priority calendar sync job - Meeting: ' . $meetingId . ', Action: ' . $actionString);

        } catch (Throwable $e) {
            $GLOBALS['log']->error('[CalendarSync][syncMeeting] Failed to schedule calendar sync - Meeting: ' . $meetingId . ', Action: ' . $actionString . ', Error: ' . $e->getMessage());
        }
    }

    /**
     * Get the most recent validated personal calendar account for a user
     *
     * @param string $userId The user ID to get the account for
     * @return CalendarAccount|null The most recent validated personal account or null if none found
     */
    protected function getActiveCalendarAccountForUser(string $userId): ?CalendarAccount
    {
        return $this->calendarAccountRepository->getValidatedPersonalCalendarAccountForUser($userId);
    }

    /** @inheritDoc */
    public function getActiveCalendarAccountsForUser(string $userId): array
    {
        if ($userId === '') {
            return [];
        }
        return $this->calendarAccountRepository->getAllValidatedCalendarAccountsForUser($userId);
    }

    /**
     * Checks if a user has a personal calendar account.
     *
     * @param string $userId The ID of the user to check for a personal calendar account.
     * @return bool Returns true if the user has a personal calendar account, otherwise false.
     */
    public function hasPersonalCalendarAccount(string $userId): bool
    {
        return $this->calendarAccountRepository->hasPersonalCalendarAccount($userId);
    }

    /**
     * Retrieves personal calendar accounts associated with the given user ID.
     *
     * @param string $userId The ID of the user whose personal calendar accounts are to be retrieved.
     * @return array An array containing the personal calendar accounts of the specified user.
     */
    public function getPersonalCalendarAccounts(string $userId): array
    {
        return $this->calendarAccountRepository->getPersonalCalendarAccounts($userId);
    }

    /** @inheritDoc */
    public function syncEvent(string $decodedData): bool
    {
        $GLOBALS['log']->debug('[CalendarSync][syncEvent] Method started with data length: ' . strlen($decodedData));

        try {
            $operation = $this->operationSerializer->deserialize($decodedData);

            $result = $this->orchestrator->syncEvent($operation);

            $GLOBALS['log']->debug('[CalendarSync][syncEvent] Sync event result: ' . ($result ? 'success' : 'failure'));

            return $result;
        } catch (Throwable $e) {
            $GLOBALS['log']->error('[CalendarSync][syncEvent] Failed to sync meeting: ' . $e->getMessage() . ' - Data length: ' . strlen($decodedData));
            return false;
        }
    }

    /** @inheritDoc */
    public function getProviderAuthMethodWithValidation(string $source): string
    {
        $GLOBALS['log']->debug('[CalendarSync][getProviderAuthMethodWithValidation] Method started with source: ' . $source);

        if (empty($source)) {
            $GLOBALS['log']->error('[CalendarSync][getProviderAuthMethodWithValidation] Source is required but was empty');
            throw new InvalidArgumentException('Source is required');
        }

        $authMethod = $this->providerRegistry->getAuthMethodForSource($source);

        if ($authMethod === null) {
            $GLOBALS['log']->error('[CalendarSync][getProviderAuthMethodWithValidation] Provider not found or not configured for source: ' . $source);
            throw new RuntimeException('Provider not found or not configured');
        }

        $GLOBALS['log']->debug('[CalendarSync][getProviderAuthMethodWithValidation] Found auth method: ' . $authMethod . ' for source: ' . $source);

        return $authMethod;
    }

    /** @inheritDoc */
    public function testProviderConnectionWithValidation(CalendarAccount $calendarAccount): CalendarConnectionTestResult
    {
        $GLOBALS['log']->debug('[CalendarSync][testProviderConnectionWithValidation] Method started for account: ' . ($calendarAccount->id ?? 'unknown'));

        if (empty($calendarAccount->source)) {
            $GLOBALS['log']->error('[CalendarSync][testProviderConnectionWithValidation] Source is required but was empty for account: ' . ($calendarAccount->id ?? 'unknown'));
            throw new InvalidArgumentException('Source is required');
        }

        $provider = $this->providerRegistry->getProviderForAccount($calendarAccount);
        if (!$provider) {
            $GLOBALS['log']->error('[CalendarSync][testProviderConnectionWithValidation] Provider instance not available for source: ' . $calendarAccount->source);
            throw new RuntimeException('Provider instance not available for source: ' . $calendarAccount->source);
        }

        $GLOBALS['log']->debug('[CalendarSync][testProviderConnectionWithValidation] Testing connection for provider: ' . $calendarAccount->source);

        return $provider->testCalendarConnection();
    }

    /**
     * Finds if an external calendar ID is already in use by another account.
     *
     * @param string $externalCalendarId The external calendar ID to check
     * @param string $accountId The account ID to exclude from the check
     * @return CalendarAccount|null The existing account using this calendar ID, or null if unique
     */
    public function findDuplicateCalendarAccount(string $externalCalendarId, string $accountId): ?CalendarAccount
    {
        $GLOBALS['log']->debug('[CalendarSync][findDuplicateCalendarAccount] Checking externalCalendarId: ' . $externalCalendarId);

        $existingCalendarAccount = $this->calendarAccountRepository->findByExternalCalendarId(
            externalCalendarId: $externalCalendarId,
            excludeAccountId: $accountId
        );

        if ($existingCalendarAccount !== null) {
            $GLOBALS['log']->debug('[CalendarSync][findDuplicateCalendarAccount] Found duplicate: ' . $existingCalendarAccount->id);
        }

        return $existingCalendarAccount;
    }

    /** @inheritDoc */
    public function getFieldsToHide(string $source): array
    {
        $GLOBALS['log']->debug('[CalendarSync][getFieldsToHide] Method started with source: ' . $source);
        $fieldMappings = [
            'oauth2' => ['oauth_connection_name'],
            'basic' => ['username', 'password', 'server_url'],
            'api_key' => ['api_key', 'api_endpoint']
        ];
        $allAuthFields = array_unique(array_merge(...array_values($fieldMappings)));

        $authMethod = $this->providerRegistry->getAuthMethodForSource($source);

        if (!$authMethod || !isset($fieldMappings[$authMethod])) {
            return $allAuthFields;
        }

        return array_diff($allAuthFields, $fieldMappings[$authMethod]);
    }

    /**
     * Get calendar source types array.
     */
    public function getCalendarSourceTypes(): array
    {
        $GLOBALS['log']->debug('[CalendarSync][getCalendarSourceTypes] Method started');
        return $this->providerRegistry->getCalendarSourceTypes();
    }

    /** @inheritDoc */
    public function saveConfig(array $postData): bool
    {
        require_once 'include/CalendarSync/domain/CalendarSyncConfig.php';

        $GLOBALS['log']->debug('[CalendarSync][saveConfig] Method started with ' . count($postData) . ' config values');

        $configValues = [];
        foreach ($this->config->getKeys() as $key) {
            if (!isset($postData[$key])) {
                continue;
            }
            $configValues[$key] = $postData[$key];
        }

        $result = $this->config->set($configValues);
        $GLOBALS['log']->info('[CalendarSync][saveConfig] Configuration saved with result: ' . ($result ? 'success' : 'failure') . ', saved ' . count($configValues) . ' values');
        return $result;
    }

    /** @inheritDoc */
    public function getConfig(): array
    {
        require_once 'include/CalendarSync/domain/CalendarSyncConfig.php';
        $GLOBALS['log']->debug('[CalendarSync][getConfig] Method started');
        return $this->config->getAll();
    }

    /** @inheritDoc */
    public function getConflictResolutionCases(): array
    {
        require_once 'include/CalendarSync/domain/enums/ConflictResolution.php';
        $GLOBALS['log']->debug('[CalendarSync][getConflictResolutionCases] Method started');
        return ConflictResolution::cases();
    }

    /** @inheritDoc */
    public function getScheduler(): ?Scheduler
    {
        $GLOBALS['log']->debug('[CalendarSync][getScheduler] Method started');
        return $this->jobFactory->getScheduler();
    }

    /** @throws RuntimeException */
    public function __wakeup()
    {
        throw new RuntimeException("Cannot unserialize singleton");
    }

    private function __clone()
    {
    }

}