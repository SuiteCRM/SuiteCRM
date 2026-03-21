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
require_once 'include/CalendarSync/domain/enums/CalendarEventType.php';
require_once 'include/CalendarSync/domain/valueObjects/CalendarConnectionTestResult.php';

use Google\Client;
use Google\Service\Calendar;
use Google\Service\Calendar\Event as GoogleEvent;
use Google\Service\Calendar\EventDateTime;
use Google\Service\Calendar\EventExtendedProperties;

/**
 * Provides integration with Google Calendar for SuiteCRM.
 *
 * This class manages OAuth-based authentication, interaction with the Google Calendar API, and synchronization
 * of calendar events between SuiteCRM and Google Calendar. It includes methods for testing the connection,
 * retrieving events, and managing OAuth tokens.
 *
 * Required OAuth scopes: openid, email, profile, https://www.googleapis.com/auth/calendar
 * OAuth endpoints: https://accounts.google.com/o/oauth2/auth (authorize), https://oauth2.googleapis.com/token (token)
 */
class GoogleCalendarProvider extends AbstractCalendarProvider
{

    private ?Client $googleClient = null;
    private ?Calendar $calendarService = null;
    private ?string $suitecrmCalendarId = null;

    /**
     * @inheritdoc
     */
    public function testCalendarConnection(): CalendarConnectionTestResult
    {
        global $log;

        try {
            return $this->testGoogleCalendarConnection();

        } catch (Throwable $e) {
            $log->error("GoogleCalendarProvider: testCalendarConnection error: " . $e->getMessage());
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
     * Tests the connection to the Google Calendar API.
     *
     * @return CalendarConnectionTestResult The result of the connection test, including status and connection details.
     */
    protected function testGoogleCalendarConnection(): CalendarConnectionTestResult
    {
        global $log;

        try {
            $this->initializeGoogleServices();

            $log->info("GoogleCalendarProvider: Successfully connected to Google Calendar");
            $log->info("GoogleCalendarProvider: Will use calendar ID: " . $this->suitecrmCalendarId);

            return new CalendarConnectionTestResult(
                success: true,
                connection: $this->connection,
                authenticationStatus: 'authenticated',
                externalCalendarId: $this->suitecrmCalendarId
            );

        } catch (Throwable $e) {
            $log->error("GoogleCalendarProvider: testGoogleCalendarAPI error: " . $e->getMessage());
            throw new RuntimeException($e->getMessage(), $e->getCode(), $e);
        }
    }

    /**
     * Initialize Google Client and Calendar Service for Google API integration.
     *
     * @return void
     */
    protected function initializeGoogleServices(): void
    {
        if ($this->googleClient !== null && $this->calendarService !== null) {
            return;
        }

        $accessToken = $this->getAuthenticatedAccessToken();

        $this->googleClient = new Client();
        $this->googleClient->setApplicationName('SuiteCRM Calendar Sync');
        $this->googleClient->setScopes([Calendar::CALENDAR]);
        $this->googleClient->setAccessToken($accessToken);

        $this->calendarService = new Calendar($this->googleClient);

        $this->suitecrmCalendarId = $this->getOrCreateSuiteCRMCalendar();
    }

    /**
     * Retrieves an authenticated access token for the current connection.
     *
     * This method ensures the OAuth connection is valid and attempts to refresh
     * the token if it has expired. If the token cannot be automatically refreshed,
     * it marks the connection for manual reauthorization.
     *
     * @return string The authenticated access token.
     *
     * @throws RuntimeException If the OAuth connection is invalid, the token cannot be refreshed, or the access token is unavailable.
     */
    protected function getAuthenticatedAccessToken(): string
    {
        global $log;

        $oauthConnection = $this->getOAuthConnection($this->connection);
        if ($oauthConnection === null || empty($oauthConnection->id)) {
            throw new RuntimeException('OAuth connection not found');
        }

        require_once 'modules/ExternalOAuthConnection/services/OAuthAuthorizationService.php';
        $oAuthService = new OAuthAuthorizationService();

        $tokenStatus = $oAuthService->hasConnectionTokenExpired($oauthConnection);
        if ($tokenStatus['expired'] && $tokenStatus['refreshToken']) {
            $log->info('GoogleCalendarProvider: Token expired, attempting automatic refresh for connection ' . $oauthConnection->id);

            $refreshResult = $oAuthService->refreshConnectionToken($oauthConnection);
            if (!$refreshResult['success']) {
                if ($refreshResult['reLogin']) {
                    $log->warn('GoogleCalendarProvider: Token refresh failed, requires manual reauthorization: ' . $refreshResult['message']);
                    throw new RuntimeException('OAuth token expired and cannot be automatically refreshed. Manual reauthorization required: ' . $refreshResult['message']);
                }

                throw new RuntimeException('Failed to refresh OAuth token: ' . $refreshResult['message']);
            }
            $log->info('GoogleCalendarProvider: Token successfully refreshed for connection ' . $oauthConnection->id);
        }

        $accessToken = $oauthConnection->access_token;
        if (empty($accessToken)) {
            throw new RuntimeException('No access token available');
        }

        return $accessToken;
    }

    /**
     * Retrieve an OAuth connection corresponding to the given calendar account.
     *
     * @param CalendarAccount $calendarAccount The calendar account associated with the OAuth connection.
     * @return ExternalOAuthConnection|null The retrieved OAuth connection if it exists and is valid, or null otherwise.
     */
    protected function getOAuthConnection(CalendarAccount $calendarAccount): ?ExternalOAuthConnection
    {
        $connectionId = $calendarAccount->oauth_connection_id ?? null;

        if (empty($connectionId)) {
            return null;
        }

        $connection = BeanFactory::getBean('ExternalOAuthConnection', $connectionId);
        return $connection instanceof ExternalOAuthConnection ? $connection : null;
    }

    /**
     * Retrieves the ID of the existing "SuiteCRM" calendar or creates a new one if not found.
     * If the calendar already exists, its ID is returned. Otherwise, a new "SuiteCRM"
     * calendar is created, and its ID is returned.
     * Logs information and error messages regarding the operation status.
     *
     * @return string The ID of the "SuiteCRM" calendar.
     * @throws RuntimeException If the operation to retrieve or create the calendar fails.
     */
    protected function getOrCreateSuiteCRMCalendar(): string
    {
        global $log;

        try {
            $calendarList = $this->calendarService->calendarList->listCalendarList();

            $calendarName = $this->getExternalCalendarName();
            foreach ($calendarList->getItems() as $calendar) {
                if ($calendar->getSummary() === $calendarName) {
                    $log->info('GoogleCalendarProvider: Found existing SuiteCRM calendar: ' . $calendar->getId());
                    return $calendar->getId();
                }
            }

            $calendar = new Calendar\Calendar();
            $calendar->setSummary($calendarName);
            $calendar->setDescription('Calendar managed by SuiteCRM');

            $createdCalendar = $this->calendarService->calendars->insert($calendar);
            $calendarId = $createdCalendar->getId();

            $log->info('GoogleCalendarProvider: Created new SuiteCRM calendar: ' . $calendarId);
            return $calendarId;

        } catch (Throwable $e) {
            $log->error('GoogleCalendarProvider: Failed to get or create SuiteCRM calendar: ' . $e->getMessage());
            throw new RuntimeException('Failed to get or create SuiteCRM calendar: ' . $e->getMessage());
        }
    }

    /**
     * @inheritdoc
     */
    public function getEvents(CalendarEventQuery $query): array
    {
        global $log;
        $log->info('GoogleCalendarProvider: getEvents called with query: ' . json_encode($query->toArray()));

        try {
            $this->initializeGoogleServices();

            $optParams = [
                'maxResults' => 500,
                'showDeleted' => false,
                'singleEvents' => true,
                'timeMin' => $query->getStartDate()?->format('Y-m-d\TH:i:s\Z'),
                'timeMax' => $query->getEndDate()?->format('Y-m-d\TH:i:s\Z'),
            ];

            $googleEvents = $this->calendarService->events->listEvents($this->suitecrmCalendarId, $optParams);

            $events = [];
            foreach ($googleEvents->getItems() as $googleEvent) {
                if ($googleEvent->getStatus() !== 'cancelled') {
                    $events[] = $this->convertFromGoogleEvent($googleEvent);
                }
            }

            $log->info('GoogleCalendarProvider: Retrieved ' . count($events) . ' events from Google Calendar');
            return $events;

        } catch (Throwable $e) {
            $log->error('GoogleCalendarProvider: getEvents error: ' . $e->getMessage());
            throw $e;
        }
    }

    /**
     * Converts a GoogleEvent instance into a CalendarAccountEvent instance.
     *
     * @param GoogleEvent $googleEvent The Google event object to be converted.
     * @return CalendarAccountEvent The converted CalendarAccountEvent object.
     * @throws Exception If there is an error during the conversion process.
     */
    protected function convertFromGoogleEvent(GoogleEvent $googleEvent): CalendarAccountEvent
    {
        require_once 'include/CalendarSync/domain/entities/CalendarAccountEvent.php';

        $extendedProperties = $googleEvent->getExtendedProperties();
        $privateProperties = $extendedProperties ? $extendedProperties->getPrivate() : [];
        $linkedEventId = $privateProperties['suitecrm_linked_event_id'] ?? null;
        $lastSyncString = $privateProperties['suitecrm_last_sync_string'] ?? null;
        $userId = $privateProperties['suitecrm_user_id'] ?? $this->connection->calendar_user_id;
        $type = CalendarEventType::tryFrom($privateProperties['suitecrm_event_type'] ?? '') ?? CalendarEventType::MEETING;

        $start = $googleEvent->getStart();
        $dateStart = $start?->getDateTime() ?? $start?->getDate();
        if ($dateStart) {
            $dateStart = new DateTime($dateStart, new DateTimeZone('UTC'));
        }

        $end = $googleEvent->getEnd();
        $dateEnd = $end?->getDateTime() ?? $end?->getDate();
        if ($dateEnd) {
            $dateEnd = new DateTime($dateEnd, new DateTimeZone('UTC'));
        }

        $lastSync = new DateTime($lastSyncString ?? '-1 year', new DateTimeZone('UTC'));

        $dateModified = $googleEvent->getUpdated() ?? '';

        return new CalendarAccountEvent(
            id: $googleEvent->getId(),
            name: $googleEvent->getSummary() ?? '',
            description: $googleEvent->getDescription() ?? '',
            location: $googleEvent->getLocation() ?? '',
            date_start: $dateStart,
            date_end: $dateEnd,
            assigned_user_id: $userId,
            type: $type,
            linked_event_id: $linkedEventId,
            last_sync: $lastSync,
            date_modified: $dateModified,
            is_external: true
        );
    }

    /**
     * @inheritdoc
     */
    public function getEvent(string $targetId): ?CalendarAccountEvent
    {
        global $log;
        $log->info("GoogleCalendarProvider: getEvent called for eventId: $targetId");

        try {
            if (empty($targetId)) {
                throw new RuntimeException('Event ID is required');
            }

            $this->initializeGoogleServices();

            $googleEvent = $this->calendarService->events->get($this->suitecrmCalendarId, $targetId);

            if ($googleEvent->getStatus() === 'cancelled') {
                $log->info("GoogleCalendarProvider: Event cancelled: $targetId");
                return null;
            }

            $calendarEvent = $this->convertFromGoogleEvent($googleEvent);

            $log->info("GoogleCalendarProvider: Successfully retrieved event: $targetId");
            return $calendarEvent;

        } catch (\Google\Service\Exception $e) {
            if ($e->getCode() === 404) {
                $log->info("GoogleCalendarProvider: Event not found: $targetId");
                return null;
            }
            $log->error("GoogleCalendarProvider: getEvent failed for $targetId: " . $e->getMessage());
            throw new RuntimeException('Google API error: ' . $e->getMessage());
        } catch (Throwable $e) {
            $log->error("GoogleCalendarProvider: getEvent failed for $targetId: " . $e->getMessage());
            throw $e;
        }
    }

    /**
     * @inheritdoc
     */
    protected function doCreateEvent(CalendarAccountEvent $targetEvent): string
    {
        global $log;
        $log->info('GoogleCalendarProvider: doCreateEvent called for event: ' . $targetEvent->getName());

        try {
            $this->initializeGoogleServices();

            $googleEvent = $this->convertToGoogleEvent($targetEvent);
            $createdEvent = $this->calendarService->events->insert($this->suitecrmCalendarId, $googleEvent);

            $eventId = $createdEvent->getId();
            if (empty($eventId)) {
                throw new RuntimeException('Google API response missing event ID');
            }

            $log->info('GoogleCalendarProvider: Successfully created event with ID: ' . $eventId);
            return $eventId;

        } catch (Throwable $e) {
            $log->error('GoogleCalendarProvider: doCreateEvent error: ' . $e->getMessage());
            throw $e;
        }
    }

    /**
     * Converts a CalendarAccountEvent instance to a GoogleEvent instance.
     *
     * @param CalendarAccountEvent $event The event instance to be converted into a GoogleEvent format.
     *
     * @return GoogleEvent The converted event in GoogleEvent format.
     */
    protected function convertToGoogleEvent(CalendarAccountEvent $event): GoogleEvent
    {
        $googleEvent = new GoogleEvent();
        $googleEvent->setSummary($event->getName());
        $googleEvent->setDescription($event->getDescription());
        $googleEvent->setLocation($event->getLocation());

        $startTime = $event->getDateStart();
        $endTime = $event->getDateEnd();

        $startDateTime = new EventDateTime();
        $endDateTime = null;
        if ($endTime) {
            $endDateTime = new EventDateTime();
        }

        $isAllDay = false;
        if ($endTime) {
            $startAtMidnight = $startTime->format('H:i:s') === '00:00:00';
            $endAtMidnight = $endTime->format('H:i:s') === '00:00:00';

            $duration = $endTime->getTimestamp() - $startTime->getTimestamp();
            $durationIsFullDays = $duration > 0 && $duration % 86400 === 0;

            $isAllDay = $startAtMidnight && $endAtMidnight && $durationIsFullDays;
        }

        if ($isAllDay) {
            $startDateTime->setDate($startTime->format('Y-m-d'));
            if ($endTime) {
                $endDateTime->setDate($endTime->format('Y-m-d'));
            }
        } else {
            $startDateTime->setDateTime($startTime->format(DATE_ATOM));
            $startDateTime->setTimeZone($startTime->getTimezone()->getName());
            if ($endTime) {
                $endDateTime->setDateTime($endTime->format(DATE_ATOM));
                $endDateTime->setTimeZone($endTime->getTimezone()->getName());
            }
        }

        $googleEvent->setStart($startDateTime);
        if ($endDateTime) {
            $googleEvent->setEnd($endDateTime);
        }

        $privateProperties = [];

        if ($event->getLinkedEventId()) {
            $privateProperties['suitecrm_linked_event_id'] = $event->getLinkedEventId();
        }

        if ($event->getLastSync()) {
            $privateProperties['suitecrm_last_sync_string'] = $event->getLastSyncString();
        }

        if ($event->getAssignedUserId()) {
            $privateProperties['suitecrm_user_id'] = $event->getAssignedUserId();
        }

        $privateProperties['suitecrm_event_type'] = $event->getType()->value;

        if (!empty($privateProperties)) {
            $extendedProperties = new EventExtendedProperties();
            $extendedProperties->setPrivate($privateProperties);
            $googleEvent->setExtendedProperties($extendedProperties);
        }

        return $googleEvent;
    }

    /**
     * @inheritdoc
     */
    protected function doUpdateEvent(CalendarAccountEvent $targetEvent): void
    {
        global $log;
        $log->info('GoogleCalendarProvider: doUpdateEvent called for event: ' . $targetEvent->getName());

        try {
            $eventId = $targetEvent->getId();
            if (empty($eventId)) {
                throw new RuntimeException('Event ID is required for update');
            }

            $this->initializeGoogleServices();

            $googleEvent = $this->convertToGoogleEvent($targetEvent);
            $this->calendarService->events->update($this->suitecrmCalendarId, $eventId, $googleEvent);

            $log->info('GoogleCalendarProvider: Successfully updated event with ID: ' . $eventId);

        } catch (Throwable $e) {
            $log->error('GoogleCalendarProvider: doUpdateEvent error: ' . $e->getMessage());
            throw $e;
        }
    }

    /**
     * @inheritdoc
     */
    protected function doDeleteEvent(string $targetId): void
    {
        global $log;
        $log->info('GoogleCalendarProvider: doDeleteEvent called for event ID: ' . $targetId);

        try {
            if (empty($targetId)) {
                throw new RuntimeException('Event ID is required for deletion');
            }

            $this->initializeGoogleServices();

            $this->calendarService->events->delete($this->suitecrmCalendarId, $targetId);

            $log->info('GoogleCalendarProvider: Successfully deleted event with ID: ' . $targetId);

        } catch (Throwable $e) {
            $log->error('GoogleCalendarProvider: doDeleteEvent error: ' . $e->getMessage());
            throw $e;
        }
    }

}