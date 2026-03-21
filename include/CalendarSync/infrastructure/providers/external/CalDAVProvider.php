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
require_once 'include/CalendarSync/domain/entities/CalendarAccountEvent.php';

/**
 * CalDAV provider implementation using HTTP Basic Authentication.
 *
 * This provider connects to CalDAV servers that use username/password authentication
 * such as Radicale, Nextcloud, ownCloud, or other CalDAV-compliant servers.
 *
 * Combines functionality from AbstractCalDAVProvider and CalDAVBasicAuthProvider
 * to provide common CalDAV functionality including iCalendar parsing,
 * WebDAV operations, and event conversion with Basic Authentication.
 */
class CalDAVProvider extends AbstractCalendarProvider
{

    protected ?string $calendarUrl = null;

    /**
     * @inheritdoc
     */
    public function testCalendarConnection(): CalendarConnectionTestResult
    {
        global $log;

        try {
            $this->getAuthHeaders();

            $this->calendarUrl = $this->getCalendarUrl();

            if (empty($this->calendarUrl)) {
                throw new RuntimeException('CalDAV calendar URL is required');
            }

            $response = $this->makeCalDAVRequest(
                'PROPFIND',
                $this->calendarUrl,
                ['Depth: 0', 'Content-Type: text/xml'],
                $this->buildPropfindXml()
            );

            if ($response['httpCode'] < 200 || $response['httpCode'] >= 300) {
                throw new RuntimeException("CalDAV server error: HTTP {$response['httpCode']}. Message: {$response['body']}");
            }

            $log->info('CalDAVProvider: Successfully connected to CalDAV server');
            return new CalendarConnectionTestResult(
                success: true,
                connection: $this->connection,
                authenticationStatus: 'authenticated',
                externalCalendarId: $this->calendarUrl
            );
        } catch (Throwable $e) {
            $log->error("CalDAVProvider: testCalendarConnection error: " . $e->getMessage());

            $authenticationStatus = 'failed';
            if (str_contains($e->getMessage(), '401') || str_contains($e->getMessage(), 'Unauthorized')) {
                $authenticationStatus = 'unauthorized';
            } elseif (str_contains($e->getMessage(), 'username and password')) {
                $authenticationStatus = 'missing_credentials';
            }

            return new CalendarConnectionTestResult(
                success: false,
                connection: $this->connection,
                errorMessage: $e->getMessage(),
                errorCode: (string)$e->getCode(),
                authenticationStatus: $authenticationStatus
            );
        }
    }

    /**
     * Returns authentication headers for HTTP Basic Authentication.
     *
     * @return array Array of HTTP headers for authentication
     */
    protected function getAuthHeaders(): array
    {
        global $log;

        $username = $this->connection?->username ?? '';
        $password = $this->connection?->password ?? '';

        if (empty($username) || empty($password)) {
            throw new RuntimeException('CalDAV Basic Auth requires username and password');
        }

        $log->debug("CalDAVProvider: Using username: " . $username);

        $credentials = base64_encode($username . ':' . $password);

        return [
            'Authorization: Basic ' . $credentials
        ];
    }

    /**
     * Returns the calendar URL from the connection configuration.
     *
     * @return string The calendar URL
     */
    protected function getCalendarUrl(): string
    {
        return $this->connection->server_url ?? '';
    }

    /**
     * Makes a CalDAV request with authentication headers.
     *
     * @param string $method HTTP method (GET, PUT, POST, DELETE, PROPFIND, REPORT)
     * @param string $url Request URL
     * @param array $headers Additional headers
     * @param string|null $body Request body
     * @return array Response with httpCode and body
     */
    protected function makeCalDAVRequest(string $method, string $url, array $headers = [], ?string $body = null): array
    {
        try {
            $curl = curl_init();

            $authHeaders = $this->getAuthHeaders();
            $allHeaders = array_merge($authHeaders, $headers);

            curl_setopt($curl, CURLOPT_URL, $url);
            curl_setopt($curl, CURLOPT_HTTPHEADER, $allHeaders);
            curl_setopt($curl, CURLOPT_RETURNTRANSFER, true);
            curl_setopt($curl, CURLOPT_SSL_VERIFYPEER, true);
            curl_setopt($curl, CURLOPT_TIMEOUT, 30);
            curl_setopt($curl, CURLOPT_CUSTOMREQUEST, $method);

            if ($body !== null) {
                curl_setopt($curl, CURLOPT_POSTFIELDS, $body);
            }

            $response = curl_exec($curl);
            $httpCode = curl_getinfo($curl, CURLINFO_HTTP_CODE);
            $error = curl_error($curl);
            curl_close($curl);

            if ($error) {
                throw new RuntimeException('Network error: ' . $error);
            }

            return [
                'httpCode' => $httpCode,
                'body' => $response
            ];
        } catch (RuntimeException $e) {
            if (str_contains($e->getMessage(), 'HTTP 401')) {
                throw new RuntimeException('Authentication failed. Please check your CalDAV credentials: ' . $e->getMessage());
            }
            throw $e;
        }
    }

    /**
     * Builds PROPFIND XML request body.
     *
     * @return string XML request body
     */
    protected function buildPropfindXml(): string
    {
        try {
            $xml = new SimpleXMLElement(
                '<?xml version="1.0" encoding="UTF-8"?>' .
                '<d:propfind xmlns:d="DAV:" xmlns:c="urn:ietf:params:xml:ns:caldav"></d:propfind>'
            );
        } catch (Exception $e) {
            return '';
        }

        $prop = $xml->addChild('d:prop', null, 'DAV:');

        $prop?->addChild('d:resourcetype', null, 'DAV:');

        $prop?->addChild('d:displayname', null, 'DAV:');

        return $xml->asXML();
    }

    /**
     * @inheritdoc
     */
    public function getEvents(CalendarEventQuery $query): array
    {
        global $log;
        $log->info('CalDAVProvider: getEvents called with query: ' . json_encode($query->toArray()));

        $events = [];

        try {
            $this->calendarUrl = $this->getCalendarUrl();

            $timeRange = [];
            if ($query->getStartDate()) {
                $timeRange['start'] = $this->formatDateTimeAsUTC($query->getStartDate());
            }
            if ($query->getEndDate()) {
                $timeRange['end'] = $this->formatDateTimeAsUTC($query->getEndDate());
            }

            $reportXml = $this->buildCalendarQueryXml($timeRange);

            $response = $this->makeCalDAVRequest(
                'REPORT',
                $this->calendarUrl,
                ['Depth: 1', 'Content-Type: text/xml'],
                $reportXml
            );

            if ($response['httpCode'] < 200 || $response['httpCode'] >= 300) {
                throw new RuntimeException('CalDAV REPORT error: HTTP ' . $response['httpCode']);
            }

            $events = $this->parseCalendarQueryResponse($response['body']);

            $log->info('CalDAVProvider: Retrieved ' . count($events) . ' events from CalDAV server');
        } catch (Throwable $e) {
            $log->error('CalDAVProvider: getEvents error: ' . $e->getMessage());
            throw $e;
        }

        return $events;
    }

    /**
     * Converts a DateTime to UTC and formats it for iCalendar.
     *
     * @param DateTime $dateTime The datetime to convert
     * @return string The UTC formatted datetime (Ymd\THis\Z)
     */
    protected function formatDateTimeAsUTC(DateTime $dateTime): string
    {
        $utcDateTime = clone $dateTime;
        $utcDateTime->setTimezone(new DateTimeZone('UTC'));
        return $utcDateTime->format('Ymd\THis\Z');
    }

    /**
     * Builds calendar-query XML for retrieving events.
     *
     * @param array $timeRange Time range filter with 'start' and 'end' keys
     * @return string XML request body
     */
    protected function buildCalendarQueryXml(array $timeRange = []): string
    {
        try {
            $xml = new SimpleXMLElement(
                '<?xml version="1.0" encoding="UTF-8"?>' .
                '<c:calendar-query xmlns:d="DAV:" xmlns:c="urn:ietf:params:xml:ns:caldav"></c:calendar-query>'
            );
        } catch (Exception $e) {
            return '';
        }

        $prop = $xml->addChild('d:prop', null, 'DAV:');
        $prop?->addChild('d:getetag', null, 'DAV:');
        $prop?->addChild('c:calendar-data', null, 'urn:ietf:params:xml:ns:caldav');

        $filter = $xml->addChild('c:filter', null, 'urn:ietf:params:xml:ns:caldav');
        $vcalendarFilter = $filter?->addChild('c:comp-filter', null, 'urn:ietf:params:xml:ns:caldav');
        $vcalendarFilter?->addAttribute('name', 'VCALENDAR');
        $veventFilter = $vcalendarFilter?->addChild('c:comp-filter', null, 'urn:ietf:params:xml:ns:caldav');
        $veventFilter?->addAttribute('name', 'VEVENT');

        if (!empty($timeRange)) {
            $timeRangeEl = $veventFilter?->addChild('c:time-range', null, 'urn:ietf:params:xml:ns:caldav');
            if (isset($timeRange['start'])) {
                $timeRangeEl?->addAttribute('start', $timeRange['start']);
            }
            if (isset($timeRange['end'])) {
                $timeRangeEl?->addAttribute('end', $timeRange['end']);
            }
        }

        return $xml->asXML();
    }

    /**
     * Parses the response from a calendar-query REPORT request.
     *
     * @param string $xmlResponse The XML response from the server
     * @return CalendarAccountEvent[] Array of calendar events
     */
    protected function parseCalendarQueryResponse(string $xmlResponse): array
    {
        $events = [];

        try {
            $xml = simplexml_load_string($xmlResponse);
            if ($xml === false) {
                return $events;
            }

            $xml->registerXPathNamespace('d', 'DAV:');
            $xml->registerXPathNamespace('c', 'urn:ietf:params:xml:ns:caldav');

            $responses = $xml->xpath('//d:response');

            foreach ($responses as $response) {
                $href = (string)$response->xpath('d:href')[0];
                $calendarData = $response->xpath('.//c:calendar-data');

                if (!empty($calendarData)) {
                    $icalData = (string)$calendarData[0];
                    $eventId = basename($href, '.ics');

                    try {
                        $events[] = $this->parseICalendarData($icalData, $eventId);
                    } catch (Exception $e) {
                        global $log;
                        $log->warn('CalDAVProvider: Failed to parse event: ' . $e->getMessage());
                    }
                }
            }
        } catch (Exception $e) {
            global $log;
            $log->error('CalDAVProvider: Failed to parse calendar query response: ' . $e->getMessage());
        }

        return $events;
    }

    /**
     * Parses iCalendar data and converts to CalendarAccountEvent.
     *
     * @param string $icalData The iCalendar data
     * @param string $eventId The event ID
     * @return CalendarAccountEvent The parsed event
     * @throws RuntimeException If parsing fails
     */
    protected function parseICalendarData(string $icalData, string $eventId): CalendarAccountEvent
    {
        $normalizedData = str_replace(["\r\n", "\r", "\n"], "\r\n", $icalData);
        $unfoldedData = preg_replace("/\r\n[ \t]/", '', $normalizedData);
        $lines = explode("\r\n", $unfoldedData);
        $eventData = [];

        $inEvent = false;
        foreach ($lines as $line) {
            if (trim($line) === 'BEGIN:VEVENT') {
                $inEvent = true;
                continue;
            }
            if (trim($line) === 'END:VEVENT') {
                break;
            }
            if (!$inEvent) {
                continue;
            }

            if (str_contains($line, ':')) {
                [$key, $value] = explode(':', $line, 2);
                $cleanKey = trim($key);

                if (str_contains($cleanKey, ';')) {
                    $parts = explode(';', $cleanKey);
                    $fieldName = $parts[0];
                    $eventData[$fieldName] = trim($value);
                    foreach ($parts as $part) {
                        if (str_starts_with($part, 'TZID=')) {
                            $eventData[$fieldName . '_TZID'] = substr($part, 5);
                        }
                    }
                } else {
                    $eventData[$cleanKey] = trim($value);
                }
            }
        }

        if (!isset($eventData['DTSTART'])) {
            throw new RuntimeException("Invalid iCalendar event: Missing required DTSTART for event ID: $eventId");
        }

        if (!isset($eventData['SUMMARY'])) {
            throw new RuntimeException("Invalid iCalendar event: Missing required SUMMARY for event ID: $eventId");
        }

        $startTimezone = $eventData['DTSTART_TZID'] ?? null;
        $startDate = $this->parseICalDateTime($eventData['DTSTART'], $startTimezone);
        if (!$startDate) {
            throw new RuntimeException("Invalid iCalendar event: Invalid DTSTART format for event ID: $eventId");
        }

        $endDate = null;
        if (isset($eventData['DTEND'])) {
            $endTimezone = $eventData['DTEND_TZID'] ?? null;
            $endDate = $this->parseICalDateTime($eventData['DTEND'], $endTimezone);
        } elseif (isset($eventData['DURATION'])) {
            $endDate = $this->calculateEndDateFromDuration($startDate, $eventData['DURATION']);
        }

        $linkedEventId = $eventData['X-SUITECRM-LINKED-EVENT'] ?? null;
        $userId = $eventData['X-SUITECRM-USER-ID'] ?? $this->connection->calendar_user_id;
        $eventType = CalendarEventType::tryFrom($eventData['X-SUITECRM-EVENT-TYPE'] ?? '') ?? CalendarEventType::MEETING;

        $lastSync = null;
        if (isset($eventData['X-SUITECRM-LAST-SYNC'])) {
            $lastSync = $this->parseICalDateTime($eventData['X-SUITECRM-LAST-SYNC']);
        }

        $dateModified = null;
        if (isset($eventData['LAST-MODIFIED'])) {
            $dateModified = $this->parseICalDateTime($eventData['LAST-MODIFIED']);
        } elseif (isset($eventData['DTSTAMP'])) {
            $dateModified = $this->parseICalDateTime($eventData['DTSTAMP']);
        }
        if (!$dateModified) {
            throw new RuntimeException("Invalid iCalendar event: Invalid LAST-MODIFIED or DTSTAMP format for event ID: $eventId");
        }

        return new CalendarAccountEvent(
            id: $eventId,
            name: $this->unescapeICalValue($eventData['SUMMARY']),
            description: $this->unescapeICalValue($eventData['DESCRIPTION'] ?? ''),
            location: $this->unescapeICalValue($eventData['LOCATION'] ?? ''),
            date_start: $startDate,
            date_end: $endDate,
            assigned_user_id: $userId,
            type: $eventType,
            linked_event_id: $linkedEventId,
            last_sync: $lastSync,
            date_modified: $dateModified,
            is_external: true
        );
    }

    /**
     * Parses an iCalendar date-time string.
     *
     * @param string $dateTime The iCalendar date-time string
     * @param string|null $timezone Optional timezone identifier (e.g., 'America/Bogota')
     * @return DateTime|null The parsed DateTime or null if invalid
     */
    protected function parseICalDateTime(string $dateTime, ?string $timezone = null): ?DateTime
    {
        if (empty($dateTime)) {
            return null;
        }

        try {
            $tz = new DateTimeZone('UTC');
            if ($timezone !== null) {
                try {
                    $tz = new DateTimeZone($timezone);
                } catch (Exception) {
                }
            }

            if (strlen($dateTime) === 8) {
                return DateTime::createFromFormat('Ymd', $dateTime, $tz);
            }

            if (str_ends_with($dateTime, 'Z')) {
                return DateTime::createFromFormat('Ymd\THis\Z', $dateTime, new DateTimeZone('UTC'));
            }

            if (strlen($dateTime) === 15) {
                $dt = DateTime::createFromFormat('Ymd\THis', $dateTime, $tz);
                if ($dt && $timezone !== null) {
                    $dt->setTimezone(new DateTimeZone('UTC'));
                }
                return $dt;
            }

            return new DateTime($dateTime, $tz);
        } catch (Exception) {
            return null;
        }
    }

    /**
     * Calculates end date from start date and duration.
     *
     * @param DateTime $startDate The start date
     * @param string $duration The ISO 8601 duration string (e.g., PT1H)
     * @return DateTime|null The calculated end date or null if invalid
     */
    protected function calculateEndDateFromDuration(DateTime $startDate, string $duration): ?DateTime
    {
        try {
            $interval = new DateInterval($duration);
            $endDate = clone $startDate;
            $endDate->add($interval);
            return $endDate;
        } catch (Exception) {
            return null;
        }
    }

    /**
     * Unescapes iCalendar values.
     *
     * @param string $value The value to unescape
     * @return string The unescaped value
     */
    protected function unescapeICalValue(string $value): string
    {
        return str_replace(["\\n", "\\r", "\\,", "\\;", "\\\\"], ["\n", "\r", ",", ";", "\\"], $value);
    }

    /**
     * @inheritdoc
     */
    public function getEvent(string $targetId): ?CalendarAccountEvent
    {
        global $log;
        $log->info("CalDAVProvider: getEvent called for eventId: $targetId");

        try {
            if (empty($targetId)) {
                throw new RuntimeException('Event ID is required');
            }

            $this->calendarUrl = $this->getCalendarUrl();
            $eventUrl = rtrim($this->calendarUrl, '/') . '/' . $targetId . '.ics';

            $response = $this->makeCalDAVRequest('GET', $eventUrl);

            if ($response['httpCode'] === 404) {
                $log->info("CalDAVProvider: Event not found: $targetId");
                return null;
            }

            if ($response['httpCode'] < 200 || $response['httpCode'] >= 300) {
                throw new RuntimeException('CalDAV GET error: HTTP ' . $response['httpCode']);
            }

            $event = $this->parseICalendarData($response['body'], $targetId);

            $log->info("CalDAVProvider: Successfully retrieved event: $targetId");
            return $event;

        } catch (Throwable $e) {
            $log->error("CalDAVProvider: getEvent failed for $targetId: " . $e->getMessage());
            return null;
        }
    }

    /**
     * @inheritdoc
     */
    protected function doCreateEvent(CalendarAccountEvent $targetEvent): string
    {
        global $log;
        $log->info('CalDAVProvider: doCreateEvent called for event: ' . $targetEvent->getName());

        try {
            $this->calendarUrl = $this->getCalendarUrl();

            $eventId = $targetEvent->getId();
            $eventUrl = rtrim($this->calendarUrl, '/') . '/' . $eventId . '.ics';

            $icalData = $this->convertToICalendar($targetEvent, $eventId);

            $response = $this->makeCalDAVRequest(
                'PUT',
                $eventUrl, ['Content-Type: text/calendar'],
                $icalData
            );

            if ($response['httpCode'] !== 201 && $response['httpCode'] !== 204) {
                throw new RuntimeException('CalDAV PUT error: HTTP ' . $response['httpCode']);
            }

            $log->info('CalDAVProvider: Successfully created event with ID: ' . $eventId);
            return $eventId;

        } catch (Throwable $e) {
            $log->error('CalDAVProvider: createEvent error: ' . $e->getMessage());
            throw $e;
        }
    }

    /**
     * Converts a CalendarAccountEvent to iCalendar format.
     *
     * @param CalendarAccountEvent $event The event to convert
     * @param string $eventId The event ID
     * @return string iCalendar data
     */
    protected function convertToICalendar(CalendarAccountEvent $event, string $eventId): string
    {
        $ical = "BEGIN:VCALENDAR\r\n";
        $ical .= "VERSION:2.0\r\n";
        $ical .= "PRODID:-//SuiteCRM//SuiteCRM Calendar//EN\r\n";
        $ical .= "BEGIN:VEVENT\r\n";
        $ical .= "UID:" . $eventId . "\r\n";
        $ical .= "DTSTAMP:" . gmdate('Ymd\THis\Z') . "\r\n";
        $ical .= "DTSTART:" . $this->formatDateTimeAsUTC($event->getDateStart()) . "\r\n";

        if ($event->getDateEnd()) {
            $ical .= "DTEND:" . $this->formatDateTimeAsUTC($event->getDateEnd()) . "\r\n";
        }

        $ical .= "SUMMARY:" . $this->escapeICalValue($event->getName()) . "\r\n";

        if (!empty($event->getDescription())) {
            $ical .= "DESCRIPTION:" . $this->escapeICalValue($event->getDescription()) . "\r\n";
        }

        if (!empty($event->getLocation())) {
            $ical .= "LOCATION:" . $this->escapeICalValue($event->getLocation()) . "\r\n";
        }

        if ($event->getLinkedEventId()) {
            $ical .= "X-SUITECRM-LINKED-EVENT:" . $event->getLinkedEventId() . "\r\n";
        }

        if ($event->getAssignedUserId()) {
            $ical .= "X-SUITECRM-USER-ID:" . $event->getAssignedUserId() . "\r\n";
        }

        $ical .= "X-SUITECRM-EVENT-TYPE:" . $event->getType()->value . "\r\n";

        if ($event->getLastSync()) {
            $ical .= "X-SUITECRM-LAST-SYNC:" . $this->formatDateTimeAsUTC($event->getLastSync()) . "\r\n";
        }

        if ($event->getDateModified()) {
            $ical .= "LAST-MODIFIED:" . $this->formatDateTimeAsUTC($event->getDateModified()) . "\r\n";
        }

        $ical .= "END:VEVENT\r\n";
        $ical .= "END:VCALENDAR\r\n";

        return $ical;
    }

    /**
     * Escapes special characters for iCalendar values.
     *
     * @param string $value The value to escape
     * @return string The escaped value
     */
    protected function escapeICalValue(string $value): string
    {
        return str_replace(["\n", "\r", ",", ";", "\\"], ["\\n", "\\r", "\\,", "\\;", "\\\\"], $value);
    }

    /**
     * @inheritdoc
     */
    protected function doUpdateEvent(CalendarAccountEvent $targetEvent): void
    {
        global $log;
        $log->info('CalDAVProvider: doUpdateEvent called for event: ' . $targetEvent->getName());

        try {
            $eventId = $targetEvent->getId();
            if (empty($eventId)) {
                throw new RuntimeException('Event ID is required for update');
            }

            $this->calendarUrl = $this->getCalendarUrl();
            $eventUrl = rtrim($this->calendarUrl, '/') . '/' . $eventId . '.ics';

            $icalData = $this->convertToICalendar($targetEvent, $eventId);

            $response = $this->makeCalDAVRequest(
                'PUT',
                $eventUrl,
                ['Content-Type: text/calendar'],
                $icalData
            );

            if ($response['httpCode'] !== 200 && $response['httpCode'] !== 204) {
                throw new RuntimeException('CalDAV PUT error: HTTP ' . $response['httpCode']);
            }

            $log->info('CalDAVProvider: Successfully updated event with ID: ' . $eventId);

        } catch (Throwable $e) {
            $log->error('CalDAVProvider: updateEvent error: ' . $e->getMessage());
        }
    }

    /**
     * @inheritdoc
     */
    protected function doDeleteEvent(string $targetId): void
    {
        global $log;
        $log->info('CalDAVProvider: doDeleteEvent called for event ID: ' . $targetId);

        try {
            if (empty($targetId)) {
                throw new RuntimeException('Event ID is required for deletion');
            }

            $this->calendarUrl = $this->getCalendarUrl();
            $eventUrl = rtrim($this->calendarUrl, '/') . '/' . $targetId . '.ics';

            $response = $this->makeCalDAVRequest('DELETE', $eventUrl);

            if ($response['httpCode'] !== 200 && $response['httpCode'] !== 204) {
                throw new RuntimeException('CalDAV DELETE error: HTTP ' . $response['httpCode']);
            }

            $log->info('CalDAVProvider: Successfully deleted event with ID: ' . $targetId);

        } catch (Throwable $e) {
            $log->error('CalDAVProvider: deleteEvent error: ' . $e->getMessage());
        }
    }

}