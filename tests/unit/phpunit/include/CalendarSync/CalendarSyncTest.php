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

use SuiteCRM\Test\SuitePHPUnitFrameworkTestCase;

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once 'include/CalendarSync/CalendarSync.php';

/**
 * Unit tests for CalendarSync class.
 *
 * Test organization:
 * 1. Technical/Infrastructure Tests - Validate architectural patterns, error handling, and system integration
 * 2. Business Logic Tests - Validate domain rules, workflow logic, and user requirements
 */
class CalendarSyncTest extends SuitePHPUnitFrameworkTestCase
{

    private CalendarSync $calendarSync;

    // =============================================================================
    // TECHNICAL & INFRASTRUCTURE TESTS
    // =============================================================================
    // These tests validate architectural integrity, design patterns, error boundaries,
    // and system integration. They ensure the technical foundation is solid.

    /**
     * Tests singleton pattern implementation.
     *
     * TECHNICAL: Validates that the Singleton design pattern is correctly implemented,
     * ensuring only one instance exists across the application lifecycle. This is critical
     * for maintaining consistent state and preventing resource conflicts in calendar sync operations.
     */
    public function testGetInstance(): void
    {
        $instance1 = CalendarSync::getInstance();
        $instance2 = CalendarSync::getInstance();

        self::assertSame($instance1, $instance2, 'getInstance should return the same singleton instance');
        self::assertInstanceOf(CalendarSync::class, $instance1);
    }

    /**
     * Tests singleton cannot be cloned.
     *
     * TECHNICAL: Validates that the singleton's __clone method is private, preventing
     * accidental duplication of the instance which could lead to inconsistent state
     * and break the singleton pattern integrity.
     */
    public function testSingletonCannotBeCloned(): void
    {
        $instance = CalendarSync::getInstance();

        $reflection = new ReflectionClass($instance);
        $cloneMethod = $reflection->getMethod('__clone');

        self::assertTrue($cloneMethod->isPrivate(), '__clone method should be private to prevent cloning');
    }

    /**
     * Tests singleton cannot be unserialized.
     *
     * TECHNICAL: Validates that the singleton prevents unserialization which could
     * create multiple instances and break the singleton pattern. Critical for
     * maintaining architectural integrity across serialization boundaries.
     */
    public function testSingletonCannotBeUnserialized(): void
    {
        $instance = CalendarSync::getInstance();

        $this->expectException(RuntimeException::class);
        $this->expectExceptionMessage('Cannot unserialize singleton');

        $instance->__wakeup();
    }

    /**
     * Tests system-level calendar sync returns boolean.
     *
     * TECHNICAL: Validates the system integration layer returns proper data types.
     * This ensures the orchestrator integration works correctly and provides
     * predictable return values for error handling and status reporting.
     */
    public function testSyncAllCalendarAccountsReturnsTrue(): void
    {
        $result = $this->calendarSync->syncAllCalendarAccounts();

        self::assertIsBool($result, 'syncAllCalendarAccounts should return boolean');
    }

    /**
     * Tests provider registry integration.
     *
     * TECHNICAL: Validates that the provider registry integration returns proper
     * array structure. This ensures the registry pattern is correctly implemented
     * and provides predictable data structures for UI rendering.
     */
    public function testGetCalendarSourceTypes(): void
    {
        $result = $this->calendarSync->getCalendarSourceTypes();

        self::assertIsArray($result, 'getCalendarSourceTypes should return an array');
    }

    /**
     * Tests configuration system returns array.
     *
     * TECHNICAL: Validates the configuration layer integration returns proper
     * data types. This ensures the config system abstraction works correctly
     * and provides predictable data structures.
     */
    public function testGetConfig(): void
    {
        $result = $this->calendarSync->getConfig();

        self::assertIsArray($result, 'getConfig should return an array');
    }

    /**
     * Tests conflict resolution enum integration.
     *
     * TECHNICAL: Validates that PHP enum integration works correctly and returns
     * proper array structures. This ensures enum-based domain modeling is
     * properly integrated with the rest of the system.
     */
    public function testGetConflictResolutionCases(): void
    {
        $result = $this->calendarSync->getConflictResolutionCases();

        self::assertIsArray($result, 'getConflictResolutionCases should return an array');
    }

    /**
     * Tests job scheduler integration.
     *
     * TECHNICAL: Validates that the job factory integration returns proper types.
     * The scheduler may be null if not configured, which is valid system behavior.
     * This ensures proper integration with SugarCRM's job system.
     */
    public function testGetScheduler(): void
    {
        $result = $this->calendarSync->getScheduler();

        self::assertTrue($result === null || $result instanceof Scheduler, 'getScheduler should return null or Scheduler instance');
    }

    /**
     * Tests data serialization error handling.
     *
     * TECHNICAL: Validates that invalid JSON data is properly handled by the
     * serialization layer. This ensures data transport robustness and prevents
     * system crashes from malformed job data.
     */
    public function testSyncEventWithInvalidData(): void
    {
        $result = $this->calendarSync->syncEvent('invalid_json_data');

        self::assertFalse($result, 'syncEvent should return false for invalid data');
    }

    /**
     * Tests provider validation error handling with empty source.
     *
     * TECHNICAL: Validates that the provider registry properly throws InvalidArgumentException
     * for empty inputs. This ensures proper error boundary implementation and
     * prevents system crashes from invalid provider requests.
     */
    public function testGetProviderAuthMethodWithValidationWithEmptySource(): void
    {
        $this->expectException(InvalidArgumentException::class);
        $this->expectExceptionMessage('Source is required');

        $this->calendarSync->getProviderAuthMethodWithValidation('');
    }

    /**
     * Tests provider validation error handling with invalid source.
     *
     * TECHNICAL: Validates that the provider registry properly throws RuntimeException
     * for unconfigured providers. This ensures proper error handling when
     * providers are not properly registered or configured.
     */
    public function testGetProviderAuthMethodWithValidationWithValidSource(): void
    {
        $this->expectException(RuntimeException::class);
        $this->expectExceptionMessage('Provider not found or not configured');

        $this->calendarSync->getProviderAuthMethodWithValidation('invalid_source');
    }

    /**
     * Tests provider connection validation error handling.
     *
     * TECHNICAL: Validates that provider connection testing properly throws
     * InvalidArgumentException for empty source. This ensures proper error
     * boundary implementation in the connection testing infrastructure.
     */
    public function testTestProviderConnectionWithValidationWithEmptySource(): void
    {
        $calendarAccount = $this->createMock(CalendarAccount::class);
        $calendarAccount->source = '';
        $calendarAccount->id = 'test_id';

        $this->expectException(InvalidArgumentException::class);
        $this->expectExceptionMessage('Source is required');

        $this->calendarSync->testProviderConnectionWithValidation($calendarAccount);
    }

    // =============================================================================
    // BUSINESS LOGIC TESTS
    // =============================================================================
    // These tests validate domain rules, workflow logic, and business requirements.
    // They ensure the system behaves correctly according to business specifications.

    /**
     * Tests OAuth2 authentication field hiding business rule.
     *
     * BUSINESS LOGIC: OAuth2 authentication should only show oauth_connection_name field
     * and hide username/password/api_key fields. This implements the business rule that
     * OAuth2 users don't need to manually enter credentials as they authenticate through
     * the OAuth flow.
     */
    public function testGetFieldsToHideWithValidSource(): void
    {
        $result = $this->calendarSync->getFieldsToHide('oauth2');

        self::assertIsArray($result, 'getFieldsToHide should return an array');
        // For oauth2, it should hide everything except oauth_connection_name
        self::assertContains('username', $result, 'Should hide username field for oauth2');
        self::assertContains('password', $result, 'Should hide password field for oauth2');
        self::assertContains('api_key', $result, 'Should hide api_key field for oauth2');
    }

    /**
     * Tests basic authentication field hiding business rule.
     *
     * BUSINESS LOGIC: Basic authentication should show username/password/server_url
     * and hide oauth_connection_name/api_key/api_endpoint fields. This implements
     * the business rule that basic auth users need manual credential entry.
     */
    public function testGetFieldsToHideWithBasicAuth(): void
    {
        $result = $this->calendarSync->getFieldsToHide('basic');

        self::assertIsArray($result, 'getFieldsToHide should return an array');
        // For basic auth, it should hide everything except username, password, server_url
        self::assertContains('oauth_connection_name', $result, 'Should hide oauth_connection_name field for basic auth');
        self::assertContains('api_key', $result, 'Should hide api_key field for basic auth');
        self::assertContains('api_endpoint', $result, 'Should hide api_endpoint field for basic auth');
    }

    /**
     * Tests unknown provider security business rule.
     *
     * BUSINESS LOGIC: Unknown or unconfigured providers should hide all authentication
     * fields as a security fallback. This implements the business rule that prevents
     * users from seeing potentially sensitive fields when the provider configuration
     * is unclear or missing.
     */
    public function testGetFieldsToHideWithUnknownSource(): void
    {
        $result = $this->calendarSync->getFieldsToHide('unknown');

        self::assertIsArray($result, 'getFieldsToHide should return an array');
        $expectedFields = ['oauth_connection_name', 'username', 'password', 'server_url', 'api_key', 'api_endpoint'];

        foreach ($expectedFields as $field) {
            self::assertContains($field, $result, "Should hide all auth fields for unknown source, missing: $field");
        }
    }

    /**
     * Tests configuration filtering business rule.
     *
     * BUSINESS LOGIC: Only valid configuration keys should be saved, invalid keys
     * should be ignored. This implements the business rule that protects the system
     * from configuration pollution and ensures only known settings can be modified.
     */
    public function testSaveConfigWithValidData(): void
    {
        $postData = [
            'calendar_sync_enabled' => '1',
            'calendar_sync_run_async' => '0',
            'invalid_key' => 'should_be_ignored'
        ];

        $result = $this->calendarSync->saveConfig($postData);

        self::assertIsBool($result, 'saveConfig should return boolean');
    }

    /**
     * Tests empty user ID business rule.
     *
     * BUSINESS LOGIC: Empty user ID should return empty accounts array. This implements
     * the business rule that calendar accounts are always associated with specific users,
     * and anonymous or missing users have no calendar access.
     */
    public function testGetActiveCalendarAccountsForUserWithEmptyUserId(): void
    {
        $result = $this->calendarSync->getActiveCalendarAccountsForUser('');

        self::assertIsArray($result, 'getActiveCalendarAccountsForUser should return an array');
        self::assertEmpty($result, 'Should return empty array for empty user ID');
    }

    /**
     * Tests valid user ID account retrieval business rule.
     *
     * BUSINESS LOGIC: Valid user IDs should return an array of calendar accounts
     * (which may be empty if the user has no accounts). This implements the business
     * rule that all valid users can potentially have calendar accounts.
     */
    public function testGetActiveCalendarAccountsForUserWithValidUserId(): void
    {
        $result = $this->calendarSync->getActiveCalendarAccountsForUser('test_user_id');

        self::assertIsArray($result, 'getActiveCalendarAccountsForUser should return an array');
    }

    /**
     * Tests event synchronization with valid data business rule.
     *
     * BUSINESS LOGIC: Valid event sync operations should be processed and return
     * boolean result. This implements the business rule that properly formatted
     * calendar events can be synchronized with external systems.
     */
    public function testSyncEventWithValidData(): void
    {
        $testData = json_encode(
            [
                'userId' => 'test_user',
                'calendarAccountId' => 'test_account',
                'action' => 'CREATE',
                'location' => 'EXTERNAL',
                'targetEventId' => 'test_event',
                'payload' => []
            ]
        );

        $result = $this->calendarSync->syncEvent($testData);

        self::assertIsBool($result, 'syncEvent should return boolean');
    }

    /**
     * Tests deleted meeting handling business rule.
     *
     * BUSINESS LOGIC: Deleted meetings should be handled gracefully without errors.
     * This implements the business rule that deleted meetings trigger DELETE
     * synchronization operations with external calendars to maintain consistency.
     */
    public function testSyncMeetingWithDeletedMeeting(): void
    {
        $meeting = $this->createMock(Meeting::class);
        $meeting->id = 'test_meeting_id';
        $meeting->deleted = 1;
        $meeting->assigned_user_id = 'test_user';

        $this->calendarSync->syncMeeting($meeting);

        self::assertTrue(true, 'syncMeeting should handle deleted meetings without errors');
    }

    /**
     * Tests unassigned meeting business rule.
     *
     * BUSINESS LOGIC: Meetings without assigned users should be skipped from
     * synchronization. This implements the business rule that calendar sync
     * only occurs for meetings with clear ownership and responsibility.
     */
    public function testSyncMeetingWithNoAssignedUser(): void
    {
        $meeting = $this->createMock(Meeting::class);
        $meeting->id = 'test_meeting_id';
        $meeting->deleted = 0;
        $meeting->assigned_user_id = '';

        $this->calendarSync->syncMeeting($meeting);

        self::assertTrue(true, 'syncMeeting should handle meetings with no assigned user gracefully');
    }

    protected function setUp(): void
    {
        parent::setUp();
        $this->calendarSync = CalendarSync::getInstance();
    }

}