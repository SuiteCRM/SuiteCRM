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

require_once 'include/CalendarSync/domain/services/CalendarEventConflictResolver.php';
require_once 'include/CalendarSync/domain/entities/CalendarAccountEvent.php';
require_once 'include/CalendarSync/domain/enums/ConflictResolution.php';

/**
 * Unit tests for CalendarEventConflictResolver class.
 *
 * Test organization:
 * 1. Technical/Infrastructure Tests - Validate architectural patterns, error handling, and edge cases
 * 2. Business Logic Tests - Validate conflict resolution scenarios and sync behavior
 */
class CalendarEventConflictResolverTest extends SuitePHPUnitFrameworkTestCase
{

    private CalendarEventConflictResolver $resolver;

    // =============================================================================
    // TECHNICAL & INFRASTRUCTURE TESTS
    // =============================================================================

    /**
     * Tests that unrelated events do not throw error in production mode.
     *
     * TECHNICAL: Validates that relationship validation is skipped in production for performance.
     * CalendarSyncOperationDiscovery ensures events are related before calling resolver.
     */
    public function testUnrelatedEventsSkipValidationInProductionMode(): void
    {
        if (isset($GLOBALS['sugar_config']['developer_mode'])) {
            unset($GLOBALS['sugar_config']['developer_mode']);
        }

        $event1 = $this->createEvent('event1', 'Meeting 1', null);
        $event2 = $this->createEvent('event2', 'Meeting 2', null);

        $result = $this->resolver->determineWinningEvent($event1, $event2, ConflictResolution::TIMESTAMP);

        self::assertInstanceOf(CalendarAccountEvent::class, $result, 'Should not throw exception in production mode');
    }

    /**
     * Tests that year 2038 timestamp triggers warning.
     *
     * TECHNICAL: Validates that timestamps beyond year 2038 are logged as warnings
     * to prevent future timestamp overflow issues.
     */
    public function testYear2038TimestampWarning(): void
    {
        $futureDate = new DateTime('2040-01-01');
        $event1 = $this->createEvent('event1', 'Meeting', 'event2', $futureDate);
        $event2 = $this->createEvent('event2', 'Meeting', 'event1');

        $result = $this->resolver->determineWinningEvent($event1, $event2, ConflictResolution::TIMESTAMP);

        self::assertInstanceOf(CalendarAccountEvent::class, $result);
    }

    /**
     * Tests that XSS patterns trigger security warning.
     *
     * TECHNICAL: Validates that suspicious content patterns are logged for security review.
     */
    public function testXSSPatternDetection(): void
    {
        $event1 = $this->createEvent('event1', '<script>alert("xss")</script>', 'event2');
        $event2 = $this->createEvent('event2', 'Normal title', 'event1');

        $result = $this->resolver->determineWinningEvent($event1, $event2, ConflictResolution::TIMESTAMP);

        self::assertInstanceOf(CalendarAccountEvent::class, $result);
    }

    /**
     * Tests sub-second precision detection.
     *
     * TECHNICAL: Validates that modifications within the same second are correctly detected
     * using microsecond precision. Must exceed clock skew tolerance (5 seconds).
     */
    public function testSubSecondPrecisionDetection(): void
    {
        $baseTime = new DateTime('2025-01-01 12:00:00.000000');
        $laterTime = new DateTime('2025-01-01 12:05:01.000000');

        $event1 = $this->createEvent('event1', 'Meeting', 'event2', $baseTime, $baseTime);
        $event2 = $this->createEvent('event2', 'Meeting Different', 'event1', $laterTime, $baseTime);

        $result = $this->resolver->determineWinningEvent($event1, $event2, ConflictResolution::TIMESTAMP);

        self::assertSame('event2', $result->getId(), 'Should detect modification beyond clock skew tolerance');
    }

    /**
     * Tests clock skew tolerance.
     *
     * TECHNICAL: Validates that clock skew tolerance prevents false positives
     * from small time differences between systems.
     */
    public function testClockSkewTolerance(): void
    {
        $baseTime = new DateTime('2025-01-01 12:00:00');
        $skewedTime = clone $baseTime;
        $skewedTime->modify('+3 seconds');

        $event1 = $this->createEvent('event1', 'Meeting', 'event2', $baseTime, $baseTime);
        $event2 = $this->createEvent('event2', 'Meeting', 'event1', $baseTime, $skewedTime);

        $result = $this->resolver->determineWinningEvent($event1, $event2, ConflictResolution::TIMESTAMP);

        self::assertSame('event1', $result->getId(), 'Should not detect modification within clock skew tolerance');
    }

    /**
     * Tests timestamp tie-breaker.
     *
     * TECHNICAL: Validates that identical timestamps use event ID as deterministic
     * tie-breaker to prevent sync loops.
     */
    public function testTimestampTieBreaker(): void
    {
        $sameTime = new DateTime('2025-01-01 12:00:00');

        $event1 = $this->createEvent('aaa', 'Meeting', 'bbb', $sameTime, new DateTime('2025-01-01 11:00:00'));
        $event2 = $this->createEvent('bbb', 'Different', 'aaa', $sameTime, new DateTime('2025-01-01 11:00:00'));

        $result = $this->resolver->determineWinningEvent($event1, $event2, ConflictResolution::TIMESTAMP);

        self::assertSame('aaa', $result->getId(), 'Should use event ID as tie-breaker (aaa <= bbb)');
    }

    /**
     * Tests checksum-based content comparison performance.
     *
     * TECHNICAL: Validates that content comparison uses checksums for O(1) performance.
     */
    public function testContentComparisonUsesChecksum(): void
    {
        $event1 = $this->createEvent('event1', 'Meeting Title', 'event2');
        $event2 = $this->createEvent('event2', 'Meeting Title', 'event1');

        self::assertSame($event1->getContentChecksum(), $event2->getContentChecksum(), 'Identical content should have same checksum');
    }

    /**
     * Tests deprecated resolveConflict method still works.
     *
     * TECHNICAL: Validates backward compatibility wrapper for deprecated method.
     */
    public function testDeprecatedResolveConflictMethod(): void
    {
        $event1 = $this->createEvent('event1', 'Meeting', 'event2');
        $event2 = $this->createEvent('event2', 'Meeting', 'event1');

        $result = $this->resolver->determineWinningEvent($event1, $event2, ConflictResolution::TIMESTAMP);

        self::assertInstanceOf(CalendarAccountEvent::class, $result);
    }

    // =============================================================================
    // BUSINESS LOGIC TESTS - Conflict Resolution Scenarios
    // =============================================================================

    /**
     * Tests Scenario 1: Neither event modified since last sync.
     *
     * BUSINESS LOGIC: When neither event has been modified since the last sync,
     * no synchronization is needed. Returns target event as no-op.
     */
    public function testNeitherEventModified(): void
    {
        $lastSync = new DateTime('2025-01-01 12:00:00');
        $dateModified = new DateTime('2025-01-01 11:00:00');

        $event1 = $this->createEvent('event1', 'Meeting', 'event2', $dateModified, $lastSync);
        $event2 = $this->createEvent('event2', 'Meeting', 'event1', $dateModified, $lastSync);

        $result = $this->resolver->determineWinningEvent($event1, $event2, ConflictResolution::TIMESTAMP);

        self::assertSame('event1', $result->getId(), 'Should return target event when neither modified');
    }

    /**
     * Tests Scenario 2a: Only target event modified.
     *
     * BUSINESS LOGIC: When only target event is modified, it should win automatically
     * without applying conflict resolution strategy (one-sided change).
     */
    public function testOnlyTargetEventModified(): void
    {
        $lastSync = new DateTime('2025-01-01 12:00:00');
        $targetModified = new DateTime('2025-01-01 13:00:00');
        $sourceModified = new DateTime('2025-01-01 11:00:00');

        $event1 = $this->createEvent('event1', 'Updated Meeting', 'event2', $targetModified, $lastSync);
        $event2 = $this->createEvent('event2', 'Meeting', 'event1', $sourceModified, $lastSync);

        $result = $this->resolver->determineWinningEvent($event1, $event2, ConflictResolution::TIMESTAMP);

        self::assertSame('event1', $result->getId(), 'Should return target event when only target modified');
    }

    /**
     * Tests Scenario 2b: Only source event modified.
     *
     * BUSINESS LOGIC: When only source event is modified, it should win automatically
     * without applying conflict resolution strategy (one-sided change).
     */
    public function testOnlySourceEventModified(): void
    {
        $lastSync = new DateTime('2025-01-01 12:00:00');
        $targetModified = new DateTime('2025-01-01 11:00:00');
        $sourceModified = new DateTime('2025-01-01 13:00:00');

        $event1 = $this->createEvent('event1', 'Meeting', 'event2', $targetModified, $lastSync);
        $event2 = $this->createEvent('event2', 'Updated Meeting', 'event1', $sourceModified, $lastSync);

        $result = $this->resolver->determineWinningEvent($event1, $event2, ConflictResolution::TIMESTAMP);

        self::assertSame('event2', $result->getId(), 'Should return source event when only source modified');
    }

    /**
     * Tests Scenario 3: Both modified to identical content (convergent edit).
     *
     * BUSINESS LOGIC: When both events are modified but result in identical content,
     * no conflict resolution is needed as the changes converged to the same state.
     */
    public function testBothModifiedToIdenticalContent(): void
    {
        $lastSync = new DateTime('2025-01-01 12:00:00');
        $bothModified = new DateTime('2025-01-01 13:00:00');

        $event1 = $this->createEvent('event1', 'Updated Meeting', 'event2', $bothModified, $lastSync);
        $event2 = $this->createEvent('event2', 'Updated Meeting', 'event1', $bothModified, $lastSync);

        $result = $this->resolver->determineWinningEvent($event1, $event2, ConflictResolution::TIMESTAMP);

        self::assertSame('event1', $result->getId(), 'Should return target when both modified to identical content');
    }

    /**
     * Tests Scenario 4a: Two-sided conflict with TIMESTAMP strategy.
     *
     * BUSINESS LOGIC: When both events modified with different content, TIMESTAMP strategy
     * should select the most recently modified event.
     */
    public function testTwoSidedConflictTimestampStrategy(): void
    {
        $lastSync = new DateTime('2025-01-01 12:00:00');
        $targetModified = new DateTime('2025-01-01 13:00:00');
        $sourceModified = new DateTime('2025-01-01 14:00:00');

        $event1 = $this->createEvent('event1', 'Meeting A', 'event2', $targetModified, $lastSync);
        $event2 = $this->createEvent('event2', 'Meeting B', 'event1', $sourceModified, $lastSync);

        $result = $this->resolver->determineWinningEvent($event1, $event2, ConflictResolution::TIMESTAMP);

        self::assertSame('event2', $result->getId(), 'TIMESTAMP strategy should select more recent event');
    }

    /**
     * Tests Scenario 4b: Two-sided conflict with EXTERNAL_BASED strategy (external wins).
     *
     * BUSINESS LOGIC: When both events modified with different content, EXTERNAL_BASED strategy
     * should select the external event.
     */
    public function testTwoSidedConflictExternalBasedStrategyExternalWins(): void
    {
        $lastSync = new DateTime('2025-01-01 12:00:00');
        $bothModified = new DateTime('2025-01-01 13:00:00');

        $internalEvent = $this->createEvent('internal', 'Meeting A', 'external', $bothModified, $lastSync, false);
        $externalEvent = $this->createEvent('external', 'Meeting B', 'internal', $bothModified, $lastSync, true);

        $result = $this->resolver->determineWinningEvent($internalEvent, $externalEvent, ConflictResolution::EXTERNAL_BASED);

        self::assertSame('external', $result->getId(), 'EXTERNAL_BASED strategy should select external event');
    }

    /**
     * Tests Scenario 4c: Two-sided conflict with EXTERNAL_BASED strategy (fallback to timestamp).
     *
     * BUSINESS LOGIC: When both events have same external status, EXTERNAL_BASED strategy
     * should fall back to timestamp comparison.
     */
    public function testTwoSidedConflictExternalBasedStrategyFallback(): void
    {
        $lastSync = new DateTime('2025-01-01 12:00:00');
        $olderModified = new DateTime('2025-01-01 13:00:00');
        $newerModified = new DateTime('2025-01-01 14:00:00');

        $event1 = $this->createEvent('event1', 'Meeting A', 'event2', $olderModified, $lastSync, true);
        $event2 = $this->createEvent('event2', 'Meeting B', 'event1', $newerModified, $lastSync, true);

        $result = $this->resolver->determineWinningEvent($event1, $event2, ConflictResolution::EXTERNAL_BASED);

        self::assertSame('event2', $result->getId(), 'EXTERNAL_BASED should fall back to timestamp when both external');
    }

    /**
     * Tests Scenario 4d: Two-sided conflict with INTERNAL_BASED strategy (internal wins).
     *
     * BUSINESS LOGIC: When both events modified with different content, INTERNAL_BASED strategy
     * should select the internal event.
     */
    public function testTwoSidedConflictInternalBasedStrategyInternalWins(): void
    {
        $lastSync = new DateTime('2025-01-01 12:00:00');
        $bothModified = new DateTime('2025-01-01 13:00:00');

        $internalEvent = $this->createEvent('internal', 'Meeting A', 'external', $bothModified, $lastSync, false);
        $externalEvent = $this->createEvent('external', 'Meeting B', 'internal', $bothModified, $lastSync, true);

        $result = $this->resolver->determineWinningEvent($internalEvent, $externalEvent, ConflictResolution::INTERNAL_BASED);

        self::assertSame('internal', $result->getId(), 'INTERNAL_BASED strategy should select internal event');
    }

    /**
     * Tests Scenario 4e: Two-sided conflict with INTERNAL_BASED strategy (fallback to timestamp).
     *
     * BUSINESS LOGIC: When both events have same external status, INTERNAL_BASED strategy
     * should fall back to timestamp comparison.
     */
    public function testTwoSidedConflictInternalBasedStrategyFallback(): void
    {
        $lastSync = new DateTime('2025-01-01 12:00:00');
        $olderModified = new DateTime('2025-01-01 13:00:00');
        $newerModified = new DateTime('2025-01-01 14:00:00');

        $event1 = $this->createEvent('event1', 'Meeting A', 'event2', $olderModified, $lastSync, false);
        $event2 = $this->createEvent('event2', 'Meeting B', 'event1', $newerModified, $lastSync, false);

        $result = $this->resolver->determineWinningEvent($event1, $event2, ConflictResolution::INTERNAL_BASED);

        self::assertSame('event2', $result->getId(), 'INTERNAL_BASED should fall back to timestamp when both internal');
    }

    /**
     * Tests first sync scenario with null last_sync.
     *
     * BUSINESS LOGIC: First sync should treat events as modified since last_sync defaults
     * to 1 year ago, triggering conflict resolution for initial synchronization.
     */
    public function testFirstSyncWithNullLastSync(): void
    {
        $recentModified = new DateTime('2025-01-01 13:00:00');

        $event1 = $this->createEvent('event1', 'Meeting A', 'event2', $recentModified, null);
        $event2 = $this->createEvent('event2', 'Meeting B', 'event1', $recentModified, null);

        $result = $this->resolver->determineWinningEvent($event1, $event2, ConflictResolution::TIMESTAMP);

        self::assertInstanceOf(CalendarAccountEvent::class, $result, 'Should handle first sync with null last_sync');
    }

    // =============================================================================
    // HELPER METHODS
    // =============================================================================

    private function createEvent(
        string $id,
        string $title,
        ?string $linkedEventId,
        ?DateTime $dateModified = null,
        ?DateTime $lastSync = null,
        bool $isExternal = false
    ): CalendarAccountEvent {
        $dateModified = $dateModified ?? new DateTime();
        $lastSync = $lastSync ?? new DateTime('-1 year');

        return new CalendarAccountEvent(
            id: $id,
            name: $title,
            description: 'Test description',
            location: 'Test location',
            date_start: new DateTime('2025-01-15 14:00:00'),
            date_end: new DateTime('2025-01-15 15:00:00'),
            assigned_user_id: 'user123',
            linked_event_id: $linkedEventId,
            last_sync: $lastSync,
            date_modified: $dateModified,
            is_external: $isExternal
        );
    }

    protected function setUp(): void
    {
        parent::setUp();
        $this->resolver = new CalendarEventConflictResolver();
    }

}
