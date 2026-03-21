<?php
/**
 * In-memory fake for CalendarAccountValidator.
 *
 * Replaces database-backed validation with in-memory account storage.
 * Provides production-like validation logic without database queries.
 *
 * Key differences from production:
 * - Accounts stored in-memory instead of database
 * - Accounts must be registered via addValidAccount() before validation
 * - Validation rules same as production (ID, user ID, source required)
 * - No database queries or external dependencies
 *
 * Use this test double when testing code that validates calendar accounts
 * but you want to avoid database access. Pre-populate valid accounts using
 * addValidAccount() in your test setup.
 */

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once 'include/CalendarSync/domain/services/CalendarAccountValidator.php';

class InMemoryCalendarAccountValidator extends CalendarAccountValidator
{
    private array $validAccounts = [];

    public function addValidAccount(CalendarAccount $account): void
    {
        if (empty($account->id)) {
            throw new InvalidArgumentException('[InMemoryCalendarAccountValidator][addValidAccount] Account ID cannot be empty');
        }

        if (empty($account->calendar_user_id)) {
            throw new InvalidArgumentException('[InMemoryCalendarAccountValidator][addValidAccount] User ID cannot be empty');
        }

        if (empty($account->source)) {
            throw new InvalidArgumentException('[InMemoryCalendarAccountValidator][addValidAccount] Source cannot be empty');
        }

        $this->validAccounts[$account->id] = $account;
    }

    public function validateCalendarAccount(string $calendarAccountId): CalendarAccount
    {
        if (empty($calendarAccountId)) {
            throw new InvalidArgumentException('Calendar account ID is required');
        }

        if (!isset($this->validAccounts[$calendarAccountId])) {
            throw new RuntimeException("Calendar account not found: {$calendarAccountId}");
        }

        return $this->validAccounts[$calendarAccountId];
    }

    public function clear(): void
    {
        $this->validAccounts = [];
    }
}
