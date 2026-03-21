<?php
/**
 * In-memory fake for CalendarAccountRepository.
 *
 * Replaces database-backed queries with in-memory account storage.
 * Provides production-like query logic without database access.
 *
 * Key differences from production:
 * - Accounts stored in-memory instead of database
 * - Accounts must be registered via addAccount() before querying
 * - Query filtering same as production (type, validated, user ID)
 * - No database queries or external dependencies
 *
 * Use this test double when testing code that queries calendar accounts
 * but you want to avoid database access. Pre-populate accounts using
 * addAccount() in your test setup.
 */

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once 'include/CalendarSync/domain/services/CalendarAccountRepository.php';

class InMemoryCalendarAccountRepository extends CalendarAccountRepository
{
    private array $accounts = [];

    public function addAccount(CalendarAccount $account): void
    {
        if (empty($account->id)) {
            throw new InvalidArgumentException('[InMemoryCalendarAccountRepository][addAccount] Account ID cannot be empty');
        }

        $this->accounts[$account->id] = $account;
    }

    protected function getCalendarAccountsForUser(
        ?string $userId = null,
        ?string $type = null,
        bool $validatedOnly = false,
        ?int $limit = null
    ): array {
        $results = [];

        foreach ($this->accounts as $account) {
            if ($userId !== null && $userId !== '' && $account->calendar_user_id !== $userId) {
                continue;
            }

            if ($type !== null && ($account->type ?? null) !== $type) {
                continue;
            }

            if ($validatedOnly) {
                if ($account->deleted ?? false) {
                    continue;
                }
                if (($account->last_connection_status ?? null) !== '1') {
                    continue;
                }
                if (empty($account->calendar_user_id)) {
                    continue;
                }
            }

            $results[] = $account;
        }

        if ($limit !== null) {
            $results = array_slice($results, 0, $limit);
        }

        return $results;
    }

    public function clear(): void
    {
        $this->accounts = [];
    }
}
