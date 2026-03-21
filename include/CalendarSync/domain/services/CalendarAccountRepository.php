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

/**
 * Repository for querying and retrieving calendar accounts.
 * Provides flexible filtering and batch operations for calendar account retrieval.
 *
 * @internal This class should only be used within the CalendarSync module.
 *           External code must use CalendarSync facade instead.
 */
class CalendarAccountRepository
{

    /**
     * Retrieves all validated calendar accounts in a single optimized query.
     *
     * This method applies validation filters at the SQL level
     * to avoid N+1 query problems when loading multiple accounts.
     *
     * @param int|null $limit Maximum number of accounts to return, or null for no limit
     * @return CalendarAccount[] Array of validated calendar accounts eligible for synchronization.
     */
    public function getValidatedAccountsBatch(?int $limit = null): array
    {
        return $this->getCalendarAccountsForUser(validatedOnly: true, limit: $limit);
    }

    /**
     * Checks whether the specified user has a personal calendar account.
     *
     * @param string $userId The ID of the user whose calendar accounts are being checked.
     * @return bool True if the user has a personal calendar account, otherwise false.
     */
    public function hasPersonalCalendarAccount(string $userId): bool
    {
        $accounts = $this->getCalendarAccountsForUser(userId: $userId, type: 'personal', limit: 1);
        return !empty($accounts);
    }

    /**
     * Checks whether the specified user has a personal calendar account.
     *
     * @param string $userId The ID of the user whose calendar accounts are being checked.
     * @return CalendarAccount[] True if the user has a personal calendar account, otherwise false.
     */
    public function getPersonalCalendarAccounts(string $userId): array
    {
        return $this->getCalendarAccountsForUser(userId: $userId, type: 'personal');
    }


    /**
     * Get the most recent validated personal calendar account for a user.
     *
     * @param string $userId The user ID to get the account for
     * @return CalendarAccount|null The most recent validated personal account, or null if none found
     */
    public function getValidatedPersonalCalendarAccountForUser(string $userId): ?CalendarAccount
    {
        $accounts = $this->getCalendarAccountsForUser(userId: $userId, type: 'personal', validatedOnly: true, limit: 1);
        return $accounts[0] ?? null;
    }

    /**
     * Get all validated calendar accounts for a user (any type).
     *
     * @param string $userId The user ID to get accounts for
     * @return CalendarAccount[] Array of all validated calendar accounts for the user
     */
    public function getAllValidatedCalendarAccountsForUser(string $userId): array
    {
        return $this->getCalendarAccountsForUser(userId: $userId, validatedOnly: true);
    }

    /**
     * Find calendar account by external_calendar_id, excluding a specific account ID.
     *
     * @param string $externalCalendarId The external calendar ID to search for
     * @param string|null $excludeAccountId Account ID to exclude from search (for edit scenarios)
     * @return CalendarAccount|null The account using this external calendar ID, or null if none found
     */
    public function findByExternalCalendarId(string $externalCalendarId, ?string $excludeAccountId = null): ?CalendarAccount
    {
        $GLOBALS['log']->debug('[CalendarAccountRepository][findByExternalCalendarId] Searching for externalCalendarId: ' . $externalCalendarId . ', excluding: ' . ($excludeAccountId ?? 'none'));

        $calendarAccount = BeanFactory::newBean('CalendarAccount');
        if (!$calendarAccount) {
            $GLOBALS['log']->error('[CalendarAccountRepository][findByExternalCalendarId] Failed to create CalendarAccount bean');
            return null;
        }

        try {
            /** @var CalendarAccount|null $result */
            $result = $calendarAccount->retrieve_by_string_fields(
                ['external_calendar_id' => $externalCalendarId]
            );

            if (!$result || empty($result->id)) {
                return null;
            }

            if ($excludeAccountId && $excludeAccountId !== 'uncreated_calendar_account' && $result->id === $excludeAccountId) {
                $GLOBALS['log']->debug('[CalendarAccountRepository][findByExternalCalendarId] Found account is excluded, treating as not found');
                return null;
            }

            $GLOBALS['log']->debug('[CalendarAccountRepository][findByExternalCalendarId] Found account: ' . $result->id);
            return $result;

        } catch (Throwable $e) {
            $GLOBALS['log']->error('[CalendarAccountRepository][findByExternalCalendarId] Failed: ' . $e->getMessage());
            return null;
        }
    }

    /**
     * Base method to retrieve calendar accounts with flexible filtering.
     *
     * @param string|null $userId The user ID to get accounts for, or null for all users
     * @param string|null $type Filter by type ('personal', 'group'), or null for all types
     * @param bool $validatedOnly If true, only return validated accounts (connection tested, not deleted, has calendar_user_id)
     * @param int|null $limit Maximum number of accounts to return, or null for no limit
     * @param bool $includeDeleted If false (default), exclude deleted records. If true, include deleted records.
     * @return CalendarAccount[] Array of calendar accounts matching the criteria
     */
    protected function getCalendarAccountsForUser(
        ?string $userId = null,
        ?string $type = null,
        bool $validatedOnly = false,
        ?int $limit = null,
        bool $includeDeleted = false
    ): array {
        $GLOBALS['log']->debug('[CalendarAccountRepository][getCalendarAccountsForUser] userId: ' . ($userId ?? 'all') . ', type: ' . ($type ?? 'all') . ', validatedOnly: ' . ($validatedOnly ? 'yes' : 'no') . ', includeDeleted: ' . ($includeDeleted ? 'yes' : 'no'));

        $calendarAccount = BeanFactory::newBean('CalendarAccount');
        if (!$calendarAccount) {
            $GLOBALS['log']->error('[CalendarAccountRepository][getCalendarAccountsForUser] Failed to create CalendarAccount bean');
            return [];
        }

        $whereClauses = [];

        if ($userId !== null && $userId !== '') {
            $whereClauses[] = "calendar_accounts.calendar_user_id = '" . $calendarAccount->db->quote($userId) . "'";
        }

        if ($type !== null) {
            $whereClauses[] = "calendar_accounts.type = '" . $calendarAccount->db->quote($type) . "'";
        }

        if ($validatedOnly) {
            $whereClauses[] = "calendar_accounts.calendar_user_id IS NOT NULL";
            $whereClauses[] = "calendar_accounts.calendar_user_id != ''";
        }

        if (!$includeDeleted || $validatedOnly) {
            $whereClauses[] = "calendar_accounts.deleted = 0";
        }

        $orderByClauses = [
            'calendar_accounts.last_sync_attempt_date IS NOT NULL',
            'calendar_accounts.last_sync_attempt_date ASC',
            'calendar_accounts.date_entered DESC',
        ];

        try {
            $sql = "SELECT $calendarAccount->table_name.id FROM $calendarAccount->table_name";

            if (!empty($whereClauses)) {
                $whereClause = implode(' AND ', $whereClauses);
                $sql .= " WHERE $whereClause";
            }

            $orderByClause = implode(', ', $orderByClauses);
            $sql .= " ORDER BY $orderByClause";

            if ($limit !== null) {
                $sql .= " LIMIT $limit";
            }

            $result = $calendarAccount->db->query($sql);

            if (!$result) {
                $GLOBALS['log']->error('[CalendarAccountRepository][getCalendarAccountsForUser] Failed to execute query');
                return [];
            }

            $accounts = [];
            while ($row = $calendarAccount->db->fetchByAssoc($result)) {
                $account = BeanFactory::getBean('CalendarAccount', $row['id']);
                if ($account && !empty($account->id)) {
                    $accounts[] = $account;
                }
            }

            $GLOBALS['log']->debug('[CalendarAccountRepository][getCalendarAccountsForUser] Found ' . count($accounts) . ' accounts');
            return $accounts;

        } catch (Throwable $e) {
            $GLOBALS['log']->error('[CalendarAccountRepository][getCalendarAccountsForUser] Failed: ' . $e->getMessage());
            return [];
        }
    }

}
