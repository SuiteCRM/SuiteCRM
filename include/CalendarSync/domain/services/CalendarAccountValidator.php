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
 * Validates calendar account eligibility for synchronization operations.
 * Ensures account exists, is configured correctly, and has an active connection.
 */
class CalendarAccountValidator
{

    /**
     * Validates the provided calendar account ID and ensures that the associated
     * calendar account exists, has an active connection, and is properly configured.
     *
     * @param string $calendarAccountId The unique identifier for the calendar account to validate.
     *
     * @return CalendarAccount The validated calendar account object.
     *
     * @throws InvalidArgumentException If the provided calendar account ID is empty.
     * @throws RuntimeException If the calendar account is not found, if the connection is not active,
     *                          or if the calendar account has no user ID assigned.
     */
    public function validateCalendarAccount(string $calendarAccountId): CalendarAccount
    {
        if (empty($calendarAccountId)) {
            throw new InvalidArgumentException('Calendar Account ID is required');
        }

        /** @var CalendarAccount|false $calendarAccount */
        $calendarAccount = BeanFactory::getBean('CalendarAccount', $calendarAccountId);

        if (!$calendarAccount || empty($calendarAccount->id)) {
            throw new RuntimeException('Calendar Account not found');
        }

        $accountId = $calendarAccount->id;

        $userId = $calendarAccount->calendar_user_id;
        $accountName = $calendarAccount->name;

        if ($calendarAccount->deleted) {
            throw new RuntimeException('Calendar Account connection is deleted.');
        }

        if (empty($userId)) {
            throw new RuntimeException("Calendar account $accountId ($accountName) has no user ID assigned");
        }

        return $calendarAccount;
    }

}