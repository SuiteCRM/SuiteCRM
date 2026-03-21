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
 * Factory class responsible for creating CalendarEventQuery instances.
 * Used to set synchronization windows for past and future events.
 */
class CalendarEventQueryFactory
{

    /**
     * Prepares a query to retrieve calendar events within a specified date range.
     *
     * @param int $pastDays Number of days in the past to include in the query. Must be 0 or greater.
     * @param int $futureDays Number of days in the future to include in the query. Must be greater than 0.
     * @param int|null $limit Optional limit for the number of events to retrieve.
     * @return CalendarEventQuery The query object containing the specified date range and limit.
     * @throws InvalidArgumentException If $pastDays is negative or $futureDays is not greater than 0.
     */
    public function forSyncWindows(int $pastDays, int $futureDays, ?int $limit = null): CalendarEventQuery
    {
        if ($pastDays < 0) {
            throw new InvalidArgumentException('Past days must be 0 or greater');
        }
        if ($futureDays <= 0) {
            throw new InvalidArgumentException('Future days must be greater than 0');
        }

        $today = new DateTime('today');
        $startDate = (clone $today)->modify("-$pastDays days");
        $endDate = (clone $today)->modify("+$futureDays days")->setTime(23, 59, 59);

        return new CalendarEventQuery(
            startDate: $startDate,
            endDate: $endDate,
            limit: $limit
        );
    }

}