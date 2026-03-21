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
 * Helper class for DateTime creation and manipulation in Calendar synchronization.
 */
class DateTimeHelper
{

    /**
     * Creates a DateTime instance from the given input.
     *
     * @param DateTime|string|int|null $date The input value, which can be a string representation of a date,
     *                              a timestamp (in milliseconds), or null.
     * @return DateTime|null A DateTime object if the input is valid or null if invalid or empty.
     */
    public function createDateTime(DateTime|string|int|null $date): ?DateTime
    {
        if (is_a($date, DateTime::class)) {
            return $date;
        }

        if (empty($date)) {
            return null;
        }

        if (is_int($date)) {
            $ts = $date / 1000.0;
            return new DateTime("@$ts");
        }

        try {
            if ($this->isTimezoneAwareString($date)) {
                $dateTime = new DateTime($date);
            } else {
                $dateTime = new DateTime($date, new DateTimeZone('UTC'));
            }

            return $dateTime;
        } catch (Throwable) {
            return null;
        }
    }

    /**
     * Determines if a date string contains explicit timezone information.
     *
     * @param string $dateString The date string to check.
     * @return bool True if the string contains timezone info, false otherwise.
     */
    protected function isTimezoneAwareString(string $dateString): bool
    {
        return (bool) preg_match('/[+-]\d{2}:\d{2}$|[+-]\d{4}$|Z$/i', $dateString);
    }

}