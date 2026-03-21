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
 * Represents a query for retrieving calendar events with optional filters such as start date, end date, calendar ID, and limit.
 *
 * This class provides methods to define query parameters and transform the data into an array format for compatibility.
 */
class CalendarEventQuery
{

    public function __construct(
        protected ?DateTime $startDate = null,
        protected ?DateTime $endDate = null,
        protected ?string $calendarId = null,
        protected ?int $limit = null
    ) {
        if ($this->startDate && $this->endDate && $this->startDate > $this->endDate) {
            throw new InvalidArgumentException('Start date must be before or equal to end date');
        }

        if ($this->limit !== null && $this->limit <= 0) {
            throw new InvalidArgumentException('Limit must be greater than 0');
        }
    }

    public function getStartDate(): ?DateTime
    {
        return $this->startDate;
    }

    public function getEndDate(): ?DateTime
    {
        return $this->endDate;
    }

    public function getLimit(): ?int
    {
        return $this->limit;
    }

    public function toArray(): array
    {
        $options = [];

        if ($this->startDate) {
            $options['start_date'] = $this->startDate->format('Y-m-d\TH:i:s\Z');
        }

        if ($this->endDate) {
            $options['end_date'] = $this->endDate->format('Y-m-d\TH:i:s\Z');
        }

        if ($this->calendarId) {
            $options['calendar_id'] = $this->calendarId;
        }

        if ($this->limit) {
            $options['limit'] = $this->limit;
        }

        return $options;
    }

    /**
     * Converts the query to a string representation for logging.
     *
     * @return string
     */
    public function toString(): string
    {
        $parts = [];

        if ($this->startDate) {
            $parts[] = 'start=' . $this->startDate->format('Y-m-d H:i:s');
        }

        if ($this->endDate) {
            $parts[] = 'end=' . $this->endDate->format('Y-m-d H:i:s');
        }

        if ($this->calendarId) {
            $parts[] = 'calendar=' . $this->calendarId;
        }

        if ($this->limit) {
            $parts[] = 'limit=' . $this->limit;
        }

        return !empty($parts) ? implode(', ', $parts) : 'no filters';
    }

    /**
     * Magic method to convert the query to a string.
     *
     * @return string
     */
    public function __toString(): string
    {
        return $this->toString();
    }

}