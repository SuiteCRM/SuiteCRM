<?php
/**
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License along with
 * this program; if not, see http://www.gnu.org/licenses or write to the Free
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 *
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */
// PHP will throw a fatal error if attempting to use the DateTime class without this param being set
date_default_timezone_set('UTC');

/**
 * This class is used to build the objects to be passed to the FullCalendar. Inspired on FullCalendar examples
 */
class CalendarObject
{

    // Tests whether the given ISO8601 string has a time-of-day or not
    public const ALL_DAY_REGEX = '/^\d{4}-\d\d-\d\d$/'; // matches strings like "2013-12-29"

    public $title;
    public $allDay; // a boolean
    public $start; // a DateTime
    public $end; // a DateTime, or null
    public $properties = array(); // an array of other misc properties

    // Constructs a CalendarObject from the given array of key=>values.
    // Can optionally force the timeZone of the parsed dates.
    public function __construct($array, $timeZone = null)
    {

        $this->title = $array['title'];

        if (isset($array['allDay'])) {
            // allDay has been explicitly specified
            $this->allDay = (bool) $array['allDay'];
        } else {
            // Guess allDay based off of ISO8601 date strings
            $this->allDay = preg_match(self::ALL_DAY_REGEX, $array['start']) &&
                (!isset($array['end']) || preg_match(self::ALL_DAY_REGEX, $array['end']));
        }

        if (!$this->allDay) {
            // If dates are allDay, parse them in UTC to avoid DST issues.
            $timeZone = null;
        }

        // Parse dates
        $this->start = parseDateTime($array['start'], $timeZone);
        $this->end = isset($array['end']) ? parseDateTime($array['end'], $timeZone) : null;

        // Record misc properties
        foreach ($array as $name => $value) {
            if (!in_array($name, array('title', 'allDay', 'start', 'end'))) {
                $this->properties[$name] = $value;
            }
        }
    }

    // Returns whether the date range of a CalendarObject intersects with the given all-day range.
    // $rangeStart and $rangeEnd are assumed to be dates in UTC with 00:00:00 time.
    public function isWithinDayRange($rangeStart, $rangeEnd)
    {
        // Normalize the CalendarObject's dates for later comparison with the all-day range
        $calendarObjectStart = stripTime($this->start);

        if (isset($this->end)) {
            $calendarObjectEnd = stripTime($this->end); // normalize
        } else {
            $calendarObjectEnd = $calendarObjectStart; // consider this a zero-duration CalendarObject
        }

        // Check if the two whole-day ranges intersect
        return $calendarObjectStart < $rangeEnd && $calendarObjectEnd >= $rangeStart;
    }

    // Converts this CalendarObject back to a plain data array, to be used for generating JSON
    public function toArray()
    {
        // Start with the misc properties (don't worry, PHP won't affect the original array)
        $array = $this->properties;

        $array['title'] = $this->title;

        // Figure out the date format. This essentially encodes allDay into the date string.
        if ($this->allDay) {
            $offset = $this->start->getOffset();
            $myInterval = DateInterval::createFromDateString((string) $offset . 'seconds');
            $this->start->add($myInterval);
            $offset = $this->end->getOffset();
            $myInterval = DateInterval::createFromDateString((string) $offset . 'seconds');
            $this->end->add($myInterval);
            $format = 'Y-m-d'; // output like "2013-12-29"
        } else {
            $format = 'c'; // full ISO8601 output, like "2013-12-29T09:00:00+08:00"
        }

        // Serialize dates into strings and removing timezone offset
        $array['start'] = explode('+', $this->start->format($format))[0];
        if (isset($this->end)) {
            $array['end'] = explode('+', $this->end->format($format))[0];
        }

        return $array;
    }

}

// Date Utilities
// --------------

// Parses a string into a DateTime object, optionally forced into the given timeZone
function parseDateTime($string, $timeZone = null)
{
    global $timedate, $current_user;

    $date = new DateTime($string);
    $date = $timedate->tzUser($date, $current_user);
    return $date;
}

// Takes the year/month/date values of the given DateTime and convert them to a new UTC DateTime
function stripTime($datetime)
{
    return new DateTime($datetime->format('Y-m-d'));
}
