<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUGARCRM, SUGARCRM DISCLAIMS THE WARRANTY
 * OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
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
 * You can contact SugarCRM, Inc. headquarters at 10050 North Wolfe Road,
 * SW2-130, Cupertino, CA 95014, USA. or at email address contact@sugarcrm.com.
 *
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo and "Supercharged by SuiteCRM" logo. If the display of the logos is not
 * reasonably feasible for technical reasons, the Appropriate Legal Notices must
 * display the words "Powered by SugarCRM" and "Supercharged by SuiteCRM".
 */


/**
 * Sugar DateTime container
 * Extends regular PHP DateTime with useful services
 * @api
 */
#[\AllowDynamicProperties]
class SugarDateTime extends DateTime
{
    // Recognized properties and their formats
    protected $formats = array(
        "sec" => "s",
        "min" => "i",
        "hour" => "G",
        "zhour" => "H",
        "day" => "j",
        "zday" => "d",
        "days_in_month" => "t",
        "day_of_week" => "w",
        "day_of_year" => "z",
        "week" => "W",
        "month" => "n",
        "zmonth" => "m",
        "year" => "Y",
        "am_pm" => "A",
        "hour_12" => "g",
    );

    // Property aliases
    protected $var_gets = array(
        "24_hour" => "hour",
        "day_of_week" => "day_of_week_long",
        "day_of_week_short" => "day_of_week_short",
        "month_name" => "month_long",
        "hour" => "hour_12",
    );

    /**
     * @var DateTimeZone
     */
    protected static $_gmt;

    /**
     * Calendar strings
     * @var array
     */
    protected $_strings;

    /**
     * For testing - if we allowed to use PHP date parse
     * @var bool
     */
    public static $use_php_parser = true;

    /**
     * For testing - if we allowed to use strptime()
     * @var bool
     */
    public static $use_strptime = true;

    /**
     * Copy of DateTime::createFromFormat
     *
     * Needed to return right type of the object
     *
     * @param string $format Format like in date()
     * @param string $time Time to parse
     * @param DateTimeZone $timezone
     * @return SugarDateTime
     * @see DateTime::createFromFormat
     */
    #[ReturnTypeWillChange]
    public static function createFromFormat($format, $time, $timezone = null)
    {
        if (empty($time) || empty($format)) {
            return false;
        }
        if (self::$use_php_parser && is_callable(array("DateTime", "createFromFormat"))) {
            // 5.3, hurray!
            if (!empty($timezone)) {
                $d = parent::createFromFormat($format, $time, $timezone);
            } else {
                $d = parent::createFromFormat($format, $time);
            }
        } else {
            // doh, 5.2, will have to simulate
            $d = self::_createFromFormat($format, $time, $timezone);
        }
        if (!$d) {
            return false;
        }
        $sd = new self($d->format(DateTime::ISO8601));
        $sd->setTimezone($d->getTimezone());
        return $sd;
    }

    /**
     * Internal _createFromFormat implementation for 5.2
     *
     * @internal
     * @deprecated No longer necessary since PHP 5.2 is no longer supported.
     * @param string $format Format like in date()
     * @param string $time Time string to parse
     * @param \DateTimeZone|null $timezone TZ
     * @return SugarDateTime
     * @see DateTime::createFromFormat
     */
    protected static function _createFromFormat($format, $time, DateTimeZone $timezone = null)
    {
        $res = new self();
        if (!empty($timezone)) {
            $res->setTimezone($timezone);
        }
        if (self::$use_strptime && function_exists("strptime")) {
            $str_format = str_replace(array_keys(TimeDate::$format_to_str), array_values(TimeDate::$format_to_str), $format);
            // for a reason unknown to modern science, %P doesn't work in strptime
            $str_format = str_replace("%P", "%p", $str_format);
            // strip spaces before am/pm as our formats don't have them
            $time = preg_replace('/\s+(AM|PM)/i', '\1', $time);
            // TODO: better way to not risk locale stuff problems?
            $data = strptime($time, $str_format);
            if (empty($data)) {
                $GLOBALS['log']->error("Cannot parse $time for format $format");
                return null;
            }
            if ($data["tm_year"] == 0) {
                unset($data["tm_year"]);
            }
            if ($data["tm_mday"] == 0) {
                unset($data["tm_mday"]);
            }
            if (isset($data["tm_year"])) {
                $data["tm_year"] += 1900;
            }
            if (isset($data["tm_mon"])) {
                $data["tm_mon"]++;
            }
            $data += self::$data_init; // fill in missing parts
        } else {
            // Windows, etc. might not have strptime - we'd have to work harder here
            $data = $res->_strptime($time, $format);
        }
        if (empty($data)) {
            $GLOBALS['log']->error("Cannot parse $time for format $format");
            return null;
        }
        if (isset($data["tm_year"])) {
            $res->setDate($data["tm_year"], $data["tm_mon"], $data["tm_mday"]);
        }
        $res->setTime($data["tm_hour"], $data["tm_min"], $data["tm_sec"]);
        return $res;
    }

    /**
     * Load language Calendar strings
     * @internal
     * @param string $name string section to return
     * @return array
     */
    protected function _getStrings($name)
    {
        if (empty($this->_strings)) {
            $this->_strings = return_mod_list_strings_language($GLOBALS['current_language'], "Calendar");
        }
        return $this->_strings[$name];
    }

    /**
     * Fetch property of the date by name
     * @param string $var Property name
     * @return mixed
     */
    public function __get($var)
    {
        // simple formats
        if (isset($this->formats[$var])) {
            return $this->format($this->formats[$var]);
        }
        // conditional, derived and translated ones
        switch ($var) {
            case "ts":
                return $this->format("U")+0;
            case "tz_offset":
                return $this->getTimezone()->getOffset($this);
            case "days_in_year":
                return $this->format("L") == '1'?366:365;
                break;
            case "day_of_week_short":
                $str = $this->_getStrings('dom_cal_weekdays');
                return $str[$this->day_of_week];
            case "day_of_week_long":
                $str = $this->_getStrings('dom_cal_weekdays_long');
                return $str[$this->day_of_week];
            case "month_short":
                $str = $this->_getStrings('dom_cal_month');
                return $str[$this->month];
            case "month_long":
                $str = $this->_getStrings('dom_cal_month_long');
                return $str[$this->month];
        }

        return '';
    }

    /**
     * Implement some get_ methods that fetch variables
     *
     * @param string $name
     * @param array $args
     * @return mixed
     */
    public function __call($name, $args)
    {
        // fill in 5.2.x gaps
        if ($name == "getTimestamp") {
            return $this->format('U')+0;
        }
        if ($name == "setTimestamp") {
            $sec = (int)$args[0];
            $sd = new self("@$sec");
            $sd->setTimezone($this->getTimezone());
            return $sd;
        }

        // getters
        if (substr($name, 0, 4) == "get_") {
            $var = substr($name, 4);

            if (isset($this->var_gets[$var])) {
                return $this->__get($this->var_gets[$var]);
            }

            if (isset($this->formats[$var])) {
                return $this->__get($var);
            }
        }
        $GLOBALS['log']->fatal("SugarDateTime: unknowm method $name called");
        sugar_die("SugarDateTime: unknowm method $name called");
        return false;
    }

    /**
     * Get specific hour of today.
     *
     * @deprecated This function is unused and will be removed in a future release.
     * @param int $hour_index
     * @return SugarDateTime
     */
    public function get_datetime_by_index_today($hour_index)
    {
        if ($hour_index < 0 || $hour_index > 23) {
            sugar_die("hour is outside of range");
        }

        $newdate = clone $this;
        $newdate->setTime($hour_index, 0, 0);
        return $newdate;
    }

    /**
     * Get the last second of current hour
     *
     * @deprecated This function is unused and will be removed in a future release.
     * @return SugarDateTime
     */
    public function get_hour_end_time()
    {
        $newdate = clone $this;
        $newdate->setTime($this->hour, 59, 59);
        return $newdate;
    }

    /**
     * Get the last second of the current day
     *
     * @deprecated This function is unused and will be removed in a future release.
     * @return SugarDateTime
     */
    public function get_day_end_time()
    {
        $newdate = clone $this;
        return $newdate->setTime(23, 59, 59);
    }

    /**
     * Get the beginning of i's day of the week
     * @param int $day_index Day, 0 is Sunday, 1 is Monday, etc.
     * @return SugarDateTime
     */
    public function get_day_by_index_this_week($day_index)
    {
        $newdate = clone $this;
        $newdate->setDate($this->year, $this->month, $this->day +
            ($day_index - $this->day_of_week))->setTime(0, 0);
        return $newdate;
    }

    /**
     * Get the beginning of the last day of i's the month
     *
     * @deprecated This function is unused and will be removed in a future release.
     * @param int $month_index Month, January is 0
     * @return SugarDateTime
     */
    public function get_day_by_index_this_year($month_index)
    {
        $newdate = clone $this;
        $newdate->setDate($this->year, $month_index+1, 1);
        $newdate->setDate($newdate->year, $newdate->month, $newdate->days_in_month);
        $newdate->setTime(0, 0);
        return $newdate;
    }

    /**
     * Get the beginning of i's day of the month
     *
     * @param int $day_index 0 is the first day of the month (sic!)
     * @return SugarDateTime
     */
    public function get_day_by_index_this_month($day_index)
    {
        $newdate = clone $this;
        return $newdate->setDate($this->year, $this->month, $day_index+1)->setTime(0, 0);
    }

    /**
     * Get new date, modified by date expression
     *
     * @example $yesterday = $today->get("yesterday");
     *
     * @param string $expression
     * @return SugarDateTime
     */
    public function get($expression)
    {
        $newdate = clone $this;
        $newdate->modify($expression);
        return $newdate;
    }

    /**
     * Create from ISO 8601 datetime
     * @param string $str
     * @return SugarDateTime
     */
    public static function parse_utc_date_time($str)
    {
        return new self($str);
    }

    /**
     * Create a list of time slots for calendar view
     * Times must be in user TZ
     *
     * @deprecated This function is unused and will be removed in a future release.
     * @param string $view Which view we are using - day, week, month
     * @param SugarDateTime $start_time Start time
     * @param SugarDateTime $end_time End time
     * @return array
     */
    public static function getHashList($view, $start_time, $end_time)
    {
        $hash_list = array();

        if ($view != 'day') {
            $end_time = $end_time->get_day_end_time();
        }

        $end = $end_time->ts;
        if ($end <= $start_time->ts) {
            $end = $start_time->ts+1;
        }

        $new_time = clone $start_time;
        $new_time->setTime($new_time->hour, 0, 0);

        while ($new_time->ts < $end) {
            if ($view == 'day') {
                $hash_list[] = $new_time->format(TimeDate::DB_DATE_FORMAT) . ":" . $new_time->hour;
                $new_time->modify("next hour");
            } else {
                $hash_list[] = $new_time->format(TimeDate::DB_DATE_FORMAT);
                $new_time->modify("next day");
            }
        }

        return $hash_list;
    }

    /**
     * Get the beginning of the given day
     *
     * @param int $day  Day, starting with 1, default is current
     * @param int $month Month, starting with 1, default is current
     * @param int $year Year, default is current
     * @return SugarDateTime
     */
    public function get_day_begin($day = null, $month = null, $year = null)
    {
        $newdate = clone $this;
        $newdate->setDate(
            $year?$year:$this->year,
            $month?$month:$this->month,
            $day?$day:$this->day
        );
        $newdate->setTime(0, 0);
        return $newdate;
    }

    /**
     * Get the last second of the given day
     *
     * @param int $day  Day, starting with 1, default is current
     * @param int $month Month, starting with 1, default is current
     * @param int $year Year, default is current
     * @return SugarDateTime
     */
    public function get_day_end($day = null, $month = null, $year = null)
    {
        $newdate = clone $this;
        $newdate->setDate(
            $year?$year:$this->year,
            $month?$month:$this->month,
            $day?$day:$this->day
        );
        $newdate->setTime(23, 59, 59);
        return $newdate;
    }

    /**
     * Get the beginning of the first day of the year
     *
     * @deprecated This function is unused and will be removed in a future release.
     * @param int $year
     * @return SugarDateTime
     */
    public function get_year_begin($year)
    {
        $newdate = clone $this;
        $newdate->setDate($year, 1, 1);
        $newdate->setTime(0, 0);
        return $newdate;
    }

    /**
     * Print datetime in standard DB format
     *
     * Set $tz parameter to false if you are sure that the date is in UTC.
     *
     * @param bool $tz do conversion to UTC
     * @return string
     */
    public function asDb($tz = true)
    {
        if ($tz) {
            if (empty(self::$_gmt)) {
                self::$_gmt = new DateTimeZone("UTC");
            }
            $this->setTimezone(self::$_gmt);
        }
        return $this->format(TimeDate::DB_DATETIME_FORMAT);
    }

    /**
     * Print date in standard DB format
     *
     * Set $tz parameter to false if you are sure that the date is in UTC.
     *
     * @param bool $tz do conversion to UTC
     * @return string
     */
    public function asDbDate($tz = true)
    {
        if ($tz) {
            if (empty(self::$_gmt)) {
                self::$_gmt = new DateTimeZone("UTC");
            }
            $this->setTimezone(self::$_gmt);
        }
        return $this->format(TimeDate::DB_DATE_FORMAT);
    }

    /**
     * Get query string for the date, year=%d&month=%d&day=%d&hour=%d
     *
     * @return string
     */
    public function get_date_str()
    {
        return sprintf("&year=%d&month=%d&day=%d&hour=%d", $this->year, $this->month, $this->day, $this->hour);
    }

    /**
     * Convert date to string - 'r' format, like: Thu, 21 Dec 2000 16:01:07 +0200
     * @return string
     */
    public function __toString()
    {
        return $this->format('r');
    }

    /**
     * Match between tm_ parts and date() format strings
     * @var array
     */
    protected static $parts_match = array(
            'Y' => 'tm_year',
            'm' => 'tm_mon',
            'n' => 'tm_mon',
            'd' => 'tm_mday',
            'H' => 'tm_hour',
            'h' => 'tm_hour',
            'i' => 'tm_min',
            's' => 'tm_sec',
    );

    protected static $data_init = array(
        "tm_hour" => 0,
        "tm_min" => 0,
        "tm_sec" => 0,
    );

    protected static $strptime_short_mon;
    protected static $strptime_long_mon;

    /**
     * DateTime homebrew parser
     *
     * Since some OSes and PHP versions (please upgrade to 5.3!) do not support built-in parsing functions,
     * we have to restort to this ugliness.
     *
     * @deprecated This function is unnecessary since we no longer support PHP 5.2, and will be removed in a future release.
     * @internal
     * @param string $time  Time formatted string
     * @param string $format Format, as accepted by strptime()
     * @return array Parsed parts
     */
    protected function _strptime($time, $format)
    {
        $data = self::$data_init;
        if (empty(self::$strptime_short_mon)) {
            self::$strptime_short_mon = array_flip($this->_getStrings('dom_cal_month'));
            unset(self::$strptime_short_mon[""]);
        }
        if (empty(self::$strptime_long_mon)) {
            self::$strptime_long_mon = array_flip($this->_getStrings('dom_cal_month_long'));
            unset(self::$strptime_long_mon[""]);
        }

        $regexp = TimeDate::get_regular_expression($format);
        if (!preg_match('@'.$regexp['format'].'@', $time, $dateparts)) {
            return false;
        }

        foreach (self::$parts_match as $part => $datapart) {
            if (isset($regexp['positions'][$part]) && isset($dateparts[$regexp['positions'][$part]])) {
                $data[$datapart] = (int)$dateparts[$regexp['positions'][$part]];
            }
        }
        // now process non-numeric ones
        if (isset($regexp['positions']['F']) && !empty($dateparts[$regexp['positions']['F']])) {
            // FIXME: locale?
            $mon = $dateparts[$regexp['positions']['F']];
            if (isset(self::$sugar_strptime_long_mon[$mon])) {
                $data["tm_mon"] = self::$sugar_strptime_long_mon[$mon];
            } else {
                return false;
            }
        }
        if (isset($regexp['positions']['M']) && !empty($dateparts[$regexp['positions']['M']])) {
            // FIXME: locale?
            $mon = $dateparts[$regexp['positions']['M']];
            if (isset(self::$sugar_strptime_short_mon[$mon])) {
                $data["tm_mon"] = self::$sugar_strptime_short_mon[$mon];
            } else {
                return false;
            }
        }
        if (isset($regexp['positions']['a']) && !empty($dateparts[$regexp['positions']['a']])) {
            $ampm = trim($dateparts[$regexp['positions']['a']]);
            if ($ampm == 'pm') {
                if ($data["tm_hour"] != 12) {
                    $data["tm_hour"] += 12;
                }
            } else {
                if ($ampm == 'am') {
                    if ($data["tm_hour"] == 12) {
                        // 12:00am is 00:00
                        $data["tm_hour"] = 0;
                    }
                } else {
                    return false;
                }
            }
        }

        if (isset($regexp['positions']['A']) && !empty($dateparts[$regexp['positions']['A']])) {
            $ampm = trim($dateparts[$regexp['positions']['A']]);
            if ($ampm == 'PM') {
                if ($data["tm_hour"] != 12) {
                    $data["tm_hour"] += 12;
                }
            } else {
                if ($ampm == 'AM') {
                    if ($data["tm_hour"] == 12) {
                        // 12:00am is 00:00
                        $data["tm_hour"] = 0;
                    }
                } else {
                    return false;
                }
            }
        }

        return $data;
    }
}
