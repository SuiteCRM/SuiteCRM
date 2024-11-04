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


if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once 'include/SugarDateTime.php';

/**
 * New Time & Date handling class
 * @api
 * Migration notes:
 * - to_db_time() requires either full datetime or time, won't work with just date
 *    The reason is that it's not possible to know if short string has only date or only time,
 *     and it makes more sense to assume time for the time conversion function.
 */
#[\AllowDynamicProperties]
class TimeDate
{
    public const DB_DATE_FORMAT = 'Y-m-d';
    public const DB_TIME_FORMAT = 'H:i:s';
    // little optimization
    public const DB_DATETIME_FORMAT = 'Y-m-d H:i:s';
    public const RFC2616_FORMAT = 'D, d M Y H:i:s \G\M\T';

    public const SECONDS_IN_A_DAY = 86400;

    // Standard DB date/time formats
    // they are constant, vars are for compatibility
    public $dbDayFormat = self::DB_DATE_FORMAT;
    public $dbTimeFormat = self::DB_TIME_FORMAT;

    /**
     * Regexp for matching format elements
     * @var array
     */
    protected static $format_to_regexp = array(
        'a' => '[ ]*[ap]m',
        'A' => '[ ]*[AP]M',
        'd' => '[0-9]{1,2}',
        'j' => '[0-9]{1,2}',
        'h' => '[0-9]{1,2}',
        'H' => '[0-9]{1,2}',
        'g' => '[0-9]{1,2}',
        'G' => '[0-9]{1,2}',
        'i' => '[0-9]{1,2}',
        'm' => '[0-9]{1,2}',
        'n' => '[0-9]{1,2}',
        'Y' => '[0-9]{4}',
        's' => '[0-9]{1,2}',
        'F' => '\w+',
        "M" => '[\w]{1,3}',
    );

    /**
     * Relation between date() and strftime() formats
     * @var array
     */
    public static $format_to_str = array(
        // date
        'Y' => '%Y',

        'm' => '%m',
        'M' => '%b',
        'F' => '%B',
        'n' => '%m',

        'd' => '%d',
        //'j' => '%e',
        // time
        'a' => '%P',
        'A' => '%p',

        'h' => '%I',
        'H' => '%H',
        //'g' => '%l',
        //'G' => '%H',

        'i' => '%M',
        's' => '%S',
    );

    /**
     * GMT timezone object
     *
     * @var DateTimeZone
     */
    protected static $gmtTimezone;

    /**
     * Current time
     * @var SugarDateTime
     */
    protected $now;

    /**
     * The current user
     *
     * @var User
     */
    protected $user;

    /**
     * Current user's ID
     *
     * @var string
     */
    protected $current_user_id;
    /**
     * Current user's TZ
     * @var DateTimeZone
     */
    protected $current_user_tz;

    /**
     * Separator for current user time format
     *
     * @var string
     */
    protected $time_separator;

    /**
     * Always consider user TZ to be GMT and date format DB format - for SOAP etc.
     *
     * @var bool
     */
    protected $always_db = false;

    /**
     * Global instance of TimeDate
     * @var TimeDate
     */
    protected static $timedate;

    /**
     * Allow returning cached now() value
     * If false, new system time is checked each time now() is required
     * If true, same value is returned for whole request.
     * Also, current user's timezone is cached.
     * @var bool
     */
    public $allow_cache = true;

    /**
     * Create TimeDate handler
     * @param User|null $user User to work with, default if current user
     */
    public function __construct(User $user = null)
    {
        if (self::$gmtTimezone == null) {
            self::$gmtTimezone = new DateTimeZone("UTC");
        }
        $this->now = new SugarDateTime();
        $this->tzGMT($this->now);
        $this->user = $user;
    }

    /**
     * Set flag specifying we should always use DB format
     * @param bool $flag
     * @return TimeDate
     */
    public function setAlwaysDb($flag = true)
    {
        $this->always_db = $flag;
        $this->clearCache();

        return $this;
    }

    /**
     * Get "always use DB format" flag
     * @return bool
     */
    public function isAlwaysDb()
    {
        return !empty($GLOBALS['disable_date_format']) || $this->always_db;
    }

    /**
     * Get TimeDate instance
     * @return TimeDate
     */
    public static function getInstance()
    {
        if (empty(self::$timedate)) {
            if (ini_get('date.timezone') == '') {
                // Remove warning about default timezone
                date_default_timezone_set(@date('e'));
                try {
                    $tz = self::guessTimezone();
                } catch (Exception $e) {
                    $tz = "UTC"; // guess failed, switch to UTC
                }
                if (isset($GLOBALS['log'])) {
                    $GLOBALS['log']->warn("Configuration variable date.timezone is not set, guessed timezone $tz. Please set date.timezone=\"$tz\" in php.ini!");
                }
                date_default_timezone_set($tz);
            }
            self::$timedate = new self;
        }

        return self::$timedate;
    }

    /**
     * Set current user for this object
     *
     * @param User|null $user User object, default is current user
     * @return TimeDate
     */
    public function setUser(User $user = null)
    {
        $this->user = $user;
        $this->clearCache();

        return $this;
    }

    /**
     * Figure out what the required user is
     *
     * The order is: supplied parameter, TimeDate's user, global current user
     *
     * @param User|null $user User object, default is current user
     * @return User
     * @internal
     */
    protected function _getUser(User $user = null)
    {
        if ($user === null) {
            $user = $this->user;
        }
        if ($user === null && isset($GLOBALS['current_user'])) {
            $user = $GLOBALS['current_user'];
        }

        return $user;
    }

    /**
     * Get timezone for the specified user
     *
     * @param User|null $user User object, default is current user
     * @return DateTimeZone
     */
    protected function _getUserTZ(User $user = null)
    {
        $user = $this->_getUser($user);
        if (empty($user) || $this->isAlwaysDb()) {
            return self::$gmtTimezone;
        }

        if ($this->allow_cache && $user->id === $this->current_user_id && !empty($this->current_user_tz)) {
            // current user is cached
            return $this->current_user_tz;
        }

        $usertimezone = $user->getPreference('timezone');
        if (empty($usertimezone)) {
            return self::$gmtTimezone;
        }
        try {
            $tz = new DateTimeZone($usertimezone);
        } catch (Exception $e) {
            $GLOBALS['log']->fatal('Unknown timezone: ' . $usertimezone);

            return self::$gmtTimezone;
        }

        if (empty($this->current_user_id)) {
            $this->current_user_id = $user->id;
            $this->current_user_tz = $tz;
        }

        return $tz;
    }

    /**
     * Clears all cached data regarding current user
     */
    public function clearCache()
    {
        $this->current_user_id = null;
        $this->current_user_tz = null;
        $this->time_separator = null;
        $this->now = new SugarDateTime();
    }

    /**
     * Get user date format.
     * @todo add caching
     *
     * @param User|null $user user object, current user if not specified
     * @return string
     */
    public function get_date_format(User $user = null)
    {
        $user = $this->_getUser($user);

        if (empty($user) || $this->isAlwaysDb()) {
            return self::DB_DATE_FORMAT;
        }

        $datef = $user->getPreference('datef');
        if (empty($datef) && isset($GLOBALS['current_user']) && $GLOBALS['current_user'] !== $user) {
            // if we got another user and it has no date format, try current user
            $datef = $GLOBALS['current_user']->getPreference('datef');
        }
        if (empty($datef)) {
            $datef = $GLOBALS['sugar_config']['default_date_format'];
        }
        if (empty($datef)) {
            $datef = '';
        }

        return $datef;
    }

    /**
     * Get user time format.
     * @todo add caching
     *
     * @param User $user user object, current user if not specified
     * @return string
     */
    public function get_time_format(/*User*/
        $user = null
    ) {
        if (is_bool($user) || func_num_args() > 1) {
            // BC dance - old signature was boolean, User
            $GLOBALS['log']->fatal('TimeDate::get_time_format(): Deprecated API used, please update you code - get_time_format() now has one argument of type User');
            if (func_num_args() > 1) {
                $user = func_get_arg(1);
            } else {
                $user = null;
            }
        }
        $user = $this->_getUser($user);

        if (empty($user) || $this->isAlwaysDb()) {
            return self::DB_TIME_FORMAT;
        }

        $timef = $user->getPreference('timef');
        if (empty($timef) && isset($GLOBALS['current_user']) && $GLOBALS['current_user'] !== $user) {
            // if we got another user and it has no time format, try current user
            $timef = $GLOBALS['current_user']->getPreference('timef');
        }
        if (empty($timef)) {
            $timef = $GLOBALS['sugar_config']['default_time_format'];
        }
        if (empty($timef)) {
            $timef = '';
        }

        return $timef;
    }

    /**
     * Get user datetime format.
     *
     * @param User $user user object, current user if not specified
     * @return string
     */
    public function get_date_time_format($user = null)
    {
        // BC fix - had (bool, user) signature before
        if (!($user instanceof User)) {
            if (func_num_args() > 1) {
                $user = func_get_arg(1);
                if (!($user instanceof User)) {
                    $user = null;
                }
            } else {
                $user = null;
            }
        }

        $cacheKey = $this->get_date_time_format_cache_key($user);
        $cachedValue = sugar_cache_retrieve($cacheKey);

        if (!empty($cachedValue)) {
            return $cachedValue;
        } else {
            $value = $this->merge_date_time($this->get_date_format($user), $this->get_time_format($user));
            sugar_cache_put($cacheKey, $value, 0);

            return $value;
        }
    }

    /**
     * Retrieve the cache key used for user date/time formats
     *
     * @param $user
     * @return string
     */
    public function get_date_time_format_cache_key($user)
    {
        $cacheKey = get_class($this) . "dateTimeFormat";
        $user = $this->_getUser($user);

        if ($user instanceof User) {
            $cacheKey .= "_{$user->id}";
        }

        if ($this->isAlwaysDb()) {
            $cacheKey .= '_asdb';
        }

        return $cacheKey;
    }

    /**
     * Get user's first day of week setting.
     *
     * @param User|null $user user object, current user if not specified
     * @return int Day, 0 = Sunday, 1 = Monday, etc...
     */
    public function get_first_day_of_week(User $user = null)
    {
        $user = $this->_getUser($user);
        $fdow = 0;

        if (!empty($user)) {
            $fdow = $user->getPreference('fdow');
            if (empty($fdow)) {
                $fdow = 0;
            }
        }

        return $fdow;
    }


    /**
     * Make one datetime string from date string and time string
     *
     * @param string $date
     * @param string $time
     * @return string New datetime string
     */
    public function merge_date_time($date, $time)
    {
        return $date . ' ' . $time;
    }

    /**
     * Split datetime string into date & time
     *
     * @param string $datetime
     * @return array
     */
    public function split_date_time($datetime)
    {
        return explode(' ', $datetime, 2);
    }

    /**
     * Get user date format in Javascript form
     * @return string
     */
    public function get_cal_date_format()
    {
        return $this->getCalFormat($this->get_date_format());
    }

    /**
     * Get user time format in Javascript form
     * @return string
     */
    public function get_cal_time_format()
    {
        return $this->getCalFormat($this->get_time_format());
    }

    /**
     * Get user date&time format in Javascript form
     * @return string
     */
    public function get_cal_date_time_format()
    {
        return $this->getCalFormat($this->get_date_time_format());
    }

    /**
     * @param string $format
     * @return string
     */
    public function getCalFormat($format)
    {
        return str_replace(array_keys(self::$format_to_str), array_values(self::$format_to_str), $format);
    }

    /**
     * Verify if the date string conforms to a format
     *
     * @param string $date
     * @param string $format Format to check
     *
     * @internal
     * @return bool Is the date ok?
     */
    public function check_matching_format($date, $format)
    {
        try {
            $dt = SugarDateTime::createFromFormat($format, $date);
            if (!is_object($dt)) {
                return false;
            }
        } catch (Exception $e) {
            return false;
        }

        return true;
    }

    /**
     * Format DateTime object as DB datetime
     *
     * @param DateTime $date
     * @return string
     */
    public function asDb(DateTime $date)
    {
        $date->setTimezone(self::$gmtTimezone);

        return $date->format($this->get_db_date_time_format());
    }

    /**
     * Format date as DB-formatted field type
     * @param DateTime $date
     * @param string $type Field type - date, time, datetime[combo]
     * @return string Formatted date
     */
    public function asDbType(DateTime $date, $type)
    {
        switch ($type) {
            case "date":
                return $this->asDbDate($date);
                break;
            case 'time':
                return $this->asDbtime($date);
                break;
            case 'datetime':
            case 'datetimecombo':
            default:
                return $this->asDb($date);
        }
    }

    /**
     * Format DateTime object as user datetime
     *
     * @param DateTime $date
     * @param User|null $user
     * @return string
     */
    public function asUser(DateTime $date, User $user = null)
    {
        $this->tzUser($date, $user);

        return $date->format($this->get_date_time_format($user));
    }

    /**
     * Format date as user-formatted field type
     * @param DateTime $date
     * @param string $type Field type - date, time, datetime[combo]
     * @param User|null $user
     * @return string
     */
    public function asUserType(DateTime $date, $type, User $user = null)
    {
        switch ($type) {
            case "date":
                return $this->asUserDate($date, true, $user);
                break;
            case 'time':
                return $this->asUserTime($date, $user);
                break;
            case 'datetime':
            case 'datetimecombo':
            default:
                return $this->asUser($date, $user);
        }
    }

    /**
     * Produce timestamp offset by user's timezone
     *
     * So if somebody converts it to format assuming GMT, it would actually display user's time.
     * This is used by Javascript.
     *
     * @param DateTime $date
     * @param User|null $user
     * @return int
     */
    public function asUserTs(DateTime $date, User $user = null)
    {
        return $date->format('U') + $this->_getUserTZ($user)->getOffset($date);
    }

    /**
     * Format DateTime object as DB date
     * Note: by default does not convert TZ!
     * @param DateTime $date
     * @param boolean $tz Perform TZ conversion?
     * @return string
     */
    public function asDbDate(DateTime $date, $tz = false)
    {
        if ($tz) {
            $date->setTimezone(self::$gmtTimezone);
        }

        return $date->format($this->get_db_date_format());
    }

    /**
     * Format DateTime object as user date
     * Note: by default does not convert TZ!
     * @param DateTime $date
     * @param boolean $tz Perform TZ conversion?
     * @param User|null $user
     * @return string
     */
    public function asUserDate(DateTime $date, $tz = false, User $user = null)
    {
        if ($tz) {
            $this->tzUser($date, $user);
        }

        return $date->format($this->get_date_format($user));
    }

    /**
     * Format DateTime object as DB time
     *
     * @param DateTime $date
     * @return string
     */
    public function asDbTime(DateTime $date)
    {
        $date->setTimezone(self::$gmtTimezone);

        return $date->format($this->get_db_time_format());
    }

    /**
     * Format DateTime object as user time
     *
     * @param DateTime $date
     * @param User|null $user
     * @return string
     */
    public function asUserTime(DateTime $date, User $user = null)
    {
        $this->tzUser($date, $user);

        return $date->format($this->get_time_format($user));
    }

    /**
     * Get DateTime from DB datetime string
     *
     * @param string $date
     * @return SugarDateTime
     */
    public function fromDb($date)
    {
        if ($date instanceof DateTime) {
            $date = $date->format(self::DB_DATETIME_FORMAT);
        }

        if (null === $date) {
            $date = '';
        }

        if (!is_string($date)) {
            $msg = 'Date should be a string, ' . gettype($date) . ' given.';
            LoggerManager::getLogger()->fatal($msg . "\nDate was:\n" . print_r($date, true));
            throw new InvalidArgumentException($msg);
        }
        try {
            return SugarDateTime::createFromFormat(self::DB_DATETIME_FORMAT, $date, self::$gmtTimezone);
        } catch (Exception $e) {
            if (is_string($date)) {
                $GLOBALS['log']->error("fromDb: Conversion of $date from DB format failed: {$e->getMessage()}");
            } else {
                LoggerManager::getLogger()->error('Date parameter is not a string');
            }
            return null;
        }
    }

    /**
     * Create a date from a certain type of field in DB format
     * The types are: date, time, datatime[combo]
     * @param string $date the datetime string
     * @param string $type string type
     * @return SugarDateTime
     */
    public function fromDbType($date, $type)
    {
        switch ($type) {
            case "date":
                return $this->fromDbDate($date);
                break;
            case 'time':
                return $this->fromDbFormat($date, self::DB_TIME_FORMAT);
                break;
            case 'datetime':
            case 'datetimecombo':
            default:
                return $this->fromDb($date);
        }
    }

    /**
     * Get DateTime from DB date string
     *
     * @param string $date
     * @return SugarDateTime
     */
    public function fromDbDate($date)
    {
        try {
            return SugarDateTime::createFromFormat(self::DB_DATE_FORMAT, $date, self::$gmtTimezone);
        } catch (Exception $e) {
            $GLOBALS['log']->error("fromDbDate: Conversion of $date from DB format failed: {$e->getMessage()}");

            return null;
        }
    }

    /**
     * Get DateTime from DB datetime string using non-standard format
     *
     * Non-standard format usually would be only date, only time, etc.
     *
     * @param string $date
     * @param string $format format to accept
     * @return SugarDateTime
     */
    public function fromDbFormat($date, $format)
    {
        try {
            return SugarDateTime::createFromFormat($format, $date, self::$gmtTimezone);
        } catch (Exception $e) {
            $GLOBALS['log']->error("fromDbFormat: Conversion of $date from DB format $format failed: {$e->getMessage()}");

            return null;
        }
    }

    /**
     * Get DateTime from user datetime string
     *
     * @param string $date
     * @param User|null $user
     * @return SugarDateTime
     */
    public function fromUser($date, User $user = null)
    {
        $res = null;
        try {
            $res = SugarDateTime::createFromFormat($this->get_date_time_format($user), $date, $this->_getUserTZ($user));
        } catch (Exception $e) {
            $GLOBALS['log']->error("fromUser: Conversion of $date exception: {$e->getMessage()}");
        }

        if (!($res instanceof DateTime)) {
            $uf = $this->get_date_time_format($user);
            $GLOBALS['log']->error("fromUser: Conversion of $date from user format $uf failed");

            return null;
        }

        return $res;
    }

    /**
     * Create a date from a certain type of field in user format
     * The types are: date, time, datatime[combo]
     * @param string $date the datetime string
     * @param string $type string type
     * @param User $user
     * @return SugarDateTime
     */
    public function fromUserType($date, $type, $user = null)
    {
        switch ($type) {
            case "date":
                return $this->fromUserDate($date, $user);
                break;
            case 'time':
                return $this->fromUserTime($date, $user);
                break;
            case 'datetime':
            case 'datetimecombo':
            default:
                return $this->fromUser($date, $user);
        }
    }

    /**
     * Get DateTime from user time string
     *
     * @param string $date
     * @param User|null $user
     * @return SugarDateTime
     */
    public function fromUserTime($date, User $user = null)
    {
        try {
            return SugarDateTime::createFromFormat($this->get_time_format($user), $date, $this->_getUserTZ($user));
        } catch (Exception $e) {
            $uf = $this->get_time_format($user);
            $GLOBALS['log']->error("fromUserTime: Conversion of $date from user format $uf failed: {$e->getMessage()}");

            return null;
        }
    }

    /**
     * Get DateTime from user date string
     * Usually for calendar-related functions like holidays
     * Note: by default does not convert tz!
     * @param string $date
     * @param bool $convert_tz perform TZ converson?
     * @param User|null $user
     * @return SugarDateTime
     */
    public function fromUserDate($date, $convert_tz = false, User $user = null)
    {
        try {
            return SugarDateTime::createFromFormat(
                $this->get_date_format($user),
                $date,
                $convert_tz ? $this->_getUserTZ($user) : self::$gmtTimezone
            );
        } catch (Exception $e) {
            $uf = $this->get_date_format($user);
            $GLOBALS['log']->error("fromUserDate: Conversion of $date from user format $uf failed: {$e->getMessage()}");

            return null;
        }
    }

    /**
     * Create a date object from any string
     *
     * Same formats accepted as for DateTime ctor
     *
     * @param string $date
     * @param User|null $user
     * @return SugarDateTime
     */
    public function fromString($date, User $user = null)
    {
        try {
            return new SugarDateTime($date, $this->_getUserTZ($user));
        } catch (Exception $e) {
            $GLOBALS['log']->error("fromString: Conversion of $date from string failed: {$e->getMessage()}");

            return null;
        }
    }

    /**
     * Create DateTime from timestamp
     *
     * @param interger|string $ts
     * @return SugarDateTime
     */
    public function fromTimestamp($ts)
    {
        return new SugarDateTime("@$ts");
    }

    /**
     * Convert DateTime to GMT timezone
     * @param DateTime $date
     * @return DateTime
     */
    public function tzGMT(DateTime $date)
    {
        return $date->setTimezone(self::$gmtTimezone);
    }

    /**
     * Convert DateTime to user timezone
     * @param DateTime $date
     * @param User|null $user
     * @return DateTime
     */
    public function tzUser(DateTime $date, User $user = null)
    {
        $userTZ = $this->_getUserTZ($user);
        $ret = $date->setTimezone($userTZ);
        return $ret;
    }

    /**
     * Get string defining midnight in current user's format
     * @param string $format Time format to use
     * @return string
     */
    protected function _get_midnight($format = null)
    {
        $zero = new DateTime("@0", self::$gmtTimezone);

        return $zero->format($format ? $format : $this->get_time_format());
    }

    /**
     *
     * Basic conversion function
     *
     * Converts between two string dates in different formats and timezones
     *
     * @param string $date
     * @param string $fromFormat
     * @param DateTimeZone $fromTZ
     * @param string $toFormat
     * @param DateTimeZone|null $toTZ
     * @param bool $expand If string lacks time, expand it to include time
     * @return string
     */
    protected function _convert($date, $fromFormat, $fromTZ, $toFormat, $toTZ, $expand = false)
    {
        if (empty($date)) {
            return $date;
        }

        if (is_string($date)) {
            $date = trim($date);
        }

        try {
            if ($expand && strlen($date) <= 10) {
                $date = $this->expandDate($date, $fromFormat);
            }
            $toFormatDate = SugarDateTime::createFromFormat($toFormat, $date, $fromTZ);
            if ($toFormatDate !== false){
                return $date;
            }
            $phpdate = SugarDateTime::createFromFormat($fromFormat, $date, $fromTZ);
            if ($phpdate == false) {
                $GLOBALS['log']->error("convert: Conversion of $date from $fromFormat to $toFormat failed");

                return '';
            }
            if ($fromTZ !== $toTZ && $toTZ != null) {
                $phpdate->setTimeZone($toTZ);
            }

            return $phpdate->format($toFormat);
        } catch (Exception $e) {
            $GLOBALS['log']->error("Conversion of $date from $fromFormat to $toFormat failed: {$e->getMessage()}");

            return '';
        }
    }

    /**
     * Convert DB datetime to local datetime
     *
     * TZ conversion is controlled by parameter
     *
     * @param string $date Original date in DB format
     * @param bool $meridiem Ignored for BC
     * @param bool $convert_tz Perform TZ conversion?
     * @param User $user User owning the conversion formats
     * @return string Date in display format
     */
    public function to_display_date_time($date, $meridiem = true, $convert_tz = true, $user = null)
    {
        return $this->_convert(
            $date,
            self::DB_DATETIME_FORMAT,
            self::$gmtTimezone,
            $this->get_date_time_format($user),
            $convert_tz ? $this->_getUserTZ($user) : self::$gmtTimezone,
            true
        );
    }

    /**
     * Converts DB time string to local time string
     *
     * TZ conversion depends on parameter
     *
     * @param string $date Time in DB format
     * @param bool $meridiem
     * @param bool $convert_tz Perform TZ conversion?
     * @return string Time in user-defined format
     */
    public function to_display_time($date, $meridiem = true, $convert_tz = true)
    {
        if ($convert_tz && strpos($date, ' ') === false) {
            // we need TZ adjustment but have no date, assume today
            $date = $this->expandTime($date, self::DB_DATETIME_FORMAT, self::$gmtTimezone);
        }

        return $this->_convert(
            $date,
            $convert_tz ? self::DB_DATETIME_FORMAT : self::DB_TIME_FORMAT,
            self::$gmtTimezone,
            $this->get_time_format(),
            $convert_tz ? $this->_getUserTZ() : self::$gmtTimezone
        );
    }

    /**
     * Splits time in given format into components
     *
     * Components: h, m, s, a (am/pm) if format requires it
     * If format has am/pm, hour is 12-based, otherwise 24-based
     *
     * @param string $date
     * @param string $format
     * @return array
     */
    public function splitTime($date, $format)
    {
        if (!($date instanceof DateTime)) {
            $date = SugarDateTime::createFromFormat($format, $date);
        }
        $ampm = strpbrk($format, 'aA');
        $datearr = array(
            "h" => ($ampm == false) ? $date->format("H") : $date->format("h"),
            'm' => $date->format("i"),
            's' => $date->format("s")
        );
        if ($ampm) {
            $datearr['a'] = ($ampm[0] == 'a') ? $date->format("a") : $date->format("A");
        }

        return $datearr;
    }

    /**
     * Converts DB date string to local date string
     *
     * TZ conversion depens on parameter
     *
     * @param string $date Date in DB format
     * @param bool $convert_tz Perform TZ conversion?
     * @return string Date in user-defined format
     */
    public function to_display_date($date, $convert_tz = true)
    {
        return $this->_convert(
            $date,
            self::DB_DATETIME_FORMAT,
            self::$gmtTimezone,
            $this->get_date_format(),
            $convert_tz ? $this->_getUserTZ() : self::$gmtTimezone,
            true
        );
    }

    /**
     * Convert date from format to format
     *
     * No TZ conversion is performed!
     *
     * @param string $date
     * @param string $from Source format
     * @param string $to Destination format
     * @return string Converted date
     */
    public function to_display($date, $from, $to)
    {
        return $this->_convert($date, $from, self::$gmtTimezone, $to, self::$gmtTimezone);
    }

    /**
     * Get DB datetime format
     * @return string
     */
    public function get_db_date_time_format()
    {
        return self::DB_DATETIME_FORMAT;
    }

    /**
     * Get DB date format
     * @return string
     */
    public function get_db_date_format()
    {
        return self::DB_DATE_FORMAT;
    }

    /**
     * Get DB time format
     * @return string
     */
    public function get_db_time_format()
    {
        return self::DB_TIME_FORMAT;
    }

    /**
     * Convert date from local datetime to GMT-based DB datetime
     *
     * Includes TZ conversion.
     *
     * @param string $date
     * @return string Datetime in DB format
     */
    public function to_db($date)
    {
        return $this->_convert(
            $date,
            $this->get_date_time_format(),
            $this->_getUserTZ(),
            $this->get_db_date_time_format(),
            self::$gmtTimezone,
            true
        );
    }

    /**
     * Convert local datetime to DB date
     *
     * TZ conversion depends on parameter. If false, only format conversion is performed.
     *
     * @param string $date Local date
     * @param bool $convert_tz Should time and TZ be taken into account?
     * @return string Date in DB format
     */
    public function to_db_date($date, $convert_tz = true)
    {
        return $this->_convert(
            $date,
            $this->get_date_time_format(),
            $convert_tz ? $this->_getUserTZ() : self::$gmtTimezone,
            self::DB_DATE_FORMAT,
            self::$gmtTimezone,
            true
        );
    }

    /**
     * Convert local datetime to DB time
     *
     * TZ conversion depends on parameter. If false, only format conversion is performed.
     *
     * @param string $date Local date
     * @param bool $convert_tz Should time and TZ be taken into account?
     * @return string Time in DB format
     */
    public function to_db_time($date, $convert_tz = true)
    {
        $format = $this->get_date_time_format();
        $tz = $convert_tz ? $this->_getUserTZ() : self::$gmtTimezone;
        if ($convert_tz && strpos($date, ' ') === false) {
            // we need TZ adjustment but have short string, expand it to full one
            // FIXME: if the string is short, should we assume date or time?
            $date = $this->expandTime($date, $format, $tz);
        }

        return $this->_convert(
            $date,
            $convert_tz ? $format : $this->get_time_format(),
            $tz,
            self::DB_TIME_FORMAT,
            self::$gmtTimezone
        );
    }

    /**
     * Takes a Date & Time value in local format and converts them to DB format
     * No TZ conversion!
     *
     * @param string $date
     * @param string $time
     * @return array Date & time in DB format
     **/
    public function to_db_date_time($date, $time)
    {
        try {
            $phpdate = SugarDateTime::createFromFormat(
                $this->get_date_time_format(),
                $this->merge_date_time($date, $time),
                self::$gmtTimezone
            );
            if ($phpdate == false) {
                return array('', '');
            }

            return array($this->asDbDate($phpdate), $this->asDbTime($phpdate));
        } catch (Exception $e) {
            $GLOBALS['log']->error("Conversion of $date,$time failed");

            return array('', '');
        }
    }

    /**
     * Return current time in DB format
     * @return string
     */
    public function nowDb()
    {
        if (!$this->allow_cache) {
            $nowGMT = $this->getNow();
        } else {
            $nowGMT = $this->now;
        }

        return $this->asDb($nowGMT);
    }

    /**
     * Return current date in DB format
     * @return string
     */
    public function nowDbDate()
    {
        if (!$this->allow_cache) {
            $nowGMT = $this->getNow();
        } else {
            $nowGMT = $this->now;
        }

        return $this->asDbDate($nowGMT, true);
    }

    /**
     * Get 'now' DateTime object
     * @param bool $userTz return in user timezone?
     * @return SugarDateTime
     */
    public function getNow($userTz = false)
    {
        if (!$this->allow_cache) {
            return new SugarDateTime("now", $userTz ? $this->_getUserTz() : self::$gmtTimezone);
        }
        // TODO: should we return clone?
        $now = clone $this->now;
        if ($userTz) {
            return $this->tzUser($now);
        }

        return $now;
    }

    /**
     * Set 'now' time
     * For testability - predictable time value
     * @param DateTime $now
     * @return TimeDate $this
     */
    public function setNow($now)
    {
        $this->now = $now;

        return $this;
    }

    /**
     * Return current datetime in local format
     * @return string
     */
    public function now()
    {
        return $this->asUser($this->getNow());
    }

    /**
     * Return current date in User format
     * @return string
     */
    public function nowDate()
    {
        return $this->asUserDate($this->getNow());
    }

    /**
     * Get user format's time separator
     * @return string
     */
    public function timeSeparator()
    {
        if (empty($this->time_separator)) {
            $this->time_separator = $this->timeSeparatorFormat($this->get_time_format());
        }

        return $this->time_separator;
    }

    /**
     * Find out format's time separator
     * @param string $timeformat Time format
     * @return stringS
     */
    public function timeSeparatorFormat($timeformat)
    {
        $date = $this->_convert("00:11:22", self::DB_TIME_FORMAT, null, $timeformat, null);
        if (preg_match('/\d+(.+?)11/', $date, $matches)) {
            $sep = $matches[1];
        } else {
            $sep = ':';
        }

        return $sep;
    }

    /**
     * Returns start and end of a certain local date in GMT
     * Example: for May 19 in PDT start would be 2010-05-19 07:00:00, end would be 2010-05-20 06:59:59
     * @param string|DateTime $date Date in any suitable format
     * @param User|null $user
     * @return array Start & end date in start, startdate, starttime, end, enddate, endtime
     */
    public function getDayStartEndGMT($date, User $user = null)
    {
        $result = [];
        if ($date instanceof DateTime) {
            $min = clone $date;
            $min->setTimezone($this->_getUserTZ($user));
            $max = clone $date;
            $max->setTimezone($this->_getUserTZ($user));
        } else {
            $min = new DateTime($date, $this->_getUserTZ($user));
            $max = new DateTime($date, $this->_getUserTZ($user));
        }
        $min->setTime(0, 0);
        $max->setTime(23, 59, 59);

        $min->setTimezone(self::$gmtTimezone);
        $max->setTimezone(self::$gmtTimezone);

        $result['start'] = $this->asDb($min);
        $result['startdate'] = $this->asDbDate($min);
        $result['starttime'] = $this->asDbTime($min);
        $result['end'] = $this->asDb($max);
        $result['enddate'] = $this->asDbDate($max);
        $result['endtime'] = $this->asDbtime($max);

        return $result;
    }

    /**
     * Expand date format by adding midnight to it
     * Note: date is assumed to be in target format already
     * @param string $date
     * @param string $format Target format
     * @return string
     */
    public function expandDate($date, $format)
    {
        $formats = $this->split_date_time($format);
        if (isset($formats[1])) {
            return $this->merge_date_time($date, $this->_get_midnight($formats[1]));
        }

        return $date;
    }

    /**
     * Expand time format by adding today to it
     * Note: time is assumed to be in target format already
     * @param string $date
     * @param string $format Target format
     * @param DateTimeZone $tz
     * @return string
     */
    public function expandTime($date, $format, $tz)
    {
        $formats = $this->split_date_time($format);
        if (isset($formats[1])) {
            $now = clone $this->getNow();
            $now->setTimezone($tz);

            return $this->merge_date_time($now->format($formats[0]), $date);
        }

        return $date;
    }

    /**
     * Get midnight (start of the day) in local time format
     *
     * @return Time string
     */
    public function get_default_midnight()
    {
        return $this->_get_midnight($this->get_time_format());
    }

    /**
     * Get the name of the timezone for the user
     * @param User|null $user User, default - current user
     * @return string
     */
    public static function userTimezone(User $user = null)
    {
        $user = self::getInstance()->_getUser($user);
        if (empty($user)) {
            return '';
        }
        $tz = self::getInstance()->_getUserTZ($user);
        if ($tz) {
            return $tz->getName();
        }

        return '';
    }

    /**
     * Guess the timezone for the current user
     * @param int $userOffset Offset from GMT in minutes
     * @return string
     */
    public static function guessTimezone($userOffset = 0)
    {
        if (!is_numeric($userOffset)) {
            return '';
        }
        $defaultZones = array(
            'America/Anchorage',
            'America/Los_Angeles',
            'America/Phoenix',
            'America/Chicago',
            'America/New_York',
            'America/Argentina/Buenos_Aires',
            'America/Montevideo',
            'Europe/London',
            'Europe/Amsterdam',
            'Europe/Athens',
            'Europe/Moscow',
            'Asia/Tbilisi',
            'Asia/Omsk',
            'Asia/Jakarta',
            'Asia/Hong_Kong',
            'Asia/Tokyo',
            'Pacific/Guam',
            'Australia/Sydney',
            'Australia/Perth',
        );

        $now = new DateTime();
        $tzlist = timezone_identifiers_list();
        if ($userOffset == 0) {
            $gmtOffset = date('Z');
            $nowtz = date('e');
            if (in_array($nowtz, $tzlist)) {
                array_unshift($defaultZones, $nowtz);
            } else {
                $nowtz = timezone_name_from_abbr(date('T'), $gmtOffset, date('I'));
                if (in_array($nowtz, $tzlist)) {
                    array_unshift($defaultZones, $nowtz);
                }
            }
        } else {
            $gmtOffset = $userOffset * 60;
        }
        foreach ($defaultZones as $zoneName) {
            $tz = new DateTimeZone($zoneName);
            if ($tz->getOffset($now) == $gmtOffset) {
                return $tz->getName();
            }
        }
        // try all zones
        foreach ($tzlist as $zoneName) {
            $tz = new DateTimeZone($zoneName);
            if ($tz->getOffset($now) == $gmtOffset) {
                return $tz->getName();
            }
        }

        return null;
    }

    /**
     * Get the description of the user timezone for specific date
     * Like: PST(+08:00)
     * We need the date because it can be DST or non-DST
     * Note it's different from TZ name in tzName() that relates to current date
     * @param DateTime $date Current date
     * @param User|null $user User, default - current user
     * @return string
     */
    public static function userTimezoneSuffix(DateTime $date, User $user = null)
    {
        $user = self::getInstance()->_getUser($user);
        if (empty($user)) {
            return '';
        }
        self::getInstance()->tzUser($date, $user);

        return $date->format('T(P)');
    }

    /**
     * Get display name for a certain timezone
     * Note: it uses current date for GMT offset, so it may be not suitable for displaying generic dates
     * @param string|DateTimeZone $name TZ name
     * @return string
     */
    public static function tzName($name)
    {
        if (empty($name)) {
            return '';
        }
        if ($name instanceof DateTimeZone) {
            $tz = $name;
        } else {
            $tz = timezone_open($name);
        }
        if (!$tz) {
            return "???";
        }
        $now = new DateTime("now", $tz);
        $off = $now->getOffset();
        $translated = translate('timezone_dom', '', $name);
        if (is_string($translated) && !empty($translated)) {
            $name = $translated;
        }

        return sprintf(
            "%s (GMT%+2d:%02d)%s",
            str_replace('_', ' ', $name),
            $off / 3600,
            (abs($off) / 60) % 60,
            ""
        );//$now->format('I')==1?"(+DST)":"");
    }


    /**
     * Timezone sorting helper
     * Sorts by name
     * @param array $a
     * @param array $b
     * @internal
     * @return int
     */
    public static function _sortTz($a, $b)
    {
        if ($a[0] == $b[0]) {
            return strcmp($a[1], $b[1]);
        } else {
            return $a[0] < $b[0] ? -1 : 1;
        }
    }

    /**
     * Get list of all timezones in the system
     * @return array
     */
    public static function getTimezoneList()
    {
        $now = new DateTime();
        $res_zones = $zones = array();
        foreach (timezone_identifiers_list() as $zoneName) {
            $tz = new DateTimeZone($zoneName);
            $zones[$zoneName] = array($tz->getOffset($now), self::tzName($zoneName));
        }
        uasort($zones, array('TimeDate', '_sortTz'));
        foreach ($zones as $name => $zonedef) {
            $res_zones[$name] = $zonedef[1];
        }

        return $res_zones;
    }

    /**
     * Print timestamp in RFC2616 format:
     * @param int|null $ts Null means current ts
     * @return string
     */
    public static function httpTime($ts = null)
    {
        if ($ts === null) {
            $ts = time();
        }

        return gmdate(self::RFC2616_FORMAT, $ts);
    }

    /**
     * Create datetime object from calendar array
     * @param array $time
     * @return SugarDateTime
     */
    public function fromTimeArray($time)
    {
        if (!isset($time) || count($time) == 0) {
            return $this->nowDb();
        } elseif (isset($time['ts'])) {
            return $this->fromTimestamp($time['ts']);
        } elseif (isset($time['date_str'])) {
            return $this->fromDb($time['date_str']);
        }
        $hour = 0;
        $min = 0;
        $sec = 0;
        $now = $this->getNow(true);
        $day = $now->day;
        $month = $now->month;
        $year = $now->year;
        if (isset($time['sec'])) {
            $sec = $time['sec'];
        }
        if (isset($time['min'])) {
            $min = $time['min'];
        }
        if (isset($time['hour'])) {
            $hour = $time['hour'];
        }
        if (isset($time['day'])) {
            $day = $time['day'];
        }
        if (isset($time['month'])) {
            $month = $time['month'];
        }
        if (isset($time['year']) && $time['year'] >= 1970) {
            $year = $time['year'];
        }

        return $now->setDate($year, $month, $day)->setTime($hour, $min, $sec)->setTimeZone(self::$gmtTimezone);
    }

    /**
     * Returns the date portion of a datetime string
     *
     * @param string $datetime
     * @return string
     */
    public function getDatePart($datetime)
    {
        list($date, $time) = $this->split_date_time($datetime);

        return $date;
    }

    /**
     * Returns the time portion of a datetime string
     *
     * @param string $datetime
     * @return string
     */
    public function getTimePart($datetime)
    {
        list($date, $time) = $this->split_date_time($datetime);

        return $time;
    }

    /**
     * Returns the offset from user's timezone to GMT
     * @param User|null $user
     * @param \DateTime|null $time When the offset is taken, default is now
     * @return int Offset in minutes
     */
    public function getUserUTCOffset(User $user = null, DateTime $time = null)
    {
        if (empty($time)) {
            $time = $this->now;
        }

        return $this->_getUserTZ($user)->getOffset($time) / 60;
    }

    /**
     * Create regexp from datetime format
     * @param string $format
     * @return string Regular expression string
     */
    public static function get_regular_expression($format)
    {
        $newFormat = '';
        $regPositions = array();
        $ignoreNextChar = false;
        $count = 1;
        foreach (str_split($format) as $char) {
            if (!$ignoreNextChar && isset(self::$format_to_regexp[$char])) {
                $newFormat .= '(' . self::$format_to_regexp[$char] . ')';
                $regPositions[$char] = $count;
                $count++;
            } else {
                $ignoreNextChar = false;
                $newFormat .= $char;
            }
            if ($char == "\\") {
                $ignoreNextChar = true;
            }
        }

        return array('format' => $newFormat, 'positions' => $regPositions);
    }

    // format - date expression ('' means now) for start and end of the range
    protected $date_expressions = array(
        'yesterday' => array("-1 day", "-1 day"),
        'today' => array("", ""),
        'tomorrow' => array("+1 day", "+1 day"),
        'last_7_days' => array("-6 days", ""),
        'next_7_days' => array("", "+6 days"),
        'last_30_days' => array("-29 days", ""),
        'next_30_days' => array("", "+29 days"),
    );

    /**
     * Parse date template
     * @internal
     * @param string $template Date expression
     * @param bool $daystart Do we want start or end of the day?
     * @param User|null $user
     * @param bool $adjustForTimezone
     * @return SugarDateTime
     */
    protected function parseFromTemplate($template, $daystart, User $user = null, $adjustForTimezone = true)
    {
        $rawTime = $this->getNow();
        $now = $adjustForTimezone ? $this->tzUser($rawTime, $user) : $rawTime;
        if (!empty($template)) {
            $now->modify($template);
        }
        if ($daystart) {
            return $now->get_day_begin();
        } else {
            return $now->get_day_end();
        }
    }

    /**
     * Get month-long range mdiff months from now
     * @internal
     * @param int $mdiff
     * @param User|null $user
     * @param bool $adjustForTimezone
     * @return array
     */
    protected function diffMon($mdiff, User $user = null, $adjustForTimezone = true)
    {
        $rawTime = $this->getNow();
        $now = $adjustForTimezone ? $this->tzUser($rawTime, $user) : $rawTime;
        $now->setDate($now->year, $now->month + $mdiff, 1);
        $start = $now->get_day_begin();
        $end = $now->setDate($now->year, $now->month, $now->days_in_month)->setTime(23, 59, 59);

        return array($start, $end);
    }

    /**
     * Get year-long range ydiff years from now
     * @internal
     * @param int $ydiff
     * @param User|null $user
     * @param bool $adjustForTimezone
     * @return array
     */
    protected function diffYear($ydiff, User $user = null, $adjustForTimezone = true)
    {
        $rawTime = $this->getNow();
        $now = $adjustForTimezone ? $this->tzUser($rawTime, $user) : $rawTime;
        $now->setDate($now->year + $ydiff, 1, 1);
        $start = $now->get_day_begin();
        $end = $now->setDate($now->year, 12, 31)->setTime(23, 59, 59);

        return array($start, $end);
    }

    /**
     * Parse date range expression
     * Returns beginning and end of the range as a date
     * @param string $range
     * @param User|null $user
     * @param bool $adjustForTimezone Do we need to adjust for timezone?
     * @return array of two Date objects, start & end
     */
    public function parseDateRange($range, User $user = null, $adjustForTimezone = true)
    {
        if (isset($this->date_expressions[$range])) {
            return array(
                $this->parseFromTemplate($this->date_expressions[$range][0], true, $user, $adjustForTimezone),
                $this->parseFromTemplate($this->date_expressions[$range][1], false, $user, $adjustForTimezone)
            );
        }
        switch ($range) {
            case 'next_month':
                return $this->diffMon(1, $user, $adjustForTimezone);
            case 'last_month':
                return $this->diffMon(-1, $user, $adjustForTimezone);
            case 'this_month':
                return $this->diffMon(0, $user, $adjustForTimezone);
            case 'last_year':
                return $this->diffYear(-1, $user, $adjustForTimezone);
            case 'this_year':
                return $this->diffYear(0, $user, $adjustForTimezone);
            case 'next_year':
                return $this->diffYear(1, $user, $adjustForTimezone);
            default:
                return null;
        }
    }

    /********************* OLD functions, should not be used publicly anymore ****************/
    /**
     * Merge time without am/pm with am/pm string
     * @TODO find better way to do this!
     * @deprecated for public use
     * @param string $date
     * @param string $format User time format
     * @param string $mer
     * @return string
     */
    public function merge_time_meridiem($date, $format, $mer)
    {
        $date = trim($date);
        if (empty($date)) {
            return $date;
        }
        $fakeMerFormat = str_replace(array('a', 'A'), array('@~@', '@~@'), $format);
        $noMerFormat = trim(str_replace(array('a', 'A'), array('', ''), $format));
        $newDate = $this->swap_formats($date, $noMerFormat, $fakeMerFormat);

        return str_replace('@~@', $mer, $newDate);
    }

    /**
     * @deprecated for public use
     * Convert date from one format to another
     *
     * @param string $date
     * @param string $from
     * @param string $to
     * @return string
     */
    public function swap_formats($date, $from, $to)
    {
        return $this->_convert($date, $from, self::$gmtTimezone, $to, self::$gmtTimezone);
    }

    /**
     * @deprecated for public use
     * handles offset values for Timezones and DST
     * @param    $date         string        date/time formatted in user's selected format
     * @param    $format         string        destination format value as passed to PHP's date() funtion
     * @param    $to             boolean
     * @param    $user         object        user object from which Timezone and DST
     * @param    $usetimezone string        timezone name
     * values will be derived
     * @return     string        date formatted and adjusted for TZ and DST
     */
    public function handle_offset($date, $format, $to = true, $user = null, $usetimezone = null)
    {
        $tz = empty($usetimezone) ? $this->_getUserTZ($user) : new DateTimeZone($usetimezone);
        $dateobj = new SugarDateTime($date, $to ? self::$gmtTimezone : $tz);
        $dateobj->setTimezone($to ? $tz : self::$gmtTimezone);

        return $dateobj->format($format);
//        return $this->_convert($date, $format, $to ? self::$gmtTimezone : $tz, $format, $to ? $tz : self::$gmtTimezone);
    }

    /**
     * @deprecated for public use
     * Get current GMT datetime in DB format
     * @return string
     */
    public function get_gmt_db_datetime()
    {
        return $this->nowDb();
    }

    /**
     * @deprecated for public use
     * Get current GMT date in DB format
     * @return string
     */
    public function get_gmt_db_date()
    {
        return $this->nowDbDate();
    }

    /**
     * @deprecated for public use
     * this method will take an input $date variable (expecting Y-m-d format)
     * and get the GMT equivalent - with an hour-level granularity :
     * return the max value of a given locale's
     * date+time in GMT metrics (i.e., if in PDT, "2005-01-01 23:59:59" would be
     * "2005-01-02 06:59:59" in GMT metrics)
     * @param $date
     * @return array
     */
    public function handleOffsetMax($date)
    {
        $gmtDateTime = [];
        $min = new DateTime($date, $this->_getUserTZ());
        $min->setTime(0, 0);
        $max = new DateTime($date, $this->_getUserTZ());
        $max->setTime(23, 59, 59);

        $min->setTimezone(self::$gmtTimezone);
        $max->setTimezone(self::$gmtTimezone);

        $gmtDateTime['date'] = $this->asDbDate($max, false);
        $gmtDateTime['time'] = $this->asDbDate($max, false);
        $gmtDateTime['min'] = $this->asDb($min);
        $gmtDateTime['max'] = $this->asDb($max);

        return $gmtDateTime;
    }

    /**
     * @deprecated for public use
     * this returns the adjustment for a user against the server time
     *
     * @return integer number of minutes to adjust a time by to get the appropriate time for the user
     */
    public function adjustmentForUserTimeZone()
    {
        $tz = $this->_getUserTZ();
        $server_tz = new DateTimeZone(date_default_timezone_get());
        if ($tz && $server_tz) {
            return ($server_tz->getOffset($this->now) - $tz->getOffset($this->now)) / 60;
        }

        return 0;
    }

    /**
     * @deprecated for public use
     * assumes that olddatetime is in Y-m-d H:i:s format
     * @param $olddatetime
     * @return string
     */
    public function convert_to_gmt_datetime($olddatetime)
    {
        if (!empty($olddatetime)) {
            return date('Y-m-d H:i:s', strtotime($olddatetime) - date('Z'));
        }

        return '';
    }

    /**
     * @deprecated for public use
     * get user timezone info
     * @param User|null $user
     * @return array
     */
    public function getUserTimeZone(User $user = null)
    {
        $tz = $this->_getUserTZ($user);

        return array("gmtOffset" => $tz->getOffset($this->now) / 60);
    }

    /**
     * @deprecated for public use
     * get timezone start & end
     * @param $year
     * @param string $zone
     * @return array
     */
    public function getDSTRange($year, $zone = null)
    {
        if (!empty($zone)) {
            $tz = timezone_open($zone);
        }
        if (empty($tz)) {
            $tz = $this->_getUserTZ();
        }

        $year_date = SugarDateTime::createFromFormat("Y", $year, self::$gmtTimezone);
        $year_end = clone $year_date;
        $year_end->setDate((int)$year, 12, 31);
        $year_end->setTime(23, 59, 59);
        $year_date->setDate((int)$year, 1, 1);
        $year_date->setTime(0, 0, 0);
        $result = array();
        $transitions = $tz->getTransitions($year_date->ts, $year_end->ts);
        $idx = 0;
        // get DST start
        while (isset($transitions[$idx]) && !$transitions[$idx]["isdst"]) {
            $idx++;
        }
        if (isset($transitions[$idx])) {
            $result["start"] = $this->fromTimestamp($transitions[$idx]["ts"])->asDb();
        }
        // get DST end
        while (isset($transitions[$idx]) && $transitions[$idx]["isdst"]) {
            $idx++;
        }
        if (isset($transitions[$idx])) {
            $result["end"] = $this->fromTimestamp($transitions[$idx]["ts"])->asDb();
        }

        return $result;
    }

    /****************** GUI stuff that really shouldn't be here, will be moved ************/
    /**
     * Get Javascript variables setup for user date format validation
     * @deprecated moved to SugarView
     * @return string JS code
     */
    public function get_javascript_validation()
    {
        return SugarView::getJavascriptValidation();
    }

    /**
     * AMPMMenu
     * This method renders a SELECT HTML form element based on the
     * user's time format preferences, with give date's value highlighted.
     *
     * If user's prefs have no AM/PM string, returns empty string.
     *
     * @todo There is hardcoded HTML in here that does not allow for localization
     * of the AM/PM am/pm Strings in this drop down menu.  Also, perhaps
     * change to the substr_count function calls to strpos
     * TODO: Remove after full switch to fields
     * @deprecated
     * @param string $prefix Prefix for SELECT
     * @param string $date Date in display format
     * @param string $attrs Additional attributes for SELECT
     * @return string SELECT HTML
     */
    public function AMPMMenu($prefix, $date, $attrs = '')
    {
        $tf = $this->get_time_format();
        $am = strpbrk($tf, 'aA');
        if ($am == false) {
            return '';
        }
        $selected = array("am" => "", "pm" => "", "AM" => "", "PM" => "");
        if (isset($date) && preg_match('/([ap]m)/i', $date, $match)) {
            $selected[$match[1]] = " selected";
        }

        $menu = "<select name='" . $prefix . "meridiem' " . $attrs . ">";
        if ($am[0] == 'a') {
            $menu .= "<option value='am'{$selected["am"]}>am";
            $menu .= "<option value='pm'{$selected["pm"]}>pm";
        } else {
            $menu .= "<option value='AM'{$selected["AM"]}>AM";
            $menu .= "<option value='PM'{$selected["PM"]}>PM";
        }

        return $menu . '</select>';
    }

    /**
     * Get user format in JS form
     * TODO: Remove after full switch to fields
     * @return string
     */
    public function get_user_date_format()
    {
        return str_replace(array('Y', 'm', 'd'), array('yyyy', 'mm', 'dd'), $this->get_date_format());
    }

    /**
     * Get user time format example
     * TODO: Remove after full switch to fields
     * @deprecated
     * @return string
     */
    public function get_user_time_format()
    {
        global $sugar_config;
        $time_pref = $this->get_time_format();

        if (!empty($time_pref) && !empty($sugar_config['time_formats'][$time_pref])) {
            return $sugar_config['time_formats'][$time_pref];
        }

        return '23:00'; //default
    }
}
