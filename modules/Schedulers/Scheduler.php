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

require_once 'modules/SchedulersJobs/SchedulersJob.php';

#[\AllowDynamicProperties]
class Scheduler extends SugarBean
{
    // table columns
    public $id;
    public $deleted;
    public $date_entered;
    public $date_modified;
    public $modified_user_id;
    public $created_by;
    public $created_by_name;
    public $modified_by_name;
    public $name;
    public $job;
    public $date_time_start;
    public $date_time_end;
    public $job_interval;
    public $time_from;
    public $time_to;
    public $last_run;
    public $status;
    public $catch_up;
    // object attributes
    public $user;
    public $intervalParsed;
    public $intervalHumanReadable;
    public $metricsVar;
    public $metricsVal;
    public $dayInt;
    public $dayLabel;
    public $monthsInt;
    public $monthsLabel;
    public $suffixArray;
    public $datesArray;
    public $scheduledJobs;
    public $timeOutMins = 60;
    // standard SugarBean attrs
    public $table_name = "schedulers";
    public $object_name = "Scheduler";
    public $module_dir = "Schedulers";
    public $new_schema = true;
    public $process_save_dates = true;
    public $order_by;

    public static $job_strings;

    public function __construct($init = true)
    {
        parent::__construct();
        $job = BeanFactory::newBean('SchedulersJobs');
        $this->job_queue_table = $job->table_name;
    }

    protected function getUser()
    {
        if (empty($this->user)) {
            $this->user = Scheduler::initUser();
        }
        return $this->user;
    }

    /**
     * Function returns an Admin user for running Schedulers or false if no admin users are present in the system
     * (which means the Scheduler Jobs which need admin rights will fail to execute)
     */
    public static function initUser()
    {
        $user = BeanFactory::newBean('Users');
        $db = DBManagerFactory::getInstance();

        //Check is default admin exists
        $adminId = $db->getOne(
            'SELECT id FROM users WHERE id = ' . $db->quoted('1') . ' AND is_admin = 1 AND deleted = 0 AND status = ' . $db->quoted('Active'),
            true,
            'Error retrieving Admin account info'
        );

        if ($adminId === false) { // Retrieve another admin if default admin doesn't exist
            $adminId = $db->getOne(
                'SELECT id FROM users WHERE is_admin = 1 AND deleted = 0 AND status = ' . $db->quoted('Active'),
                true,
                'Error retrieving Admin account info'
            );
            if ($adminId) { // Get admin user
                $user->retrieve($adminId);
            } else { // Return false and log error
                $GLOBALS['log']->fatal('No Admin account found!');
                return false;
            }
        } else { // Scheduler jobs run as default Admin
            $user->retrieve('1');
        }
        return $user;
    }

    ///////////////////////////////////////////////////////////////////////////
    ////	SCHEDULER HELPER FUNCTIONS

    /**
     * calculates if a job is qualified to run
     */
    public function fireQualified()
    {
        if (empty($this->id)) { // execute only if we have an instance
            $GLOBALS['log']->fatal('Scheduler called fireQualified() in a non-instance');
            return false;
        }

        $now = TimeDate::getInstance()->getNow();
        $now = $now->setTime($now->hour, $now->min, "00")->asDb();
        $validTimes = $this->deriveDBDateTimes($this);

        if (is_array($validTimes) && in_array($now, $validTimes)) {
            $GLOBALS['log']->debug('----->Scheduler found valid job (' . $this->name . ') for time GMT(' . $now . ')');
            return true;
        }
        $GLOBALS['log']->debug('----->Scheduler did NOT find valid job (' . $this->name . ') for time GMT(' . $now . ')');
        return false;
    }

    /**
     * Create a job from this scheduler
     * @return SchedulersJob
     */
    public function createJob()
    {
        $job = BeanFactory::newBean('SchedulersJobs');
        $job->scheduler_id = $this->id;
        $job->name = $this->name;
        $job->execute_time = $GLOBALS['timedate']->nowDb();

        $user = $this->getUser();
        if (!is_object($user)) {
            LoggerManager::getLogger()->warn('Scheduler / create job: User object not found.');
            $job->assigned_user_id = null;
        } else {
            $job->assigned_user_id = $user->id;
        }

        $job->target = $this->job;
        return $job;
    }

    /**
     * Checks if any jobs qualify to run at this moment
     * @param SugarJobQueue $queue
     */
    public function checkPendingJobs($queue)
    {
        $allSchedulers = $this->get_full_list('', "schedulers.status='Active' AND NOT EXISTS(SELECT id FROM {$this->job_queue_table} WHERE scheduler_id=schedulers.id AND status!='" . SchedulersJob::JOB_STATUS_DONE . "')");

        $GLOBALS['log']->info('-----> Scheduler found [ ' . (is_countable($allSchedulers) ? count($allSchedulers) : 0) . ' ] ACTIVE jobs');

        if (!empty($allSchedulers)) {
            foreach ($allSchedulers as $focus) {
                if ($focus->fireQualified()) {
                    $job = $focus->createJob();
                    $queue->submitJob($job, $this->getUser());
                }
            }
        } else {
            $GLOBALS['log']->debug('----->No Schedulers found');
        }
    }

    /**
     * This function takes a Scheduler object and uses its job_interval
     * attribute to derive DB-standard datetime strings, as many as are
     * qualified by its ranges.  The times are from the time of calling the
     * script.
     *
     * @param $focus Scheduler object
     * @return $dateTimes array loaded with DB datetime strings derived from
     *      the job_interval attribute.
     * @return false If the Scheduler is not in scope, return false.
     */
    public function deriveDBDateTimes($focus)
    {
        global $timedate;
        $GLOBALS['log']->debug('----->Schedulers->deriveDBDateTimes() got an object of type: ' . $focus->object_name);
        /* [min][hr][dates][mon][days] */
        $dateTimes = array();
        $ints    = explode('::', str_replace(' ', '', (string) $focus->job_interval));
        $days    = $ints[4];
        $mons    = $ints[3];
        $dates   = $ints[2];
        $hrs     = $ints[1];
        $mins    = $ints[0];
        $today   = getdate($timedate->getNow()->ts);

        // derive day part
        $dayName = array();
        if ($days == '*') {
            $GLOBALS['log']->debug('----->got * day');
        } elseif (strstr($days, '*/')) {
            // the "*/x" format is nonsensical for this field
            // do basically nothing.
            $theDay = str_replace('*/', '', $days);
            $dayName[] = $theDay;
        } elseif ($days != '*') { // got particular day(s)
            if (strstr($days, ',')) {
                $exDays = explode(',', $days);
                foreach ($exDays as $k1 => $dayGroup) {
                    if (strstr($dayGroup, '-')) {
                        $exDayGroup = explode('-', $dayGroup); // build up range and iterate through
                        for ($i = $exDayGroup[0]; $i <= $exDayGroup[1]; $i++) {
                            $dayName[] = $i;
                        }
                    } else { // individuals
                        $dayName[] = $dayGroup;
                    }
                }
            } elseif (strstr($days, '-')) {
                $exDayGroup = explode('-', $days); // build up range and iterate through
                for ($i = $exDayGroup[0]; $i <= $exDayGroup[1]; $i++) {
                    $dayName[] = $i;
                }
            } else {
                $dayName[] = $days;
            }

            // check the day to be in scope:
            if (!in_array($today['wday'], $dayName)) {
                return false;
            }
        } else {
            return false;
        }

        // derive months part
        if ($mons == '*') {
            $GLOBALS['log']->debug('----->got * months');
        } elseif (strstr($mons, '*/')) {
            $mult = str_replace('*/', '', $mons);
            $startMon = $timedate->fromDb($focus->date_time_start)->month;
            $startFrom = ($startMon % $mult);

            $compMons = array();
            for ($i = $startFrom; $i <= 12; $i + $mult) {
                $compMons[] = $i + $mult;
                $i += $mult;
            }
            // this month is not in one of the multiplier months
            if (!in_array($today['mon'], $compMons)) {
                return false;
            }
        } elseif ($mons != '*') {
            $monName = array();
            if (strstr($mons, ',')) { // we have particular (groups) of months
                $exMons = explode(',', $mons);
                foreach ($exMons as $k1 => $monGroup) {
                    if (strstr($monGroup, '-')) { // we have a range of months
                        $exMonGroup = explode('-', $monGroup);
                        for ($i = $exMonGroup[0]; $i <= $exMonGroup[1]; $i++) {
                            $monName[] = $i;
                        }
                    } else {
                        $monName[] = $monGroup;
                    }
                }
            } elseif (strstr($mons, '-')) {
                $exMonGroup = explode('-', $mons);
                for ($i = $exMonGroup[0]; $i <= $exMonGroup[1]; $i++) {
                    $monName[] = $i;
                }
            } else { // one particular month
                $monName[] = $mons;
            }

            // check that particular months are in scope
            if (!in_array($today['mon'], $monName)) {
                return false;
            }
        }

        // derive dates part
        $dateName = array();
        if ($dates == '*') {
            $GLOBALS['log']->debug('----->got * dates');
        } elseif (strstr($dates, '*/')) {
            $mult = str_replace('*/', '', $dates);
            $startDate = $timedate->fromDb($focus->date_time_start)->day;
            $startFrom = ($startDate % $mult);

            for ($i = $startFrom; $i <= 31; $i + $mult) {
                $dateName[] = str_pad(($i + $mult), 2, '0', STR_PAD_LEFT);
                $i += $mult;
            }

            if (!in_array($today['mday'], $dateName)) {
                return false;
            }
        } elseif ($dates != '*') {
            if (strstr($dates, ',')) {
                $exDates = explode(',', $dates);
                foreach ($exDates as $k1 => $dateGroup) {
                    if (strstr($dateGroup, '-')) {
                        $exDateGroup = explode('-', $dateGroup);
                        for ($i = $exDateGroup[0]; $i <= $exDateGroup[1]; $i++) {
                            $dateName[] = $i;
                        }
                    } else {
                        $dateName[] = $dateGroup;
                    }
                }
            } elseif (strstr($dates, '-')) {
                $exDateGroup = explode('-', $dates);
                for ($i = $exDateGroup[0]; $i <= $exDateGroup[1]; $i++) {
                    $dateName[] = $i;
                }
            } else {
                $dateName[] = $dates;
            }

            // check that dates are in scope
            if (!in_array($today['mday'], $dateName)) {
                return false;
            }
        }

        // derive hours part
        //$currentHour = gmdate('G');
        //$currentHour = date('G', strtotime('00:00'));
        $hrName = array();
        if ($hrs == '*') {
            $GLOBALS['log']->debug('----->got * hours');
            for ($i = 0; $i < 24; $i++) {
                $hrName[] = $i;
            }
        } elseif (strstr($hrs, '*/')) {
            $mult = str_replace('*/', '', $hrs);
            for ($i = 0; $i < 24; $i) { // weird, i know
                $hrName[] = $i;
                $i += $mult;
            }
        } elseif ($hrs != '*') {
            if (strstr($hrs, ',')) {
                $exHrs = explode(',', $hrs);
                foreach ($exHrs as $k1 => $hrGroup) {
                    if (strstr($hrGroup, '-')) {
                        $exHrGroup = explode('-', $hrGroup);
                        for ($i = $exHrGroup[0]; $i <= $exHrGroup[1]; $i++) {
                            $hrName[] = $i;
                        }
                    } else {
                        $hrName[] = $hrGroup;
                    }
                }
            } elseif (strstr($hrs, '-')) {
                $exHrs = explode('-', $hrs);
                for ($i = $exHrs[0]; $i <= $exHrs[1]; $i++) {
                    $hrName[] = $i;
                }
            } else {
                $hrName[] = $hrs;
            }
        }
        // derive minutes
        //$currentMin = date('i');
        $minName = array();
        $currentMin = (int)$timedate->getNow()->min;
        if ($mins == '*') {
            $GLOBALS['log']->debug('----->got * mins');
            for ($i = 0; $i < 60; $i++) {
                if (($currentMin + $i) > 59) {
                    $minName[] = ($i + $currentMin - 60);
                } else {
                    $minName[] = ($i + $currentMin);
                }
            }
        } elseif (strstr($mins, '*/')) {
            $mult = str_replace('*/', '', $mins);
            for ($i = 0; $i < 60; $i += $mult) {
                $minName[] = $i;
            }
        } elseif ($mins != '*') {
            if (strstr($mins, ',')) {
                $exMins = explode(',', $mins);
                foreach ($exMins as $k1 => $minGroup) {
                    if (strstr($minGroup, '-')) {
                        $exMinGroup = explode('-', $minGroup);
                        for ($i = $exMinGroup[0]; $i <= $exMinGroup[1]; $i++) {
                            $minName[] = $i;
                        }
                    } else {
                        $minName[] = $minGroup;
                    }
                }
            } elseif (strstr($mins, '-')) {
                $exMinGroup = explode('-', $mins);
                for ($i = $exMinGroup[0]; $i <= $exMinGroup[1]; $i++) {
                    $minName[] = $i;
                }
            } else {
                $minName[] = $mins;
            }
        }

        // prep some boundaries - these are not in GMT b/c gmt is a 24hour period, possibly bridging 2 local days
        if (empty($focus->time_from) && empty($focus->time_to)) {
            $timeFromTs = 0;
            $timeToTs = $timedate->getNow(true)->get('+1 day')->ts;
        } else {
            $tfrom = $timedate->fromDbType($focus->time_from, 'time');
            $timeFromTs = $timedate->getNow(true)->setTime($tfrom->hour, $tfrom->min)->ts;
            $tto = $timedate->fromDbType($focus->time_to, 'time');
            $timeToTs = $timedate->getNow(true)->setTime($tto->hour, $tto->min)->ts;
        }
        $timeToTs++;

        if (empty($focus->last_run)) {
            $lastRunTs = 0;
        } else {
            $lastRunTs = $timedate->fromDb($focus->last_run)->ts;
        }

        /**
         * initialize return array
         */
        $validJobTime = array();

        global $timedate;
        $timeStartTs = $timedate->fromDb($focus->date_time_start)->ts;
        if (!empty($focus->date_time_end)) { // do the same for date_time_end if not empty
            $timeEndTs = $timedate->fromDb($focus->date_time_end)->ts;
        } else {
            $timeEndTs = $timedate->getNow(true)->get('+1 day')->ts;
            // $dateTimeEnd = '2020-12-31 23:59:59'; // if empty, set it to something ridiculous
        }
        $timeEndTs++;
        $dateobj = $timedate->getNow();
        $nowTs = $dateobj->ts;
        $GLOBALS['log']->debug(sprintf(
            "Constraints: start: %s from: %s end: %s to: %s now: %s",
            gmdate('Y-m-d H:i:s', $timeStartTs),
            gmdate('Y-m-d H:i:s', $timeFromTs),
            gmdate('Y-m-d H:i:s', $timeEndTs),
            gmdate('Y-m-d H:i:s', $timeToTs),
            $timedate->nowDb()
        ));
        foreach ($hrName as $kHr => $hr) {
            foreach ($minName as $kMin => $min) {
                $timedate->tzUser($dateobj);
                $dateobj->setTime($hr, $min, 0);
                $tsGmt = $dateobj->ts;

                if ($tsGmt >= $timeStartTs) { // start is greater than the date specified by admin
                    if ($tsGmt >= $timeFromTs) { // start is greater than the time_to spec'd by admin
                        if ($tsGmt > $lastRunTs) { // start from last run, last run should not be included
                            if ($tsGmt <= $timeEndTs) { // this is taken care of by the initial query - start is less than the date spec'd by admin
                                if ($tsGmt <= $timeToTs) { // start is less than the time_to
                                    $validJobTime[] = $dateobj->asDb();
                                }
                            }
                        }
                    }
                }
            }
        }
        // need ascending order to compare oldest time to last_run
        sort($validJobTime);
        /**
         * If "Execute If Missed bit is set
         */
        $now = TimeDate::getInstance()->getNow();
        $now = $now->setTime($now->hour, $now->min, "00")->asDb();

        if ($focus->catch_up == 1) {
            if ($focus->last_run == null) {
                // always "catch-up"
                $validJobTime[] = $now;
            } else {
                // determine what the interval in min/hours is
                // see if last_run is in it
                // if not, add NOW
                if (!empty($validJobTime) && ($focus->last_run < $validJobTime[0]) && ($now > $validJobTime[0])) {
                    // cn: empty() bug 5914;
                    // if(!empty) should be checked, becasue if a scheduler is defined to run every day 4pm, then after 4pm, and it runs as 4pm,
                    // the $validJobTime will be empty, and it should not catch up.
                    // If $focus->last_run is the day before yesterday,  it should run yesterday and tomorrow,
                    // but it hadn't run yesterday, then it should catch up today.
                    // But today is already filtered out when doing date check before. The catch up will not work on this occasion.
                    // If the scheduler runs at least one time on each day, I think this bug can be avoided.
                    $validJobTime[] = $now;
                }
            }
        }
        return $validJobTime;
    }

    public function handleIntervalType($type, $value, $mins, $hours)
    {
        global $mod_strings;
        /* [0]:min [1]:hour [2]:day of month [3]:month [4]:day of week */
        $days = array(
            1 => $mod_strings['LBL_MON'],
            2 => $mod_strings['LBL_TUE'],
            3 => $mod_strings['LBL_WED'],
            4 => $mod_strings['LBL_THU'],
            5 => $mod_strings['LBL_FRI'],
            6 => $mod_strings['LBL_SAT'],
            0 => $mod_strings['LBL_SUN'],
            '*' => $mod_strings['LBL_ALL']
        );
        switch ($type) {
            case 0: // minutes
                if ($value == '0') {
                    //return;
                    return trim($mod_strings['LBL_ON_THE']) . $mod_strings['LBL_HOUR_SING'];
                } elseif (!preg_match('/[^0-9]/', (string) $hours) && !preg_match('/[^0-9]/', (string) $value)) {
                    return;
                } elseif (preg_match('/\*\//', (string) $value)) {
                    $value = str_replace('*/', '', (string) $value);

                    return $value . $mod_strings['LBL_MINUTES'];
                } elseif (!preg_match('[^0-9]', (string) $value)) {
                    return $mod_strings['LBL_ON_THE'] . $value . $mod_strings['LBL_MIN_MARK'];
                }

                return $value;

            case 1: // hours
                global $current_user;
                if (preg_match('/\*\//', (string) $value)) { // every [SOME INTERVAL] hours
                    $value = str_replace('*/', '', (string) $value);

                    return $value . $mod_strings['LBL_HOUR'];
                } elseif (preg_match(
                    '/[^0-9]/',
                    (string) $mins
                )) { // got a range, or multiple of mins, so we return an 'Hours' label
                    return $value;
                }    // got a "minutes" setting, so it will be at some o'clock.
                $datef = $current_user->getUserDateTimePreferences();

                return date($datef['time'], strtotime($value . ':' . str_pad($mins, 2, '0', STR_PAD_LEFT)));

            case 2: // day of month
                if (preg_match('/\*/', (string) $value)) {
                    return $value;
                }

                return date('jS', strtotime('December ' . $value));


            case 3: // months
                return date('F', strtotime('2005-' . $value . '-01'));
            case 4: // days of week
                return $days[$value];
            default:
                return 'bad'; // no condition to touch this branch
        }
    }

    public function setIntervalHumanReadable()
    {
        global $mod_strings;

        /* [0]:min [1]:hour [2]:day of month [3]:month [4]:day of week */
        $ints = $this->intervalParsed;
        $intVal = array('-', ',');
        $intSub = array($mod_strings['LBL_RANGE'], $mod_strings['LBL_AND']);
        $intInt = array(0 => $mod_strings['LBL_MINS'], 1 => $mod_strings['LBL_HOUR']);
        $tempInt = '';
        $iteration = '';

        foreach ($ints['raw'] as $key => $interval) {
            if ($tempInt !== $iteration) {
                $tempInt .= '; ';
            }
            $iteration = $tempInt;

            if ($interval != '*' && $interval != '*/1') {
                if (false !== strpos((string) $interval, ',')) {
                    $exIndiv = explode(',', $interval);
                    foreach ($exIndiv as $val) {
                        if (false !== strpos($val, '-')) {
                            $exRange = explode('-', $val);
                            foreach ($exRange as $valRange) {
                                if ($tempInt != '') {
                                    $tempInt .= $mod_strings['LBL_AND'];
                                }
                                $tempInt .= $this->handleIntervalType($key, $valRange, $ints['raw'][0], $ints['raw'][1]);
                            }
                        } elseif ($tempInt !== $iteration) {
                            $tempInt .= $mod_strings['LBL_AND'];
                        }
                        $tempInt .= $this->handleIntervalType($key, $val, $ints['raw'][0], $ints['raw'][1]);
                    }
                } elseif (false !== strpos((string) $interval, '-')) {
                    $exRange = explode('-', $interval);
                    $tempInt .= $mod_strings['LBL_FROM'];
                    $check = $tempInt;

                    foreach ($exRange as $val) {
                        if ($tempInt === $check) {
                            $tempInt .= $this->handleIntervalType($key, $val, $ints['raw'][0], $ints['raw'][1]);
                            $tempInt .= $mod_strings['LBL_RANGE'];
                        } else {
                            $tempInt .= $this->handleIntervalType($key, $val, $ints['raw'][0], $ints['raw'][1]);
                        }
                    }
                } elseif (false !== strpos((string) $interval, '*/')) {
                    $tempInt .= $mod_strings['LBL_EVERY'];
                    $tempInt .= $this->handleIntervalType($key, $interval, $ints['raw'][0], $ints['raw'][1]);
                } else {
                    $tempInt .= $this->handleIntervalType($key, $interval, $ints['raw'][0], $ints['raw'][1]);
                }
            }
        } // end foreach()

        if ($tempInt == '') {
            $this->intervalHumanReadable = $mod_strings['LBL_OFTEN'];
        } else {
            $tempInt = trim($tempInt);
            if (';' == substr($tempInt, (strlen($tempInt) - 1), strlen($tempInt))) {
                $tempInt = substr($tempInt, 0, (strlen($tempInt) - 1));
            }
            $this->intervalHumanReadable = $tempInt;
        }
    }

    /* take an integer and return its suffix */
    public function setStandardArraysAttributes()
    {
        global $mod_strings;
        global $app_list_strings; // using from month _dom list

        $suffArr = array('', 'st', 'nd', 'rd');
        for ($i = 1; $i < 32; $i++) {
            if ($i > 3 && $i < 21) {
                $this->suffixArray[$i] = $i . "th";
            } elseif (substr($i, -1, 1) < 4 && substr($i, -1, 1) > 0) {
                $this->suffixArray[$i] = $i . $suffArr[substr($i, -1, 1)];
            } else {
                $this->suffixArray[$i] = $i . "th";
            }
            $this->datesArray[$i] = $i;
        }

        $this->dayInt = array('*', 1, 2, 3, 4, 5, 6, 0);
        $this->dayLabel = array('*', $mod_strings['LBL_MON'], $mod_strings['LBL_TUE'], $mod_strings['LBL_WED'], $mod_strings['LBL_THU'], $mod_strings['LBL_FRI'], $mod_strings['LBL_SAT'], $mod_strings['LBL_SUN']);
        $this->monthsInt = array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12);
        $this->monthsLabel = $app_list_strings['dom_cal_month_long'];
        $this->metricsVar = array("*", "/", "-", ",");
        $this->metricsVal = array(' every ', '', ' thru ', ' and ');
    }

    /**
     *  takes the serialized interval string and renders it into an array
     */
    public function parseInterval()
    {
        $ws = array(' ', '\r', '\t');
        $blanks = array('', '', '');

        $intv = $this->job_interval;
        $rawValues = explode('::', $intv);
        $rawProcessed = str_replace($ws, $blanks, $rawValues); // strip all whitespace

        $hours = $rawValues[1] . ':::' . $rawValues[0];
        $months = $rawValues[3] . ':::' . $rawValues[2];

        $intA = array(
            'raw' => $rawProcessed,
            'hours' => $hours,
            'months' => $months,
        );

        $this->intervalParsed = $intA;
    }

    /**
     * checks for cURL libraries
     */
    public function checkCurl()
    {
        global $mod_strings;

        if (!function_exists('curl_init')) {
            echo '
            <table cellpadding="0" cellspacing="0" width="100%" border="0" class="list view">
                <tr height="20">
                    <th width="25%" colspan="2"><span>
                        ' . $mod_strings['LBL_WARN_CURL_TITLE'] . '
                    </span></td>
                </tr>
                <tr class="oddListRowS1" >
                    <td scope="row" valign=TOP width="20%"><span>
                        ' . $mod_strings['LBL_WARN_CURL'] . '
                    <td scope="row" valign=TOP width="80%"><span>
                        <span class=error>' . $mod_strings['LBL_WARN_NO_CURL'] . '</span>
                    </span></td>
                </tr>
            </table>
            <br>';
        }
    }

    public function displayCronInstructions()
    {
        global $mod_strings;
        global $sugar_config;
        $error = '';
        if (!isset($_SERVER['Path'])) {
            $_SERVER['Path'] = getenv('Path');
        }
        if (is_windows()) {
            if (isset($_SERVER['Path']) && !empty($_SERVER['Path'])) { // IIS IUSR_xxx may not have access to Path or it is not set
                if (!strpos((string) $_SERVER['Path'], 'php')) {
                    // $error = '<em>'.$mod_strings['LBL_NO_PHP_CLI'].'</em>';
                }
            }
        } else {
            if (isset($_SERVER['Path']) && !empty($_SERVER['Path'])) { // some Linux servers do not make this available
                if (!strpos((string) $_SERVER['PATH'], 'php')) {
                    // $error = '<em>'.$mod_strings['LBL_NO_PHP_CLI'].'</em>';
                }
            }
        }



        if (is_windows()) {
            echo '<br>';
            echo '
                <table cellpadding="0" cellspacing="0" width="100%" border="0" class="list view">
                <tr height="20">
                    <th><span>
                        ' . $mod_strings['LBL_CRON_INSTRUCTIONS_WINDOWS'] . '
                    </span></th>
                </tr>
                <tr class="evenListRowS1">
                    <td scope="row" valign="top" width="70%"><span>
                        ' . $mod_strings['LBL_CRON_WINDOWS_DESC'] . '<br>
                        <b>cd /D ' . realpath('./') . '<br>
                        php.exe -f cron.php</b>
                    </span></td>
                </tr>
            </table>';
        } else {
            require_once 'install/install_utils.php';
            $webServerUser = getRunningUser();
            if ($webServerUser == '') {
                $webServerUser = '<web_server_user>';
            }
            echo '<br>';
            echo '
                <table cellpadding="0" cellspacing="0" width="100%" border="0" class="list view">
                <tr height="20">
                    <th><span>
                        ' . $mod_strings['LBL_CRON_INSTRUCTIONS_LINUX'] . '
                    </span></th>
                </tr>
                <tr>
                    <td scope="row" valign=TOP class="oddListRowS1" bgcolor="#fdfdfd" width="70%"><span style="font-weight:unset;">
                        ' . $mod_strings['LBL_CRON_LINUX_DESC1'] . '<br>
                        <b>sudo crontab -e -u ' . $webServerUser . '</b><br> ' . $mod_strings['LBL_CRON_LINUX_DESC2'] . '<br>
                        <b>*&nbsp;&nbsp;&nbsp;&nbsp;*&nbsp;&nbsp;&nbsp;&nbsp;*&nbsp;&nbsp;&nbsp;&nbsp;*&nbsp;&nbsp;&nbsp;&nbsp;*&nbsp;&nbsp;&nbsp;&nbsp;
                        cd ' . realpath('./') . '; php -f cron.php > /dev/null 2>&1</b>
                        <br>' . $error . '
                    </span></td>
                </tr>
            </table>';
        }
    }

    /**
     * Archives schedulers of the same functionality, then instantiates new
     * ones.
     */
    public function rebuildDefaultSchedulers()
    {
        $mod_strings = return_module_language($GLOBALS['current_language'], 'Schedulers');
        // truncate scheduler-related tables
        $this->db->query('DELETE FROM schedulers');

        $sched1 = BeanFactory::newBean('Schedulers');
        $sched1->name               = $mod_strings['LBL_OOTB_WORKFLOW'];
        $sched1->job                = 'function::processAOW_Workflow';
        $sched1->date_time_start    = create_date(2015, 1, 1) . ' ' . create_time(0, 0, 1);
        $sched1->date_time_end      = null;
        $sched1->job_interval       = '*::*::*::*::*';
        $sched1->status             = 'Active';
        $sched1->created_by         = '1';
        $sched1->modified_user_id   = '1';
        $sched1->catch_up           = '1';
        $sched1->save();

        $sched2 = BeanFactory::newBean('Schedulers');
        $sched2->name               = $mod_strings['LBL_OOTB_REPORTS'];
        $sched2->job                = 'function::aorRunScheduledReports';
        $sched2->date_time_start    = create_date(2015, 1, 1) . ' ' . create_time(0, 0, 1);
        $sched2->date_time_end      = null;
        $sched2->job_interval       = '*::*::*::*::*';
        $sched2->status             = 'Active';
        $sched2->created_by         = '1';
        $sched2->modified_user_id   = '1';
        $sched2->catch_up           = '1';
        $sched2->save();

        $sched3 = BeanFactory::newBean('Schedulers');
        $sched3->name               = $mod_strings['LBL_OOTB_TRACKER'];
        $sched3->job                = 'function::trimTracker';
        $sched3->date_time_start    = create_date(2015, 1, 1) . ' ' . create_time(0, 0, 1);
        $sched3->date_time_end      = null;
        $sched3->job_interval       = '0::2::1::*::*';
        $sched3->status             = 'Active';
        $sched3->created_by         = '1';
        $sched3->modified_user_id   = '1';
        $sched3->catch_up           = '1';
        $sched3->save();

        $sched4 = BeanFactory::newBean('Schedulers');
        $sched4->name                = $mod_strings['LBL_OOTB_IE'];
        $sched4->job                = 'function::pollMonitoredInboxesAOP';
        $sched4->date_time_start    = create_date(2015, 1, 1) . ' ' . create_time(0, 0, 1);
        $sched4->date_time_end        = null;
        $sched4->job_interval        = '*::*::*::*::*';
        $sched4->status                = 'Active';
        $sched4->created_by            = '1';
        $sched4->modified_user_id    = '1';
        $sched4->catch_up            = '0';
        $sched4->save();

        $sched5 = BeanFactory::newBean('Schedulers');
        $sched5->name              = $mod_strings['LBL_OOTB_BOUNCE'];
        $sched5->job               = 'function::pollMonitoredInboxesForBouncedCampaignEmails';
        $sched5->date_time_start   = create_date(2015, 1, 1) . ' ' . create_time(0, 0, 1);
        $sched5->date_time_end     = null;
        $sched5->job_interval      = '0::2-6::*::*::*';
        $sched5->status            = 'Active';
        $sched5->created_by        = '1';
        $sched5->modified_user_id  = '1';
        $sched5->catch_up          = '1';
        $sched5->save();

        $sched6 = BeanFactory::newBean('Schedulers');
        $sched6->name             = $mod_strings['LBL_OOTB_CAMPAIGN'];
        $sched6->job              = 'function::runMassEmailCampaign';
        $sched6->date_time_start  = create_date(2015, 1, 1) . ' ' . create_time(0, 0, 1);
        $sched6->date_time_end    = null;
        $sched6->job_interval     = '0::2-6::*::*::*';
        $sched6->status           = 'Active';
        $sched6->created_by       = '1';
        $sched6->modified_user_id = '1';
        $sched6->catch_up         = '1';
        $sched6->save();

        $sched7 = BeanFactory::newBean('Schedulers');
        $sched7->name               = $mod_strings['LBL_OOTB_PRUNE'];
        $sched7->job                = 'function::pruneDatabase';
        $sched7->date_time_start    = create_date(2015, 1, 1) . ' ' . create_time(0, 0, 1);
        $sched7->date_time_end      = null;
        $sched7->job_interval       = '0::4::1::*::*';
        $sched7->status             = 'Inactive';
        $sched7->created_by         = '1';
        $sched7->modified_user_id   = '1';
        $sched7->catch_up           = '0';
        $sched7->save();

        $sched8 = BeanFactory::newBean('Schedulers');
        $sched8->name               = $mod_strings['LBL_OOTB_LUCENE_INDEX'];
        $sched8->job                = 'function::aodIndexUnindexed';
        $sched8->date_time_start    = create_date(2015, 1, 1) . ' ' . create_time(0, 0, 1);
        $sched8->date_time_end      = null;
        $sched8->job_interval       = "0::0::*::*::*";
        $sched8->status             = 'Active';
        $sched8->created_by         = '1';
        $sched8->modified_user_id   = '1';
        $sched8->catch_up           = '0';
        $sched8->save();

        $sched9 = BeanFactory::newBean('Schedulers');
        $sched9->name               = $mod_strings['LBL_OOTB_OPTIMISE_INDEX'];
        $sched9->job                = 'function::aodOptimiseIndex';
        $sched9->date_time_start    = create_date(2015, 1, 1) . ' ' . create_time(0, 0, 1);
        $sched9->date_time_end      = null;
        $sched9->job_interval       = "0::*/3::*::*::*";
        $sched9->status             = 'Active';
        $sched9->created_by         = '1';
        $sched9->modified_user_id   = '1';
        $sched9->catch_up           = '0';
        $sched9->save();

        $sched12 = BeanFactory::newBean('Schedulers');
        $sched12->name               = $mod_strings['LBL_OOTB_SEND_EMAIL_REMINDERS'];
        $sched12->job                = 'function::sendEmailReminders';
        $sched12->date_time_start    = create_date(2015, 1, 1) . ' ' . create_time(0, 0, 1);
        $sched12->date_time_end      = null;
        $sched12->job_interval       = '*::*::*::*::*';
        $sched12->status             = 'Active';
        $sched12->created_by         = '1';
        $sched12->modified_user_id   = '1';
        $sched12->catch_up           = '0';
        $sched12->save();

        $sched13 = BeanFactory::newBean('Schedulers');
        $sched13->name               = $mod_strings['LBL_OOTB_CLEANUP_QUEUE'];
        $sched13->job                = 'function::cleanJobQueue';
        $sched13->date_time_start    = create_date(2015, 1, 1) . ' ' . create_time(0, 0, 1);
        $sched13->date_time_end      = null;
        $sched13->job_interval       = '0::5::*::*::*';
        $sched13->status             = 'Active';
        $sched13->created_by         = '1';
        $sched13->modified_user_id   = '1';
        $sched13->catch_up           = '0';
        $sched13->save();

        $sched14 = BeanFactory::newBean('Schedulers');
        $sched14->name              = $mod_strings['LBL_OOTB_REMOVE_DOCUMENTS_FROM_FS'];
        $sched14->job               = 'function::removeDocumentsFromFS';
        $sched14->date_time_start   = create_date(2015, 1, 1) . ' ' . create_time(0, 0, 1);
        $sched14->date_time_end     = null;
        $sched14->job_interval      = '0::3::1::*::*';
        $sched14->status            = 'Active';
        $sched14->created_by        = '1';
        $sched14->modified_user_id  = '1';
        $sched14->catch_up          = '0';
        $sched14->save();

        $sched15 = BeanFactory::newBean('Schedulers');
        $sched15->name               = $mod_strings['LBL_OOTB_SUITEFEEDS'];
        $sched15->job                = 'function::trimSugarFeeds';
        $sched15->date_time_start    = create_date(2015, 1, 1) . ' ' . create_time(0, 0, 1);
        $sched15->date_time_end      = null;
        $sched15->job_interval       = '0::2::1::*::*';
        $sched15->status             = 'Active';
        $sched15->created_by         = '1';
        $sched15->modified_user_id   = '1';
        $sched15->catch_up           = '1';
        $sched15->save();

        $sched16 = new Scheduler();
        $sched16->name = $mod_strings['LBL_OOTB_GOOGLE_CAL_SYNC'];
        $sched16->job = 'function::syncGoogleCalendar';
        $sched16->date_time_start = create_date(2015, 1, 1) . ' ' . create_time(0, 0, 1);
        $sched16->date_time_end = null;
        $sched16->job_interval = '*/15::*::*::*::*';
        $sched16->status = 'Active';
        $sched16->created_by = '1';
        $sched16->modified_user_id = '1';
        $sched16->catch_up = '0';
        $sched16->save();

        $sched17 = new Scheduler;
        $sched17->name = $mod_strings['LBL_OOTB_ELASTIC_INDEX'];
        $sched17->job = 'function::runElasticSearchIndexerScheduler';
        $sched17->date_time_start = create_date(2015, 1, 1) . ' ' . create_time(0, 0, 1);
        $sched17->date_time_end = null;
        $sched17->job_interval = '30::4::*::*::*';
        $sched17->status = 'Active';
        $sched17->created_by = '1';
        $sched17->modified_user_id = '1';
        $sched17->catch_up = '0';
        $sched17->save();
    }

    ////	END SCHEDULER HELPER FUNCTIONS
    ///////////////////////////////////////////////////////////////////////////


    ///////////////////////////////////////////////////////////////////////////
    ////	STANDARD SUGARBEAN OVERRIDES
    /**
     * function overrides the one in SugarBean.php
     */
    public function create_export_query($order_by, $where, $show_deleted = 0)
    {
        return $this->create_new_list_query($order_by, $where, array(), array(), $show_deleted);
    }

    /**
     * function overrides the one in SugarBean.php
     */
    public function fill_in_additional_list_fields()
    {
        $this->fill_in_additional_detail_fields();
    }

    /**
     * function overrides the one in SugarBean.php
     */
    public function fill_in_additional_detail_fields()
    { }

    /**
     * function overrides the one in SugarBean.php
     */
    public function get_list_view_data()
    {
        global $mod_strings;
        $temp_array = $this->get_list_view_array();
        $temp_array["ENCODED_NAME"] = $this->name;
        $this->parseInterval();
        $this->setIntervalHumanReadable();
        $temp_array['JOB_INTERVAL'] = $this->intervalHumanReadable;
        if ($this->date_time_end == '2020-12-31 23:59' || $this->date_time_end == '') {
            $temp_array['DATE_TIME_END'] = $mod_strings['LBL_PERENNIAL'];
        }
        $this->created_by_name = get_assigned_user_name($this->created_by);
        $this->modified_by_name = get_assigned_user_name($this->modified_user_id);
        return $temp_array;
    }

    /**
     * returns the bean name - overrides SugarBean's
     */
    public function get_summary_text()
    {
        return $this->name;
    }
    ////	END STANDARD SUGARBEAN OVERRIDES
    ///////////////////////////////////////////////////////////////////////////
    public static function getJobsList()
    {
        if (empty(self::$job_strings)) {
            global $mod_strings;
            include_once('modules/Schedulers/_AddJobsHere.php');

            // job functions
            self::$job_strings = array('url::' => 'URL');
            foreach ($job_strings as $k => $v) {
                self::$job_strings['function::' . $v] = $mod_strings['LBL_' . strtoupper($v)];
            }
        }
        return self::$job_strings;
    }
}
