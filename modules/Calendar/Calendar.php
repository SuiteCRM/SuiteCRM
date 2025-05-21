<?php

/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2024 SalesAgility Ltd.
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

require_once('include/utils/activity_utils.php');
require_once('modules/Calendar/CalendarUtils.php');
require_once('modules/Calendar/CalendarActivity.php');

#[\AllowDynamicProperties]
class Calendar
{
    public $activityList = [
        "FP_events" => [
            "showCompleted" => true,
            "start" => "date_start",
            "end" => "date_end",
        ],
        "Meetings" => [
            "showCompleted" => true,
            "start" => "date_start",
            "end" => "date_end",
            ],
        "Calls" => [
            "showCompleted" => true,
            "start" => "date_start",
            "end" => "date_end",
            ],
        "Tasks" => [
            "showCompleted" => true,
            "start" => "date_due",
            "end" => "date_due",
            ]
        ];

    public $views = array("agendaDay" => array(),"basicDay" => array(), "basicWeek" => array(), "agendaWeek" => array(),"month" => array(), "sharedMonth" => array(), "sharedWeek" => array());

    public $view = 'agendaWeek'; // current view
    public $style; // calendar style (basic or advanced)
    public $dashlet = false; // if is displayed in dashlet
    public $date_time; // current date

    public $show_tasks = true;
    public $show_calls = true;
    public $show_completed = true;
    public $enable_repeat = true;

    public $time_step = 60; // time step of each slot in minutes

    public $acts_arr = array(); // Array of activities objects
    public $items = array(); // Array of activities data to be displayed
    public $shared_ids = array(); // ids of users for shared view


    public $cells_per_day; // entire 24h day count of slots
    public $grid_start_ts; // start timestamp of calendar grid

    public $day_start_time; // working day start time in format '11:00'
    public $day_end_time; // working day end time in format '11:00'
    public $scroll_slot; // first slot of working day
    public $celcount; // count of slots in a working day

    /**
     * @var bool $print Whether is print mode.
     */
    private $print = false;

    /**
     * constructor
     * @param string $view
     * @param array $time_arr
     * @param array $views
     */
    public function __construct($view = "agendaWeek", $time_arr = array())
    {
        global $current_user, $timedate, $current_language;

        $this->view = $view;

        if (!array_key_exists($this->view, $this->views)) {
            $this->view = 'agendaWeek';
        }

        $date_arr = array();
        if (!empty($_REQUEST['day'])) {
            $_REQUEST['day'] = (int)$_REQUEST['day'];
        }
        if (!empty($_REQUEST['month'])) {
            $_REQUEST['month'] = (int)$_REQUEST['month'];
        }

        if (!empty($_REQUEST['day'])) {
            $date_arr['day'] = $_REQUEST['day'];
        }
        if (!empty($_REQUEST['month'])) {
            $date_arr['month'] = $_REQUEST['month'];
        }
        if (!empty($_REQUEST['week'])) {
            $date_arr['week'] = $_REQUEST['week'];
        }
        if (!empty($_REQUEST['mobile'])) {
            $date_arr['mobile'] = $_REQUEST['mobile'];
        }

        if (!empty($_REQUEST['year'])) {
            if ($_REQUEST['year'] > 2037 || $_REQUEST['year'] < 1970) {
                $calendarStrings = return_module_language($current_language, 'Calendar');
                print($calendarStrings['ERR_YEAR_BETWEEN']);
                exit;
            }
            $date_arr['year'] = $_REQUEST['year'];
        }

        if (empty($_REQUEST['day'])) {
            $_REQUEST['day'] = "";
        }
        if (empty($_REQUEST['week'])) {
            $_REQUEST['week'] = "";
        }
        if (empty($_REQUEST['month'])) {
            $_REQUEST['month'] = "";
        }
        if (empty($_REQUEST['year'])) {
            $_REQUEST['year'] = "";
        }
        if (empty($_REQUEST['mobile'])) {
            $_REQUEST['mobile'] = "";
        }

        // if date is not set in request use current date
        if (empty($date_arr) || !isset($date_arr['year']) || !isset($date_arr['month']) || !isset($date_arr['day'])) {
            $today = $timedate->getNow(true);
            $date_arr = array(
                  'year' => $today->year,
                  'month' => $today->month,
                  'day' => $today->day,
                  'mobile' => $today->day,
            );
        }

        $current_date_db = $date_arr['year']."-".str_pad($date_arr['month'], 2, "0", STR_PAD_LEFT)."-".str_pad($date_arr['day'], 2, "0", STR_PAD_LEFT);
        $this->date_time = $GLOBALS['timedate']->fromString($current_date_db);

        $this->show_tasks = $current_user->getPreference('show_tasks');
        if (is_null($this->show_tasks)) {
            $this->show_tasks = SugarConfig::getInstance()->get('calendar.show_tasks_by_default', true);
        }

        $this->show_calls = $current_user->getPreference('show_calls');
        if (is_null($this->show_calls)) {
            $this->show_calls = SugarConfig::getInstance()->get('calendar.show_calls_by_default', true);
        }

        // Show completed Meetings, Calls, Tasks
        $this->show_completed = $current_user->getPreference('show_completed');
        if (is_null($this->show_completed)) {
            $this->show_completed = SugarConfig::getInstance()->get('calendar.show_completed_by_default', true);
        }

        $this->enable_repeat = SugarConfig::getInstance()->get('calendar.enable_repeat', true);

        if (in_array($this->view, array('month','year'))) {
            $this->style = "basic";
        } else {
            $displayTimeslots = $GLOBALS['current_user']->getPreference('calendar_display_timeslots');
            if (is_null($displayTimeslots)) {
                $displayTimeslots = SugarConfig::getInstance()->get('calendar.display_timeslots', true);
            }
            if (!$displayTimeslots) {
                switch ($this->view) {
                    case "agendaDay":
                        $this->view = "basicDay";
                    break;
                    case "agendaWeek":
                        $this->view = "basicWeek";
                    break;
                }
            } else {
                switch ($this->view) {
                    case "basicDay":
                        $this->view = "agendaDay";
                        break;
                    case "basicWeek":
                        $this->view = "agendaWeek";
                        break;
                }
            }
        }

        $this->day_start_time = $current_user->getPreference('day_start_time');
        if (is_null($this->day_start_time)) {
            $this->day_start_time = SugarConfig::getInstance()->get('calendar.default_day_start', "08:00");
        }
        $this->day_end_time = $current_user->getPreference('day_end_time');
        if (is_null($this->day_end_time)) {
            $this->day_end_time = SugarConfig::getInstance()->get('calendar.default_day_end', "19:00");
        }

        if ($this->view == "day") {
            $this->time_step = SugarConfig::getInstance()->get('calendar.day_timestep', 15);
        } else {
            if ($this->view == "week" || $this->view == "shared") {
                $this->time_step = SugarConfig::getInstance()->get('calendar.week_timestep', 30);
            } else {
                if ($this->view == "month") {
                    $this->time_step = SugarConfig::getInstance()->get('calendar.month_timestep', 60);
                } else {
                    $this->time_step = 60;
                }
            }
        }
        $this->cells_per_day = 24 * (60 / $this->time_step);
        $this->calculate_grid_start_ts();
        $this->calculate_day_range();
    }

    /**
     * Load activities data to array
     */
    public function load_activities()
    {
        $field_list = CalendarUtils::get_fields();

        $i = 0;
        foreach ($this->acts_arr as $user_id => $acts) {
            if (isset($acts) && empty($acts)) {
                $shared_calendar_separate = $GLOBALS['current_user']->getPreference('calendar_display_shared_separate');
                if (is_null($shared_calendar_separate)) {
                    $shared_calendar_separate = SugarConfig::getInstance()->get('calendar.calendar_display_shared_separate', true);
                }
                //if no calendar items we add the user to the list.
                if ($shared_calendar_separate) {
                    //$this->items[ $item['user_id'] ][] = $item;
                    $this->items[ $user_id ][] = array();
                } else {
                    $this->items[ $GLOBALS['current_user']->id ][] = array();
                }
                continue;
            }
            foreach ($acts as $act) {
                $item = array();
                $item['user_id'] = $user_id;
                $item['module_name'] = $act->sugar_bean->module_dir;
                $item['type'] = strtolower($act->sugar_bean->object_name);
                $item['assigned_user_id'] = $act->sugar_bean->assigned_user_id;
                $item['record'] = $act->sugar_bean->id;
                $item['name'] = html_entity_decode($act->sugar_bean->name, ENT_QUOTES) . ' ' . $act->sugar_bean->assigned_user_name;
                $item['description'] = $act->sugar_bean->description;

                if (isset($act->sugar_bean->duration_hours)) {
                    $item['duration_hours'] = $act->sugar_bean->duration_hours;
                    $item['duration_minutes'] = $act->sugar_bean->duration_minutes;
                }

                $item['detail'] = 0;
                $item['edit'] = 0;

                if ($act->sugar_bean->ACLAccess('DetailView')) {
                    $item['detail'] = 1;
                }
                if ($act->sugar_bean->ACLAccess('Save')) {
                    $item['edit'] = 1;
                }

                if (empty($act->sugar_bean->id)) {
                    $item['detail'] = 0;
                    $item['edit'] = 0;
                }

                if (!empty($act->sugar_bean->repeat_parent_id)) {
                    $item['repeat_parent_id'] = $act->sugar_bean->repeat_parent_id;
                }

                if ($item['detail'] == 1) {
                    if (isset($field_list[$item['module_name']])) {
                        foreach ($field_list[$item['module_name']] as $field) {
                            if (!isset($item[$field]) && isset($act->sugar_bean->$field)) {
                                $item[$field] = $act->sugar_bean->$field;
                                if (empty($item[$field])) {
                                    $item[$field] = "";
                                }
                            }
                        }
                    }
                }

                if (!empty($act->sugar_bean->parent_type) && !empty($act->sugar_bean->parent_id)) {
                    $focus = BeanFactory::getBean($act->sugar_bean->parent_type, $act->sugar_bean->parent_id);
                    // If the bean wasn't loaded, e.g. insufficient permissions
                    if (!empty($focus)) {
                        $item['related_to'] = $focus->name;
                    }
                }

                if (!isset($item['duration_hours']) || empty($item['duration_hours'])) {
                    $item['duration_hours'] = 0;
                }
                if (!isset($item['duration_minutes']) || empty($item['duration_minutes'])) {
                    $item['duration_minutes'] = 0;
                }

                if (isset($this->activityList[ $act->sugar_bean->module_name ]['start']) && !empty($this->activityList[ $act->sugar_bean->module_name ]['start'])) {
                    $item = array_merge($item, CalendarUtils::get_time_data($act->sugar_bean, $this->activityList[ $act->sugar_bean->module_name ]['start'], $this->activityList[ $act->sugar_bean->module_name ]['end']));
                } else {
                    $item = array_merge($item, CalendarUtils::get_time_data($act->sugar_bean));
                }


                $shared_calendar_separate = $GLOBALS['current_user']->getPreference('calendar_display_shared_separate');
                if (is_null($shared_calendar_separate)) {
                    $shared_calendar_separate = SugarConfig::getInstance()->get('calendar.calendar_display_shared_separate', true);
                }
                //if no calendar items we add the user to the list.
                if ($shared_calendar_separate) {
                    $this->items[ $item['user_id'] ][] = $item;
                } else {
                    $this->items[ $GLOBALS['current_user']->id ][] = $item;
                }
            }
            $i++;
        }
    }

    /**
     * initialize ids of shared users
     */
    public function init_shared()
    {
        global $current_user;


        $user_ids = $current_user->getPreference('shared_ids');
        if (!empty($user_ids) && (is_countable($user_ids) ? count($user_ids) : 0) != 0 && !isset($_REQUEST['shared_ids'])) {
            $this->shared_ids = $user_ids;
        } else {
            if (isset($_REQUEST['shared_ids']) && (is_countable($_REQUEST['shared_ids']) ? count($_REQUEST['shared_ids']) : 0) > 0) {
                $this->shared_ids = $_REQUEST['shared_ids'];
                $current_user->setPreference('shared_ids', $_REQUEST['shared_ids']);
            } else {
                $this->shared_ids = array($current_user->id);
            }
        }
    }

    /**
     * Calculate timestamp the calendar grid should be started from
     */
    protected function calculate_grid_start_ts()
    {
        if ($this->view == "agendaWeek" || $this->view == "shared") {
            $week_start = CalendarUtils::get_first_day_of_week($this->date_time);
            $this->grid_start_ts = $week_start->format('U') + $week_start->getOffset();
        } else {
            if ($this->view == "month") {
                $month_start = $this->date_time->get_day_by_index_this_month(0);
                $week_start = CalendarUtils::get_first_day_of_week($month_start);
                $this->grid_start_ts = $week_start->format('U') + $week_start->getOffset(); // convert to timestamp, ignore tz
            } else {
                if ($this->view == "agendaDay") {
                    $this->grid_start_ts = $this->date_time->format('U') + $this->date_time->getOffset();
                }
            }
        }
    }

    /**
     * calculate count of timeslots per visible day, calculates day start and day end in minutes
     */
    public function calculate_day_range()
    {
        list($hour_start, $minute_start) =  explode(":", $this->day_start_time);
        list($hour_end, $minute_end) =  explode(":", $this->day_end_time);
        $this->scroll_slot = (int)($hour_start * (60 / $this->time_step) + ($minute_start / $this->time_step));
        $this->celcount = (($hour_end * 60 + $minute_end) - ($hour_start * 60 + $minute_start)) / $this->time_step;
    }

    /**
     * loads array of objects
     * @param User $user user object
     * @param string $type
     */
    public function add_activities($user, $type='sugar')
    {
        global $timedate;
        $start_date_time = $this->date_time;
        if ($this->view == 'agendaWeek'|| $this->view == 'basicWeek'  || $this->view == 'sharedWeek') {
            $start_date_time = CalendarUtils::get_first_day_of_week($this->date_time);
            $end_date_time = $start_date_time->get("+7 days");
        } else {
            if ($this->view == 'month' || $this->view == "sharedMonth") {
                $start_date_time = $this->date_time->get_day_by_index_this_month(0);
                $end_date_time = $start_date_time->get("+".$start_date_time->format('t')." days");
                $start_date_time = CalendarUtils::get_first_day_of_week($start_date_time);
                $end_date_time = CalendarUtils::get_first_day_of_week($end_date_time)->get("+7 days");
            } else {
                $end_date_time = $this->date_time->get("+1 day");
            }
        }

        $start_date_time = $start_date_time->get("-5 days"); // 5 days step back to fetch multi-day activities that

        $acts_arr = array();
        if ($type == 'vfb') {
            $acts_arr = CalendarActivity::get_freebusy_activities($user, $start_date_time, $end_date_time);
        } else {
            $acts_arr = CalendarActivity::get_activities($this->activityList, $user->id, $this->show_tasks, $start_date_time, $end_date_time, $this->view, $this->show_calls, $this->show_completed);
        }


        //$this->acts_arr[$user->id] = $acts_arr;
        $this->acts_arr[$user->id] = $acts_arr;
    }

    /**
     * Get date string of next or previous calendar grid
     * @param string $direction next or previous
     * @return string
     */
    public function get_neighbor_date_str($direction)
    {
        if ($direction == "previous") {
            $sign = "-";
        } else {
            $sign = "+";
        }

        if ($this->view == 'month' || $this->view == "sharedMonth") {
            $day = $this->date_time->get_day_by_index_this_month(0)->get($sign."1 month")->get_day_begin(1);
        } else {
            if ($this->view == 'agendaWeek' || $this->view == 'sharedWeek' || $this->view == 'basicWeek') {
                $day = CalendarUtils::get_first_day_of_week($this->date_time);
                $day = $day->get($sign."7 days");
            } else {
                if ($this->view == 'agendaDay' || $this->view == 'basicDay') {
                    $day = $this->date_time->get($sign."1 day")->get_day_begin();
                } else {
                    if ($this->view == 'year') {
                        $day = $this->date_time->get($sign."1 year")->get_day_begin();
                    } else {
                        $calendarStrings = return_module_language($GLOBALS['current_language'], 'Calendar');
                        return $calendarStrings['ERR_NEIGHBOR_DATE'];
                    }
                }
            }
        }
        return $day->get_date_str();
    }

    public function setPrint($print)
    {
        $this->print = $print;
    }

    public function isPrint()
    {
        return $this->print;
    }
}
