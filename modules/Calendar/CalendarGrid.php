<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}
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



global $timedate;

#[\AllowDynamicProperties]
class CalendarGrid
{
    protected $cal; // Calendar object
    public $style = ""; // style of calendar (basic or advanced); advanced contains time slots
    protected $today_ts; // timestamp of today
    protected $weekdays; // string array of names of week days
    protected $startday; // first day of week
    protected $scrollable; // scrolling in calendar
    protected $time_step = 30; // time step
    protected $time_format; // user time format
    protected $date_time_format; // user date time format
    protected $scroll_height; // height of scrollable div

    /**
     * constructor
     * @param Calendar $cal
     */
    public function __construct(Calendar $cal)
    {
        global $current_user;
        $this->cal = $cal;
        $today = $GLOBALS['timedate']->getNow(true)->get_day_begin();
        $this->today_ts = $today->format('U') + $today->getOffset();
        $this->startday = $current_user->get_first_day_of_week();

        $weekdays = array();
        for ($i = 0; $i < 7; $i++) {
            $j = $i + $this->startday;
            if ($j >= 7) {
                $j = $j - 7;
            }
            $weekdays[$i] = $GLOBALS['app_list_strings']['dom_cal_day_short'][$j+1];
        }

        $this->weekdays = $weekdays;

        $this->scrollable = false;
        if (!($this->cal->isPrint() && $this->cal->view == 'day')) {
            if (in_array($this->cal->view, array('day','week'))) {
                $this->scrollable = true;
                if ($this->cal->time_step < 30) {
                    $this->scroll_height = 480;
                } else {
                    $this->scroll_height = $this->cal->celcount * 15 + 1;
                }
            }
        }

        $this->time_step = $this->cal->time_step;
        $this->time_format = $GLOBALS['timedate']->get_time_format();
        $this->date_time_format = $GLOBALS['timedate']->get_date_time_format();

        $this->style = $this->cal->style;
    }


    /** Get html of calendar grid
     * @return string
     */
    public function display()
    {
        $action = "display_".strtolower($this->cal->view);
        return $this->$action();
    }

    /** Get html of time column
     * @param integer $start timestamp
     * @return string
     */
    protected function get_time_column($start)
    {
        $str = "";
        $head_content = "&nbsp;";
        if ($this->cal->view == 'month') {
            if ($this->startday == 0) {
                $wf = 1;
            } else {
                $wf = 0;
            }
            $head_content = "<a href='".ajaxLink("index.php?module=Calendar&action=index&view=week&hour=0&day=".$GLOBALS['timedate']->fromTimestamp($start)->format('j')."&month=".$GLOBALS['timedate']->fromTimestamp($start)->format('n')."&year=".$GLOBALS['timedate']->fromTimestamp($start)->format('Y'))."'>".$GLOBALS['timedate']->fromTimestamp($start + $wf*3600*24)->format('W')."</a>";
        }
        $str .= "<div class='left_col'>";
        //if(!$this->scrollable)
        //	$str .= "<div class='col_head'>".$head_content."</div>";
        $cell_number = 0;
        $first_cell  = $this->cal->scroll_slot;
        $last_cell   = $first_cell + $this->cal->celcount - 1;
        for ($i = 0; $i < 24; $i++) {
            for ($j = 0; $j < 60; $j += $this->time_step) {
                if ($j == 0) {
                    $innerText = $GLOBALS['timedate']->fromTimestamp($start + $i * 3600)->format($this->time_format);
                } else {
                    $innerText = "&nbsp;";
                }
                if ($j == 60 - $this->time_step && $this->time_step < 60) {
                    $class = " odd_border";
                } else {
                    $class = "";
                }
                if ($this->scrollable || ($cell_number >= $first_cell && $cell_number <= $last_cell)) {
                    $str .= "<div class='left_slot".$class."'>".$innerText."</div>";
                }
                $cell_number++;
            }
        }
        $str .= "</div>";
        return $str;
    }

    /**
     * Get html of day slots column
     * @param integer $start timestamp
     * @param integer $day number of day in week
     * @param string $suffix suffix for id of time slot used in shared view
     * @return string
     */
    protected function get_day_column($start, $day = 0, $suffix = "")
    {
        $curr_time = $start;
        $str = "";
        $str .= "<div class='col'>";
        //$str .= $this->get_day_head($start,$day);
        $cell_number = 0;
        $first_cell  = $this->cal->scroll_slot;
        $last_cell   = $first_cell + $this->cal->celcount - 1;
        for ($i = 0; $i < 24; $i++) {
            for ($j = 0; $j < 60; $j += $this->time_step) {
                $timestr = $GLOBALS['timedate']->fromTimestamp($curr_time)->format($this->time_format);
                if ($j == 60 - $this->time_step && $this->time_step < 60) {
                    $class = " odd_border";
                } else {
                    $class = "";
                }
                if ($this->scrollable || ($cell_number >= $first_cell && $cell_number <= $last_cell)) {
                    $str .= "<div id='t_".$curr_time.$suffix."' class='slot".$class."' time='".$timestr."' datetime='".$GLOBALS['timedate']->fromTimestamp($curr_time)->format($this->date_time_format)."'></div>";
                }
                $cell_number++;
                $curr_time += $this->time_step*60;
            }
        }
        $str .= "</div>";
        return $str;
    }

    public function display_mobile()
    {
        global $mod_strings;

        $str = "<div class='mobile_calendar_container'>";

        $agenda_array = [];

        foreach ($this->cal->items as $cal_item) {
            if (date("Y-m-d", $cal_item['ts_start']) >= date("Y-m-d", $this->today_ts)) {
                $agenda_array[$cal_item['ts_start']][] = $cal_item;
                ksort($agenda_array);
            }
        }

        $days = array_keys($agenda_array);

        if ($days) {
            foreach ($days as $day) {
                $agenda_array[$day] = $this->mobile_sort_items($agenda_array[$day]);

                if ($day == $this->today_ts) {
                    $str .= "<div class='mobile_calendar_title today'><b>Today</b> " . date("D dS, M Y", $agenda_array[$day][0]['ts_start']) . "</div>";
                } else {
                    $str .= "<div class='mobile_calendar_title'>" . date("D dS, M Y", $agenda_array[$day][0]['ts_start']) . "</div>";
                }

                $i = 0;

                while ($i < (is_countable($agenda_array[$day]) ? count($agenda_array[$day]) : 0)) {
                    $day_item = $agenda_array[$day][$i];

                    $str .= $this->mobile_display_items($day_item);

                    $i++;
                }
            }
        } else {
            $str .= "<div class='mobile_calendar_title no_items'>" . $mod_strings['LBL_NO_ITEMS_MOBILE'] . "</div>";
        }




        $str .= "</div>";
        return $str;
    }

    public function mobile_display_items($day_item)
    {
        $end_time = $this->mobile_get_end_time($day_item);
        $status_color = $this->mobile_get_status_colour($day_item['status']);
        $type_color = $this->mobile_get_type_colour($day_item['type']);

        $display = "";

        $display .= "<div class='mobile_calendar_item'>";

        $display .= "<div class='mobile_calendar_item_left' >";

        $display .= "<div class='mobile_calendar_item_left_type' style='background-color:" . $type_color .";'>";
        $display .=  ucfirst($day_item['type']);
        $display .= "</div>";

        $display .= "<div class='mobile_calendar_item_left_date'>";
        $display .= "<div class='mobile_calendar_item_left_time'>" . $day_item['time_start'] . "</div>";
        $display .= "<div class='mobile_calendar_item_left_time'>" .  $end_time . "</div>";
        $display .= "</div>";

        $display .= "<div class='mobile_calendar_item_left_status' style='background-color:" . $status_color ."';>";
        $display .= $day_item['status'];
        $display .= "</div>";

        $display .= "</div>";

        $display .= "<div class='mobile_calendar_item_center'>";
        $display .= "<p class='mobile_calendar_item_name'>" . $day_item['name'] . "</p>";
        $display .= "<p class='mobile_calendar_item_desc'>" . $day_item['description'] . "</p>";
        $display .= "</div>";


        if ($day_item['type'] == "task") {
            $display .= "<div class='mobile_calendar_item_edit'>";
            $display .= "<a class='button' module_name ='" . ucfirst($day_item['type']) ."s' href='index.php?action=EditView&module=Tasks&return_module=Calendar&return_action=index&record=" . $day_item['record'] . "'>Edit</a>";
            $display .= "</div>";
            $display .= "</div>";
        } else {
            $display .= "<div class='mobile_calendar_item_edit'>";
            $display .= "<a class='button' href='#' module_name ='" . ucfirst($day_item['type']) ."s' record = '" . $day_item['record'] ."' onclick=CAL.load_form(this.getAttribute('module_name'),this.getAttribute('record'),true);>Edit</a>";
            $display .= "</div>";
            $display .= "</div>";
        }

        return $display;
    }

    public function mobile_get_end_time($day_item)
    {
        $start_time = DateTime::createFromFormat("h:ia", $day_item['time_start']);
        $start_time->modify('+' . $day_item['duration_minutes'] .'minutes');
        return $start_time->format("h:ia");
    }


    public function mobile_get_type_colour($type)
    {
        switch ($type) {
            case "meeting":
                $colour = "#D2E5FC";
                break;
            case "call":
                $colour = "#FCDCDC";
                break;
            case "task":
                $colour = "#B1F5AE";
                break;
            default:
                $colour = "#777777";
                break;
        }
        return $colour;
    }

    public function mobile_get_status_colour($type)
    {
        switch ($type) {
            case "Held":
            case "Completed":
            $colour = "green";
                break;
            case "Planned":
            case "Not Started":
            case "In Progress":
                $colour = "#1B4B94";
                break;
            case "Not Held":
            case "Deferred":
                $colour = "red";
                break;
            default:
                $colour = "#777777";
                break;
        }
        return $colour;
    }

    public function mobile_sort_items($agenda_array)
    {
        $times = "";

        foreach ($agenda_array as $key => $row) {
            $times[$key] = $row['timestamp'];
        }

        array_multisort($times, SORT_ASC, $agenda_array);

        return $agenda_array;
    }

    /**
     * Get html of basic cell
     * @param integer $start timestamp
     * @param integer $height slot height
     * @param string $prefix prefix for id of slot used in shared view
     * @return string
     */
    protected function get_basic_cell($start, $height = 80, $suffix = "")
    {
        $str = "";
        $dt = $GLOBALS['timedate']->fromTimestamp($start)->get("+8 hours");
        $str .= "<div class='col'>";
        $str .= "<div class='basic_slot' id='b_".$start.$suffix."' style='height: ".$height."px;' time='' datetime='".$dt->format($this->date_time_format)."'></div>";
        $str .= "</div>";
        return $str;
    }

    /**
     * Get html of basic week grid
     * @param integer $start timestamp
     * @param string $prefix prefix for id of slot used in shared view
     * @return string
     */
    protected function get_basic_row($start, $cols = 7, $suffix = "")
    {
        $height = 20;
        $str = "";
        $head_content = "&nbsp;";
        if ($this->cal->view == 'month' || $this->cal->style == "basic") {
            $wf = 0;
            if ($this->startday == 0) {
                $wf = 1;
            }
            $head_content = $GLOBALS['timedate']->fromTimestamp($start + $wf*3600*24)->format('W');
            $head_content = "<a href='".ajaxLink("index.php?module=Calendar&action=index&view=week&hour=0&day=".$GLOBALS['timedate']->fromTimestamp($start)->format('j')."&month=".$GLOBALS['timedate']->fromTimestamp($start)->format('n')."&year=".$GLOBALS['timedate']->fromTimestamp($start)->format('Y'))."'>".$head_content."</a>";
        }
        $left_col = ($this->style != 'basic' || $this->cal->view == 'month');

        $attr = "";
        if ($this->cal->style != "basic") {
            $attr = " id='cal-multiday-bar'";
        }

        $str .= "<div>";
        if ($cols > 1) {
            $str .= "<div>";
            if ($left_col) {
                $str .= "<div class='left_col'>";
                $str .= "<div class='col_head'>".$head_content."</div>";
                $str .= "</div>";
            }

            $str .= "<div class='week'>";
            for ($d = 0; $d < $cols; $d++) {
                $curr_time = $start + $d * 86400;
                $str .= "<div class='col'>";
                $str .= $this->get_day_head($curr_time, $d, true);
                $str .= "</div>";
            }
            $str .= "</div>";
            $str .= "<br style='clear: left;'/>";
            $str .= "</div>";
        }
        $str .= "<div class='cal-basic' ".$attr.">";
        if ($left_col) {
            $str .= "<div class='left_col'>";
            $str .= "<div class='left_basic_slot' style='height: ".$height."px;'>&nbsp;</div>";
            $str .= "</div>";
        }
        $str .= "<div class='week'>";
        for ($d = 0; $d < $cols; $d++) {
            $curr_time = $start + $d*86400;
            $str .= $this->get_basic_cell($curr_time, $height, $suffix);
        }
        $str .= "</div>";
        $str .= "<div style='clear: left;'></div>";
        $str .= "</div>";
        $str .= "</div>";

        return $str;
    }

    /**
     * Get html of day head
     * @param integer $start timestamp
     * @param integer $day number of day in week
     * @param bulean $force force display header
     * @return string
     */
    protected function get_day_head($start, $day = 0, $force = false)
    {
        $str = "";
        if ($force) {
            $headstyle = "";
            if ($this->today_ts == $start) {
                $headstyle = " today";
            }
            $str .= "<div class='col_head".$headstyle."'><a href='".ajaxLink("index.php?module=Calendar&action=index&view=day&hour=0&day=".$GLOBALS['timedate']->fromTimestamp($start)->format('j')."&month=".$GLOBALS['timedate']->fromTimestamp($start)->format('n')."&year=".$GLOBALS['timedate']->fromTimestamp($start)->format('Y'))."'>".$this->weekdays[$day]." ".$GLOBALS['timedate']->fromTimestamp($start)->format('d')."</a></div>";
        }
        return $str;
    }

    /**
     * Get html of week calendar grid
     * @return string
     */
    protected function display_week()
    {
        $basic = $this->style == "basic";
        $week_start_ts = $this->cal->grid_start_ts;

        $str = "";
        $str .= "<div id='cal-grid' style='visibility: hidden;'>";
        $str .= $this->get_basic_row($week_start_ts);
        if (!$basic) {
            $str .= "<div id='cal-scrollable' style='clear: both; height: ".$this->scroll_height."px;'>";
            $str .= $this->get_time_column($week_start_ts);
            $str .= "<div class='week'>";
            for ($d = 0; $d < 7; $d++) {
                $curr_time = $week_start_ts + $d*86400;
                $str .= $this->get_day_column($curr_time);
            }
            $str .= "</div>";
            $str .= "<div style='clear: left;'></div>";
            $str .= "</div>";
        }
        $str .= "</div>";

        return $str;
    }

    /**
     * Get html of day calendar grid
     * @return string
     */
    protected function display_day()
    {
        $basic = $this->style == "basic";
        $day_start_ts = $this->cal->grid_start_ts;

        $str = "";
        $str .= "<div id='cal-grid' style='visibility: hidden;'>";
        $str .= $this->get_basic_row($day_start_ts, 1);
        if (!$basic) {
            $str .= "<div id='cal-scrollable' style='height: ".$this->scroll_height."px;'>";
            $str .= $this->get_time_column($day_start_ts);
            $d = 0;
            $curr_time = $day_start_ts + $d*86400;
            $str .= "<div class='week'>";
            $str .= $this->get_day_column($curr_time);
            $str .= "</div>";
            $str .= "<div style='clear: left;'></div>";
            $str .= "</div>";
        }
        $str .= "</div>";

        return $str;
    }

    /**
     * Get html of month calendar grid
     * @return string
     */
    protected function display_month()
    {
        $basic = $this->style == "basic";
        $week_start_ts = $this->cal->grid_start_ts;
        $current_date = $this->cal->date_time;
        $month_start = $current_date->get_day_by_index_this_month(0);
        $month_end = $month_start->get("+".$month_start->format('t')." days");
        $week_start = CalendarUtils::get_first_day_of_week($month_start);
        $month_end_ts = $month_end->format('U') + $month_end->getOffset();

        $str = "";
        $str .= "<div id='cal-grid' style='visibility: hidden;'>";
        $curr_time_global = $week_start_ts;
        $w = 0;
        while ($curr_time_global < $month_end_ts) {
            if ($basic) {
                $str .= $this->get_basic_row($curr_time_global);
            } else {
                $str .= $this->get_time_column($curr_time_global);
                $str .= "<div class='week'>";
                for ($d = 0; $d < 7; $d++) {
                    $curr_time = $week_start_ts + $d*86400 + $w*60*60*24*7;
                    $str .= $this->get_day_column($curr_time, $d);
                }
                $str .= "</div>";
                $str .= "<div style='clear: left;'></div>";
            }
            $curr_time_global += 60*60*24*7;
            $w++;
        }
        $str .= "</div>";

        return $str;
    }

    /**
     * Get html of week shared grid
     * @return string
     */
    protected function display_shared()
    {
        $basic = $this->style == "basic";
        $week_start_ts = $this->cal->grid_start_ts;

        $str = "";
        $str .= "<div id='cal-grid' style='visibility: hidden;'>";
        $user_number = 0;

        $shared_user = BeanFactory::newBean('Users');
        foreach ($this->cal->shared_ids as $member_id) {
            $user_number_str = "_".$user_number;

            $shared_user->retrieve($member_id);
            $str .= "<div style='clear: both;'></div>";
            $str .= "<div class='monthCalBody'><h5 class='calSharedUser'>".$shared_user->full_name."</h5></div>";
            $str .= "<div user_id='".$member_id."' user_name='".$shared_user->user_name."'>";

            $str .= $this->get_basic_row($week_start_ts, 7, $user_number_str);
            if (!$basic) {
                $str .= $this->get_time_column($week_start_ts);
                $str .= "<div class='week'>";
                for ($d = 0; $d < 7; $d++) {
                    $curr_time = $week_start_ts + $d*86400;
                    $str .= $this->get_day_column($curr_time, $d, $user_number_str);
                }
                $str .= "</div>";
                $str .= "</div>";
            }
            $user_number++;
        }
        $str .= "</div>";

        return $str;
    }

    /**
     * Get html of year calendar
     * @return string
     */
    protected function display_year()
    {
        $weekEnd1 = 0 - $this->startday;
        $weekEnd2 = -1 - $this->startday;
        if ($weekEnd1 < 0) {
            $weekEnd1 += 7;
        }
        if ($weekEnd2 < 0) {
            $weekEnd2 += 7;
        }

        $year_start = $GLOBALS['timedate']->fromString($this->cal->date_time->year.'-01-01');

        $str = "";
        $str .= '<table id="daily_cal_table" cellspacing="1" cellpadding="0" border="0" width="100%">';

        for ($m = 0; $m < 12; $m++) {
            $month_start = $year_start->get("+".$m." months");
            $month_start_ts = $month_start->format('U') + $month_start->getOffset();
            $month_end = $month_start->get("+".$month_start->format('t')." days");
            $week_start = CalendarUtils::get_first_day_of_week($month_start);
            $week_start_ts = $week_start->format('U') + $week_start->getOffset(); // convert to timestamp, ignore tz
            $month_end_ts = $month_end->format('U') + $month_end->getOffset();
            $table_id = "daily_cal_table".$m; //bug 47471

            if ($m % 3 == 0) {
                $str .= "<tr>";
            }
            $str .= '<td class="yearCalBodyMonth" align="center" valign="top" scope="row">';
            $str .= '<a class="yearCalBodyMonthLink" href="'.ajaxLink('index.php?module=Calendar&action=index&view=month&&hour=0&day=1&month='.($m+1).'&year='.$GLOBALS['timedate']->fromTimestamp($month_start_ts)->format('Y')).'">'.$GLOBALS['app_list_strings']['dom_cal_month_long'][$m+1].'</a>';
            $str .= '<table id="'. $table_id. '" cellspacing="1" cellpadding="0" border="0" width="100%">';
            $str .= '<tr class="monthCalBodyTH">';
            for ($d = 0; $d < 7; $d++) {
                $str .= '<th width="14%">'.$this->weekdays[$d].'</th>';
            }
            $str .= '</tr>';
            $curr_time_global = $week_start_ts;
            $w = 0;
            while ($curr_time_global < $month_end_ts) {
                $str .= '<tr class="monthViewDayHeight yearViewDayHeight">';
                for ($d = 0; $d < 7; $d++) {
                    $curr_time = $week_start_ts + $d*86400 + $w*60*60*24*7;

                    if ($curr_time < $month_start_ts || $curr_time >= $month_end_ts) {
                        $monC = "";
                    } else {
                        $monC = '<a href="'.ajaxLink('index.php?module=Calendar&action=index&view=day&hour=0&day='.$GLOBALS['timedate']->fromTimestamp($curr_time)->format('j').'&month='.$GLOBALS['timedate']->fromTimestamp($curr_time)->format('n').'&year='.$GLOBALS['timedate']->fromTimestamp($curr_time)->format('Y')) .'">'.$GLOBALS['timedate']->fromTimestamp($curr_time)->format('j').'</a>';
                    }

                    if ($d == $weekEnd1 || $d == $weekEnd2) {
                        $str .= "<td class='weekEnd monthCalBodyWeekEnd'>";
                    } else {
                        $str .= "<td class='monthCalBodyWeekDay'>";
                    }

                    $str .= $monC;
                    $str .= "</td>";
                }
                $str .= "</tr>";
                $curr_time_global += 60*60*24*7;
                $w++;
            }
            $str .= '</table>';
            $str .= '</td>';
            if (($m - 2) % 3 == 0) {
                $str .= "</tr>";
            }
        }
        $str .= "</table>";

        return $str;
    }
}
