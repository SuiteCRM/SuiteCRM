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




#[\AllowDynamicProperties]
class CalendarDisplay
{

    /**
     * colors of items on calendar
     */
    public $activity_colors = array(
        'Meetings' => array(
            'border' => '87719C',
            'body' => '6B5171',
            'text' => 'E5E5E5'
        ),
        'Calls' => array(
            'border' => '487166',
            'body' => '72B3A1',
            'text' => 'E5E5E5'
        ),
        'Tasks' => array(
            'border' => '515A71',
            'body' => '707C9C',
            'text' => 'E5E5E5'
        ),
        'FP_events' => array(
            'border' => 'C29B8A',
            'body' => '7D6459',
            'text' => 'E5E5E5'
        ),
        'Project' => array(
            'border' => '699DC9',
            'body' => '557FA3',
            'text' => 'E5E5E5'
        ),
        'ProjectTask' => array(
            'border' => '83C489',
            'body' => '659769',
            'text' => 'E5E5E5'
        ),
    );
    /**
     * constructor
     * @param Calendar $cal
     * @param string $dashlet_id for dashlet mode
     * @param array $views
     */
    public function __construct(Calendar $cal, $dashlet_id = "", $views = array())
    {
        global $sugar_config;
        if (isset($sugar_config['CalendarColors']) && is_array($sugar_config['CalendarColors'])) {
            $this->activity_colors = array_merge($this->activity_colors, $sugar_config['CalendarColors']);
        }
        $this->cal = $cal;
        $this->dashlet_id = $dashlet_id;
        $this->views = $views;
    }

    /**
     * main displaying function of Calendar
     */
    public function display()
    {
        global $timedate, $current_user;

        $cal = &$this->cal;
        $ss = new Sugar_Smarty();

        $ss->assign('APP', $GLOBALS['app_strings']);
        $ss->assign('APPLIST', $GLOBALS['app_list_strings']);
        $ss->assign('MOD', $GLOBALS['cal_strings']);

        $ss->assign('view', $cal->view);
        $ss->assign('style', $cal->style);
        $ss->assign('t_step', $cal->time_step);
        $ss->assign('current_user_id', $GLOBALS['current_user']->id);
        $ss->assign('current_user_name', $GLOBALS['current_user']->name);
        $ss->assign('time_format', $GLOBALS['timedate']->get_user_time_format());
        $ss->assign('enable_repeat', $this->cal->enable_repeat);
        $ss->assign('items_draggable', SugarConfig::getInstance()->get('calendar.items_draggable', true));
        $ss->assign('items_resizable', SugarConfig::getInstance()->get('calendar.items_resizable', true));
        $ss->assign('cells_per_day', $cal->cells_per_day);
        $ss->assign('activityColors', json_encode($this->checkActivity($this->activity_colors)));
        $ss->assign('dashlet', $cal->dashlet);
        $ss->assign('grid_start_ts', (int)$cal->grid_start_ts);

        $ss->assign('year', $cal->date_time->format('Y'));
        $ss->assign('month', $cal->date_time->format('m'));
        $ss->assign('day', $cal->date_time->format('d'));

        $ss->assign('CALENDAR_FORMAT', $GLOBALS['timedate']->get_cal_date_format());
        $ss->assign('CALENDAR_FDOW', $GLOBALS['current_user']->get_first_day_of_week());



        $ss->assign('basic_min_height', "'auto'");

        $ss->assign('isPrint', $this->cal->isPrint() ? 'true': 'false');


        $sharedIdsCount = is_countable($cal->shared_ids) ? count($cal->shared_ids) : 0;

        if ($sharedIdsCount) {
            $ss->assign('shared_ids', $cal->shared_ids);
            $ss->assign('shared_users_count', $sharedIdsCount);
        }


        $ss->assign('start_weekday', $GLOBALS['current_user']->get_first_day_of_week());
        $ss->assign('scroll_slot', $this->cal->scroll_slot);

        $ss->assign('editview_width', SugarConfig::getInstance()->get('calendar.editview_width', 800));
        $ss->assign('editview_height', SugarConfig::getInstance()->get('calendar.editview_height', 600));

        $ss->assign('a_str', json_encode($cal->items));

        $start = $current_user->getPreference('day_start_time');
        if (is_null($start)) {
            $start = SugarConfig::getInstance()->get('calendar.default_day_start', "08:00");
        }
        $ss->assign('day_start_time', $start);

        $end = $current_user->getPreference('day_end_time');
        if (is_null($end)) {
            $end = SugarConfig::getInstance()->get('calendar.default_day_end', "19:00");
        }
        $ss->assign('day_end_time', $end);

        $ss->assign('sugar_body_only', (isset($_REQUEST['to_pdf']) && $_REQUEST['to_pdf'] || isset($_REQUEST['sugar_body_only']) && $_REQUEST['sugar_body_only']));
        require_once('include/json_config.php');
        global $json;
        $json = getJSONobj();
        $json_config = new json_config();
        $ss->assign('GRjavascript', $json_config->get_static_json_server(false, true, 'Meetings'));

        // form
        $user_default_date_start  = $timedate->asUser($timedate->getNow());
        $ss->assign('user_default_date_start', $user_default_date_start);
        // end form
        $location_array = array();
        foreach ($this->views as $view) {
            $location_array[] = $view;
        }
        $current_language = explode("_", $GLOBALS['current_language']);
        $ss->assign("langprefix", $current_language[0]);

        $ss->assign('custom_views', $location_array);
        if ($_REQUEST['module'] == "Calendar") {
            $this->load_settings_template($ss);
            $settings = get_custom_file_if_exists("modules/Calendar/tpls/settings.tpl");
            $ss->assign("settings", $settings);
        }
        //mark date format for user.
        $date_format = $this->convertPHPToMomentFormat($GLOBALS['current_user']->getPreference('datef') . " " . $GLOBALS['current_user']->getPreference('timef'));
        $ss->assign('datetime_user_format', $date_format);

        $main = get_custom_file_if_exists("modules/Calendar/tpls/main.tpl");
        $form_tpl = get_custom_file_if_exists("modules/Calendar/tpls/form.tpl");

        $ss->assign("form", $form_tpl);

        if ($this->cal->enable_repeat) {
            $repeat_tpl = get_custom_file_if_exists("modules/Calendar/tpls/repeat.tpl");
            $ss->assign("repeat", $repeat_tpl);

            $repeat_intervals = array();
            for ($i = 1; $i <= 30; $i++) {
                $repeat_intervals[$i] = $i;
            }
            $ss->assign("repeat_intervals", $repeat_intervals);


            $fdow = $GLOBALS['current_user']->get_first_day_of_week();
            $dow = array();
            for ($i = $fdow; $i < $fdow + 7; $i++) {
                $day_index = $i % 7;
                $dow[] = array("index" => $day_index , "label" => $GLOBALS['app_list_strings']['dom_cal_day_short'][$day_index + 1]);
            }
            $ss->assign("dow", $dow);
        }
        echo $ss->fetch($main);
    }

    public function checkActivity($activity = "")
    {
        global $current_user;
        if (empty($activity)) {
            $activity = $this->activity_colors;
        }
        $newActivities = unserialize(base64_decode($current_user->getPreference("CalendarActivities")), ['allowed_classes' => false]);
        if ($newActivities) {
            $activity = array_merge($activity, $newActivities);
        }
        foreach ($activity as $key => $activityItem) {
            if (isset($GLOBALS['app_list_strings']['moduleList'][ $key ]) && !empty($GLOBALS['app_list_strings']['moduleList'][ $key ]) && !empty($this->cal->activityList[ $key ])) {
                $activity[ $key ]['label'] = $GLOBALS['app_list_strings']['moduleList'][ $key ];
            } else {
                unset($activity[ $key ]);
            }
        }
        if (isset($activity) && !empty($activity)) {
            $this->activity_colors = $activity;
        }
        if (!empty($this->cal->activityList)) {
            foreach ($this->cal->activityList as $key=>$value) {
                if (isset($GLOBALS['beanList'][$key]) && !empty($GLOBALS['beanList'][$key]) && !isset($this->activity_colors[ $key ])) {
                    $this->activity_colors[ $key ] = $GLOBALS['sugar_config']['CalendarColors'][$key];
                    $activity[ $key ] = $GLOBALS['sugar_config']['CalendarColors'][$key];
                }
            }
        }

        return $activity;
    }
    /**
     * load settings popup template
     */
    protected function load_settings_template(&$ss)
    {

        global $app_strings,$app_list_strings,$beanList;
        global $timedate;

        list($d_start_hour, $d_start_min) =  explode(":", $this->cal->day_start_time);
        list($d_end_hour, $d_end_min) =  explode(":", $this->cal->day_end_time);

        $match = [];

        require_once("include/utils.php");
        $user_default_date_start  = $timedate->asUser($timedate->getNow());

        if (!isset($time_separator)) {
            $time_separator = ":";
        }
        $date_format = $timedate->get_cal_date_format();
        $time_format = $timedate->get_user_time_format();
        $TIME_FORMAT = $time_format;
        $t23 = strpos((string) $time_format, '23') !== false ? '%H' : '%I';
        if (!isset($match[2]) || $match[2] == '') {
            $CALENDAR_FORMAT = $date_format . ' ' . $t23 . $time_separator . "%M";
        } else {
            $pm = $match[2] == "pm" ? "%P" : "%p";
            $CALENDAR_FORMAT = $date_format . ' ' . $t23 . $time_separator . "%M" . $pm;
        }
        $hours_arr = array();
        $num_of_hours = 24;
        $start_at = 0;
        $TIME_MERIDIEM = "";
        $time_pref = $timedate->get_time_format();
        $start_m = "";
        if (strpos((string) $time_pref, 'a') || strpos((string) $time_pref, 'A')) {
            $num_of_hours = 12;
            $start_at = 1;
            $start_m = 'am';
            if ($d_start_hour == 0) {
                $d_start_hour = 12;
                $start_m = 'am';
            } elseif ($d_start_hour == 12) {
                $start_m = 'pm';
            }
            if ($d_start_hour > 12) {
                $d_start_hour = $d_start_hour - 12;
                $start_m = 'pm';
            }
            $end_m = 'am';
            if ($d_end_hour == 0) {
                $d_end_hour = 12;
                $end_m = 'am';
            } elseif ($d_end_hour == 12) {
                $end_m = 'pm';
            }

            if ($d_end_hour > 12) {
                $d_end_hour = $d_end_hour - 12;
                $end_m = 'pm';
            }
            if (strpos((string) $time_pref, 'A')) {
                $start_m = strtoupper($start_m);
                $end_m = strtoupper($end_m);
            }
            $options = strpos((string) $time_pref, 'a') ? $app_list_strings['dom_meridiem_lowercase'] : $app_list_strings['dom_meridiem_uppercase'];
            $TIME_START_MERIDIEM = get_select_options_with_id($options, $start_m);
            $TIME_END_MERIDIEM = get_select_options_with_id($options, $end_m);
            $TIME_START_MERIDIEM = "<select id='day_start_meridiem' name='day_start_meridiem' tabindex='2'>".$TIME_START_MERIDIEM."</select>";
            $TIME_END_MERIDIEM = "<select id='day_end_meridiem' name='day_end_meridiem' tabindex='2'>".$TIME_END_MERIDIEM."</select>";
        } else {
            $TIME_START_MERIDIEM = $TIME_END_MERIDIEM = "";
        }
        for ($i = $start_at; $i <= $num_of_hours; $i ++) {
            $i = $i."";
            if (strlen($i) == 1) {
                $i = "0".$i;
            }
            $hours_arr[$i] = $i;
        }
        $TIME_START_HOUR_OPTIONS = get_select_options_with_id($hours_arr, $d_start_hour);
        $TIME_START_MINUTES_OPTIONS = get_select_options_with_id(array('0'=>'00','15'=>'15','30'=>'30','45'=>'45'), $d_start_min);
        $TIME_END_HOUR_OPTIONS = get_select_options_with_id($hours_arr, $d_end_hour);
        $TIME_END_MINUTES_OPTIONS = get_select_options_with_id(array('0'=>'00','15'=>'15','30'=>'30','45'=>'45'), $d_end_min);

        $displayTimeslots = $GLOBALS['current_user']->getPreference('calendar_display_timeslots');
        if (is_null($displayTimeslots)) {
            $displayTimeslots = SugarConfig::getInstance()->get('calendar.display_timeslots', true);
        }

        $shared_calendar_separate = $GLOBALS['current_user']->getPreference('calendar_display_shared_separate');
        if (is_null($shared_calendar_separate)) {
            $shared_calendar_separate = SugarConfig::getInstance()->get('calendar.calendar_display_shared_separate', true);
        }
        $ss->assign('week', $_REQUEST['week']);
        $ss->assign('activity', $this->checkActivity($this->activity_colors));
        $ss->assign('display_timeslots', $displayTimeslots);
        $ss->assign('shared_calendar_separate', $shared_calendar_separate);
        $ss->assign('show_calls', $this->cal->show_calls);
        $ss->assign('show_tasks', $this->cal->show_tasks);
        $ss->assign('show_completed', $this->cal->show_completed);
        $ss->assign('TIME_START_HOUR_OPTIONS', $TIME_START_HOUR_OPTIONS);
        $ss->assign('TIME_START_MINUTES_OPTIONS', $TIME_START_MINUTES_OPTIONS);
        $ss->assign('TIME_START_MERIDIEM', $TIME_START_MERIDIEM);
        $ss->assign('TIME_END_HOUR_OPTIONS', $TIME_END_HOUR_OPTIONS);
        $ss->assign('TIME_END_MINUTES_OPTIONS', $TIME_END_MINUTES_OPTIONS);
        $ss->assign('TIME_END_MERIDIEM', $TIME_END_MERIDIEM);
    }

    /**
     * Get date info string (legacy from old calendar)
     * @return string
     */
    public function get_date_info($view, $date_time)
    {
        $str = "";

        global $current_user;
        $dateFormat = $current_user->getUserDateTimePreferences();

        if ($view == 'month' || $view == 'sharedMonth') {
            for ($i=0; $i<strlen((string) $dateFormat['date']); $i++) {
                switch ($dateFormat['date'][$i]) {
                    case "Y":
                        $str .= " ".$date_time->year;
                        break;
                    case "m":
                        $str .= " ".$date_time->get_month_name();
                        break;
                }
            }
        } elseif ($view == 'agendaWeek' || $view == 'sharedWeek') {
            $first_day = $date_time;

            $first_day = CalendarUtils::get_first_day_of_week($date_time);
            $last_day = $first_day->get("+6 days");

            for ($i=0; $i<strlen((string) $dateFormat['date']); $i++) {
                switch ($dateFormat['date'][$i]) {
                        case "Y":
                            $str .= " ".$first_day->year;
                            break;
                        case "m":
                            $str .= " ".$first_day->get_month_name();
                            break;
                        case "d":
                            $str .= " ".$first_day->get_day();
                            break;
                    }
            }
            $str .= " - ";
            for ($i=0; $i<strlen((string) $dateFormat['date']); $i++) {
                switch ($dateFormat['date'][$i]) {
                        case "Y":
                            $str .= " ".$last_day->year;
                            break;
                        case "m":
                            $str .= " ".$last_day->get_month_name();
                            break;
                        case "d":
                            $str .= " ".$last_day->get_day();
                            break;
                    }
            }
        } elseif ($view == 'agendaDay') {
            $str .= $date_time->get_day_of_week()." ";

            for ($i=0; $i<strlen((string) $dateFormat['date']); $i++) {
                switch ($dateFormat['date'][$i]) {
                            case "Y":
                                $str .= " ".$date_time->year;
                                break;
                            case "m":
                                $str .= " ".$date_time->get_month_name();
                                break;
                            case "d":
                                $str .= " ".$date_time->get_day();
                                break;
                        }
            }
        } elseif ($view == 'mobile') {
            $str .= $date_time->get_day_of_week()." ";

            for ($i=0; $i<strlen((string) $dateFormat['date']); $i++) {
                switch ($dateFormat['date'][$i]) {
                        case "Y":
                            $str .= " ".$date_time->year;
                            break;
                        case "m":
                            $str .= " ".$date_time->get_month_name();
                            break;
                        case "d":
                            $str .= " ".$date_time->get_day();
                            break;
                    }
            }
        } elseif ($view == 'year') {
            $str .= $date_time->year;
        } else {
            //could be a custom view.
            $first_day = $date_time;

            $first_day = CalendarUtils::get_first_day_of_week($date_time);
            $last_day = $first_day->get("+6 days");

            for ($i=0; $i<strlen((string) $dateFormat['date']); $i++) {
                switch ($dateFormat['date'][$i]) {
                        case "Y":
                            $str .= " ".$first_day->year;
                            break;
                        case "m":
                            $str .= " ".$first_day->get_month_name();
                            break;
                        case "d":
                            $str .= " ".$first_day->get_day();
                            break;
                    }
            }
            $str .= " - ";
            for ($i=0; $i<strlen((string) $dateFormat['date']); $i++) {
                switch ($dateFormat['date'][$i]) {
                        case "Y":
                            $str .= " ".$last_day->year;
                            break;
                        case "m":
                            $str .= " ".$last_day->get_month_name();
                            break;
                        case "d":
                            $str .= " ".$last_day->get_day();
                            break;
                    }
            }
        }
        return $str;
    }

    /**
     * Get link to next date range
     * @return string
     */
    protected function get_next_calendar()
    {
        global $cal_strings,$image_path;
        $str = "";
        if ($_REQUEST['module'] == "Calendar") {
            $str .= "<a href='".ajaxLink("index.php?action=index&module=Calendar&view=".$this->cal->view."&".$this->cal->get_neighbor_date_str("next"))."'>";
        } else {
            $str .= "<a href='#' onclick='return SUGAR.mySugar.retrieveDashlet(\"".$this->dashlet_id."\", \"index.php?module=Home&action=DynamicAction&DynamicAction=displayDashlet&sugar_body_only=1&".$this->cal->get_neighbor_date_str("next")."&id=".$this->dashlet_id."\")'>";
        }

        $str .= "&nbsp;&nbsp;".SugarThemeRegistry::current()->getImage("calendar_next", 'align="absmiddle" border="0"', null, null, '.gif', '') . "</a>"; //setting alt tag blank on purpose for 508 compliance
        return $str;
    }

    /**
     * Get link to previous date range
     * @return string
     */
    protected function get_previous_calendar()
    {
        global $cal_strings,$image_path;
        $str = "";
        if ($_REQUEST['module'] == "Calendar") {
            $str .= "<a href='".ajaxLink("index.php?action=index&module=Calendar&view=".$this->cal->view."&".$this->cal->get_neighbor_date_str("previous")."")."'>";
        } else {
            $str .= "<a href='#' onclick='return SUGAR.mySugar.retrieveDashlet(\"".$this->dashlet_id."\", \"index.php?module=Home&action=DynamicAction&DynamicAction=displayDashlet&sugar_body_only=1&".$this->cal->get_neighbor_date_str("previous")."&id=".$this->dashlet_id."\")'>";
        }
        $str .= SugarThemeRegistry::current()->getImage('calendar_previous', 'align="absmiddle" border="0"', null, null, '.gif', ''); //setting alt tag blank on purpose for 508 compliance
        $str .= "&nbsp;&nbsp;</a>";
        return $str;
    }

    /**
     * display header
     * @param boolean $controls display ui contol itmes
     */
    public function display_calendar_header($controls = true)
    {
        global $cal_strings;

        $ss = new Sugar_Smarty();
        $ss->assign("MOD", $cal_strings);
        $ss->assign("view", $this->cal->view);

        $ss->assign('print', $this->cal->isPrint());

        if ($controls) {
            $current_date = str_pad($this->cal->date_time->month, 2, '0', STR_PAD_LEFT)."/".str_pad($this->cal->date_time->day, 2, '0', STR_PAD_LEFT)."/".$this->cal->date_time->year;

            $tabs = $this->views;
            $tabs_params = array();
            foreach ($tabs as $key => $tab) {
                if ($key != "basicDay" && $key != "basicWeek") {
                    $tabs_params[$key]['title'] = $cal_strings["LBL_" . strtoupper($key)];
                    $tabs_params[$key]['id'] = $key . "-tab";
                    $tabs_params[$key]['link'] = "window.location.href='" . ajaxLink("index.php?module=Calendar&action=index&view=" . $key . $this->cal->date_time->get_date_str()) . "'";
                } else {
                    unset($tabs[$key]);
                }
            }
            $ss->assign('controls', $controls);
            $ss->assign('tabs', $tabs);
            $ss->assign('tabs_params', $tabs_params);
            $ss->assign('current_date', $current_date);
            $ss->assign('start_weekday', $GLOBALS['current_user']->get_first_day_of_week());
            $ss->assign('cal_img', '<span class="suitepicon suitepicon-module-calendar"></span>');
        }

        $ss->assign('previous', $this->get_previous_calendar());
        $ss->assign('next', $this->get_next_calendar());

        $ss->assign('date_info', $this->get_date_info($this->cal->view, $this->cal->date_time));

        $header = get_custom_file_if_exists("modules/Calendar/tpls/header.tpl");
        echo $ss->fetch($header);
    }

    /**
     * display footer
     */
    public function display_calendar_footer()
    {
        global $cal_strings;

        $ss = new Sugar_Smarty();
        $ss->assign("MOD", $cal_strings);
        $ss->assign("view", $this->cal->view);

        $ss->assign('previous', $this->get_previous_calendar());
        $ss->assign('next', $this->get_next_calendar());

        $footer = get_custom_file_if_exists("modules/Calendar/tpls/footer.tpl");
        echo $ss->fetch($footer);
    }

    public function convertPHPToMomentFormat($format)
    {
        $replacements = array(
            'd' => 'DD',
            'D' => 'ddd',
            'j' => 'D',
            'l' => 'dddd',
            'N' => 'E',
            'S' => 'o',
            'w' => 'e',
            'z' => 'DDD',
            'W' => 'W',
            'F' => 'MMMM',
            'm' => 'MM',
            'M' => 'MMM',
            'n' => 'M',
            't' => '', // no equivalent
            'L' => '', // no equivalent
            'o' => 'YYYY',
            'Y' => 'YYYY',
            'y' => 'YY',
            'a' => 'a',
            'A' => 'A',
            'B' => '', // no equivalent
            'g' => 'h',
            'G' => 'H',
            'h' => 'hh',
            'H' => 'HH',
            'i' => 'mm',
            's' => 'ss',
            'u' => 'SSS',
            'e' => 'zz', // deprecated since version 1.6.0 of moment.js
            'I' => '', // no equivalent
            'O' => '', // no equivalent
            'P' => '', // no equivalent
            'T' => '', // no equivalent
            'Z' => '', // no equivalent
            'c' => '', // no equivalent
            'r' => '', // no equivalent
            'U' => 'X',
        );
        $momentFormat = strtr($format, $replacements);
        return $momentFormat;
    }
    /**
     * display title
     */
    public function display_title()
    {
        global $mod_strings;
        //Hack to make this 6.5 compatible until this module is converted to MVC

        if ($this->cal->view == "mobile") {
            $buttons = array("Meeting","Call","Task");

            echo "<div class='custom_module_title moduleTitle'><h2>". $mod_strings['LBL_MODULE_TITLE'] ."</h2></div>";
            echo "<div style='float:right;' class='moduleTitle'>";

            echo '<div class="btn-group">
                    <button type="button" class="btn button dropdown-toggle" data-toggle="dropdown">Add Item <span class="caret"></span></button>
                       <ul class="dropdown-menu pull-left" style="right: 0; left: auto;">';

            foreach ($buttons as $module) {
                echo '<li><a href="index.php?return_module=Calendar&return_action=index&module=' . $module .'s&action=EditView">Add ' . $module .'</a></li>';
            }

            echo '</ul></div></div>';
        } else {
            echo "<div class='moduleTitle'><h2>". $mod_strings['LBL_MODULE_TITLE'] ."</h2></div>";
        }
    }

    /**
     * display html used in shared view
     */
    public function display_shared_html($view)
    {
        global $app_strings,$cal_strings,$action;

        $ss = new Sugar_Smarty();
        $ss->assign("APP", $app_strings);
        $ss->assign("MOD", $cal_strings);
        $ss->assign("view", $view);
        $ss->assign("UP", SugarThemeRegistry::current()->getImage('uparrow_big', 'border="0" style="margin-bottom: 1px;"', null, null, '.gif', $app_strings['LBL_SORT']));
        $ss->assign("DOWN", SugarThemeRegistry::current()->getImage('downarrow_big', 'border="0" style="margin-top: 1px;"', null, null, '.gif', $app_strings['LBL_SORT']));
        //hide by default.
        if (!empty($_REQUEST['edit_shared'])) {
            $ss->assign("edit_shared", true);
        }
        $ss->assign("users_options", get_select_options_with_id(get_user_array(false), $this->cal->shared_ids));
        $tpl = get_custom_file_if_exists("modules/Calendar/tpls/shared_users.tpl");
        echo $ss->fetch($tpl);
    }
}
