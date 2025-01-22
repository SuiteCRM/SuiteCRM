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

/**

 * Description:
 */
global $current_user;
if (!is_admin($current_user)) {
    sugar_die("Unauthorized access to administration.");
}

$_REQUEST['edit']='true';


$header_text = '';
global $mod_strings;
global $app_list_strings;
global $app_strings;
global $current_user;
global $timedate;

$email = BeanFactory::newBean('Emails');

/* Start standard EditView setup logic */
$mod_strings = return_module_language($current_language, 'Schedulers');
$focus = BeanFactory::newBean('Schedulers');
$focus->checkCurl();
if (isset($_REQUEST['record'])) {
    $GLOBALS['log']->debug("In Scheduler edit view, about to retrieve record: ".$_REQUEST['record']);
    $result = $focus->retrieve($_REQUEST['record']);
    if ($result == null) {
        sugar_die($app_strings['ERROR_NO_RECORD']);
    }
}
if (isset($_REQUEST['isDuplicate']) && $_REQUEST['isDuplicate'] == 'true') {
    $GLOBALS['log']->debug("isDuplicate found - duplicating record of id: ".$focus->id);
    $focus->id = "";
}



$params = array();
$params[] = "<a href='index.php?module=Schedulers&action=index'>{$mod_strings['LBL_MODULE_TITLE']}</a>";
if (empty($focus->id)) {
    $params[] = $GLOBALS['app_strings']['LBL_CREATE_BUTTON_LABEL'];
} else {
    $params[] = "<a href='index.php?module=Schedulers&action=DetailView&record={$focus->id}'>{$focus->name}</a>";
    $params[] = $GLOBALS['app_strings']['LBL_EDIT_BUTTON_LABEL'];
}
echo getClassicModuleTitle("Schedulers", $params, true);

$GLOBALS['log']->info("Scheduler Edit View");
/* End standard EditView setup logic */

// javascript calls

$javascript = new javascript();
$javascript->setSugarBean($focus);
$javascript->setFormName('EditView');
$javascript->addAllFields('');
$javascript->addFieldGeneric('mins', 'alpha', 'Mins', true, $prefix='');
$javascript->addFieldGeneric('day_of_month', 'alpha', 'Days of Month', true, $prefix='');
$javascript->addFieldGeneric('months', 'alpha', 'Months', true, $prefix='');
$javascript->addFieldGeneric('day_of_week', 'alpha', 'Days of Week', true, $prefix='');
$javascript->addFieldGeneric('date_start', 'date', 'Start Date', true, $prefix='');
//$javascript->addFieldDateBeforeAllowBlank('date_start', 'date', 'Date End', 'false', $prefix='', 'date_end', 'true'); // cn: disabled because it does not handle user-pref date format
$javascript->addToValidateBinaryDependency('time_hour_from', 'alpha', 'Active From (hr) must be set with Active To (hr) ', 'false', $prefix='', 'time_hour_to');
$javascript->addToValidateBinaryDependency('time_minute_from', 'alpha', 'Active From (min) must be set with Active To (min) ', 'false', $prefix='', 'time_minute_to');
//($field, $type,$displayName, $required, $prefix='',$compareTo)

// split the date/time of start/end
$dtStart = $focus->date_time_start;
$dtEnd = $focus->date_time_end;
if (!empty($dtStart)) {
    $exStart = explode(" ", $dtStart);
    $date_start = $exStart[0];
    $time_start = $exStart[1];
} else {
    $prefDate = $current_user->getUserDateTimePreferences();
    $date_start =  $timedate->asUserDate($timedate->fromString('2005-01-01'));
    $time_start = '';
}

if (!empty($dtEnd) && $dtEnd != '2020-12-31 23:59') {
    $exEnd = explode(" ", $dtEnd);
    $date_end = $exEnd[0];
    $time_end = $exEnd[1];
} else {
    $date_end = '';
    $time_end = '';
}

// setup calendar dropdowns
$time_format = $timedate->get_user_time_format();
$time_meridiem_start = $timedate->AMPMMenu('time_start_', $time_start, '');
$time_meridiem_end = $timedate->AMPMMenu('time_end_', $time_end, '');
$time_meridiem_from = $timedate->AMPMMenu('time_from_', $focus->time_from, '');
$time_meridiem_to = $timedate->AMPMMenu('time_to_', $focus->time_to, '');
$time_start_hour = (int)substr($time_start, 0, 2);
$time_start_minutes = substr($time_start, 3, 5);
$time_end_hour = (int)substr($time_end, 0, 2);
$time_end_minutes = substr($time_end, 3, 5);
$time_from_hour = (int)substr((string) $focus->time_from, 0, 2);
$time_from_min = substr((string) $focus->time_from, 3, 5);
$time_to_hour = (int)substr((string) $focus->time_to, 0, 2);
$time_to_min = substr((string) $focus->time_to, 3, 5);
$hours_arr = array();
$mins_arr = array();
$num_of_hours = 13;
$start_at = 1;

// setup function drop down
include_once('modules/Schedulers/_AddJobsHere.php');

if (is_array($job_strings) && !empty($job_strings)) {
    $job_function = "<option value=''>--</option>";
    foreach ($job_strings as $k => $function) {
        $job_function .= "<option value='function::".$function."'";
        if ($focus->job === "function::".$function) {
            $job_function .= " SELECTED ";
        }
        $job_function .= ">".$mod_strings['LBL_'.strtoupper($function)]."</option>";
    }
}

if (empty($time_meridiem_start)) {
    $num_of_hours = 24;
    $start_at = 0;
}

for ($i = $start_at; $i < $num_of_hours; $i ++) {
    $i = $i."";
    if (strlen($i) == 1) {
        $i = "0".$i;
    }
    $hours_arr[$i] = $i;
}

for ($j=0; $j<60; $j++) {
    $mins_arr[$j] = str_pad($j, 2, 0, STR_PAD_LEFT);
}

// make two more array with "nulls"
$hours_arr_unreq = $hours_arr;
$mins_arr_unreq = $mins_arr;
$hours_arr_unreq[''] = '--';
$mins_arr_unreq[''] = '--';

// explode crontab notation
if (!empty($focus->job_interval)) {
    $exInterval = explode("::", $focus->job_interval);
} else {
    $exInterval = array('*','*','*','*','*');
}



// TEMPLATE ASSIGNMENTS
$xtpl = new XTemplate('modules/Schedulers/EditView.html');

///////////////////////////////////////////////////////////////////////////////
////	PARSING FOR BASIC SETUP
// Days of the week
$xtpl->assign('USE_ADV_BOOL', 'false');
$xtDays = array(1 => 'MON',
                2 => 'TUE',
                3 => 'WED',
                4 => 'THU',
                5 => 'FRI',
                6 => 'SAT',
                0 => 'SUN');

if ($exInterval[4] == '*') {
    $xtpl->assign('ALL', "CHECKED");
    $xtpl->assign('MON', "CHECKED");
    $xtpl->assign('TUE', "CHECKED");
    $xtpl->assign('WED', "CHECKED");
    $xtpl->assign('THU', "CHECKED");
    $xtpl->assign('FRI', "CHECKED");
    $xtpl->assign('SAT', "CHECKED");
    $xtpl->assign('SUN', "CHECKED");
} elseif (strpos($exInterval[4], ',')) {
    $daysRun = array();
    $exDays = explode(',', trim($exInterval[4]));
    foreach ($exDays as $k => $days) {
        if (strpos($days, '-')) {
            $exDaysRange = explode('-', $days);
            for ($i=$exDaysRange[0]; $i<=$exDaysRange[1]; $i++) {
                $xtpl->assign($xtDays[$days], "CHECKED");
            }
        } else {
            $xtpl->assign($xtDays[$days], "CHECKED");
        }
    }
} elseif (strpos($exInterval[4], '-')) {
    $exDaysRange = explode('-', $exInterval[4]);
    for ($i=$exDaysRange[0]; $i<=$exDaysRange[1]; $i++) {
        $xtpl->assign($xtDays[$i], "CHECKED");
    }
} else {
    $xtpl->assign($xtDays[$exInterval[4]], "CHECKED");
}

// Hours
for ($i=1; $i<=30; $i++) {
    $ints[$i] = $i;
}
$use_adv = false;
if ($exInterval[0] == '*' && $exInterval[1] == '*') {
    $xtpl->assign('BASIC_INTERVAL', get_select_options_with_id($ints, '1'));
    $xtpl->assign('BASIC_PERIOD', get_select_options_with_id($app_list_strings['scheduler_period_dom'], 'min'));
// hours
} elseif (strpos($exInterval[1], '*/') !== false && $exInterval[0] == '0') {
    // we have a "BASIC" type of hour setting
    $exHours = explode('/', $exInterval[1]);
    $xtpl->assign('BASIC_INTERVAL', get_select_options_with_id($ints, $exHours[1]));
    $xtpl->assign('BASIC_PERIOD', get_select_options_with_id($app_list_strings['scheduler_period_dom'], 'hour'));
// Minutes
} elseif (strpos($exInterval[0], '*/') !== false && $exInterval[1] == '*') {
    // we have a "BASIC" type of min setting
    $exMins = explode('/', $exInterval[0]);
    $xtpl->assign('BASIC_INTERVAL', get_select_options_with_id($ints, $exMins[1]));
    $xtpl->assign('BASIC_PERIOD', get_select_options_with_id($app_list_strings['scheduler_period_dom'], 'min'));
// we've got an advanced time setting
} else {
    $xtpl->assign('BASIC_PERIOD', get_select_options_with_id($app_list_strings['scheduler_period_dom'], 'hour'));
    $xtpl->assign('BASIC_INTERVAL', get_select_options_with_id($ints, 10));
    $xtpl->assign('ONLY_ADV', 'DISABLED');
    $xtpl->assign('USE_ADV_BOOL', 'true');
    $use_adv = true;
}



////	END PARSING FOR BASIC
///////////////////////////////////////////////////////////////////////////////




// calendar assignments
$xtpl->assign('DATE_START', $date_start);
$xtpl->assign('TIME_START', $time_start);
$xtpl->assign('DATE_END', $date_end);
$xtpl->assign('TIME_END', $time_end);
$xtpl->assign('TIME_START_HOUR_OPTIONS', get_select_options_with_id($hours_arr, $time_start_hour));
$xtpl->assign('TIME_START_MINUTE_OPTIONS', get_select_options_with_id($mins_arr, $time_start_minutes));
$xtpl->assign('TIME_END_HOUR_OPTIONS', get_select_options_with_id($hours_arr_unreq, $time_end_hour));
$xtpl->assign('TIME_END_MINUTE_OPTIONS', get_select_options_with_id($mins_arr_unreq, $time_end_minutes));
$xtpl->assign('TIME_TO_HOUR_OPTIONS', get_select_options_with_id($hours_arr_unreq, $time_to_hour));
$xtpl->assign('TIME_TO_MIN_OPTIONS', get_select_options_with_id($mins_arr_unreq, $time_to_min));
$xtpl->assign('TIME_FROM_HOUR_OPTIONS', get_select_options_with_id($hours_arr_unreq, $time_from_hour));
$xtpl->assign('TIME_FROM_MIN_OPTIONS', get_select_options_with_id($mins_arr_unreq, $time_from_min));
$xtpl->assign('TIME_MERIDIEM_START', $time_meridiem_start);
$xtpl->assign('TIME_MERIDIEM_END', $time_meridiem_end);
$xtpl->assign('TIME_MERIDIEM_FROM', $time_meridiem_from);
$xtpl->assign('TIME_MERIDIEM_TO', $time_meridiem_to);
if (preg_match('/\d([^\d])\d/', (string) $time_format, $match)) {
    $xtpl->assign('TIME_SEPARATOR', $match[1]);
} else {
    $xtpl->assign('TIME_SEPARATOR', ':');
}
$xtpl->assign('TIME_FORMAT', '('.$time_format.')');
$xtpl->assign('USER_DATEFORMAT', '('.$timedate->get_user_date_format().')');
$xtpl->assign('CALENDAR_DATEFORMAT', $timedate->get_cal_date_format());
// standard assigns
$xtpl->assign('MOD', $mod_strings);
$xtpl->assign('APP', $app_strings);
$xtpl->assign('THEME', (string)SugarThemeRegistry::current());
$xtpl->assign('GRIDLINE', $gridline);
if ($use_adv) {
    $javascript->script .= 'toggleAdv();';
    $xtpl->assign('JAVASCRIPT', get_set_focus_js().$javascript->getScript());
} else {
    $xtpl->assign('JAVASCRIPT', get_set_focus_js().$javascript->getScript());
}
$xtpl->assign('RETURN_MODULE', 'Schedulers');
$xtpl->assign('RETURN_ID', $focus->id);
$xtpl->assign('RETURN_ACTION', 'DetailView');
// module specific
$xtpl->assign('ID', $focus->id);
$xtpl->assign('NAME', $focus->name);
if ($focus->catch_up == 1) {
    $xtpl->assign('CATCH_UP_CHECKED', 'CHECKED');
}
// job
if (strstr((string) $focus->job, 'url::')) {
    $job_url = str_replace('url::', '', (string) $focus->job);
} else {
    $job_url = 'http://';
}

$xtpl->assign('JOB_FUNCTION', $job_function);
$xtpl->assign('JOB_URL', $job_url);
$xtpl->assign('JOB_INTERVAL', $focus->job_interval);


$xtpl->assign('TIME_FROM', $focus->time_from);
$xtpl->assign('TIME_TO', $focus->time_to);
$xtpl->assign('STATUS_OPTIONS', get_select_options_with_id($app_list_strings['scheduler_status_dom'], $focus->status));
$xtpl->assign('MINS', $exInterval[0]);
$xtpl->assign('HOURS', $exInterval[1]);
$xtpl->assign('DAY_OF_MONTH', $exInterval[2]);
$xtpl->assign('MONTHS', $exInterval[3]);
$xtpl->assign('DAY_OF_WEEK', $exInterval[4]);
$xtpl->assign('ROLLOVER', $email->rolloverStyle);

$xtpl->assign('SERVER_TIMEZONE', date("T"));
$xtpl->assign('SERVER_OFFSET', date("O"));

$xtpl->parse("main");
$xtpl->out("main");

//$focus->displayCronInstructions();
