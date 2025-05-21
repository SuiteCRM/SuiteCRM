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
class stic_Work_CalendarUtils
{
    /**
     * Check if the work calendar record to be created causes an error with other records that already exist in the CRM
     * - If there is already a non-work record that takes up the entire day, in that case, it is not posible to create the record
     * - If exist a record that does not occupy the entire day, in that case, since the record to be created is an all-day record
     * @param $id   record identification
     * @param $startDate record start date
     * @param $endDate  record end date
     * @param $type record type
     * @param $assignedUserId   record assigned user identificator
     * @return boolean true if exists and false if not
     */
    public static function existsRecordsWithIncompatibleType($id, $startDate, $endDate, $type, $assignedUserId)
    {
        if ($type != 'canceled') {
            require_once 'modules/stic_Work_Calendar/stic_Work_Calendar.php';

            // Check if there is already a non-work record that takes up the entire day, in that case, it is not posible to create the record
            global $db, $current_user;
            $tzone = $current_user->getPreference('timezone') ?? $sugar_config['default_timezone'] ?? date_default_timezone_get();        

            $query = "SELECT * FROM stic_work_calendar
                        WHERE deleted = 0 
                            AND type != 'canceled'
                            AND id != '". $id . "' 
                            AND assigned_user_id = '" . $assignedUserId . "' 
                            AND type IN ('" .  implode("', '", stic_Work_Calendar::ALL_DAY_TYPES) . "')
                            AND DATE(CONVERT_TZ(start_date, '+00:00', '" . $tzone ."')) = DATE(CONVERT_TZ('" . $startDate . "', '+00:00', '" . $tzone ."'))";

            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": " . $query);
            $result = $db->query($query);

            if (!is_null($result) && $result->num_rows > 0) {
                return false;
            } else {
                if (in_array($type, stic_Work_Calendar::ALL_DAY_TYPES)) {
                    // Checks if exist a record that does not occupy the entire day, in that case, since the record to be created is an all-day record, it is not possible to create the record.
                    $query = "SELECT * FROM stic_work_calendar
                        WHERE deleted = 0 
                            AND id != '". $id . "' 
                            AND assigned_user_id = '" . $assignedUserId . "' 
                            AND type != 'canceled'
                            AND type NOT IN ('" .  implode("', '", stic_Work_Calendar::ALL_DAY_TYPES) . "')
                            AND DATE(CONVERT_TZ(start_date, '+00:00', '" . $tzone ."')) = DATE(CONVERT_TZ('" . $startDate . "', '+00:00', '" . $tzone ."'))";

                    $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": " . $query);
                    $result = $db->query($query);

                    if (!is_null($result) && $result->num_rows > 0) {
                        return false;
                    } else {
                        return true;
                    }
                } else {
                    // Checks if exist a record that does not occupy the entire day, in that case, since the record to be created is an all-day record, it is not possible to create the record.
                    $query = "SELECT * FROM stic_work_calendar
                        WHERE deleted = 0 
                            AND id != '". $id . "' 
                            AND assigned_user_id = '" . $assignedUserId . "' 
                            AND type != 'canceled'
                            AND type NOT IN ('" .  implode("', '", stic_Work_Calendar::ALL_DAY_TYPES) . "')                        
                            AND TIMESTAMPDIFF(SECOND, start_date,  '" . $endDate . "') > 0
                            AND TIMESTAMPDIFF(SECOND, '" . $startDate . "',end_date) > 0";
                    $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": " . $query);
                    $result = $db->query($query);

                    if (!is_null($result) && $result->num_rows > 0) {
                        return false;
                    }
                }
            }
        }
        return true;
    }

    /**
     * Creates periodic work calendar records for a employee or employees, based on the parameters received via $_REQUEST
     * and defined in the periodic work calendar records creation wizard (custom/modules/stic_Work_Calendar/tpls/workCalendarWizard.tpl)
     *
     * @return void
     */
    public static function createPeriodicWorkCalendarRecords()
    {
        // Disable Advanced Open Discovery to avoid slowing down the writing of the records affected by this function.
        global $sugar_config, $app_list_strings, $current_user, $timedate;
        $aodConfig = $sugar_config['aod']['enable_aod'];
        $sugar_config['aod']['enable_aod'] = false;
        $startTime = microtime(true);

        // Get the data from the smarty template
        $repeat_type = $_REQUEST['repeat_type'] ?: 'Daily' ;
        $interval = $_REQUEST['repeat_interval'] ?: '1';
        $count = $_REQUEST['repeat_count'] ?? '';
        $until = $_REQUEST['repeat_until'] ?? '';
        $type = $_REQUEST['type'];
        $startDay = $_REQUEST['repeat_start_day'];
        $finalDay = $_REQUEST['repeat_final_day'];
        $startHour = $_REQUEST['repeat_start_hour'];
        $startMinute = $_REQUEST['repeat_start_minute'];
        $finalHour = $_REQUEST['repeat_final_hour'];
        $finalMinute = $_REQUEST['repeat_final_minute'];
        
        // Get absolute values of 'minutes' and set values
        // Set minute interval as defined in $sugar_config
        $m = 0;
        $minutesInterval = 1;
        $repeatMinuts1 = array('00');
        do {
            $m = $m + $minutesInterval;
            $repeatMinuts1[] = str_pad($m, 2, '0', STR_PAD_LEFT);
        } while ($m < (60 - $minutesInterval));
        $startMinute = $repeatMinuts1[$startMinute];
        $finalMinute = $repeatMinuts1[$finalMinute];

        // Take the dates collected in the smarty template and set their values
        // in order to calculate the duration of the work calendar record
        $until = str_replace('/', '-', $until);
        $until = date("Y-m-d", strtotime($until));
        $startDay = str_replace('/', '-', $startDay);
        $startDay = date('Y-m-d H:i:s', strtotime($startDay . " + $startHour hours + $startMinute minutes"));
        $finalDay = str_replace('/', '-', $finalDay);
        $finalDay = date('Y-m-d H:i:s', strtotime($finalDay . " + $finalHour hours + $finalMinute minutes"));
        $duration = strtotime($finalDay) - strtotime($startDay);

        // Depending on the chosen type, perform the right operation
        // (none, daily, weekly, monthly or annual)
        $date = [];
        if ($repeat_type == '') {
            header("Location: index.php?action=index&module=stic_Work_Calendar");
        } else {
            // Daily
            if ($repeat_type == 'Daily') {
                $firstDay = $startDay;
                if ($count != '' and $count != '0') {
                    for ($i = 0; $i < $count; $i++) {
                        $date[$i] = $firstDay;
                        $firstDay = date('Y-m-d H:i:s', strtotime($firstDay . " + $interval days"));
                    }
                } else if ($until != '') {
                    $first_d = date("Y-m-d", strtotime($firstDay));
                    for ($i = 0; strtotime($first_d) <= strtotime($until); $i++) {
                        $date[$i] = $firstDay;
                        $firstDay = date('Y-m-d H:i:s', strtotime($firstDay . " + $interval days"));
                        $first_d = date("Y-m-d", strtotime($firstDay));
                    }
                }
            }
            // Monthly
            if ($repeat_type == 'Monthly') {
                $firstMonth = $startDay;
                if ($count != '' and $count != '0') {
                    for ($i = 0; $i < $count; $i++) {
                        $date[$i] = $firstMonth;
                        $firstMonth = date('Y-m-d H:i:s', strtotime($firstMonth . " + $interval months"));
                    }
                } else if ($until != '') {
                    $firstM = date("Y-m-d", strtotime($firstMonth));
                    for ($i = 0; strtotime($firstM) <= strtotime($until); $i++) {
                        $date[$i] = $firstMonth;
                        $firstMonth = date('Y-m-d H:i:s', strtotime($firstMonth . " + $interval  months"));
                        $firstM = date("Y-m-d", strtotime($firstMonth));
                    }
                }
            }
            // Yearly
            if ($repeat_type == 'Yearly') {
                $firstYear = $startDay;
                if ($count != '' and $count != '0') {
                    for ($i = 0; $i < $count; $i++) {
                        $date[$i] = $firstYear;
                        $firstYear = date('Y-m-d H:i:s', strtotime($firstYear . " + $interval years"));
                    }
                } else if ($until != '') {
                    $firstY = date("Y-m-d", strtotime($firstYear));
                    for ($i = 0; strtotime($firstY) <= strtotime($until); $i++) {
                        $date[$i] = $firstYear;
                        $firstYear = date('Y-m-d H:i:s', strtotime($firstYear . " + $interval years"));
                        $firstY = date("Y-m-d", strtotime($firstYear));
                    }
                }
            }
            // Weekly
            if ($repeat_type == 'Weekly') {
                // We create the table $dow of the days of the week, fixing the problem that
                // in the smarty template Sunday is in position '0' and not in position '7'
                $times = 0;
                for ($i = 1; $i < 7; $i++) {
                    $dow[$i] = $_REQUEST['repeat_dow_' . $i] ?? '';
                    if ($dow[$i] == 'on') {
                        $times = $times + 1;
                        $dow[$i] = 1;} else { $dow[$i] = 0;}
                }
                $zero = 0;
                $dow[7] = $_REQUEST['repeat_dow_' . $zero] ?? '';
                if ($dow[7] == 'on') {
                    $times = $times + 1;
                    $dow[7] = 1;} else { $dow[7] = 0;}

                if ($times > '0') {
                    $days = array('1', '2', '3', '4', '5', '6', '7');
                    $firstWeek = $startDay;
                    $week = 1;
                    $i = 0;
                    if ($count != '' and $count != '0') {
                        while ($i < $count) {
                            if ($week == '1') {
                                $firstDate = $firstWeek;
                                $weekDay = $days[date('w', strtotime($firstDate))];
                                $weekDay2 = $weekDay;
                                if ($weekDay == '1') {$weekDay = '7';} else { $weekDay = $weekDay - 1;}
                                for ($x = $weekDay; $x < 8; $x++) {
                                    if ($dow[$x] == 1) {
                                        $adder = $x - $weekDay;
                                        $firstWeek = date('Y-m-d H:i:s', strtotime($firstDate . " + $adder days"));
                                        $date[$i] = $firstWeek;
                                        $i = $i + 1;
                                        if ($i == $count) {$x = 8;}
                                    }
                                }
                                $week = '2';
                                if ($interval > 1) {$x = $interval - 1;} else { $x = '0';}
                                if ($weekDay2 == '1') {
                                    $weekDay2 = '8';
                                    $x = 0;}
                                $differenceDay = 8 - $weekDay2;
                                $firstWeek = date('Y-m-d H:i:s', strtotime($startDay . " + $x weeks + $differenceDay days"));
                            }

                            if ($week == '2' and $i < $count) {
                                $newWeek = $firstWeek;
                                for ($x = 1; $x < 8; $x++) {
                                    if ($dow[$x] == 1) {
                                        $addDays = $x;
                                        $firstWeek = date('Y-m-d H:i:s', strtotime($newWeek . " + $addDays days"));
                                        $date[$i] = $firstWeek;
                                        $i = $i + 1;if ($i == $count) {$x = 8;}
                                    }
                                }
                                $firstWeek = date('Y-m-d H:i:s', strtotime($newWeek . " + $interval weeks"));
                            }
                        }
                    } else if ($until != '') {
                        $firstWeek = $startDay;
                        $week = 1;
                        $i = 0;

                        while (strtotime($firstWeek) <= strtotime($until)) {
                            if ($week == '1') {
                                $startDay = $timedate->asDb($timedate->fromString($startDay), $current_user);
                                $finalDay = $timedate->asDb($timedate->fromString($finalDay), $current_user);
                                $firstDate = $firstWeek;
                                $weekDay = $days[date('w', strtotime($firstDate))];
                                $weekDay2 = $weekDay;
                                if ($weekDay == '1') {$weekDay = '7';} else { $weekDay = $weekDay - 1;}
                                for ($x = $weekDay; $x < 8; $x++) {
                                    if ($dow[$x] == 1) {
                                        $adder = $x - $weekDay;
                                        $firstWeek = date('Y-m-d H:i:s', strtotime($firstDate . " + $adder days"));
                                        $date[$i] = $firstWeek;
                                        $i = $i + 1;
                                        if (strtotime($firstWeek) == strtotime($until)) {$x = 8;}
                                    }
                                }
                                $week = '2';
                                if ($interval > 1) {$x = $interval - 1;} else { $x = '0';}
                                if ($weekDay2 == '1') {
                                    $weekDay2 = '8';
                                    $x = 0;}
                                $differenceDay = 8 - $weekDay2;
                                $firstWeek = date('Y-m-d H:i:s', strtotime($firstDate . " + $x weeks + $differenceDay days"));
                            }
                            if ($week == '2' and strtotime($firstWeek) <= strtotime($until)) {
                                $newWeek = $firstWeek;
                                for ($x = 1; $x < 8; $x++) {
                                    if ($dow[$x] == 1) {
                                        $addDays = $x;
                                        $firstWeek = date('Y-m-d H:i:s', strtotime($newWeek . " + $addDays days"));

                                        if ($until >= substr($firstWeek, 0, 10)) {
                                            $date[$i] = $firstWeek;
                                        }

                                        $i = $i + 1;
                                        if (strtotime($firstWeek) == strtotime($until)) {$x = 8;}
                                    }
                                }

                                $firstWeek = date('Y-m-d H:i:s', strtotime($newWeek . " + $interval weeks"));
                            }
                        }
                    }
                } else {
                    $firstWeek = $startDay;
                    if ($count != '' and $count != '0') {
                        for ($i = 0; $i < $count; $i++) {
                            $date[$i] = $firstWeek;
                            $firstWeek = date('Y-m-d H:i:s', strtotime($firstWeek . " + $interval weeks"));
                        }
                    } else if ($until != '') {
                        $firstW = date("Y-m-d", strtotime($firstWeek));
                        for ($i = 0; strtotime($firstW) <= strtotime($until); $i++) {
                            $date[$i] = $firstWeek;
                            $firstWeek = date('Y-m-d H:i:s', strtotime($firstWeek . " + $interval weeks"));
                            $firstW = date("Y-m-d", strtotime($firstWeek));
                        }
                    }
                }
            }
        }

        // Loop for work calendar records creation
        if (!empty($_REQUEST["assigned_user_id"])) {
            $employeeIDs = array (0 => $_REQUEST["assigned_user_id"]);
        } else if (!empty($_REQUEST["employeeId"])){
            $employeeIDs = array (0 => $_REQUEST["employeeId"]);
        } else {
            $employeeIDs = explode(",", $_REQUEST["employeeIds"]);
        } 

        $aux = array();
        $counter = count($date);
        for ($i = 0; $i < $counter; $i++) 
        {
            $aux[$i] = $timedate->to_db($timedate->to_display_date_time($date[$i], true, false, $current_user));
        }
        
        $summary['global'] = array(
            'totalRecordsProcessed' => 0,
            'totalRecordsCreated' => 0,
            'totalRecordsNotCreated' => 0,
        );
        foreach ($employeeIDs as $assignedUserId) 
        {
            $employee = BeanFactory::getBean('Users', $assignedUserId);
            $summary['users'][$assignedUserId] = array(
                'name' => $employee->full_name,
                'numRecordsProcessed' => 0,
                'numRecordsCreated' => 0,
                'numRecordsNotCreated' => 0,
            );
            for ($i = 0; $i < $counter; $i++) 
            {
                if ($finalDay != '') {
                    $finalDay = strtotime($aux[$i]) + $duration;
                    $finalDay = date('Y-m-d H:i:s', $finalDay);
                    if (in_array($type, stic_Work_Calendar::ALL_DAY_TYPES)){
                        $finalDay = new DateTime($finalDay);
                        $finalDay->modify('+24 hours');
                        $finalDay = $finalDay->format(TimeDate::DB_DATETIME_FORMAT);
                    }
                }
                $workCalendarBean = BeanFactory::newBean('stic_Work_Calendar');
                $workCalendarBean->start_date = $aux[$i];
                $workCalendarBean->end_date = $finalDay;

                if (isset($type) && $type != '') {
                    $workCalendarBean->type = $type;
                }
                $workCalendarBean->assigned_user_id = $assignedUserId;
                if (isset($_REQUEST['description']) && $_REQUEST['description'] != '') {
                    $workCalendarBean->description = $_REQUEST['description'];
                }
                
                $save = self::existsRecordsWithIncompatibleType('', $aux[$i], $finalDay, $type, $assignedUserId);
                if ($save) {
                    $workCalendarBean->save(false);
                    $summary['global']['totalRecordsCreated']++;
                    $summary['users'][$assignedUserId]['numRecordsCreated']++;
                } else {
                    $user = BeanFactory::getBean('Users', $assignedUserId);
                    $summary['global']['totalRecordsNotCreated']++;
                    $summary['users'][$assignedUserId]['numRecordsNotCreated']++;
                    $startDate = $timedate->fromDbFormat($aux[$i], TimeDate::DB_DATETIME_FORMAT);
                    $startDate = $timedate->asUser($startDate, $current_user);
                    $endDate = $timedate->fromDbFormat($finalDay, TimeDate::DB_DATETIME_FORMAT);
                    $endDate = $timedate->asUser($endDate, $current_user);    
                    $typeLabel = $app_list_strings['stic_work_calendar_types_list'][$type];
                    $summary['users'][$assignedUserId]['recordsNotCreated'][] =  array ('username' => $user->name, 'type' => $typeLabel, 'startDate' => $startDate, 'endDate' => $endDate);
                }
                $summary['global']['totalRecordsProcessed']++;
                $summary['users'][$assignedUserId]['numRecordsProcessed']++;
            }
        }
        $_SESSION['summary'] = $summary;
        $endTime = microtime(true);
        $totalTime = $endTime - $startTime;
        $GLOBALS['log']->debug(__METHOD__ . '(' . __LINE__ . ") >> Has been created $i work calendar records in $totalTime seconds");

        // Reactivamos la configuración previa de Advanced Open Discovery
        $sugar_config['aod']['enable_aod'] = $aodConfig;
        
        header("Location: index.php?module=stic_Work_Calendar&action=workCalendarAssistantSummary"); // From list view
    }
}
