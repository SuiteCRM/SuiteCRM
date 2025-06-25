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

require_once 'SticInclude/Utils.php';

/**
 * Check for each user who has activated the Time Tracker and Work Calendar modules if the user has completed, or not, 
 * a percentage of the duration of his working day, whether he has worked more or less hours
 */
class CheckHoursWorkedInPreviousWeek extends DataCheckFunction 
{
    /**
     * It receives a proposal from SQL and modifies it with the necessary features for the function.
     * Most functions should override this method.
     * @param $actionBean Bean of the action the function is running on.
     * @param $proposedSQL Automatically generated array (if possible) with the keys select, from, where and order_by.
     * @return string
     */
    public function prepareSQL(stic_Validation_Actions $actionBean, $proposedSQL)
    {

        global $current_user;
        $tzone = $current_user->getPreference('timezone') ?? $sugar_config['default_timezone'] ?? date_default_timezone_get();

        // Obtain the total hours scheduled and worked by each user during the previous week.
        // Only those users who have activated the use of the Time Tracker and Work Calendar modules will be selected.
        // The query returns duplicate records but there is a subsequent process that eliminates them. This is done to consider the use case 
        // where there are users who have records in one module and not the other.
        $sql = "
        SELECT tt.assigned_user_id as ttAssignedUser, tt.total_duration as ttDuration, wc.assigned_user_id  as wcAssignedUser, wc.total_duration  as wcDuration
        FROM (
            SELECT stt.assigned_user_id, SUM(stt.duration) AS total_duration, sttu.stic_clock_c
            FROM stic_time_tracker as stt
            JOIN users_cstm as sttu ON stt.assigned_user_id = sttu.id_c
            WHERE DATE(CONVERT_TZ(stt.start_date, '+00:00', '" . $tzone ."')) BETWEEN DATE(DATE_SUB(CONVERT_TZ(UTC_TIMESTAMP(), '+00:00', '" . $tzone ."'), INTERVAL 7 DAY)) AND DATE(DATE_SUB(CONVERT_TZ(UTC_TIMESTAMP(), '+00:00', '" . $tzone ."'), INTERVAL 1 DAY))
                AND stt.deleted = 0
            GROUP BY stt. assigned_user_id) AS tt
        LEFT JOIN (
            SELECT swc.assigned_user_id, SUM(swc.duration) AS total_duration, swcu.stic_work_calendar_c
            FROM stic_work_calendar swc
            JOIN users_cstm as swcu ON swc.assigned_user_id = swcu.id_c            
            WHERE DATE(CONVERT_TZ(swc.start_date, '+00:00', '" . $tzone ."')) BETWEEN DATE(DATE_SUB(CONVERT_TZ(UTC_TIMESTAMP(), '+00:00', '" . $tzone ."'), INTERVAL 7 DAY)) AND DATE(DATE_SUB(CONVERT_TZ(UTC_TIMESTAMP(), '+00:00', '" . $tzone ."'), INTERVAL 1 DAY))
                AND swc.type = 'working'
                AND swc.type != 'canceled'         
                AND swc.deleted = 0
            GROUP BY swc.assigned_user_id) AS wc 
        ON tt.assigned_user_id = wc.assigned_user_id

        UNION ALL
        
        SELECT tt.assigned_user_id as ttAssignedUser, tt.total_duration as ttDuration, wc.assigned_user_id  as wcAssignedUser, wc.total_duration  as wcDuration
        FROM (
            SELECT stt.assigned_user_id, SUM(stt.duration) AS total_duration, sttu.stic_clock_c
            FROM stic_time_tracker as stt
            JOIN users_cstm as sttu ON stt.assigned_user_id = sttu.id_c
            WHERE DATE(CONVERT_TZ(stt.start_date, '+00:00', '" . $tzone ."')) BETWEEN DATE(DATE_SUB(CONVERT_TZ(UTC_TIMESTAMP(), '+00:00', '" . $tzone ."'), INTERVAL 7 DAY)) AND DATE(DATE_SUB(CONVERT_TZ(UTC_TIMESTAMP(), '+00:00', '" . $tzone ."'), INTERVAL 1 DAY))
                AND stt.deleted = 0
            GROUP BY stt. assigned_user_id) AS tt
        RIGHT JOIN (
            SELECT swc.assigned_user_id, SUM(swc.duration) AS total_duration, swcu.stic_work_calendar_c
            FROM stic_work_calendar swc
            JOIN users_cstm as swcu ON swc.assigned_user_id = swcu.id_c            
            WHERE DATE(CONVERT_TZ(swc.start_date, '+00:00', '" . $tzone ."')) BETWEEN DATE(DATE_SUB(CONVERT_TZ(UTC_TIMESTAMP(), '+00:00', '" . $tzone ."'), INTERVAL 7 DAY)) AND DATE(DATE_SUB(CONVERT_TZ(UTC_TIMESTAMP(), '+00:00', '" . $tzone ."'), INTERVAL 1 DAY))
                AND swc.type = 'working'
                AND swc.type != 'canceled'
                AND swc.deleted = 0
            GROUP BY swc.assigned_user_id) AS wc 
        ON tt.assigned_user_id = wc.assigned_user_id;";

        return $sql;
    }


    /**
     * DoAction function
     * Perform the action defined in the function
     * @param $records Set of records on which the validation action is to be applied 
     * @param $actionBean stic_Validation_Actions Bean of the action in which the function is being executed.
     * @return boolean It will return true in case of success and false in case of error.
     */
    public function doAction($records, stic_Validation_Actions $actionBean) 
    {
        include_once 'modules/stic_Validation_Actions/DataAnalyzer/Functions/include/Mailing/Utils.php';
        global $sugar_config, $current_user;

        // It will indicate if records have been found to validate.
        $count = 0;
        // It will indicate if records with errors have been found.
        $errors = 0;
        // Array donde ir añadiendo a los usuarios validados ya que la query, al consultar dos módulos, pudiera devolver usuarios repetidos. 
        $validatedUsers = array();

        // Get upper and lower margin settings
        include_once 'modules/stic_Settings/Utils.php';
        $lowerMarginPercent = stic_SettingsUtils::getSetting('TIMETRACKER_LOWER_MARGIN_PERCENT');
        $upperMarginPercent = stic_SettingsUtils::getSetting('TIMETRACKER_UPPER_MARGIN_PERCENT');

        while ($row = array_pop($records)) 
        {
            $stAssignedUser = $row['ttAssignedUser'];
            $wcAssignedUser = $row['wcAssignedUser'];

            // Get the assigned user
            if (!empty($stAssignedUser)) {
                $assignedUser = BeanFactory::getBean('Users', $stAssignedUser);
            } else {
                $assignedUser = BeanFactory::getBean('Users', $wcAssignedUser);
            }
            
            // If the user is not empty and has not been previously validated
            if (!empty($assignedUser) && !in_array($assignedUser->id, $validatedUsers)) 
            {
                // Add the user id to the validated users array
                array_push($validatedUsers, $assignedUser->id);      
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Validating the next users: {$assignedUser->name}.");
                
                // Check if the user has the Time Tracker and Work Calendar option activated
                $isActivateTimeTracker = $assignedUser->stic_clock_c == '1';
                $isActivateWorkCalendar = $assignedUser->stic_work_calendar_c == '1';
                if ($isActivateTimeTracker && $isActivateWorkCalendar) {

                    // Manage the counter
                    $count++; 

                    // get the durations of both modules
                    $ttDuration = $row['ttDuration'];
                    $wcDuration = $row['wcDuration'];
                    
                    $error = 0;
                    if (!empty($ttDuration) && !empty($wcDuration)) 
                    {
                        // Calculate the upper and lower allowed difference
                        $lowerDifference = ($lowerMarginPercent === false) ? 0 : $wcDuration * ($lowerMarginPercent/100);
                        $upperDifference = ($upperMarginPercent === false) ? 0 : $wcDuration * ($upperMarginPercent/100);
                        
                        // Get the difference between the two durations
                        $difference = $wcDuration - $ttDuration;
                        
                        if (   ($difference > 0 && $difference > $lowerDifference) 
                            || ($difference < 0 && $difference > $upperDifference)) 
                        {
                            $errorMsg = $this->getLabel('HOURS_NOT_MATCH') . $assignedUser->name;
                            $error = 1;
                        }
                    } else {
                        $errorMsg = $this->getLabel('HOURS_NOT_MATCH') . $assignedUser->name;
                        $error = 1;
                    }

                    if ($error == 1) 
                    {
                        // Manage the counter
                        $errors++;
                        
                        // Create the validation result
                        if (!empty($errorMsg)) 
                        {
                            $dateFormat = $current_user->getUserDateTimePreferences();
                            $weekAgoDate = new DateTime();
                            $weekAgoDate->modify('-1 week');
                            $weekAgoDate = $weekAgoDate->format($dateFormat["date"]);
                            $oneDayAgoDate = new DateTime();
                            $oneDayAgoDate->modify('-1 day');
                            $oneDayAgoDate = $oneDayAgoDate->format($dateFormat["date"]);

                            $errorMsg .= '<br /><br />';
                            $errorMsg .= '<a style="text-decoration:none" href="' . $sugar_config["site_url"] . '/index.php?module=stic_Time_Tracker&action=index&query=true&searchFormTab=advanced_search&assigned_user_id_advanced=' . $assignedUser->id . '&start_date_advanced_range_choice=between&start_range_start_date_advanced=' . $weekAgoDate .'&end_range_start_date_advanced=' . $oneDayAgoDate.'"><span class="suitepicon suitepicon-action-list-maps" style="font-size:12px">&nbsp;&nbsp;</span><span> - ' . $this->getLabel("STIC_TIME_TRACKER_LIST_VIEW") . '</span></a><br />';
                            $errorMsg .= '<a style="text-decoration:none" href="' . $sugar_config["site_url"] . '/index.php?module=stic_Work_Calendar&action=index&query=true&searchFormTab=advanced_search&assigned_user_id_advanced=' . $assignedUser->id . '&start_date_advanced_range_choice=between&start_range_start_date_advanced=' . $weekAgoDate .'&end_range_start_date_advanced=' . $oneDayAgoDate.'"><span class="suitepicon suitepicon-action-list-maps" style="font-size:12px">&nbsp;&nbsp;</span><span> - ' . $this->getLabel("STIC_WORK_CALENDAR_LIST_VIEW") . '</span></a><br />';
                            $errorMsg .= '<br />';

                            $validationResultMsg = '<span style="color:red;">' . $errorMsg . '</span>';                        
                            $data = array(
                                'name' => $this->getLabel('NAME'),
                                'stic_validation_actions_id' => $actionBean->id,
                                'log' => '<div>' . $validationResultMsg . '</div>',
                                'parent_type' => $this->functionDef['module'],
                                'parent_id' => $assignedUser->id,
                                'reviewed' => 'no',   
                                'assigned_user_id' => $assignedUser->id,
                            );
                            $this->logValidationResult($data);

                            $info['subject'] = $this->getLabel('EMAIL_SUBJECT');
                            $info['errorMsg'] = $errorMsg;
                            $info['module'] = $this->functionDef["module"];                        
                            sendEmailToEmployeeAndResponsible($assignedUser, $row, $info);
                        }

                        // Report_always
                        global $current_user;
                        if (!$count && $actionBean->report_always) {
                            $errorMsg = $this->getLabel('NO_ROWS');
                            $data = array(
                                'name' => $errorMsg,
                                'stic_validation_actions_id' => $actionBean->id,
                                'log' => '<div>' . $errorMsg . '</div>',
                                'reviewed' => 'not_necessary',              
                                'assigned_user_id' => $current_user->id, // In this message we indicate the administrator user
                            );
                            $this->logValidationResult($data);
                        } else if (!$errors && $actionBean->report_always) {
                            $errorMsg = $this->getLabel('NO_ERRORS');
                            $data = array(
                                'name' => $errorMsg,
                                'stic_validation_actions_id' => $actionBean->id,
                                'log' => '<div>' . $errorMsg . '</div>',
                                'reviewed' => 'not_necessary',       
                                'assigned_user_id' => $current_user->id, // In this message we indicate the administrator user            
                            );
                            $this->logValidationResult($data);
                        }
                        $error = 0;
                    }    
                }      
            }
        }
        
        $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ": Reviewed Records [{$count}] wrong [{$errors}]");        
        return true;
    }
}