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
 * Class to check the previous day Time Tracker records
 */
class CheckWorkCalendarBeanData extends DataCheckFunction 
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

        $sql = "SELECT id, name, start_date, assigned_user_id
                FROM stic_work_calendar
                WHERE DATE(CONVERT_TZ(start_date, '+00:00', '" . $tzone ."')) = DATE(DATE_SUB(CONVERT_TZ(UTC_TIMESTAMP(), '+00:00', '" . $tzone ."'), INTERVAL 1 DAY))
                    AND type = 'working'
                    AND type != 'canceled'
                    AND deleted = 0
                ORDER BY assigned_user_id;";

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
        // It will indicate if records have been found to validate.
        $count = 0;
        // It will indicate if records with errors have been found.
        $errors = 0;

        include_once 'modules/stic_Validation_Actions/DataAnalyzer/Functions/include/Mailing/Utils.php';
        $info['subject'] = $this->getLabel('EMAIL_SUBJECT');
        $info['body'] = $this->getLabel('EMAIL_BODY');

        while ($row = array_pop($records)) 
        {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Validating the next record: {$row['name']}.");

            $assignedUserId = $row['assigned_user_id'];
            $assignedUser = BeanFactory::getBean('Users', $assignedUserId);
            $isActivateTimeTracker = $assignedUser->stic_clock_c == '1';
            $isActivateWorkCalendar = $assignedUser->stic_work_calendar_c == '1';

            $errorMsg = '';
            $contTemp = 0;
            if ($isActivateWorkCalendar) 
            {
                if ($isActivateTimeTracker) {
                    include_once 'modules/stic_Time_Tracker/stic_Time_Tracker.php';  
                    if (!stic_Time_Tracker::existAtLeastOneRecordFromYesterday($assignedUserId)) {
                        $errorMsg = $this->getLabel('NO_RECORD_IN_TIME_TRACKER');
                        $contTemp = 1;
                    }
                } 
            } else {
                $errorMsg = $this->getLabel('WORK_CALENDAR_INACTIVE_IN_USER') . $assignedUser->name;
                $contTemp = 1;
            }

            // Manage the counters
            $count++; 
            $errors = $errors + $contTemp;
            
            // Create the validation results
            if (!empty($errorMsg)) {    
                $errorMsg = '<span style="color:red;">' . $errorMsg . '</span>';
                $data = array(
                    'name' => $this->getLabel('NAME'),
                    'stic_validation_actions_id' => $actionBean->id,
                    'log' => '<div>' . $errorMsg . '</div>',
                    'parent_type' => $this->functionDef['module'],
                    'parent_id' => $row['id'],
                    'reviewed' => 'no',   
                    'assigned_user_id' => $row['assigned_user_id'],
                );
                $this->logValidationResult($data);
                $info['errorMsg'] = $errorMsg;
                $info['module'] = $this->functionDef["module"];                
                sendEmailToEmployeeAndResponsible($assignedUser, $row, $info);
            }
        }

        $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ": Reviewed Records [{$count}] wrong [{$errors}]");

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

        return true;
    }
}
