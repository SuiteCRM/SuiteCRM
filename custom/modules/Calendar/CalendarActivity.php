<?php
/**
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
 *
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 * 
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo, "Supercharged by SuiteCRM" logo and “Nonprofitized by SinergiaCRM” logo. 
 * If the display of the logos is not reasonably feasible for technical reasons, 
 * the Appropriate Legal Notices must display the words "Powered by SugarCRM", 
 * "Supercharged by SuiteCRM" and “Nonprofitized by SinergiaCRM”. 
 */


require_once 'include/utils/activity_utils.php';
require_once('modules/Calendar/CalendarActivity.php');


class CustomCalendarActivity extends CalendarActivity
{
    // Overriding this function to include STIC modules into iCal service
    // STIC#625
    public function __construct($args)
    {
        // if we've passed in an array, then this is a free/busy slot
        // and does not have a sugarbean associated to it
        global $timedate;

        if (is_array($args)) {
            $this->start_time = clone $args[0];
            $this->end_time = clone $args[1];
            $this->sugar_bean = null;
            $timedate->tzGMT($this->start_time);
            $timedate->tzGMT($this->end_time);

            return;
        }

        // else do regular constructor..

        $sugar_bean = $args;
        $this->sugar_bean = $sugar_bean;


        if ($sugar_bean->object_name === 'Task') {
            if (!empty($this->sugar_bean->date_start)) {
                $this->start_time = $timedate->fromUser($this->sugar_bean->date_start);
            } else {
                $this->start_time = $timedate->fromUser($this->sugar_bean->date_due);
            }
            if (empty($this->start_time)) {
                return;
            }
            $this->end_time = $timedate->fromUser($this->sugar_bean->date_due);
        // STIC-Custom 20220314 AAM - Adding STIC modules to iCal
        // STIC#625
        // STIC-Custom 20240222 MHP - Adding stic_Work_Calendar module to iCal
        // https://github.com/SinergiaTIC/SinergiaCRM/pull/114
        } else if ($sugar_bean->object_name === 'stic_Sessions' || $sugar_bean->object_name === 'stic_Work_Calendar') {
            $this->start_time = $timedate->fromUser($this->sugar_bean->start_date);
            if (empty($this->start_time)) {
                return;
            }
            $this->end_time = $timedate->fromUser($this->sugar_bean->end_date);
            if (empty($this->end_time)) {
                return;
            }
        } else if ($sugar_bean->object_name === 'stic_FollowUps') {
            $this->start_time = $timedate->fromUser($this->sugar_bean->start_date);
            if (empty($this->start_time)) {
                return;
            }
            $mins = $this->sugar_bean->duration;
            if (empty($mins)) {
                $mins = 0;
            }
            $this->end_time = $this->start_time->get("+$mins minutes");
            if (empty($this->end_time)) {
                return;
            }
        // END STIC
        } else {
            $this->start_time = $timedate->fromUser($this->sugar_bean->date_start);
            if (empty($this->start_time)) {
                return;
            }
            $hours = $this->sugar_bean->duration_hours;
            if (empty($hours)) {
                $hours = 0;
            }
            $mins = $this->sugar_bean->duration_minutes;
            if (empty($mins)) {
                $mins = 0;
            }
            $this->end_time = $this->start_time->get("+$hours hours $mins minutes");
        }
        // Convert it back to database time so we can properly manage it for getting the proper start and end dates
        $timedate->tzGMT($this->start_time);
        $timedate->tzGMT($this->end_time);
    }

    // Overriding this function to use CustomCalendarActivity
    // STIC#625
    public static function get_activities(
        $activities,
        $user_id,
        $show_tasks,
        $view_start_time,
        $view_end_time,
        $view,
        $show_calls = true,
        $show_completed = true
    ) {
        global $current_user;
        global $beanList;
        $act_list = array();
        $seen_ids = array();

        $completedCalls = '';
        $completedMeetings = '';
        $completedTasks = '';
        if (!$show_completed) {
            $completedCalls = " AND calls.status = 'Planned' ";
            $completedMeetings = " AND meetings.status = 'Planned' ";
            $completedTasks = " AND tasks.status != 'Completed' ";
        }

        // STIC-Custom 20240222 MHP - Get the user preference
        // https://github.com/SinergiaTIC/SinergiaCRM/pull/114
        $show_work_calendar = $GLOBALS['current_user']->getPreference('show_work_calendar');
        $show_work_calendar = $show_work_calendar ?: false;
        // END STIC-Custom

        foreach ($activities as $key => $activity) {
            if (ACLController::checkAccess($key, 'list', true)) {
                /* END - SECURITY GROUPS */
                $class = $beanList[$key];
                $bean = new $class();

                if ($current_user->id === $user_id) {
                    $bean->disable_row_level_security = true;
                }

                $where = self::get_occurs_within_where_clause(
                    $bean->table_name,
                    isset($bean->rel_users_table) ? $bean->rel_users_table : null,
                    $view_start_time,
                    $view_end_time,
                    $activity['start'],
                    $activity['end']
                );

                if ($key === 'Meetings') {
                    $where .= $completedMeetings;
                } elseif ($key === 'Calls') {
                    $where .= $completedCalls;
                    if (!$show_calls) {
                        continue;
                    }
                } elseif ($key === 'Tasks') {
                    $where .= $completedTasks;
                    if (!$show_tasks) {
                        continue;
                    }
                }

                // STIC-Custom 20240222 MHP - Get the user preference
                // https://github.com/SinergiaTIC/SinergiaCRM/pull/114
                if ($key === 'stic_Work_Calendar' && !$show_work_calendar) {
                    continue;
                }
                // END STIC-Custom

                $focus_list = build_related_list_by_user_id($bean, $user_id, $where);
                // require_once 'modules/SecurityGroups/SecurityGroup.php';
                foreach ($focus_list as $focusBean) {
                    if (isset($seen_ids[$focusBean->id])) {
                        continue;
                    }
                    /* TODO update currently unused functionality, disabled as expensive
                    $in_group = SecurityGroup::groupHasAccess($key, $focusBean->id, 'list');
                    $show_as_busy = !ACLController::checkAccess(
                        $key,
                        'list',
                        $current_user->id === $user_id,
                        'module',
                        $in_group
                    );
                    $focusBean->show_as_busy = $show_as_busy;*/

                    $seen_ids[$focusBean->id] = 1;
                    // STIC-Custom 20220314 AAM - Adding STIC modules to iCal
                    // STIC#625
                    // $act = new CalendarActivity($focusBean);
                    $act = new CustomCalendarActivity($focusBean);

                    if (!empty($act)) {
                        // STIC-Custom 20241004 MHP - Exclude cancelled work calendar records from displaying them in the Activity Calendar
                        // https://github.com/SinergiaTIC/SinergiaCRM/pull/425
                        if ($act->sugar_bean->module_dir === 'stic_Work_Calendar' && $act->sugar_bean->type == 'canceled' ) {
                            continue;
                        }
                        // END STIC-Custom                        
                        $act_list[] = $act;
                    }
                }
            }
        }

        return $act_list;
    }
}