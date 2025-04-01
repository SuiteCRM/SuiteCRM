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
class updateDependencies
{
    public function update_dependency(&$bean, $event, $arguments)
    {
        //Get all tasks that are dependant on the current task being saved.
        $Task = BeanFactory::getBean('ProjectTask');
        $tasks = $Task->get_full_list("", "project_task.project_id = '".$bean->project_id."' AND project_task.predecessors = '".$bean->project_task_id."'");

        // Make sure the fetched row exists.
        if ($bean->fetched_row === false) {
            $fetchedDateFinish = null;
        } else {
            $fetchedDateFinish = $bean->fetched_row['date_finish'];
        }

        if ($bean->date_finish != $fetchedDateFinish) { //if the end date of a current task is changed

            $diff = $this->count_days($bean->date_finish, $bean->fetched_row['date_finish'] ?? ''); //Gets the difference in days

            if ($tasks) {
                foreach ($tasks as $task) { //loop through all dependant tasks

                    $rel_type = $task->relationship_type;//Determine their dependency type

                    if ($rel_type == 'FS') {//if its a Finish to start
                        //Modify the task's start and end date dependant on the difference in days
                        $start = new DateTime($task->date_start);
                        $start = $start->modify($diff);
                        $startdate = $start->format('Y-m-d');

                        $duration = $task->duration - 1;//take one off to maintain correct gantt bar length

                        $enddate = $start->modify('+' . $duration . ' days');
                        $enddate = $enddate->format('Y-m-d');

                        $task->date_start = $startdate;
                        $task->date_finish = $enddate;
                        $task->save();
                    } else {
                        if ($rel_type == 'SS') {//if its a start to start
                            //check if the tasks duration has not been changed so that it does not update when the parent tasks duration is changed
                            if ($bean->fetched_row['duration'] == $bean->duration) {
                                $start = new DateTime($task->date_start);
                                $start = $start->modify($diff);
                                $startdate = $start->format('Y-m-d');

                                $duration = $task->duration - 1;

                                $enddate = $start->modify('+' . $duration . ' days');
                                $enddate = $enddate->format('Y-m-d');

                                $task->date_start = $startdate;
                                $task->date_finish = $enddate;
                                $task->save();
                            }
                        }
                    }
                }
            }
        }
    }

    //Gets the difference in days between two dates
    public function count_days($start_date, $end_date)
    {
        $d1 = new DateTime($start_date);
        $d2 = new DateTime($end_date);
        $difference = $d1->diff($d2);
        if ($difference->invert == 1) {
            return '+'.$difference->d.' days'; //returns positive days
        } else {
            return -$difference->d.' days';//returns negative days
        }
    }
}
