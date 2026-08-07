<?php
/**
 * SuiteCRM is a customer relationship management program developed by SuiteCRM Ltd.
 * Copyright (C) 2014 - 2026 SuiteCRM Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUITECRM, SUITECRM DISCLAIMS THE
 * WARRANTY OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License
 * version 3, these Appropriate Legal Notices must retain the display of the
 * "Supercharged by SuiteCRM" logo. If the display of the logos is not reasonably
 * feasible for technical reasons, the Appropriate Legal Notices must display
 * the words "Supercharged by SuiteCRM".
 */

#[\AllowDynamicProperties]
class ProjectTable
{
    private $tasks;

    public function __construct($tasks)
    {
        $this->tasks = $tasks;
        //draw the grid
        $this->draw($this->tasks);
    }

    public function draw($tasks)
    {
        global $mod_strings, $app_list_strings;

        // Instantiate the TimeDate Class
        $timeDate = new TimeDate();

        echo '<table id="Task_table" class="project_table_header">
                <tr class="disable_sort">
                    <td style="min-width:32px;" class="project_table_headings">'.$mod_strings['LBL_TASK_ID'].'</td>
                    <td style="min-width:85px;" class="project_table_headings">'.$mod_strings['LBL_TASK_NAME'].'</td>
                    <td style="min-width:100px;" class="project_table_headings">'.$mod_strings['LBL_PREDECESSORS'].'</td>
                    <td style="min-width:100px;" class="project_table_headings">'.$mod_strings['LBL_START'].'</td>
                    <td style="min-width:100px;" class="project_table_headings">'.$mod_strings['LBL_FINISH'].'</td>
                    <td style="min-width:100px;" class="project_table_headings">'.$mod_strings['LBL_DURATION'].'</td>
                    <td style="min-width:120px;" class="project_table_headings">'.$mod_strings['LBL_ASSIGNED_USER_ID'].'</td>
                    <td style="min-width:48px;" class="project_table_headings">'.$mod_strings['LBL_PERCENT_COMPLETE'].'</td>
                    <td style="min-width:80px;" class="project_table_headings">'.$mod_strings['LBL_MILESTONE_FLAG'].'</td>
                    <td style="min-width:80px;" class="project_table_headings">'.$mod_strings['LBL_ACTUAL_DURATION'].'</td>
                    <td style="min-width:30px;" class="project_table_headings"></td>
                </tr>
        ';

        $task_count = 0;

        if (!is_null($tasks)) {
            foreach ($tasks as $task) {
                //Get resources
                $project = BeanFactory::newBean('Project');
                $project->retrieve($task->project_id);
                //Get project resources (users & contacts)
                $resources1 = $project->get_linked_beans('project_users_1', 'User');
                $resources2 = $project->get_linked_beans('project_contacts_1', 'Contact');
                //Combine resources into array of objects
                $resource_array = array();
                foreach ($resources1 as $user) {
                    $resource = new stdClass;
                    $resource->id = $user->id;
                    $resource->name = $user->name;
                    $resource->type = 'user';
                    $resource_array[] = $resource;
                }
                foreach ($resources2 as $contact) {
                    $resource = new stdClass;
                    $resource->id = $contact->id;
                    $resource->name = $contact->name;
                    $resource->type = 'contact';
                    $resource_array[] = $resource;
                }

                echo '<tr class="row_sortable">
                        <td class="project_table_cells"><input class="order_number" name="order_number[]" rel="'.$task->id.'" type="hidden" value="'.$task->order_number.'" />'.$task->project_task_id.'</td>';
                strlen($task->name) < 51 ? $taskName = $task->name : $taskName = substr($task->name, 0,49).'...';
                if (ACLController::checkAccess('Project', 'edit', true)) {
                    echo '<td class="project_table_cells" ><span><a data = "'.$task->id.','.$task->predecessors.','.$task->relationship_type.','.$timeDate->to_display_date($task->date_start, true).','.$task->duration.','.$task->duration_unit.','.$task->assigned_user_id.','.$task->milestone_flag.','.$task->percent_complete.','.$task->description.','.$task->actual_duration.'" onclick = "edit_task($(this));" title = "'.$mod_strings['LBL_TASK_TITLE'].'" href = "#" >'.$taskName.'</a ></span></td>';
                } else {
                    echo '<td class="project_table_cells" ><span>'.$taskName.'</span ></td>';
                }
                $prName = '';
                foreach ($tasks as $predecessor) {
                    if ($predecessor->project_task_id==$task->predecessors) {
                        echo $predecessor->name;
                        $prName .= $predecessor->name.'<br>';
                    }
                }
                echo '<td class="project_table_cells"><div class="tooltip-gantt-wrap">';
                echo 'Hover to open......';
                echo '<div class="tooltip-gantt-content">';
                echo $prName;
                echo '
                          </div></div>
                        </td>
                        <td class="project_table_cells">'
                            .$timeDate->to_display_date($task->date_start, true).
                        '</td>
                        <td class="project_table_cells">'
                            .$timeDate->to_display_date($task->date_finish, true).
                        '</td>
                        <td class="project_table_cells">'
                            .$task->duration.' '.$app_list_strings['duration_unit_dom'][$task->duration_unit].
                        '</td>
                        <td style="min-width:105px;" class="project_table_cells" >';
                $rflag = '0';
                foreach ($resource_array as $resource) {
                    if ($resource->id == $task->assigned_user_id) {
                        if ($resource->type == 'user') {
                            echo '<a target="blank" href="index.php?module=Users&action=DetailView&record='.$resource->id.'">'.$resource->name.'</a>';
                            $rflag = '1';
                        } elseif ($resource->type == 'contact') {
                            echo '<a target="blank" href="index.php?module=Contacts&action=DetailView&record='.$resource->id.'">'.$resource->name.'</a>';
                            $rflag = '1';
                        }
                    }
                }

                if ($rflag == '0') {
                    echo $mod_strings['LBL_UNASSIGNED'];
                }

                if ($task->milestone_flag == '1') {
                    $checked = $app_list_strings['checkbox_dom']['1'];
                } else {
                    $checked = $app_list_strings['checkbox_dom']['2'];
                }
                echo '</td>
                        <td class="project_table_cells">'.$task->percent_complete.'</td>
                        <td class="project_table_cells">'.$checked.'</td>
                        <td class="project_table_cells">'.$task->actual_duration.'</td>
                        <td class="project_table_cells">
                            <span id="exportToPDFSpan">';

                if (ACLController::checkAccess('Project', 'delete', true)) {
                    echo '<button style = "height:20px;width:20px;" class="remove_button" value = "'.$task->id.'" class="gantt_button" > '.$mod_strings["LBL_DELETE_TASK"].' </button >';
                } else {
                    echo '<button disabled="disabled" style = "height:20px;width:20px;" class="remove_button" value = "'.$task->id.'" class="gantt_button" > '.$mod_strings["LBL_DELETE_TASK"].' </button >';
                }
                echo '</span>
                         </td>
                    </tr>';

                $task_count++;
            }
        }

        echo '</table>';
    }

    // Function for basic field validation (present and neither empty nor only white space
    public function IsNullOrEmptyString($question)
    {
        return (!isset($question) || trim($question)==='');
    }
}
