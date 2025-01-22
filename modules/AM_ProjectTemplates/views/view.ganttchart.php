<?php
/**
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU AFFERO GENERAL PUBLIC LICENSE as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU AFFERO GENERAL PUBLIC LICENSE
 * along with this program; if not, see http://www.gnu.org/licenses
 * or write to the Free Software Foundation,Inc., 51 Franklin Street,
 * Fifth Floor, Boston, MA 02110-1301  USA
 * @Package Gantt chart
 * @copyright Andrew Mclaughlan 2014
 * @author Andrew Mclaughlan <andrew@mclaughlan.info>
 */

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

#[\AllowDynamicProperties]
class AM_ProjectTemplatesViewGanttChart extends ViewDetail
{
    public function display()
    {
        global $mod_strings, $app_list_strings;
        $db = DBManagerFactory::getInstance();

        echo '<link rel="stylesheet" type="text/css" href="modules/AM_ProjectTemplates/css/style.css" />';
        echo '<link rel="stylesheet" type="text/css" href="modules/AM_ProjectTemplates/qtip/jquery.qtip.min.css" />';
        echo '<script type="text/javascript" src="modules/AM_ProjectTemplates/js/splitter.js"></script>';
        echo '<script type="text/javascript" src="modules/AM_ProjectTemplates/js/jquery.blockUI.js"></script>';
        echo '<script type="text/javascript" src="modules/AM_ProjectTemplates/js/jquery.validate.min.js"></script>';
        echo '<script type="text/javascript" src="modules/AM_ProjectTemplates/js/main_lib.js"></script>';
        echo '<script type="text/javascript" src="include/javascript/qtip/jquery.qtip.min.js"></script>';


        $project_template = BeanFactory::newBean('AM_ProjectTemplates');

        if (!isset($_REQUEST["record"]) || trim($_REQUEST["record"]) == "") {
            $_REQUEST["record"] = $_REQUEST["project_id"];
        }

        $project_template->retrieve($_REQUEST["record"]);
        //Get project_template resources (users & contacts)
        $resources1 = $project_template->get_linked_beans('am_projecttemplates_users_1', 'User');
        $resources2 = $project_template->get_linked_beans('am_projecttemplates_contacts_1', 'Contact');
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


        //Get the start and end date of the project in database format
        $start_date =  Date('Y-m-d');
        $end_date = Date('Y-m-d', strtotime("+30 days"));

        parent::display(); ?>
        <!--Create task pop-up-->
        <div style="display: none;">
            <div id="template_dialog"  title="<?php echo $mod_strings['LBL_ADD_NEW_TASK']; ?>">
                <p>
                    <?php echo $mod_strings['LBL_EDIT_TASK_PROPERTIES']; ?>
                </p>
                <form id="popup_form">
                    <fieldset>
						<table width="100%">
							<tr><td width="50%">
						
							<input type="hidden" name="project_template_id" id="project_template_id" value="<?php echo $project_template->id; ?>">
							<input type="hidden" name="override_business_hours" id="override_business_hours" value="<?php echo $project_template->override_business_hours; ?>">
							<input type="hidden" name="Start" id="Start" value="">
							<input type="hidden" name="Actual_duration" id="Actual_duration" value="0">
							
							<input type="text" style="display: none;" name="task_id" id="task_id" value="">
							<input type="radio" name="Milestone" value="Subtask" checked="checked" id="Subtask" />
							<label id="Subtask_label" for="Subtask"><?php echo $mod_strings['LBL_SUBTASK']; ?></label>
							<input type="radio" name="Milestone" value="Milestone" id="Milestone" />
							<label id="Milestone_label" for="Milestone"><?php echo $mod_strings['LBL_MILESTONE_FLAG']; ?></label>&nbsp;<br /><br />
							<label id="parent_task_id" for="parent_task" style="display: none;"><?php echo isset($mod_strings['LBL_PARENT_TASK_ID']) ? $mod_strings['LBL_PARENT_TASK_ID'] : ''; ?></label>
							<input id="parent_task" class="text ui-widget-content ui-corner-all" style="display: none;" type="text" name="parent_task" value="" />
							<label for="task_name"><?php echo $mod_strings['LBL_TASK_NAME']; ?></label>
							<input type="text" name="task_name" id="task_name" class="text ui-widget-content ui-corner-all" />
							<label for="Predecessor"><?php echo $mod_strings['LBL_PREDECESSORS']; ?></label>
							<?php
                            echo '<select id="Predecessor" name="Predecessor" class="text ui-widget-content ui-corner-all" />';
        if (!isset($tasks)) {
            $tasks = [];
        }
        foreach ($tasks as $task) {
            echo '<option rel="'.$task->id.'" value="'.$task->order_number.'">'.$task->name.'</opion>';
        }
        echo '</select>'; ?>
							<label for="relation_type"><?php echo $mod_strings['LBL_RELATIONSHIP_TYPE']; ?></label>
							<?php
                            echo '<select id="relation_type" name="relation_type" class="text ui-widget-content ui-corner-all">
									'.get_select_options_with_id($app_list_strings['relationship_type_list'], '').'
							</select>'; ?>

					
						</td><td width="50%"> 

							<label for="Duration"><?php echo $mod_strings['LBL_DURATION_TITLE']; ?></label>
							<input type="text" name="Duration" id="Duration" class="text ui-widget-content ui-corner-all" />
							<select id="Duration_unit" name="Duration_unit" class="text ui-widget-content ui-corner-all" />
								<option value="Days">Days</option>
							</select>

							<label for="Resources"><?php echo $mod_strings['LBL_ASSIGNED_USER_ID']; ?></label>
							<?php
                            echo '<select id="Resources" name="Resources" class="text ui-widget-content ui-corner-all" />';
        echo '<option value="0">'.$mod_strings['LBL_UNASSIGNED'].'</option>';
        foreach ($resource_array as $resource) {
            echo '<option rel="'.$resource->type.'" value="'.$resource->id.'">'.$resource->name.'</opion>';
        }
        echo '</select>'; ?>
							<label for="%Complete"><?php echo $mod_strings['LBL_PERCENT_COMPLETE']; ?></label>
							<input type="text" name="Complete" id="Complete" value="0" class="text ui-widget-content ui-corner-all" />
							<input type="hidden" name="Notes" id="Notes" />
							<!--label for="Notes"><?php echo $mod_strings['LBL_DESCRIPTION']; ?></label>
							<textarea id="Notes" cols="34" name="Notes" class="text ui-widget-content ui-corner-all"></textarea-->
						</td>
						</tr>
						</table>
					</fieldset>
                </form>
            </div>
            <!--Delete task pop-up-->
            <div id="delete_dialog" title="<?php echo $mod_strings['LBL_DELETE_TASK']; ?>">
                <p>
                    Are you sure you want to delete this task?
                </p>
            </div>
        </div>
        <!-- Pop-up End -->

<?php


       echo '<style>
                    .p_form { font-size: 62.5%; }
                    .p_form label, .p_form input { display:block; }
                    .p_form input.text { margin-bottom:12px; width:95%; padding: .4em; }
                    .p_form fieldset { padding:0; border:0; margin-top:25px; }
                    .p_form h1 { font-size: 1.2em; margin: .6em 0; }
                    .ui-dialog .ui-state-error { padding: .3em; }
                    .validateTips { border: 1px solid transparent; padding: 0.3em; }
                </style>';

        echo '<div style="display: none;" id="dialog-confirm" title="'.$mod_strings['LBL_CREATE_PROJECT_TITLE'].'">
                 <p class="validateTips"></p>
                <p class="p_form">
                     <form id="project_form" name="project_form" action="index.php?module=AM_ProjectTemplates&action=create_project" method="post">
                        <fieldset style="border: none;">
                             <label for="name">'.$mod_strings['LBL_PROJECT_NAME'].':<span class="required">*</span></label>
                             <input style="margin-bottom:12px; width:95%; padding: .4em;" type="text" name="p_name" id="p_name" class="text ui-widget-content ui-corner-all" />

                             <label for="start_date">'.$mod_strings['LBL_START_DATE'].':</label>
                             <input style="margin-bottom:12px; width:95%; padding: .4em;" type="text" name="start_date" id="start_date" class="text ui-widget-content ui-corner-all" />

                             <script type="text/javascript">
                                var now = new Date();
                                Calendar.setup ({
                                    inputField : "start_date",
                                    ifFormat : cal_date_format,
                                    daFormat : "%m/%d/%Y %I:%M%P",
                                    button : "start_date",
                                    singleClick : true,
                                    step : 1,
                                    weekNumbers: false,
                                    startWeekday: 0
                                });
                                addForm("project_form");
                                addToValidate("project_form", "p_name", "name", true,"'.$mod_strings['LBL_PROJECT_NAME'].'" );
                                addToValidate("project_form", "start_date", "date", false,"'.$mod_strings['LBL_START_DATE'].'" );
                            </script>
							 <label for="copy_all_tasks">'.$mod_strings['LBL_COPY_ALL_TASKS'].':</label>&nbsp;
                             <input type="checkbox" style="position: relative; vertical-align:middle" id="copy_all_tasks" name="copy_all_tasks" value="1" title="" />&nbsp;
							 <span style="position: relative;"  id="copy_all_tasks_help"><span class="suitepicon suitepicon-action-info"></span>
							 </span>
							<script type="text/javascript">

									var help = $("#copy_all_tasks_help");
									//set tooltip title
									var title = "' . $mod_strings['LBL_TOOLTIP_TITLE'] . '" ;
									var text = "' . $mod_strings['LBL_TOOLTIP_TEXT'] . '" ;
									//console.log(title);

									help.qtip({
										content: {
											text: text,
											title: {
												//button: true,
												text: title
											}
										},
										position: {
											my: "bottom center",
											at: "top center",
											target: "mouse",
											adjust: {
												mouse: false,
												scroll: false,
												y: -10
											}
										},
										show: {
											event: "mouseover"
										},
										hide: {
											event: "mouseout"
										},
										style: {
											classes : "qtip-green qtip-shadow qtip_box", //qtip-rounded"
											tip: {
												offset: 10

											}
										}
									});

									//help.qtip("disable");

							</script>
                             <label for="tasks" id="tasks_label">'.$mod_strings['LBL_COPY_SEL_TASKS'].':</label>
                             <select id="tasks" name="tasks[]" multiple style="margin-bottom:12px; width:95%; padding: .4em;" >';
                                
        $this->bean->load_relationship('am_tasktemplates_am_projecttemplates');
        $task_list = $this->bean->get_linked_beans('am_tasktemplates_am_projecttemplates', 'AM_TaskTemplates');

        //From the query above, populates the select box
        foreach ($task_list as $task) {
            echo '<option value="'.$task->id.'">'.$task->name.'</option>';
        }

        echo '</select><br />

							 <input type="hidden" name="template_id" value="'.$this->bean->id .'" />

                        </fieldset>
                     </form>
                </p>
              </div>'; ?>

        <!--Mark-up for the main body of the view-->
        
        <div id="wrapper" >

			<?php
                if (ACLController::checkAccess('AM_ProjectTemplates', 'edit', true)) {
                    echo '<div style="clear:both;padding:10px;"><button id="add_button" class="gantt_button">' . $mod_strings['LBL_ADD_NEW_TASK'] . '</button></div>';
                    echo '<input id="is_editable" name="is_editable" type="hidden" value="1" >';
                } ?>
            <input id="record" type="hidden" name="record" value="<?php echo $_REQUEST["record"]; ?>" />
            <div id="project_wrapper">

            </div>
        </div>
        <!--Main body end-->
<?php
    }

    //Returns the time span between two dates in years  months and days
    public function time_range($start_date, $end_date)
    {
        global $mod_strings;

        $datetime1 = new DateTime($start_date);
        $datetime2 = new DateTime($end_date);
        $datetime2->add(new DateInterval('P1D')); //Add 1 day to include the end date as a day
        $interval = $datetime1->diff($datetime2);
        return $interval->format('%m '.$mod_strings['LBL_MONTHS'].', %d '.$mod_strings['LBL_DAYS']);
    }
}
