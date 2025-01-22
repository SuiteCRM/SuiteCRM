<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}
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
 * @Package Project templates
 * @copyright Andrew Mclaughlan 2014
 * @author Andrew Mclaughlan <andrew@mclaughlan.info>
 */


#[\AllowDynamicProperties]
class AM_ProjectTemplatesViewDetail extends ViewDetail
{
    public function __construct()
    {
        parent::__construct();
    }




    public function display()
    {
        global $app_strings, $mod_strings;
        parent::display();

        echo '<link rel="stylesheet" type="text/css" href="modules/Project/qtip/jquery.qtip.min.css" />';
        //echo '<script type="text/javascript" src="modules/Project/js/jquery.blockUI.js"></script>';
        echo '<script type="text/javascript" src="modules/Project/qtip/jquery.qtip.min.js"></script>';
      
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
              </div>';
    }
}
