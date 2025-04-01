<?php
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

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

#[\AllowDynamicProperties]
class chart
{
    private $start_date;
    private $end_date;
    private $tasks;
    private $projects;
    private $users;
    private $contacts;
    private $chart_type;

    public function __construct($start_date, $end_date, $projects, $users, $contacts, $tasks, $chart_type)
    {
        $this->start_date = $start_date;
        $this->end_date = $end_date;
        $this->tasks = $tasks;
        $this->projects = $projects;
        $this->users = $users;
        $this->contacts = $contacts;
        $this->chart_type = $chart_type;

        //draw the grid
        $this->draw($this->start_date, $this->end_date, $this->projects, $this->users, $this->contacts, $this->tasks, $this->chart_type);
    }

    public function draw($start_date, $end_date, $sel_projects, $sel_users, $sel_contacts, $resources, $chart_type)
    {
        global $current_user, $mod_strings;
        $db = DBManagerFactory::getInstance();

        if ($chart_type == "monthly") {
            list($time_span, $day_count) = $this->year_week($start_date, $end_date);
        } elseif ($chart_type == "quarterly") {
            list($time_span, $day_count) = $this->year_quarter($start_date, $end_date);
        } else {
            $time_span = $this->year_month($start_date, $end_date);
            $day_count = $this->count_days($start_date, $end_date) + 1;
        }



        $weeks = $this->get_weeks($start_date, $end_date);


        //Get projects. This is for the Select box values
        $projects_query = "SELECT DISTINCT id, name FROM project WHERE deleted =0";
        $projects_list = $db->query($projects_query);

        $project_list = array();
        while ($row = $db->fetchByAssoc($projects_list)) {
            //create array of user objects
            $project_list[] = (object)$row;
        }

        //Get users that are associated to any project. This is for the Select box values
        $users_query = "SELECT DISTINCT project_users_1users_idb AS id, first_name, last_name, 'project_users_1_c' AS
                                    type
                                    FROM project_users_1_c
                                    JOIN users ON users.id = project_users_1users_idb
                                    WHERE project_users_1_c.deleted =0";
        $users_list = $db->query($users_query);

        $user_list = array();
        while ($row = $db->fetchByAssoc($users_list)) {
            //create array of user objects
            $user_list[] = (object)$row;
        }

        //Get contacts that are associated to any project. This is for the Select box values
        $contacts_query = "SELECT DISTINCT project_contacts_1contacts_idb AS id, first_name, last_name, 'project_contacts_1_c' AS
                                    type
                                    FROM project_contacts_1_c
                                    JOIN contacts ON contacts.id = project_contacts_1contacts_idb
                                    WHERE project_contacts_1_c.deleted =0";
        $contacts_list = $db->query($contacts_query);



        $contact_list = array();
        while ($row = $db->fetchByAssoc($contacts_list)) {
            //create array of user objects
            $contact_list[] = (object)$row;
        }

        //Generate main table and the first row containing the months

        echo '<div class="moduleTitle"><h2> ' . $mod_strings["LBL_RESOURCE_CHART"] . ' </h2></div>
              <table id="header_table_chart" border="0" cellpadding="0" cellspacing="0" width="100%">
                  <tr>
                      <td scope="row_label" nowrap="nowrap" >
                          <label for="projects">'.$mod_strings["LBL_PROJECTS_SEARCH"].'</label>
                      </td>
                      <td scope="row_val" nowrap="nowrap" >
                          <select id="projects" name="projects" multiple size="6" style="width: 250px" >
                          <option value="">'.$mod_strings["LBL_ALL_PROJECTS"].'</option>';

        //From the query above, populates the select box
        foreach ($project_list as $project) {
            if (in_array($project->id, $sel_projects)) {//Check if the select box option matches the resource passed in.
                $selected = "selected='selected'"; //if so set it to selected
            } else {
                $selected = "";
            }

            echo '<option '.$selected.'  value="'.$project->id.'">'.$project->name.'</option>';
        }

        echo '</select><br /><br />';

        if (empty($project_list)) {
            echo '<span style="color: red;">'.$mod_strings['LBL_RESOURCE_CHART_WARNING'].'</span><br /><br />';
        }

        echo '</td>
              <td scope="row_label" nowrap="nowrap" >
                  <label for="users">'.$mod_strings["LBL_USERS_SEARCH"].'</label>
              </td>
              <td scope="row_val" nowrap="nowrap" >
                  <select id="users" name="users" multiple size="6" style="width: 250px" >
                  <option ' . ($sel_users[0] == ''? "selected='selected'" : "") . ' value="">'.$mod_strings['LBL_ALL_USERS'].'</option>
                  <option ' . ($sel_users[0] == 'none'? "selected='selected'" : "") . ' value="none">None</option>';

        //From the query above, populates the select box
        foreach ($user_list as $user) {
            $user_obj = BeanFactory::newBean('Users');
            $user_obj->retrieve($user->id);
            var_dump($user_obj->id);
            if (in_array($user->id, $sel_users)) {//Check if the select box option matches the resource passed in.
                $selected = "selected='selected'"; //if so set it to selected
            } else {
                $selected = "";
            }
            echo '<option '.$selected.' data-type="'.$user->type.'" value="'.$user->id.'">'. $user_obj->full_name .'</option>'; //$user->last_name
        }

        echo '</select><br /><br />';

        if (empty($user_list)) {
            echo '<span style="color: red;">'.$mod_strings['LBL_RESOURCE_CHART_WARNING'].'</span><br /><br />';
        }

        echo '</td>
              <td scope="row_label" nowrap="nowrap" >
                  <label for="contacts">'.$mod_strings["LBL_CONTACTS_SEARCH"].'</label>
              </td>
              <td scope="row_val" nowrap="nowrap" >
                  <select id="contacts" name="contacts" multiple size="6" style="width: 250px" >
                  <option ' . ($sel_contacts[0] == ''? "selected='selected'" : "") . ' value="">'.$mod_strings['LBL_ALL_CONTACTS'].'</option>
                  <option ' . ($sel_contacts[0] == 'none'? "selected='selected'" : "") . ' value="none">None</option>';

        //From the query above, populates the select box
        foreach ($contact_list as $contact) {
            $contact_obj = BeanFactory::newBean('Contacts');
            $contact_obj->retrieve($contact->id);

            if (in_array($contact->id, $sel_contacts)) {//Check if the select box option matches the resource passed in.
                $selected = "selected='selected'"; //if so set it to selected
            } else {
                $selected = "";
            }

            echo '<option '.$selected.' data-type="'.$contact->type.'" value="'.$contact->id.'">'.$contact_obj->full_name.'</option>';
        }

        echo '</select><br /><br />';

        if (empty($contact_list)) {
            echo '<span style="color: red;">'.$mod_strings['LBL_RESOURCE_CHART_WARNING'].'</span><br /><br />';
        }


        echo '</td></tr>';
        echo '<tr>
                  <td scope="row_label" nowrap="nowrap" >
                      <label for="chart_type">'.$mod_strings["LBL_CHART_TYPE"].'</label>
                  </td>
                  <td scope="row_val" nowrap="nowrap" >
                      <select id="chart_type" name="chart_type" style="width: 250px">';
        echo '<option '. ($chart_type == "weekly" ? "selected" : "") .'  value="weekly">'.$mod_strings['LBL_CHART_WEEKLY'].'</option>';
        echo '<option '. ($chart_type == "monthly" ? "selected" : "") .'  value="monthly">'.$mod_strings['LBL_CHART_MONTHLY'].'</option>';
        echo '<option '. ($chart_type == "quarterly" ? "selected" : "") .'  value="quarterly">'.$mod_strings['LBL_CHART_QUARTERLY'].'</option>';
        echo '</select><br /><br />';
        echo '</td>';

            
        echo '<td scope="row_label" nowrap="nowrap" >
                  <label for="field_chart">'.$mod_strings["LBL_DATE_START"].'</label>
              </td>
              <td scope="row_val" nowrap="nowrap" >
                  <input id="date_start" type="text" name="date_start" value="'.$start_date.'" size=8 readonly/>
                  <span id="date_start_trigger" class="suitepicon suitepicon-module-calendar"></span>
              </td>
              <td scope="row_label" nowrap="nowrap" >
                  <label for="field_chart">&nbsp;</label>
              </td>
              <td scope="row_val" nowrap="nowrap" >
                  <input id="date_end" class="date_chart" type="hidden" name="date_end" value="'.$end_date.'" />
              </td>';

        echo '<script type="text/javascript">
                var now = new Date();
                Calendar.setup 
				({
                  inputField : "date_start",
                    ifFormat : cal_date_format,
                    daFormat : "%m/%d/%Y %I:%M%P",
                    button : "date_start_trigger",
                    singleClick : true,
                    step : 1,
                    weekNumbers: false,
                    startWeekday: 0
                    });
              </script>';

        echo '</tr>
              <tr>
                  <td style="padding:5px;">                             
                      &nbsp;<a class="utilsLink" href="#" id="create_link">'.$mod_strings['LBL_RESOURCE_CHART_SEARCH_BUTTON'].'</a>
                  </td>
              </tr>
          </table>';




        echo '<table class="main_table">
                  <tr class="select_row">
                      <td colspan="100%">
                      </td>
                  </tr>    
                  <tr>
                      <td colspan="100%">
                          <table style="border: none; table-layout: fixed; width: 100%;" class="main_table" width="100%" cellspacing="0" cellpadding="0" border="0">
                              <tr>
                                  <td class="top_row" style="text-align: left; width: 20%;"><a id="prev_month" href="index.php?module=Project&action=ResourceList"><img width="6" height="10" border="0" align="absmiddle" alt="Previous Month" src="themes/default/images/calendar_previous.gif"> '.$mod_strings['LBL_RESOURCE_CHART_PREVIOUS_MONTH'].'</a></td>
                                  <td class="top_row" style="text-align: center; width: 60%;"><span class="heading_chart">'.$mod_strings['LBL_RESOURCE_CHART'].'</span></td>
                                  <td class="top_row" style="padding-right:5px; text-align: right; width: 20%;"><a id="next_month" href="index.php?module=Project&action=ResourceList">'.$mod_strings['LBL_RESOURCE_CHART_NEXT_MONTH'].' <img width="6" height="10" border="0" align="absmiddle" alt="Next Month" src="themes/default/images/calendar_next.gif"></a></td>
                              </tr>
                          </table>
                      </td>
                  </tr>';
        echo '<tr>';

        //weekly view
        if ($chart_type == "weekly" || $chart_type == "") {
            echo '<td class="main_table week">'.$mod_strings['LBL_RESOURCE_CHART_WEEK'].'</td>';
            foreach ($weeks as $week) {
                echo '<td class="main_table weeks" colspan="7">'.$week.'</td>';
            }

            echo '</tr><tr><td rowspan="3" class="main_table day">'.$mod_strings['LBL_RESOURCE_CHART_DAY'].'</td>';
            foreach ($time_span as $year => $months) {
                foreach ($months as $month => $days) {//count the number of days in each month
                    
                    $daycount=0;
                    foreach ($days as $day) {
                        $daycount++;
                    }
                    $width = $daycount * 26; //used to set width on years row. width needed for css text clipping
                    echo '<td colspan="'.$daycount.'" class="main_table years"><div style="width: '.$width.'px;" class="year_div">'.$month.', '.$year.'</div></td>';
                }
            }
            echo '</tr><tr class="days_row">';

            $month_count = 0;//start month count
            foreach ($time_span as $year => $months) {
                foreach ($months as $days) {
                    foreach ($days as $day => $d) {
                        echo '<td class="inner_td"><div class="cell_width">'.$day.'</div></td>';//day number shown
                    }
                }
            }
            echo '</tr><tr class="days_row">';

            foreach ($time_span as $year => $months) {
                foreach ($months as $days) {
                    foreach ($days as $day => $d) {
                        echo '<td class="inner_td"><div class="cell_width">'.$d.'</div></td>';//First letter of the days name shown
                    }
                }
            }

            echo '</tr>';

            foreach ($resources as $resource) {
                $count = $resource->task_count;

                if ($resource->type == 'project_users_1_c') {
                    $user_obj = BeanFactory::newBean('Users');
                    $user_obj->retrieve($resource->id);

                    echo '<tr id="'.$resource->id.'" class="task_row"><td no class="main_table no_wrap"><a title="'.$mod_strings["LBL_RESOURCE_TYPE_TITLE_USER"].'" href="index.php?module=Users&action=DetailView&record='.$resource->id.'">'.$user_obj->full_name.'</a></td>';
                } elseif ($resource->type == 'project_contacts_1_c') {
                    $contact_obj = BeanFactory::newBean('Contacts');
                    $contact_obj->retrieve($resource->id);

                    echo '<tr id="'.$resource->id.'" class="task_row"><td no class="main_table no_wrap"><a title="'.$mod_strings["LBL_RESOURCE_TYPE_TITLE_CONTACT"].'" href="index.php?module=Contacts&action=DetailView&record='.$resource->id.'">'.$contact_obj->full_name.'</a></td>';
                }

                $i=0;
                for ($x=0; $x< $day_count; $x++) {
                    $dateq = $this->get_date($start_date, $i);

                    $class = '';

                    if ($this->check_weekend($dateq) == 'today') {
                        $class = 'today';
                    } elseif ($this->check_weekend($dateq) == 'weekend') {
                        $class = 'weekend';
                    } elseif ($this->check_weekend($dateq) == 'weekend-today') {
                        $class = 'weekend-today';
                    }
                    $square = '';
                    $dup = 0;

                    for ($c=0; $c < $count; $c++) {
                        if ($x == $resource->tasks[$c]['start_day']) {
                            $dup++;
                            $square =  '<td class="inner_td"><div style="color: #ffffff;" rel="'.$dateq.'|'.$dateq.'|'.$resource->id.'|'.$resource->type.'" class="cell_width day_block '.$class.' ' . $this->get_cell_class($dup) .'"></div></td>';
                        } elseif ($x > $resource->tasks[$c]['start_day'] && $x <= $resource->tasks[$c]['end_day']) {
                            $dup++;
                            $square =  '<td class="inner_td"><div rel="'.$dateq.'|'.$dateq.'|'.$resource->id.'|'.$resource->type.'" class="cell_width day_block '.$class.' ' . $this->get_cell_class($dup) .'"></div></td>';
                        }
                    }

                    if ($square == '') {
                        $square = '<td class="inner_td"><div class="cell_width day_block"><div class="'.$class.'"></div></div></td>';
                    }
                    echo $square;
                    $i++;
                }
            }
        }//end weekly view

        elseif ($chart_type == "monthly") {
            echo '<td class="main_table week">'.$mod_strings['LBL_RESOURCE_CHART_MONTH'].'</td>';
            /*foreach ($weeks as $week)
            {
                echo '<td class="main_table weeks" colspan="7">'.$week.'</td>';
            }*/

            foreach ($time_span as $year => $months) {
                foreach ($months as $month => $weeks) {//count the number of days in each month

                    echo '<td class="main_table weeks" colspan="' . (is_countable($weeks) ? count($weeks) : 0) . '">'.$month .'</td>';
                }
            }

            echo '</tr><tr><td rowspan="3" class="main_table day">'.$mod_strings['LBL_RESOURCE_CHART_WEEK'].'</td>';
            foreach ($time_span as $year => $months) {
                $wcount= 0;
                foreach ($months as $month => $weeks) {//count the number of weeks in each month
                    /*foreach ($weeks as $week)
                    {
                        $wcount++;
                    }*/
                    $wcount+= is_countable($weeks) ? count($weeks) : 0;
                }
                $width = $wcount * 26; //used to set width on years row. width needed for css text clipping
                echo '<td colspan="'.$wcount.'" class="main_table years"><div style="width: '.$width.'px;" class="year_div">' . $year.'</div></td>';
            }
            echo '</tr><tr class="days_row">';

            $month_count = 0;//start month count
            foreach ($time_span as $year => $months) {
                foreach ($months as $weeks) {
                    foreach ($weeks as $week => $w) {
                        echo '<td class="inner_td"><div class="cell_width">'.$w.'</div></td>';//day number shown
                    }
                }
            }

            echo '</tr><tr class="days_row">';

            foreach ($time_span as $year => $months) {
                foreach ($months as $weeks) {
                    foreach ($weeks as $week => $d) {
                        echo '<td class="inner_td"><div class="cell_width">'. ($week + 1) .'</div></td>';//First letter of the days name shown
                    }
                }
            }

            echo '</tr>';

                foreach ($resources as $resource) {
                    $count = $resource->task_count;

                if ($resource->type == 'project_users_1_c') {
                    $user_obj = BeanFactory::newBean('Users');
                    $user_obj->retrieve($resource->id);

                    echo '<tr id="'.$resource->id.'" class="task_row"><td no class="main_table no_wrap"><a title="'.$mod_strings["LBL_RESOURCE_TYPE_TITLE_USER"].'" href="index.php?module=Users&action=DetailView&record='.$resource->id.'">'.$user_obj->full_name.'</a></td>';
                } elseif ($resource->type == 'project_contacts_1_c') {
                    $contact_obj = BeanFactory::newBean('Contacts');
                    $contact_obj->retrieve($resource->id);

                    echo '<tr id="'.$resource->id.'" class="task_row"><td no class="main_table no_wrap"><a title="'.$mod_strings["LBL_RESOURCE_TYPE_TITLE_CONTACT"].'" href="index.php?module=Contacts&action=DetailView&record='.$resource->id.'">'.$contact_obj->full_name.'</a></td>';
                }

                $i=0;
                for ($x=0; $x< $day_count; $x++) {
                    //Get dates for each week
                    $dateq = $this->get_week_dates($start_date, $x);

                    $class = '';

                    $square = '';
                    $dup = 0;

                    for ($c=0; $c < $count; $c++) {
                        if ($x == floor($resource->tasks[$c]['start_day'] /7) && ($resource->tasks[$c]['start_day'] /7) > 0) {
                            $dup++;
                            $square =  '<td class="inner_td"><div style="color: #ffffff;"  rel="'.$dateq.'|'.$resource->id.'|'.$resource->type.'" class="cell_width day_block '.$class.' ' . $this->get_cell_class($dup) .'"></div></td>';
                        } elseif ($x > floor($resource->tasks[$c]['start_day']/7) && $x <= floor($resource->tasks[$c]['end_day']/7)) {
                            $dup++;
                            $square =  '<td class="inner_td"><div rel="'.$dateq.'|'.$resource->id.'|'.$resource->type.'" class="cell_width day_block '.$class.' ' . $this->get_cell_class($dup) .'"></div></td>';
                        }
                    }

                    if ($square == '') {
                        $square = '<td class="inner_td"><div class="cell_width day_block"><div class="'.$class.'"></div></div></td>';
                    }
                    echo $square;
                    $i++;
                }
            }
        }
        //end monthly view

        elseif ($chart_type == "quarterly") {
            echo '<td class="main_table week">'.$mod_strings['LBL_RESOURCE_CHART_QUARTER'].'</td>';
            foreach ($time_span as $year => $quarters) {
                foreach ($quarters as $quarter => $months) {//count the number of days in each month
                    
                    echo '<td class="main_table weeks" colspan="' . (is_countable($months) ? count($months) : 0) . '">'.$quarter .'</td>';
                }
            }

            echo '</tr><tr><td rowspan="3" class="main_table day">'.$mod_strings['LBL_RESOURCE_CHART_MONTH'].'</td>';
            foreach ($time_span as $year => $quarters) {
                $qcount= 0;
                foreach ($quarters as $quarter => $months) {//count the number of months in each quarter
                
                    $qcount+= is_countable($months) ? count($months) : 0;
                }
                $width = $qcount * 26; //used to set width on years row. width needed for css text clipping
                echo '<td colspan="'.$qcount.'" class="main_table years"><div style="width: '.$width.'px;" class="year_div">' . $year.'</div></td>';
            }
            echo '</tr><tr class="days_row">';

            $month_count = 0;//start month count
            foreach ($time_span as $year => $quarters) {
                foreach ($quarters as $quarter) {
                    foreach ($quarter as $month => $m) {
                        echo '<td class="inner_td"><div class="cell_width">'.$month.'</div></td>';//day number shown
                    }
                }
            }

            echo '</tr><tr class="days_row">';

            foreach ($time_span as $year => $quarters) {
                foreach ($quarters as $quarter) {
                    foreach ($quarter as $month => $m) {
                        echo '<td class="inner_td"><div class="cell_width">'. $m .'</div></td>';//First letter of the days name shown
                    }
                }
            }

            echo '</tr>';

            foreach ($resources as $resource) {
                $count = $resource->task_count;

                if ($resource->type == 'project_users_1_c') {
                    $user_obj = BeanFactory::newBean('Users');
                    $user_obj->retrieve($resource->id);

                    echo '<tr id="'.$resource->id.'" class="task_row"><td no class="main_table no_wrap"><a title="'.$mod_strings["LBL_RESOURCE_TYPE_TITLE_USER"].'" href="index.php?module=Users&action=DetailView&record='.$resource->id.'">'.$user_obj->full_name.'</a></td>';
                } elseif ($resource->type == 'project_contacts_1_c') {
                    $contact_obj = BeanFactory::newBean('Contacts');
                    $contact_obj->retrieve($resource->id);

                    echo '<tr id="'.$resource->id.'" class="task_row"><td no class="main_table no_wrap"><a title="'.$mod_strings["LBL_RESOURCE_TYPE_TITLE_CONTACT"].'" href="index.php?module=Contacts&action=DetailView&record='.$resource->id.'">'.$contact_obj->full_name.'</a></td>';
                }


                $i=0;
                for ($x=0; $x< $day_count; $x++) {
                    //Get date for each day
                    $dateq = $this->get_month_dates($start_date, $x);

                    $class = '';
                    $square = '';
                    $dup = 0;

                    for ($c=0; $c < $count; $c++) {
                        $ds_month = $this->count_months($start_date, $resource->tasks[$c]['start_day'], $x);
                        $de_month = $this->count_months($start_date, $resource->tasks[$c]['end_day'], $x);

                        if (($ds_month == 0 || $de_month == 0) && $resource->tasks[$c]['start_day'] <= $resource->tasks[$c]['end_day'] && $resource->tasks[$c]['start_day'] >=0 && $resource->tasks[$c]['end_day']>=0) {
                            $dup++;
                            $square =  '<td class="inner_td"><div rel="'.$dateq.'|'.$resource->id.'|'.$resource->type.'" class="cell_width day_block '.$class.' ' . $this->get_cell_class($dup) .'"></div></td>';
                        }
                    }

                    if ($square == '') {
                        $square = '<td class="inner_td"><div class="cell_width day_block"><div class="'.$class.'"></div></div></td>';
                    }
                    echo $square;
                    $i++;
                }
            }
        }
        //end quarterly view

        echo '</table>';
    }



    //Returns an array containing the years, months and weeks between two dates
    public function year_quarter($start_date, $end_date)
    {
        $begin = new DateTime($start_date);
        $end = new DateTime($end_date);
        $end->add(new DateInterval('P1D')); //Add 1 day to include the end date as a day
        $interval = new DateInterval('P1M'); // 1 week interval
        $period = new DatePeriod($begin, $interval, $end);
        $aResult = array();

        $count = 0;
        foreach ($period as $dt) {
            $count++;
            $y = $dt->format('Y');
            $c = ceil($dt->format('m')/3);
            $m = mb_substr($GLOBALS['app_list_strings']['dom_cal_month_short'][$dt->format('n')], 0, 3);
            
            $aResult[$y][$c][$count] = $m;
        }

        return array($aResult, $count);
    }


    //Returns an array containing the years, months and weeks between two dates
    public function year_week($start_date, $end_date)
    {
        $begin = new DateTime($start_date);
        $end = new DateTime($end_date);
        $end->add(new DateInterval('P1D')); //Add 1 day to include the end date as a day
        $interval = new DateInterval('P1W'); // 1 week interval
        $period = new DatePeriod($begin, $interval, $end);
        $aResult = array();

        $count = 0;
        foreach ($period as $dt) {
            $count++;
            $y = $dt->format('Y');
            $m = $GLOBALS['app_list_strings']['dom_cal_month_short'][$dt->format('n')];
            $w = $dt->format('W');

            $aResult[$y][$m][] = $w;
        }
        
        return array($aResult, $count);
    }


    //Returns an array containing the years, months and days between two dates
    public function year_month($start_date, $end_date)
    {
        $begin = new DateTime($start_date);
        $end = new DateTime($end_date);
        $end->add(new DateInterval('P1D')); //Add 1 day to include the end date as a day
        $interval = new DateInterval('P1D'); // 1 month interval
        $period = new DatePeriod($begin, $interval, $end);
        $aResult = array();
    
        foreach ($period as $dt) {
            $y = $dt->format('Y');
            $m = mb_substr($GLOBALS['app_list_strings']['dom_cal_month_short'][$dt->format('n')], 0, 3);
            $j = $dt->format('j');
            $d = mb_substr($GLOBALS['app_list_strings']['dom_cal_day_short'][$dt->format('w')+1], 0, 1);

            $aResult[$y][$m][$j] = $d;
        }

        return $aResult;
    }

    public function get_weeks($start_date, $end_date)
    {
        $begin = new DateTime($start_date);
        $end = new DateTime($end_date);
        $end->add(new DateInterval('P1D')); //Add 1 day to include the end date as a day
        $interval = new DateInterval('P1W'); // 1 week interval
        $period = new DatePeriod($begin, $interval, $end);
        $aResult = array();
    
        foreach ($period as $dt) {
            $aResult[] = $dt->format('W');
        }

        return $aResult;
    }


    //count number of months between task start day and chart current month
    public function count_months($start, $day, $x)
    {
        $sdate = DateTime::createFromFormat('Y-m-d', $start);
        $edate = DateTime::createFromFormat('Y-m-d', $start);
        // $date->setTimezone(new DateTimeZone("Europe/London"));
        $sdate->modify('+'.$day.' days');
        $edate->modify('+'.$x.' months');

        if ($sdate->format('Y') !== $edate->format('Y')) {
            return -1;
        }

        if ($sdate->format('m') !== $edate->format('m')) {
            return -1;
        }
        return 0;
    }


    //Returns the total number of days between two dates
    public function count_days($start_date, $end_date)
    {
        $d1 = new DateTime($start_date);
        $d2 = new DateTime($end_date);
        //If the task's end date is before chart's start date return -1 to make sure task starts on first day of the chart
        if ($d2 < $d1) {
            return -1;
        } elseif ($d2 == $d1) {
            return 1;
        }
        // $d2->add(new DateInterval('P1D')); //Add 1 day to include the end date as a day
        $difference = $d1->diff($d2);
        return $difference->days;
    }


    //returns first and last date of a week
    public function get_week_dates($start, $weeks)
    {
        $date = DateTime::createFromFormat('Y-m-d', $start);

        $date->modify('+'.($weeks + 1).' weeks');

        $ts = strtotime($date->format('Y-m-d'));
        $start = (date('w', $ts) == 0) ? $ts : strtotime('last monday', $ts);
        return date('Y-m-d', $start) . "|" . date('Y-m-d', strtotime('next sunday', $start));
    }

    //returns first and last date of a month
    public function get_month_dates($start, $months)
    {
        $date = DateTime::createFromFormat('Y-m-d', $start);

        $date->modify('+'.($months).' months');

        return $date->format('Y-m-01') . "|" . $date->format('Y-m-t');
    }


    //get date of passed in day in relation to the charts start date
    public function get_date($start, $day)
    {
        $date = DateTime::createFromFormat('Y-m-d', $start);
        // $date->setTimezone(new DateTimeZone("Europe/London"));
        $date->modify('+'.$day.' days');

        return $date->format('Y-m-d');
    }


    //checks if the day is a weekend and if the day is today.
    public function check_weekend($day)
    {
        global $current_user;
        //get users timezone setting
        $timezone = TimeDate::userTimezone($current_user);
        $now = new DateTime();
        $now->setTimezone(new DateTimeZone($timezone));
        $date1 = DateTime::createFromFormat('Y-m-d', $day);
        $date = $date1->format('Y-m-d');
        $now = $now->format('Y-m-d');
        // $GLOBALS['log']->fatal("date2 ".$now);

        if ($date1->format('l') == 'Sunday' && $date === $now) {
            return 'weekend-today';
        } elseif ($date1->format('l') == 'Saturday' && $date === $now) {
            return 'weekend-today';
        } elseif ($date1->format('l') == 'Sunday') {
            return 'weekend';
        } elseif ($date1->format('l') == 'Saturday') {
            return 'weekend';
        } elseif ($date === $now) {
            return 'today';
        }
        return false;
    }



    //Returns the time span between two dates in years months and days
    public function time_range($start_date, $end_date)
    {
        $datetime1 = new DateTime($start_date);
        $datetime2 = new DateTime($end_date);
        $interval = $datetime1->diff($datetime2);
        echo $interval->format('%y years %m months and %d days');
    }

    //returns the css class for cell color/ h => non duplicate, d =>duplicate
    public function get_cell_class($days)
    {
        if ($days > 1) {
            return " d";
        }
        return " h";
    }
}
