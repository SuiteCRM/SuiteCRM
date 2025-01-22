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
class Gantt
{
    private $start_date;
    private $end_date;
    private $tasks;

    public function __construct($start_date, $end_date, $tasks)
    {
        $this->start_date = $start_date;
        $this->end_date = $end_date;
        $this->tasks = $tasks;
        
        
        //draw the grid
        $this->draw($this->start_date, $this->end_date, $this->tasks);
    }

    public function draw($start_date, $end_date, $tasks)
    {
        $day_count = $this->count_days($start_date, $end_date);
        $diff_interval = 30 - $day_count;
        if ($diff_interval <= 0) {
            $time_span = $this->year_month($start_date, $end_date);
        } else {
            $time_span = $this->year_month($start_date, $end_date, $diff_interval);
        }

        // $project_length = $this->time_range($start_date, $end_date);




        //Generate main table and the first row containing the months
        echo '<table class="main_table"><tr class="month_row">';

        foreach ($time_span as $months) {
            foreach ($months as $month => $days) {
                echo '<td class="months">'.$month.'</td>';
            }
        }

        //end the month row and start the days row
        echo '</tr><tr class="day_row">';

        $month_count = 0;//start month count
        foreach ($time_span as $months) {
            $m=0;
            foreach ($months as $days) {
                echo '<td class="inner_container">';
                //Generate a table containing the days in each month
                echo '<table class="table_inner"><tr>';

                foreach ($days as $day => $d) {
                    echo '<td class="inner_td"><div class="cell_width">'.$day.'</div></td>';//day number shown
                }
                echo '</tr><tr>';

                foreach ($days as $d) {
                    echo '<td class="inner_td"><div class="cell_width">'.$this->substr_unicode($d, 0, 1).'</div></td>';//First letter of the days name shown
                }
                echo '</tr></table></td>';//end table containing the days in each month
                $m++;
            }

            $month_count += $m; //total month count
        }
        //for each task generate a row of empty days
        $i=1;
        if (!is_null($tasks)) {
            foreach ($tasks as $task) {
                echo '</tr><tr class="task_row">';
                echo '<td colspan="'.$month_count.'"><table id="task'.$i.'" class="table_inner"><tr>';

                $task_start_day = $this->count_days($start_date, $task->date_start);
                $task_end_day = $this->count_days($start_date, $task->date_finish);
                $task_duration = $this->count_days($task->date_start, $task->date_finish);
                $task->predecessors = $task->predecessors == '' ? 0 : $task->predecessors;


                for ($x=1; $x<= $day_count; $x++) {
                    if ($x==1 && $x != $task_start_day) {
                        echo '<td class="inner_td"><div class="cell_width day_block"></div></td>';
                    } else {
                        if ($x==1 && $x == $task_start_day) {
                            if ($task->milestone_flag == '1' && ($task_duration == 0 || $task_duration == 1)) {
                                echo '<td class="task_td2"><div class="cell_width task_block1">
                                    <div class="task_block_inner">
                                        <div class="milestone link" id="'.$task->project_task_id.'" pre="'.$task->predecessors.'" link="'.$task->relationship_type.'" rel="'.$task->name.'">
                                           <img src="modules/Project/images/add_milestone.png" />
                                        </div>
                                    </div>
                                  </div></td><td class="inner_td"><div class="cell_width day_block"></div></td>';
                            } else {
                                if ($task_duration == 0 || $task_duration == 1) {
                                    echo '<td class="task_td2"><div class="cell_width task_block1">
                                    <div class="task_block_inner">
                                        <div class="task1 link" id="'.$task->project_task_id.'" pre="'.$task->predecessors.'" link="'.$task->relationship_type.'" rel="'.$task->name.'">
                                            <div class="task_percent" rel="'.$task->percent_complete.'"></div>
                                        </div>
                                    </div>
                                  </div></td><td class="inner_td"><div class="cell_width day_block"></div></td>';
                                } else {
                                    echo '<td class="task_td" colspan="'.$task_duration.'"><div class="cell_width task_block">
                                    <div class="task_block_inner">
                                        <div class="task link" id="'.$task->project_task_id.'" pre="'.$task->predecessors.'" link="'.$task->relationship_type.'" rel="'.$task->name.'">
                                            <div class="task_percent" rel="'.$task->percent_complete.'">'.$task->name.'</div>
                                        </div>
                                    </div>
                                  </div></td>';
                                }
                            }
                        } else {
                            if ($x == $task_start_day && $x == $day_count) {
                                if ($task->milestone_flag == '1' && ($task_duration == 0 || $task_duration == 1)) {
                                    echo '<td class="task_td2"><div class="cell_width task_block1">
                                    <div class="task_block_inner">
                                        <div class="milestone link" id="'.$task->project_task_id.'" pre="'.$task->predecessors.'" link="'.$task->relationship_type.'" rel="'.$task->name.'">
                                            <img src="modules/Project/images/add_milestone.png" />
                                        </div>
                                    </div>
                                  </div></td>';
                                } else {
                                    if ($task_duration == 0 || $task_duration == 1) {
                                        echo '<td class="task_td2"><div class="cell_width task_block1">
                                <div class="task_block_inner">
                                     <div class="task1 link" id="'.$task->project_task_id.'" pre="'.$task->predecessors.'" link="'.$task->relationship_type.'" rel="'.$task->name.'">
                                        <div class="task_percent" rel="'.$task->percent_complete.'"></div>
                                    </div>
                                </div>
                              </div></td>';
                                    } else {
                                        echo '<td class="task_td" colspan="'.$task_duration.'"><div class="cell_width task_block">
                                    <div class="task_block_inner">
                                        <div class="task link" id="'.$task->project_task_id.'" pre="'.$task->predecessors.'" link="'.$task->relationship_type.'" rel="'.$task->name.'">
                                            <div class="task_percent" rel="'.$task->percent_complete.'">'.$task->name.'</div>
                                        </div>
                                    </div>
                                  </div></td>';
                                    }
                                }
                            } else {
                                if ($x == $task_start_day) {
                                    if ($task->milestone_flag == '1' && ($task_duration == 0 || $task_duration == 1)) {
                                        echo '<td class="task_td2"><div class="cell_width task_block1">
                                    <div class="task_block_inner">
                                        <div class="milestone link" id="'.$task->project_task_id.'" pre="'.$task->predecessors.'" link="'.$task->relationship_type.'" rel="'.$task->name.'">
                                            <img src="modules/Project/images/add_milestone.png" />
                                        </div>
                                    </div>
                                  </div></td><td class="inner_td"><div class="cell_width day_block"></div></td>';
                                    } else {
                                        if ($task_duration == 0 || $task_duration == 1) {
                                            echo '<td class="task_td2"><div class="cell_width task_block1">
                                <div class="task_block_inner">
                                     <div class="task1 link" id="'.$task->project_task_id.'" pre="'.$task->predecessors.'" link="'.$task->relationship_type.'" rel="'.$task->name.'">
                                        <div class="task_percent" rel="'.$task->percent_complete.'"></div>
                                    </div>
                                </div>
                              </div></td><td class="inner_td"><div class="cell_width day_block"></div></td>';
                                        } else {
                                            echo '<td class="task_td" colspan="'.$task_duration.'"><div class="cell_width task_block">
                                <div class="task_block_inner">
                                     <div class="task link" id="'.$task->project_task_id.'" pre="'.$task->predecessors.'" link="'.$task->relationship_type.'" rel="'.$task->name.'">
                                        <div class="task_percent" rel="'.$task->percent_complete.'"></div>
                                    </div>
                                </div>
                              </div></td>';
                                        }
                                    }
                                } else {
                                    if ($x == $day_count) {
                                    } else {
                                        if ($x > $task_start_day && $x < $task_end_day) {
                                            //leave blank
                                        } else {
                                            echo '<td class="inner_td"><div class="cell_width day_block"></div></td>';
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                echo '</tr></table ></td></tr>';
                $i++;
            }
        }
        echo '</table>';
    }



    //Returns an array containing the years, months and days between two dates
    public function year_month($start_date, $end_date, $diff_interval=1)
    {
        $begin = new DateTime($start_date);
        $end = new DateTime($end_date);
        $end->add(new DateInterval('P'. $diff_interval .'D')); //Add 1 day to include the end date as a day
        
        $interval = new DateInterval('P1D'); // 1 month interval
        $period = new DatePeriod($begin, $interval, $end);
        $aResult = array();
        
        foreach ($period as $dt) {
            $y = $dt->format('Y');
            $m = $GLOBALS['app_list_strings']['dom_cal_month_short'][$dt->format('n')];
            $j = $dt->format('j');
            $d = $GLOBALS['app_list_strings']['dom_cal_day_short'][$dt->format('w')+1];

            $aResult[$y][$m][$j] = $d;
        }

        return $aResult;
    }

    //Returns the total number of days between two dates
    public function count_days($start_date, $end_date)
    {
        $d1 = new DateTime($start_date);
        $d2 = new DateTime($end_date);

        //If the task's end date is before chart's start date return 1 to make sure task starts on first day of the chart
        if ($d2 < $d1) {
            return 1;
        }

        $d2->add(new DateInterval('P1D')); //Add 1 day to include the end date as a day
        $difference = $d1->diff($d2);
        return $difference->days;
    }
    //Returns the time span between two dates in years  months and days
    public function time_range($start_date, $end_date)
    {
        $datetime1 = new DateTime($start_date);
        $datetime2 = new DateTime($end_date);
        $datetime2->add(new DateInterval('P1D')); //Add 1 day to include the end date as a day
        $interval = $datetime1->diff($datetime2);
        return $interval->format('%y years %m months and %d days');
    }

    public function substr_unicode($str, $s, $l = null)
    {
        return implode("", array_slice(
            preg_split("//u", (string) $str, -1, PREG_SPLIT_NO_EMPTY),
            $s,
            $l
        ));
    }

    // Function for basic field validation (present and neither empty nor only white space
    public function IsNullOrEmptyString($question)
    {
        return (!isset($question) || trim($question)==='');
    }
}
