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

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}


#[\AllowDynamicProperties]
class ProjectViewResourceList extends SugarView
{
    public function display()
    {
        include('modules/Project/chart.php');

        echo '<link rel="stylesheet" type="text/css" href="modules/Project/css/style.css" />';
        echo '<link rel="stylesheet" type="text/css" href="modules/Project/css/style_chart.css" />';
        echo '<link rel="stylesheet" type="text/css" href="modules/Project/qtip/jquery.qtip.min.css" />';
        echo '<script type="text/javascript" src="modules/Project/js/jquery.blockUI.js"></script>';
        echo '<script type="text/javascript" src="modules/Project/qtip/jquery.qtip.min.js"></script>';
        echo '<script type="text/javascript" src="modules/Project/js/main_lib_chart.js"></script>'; ?>
        <!--Mark-up for the main body of the view-->
        <div id="wrapper_chart">

            <div id="project_chart">
                <div id="gantt_chart">
                  <!-- chart space -->
                </div> </table>
                </div>
            </div>
            <div style="" id="task_divs" >
                <!--The task overlay divs are appended in here-->
            </div>
            <!--input id="date_start" type="hidden" name="date_start" value="">
            <input id="date_end" class="date_chart" type="hidden" name="date_end" value="" -->
        </div>
        <!--Main body end-->
<?php
    }
}
