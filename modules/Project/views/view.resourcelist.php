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
