<?php
 /**
 *
 *
 * @package
 * @copyright SalesAgility Ltd http://www.salesagility.com
 *
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
 *
 * @author SalesAgility Ltd <support@salesagility.com>
 */
require_once 'modules/AOW_WorkFlow/aow_utils.php';
require_once 'modules/AOR_Reports/aor_utils.php';
#[\AllowDynamicProperties]
class AOR_ReportsViewDetail extends ViewDetail
{
    private function getReportParameters()
    {
        if (!$this->bean->id) {
            return array();
        }
        $conditions = $this->bean->get_linked_beans('aor_conditions', 'AOR_Conditions', 'condition_order');
        $parameters = array();
        foreach ($conditions as $condition) {
            if (!$condition->parameter) {
                continue;
            }
            $condition->module_path = implode(":", unserialize(base64_decode($condition->module_path),['allowed_classes' => false]));
            if ($condition->value_type == 'Date') {
                $condition->value = unserialize(base64_decode($condition->value),['allowed_classes' => false]);
            }
            $condition_item = $condition->toArray();
            $display = getDisplayForField($condition->module_path, $condition->field, $this->bean->report_module);
            $condition_item['module_path_display'] = $display['module'];
            $condition_item['field_label'] = $display['field'];
            if (!empty($this->bean->user_parameters[$condition->id])) {
                $param = $this->bean->user_parameters[$condition->id];
                $condition_item['operator'] = $param['operator'];
                $condition_item['value_type'] = $param['type'];
                $condition_item['value'] = $param['value'];
            }
            if (isset($parameters[$condition_item['condition_order']])) {
                $parameters[] = $condition_item;
            } else {
                $parameters[$condition_item['condition_order']] = $condition_item;
            }
        }
        return $parameters;
    }

    public function preDisplay()
    {
        global $app_list_strings;
        parent::preDisplay();

        $canExport = $this->bean->ACLAccess('Export');
        $this->ss->assign('can_export', $canExport);

        $this->ss->assign('report_module', $this->bean->report_module);



        $this->bean->user_parameters = requestToUserParameters($this->bean);

        //$reportHTML = $this->bean->build_group_report(0,true);
        $reportHTML = $this->bean->buildMultiGroupReport(0, true);

        $chartsHTML = $this->bean->build_report_chart(null, AOR_Report::CHART_TYPE_RGRAPH);

        $chartsPerRow = $this->bean->graphs_per_row;

        $this->ss->assign('charts_content', $chartsHTML);

        $this->ss->assign('report_content', $reportHTML);

        echo "<input type='hidden' name='report_module' id='report_module' value='{$this->bean->report_module}'>";
        if (!is_file('cache/jsLanguage/AOR_Conditions/' . $GLOBALS['current_language'] . '.js')) {
            require_once('include/language/jsLanguage.php');
            jsLanguage::createModuleStringsCache('AOR_Conditions', $GLOBALS['current_language']);
        }
        echo '<script src="cache/jsLanguage/AOR_Conditions/'. $GLOBALS['current_language'] . '.js"></script>';

        $params = $this->getReportParameters();
        echo "<script>var reportParameters = ".json_encode($params).";</script>";

        $resizeGraphsPerRow = <<<EOD

       <script>
        function resizeGraphsPerRow()
        {
                var maxWidth = 900;
                var maxHeight = 500;
                var maxTextSize = 10;
                var divWidth = $("#detailpanel_report").width();

                var graphWidth = Math.floor(divWidth / $chartsPerRow);

                var graphs = document.getElementsByClassName('resizableCanvas');
                for(var i = 0; i < graphs.length; i++)
                {
                    if(graphWidth * 0.9 > maxWidth)
                    graphs[i].width  = maxWidth;
                else
                    graphs[i].width = graphWidth * 0.9;
                if(graphWidth * 0.9 > maxHeight)
                    graphs[i].height = maxHeight;
                else
                    graphs[i].height = graphWidth * 0.9;


                /*
                var text_size = Math.min(12, (graphWidth / 1000) * 12 );
                if(text_size < 6)text_size=6;
                if(text_size > maxTextSize) text_size = maxTextSize;

                if(     graphs[i] !== undefined
                    &&  graphs[i].__object__ !== undefined
                    &&  graphs[i].__object__["properties"] !== undefined
                    &&  graphs[i].__object__["properties"]["chart.text.size"] !== undefined
                    &&  graphs[i].__object__["properties"]["chart.key.text.size"] !== undefined)
                 {
                    graphs[i].__object__["properties"]["chart.text.size"] = text_size;
                    graphs[i].__object__["properties"]["chart.key.text.size"] = text_size;
                 }
                //http://www.rgraph.net/docs/issues.html
                //As per Google Chrome not initially drawing charts
                RGraph.redrawCanvas(graphs[i]);
                */
                }
                if (typeof RGraph !== 'undefined') {
                    RGraph.redraw();
                }
        }
        </script>

EOD;



        echo $resizeGraphsPerRow;
        echo "<script> $(document).ready(function(){resizeGraphsPerRow();}); </script>";
        echo "<script> $(window).resize(function(){resizeGraphsPerRow();}); </script>";
    }
}
