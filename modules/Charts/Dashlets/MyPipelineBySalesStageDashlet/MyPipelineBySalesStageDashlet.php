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





require_once('include/Dashlets/DashletGenericChart.php');

#[\AllowDynamicProperties]
class MyPipelineBySalesStageDashlet extends DashletGenericChart
{
    public $mypbss_date_start;
    public $mypbss_date_end;
    public $mypbss_sales_stages = array();

    protected $_seedName = 'Opportunities';

    /**
     * @see DashletGenericChart::__construct()
     */
    public function __construct(
        $id,
        array $options = null
    ) {
        global $timedate;

        if (empty($options['mypbss_date_start'])) {
            $options['mypbss_date_start'] = $timedate->nowDbDate();
        }
        if (empty($options['mypbss_date_end'])) {
            $options['mypbss_date_end'] = $timedate->asDbDate($timedate->getNow()->modify("+6 months"));
        }
        if (empty($options['title'])) {
            $options['title'] = translate('LBL_MY_PIPELINE_FORM_TITLE', 'Home');
        }

        parent::__construct($id, $options);
    }

    /**
     * @see DashletGenericChart::displayOptions()
     */
    public function displayOptions()
    {
        global $app_list_strings;

        $selected_datax = array();
        if (isset($this->mypbss_sales_stages) && count($this->mypbss_sales_stages) > 0) {
            foreach ($this->mypbss_sales_stages as $key) {
                $selected_datax[] = $key;
            }
        } else {
            $selected_datax = array_keys($app_list_strings['sales_stage_dom']);
        }

        $this->_searchFields['mypbss_sales_stages']['options'] = $app_list_strings['sales_stage_dom'];
        $this->_searchFields['mypbss_sales_stages']['input_name0'] = $selected_datax;

        return parent::displayOptions();
    }

    /**
     * @see DashletGenericChart::display()
     */
    public function display()
    {
        global $sugar_config, $current_user, $timedate;

        $module = 'Opportunities';
        $action = 'index';
        $queryable = 'true';
        $searchFormTab = 'advanced_search';
        $userId = $current_user->id;

        $url_params = array( 'assigned_user_id' => $current_user->id );
        $group_by = $this->constructGroupBy();

        $currency_symbol = $sugar_config['default_currency_symbol'];
        if ($current_user->getPreference('currency')) {
            $currency = BeanFactory::newBean('Currencies');
            $currency->retrieve($current_user->getPreference('currency'));
            $currency_symbol = $currency->symbol;
        }

        $thousands_symbol = translate('LBL_OPP_THOUSANDS', 'Charts');

        $subtitle = translate('LBL_OPP_SIZE', 'Charts') . " " . $currency_symbol . "1" . translate('LBL_OPP_THOUSANDS', 'Charts');

        $query = $this->constructQuery();
        $data = $this->getChartData($query);

        $chartReadyData = $this->prepareChartData($data, $currency_symbol, $thousands_symbol);

        $canvasId = 'rGraphOppByLeadSourceByOutcome'.uniqid();
        $chartWidth     = 900;
        $chartHeight    = 500;
        $autoRefresh = $this->processAutoRefresh();

        //$chartReadyData['data'] = [[1.1,2.2],[3.3,4.4]];
        $jsonData = json_encode($chartReadyData['data']);
        $jsonLabels = json_encode($chartReadyData['labels']);
        $jsonLabelsAndValues = json_encode($chartReadyData['labelsAndValues'] ?? '');
        $jsonTooltips = json_encode($chartReadyData['tooltips']);

        $total = $chartReadyData['total'];


        $jsonKey = json_encode($chartReadyData['key']);
        $jsonTooltips = json_encode($chartReadyData['tooltips']);

        $colours = "['#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928']";

        $startDate = $timedate->to_display_date($this->mypbss_date_start, false);
        $endDate = $timedate->to_display_date($this->mypbss_date_end, false);

        if (!is_array($chartReadyData['data'])||count($chartReadyData['data']) < 1) {
            return "<h3 class='noGraphDataPoints'>$this->noDataMessage</h3>";
        }

        $chart = <<<EOD
        <canvas id='$canvasId'   class='resizableCanvas'  width='$chartWidth' height='$chartHeight'>[No canvas support]</canvas>
        <input type='hidden' class='module' value='$module' />
        <input type='hidden' class='action' value='$action' />
        <input type='hidden' class='query' value='$queryable' />
        <input type='hidden' class='searchFormTab' value='$searchFormTab' />
        <input type='hidden' class='userId' value='$userId' />
        <input type='hidden' class='startDate' value='$startDate' />
        <input type='hidden' class='endDate' value='$endDate' />
             $autoRefresh
         <script>
        window["chartHBarKeys$canvasId"] = $jsonKey;
           var hbar = new RGraph.HBar({
            id: '$canvasId',
            data:$jsonData,
            options: {
                //grouping: 'stacked',
                colorsSequential:true,
                tooltipsEvent:'mousemove',
                tooltips:$jsonTooltips,
                labels: $jsonLabels,
                xlabels:true,
                labelsAbove: true,
                labelsAbovedecimals: 2,
                linewidth: 2,
                textSize:10,
                eventsClick:myPipelineBySalesStageClick,
                //textSize:8,
                strokestyle: 'white',
                //colors: ['Gradient(#4572A7:#66f)','Gradient(#AA4643:white)','Gradient(#89A54E:white)'],
                //shadowOffsetx: 1,
                //shadowOffsety: 1,
                //shadowBlur: 10,
                //hmargin: 25,
                //gutterTop:50,
                gutterLeft: 150,
                //gutterRight:50,
                //gutterBottom: 155,
                //textAngle: 45,
                backgroundGridVlines: false,
                backgroundGridBorder: false,
                tooltips:$jsonTooltips,
                tooltipsEvent:'mousemove',
                colors:$colours,
                key: $jsonKey,
                keyColors: $colours,
                //keyPosition: 'gutter',
                keyPositionX: $canvasId.width - 190,
                //keyPositionY: 18,
                keyPositionGutterBoxed: true,
                axisColor: '#ccc',
                unitsPre:'$currency_symbol',
                unitsPost:'$thousands_symbol',
                tooltipsCssClass: 'rgraph_chart_tooltips_css',
                noyaxis: true
            }
        }).draw();
        /*
        .on('draw', function (obj)
        {
            for (var i=0; i<obj.coords.length; ++i) {
                obj.context.fillStyle = 'black';
                if(obj.data_arr[i] > 0)
                {
                RGraph.Text2(obj.context, {
                    font:'Arial',
                    'size':10,
                    'x':obj.coords[i][0] + (obj.coords[i][2] / 2),
                    'y':obj.coords[i][1] + (obj.coords[i][3] / 2),
                    'text':obj.data_arr[i].toString(),
                    'valign':'center',
                    'halign':'center'
                });
                }
            }
        }).draw();
        */
        hbar.canvas.onmouseout = function (e)
        {
            // Hide the tooltip
            RGraph.hideTooltip();

            // Redraw the canvas so that any highlighting is gone
            RGraph.redraw();
        }
/*
        new RGraph.Drawing.Text({
            id: '$canvasId',
            x: 10,
            y: 30,
            text: 'Pipeline total is {$currency_symbol}$total$thousands_symbol',
            options: {
                font: 'Arial',
                bold: true,
                //halign: 'left',
                //valign: 'bottom',
                colors: ['black'],
                size: 10
            }
        }).draw();
*/
</script>
EOD;

        return $chart;


        /*
        $sugarChart->setData($dataset);
        $total = format_number($this->getHorizBarTotal($dataset), 0, 0, array('convert'=>true));
        $pipeline_total_string = translate('LBL_TOTAL_PIPELINE', 'Charts') . $sugarChart->currency_symbol . $total . $sugarChart->thousands_symbol;
        $sugarChart->setProperties($pipeline_total_string, $subtitle, 'horizontal bar chart');

        // Bug #53753 We have to add values for filter based on "Expected Close Date" field
        if (!empty($this->mypbss_date_start) && !empty($this->mypbss_date_end))
        {
            $sugarChart->url_params['date_closed_advanced_range_choice'] = 'between';
            $sugarChart->url_params['start_range_date_closed_advanced'] = $timedate->to_display_date($this->mypbss_date_start, false);
            $sugarChart->url_params['end_range_date_closed_advanced'] = $timedate->to_display_date($this->mypbss_date_end, false);
        }
        elseif (!empty($this->mypbss_date_start))
        {
            $sugarChart->url_params['date_closed_advanced_range_choice'] = 'greater_than';
            $sugarChart->url_params['range_date_closed_advanced'] = $timedate->to_display_date($this->mypbss_date_start, false);
        }
        elseif (!empty($this->mypbss_date_end))
        {
            $sugarChart->url_params['date_closed_advanced_range_choice'] = 'less_than';
            $sugarChart->url_params['range_date_closed_advanced'] = $timedate->to_display_date($this->mypbss_date_end, false);
        }

        $xmlFile = $sugarChart->getXMLFileName($this->id);
        $sugarChart->saveXMLFile($xmlFile, $sugarChart->generateXML());

        return $this->getTitle('') .
        '<div align="center">' .$sugarChart->display($this->id, $xmlFile, '100%', '480', false) . '</div><br />'. $this->processAutoRefresh();
        */
    }

    /**
     * awu: Bug 16794 - this function is a hack to get the correct sales stage order
     * until i can clean it up later
     *
     * @param  $query string
     * @return array
     */
    public function getChartData(
        $query
    ) {
        global $app_list_strings, $db;

        $data = array();
        $temp_data = array();
        $selected_datax = array();

        $user_sales_stage = $this->mypbss_sales_stages;
        $tempx = $user_sales_stage;

        //set $datax using selected sales stage keys
        if (isset($tempx) && (is_countable($tempx) ? count($tempx) : 0) > 0) {
            foreach ($tempx as $key) {
                $datax[$key] = $app_list_strings['sales_stage_dom'][$key];
                array_push($selected_datax, $key);
            }
        } else {
            $datax = $app_list_strings['sales_stage_dom'];
            $selected_datax = array_keys($app_list_strings['sales_stage_dom']);
        }

        $result = $db->query($query);
        $row = $db->fetchByAssoc($result, false);

        while ($row != null) {
            $temp_data[] = $row;
            $row = $db->fetchByAssoc($result, false);
        }

        // reorder and set the array based on the order of selected_datax
        foreach ($selected_datax as $sales_stage) {
            foreach ($temp_data as $key => $value) {
                if ($value['sales_stage'] == $sales_stage) {
                    $value['sales_stage'] = $app_list_strings['sales_stage_dom'][$value['sales_stage']];
                    $value['key'] = $sales_stage;
                    $value['value'] = $value['sales_stage'];
                    $data[] = $value;
                    unset($temp_data[$key]);
                }
            }
        }
        return $data;
    }

    /**
     * @param  $dataset array
     * @return int
     */
    private function getHorizBarTotal(
        $dataset
    ) {
        $total = 0;
        foreach ($dataset as $value) {
            $total += $value;
        }

        return $total;
    }

    /**
     * @see DashletGenericChart::constructQuery()
     */
    protected function constructQuery()
    {
        $query = "SELECT opportunities.sales_stage,
                        users.user_name,
                        opportunities.assigned_user_id,
                        count(*) AS opp_count,
                        sum(amount_usdollar/1000) AS total
                    FROM users,opportunities  ";
        $query .= " WHERE opportunities.assigned_user_id IN ('{$GLOBALS['current_user']->id}') " .
            " AND opportunities.date_closed >= ". DBManagerFactory::getInstance()->convert("'".$this->mypbss_date_start."'", 'date').
            " AND opportunities.date_closed <= ". DBManagerFactory::getInstance()->convert("'".$this->mypbss_date_end."'", 'date') .
            " AND opportunities.assigned_user_id = users.id  AND opportunities.deleted=0 ";
        if (isset($this->mypbss_sales_stages) && count($this->mypbss_sales_stages) > 0) {
            $query .= " AND opportunities.sales_stage IN ('" . implode("','", $this->mypbss_sales_stages) . "') ";
        }
        $query .= " GROUP BY opportunities.sales_stage ,users.user_name,opportunities.assigned_user_id";

        return $query;
    }

    /**
     * @see DashletGenericChart::constructGroupBy()
     */
    protected function constructGroupBy()
    {
        $groupBy = array('sales_stage');

        array_push($groupBy, 'user_name');
        return $groupBy;
    }

    protected function prepareChartData($dataset, $currency_symbol, $thousands_symbol)
    {
        $chart = [];
        //Use the  lead_source to categorise the data for the charts
        $chart['labels'] = array();
        $chart['data'] = array();
        //Need to add all elements into the key, as they are stacked (even though the category is not present, the value could be)
        $chart['key'] = array();
        $chart['tooltips']= array();
        $chart['total'] = 0;

        foreach ($dataset as $data) {
            $formattedFloat = (float)number_format((float)$data['total'], 2, '.', '');
            $chart['labels'][] = $data['sales_stage'];
            $chart['key'][] = $data['key'];
            $chart['data'][] = $formattedFloat;
            $chart['total']+=$formattedFloat;
            $chart['tooltips'][] = "'" . $data['sales_stage']
                . "' amounts to $currency_symbol$formattedFloat$thousands_symbol (click bar to drill-through)";
        }
        return $chart;
    }
}
