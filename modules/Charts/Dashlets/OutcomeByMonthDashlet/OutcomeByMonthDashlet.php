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
class OutcomeByMonthDashlet extends DashletGenericChart
{
    public $obm_ids = array();
    public $obm_date_start;
    public $obm_date_end;

    /**
     * @see DashletGenericChart::$_seedName
     */
    protected $_seedName = 'Opportunities';

    /**
     * @see DashletGenericChart::__construct()
     */
    public function __construct(
        $id,
        array $options = null
    ) {
        global $timedate;

        if (empty($options['obm_date_start'])) {
            $options['obm_date_start'] = $timedate->nowDbDate();
        }

        if (empty($options['obm_date_end'])) {
            $options['obm_date_end'] = $timedate->asDbDate($timedate->getNow()->modify("+6 months"));
        }

        parent::__construct($id, $options);
    }

    /**
     * @see DashletGenericChart::displayOptions()
     */
    public function displayOptions()
    {
        if (!isset($this->obm_ids) || count($this->obm_ids) == 0) {
            $this->_searchFields['obm_ids']['input_name0'] = array_keys(get_user_array(false));
        }

        return parent::displayOptions();
    }

    /**
     * @see DashletGenericChart::display()
     */
    public function display()
    {
        $currency_symbol = $GLOBALS['sugar_config']['default_currency_symbol'];
        if ($GLOBALS['current_user']->getPreference('currency')) {
            $currency = BeanFactory::newBean('Currencies');
            $currency->retrieve($GLOBALS['current_user']->getPreference('currency'));
            $currency_symbol = $currency->symbol;
        }
        $thousands_symbol = translate('LBL_OPP_THOUSANDS', 'Charts');
        $module = 'Opportunities';
        $action = 'index';
        $query = 'true';
        $searchFormTab = 'advanced_search';
        $groupBy = array( 'm', 'sales_stage', );


        $data = $this->getChartData($this->constructQuery());

        //I have taken out the sort as this will throw off the labels we have calculated
        $data = $this->sortData($data, 'm', false, 'sales_stage', true, true);

        $chartReadyData = $this->prepareChartData($data, $currency_symbol, $thousands_symbol);
        $canvasId = 'rGraphOutcomeByMonth'.uniqid();
        $chartWidth     = 900;
        $chartHeight    = 500;
        $autoRefresh = $this->processAutoRefresh();

        //$chartReadyData['data'] = [[1.1,2.2],[3.3,4.4]];
        $jsonData = json_encode($chartReadyData['data']);
        $jsonLabels = json_encode($chartReadyData['labels']);
        $jsonLabelsAndValues = json_encode($chartReadyData['labelsAndValues'] ?? '');


        $jsonKey = json_encode($chartReadyData['key']);
        $jsonTooltips = json_encode($chartReadyData['tooltips']);

        $colours = "['#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928']";


        if (!is_array($chartReadyData['data'])||count($chartReadyData['data']) < 1) {
            return "<h3 class='noGraphDataPoints'>$this->noDataMessage</h3>";
        }

        $chart = <<<EOD
        <canvas id='$canvasId' class='resizableCanvas'  width='$chartWidth' height='$chartHeight'>[No canvas support]</canvas>
             $autoRefresh
         <script>
           var bar = new RGraph.Bar({
            id: '$canvasId',
            data:$jsonData,
            options: {
                grouping: 'stacked',
                labels: $jsonLabels,
                xlabels:true,
                textSize:10,
                labelsAbove: true,
                //labelsAboveSize:10,
                labelsAboveUnitsPre:'$currency_symbol',
                labelsAboveUnitsPost:'$thousands_symbol',
                labelsAbovedecimals: 2,
                //linewidth: 2,
                eventsClick:outcomeByMonthClick,
                //textSize:10,
                strokestyle: 'white',
                //colors: ['Gradient(#4572A7:#66f)','Gradient(#AA4643:white)','Gradient(#89A54E:white)'],
                //shadowOffsetx: 1,
                //shadowOffsety: 1,
                //shadowBlur: 10,
                //hmargin: 25,
               // colors:$colours,
                gutterLeft: 60,
                gutterTop:50,
                //gutterRight:160,
                //gutterBottom: 155,
                //textAngle: 45,
                backgroundGridVlines: false,
                backgroundGridBorder: false,
                tooltips:$jsonTooltips,
                tooltipsEvent:'mousemove',
                colors:$colours,
                key: $jsonKey,
                keyColors: $colours,
                keyBackground:'rgba(255,255,255,0.7)',
                //keyPosition: 'gutter',
                //keyPositionX: $canvasId.width - 150,
                //keyPositionY: 18,
                //keyPositionGutterBoxed: true,
                axisColor: '#ccc',
                unitsPre:'$currency_symbol',
                unitsPost:'$thousands_symbol',
                keyHalign:'right',
                tooltipsCssClass: 'rgraph_chart_tooltips_css',
                noyaxis: true
            }
        }).draw();
        /*.on('draw', function (obj)
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

        bar.canvas.onmouseout = function (e)
        {
            // Hide the tooltip
            RGraph.hideTooltip();

            // Redraw the canvas so that any highlighting is gone
            RGraph.redraw();
        }
/*
         var sizeIncrement = new RGraph.Drawing.Text({
            id: '$canvasId',
            x: 10,
            y: 20,
            text: 'Opportunity size in {$currency_symbol}1$thousands_symbol',
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
    }

    /**
     * @see DashletGenericChart::constructQuery()
     */
    protected function constructQuery()
    {
        $query = "SELECT sales_stage,".
            DBManagerFactory::getInstance()->convert('opportunities.date_closed', 'date_format', array("'%Y-%m'"), array("'YYYY-MM'"))." as m, ".
            "sum(amount_usdollar/1000) as total, count(*) as opp_count FROM opportunities ";
        $query .= " WHERE opportunities.date_closed >= ".DBManagerFactory::getInstance()->convert("'".$this->obm_date_start."'", 'date') .
            " AND opportunities.date_closed <= ".DBManagerFactory::getInstance()->convert("'".$this->obm_date_end."'", 'date') .
            " AND opportunities.deleted=0";
        if (isset($this->obm_ids) && count($this->obm_ids) > 0) {
            $query .= " AND opportunities.assigned_user_id IN ('" . implode("','", $this->obm_ids) . "')";
        }
        $query .= " GROUP BY sales_stage,".
            DBManagerFactory::getInstance()->convert('opportunities.date_closed', 'date_format', array("'%Y-%m'"), array("'YYYY-MM'")) .
            " ORDER BY m";

        return $query;
    }

    protected function prepareChartData($data, $currency_symbol, $thousands_symbol)
    {
        $chart = [];
        //Use the  lead_source to categorise the data for the charts
        $chart['labels'] = array();
        $chart['data'] = array();
        //Need to add all elements into the key, as they are stacked (even though the category is not present, the value could be)
        $chart['key'] = array();
        $chart['tooltips']= array();

        foreach ($data as $i) {
            $key = $i["m"];
            $stage = $i["sales_stage"];
            $stage_dom_option = $i["sales_stage_dom_option"];
            if (!in_array($key, $chart['labels'])) {
                $chart['labels'][] = $key;
                $chart['data'][] = array();
            }
            if (!in_array($stage, $chart['key'])) {
                $chart['key'][] = $stage;
            }

            $formattedFloat = (float)number_format((float)$i["total"], 2, '.', '');
            $chart['data'][count($chart['data'])-1][] = $formattedFloat;
            $chart['tooltips'][]="<div><input type='hidden' class='stage' value='$stage_dom_option'><input type='hidden' class='date' value='$key'></div>".$stage.'('.$currency_symbol.$formattedFloat.$thousands_symbol.') '.$key;
        }
        return $chart;
    }
}
