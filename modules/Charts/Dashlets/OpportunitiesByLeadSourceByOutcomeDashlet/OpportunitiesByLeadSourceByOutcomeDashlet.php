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
class OpportunitiesByLeadSourceByOutcomeDashlet extends DashletGenericChart
{
    public $lsbo_lead_sources = array();
    public $lsbo_ids          = array();

    /**
     * @see DashletGenericChart::$_seedName
     */
    protected $_seedName = 'Opportunities';

    /**
     * @see DashletGenericChart::displayOptions()
     */
    public function displayOptions()
    {
        global $app_list_strings;

        $selected_datax = array();
        if (!empty($this->lsbo_lead_sources) && count($this->lsbo_lead_sources) > 0) {
            foreach ($this->lsbo_lead_sources as $key) {
                $selected_datax[] = $key;
            }
        } else {
            $selected_datax = array_keys($app_list_strings['lead_source_dom']);
        }

        $this->_searchFields['lsbo_lead_sources']['options'] = array_filter($app_list_strings['lead_source_dom']);
        $this->_searchFields['lsbo_lead_sources']['input_name0'] = $selected_datax;

        if (!isset($this->lsbo_ids) || count($this->lsbo_ids) == 0) {
            $this->_searchFields['lsbo_ids']['input_name0'] = array_keys(get_user_array(false));
        }

        return parent::displayOptions();
    }

    /**
     * @see DashletGenericChart::display()
     */
    public function display()
    {
        global $current_user, $sugar_config;

        /*
        require("modules/Charts/chartdefs.php");
        $chartDef = $chartDefs['lead_source_by_outcome'];
        require_once('include/SugarCharts/SugarChartFactory.php');
        $sugarChart = SugarChartFactory::getInstance();
        $sugarChart->is_currency = true;

        $currency_symbol = $sugar_config['default_currency_symbol'];
        if ($current_user->getPreference('currency')){

            $currency = BeanFactory::newBean('Currencies');
            $currency->retrieve($current_user->getPreference('currency'));
            $currency_symbol = $currency->symbol;
        }
        $subtitle = translate('LBL_OPP_SIZE', 'Charts') . " " . $currency_symbol . "1" . translate('LBL_OPP_THOUSANDS', 'Charts');
        $sugarChart->setProperties('', $subtitle, $chartDef['chartType']);
        $sugarChart->base_url = $chartDef['base_url'];
        $sugarChart->group_by = $chartDef['groupBy'];
        $sugarChart->url_params = array();
        if ( count($this->lsbo_ids) > 0 )
            $sugarChart->url_params['assigned_user_id'] = array_values($this->lsbo_ids);
        $sugarChart->getData($this->constuctQuery());
        $sugarChart->data_set = $sugarChart->sortData($sugarChart->data_set, 'lead_source', true, 'sales_stage', true, true);
        $xmlFile = $sugarChart->getXMLFileName($this->id);
        $sugarChart->saveXMLFile($xmlFile, $sugarChart->generateXML());

        return $this->getTitle('<div align="center"></div>') .
            '<div align="center">' . $sugarChart->display($this->id, $xmlFile, '100%', '480', false) . '</div>'. $this->processAutoRefresh();
        */

        $currency_symbol = $sugar_config['default_currency_symbol'];
        if ($current_user->getPreference('currency')) {
            $currency = BeanFactory::newBean('Currencies');
            $currency->retrieve($current_user->getPreference('currency'));
            $currency_symbol = $currency->symbol;
        }
        $subtitle = translate('LBL_OPP_SIZE', 'Charts') . " " . $currency_symbol . "1" . translate('LBL_OPP_THOUSANDS', 'Charts');
        $thousands_symbol = translate('LBL_OPP_THOUSANDS', 'Charts');

        $module = 'Opportunities';
        $action =  'index';
        $query =  'true';
        $searchFormTab = 'advanced_search';
        $groupBy = array( 'lead_source', 'sales_stage' );


        $url_params = array();
        if (isset($this->lsbo_ids) && count($this->lsbo_ids) > 0) {
            $url_params['assigned_user_id'] = array_values($this->lsbo_ids);
        }


        $data = $this->getChartData($this->constructQuery());

        $data = $this->sortData($data, 'lead_source', true, 'sales_stage', true, true);

        $chartReadyData = $this->prepareChartData($data, $currency_symbol, $thousands_symbol);

        $canvasId = 'rGraphOppByLeadSourceByOutcome'.uniqid();
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
        <canvas id='$canvasId'   class='resizableCanvas'  width='$chartWidth' height='$chartHeight'>[No canvas support]</canvas>
             $autoRefresh
         <script>
           var hbar = new RGraph.HBar({
            id: '$canvasId',
            data:$jsonData,
            options: {
                grouping: 'stacked',
                labels: $jsonLabels,
                xlabels:true,
                labelsAbove: true,
                labelsAbovedecimals: 2,
                linewidth: 2,
                eventsClick:allOpportunititesByLeadSourceByOutcomeClick,
                //eventsMousemove:rgraphMouseMove,
                strokestyle: 'white',
                gutterLeft: 150,
                //gutterRight:200,
                backgroundGridVlines: false,
                backgroundGridBorder: false,
                tooltips:$jsonTooltips,
                tooltipsEvent:'mousemove',
                colors:$colours,
                textSize:10,
                key: $jsonKey,
                keyColors: $colours,
                keyBackground:'rgba(255,255,255,0.7)',
                unitsPre:'$currency_symbol',
                unitsPost:'$thousands_symbol',
                //keyPositionX: $canvasId.width - 190,
                keyPositionGutterBoxed: true,
                axisColor: '#ccc',
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
                    font:'Verdana',
                    'size':9,
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

        hbar.set({
    contextmenu: [
        ['Get PNG', RGraph.showPNG],
        null,
        ['Cancel', function () {}]
    ]
});

</script>
EOD;

        return $chart;
    }

    /**
     * @see DashletGenericChart::constructQuery()
     */
    protected function constructQuery()
    {
        $query = "SELECT lead_source,sales_stage,sum(amount_usdollar/1000) as total, ".
            "count(*) as opp_count FROM opportunities ";
        $query .= " WHERE opportunities.deleted=0 ";
        if (isset($this->lsbo_ids) && count($this->lsbo_ids) > 0) {
            $query .= "AND opportunities.assigned_user_id IN ('".implode("','", $this->lsbo_ids)."') ";
        }
        if (isset($this->lsbo_lead_sources) && count($this->lsbo_lead_sources) > 0) {
            $query .= "AND opportunities.lead_source IN ('".implode("','", $this->lsbo_lead_sources)."') ";
        } else {
            $query .= "AND opportunities.lead_source IN ('".implode("','", array_keys($GLOBALS['app_list_strings']['lead_source_dom']))."') ";
        }
        $query .= " GROUP BY sales_stage,lead_source ORDER BY lead_source,sales_stage";

        return $query;
    }

    protected function prepareChartData($data, $currency_symbol, $thousands_symbol)
    {
        $chart = [];
        //Use the  lead_source to categorise the data for the charts
        $chart['labels'] = array();
        $chart['data'] = array();
        //Need to add all elements into the key, as they are stacked (even though the category is not present, the value could be
        $chart['key'] = array();
        $chart['tooltips']= array();

        foreach ($data as $i) {
            $key = $i["lead_source"];
            $keyDom = $i["lead_source_dom_option"];
            $stage = $i["sales_stage"];
            $stageDom = $i["sales_stage_dom_option"];
            if (!in_array($key, $chart['labels'])) {
                $chart['labels'][] = $key;
                $chart['data'][] = array();
            }
            if (!in_array($stage, $chart['key'])) {
                $chart['key'][] = $stage;
            }

            $formattedFloat = (float)number_format((float)$i["total"], 2, '.', '');
            $chart['data'][count($chart['data'])-1][] = $formattedFloat;
            $chart['tooltips'][]="<div><input type='hidden' class='stage' value='$stageDom'><input type='hidden' class='category' value='$keyDom'></div>".$stage.'('.$currency_symbol.$formattedFloat.$thousands_symbol.') '.$key;
        }
        return $chart;
    }
}
