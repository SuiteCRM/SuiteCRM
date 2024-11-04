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
class OpportunitiesByLeadSourceDashlet extends DashletGenericChart
{
    public $pbls_lead_sources = array();
    public $pbls_ids          = array();

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
        if (!empty($this->pbls_lead_sources) && count($this->pbls_lead_sources) > 0) {
            foreach ($this->pbls_lead_sources as $key) {
                $selected_datax[] = $key;
            }
        } else {
            $selected_datax = array_keys($app_list_strings['lead_source_dom']);
        }

        $this->_searchFields['pbls_lead_sources']['options'] = array_filter($app_list_strings['lead_source_dom']);
        $this->_searchFields['pbls_lead_sources']['input_name0'] = $selected_datax;

        if (!isset($this->pbls_ids) || count($this->pbls_ids) == 0) {
            $this->_searchFields['pbls_ids']['input_name0'] = array_keys(get_user_array(false));
        }

        return parent::displayOptions();
    }

    /**
     * @see DashletGenericChart::display()
     */
    public function display()
    {
        global $current_user, $sugar_config;

        $currency_symbol = $sugar_config['default_currency_symbol'];
        if ($current_user->getPreference('currency')) {
            $currency = BeanFactory::newBean('Currencies');
            $currency->retrieve($current_user->getPreference('currency'));
            $currency_symbol = $currency->symbol;
        }
        $thousands_symbol = translate('LBL_OPP_THOUSANDS', 'Charts');
        $data = $this->getChartData($this->constructQuery());
        $chartReadyData = $this->prepareChartData($data, $currency_symbol, $thousands_symbol);
        $canvasId = 'rGraphLeadSource'.uniqid();
        $chartWidth     = 900;
        $chartHeight    = 500;

        $jsonData = json_encode($chartReadyData['data']);
        $jsonKeys = json_encode($chartReadyData['keys']);
        $jsonLabels = json_encode($chartReadyData['labels']);
        $jsonLabelsAndValues = json_encode($chartReadyData['labelsAndValues']);

        $autoRefresh = $this->processAutoRefresh();

        $module = 'Opportunities';
        $action = 'index';
        $query  ='true';
        $searchFormTab ='advanced_search';

        $colours = "['#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928','#8080ff','#c03f80']";

        if (!is_array($chartReadyData['data'])||count($chartReadyData['data']) < 1) {
            return "<h3 class='noGraphDataPoints'>$this->noDataMessage</h3>";
        }

        //<canvas id='$canvasId' width='$chartWidth' height='$chartHeight' class='resizableCanvas' style='width: 100%;'>[No canvas support]</canvas>
        //<canvas id='$canvasId' width=canvas.width height=canvas.width class='resizableCanvas'>[No canvas support]</canvas>
        $chart = <<<EOD

<canvas id='$canvasId' width='$chartWidth' height='$chartHeight'  class='resizableCanvas' >[No canvas support]</canvas>

        <input type='hidden' class='module' value='$module' />
        <input type='hidden' class='action' value='$action' />
        <input type='hidden' class='query' value='$query' />
        <input type='hidden' class='searchFormTab' value='$searchFormTab' />
        $autoRefresh
        <script>
           window["chartHBarKeys$canvasId"] = $jsonKeys;
           var pie = new RGraph.Pie({
                id: '$canvasId',
                data: $jsonData,
                options: {
                strokestyle: '#e8e8e8',
                linewidth: 2,
                eventsMousemove:rgraphMouseMove,
                eventsClick:opportunitiesByLeadSourceDashletClick,
                shadowBlur: 5,
                tooltips:$jsonLabels,
                tooltipsEvent:'mousemove',
                shadowOffsetx: 5,
                shadowOffsety: 5,
                shadowColor: '#aaa',
                centerx:true,
                key: $jsonLabelsAndValues,
                labels:$jsonLabels,
                keyPosition:'graph',
                keyPositionX:0,
                keyBackground:'rgba(255,255,255,0.7)',
                colors:$colours,
                textSize:10,
                tooltipsCssClass: 'rgraph_chart_tooltips_css',
                keyColors:$colours
                //keyInteractive: true
                }
            }).draw();

            pie.set({
    contextmenu: [
        ['Get PNG', RGraph.showPNG],
        null,
        ['Cancel', function () {}]
    ]
});
        </script>
EOD;

        return $chart;
        /*
        require("modules/Charts/chartdefs.php");
        $chartDef = $chartDefs['pipeline_by_lead_source'];

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
        if ( count($this->pbls_ids) > 0 )
            $sugarChart->url_params['assigned_user_id'] = array_values($this->pbls_ids);
        $sugarChart->getData($this->constructQuery());
        $sugarChart->data_set = $sugarChart->sortData($sugarChart->data_set, 'lead_source', true);
        $xmlFile = $sugarChart->getXMLFileName($this->id);
        $sugarChart->saveXMLFile($xmlFile, $sugarChart->generateXML());

        return $this->getTitle('<div align="center"></div>') .
            '<div align="center">' . $sugarChart->display($this->id, $xmlFile, '100%', '480', false) . '</div>'. $this->processAutoRefresh();
        */
    }

    public function getChartData($query)
    {
        global $app_list_strings;
        $db = DBManagerFactory::getInstance();
        $dataSet = [];
        $result = $db->query($query);

        $row = $db->fetchByAssoc($result);

        while ($row != null) {
            if (isset($row['lead_source']) && $app_list_strings['lead_source_dom'][$row['lead_source']]) {
                $row['lead_source_key'] = $row['lead_source'];
                $row['lead_source'] = $app_list_strings['lead_source_dom'][$row['lead_source']];
            }
            $dataSet[] = $row;
            $row = $db->fetchByAssoc($result);
        }
        return $dataSet;
    }

    protected function prepareChartData($data, $currency_symbol, $thousands_symbol)
    {
        $chart = [];
        //return $data;
        $chart['labels'] = [];
        $chart['data'] = [];
        $chart['keys'] = [];
        $total = 0;
        foreach ($data as $i) {
            if (!isset($i['lead_source_key'])){
                $i['lead_source_key'] = $i['lead_source'];
            }
            $chart['labelsAndValues'][] = $i['lead_source'] . ' (' . $currency_symbol . (int)$i['total'] . $thousands_symbol . ')';
            //$chart['labelsAndValues'][]=$currency_symbol.(int)$i['total'].$thousands_symbol;
            $chart['labels'][] = $i['lead_source'];
            $chart['keys'][] = $i['lead_source_key'];
            $chart['data'][] = (int)$i['total'];
            $total += (int)$i['total'];
        }
        $chart['total'] = $total;
        return $chart;
    }

    /**
     * @see DashletGenericChart::constructQuery()
     */
    protected function constructQuery()
    {
        $query = "SELECT lead_source,sum(amount_usdollar/1000) as total,count(*) as opp_count ".
            "FROM opportunities ";
        $query .= "WHERE opportunities.deleted=0 ";
        if (isset($this->pbls_ids) && count($this->pbls_ids) > 0) {
            $query .= "AND opportunities.assigned_user_id IN ('".implode("','", $this->pbls_ids)."') ";
        }
        if (isset($this->pbls_lead_sources) && count($this->pbls_lead_sources) > 0) {
            $query .= "AND opportunities.lead_source IN ('".implode("','", $this->pbls_lead_sources)."') ";
        } else {
            $query .= "AND opportunities.lead_source IN ('".implode("','", array_keys($GLOBALS['app_list_strings']['lead_source_dom']))."') ";
        }
        $query .= "GROUP BY lead_source ORDER BY total DESC";

        return $query;
    }
}
