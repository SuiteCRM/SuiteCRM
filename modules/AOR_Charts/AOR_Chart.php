<?php
/**
 * Advanced OpenReports, SugarCRM Reporting.
 * @package Advanced OpenReports for SugarCRM
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
 * @author SalesAgility <info@salesagility.com>
 */

#[\AllowDynamicProperties]
class AOR_Chart extends Basic
{
    public const COLOUR_DEFAULTS = "['#1f78b4','#a6cee3','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928','#144c73','#6caed1','#8acf4e','#20641c','#f8514f','#9e1214','#fc9d24','#b35900','#a880bb','#442763','#ffff4d','#733a1a']";
    public $colours;
    public $new_schema = true;
    public $module_dir = 'AOR_Charts';
    public $object_name = 'AOR_Chart';
    public $table_name = 'aor_charts';
    public $importable = true;
    public $disable_row_level_security = true ;

    public $id;
    public $name;
    public $date_entered;
    public $date_modified;
    public $modified_user_id;
    public $modified_by_name;
    public $created_by;
    public $created_by_name;
    public $description;
    public $deleted;
    public $created_by_link;
    public $modified_user_link;

    public $type;
    public $x_field;
    public $y_field;
    public $noDataMessage = "No Results";



    public function __construct()
    {
        parent::__construct();
        $this->colours = self::COLOUR_DEFAULTS;
    }




    public function save_lines(array $post, AOR_Report $bean, $postKey)
    {
        $seenIds = array();
        if (isset($post[$postKey.'id'])) {
            foreach ($post[$postKey . 'id'] as $key => $id) {
                if ($id && $post['record']!='') {
                    $aorChart = BeanFactory::getBean('AOR_Charts', $id);
                } else {
                    $aorChart = BeanFactory::newBean('AOR_Charts');
                }
                $aorChart->name = $post[$postKey . 'title'][$key];
                $aorChart->type = $post[$postKey . 'type'][$key];
                $aorChart->x_field = $post[$postKey . 'x_field'][$key];
                $aorChart->y_field = $post[$postKey . 'y_field'][$key];
                $aorChart->aor_report_id = $bean->id;
                $aorChart->save();
                $seenIds[] = $aorChart->id;
            }
        }
        //Any beans that exist but aren't in $seenIds must have been removed.
        foreach ($bean->get_linked_beans('aor_charts', 'AOR_Charts') as $chart) {
            if (!in_array($chart->id, $seenIds)) {
                $chart->mark_deleted($chart->id);
            }
        }
    }

    private function getValidChartTypes()
    {
        return array('bar','line','pie','radar','rose', 'grouped_bar', 'stacked_bar');
    }


    private function getColour($seed, $rgbArray = false)
    {
        $hash = md5($seed);
        $r = hexdec(substr($hash, 0, 2));
        $g = hexdec(substr($hash, 2, 2));
        $b = hexdec(substr($hash, 4, 2));
        if ($rgbArray) {
            return array('R'=>$r,'G'=>$g,'B'=>$b);
        }
        $highR = $r + 10;
        $highG = $g + 10;
        $highB = $b + 10;
        $main = '#'.str_pad(dechex($r), 2, '0', STR_PAD_LEFT)
            .str_pad(dechex($g), 2, '0', STR_PAD_LEFT)
            .str_pad(dechex($b), 2, '0', STR_PAD_LEFT);
        $highlight = '#'.dechex($highR).dechex($highG).dechex($highB);
        return array('main'=>$main,'highlight'=>$highlight);
    }

    public function buildChartImageBar($chartPicture, $recordImageMap = false)
    {
        $scaleSettings = array("DrawSubTicks" => false, "LabelRotation" => 30, 'MinDivHeight' => 50);
        $chartPicture->drawScale($scaleSettings);
        $chartPicture->drawBarChart(array("RecordImageMap"=>$recordImageMap));
    }

    public function buildChartImagePie($chartPicture, $chartData, $reportData, $imageHeight, $imageWidth, $xName, $recordImageMap)
    {
        $PieChart = new pPie($chartPicture, $chartData);
        $x = 0;
        foreach ($reportData as $row) {
            $PieChart->setSliceColor($x, $this->getColour($row[$xName], true));
            $x++;
        }
        $PieChart->draw2DPie($imageWidth/3, $imageHeight/2, array("Border"=>true,'Radius'=>200,''=>true,"RecordImageMap"=>$recordImageMap));
        $PieChart->drawPieLegend($imageWidth*0.7, $imageHeight/3, array('FontSize'=>10,"FontName"=>"modules/AOR_Charts/lib/pChart/fonts/verdana.ttf",'BoxSize'=>14));
    }

    public function buildChartImageLine($chartPicture, $recordImageMap = false)
    {
        $scaleSettings = array("XMargin"=>10,"YMargin"=>10,"GridR"=>200,"GridG"=>200,"GridB"=>200,'MinDivHeight' => 50,"LabelRotation" => 30);
        $chartPicture->drawScale($scaleSettings);
        $chartPicture->drawLineChart(array("RecordImageMap"=>$recordImageMap));
    }

    public function buildChartImageRadar($chartPicture, $chartData, $recordImageMap)
    {
        $SplitChart = new pRadar();
        $Options = array("LabelPos"=>RADAR_LABELS_HORIZONTAL,"RecordImageMap"=>$recordImageMap);
        $SplitChart->drawRadar($chartPicture, $chartData, $Options);
    }

    public function buildChartImage(array $reportData, array $fields, $asDataURI = true, $generateImageMapId = false)
    {
        global $current_user;
        require_once 'modules/AOR_Charts/lib/pChart/pChart.php';

        if ($generateImageMapId !== false) {
            $generateImageMapId = $current_user->id."-".$generateImageMapId;
        }

        $html = '';
        if (!in_array($this->type, $this->getValidChartTypes())) {
            return $html;
        }
        $x = $fields[$this->x_field];
        $y = $fields[$this->y_field];
        if (!$x || !$y) {
            //Malformed chart object - missing an axis field
            return '';
        }
        $xName = str_replace(' ', '_', (string) $x->label) . $this->x_field;
        $yName = str_replace(' ', '_', (string) $y->label) . $this->y_field;

        $chartData = new pData();
        $chartData->loadPalette("modules/AOR_Charts/lib/pChart/palettes/navy.color", true);
        $labels = array();
        foreach ($reportData as $row) {
            $chartData->addPoints($row[$yName], 'data');
            $chartData->addPoints($row[$xName], 'Labels');
            $labels[] = $row[$xName];
        }

        $chartData->setSerieDescription("Months", "Month");
        $chartData->setAbscissa("Labels");

        $imageHeight = 700;
        $imageWidth = 700;

        $chartPicture = new pImage($imageWidth, $imageHeight, $chartData);
        if ($generateImageMapId) {
            $imageMapDir = create_cache_directory('modules/AOR_Charts/ImageMap/'.$current_user->id.'/');
            $chartPicture->initialiseImageMap($generateImageMapId, IMAGE_MAP_STORAGE_FILE, $generateImageMapId, $imageMapDir);
        }

        $chartPicture->Antialias = true;

        $chartPicture->drawFilledRectangle(0, 0, $imageWidth-1, $imageHeight-1, array("R"=>240,"G"=>240,"B"=>240,"BorderR"=>0,"BorderG"=>0,"BorderB"=>0,));

        $chartPicture->setFontProperties(array("FontName"=>"modules/AOR_Charts/lib/pChart/fonts/verdana.ttf","FontSize"=>14));

        $chartPicture->drawText($imageWidth/2, 20, $this->name, array("R"=>0,"G"=>0,"B"=>0,'Align'=>TEXT_ALIGN_TOPMIDDLE));
        $chartPicture->setFontProperties(array("FontName"=>"modules/AOR_Charts/lib/pChart/fonts/verdana.ttf","FontSize"=>6));

        $chartPicture->setGraphArea(60, 60, $imageWidth-60, $imageHeight-100);

        switch ($this->type) {
            case 'radar':
                $this->buildChartImageRadar($chartPicture, $chartData, !empty($generateImageMapId));
                break;
            case 'pie':
                $this->buildChartImagePie($chartPicture, $chartData, $reportData, $imageHeight, $imageWidth, $xName, !empty($generateImageMapId));
                break;
            case 'line':
                $this->buildChartImageLine($chartPicture, !empty($generateImageMapId));
                break;
            case 'bar':
            default:
                $this->buildChartImageBar($chartPicture, !empty($generateImageMapId));
                break;
        }
        if ($generateImageMapId) {
            $chartPicture->replaceImageMapTitle("data", $labels);
        }
        ob_start();
        $chartPicture->render(null);
        $img = ob_get_clean();
        if ($asDataURI) {
            return 'data:image/png;base64,'.base64_encode($img);
        }
        return $img;
    }

    public function buildChartHTML(array $reportData, array $fields, $index = 0, $chartType = AOR_Report::CHART_TYPE_PCHART, AOR_Field $mainGroupField = null)
    {
        switch ($chartType) {
            case AOR_Report::CHART_TYPE_PCHART:
                return $this->buildChartHTMLPChart($reportData, $fields, $index);
            case AOR_Report::CHART_TYPE_CHARTJS:
                return $this->buildChartHTMLChartJS($reportData, $fields);
            case AOR_Report::CHART_TYPE_RGRAPH:
                return $this->buildChartHTMLRGraph($reportData, $fields, $mainGroupField);
        }
        return '';
    }

    /**
     *
     * @param type $labels
     * @return boolean Returns TRUE if colours successfully changed. FALSE on error and using colour defaults.
     */
    private function generateChartColoursFromLabels($labels)
    {
        if (is_array($labels) && count($labels) > 1) {
            $colours = [];
            foreach ($labels as $onelabel) {
                $hash = md5($onelabel);
                $colours[] = substr($hash, 0, 6);
            }
            $this->colours = "['#" . implode("','#", $colours) . "']";
            return true;
        }
        LoggerManager::getLogger()->warn('Incorrect labels given. Using default colours in charts.');
        $this->colours = self::COLOUR_DEFAULTS;
        return false;
    }

    private function buildChartHTMLRGraph(array $reportData, array $fields, AOR_Field $mainGroupField = null)
    {
        $html = '';
        if (!in_array($this->type, $this->getValidChartTypes())) {
            return $html;
        }
        $x = $fields[$this->x_field];
        $y = $fields[$this->y_field];
        if (!$x || !$y) {
            //Malformed chart object - missing an axis field
            return '';
        }
        $xName = str_replace(' ', '_', (string) $x->label) . $this->x_field;
        $yName = str_replace(' ', '_', (string) $y->label) . $this->y_field;

        $defaultHeight = 500;
        $defaultWidth = 900;

        switch ($this->type) {
            /*
              //Polar was not implemented for the previous library (it is not in the getValidChartTypes method)
              case 'polar':
              $chartFunction = 'PolarArea';
              $data = $this->getPolarChartData($reportData, $xName,$yName);
              $config = $this->getPolarChartConfig();
              break;
             */
            case 'radar':
                $chartFunction = 'Radar';
                $data = $this->getRGraphBarChartData($reportData, $xName, $yName);
                $valid = $this->generateChartColoursFromLabels($data['labels']);
                $config = $this->getRadarChartConfig();
                $chart = $this->getRGraphRadarChart(json_encode($data['data']), json_encode($data['labels']), json_encode($data['tooltips']), $this->name, $this->id, $defaultHeight, $defaultWidth);
                break;
            case 'pie':
                $chartFunction = 'Pie';
                $data = $this->getRGraphBarChartData($reportData, $xName, $yName);
                $valid = $this->generateChartColoursFromLabels($data['labels']);
                $config = $this->getPieChartConfig();
                $chart = $this->getRGraphPieChart(json_encode($data['data']), json_encode($data['labels']), json_encode($data['tooltips']), $this->name, $this->id, $defaultHeight, $defaultWidth);
                break;
            case 'line':
                $chartFunction = 'Line';
                $data = $this->getRGraphBarChartData($reportData, $xName, $yName);
                $valid = $this->generateChartColoursFromLabels($data['labels']);
                $config = $this->getLineChartConfig();
                $chart = $this->getRGraphLineChart(json_encode($data['data']), json_encode($data['labels']), json_encode($data['tooltips']), $this->name, $this->id, $defaultHeight, $defaultWidth);
                break;
            case 'rose':
                $chartFunction = 'Rose';
                $data = $this->getRGraphBarChartData($reportData, $xName, $yName);
                $valid = $this->generateChartColoursFromLabels($data['labels']);
                $config = $this->getRoseChartConfig();
                $chart = $this->getRGraphRoseChart(json_encode($data['data']), json_encode($data['labels']), json_encode($data['tooltips']), $this->name, $this->id, $defaultHeight, $defaultWidth);
                break;
            case 'grouped_bar':
                $chartFunction = 'Grouped bar';
                $data = $this->getRGraphGroupedBarChartData($reportData, $xName, $yName, $mainGroupField);
                $valid = $this->generateChartColoursFromLabels($data['labels']);
                $config = $this->getGroupedBarChartConfig();
                $chart = $this->getRGraphGroupedBarChart(json_encode($data['data']), json_encode($data['labels']), json_encode($data['tooltips']), $this->name, $this->id, $defaultHeight, $defaultWidth, true);
                break;
            case 'stacked_bar':
                $chartFunction = 'Stacked bar';
                $data = $this->getRGraphGroupedBarChartData($reportData, $xName, $yName, $mainGroupField);
                $valid = $this->generateChartColoursFromLabels($data['labels']);
                $config = $this->getStackedBarChartConfig();
                $chart = $this->getRGraphGroupedBarChart(json_encode($data['data']), json_encode($data['labels']), json_encode($data['tooltips']), $this->name, $this->id, $defaultHeight, $defaultWidth, false);
                break;
            case 'bar':
            default:
                $chartFunction = 'Bar';
                $data = $this->getRGraphBarChartData($reportData, $xName, $yName);
                $valid = $this->generateChartColoursFromLabels($data['labels']);
                $config = $this->getBarChartConfig();
                $chart = $this->getRGraphBarChart(json_encode($data['data']), json_encode($data['labels']), json_encode($data['tooltips']), $this->name, $this->id, $defaultHeight, $defaultWidth);
                break;
        }

        if (!$valid) {
            // exception
            LoggerManager::getLogger()->error("Invalid char data labels detected for chart type: $this->type");
        }

        return $chart;
    }

    private function getRGraphRoseChart($chartDataValues, $chartLabelValues, $chartTooltips, $chartName, $chartId, $chartHeight = 400, $chartWidth = 400)
    {
        $dataArray = json_decode((string) $chartDataValues);
        if (!is_array($dataArray)||count($dataArray) < 1) {
            return "<h3>$this->noDataMessage</h3>";
        }
        $html = '';
        $html .= "<canvas id='$chartId' width='$chartWidth' height='$chartHeight' class='resizableCanvas'></canvas>";
        $html .= <<<EOF
        <script>
            new RGraph.Rose({
            id: '$chartId',
            options:{
                //title: '$chartName',
                //labels: $chartLabelValues,
                //textSize:8,
                textSize:10,
                //titleSize:10,
                 tooltips:$chartTooltips,
                tooltipsEvent:'onmousemove',
                tooltipsCssClass: 'rgraph_chart_tooltips_css',
                colors: $this->colours,
                colorsSequential:true
            },
            data: $chartDataValues
        }).draw();
        </script>
EOF;
        return $html;
    }



    //I have not used a parameter for getRGraphBarChart to say whether to group etc, as the future development could be quite different
    //for both, hence the separate methods.  However, the $grouped parameter allows us to specify whether the chart is grouped (true)
    //or stacked (false)
    private function getRGraphGroupedBarChart($chartDataValues, $chartLabelValues, $chartTooltips, $chartName, $chartId, $chartHeight = 400, $chartWidth = 400, $grouped = false)
    {
        $dataArray = json_decode((string) $chartDataValues);
        $grouping = 'grouped'; //$mainGroupField->label; //'grouped';
        if (!$grouped) {
            $grouping='stacked';
        }
        if (!is_array($dataArray)||count($dataArray) < 1) {
            return "<h3>$this->noDataMessage</h3>";
        }
        $html = '';
        $html .= "<canvas id='$chartId' width='$chartWidth' height='$chartHeight' class='resizableCanvas'></canvas>";
        $html .= <<<EOF
        <script>
            new RGraph.Bar({
            id: '$chartId',
            data: $chartDataValues,
            options: {
                grouping:'$grouping',
                backgroundGrid:false,
                backgroundGrid:false,
                gutterBottom: 150,
                gutterTop:25,
                gutterLeft:128,
                title: '$chartName',

                tooltips:$chartTooltips,
                tooltipsEvent:'onmousemove',
                tooltipsCssClass: 'rgraph_chart_tooltips_css',

                shadow:false,
                titleSize:10,
                labels: $chartLabelValues,
                textSize:10,
                textAngle: 90,
                colors: $this->colours,
                ymax:calculateMaxYForSmallNumbers($chartDataValues)
            }
        }).draw();
        </script>
EOF;
        return $html;
    }



    private function getRGraphBarChart($chartDataValues, $chartLabelValues, $chartTooltips, $chartName, $chartId, $chartHeight = 400, $chartWidth = 400)
    {
        $dataArray = json_decode((string) $chartDataValues);
        if (!is_array($dataArray)||count($dataArray) < 1) {
            return "<h3>$this->noDataMessage</h3>";
        }
        $html = '';
        $html .= "<canvas id='$chartId' width='$chartWidth' height='$chartHeight' class='resizableCanvas'></canvas>";
        $html .= <<<EOF
        <script>
            new RGraph.Bar({
            id: '$chartId',
            data: $chartDataValues,
            options: {
            title: '$chartName',
                gutterBottom: 150,
                gutterLeft: 128,
                gutterTop: 25,
                //title: '$chartName',
                labels: $chartLabelValues,
                colorsSequential:true,
                textAngle: 90,
                textSize:10,
                titleSize:10,
                backgroundGrid:false,

                tooltips:$chartTooltips,
                tooltipsCssClass: 'rgraph_chart_tooltips_css',
                tooltipsEvent:'onmousemove',

                colors: $this->colours,
                ymax:calculateMaxYForSmallNumbers($chartDataValues)
            }
        }).draw();
        </script>
EOF;
        return $html;
    }

    private function getRGraphRadarChart($chartDataValues, $chartLabelValues, $chartTooltips, $chartName, $chartId, $chartHeight = 400, $chartWidth = 400)
    {
        $dataArray = json_decode((string) $chartDataValues);
        if (!is_array($dataArray)||count($dataArray) < 1) {
            return "<h3>$this->noDataMessage</h3>";
        }
        $html = '';
        $html .= "<canvas id='$chartId' width='$chartWidth' height='$chartHeight' class='resizableCanvas'></canvas>";
        $html .= <<<EOF
        <script>
            new RGraph.Radar({
            id: '$chartId',
            data: $chartDataValues,
            options: {
                title: '$chartName',
                labels: $chartLabelValues,
                textSize:10,


                tooltips:$chartTooltips,
                tooltipsEvent:'onmousemove',
                tooltipsCssClass: 'rgraph_chart_tooltips_css',

                colors: $this->colours,
                ymax:calculateMaxYForSmallNumbers($chartDataValues)
            }
        }).draw();
        </script>
EOF;
        return $html;
    }

    private function getRGraphPieChart($chartDataValues, $chartLabelValues, $chartTooltips, $chartName, $chartId, $chartHeight = 400, $chartWidth = 400)
    {
        $dataArray = json_decode((string) $chartDataValues);
        if (!is_array($dataArray)||count($dataArray) < 1) {
            return "<h3>$this->noDataMessage</h3>";
        }
        /*
                if($chartHeight > 400)
                    $chartHeight = 400;
                if($chartWidth > 600)
                    $chartWidth = 400;
        */
        $html = '';
        $html .= "<canvas id='$chartId' width='$chartWidth' height='$chartHeight' class='resizableCanvas'></canvas>";
        $html .= <<<EOF
        <script>
            new RGraph.Pie({
            id: '$chartId',
            data: $chartDataValues,
            options: {
                title: '$chartName',
                textSize:10,
                titleSize:10,
                 tooltips:$chartTooltips,
                tooltipsEvent:'onmousemove',
                tooltipsCssClass: 'rgraph_chart_tooltips_css',
                labels: $chartLabelValues,
                colors: $this->colours
            }
        }).draw();
        </script>
EOF;
        return $html;
    }

    private function getRGraphLineChart($chartDataValues, $chartLabelValues, $chartTooltips, $chartName, $chartId, $chartHeight = 400, $chartWidth = 400)
    {
        $dataArray = json_decode((string) $chartDataValues);
        if (!is_array($dataArray)||count($dataArray) < 1) {
            return "<h3>$this->noDataMessage</h3>";
        }
        $html = '';
        $html .= "<canvas id='$chartId' width='$chartWidth' height='$chartHeight' class='resizableCanvas'></canvas>";
        $html .= <<<EOF
        <script>
            new RGraph.Line({
            id: '$chartId',
            data: $chartDataValues,
            options: {
                title: '$chartName',
                gutterBottom: 150,
                //gutterTop:50,
                tickmarks:'encircle',
                textSize:10,
                titleSize:10,
                gutterLeft:128,
                //title: '$chartName',
                labels: $chartLabelValues,

                 tooltips:$chartTooltips,
                tooltipsEvent:'onmousemove',
                tooltipsCssClass: 'rgraph_chart_tooltips_css',

                tickmarks:'circle',

                textAngle: 90,
                //titleSize:10,
                backgroundGrid:false,
                colors: $this->colours,
                ymax:calculateMaxYForSmallNumbers($chartDataValues),
            }
        }).draw();
        </script>
EOF;
        return $html;
    }

    private function buildChartHTMLChartJS(array $reportData, array $fields)
    {
        $html = '';
        if (!in_array($this->type, $this->getValidChartTypes())) {
            return $html;
        }
        $x = $fields[$this->x_field];
        $y = $fields[$this->y_field];
        if (!$x || !$y) {
            //Malformed chart object - missing an axis field
            return '';
        }
        $xName = str_replace(' ', '_', (string) $x->label) . $this->x_field;
        $yName = str_replace(' ', '_', (string) $y->label) . $this->y_field;

        switch ($this->type) {
            case 'radar':
                $chartFunction = 'Radar';
                $data = $this->getRadarChartData($reportData, $xName, $yName);
                $config = $this->getRadarChartConfig();
                break;
            case 'pie':
                $chartFunction = 'Pie';
                $data = $this->getPieChartData($reportData, $xName, $yName);
                $config = $this->getPieChartConfig();
                break;
            case 'line':
                $chartFunction = 'Line';
                $data = $this->getLineChartData($reportData, $xName, $yName);
                $config = $this->getLineChartConfig();
                break;
            case 'bar':
            default:
                $chartFunction = 'Bar';
                $data = $this->getBarChartData($reportData, $xName, $yName);
                $config = $this->getBarChartConfig();
                break;
        }
        $data = json_encode($data);
        $config = json_encode($config);
        $chartId = 'chart'.$this->id;
        $html .= "<h3>{$this->name}</h3>";
        $html .= "<canvas id='{$chartId}' width='400' height='400'></canvas>";
        $html .= <<<EOF
        <script>
        $(document).ready(function(){
            SUGAR.util.doWhen("typeof Chart != 'undefined'", function(){
                var data = {$data};
                var ctx = document.getElementById("{$chartId}").getContext("2d");
                console.log('Creating new chart');
                var config = {$config};
                var chart = new Chart(ctx).{$chartFunction}(data, config);
                var legend = chart.generateLegend();
                $('#{$chartId}').after(legend);
            });
        });
        </script>
EOF;
        return $html;
    }

    private function buildChartHTMLPChart(array $reportData, array $fields, $index = 0)
    {
        $html = '';
        $imgUri = $this->buildChartImage($reportData, $fields, true, $index);
        $img = "<img id='{$this->id}_img' src='{$imgUri}'>";
        $html .= $img;
        $html .= <<<EOF
<script>
SUGAR.util.doWhen("typeof addImage != 'undefined'", function(){
    addImage('{$this->id}_img','{$this->id}_img_map','index.php?module=AOR_Charts&action=getImageMap&to_pdf=1&imageMapId={$index}');
});
</script>
EOF;
        return $html;
    }

    private function getShortenedLabel($label, $maxLabelSize = 20)
    {
        if (strlen((string) $label) > $maxLabelSize) {
            return mb_substr($label, 0, $maxLabelSize).'...';
        }
        return $label;
    }


    private function getRGraphGroupedBarChartData($reportData, $xName, $yName, AOR_Field $mainGroupField = null)
    {


        // get z-axis name

        $zName = null;
        foreach ($reportData[0] as $key => $value) {
            $field = str_replace(' ', '_', is_null($mainGroupField) ? 'no data' : $mainGroupField->label);
            if (preg_match('/^' . $field . '[0-9]+/', (string) $key)) {
                $zName = $key;
                break;
            }
        }



        // get grouped values

        $data = array();
        $tooltips = array();

        $usedKeys = array();
        foreach ($reportData as $key => $row) {
            $filter = $row[$xName];
            foreach ($reportData as $key2 => $row2) {
                if ($row2[$xName] == $filter && !in_array($key, $usedKeys)) {
                    $data      [ $row[$xName]  ]   [] = (float) $row[$yName];
                    $tooltips  [ $row[$xName]  ]   [] = isset($row[$zName]) ? $row[$zName] : null;
                    $usedKeys[] = $key;
                }
            }
        }

        $_data = array();
        foreach ($data as $label => $values) {
            $_data[$label] = [];
            foreach ($values as $key => $value) {
                $_data[$label][$tooltips[$label][$key]] = ($_data[$label][$tooltips[$label][$key]] ?? 0) + 1;
            }
        }

        $data = $_data;


        // make data format for charts

        $_data = array();
        $_labels = array();
        $_tooltips = array();
        foreach ($data as $label => $values) {
            $_labels[] = $this->getShortenedLabel($label);
            $_values = array();
            foreach ($values as $tooltip => $value) {
                $_tooltips[] = $tooltip . " ($value)";
                $_values[] = $value;
            }
            $_data[] = $_values;
        }


        $chart = array(
            'data' => $_data,
            'labels' => $_labels,
            'tooltips' => $_tooltips,
        );

        return $chart;
    }

    private function getRGraphBarChartData($reportData, $xName, $yName)
    {
        $chart = [];
        $chart['labels']=array();
        $chart['data']=array();
        $chart['tooltips']=array();
        foreach ($reportData as $row) {
            $chart['labels'][] = $this->getShortenedLabel($row[$xName]);
            $chart['tooltips'][] = $row[$xName].': '.$row[$yName];
            $chart['data'][] = (float)$row[$yName];
        }
        return $chart;
    }


    private function getBarChartData($reportData, $xName, $yName)
    {
        $data = array();
        $data['labels'] = array();
        $datasetData = array();
        foreach ($reportData as $row) {
            $data['labels'][] = $row[$xName];
            $datasetData[] = $row[$yName];
        }

        $data['datasets'] = array();
        $data['datasets'][] = array(
            'fillColor' => "rgba(151,187,205,0.2)",
            'strokeColor' => "rgba(151,187,205,1)",
            'pointColor' => "rgba(151,187,205,1)",
            'pointStrokeColor' => "#fff",
            'pointHighlightFill' => "#fff",
            'pointHighlightStroke' => "rgba(151,187,205,1)4",
            'data'=>$datasetData);
        return $data;
    }

    private function getLineChartData($reportData, $xName, $yName)
    {
        return $this->getBarChartData($reportData, $xName, $yName);
    }

    private function getBarChartConfig()
    {
        return array();
    }
    private function getLineChartConfig()
    {
        return $this->getBarChartConfig();
    }

    private function getGroupedBarChartConfig()
    {
        return $this->getBarChartConfig();
    }

    private function getStackedBarChartConfig()
    {
        return $this->getBarChartConfig();
    }

    private function getRoseChartConfig()
    {
        return $this->getBarChartConfig();
    }

    private function getRadarChartData($reportData, $xName, $yName)
    {
        return $this->getBarChartData($reportData, $xName, $yName);
    }

    private function getRadarChartConfig()
    {
        return array();
    }

    private function getPieChartConfig()
    {
        $config = array();
        $config['legendTemplate'] = "<ul class=\"<%=name.toLowerCase()%>-legend\"><% for (var i=0; i<segments.length; i++){%><li><span style=\"background-color:<%=segments[i].fillColor%>\">&nbsp;&nbsp;&nbsp;&nbsp;</span>&nbsp;<%if(segments[i].label){%><%=segments[i].label%><%}%></li><%}%></ul>";
        return $config;
    }

    private function getPieChartData($reportData, $xName, $yName)
    {
        $data = array();

        foreach ($reportData as $row) {
            if (!$row[$yName]) {
                continue;
            }
            $colour = $this->getColour($row[$xName]);
            $data[] = array(
                'value' => (int)$row[$yName],
                'label' => $row[$xName],
                'color' => $colour['main'],
                'highlight' => $colour['highlight'],
            );
        }
        return $data;
    }
}
