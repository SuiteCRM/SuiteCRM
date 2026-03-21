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




require_once("include/SugarCharts/SugarChart.php");

class JsChart extends SugarChart
{
    protected $ss;
    public $xmlFile;
    public $jsonFilename;
    public $chartId;
    public $width;
    public $height;
    public $chartType;

    public function __construct()
    {
        parent::__construct();
    }

    public function isSupported($chartType)
    {
        $charts = array(
            "stacked group by chart",
            "group by chart",
            "bar chart",
            "horizontal group by chart",
            "horizontal",
            "horizontal bar chart",
            "pie chart",
            "gauge chart",
            "funnel chart 3D",
            "line chart",
        );

        if (in_array($chartType, $charts)) {
            return true;
        } else {
            return false;
        }
    }

    public function tab($str, $depth)
    {
        // $str = preg_replace('/(<\w+>)(.*)(<\/\w+>)/e', "'\\1'.htmlentities(from_html('\\2')).'\\3'", $str);
        return str_repeat("\t", $depth) . $str . "\n";
    }

    public function display($name, $xmlFile, $width='320', $height='480', $resize=false)
    {
        $this->chartId = $name;
        $this->height = $height;
        $this->width = $width;
        $this->xmlFile = $xmlFile;
        $this->chartType = $this->chart_properties['type'];

        $style = array();
        $chartConfig = array();
        try {
            $xmlStr = $this->processXML($this->xmlFile);
            $json = $this->buildJson($xmlStr);
        } catch (Exception $e) {
            $GLOBALS['log']->fatal("Unable to return chart data, invalid xml for file {$this->xmlFile}");
            return '';
        }
        $this->saveJsonFile($json);
        $this->ss->assign("chartId", $this->chartId);
        $this->ss->assign("filename", $this->jsonFilename);
        global $mod_strings, $app_strings;
        if (isset($mod_strings['LBL_REPORT_SHOW_CHART'])) {
            $this->ss->assign("showchart", $mod_strings['LBL_REPORT_SHOW_CHART']);
        }

        $dimensions = $this->getChartDimensions($xmlStr);
        $this->ss->assign("width", $dimensions['width']);
        $this->ss->assign("height", $dimensions['height']);
        $config = $this->getConfigProperties();
        $style['gridLineColor'] = str_replace("0x", "#", $config->gridLines);
        $style['font-family'] = $config->labelFontFamily;
        $style['color'] = str_replace("0x", "#", $config->labelFontColor);
        $this->ss->assign("css", $style);
        foreach ($this->getChartConfigParams($xmlStr) as $key => $value) {
            $chartConfig[$key] = $value;
        }
        $chartConfig['imageExportType'] = $this->image_export_type;
        $this->ss->assign("config", $chartConfig);
        if ($json == "No Data") {
            $this->ss->assign("error", $app_strings['LBL_NO_DATA']);
        }

        if (!$this->isSupported($this->chartType)) {
            $this->ss->assign("error", "Unsupported Chart Type");
        }
    }


    public function getDashletScript($id, $xmlFile="")
    {
        global $sugar_config, $current_user, $current_language;
        $this->id = $id;
        $this->chartId = $id;
        $this->xmlFile = (!$xmlFile) ? sugar_cached("xml/".$current_user->getUserPrivGuid()."_{$this->id}.xml") : $xmlFile;


        $style = array();
        $chartConfig = array();
        $this->ss->assign("chartId", $this->chartId);
        $this->ss->assign("filename", str_replace(".xml", ".js", $this->xmlFile));
        $config = $this->getConfigProperties();
        $style['gridLineColor'] = str_replace("0x", "#", $config->gridLines);
        $style['font-family'] = $config->labelFontFamily;
        $style['color'] = str_replace("0x", "#", $config->labelFontColor);
        $this->ss->assign("css", $style);
        $xmlStr = $this->processXML($this->xmlFile);
        foreach ($this->getChartConfigParams($xmlStr) as $key => $value) {
            $chartConfig[$key] = $value;
        }

        $chartConfig['imageExportType'] = $this->image_export_type;
        $this->ss->assign("config", $chartConfig);
    }

    public function chartArray($chartsArray)
    {
        $customChartsArray = array();
        $style = array();
        $chartConfig = array();
        foreach ($chartsArray as $id => $data) {
            $customChartsArray[$id] = array();
            $customChartsArray[$id]['chartId'] = $id;
            $customChartsArray[$id]['filename'] = str_replace(".xml", ".js", $data['xmlFile']);
            $customChartsArray[$id]['width'] = $data['width'];
            $customChartsArray[$id]['height'] = $data['height'];

            $config = $this->getConfigProperties();
            $style['gridLineColor'] = str_replace("0x", "#", $config->gridLines);
            $style['font-family'] = (string)$config->labelFontFamily;
            $style['color'] = str_replace("0x", "#", $config->labelFontColor);
            $customChartsArray[$id]['css'] = $style;
            $xmlStr = $this->processXML($data['xmlFile']);
            $xml = new SimpleXMLElement($xmlStr);
            $params = $this->getChartConfigParams($xmlStr);
            $customChartsArray[$id]['supported'] = ($this->isSupported($xml->properties->type)) ? "true" : "false";
            foreach ($params as $key => $value) {
                $chartConfig[$key] = $value;
            }
            $chartConfig['imageExportType'] = $this->image_export_type;
            $customChartsArray[$id]['chartConfig'] = $chartConfig;
        }

        return $customChartsArray;
    }

    public function getChartConfigParams($xmlStr)
    {
        $xml = new SimpleXMLElement($xmlStr);

        $chartType = $xml->properties->type;
        if ($chartType == "pie chart") {
            return array("pieType" => "basic","tip" => "name","chartType" => "pieChart");
        } elseif ($chartType == "line chart") {
            return array("lineType" => "basic","tip" => "name","chartType" => "lineChart");
        } elseif ($chartType == "funnel chart 3D") {
            return array("funnelType" => "basic","tip" => "name","chartType" => "funnelChart");
        } elseif ($chartType == "gauge chart") {
            return array("gaugeType" => "basic","tip" => "name","chartType" => "gaugeChart");
        } elseif ($chartType == "stacked group by chart") {
            return array("orientation" => "vertical","barType" => "stacked","tip" => "name","chartType" => "barChart");
        } elseif ($chartType == "group by chart") {
            return array("orientation" => "vertical", "barType" => "grouped", "tip" => "title","chartType" => "barChart");
        } elseif ($chartType == "bar chart") {
            return array("orientation" => "vertical", "barType" => "basic", "tip" => "label","chartType" => "barChart");
        } elseif ($chartType == "horizontal group by chart") {
            return array("orientation" => "horizontal", "barType" => "stacked", "tip" => "name","chartType" => "barChart");
        } elseif ($chartType == "horizontal bar chart" || "horizontal") {
            return array("orientation" => "horizontal","barType" => "basic","tip" => "label","chartType" => "barChart");
        } else {
            return array("orientation" => "vertical","barType" => "stacked","tip" => "name","chartType" => "barChart");
        }
    }
    public function getChartDimensions($xmlStr)
    {
        if ($this->getNumNodes($xmlStr) > 9 && $this->chartType != "pie chart") {
            if ($this->chartType == "horizontal group by chart" || $this->chartType == "horizontal bar chart") {
                $height = ($this->getNumNodes($xmlStr) * 60) + 100;
                return array("width"=>$this->width, "height"=>($height));
            } else {
                return array("width"=>($this->width * 2), "height"=>$this->height);
            }
        } else {
            return array("width"=>"100%", "height"=>$this->height);
        }
    }

    public function checkData($xmlstr)
    {
        $xml = new SimpleXMLElement($xmlstr);
        if (count($xml->data->group) > 0) {
            return true;
        } else {
            return false;
        }
    }

    public function getNumNodes($xmlstr)
    {
        $xml = new SimpleXMLElement($xmlstr);
        return count($xml->data->group);
    }

    public function buildProperties($xmlstr)
    {
        $content = $this->tab("\"properties\": [\n", 1);
        $properties = array();
        $xml = new SimpleXMLElement($xmlstr);
        foreach ($xml->properties->children() as $property) {
            $properties[] = $this->tab("\"".$property->getName()."\":"."\"".$this->processSpecialChars($property)."\"", 2);
        }
        $content .= $this->tab("{\n", 1);
        $content .= implode(",\n", $properties)."\n";
        $content .= $this->tab("}\n", 1);
        $content .= $this->tab("],\n", 1);
        return $content;
    }

    public function buildLabelsBarChartStacked($xmlstr)
    {
        $content = $this->tab("\"label\": [\n", 1);
        $labels = array();
        $xml = new SimpleXMLElement($xmlstr);
        foreach ($xml->data->group[0]->subgroups->group as $group) {
            $labels[] = $this->tab("\"".$this->processSpecialChars($group->title)."\"", 2);
        }
        $content .= implode(",\n", $labels)."\n";
        $content .= $this->tab("],\n", 1);
        return $content;
    }

    public function buildLabelsBarChart($xmlstr)
    {

        // fix for bug42326: if there is label data that is deeper than that of a normal bar chart, render the labels
        //    like a stacked bar chart, this applies when a stacked chart has been converted in templates_chart.php to a bar chart due to it
        //    having nothing but single value columns
        $xml = new SimpleXMLElement($xmlstr);
        if (count($xml->data->group[0]->subgroups->group) > 0) {
            return $this->buildLabelsBarChartStacked($xmlstr);
        }

        $content = $this->tab("\"label\": [\n", 1);
        $labels = array();

        foreach ($xml->data->group as $group) {
            $labels[] = $this->tab("\"".$this->processSpecialChars($group->title)."\"", 2);
        }
        $labelStr = implode(",\n", $labels)."\n";
        $content .= $labelStr;
        $content .= $this->tab("],\n", 1);
        return $content;
    }

    public function buildDataBarChartStacked($xmlstr)
    {
        $content = $this->tab("\"values\": [\n", 1);
        $data = array();
        $xml = new SimpleXMLElement($xmlstr);
        foreach ($xml->data->group as $group) {
            $groupcontent = $this->tab("{\n", 1);
            $groupcontent .= $this->tab("\"label\": \"".$this->processSpecialChars($group->title)."\",\n", 2);
            $groupcontent .= $this->tab("\"gvalue\": \"{$group->value}\",\n", 2);
            $groupcontent .= $this->tab("\"gvaluelabel\": \"{$group->label}\",\n", 2);
            if (!empty($group->id)) {
                $groupcontent .= $this->tab("\"id\": \"{$group->id}\",\n", 2);
            }
            $subgroupValues = array();
            $subgroupValueLabels = array();
            $subgroupLinks = array();
            foreach ($group->subgroups->group as $subgroups) {
                $subgroupValues[] = $this->tab(($subgroups->value == "NULL") ? 0 : $subgroups->value, 3);
                $subgroupValueLabels[] = $this->tab("\"".$this->processSpecialChars($subgroups->label)."\"", 3);
                $subgroupLinks[] = $this->tab("\"".$subgroups->link."\"", 3);
            }
            $subgroupValuesStr = implode(",\n", $subgroupValues)."\n";
            $subgroupValueLabelsStr = implode(",\n", $subgroupValueLabels)."\n";
            $subgroupLinksStr = implode(",\n", $subgroupLinks)."\n";

            $groupcontent .= $this->tab("\"values\": [\n".$subgroupValuesStr, 2);
            $groupcontent .= $this->tab("],\n", 2);
            $groupcontent .= $this->tab("\"valuelabels\": [\n".$subgroupValueLabelsStr, 2);
            $groupcontent .= $this->tab("],\n", 2);
            $groupcontent .= $this->tab("\"links\": [\n".$subgroupLinksStr, 2);
            $groupcontent .= $this->tab("]\n", 2);
            $groupcontent .= $this->tab("}", 1);
            $data[] = $groupcontent;
        }
        $content .= implode(",\n", $data)."\n";
        $content .= $this->tab("]", 1);
        return $content;
    }

    public function buildDataBarChartGrouped($xmlstr)
    {
        $content = $this->tab("\"values\": [\n", 1);
        $data = array();
        $xml = new SimpleXMLElement($xmlstr);
        foreach ($xml->data->group as $group) {
            $groupcontent = $this->tab("{\n", 1);
            $groupcontent .= $this->tab("\"label\": \"".$this->processSpecialChars($group->title)."\",\n", 2);
            $groupcontent .= $this->tab("\"gvalue\": \"{$group->value}\",\n", 2);
            $groupcontent .= $this->tab("\"gvaluelabel\": \"{$group->label}\",\n", 2);
            if (!empty($group->id)) {
                $groupcontent .= $this->tab("\"id\": \"{$group->id}\",\n", 2);
            }
            $subgroupValues = array();
            $subgroupValueLabels = array();
            $subgroupLinks = array();
            $subgroupTitles = array();
            foreach ($group->subgroups->group as $subgroups) {
                $subgroupValues[] = $this->tab(($subgroups->value == "NULL") ? 0 : $subgroups->value, 3);
                $subgroupValueLabels[] = $this->tab("\"".$subgroups->label."\"", 3);
                $subgroupLinks[] = $this->tab("\"".$subgroups->link."\"", 3);
                $subgroupTitles[] = $this->tab("\"".$this->processSpecialChars($subgroups->title)."\"", 3);
            }
            $subgroupValuesStr = implode(",\n", $subgroupValues)."\n";
            $subgroupValueLabelsStr = implode(",\n", $subgroupValueLabels)."\n";
            $subgroupLinksStr = implode(",\n", $subgroupLinks)."\n";
            $subgroupTitlesStr = implode(",\n", $subgroupTitles)."\n";

            $groupcontent .= $this->tab("\"values\": [\n".$subgroupValuesStr, 2);
            $groupcontent .= $this->tab("],\n", 2);
            $groupcontent .= $this->tab("\"valuelabels\": [\n".$subgroupValueLabelsStr, 2);
            $groupcontent .= $this->tab("],\n", 2);
            $groupcontent .= $this->tab("\"links\": [\n".$subgroupLinksStr, 2);
            $groupcontent .= $this->tab("],\n", 2);
            $groupcontent .= $this->tab("\"titles\": [\n".$subgroupTitlesStr, 2);
            $groupcontent .= $this->tab("]\n", 2);
            $groupcontent .= $this->tab("}", 1);
            $data[] = $groupcontent;
        }
        $content .= implode(",\n", $data)."\n";
        $content .= $this->tab("]", 1);
        return $content;
    }

    public function buildDataBarChart($xmlstr)
    {
        $content = $this->tab("\"values\": [\n", 1);
        $data = array();
        $xml = new SimpleXMLElement($xmlstr);
        $groupcontent = "";
        $groupcontentArr = array();

        foreach ($xml->data->group as $group) {
            $groupcontent = $this->tab("{\n", 1);
            $groupcontent .= $this->tab("\"label\": [\n", 2);
            $groupcontent .= $this->tab("\"".$this->processSpecialChars($group->title)."\"\n", 3);
            $groupcontent .= $this->tab("],\n", 2);
            $groupcontent .= $this->tab("\"values\": [\n", 2);
            $groupcontent .= $this->tab(($group->value == "NULL") ? 0 : $group->value."\n", 3);
            $groupcontent .= $this->tab("],\n", 2);
            if (!empty($group->id)) {
                $groupcontent .= $this->tab("\"id\": \"{$group->id}\",\n", 2);
            }
            if ($group->label) {
                $groupcontent .= $this->tab("\"valuelabels\": [\n", 2);
                $groupcontent .= $this->tab("\"{$group->label}\"\n", 3);
                $groupcontent .= $this->tab("],\n", 2);
            }
            $groupcontent .= $this->tab("\"links\": [\n", 2);
            $groupcontent .= $this->tab("\"{$group->link}\"\n", 3);
            $groupcontent .= $this->tab("]\n", 2);
            $groupcontent .= $this->tab("}", 1);
            $groupcontentArr[] = $groupcontent;
        }
        $content .= implode(",\n", $groupcontentArr)."\n";
        $content .= $this->tab("]", 1);
        return $content;
    }

    public function buildLabelsPieChart($xmlstr)
    {
        $content = $this->tab("\"label\": [\n", 1);
        $labels = array();
        $xml = new SimpleXMLElement($xmlstr);
        foreach ($xml->data->group as $group) {
            $labels[] = $this->tab("\"".$this->processSpecialChars($group->title)."\"", 2);
        }
        $labelStr = implode(",\n", $labels)."\n";
        $content .= $labelStr;
        $content .= $this->tab("],\n", 1);
        return $content;
    }


    public function buildDataPieChart($xmlstr)
    {
        $content = $this->tab("\"values\": [\n", 1);
        $data = array();
        $xml = new SimpleXMLElement($xmlstr);
        $groupcontent = "";
        $groupcontentArr = array();

        foreach ($xml->data->group as $group) {
            $groupcontent = $this->tab("{\n", 1);
            $groupcontent .= $this->tab("\"label\": [\n", 2);
            $groupcontent .= $this->tab("\"".$this->processSpecialChars($group->title)."\"\n", 3);
            $groupcontent .= $this->tab("],\n", 2);
            $groupcontent .= $this->tab("\"values\": [\n", 2);
            $groupcontent .= $this->tab("{$group->value}\n", 3);
            $groupcontent .= $this->tab("],\n", 2);
            $groupcontent .= $this->tab("\"valuelabels\": [\n", 2);
            $groupcontent .= $this->tab("\"{$group->label}\"\n", 3);
            $groupcontent .= $this->tab("],\n", 2);
            $groupcontent .= $this->tab("\"links\": [\n", 2);
            $groupcontent .= $this->tab("\"{$group->link}\"\n", 3);
            $groupcontent .= $this->tab("]\n", 2);
            $groupcontent .= $this->tab("}", 1);
            $groupcontentArr[] = $groupcontent;
        }


        $content .= implode(",\n", $groupcontentArr)."\n";
        $content .= $this->tab("\n]", 1);
        return $content;
    }

    public function buildLabelsGaugeChart($xmlstr)
    {
        $content = $this->tab("\"label\": [\n", 1);
        $labels = array();
        $xml = new SimpleXMLElement($xmlstr);
        foreach ($xml->data->group as $group) {
            $labels[] = $this->tab("\"".$this->processSpecialChars($group->title)."\"", 2);
        }
        $labelStr = implode(",\n", $labels)."\n";
        $content .= $labelStr;
        $content .= $this->tab("],\n", 1);
        return $content;
    }

    public function buildDataGaugeChart($xmlstr)
    {
        $content = $this->tab("\"values\": [\n", 1);
        $data = array();
        $xml = new SimpleXMLElement($xmlstr);
        foreach ($xml->data->group as $group) {
            $groupcontent = $this->tab("{\n", 1);
            $groupcontent .= $this->tab("\"label\": \"".$this->processSpecialChars($group->title)."\",\n", 2);
            $groupcontent .= $this->tab("\"gvalue\": \"{$group->value}\",\n", 2);
            $finalComma = ($group->title != "GaugePosition") ? "," : "";
            $groupcontent .= $this->tab("\"gvaluelabel\": \"{$group->label}\"{$finalComma}\n", 2);
            $subgroupTitles = array();
            $subgroupValues = array();
            $subgroupValueLabels = array();
            $subgroupLinks = array();

            if (is_object($group->subgroups->group)) {
                foreach ($group->subgroups->group as $subgroups) {
                    $subgroupTitles[] = $this->tab("\"".$subgroups->title."\"", 3);
                    //$subgroupValues[] = $this->tab($subgroups->value,3);
                    $subgroupValues[] = $subgroups->value;
                    $subgroupValueLabels[] = $this->tab("\"".$subgroups->label."\"", 3);
                    $subgroupLinks[] = $this->tab("\"".$subgroups->link."\"", 3);
                }
                $subgroupTitlesStr = implode(",\n", $subgroupTitles)."\n";
                $subgroupValuesStr = implode(",\n", $subgroupValues)."\n";
                $subgroupValueLabelsStr = implode(",\n", $subgroupValueLabels)."\n";
                $subgroupLinksStr = implode(",\n", $subgroupLinks)."\n";

                //$groupcontent .= $this->tab("\"labels\": [\n".$subgroupTitlesStr,2);
                //$groupcontent .= $this->tab("],\n",2);
                $val = ((int)$subgroupValues[1] == (int)$subgroupValues[0]) ? $this->tab($subgroupValues[1], 3)."\n" : $this->tab($subgroupValues[1] - $subgroupValues[0], 3)."\n";

                $groupcontent .= $this->tab("\"values\": [\n".$val, 2);
                $groupcontent .= $this->tab("],\n", 2);
                $groupcontent .= $this->tab("\"valuelabels\": [\n".$subgroupValueLabelsStr, 2);
                $groupcontent .= $this->tab("]\n", 2);
                //$groupcontent .= $this->tab("\"links\": [\n".$subgroupLinksStr,2);
                //$groupcontent .= $this->tab("]\n",2);
            }

            $groupcontent .= $this->tab("}", 1);
            $data[] = $groupcontent;
        }

        $content .= implode(",\n", $data)."\n";


        $content .= $this->tab("]", 1);
        return $content;
    }


    public function getConfigProperties()
    {
        $path = SugarThemeRegistry::current()->getImageURL('sugarColors.xml', false);

        if (!file_exists($path)) {
            $GLOBALS['log']->debug("Cannot open file ($path)");
        }
        $xmlstr = file_get_contents($path);
        $xml = new SimpleXMLElement($xmlstr);
        return $xml->charts;
    }

    public function buildChartColors()
    {
        $content = $this->tab("\"color\": [\n", 1);
        $colorArr = array();
        $xml = $this->getConfigProperties();
        $colors = ($this->chartType == "gauge chart") ? $xml->gaugeChartElementColors->color : $xml->chartElementColors->color;
        foreach ($colors as $color) {
            $colorArr[] = $this->tab("\"".str_replace("0x", "#", $color)."\"", 2);
        }
        $content .= implode(",\n", $colorArr)."\n";
        $content .= $this->tab("],\n", 1);

        return $content;
    }

    public function buildJson($xmlstr)
    {
        if ($this->checkData($xmlstr)) {
            $content = "{\n";
            if ($this->chartType == "pie chart" || $this->chartType == "funnel chart 3D") {
                $content .= $this->buildProperties($xmlstr);
                $content .= $this->buildLabelsPieChart($xmlstr);
                $content .= $this->buildChartColors();
                $content .= $this->buildDataPieChart($xmlstr);
            } elseif ($this->chartType == "gauge chart") {
                $content .= $this->buildProperties($xmlstr);
                $content .= $this->buildLabelsGaugeChart($xmlstr);
                $content .= $this->buildChartColors();
                $content .= $this->buildDataGaugeChart($xmlstr);
            } elseif ($this->chartType == "horizontal bar chart" || $this->chartType == "bar chart") {
                $content .= $this->buildProperties($xmlstr);
                $content .= $this->buildLabelsBarChart($xmlstr);
                $content .= $this->buildChartColors();
                $content .= $this->buildDataBarChart($xmlstr);
            } elseif ($this->chartType == "group by chart") {
                $content .= $this->buildProperties($xmlstr);
                $content .= $this->buildLabelsBarChartStacked($xmlstr);
                $content .= $this->buildChartColors();
                $content .= $this->buildDataBarChartGrouped($xmlstr);
            } else {
                $content .= $this->buildProperties($xmlstr);
                $content .= $this->buildLabelsBarChartStacked($xmlstr);
                $content .= $this->buildChartColors();
                $content .= $this->buildDataBarChartStacked($xmlstr);
            }
            $content .= "\n}";
            return $content;
        } else {
            return "No Data";
        }
    }

    public function buildHTMLLegend($xmlFile)
    {
        $xmlstr = $this->processXML($xmlFile);
        $xml = new SimpleXMLElement($xmlstr);
        $this->chartType = $xml->properties->type;
        $html = "<table align=\"left\" cellpadding=\"2\" cellspacing=\"2\">";

        if (
            $this->chartType == "group by chart"
            || $this->chartType == "horizontal group by chart"
            || $this->chartType == 'line chart'
            || $this->chartType == 'stacked group by chart'
        ) {
            $groups = $xml->data->group[0]->subgroups->group;
            $items = (count($xml->data->group[0]->subgroups->group) <= 5) ? 5 : count($xml->data->group[0]->subgroups->group);
        } else {
            if ($this->chartType == "funnel chart 3D") {
                // reverse legend
                $groups = array();
                foreach ($xml->data->group as $group) {
                    array_unshift($groups, $group);
                }
            } else {
                $groups = $xml->data->group;
            }
            $items = (count($xml->data->group) <= 5) ? 5 : count($xml->data->group);
        }

        $rows = ceil($items/5);
        $fullItems = $rows * 5;
        $remainder = ($items < $fullItems) ? $fullItems - $items : 0;
        $i = 0;
        $x = 0;


        $colorArr = array();
        $xmlColors = $this->getConfigProperties();
        $colors = ($this->chartType == "gauge chart") ? $xmlColors->gaugeChartElementColors->color : $xmlColors->chartElementColors->color;

        foreach ($colors as $color) {
            $colorArr[] = str_replace("0x", "#", $color);
        }

        $isTrClosed = false;
        foreach ($groups as $group) {
            if ($i == 5) {
                $i = 0;
            }
            $html .= ($i == 0) ? "<tr>" : "";
            $html .= "<td width=\"50\">";
            $html .= "<div style=\"background-color:".$colorArr[$x].";\">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</div>";
            $html .= "</td>";
            $html .= "<td>";
            $html .= $group->title;
            $html .= "</td>";
            $html .= ($x+1 == $items) ? "<td colspan=".($remainder*2)."></td>" : "";
            if ($i == 4) {
                $html .= "</tr>";
                $isTrClosed = true;
            } else {
                $isTrClosed = false;
            }
            $x++;
            $i++;
        }
        if ($isTrClosed == false) {
            $html .= '</tr>';
        }

        $html .= "</table>";
        return $html;
    }

    public function saveJsonFile($jsonContents)
    {
        $this->jsonFilename = str_replace(".xml", ".js", $this->xmlFile);
        //$jsonContents = $GLOBALS['locale']->translateCharset($jsonContents, 'UTF-8', 'UTF-16LE');

        // open file
        if (!$fh = sugar_fopen($this->jsonFilename, 'w')) {
            $GLOBALS['log']->debug("Cannot open file ($this->jsonFilename)");
            return;
        }

        // write the contents to the file
        if (fwrite($fh, $jsonContents) === false) {
            $GLOBALS['log']->debug("Cannot write to file ($this->jsonFilename)");
            return false;
        }

        $GLOBALS['log']->debug("Success, wrote ($jsonContents) to file ($this->jsonFilename)");

        fclose($fh);
        return true;
    }

    public function get_image_cache_file_name($xmlFile, $ext = ".png")
    {
        $filename = str_replace("/xml/", "/images/", str_replace(".xml", $ext, $xmlFile));

        return $filename;
    }


    public function getXMLChartProperties($xmlStr)
    {
        $props = array();
        $xml = new SimpleXMLElement($xmlStr);
        foreach ($xml->properties->children() as $properties) {
            $props[$properties->getName()] = $properties;
        }
        return $props;
    }

    public function processSpecialChars($str)
    {
        return addslashes(html_entity_decode($str, ENT_QUOTES));
    }

    public function processXML($xmlFile)
    {
        if (!file_exists($xmlFile)) {
            $GLOBALS['log']->debug("Cannot open file ($xmlFile)");
        }

        $pattern = array();
        $replacement = array();
        $content = file_get_contents($xmlFile);
        $content = $GLOBALS['locale']->translateCharset($content, 'UTF-16LE', 'UTF-8');

        /*
        * Fix to Bug 1934 https://suitecrm.com/forum/bug-tracker/1934-unable-to-return-chart-data-invalid-xml-for-file-cache-xml
        *
        * This compensates for changes to PCRE Extension for PHP > 5.5.1
        * Where original Regex expression failed during compilation time using
        * pattern[] = '/\<link\>([a-zA-Z0-9#?&%.;\[\]\/=+_-\s]+)\<\/link\>/e';
        *
        * Replacement regex $pattern applied
        */

        $return = preg_replace_callback('/\<link\>([a-zA-Z0-9#?&%.;\[\]\/=+\s\-\_]+)\<\/link\>/', function ($m) {
            return '<link>'.urlencode($m[1]).'</link>';
        }, $content);
        return $return;
    }
}
