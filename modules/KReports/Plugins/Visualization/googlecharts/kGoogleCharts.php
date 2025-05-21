<?php
/**
 * This file is part of KReporter. KReporter is an enhancement developed
 * by Christian Knoll. All rights are (c) 2012 by Christian Knoll
 *
 * This file has been modified by SinergiaTIC in SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
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
 * You can contact Christian Knoll at info@kreporter.org
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */

if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

require_once('modules/KReports/KReportChartData.php');
require_once('modules/KReports/Plugins/prototypes/kreportvisualizationplugin.php');

class kGoogleChart extends kreportvisualizationplugin{

    function __construct() {
        
    }

    public function getHeader() {
        // STIC 14/05/2020: Update regarding the loading of the Google Graphic libraries
        // $coreString = "<script type='text/javascript' src='https://www.google.com/jsapi?autoload={\"modules\":[{\"name\":\"visualization\",\"version\":\"1\"}]}'></script>";
        $coreString = "<script type='text/javascript' src='https://www.gstatic.com/charts/loader.js'></script>";
        return $coreString;
    }

    /*
     * get only the data component if the selction has changed
     */

    public function getItemUpdate($thisReport, $thisParams, $snaphotid = 0, $addReportParams = array()) {
        return json_encode($this->getChartData($thisReport, $thisParams, $snaphotid, $addReportParams));
    }

    /*
     * get the Chart Object to render into the visualization
     */

    public function getItem($thisDivId, $thisReport, $thisParams, $addReportParams = array()) {

        $googleData = $this->getChartData($thisReport, $thisParams, 0, $addReportParams);
        $chartData = $this->wrapGoogleData($googleData, $thisDivId, $thisParams);

        $chartDataString = '<script type="text/javascript">';

        // STIC 14/05/2020: Update regarding the loading of the Google Graphic libraries
        $chartDataString .= "google.charts.load('current'); ";
        $chartDataString .= "google.charts.setOnLoadCallback(drawVisualization); ";
        $chartDataString .= "function drawVisualization() { ";
        // STIC 14/05/2020: End of update

        $chartDataString .= $thisParams['uid'] . " = new Object({
                uid: '" . $thisParams['uid'] . "',
                chartWrapper: new google.visualization.ChartWrapper(" . json_encode($chartData) . "), 
                update: function(chartData){
                    this.chartWrapper.setDataTable(chartData);
                    this.chartWrapper.draw();
                }
                });
                document.addEventListener('load', " . $thisParams['uid'] . ".chartWrapper.draw());";
        $chartDataString .= "}"; // STIC Update 14/05/2020
        $chartDataString .= '</script>';

        return $chartDataString;
    }

    public function getChartData($thisReport, $thisParams, $snaphotid = 0, $addReportParams = array()) {
        $chartDataObj = new KReportChartData();
        $fields = json_decode(html_entity_decode($thisReport->listfields, ENT_QUOTES, 'UTF-8'), true);

        // check for all the fieldids we have
        $fieldMap = array();
        foreach ($fields as $thisFieldIndex => $thisFieldData) {
            $fieldMap[$thisFieldData['fieldid']] = $thisFieldIndex;
        }

        //$dimensions = array(array('fieldid' => $fields[0]['fieldid']));
        $dimensions = array();
        foreach ($thisParams['dimensions'] as $thisDimension => $thisDimensionData) {
            if ($thisDimensionData != null)
                $dimensions[] = array('fieldid' => $thisDimensionData);
        }

        //$dataseries = array($fields[1]['fieldid'], $fields[2]['fieldid']);
        $dataseries = array();
        foreach ($thisParams['dataseries'] as $thisDataSeries => $thisDataSeriesData) {
            $dataseries[$thisDataSeriesData['fieldid']] = array(
                'fieldid' => $thisDataSeriesData['fieldid'],
                'name' => $fields[$fieldMap[$thisDataSeriesData['fieldid']]]['name']
            );
        }

        // set Chart Params
        $chartParams = array();
        $chartParams['showEmptyValues'] = ($thisParams['options']['emptyvalues']??'' == 'on' ? true : false);
        if ($thisParams['context']??'' != '')
            $chartParams['context'] = $thisParams['context'];

        $rawData = $chartDataObj->getChartData($thisReport, $snaphotid, $chartParams, $dimensions, $dataseries, $addReportParams);

        return $this->convertRawToGoogleData($rawData['chartData'], $rawData['dimensions'], $rawData['dataseries']);
    }

    /*
     * helper function to mingle the data and prepare for a google represenatation
     */

    public function convertRawToGoogleData($chartData, $dimensions, $dataseries) {
        $googleData = array();
        $googleData['cols'] = array();
        $googleData['rows'] = array();

        foreach ($dimensions as $thisDimension) {
            $googleData['cols'][] = array('id' => $thisDimension['fieldid'], 'type' => 'string', 'label' => $thisDimension['fieldid']);
        }

        foreach ($dataseries as $thisDataseries) {
            $googleData['cols'][] = array('id' => $thisDataseries['fieldid'], 'type' => 'number', 'label' => ($thisDataseries['name'] != '' ? $thisDataseries['name'] : $thisDataseries['fieldid']));
        }

        foreach ($chartData as $thisDimensionId => $thisData) {
            $rowArray = array();
            $rowArray[] = array('v' => $dimensions[0]['values'][$thisDimensionId]);
            foreach ($dataseries as $thisDataseries) {
                $rowArray[] = array('v' => $thisData[$thisDataseries['fieldid']]);
            }
            $googleData['rows'][] = array('c' => $rowArray);
        }

        return $googleData;
    }

    /*
     * function to wrap the code with the google visualization API options etc. 
     */

    public function wrapGoogleData($googleData, $divId, $thisParams) {
        // else continue processing .. 
        $googleChart = array(
            'chartType' => ($thisParams['type'] != 'Gauge' ? $thisParams['type'] . 'Chart' : 'Gauge'),
            'containerId' => $divId,
            'options' => array(
                'legend' => 'none',
                'fontSize' => 11
            ),
            'dataTable' => $googleData
        );

        // handle options
        foreach ($thisParams['options'] as $thisOption => $thisOptionCount) {
            switch ($thisOption) {
                case 'is3D':
                    $googleChart['options']['is3D'] = true;
                    break;
                case 'legend':
                    $googleChart['options']['legend'] = array(
                        'position' => 'bottom'
                    );
                    break;
            }
        }

        // set the title if we have one
        if ($thisParams['title'] != '') {
            $googleChart['options']['title'] = $thisParams['title'];
            $googleChart['options']['titleTextStyle'] = array(
                'fontSize' => 14
            );
        }


        // send back the Chart as Array
        return $googleChart;
    }

}

?>
