<?php
/**
 * This file is part of KReporter. KReporter is an enhancement developed
 * by Christian Knoll. All rights are (c) 2012 by Christian Knoll
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
 */
if (!defined('sugarEntry') || !sugarEntry)
    die('Not A Valid Entry Point');

require_once('modules/KReports/KReport.php');

class KReportPresentationManager {

    // centrally keep the pluginmanager
    public $pluginManager;

    public function __construct() {

        $this->pluginManager = new KReportPluginManager();
    }
    
    // function to get the Plugin Object
    public function getPresentationPlugin($thisReport){
       $listOptions = json_decode(html_entity_decode($thisReport->presentation_params), true);
        if(!empty($listOptions['plugin']))
            $pluginObject = $this->pluginManager->getPresentationObject($listOptions['plugin']);
        else if (!empty($thisReport->listtype))
            $pluginObject = $this->pluginManager->getPresentationObject($thisReport->listtype);
        else 
            $pluginObject = $this->pluginManager->getPresentationObject('standard');
        return $pluginObject;
    }
    
    public function renderPresentation($thisReport) {
        $pluginObject = $this->getPresentationPlugin($thisReport);
        return $pluginObject->display($thisReport);
    }
    
    public function getPresentationExport($thisReport, $dynamicols, $renderFields = true){
        $pluginObject = $this->getPresentationPlugin($thisReport);
        return $pluginObject->getExportData($thisReport, $dynamicols, $renderFields);
    }
}
?>
