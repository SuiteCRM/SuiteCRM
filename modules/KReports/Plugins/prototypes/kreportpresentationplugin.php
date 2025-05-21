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
if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');


/*
 * prototype class for an presentation item 
 * all presentation items should be based on this class and then extend the methods herein. 
 * KReporter expects these functions to be available
 */

class kreportpresentationplugin {
    public $canExport = false; 
    
    public function display(&$thisview){
        
    }
    // function that should be overwritten if the Presentation Plugin provides a separate export to XLS and PDF
    public function getExportData($thisReport, $dynamicols = '', $renderFields = true){
        return false;
    }
    
    public function buildFieldArray($thisReport){
        return array();
    }
    
    public function buildColumnArray($thisReport){
       return array();
    }
}
?>
