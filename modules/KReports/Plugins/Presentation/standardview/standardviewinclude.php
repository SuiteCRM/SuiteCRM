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

require_once('modules/KReports/Plugins/prototypes/kreportpresentationplugin.php');

class kreportpresentationstandard extends kreportpresentationplugin {

//function standarddetailviewdisplay(&$thisview) {
   public function display(&$thisReport) {
      global $app_list_strings, $mod_strings, $current_language, $current_user;

      // $fieldArray = htmlentities($fieldArray, ENT_QUOTES, 'UTF-8');
      // STIC-Custom 20250217 EPS -  htmlentities was changing single quotes by html special character &#039; and causing a javascript error
      // $viewJS .= '<script type="text/javascript">FieldsArray = ' . htmlentities($this->buildFieldArray($thisReport)) . ';</script>';
      $viewJS = '';
      $viewJS .= '<script type="text/javascript">FieldsArray = ' . htmlspecialchars($this->buildFieldArray($thisReport), ENT_NOQUOTES|ENT_HTML401) . ';</script>';
      // END STIC-Custom

      // TODO .. JSON Encode with Bitmask ... need Doku
      // $thisview->ss->assign('columnmodeldarray', str_replace('"', '', json_encode($columnArray)));
      // include the js file 

      $viewJS .= '<script type="text/javascript">ColumnsArray = ' . str_replace('"', '', json_encode($this->buildColumnArray($thisReport))) . ';</script>';
      $viewJS .= '<script type="text/javascript" src="modules/KReports/Plugins/Presentation/standardview/js/viewstandard.js"></script>';

      return $viewJS;
      // $thisview->ss->assign('viewJS', $viewJS);
   }

   public function buildFieldArray($thisReport) {

      // build the Field List for the JSON Reader
      $arrayList = json_decode(html_entity_decode($thisReport->listfields, ENT_QUOTES), true);
      $fieldArray = '[';

      foreach ($arrayList as $thisList) {
         if ($fieldArray != '[')
            $fieldArray .= ',';
         $fieldArray .= '{name : \'' . trim($thisList['fieldid'], ':') . '\'}';

         // see if we nee to add a field for the currency_id to the store 2010-12-25 
         // change to check if set to avoid php notice
         if ((isset($thisReport->fieldNameMap[$thisList['fieldid']]['fields_name_map_entry']['type']) && $thisReport->fieldNameMap[$thisList['fieldid']]['fields_name_map_entry']['type'] == 'currency') || (isset($thisReport->fieldNameMap[$thisList['fieldid']]['fields_name_map_entry']['kreporttype']) && $thisReport->fieldNameMap[$thisList['fieldid']]['fields_name_map_entry']['kreporttype'] == 'currency'))
            $fieldArray .= ',{name : \'' . trim($thisList['fieldid'], ':') . '_curid\'}';
      }

      //$fieldArray .= trim($fieldArray, ',');
      $fieldArray .= "]";

      return $fieldArray;
   }

   public function buildColumnArray($thisReport) {
      $arrayList = json_decode(html_entity_decode($thisReport->listfields, ENT_QUOTES), true);
      $columnArray = array();
      foreach ($arrayList as $thisList) {
         if ($thisList['display'] != 'hid') {
            $thisColumn = array(
                //2013-03-05 html entities to support special chars in Text
                //2013-04-19 added UTF-8 support
                'text' => '\'' . htmlentities($thisList['name'], ENT_QUOTES, 'UTF-8') . '\'',
                'width' => ((isset($thisList['width']) && $thisList['width'] != '' && $thisList['width'] != '0') ? $thisList['width'] : '150'),
                'sortable' => ($thisList['sort'] != '' && $thisList['sort'] != '-' ? 'true' : 'false'),
                'dataIndex' => '\'' . trim($thisList['fieldid'], ':') . '\'',
                'hidden' => ($thisList['display'] != 'yes' ? true : false)
            );

            // see if we have renderer we need to process
            $renderer = $thisReport->getXtypeRenderer($thisReport->getFieldTypeById($thisList['fieldid']), $thisList['fieldid']);
            if ($renderer != '')
               $thisColumn['renderer'] = "'" . $renderer . "'";
            //2012-12-01 add default renderer for buuilding links
            else
               $thisColumn['renderer'] = "'fieldRenderer'";

            // see if we have alignment we need to process
            $alignment = $thisReport->getXtypeAlignment($thisReport->getFieldTypeById($thisList['fieldid']), $thisList['fieldid']);
            if ($alignment != '')
               $thisColumn['align'] = "'" . $alignment . "'";

            // pass back the column Array
            $columnArray[] = $thisColumn;
         }
      }
      return $columnArray;
   }

}