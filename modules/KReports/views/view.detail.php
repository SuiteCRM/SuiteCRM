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

require_once('include/MVC/View/views/view.detail.php');
require_once('include/utils.php');
require_once('modules/KReports/utils.php');
require_once('modules/KReports/KReportVisualizationManager.php');
require_once('modules/KReports/KReportPresentationManager.php');

if (!class_exists('JSMin'))
   require_once('modules/KReports/jsmin.php');

global $dictionary;

#[\AllowDynamicProperties]
class KReportsViewDetail extends ViewDetail {

   function __construct() {
      parent::__construct();
   }

   function display() {

      global $app_list_strings, $mod_strings, $current_language, $current_user, $dictionary;

/*      if ($GLOBALS['sugar_config']['KReports']['trace']) {
         global $db;
         $db->query("INSERT INTO kreportstats SET id='" . create_guid() . "', user_id='$current_user->id', report_id='" . $this->bean->id . "', date=now()");
      }
*/
      // build the langiage strings
      $mod_lang = return_module_language($current_language, 'KReports');

      foreach ($mod_lang as $id => $value) {
         $returnArray[] = array('lblid' => $id, 'value' => str_replace("'","#039;",$value));
      }

	  // maretval: get parent Report information
      if(isset($_REQUEST['parentreportid']) && !empty($_REQUEST['parentreportid'])){
          $parentReport = new KReport();
          $parentReport->retrieve($_REQUEST['parentreportid']);
          $parentReport_whereconditions = json_decode(html_entity_decode($parentReport->whereconditions));
      }
	  
      // dynamic Select Options
      if(isset($_REQUEST['dynamicoptions'])){
         $whereFieldsList = json_decode_kinamu(html_entity_decode($this->bean->whereconditions, ENT_QUOTES));
         $loadParams = json_decode(html_entity_decode(base64_decode($_REQUEST['dynamicoptions'])), true);
         foreach($loadParams as $loadParam){
            foreach($whereFieldsList as $fieldIndex => $fieldData){
               if($fieldData['usereditable'] != 'no' && ((!empty($loadParam['reference']) && $loadParam['reference'] == $fieldData['reference'])|| (!empty($loadParam['fieldid'])  && $loadParam['fieldid'] == $fieldData['fieldid']))){
                  if(!empty($loadParam['operator'])) $whereFieldsList[$fieldIndex]['operator'] = $loadParam['operator'];
                  if(!empty($loadParam['value'])) $whereFieldsList[$fieldIndex]['value'] = $loadParam['value'];
                  if(!empty($loadParam['valuekey'])) $whereFieldsList[$fieldIndex]['valuekey'] = $loadParam['valuekey'];
                  if(!empty($loadParam['valueto'])) $whereFieldsList[$fieldIndex]['valueto'] = $loadParam['valueto'];
                  if(!empty($loadParam['valuetokey'])) $whereFieldsList[$fieldIndex]['valuetokey'] = $loadParam['valuetokey'];
               }
            }
         }
         $this->bean->whereconditions = json_encode($whereFieldsList);
      }
      
      $editableWhereFields = $this->bean->get_runtime_wherefilters();
      $jsonWhereOptions = str_replace("\"", "'", json_encode($editableWhereFields, JSON_HEX_APOS));

      if (count($editableWhereFields) > 0)
         $this->ss->assign('dynamicoptions', $jsonWhereOptions);
      else
         $this->ss->assign('dynamicoptions', '');

      // set the language Parameters
      $this->ss->assign('jsonlanguage', json_encode_kinamu($returnArray));

      // view Specifics
      //if ($this->bean->listtype != 'standard' & $this->bean->listtype != '') {
      $pluginManager = new KReportPluginManager();


      $thisPresentationManager = new KReportPresentationManager();
      $this->ss->assign('presentation', JSMin::minify($thisPresentationManager->renderPresentation($this->bean)));

      // get the Integration PPlugins
      $this->ss->assign('integrationpluginjs', JSMin::minify($pluginManager->getIntegrationPlugins($this->bean)));

      $thisVisualizationManager = new KReportVisualizationManager();
      //$this->ss->assign('visualization', JSMin::minify($thisVisualizationManager->renderVisualization(html_entity_decode($this->bean->visualization_params, ENT_QUOTES, 'UTF-8'), $this->bean)));
      $this->ss->assign('visualization', $thisVisualizationManager->renderVisualization(html_entity_decode($this->bean->visualization_params, ENT_QUOTES, 'UTF-8'), $this->bean));

      // set the view js
      //if ($this->bean->listtype == '')
      //    $this->bean->listtype = 'standard';
      //$this->ss->assign('viewJS', $this->setFormatVars() . '<script type="text/javascript" src="modules/KReports/views/view.detail.' . $this->bean->listtype . /* @ObfsProperty@ */ '.js" charset="utf-8"></script>');
      // override the options settings if the user is the admin
      $optionsJson = json_decode(html_entity_decode($this->bean->reportoptions ?? '')) ?: (object) [];
      if ($current_user->is_admin) {
         $optionsJson->showTools = true;
         $optionsJson->showExport = true;
         $optionsJson->showSnapshots = true;
      }
      $this->ss->assign('reportoptions', json_encode($optionsJson));

      // build js variables
      require_once('modules/ACL/ACLController.php');
      $jsVariables = '';

      //edit & delete
      // if (ACLController::checkAccess($this->bean->module_dir, 'edit', $this->bean->assigned_user_id == $current_user->id ? true : false)) {
      if ($this->bean->ACLAccess('edit')) {
         //if (ACLController::checkAccess($this->bean->module_dir, 'delete', $this->bean->assigned_user_id == $current_user->id ? true : false))
         if ($this->bean->ACLAccess('delete'))
            $jsVariables .= "var accessLevel= 2;";
         else
            $jsVariables .= "var accessLevel= 1;";
      } else {
         $jsVariables .= "var accessLevel= 0;";
      }

      // set the record id
      $jsVariables .= "var reportId='" . $this->bean->id . "';";

      // set if the Reporter is in DebugMode
      if (isset($GLOBALS['sugar_config']['KReports']) && isset($GLOBALS['sugar_config']['KReports']['debug']) && $GLOBALS['sugar_config']['KReports']['debug']) {
         $jsVariables .= "var kreportDebug=true;";
         $this->ss->assign('kreportDebug', true);
      } else {
         $jsVariables .= "var kreportDebug=false;";
         $this->ss->assign('kreportDebug', false);
      }

      // get all the where editable fields per operator
      include('modules/KReports/config/KReportWhereOperators.php');
      $jsVariables .= "var kreportoperatorcount=" . json_encode($kreporterWhereOperatorCount) . ";";

      // add general format vars
      $jsVariables .= $this->setFormatVars();

      // 2013-03-18 add a config param for the Ext.AJAX timeout Bug #446
/*
      if (!empty($GLOBALS['sugar_config']['KReports']['AJAXTimeout']))
         $jsVariables .= "Ext.Ajax.timeout = " . $GLOBALS['sugar_config']['KReports']['AJAXTimeout'] . ";";
*/
      $this->ss->assign('jsVariables', JSMin::minify($jsVariables));

      // process the view
      parent::display();
   }

   function setFormatVars() {
      global $current_user;

      //$varJS = '<script type="text/javascript">';

      $varJS = 'kreport_default_currency_symbol = \'' . ($GLOBALS['sugar_config']['default_currency_symbol'] ?? '') . '\';';
      $varJS .= 'kreport_dec_sep = \'' . ($_SESSION[$current_user->user_name . '_PREFERENCES']['global']['dec_sep'] ?? '') . '\';';
      $varJS .= 'kreport_num_grp_sep = \'' . ($_SESSION[$current_user->user_name . '_PREFERENCES']['global']['num_grp_sep'] ?? '') . '\';';
      $varJS .= 'kreport_default_currency_significant_digits = \'' . ($_SESSION[$current_user->user_name . '_PREFERENCES']['global']['default_currency_significant_digits'] ?? '') . '\';';

      //2013-05-17 add users time format BUG #484
      $varJS .= 'kreport_tf = \'' . ($_SESSION[$current_user->user_name . '_PREFERENCES']['global']['timef'] ?? '') . '\';';
      
      // get currencies
      $curResArray = $GLOBALS['db']->query('SELECT id, symbol FROM currencies WHERE deleted = \'0\'');

      $curArray = array();
      $curArray = ['-99' => ($GLOBALS['sugar_config']['default_currency_symbol'] ?? '')];
      while ($thisCurEntry = $GLOBALS['db']->fetchByAssoc($curResArray)) {
         $curArray[$thisCurEntry['id']] = $thisCurEntry['symbol'];
      }
      $varJS .= 'kreport_currencies = ' . json_encode($curArray) . ';';
      // $varJS .= '</script>';

      return $varJS;
   }

}

?>
