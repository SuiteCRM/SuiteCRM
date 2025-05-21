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

if (!defined('sugarEntry') || !sugarEntry)
   die('Not A Valid Entry Point');

require_once('include/MVC/View/views/view.edit.php');
require_once('modules/KReports/utils.php');
require_once('modules/KReports/KReportVisualizationManager.php');

global $dictionary;

#[\AllowDynamicProperties]
class KReportsViewEdit extends ViewEdit {

   function __construct() {

      // 2013-02-28 disable VCR Display
      $GLOBALS['sugar_config']['disable_vcr'] = true;

      parent::__construct();
   }

   function display() {
      global $app_list_strings, $mod_strings, $current_language, $dictionary, $sugar_config, $db;

      // ann Add Js Strinf that will be assigned at the end befor ehte view gets rendered
      $this->addJsString = '';
      $jsVariables = '';

      // get the Sugar Flavor
      $jsVariables .= "var sugar_flavor='" . $GLOBALS['sugar_flavor'] . "';";

      // get all the where editable fields per operator
      include('modules/KReports/config/KReportWhereOperators.php');
      $jsVariables .= "var kreportoperatorcount=" . json_encode($kreporterWhereOperatorCount) . ";";


      $mod_lang = return_module_language($current_language, 'KReports');

      foreach ($mod_lang as $id => $value) {
         $returnArray[] = array('lblid' => $id, 'value' => str_replace("'","#039;",$value));
      }

      // add the app list array we need
      if (is_array($this->bean->field_defs) && count($this->bean->field_defs) > 0) {
         foreach ($this->bean->field_defs as $fieldId => $fieldDetails) {
            if (isset($fieldDetails['options']) && isset($app_list_strings[$fieldDetails['options']])) {
               $thisString = jarray_encode_kinamu($app_list_strings[$fieldDetails['options']]);
               $returnArray[] = array('lblid' => $fieldId . '_options', 'value' => jarray_encode_kinamu($app_list_strings[$fieldDetails['options']]));
            }
         }
      }

      // STIC-Custom 20221027 MHP -Set is_admin property
      // STIC#897
      // Set admin user or not
      global $current_user;
      if ($current_user->is_admin){
         $this->ss->assign('is_admin', '1');
      } else {
         $this->ss->assign('is_admin', '0');
      }
      // END STIC-Custom

      // set the language
      $langJson = json_encode_kinamu($returnArray);
      $this->ss->assign('jsonlanguage', json_encode_kinamu($returnArray));

      // see if we have a return id
      if (!isset($_REQUEST['return_id']) || $_REQUEST['return_id'] == '')
         $_REQUEST['return_id'] = $this->bean->id;

      // set Options
      if ($this->bean->reportoptions == '')
         // STIC-Custom 20230726 AAM - Change default behaviour search options and results panel
         // STIC#1172
         // STIC#1174
         // STIC#1214
         // $this->bean->reportoptions = '{"resultsFolded":false,"optionsFolded":true,"authCheck":"full","showDeleted":false,"showExport":true,"showSnapshots":false,"showTools":true}';
         $this->bean->reportoptions = '{"resultsFolded":"open","optionsFolded":"open","authCheck":"full","showDeleted":false,"showExport":true,"showSnapshots":false,"showTools":true}';
         // END STIC-Custom

      // handle Plugins
      $pluginManager = new KReportPluginManager();
      $pluginManager->getEditViewPlugins($this);

      // handle authorization objects
      if (!empty($GLOBALS['KAuthAccessController'])) {
         if ($GLOBALS['KAuthAccessController']->orgManaged('KReport')) {
            $jsVariables .= "var korgmanaged=true;";
         } else {
            $jsVariables .= "var korgmanaged=false;";
         }
      }

      // manage Visualitazion Variables
      $thisVisualizationManager = new KReportVisualizationManager();
      $this->addJsString .= $thisVisualizationManager->getLayouts();

      // assign to the Template
      $this->ss->assign('editViewAddJs', $this->addJsString);

      //2013-03-15 pass in teh auth check type
      $autcheck='';
      if (isset($sugar_config['KReports']) && isset($sugar_config['KReports']['authCheck'])){
      	//$autcheck=$sugar_config['KReports']['authCheck'];
      }
      $jsVariables .= 'kreportAuthCheck=\'' . $autcheck . '\';';
      // handle access authentication for Dialog
      switch ($autcheck) {
         case 'KAuthObjects':
            $this->ss->assign('authaccess_id', $this->bean->korgobjectmain);

            // get the name 
            require_once('modules/KOrgObjects/KOrgObject.php');
            $thisObject = new KOrgObject();
            $thisObject->retrieve($this->bean->korgobjectmain);
            $this->ss->assign('authaccess_name', $thisObject->name);
            break;

         case 'SecurityGroups':
            $thisRecord = $db->fetchByAssoc($db->query("SELECT securitygroups_records.id, name FROM securitygroups_records INNER JOIN securitygroups ON securitygroups.id = securitygroups_records.securitygroup_id WHERE securitygroups_records.record_id='" . $this->bean->id . "'"));
            if ($thisRecord) {
               //$this->ss->assign('authaccess_id', $thisRecord['id']);
               //$this->ss->assign('authaccess_name', $thisRecord['name']);
            }
            break;
         case 'PRO':
            if (!empty($this->bean->team_id)) {
               $thisTeam = BeanFactory::getBean('Teams', $this->bean->team_id);
               $this->ss->assign('team_name', $thisTeam->name . ' ' . $thisTeam->name_2);
            }
            break;
      }

      // set ambigious ariables
      $this->ss->assign('jsVariables', $jsVariables);

      // set if the Reporter is in DebugMode
         $jsVariables .= "var kreportDebug=false;";
         $this->ss->assign('kreportDebug', false);


      if (!empty($this->bean->team_id)) {
         $thisTeam = BeanFactory::getBean('Teams', $this->bean->team_id);
         $this->ss->assign('team_name', $thisTeam->name . ' ' . $thisTeam->name_2);
      }

      // off we go
      parent::display();
      
echo <<<EOQ
	<script>
			$('#SAVE').closest('div').hide();			
	</script>
EOQ;
   }

}

?>
