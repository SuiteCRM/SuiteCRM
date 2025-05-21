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
require_once('include/MVC/Controller/SugarController.php');
require_once('modules/Contacts/Contact.php');
require_once('modules/KReports/utils.php');
require_once('include/utils/db_utils.php');
require_once('include/utils.php');

class KReportsController extends SugarController {

   //2013-02-18 make sure we disable display_errors
   public function __construct() {
      ini_set('display_errors', '0');
      parent::__construct();
   }

   public function action_pluginaction() {
      $pluginManager = new KReportPluginManager();
      return $pluginManager->processPluginAction($_REQUEST['plugin'], 'action_' . $_REQUEST['pluginaction'], $this);
   }

   function action_get_modules() {
      global $app_list_strings;

      $excludedModules = array();

      if (file_exists('modules/KReports/kreportsConfig.php'))
         include('modules/KReports/kreportsConfig.php');

      $returnArray = array();
      foreach ($app_list_strings['moduleList'] as $module => $description) {
         if (!in_array($module, $excludedModules))
            $returnArray[] = array('module' => $module, 'description' => $description);
      }

      //2014-03-16 sort list by Description Bug #517
      //sort($returnArray);
      usort($returnArray, "arraySortByDescription");
      
      print json_encode_kinamu($returnArray);
   }

   function action_get_tabs() {
      require_once('include/GroupedTabs/GroupedTabStructure.php');
      $returnArray = array();
      foreach ($GLOBALS['tabStructure'] as $tabItem) {
         $returnArray[] = array('tab' => $tabItem['label'], 'description' => translate($tabItem['label']));
      }
      print json_encode_kinamu($returnArray);
   }

   function action_get_modulefields() {
      global $beanFiles, $beanList;

      $retarray = array();
      if (isset($_REQUEST['inputmodule']) && $_REQUEST['inputmodule'] != '') {
         require_once($beanFiles[$beanList[$_REQUEST['inputmodule']]]);
         $inputModule = new $beanList[$_REQUEST['inputmodule']];

         //2013-01-18 take in account the users language
         $langArray = return_module_language($_SESSION['authenticated_user_language'], $_REQUEST['inputmodule']);

         foreach ($inputModule->field_defs as $fieldname => $fielddefs) {
            $retarray[] = array(
                'field' => $fieldname,
                'description' => isset($fielddefs['vname']) ? isset($langArray[$fielddefs['vname']]) ? $langArray[$fielddefs['vname']] : $fielddefs['vname']  : $fieldname
            );
         }
      }

      print json_encode_kinamu($retarray);
   }

   //2012-11-24 add controler action to get Where Operators
   function action_get_whereoperators() {
      global $app_list_strings, $beanFiles, $beanList, $db;

      include('modules/KReports/config/KReportWhereOperators.php');

      //2013-01-18 take in account the users language
      $mod_strings = return_module_language($_SESSION['authenticated_user_language'] ?? '', 'KReports');

      $retarray[] = array(
          'operator' => 'ignore',
          'values' => $kreporterWhereOperatorCount['ignore'],
          'display' => $mod_strings['LBL_OP_IGNORE']
      );

      //2013-08-07 check if path is set
      if (empty($_REQUEST['path'])) {
         // parse the options into the return array
         foreach ($kreporterWhereOperatorTypes[$kreporterWhereOperatorAssignments['fixed']] as $operator)
            $retarray[] = array(
                'operator' => $operator,
                'values' => $kreporterWhereOperatorCount[$operator],
                'display' => $mod_strings['LBL_OP_' . strtoupper($operator)]
            );
      } else {

         // explode the path
         $pathArray = explode('::', $_REQUEST['path']);

         // get Field and Module from the path
         $fieldArray = explode(':', $pathArray[count($pathArray) - 1]);
         $moduleArray = explode(':', $pathArray[count($pathArray) - 2]);

         // load the parent module
         require_once($beanFiles[$beanList[$moduleArray[1]]]);
         $parentModule = new $beanList[$moduleArray[1]];

         // get the module we need to determine the type
         if ($moduleArray[0] == 'link') {
            // load the Relationshop to get the module
            $parentModule->load_relationship($moduleArray[2]);
            $nodeEL = $moduleArray['2'];

            // load the Module
            require_once($beanFiles[$beanList[$parentModule->$nodeEL->getRelatedModuleName()]]);
            $thisModule = new $beanList[$parentModule->$nodeEL->getRelatedModuleName()];
         }
         //2013-09-25 add support for relate fields BUG #498
         elseif ($moduleArray[0] == 'relate') {
            require_once($beanFiles[$beanList[$moduleArray['1']]]);
            $nodeModule = new $beanList[$moduleArray['1']]();
            $thisModuleName = $nodeModule->field_defs[$moduleArray[2]]['module'];
            require_once($beanFiles[$beanList[$thisModuleName]]);
            $thisModule = new $beanList[$thisModuleName]();
         } else
            $thisModule = $parentModule;

         //2013-02-26 ... added operators for audit fields
         if ($moduleArray[0] == 'audit') {
            //2013-04-12 handle where operators for types ... Bug #465
            switch ($fieldArray[1]) {
               case 'date_created':
                  foreach ($kreporterWhereOperatorTypes['date'] as $operator)
                     $retarray[] = array(
                         'operator' => $operator,
                         'values' => $kreporterWhereOperatorCount[$operator],
                         'display' => $mod_strings['LBL_OP_' . strtoupper($operator)]
                     );
                  break;
               default:
                  foreach ($kreporterWhereOperatorTypes['varchar'] as $operator)
                     $retarray[] = array(
                         'operator' => $operator,
                         'values' => $kreporterWhereOperatorCount[$operator],
                         'display' => $mod_strings['LBL_OP_' . strtoupper($operator)]
                     );
                  break;
            }
         }

         // special handling for Kreporttype if we have an eval array
         if ($thisModule->field_name_map[$fieldArray[1]]['type'] == 'kreporter' && is_array($thisModule->field_name_map[$fieldArray[1]]['eval'])) {
            foreach ($thisModule->field_name_map[$fieldArray[1]]['eval']['selection'] as $operator => $eval)
               $retarray[] = array(
                   'operator' => $operator,
                   'values' => $kreporterWhereOperatorCount[$operator],
                   'display' => $mod_strings['LBL_OP_' . strtoupper($operator)]
               );

            //2013-02-26 ... add reference also for kreporter fields
            if ($_REQUEST['editview']) {
               $retarray[] = array(
                   'operator' => 'reference',
                   'values' => 1,
                   'display' => $mod_strings['LBL_OP_REFERENCE']
               );
            }
         } else {
            // parse the options into the return array
            foreach ($kreporterWhereOperatorTypes[$kreporterWhereOperatorAssignments[isset($thisModule->field_name_map[$fieldArray[1]]['kreporttype']) ? $thisModule->field_name_map[$fieldArray[1]]['kreporttype'] : $thisModule->field_name_map[$fieldArray[1]]['type']]] as $operator)
               $retarray[] = array(
                   'operator' => $operator,
                   'values' => $kreporterWhereOperatorCount[$operator],
                   'display' => $mod_strings['LBL_OP_' . strtoupper($operator)]
               );

            if ($_REQUEST['editview']) {
               $retarray[] = array(
                   'operator' => 'function',
                   'values' => 1,
                   'display' => $mod_strings['LBL_OP_FUNCTION']
               );
               $retarray[] = array(
                   'operator' => 'reference',
                   'values' => 1,
                   'display' => $mod_strings['LBL_OP_REFERENCE']
               );
            }
         }
      }
      print json_encode_kinamu($retarray);
   }

   function action_get_wherefunctions() {
      $retarray = array();
      $retarray[] = array(
          'field' => '',
          'description' => '-'
      );
      $customFunctionInclude = '';
      $kreportCustomFunctions = array();
      include('modules/KReports/kreportsConfig.php');
      if ($customFunctionInclude != '') {
         include($customFunctionInclude);
         if (is_array($kreportCustomFunctions) && count($kreportCustomFunctions) > 0) {
            foreach ($kreportCustomFunctions as $functionname => $functiondescription) {
               $retarray[] = array(
                   'field' => $functionname,
                   'description' => $functiondescription
               );
            }
         }
      }

      print json_encode_kinamu($retarray);
   }

   function action_get_reports() {
      global $app_list_strings, $db, $current_user;
      $queryArray = preg_split('/::/', $_REQUEST['node']);
      switch ($queryArray[0]) {
         case 'src':
            $returnArray[] = array('id' => 'favorites', 'text' => 'Favorites', 'expanded' => true);
            $returnArray[] = array('id' => 'modules', 'text' => 'by Module', 'expanded' => true);
            break;
         case 'modules':
            if (isset($_SESSION['KReports']['lastviewed']))
               $lastViewedArray = preg_split('/::/', $_SESSION['KReports']['lastviewed']);
            $modulesQuery = 'SELECT distinct report_module FROM kreports ';

            // check if we have KINAMu orManagement Installed for Authorization Check
            if (file_exists('modules/KOrgObjects/KOrgObject.php')) {
               require_once('modules/KOrgObjects/KOrgObject.php');
               $thisKOrgObject = new KOrgObject();
               $modulesQuery .= $thisKOrgObject->getOrgunitJoin('kreports', 'KReport', 'kreports', '1') . ' ';
            }

            $modulesQuery .= 'WHERE deleted =  \'0\' ORDER BY report_module ASC';

            $reportResults = $db->query($modulesQuery);

            while ($moduleEntry = $db->fetchByAssoc($reportResults)) {
               $returnArray[] = array('id' => 'module::' . $moduleEntry['report_module'], 'text' => $app_list_strings['moduleList'][$moduleEntry['report_module']], 'expanded' => (isset($lastViewedArray[0]) && $lastViewedArray[0] == $moduleEntry['report_module'] ) ? true : false);
            }
            break;
         case 'module':
            $moduleQuery = 'SELECT * FROM kreports ';

            if (file_exists('modules/KOrgObjects/KOrgObject.php')) {
               require_once('modules/KOrgObjects/KOrgObject.php');
               $thisKOrgObject = new KOrgObject();
               $moduleQuery .= $thisKOrgObject->getOrgunitJoin('kreports', 'KReport', 'kreports', '1') . ' ';
            }

            $moduleQuery .= 'WHERE report_module = \'' . $queryArray[1] . '\' AND deleted =  \'0\' ORDER BY report_module ASC';

            $reportResults = $db->query($moduleQuery);

            while ($moduleEntry = $db->fetchByAssoc($reportResults)) {
               $returnArray[] = array('id' => $moduleEntry['id'], 'leaf' => true, 'text' => $moduleEntry['name'], 'href' => 'index.php?module=KReports&action=DetailView&record=' . $moduleEntry['id']);
            }
            break;
         case 'favorites':
            $returnArray[] = array('id' => 'last10', 'leaf' => false, 'text' => 'last 10');
            $returnArray[] = array('id' => 'top10', 'leaf' => false, 'text' => 'top 10');

            $reportResults = $db->query('SELECT * FROM kreportsfavorites WHERE user_id = \'' . $current_user->id . '\'  ORDER BY description ASC');
            while ($moduleEntry = $db->fetchByAssoc($reportResults)) {
               $returnArray[] = array('id' => $moduleEntry['report_id'], 'leaf' => true, 'text' => $moduleEntry['description'], 'href' => 'index.php?module=KReports&action=DetailView&record=' . $moduleEntry['report_id'] . '&favid=' . $moduleEntry['report_id']);
            }
            break;
         case 'last10':
            $reportResults = $db->query('SELECT report_id, name FROM kreportstats INNER JOIN kreports ON kreports.id = kreportstats.report_id  WHERE user_id = \'' . $current_user->id . '\' GROUP  BY report_id ORDER BY max(date) DESC');
            while ($moduleEntry = $db->fetchByAssoc($reportResults)) {
               $returnArray[] = array('id' => $moduleEntry['report_id'], 'leaf' => true, 'text' => $moduleEntry['name'], 'href' => 'index.php?module=KReports&action=DetailView&record=' . $moduleEntry['report_id']);
            }
            break;
         case 'top10':
            $reportResults = $db->query('SELECT report_id, name FROM kreportstats INNER JOIN kreports ON kreports.id = kreportstats.report_id  WHERE user_id = \'' . $current_user->id . '\' GROUP  BY report_id ORDER BY count(kreportstats.id) DESC');
            while ($moduleEntry = $db->fetchByAssoc($reportResults)) {
               $returnArray[] = array('id' => $moduleEntry['report_id'], 'leaf' => true, 'text' => $moduleEntry['name'], 'href' => 'index.php?module=KReports&action=DetailView&record=' . $moduleEntry['report_id']);
            }
            break;
      }
      print json_encode_kinamu($returnArray);
   }

   /*
    * Custom Action for Soap Call to get Report Query
    */

   /*
     function action_get_new_sql() {
     require_once('modules/KReports/KReport.php');
     require_once('modules/KReports/KReportQuery.php');

     $thisReport = new KReport();
     $thisReport->retrieve($_REQUEST['record']);

     if (isset($_REQUEST['whereConditions'])) {
     $thisReport->whereOverride = json_decode_kinamu(html_entity_decode($_REQUEST['whereConditions']));
     }

     $sqlArray = $thisReport->get_report_main_sql_query();

     return $sqlArray['select'] . ' ' . $sqlArray['from'] . ' ' . $sqlArray['where'] . ' ' . $sqlArray['groupby'] . ' ' . $sqlArray['having'] . ' ' . $sqlArray['orderby'];
     }
    */
   /*
    * Custom Action for Soap Call to get Sna�pshots for a Report
    */

   function action_get_snapshots() {
      require_once('modules/KReports/KReport.php');

      $thisReport = new KReport();
      $thisReport->retrieve($_REQUEST['requester']);

      print json_encode_kinamu($thisReport->getSnapshots());
   }
   
   function action_delete_snaphot(){
      $thisReport = new KReport();
      $thisReport->retrieve($_REQUEST['record']);
      $thisReport->deleteSnapshot($_REQUEST['snapshotid']);
   }

   function action_get_report_massupdate() {
      global $beanFiles, $beanList, $app_list_strings;

      $retarray = array();
      $retarray['data'] = array();

      $thisReport = new KReport();
      $thisReport->retrieve($_REQUEST['requester']);
      //2013-01-18 take in account the users language
      $langArray = return_module_language($_SESSION['authenticated_user_language'], $thisReport->report_module);

      require_once($beanFiles[$beanList[$thisReport->report_module]]);
      $nodeModule = new $beanList[$thisReport->report_module];

      foreach ($nodeModule->field_defs as $fieldname => $fielddefs) {
         if (isset($fielddefs['massupdate']) && $fielddefs['massupdate'] == true) {
            $retarray['data'][] = array(
                'fieldname' => $fieldname,
                'fieldlabel' => isset($fielddefs['vname']) ? isset($langArray[$fielddefs['vname']]) ? $langArray[$fielddefs['vname']] : $fielddefs['vname']  : $fieldname,
                'fieldtype' => $fielddefs['type'],
                'fieldoptions' => isset($fielddefs['options']) ? json_encode($app_list_strings[$fielddefs['options']]) : ''
            );
         }
      }

      echo json_encode($retarray);
   }

   /*
    * Custom Action for Soap Call to get Sna�pshots for a Report
    */

   function action_get_listfields() {
      require_once('modules/KReports/KReport.php');

      $thisReport = new KReport();
      $thisReport->retrieve($_REQUEST['record']);

      print json_encode_kinamu($thisReport->getListfields());
   }

   /*
    * Function lo load enum values
    */

   function action_take_snapshot() {

      require_once('modules/KReports/KReport.php');
      require_once('include/utils.php');
      $thisReport = new KReport();
      $thisReport->retrieve($_REQUEST['record']);

      $thisReport->takeSnapshot();
      return true;
   }

   function action_export_to_csv() {

      // 2014-02-24 add config option for memory limit see if we should set the runtime and memory limit
/*
      if (!empty($sugar_config['KReports']['csvmemorylimit']))
         ini_set('memory_limit', $sugar_config['KReports']['csvmemorylimit']);
      if (!empty($sugar_config['KReports']['csvmaxruntime']))
         ini_set('max_execution_time', $sugar_config['KReports']['csvmaxruntime']);
*/

		$_SESSION['KReports']['export'] = true; // NS-Team
		
      require_once('modules/KReports/KReport.php');
      $thisReport = new KReport();
      $thisReport->retrieve($_REQUEST['record']);


      // check if we have set dynamic Options
      if (isset($_REQUEST['dynamicoptions']))
      // Bugfix 2010-11-12 to handle dynamic options in Excel Export
      //		  $thisReport->whereOverride = json_decode_kinamu( html_entity_decode_utf8($_REQUEST['dynamicoptions']));
         $_REQUEST['whereConditions'] = $_REQUEST['dynamicoptions'];


      // force Download
      $filename = "kreporter.csv";
      header('Content-type: application/ms-excel');
      header('Content-Disposition: attachment; filename=' . $filename);

      echo $thisReport->createCSV();
		$_SESSION['KReports']['export'] = false; // NS-Team
   }

   function action_check_access_level() {

      global $current_user;
      require_once('modules/KReports/KReport.php');
      $thisReport = new KReport();
      $thisReport->retrieve($_REQUEST['record']);

      require_once('modules/ACL/ACLController.php');

      if (ACLController::checkAccess($thisReport->module_dir, 'edit', $thisReport->assigned_user_id == $current_user->id ? true : false)) {
         if (ACLController::checkAccess($thisReport->module_dir, 'delete', $thisReport->assigned_user_id == $current_user->id ? true : false))
            print 2;
         else
            print 1;
      }
      else {
         print 0;
      }
   }

   function action_get_userids() {
      global $db;

      require_once('include/Localization/Localization.php');
      $locObject = new Localization();

      //2013-10-04 changed to total with ext 4.* so paging works properly Bug#505
      $returnArray['total'] = $db->getRowCount($db->query('SELECT id, user_name FROM users WHERE deleted = \'0\' AND status = \'Active\' AND user_name like \'' . $_REQUEST['query'] . '%\''));

      //if(isset($_REQUEST['query']) && $_REQUEST['query'] != '')
      //2013-10-04 changed to search also be name fields BUG#506
      $usersResult = $db->query('SELECT id, user_name, first_name, last_name FROM users WHERE deleted = \'0\' AND status = \'Active\' AND (user_name like \'%' . $_REQUEST['query'] . '%\' OR  first_name like \'%' . $_REQUEST['query'] . '%\' OR last_name like \'%' . $_REQUEST['query'] . '%\') LIMIT ' . $_REQUEST['start'] . ',' . $_REQUEST['limit']);
      //else
      //	$usersResult = $db->query('SELECT id, user_name FROM users WHERE deleted = \'0\' AND status = \'Active\'');

      while ($userRecord = $db->fetchByAssoc($usersResult)) {
         // bugfix 2010-09-28 since id was asisgned and not user name ..  no properly evaluates active user
         $returnArray['data'][] = array('value' => $userRecord['id'], 'text' => $locObject->getLocaleFormattedName($userRecord['first_name'], $userRecord['last_name']));
      }

      echo json_encode($returnArray);
   }

   function action_get_teamids() {
      global $db;

      $returnArray['count'] = $db->getRowCount($db->query('SELECT id, name FROM teams WHERE deleted = \'0\'  AND name like \'' . $_REQUEST['query'] . '%\''));

      //if(isset($_REQUEST['query']) && $_REQUEST['query'] != '')
      $teamResult = $db->query('SELECT id, name, name_2 FROM teams WHERE deleted = \'0\' AND name like \'' . $_REQUEST['query'] . '%\' LIMIT ' . $_REQUEST['start'] . ',' . $_REQUEST['limit']);
      //else
      //	$usersResult = $db->query('SELECT id, user_name FROM users WHERE deleted = \'0\' AND status = \'Active\'');

      while ($teamRecord = $db->fetchByAssoc($teamResult)) {
         // bugfix 2010-09-28 since id was asisgned and not user name ..  no properly evaluates active user
         $returnArray['data'][] = array('value' => $teamRecord['id'], 'text' => $teamRecord['name'] . ' ' . $teamRecord['name_2']);
      }

      echo json_encode($returnArray);
   }

   function action_get_securitygroups(){
      global $db;

      $returnArray['count'] = $db->getRowCount($db->query('SELECT id, name FROM securitygroups WHERE deleted = \'0\'  AND name like \'' . $_REQUEST['query'] . '%\''));

      //if(isset($_REQUEST['query']) && $_REQUEST['query'] != '')
      $teamResult = $db->query('SELECT id, name FROM securitygroups WHERE deleted = \'0\' AND name like \'' . $_REQUEST['query'] . '%\' LIMIT ' . $_REQUEST['start'] . ',' . $_REQUEST['limit']);
      //else
      //	$usersResult = $db->query('SELECT id, user_name FROM users WHERE deleted = \'0\' AND status = \'Active\'');

      while ($teamRecord = $db->fetchByAssoc($teamResult)) {
         // bugfix 2010-09-28 since id was asisgned and not user name ..  no properly evaluates active user
         $returnArray['data'][] = array('value' => $teamRecord['id'], 'text' => $teamRecord['name']);
      }

      echo json_encode($returnArray);
   }
   
   function action_get_korgobjects() {
      global $db;
      require_once('modules/KOrgObjects/KOrgObject.php');
      $thisOrgObject = new KOrgObject();
      $returnArray['count'] = $db->getRowCount($db->query($thisOrgObject->getEditViewOrgUnitQuery('KReports', $_REQUEST['query'])));
      $queryObj = $db->query($thisOrgObject->getEditViewOrgUnitQuery('KReports', $_REQUEST['query']) . ' LIMIT ' . $_REQUEST['start'] . ',' . $_REQUEST['limit']);
      while ($korgobjectrecord = $db->fetchByAssoc($queryObj)) {
         $returnArray['data'][] = array('value' => $korgobjectrecord['id'], 'text' => $korgobjectrecord['name']);
      }
      echo json_encode($returnArray);
   }

   function action_get_autocompletevalues() {
      global $beanFiles, $beanList, $db;

      $returnArray = array();
      $fieldArray = array();


      // explode the path
      $pathArray = explode('::', $_REQUEST['path']);

      // get Field and Module from the path
      $fieldArray = explode(':', $pathArray[count($pathArray) - 1]);
      $moduleArray = explode(':', $pathArray[count($pathArray) - 2]);

      // load the parent module
      require_once($beanFiles[$beanList[$moduleArray[1]]]);
      $parentModule = new $beanList[$moduleArray[1]];

      if ($moduleArray[0] == 'link') {
         // load the Relationshop to get the module
         $parentModule->load_relationship($moduleArray[2]);
		$nodeEL = $moduleArray[2];
         // load the Module
         $thisModuleName = $parentModule->$nodeEl->getRelatedModuleName();
         require_once($beanFiles[$beanList[$thisModuleName]]);
         $thisModule = new $beanList[$thisModuleName];
      } else
         $thisModule = $parentModule;

      if ($thisModule->table_name != '') {

         // determine the field name we need to go for 
         $fieldName = 'name';
         // #bug 520 changed to object rather than array.
         if ($fieldArray[0] == 'field' && isset($thisModule->field_name_map[$fieldArray[1]]) && $fieldArray[1] != 'id')
            $fieldName = $fieldArray[1];

         $query_res = $db->limitQuery("SELECT id, " . $fieldName . " FROM $thisModule->table_name WHERE " . (!empty($_REQUEST['query']) ? "name like '%" . $_REQUEST['query'] . "%' AND" : "") . " deleted='0' ORDER BY name ASC", (!empty($_REQUEST['start']) ? $_REQUEST['start'] : 0), (!empty($_REQUEST['limit']) ? $_REQUEST['limit'] : 25));
         while ($thisEntry = $db->fetchByAssoc($query_res)) {
            $returnArray['data'][] = array('itemid' => $thisEntry['id'], 'itemtext' => $thisEntry[$fieldName]);
         }

         // get count
         $totalRec = $db->fetchByAssoc($db->query("SELECT count(*) as count FROM $thisModule->table_name WHERE " . (!empty($_REQUEST['query']) ? "name like '%" . $_REQUEST['query'] . "%' AND" : "") . " deleted='0'"));
         $returnArray['total'] = $totalRec['count'];
      }

      echo json_encode($returnArray);
   }

   function action_get_enum() {

      global $app_list_strings, $beanFiles, $beanList, $db;

      // explode the path
      $pathArray = explode('::', $_REQUEST['path']);

      // get Field and Module from the path
      $fieldArray = explode(':', $pathArray[count($pathArray) - 1]);
      $moduleArray = explode(':', $pathArray[count($pathArray) - 2]);


      // load the parent module
      require_once($beanFiles[$beanList[$moduleArray[1]]]);
      $parentModule = new $beanList[$moduleArray[1]];

      if ($moduleArray[0] == 'link' || $moduleArray[0] == 'relate') {
         switch ($moduleArray[0]) {
            case 'link':
               // load the Relationshop to get the module
               $parentModule->load_relationship($moduleArray[2]);
				$nodeEL = $moduleArray[2];
               // load the Module
               $thisModuleName = $parentModule->$nodeEL->getRelatedModuleName();
               require_once($beanFiles[$beanList[$thisModuleName]]);
               $thisModule = new $beanList[$thisModuleName];
               break;
            //2013-09-25 add support for relate fields BUG #499
            case 'relate':
               require_once($beanFiles[$beanList[$moduleArray['1']]]);
               $nodeModule = new $beanList[$moduleArray['1']]();
               $thisModuleName = $nodeModule->field_defs[$moduleArray[2]]['module'];
               require_once($beanFiles[$beanList[$thisModuleName]]);
               $thisModule = new $beanList[$thisModuleName]();
               break;
         }

         //2013-02-28 if we have the kreporttype set ... override the type
         if ($thisModule->field_name_map[$fieldArray[1]]['type'] == 'kreporter' && !empty($thisModule->field_name_map[$fieldArray[1]]['kreporttype']))
            $thisModule->field_name_map[$fieldArray[1]]['type'] = $thisModule->field_name_map[$fieldArray[1]]['kreporttype'];


         // pars the otpions into the return array
         switch ($thisModule->field_name_map[$fieldArray[1]]['type']) {
            case 'enum':
            case 'radioenum':
            case 'multienum':
            case 'dynamicenum' :
               foreach ($app_list_strings[$thisModule->field_name_map[$fieldArray[1]]['options']] as $value => $text) {
                  $returnArray[] = array('value' => $value, 'text' => $text);
               }
               break;
            case 'parent_type':
               // bug 2011-08-08 we assume it is parent_name 
               // not completely correct since we should look for the field where the name is the type but will be sufficient
               foreach ($app_list_strings[$thisModule->field_name_map['parent_name']['options']] as $value => $text) {
                  $returnArray[] = array('value' => $value, 'text' => $text);
               }
               break;
            case 'user_name':
            case 'assigned_user_name':
               global $locale;
               $returnArray[] = array('value' => 'current_user_id', 'text' => 'active user');
               $usersResult = $db->query('SELECT id, user_name, first_name, last_name FROM users WHERE deleted = \'0\' AND status=\'Active\' ORDER BY last_name'); //  AND status = \'Active\'');
               while ($userRecord = $db->fetchByAssoc($usersResult)) {
                  // bugfix 2010-09-28 since id was asisgned and not user name ..  no properly evaluates active user
                  // bugfix 2012-03-29 proper name formatting based on user preferences
                  // $returnArray[] = array('value' => $userRecord['user_name'], 'text' => ($userRecord['last_name'] =! '' ? $userRecord['first_name'] . ' ' . $userRecord['last_name'] : $userRecord['user_name']));
                  $returnArray[] = array('value' => $userRecord['user_name'], 'text' => ($userRecord['last_name'] = !'' ? $locale->getLocaleFormattedName($userRecord['first_name'], $userRecord['last_name'], '') : $userRecord['user_name']));
               }
               break;
            case 'team_name':
               $teamsResult = $db->query('SELECT team_name FROM teams WHERE deleted = \'0\' ORDER BY name'); //  AND status = \'Active\'');
               while ($teamRecord = $db->fetchByAssoc($teamsResult)) {
                  // bugfix 2010-09-28 since id was asisgned and not user name ..  no properly evaluates active user
                  $returnArray[] = array('value' => $teamRecord['name'], 'text' => $teamRecord['name']);
               }
               break;
         }
      } else {

         //2013-02-28 if we have the kreporttype set ... override the type
         if ($parentModule->field_name_map[$fieldArray[1]]['type'] == 'kreporter' && !empty($parentModule->field_name_map[$fieldArray[1]]['kreporttype']))
            $parentModule->field_name_map[$fieldArray[1]]['type'] = $parentModule->field_name_map[$fieldArray[1]]['kreporttype'];

         // we have the root module
         switch ($parentModule->field_name_map[$fieldArray[1]]['type']) {
            case 'enum':
            case 'radioenum':
            case 'multienum':
            case 'dynamicenum' :
               foreach ($app_list_strings[$parentModule->field_name_map[$fieldArray[1]]['options']] as $value => $text) {
                  $returnArray[] = array('value' => $value, 'text' => $text);
               }
               break;
            case 'parent_type':
               // bug 2011-08-08 we assume it is parent_name 
               // not completely correct since we should look for the field where the name is the type but will be sufficient
               foreach ($app_list_strings[$parentModule->field_name_map['parent_name']['options']] as $value => $text) {
                  $returnArray[] = array('value' => $value, 'text' => $text);
               }
               break;
            case 'user_name':
            case 'assigned_user_name':
               $returnArray[] = array('value' => 'current_user_id', 'text' => 'active user');
               $usersResult = $db->query('SELECT id, user_name FROM users WHERE deleted = \'0\' AND status = \'Active\'');
               while ($userRecord = $db->fetchByAssoc($usersResult)) {
                  // bugfix 2010-09-28 since id was asisgned and not user name ..  no properly evaluates active user
                  $returnArray[] = array('value' => $userRecord['user_name'], 'text' => $userRecord['user_name']);
               }
               break;
         }
      }


      print json_encode_kinamu($returnArray);
   }

   /*
    * Custom Action to load the Report Data
    * also gets called during paging limit and start currently only works for MySQL
    * MSSQL needs adoption
    */

   function action_load_report() {
      global $db;

      // set_time_limit(1);
      //sleep(60);
      //2013-09-25 start OB Buffering
      ob_start();

      require_once('modules/KReports/KReport.php');

      $thisReport = new KReport();
      $thisReport->retrieve($_REQUEST['requester']);

      // set start and limit if not set 
      if (!isset($_REQUEST['start']))
         $_REQUEST['start'] = 0;
      if (!isset($_REQUEST['limit']))
         $_REQUEST['limit'] = 0;

      // set the override Where if set in the request
      if (isset($_REQUEST['whereConditions'])) {
         $thisReport->whereOverride = json_decode(html_entity_decode($_REQUEST['whereConditions']), true);
      }

      // set request Paramaters
      $reportParams = array('noFormat' => true, 'start' => isset($_REQUEST['start']) ? $_REQUEST['start'] : 0, 'limit' => isset($_REQUEST['limit']) ? $_REQUEST['limit'] : 0);

      // see if we should dort
      //if(isset($parameters['sortseq'])) $paramsArray['sortseq'] = $parameters['sortseq'];
      //if(isset($parameters['sortid'])) $paramsArray['sortid'] = $parameters['sortid'];
      if (isset($_REQUEST['sort']) && isset($_REQUEST['dir'])) {
         $reportParams['sortseq'] = $_REQUEST['dir'];
         $reportParams['sortid'] = $_REQUEST['sort'];
      } elseif (isset($_REQUEST['sort'])) {
         $sortParams = json_decode(html_entity_decode($_REQUEST['sort']));
         $reportParams['sortid'] = $sortParams[0]->property;
         $reportParams['sortseq'] = $sortParams[0]->direction;
      }

      $totalArray = array();
      $totalArray['records'] = $thisReport->getSelectionResults($reportParams, isset($_REQUEST['snapshotid']) ? $_REQUEST['snapshotid'] : '0', false);

      // rework ... load from kQuery fieldArray
      $fieldArr = array();

      //2012-12-01 added link array to add to metadata for buiilding links in the frontend
      $linkArray = $thisReport->buildLinkArray($thisReport->kQueryArray->queryArray['root']['kQuery']->fieldArray);

      foreach ($thisReport->kQueryArray->queryArray['root']['kQuery']->fieldArray as $fieldid => $fieldname) {
         $thisFieldArray = array('name' => $fieldname);
         if (isset($linkArray[$fieldid]))
            $thisFieldArray['linkInfo'] = json_encode($linkArray[$fieldid]);
         $fieldArr[] = $thisFieldArray;
      }

      $totalArray['metaData'] = array(
          'totalProperty' => 'count',
          'root' => 'records',
          'fields' => $fieldArr
      );
      // 2012-01-18 Ende
      // 2012-10-02 to process totals in the grid
      // 2013-02-25
      // send the total along
      if ($thisReport->kQueryArray->summarySelectString != '') {
         $totalArray['recordtotal'] = $db->fetchByAssoc($db->query($thisReport->kQueryArray->summarySelectString));
         $thisReport->processFormulas($totalArray['recordtotal']);
      }

      // do a count 
      if (isset($_REQUEST['doCount']) && $_REQUEST['doCount'] == 'true') {
         $totalArray['count'] = $thisReport->getSelectionResults(array('start' => $_REQUEST['start'], 'limit' => $_REQUEST['limit']), isset($_REQUEST['snapshotid']) ? $_REQUEST['snapshotid'] : '0', true);
      } else {
         $totalArray['count'] = (count($totalArray['records']) < $_REQUEST['limit'] ? $_REQUEST['start'] + count($totalArray['records']) : $_REQUEST['start'] + $_REQUEST['limit'] + 1);
      }



      // jscon encode the result and return it
      $json_string = json_encode_kinamu($totalArray);

      //2013-09-25 start OB Buffering
      $collectedMessage = ob_end_flush();
      if ($collectedMessage)
         $GLOBALS['log']->debug('KReport Message collected: ' . $collectedMessage);

      // echo the response
      echo $json_string;

      exit();
   }

   function action_load_report_count() {
      global $db;

      require_once('modules/KReports/KReport.php');

      $thisReport = new KReport();
      $thisReport->retrieve($_REQUEST['requester']);

      // set the override Where if set in the request
      if (isset($_REQUEST['whereConditions'])) {
         $thisReport->whereOverride = json_decode_kinamu(html_entity_decode($_REQUEST['whereConditions']));
      }

      echo $thisReport->getSelectionResults(array('start' => ($_REQUEST['start'] ?? null), 'limit' => ($_REQUEST['limit'] ?? null)), isset($_REQUEST['snapshotid']) ? $_REQUEST['snapshotid'] : '0', true);
   }

   function action_get_nodes() {
      // main processing
      global $_REQUEST, $beanFiles, $beanList;

      if ($_REQUEST['node'] != 'unionroot') {

         //2013-02-08 unstrip the path information
         if (strpos($_REQUEST['node'], '#') !== false) {
            $preNodeArray = explode('#', $_REQUEST['node']);
            $thisNode = $preNodeArray[1];
         } else
            $thisNode = $_REQUEST['node'];

         $nodeArray = explode(':', $thisNode);

         $returnArray = array();

         if ($nodeArray[0] == 'root' || preg_match('/union/', $nodeArray[0]) > 0) {
            print json_encode_kinamu($this->buildNodeArray($nodeArray['1'], $_REQUEST['node']));
         }

         if ($nodeArray[0] == 'link') {
            require_once($beanFiles[$beanList[$nodeArray['1']]]);
            $nodeModule = new $beanList[$nodeArray['1']];
            $nodeModule->load_relationship($nodeArray['2']);
            $nodeEL = $nodeArray['2'];

            $returnJArray = json_encode_kinamu($this->buildNodeArray($nodeModule->$nodeEL->getRelatedModuleName(), 'TREE', $nodeModule->$nodeEL, $nodeArray['2']));

            print $returnJArray;
         }

         //2013-01-09 add support for Studio Relate Fields
         if ($nodeArray[0] == 'relate') {
            require_once($beanFiles[$beanList[$nodeArray['1']]]);
            $nodeModule = new $beanList[$nodeArray['1']];
            $nodeEL = $nodeArray['2'];

            $returnJArray = json_encode_kinamu($this->buildNodeArray($nodeModule->field_defs[$nodeArray[2]]['module'], 'TREE', $nodeModule->$nodeEL, $nodeArray['2']));

            print $returnJArray;
         }
      } else
         echo '';
   }

   /*
    * Custom Action to get the Fields for a Module
    */

   function action_get_fields() {
      global $_REQUEST, $beanFiles, $beanList;

      $nodeArray = explode(':', $_REQUEST['nodeid']);

      $returnArray = array();

      // check if we have the root module or a union module ...
      if ($nodeArray[0] == 'root' || preg_match('/union/', $nodeArray[0]) == 1) {
         print json_encode_kinamu($this->buildFieldArray($nodeArray['1']));
      }
      if ($nodeArray[0] == 'link') {
            $nodeModule = BeanFactory::getBean($nodeArray['1']);
            $nodeModule->load_relationship($nodeArray['2']);
            $nodeArrayEl = $nodeArray['2'];
            $returnJArray = json_encode_kinamu($this->buildFieldArray($nodeModule->$nodeArrayEl->getRelatedModuleName()));
	        print $returnJArray;
      }

      //2013-01-09 add support for Studio Relate Fields
      if ($nodeArray[0] == 'relate') {
         require_once($beanFiles[$beanList[$nodeArray['1']]]);
         $nodeModule = new $beanList[$nodeArray['1']];
         $returnJArray = json_encode_kinamu($this->buildFieldArray($nodeModule->field_defs[$nodeArray[2]]['module']));

         print $returnJArray;
      }

      if ($nodeArray[0] == 'relationship') {
         require_once($beanFiles[$beanList[$nodeArray['1']]]);
         $nodeModule = new $beanList[$nodeArray['1']];
         $nodeModule->load_relationship($nodeArray['2']);
         $nodeArrayEl = $nodeArray['2'];
         $returnJArray = json_encode_kinamu($this->buildLinkFieldArray($nodeModule->$nodeArrayEl));

         print $returnJArray;
      }
      if ($nodeArray[0] == 'audit') {
         $returnJArray = json_encode_kinamu($this->buildAuditFieldArray());

         print $returnJArray;
      }
   }

   /*
    * Helper function to get the Fields for a module
    */

   function buildFieldArray($module) {
      global $beanFiles, $beanList;
      require_once('include/utils.php');
      $returnArray = array();
      if ($module != '' && $module != 'undefined' && file_exists($beanFiles[$beanList[$module]])) {
         require_once($beanFiles[$beanList[$module]]);
         $nodeModule = new $beanList[$module];

         foreach ($nodeModule->field_name_map as $field_name => $field_defs) {
            if ($field_defs['type'] != 'link' && (!isset($field_defs['reportable']) || (isset($field_defs['reportable']) && $field_defs['reportable'] == true))
                    //&& $field_defs['type'] != 'relate'
                    && (!array_key_exists('source', $field_defs) ||
                    (array_key_exists('source', $field_defs) && (
                    $field_defs['source'] != 'non-db' || ($field_defs['source'] == 'non-db' && $field_defs['type'] == 'kreporter')
                    )
                    ))
            ) {
               $returnArray[] = array(
                   'id' => 'field:' . $field_defs['name'],
                   'text' => $field_defs['name'],
                   // in case of a kreporter field return the report_data_type so operators ar processed properly
                   // 2011-05-31 changed to kreporttype returned if fieldttype is kreporter
                   // 2011-10-15 if the kreporttype is set return it
                   //'type' => ($field_defs['type'] == 'kreporter') ? $field_defs['kreporttype'] :  $field_defs['type'],
                   'type' => (isset($field_defs['kreporttype'])) ? $field_defs['kreporttype'] : $field_defs['type'],
                   'name' => (translate($field_defs['vname'], $module) != '') ? translate($field_defs['vname'], $module) : $field_defs['name'],
                   'leaf' => true
               );
            }
         }
      }

      // 2013-08-21 Bug#493 sorting name for the fields   
      usort($returnArray, "arraySortByName");

      return $returnArray;
   }

   /*
    * Helper Function to build the nodes ...
    */

   function buildNodeArray($module, $requester, $thisLink = '', $thisLinkName = '') {
      global $beanFiles, $beanList;
      require_once('include/utils.php');

      include('modules/KReports/kreportsConfig.php');

      $returnArray = array();

      // 2013-08-21 BUG #492 create a functional eleent holding the leafs for audit and relationships to make sure they stay on top after sorting
      $functionsArray = array();

      if (file_exists($beanFiles[$beanList[$module]])) {
         require_once($beanFiles[$beanList[$module]]);
         $nodeModule = new $beanList[$module];

         $nodeModule->load_relationships();

         // 2011-07-21 add audit table
         if (isset($GLOBALS['dictionary'][$nodeModule->object_name]['audited']) && $GLOBALS['dictionary'][$nodeModule->object_name]['audited'])
            $functionsArray[] = array(
                'id' => /* ($requester != '' ? $requester. '#': '') . */ 'audit:' . $module . ':audit',
                'text' => 'auditRecords',
                'leaf' => true
            );

         //2011-08-15 add relationship fields in many-to.many relationships          
         //2012-03-20 change for 6.4
         if ($thisLink != '' && get_class($thisLink) == 'Link2') {
            if ($thisLink != '' && $thisLink->_relationship->relationship_type == 'many-to-many')
               $functionsArray[] = array(
                   'id' => /* ($requester != '' ? $requester. '#': '') . */ 'relationship:' . $thisLink->focus->module_dir /* $module */ . ':' . $thisLinkName,
                   'text' => 'relationship Fields',
                   'leaf' => true
               );
         }
         else {
            if ($thisLink != '' && $thisLink->_relationship->relationship_type == 'many-to-many')
               $functionsArray[] = array(
                   'id' => /* ($requester != '' ? $requester. '#': '') . */ 'relationship:' . $thisLink->_bean->module_dir /* $module */ . ':' . $thisLinkName,
                   'text' => 'relationship Fields',
                   'leaf' => true
               );
         }

         foreach ($nodeModule->field_name_map as $field_name => $field_defs) {
            // 2011-03-23 also exculde the excluded modules from the config in the Module Tree
            //if ($field_defs['type'] == 'link' && (!isset($field_defs['module']) || (isset($field_defs['module']) && array_search($field_defs['module'], $excludedModules) == false))) {
            if ($field_defs['type'] == 'link' && (!isset($field_defs['reportable']) || (isset($field_defs['reportable']) && $field_defs['reportable'])) && (!isset($field_defs['module']) || (isset($field_defs['module']) && array_search($field_defs['module'], $excludedModules) == false))) {
               //BUGFIX 2010/07/13 to display alternative module name if vname is not maintained
               if (isset($field_defs['vname']))
                  $returnArray[] = array(
                      'id' => /* ($requester != '' ? $requester. '#': '') . */ 'link:' . $module . ':' . $field_name,
                      'text' => ((translate($field_defs['vname'], $module)) == "" ? ('[' . $field_defs['name'] . ']') : (translate($field_defs['vname'], $module))),
                      'leaf' => false
                  );
               elseif (isset($field_defs['module']))
                  $returnArray[] = array(
                      'id' => /* ($requester != '' ? $requester. '#': '') . */ 'link:' . $module . ':' . $field_name,
                      'text' => translate($field_defs['module'], $module),
                      'leaf' => false
                  );
               else
                  $returnArray[] = array(
                      'id' => /* ($requester != '' ? $requester. '#': '') . */ 'link:' . $module . ':' . $field_name,
                      'text' => get_class($nodeModule->$field_defs['relationship']->_bean),
                      'leaf' => false
                  );
            }

            //2013-01-09 add support for Studio Relate Fields
            // get all relate fields where the link is empty ... those with link we get via the link anyway properly
            if ($field_defs['type'] == 'relate' && empty($field_defs['link']) && (!isset($field_defs['reportable']) || (isset($field_defs['reportable']) && $field_defs['reportable'])) && (!isset($field_defs['module']) || (isset($field_defs['module']) && array_search($field_defs['module'], $excludedModules) == false))) {
               if (isset($field_defs['vname']))
                  $returnArray[] = array(
                      'id' => 'relate:' . $module . ':' . $field_name,
                      'text' => ((translate($field_defs['vname'], $module)) == "" ? ('[' . $field_defs['name'] . ']') : (translate($field_defs['vname'], $module))),
                      'leaf' => false
                  );
               elseif (isset($field_defs['module']))
                  $returnArray[] = array(
                      'id' => 'relate:' . $module . ':' . $field_name,
                      'text' => translate($field_defs['module'], $module),
                      'leaf' => false
                  );
               else
                  $returnArray[] = array(
                      'id' => 'relate:' . $module . ':' . $field_name,
                      'text' => $field_defs['name'],
                      'leaf' => false
                  );
            }
         }
      }
      // 2013-08-21 BUG #492 added sorting for the module tree 
      usort($returnArray, "arraySortByText");

      // 2013-08-21 BUG #492 merge with the basic functional elelements 
      return array_merge($functionsArray, $returnArray);
   }

   function buildLinkFieldArray($thisLink) {

      global $db;

      $queryRes = $db->query('describe ' . $thisLink->_relationship->join_table);

      while ($thisRow = $db->fetchByAssoc($queryRes)) {
         $returnArray[] = array(
             'id' => 'field:' . $thisRow['Field'],
             'text' => $thisRow['Field'],
             // in case of a kreporter field return the report_data_type so operators ar processed properly
             'type' => 'varchar',
             'name' => $thisRow['Field'],
             'leaf' => true
         );
      }

      return $returnArray;
   }

   function buildAuditFieldArray() {

      //date_created
      $returnArray[] = array(
          'id' => 'field:date_created',
          'text' => 'date_created',
          'type' => 'datetime',
          'name' => 'date_created',
          'leaf' => true
      );

      $returnArray[] = array(
          'id' => 'field:created_by',
          'text' => 'created_by',
          'type' => 'varchar',
          'name' => 'created_by',
          'leaf' => true
      );

      $returnArray[] = array(
          'id' => 'field:field_name',
          'text' => 'field_name',
          'type' => 'varchar',
          'name' => 'field_name',
          'leaf' => true
      );

      $returnArray[] = array(
          'id' => 'field:before_value_string',
          'text' => 'before_value_string',
          'type' => 'varchar',
          'name' => 'before_value_string',
          'leaf' => true
      );

      $returnArray[] = array(
          'id' => 'field:after_value_string',
          'text' => 'after_value_string',
          'type' => 'varchar',
          'name' => 'after_value_string',
          'leaf' => true
      );

      $returnArray[] = array(
          'id' => 'field:before_value_text',
          'text' => 'before_value_text',
          'type' => 'text',
          'name' => 'before_value_text',
          'leaf' => true
      );

      $returnArray[] = array(
          'id' => 'field:after_value_text',
          'text' => 'after_value_text',
          'type' => 'text',
          'name' => 'after_value_text',
          'leaf' => true
      );
      return $returnArray;
   }

   function action_getVisualizationPreview() {
      require_once('modules/KReports/KReportVisualizationManager.php');
      $thisVisualizationmanager = new KReportVisualizationManager();
      echo $thisVisualizationmanager->generateLayoutPreview($_REQUEST['layout']);
   }

   /*
    * get the update for the visualization
    */

   function action_update_visualization() {
      require_once('modules/KReports/KReportVisualizationManager.php');
      $thisReport = new KReport();
      $thisReport->retrieve($_REQUEST['record']);
      
      
      
      $thisVisualizationmanager = new KReportVisualizationManager();
      echo $thisVisualizationmanager->updateVisualization(html_entity_decode($thisReport->visualization_params, ENT_QUOTES, 'UTF-8'), $thisReport, ($_REQUEST['snapshotid'] != '' ? $_REQUEST['snapshotid'] : 0));
   }

   function action_get_trendchart() {

      require_once('modules/KReports/KReport.php');
      require_once('modules/KReports/KReportChart.php');
      $thisReport = new KReport();
      $thisReport->retrieve($_REQUEST['record']);
      $thisReportchart = new KReportChart($thisReport);

      $result = $thisReportchart->renderTrendChart($_REQUEST['height'], $_REQUEST['dataSeriesFieldId'], $_REQUEST['dimensionsFieldId'], $_REQUEST['chartid'], $_REQUEST['chartType']);

      echo $result;
   }

   /*
    * function that returns the generated SQL Query
    */
   /*
     function action_get_sql() {
     require_once('modules/KReports/KReport.php');
     $thisReport = new KReport();
     $thisReport->retrieve($_REQUEST['record']);

     // set the override Where if set in the request
     if (isset($_REQUEST['whereConditions']) && $_REQUEST['whereConditions'] != '') {
     $thisReport->whereOverride = json_decode_kinamu(html_entity_decode($_REQUEST['whereConditions']));
     }

     //echo $thisReport->get_report_main_sql_query('', true, '');

     echo $thisReport->get_report_main_sql_query(true, '', '');
     echo '

     count

     ';
     echo $thisReport->kQueryArray->countSelectString;

     echo '

     total

     ';
     echo $thisReport->kQueryArray->totalSelectString;

     //$sqlArray = $thisReport->get_report_main_sql_query('', true, '');
     //echo $sqlArray['select'] . ' ' . $sqlArray['from'] . ' ' . $sqlArray['where'] . ' ' . $sqlArray['groupby'] . ' ' . $sqlArray['having'] . ' ' . $sqlArray['orderby'];
     }
    */

   function action_duplicate_report() {
      global $db, $beanList, $current_user;

      require_once('modules/KReports/KReport.php');

      //$thisReport->retrieve($_REQUEST['record']);

      $row = $db->fetchByAssoc($db->query('SELECT * FROM kreports WHERE id=\'' . $_REQUEST['record'] . '\''));

      $thisReport = new KReport();
      $thisReport->populateFromRow($row);
      $thisReport->id = create_guid();
      $thisReport->new_with_id = true;
      $thisReport->name = $_REQUEST['newName'];

      //2012-12-12 ... manage ids of visualization
      if ($thisReport->visualization_params != '') {
         $visParams = json_decode(html_entity_decode($thisReport->visualization_params), true);

         foreach ($visParams as $visId => $visData) {
            if (is_array($visData)) {
               foreach ($visData as $thisPlugin => $thisPluginData) {
                  if (is_array($thisPluginData) && !empty($thisPluginData['uid'])) {
                     $newUid = 'k' . substr(str_replace('-', '', create_guid()), 0, 28);
                     $visParams[$visId][$thisPlugin]['uid'] = $newUid;
                  }
               }
            }
         }
         $thisReport->visualization_params = json_encode($visParams);
      }

      // some cleanup
      $thisReport->date_entered = '';
      $thisReport->assigned_user_id = $current_user->id;

      $thisReport->save(false);

      if ($beanList['KOrgObjects']) {
         // also duplicate the privileges if korgobjects is installed
         $resultSet = $db->query("SELECT * FROM korgobjects_beans WHERE bean_id = '" . $db->quote($_REQUEST['record']) . "' AND bean_name = 'KReport' AND deleted = 0");
         while ($row = $db->fetchByAssoc($resultSet)) {
            $db->query("INSERT INTO korgobjects_beans (id, korgobject_id, bean_id, date_modified, bean_name, from_sap, deleted)
                               VALUES ('" . create_guid() . "', '" . $row['korgobject_id'] . "', '" . $thisReport->id . "', '" . $row['date_modified'] . "', '" . $row['bean_name'] . "', '" . $row['from_sap'] . "', '" . $row['deleted'] . "')");
         }
      }
      
   }

   // for the maps integration
   function action_get_report_geocodes() {

      $thisReport = new KReport();
      $thisReport->retrieve($_REQUEST['record']);

      // check if we have set dynamic Options
      if (isset($_REQUEST['whereConditions']))
         $thisReport->whereOverride = json_decode_kinamu(html_entity_decode($_REQUEST['whereConditions']));

      echo $thisReport->getGeoCodes();
   }

   /*
    * Function to generate Target List
    */

   function action_geocode_report_results() {
      $thisReport = new KReport();
      $thisReport->retrieve($_REQUEST['record']);

      // check if we have set dynamic Options
      if (isset($_REQUEST['whereConditions']))
         $thisReport->whereOverride = json_decode_kinamu(html_entity_decode($_REQUEST['whereConditions']));

      $thisReport->massGeoCode();

      return true;
   }

   /*
    * function to deliuver html data for Rpeort and Dashlet
    */

   function action_getReportHtmlResults() {
      require_once('modules/KReports/views/view.htmlpremium.php');
      $thisReport = new KReport();
      $thisReport->retrieve($_REQUEST['record']);

      // see if we have custom Dashlet Filters
      if (isset($_REQUEST['whereClause'])) {
         $whereClause = json_decode(html_entity_decode($_REQUEST['whereClause']), true);

         foreach ($whereClause as $whereClauseData) {
            $dashletWhereClause[$whereClauseData['fieldid']] = $whereClauseData;
         }

         // get and update Report where Clause
         $repWhereClause = json_decode(html_entity_decode($this->bean->whereconditions), true);

         foreach ($repWhereClause as $repWhereClauseIndex => $repWhereClauseData) {
            if (isset($dashletWhereClause[$repWhereClauseData['fieldid']]) && $dashletWhereClause[$repWhereClauseData['fieldid']]['value'] !== 'KRignoreFilter') {
               switch ($repWhereClause[$repWhereClauseIndex]['usereditable']) {
                  // if we are a checkbox set either on or off ...
                  // still not happy with this but it works
                  case 'yo2':
                  case 'yo1':
                     if ($dashletWhereClause[$repWhereClauseData['fieldid']]['value'] == true)
                        $repWhereClause[$repWhereClauseIndex]['usereditable'] = 'yo1';
                     else
                        $repWhereClause[$repWhereClauseIndex]['usereditable'] = 'yo2';
                     break;
                  // default handling
                  default:
                     switch ($repWhereClause[$repWhereClauseIndex]['type']) {
                        case 'enum':
                        case 'radioenum':
		                case 'dynamicenum' :
                        case 'date':
                        case 'datetime':
                           $repWhereClause[$repWhereClauseIndex]['value'] = $dashletWhereClause[$repWhereClauseData['fieldid']]['value'];
                           $repWhereClause[$repWhereClauseIndex]['valuekey'] = $dashletWhereClause[$repWhereClauseData['fieldid']]['value'];

                           // if clause has not been set we assume it has to be equal
                           if ($repWhereClause[$repWhereClauseIndex]['operator'] == 'ignore')
                              $repWhereClause[$repWhereClauseIndex]['operator'] = 'equals';
                           break;
                        default:
                           $repWhereClause[$repWhereClauseIndex]['value'] = $dashletWhereClause[$repWhereClauseData['fieldid']]['value'];
                           if ($repWhereClause[$repWhereClauseIndex]['operator'] == 'ignore')
                              $repWhereClause[$repWhereClauseIndex]['operator'] = 'contains';
                           break;
                     }
                     break;
               }
            }
            elseif (isset($dashletWhereClause[$repWhereClauseData['fieldid']]) && $dashletWhereClause[$repWhereClauseData['fieldid']]['value'] == 'KRignoreFilter') {
               $repWhereClause[$repWhereClauseIndex]['operator'] = 'ignore';
            }
         }

         $this->bean->whereconditions = json_encode_kinamu($repWhereClause);
      }

      echo json_encode(
              array(
                  'content' => reportResultsToHTML($_REQUEST['divID'], $this->bean, array('start' => $_REQUEST['start'], 'limit' => $_REQUEST['limit'], 'dashletLinks' => true)),
                  'divId' => $_REQUEST['divID'],
                  'reloadInterval' => 3000,
                  'reportId' => $_REQUEST['record'],
                  'start' => $_REQUEST['start'],
                  'limit' => $_REQUEST['limit']
              )
      );
   }

   /**
    * Save controller function
    * @see SugarController::action_save()
    */
   function action_save() {
      global $mod_strings;

      if (empty($this->bean->name)) {
         $this->bean->name = $mod_strings['LBL_DEFAULT_NAME'];
      }

      $this->bean->save();
   }

   /*
    * Function to save standard list layout
    */

   function action_save_standard_layout() {
      $thisReport = new KReport();
      $thisReport->retrieve($_REQUEST['record']);

      $layoutParams = json_decode_kinamu(html_entity_decode($_REQUEST['layoutparams']));

      $listFields = json_decode_kinamu(html_entity_decode($thisReport->listfields));

      // process the Fields
      foreach ($listFields as $thisFieldIndex => $thisListField) {
         reset($layoutParams);
         foreach ($layoutParams as $thisLayoutParam) {
            if ($thisLayoutParam['dataIndex'] == $thisListField['fieldid']) {
               $thisListField['width'] = $thisLayoutParam['width'];
               $thisListField['sequence'] = (string) $thisLayoutParam['sequence'];

               // bug 2011-03-04 sequence needs leading 0
               if (strlen($thisListField['sequence']) < 2)
                  $thisListField['sequence'] = '0' . $thisListField['sequence'];

               $thisListField['display'] = $thisLayoutParam['isHidden'] ? 'no' : 'yes';
               $listFields[$thisFieldIndex] = $thisListField;
               break;
            }
         }
      }

      usort($listFields, 'arraySortBySequence');

      $thisReport->listfields = json_encode_kinamu($listFields);
      echo $thisReport->save();
      echo $thisReport->listfields;
   }

   /*
     function action_get_new_sql() {
     require_once('modules/KReports/KReport.php');
     require_once('modules/KReports/KReportQuery.php');

     $thisReport = new KReport();
     $thisReport->retrieve($_REQUEST['record']);

     if (isset($_REQUEST['whereConditions'])) {
     $thisReport->whereOverride = json_decode_kinamu(html_entity_decode($_REQUEST['whereConditions']));
     }

     $sqlArray = $thisReport->get_report_main_sql_query();

     return $sqlArray['select'] . ' ' . $sqlArray['from'] . ' ' . $sqlArray['where'] . ' ' . $sqlArray['groupby'] . ' ' . $sqlArray['having'] . ' ' . $sqlArray['orderby'];
     }
    */

   function action_get_pushpinimages() {
      //open the pushpin directory
      $retArray = array();
      if ($handle = opendir('modules/KReports/images/pushpins')) {

         while (false !== ($file = readdir($handle))) {
            if (substr($file, 0, 1) != '.')
               $retArray[] = array('filename' => $file);
         }

         closedir($handle);
      }
      echo json_encode($retArray);
   }

}

/*
 * function for array sorting
 */

function arraySortBySequence($a, $b) {
   return ($a['sequence'] < $b['sequence']) ? -1 : 1;
}

// 2013-08-21 BUG #492 function to be called from usort to sort by Text
function arraySortByText($a, $b) {
   if (strtolower($a['text']) > strtolower($b['text']))
      return 1;
   elseif (strtolower($a['text']) == strtolower($b['text']))
      return 0;
   else
      return -1;
}

// 2013-08-21 Bug#493 sorting name for the fields    
function arraySortByName($a, $b) {
   if (strtolower($a['name']) > strtolower($b['name']))
      return 1;
   elseif (strtolower($a['name']) == strtolower($b['name']))
      return 0;
   else
      return -1;
}

// 2014-03-26 sorting of modules Bug #517
function arraySortByDescription($a, $b) {
   if (strtolower($a['description']) > strtolower($b['description']))
      return 1;
   elseif (strtolower($a['description']) == strtolower($b['description']))
      return 0;
   else
      return -1;
}