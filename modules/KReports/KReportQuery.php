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

global $dictionary;

require_once('modules/KReports/utils.php');
require_once('modules/ACL/ACLController.php');

// class for the query Array if we have multiple query we join
class KReportQueryArray {

   public $thisKReport;
   public $root_module;
   public $union_modules;
   public $listArray;
   public $whereArray;
   public $whereAddtionalFilter;
   public $whereGroupsArray;
   public $groupsByLimit;
   public $additionalGroupBy;
   public $evalSQLFunctions;
   public $whereOverrideArray;
   public $unionListArray;
   public $fieldNameMap;
   // the selct strings
   public $selectString;
   public $countSelectString;
   public $totalSelectString;
   public $summarySelectString;
   public $fromString;
   public $whereString;
   public $groupbyString;
   public $havingString;
   public $orderbyString;
   public $queryContext = array();
   public $addParams;
   public $queryArray;

   public function __construct($rootModule = '', $unionModules = '', $evalSQLFunctions = true, $listFields = array(), $unionListFields = array(), $whereFields = array(), $additonalFilter = '', $whereGroupFields = array(), $additionalGroupBy = array(), $addParams = array()) {
      // set the various Fields
      $this->root_module = $rootModule;
      $this->union_modules = $unionModules;
      $this->listArray = $listFields;
      $this->unionListArray = $unionListFields;
      $this->whereArray = $whereFields;
      $this->whereAddtionalFilter = $additonalFilter;
      $this->whereGroupsArray = $whereGroupFields;
      // $this->groupByLimit = $groupByLimit;
      $this->additionalGroupBy = $additionalGroupBy;
      $this->evalSQLFunctions = $evalSQLFunctions;

      $this->addParams = $addParams;

      // handle the context if the value is set
      if (isset($this->addParams['context']) && $this->addParams['context'] != '') {
         $this->queryContext = $this->context_to_array($this->addParams['context']);
      }

      // handle Where Override
      if (isset($_REQUEST['whereConditions'])) {
         $this->whereOverrideArray = json_decode(html_entity_decode($_REQUEST['whereConditions'], ENT_QUOTES, 'UTF-8'), true);
      }
   }

   /*
    * function to replace all whitspaces in a string and ocnvert to an array with the entries
    */

   function context_to_array($contextString) {
      $inputString = preg_replace('/ /', '', $contextString);
      $contextArray = explode(',', $inputString);
      return $contextArray;
   }

   function handle_where_conditions() {

      // 2011-10-17 array for ereferences
      $referencedFields = array();
      $referencingFields = array();

      // handle parent bean assignments and see if we need to handle references
      reset($this->whereArray);
      foreach ($this->whereArray as $originalKey => $originalData) {

         // 2011-10-17 build reference arrays
         if (array_key_exists('reference', $originalData) && $originalData['reference'] != '') {
            $referencedFields[$originalData['reference']] = $originalKey;
         }
         if ($originalData['operator'] == 'reference') {
            $referencingFields[$originalKey] = $originalData['value'];
         }

         if ($originalData['operator'] == 'parent_assign') {
            if (!isset($this->addParams['parentbean']) || (isset($this->addParams['parentbean']) && $originalData['valuekey'] == '')) {
               // if we do not have a parentbean we do not evaluate this condition
               unset($this->whereArray[$originalKey]);
            } else {
               // get the value from the parentbean
               $fieldName = $originalData['valuekey'];
               $thisNewValue = $this->addParams['parentbean']->$fieldName;

               //set the value
               $this->whereArray[$originalKey]['operator'] = 'equals';
               $this->whereArray[$originalKey]['value'] = $thisNewValue;
               $this->whereArray[$originalKey]['valuekey'] = $thisNewValue;
            }
         }
      }

      // handle whereoverride
      if (is_array($this->whereOverrideArray)) {
         foreach ($this->whereOverrideArray as $overrideKey => $overrideData) {
            reset($this->whereArray);
            foreach ($this->whereArray as $originalKey => $originalData) {
               if ($originalData['fieldid'] == $overrideData['fieldid']) {
                  // bug 2011-03-12 move corresponding fields
                  $transferFields = array('operator', 'value', 'valuekey', 'valueto', 'valuetokey', 'usereditable');
                  foreach ($transferFields as $thisFieldName) {
                     $this->whereArray[$originalKey][$thisFieldName] = $overrideData[$thisFieldName];
                  }
                  // need to exit the while loop
               }
            }
         }
      }

      // handle context and other things
      foreach ($this->whereArray as $whereId => $whereArrayEntry) {

         // 2012-09-19 set valuto and valuekey if not set 
         if (!isset($whereArrayEntry['valueto']))
            $this->whereArray[$whereId]['valueto'] = '';
         if (!isset($whereArrayEntry['valuetokey']))
            $this->whereArray[$whereId]['valuetokey'] = '';

         // check context
         if (array_key_exists('context', $whereArrayEntry) && $whereArrayEntry['context'] != '') { // && trim($whereArrayEntry['context']) != $this->queryContext)
            // by default we delte unless we find a matching context
            $keepCondition = false;

            // build an array with entries
            $thisWhereConditionContextArray = $this->context_to_array($whereArrayEntry['context']);

            foreach ($thisWhereConditionContextArray as $thisContextEntry) {
               if (in_array($thisContextEntry, $this->queryContext))
                  $keepCondition = true;
            }

            // if we did not find a match remove the condition
            if (!$keepCondition)
               unset($this->whereArray[$whereId]);
         }

         // see if we need to evaluate based on usereditable setting
         if ($whereArrayEntry['usereditable'] == 'yo2')
            unset($this->whereArray[$whereId]); //['operator'] = 'ignore';
            
// 2011-03-25 added function to be evaluated
         if ($whereArrayEntry['operator'] == 'function') {
            include('modules/KReports/kreportsConfig.php');
            if ($customFunctionInclude != '') {
               include($customFunctionInclude);
               if (function_exists($whereArrayEntry['valuekey'])) {
                  $this->whereArray[$whereId]['operator'] = '';
                  $this->whereArray[$whereId]['value'] = '';
                  $this->whereArray[$whereId]['valuekey'] = '';

                  eval("\$opReturn=" . $whereArrayEntry['valuekey'] . "(\$whereArrayEntry);");
                  if (is_array($opReturn) && count($opReturn) > 0) {
                     foreach ($opReturn as $thisOpField => $thisOpValue)
                        $this->whereArray[$whereId][$thisOpField] = $thisOpValue;
                  }
                  else
                     unset($this->whereArray[$whereId]);
               }
               else {
                  // delete the condition if we do not find the function
                  unset($this->whereArray[$whereId]);
               }
            }
         }
      }

      //2011-10-17 manage references
      foreach ($referencingFields as $originalKey => $referenceValue) {
         if (isset($referencedFields[$referenceValue]) && isset($this->whereArray[$referencedFields[$referenceValue]])) {
            $this->whereArray[$originalKey]['operator'] = $this->whereArray[$referencedFields[$referenceValue]]['operator'];
            $this->whereArray[$originalKey]['value'] = $this->whereArray[$referencedFields[$referenceValue]]['value'];
            $this->whereArray[$originalKey]['valuekey'] = $this->whereArray[$referencedFields[$referenceValue]]['valuekey'];
            $this->whereArray[$originalKey]['valueto'] = $this->whereArray[$referencedFields[$referenceValue]]['valueto'];
            $this->whereArray[$originalKey]['valuetokey'] = $this->whereArray[$referencedFields[$referenceValue]]['valuetokey'];
         }
         // 2012-08-01 use request Parameter
         elseif (isset($_REQUEST[$referenceValue])) {
            $this->whereArray[$originalKey]['operator'] = 'equals';
            $this->whereArray[$originalKey]['value'] = $_REQUEST[$referenceValue];
            $this->whereArray[$originalKey]['valuekey'] = $_REQUEST[$referenceValue];
            $this->whereArray[$originalKey]['valueto'] = '';
            $this->whereArray[$originalKey]['valuetokey'] = '';
         }
         else
            unset($this->whereArray[$originalKey]);
      }

      // bugfix 2012-02-24
      // handle the ignore settings ... messes upo joins
      foreach ($this->whereArray as $whereId => $whereArrayEntry) {
         // bugfix 2012-08-07 .. refertence to wrong array entry
         if ($whereArrayEntry['operator'] == 'ignore') {
            unset($this->whereArray[$whereId]);
         }
      }

      // bugifx 2011-04-01
      // renumber the array so we make sure we start at 0 again
      $this->whereArray = array_values($this->whereArray);
   }

   /*
    * function to return where array with conditions to be printed in PDF
    */

   function get_where_array() {
      $returnArray = array();

      $modStrings = return_module_language('en_us', 'KReports');

      foreach ($this->whereArray as $thisWhereCondition) {
         // 2011-06-05 do not pass over conditions with Operator ignore
         if ($thisWhereCondition['exportpdf'] == 'yes' && $thisWhereCondition['operator'] != 'ignore') {
            //build the string we shoot
            $valueString = '';
            //determine value string
            if ($thisWhereCondition['value'] != '' && $thisWhereCondition['value'] != '---') {
               if ($thisWhereCondition['valueto'] != '' && $thisWhereCondition['valueto'] != '---')
                  $valueString = $thisWhereCondition['value'] . ' - ' . $thisWhereCondition['valueto'];
               else
                  $valueString = $thisWhereCondition['value'];
            }

            // add to the array
            $returnArray[] = array(
                'name' => $thisWhereCondition['name'],
                'operator' => $modStrings['LBL_OP_' . strtoupper($thisWhereCondition['operator'])],
                'value' => $valueString
            );
         }
      }

      return $returnArray;
   }

   function build_query_strings() {
      // manage special handling of where conditions
      $this->handle_where_conditions();

      if ($this->union_modules != '') {
         // handle root module
         // filter the array to only have root
         $i = 0;
         $this->queryArray['root']['whereArray'] = array();
         while ($i < count($this->whereArray)) {
            if ($this->whereArray[$i]['unionid'] == 'root')
               $this->queryArray['root']['whereArray'][] = $this->whereArray[$i];
            $i++;
         }

         $i = 0;
         $this->queryArray['root']['whereGroupsArray'] = array();
         while ($i < count($this->whereGroupsArray)) {
            if ($this->whereGroupsArray[$i]['unionid'] == 'root')
               $this->queryArray['root']['whereGroupsArray'][] = $this->whereGroupsArray[$i];
            $i++;
         }

         $this->queryArray['root']['kQuery'] = new KReportQuery($this->root_module, $this->evalSQLFunctions, $this->listArray, $this->queryArray['root']['whereArray'], $this->whereAddtionalFilter, $this->queryArray['root']['whereGroupsArray'], $this->additionalGroupBy, $this->addParams);

         // set union ID & groupings as well as order clause to be by ID
         $this->queryArray['root']['kQuery']->unionId = 'root';
         $this->queryArray['root']['kQuery']->orderByFieldID = true;
         $this->queryArray['root']['kQuery']->groupByFieldID = true;

         // build the query Strings for the root Query
         $this->queryArray['root']['kQuery']->build_query_strings();
         $this->fieldNameMap = $this->queryArray['root']['kQuery']->fieldNameMap;

         //hanlde union
         $unionArrayNew = json_decode(html_entity_decode($this->union_modules, ENT_QUOTES, 'UTF-8'), true);
         // $unionArray = preg_split('/;/', $this->union_modules);
         foreach ($unionArrayNew as $thisUnionArrayEntry) {

            $thisUnionId = $thisUnionArrayEntry['unionid'];
            $thisUnionModule = $thisUnionArrayEntry['module'];

            //filter where and where groups
            $i = 0;
            $this->queryArray[$thisUnionId]['whereArray'] = array();
            while ($i < count($this->whereArray)) {
               if ($this->whereArray[$i]['unionid'] == $thisUnionId) {
                  $this->queryArray[$thisUnionId]['whereArray'][] = $this->whereArray[$i];
                  // replace the beginning of the string to make it root
                  $this->queryArray[$thisUnionId]['whereArray'][count($this->queryArray[$thisUnionId]['whereArray']) - 1]['path'] = preg_replace('/unionroot::union[A-Za-z0-9]*:/', 'root:', $this->queryArray[$thisUnionId]['whereArray'][count($this->queryArray[$thisUnionId]['whereArray']) - 1]['path']);
               }
               $i++;
            }

            $i = 0;
            $this->queryArray[$thisUnionId]['whereGroupsArray'] = array();
            while ($i < count($this->whereGroupsArray)) {
               if ($this->whereGroupsArray[$i]['unionid'] == $thisUnionId)
                  $this->queryArray[$thisUnionId]['whereGroupsArray'][] = $this->whereGroupsArray[$i];
               $i++;
            }

            //build the list array for this union
            $i = 0;
            while ($i < count($this->listArray)) {
               $this->queryArray[$thisUnionId]['listArray'][$i] = $this->listArray[$i];

               foreach ($this->unionListArray as $thisUnionListEntryId => $thisUnionListEntry) {
                  if ($thisUnionListEntry['joinid'] == $thisUnionId && $thisUnionListEntry['fieldid'] == $this->listArray[$i]['fieldid']) {
                     // we have a match
                     if ($thisUnionListEntry['unionfieldpath'] != '' && (!isset($thisUnionListEntry['fixedvalue']) || $thisUnionListEntry['fixedvalue'] == '')) {
                        // also replace the union id with the new root in the fieldpath ...
                        // the union entry is root in the new subquery
                        $this->queryArray[$thisUnionId]['listArray'][$i]['path'] = preg_replace('/union' . $thisUnionId . '/', 'root', $thisUnionListEntry['unionfieldpath']);
                        // make sure we also replace the fixed value
                        $this->queryArray[$thisUnionId]['listArray'][$i]['fixedvalue'] = '';
                     } else {
                        //reset the path in any case
                        // 2012-09-23 ... not sure why this was commented out ... put it in again to avoid fatal error in join
                        // 2012-10-14 ... required for field type ... esp. currency ... need to fix this otherways
                        $this->queryArray[$thisUnionId]['listArray'][$i]['path'] = '';

                        // set a fixed value to '-' if we do not have a fixed value
                        // TODO: change query logic to adopt to empty field if no path is set and then take the fixed value ''

                        if (isset($thisUnionListEntry['fixedvalue']) && $thisUnionListEntry['fixedvalue'] != '')
                           $this->queryArray[$thisUnionId]['listArray'][$i]['fixedvalue'] = $thisUnionListEntry['fixedvalue'];
                        else
                           $this->queryArray[$thisUnionId]['listArray'][$i]['fixedvalue'] = '-';
                     }
                  }
               }

               $i++;
               // find the entry in the unionlist fields array
            }

            $this->queryArray[$thisUnionId]['kQuery'] = new KReportQuery($thisUnionModule, $this->evalSQLFunctions, $this->queryArray[$thisUnionId]['listArray'], $this->queryArray[$thisUnionId]['whereArray'], $this->whereAddtionalFilter, $this->queryArray[$thisUnionId]['whereGroupsArray'], $this->additionalGroupBy, $this->addParams);

            // 2012-11-04 set the root fields name map 
            // so we know if a field is a cur field
            $this->queryArray[$thisUnionId]['kQuery']->rootfieldNameMap = $this->queryArray['root']['kQuery']->fieldNameMap;

            // set the unionid & grouping as well as order clause
            $this->queryArray[$thisUnionId]['kQuery']->unionId = $thisUnionId;
            $this->queryArray[$thisUnionId]['kQuery']->orderByFieldID = true;
            $this->queryArray[$thisUnionId]['kQuery']->groupByFieldID = true;

            // build the query strings
            $this->queryArray[$thisUnionId]['kQuery']->build_query_strings();
         }

         // enrich the kqueries by all joinsegments and reporcess the select to get all join segments in (nned that for the ids for the various records
         $totalJoinSegments = array();
         foreach ($this->queryArray as $thisUnionId => $thisUnionQuery) {
            foreach ($thisUnionQuery['kQuery']->joinSegments as $thisPath => $thisPathProperties) {
               $totalJoinSegments[$thisPathProperties['alias']] = array('level' => $thisPathProperties['level'], 'path' => $thisPath, 'unionid' => $thisUnionId);
            }
         }

         // revuild all select strings
         // first for root
         $this->queryArray['root']['kQuery']->build_select_string($totalJoinSegments);
         // then for all the joins
         foreach ($unionArrayNew as $thisUnionArrayEntry) {
            $this->queryArray[$thisUnionArrayEntry['unionid']]['kQuery']->build_select_string($totalJoinSegments);
         }

         // build the root string
         $queryString = '';
         foreach ($this->queryArray as $id => $queryArrayData) {
            if ($queryString != '')
               $queryString .= ' UNION ';
            $queryString .= $queryArrayData['kQuery']->selectString . ' ' . $queryArrayData['kQuery']->fromString . ' ' . $queryArrayData['kQuery']->whereString . ' ' . $queryArrayData['kQuery']->groupbyString;
         }

         // specific Union handling
         // $queryString .= ' ' . /*$this->queryArray['root']['kQuery']->groupbyString .*/ ' ' . $this->queryArray['root']['kQuery']->havingString . ' ' . $this->queryArray['root']['kQuery']->orderbyString;
         // changes for MSSQL Support see if we need to limit the query
         // get a sort string by ID
         $this->queryArray['root']['kQuery']->orderByFieldID = true;
         $this->queryArray['root']['kQuery']->build_orderby_string();

         if (isset($this->addParams['start']) && isset($this->addParams['limit'])) {
            // add a count query
            // build a limited query
            switch ($GLOBALS['db']->dbType) {
               case 'mssql':
                  $this->countSelectString = 'SELECT COUNT(sugarRecordId) as totalCount from (' . $queryString . ') as origCountSQL';
                  $limitSelect = preg_replace('/SELECT/', 'SELECT row_number() OVER(' . $this->queryArray['root']['kQuery']->orderbyString . ') AS row_number, ', $this->queryArray['root']['kQuery']->unionSelectString);
                  $queryString = 'SELECT TOP(' . $this->addParams['limit'] . ') * FROM (' . $limitSelect . ' FROM (' . $queryString . ') unionResult ' . $this->queryArray['root']['kQuery']->groupbyString . ') AS topSelect WHERE row_number > ' . $this->addParams['start'];
                  break;
               case 'oci8':
                  $this->countSelectString = 'SELECT COUNT(*) as totalCount from (' . $queryString . ') ';
                  $queryString = "SELECT * FROM (
				SELECT sorted_tmp.*, ROWNUM AS rnum FROM (" . $this->queryArray['root']['kQuery']->unionSelectString . ' FROM (' . $queryString . ') unionResult ' . $this->queryArray['root']['kQuery']->groupbyString . ' ' . $this->queryArray['root']['kQuery']->havingString . ' ' . $this->queryArray['root']['kQuery']->orderbyString . ") sorted_tmp 
				WHERE ROWNUM <= " . ($this->addParams['start'] + $this->addParams['limit'] - 1) . ") 
				WHERE rnum >= " . $this->addParams['start'];
                  // $queryString = 'SELECT * FROM (' . $this->queryArray['root']['kQuery']->unionSelectString . ' FROM (' . $queryString . ') unionResult ' . $this->queryArray['root']['kQuery']->groupbyString . ' ' . $this->queryArray['root']['kQuery']->havingString . ' ' . $this->queryArray['root']['kQuery']->orderbyString . ') WHERE rownum >=  ' . $this->addParams['start'] . ' AND rownum < ' . ($this->addParams['start'] + $this->addParams['limit']);
                  break;
               case 'mysql':
                  $this->countSelectString = 'SELECT COUNT(sugarRecordId) as totalCount from (' . $queryString . ') as origCountSQL';
                  $queryString = $this->queryArray['root']['kQuery']->unionSelectString . ' FROM (' . $queryString . ') unionResult ' . $this->queryArray['root']['kQuery']->groupbyString . ' ' . $this->queryArray['root']['kQuery']->havingString . ' ' . $this->queryArray['root']['kQuery']->orderbyString . ' LIMIT ' . $this->addParams['start'] . ',' . $this->addParams['limit'];
                  break;
            }
         } else {
            switch ($GLOBALS['db']->dbType) {
               case 'oci8':
               case 'mssql':
                  $this->countSelectString = 'SELECT COUNT(*) as totalCount from (' . $queryString . ')';
                  break;
               default:
                  $this->countSelectString = 'SELECT COUNT(sugarRecordId) as totalCount from (' . $queryString . ') as origCountSQL';
            }
            $queryString = $this->queryArray['root']['kQuery']->unionSelectString . ' FROM (' . $queryString . ') unionResult ' . $this->queryArray['root']['kQuery']->groupbyString . ' ' . $this->queryArray['root']['kQuery']->havingString . ' ' . $this->queryArray['root']['kQuery']->orderbyString;
         }
         // build the unions
         // build the total Select Sting if we need to calculate Percentage Values
         $this->buildTotalSelectString($this->queryArray['root']['kQuery']->unionSelectString . ' FROM (' . $queryString . ') unionResult ' . $this->queryArray['root']['kQuery']->groupbyString . ' ' . $this->queryArray['root']['kQuery']->havingString . ' ' . $this->queryArray['root']['kQuery']->orderbyString);

         // return the main query string
         return $queryString;
      } else {
         // handle root module
         // filter the array to only have root
         $i = 0;
         while ($i < count($this->whereArray)) {
            if ($this->whereArray[$i]['unionid'] == 'root')
               $this->queryArray['root']['whereArray'][] = $this->whereArray[$i];
            $i++;
         }

         $i = 0;
         while ($i < count($this->whereGroupsArray)) {
            if ($this->whereGroupsArray[$i]['unionid'] == 'root')
               $this->queryArray['root']['whereGroupsArray'][] = $this->whereGroupsArray[$i];
            $i++;
         }

         $this->queryArray['root']['kQuery'] = new KReportQuery($this->root_module, $this->evalSQLFunctions, $this->listArray, $this->queryArray['root']['whereArray']??'', $this->whereAddtionalFilter, $this->queryArray['root']['whereGroupsArray'], $this->additionalGroupBy, $this->addParams);
         //temp see if this works

         $this->queryArray['root']['kQuery']->build_query_strings();
         $this->fieldNameMap = $this->queryArray['root']['kQuery']->fieldNameMap;

         $this->selectString = $this->queryArray['root']['kQuery']->selectString;
         $this->fromString = $this->queryArray['root']['kQuery']->fromString;
         $this->whereString = $this->queryArray['root']['kQuery']->whereString;
         $this->groupbyString = $this->queryArray['root']['kQuery']->groupbyString;
         $this->havingString = $this->queryArray['root']['kQuery']->havingString;
         $this->orderbyString = $this->queryArray['root']['kQuery']->orderbyString;

         //if($this->queryArray['root']['kQuery']->totalSelectString != '')
         //	$this->totalSelectString = $this->queryArray['root']['kQuery']->totalSelectString . ' ' . $this->fromString . ' ' . $this->whereString;
         // build the total Select Sting if we need to calculate Percentage Values
         $this->buildTotalSelectString($this->selectString . ' ' . $this->fromString . ' ' . $this->whereString . ' ' . $this->groupbyString . ' ' . $this->havingString);

         if ($this->queryArray['root']['kQuery']->countSelectString != '') {
            switch ($GLOBALS['db']->dbType) {
               case 'oci8':
               case 'mssql':
                  $this->countSelectString = 'SELECT COUNT(*) as "totalCount" from (' . $this->queryArray['root']['kQuery']->countSelectString . ' ' . $this->fromString . ' ' . $this->whereString . ' ' . $this->groupbyString . ')';
                  break;
               default:
                  $this->countSelectString = 'SELECT COUNT(sugarRecordId) as totalCount from (' . $this->queryArray['root']['kQuery']->countSelectString . ' ' . $this->fromString . ' ' . $this->whereString . ' ' . $this->groupbyString . ') as origCountSQL';
                  // quickfix Stueber
                  $this->countSelectString = '';
                  break;
            }
         }
         // changes for MSSQL Support see if we need to limit the query
         if (isset($this->addParams['start']) && isset($this->addParams['limit'])) {
            switch ($GLOBALS['db']->dbType) {
               case 'mssql':
                  $limitSelect = preg_replace('/SELECT/', 'SELECT row_number() OVER(' . $this->orderbyString . ') AS row_number, ', $this->selectString);
                  return 'SELECT TOP(' . $this->addParams['limit'] . ') * FROM (' . $limitSelect . ' ' . $this->fromString . ' ' . $this->whereString . ' ' . $this->groupbyString . ') AS topSelect WHERE row_number > ' . $this->addParams['start'];
                  break;
               case 'oci8':
                  return "SELECT * FROM (
				SELECT  sorted_tmp.*, ROWNUM AS rnum FROM (" . $this->selectString . ' ' . $this->fromString . ' ' . $this->whereString . ' ' . $this->groupbyString . ' ' . $this->havingString . ' ' . $this->orderbyString . ") sorted_tmp 
				WHERE ROWNUM <= " . ($this->addParams['start'] + $this->addParams['limit'] - 1) . ") 
				WHERE rnum >= " . $this->addParams['start'];

                  // return 'SELECT * FROM (' . $this->selectString . ' ' . $this->fromString . ' ' . $this->whereString . ' ' . $this->groupbyString . ' ' . $this->havingString . ' ' . $this->orderbyString . ') WHERE rownum >= ' . $this->addParams['start'] . ' AND rownum <' . ($this->addParams['start']  + $this->addParams['limit']);
                  break;
               case 'mysql':
                  return $this->selectString . ' ' . $this->fromString . ' ' . $this->whereString . ' ' . $this->groupbyString . ' ' . $this->havingString . ' ' . $this->orderbyString . ' LIMIT ' . $this->addParams['start'] . ',' . $this->addParams['limit'];
                  break;
            }
         }
         else
            return $this->selectString . ' ' . $this->fromString . ' ' . $this->whereString . ' ' . $this->groupbyString . ' ' . $this->havingString . ' ' . $this->orderbyString;
      }
   }

   function buildTotalSelectString($fullQuery, $valueBase = 'valuetype') {
      
      //2014-06-27 support for Oracle
      $fromArray = array(); $toArray = array(); 
      
      $this->totalSelectString = '';
      $this->summarySelectString = '';
      foreach ($this->listArray as $thisListEntry) {
         if ($valueBase == 'valuetype') {
            if (isset($thisListEntry['valuetype']) && $thisListEntry['valuetype'] != '' && $thisListEntry['valuetype'] != '-') {
               $funcArray = explode('OF', $thisListEntry['valuetype']);
               if ($this->totalSelectString == '')
                  $this->totalSelectString = 'SELECT '; else
                  $this->totalSelectString .= ', ';
                  
                  //2014-06-27 support for Oracle with " instead of '
               $this->totalSelectString .= ' ' . ($funcArray[1]??'') . '(' . $thisListEntry['fieldid'] . ")  as \"" . $thisListEntry['fieldid'] . "_total\"";
               
               //2014-06-27 support for Oracle
               $fromArray[] = $thisListEntry['fieldid']; $toArray[] = strtoupper($thisListEntry['fieldid']);
               
            }

            if (isset($thisListEntry['summaryfunction']) && $thisListEntry['summaryfunction'] != '' && $thisListEntry['summaryfunction'] != '-') {

               if ($this->summarySelectString == '')
                  $this->summarySelectString = 'SELECT '; else
                  $this->summarySelectString .= ', ';
                   //2014-06-27 support for Oracle with " instead of '
               $this->summarySelectString .= ' ' . $thisListEntry['summaryfunction'] . '(' . $thisListEntry['fieldid'] . ")  as \"" . $thisListEntry['fieldid'] . "\"";
               
               //2014-06-27 support for Oracle
               $fromArray[] = $thisListEntry['fieldid']; $toArray[] = strtoupper($thisListEntry['fieldid']);
            }
         }
      }

      // check if we have any fgield we found
      if ($this->totalSelectString != '') {
         $this->totalSelectString .= ' FROM (' . $fullQuery . ') fullSelect';
      }

      if ($this->summarySelectString != '') {
         $this->summarySelectString .= ' FROM (' . $fullQuery . ') fullSelect';
      }
      
      //2014-06-27 support for Oracle
      if($GLOBALS['db']->dbType == 'oci8'){
          $this->summarySelectString = str_replace($fromArray, $toArray, $this->summarySelectString);
      }
   }

}

// basic class for the query itself
class KReportQuery {
   /*
    * Min things to know
    * first initialize the class
    * call build_ath to explode the various fields we might look at and build the path
    * call build_from_string to build all the join_segments and build the from string
    * after tha you can call the other functions
    */

   public $root_module;
   public $unionId = '';
   public $whereArray;
   public $whereAddtionalFilter = '';
   public $whereOverrideArray;
   public $listArray;
   public $whereGroupsArray;
   public $fieldNameMap;
   // 2012-11-04 a var for the root field nam map Array
   // will feed that sop we now in union what types the root field is ... to react properly on that 
   // especially reqwuired for the currency handling since we need to ensure we have the same number of fields
   public $rootfieldNameMap;
   public $tablePath;
   public $rootGuid;
   public $joinSegments;
   public $unionJoinSegments;
   public $maxDepth;
   public $queryID = '';
   public $orderByFieldID = false;
   public $groupByFieldID = false;
   // parts of the SQL Query
   public $selectString;
   public $countSelectString;
   public $totalSelectString;
   //2010-03-28 add a select for the union with cosndieration of functions
   public $unionSelectString;
   public $fromString;
   public $whereString;
   public $havingString;
   public $groupbyString;
   public $additionalGroupBy;
   public $orderbyString;
   // Parameters
   public $evalSQLFunctions = true;
   // auth Check level (full, top, none)
   public $authChecklevel = 'full';
   public $showDeleted = false;
   //for a custom sort
   public $sortOverride = [];
   // for the exclusive Groping required for the tree if add grouping params are sent in
   public $exclusiveGroupinbgByAddParams = false;
   // array for all fields we are selcting from the database
   public $fieldArray = [];
   // for MSSQL 
   public $isGrouped = false;

   // constructor
   /*
    *  Additonal Filter = array with Fieldid and value .. wich is then applied to the where clause
    *  $addParams - AuthCheckLevel = full, top none
    *  			 showDeleted = true, false
    */
   public function __construct($rootModule = '', $evalSQLFunctions = true, $listFields = array(), $whereFields = array(), $additonalFilter = '', $whereGroupFields = array(), $additionalGroupBy = array(), $addParams = array()) {
      // set the various Fields
      $this->root_module = $rootModule;
      $this->listArray = $listFields;
      $this->whereArray = $whereFields;
      $this->whereAddtionalFilter = $additonalFilter;
      $this->whereGroupsArray = $whereGroupFields;
      $this->additionalGroupBy = $additionalGroupBy;
      $this->evalSQLFunctions = $evalSQLFunctions;


      // handle additional parameters
      if (isset($addParams['authChecklevel']))
         $this->authChecklevel = $addParams['authChecklevel'];

      if (isset($addParams['showDeleted']))
         $this->showDeleted = $addParams['showDeleted'];

      //2011-03-21 sort override params
      if (isset($addParams['sortid']) && isset($addParams['sortseq'])) {
         $this->sortOverride = array(
             'sortid' => $addParams['sortid'],
             'sortseq' => $addParams['sortseq']
         );
      }

      if (isset($addParams['exclusiveGrouping']))
         $this->exclusiveGroupinbgByAddParams = $addParams['exclusiveGrouping'];

      // handle Where Override
      // need to think about moving this
      /*
        if(isset($_REQUEST['whereConditions']))
        {
        $this->whereOverrideArray = json_decode_kinamu( html_entity_decode_utf8($_REQUEST['whereConditions']));
        }
       */
   }

   function build_query_strings() {
      if ($GLOBALS['db']->dbType == 'mssql' || $GLOBALS['db']->dbType == 'oci8')
         $this->check_groupby($this->additionalGroupBy);

      $this->build_path();
      $this->build_from_string();
      $this->build_select_string();
      $this->build_where_string();
      $this->build_orderby_string();
      $this->build_groupby_string($this->additionalGroupBy);
   }

   /*
    * Function to build the JOin Type form th type in the Report
    */

   function switchJoinType($jointype) {
      // TODO handle not existing join type
      switch ($jointype) {
         case "optional":
            return ' LEFT JOIN ';
            break;
         case "required":
            return ' INNER JOIN ';
            break;
         case "notexisting":
            // 2011-12-29 retun no jointype 
            //return ' LEFT JOIN ';
            return '';
            break;
      }
   }

   /*
    * Build Path:
    * function to extract all the path informatios out of the JSON Array we store
    */

   function build_path() {

      if ((is_array($this->whereArray) && count($this->whereArray) > 0) || (is_array($this->listArray) && count($this->listArray) > 0)) {
         /*
          * Build the path array with all valid comkbinations (basically joins we can meet
          */
         // collect Path entries for the Where Clauses
         if (!is_array($this->whereArray))
            $this->whereArray = array();
         foreach ($this->whereArray as $thisWhere) {
            // $this->addPath($thisWhere['path'], $this->switchJoinType($thisWhere['jointype']));
            // check if the group this belongs to is a notexits group
            $flagNotExists = false;
            if (is_array($this->whereGroupsArray)) {
               reset($this->whereGroupsArray);
               foreach ($this->whereGroupsArray as $thisWhereGroupEntry) {
                  if ($thisWhereGroupEntry['id'] == $thisWhere['groupid'] && array_key_exists('notexists', $thisWhereGroupEntry) && $thisWhereGroupEntry['notexists'] == '1')
                     $flagNotExists = false;
               }
            }
            // if the flag is set -> LEFT JOIN ...
            //$this->addPath($thisWhere['path'], ($flagNotExists) ? 'LEFT JOIN' : 'INNER JOIN');
            // revert ..
            $this->addPath($thisWhere['path'], $this->switchJoinType($thisWhere['jointype']));
         }

         // same for the List Clauses
         foreach ($this->listArray as $thisListEntry) {
            $this->addPath($thisListEntry['path'], $this->switchJoinType($thisListEntry['jointype']));
         }
      }
   }

   /*
    * Helper function to add the path we found
    */

   function addPath($path, $jointype) {
      if ($path != '') {
         // require_once('include/utils.php');
         $fieldPos = strpos($path, "::field");
         $path = substr($path, 0, $fieldPos);

         if (!isset($this->tablePath[$path])) {
            $this->tablePath[$path] = $jointype;
         } else {
            // if we have an inner join now ... upgrade ..
            // 2011-12-29 proper seeuence and upgrading ... added empty join
            if ($this->tablePath[$path] == '' || ($this->tablePath[$path] == 'LEFT JOIN' && $jointype == 'INNER JOIN'))
               $this->tablePath[$path] = $jointype;
         }

         // check if we have more to add
         // required if we have roiutes where there is no field used in between
         // search for a separator from the end and add the path if we do not yet know it
         // the join build will pick this up in the next step
         while ($sepPos = strrpos($path, "::")) {
            // cut down the path
            $path = substr($path, 0, $sepPos);

            // see if we have to add the path
            if (!isset($this->tablePath[$path])) {
               $this->tablePath[$path] = $jointype;
            } else {
               // if we have an inner join now ... upgrade ..
               // 2011-12-29 proper seeuence and upgrading ... added empty join
               if ($this->tablePath[$path] == '' || ($this->tablePath[$path] == 'LEFT JOIN' && $jointype == 'INNER JOIN'))
                  $this->tablePath[$path] = $jointype;
            }
         }
      }
   }

   /*
    * Function that evaluates the path and then build the join segments
    * need that later on to identify the segmets of the select statement
    */

   function build_from_string() {
      global $db, $app_list_strings, $beanList, $beanFiles, $current_user, $sugar_config;

		//NS-Team
		$access_check = 'list';
		if (isset($_SESSION['KReports']['export']) && $_SESSION['KReports']['export']) $access_check = 'export';

      // Create a root GUID
      $this->rootGuid = randomstring();

      $this->joinSegments = array();
      $this->maxDepth = 0;

      $kOrgUnits = false;

      //check if we do the Org Check

      if (file_exists('modules/KOrgObjects/KOrgObject.php') && $GLOBALS['sugarconfig']['orgmanaged']) {
         require_once('modules/KOrgObjects/KOrgObject.php');
         $thisKOrgObject = new KOrgObject();
         $kOrgUnits = true;
      }

      /*
       * Build the array for the joins based on the various Path we have
       */
      foreach (($this->tablePath ?? []) as $thisPath => $thisPathJoinType) {
         // Process backcutting until we have found the node going upwards
         // in the segments array or we are on the root segment
         // (when no '::' can be found)
         if (substr_count($thisPath, '::') > $this->maxDepth)
            $this->maxDepth = substr_count($thisPath, '::');

         while (strpos($thisPath, '::') && !isset($this->joinSegments[$thisPath])) {
            // add the segment to the segments table
            $this->joinSegments[$thisPath] = array('alias' => randomstring(), 'linkalias' => randomstring(), 'level' => substr_count($thisPath, '::'), 'jointype' => $thisPathJoinType);

            // find last occurence of '::' in the string and cut off there
            $thisPath = substr($thisPath, strrpos($thisPath, "::"));
         }
      }

      // Get the main Table we select from
      $this->fromString = 'FROM ' . $this->get_table_for_module($this->root_module) . ' ' . $this->rootGuid;
      // check if this is an array so we need to add joins ...
      // add an entry for the root Object ...
      // needed as reference for the GUID
      $this->joinSegments['root:' . $this->root_module] = array('alias' => $this->rootGuid, 'level' => 0);

      // get ther root Object
      require_once($beanFiles[$beanList[$this->root_module]]);
      $this->joinSegments['root:' . $this->root_module]['object'] = new $beanList[$this->root_module]();
      $root_bean = $this->joinSegments['root:' . $this->root_module]['object'];
      
      // check for Custom Fields
      if ($root_bean->hasCustomFields()) {
         $this->joinSegments['root:' . $this->root_module]['customjoin'] = randomstring();
         $this->fromString .= ' LEFT JOIN ' . $this->get_table_for_module($this->root_module) . '_cstm as ' . $this->joinSegments['root:' . $this->root_module]['customjoin'] . '  ON ' . $this->rootGuid . '.id = ' . $this->joinSegments['root:' . $this->root_module]['customjoin'] . '.id_c';
      }


      // changed so we spport teams in Pro
      if ($this->authChecklevel != 'none' && isset($sugar_config['KReports']) && isset($sugar_config['KReports']['authCheck'])) {

         switch ($sugar_config['KReports']['authCheck']) {
            case 'KOrgObjects':
               $this->fromString .= $thisKOrgObject->getOrgunitJoin($this->joinSegments['root:' . $this->root_module]['object']->table_name, $this->joinSegments['root:' . $this->root_module]['object']->object_name, $this->rootGuid, '1');
               break;
            case 'KAuthObjects':
               $selectArray = array('where' => '', 'from' => '', 'select' => '');
               $GLOBALS['KAuthAccessController']->addAuthAccessToListArray($selectArray, $this->joinSegments['root:' . $this->root_module]['object'], $this->joinSegments['root:' . $this->root_module]['alias'], true);
               if (!empty($selectArray['where'])) {
                  if (empty($this->whereString)) {
                     $this->whereString = " " . $selectArray['where'] . " ";
                  } else {
                     $this->whereString .= " AND " . $selectArray['where'] . " ";
                  }
               }
               if (!empty($selectArray['join'])) {
                  $this->fromString .= ' ' . $selectArray['join'] . ' ';
               }
               break;
            case 'PRO':
               $this->fromString .= ' ';
               $this->joinSegments['root:' . $this->root_module]['object']->add_team_security_where_clause($this->fromString, $this->rootGuid);
               break;
            //2013-03-26 Bug#460 Typo changed
            case 'SecurityGroups':
               // NS-Team if ($this->joinSegments['root:' . $this->root_module]['object']->bean_implements('ACL') && ACLController::requireSecurityGroup($this->joinSegments['root:' . $this->root_module]['object']->module_dir, 'list')) {
               if ($root_bean->bean_implements('ACL') && ACLController::requireSecurityGroup($root_bean->module_dir, $access_check)) {
                  require_once('modules/SecurityGroups/SecurityGroup.php');
                  global $current_user;
                  $owner_where = str_replace($root_bean->table_name, $this->rootGuid, $root_bean->getOwnerWhere($current_user->id));
                  $group_where = SecurityGroup::getGroupWhere($this->rootGuid, $root_bean->module_dir, $current_user->id);
                  if (!empty($owner_where)) {
                     if (empty($this->whereString)) {
                        $this->whereString = " (" . $owner_where . " or " . $group_where . ") ";
                     } else {
                        $this->whereString .= " AND (" . $owner_where . " or " . $group_where . ") ";
                     }
                  } else {
                     $this->whereString .= ' AND ' . $group_where;
                  }
               }
               break;
         }
      }


      // Index to iterate through the join table building the joins
      // from the root object outward going
      $levelCounter = 1;

      if (is_array($this->joinSegments)) {

         while ($levelCounter <= $this->maxDepth) {
            // set the array back to the first element in the array
            reset($this->joinSegments);

            foreach ($this->joinSegments as $thisPath => $thisPathDetails) {
               // process only entries for the respective levels
               if ($thisPathDetails['level'] == $levelCounter) {
                  // get the last enrty and the one before and the relevant arrays
                  $rightPath = substr($thisPath, strrpos($thisPath, "::") + 2, strlen($thisPath));
                  $leftPath = substr($thisPath, 0, strrpos($thisPath, "::"));

                  // explode into the relevant arrays
                  $rightArray = explode(':', $rightPath);
                  $leftArray = explode(':', $leftPath);

                  // 2011-07-21 add check for audit records 
                  if ($rightArray[2] == 'audit') {
                     //handle audit link
                     $this->fromString .= $thisPathJoinType . $this->joinSegments[$leftPath]['object']->table_name . '_audit ' . $this->joinSegments[$thisPath]['alias'] . ' ON ' . $this->joinSegments[$thisPath]['alias'] . '.parent_id = ' . $this->joinSegments[$leftPath]['alias'] . '.id';
                  }
                  //2011-08-17 reacht to a relationship record and replace the alias in the path
                  elseif ($rightArray[0] == 'relationship') {
                     // set alias for the path to the linkalias of the connected bean
                     $this->joinSegments[$thisPath]['alias'] = $this->joinSegments[$leftPath]['linkalias'];
                  }
                  //2013-01-09 add support for Studio Relate Fields
                  elseif ($rightArray[0] == 'relate') {
                     //left Path Object must be set since we process from the top
                     if (!($this->joinSegments[$leftPath]['object'] instanceof $beanList[$rightArray[1]])) {
                        die('fatal Error in Join');
                     }
                     // load the module on the right hand side
                     require_once($beanFiles[$beanList[$this->joinSegments[$leftPath]['object']->field_defs[$rightArray[2]]['module']]]);
                     $this->joinSegments[$thisPath]['object'] = new $beanList[$this->joinSegments[$leftPath]['object']->field_defs[$rightArray[2]]['module']]();

                     // join on the id = relate id .. on _cstm if custom field .. on main if regular
                     $this->fromString .= ' ' . $thisPathDetails['jointype'] . ' ' . $this->joinSegments[$thisPath]['object']->table_name . ' AS ' . $this->joinSegments[$thisPath]['alias'] . ' ON ' . $this->joinSegments[$thisPath]['alias'] . '.id=' . ( ($this->joinSegments[$leftPath]['object']->field_defs[$this->joinSegments[$leftPath]['object']->field_defs[$rightArray[2]]['id_name']]['source']??'') == 'custom_fields' ? $this->joinSegments[$leftPath]['customjoin'] : $this->joinSegments[$leftPath]['alias']) . '.' . $this->joinSegments[$leftPath]['object']->field_defs[$rightArray[2]]['id_name'] . ' ';

                     // check for Custom Fields
                     if ($this->joinSegments[$thisPath]['object']->hasCustomFields()) {
                        $this->joinSegments[$thisPath]['customjoin'] = randomstring();
                        $this->fromString .= ' LEFT JOIN ' . $this->joinSegments[$thisPath]['object']->table_name . '_cstm as ' . $this->joinSegments[$thisPath]['customjoin'] . ' ON ' . $this->joinSegments[$thisPath]['alias'] . '.id = ' . $this->joinSegments[$thisPath]['customjoin'] . '.id_c';
                     }
                  } else {
                     //left Path Object must be set since we process from the top
                     if (!($this->joinSegments[$leftPath]['object'] instanceof $beanList[$rightArray[1]])) {
                        $GLOBALS['log']->error('KReporter: fatal error in join with left path ' . $thisPath);
                        die('fatal Error in Join ' . $thisPath);
                     }

                     // load the relationship .. resp link
                     $this->joinSegments[$leftPath]['object']->load_relationship($rightArray[2]);
                     // set aliases for left and right .. will be processed properly anyway in the build of the link
                     // ... funny enough so
                     //2011-12-29 check if we have a jointpye
                    $obj = $this->joinSegments[$leftPath]['object'];
                    $right=$rightArray[2];
                     if ($thisPathDetails['jointype'] != '') {
                        //2011-12-29 see if the relationship vuilds on a custom field
                        if (isset($obj->field_name_map[$obj->$right->_relationship->rhs_key]['source']) && ($obj->field_name_map[$obj->$right->_relationship->rhs_key]['source'] == 'custom_fields' || $obj->field_name_map[$obj->$right->_relationship->lhs_key]['source'] == 'custom_fields')) {
                           $join_params = array(
                               'join_type' => $thisPathDetails['jointype'],
                               'right_join_table_alias' => $this->joinSegments[$leftPath]['customjoin'],
                               'left_join_table_alias' => $this->joinSegments[$leftPath]['customjoin'],
                               'join_table_link_alias' => $this->joinSegments[$thisPath]['linkalias'], // randomstring() ,
                               'join_table_alias' => $this->joinSegments[$thisPath]['alias']
                           );
                        } else {
                           $join_params = array(
                               'join_type' => $thisPathDetails['jointype'],
                               'right_join_table_alias' => $this->joinSegments[$leftPath]['alias'],
                               'left_join_table_alias' => $this->joinSegments[$leftPath]['alias'],
                               'join_table_link_alias' => $this->joinSegments[$thisPath]['linkalias'], // randomstring() ,
                               'join_table_alias' => $this->joinSegments[$thisPath]['alias']
                           );
                        }

                        //2010-09-09 Bug to handle left side join relationship
                        if (isset($obj->field_defs[$right]['side']) && $obj->field_defs[$right]['side'] == 'left' && !$obj->$right->_swap_sides)
                           $obj->$right->_swap_sides = true;

                        $linkJoin = $obj->$right->getJoin($join_params);

                        $this->fromString .= ' ' . $linkJoin;
                     }
                     // load the module on the right hand side
                     require_once($beanFiles[$beanList[$obj->$right->getRelatedModuleName()]]);
                     $this->joinSegments[$thisPath]['object'] = new $beanList[$obj->$right->getRelatedModuleName()]();

                     //bugfix 2010-08-19, respect ACL role access for owner reuqired in select
                     // NS-Team if ($this->joinSegments[$leftPath]['object']->bean_implements('ACL') && ACLController::requireOwner($this->joinSegments[$leftPath]['object']->module_dir, 'list')) {
                     if ($this->joinSegments[$leftPath]['object']->bean_implements('ACL') && ACLController::requireOwner($this->joinSegments[$leftPath]['object']->module_dir, $access_check)) {
                        //2013-02-22 missing check if we have a wherestring at all 
                        if ($this->whereString != '')
                           $this->whereString .= ' AND ';
                        $this->whereString .= $this->joinSegments[$leftPath]['alias'] . '.assigned_user_id=\'' . $current_user->id . '\'';
                     }

                     // check for Custom Fields
                     if ($this->joinSegments[$thisPath]['object']->hasCustomFields()) {
                        $this->joinSegments[$thisPath]['customjoin'] = randomstring();
                        $this->fromString .= ' LEFT JOIN ' . $this->joinSegments[$thisPath]['object']->table_name . '_cstm as ' . $this->joinSegments[$thisPath]['customjoin'] . ' ON ' . $this->joinSegments[$thisPath]['alias'] . '.id = ' . $this->joinSegments[$thisPath]['customjoin'] . '.id_c';
                     }

                     // append join for Orgobjects if Object is OrgManaged
                     if ($this->authChecklevel != 'none' && $this->authChecklevel != 'top' && isset($GLOBALS['sugar_config']['KReports']) && isset($GLOBALS['sugar_config']['KReports']['authCheck'])) {
                        switch ($GLOBALS['sugar_config']['KReports']['authCheck']) {
                           case 'KOrgObjects':
                              $this->fromString .= $thisKOrgObject->getOrgunitJoin($this->joinSegments[$thisPath]['object']->table_name, $this->joinSegments[$thisPath]['object']->object_name, $this->joinSegments[$thisPath]['alias'], '1');
                              break;
                           case 'KAuthObjects':
                              $selectArray = array('where' => '', 'from' => '', 'select' => '');
                              $GLOBALS['KAuthAccessController']->addAuthAccessToListArray($selectArray, $this->joinSegments[$thisPath]['object'], $this->joinSegments[$thisPath]['alias'], true);
                              if (!empty($selectArray['where'])) {
                                 if (empty($this->whereString)) {
                                    $this->whereString = " " . $selectArray['where'] . " ";
                                 } else {
                                    $this->whereString .= " AND " . $selectArray['where'] . " ";
                                 }
                              }
                              if (!empty($selectArray['join'])) {
                                 $this->fromString .= ' ' . $selectArray['join'] . ' ';
                              }
                              break;
                           case 'PRO':
                              $this->fromString .= ' ';
                              $this->joinSegments[$thisPath]['object']->add_team_security_where_clause($this->fromString, $this->joinSegments[$thisPath]['alias']);
                              break;
                           //2013-03-26 Bug#460 Typo changed
                           case 'SecurityGroups':
                              // NS-Team if ($this->joinSegments[$thisPath]['object']->bean_implements('ACL') && ACLController::requireSecurityGroup($this->joinSegments[$thisPath]['object']->module_dir, 'list')) {
                              if ($this->joinSegments[$thisPath]['object']->bean_implements('ACL') && ACLController::requireSecurityGroup($this->joinSegments[$thisPath]['object']->module_dir, $access_check)) {
                                 require_once('modules/SecurityGroups/SecurityGroup.php');
                                 global $current_user;
                                 $owner_where = str_replace($this->joinSegments[$thisPath]['object']->table_name, $this->joinSegments[$thisPath]['alias'], $this->joinSegments[$thisPath]['object']->getOwnerWhere($current_user->id));
                                 $group_where = SecurityGroup::getGroupWhere($this->joinSegments[$thisPath]['alias'], $this->joinSegments[$thisPath]['object']->module_dir, $current_user->id);
                                 if (!empty($owner_where)) {
                                    if (empty($this->whereString)) {
                                       $this->whereString = " (" . $owner_where . " or " . $group_where . ") ";
                                    } else {
                                       $this->whereString .= " AND (" . $owner_where . " or " . $group_where . ") ";
                                    }
                                 } else {
                                    $this->whereString .= ' AND ' . $group_where;
                                 }
                              }
                              break;
                        }
                     }
                  }
               }
            }

            // increase Counter to tackle next level
            $levelCounter++;
         }
      }
   }

   /*
    * function that build the selct string
    * parameter unionJoinSegments to hand in more join segments to include
    * in select stamenet when we are in a union join mode - then this function gets
    * processed twice
    */

   function build_select_string($unionJoinSegments = '') {
      // require_once('include/utils.php');
      global $db, $app_list_strings, $beanList, $beanFiles;
      /*
       * Block to build the selct clause with all fields selected
       */
      // reset the Fiels Array
      $this->fieldArray = array();


      // build select
      if ($this->isGrouped && ($GLOBALS['db']->dbType == 'mssql' || $GLOBALS['db']->dbType == 'oci8'))
         $this->selectString = 'SELECT MIN(' . $this->rootGuid . '.id) as sugarRecordId';
      else
         $this->selectString = 'SELECT ' . $this->rootGuid . '.id as "sugarRecordId"';

      $this->unionSelectString = 'SELECT sugarRecordId';

      $this->fieldArray['sugarRecordId'] = 'sugarRecordId';

      // add rootmodule for this record
      $this->selectString .= ", '" . $this->root_module . "' as \"sugarRecordModule\" ";
      $this->unionSelectString .= ', sugarRecordModule';

      $this->fieldArray['sugarRecordModule'] = 'sugarRecordModule';

      // 2011-02-03 for Performance add a count Query
      // just for the count
      $this->countSelectString = 'SELECT ' . $this->rootGuid . '.id as "sugarRecordId"';

      // see if we are in a union statement then we select the unionid as well
      if ($this->unionId != '') {
         $this->selectString .= ', \'' . $this->unionId . '\' as "unionid"';
         $this->unionSelectString .= ', unionid';

         $this->fieldArray['unionid'] = 'unionid';
      }

      // select the ids for the various linked tables
      // check if we have joins for a union passed in ...
      if ($unionJoinSegments != '' && is_array($unionJoinSegments)) {
         $this->unionJoinSegments = $unionJoinSegments;

         foreach ($unionJoinSegments as $thisAlias => $thisJoinIdData) {
            if ($thisJoinIdData['unionid'] == $this->unionId) {
               // this is for this join ... so we select the id
               if ($this->isGrouped && ($GLOBALS['db']->dbType == 'mssql' || $GLOBALS['db']->dbType == 'oci8'))
                  $this->selectString .= ', MIN(' . $thisAlias . '.id) as ' . $thisAlias . 'id';
               else
                  $this->selectString .= ', ' . $thisAlias . '.id as "' . $thisAlias . 'id"';

               $this->selectString .= ', \'' . $thisJoinIdData['path'] . '\' as "' . $thisAlias . 'path"';
            } else {
               // this is for another join ... so we select an empty field
               $this->selectString .= ', \'\' as "' . $thisAlias . 'id"';
               $this->selectString .= ', \'\'  as "' . $thisAlias . 'path"';
            }

            $this->unionSelectString .= ', ' . $thisAlias . 'id';
            $this->unionSelectString .= ', ' . $thisAlias . 'path';

            $this->fieldArray[$thisAlias . 'id'] = $thisAlias . 'id';
            $this->fieldArray[$thisAlias . 'path'] = $thisAlias . 'path';
         }
      } else {
         // standard processing ... we simply loop throgh the joinsegments
         foreach ($this->joinSegments as $joinpath => $joinsegment) {
            // 2012-02-3 cant take this out sinceit breaks the links!!!!
            // 2011-12-29 check if Jointype is set
            //if( $joinsegment['jointype'] != '')
            //{
            if ($this->isGrouped && ($GLOBALS['db']->dbType == 'mssql' || $GLOBALS['db']->dbType == 'oci8'))
               $this->selectString .= ', MIN(' . $joinsegment['alias'] . '.id) as ' . $joinsegment['alias'] . 'id';
            else
               $this->selectString .= ', ' . $joinsegment['alias'] . '.id as "' . $joinsegment['alias'] . 'id"';
            $this->selectString .= ', \'' . $joinpath . '\' as "' . $joinsegment['alias'] . 'path"';
            $this->unionSelectString .= ', ' . $joinsegment['alias'] . 'id';
            $this->unionSelectString .= ', ' . $joinsegment['alias'] . 'path';
            //}

            $this->fieldArray[$joinsegment['alias'] . 'id'] = $joinsegment['alias'] . 'id';
            $this->fieldArray[$joinsegment['alias'] . 'path'] = $joinsegment['alias'] . 'path';
         }
      }

      if (is_array($this->listArray)) {
         foreach ($this->listArray as $thisListEntry) {
            // $this->addPath($thisList['path'], $this->switchJoinType($thisList['jointype']));
            $fieldName = substr($thisListEntry['path'], strrpos($thisListEntry['path'], "::") + 2, strlen($thisListEntry['path']));
            $pathName = substr($thisListEntry['path'], 0, strrpos($thisListEntry['path'], "::"));

            $fieldArray = explode(':', $fieldName);


            // process an SQL Function if one is set and the eval trigger is set to true
            // if we have a fixed value select that value
            if ($thisListEntry['path'] == '' || (isset($thisListEntry['fixedvalue']) && $thisListEntry['fixedvalue'] != '')) {
               //if($thisListEntry['sqlfunction'] != '-' && $this->evalSQLFunctions )
               //	$this->selectString .= ', ' . $thisListEntry['sqlfunction'] . '(' . $thisListEntry['fixedvalue'] . ') as ' . $thisListEntry['fieldid'];
               // else
               $this->selectString .= ", '" . $thisListEntry['fixedvalue'] . "' as \"" . $thisListEntry['fieldid'] . "\"";


               // required handling foir sql function also needed for
               if ($thisListEntry['sqlfunction'] != '-' && $this->evalSQLFunctions && 
               ( $pathName == '' ||
                  (($this->joinSegments[$pathName]['object']->field_name_map[$fieldArray[1]]['type'] ?? '') != 'kreporter' 
                  || (($this->joinSegments[$pathName]['object']->field_name_map[$fieldArray[1]]['type'] ?? '') == 'kreporter' 
                  && ($this->joinSegments[$pathName]['object']->field_name_map[$fieldArray[1]]['evalSQLFunction'] ?? '') == 'X')))
                  ) {
                  if ($thisListEntry['sqlfunction'] == 'GROUP_CONCAT') {
                     $this->unionSelectString .= ', ' . $thisListEntry['sqlfunction'] . '(DISTINCT ' . $thisListEntry['fieldid'] . ' SEPARATOR \', \')';
                  }
                  //2013-03-01 Sort function for Group Concat
                  elseif ($thisListEntry['sqlfunction'] == 'GROUP_CONASC') {
                     $this->unionSelectString .= ', GROUP_CONCAT(DISTINCT ' . $thisListEntry['fieldid'] . ' ORDER BY ' . $thisListEntry['fieldid'] . ' ASC SEPARATOR \', \')';
                  } elseif ($thisListEntry['sqlfunction'] == 'GROUP_CONDSC') {
                     $this->unionSelectString .= ', GROUP_CONCAT(DISTINCT ' . $thisListEntry['fieldid'] . ' ORDER BY ' . $thisListEntry['fieldid'] . ' DESC SEPARATOR \', \')';
                  }
                  // 2012-10-11 add count distinct
                  //2013-04-22 also for Count Distinct ... Bug #469
                  //elseif ($thisListEntry['sqlfunction'] == 'COUNT_DISTINCT') {
                  //    $this->unionSelectString .= ', COUNT(DISTINCT ' . $thisListEntry['fieldid'] . ')';
                  // } 
                  else {
                     //2011-05-31 if function is count - sum in union
                     //2013-04-22 also for Count Distinct ... Bug #469
                     $this->unionSelectString .= ', ' . ($thisListEntry['sqlfunction'] == 'COUNT' || $thisListEntry['sqlfunction'] == 'COUNT_DISTINCT' ? 'SUM' : $thisListEntry['sqlfunction']) . '(' . $thisListEntry['fieldid'] . ')';
                  }
                  $this->unionSelectString .= ' as "' . $thisListEntry['fieldid'] . '"';
               }
               else
                  $this->unionSelectString .= ', ' . $thisListEntry['fieldid'];

               // add this to the fieldName Map in case we link a fixed
               $this->fieldNameMap[$thisListEntry['fieldid']] = array(
                   'fieldname' => '',
                   'path' => '',
                   'islink' => ($thisListEntry['link'] == 'yes') ? true : false,
                   'sqlFunction' => '',
                   'tablealias' => $this->rootGuid,
                   'fields_name_map_entry' => '',
                   'type' => /* 'fixedvalue' */ (isset($this->joinSegments[$pathName]) ? (($this->joinSegments[$pathName]['object']->field_name_map[$fieldArray[1]]['type'] == 'kreporter') ? $this->joinSegments[$pathName]['object']->field_name_map[$fieldArray[1]]['kreporttype'] : $this->joinSegments[$pathName]['object']->field_name_map[$fieldArray[1]]['type'])  : 'fixedvalue'),
                   'module' => $this->root_module,
                   'fields_name_map_entry' => (isset($this->joinSegments[$pathName]) ? $this->joinSegments[$pathName]['object']->field_name_map[$fieldArray[1]] : array()));
            }
            else {
               if ($thisListEntry['sqlfunction'] != '-' && $this->evalSQLFunctions && (($this->joinSegments[$pathName]['object']->field_name_map[$fieldArray[1]]['type']??'') != 'kreporter' || ($this->joinSegments[$pathName]['object']->field_name_map[$fieldArray[1]]['type'] == 'kreporter' && (array_key_exists('evalSQLFunction', $this->joinSegments[$pathName]['object']->field_name_map[$fieldArray[1]]) && $this->joinSegments[$pathName]['object']->field_name_map[$fieldArray[1]]['evalSQLFunction'] == 'X')))) {
                  if ($thisListEntry['sqlfunction'] == 'GROUP_CONCAT') {
                     $this->selectString .= ', ' . $thisListEntry['sqlfunction'] . '(DISTINCT ' . $this->get_field_name($pathName, $fieldArray[1], $thisListEntry['fieldid'], ($thisListEntry['link'] == 'yes') ? true : false, $thisListEntry['sqlfunction']) . ' SEPARATOR \', \')';
                     $this->unionSelectString .= ', ' . $thisListEntry['sqlfunction'] . '(DISTINCT ' . $thisListEntry['fieldid'] . ' SEPARATOR \', \')';
                  }
                  //2013-03-01 Sort function for Group Concat
                  elseif ($thisListEntry['sqlfunction'] == 'GROUP_CONASC') {
                     $this->selectString .= ', GROUP_CONCAT(DISTINCT ' . $this->get_field_name($pathName, $fieldArray[1], $thisListEntry['fieldid'], ($thisListEntry['link'] == 'yes') ? true : false, $thisListEntry['sqlfunction']) . ' ORDER BY ' . $this->get_field_name($pathName, $fieldArray[1], $thisListEntry['fieldid'], ($thisListEntry['link'] == 'yes') ? true : false, $thisListEntry['sqlfunction']) . ' ASC SEPARATOR \', \')';
                     $this->unionSelectString .= ', GROUP_CONCAT(DISTINCT ' . $thisListEntry['fieldid'] . ' ORDER BY ' . $thisListEntry['fieldid'] . ' ASC SEPARATOR \', \')';
                  } elseif ($thisListEntry['sqlfunction'] == 'GROUP_CONDSC') {
                     $this->selectString .= ', GROUP_CONCAT(DISTINCT ' . $this->get_field_name($pathName, $fieldArray[1], $thisListEntry['fieldid'], ($thisListEntry['link'] == 'yes') ? true : false, $thisListEntry['sqlfunction']) . ' ORDER BY ' . $this->get_field_name($pathName, $fieldArray[1], $thisListEntry['fieldid'], ($thisListEntry['link'] == 'yes') ? true : false, $thisListEntry['sqlfunction']) . ' ASC SEPARATOR \', \')';
                     $this->unionSelectString .= ', GROUP_CONCAT(DISTINCT ' . $thisListEntry['fieldid'] . ' ORDER BY ' . $thisListEntry['fieldid'] . ' DESC SEPARATOR \', \')';
                  }
                  // 2012-10-11 add count distinct
                  elseif ($thisListEntry['sqlfunction'] == 'COUNT_DISTINCT') {
                     $this->selectString .= ', COUNT(DISTINCT ' . $this->get_field_name($pathName, $fieldArray[1], $thisListEntry['fieldid'], ($thisListEntry['link'] == 'yes') ? true : false, $thisListEntry['sqlfunction']) . ')';
                     //2013-04-22 also for Count Distinct ... Bug #469
                     //$this->unionSelectString .= ', COUNT(DISTINCT ' . $thisListEntry['fieldid'] . ')';
                     $this->unionSelectString .= ', SUM(' . $thisListEntry['fieldid'] . ')';
                  } else {
                     $this->selectString .= ', ' . $thisListEntry['sqlfunction'] . '(' . $this->get_field_name($pathName, $fieldArray[1], $thisListEntry['fieldid'], ($thisListEntry['link'] == 'yes') ? true : false, $thisListEntry['sqlfunction']) . ')';
                     // 2011-05-31 if function is count - sum in union
                     $this->unionSelectString .= ', ' . ($thisListEntry['sqlfunction'] == 'COUNT' ? 'SUM' : $thisListEntry['sqlfunction']) . '(' . $thisListEntry['fieldid'] . ')';
                  }
               } else {
                  //if(isset($thisListEntry['customsqlfunction']) && $thisListEntry['customsqlfunction'] != '')
                  //	$this->selectString .= ', ' . str_replace('$', $this->get_field_name($pathName, $fieldArray[1], $thisListEntry['fieldid'], ($thisListEntry['link'] == 'yes') ? true : false), $thisListEntry['customsqlfunction']);
                  //else
                  $this->selectString .= ', ' . $this->get_field_name($pathName, $fieldArray[1], $thisListEntry['fieldid'], ($thisListEntry['link'] == 'yes') ? true : false);
                  $this->unionSelectString .=', ' . $thisListEntry['fieldid'];
               }


               if (isset($thisListEntry['fieldid']) && $thisListEntry['fieldid'] != '') {
                  $this->selectString .= " as \"" . $thisListEntry['fieldid'] . "\"";
                  $this->unionSelectString .= " as \"" . $thisListEntry['fieldid'] . "\"";
               }



               //2011-03-05 moved to the query array so we can also handle unions
               /*
                 //2011-02-03 for calculating percentages
                 if(isset($thisListEntry['valuetype']) && $thisListEntry['valuetype'] != '' && $thisListEntry['valuetype'] != '-')
                 {
                 // first part of value is calulated what to do with the alue ... 2nd part is SQL function we need
                 // 'OF' separates
                 $funcArray = split('OF', $thisListEntry['valuetype']);
                 if($this->totalSelectString == '') $this->totalSelectString = 'SELECT '; else $this->totalSelectString .= ', ';
                 $this->totalSelectString .= ' ' . $funcArray[1] . '(' . $this->get_field_name($pathName, $fieldArray[1], $thisListEntry['fieldid'], ($thisListEntry['link'] == 'yes') ? true : false) . ")  as '" . $thisListEntry['fieldid'] . "_total'";
                 }
                */
            }

            $this->fieldArray[$thisListEntry['fieldid']] = $thisListEntry['fieldid'];

            // 2010-12-18 handle currencies if value is set in vardefs
            // 2011-03-28 handle curencies in any case
            // 2012-11-04 also check the rootfieldNameMap
            if (isset($this->joinSegments[$pathName]) && (($this->joinSegments[$pathName]['object']->field_name_map[$fieldArray[1]]['type'] ?? '') == 'currency' || (isset($this->joinSegments[$pathName]['object']->field_name_map[$fieldArray[1]]['kreporttype']) && $this->joinSegments[$pathName]['object']->field_name_map[$fieldArray[1]]['kreporttype'] == 'currency')) || (isset($this->rootfieldNameMap) && $this->rootfieldNameMap[$thisListEntry['fieldid']]['type'] == 'currency')) {
               // if we have a currency id and no SQL function select the currency .. if we have an SQL fnction select -99 for the system currency
               if (isset($fieldArray[1]) && isset($this->joinSegments[$pathName]['object']->field_name_map[$fieldArray[1]]['currency_id']) && ($thisListEntry['sqlfunction'] == '-' || strtoupper($thisListEntry['sqlfunction']) == 'SUM'))
                  $this->selectString .= ", " . $this->joinSegments[$pathName]['alias'] . "." . $this->joinSegments[$pathName]['object']->field_name_map[$fieldArray[1]]['currency_id'] . " as '" . $thisListEntry['fieldid'] . "_curid'";
               else
                  $this->selectString .= ", '-99' as '" . $thisListEntry['fieldid'] . "_curid'";

               $this->unionSelectString .=', ' . $thisListEntry['fieldid'] . "_curid";

               $this->fieldArray[$thisListEntry['fieldid'] . "_curid"] = $thisListEntry['fieldid'] . "_curid";
            }

            // whatever we need this for?
            // TODO: check if we still need this and if what for
            //$selectFields[] = trim($thisListEntry['name'], ':');
         }
      }
      else {
         $this->selectString .= '*';
      }
   }

   /*
    * Function to build the where String
    */

   function build_where_string() {
      global $db, $app_list_strings, $beanList, $beanFiles, $current_user;

      /*
       * Block to build the Where Clause
       */
      // see if we need to ovveride
      /*
        if(is_array($this->whereOverrideArray))
        {
        foreach($this->whereOverrideArray as $overrideKey => $overrideData)
        {
        reset($this->whereArray);
        foreach($this->whereArray as $originalKey => $originalData)
        {
        if($originalData['fieldid'] == $overrideData['fieldid'])
        {
        $this->whereArray[$originalKey] = $overrideData;
        // need to exit the while loop
        }
        }
        }
        }
       */
      // initialize
      $arrayWhereGroupsIndexed = array();
      // $arrayWhereGroupsIndexed['root'] = array();
      // build the where String for each Group
      foreach ($this->whereGroupsArray as $whereGroupIndex => $thisWhereGroup) {
         $thisWhereString = '';
         // reset the Where fields and loop over all fields to see if any is in our group
         if (!is_array($this->whereArray)) {
            $this->whereArray = array();
         }
         reset($this->whereArray);
         foreach ($this->whereArray as $thisWhere) {
            //2012-11-24 cater for a potential empty where string
            $tempWhereString = '';
            // check if this is for the current group
            // 2011--01-24 add ignore filter
            if ($thisWhere['groupid'] == $thisWhereGroup['id'] && $thisWhere['operator'] != 'ignore') {

               // process the Field and link with the joinalias
               $fieldName = substr($thisWhere['path'], strrpos($thisWhere['path'], "::") + 2, strlen($thisWhere['path']));
               $pathName = substr($thisWhere['path'], 0, strrpos($thisWhere['path'], "::"));
               $fieldArray = explode(':', $fieldName);

               if ($thisWhere['jointype'] != 'notexisting') {
                  //getWhereOperatorClause($operator, $fieldname, $alias,  $value, $valuekey, $valueto)
                  //$thisWhereString .= $this->getWhereOperatorClause($thisWhere['operator'], $fieldArray[1], $this->joinSegments[$pathName]['alias'],  $thisWhere['value'], $thisWhere['valuekey'], $thisWhere['valueto']);
                  //2012-11-24 ... changed to fill into temnpWherestring
                  //2013-08-07 .. process fixed value 
                  if (!empty($thisWhere['fixedvalue']))
                     $tempWhereString = $this->getWhereOperatorClause($thisWhere['operator'], $fieldArray[1], '\'' . $thisWhere['fixedvalue'] . '\'', $pathName, $thisWhere['value'], $thisWhere['valuekey'], $thisWhere['valueto'], $thisWhere['valuetokey'], $thisWhere['jointype']);
                  elseif (!empty($pathName))
                     $tempWhereString = $this->getWhereOperatorClause($thisWhere['operator'], $fieldArray[1], $thisWhere['fieldid'], $pathName, $thisWhere['value'], $thisWhere['valuekey'], $thisWhere['valueto'], $thisWhere['valuetokey'], $thisWhere['jointype']);
               } else {
                  // we have a not esists clause
                  $tempWhereString .= 'not exists(';

                  // get the last enrty and the one before and the relevant arrays
                  $rightPath = substr($pathName, strrpos($pathName, "::") + 2, strlen($pathName));
                  $leftPath = substr($pathName, 0, strrpos($pathName, "::"));

                  // explode into the relevant arrays
                  $rightArray = explode(':', $rightPath);
                  $leftArray = explode(':', $leftPath);

                  // set aliases for left and right .. will be processed properly anyway in the build of the link
                  // ... funny enough so
                  $join_params = array(
                      'right_join_table_alias' => $this->joinSegments[$leftPath]['alias'],
                      'left_join_table_alias' => $this->joinSegments[$leftPath]['alias'],
                      'join_table_link_alias' => randomstring(),
                      'join_table_alias' => $this->joinSegments[$pathName]['alias']
                  );

                  $tempWhereString .= $this->joinSegments[$leftPath]['object']->$rightArray[2]->getWhereExistsStatement($join_params);

                  // add the standard Where Clause
                  // $thisWhereString .= $this->getWhereOperatorClause($thisWhere['operator'], $fieldArray[1], $this->joinSegments[$pathName]['alias'],  $thisWhere['value'], $thisWhere['valuekey'], $thisWhere['valueto']);
                  $tempWhereString .= 'AND ' . $this->getWhereOperatorClause($thisWhere['operator'], $fieldArray[1], $thisWhere['fieldid'], $pathName, $thisWhere['value'], $thisWhere['valuekey'], $thisWhere['valueto'], $thisWhere['valuetokey']);

                  // close the select clause
                  $tempWhereString .= ')';
               }

               //2012-11-24 moved to cehck if the where string returned something at all
               if ($tempWhereString != '') {
                  // if we have an where string already concetanate with the type for the group AND or OR
                  if ($thisWhereString != '')
                     $thisWhereString .= ' ' . $thisWhereGroup['type'] . ' (';
                  else
                     $thisWhereString .= '(';

                  $thisWhereString .= $tempWhereString;

                  // close this condition
                  $thisWhereString .= ')';
               }
            }
         }
         $thisWhereGroup['whereClause'] = $thisWhereString;

         // write into an array with the id as index in the array (will need that tobuild the hierarchy
         $arrayWhereGroupsIndexed[$thisWhereGroup['id']] = $thisWhereGroup;
      }

      // 2013-01-16 check if we have a where string already from the auth check
      // 2013-02-22 moved into the adding of the where clause ... 
      //if ($this->whereString != '')
      //    $this->whereString .= ' AND ';
      // process now topDown
      if (isset($arrayWhereGroupsIndexed['root'])) {
         $levelWhere = $this->buildWhereClauseForLevel($arrayWhereGroupsIndexed['root'], $arrayWhereGroupsIndexed);
         if ($levelWhere != '') {
            if ($this->whereString != '')
               $this->whereString .= ' AND ';
            $this->whereString .=$levelWhere;
         }
      }
      // 2010-07-18 additonal Filter mainly for the treeview
      if (is_array($this->whereAddtionalFilter)) {
         foreach ($this->whereAddtionalFilter as $filterFieldId => $filterFieldValue) {
            //special treatment for fied values where we do not have a path
            if ($this->get_fieldname_by_fieldid($filterFieldId) == '') {
               ($this->havingString == '') ? $this->havingString = 'HAVING ' : $this->havingString .= ' AND ';
               $this->havingString .= $filterFieldId . " = '" . $filterFieldValue . "'";
            } else {
               $whereOperatorWhere = $this->getWhereOperatorClause('equals', $this->get_fieldname_by_fieldid($filterFieldId), $filterFieldId, $this->get_fieldpath_by_fieldid($filterFieldId), $filterFieldValue, '', '', '');
               if ($whereOperatorWhere != '') {
                  if ($this->whereString != '')
                     $this->whereString .= ' AND ';
                  $this->whereString .= $whereOperatorWhere;
               }
            }
            // $this->whereString .= ' ' . $this->fieldNameMap[$filterFieldId]['tablealias'] . '.' . $this->fieldNameMap[$filterFieldId]['fieldname'] . ' = \'' . $filterFieldValue . '\'';
         }
      }

      // bugfix 2010-06-14 exclude deleted items
      // add feature fcheck if we shod show deleted records
      if (!$this->showDeleted) {
         if ($this->whereString != '')
            $this->whereString = 'WHERE ' . $this->rootGuid . '.deleted = \'0\' AND ' . $this->whereString;
         else
            $this->whereString = 'WHERE ' . $this->rootGuid . '.deleted = \'0\'';
      }
      else {
         if ($this->whereString != '')
            $this->whereString = 'WHERE ' . $this->whereString;
         else
            $this->whereString = '';
      }

    	$GLOBALS['log']->info('KReporter query: ' . $this->whereString);

      // bugfix 2010-08-19, respect ACL access for owner required
      // check for Role based access on root module
      // 2013-02-22 ... added anyway for each segment ... no need to add here again ... 
      /*
        if (!$current_user->is_admin && $this->joinSegments['root:' . $this->root_module]['object']->bean_implements('ACL') && ACLController::requireOwner($this->joinSegments['root:' . $this->root_module]['object']->module_dir, 'list')) {
        $this->whereString .= ' AND ' . $this->rootGuid . '.assigned_user_id=\'' . $current_user->id . '\'';
        }
       */
   }

   /*
    * Function to build the Where Clause for one level
    * calls build for Children and get calls recursively
    */

   function buildWhereClauseForLevel($thisLevel, $completeArray = array()) {
      $whereClause = '';

      //find Children
      foreach ($completeArray as $currentEntry) {
         if ($currentEntry['parent'] == $thisLevel['id']) {
            $thisLevel['children'][$currentEntry['id']] = $currentEntry;
         }
      }

      // if we have Children build the Where Clause for the Children
      if (isset($thisLevel['children']) && is_array($thisLevel['children']))
         $whereClauseChildren = $this->buildWhereClauseForChildren($thisLevel['children'], $thisLevel['type'], $completeArray);
      else
         $whereClauseChildren = '';

      // build the combined Whereclause
      if (isset($thisLevel['whereClause']) && $thisLevel['whereClause'] != '') {
         $whereClause = $thisLevel['whereClause'];
      }

      // add the Children Where Clauses if there is any
      if ($whereClauseChildren != '') {
         if ($whereClause != '')
            $whereClause .= ' ' . $thisLevel['type'] . ' ' . $whereClauseChildren;
         else
            $whereClause = $whereClauseChildren;
      }

      // if there is a where clause encapsulate it
      if ($whereClause != '')
         $whereClause = '(' . $whereClause . ')';

      // return whatever we have built
      return $whereClause;
   }

   /*
    * Function to build the Where Clause for the Children and return it
    */

   function buildWhereClauseForChildren($thisChildren, $thisOperator, $completeArray) {
      $whereClause = '';
      foreach ($thisChildren as $thisChild) {
         // recursively build the clause for this level and if we have
         // children we get called again ... loop top down ...
         $childWhereClause = $this->buildWhereClauseForLevel($thisChild, $completeArray);

         // check if there is something to add
         if ($childWhereClause != '') {
            if ($whereClause != '')
               $whereClause .= ' ' . $thisOperator . ' ' . $childWhereClause;
            else
               $whereClause = $childWhereClause;
         }
      }

      // check if we have a where Clause at all and if encapsulate
      return $whereClause;
   }

   /*
    * process the where operator
    */

   function getWhereOperatorClause($operator, $fieldname, $fieldid, $path, $value, $valuekey, $valueto, $valuetokey = '', $jointype = '') {
      global $current_user;
      // STIC-Custom EPS 20211220 - Anular el PR #505
      // https://github.com/SinergiaTIC/SinergiaCRM/pull/524
      // // STIC-Custom EPS 20241205
      // // https://github.com/SinergiaTIC/SinergiaCRM/pull/505
      $db = DBManagerFactory::getInstance();
      // // ENS STIC-Custom
      // ENS STIC-Custom

      // initialize
      $thisWhereString = '';

      // add ignore Operator 2011-01-24
      // in this case we simply jump back out returning an empty string.
      if ($operator == 'ignore')
         return '';



      //change if valuekey is set
      if (isset($valuekey) && $valuekey != '' && $valuekey != 'undefined')
         $value = $valuekey;
      if (isset($valuetokey) && $valuetokey != '' && $valuetokey != 'undefined')
         $valueto = $valuetokey;

      // replace the current _user_id if that one is set
      // bugfix 2010-09-28 since id was asisgned and not user name ..  no properly evaluates active user
      if ($value == 'current_user_id')
         $value = $current_user->user_name;


      // 2011-07-15 manage Date & DateTime Fields
      if (
              // STIC-Custom 20211104 AAM - Adding operators "after/before N days"  functionality
              // STIC#458
              $operator != 'beforendays' &&
              $operator != 'lastndays' &&
              $operator != 'lastnfdays' &&
              $operator != 'lastnweeks' &&
              $operator != 'notlastnweeks' &&
              $operator != 'lastnfweeks' &&
              // STIC-Custom 20211104 AAM - Adding operators "after/before N days"  functionality
              // STIC#458
              $operator != 'afterndays' &&
              $operator != 'nextndays' &&
              $operator != 'nextnweeks' &&
              $operator != 'notnextnweeks' &&
              $operator != 'betwndays' &&
              ($operator != 'lastnddays' && !is_numeric($value)) &&
              ($operator != 'nextnddays' && !is_numeric($value)) &&
              ($operator != 'betwnddays' && !is_numeric($value))
      ) {
         if (($this->fieldNameMap[$fieldid]['type']??'') == 'date') {
            //2011-07-17 ... get db formatted field from key field
            //else try legacy handliung with date interpretation
            if ($valuekey != '')
               $value = $valuekey;
            else
               $value = $GLOBALS['timedate']->to_db_date($value, false);

            if ($valuetokey != '')
               $valueto = $valuetokey;
            else
               $valueto = $GLOBALS['timedate']->to_db_date($value, false);
         }
         if (($this->fieldNameMap[$fieldid]['type']??'') == 'datetime' || ($this->fieldNameMap[$fieldid]['type']??'') == 'datetimecombo') {
            //2011-07-17 .. db formated dtae stroed in key field
            if ($valuekey != '')
               $value = $valuekey;
            else {
               // legacy handling ... try to interpret date
               $timeArray = explode(' ', $value);
               $value = $GLOBALS['timedate']->to_db_date($timeArray[0], false) . ' ' . $timeArray[1];
            }

            if ($valueto != '' || $valuetokey != '') {
               if ($valuetokey != '')
                  $valueto = $valuetokey;
               else {
                  // legacy handling ... try to interpret date
                  $timeArray = explode(' ', $valueto);
                  $valueto = $GLOBALS['timedate']->to_db_date($timeArray[0], false) . ' ' . $timeArray[1];
               }
            }
         }
      }


      // 2012-11-24 special handling for kreporttype fields that have a select eval set
      if ((($this->joinSegments[$path]['object']->field_name_map[$fieldname]['type']??'') == 'kreporter') && is_array(($this->joinSegments[$path]['object']->field_name_map[$fieldname]['eval'] ?? ''))) {
         //2013-01-22 added {tc}replacement with custom join
         $selString = preg_replace(array('/{t}/', '/{tc}/', '/{p1}/', '/{p2}/'), array($this->joinSegments[$path]['alias'], $this->joinSegments[$path]['customjoin'], $value, $valueto), $this->joinSegments[$path]['object']->field_name_map[$fieldname]['eval']['selection'][$operator]);
         return $selString;
      }

      switch ($operator) {
         case 'autocomplete':
            $thisWhereString .= $this->joinSegments[$path]['alias'] . '.id';
            break;
         default:
            $thisWhereString .= $this->get_field_name($path, $fieldname, $fieldid);
            break;
      }
      // process the operator

      switch ($operator) {
         case 'autocomplete':
            // STIC-Custom EPS 20241220 - only quote on some operators
            $value = $db->quote($value);
            // END STIC-Custom
            $thisWhereString .= ' = \'' . $value . '\'';
            break;
         case 'equals':
            // bug 2011-03-07 .. handle multienums
            // bug 2011-03-10 .. fixed date handling
            // bug 2011-03-21 fixed custom sql function
            // bug 2011-03-25 ... date handling no managed in client
            if ($this->fieldNameMap[$fieldid]['customFunction'] == '' && $this->fieldNameMap[$fieldid]['sqlFunction'] == '') {
               switch ($this->fieldNameMap[$fieldid]['type']) {
                  case 'multienum':
                     // STIC-Custom EPS 20241220 - only quote on some operators
                     // https://github.com/SinergiaTIC/SinergiaCRM/pull/523
                     // $thisWhereString .= ' LIKE \'%^' . $value . '^%\'';
                     // $thisWhereString .= ' OR ' . $this->get_field_name($path, $fieldname, $fieldid);
                     // $thisWhereString .= ' LIKE \'' . $value . '^%\'';
                     // $thisWhereString .= ' OR ' . $this->get_field_name($path, $fieldname, $fieldid);
                     // $thisWhereString .= ' LIKE \'%^' . $value . '\'';
                     // $thisWhereString .= ' OR ' . $this->get_field_name($path, $fieldname, $fieldid);
                     // $thisWhereString .= ' = \'' . $value . '\'';
                     $valueArray = (is_array($value) ? $value : preg_split('/,/', $value));
                     $multienumWhereString = '';
                     foreach ($valueArray as $thisMultiEnumValue) {
                        if ($multienumWhereString != '') {
                           $multienumWhereString .= ') AND (' . $this->get_field_name($path, $fieldname, $fieldid);
                           $closeMultienumWhereString = ')';
                        }
                        else {
                           $closeMultienumWhereString = '';
                        }
                           $multienumWhereString .= ' LIKE \'%^' . $thisMultiEnumValue . '^%\'';
                           $multienumWhereString .= ' OR ' . $this->get_field_name($path, $fieldname, $fieldid);
                           $multienumWhereString .= ' LIKE \'' . $thisMultiEnumValue . '^%\'';
                           $multienumWhereString .= ' OR ' . $this->get_field_name($path, $fieldname, $fieldid);
                           $multienumWhereString .= ' LIKE \'%^' . $thisMultiEnumValue . '\'';
                           $multienumWhereString .= ' OR ' . $this->get_field_name($path, $fieldname, $fieldid);
                           $multienumWhereString .= ' = \'' . $thisMultiEnumValue . '\'';
                     }
                     $numValues = count($valueArray);
                     if ($numValues > 1) {
                        $thisWhereString = '(' . $thisWhereString;
                     }
                     $thisWhereString .= $multienumWhereString . $closeMultienumWhereString;
                     if ($numValues > 0) {
                        $fieldName = $this->get_field_name($path, $fieldname, $fieldid);
                        // add condition counting the number of values to prevent selecting records withmore values selected
                        // there's no native way to count occurrences of substring in mysql, so difference in legth is used to simulate
                        $thisWhereString = '(' . $thisWhereString . ')' . " AND {$numValues} = ((CHAR_LENGTH({$fieldName}) - CHAR_LENGTH(REPLACE({$fieldName}, '^,^', ''))) / CHAR_LENGTH('^,^') + 1)";
                     }
                     // END STIC-Custom
                     break;
                     //		case 'date':
                        //		case 'datetime':
                           //			$thisWhereString .= ' = \'' . $GLOBALS['timedate']->to_db_date($value, false) . '\'';
                           //			break;
                  default:

                     // STIC-Custom EPS 20241220 - only quote on some operators
                     // https://github.com/SinergiaTIC/SinergiaCRM/pull/523
                     $value = $db->quote($value);
                     // END STIC-Custom
                     $thisWhereString .= ' = \'' . $value . '\'';
                     break;
               }
            }
            else {
               $value = $db->quote($value);
               $thisWhereString .= ' = \'' . $value . '\'';
            }
            break;
         case 'soundslike':
            // STIC-Custom EPS 20241220 - only quote on some operators
            // https://github.com/SinergiaTIC/SinergiaCRM/pull/523
            $value = $db->quote($value);
            // END STIC-Custom
            $thisWhereString .= ' SOUNDS LIKE \'' . $value . '\'';
            break;
         case 'notequal':
            // STIC-Custom EPS 20241220 - not equal not working on multi-enum
            // https://github.com/SinergiaTIC/SinergiaCRM/pull/523
            // $thisWhereString .= ' <> \'' . $value . '\'';
            if ($this->fieldNameMap[$fieldid]['customFunction'] == '' && $this->fieldNameMap[$fieldid]['sqlFunction'] == '') {
               switch ($this->fieldNameMap[$fieldid]['type']) {
                  case 'multienum':
                     $valueArray = (is_array($value) ? $value : preg_split('/,/', $value));
                     $multienumWhereString = '';
                     foreach ($valueArray as $thisMultiEnumValue) {
                        if ($multienumWhereString != '') {
                           $multienumWhereString .= ') AND (' . $this->get_field_name($path, $fieldname, $fieldid);
                           $closeMultienumWhereString = ')';
                        }
                        else {
                           $closeMultienumWhereString = '';
                        }
                           $multienumWhereString .= ' LIKE \'%^' . $thisMultiEnumValue . '^%\'';
                           $multienumWhereString .= ' OR ' . $this->get_field_name($path, $fieldname, $fieldid);
                           $multienumWhereString .= ' LIKE \'' . $thisMultiEnumValue . '^%\'';
                           $multienumWhereString .= ' OR ' . $this->get_field_name($path, $fieldname, $fieldid);
                           $multienumWhereString .= ' LIKE \'%^' . $thisMultiEnumValue . '\'';
                           $multienumWhereString .= ' OR ' . $this->get_field_name($path, $fieldname, $fieldid);
                           $multienumWhereString .= ' = \'' . $thisMultiEnumValue . '\'';
                     }
                     $numValues = count($valueArray);
                     if ($numValues > 1) {
                        $thisWhereString = '(' . $thisWhereString;
                     }
                     $thisWhereString .= $multienumWhereString . $closeMultienumWhereString;
                     if ($numValues > 0) {
                        $fieldName = $this->get_field_name($path, $fieldname, $fieldid);
                        // add condition counting the number of values to prevent selecting records withmore values selected
                        // there's no native way to count occurrences of substring in mysql, so difference in legth is used to simulate
                        $thisWhereString = '(' . $thisWhereString . ')' . " AND {$numValues} = ((CHAR_LENGTH({$fieldName}) - CHAR_LENGTH(REPLACE({$fieldName}, '^,^', ''))) / CHAR_LENGTH('^,^') + 1)";
                        $thisWhereString = 'NOT(' . $thisWhereString . ')';
                     }
                     break;
                  default:
                     $value = $db->quote($value);
                     $thisWhereString .= ' <> \'' . $value . '\'';
                     break;
               }
            }
            else {
               $value = $db->quote($value);
               $thisWhereString .= ' <> \'' . $value . '\'';
            }
            // END STIC-Custom
            break;
         case 'greater':
            // STIC-Custom EPS 20241220 - only quote on some operators
            // https://github.com/SinergiaTIC/SinergiaCRM/pull/523
            $value = $db->quote($value);
            // END STIC-Custom
            $thisWhereString .= ' > \'' . $value . '\'';
            break;
         case 'after':
            // bug 2011-03-10 .. fixed date handling
            // bug 2011-03-25 date no handled in client
            // $thisWhereString .= ' > \'' . $GLOBALS['timedate']->to_db_date($value, false) . '\'';
            // STIC-Custom EPS 20241220 - only quote on some operators
            // https://github.com/SinergiaTIC/SinergiaCRM/pull/523
            $value = $db->quote($value);
            // END STIC-Custom
            $thisWhereString .= ' > \'' . $value . '\'';
            break;
         case 'less':
            // STIC-Custom EPS 20241220 - only quote on some operators
            // https://github.com/SinergiaTIC/SinergiaCRM/pull/523
            $value = $db->quote($value);
            // END STIC-Custom
            $thisWhereString .= ' < \'' . $value . '\'';
            break;
         case 'before':
            // bug 2011-03-10 .. fixed date handling
            // bug 2011-03-25 date no handled in client
            // $thisWhereString .= ' < \'' . $GLOBALS['timedate']->to_db_date($value, false) . '\'';
            // STIC-Custom EPS 20241220 - only quote on some operators
            // https://github.com/SinergiaTIC/SinergiaCRM/pull/523
            $value = $db->quote($value);
            // END STIC-Custom
            $thisWhereString .= ' < \'' . $value . '\'';
            break;
         case 'greaterequal':
            // STIC-Custom EPS 20241220 - only quote on some operators
            // https://github.com/SinergiaTIC/SinergiaCRM/pull/523
            $value = $db->quote($value);
            // END STIC-Custom
            $thisWhereString .= ' >= \'' . $value . '\'';
            break;
         case 'lessequal':
            // STIC-Custom EPS 20241220 - only quote on some operators
            // https://github.com/SinergiaTIC/SinergiaCRM/pull/523
            $value = $db->quote($value);
            // END STIC-Custom
            $thisWhereString .= ' <= \'' . $value . '\'';
            break;
         case 'starts':
            // STIC-Custom EPS 20241220 - only quote on some operators
            // https://github.com/SinergiaTIC/SinergiaCRM/pull/523
            $value = $db->quote($value);
            // END STIC-Custom
            $thisWhereString .= ' LIKE \'' . $value . '%\'';
            break;
         case 'notstarts':
            // STIC-Custom EPS 20241220 - only quote on some operators
            // https://github.com/SinergiaTIC/SinergiaCRM/pull/523
            $value = $db->quote($value);
            // END STIC-Custom
            $thisWhereString .= ' NOT LIKE \'' . $value . '%\'';
            break;
         case 'contains':
            // STIC-Custom EPS 20241220 - only quote on some operators
            // https://github.com/SinergiaTIC/SinergiaCRM/pull/523
            $value = $db->quote($value);
            // END STIC-Custom
            $thisWhereString .= ' LIKE \'%' . $value . '%\'';
            break;
         case 'notcontains':
            // STIC-Custom EPS 20241220 - only quote on some operators
            // https://github.com/SinergiaTIC/SinergiaCRM/pull/523
            $value = $db->quote($value);
            // END STIC-Custom
            $thisWhereString .= ' NOT LIKE \'%' . $value . '%\'';
            break;
         case 'between':
            // bug 2011-03-10 .. fixed date handling
            // bug 2011-03-25 date handling now on client side
            // STIC-Custom EPS 20241220 - only quote on some operators
            // https://github.com/SinergiaTIC/SinergiaCRM/pull/523
            $value = $db->quote($value);
            // END STIC-Custom
            if ($this->fieldNameMap[$fieldid]['type'] == 'date' || $this->fieldNameMap[$fieldid]['type'] == 'datetime' || $this->fieldNameMap[$fieldid]['type'] == 'datetimecombo') {
            // $thisWhereString .= ' >= \'' . $GLOBALS['timedate']->to_db_date($value, false) . '\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . '<=\'' . $GLOBALS['timedate']->to_db_date($valueto, false) . '\'';
               // STIC-Custom EPS 20241220 - only quote on some operators
               // https://github.com/SinergiaTIC/SinergiaCRM/pull/523
               $valueto = $db->quote($valueto);
               // END STIC-Custom
               $thisWhereString .= ' >= \'' . $value . '\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . '<=\'' . $valueto . '\'';
            }
            elseif ($this->fieldNameMap[$fieldid]['type'] == 'varchar' || $this->fieldNameMap[$fieldid]['type'] == 'name') {
               //2012-11-24 change so we increae the last char by one ord numkber and change to a smaller than
               // this is more in the logic of the user
               $valueto = substr($valueto, 0, strlen($valueto) - 1) . chr(ord($valueto[strlen($valueto) - 1]) + 1);
               // STIC-Custom EPS 20241220 - only quote on some operators
               // https://github.com/SinergiaTIC/SinergiaCRM/pull/523
               $valueto = $db->quote($valueto);
               // END STIC-Custom
               $thisWhereString .= ' >= \'' . $value . '\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . '<\'' . $valueto . '\'';
            }
            else
               $thisWhereString .= ' >= \'' . $value . '\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . '<=\'' . $valueto . '\'';
            break;
         case 'isempty':
            $thisWhereString .= ' = \'\'';
            break;
         case 'isemptyornull':
            $thisWhereString .= ' = \'\' OR ' . $this->get_field_name($path, $fieldname, $fieldid) . ' IS NULL';
            break;
         case 'isnull':
            $thisWhereString .= ' IS NULL';
            break;
         case 'isnotempty':
            $thisWhereString .= ' <> \'\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . ' is not null';
            break;
         case 'oneof':
            if ($this->fieldNameMap[$fieldid]['type'] == 'multienum') {
               $valueArray = (is_array($value) ? $value : preg_split('/,/', $value));
               $multienumWhereString = '';
               foreach ($valueArray as $thisMultiEnumValue) {
                  if ($multienumWhereString != '')
                     $multienumWhereString .= ' OR ' . $this->get_field_name($path, $fieldname, $fieldid);
                     // STIC-Custom EPS 20241220 - oneof on multienum was potentially returning incorrect results
                     // https://github.com/SinergiaTIC/SinergiaCRM/pull/523
                     // $multienumWhereString .= ' LIKE \'%' . $thisMultiEnumValue . '%\'';
                     $multienumWhereString .= ' LIKE \'%^' . $thisMultiEnumValue . '^%\'';
                     $multienumWhereString .= ' OR ' . $this->get_field_name($path, $fieldname, $fieldid);
                     $multienumWhereString .= ' LIKE \'' . $thisMultiEnumValue . '^%\'';
                     $multienumWhereString .= ' OR ' . $this->get_field_name($path, $fieldname, $fieldid);
                     $multienumWhereString .= ' LIKE \'%^' . $thisMultiEnumValue . '\'';
                     $multienumWhereString .= ' OR ' . $this->get_field_name($path, $fieldname, $fieldid);
                     $multienumWhereString .= ' = \'' . $thisMultiEnumValue . '\'';
                     // END STIC-Custom
               }
               $thisWhereString .= $multienumWhereString;
            }
            else {
               $thisWhereString .= ' IN (\'' . str_replace(',', '\',\'', (is_array($value) ? implode(',', $value) : $value)) . '\')';
            }
            break;
         case 'oneofnot':
            if ($this->fieldNameMap[$fieldid]['type'] == 'multienum') {
               $valueArray = (is_array($value) ? $value : preg_split('/,/', $value));
               $multienumWhereString = '';
               // STIC-Custom EPS 20241220 - oneof on multienum was potentially returning incorrect results
               // https://github.com/SinergiaTIC/SinergiaCRM/pull/523
               // foreach ($valueArray as $thisMultiEnumValue) {
               //    if ($multienumWhereString != '')
               //       $multienumWhereString .= ' OR ' . $this->get_field_name($path, $fieldname, $fieldid);

               //    $multienumWhereString .= ' NOT LIKE \'%' . $thisMultiEnumValue . '%\'';
               // }
               // $thisWhereString .= $multienumWhereString;
               foreach ($valueArray as $thisMultiEnumValue) {
                  if ($multienumWhereString != '')
                     $multienumWhereString .= ' OR ' . $this->get_field_name($path, $fieldname, $fieldid);
                     $multienumWhereString .= ' LIKE \'%^' . $thisMultiEnumValue . '^%\'';
                     $multienumWhereString .= ' OR ' . $this->get_field_name($path, $fieldname, $fieldid);
                     $multienumWhereString .= ' LIKE \'' . $thisMultiEnumValue . '^%\'';
                     $multienumWhereString .= ' OR ' . $this->get_field_name($path, $fieldname, $fieldid);
                     $multienumWhereString .= ' LIKE \'%^' . $thisMultiEnumValue . '\'';
                     $multienumWhereString .= ' OR ' . $this->get_field_name($path, $fieldname, $fieldid);
                     $multienumWhereString .= ' = \'' . $thisMultiEnumValue . '\'';
                     
               }
               $thisWhereString .= $multienumWhereString;
               $thisWhereString = ' NOT(' . $thisWhereString .') ';
               // END STIC-Custom
            }
            else {
               $thisWhereString .= ' NOT IN (\'' . str_replace(',', '\',\'', (is_array($value) ? implode(',', $value) : $value)) . '\')';
            }
            break;
         case 'oneofnotornull':
            if ($this->fieldNameMap[$fieldid]['type'] == 'multienum') {
               $valueArray = (is_array($value) ? $value : preg_split('/,/', $value));
               $multienumWhereString = '';
               // STIC-Custom EPS 20241220 - oneof on multienum was potentially returning incorrect results
               // https://github.com/SinergiaTIC/SinergiaCRM/pull/523
               // foreach ($valueArray as $thisMultiEnumValue) {
               //    if ($multienumWhereString != '')
               //       $multienumWhereString .= ' OR ' . $this->get_field_name($path, $fieldname, $fieldid);

               //    $multienumWhereString .= ' NOT LIKE \'%' . $thisMultiEnumValue . '%\'';
               // }
               // $thisWhereString .= $multienumWhereString . 'OR ' . $this->get_field_name($path, $fieldname, $fieldid) . ' IS NULL';
               foreach ($valueArray as $thisMultiEnumValue) {
                  if ($multienumWhereString != '')
                     $multienumWhereString .= ' OR ' . $this->get_field_name($path, $fieldname, $fieldid);
                     $multienumWhereString .= ' LIKE \'%^' . $thisMultiEnumValue . '^%\'';
                     $multienumWhereString .= ' OR ' . $this->get_field_name($path, $fieldname, $fieldid);
                     $multienumWhereString .= ' LIKE \'' . $thisMultiEnumValue . '^%\'';
                     $multienumWhereString .= ' OR ' . $this->get_field_name($path, $fieldname, $fieldid);
                     $multienumWhereString .= ' LIKE \'%^' . $thisMultiEnumValue . '\'';
                     $multienumWhereString .= ' OR ' . $this->get_field_name($path, $fieldname, $fieldid);
                     $multienumWhereString .= ' = \'' . $thisMultiEnumValue . '\'';
                     
               }
               $thisWhereString .= $multienumWhereString;
               $thisWhereString = ' NOT(' . $thisWhereString .') OR ' . $this->get_field_name($path, $fieldname, $fieldid) . ' IS NULL';
               // END STIC-Custom
            }
            else {
               $thisWhereString .= ' NOT IN (\'' . str_replace(',', '\',\'', (is_array($value) ? implode(',', $value) : $value)) . '\') OR ' . $this->get_field_name($path, $fieldname, $fieldid) . ' IS NULL';
            }
            break;
         case 'today':
            $todayDate = date('Y-m-d', time());
            // STIC 20210714 AAM - MariaDB does not accept the time 24:00:00
            // STIC#354
            // $thisWhereString .= ' >= \'' . $todayDate . ' 00:00:00\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . ' < \'' . $todayDate . ' 24:00:00\'';
            $thisWhereString .= ' >= \'' . $todayDate . ' 00:00:00\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . ' <= \'' . $todayDate . ' 23:59:59\'';
            break;
         case 'past':
            $thisWhereString .= ' <= \'' . date('Y-m-d H:i:s', gmmktime(gmdate("H"))) . '\'';
            break;
         case 'future':
            $thisWhereString .= ' >= \'' . date('Y-m-d H:i:s', gmmktime(gmdate("H"))) . '\'';
            break;
         // STIC-Custom 20211104 AAM - Adding operators "after/before N days"  functionality
         // STIC#458
         case 'beforendays':
            $thisWhereString .= ' <= DATE_ADD(NOW(), INTERVAL -'.$value.' DAY)';
            break;
         case 'lastndays':
            $date = gmmktime(gmdate("H"));
            $thisWhereString .= ' >= \'' . date('Y-m-d H:i:s', gmmktime(gmdate("H")) - $value * 86400) . '\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . ' < \'' . date('Y-m-d H:i:s', gmmktime(gmdate("H"))) . '\'';
            break;
         case 'lastnfdays':
            $date = gmmktime(0, 0, 0, date('m', gmmktime(gmdate("H"))), date('d', gmmktime(gmdate("H"))), date('Y', gmmktime(gmdate("H"))));
            $thisWhereString .= ' >= \'' . gmdate('Y-m-d H:i:s', $date - $value * 86400) . '\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . ' < \'' . gmdate('Y-m-d H:i:s', $date) . '\'';
            break;
         case 'lastnddays':
            // if numeric we still have the number of days .. else we have a date
            if (is_numeric($value)) {
               $date = gmmktime(gmdate("H"));
               $thisWhereString .= ' >= \'' . date('Y-m-d H:i:s', gmmktime(gmdate("H")) - $value * 86400) . '\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . ' < \'' . date('Y-m-d H:i:s', gmmktime(gmdate("H"))) . '\'';
            }
            else
            // 2011-03-25 date handling no on client side
            //$thisWhereString .= ' >= \'' . $GLOBALS['timedate']->to_db_date($value, false) . '\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . ' < \'' . date('Y-m-d H:i:s', gmmktime(gmdate("H"))) . '\'';
               $thisWhereString .= ' >= \'' . $value . '\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . ' < \'' . date('Y-m-d H:i:s', gmmktime(gmdate("H"))) . '\'';
            break;
         case 'lastnweeks':
            $date = gmmktime(gmdate("H"));
            $thisWhereString .= ' >= \'' . date('Y-m-d H:i:s', gmmktime(gmdate("H")) - $value * 604800) . '\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . ' < \'' . date('Y-m-d H:i:s', gmmktime(gmdate("H"))) . '\'';
            break;
         case 'notlastnweeks':
            $date = gmmktime(gmdate("H"));
            $thisWhereString .= ' <= \'' . date('Y-m-d H:i:s', gmmktime(gmdate("H")) - $value * 604800) . '\' OR ' . $this->get_field_name($path, $fieldname, $fieldid) . ' > \'' . date('Y-m-d H:i:s', gmmktime(gmdate("H"))) . '\'';
            break;
         case 'lastnfweeks':
            $dayofWeek = date('N');
            $todayMidnight = gmmktime('23', '59', '59', date('n'), date('d'), date('Y'));
            $endStamp = gmmktime('23', '59', '59', date('n'), date('d'), date('Y')) - ( date('N') * 3600 * 24);
            $thisWhereString .= ' >= \'' . gmdate('Y-m-d H:i:s', $endStamp - $value * 604800 + 1) . '\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . ' < \'' . gmdate('Y-m-d H:i:s', $endStamp) . '\'';
            break;
         case 'lastnfmonth':
            $endMonth = date('n'); $endYear = date('Y');
            $endMonth = $endMonth - 1; 
            if($endMonth == 0){
               $endMonth = 12;
               $endYear--;
            }
            
            $endStamp = gmmktime('23', '59', '59', $endMonth, date('t', mktime(0, 0, 0, $endMonth, 1, $endYear)), $endYear);
            
            // get the startdate
            $startMonth = $endMonth; $startYear = $endYear;
            $value = $value -1;
            if($value >= 12){
               $startMonth = $startMonth - ($value % 12);
               if($startMonth <= 0){
                  $startMonth += 12;
                  $startYear--;
               }
               $startYear = $startYear - (($value - ($value % 12)) / 12);
            }
            else
            {
               $startMonth = $startMonth - $value;
               if($startMonth <= 0){
                  $startMonth += 12;
                  $startYear--;
               }
            }
            $startStamp = gmmktime('0', '0', '0', $startMonth, '1', $startYear);

            $thisWhereString .= ' >= \'' . gmdate('Y-m-d H:i:s', $startStamp) . '\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . ' < \'' . gmdate('Y-m-d H:i:s', $endStamp) . '\'';
            break;
         case 'thisweek':
            $dayofWeek = date('N');
            $todayMidnight = gmmktime('23', '59', '59', date('n'), date('d'), date('Y'));
            $startStamp = gmmktime('00', '00', '00', date('n'), date('d'), date('Y')) - ( (date('N') - 1) * 3600 * 24);
            $thisWhereString .= ' >= \'' . gmdate('Y-m-d H:i:s', $startStamp) . '\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . ' < \'' . gmdate('Y-m-d H:i:s', $startStamp + 604800 - 1) . '\'';
            break;
         // STIC-Custom 20211104 AAM - Adding operators "after/before N days"  functionality
         // STIC#458
         case 'afterndays':
            $thisWhereString .= ' >= DATE_ADD(NOW(), INTERVAL '.$value.' DAY)';
            break;
         case 'nextndays':
            $date = gmmktime(gmdate("H"));
            $thisWhereString .= ' <= \'' . date('Y-m-d H:i:s', gmmktime(gmdate("H")) + $value * 86400) . '\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . ' > \'' . date('Y-m-d H:i:s', gmmktime(gmdate("H"))) . '\'';
            break;
         case 'nextnddays':
            // if numeric we still have the number of days .. else we have a date
            if (is_numeric($value)) {
               $date = gmmktime(gmdate("H"));
               $thisWhereString .= ' <= \'' . date('Y-m-d H:i:s', gmmktime(gmdate("H")) + $value * 86400) . '\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . ' > \'' . date('Y-m-d H:i:s', gmmktime(gmdate("H"))) . '\'';
            } else {
               //$conCatAdd = ' <= \'' . $GLOBALS['timedate']->to_db_date($value) . '\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . ' > \'' . date('Y-m-d H:i:s') . '\'';
               // 2011-03-25 date handling now on client side
               // $thisWhereString .= ' <= \'' . $GLOBALS['timedate']->to_db_date($value, false) . '\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . ' > \'' . date('Y-m-d H:i:s', gmmktime(gmdate("H"))) . '\'';
               $thisWhereString .= ' <= \'' . $value . '\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . ' > \'' . date('Y-m-d H:i:s', gmmktime(gmdate("H"))) . '\'';
            }
            break;
         //2011-05-20 added between n days option
         case 'betwndays':
            $date = gmmktime(0, 0, 0, date('m', gmmktime(gmdate("H"))), date('d', gmmktime(gmdate("H"))), date('Y', gmmktime(gmdate("H"))));
            $thisWhereString .= ' >= \'' . gmdate('Y-m-d H:i:s', $date + $value * 86400) . '\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . ' < \'' . gmdate('Y-m-d H:i:s', $date + $valueto * 86400) . '\'';
            break;
            break;
         case 'betwnddays':
            if (is_numeric($value)) {
               $date = gmmktime(0, 0, 0, date('m', gmmktime(gmdate("H"))), date('d', gmmktime(gmdate("H"))), date('Y', gmmktime(gmdate("H"))));
               $thisWhereString .= ' >= \'' . gmdate('Y-m-d H:i:s', $date + $value * 86400) . '\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . ' < \'' . gmdate('Y-m-d H:i:s', $date + $valueto * 86400) . '\'';
            } else {
               $thisWhereString .= ' >= \'' . $value . '\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . ' < \'' . $valueto . '\'';
            }
            break;
         case 'nextnweeks':
            $date = gmmktime(gmdate("H"));
            $thisWhereString .= ' <= \'' . date('Y-m-d H:i:s', gmmktime(gmdate("H")) + $value * 604800) . '\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . ' > \'' . date('Y-m-d H:i:s', gmmktime(gmdate("H"))) . '\'';
            break;
         case 'notnextnweeks':
            $date = gmmktime(gmdate("H"));
            $thisWhereString .= ' >= \'' . date('Y-m-d H:i:s', gmmktime(gmdate("H")) + $value * 604800) . '\' OR ' . $this->get_field_name($path, $fieldname, $fieldid) . ' < \'' . date('Y-m-d H:i:s', gmmktime(gmdate("H"))) . '\'';
            break;
         case 'firstdayofmonth':
            $dateArray = getdate();
            $fromDate = date('Y-m-d', mktime(0, 0, 0, $dateArray['mon'], 1, $dateArray['year']));
            $thisWhereString .= ' >= \'' . $fromDate . ' 00:00:00\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . ' <= \'' . $fromDate . ' 23:59:59\'';
            break;
         case 'firstdaynextmonth':
            $dateArray = getdate();
            $fromDate = date('Y-m-d', mktime(0, 0, 0, $dateArray['mon'] == '12' ? 1 : $dateArray['mon'] + 1, 1, $dateArray['mon'] == '12' ? $dateArray['year'] + 1 : $dateArray['year']));
            $thisWhereString .= ' >= \'' . $fromDate . ' 00:00:00\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . ' <= \'' . $fromDate . ' 23:59:59\'';
            break;
         case 'nthdayofmonth':
            $dateArray = getdate();
            $fromDate = date('Y-m-d', mktime(0, 0, 0, $dateArray['mon'], $value, $dateArray['year']));
            $thisWhereString .= ' >= \'' . $fromDate . ' 00:00:00\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . ' <= \'' . $fromDate . ' 23:59:59\'';
            break;
         case 'thismonth':
            $dateArray = getdate();
            $fromDate = date('Y-m-d', mktime(0, 0, 0, $dateArray['mon'], 1, $dateArray['year']));
            $toDate = (($dateArray['mon'] + 1) > 12) ? date('Y-m-d', mktime(0, 0, 0, $dateArray['mon'] - 11, 1, $dateArray['year'] + 1)) : date('Y-m-d', mktime(0, 0, 0, $dateArray['mon'] + 1, 1, $dateArray['year']));
            $thisWhereString .= ' >= \'' . $fromDate . ' 00:00:00\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . ' < \'' . $toDate . ' 00:00:00\'';
            break;
         case 'notthismonth':
            $dateArray = getdate();
            $fromDate = date('Y-m-d', mktime(0, 0, 0, $dateArray['mon'], 1, $dateArray['year']));
            $toDate = (($dateArray['mon'] + 1) > 12) ? date('Y-m-d', mktime(0, 0, 0, $dateArray['mon'] - 11, 1, $dateArray['year'] + 1)) : date('Y-m-d', mktime(0, 0, 0, $dateArray['mon'] + 1, 1, $dateArray['year']));
            $thisWhereString .= ' <= \'' . $fromDate . ' 00:00:00\' OR ' . $this->get_field_name($path, $fieldname, $fieldid) . ' > \'' . $toDate . ' 00:00:00\'';
            break;
         case 'next3month':
            $dateArray = getdate();
            $fromDate = date('Y-m-d', mktime(0, 0, 0, $dateArray['mon'], 1, $dateArray['year']));
            $toDate = (($dateArray['mon'] + 3) > 12) ? date('Y-m-d', mktime(0, 0, 0, $dateArray['mon'] - 8, 1, $dateArray['year'] + 1)) : date('Y-m-d', mktime(0, 0, 0, $dateArray['mon'] + 3, 1, $dateArray['year']));
            $thisWhereString .= ' >= \'' . $fromDate . ' 00:00:00\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . ' < \'' . $toDate . ' 00:00:00\'';
            break;
        case 'nextnmonth':
                //MySQL way
                //$thisWhereString .= ' >= \'DATE_ADD(LAST_DAY(CURDATE()), INTERVAL 1 DAY)\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . ' < \'DATE_ADD(LAST_DAY(DATE_ADD(CURDATE(), INTERVAL '.$value.' MONTH)), INTERVAL 1 DAY)\'';
                //do it per dates to be comaptible with other databases    
        	$calcDate = new DateTime(gmdate('Y-m-d'));                
            $calcDate->add(new DateInterval('P1M')); //Add 1 month
            $fromDate = $calcDate->format('Y-m-01');   //Get the first day of the next month as string
            $calcDate->add(new DateInterval('P'.$value.'M'));
            $toDate = $calcDate->format('Y-m-01');
            $thisWhereString .= ' >= \''.$fromDate.'\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . ' < \''.$toDate.'\'';                
            break;
         case 'nextmonth':
            $dateArray = getdate();
            $fromDate = date('Y-m-d', mktime(0, 0, 0, $dateArray['mon'], 1, $dateArray['year']));
            $toDate = (($dateArray['mon'] + 1) > 12) ? date('Y-m-d', mktime(0, 0, 0, $dateArray['mon'] - 10, 1, $dateArray['year'] + 1)) : date('Y-m-d', mktime(0, 0, 0, $dateArray['mon'] + 1, 1, $dateArray['year']));
            $thisWhereString .= ' >= \'' . $fromDate . ' 00:00:00\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . ' < \'' . $toDate . ' 00:00:00\'';
            break;
         // added mor where Opertors Bug #486
         case 'next3monthDaily':
            $dateArray = getdate();
            $fromDate = date('Y-m-d', mktime(0, 0, 0, $dateArray['mon'], 1, $dateArray['year']));
            $toDate = (($dateArray['mon'] + 3) > 12) ? date('Y-m-d', mktime(0, 0, 0, $dateArray['mon'] - 9, $dateArray['mday'], $dateArray['year'] + 1)) : date('Y-m-d', mktime(0, 0, 0, $dateArray['mon'] + 3, $dateArray['mday'], $dateArray['year']));
            $thisWhereString .= ' >= \'' . $fromDate . ' 00:00:00\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . ' < \'' . $toDate . ' 00:00:00\'';
            break;
         case 'next6month':
            $dateArray = getdate();
            $fromDate = date('Y-m-d', mktime(0, 0, 0, $dateArray['mon'], 1, $dateArray['year']));
            $toDate = (($dateArray['mon'] + 6) > 12) ? date('Y-m-d', mktime(0, 0, 0, $dateArray['mon'] - 5, 1, $dateArray['year'] + 1)) : date('Y-m-d', mktime(0, 0, 0, $dateArray['mon'] + 6, 1, $dateArray['year']));
            $thisWhereString .= ' >= \'' . $fromDate . ' 00:00:00\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . ' < \'' . $toDate . ' 00:00:00\'';
            break;
         case 'next6monthDaily':
            $dateArray = getdate();
            $fromDate = date('Y-m-d', mktime(0, 0, 0, $dateArray['mon'], 1, $dateArray['year']));
            $toDate = (($dateArray['mon'] + 6) > 12) ? date('Y-m-d', mktime(0, 0, 0, $dateArray['mon'] - 6, $dateArray['mday'], $dateArray['year'] + 1)) : date('Y-m-d', mktime(0, 0, 0, $dateArray['mon'] + 6, $dateArray['mday'], $dateArray['year']));
            $thisWhereString .= ' >= \'' . $fromDate . ' 00:00:00\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . ' < \'' . $toDate . ' 00:00:00\'';
            break;
         case 'last3monthDaily':
            $dateArray = getdate();
            $fromDate = (($dateArray['mon'] - 3) < 1) ? date('Y-m-d', mktime(0, 0, 0, $dateArray['mon'] + 9, $dateArray['mday'], $dateArray['year'] - 1)) : date('Y-m-d', mktime(0, 0, 0, $dateArray['mon'] - 3, $dateArray['mday'], $dateArray['year']));
            $toDate = date('Y-m-d', mktime(0, 0, 0, $dateArray['mon'], 1, $dateArray['year']));
            $thisWhereString .= ' >= \'' . $fromDate . ' 00:00:00\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . ' < \'' . $toDate . ' 00:00:00\'';
            break;
         case 'last6month':
            $dateArray = getdate();
            $fromDate = (($dateArray['mon'] - 6) < 1) ? date('Y-m-d', mktime(0, 0, 0, $dateArray['mon'] + 6, 1, $dateArray['year'] - 1)) : date('Y-m-d', mktime(0, 0, 0, $dateArray['mon'] - 6, 1, $dateArray['year']));
            $toDate = date('Y-m-d', mktime(0, 0, 0, $dateArray['mon'], 1, $dateArray['year']));
            $thisWhereString .= ' >= \'' . $fromDate . ' 00:00:00\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . ' < \'' . $toDate . ' 00:00:00\'';
            break;
         case 'last6monthDaily':
            $dateArray = getdate();
            $fromDate = (($dateArray['mon'] - 6) < 1) ? date('Y-m-d', mktime(0, 0, 0, $dateArray['mon'] + 6, $dateArray['mday'], $dateArray['year'] - 1)) : date('Y-m-d', mktime(0, 0, 0, $dateArray['mon'] - 6, $dateArray['mday'], $dateArray['year']));
            $toDate = date('Y-m-d', mktime(0, 0, 0, $dateArray['mon'], 1, $dateArray['year']));
            $thisWhereString .= ' >= \'' . $fromDate . ' 00:00:00\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . ' < \'' . $toDate . ' 00:00:00\'';
            break;

         case 'thisyear':
            $dateArray = getdate();
            $fromDate = date('Y-m-d', mktime(0, 0, 0, 1, 1, $dateArray['year']));
            $toDate = date('Y-m-d', mktime(0, 0, 0, 1, 1, $dateArray['year'] + 1));
            $thisWhereString .= ' >= \'' . $fromDate . ' 00:00:00\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . ' < \'' . $toDate . ' 00:00:00\'';
            break;
         case 'lastmonth':
            $dateArray = getdate();
            //bug 2011-08-09 removed h:i:s from format
            $fromDate = (($dateArray['mon'] - 1) < 1) ? date('Y-m-d', mktime(0, 0, 0, $dateArray['mon'] + 11, 1, $dateArray['year'] - 1)) : date('Y-m-d', mktime(0, 0, 0, $dateArray['mon'] - 1, 1, $dateArray['year']));
            $toDate = date('Y-m-d', mktime(0, 0, 0, $dateArray['mon'], 1, $dateArray['year']));
            $thisWhereString .= ' >= \'' . $fromDate . ' 00:00:00\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . ' < \'' . $toDate . ' 00:00:00\'';
            break;
         case 'last3month':
            $dateArray = getdate();
            //bug 2011-08-09 removed h:i:s from format
            $fromDate = (($dateArray['mon'] - 3) < 1) ? date('Y-m-d', mktime(0, 0, 0, $dateArray['mon'] + 8, 1, $dateArray['year'] - 1)) : date('Y-m-d', mktime(0, 0, 0, $dateArray['mon'] - 3, 1, $dateArray['year']));
            $toDate = date('Y-m-d', mktime(0, 0, 0, $dateArray['mon'], 1, $dateArray['year']));
            $thisWhereString .= ' >= \'' . $fromDate . ' 00:00:00\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . ' < \'' . $toDate . ' 00:00:00\'';
            break;
         case 'lastyear':
            $dateArray = getdate();
            $fromDate = date('Y-m-d', mktime(0, 0, 0, 1, 1, $dateArray['year'] - 1));
            $toDate = date('Y-m-d', mktime(0, 0, 0, 1, 1, $dateArray['year']));
            $thisWhereString .= ' >= \'' . $fromDate . ' 00:00:00\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . ' < \'' . $toDate . ' 00:00:00\'';
            break;
         case 'tyytd':
            $year = date('Y');
            $thisWhereString .= ' >= \'' . $year . '-1-1\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . '<=\'' . date('Y-m-d') . '\'';
            break;
         case 'lyytd':
            $year = date('Y') - 1;
            $thisWhereString .= ' >= \'' . $year . '-1-1\' AND ' . $this->get_field_name($path, $fieldname, $fieldid) . '<=\'' . $year . '-' . date('m-d') . '\'';
            break;
      }

      //2011-07-20 handle jointype
      if ($jointype == 'optional' && $operator != 'isnull') {
         $thisWhereString = '( ' . $thisWhereString . ' ) OR ' . $this->get_field_name($path, $fieldname, $fieldid) . ' IS NULL';
      }

      return $thisWhereString;
   }

   /*
    * function to biuild the Group By Clause
    */

   function check_groupby($additionalGroupBy = array()) {
      //2013-07-22 always an array .. cuased issues on MSSQL and Oracle
      if (is_array($additionalGroupBy) && count($additionalGroupBy) > 0) {
         $this->isGrouped = true;
      } else {
         foreach ($this->listArray as $thisList) {
            if ($thisList['groupby'] != 'no') {
               $this->isGrouped = true;
            }
         }
      }
      if ($this->isGrouped)
         foreach ($this->listArray as $listArrayIndex => $thisList) {
            if ($thisList['groupby'] == 'no' && ($GLOBALS['db']->dbType == 'mssql' || $GLOBALS['db']->dbType == 'oci8') && $this->evalSQLFunctions && ($thisList['sqlfunction'] == '' || $thisList['sqlfunction'] == '-'))
               $this->listArray[$listArrayIndex]['sqlfunction'] = 'MIN';
         }
   }

   function build_groupby_string($additionalGroupBy = array()) {
      global $db, $app_list_strings, $beanList, $beanFiles;

      /*
       * Block to build the Group By Clause
       */
      // build Group By
      reset($this->listArray);

      // empty String
      $this->groupbyString = '';
      if (is_array($additionalGroupBy)) {
         foreach ($additionalGroupBy as $thisFieldData)
            $groupedFields[] = $thisFieldData['fieldid'];
      }
      else
         $additionalGroupBy = array();

      if (is_array($this->listArray)) {

         foreach ($this->listArray as $thisList) {
            // 2011-03-25 add check on exclusive grouping - called from the tree
            if ((!$this->exclusiveGroupinbgByAddParams && $thisList['groupby'] != 'no') || in_array($thisList['fieldid'], $additionalGroupBy)) {

               // if we are first add GROUP By to the Group By String else a comma
               if ($this->groupbyString == '')
                  $this->groupbyString .= 'GROUP BY ';
               else
                  $this->groupbyString .= ', ';

               // $this->addPath($thisList['path'], $this->switchJoinType($thisList['jointype']));
               $fieldName = substr($thisList['path'], strrpos($thisList['path'], "::") + 2, strlen($thisList['path']));
               $pathName = substr($thisList['path'], 0, strrpos($thisList['path'], "::"));

               $fieldArray = explode(':', $fieldName);

               // if we have a fixed value or we simply group by the fields
               if ((isset($thisList['fixedvalue']) && $thisList['fixedvalue'] != '') || $this->groupByFieldID)
                  $this->groupbyString .= $thisList['fieldid'];
               else {
                  // process custom SQL functions here
                  //if(isset($thisList['customsqlfunction']) && $thisList['customsqlfunction'] != '')
                  //	$this->groupbyString .= str_replace('$', $this->get_field_name($pathName, $fieldArray[1], $fieldArray[0]), $thisList['customsqlfunction']);
                  //else
                  $this->groupbyString .= $this->get_field_name($pathName, $fieldArray[1], $thisList['fieldid']);
               }

               // $this->groupbyString .= $this->get_field_name($pathName, $fieldArray[1], $fieldArray[0]);
               //$groupbyString .=  (isset($thisList['name'])) ? "'" . trim($thisList['name'], ':') . "'" : $this->joinSegments[$pathName]['alias'] . '.' . $fieldArray[1];
            }
         }
      }

      //2011-05-02 select max id when we have a grouping
      //if($this->groupbyString != '')
      //	$this->selectString =  str_replace($this->rootGuid . '.id', 'MAX(' . $this->rootGuid . '.id)', $this->selectString);
   }

   function build_orderby_string() {
      global $db, $app_list_strings, $beanList, $beanFiles;

      /*
       * Block to Build the Order by Clause
       */
      $sortArray = array();

      // build Order By
      reset($this->listArray);

      // empty String
      $this->orderbyString = '';

      if (is_array($this->listArray)) {

         if (count($this->sortOverride) > 0) {
            if ($this->orderByFieldID)
               $sortArray['1'][] = $this->sortOverride['sortid'] . ' ' . strtoupper($this->sortOverride['sortseq']);
            else {
               //2013-11-12 bugfix to include SWL function when sorting. Bug #510
               if ($this->fieldNameMap[$this->sortOverride['sortid']]['sqlFunction'] == '-' || $this->fieldNameMap[$this->sortOverride['sortid']]['sqlFunction'] == '' || !$this->evalSQLFunctions)
                  $sortArray['1'][] = $this->get_field_name($this->get_fieldpath_by_fieldid($this->sortOverride['sortid']), $this->get_fieldname_by_fieldid($this->sortOverride['sortid']), $this->sortOverride['sortid']) . ' ' . strtoupper($this->sortOverride['sortseq']);
               else {
                  // STIC 20210714 AAM - It was using COUNT_DISTINCT in the sorting query but it's wrong. COUNT_DISTINCT is only an internal SuiteCRM/KReporter name for the function COUNT(DISTINCT <field>)
                  // STIC#354
                  // $sortArray[$thisList['sortpriority']][] = $this->fieldNameMap[$this->sortOverride['sortid']]['sqlFunction'] . '(' . $this->get_field_name($this->get_fieldpath_by_fieldid($this->sortOverride['sortid']), $this->get_fieldname_by_fieldid($this->sortOverride['sortid']), $this->sortOverride['sortid']) . ')' . ' ' . strtoupper($this->sortOverride['sortseq']);
                  $sortArray['1'][] = ($this->fieldNameMap[$this->sortOverride['sortid']]['sqlFunction'] == 'COUNT_DISTINCT' ? 'COUNT(DISTINCT ' : $this->fieldNameMap[$this->sortOverride['sortid']]['sqlFunction'] .'(') . $this->get_field_name($this->get_fieldpath_by_fieldid($this->sortOverride['sortid']), $this->get_fieldname_by_fieldid($this->sortOverride['sortid']), $this->sortOverride['sortid']) . ')' . ' ' . strtoupper($this->sortOverride['sortseq']);
               }
            }
         } else {
            foreach ($this->listArray as $thisList) {
               if ($thisList['sort'] == 'asc' || $thisList['sort'] == 'desc') {

                  $fieldName = substr($thisList['path'], strrpos($thisList['path'], "::") + 2, strlen($thisList['path']));
                  $pathName = substr($thisList['path'], 0, strrpos($thisList['path'], "::"));

                  $fieldArray = explode(':', $fieldName);

                  if ($thisList['sortpriority'] != '') {
                     // check if we should build a sort string based on ID (mainly for the Union Joins)
                     if ($this->orderByFieldID) {
                        $sortArray[$thisList['sortpriority']][] = $thisList['fieldid'] . ' ' . strtoupper($thisList['sort']);
                     } else {
                        if ($thisList['sqlfunction'] == '-' || !$this->evalSQLFunctions)
                        // 2013-01-20 change in call paramteres
                        //$sortArray[$thisList['sortpriority']][] = $this->get_field_name($pathName, $fieldArray[1], $fieldArray[0]) . ' ' . strtoupper($thisList['sort']);
                           $sortArray[$thisList['sortpriority']][] = $this->get_field_name($pathName, $fieldArray[1], $thisList['fieldid']) . ' ' . strtoupper($thisList['sort']);
                        else
                        // 2013-01-20 change in call paramteres
                        //$sortArray[$thisList['sortpriority']][] = $thisList['sqlfunction'] . '(' . $this->get_field_name($pathName, $fieldArray[1], $fieldArray[0]) . ')' . ' ' . strtoupper($thisList['sort']);
                           $sortArray[$thisList['sortpriority']][] = $thisList['sqlfunction'] . '(' . $this->get_field_name($pathName, $fieldArray[1], $fieldArray[0]) . ')' . ' ' . strtoupper($thisList['sort']);
                     }
                  }
                  else {
                     if ($this->orderByFieldID) {
                        $sortArray['100'][] = $thisList['fieldid'] . ' ' . strtoupper($thisList['sort']);
                     } else {
                        if ($thisList['sqlfunction'] == '-' || !$this->evalSQLFunctions) {
                           // 2013-01-20 change in call paramteres
                           //$sortArray['100'][] = $this->get_field_name($pathName, $fieldArray[1], $fieldArray[0]) . ' ' . strtoupper($thisList['sort']);
                           $sortArray['100'][] = $this->get_field_name($pathName, $fieldArray[1], $thisList['fieldid']) . ' ' . strtoupper($thisList['sort']);
                        } else {
                           // 2013-01-20 change in call paramteres
                           // $sortArray['100'][] = $thisList['sqlfunction'] . '(' . $this->get_field_name($pathName, $fieldArray[1], $fieldArray[0]) . ')' . ' ' . strtoupper($thisList['sort']);

                           // STIC 20210714 AAM - It was using COUNT_DISTINCT in the sort query but it's wrong. COUNT_DISTINCT is only an internal name for the function COUNT(DISTINCT <field>)
                           // STIC#354
                           // $sortArray['100'][] = $thisList['sqlfunction'] . '(' . $this->get_field_name($pathName, $fieldArray[1], $thisList['fieldid']) . ')' . ' ' . strtoupper($thisList['sort']);
                           $sortArray['100'][] = ($thisList['sqlfunction'] == 'COUNT_DISTINCT' ? 'COUNT(DISTINCT ' : $thisList['sqlfunction'] . '(') . $this->get_field_name($pathName, $fieldArray[1], $thisList['fieldid']) . ')' . ' ' . strtoupper($thisList['sort']);
                        }
                     }
                  }
               }
            }
         }

         //2013-02-05 add deault sort to support MSSQL
         if (is_array($sortArray) && count($sortArray) > 0) {
            // set flag since we are first Entry
            $isFirst = true;

            // sort the array by the sort priority
            ksort($sortArray);

            foreach ($sortArray as $sortStrings) {
               foreach ($sortStrings as $sortString) {
                  if ($isFirst) {
                     $this->orderbyString .= 'ORDER BY ' . $sortString;
                     $isFirst = false;
                  } else {
                     $this->orderbyString .= ', ' . $sortString;
                  }
               }
            }
         } else {
            if ($this->isGrouped && ($GLOBALS['db']->dbType == 'mssql' || $GLOBALS['db']->dbType == 'oci8')) {
               if ($this->orderByFieldID)
                  $this->orderbyString .= 'ORDER BY MIN(sugarRecordId) ASC';
               else
                  $this->orderbyString .= 'ORDER BY MIN(' . $this->rootGuid . '.id) ASC';
            }
            else {
               if ($this->orderByFieldID)
                  $this->orderbyString .= 'ORDER BY sugarRecordId ASC';
               else
                  $this->orderbyString .= 'ORDER BY ' . $this->rootGuid . '.id ASC';
            }
         }
         //else
         //    $this->orderbyString .= 'ORDER BY sugarRecordId';
      }
   }

   /*
    * Function that gets the table for a module
    */

   function get_table_for_module($module) {
      global $beanList, $beanFiles;
      if (isset($beanList[$module]) && !empty($beanList[$module]) && isset($beanFiles[$beanList[$module]])) {
         require_once($beanFiles[$beanList[$module]]);

         if (class_exists($beanList[$module])) {
            $thisModule = new $beanList[$module];
            return $thisModule->table_name;
         }
      }
      return '';
   }

   /*
    * Helper function to get the Field name
    */

   function get_field_name($path, $field, $fieldid, $link = false, $sqlFunction = '') {
      // if we do not have a path we have a fixed value field so do not return a name
      if ($path != '') {
         // normal processing
         $thisAlias = (isset($this->joinSegments[$path]['object']) && 
                       isset($this->joinSegments[$path]['object']->field_name_map[$field]['source']) && 
                       $this->joinSegments[$path]['object']->field_name_map[$field]['source'] === 'custom_fields') 
                     ? ($this->joinSegments[$path]['customjoin'] ?? '')
                     : ($this->joinSegments[$path]['alias'] ?? '');

         global $beanList;
         // 2010-25-10 replace the -> object name with get_class function to handle also the funny aCase obejcts
         $thisModule = (isset($this->joinSegments[$path]['object']) && is_object($this->joinSegments[$path]['object'])) 
                     ? array_search(get_class($this->joinSegments[$path]['object']), $beanList) 
                     : false;

         // bugfix 2011-03-21 moved up to allow proper value handling in tree
         // get the field details
         $thisFieldIdEntry = $this->get_listfieldentry_by_fieldid($fieldid);

         // set the FieldName Map entries
         if (!isset($this->fieldNameMap[$fieldid]))
            $this->fieldNameMap[$fieldid] = array(
                'fieldname' => $field,
                'path' => $path,
                // 2013-08-21 BUG #491 adding path alias ... required for custom fields
                'pathalias' => $this->joinSegments[$path]['alias'],
                'islink' => $link,
                'sqlFunction' => $sqlFunction,
                'customFunction' => (is_array($thisFieldIdEntry) && array_key_exists('customsqlfunction', $thisFieldIdEntry) ? $thisFieldIdEntry['customsqlfunction'] : ''),
                'tablealias' => $thisAlias,
                'fields_name_map_entry' => ($this->joinSegments[$path]['object']->field_name_map[$field] ?? null),
                'type' => (($this->joinSegments[$path]['object']->field_name_map[$field]['type']??'') == 'kreporter') ? $this->joinSegments[$path]['object']->field_name_map[$field]['kreporttype'] : ($this->joinSegments[$path]['object']->field_name_map[$field]['type']??''),
                'module' => $thisModule);

         // check for custom function
         if (($this->joinSegments[$path]['object']->field_name_map[$field]['type']?? '') == 'kreporter') {
            //2012-12-24 checkif eval is an array ... then we have to do more
            $thisEval = '';
            if (is_array($this->joinSegments[$path]['object']->field_name_map[$field]['eval']))
               $thisEval = $this->joinSegments[$path]['object']->field_name_map[$field]['eval']['presentation']['eval'];
            else
               $thisEval = $this->joinSegments[$path]['object']->field_name_map[$field]['eval'];

            // 2010-12-06 add § for custom Fields to be evaluated
            // 2012-11-24 changed to pregreplace and also included {t}
            if (array_key_exists('customjoin', $this->joinSegments[$path]))
            //2013-01-22 also replace {tc}with the custom table join
            //return '(' . str_replace('§', $this->joinSegments[$path]['customjoin'], preg_replace(array('/\$/', '/{t}/'), $thisAlias, $thisEval)) . ')';
            // 2013-02-20 change to set proper alias if field is a cstm field
            //return '(' . preg_replace(array('/§/', '/{tc}/'), $this->joinSegments[$path]['customjoin'], preg_replace(array('/\$/', '/{t}/'), $thisAlias, $thisEval)) . ')';
               return '(' . preg_replace(array('/\$/', '/{t}/', '/§/', '/{tc}/'), array($this->joinSegments[$path]['alias'], $this->joinSegments[$path]['alias'], $this->joinSegments[$path]['customjoin'], $this->joinSegments[$path]['customjoin']), $thisEval) . ')';
            else
               return '(' . preg_replace(array('/\$/', '/{t}/'), $thisAlias, $thisEval) . ')';
         }
         elseif (isset($thisFieldIdEntry['customsqlfunction']) && $thisFieldIdEntry['customsqlfunction'] != '') { {
               //2012-11-28 srip unicode characters with the pregreplace [^(\x20-\x7F)]* from the string .. 
               //2013-01-22 changed to rawurldecode
               //$functionRaw = preg_replace('/[^(\x20-\x7F)]*/', '', urldecode(base64_decode($thisFieldIdEntry['customsqlfunction'], true)));
               $functionRaw = preg_replace('/[^(\x20-\x7F)]*/', '', rawurldecode(base64_decode($thisFieldIdEntry['customsqlfunction'], true)));

               // if the value is not base 64
               if (!$functionRaw)
                  $functionRaw = $thisFieldIdEntry['customsqlfunction'];

               //2012-10-03 add support to use the field and table explicit using {f} & {t}
               //2013-01-22 also replace {tc}with custom table
               //$functionRaw = trim(preg_replace(array('/{t}/', '/{f}/', '/\$/'), array($thisAlias, $field, $thisAlias), $functionRaw));
               //2013-02-20 change to set proper alias if field is a cstm field
               //$functionRaw = trim(preg_replace(array('/{t}/', '/{tc}/', '/{f}/', '/\$/'), array($thisAlias, $this->joinSegments[$path]['customjoin'], $field, $thisAlias), $functionRaw));
               $functionRaw = trim(preg_replace(array('/{t}/', '/{tc}/', '/{f}/', '/\$/'), array($this->joinSegments[$path]['alias'], $this->joinSegments[$path]['customjoin']??'', $field, $this->joinSegments[$path]['alias']), $functionRaw));
               return '(' . $functionRaw . ')';
            }
         } else {
            return $thisAlias . '.' . $field;
         }
      }
      else
         return $fieldid;
   }

   function get_fieldname_by_fieldid($fieldid) {
      return isset($this->fieldNameMap[$fieldid]) ? $this->fieldNameMap[$fieldid]['fieldname'] : '';
   }

   function get_fieldpath_by_fieldid($fieldid) {
      return isset($this->fieldNameMap[$fieldid]) ? $this->fieldNameMap[$fieldid]['path'] : '';
   }

   function get_listfieldentry_by_fieldid($fieldid) {
      foreach ($this->listArray as $thisIndex => $listFieldEntry) {
         if ($listFieldEntry['fieldid'] == $fieldid)
            return $listFieldEntry;
      }
      // 2013-05-16 ... bug #480 since we might query for fields that are not in the report
      // those fields are created dynamically in the pivot for the grid ... 
      // there the formatter set in the Pivot Paraeters is then used
      // if we do not find the field return false
      return false;
   }

}

?>
