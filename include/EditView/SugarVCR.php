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

 define('VCREND', '50');
 define('VCRSTART', '10');
 /**
  * @api
  */
 #[\AllowDynamicProperties]
 class SugarVCR
 {

    /**
     * records the query in the session for later retrieval
     */
     public static function store($module, $query)
     {
         $_SESSION[$module .'2_QUERY'] = $query;
     }

     /**
      * This function retrieves a query from the session
      */
     public static function retrieve($module)
     {
         return (!empty($_SESSION[$module .'2_QUERY']) ? $_SESSION[$module .'2_QUERY'] : '');
     }

     /**
      * return the start, prev, next, end
      */
     public static function play($module, $offset)
     {
         //given some global offset try to determine if we have this
         //in our array.
         $ids = array();
         if (!empty($_SESSION[$module.'QUERY_ARRAY'])) {
             $ids = $_SESSION[$module.'QUERY_ARRAY'];
         }
         if (empty($ids[$offset]) || empty($ids[$offset+1]) || empty($ids[$offset-1])) {
             $ids = SugarVCR::record($module, $offset);
         }
         $menu = array();
         if (!empty($ids[$offset])) {
             //return the control given this id
             $menu['PREV'] = ($offset > 1) ? $ids[$offset-1] : '';
             $menu['CURRENT'] = $ids[$offset];
             $menu['NEXT'] = !empty($ids[$offset+1]) ? $ids[$offset+1] : '';
         }
         return $menu;
     }

     public static function menu($module, $offset, $isAuditEnabled, $saveAndContinue = false)
     {
         $html_text = "";
         if ($offset < 0) {
             $offset = 0;
         }

         //this check if require in cases when you visit the edit view before visiting that modules list view.
         //you can do this easily either from home, activities or sitemap.
         $stored_vcr_query = SugarVCR::retrieve($module);

         // bug 15893 - only show VCR if called as an element in a set of records
         if (!empty($_REQUEST['record']) && !empty($stored_vcr_query) && isset($_REQUEST['offset']) && (empty($_REQUEST['isDuplicate']) || $_REQUEST['isDuplicate'] == 'false')) {
             //syncing with display offset;
             $offset ++;
             $action = (!empty($_REQUEST['action']) ? $_REQUEST['action'] : 'EditView');

             $menu = SugarVCR::play($module, $offset);

             $list_link = '';
             if ($saveAndContinue && !empty($menu['NEXT'])) {
                 $list_link = ajaxLink('index.php?action=' . $action . '&module=' . $module . '&record=' . $menu['NEXT'] . '&offset=' . ($offset + 1));
             }

             $previous_link = "";
             if (!empty($menu['PREV'])) {
                 $previous_link = ajaxLink('index.php?module=' . $module . '&action=' . $action . '&offset=' . ($offset - 1) . '&record=' . $menu['PREV']);
             }

             $next_link = "";
             if (!empty($menu['NEXT'])) {
                 $next_link = ajaxLink('index.php?module=' . $module . '&action=' . $action . '&offset=' . ($offset + 1) . '&record=' . $menu['NEXT']);
             }

             $ss = new Sugar_Smarty();
             $ss->assign('app_strings', $GLOBALS['app_strings']);
             $ss->assign('module', $module);
             $ss->assign('action', $action);
             $ss->assign('menu', $menu);
             $ss->assign('list_link', $list_link);
             $ss->assign('previous_link', $previous_link);
             $ss->assign('next_link', $next_link);

             $ss->assign('offset', $offset);
             $ss->assign('total', '');
             $ss->assign('plus', '');

             if (!empty($_SESSION[$module . 'total'])) {
                 $ss->assign('total', $_SESSION[$module . 'total']);
                 if (
                    !empty($GLOBALS['sugar_config']['disable_count_query'])
                    && (($_SESSION[$module. 'total']-1) % $GLOBALS['sugar_config']['list_max_entries_per_page'] == 0)
                ) {
                     $ss->assign('plus', '+');
                 }
             }

             if (is_file('custom/include/EditView/SugarVCR.tpl')) {
                 $html_text .= $ss->fetch('custom/include/EditView/SugarVCR.tpl');
             } else {
                 $html_text .= $ss->fetch('include/EditView/SugarVCR.tpl');
             }
         }
         return $html_text;
     }

     public static function record($module, $offset)
     {
         $GLOBALS['log']->debug('SUGARVCR is recording more records');
         $start = max(0, $offset - VCRSTART);
         $index = $start;
         $db = DBManagerFactory::getInstance();

         $result = $db->limitQuery(SugarVCR::retrieve($module), $start, ($offset+VCREND), false);
         $index++;

         $ids = array();
         while (($row = $db->fetchByAssoc($result)) != null) {
             $ids[$index] = $row['id'];
             $index++;
         }
         //now that we have the array of ids, store this in the session
         $_SESSION[$module.'QUERY_ARRAY'] = $ids;
         return $ids;
     }

     public static function recordIDs($module, $rids, $offset, $totalCount)
     {
         $index = $offset;
         $index++;
         $ids = array();
         foreach ($rids as $id) {
             $ids[$index] = $id;
             $index++;
         }
         //now that we have the array of ids, store this in the session
         $_SESSION[$module.'QUERY_ARRAY'] = $ids;
         $_SESSION[$module.'total'] = $totalCount;
     }

     public static function erase($module)
     {
         if (isset($_SESSION) && isset($_SESSION[$module. 'QUERY_ARRAY'])) {
             unset($_SESSION[$module. 'QUERY_ARRAY']);
         }
     }
 }
