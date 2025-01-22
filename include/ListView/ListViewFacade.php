<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}
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


/*
 * Created on Sep 10, 2007
 *
 * To change the template for this generated file go to
 * Window - Preferences - PHPeclipse - PHP - Code Templates
 */
 require_once('include/ListView/ListViewSmarty.php');


 /**
  * A Facade to ListView and ListViewSmarty
  */
 class ListViewFacade
 {
     public $focus = null;
     public $module = '';
     public $type = 0;

     public $lv;

     //ListView fields
     public $template;
     public $title;
     public $where = '';
     public $params = array();
     public $offset = 0;
     public $limit = -1;
     public $filter_fields = array();
     public $id_field = 'id';
     public $prefix = '';
     public $mod_strings = array();

     /**
      * Constructor
      * @param $focus - the bean
      * @param $module - the module name
      * @param - 0 = decide for me, 1 = ListView.html, 2 = ListViewSmarty
      */
     public function __construct($focus, $module, $type = 0)
     {
         $this->focus = $focus;
         $this->module = $module;
         $this->type = $type;
         $this->build();
     }

     /**
      * Retrieves display columns on list view of specified module
      *
      * @param string $module
      * @param array $request
      * @return array
      */
     public static function getDisplayColumns($module, $request = [])
     {
         $metadataFile = null;
         $foundViewDefs = false;
         if (file_exists('custom/modules/' . $module. '/metadata/listviewdefs.php')) {
             $metadataFile = 'custom/modules/' .  $module . '/metadata/listviewdefs.php';
             $foundViewDefs = true;
         } else {
             if (file_exists('custom/modules/'. $module.'/metadata/metafiles.php')) {
                 require_once('custom/modules/'. $module.'/metadata/metafiles.php');
                 if (!empty($metafiles[$module]['listviewdefs'])) {
                     $metadataFile = $metafiles[$module]['listviewdefs'];
                     $foundViewDefs = true;
                 }
             } elseif (file_exists('modules/'. $module.'/metadata/metafiles.php')) {
                 require_once('modules/'. $module.'/metadata/metafiles.php');
                 if (!empty($metafiles[$module]['listviewdefs'])) {
                     $metadataFile = $metafiles[$module]['listviewdefs'];
                     $foundViewDefs = true;
                 }
             }
         }
         if (!$foundViewDefs && file_exists('modules/'. $module.'/metadata/listviewdefs.php')) {
             $metadataFile = 'modules/'. $module.'/metadata/listviewdefs.php';
         }
         
         if ($metadataFile) {
             if (!file_exists($metadataFile)) {
                 throw new Exception("Metadata file '$metadataFile' not found for module '$module'.");
             }
             require_once($metadataFile);
         }

         $displayColumns = array();
         if (!empty($listViewDefs)) {
             if (!empty($request['displayColumns'])) {
                 foreach (explode('|', $_REQUEST['displayColumns']) as $num => $col) {
                     if (!empty($listViewDefs[$module][$col])) {
                         $displayColumns[$col] = $listViewDefs[$module][$col];
                     }
                 }
             } else {
                 foreach ($listViewDefs[$module] as $col => $params) {
                     if (!empty($params['default']) && $params['default']) {
                         $displayColumns[$col] = $params;
                     }
                 }
             }
         } else {
             throw new Exception("List view definition is not found for module '$module'");
         }
         return $displayColumns;
     }

     public function build()
     {
         //we will assume that if the ListView.html file exists we will want to use that one
         if (file_exists('modules/'.$this->module.'/ListView.html')) {
             $this->type = 1;
             $this->lv = new ListView();
             $this->template = 'modules/'.$this->module.'/ListView.html';
         } else {
             $this->lv = new ListViewSmarty();
             $this->lv->displayColumns = self::getDisplayColumns($this->module, $_REQUEST);
             $this->type = 2;
             $this->template = 'include/ListView/ListViewGeneric.tpl';
         }
     }

     public function setup($template = '', $where = '', $params = array(), $mod_strings = array(), $offset = 0, $limit = -1, $orderBy = '', $prefix = '', $filter_fields = array(), $id_field = 'id')
     {
         if (!empty($template)) {
             $this->template = $template;
         }

         $this->mod_strings = $mod_strings;

         if ($this->type == 1) {
             $this->lv->initNewXTemplate($this->template, $this->mod_strings);
             $this->prefix = $prefix;
             $this->lv->setQuery($where, $limit, $orderBy, $prefix);
             $this->lv->show_select_menu = false;
             $this->lv->show_export_button = false;
             $this->lv->show_delete_button = false;
             $this->lv->show_mass_update = false;
             $this->lv->show_mass_update_form = false;
         } else {
             $this->lv->export = false;
             $this->lv->delete = false;
             $this->lv->select = false;
             $this->lv->mailMerge = false;
             $this->lv->multiSelect = false;
             $this->lv->setup($this->focus, $this->template, $where, $params, $offset, $limit, $filter_fields, $id_field);
         }
     }

     public function display($title = '', $section = 'main', $return = false)
     {
         if ($this->type == 1) {
             ob_start();
             $this->lv->setHeaderTitle($title);
             $this->lv->processListView($this->focus, $section, $this->prefix);
             $output = ob_get_contents();
             ob_end_clean();
         } else {
             $output = get_form_header($title, '', false) . $this->lv->display();
         }
         if ($return) {
             return $output;
         } else {
             echo $output;
         }
     }

     public function setTitle($title = '')
     {
         $this->title = $title;
     }
 }
