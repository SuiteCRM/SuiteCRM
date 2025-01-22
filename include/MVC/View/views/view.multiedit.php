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

/*
 * Created on Apr 13, 2007
 *
 * To change the template for this generated file go to
 * Window - Preferences - PHPeclipse - PHP - Code Templates
 */
require_once('include/EditView/EditView2.php');
 class ViewMultiedit extends SugarView
 {
     public $type ='edit';

     public function __construct()
     {
         parent::__construct();
     }

     public function display()
     {
         global $beanList, $beanFiles;
         if ($this->action == 'AjaxFormSave') {
             echo "<a href='index.php?action=DetailView&module=".$this->module."&record=".$this->bean->id."'>".$this->bean->id."</a>";
         } else {
             if (!empty($_REQUEST['modules'])) {
                 $js_array = 'Array(';

                 $count = count($_REQUEST['modules']);
                 $index = 1;
                 foreach ($_REQUEST['modules'] as $module) {
                     $js_array .= "'form_".$module."'";
                     if ($index < $count) {
                         $js_array .= ',';
                     }
                     $index++;
                 }
                 //$js_array = "Array(".implode(",", $js_array). ")";
                 $js_array .= ');';
                 echo "<script language='javascript'>var ajaxFormArray = new ".$js_array."</script>";
                 if ($count > 1) {
                     echo '<input type="button" class="button" value="Save All" id=\'ajaxsaveall\' onclick="return saveForms(\'Saving...\', \'Save Complete\');"/>';
                 }
                 foreach ($_REQUEST['modules'] as $module) {
                     $bean = $beanList[$module];
                     require_once($beanFiles[$bean]);
                     $GLOBALS['mod_strings'] = return_module_language($GLOBALS['current_language'], $module);
                     $ev = new EditView($module);
                     $ev->process();
                     echo "<div id='multiedit_form_".$module."'>";
                     echo $ev->display(true, true);
                     echo "</div>";
                 }
             }
         }
     }
 }
