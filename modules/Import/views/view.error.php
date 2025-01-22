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

/**

 * Description: view handler for error page of the import process
 * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
 * All Rights Reserved.
 */

require_once('include/MVC/View/SugarView.php');
        
#[\AllowDynamicProperties]
class ImportViewError extends SugarView
{
    /**
     * @see SugarView::getMenu()
     */
    public function getMenu(
        $module = null
        ) {
        global $mod_strings, $current_language;
        
        if (empty($module)) {
            $module = $_REQUEST['import_module'];
        }
        
        $old_mod_strings = $mod_strings;
        $mod_strings = return_module_language($current_language, $module);
        $returnMenu = parent::getMenu($module);
        $mod_strings = $old_mod_strings;
        
        return $returnMenu;
    }
    
    /**
     * @see SugarView::_getModuleTab()
     */
    protected function _getModuleTab()
    {
        global $app_list_strings, $moduleTabMap;
        
        // Need to figure out what tab this module belongs to, most modules have their own tabs, but there are exceptions.
        if (!empty($_REQUEST['module_tab'])) {
            return $_REQUEST['module_tab'];
        } elseif (isset($moduleTabMap[$_REQUEST['import_module']])) {
            return $moduleTabMap[$_REQUEST['import_module']];
        }
        // Default anonymous pages to be under Home
        elseif (!isset($app_list_strings['moduleList'][$_REQUEST['import_module']])) {
            return 'Home';
        } else {
            return $_REQUEST['import_module'];
        }
    }
    
    /**
     * @see SugarView::display()
     */
    public function display()
    {
        $module = $_REQUEST['import_module'] ?? '';
        if (!empty($module) && !isAllowedModuleName($module)) {
            throw new InvalidArgumentException('Invalid target_module');
        }

        $source = $_REQUEST['source'] ?? '';
        $result = preg_match("/^[\w\-\_\.\:]+$/", $source);
        if (!empty($source) && empty($result)) {
            throw new InvalidArgumentException('Invalid source');
        }

        $this->ss->assign("IMPORT_MODULE", $module);
        $this->ss->assign("ACTION", 'Step1');
        $this->ss->assign("MESSAGE", $_REQUEST['message'] ?? '');
        $this->ss->assign("SOURCE", "");
        if (isset($_REQUEST['source'])) {
            $this->ss->assign("SOURCE", $source);
        }

        $this->ss->display('modules/Import/tpls/error.tpl');
    }
}
