<?php
/**
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation.
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once('include/MVC/View/SugarView.php');
        
#[\AllowDynamicProperties]
class stic_Import_ValidationViewError extends SugarView
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
        $this->ss->assign("IMPORT_MODULE", $_REQUEST['import_module']);
        $this->ss->assign("ACTION", 'Step1');
        $this->ss->assign("MESSAGE", $_REQUEST['message']);
        $this->ss->assign("SOURCE", "");
        if (isset($_REQUEST['source'])) {
            $this->ss->assign("SOURCE", $_REQUEST['source']);
        }

        $this->ss->display('modules/stic_Import_Validation/tpls/error.tpl');
    }
}
