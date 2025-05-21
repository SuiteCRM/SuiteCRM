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

require_once('modules/stic_Import_Validation/views/ImportView.php');

#[\AllowDynamicProperties]
class stic_Import_ValidationViewUndo extends stic_Import_ValidationView
{
    protected $pageTitleKey = 'LBL_UNDO_LAST_IMPORT';
    
    /**
     * @see SugarView::display()
     */
    public function display()
    {
        global $mod_strings, $current_user, $current_language;
        
        $this->ss->assign("IMPORT_MODULE", $_REQUEST['import_module']);
        // lookup this module's $mod_strings to get the correct module name
        $old_mod_strings = $mod_strings;
        $module_mod_strings =
            return_module_language($current_language, $_REQUEST['import_module']);
        $this->ss->assign("MODULENAME", $module_mod_strings['LBL_MODULE_NAME']);
        $this->ss->assign("MODULE_TITLE", $this->getModuleTitle(false), ENT_NOQUOTES);
        // reset old ones afterwards
        $mod_strings = $old_mod_strings;
        
        $last_import = BeanFactory::newBean('Import_2');
        $this->ss->assign('UNDO_SUCCESS', $last_import->undo($_REQUEST['import_module']));
        $this->ss->assign("JAVASCRIPT", $this->_getJS());
        $content = $this->ss->fetch('modules/stic_Import_Validation/tpls/undo.tpl');
        $this->ss->assign("CONTENT", $content);
        $this->ss->display('modules/stic_Import_Validation/tpls/wizardWrapper.tpl');
    }
    
    /**
     * Returns JS used in this view
     */
    private function _getJS()
    {
        return <<<EOJAVASCRIPT

document.getElementById('finished').onclick = function(){
    document.getElementById('importundo').module.value = document.getElementById('importundo').import_module.value;
    document.getElementById('importundo').action.value = 'index';
}
EOJAVASCRIPT;
    }
}
