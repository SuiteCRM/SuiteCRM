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

require_once('modules/ModuleBuilder/MB/AjaxCompose.php');

#[\AllowDynamicProperties]
class ViewDropdown extends SugarView
{
    /**
     * @see SugarView::_getModuleTitleParams()
     */
    protected function _getModuleTitleParams($browserTitle = false)
    {
        global $mod_strings;
        
        return array(
           translate('LBL_MODULE_NAME', 'Administration'),
           ModuleBuilderController::getModuleTitle(),
           );
    }

    public function display()
    {
        $ajax = new AjaxCompose();
        $smarty = $this->generateSmarty();
        
        if (isset($_REQUEST['refreshTree'])) {
            require_once('modules/ModuleBuilder/Module/DropDownTree.php');
            $mbt = new DropDownTree();
            $ajax->addSection('west', $mbt->getName(), $mbt->fetchNodes());
            $smarty->assign('refreshTree', true);
        }

        global $mod_strings;

        $smarty->assign('deleteImage', SugarThemeRegistry::current()->getImage('delete_inline', '', null, null, '.gif', $mod_strings['LBL_MB_DELETE']));
        $smarty->assign('editImage', SugarThemeRegistry::current()->getImage('edit_inline', '', null, null, '.gif', $mod_strings['LBL_EDIT'], true, '.gif'));
        $smarty->assign('action', 'savedropdown');
        $body = $smarty->fetch('modules/ModuleBuilder/tpls/MBModule/dropdown.tpl');
        $ajax->addSection('east2', $mod_strings['LBL_SECTION_DROPDOWNED'], $body);
        echo $ajax->getJavascript();
    }
    
    public function generateSmarty()
    {
        $module = null;
        //get the selected language
        $selected_lang = (!empty($_REQUEST['dropdown_lang'])?$_REQUEST['dropdown_lang']:$_SESSION['authenticated_user_language']);
        $vardef = array();
        $package_name = 'studio';
        $package_strings = array();
        $new =false;
        $my_list_strings = return_app_list_strings_language($selected_lang) ;
        //		$my_list_strings = $GLOBALS['app_list_strings'];

        $smarty = new Sugar_Smarty();
              
        //if we are using ModuleBuilder then process the following
        if (!empty($_REQUEST['view_package']) && $_REQUEST['view_package'] != 'studio') {
            require_once('modules/ModuleBuilder/MB/ModuleBuilder.php');
            $mb = new ModuleBuilder();
            $module = $mb->getPackageModule($_REQUEST['view_package'], $_REQUEST['view_module']);
            $package = $mb->packages[$_REQUEST['view_package']];
            $package_name = $package->name;
            $module->getVardefs();
            if (empty($_REQUEST['dropdown_name']) && !empty($_REQUEST['field'])) {
                $new = true;
                $_REQUEST['dropdown_name'] = $_REQUEST['field']. '_list';
            }
            
            $vardef = (!empty($module->mbvardefs->fields[$_REQUEST['dropdown_name']]))? $module->mbvardefs->fields[$_REQUEST['dropdown_name']]: array();
            $module->mblanguage->generateAppStrings(false) ;
            $my_list_strings = array_merge($my_list_strings, $module->mblanguage->appListStrings[$selected_lang.'.lang.php']);
            $smarty->assign('module_name', $module->name);
        }

        $module_name = !empty($module->name) ?  $module->name : '';
        $module_name = (empty($module_name) && !empty($_REQUEST['view_module'])) ?  $_REQUEST['view_module'] : $module_name;

        foreach ($my_list_strings as $key=>$value) {
            if (!is_array($value)) {
                unset($my_list_strings[$key]);
            }
        }
        
        $dropdowns = array_keys($my_list_strings);
        asort($dropdowns);
        $keys = array_keys($dropdowns);
        $first_string = $my_list_strings[$dropdowns[$keys[0]]];

        $name = '';
        $selected_dropdown = array();

        $json = getJSONobj();

        if (!empty($_REQUEST['dropdown_name']) && !$new) {
            $name = $_REQUEST['dropdown_name'];
            
            // handle the case where we've saved a dropdown in one language, and now attempt to edit it for another language. The $name exists, but $my_list_strings[$name] doesn't
            // for now, we just treat it as if it was new. A better approach might be to use the first language version as a template for future languages
            if (!isset($my_list_strings[$name])) {
                $my_list_strings[$name] = array() ;
            }
 
            $selected_dropdown = (!empty($vardef['options']) && !empty($my_list_strings[$vardef['options']])) ? $my_list_strings[$vardef['options']] : $my_list_strings[$name];
            $smarty->assign('ul_list', 'list = '.$json->encode(array_keys($selected_dropdown)));
            $smarty->assign('dropdown_name', (!empty($vardef['options']) ? $vardef['options'] : $_REQUEST['dropdown_name']));
            $smarty->assign('name', $_REQUEST['dropdown_name']);
            $smarty->assign('options', $selected_dropdown);
        } else {
            $smarty->assign('ul_list', 'list = {}');
            //we should try to find a name for this dropdown based on the field name.
            $pre_pop_name = '';
            if (!empty($_REQUEST['field'])) {
                $pre_pop_name = $_REQUEST['field'];
            }
            //ensure this dropdown name does not already exist
            $use_name = $pre_pop_name.'_list';
            for ($i = 0; $i < 100; $i++) {
                if (empty($my_list_strings[$use_name])) {
                    break;
                } else {
                    $use_name = $pre_pop_name.'_'.$i;
                }
            }
            $smarty->assign('prepopulated_name', $use_name);
        }

        $smarty->assign('module_name', $module_name);
        $smarty->assign('APP', $GLOBALS['app_strings']);
        $smarty->assign('MOD', $GLOBALS['mod_strings']);
        $smarty->assign('selected_lang', $selected_lang);
        $smarty->assign('available_languages', get_languages());
        $smarty->assign('package_name', $package_name);
        return $smarty;
    }
}
