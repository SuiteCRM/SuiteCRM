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
 * Created on Aug 6, 2007
 *
 * To change the template for this generated file go to
 * Window - Preferences - PHPeclipse - PHP - Code Templates
 */

require_once('modules/ModuleBuilder/MB/AjaxCompose.php');

#[\AllowDynamicProperties]
class ViewModulelabels extends SugarView
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
        global $mod_strings;
        $bak_mod_strings=$mod_strings;
        $smarty = new Sugar_Smarty();
        $smarty->assign('mod_strings', $mod_strings);
        $package_name = $_REQUEST['view_package'];
        $module_name = $_REQUEST['view_module'];

        require_once('modules/ModuleBuilder/MB/ModuleBuilder.php');
        $mb = new ModuleBuilder();
        $mb->getPackage($_REQUEST['view_package']);
        $package = $mb->packages[$_REQUEST['view_package']];
        $package->getModule($module_name);
        $mbModule = $package->modules[$module_name];
        $selected_lang = (!empty($_REQUEST['selected_lang'])?$_REQUEST['selected_lang']:$_SESSION['authenticated_user_language']);
        if (empty($selected_lang)) {
            $selected_lang = $GLOBALS['sugar_config']['default_language'];
        }
        //need to change the following to interface with MBlanguage.

        $smarty->assign('MOD_LABELS', $mbModule->getModStrings($selected_lang));
        $smarty->assign('APP', $GLOBALS['app_strings']);
        $smarty->assign('selected_lang', $selected_lang);
        $smarty->assign('view_package', $package_name);
        $smarty->assign('view_module', $module_name);
        $smarty->assign('mb', '1');
        $smarty->assign('available_languages', get_languages());
        ///////////////////////////////////////////////////////////////////
        ////ASSISTANT
        $smarty->assign('assistant', array('group'=>'module', 'key'=>'labels'));
        /////////////////////////////////////////////////////////////////
        ////ASSISTANT

        $ajax = new AjaxCompose();
        $ajax->addCrumb($bak_mod_strings['LBL_MODULEBUILDER'], 'ModuleBuilder.main("mb")');
        $ajax->addCrumb($package_name, 'ModuleBuilder.getContent("module=ModuleBuilder&action=package&package='.$package->name. '")');
        $ajax->addCrumb($module_name, 'ModuleBuilder.getContent("module=ModuleBuilder&action=module&view_package='.$package->name.'&view_module='. $module_name . '")');
        $ajax->addCrumb($bak_mod_strings['LBL_LABELS'], '');
        $ajax->addSection('center', $bak_mod_strings['LBL_LABELS'], $smarty->fetch('modules/ModuleBuilder/tpls/labels.tpl'));
        echo $ajax->getJavascript();
    }
}
