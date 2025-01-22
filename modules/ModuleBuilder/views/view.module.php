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
class ViewModule extends SugarView
{
    public $mbModule;

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
        $translated_type = [];
        global $mod_strings, $current_language;
        $smarty = new Sugar_Smarty();

        $mod_strings = return_module_language($current_language, 'ModuleBuilder');

        require_once('modules/ModuleBuilder/MB/ModuleBuilder.php');
        $mb = new ModuleBuilder();
        $mb->getPackage($_REQUEST['view_package']);
        $package = $mb->packages[$_REQUEST['view_package']];
        $module_name = (!empty($_REQUEST['view_module']))?$_REQUEST['view_module']:'';
        $package->getModule($module_name);
        $this->mbModule = $package->modules[$module_name];
        $this->loadPackageHelp($module_name);

        // set up the list of either available types for a new module, or implemented types for an existing one
        $types = (empty($module_name)) ? MBModule::getTypes() : $this->mbModule->mbvardefs->templates ;

        foreach ($types as $type=>$definition) {
            $translated_type[$type]=translate('LBL_TYPE_'.strtoupper($type), 'ModuleBuilder');
        }
        natcasesort($translated_type);
        $smarty->assign('types', $translated_type);

        $smarty->assign('package', $package);
        $smarty->assign('module', $this->mbModule);
        $smarty->assign('mod_strings', $mod_strings);

        $ajax = new AjaxCompose();
        $ajax->addCrumb($GLOBALS['mod_strings']['LBL_MODULEBUILDER'] ?? '', 'ModuleBuilder.main("mb")');
        $ajax->addCrumb(' '. $package->name, 'ModuleBuilder.getContent("module=ModuleBuilder&action=package&package='.$package->name.'")');
        if (empty($module_name)) {
            $module_name = translate('LBL_NEW_MODULE', 'ModuleBuilder');
        }
        $ajax->addCrumb($module_name, '');
        $html=$smarty->fetch('modules/ModuleBuilder/tpls/MBModule/module.tpl');
        if (!empty($_REQUEST['action']) && $_REQUEST['action']=='SaveModule') {
            $html .="<script>ModuleBuilder.treeRefresh('ModuleBuilder')</script>";
        }
        $ajax->addSection('center', translate('LBL_SECTION_MODULE', 'ModuleBuilder'), $html);

        echo $ajax->getJavascript();
    }

    public function loadPackageHelp(
        $name
        ) {
        $this->mbModule->help['default'] = (empty($name))?'create':'modify';
        $this->mbModule->help['group'] = 'module';
    }
}
