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
require_once('modules/ModuleBuilder/MB/ModuleBuilder.php');
#[\AllowDynamicProperties]
class Viewpackage extends SugarView
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
        $smarty = new Sugar_Smarty();
        $mb = new ModuleBuilder();
        //if (!empty($_REQUEST['package'])) {
        if (empty($_REQUEST['package']) && empty($_REQUEST['new'])) {
            $this->generatePackageButtons($mb->getPackageList());

            $smarty->assign('buttons', $this->buttons);
            $smarty->assign('title', $GLOBALS['mod_strings']['LBL_MODULEBUILDER']);
            $smarty->assign("question", $GLOBALS['mod_strings']['LBL_QUESTION_PACKAGE']);
            $smarty->assign("defaultHelp", "mbHelp");

            $ajax = new AjaxCompose();
            $ajax->addCrumb($GLOBALS['mod_strings']['LBL_MODULEBUILDER'], 'ModuleBuilder.getContent("module=ModuleBuilder&action=package")');
            $ajax->addCrumb($GLOBALS['mod_strings']['LBL_PACKAGE_LIST'], '');
            $ajax->addSection('center', $GLOBALS['mod_strings']['LBL_PACKAGE_LIST'], $smarty->fetch('modules/ModuleBuilder/tpls/wizard.tpl'));
            echo $ajax->getJavascript();
        } else {
            $name = (!empty($_REQUEST['package']))?$_REQUEST['package']:'';
            $mb->getPackage($name);
            
            require_once('modules/ModuleBuilder/MB/MBPackageTree.php') ;
            $mbt = new MBPackageTree();
            $nodes = $mbt->fetchNodes();
            
            $package_labels = array();
            if (!empty($nodes['tree_data']['nodes'])) {
                foreach ($nodes['tree_data']['nodes'] as $entry) {
                    if (!empty($entry['data']['label']) && $name != $entry['data']['label']) {
                        $package_labels[] = strtoupper($entry['data']['label']);
                    }
                }
            }
            
            $json = getJSONobj();
            $smarty->assign('package_labels', $json->encode($package_labels));
            
            $this->package =& $mb->packages[$name];
            $this->loadModuleTypes();
            $this->loadPackageHelp($name);
            $this->package->date_modified = $GLOBALS['timedate']->to_display_date_time($this->package->date_modified);
            $smarty->assign('package', $this->package);
            $smarty->assign('mod_strings', $mod_strings);
            $smarty->assign('package_already_deployed', 'false');
            foreach ($this->package->modules as $a_module) {
                if (in_array($a_module->key_name, $GLOBALS['moduleList'])) {
                    $smarty->assign('package_already_deployed', 'true');
                    break;
                }
            }

            $ajax = new AjaxCompose();
            $ajax->addCrumb($GLOBALS['mod_strings']['LBL_MODULEBUILDER'], 'ModuleBuilder.getContent("module=ModuleBuilder&action=package")');
            if (empty($name)) {
                $name = $mod_strings['LBL_NEW_PACKAGE'];
            }
            $ajax->addCrumb($name, '');
            $html=$smarty->fetch('modules/ModuleBuilder/tpls/MBPackage/package.tpl');
            if (!empty($_REQUEST['action']) && $_REQUEST['action']=='SavePackage') {
                $html.="<script>ModuleBuilder.treeRefresh('ModuleBuilder')</script>";
            }
            $ajax->addSection('center', translate('LBL_SECTION_PACKAGE', 'ModuleBuilder'), $html);
            echo $ajax->getJavascript();
        }
    }

    public function loadModuleTypes()
    {
        $this->package->moduleTypes = array();
        $this->package->loadModules();
        foreach (array_keys($this->package->modules) as $name) {
            foreach ($this->package->modules[$name]->config['templates'] as $template=>$var) {
                $this->package->moduleTypes[$name] = $template;
            }
        }
    }
    public function loadPackageHelp(
        $name
        ) {
        $this->package->help['default'] = (empty($name))?'create':'modify';
        $this->package->help['group'] = 'package';
    }

    public function generatePackageButtons(
        $packages
        ) {
        global $mod_strings;
        $this->buttons[$mod_strings['LBL_NEW_PACKAGE']] = array(
                                        'action' => "module=ModuleBuilder&action=package&new=1",
                                        'help' => 'newPackage',
                                        'linkId' => 'newPackageLink',
                                        'icon' => 'new-package'
                                        );
        foreach ($packages as $package) {
            $this->buttons[$package] = array(
                                        'action' =>"module=ModuleBuilder&action=package&package={$package}",
                                        'icon' => 'existing-package'
                                        );
        }
    }
}
