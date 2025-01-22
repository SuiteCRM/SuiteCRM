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


#[\AllowDynamicProperties]
class ViewMain extends SugarView
{
    public function __construct()
    {
        $this->options['show_footer'] = true;
        parent::__construct();
    }




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
        global $app_strings, $current_user, $mod_strings, $theme;

        $smarty = new Sugar_Smarty();
        $type = (!empty($_REQUEST['type']))?$_REQUEST['type']:'main';
        $mbt = false;
        $admin = false;
        $mb = strtolower($type);
        $smarty->assign('TYPE', $type);
        $smarty->assign('app_strings', $app_strings);
        $smarty->assign('mod', $mod_strings);
        //Replaced by javascript function "setMode"
        switch ($type) {
            case 'studio':
                //$smarty->assign('ONLOAD','ModuleBuilder.getContent("module=ModuleBuilder&action=wizard")');
                require_once('modules/ModuleBuilder/Module/StudioTree.php');
                $mbt = new StudioTree();
                break;
            case 'mb':
                //$smarty->assign('ONLOAD','ModuleBuilder.getContent("module=ModuleBuilder&action=package&package=")');
                require_once('modules/ModuleBuilder/MB/MBPackageTree.php');
                $mbt = new MBPackageTree();
                break;
            case 'dropdowns':
               // $admin = is_admin($current_user);
                require_once('modules/ModuleBuilder/Module/DropDownTree.php');
                $mbt = new DropDownTree();
                break;
            default:
                //$smarty->assign('ONLOAD','ModuleBuilder.getContent("module=ModuleBuilder&action=home")');
                require_once('modules/ModuleBuilder/Module/MainTree.php');
                $mbt = new MainTree();
        }
        $smarty->assign('TEST_STUDIO', displayStudioForCurrentUser());
        $smarty->assign('ADMIN', is_admin($current_user));
        $smarty->display('modules/ModuleBuilder/tpls/includes.tpl');
        if ($mbt) {
            $smarty->assign('TREE', $mbt->fetch());
            $smarty->assign('TREElabel', $mbt->getName());
        }
        $userPref = $current_user->getPreference('mb_assist', 'Assistant');
        if (!$userPref) {
            $userPref="na";
        }
        $smarty->assign('userPref', $userPref);

        ///////////////////////////////////
        require_once('include/SugarTinyMCE.php');
        $tiny = new SugarTinyMCE();
        $tiny->defaultConfig['width']=300;
        $tiny->defaultConfig['height']=300;
        $tiny->buttonConfig = "code,separator,bold,italic,underline,strikethrough,separator,justifyleft,justifycenter,justifyright,
	                         justifyfull,separator,forecolor,backcolor,
	                         ";
        $tiny->buttonConfig2 = "pastetext,pasteword,fontselect,fontsizeselect,";
        $tiny->buttonConfig3 = "";
        $ed = $tiny->getInstance();
        $smarty->assign("tiny", $ed);

        $smarty->display('modules/ModuleBuilder/tpls/index.tpl');
    }
}
