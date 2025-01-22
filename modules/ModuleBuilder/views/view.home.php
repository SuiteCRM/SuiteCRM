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
class ViewHome extends SugarView
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
        global $current_user;
        global $mod_strings;
        $smarty = new Sugar_Smarty();
        $smarty->assign('title', $mod_strings['LBL_DEVELOPER_TOOLS']);
        $smarty->assign('question', $mod_strings['LBL_QUESTION_EDITOR']);
        $smarty->assign('defaultHelp', 'mainHelp');
        $this->generateHomeButtons();
        $smarty->assign('buttons', $this->buttons);
        $assistant=array('group'=>'main', 'key'=>'welcome');
        $smarty->assign('assistant', $assistant);
        //initialize Assistant's display property.
        $userPref = $current_user->getPreference('mb_assist', 'Assistant');
        if (!$userPref) {
            $userPref="na";
        }
        $smarty->assign('userPref', $userPref);
        $ajax = new AjaxCompose();
        $ajax->addSection('center', $mod_strings['LBL_HOME'], $smarty->fetch('modules/ModuleBuilder/tpls/wizard.tpl'));
        echo $ajax->getJavascript();
    }


    public function generateHomeButtons()
    {
        global $current_user;
        if (displayStudioForCurrentUser() == true) {
            //$this->buttons['Application'] = array ('action' => '', 'imageTitle' => 'Application', 'size' => '128', 'help'=>'appBtn');
            $this->buttons[$GLOBALS['mod_strings']['LBL_STUDIO']] = array('action' => 'javascript:ModuleBuilder.main("studio")', 'imageTitle' => 'Studio', 'size' => '128', 'help'=>'studioBtn');
        }
        if (is_admin($current_user)) {
            $this->buttons[$GLOBALS['mod_strings']['LBL_MODULEBUILDER']] = array('action' => 'javascript:ModuleBuilder.main("mb")', 'imageTitle' => 'ModuleBuilder', 'size' => '128', 'help'=>'mbBtn');
        }
        $this->buttons[$GLOBALS['mod_strings']['LBL_DROPDOWNEDITOR']] = array('action' => 'javascript:ModuleBuilder.main("dropdowns")', 'imageTitle' => $GLOBALS['mod_strings']['LBL_HOME_EDIT_DROPDOWNS'], 'imageName' => 'DropDownEditor', 'size' => '128', 'help'=>'dropDownEditorBtn');
    }
}
