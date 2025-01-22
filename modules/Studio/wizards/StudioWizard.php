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





#[\AllowDynamicProperties]
class StudioWizard
{
    public $tplfile = 'modules/Studio/wizards/tpls/wizard.tpl';
    public $wizard = 'StudioWizard';
    public $status = '';
    public $assign = array();
    
    public function welcome()
    {
        return $GLOBALS['mod_strings']['LBL_SW_WELCOME'];
    }

    public function options()
    {
        $options = array('SelectModuleWizard'=>$GLOBALS['mod_strings']['LBL_SW_EDIT_MODULE'],
                         'EditDropDownWizard'=>$GLOBALS['mod_strings']['LBL_SW_EDIT_DROPDOWNS'],
                         'RenameTabs'=>$GLOBALS['mod_strings']['LBL_SW_RENAME_TABS'],
                         'ConfigureTabs'=>$GLOBALS['mod_strings']['LBL_SW_EDIT_TABS'],
                         'ConfigureGroupTabs'=>$GLOBALS['mod_strings']['LBL_SW_EDIT_GROUPTABS'],
                         'Portal'=>$GLOBALS['mod_strings']['LBL_SW_EDIT_PORTAL'],
                         'RepairCustomFields'=>$GLOBALS['mod_strings']['LBL_SW_REPAIR_CUSTOMFIELDS'],
                         'MigrateCustomFields'=>$GLOBALS['mod_strings']['LBL_SW_MIGRATE_CUSTOMFIELDS'],

        
        );
        return $options;
    }
    public function back()
    {
    }
    public function process($option)
    {
        switch ($option) {
            case 'SelectModuleWizard':
                require_once('modules/Studio/wizards/'. $option . '.php');
                $newWiz = new $option();
                $newWiz->display();
                break;
            case 'EditDropDownWizard':
                require_once('modules/Studio/wizards/'. $option . '.php');
                $newWiz = new $option();
                $newWiz->display();
                break;
            case 'RenameTabs':
                require_once('modules/Studio/wizards/RenameModules.php');
                $newWiz = new RenameModules();
                $newWiz->process();
                break;
            case 'ConfigureTabs':
                header('Location: index.php?module=Administration&action=ConfigureTabs');
                sugar_cleanup(true);
                // no break
            case 'ConfigureGroupTabs':
                require_once('modules/Studio/TabGroups/EditViewTabs.php');
                break;
            case 'Workflow':
                header('Location: index.php?module=WorkFlow&action=ListView');
                sugar_cleanup(true);
                // no break
            case 'RepairCustomFields':
                header('Location: index.php?module=Administration&action=UpgradeFields');
                sugar_cleanup(true);
                // no break
            case 'MigrateCustomFields':
                header('LOCATION: index.php?module=Administration&action=Development');
                sugar_cleanup(true);
                // no break
            case 'SugarPortal':
                header('LOCATION: index.php?module=Studio&action=Portal');
                sugar_cleanup(true);
                // no break
            case 'Classic':
                header('Location: index.php?module=DynamicLayout&action=index');
                sugar_cleanup(true);
                // no break
            default:
                $this->display();
        }
    }
    public function display($error = '')
    {
        echo $this->fetch($error);
    }
    
    public function fetch($error = '')
    {
        global $mod_strings;
        echo getClassicModuleTitle('StudioWizard', array($mod_strings['LBL_MODULE_TITLE']), false);
        $sugar_smarty = new Sugar_Smarty();
        $sugar_smarty->assign('welcome', $this->welcome());
        $sugar_smarty->assign('options', $this->options());
        $sugar_smarty->assign('MOD', $GLOBALS['mod_strings']);
        $sugar_smarty->assign('option', (!empty($_REQUEST['option'])?$_REQUEST['option']:''));
        $sugar_smarty->assign('wizard', $this->wizard);
        $sugar_smarty->assign('error', $error);
        $sugar_smarty->assign('status', $this->status);
        $sugar_smarty->assign('mod', $mod_strings);
        foreach ($this->assign as $name=>$value) {
            $sugar_smarty->assign($name, $value);
        }
        return  $sugar_smarty->fetch($this->tplfile);
    }
}
