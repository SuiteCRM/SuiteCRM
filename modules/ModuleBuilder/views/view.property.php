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
require_once('include/MVC/View/SugarView.php');
require_once('modules/ModuleBuilder/parsers/ParserFactory.php');

#[\AllowDynamicProperties]
class ViewProperty extends SugarView
{
    public function __construct()
    {
        $this->init();
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


    public function init($bean = null, $view_object_map = array()) // pseudo-constuctor - given a well-known name to allow subclasses to call this classes constructor
    {
        $this->editModule = (! empty($_REQUEST['view_module'])) ? $_REQUEST['view_module'] : null;
        $this->editPackage = (! empty($_REQUEST['view_package'])) ? $_REQUEST['view_package'] : null;
        $this->id = (! empty($_REQUEST['id'])) ? $_REQUEST['id'] : null;
        $this->subpanel = (! empty($_REQUEST['subpanel'])) ? $_REQUEST['subpanel'] : "";
        $this->properties = array();
        foreach ($_REQUEST as $key=>$value) {
            if (substr($key, 0, 4) == 'name') {
                $this->properties[substr($key, 5)]['name'] = $value;
            }
            if (substr($key, 0, 2) == 'id') {
                $this->properties[substr($key, 3)]['id'] = $value;
            }
            if (substr($key, 0, 5) == 'value') {
                $this->properties[substr($key, 6)]['value'] = $value;
                // tyoung - now a nasty hack to disable editing of labels which contain Smarty functions - this is envisaged to be a temporary fix to prevent admins modifying these functions then being unable to restore the original complicated value if they regret it
                if (substr($key, 6) == 'label') {
                    //#29796  , we disable the edit function for sub panel label
                    if (preg_match('/\{.*\}/', (string) $value) || !empty($this->subpanel)) {
                        $this->properties[substr($key, 6)]['hidden'] = 1;
                    }
                }
            }
            if (substr($key, 0, 5) == 'title') {
                $this->properties[substr($key, 6)]['title'] = $value;
            }
        }
    }

    public function display()
    {
        global $mod_strings;
        $ajax = new AjaxCompose();
        $smarty = new Sugar_Smarty();
        if (isset($_REQUEST['MB']) && $_REQUEST['MB'] == "1") {
            $smarty->assign("MB", $_REQUEST['MB']);
            $smarty->assign("view_package", $_REQUEST['view_package']);
        }

        $selected_lang = (!empty($_REQUEST['selected_lang'])?$_REQUEST['selected_lang']:$_SESSION['authenticated_user_language']);
        if (empty($selected_lang)) {
            $selected_lang = $GLOBALS['sugar_config']['default_language'];
        }
        $smarty->assign('available_languages', get_languages());
        $smarty->assign('selected_lang', $selected_lang);

        ksort($this->properties);

        $smarty->assign("properties", $this->properties);
//        $smarty->assign("id",$this->id);

        $smarty->assign("mod_strings", $mod_strings);
        $smarty->assign('APP', $GLOBALS['app_strings']);
        $smarty->assign("view_module", $this->editModule);
        $smarty->assign("subpanel", $this->subpanel);
        if (isset($this->editPackage)) {
            $smarty->assign("view_package", $this->editPackage);
        }

        $ajax->addSection('east', translate('LBL_SECTION_PROPERTIES', 'ModuleBuilder'), $smarty->fetch('modules/ModuleBuilder/tpls/editProperty.tpl'));
        echo $ajax->getJavascript();
    }
}
