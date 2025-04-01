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

require_once('modules/Administration/Forms.php');
require_once('modules/Configurator/Configurator.php');
require_once('include/MVC/View/SugarView.php');
        
#[\AllowDynamicProperties]
class AdministrationViewThemeConfigSettings extends SugarView
{
    /**
     * @see SugarView::_getModuleTitleParams()
     */
    protected function _getModuleTitleParams($browserTitle = false)
    {
        global $mod_strings;
        
        return array(
           "<a href='index.php?module=Administration&action=index'>".$mod_strings['LBL_MODULE_NAME']."</a>",
           $mod_strings['LBL_THEME_SETTINGS']
           );
    }
    
    /**
     * @see SugarView::process()
     */
    public function process()
    {
        global $current_user;
        if (!is_admin($current_user)) {
            sugar_die("Unauthorized access to administration.");
        }

        // Check if the theme is valid
        if (!isset($_REQUEST['theme']) || !array_key_exists($_REQUEST['theme'], SugarThemeRegistry::allThemes())) {
            sugar_die("theme is invalid.");
        }

        if (isset($_REQUEST['do']) && $_REQUEST['do'] == 'save') {
            $theme_config = SugarThemeRegistry::getThemeConfig($_REQUEST['theme']);

            $configurator = new Configurator();

            foreach ($theme_config as $name => $def) {
                if (isset($_REQUEST[$name])) {
                    if ($_REQUEST[$name] == 'true') {
                        $_REQUEST[$name] = true;
                    } else {
                        if ($_REQUEST[$name] == 'false') {
                            $_REQUEST[$name] = false;
                        }
                    }
                    $configurator->config['theme_settings'][$_REQUEST['theme']][$name] = $_REQUEST[$name];
                }
            }
            $configurator->handleOverride();
            sleep(3);
            SugarApplication::redirect('index.php?module=Administration&action=ThemeSettings');
            exit();
        }

        parent::process();
    }
    
    /**
     * display the form
     */
    public function display()
    {
        global $mod_strings, $app_strings;

        $this->ss->assign('config', SugarThemeRegistry::getThemeConfig($_REQUEST['theme']));
        $this->ss->assign('mod', $mod_strings);
        $this->ss->assign('APP', $app_strings);
        
        echo $this->getModuleTitle(false);
        echo $this->ss->fetch('modules/Administration/templates/themeConfigSettings.tpl');
    }
}
