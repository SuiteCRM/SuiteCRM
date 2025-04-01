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

/*********************************************************************************

 * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
 * All Rights Reserved.
 * Contributor(s): ______________________________________..
 ********************************************************************************/

require_once('include/MVC/View/SugarView.php');
require_once('modules/Configurator/Forms.php');
require_once('modules/Administration/Forms.php');
require_once('modules/Configurator/Configurator.php');

#[\AllowDynamicProperties]
class ViewAdminwizard extends SugarView
{
    public function __construct($bean = null, $view_object_map = array())
    {
        parent::__construct($bean, $view_object_map);
        
        $this->options['show_header'] = false;
        $this->options['show_footer'] = false;
        $this->options['show_javascript'] = false;
    }
    
    /**
     * @see SugarView::display()
     */
    public function display()
    {
        global $current_user, $mod_strings, $app_list_strings, $sugar_config, $locale, $sugar_version;
            
        if (!is_admin($current_user)) {
            sugar_die($GLOBALS['app_strings']['ERR_NOT_ADMIN']);
        }
                
        $themeObject = SugarThemeRegistry::current();
        
        $configurator = new Configurator();
        $sugarConfig = SugarConfig::getInstance();
        $focus = BeanFactory::newBean('Administration');
        $focus->retrieveSettings();
        
        $ut = $GLOBALS['current_user']->getPreference('ut');
        if (empty($ut)) {
            $this->ss->assign('SKIP_URL', 'index.php?module=Users&action=Wizard&skipwelcome=1');
        } else {
            $this->ss->assign('SKIP_URL', 'index.php?module=Home&action=index');
        }

        $silentInstall = $GLOBALS['current_user']->getPreference('silentInstall');
        //If not set, show the configuration
        if ($silentInstall === null) {
            $silentInstall = false;
        }

        // Always mark that we have got past this point
        $focus->saveSetting('system', 'adminwizard', 1);
        $css = $themeObject->getCSS();
        $favicon = $themeObject->getImageURL('sugar_icon.ico', false);
        $this->ss->assign('FAVICON_URL', getJSPath($favicon));
        $this->ss->assign('SUGAR_CSS', $css);
        $this->ss->assign('MOD_USERS', return_module_language($GLOBALS['current_language'], 'Users'));
        $this->ss->assign('CSS', '<link rel="stylesheet" type="text/css" href="'.SugarThemeRegistry::current()->getCSSURL('wizard.css').'" />');
        $this->ss->assign('LANGUAGES', get_languages());
        $this->ss->assign('config', $sugar_config);
        $this->ss->assign('SUGAR_VERSION', $sugar_version);
        $this->ss->assign('settings', $focus->settings);
        $this->ss->assign('exportCharsets', get_select_options_with_id($locale->getCharsetSelect(), $sugar_config['default_export_charset']));
        $this->ss->assign('getNameJs', $locale->getNameJs());
        $this->ss->assign('NAMEFORMATS', $locale->getUsableLocaleNameOptions($sugar_config['name_formats']));
        $this->ss->assign('JAVASCRIPT', get_set_focus_js(). get_configsettings_js());
        $this->ss->assign('company_logo', SugarThemeRegistry::current()->getImageURL('company_logo.png'));
        $this->ss->assign('mail_smtptype', $focus->settings['mail_smtptype']);
        $this->ss->assign('mail_smtpserver', $focus->settings['mail_smtpserver']);
        $this->ss->assign('mail_smtpport', $focus->settings['mail_smtpport']);
        $this->ss->assign('mail_smtpuser', $focus->settings['mail_smtpuser']);
        $this->ss->assign('mail_smtppass', $focus->settings['mail_smtppass']);
        $this->ss->assign('mail_smtpauth_req', ($focus->settings['mail_smtpauth_req']) ? "checked='checked'" : '');
        $this->ss->assign('MAIL_SSL_OPTIONS', get_select_options_with_id($app_list_strings['email_settings_for_ssl'], $focus->settings['mail_smtpssl']));
        $this->ss->assign('notify_allow_default_outbound_on', (!empty($focus->settings['notify_allow_default_outbound']) && $focus->settings['notify_allow_default_outbound'] == 2) ? 'CHECKED' : '');
        $this->ss->assign('THEME', (string)SugarThemeRegistry::current());

        $this->ss->assign('silentInstall', $silentInstall);

        // get javascript
        ob_start();
        $this->options['show_javascript'] = true;
        $this->renderJavascript();
        $this->options['show_javascript'] = false;
        $this->ss->assign("SUGAR_JS", ob_get_contents().$themeObject->getJS());
        ob_end_clean();

        $this->ss->assign('langHeader', get_language_header());
        $this->ss->assign('START_PAGE', !empty($_REQUEST['page']) ? $_REQUEST['page'] : 'welcome');

        //Start of scenario block
        require_once('install/suite_install/scenarios.php');
        if (isset($installation_scenarios)) {
            $installation_scenariosCount = is_countable($installation_scenarios) ? count($installation_scenarios) : 0;
            for ($i = 0; $i < $installation_scenariosCount; $i++) {
                $installation_scenarios[$i]['moduleOverview']='( '.implode(', ', $installation_scenarios[$i]['modules']).')';
            }

            $this->ss->assign('scenarios', $installation_scenarios);
        } else {
            $this->ss->assign('scenarios', []);
        }
        //End of scenario block
                
        $this->ss->display('modules/Configurator/tpls/adminwizard.tpl');
    }
}
