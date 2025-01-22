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

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

/*********************************************************************************

 * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
 * All Rights Reserved.
 * Contributor(s): ______________________________________..
 ********************************************************************************/

require_once('modules/Configurator/Forms.php');
require_once('modules/Administration/Forms.php');
require_once('modules/Configurator/Configurator.php');
require_once('include/SugarLogger/SugarLogger.php');
require_once('modules/Leads/Lead.php');

#[\AllowDynamicProperties]
class ConfiguratorViewEdit extends ViewEdit
{
    /**
     * @var Configurator
     */
    protected $configurator;

    /**
     * @see SugarView::preDisplay()
     */
    public function preDisplay()
    {
        if (!is_admin($GLOBALS['current_user'])) {
            sugar_die($GLOBALS['app_strings']['ERR_NOT_ADMIN']);
        }
    }

    /**
     * @see SugarView::_getModuleTitleParams()
     */
    protected function _getModuleTitleParams($browserTitle = false)
    {
        global $mod_strings;

        return array(
           "<a href='index.php?module=Administration&action=index'>".translate('LBL_MODULE_NAME', 'Administration')."</a>",
           $mod_strings['LBL_SYSTEM_SETTINGS']
           );
    }

    public function __construct()
    {
        $this->configurator = new Configurator;
    }

    public function process()
    {
        if (isset($this->errors['company_logo'])) {
            $this->configurator->errors['company_logo'] = $this->errors['company_logo'];
            unset($this->errors['company_logo']);
        }

        return parent::process();
    }

    /**
     * @see SugarView::display()
     */
    public function display()
    {
        global $current_user, $mod_strings, $app_strings, $app_list_strings, $sugar_config, $locale;

        $configurator = $this->configurator;
        $sugarConfig = SugarConfig::getInstance();
        $focus = BeanFactory::newBean('Administration');
        $configurator->parseLoggerSettings();

        $focus->retrieveSettings();
        if (!empty($_POST['restore'])) {
            $configurator->restoreConfig();
        }


        $mailSendType = null;
        if (isset($focus->settings['mail_sendtype'])) {
            $mailSendType = $focus->settings['mail_sendtype'];
        } else {
            LoggerManager::getLogger()->error('ConfiguratorViewEdit view display error: mail send type is not set for focus');
        }

        $this->ss->assign('MOD', $mod_strings);
        $this->ss->assign('APP', $app_strings);
        $this->ss->assign('APP_LIST', $app_list_strings);
        $this->ss->assign('config', $configurator->config);
        $this->ss->assign('error', $configurator->errors);
        $this->ss->assign("AUTO_REFRESH_INTERVAL_OPTIONS", get_select_options_with_id($app_list_strings['dashlet_auto_refresh_options_admin'], isset($configurator->config['dashlet_auto_refresh_min']) ? $configurator->config['dashlet_auto_refresh_min'] : 30));
        $this->ss->assign('LANGUAGES', get_languages());
        $this->ss->assign("JAVASCRIPT", get_set_focus_js(). get_configsettings_js());
        $this->ss->assign('company_logo', SugarThemeRegistry::current()->getImageURL('company_logo.png'));
        $this->ss->assign("settings", $focus->settings);
        $this->ss->assign("mail_sendtype_options", get_select_options_with_id($app_list_strings['notifymail_sendtype'], $mailSendType));
        if (!empty($focus->settings['proxy_on'])) {
            $this->ss->assign("PROXY_CONFIG_DISPLAY", 'inline');
        } else {
            $this->ss->assign("PROXY_CONFIG_DISPLAY", 'none');
        }
        if (!empty($focus->settings['proxy_auth'])) {
            $this->ss->assign("PROXY_AUTH_DISPLAY", 'inline');
        } else {
            $this->ss->assign("PROXY_AUTH_DISPLAY", 'none');
        }
        if (!empty($configurator->config['logger']['level'])) {
            $this->ss->assign('log_levels', get_select_options_with_id(LoggerManager::getLoggerLevels(), $configurator->config['logger']['level']));
        } else {
            $this->ss->assign('log_levels', get_select_options_with_id(LoggerManager::getLoggerLevels(), ''));
        }
        if (!empty($configurator->config['lead_conv_activity_opt'])) {
            $this->ss->assign('lead_conv_activities', get_select_options_with_id(Lead::getActivitiesOptions(), $configurator->config['lead_conv_activity_opt']));
        } else {
            $this->ss->assign('lead_conv_activities', get_select_options_with_id(Lead::getActivitiesOptions(), ''));
        }
        if (!empty($configurator->config['logger']['file']['suffix'])) {
            $this->ss->assign('filename_suffix', get_select_options_with_id(SugarLogger::$filename_suffix, $configurator->config['logger']['file']['suffix']));
        } else {
            $this->ss->assign('filename_suffix', get_select_options_with_id(SugarLogger::$filename_suffix, ''));
        }
        if (isset($configurator->config['logger_visible'])) {
            $this->ss->assign('logger_visible', $configurator->config['logger_visible']);
        } else {
            $this->ss->assign('logger_visible', true);
        }
        if (isset($configurator->config['stackTrace'])) {
            $this->ss->assign('stackTrace', $configurator->config['stackTrace']);
        } else {
            $this->ss->assign('stackTrace', false);
        }

        // Check for Google Sync JSON
        $this->checkGoogleSyncJSON($configurator->config['google_auth_json']);

        echo $this->getModuleTitle(false);

        $this->ss->display('modules/Configurator/tpls/EditView.tpl');

        $javascript = new javascript();
        $javascript->setFormName("ConfigureSettings");
        $javascript->addFieldGeneric("notify_fromaddress", "email", $mod_strings['LBL_NOTIFY_FROMADDRESS'], true, "");
        $javascript->addFieldGeneric("notify_subject", "varchar", $mod_strings['LBL_NOTIFY_SUBJECT'], true, "");
        $javascript->addFieldGeneric("proxy_host", "varchar", $mod_strings['LBL_PROXY_HOST'], true, "");
        $javascript->addFieldGeneric("proxy_port", "int", $mod_strings['LBL_PROXY_PORT'], true, "");
        $javascript->addFieldGeneric("proxy_password", "varchar", $mod_strings['LBL_PROXY_PASSWORD'], true, "");
        $javascript->addFieldGeneric("proxy_username", "varchar", $mod_strings['LBL_PROXY_USERNAME'], true, "");
        echo $javascript->getScript();
    }

    /**
     *
     * @param string $googleAuthJSON
     */
    protected function checkGoogleSyncJSON($googleAuthJSON)
    {
        $json = base64_decode($googleAuthJSON);
        $config = json_decode($json, true);
        if ($config) {
            $this->ss->assign("GOOGLE_JSON_CONF", 'CONFIGURED');
            $this->ss->assign("GOOGLE_JSON_CONF_COLOR", 'green');
        } else {
            $this->ss->assign("GOOGLE_JSON_CONF", 'UNCONFIGURED');
            $this->ss->assign("GOOGLE_JSON_CONF_COLOR", 'black');
        }
    }
}
