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



require_once('include/MVC/View/SugarView.php');
require_once('modules/EmailMan/Forms.php');

#[\AllowDynamicProperties]
class ViewCampaignconfig extends SugarView
{
    /**
     * @see SugarView::_getModuleTitleParams()
     */
    protected function _getModuleTitleParams($browserTitle = false)
    {
        global $mod_strings;
        
        return array(
           "<a href='index.php?module=Administration&action=index'>".translate('LBL_MODULE_NAME', 'Administration')."</a>",
           translate('LBL_CAMPAIGN_CONFIG_TITLE', 'Administration'),
           );
    }
    
    /**
     * @see SugarView::preDisplay()
     */
    public function preDisplay()
    {
        global $current_user;
        
        if (!is_admin($current_user)
                && !is_admin_for_module($GLOBALS['current_user'], 'Campaigns')) {
            sugar_die("Unauthorized access to administration.");
        }
    }
    
    /**
     * @see SugarView::display()
     */
    public function display()
    {
        global $mod_strings;
        global $app_list_strings;
        global $app_strings;
        global $current_user;
        
        echo $this->getModuleTitle(false);
        global $currentModule, $sugar_config;
        
        $focus = BeanFactory::newBean('Administration');
        $focus->retrieveSettings(); //retrieve all admin settings.
        $GLOBALS['log']->info("Mass Emailer(EmailMan) ConfigureSettings view");
        
        $this->ss->assign("MOD", $mod_strings);
        $this->ss->assign("APP", $app_strings);
        $this->ss->assign("THEME", (string)SugarThemeRegistry::current());
        $this->ss->assign("RETURN_MODULE", "Administration");
        $this->ss->assign("RETURN_ACTION", "index");
        
        $this->ss->assign("MODULE", $currentModule);
        $this->ss->assign("PRINT_URL", "index.php?".$GLOBALS['request_string']);
        
        if (isset($focus->settings['massemailer_campaign_emails_per_run']) && !empty($focus->settings['massemailer_campaign_emails_per_run'])) {
            $this->ss->assign("EMAILS_PER_RUN", $focus->settings['massemailer_campaign_emails_per_run']);
        } else {
            $this->ss->assign("EMAILS_PER_RUN", 500);
        }
        
        if (!isset($focus->settings['massemailer_tracking_entities_location_type']) || empty($focus->settings['massemailer_tracking_entities_location_type']) || $focus->settings['massemailer_tracking_entities_location_type']=='1') {
            $this->ss->assign("default_checked", "checked");
            $this->ss->assign("TRACKING_ENTRIES_LOCATION_STATE", "disabled");
            $this->ss->assign("TRACKING_ENTRIES_LOCATION", $mod_strings['TRACKING_ENTRIES_LOCATION_DEFAULT_VALUE']);
        } else {
            $this->ss->assign("userdefined_checked", "checked");
            $this->ss->assign("TRACKING_ENTRIES_LOCATION", $focus->settings["massemailer_tracking_entities_location"]);
        }
        $this->ss->assign("SITEURL", $sugar_config['site_url']);
        
        
        // Change the default campaign to not store a copy of each message.
        if (!empty($focus->settings['massemailer_email_copy']) && $focus->settings['massemailer_email_copy']=='1') {
            $this->ss->assign("yes_checked", "checked='checked'");
        } else {
            $this->ss->assign("no_checked", "checked='checked'");
        }
        
        $email = BeanFactory::newBean('Emails');
        $this->ss->assign('ROLLOVER', $email->rolloverStyle);
        
        $this->ss->assign("JAVASCRIPT", get_validate_record_js());
        $this->ss->display("modules/EmailMan/tpls/campaignconfig.tpl");
    }
}
