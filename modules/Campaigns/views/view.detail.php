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

 * Description: This file is used to override the default Meta-data DetailView behavior
 * to provide customization specific to the Campaigns module.
 * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
 * All Rights Reserved.
 * Contributor(s): ______________________________________..
 ********************************************************************************/

require_once('include/json_config.php');


#[\AllowDynamicProperties]
class CampaignsViewDetail extends ViewDetail
{
    public function __construct()
    {
        parent::__construct();
        //turn off normal display of subpanels
        $this->options['show_subpanels'] = false;
    }





    public function preDisplay()
    {
        global $mod_strings;
        if (isset($this->bean->campaign_type) && strtolower($this->bean->campaign_type) == 'newsletter') {
            $mod_strings['LBL_MODULE_NAME'] = $mod_strings['LBL_NEWSLETTERS'];
        }
        parent::preDisplay();
        $this->options['show_subpanels'] = false;
    }

    public function display()
    {
        global $app_list_strings;
        $this->ss->assign('APP_LIST', $app_list_strings);

        if (isset($_REQUEST['mode']) && $_REQUEST['mode']=='set_target') {
            require_once('modules/Campaigns/utils.php');
            //call function to create campaign logs
            $mess = track_campaign_prospects($this->bean);

            $confirm_msg = "var ajax_C_LOG_Status = new SUGAR.ajaxStatusClass();
            window.setTimeout(\"ajax_C_LOG_Status.showStatus('".$mess."')\",1000);
            window.setTimeout('ajax_C_LOG_Status.hideStatus()', 1500);
            window.setTimeout(\"ajax_C_LOG_Status.showStatus('".$mess."')\",2000);
            window.setTimeout('ajax_C_LOG_Status.hideStatus()', 5000); ";
            $this->ss->assign("MSG_SCRIPT", $confirm_msg);
        }

        if (($this->bean->campaign_type == 'Email') || ($this->bean->campaign_type == 'NewsLetter')) {
            $this->ss->assign("ADD_BUTTON_STATE", "submit");
            $this->ss->assign("TARGET_BUTTON_STATE", "hidden");
        } else {
            $this->ss->assign("ADD_BUTTON_STATE", "hidden");
            $this->ss->assign("DISABLE_LINK", "display:none");
            $this->ss->assign("TARGET_BUTTON_STATE", "submit");
        }

        $currency = BeanFactory::newBean('Currencies');
        if (isset($this->bean->currency_id) && !empty($this->bean->currency_id)) {
            $currency->retrieve($this->bean->currency_id);
            if ($currency->deleted != 1) {
                $this->ss->assign('CURRENCY', $currency->iso4217 .' '.$currency->symbol);
            } else {
                $this->ss->assign('CURRENCY', $currency->getDefaultISO4217() .' '.$currency->getDefaultCurrencySymbol());
            }
        } else {
            $this->ss->assign('CURRENCY', $currency->getDefaultISO4217() .' '.$currency->getDefaultCurrencySymbol());
        }

        parent::display();

        //We want to display subset of available, panels, so we will call subpanel
        //object directly instead of using sugarview.
        $GLOBALS['focus'] = $this->bean;
        require_once('include/SubPanel/SubPanelTiles.php');
        $subpanel = new SubPanelTiles($this->bean, $this->module);
        //get available list of subpanels
        $alltabs=$subpanel->subpanel_definitions->get_available_tabs();
        if (!empty($alltabs)) {
            //iterate through list, and filter out all but 3 subpanels
            foreach ($alltabs as $key=>$name) {
                if ($name != 'prospectlists' && $name!='emailmarketing' && $name != 'tracked_urls' && $name != 'history'
                /* BEGIN - SECURITY GROUPS */
                    && $name != 'securitygroups'
                /* END - SECURITY GROUPS */
                ) {
                    //exclude subpanels that are not prospectlists, emailmarketing, or tracked urls
                    $subpanel->subpanel_definitions->exclude_tab($name);
                }
            }
            //only show email marketing subpanel for email/newsletter campaigns
            if ($this->bean->campaign_type != 'Email' && $this->bean->campaign_type != 'NewsLetter') {
                //exclude emailmarketing subpanel if not on an email or newsletter campaign
                $subpanel->subpanel_definitions->exclude_tab('emailmarketing');
                // Bug #49893  - 20120120 - Captivea (ybi) - Remove trackers subpanels if not on an email/newsletter campaign (useless subpannl)
                $subpanel->subpanel_definitions->exclude_tab('tracked_urls');
            }
        }
        //show filtered subpanel list
        echo $subpanel->display();
    }
}
