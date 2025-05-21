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
class EmailManController extends SugarController
{
    public function action_Save()
    {
        require_once('include/OutboundEmail/OutboundEmail.php');
        require_once('modules/Configurator/Configurator.php');

        $configurator = new Configurator();
        global $sugar_config;
        global $current_user;
        if (!is_admin($current_user)
                && !is_admin_for_module($GLOBALS['current_user'], 'Emails')
                && !is_admin_for_module($GLOBALS['current_user'], 'Campaigns')) {
            sugar_die("Unauthorized access to administration.");
        }

        //Do not allow users to spoof for sendmail if the config flag is not set.
        if (!isset($sugar_config['allow_sendmail_outbound']) || !$sugar_config['allow_sendmail_outbound']) {
            $_REQUEST['mail_sendtype'] = "SMTP";
        }

        // save Outbound settings  #Bug 20033 Ensure data for Outbound email exists before trying to update the system mailer.
        if (isset($_REQUEST['mail_sendtype']) && empty($_REQUEST['campaignConfig'])) {
            $oe = new OutboundEmail();
            $oe->populateFromPost();
            $oe->saveSystem();
        }



        $focus = BeanFactory::newBean('Administration');

        if (isset($_POST['tracking_entities_location_type'])) {
            if ($_POST['tracking_entities_location_type'] != '2') {
                unset($_POST['tracking_entities_location']);
                unset($_POST['tracking_entities_location_type']);
            }
        }
        // cn: handle mail_smtpauth_req checkbox on/off (removing double reference in the form itself
        if (!isset($_POST['mail_smtpauth_req'])) {
            $_POST['mail_smtpauth_req'] = 0;
        }

        if (!empty($_POST['notify_allow_default_outbound'])) {
            $oe = new OutboundEmail();
            if (!$oe->isAllowUserAccessToSystemDefaultOutbound()) {
                $oe->removeUserOverrideAccounts();
            }
        }

        $focus->saveConfig();

        if (empty($_POST['campaignConfig'])) {
            // save User defaults for emails
            $configurator->config['email_default_delete_attachments'] = (isset($_REQUEST['email_default_delete_attachments'])) ? true : false;
            $configurator->config['email_warning_notifications'] = (isset($_REQUEST['email_warning_notifications'])) ? true : false;
            $configurator->config['email_enable_confirm_opt_in'] = isset($_REQUEST['email_enable_confirm_opt_in']) ? $_REQUEST['email_enable_confirm_opt_in'] : SugarEmailAddress::COI_STAT_DISABLED;
            $configurator->config['email_enable_auto_send_opt_in'] = (isset($_REQUEST['email_enable_auto_send_opt_in'])) ? true : false;
            $configurator->config['email_confirm_opt_in_email_template_id'] = isset($_REQUEST['email_template_id_opt_in']) ? $_REQUEST['email_template_id_opt_in'] : $configurator->config['aop']['confirm_opt_in_template_id'];
            $configurator->config['email_allow_send_as_user']  = (isset($_REQUEST['mail_allowusersend']) && $_REQUEST['mail_allowusersend'] == '1') ? true : false;
            $configurator->config['legacy_email_behaviour']  = isTrue($_REQUEST['legacy_email_behaviour'] ?? false);
            ///////////////////////////////////////////////////////////////////////////////
            ////	SECURITY
            $security = array();
            if (isset($_REQUEST['applet'])) {
                $security['applet'] = 'applet';
            }
            if (isset($_REQUEST['base'])) {
                $security['base'] = 'base';
            }
            if (isset($_REQUEST['embed'])) {
                $security['embed'] = 'embed';
            }
            if (isset($_REQUEST['form'])) {
                $security['form'] = 'form';
            }
            if (isset($_REQUEST['frame'])) {
                $security['frame'] = 'frame';
            }
            if (isset($_REQUEST['frameset'])) {
                $security['frameset'] = 'frameset';
            }
            if (isset($_REQUEST['iframe'])) {
                $security['iframe'] = 'iframe';
            }
            if (isset($_REQUEST['import'])) {
                $security['import'] = '\?import';
            }
            if (isset($_REQUEST['layer'])) {
                $security['layer'] = 'layer';
            }
            if (isset($_REQUEST['link'])) {
                $security['link'] = 'link';
            }
            if (isset($_REQUEST['object'])) {
                $security['object'] = 'object';
            }
            if (isset($_REQUEST['style'])) {
                $security['style'] = 'style';
            }
            if (isset($_REQUEST['xmp'])) {
                $security['xmp'] = 'xmp';
            }
            $security['script'] = 'script';

            $configurator->config['email_xss'] = base64_encode(serialize($security));

            ////	SECURITY
            ///////////////////////////////////////////////////////////////////////////////

            ksort($sugar_config);

            $configurator->handleOverride();
        }

        SugarThemeRegistry::clearAllCaches();
    }
}
