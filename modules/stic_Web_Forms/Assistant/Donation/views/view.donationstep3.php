<?php
/**
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation.
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */

require_once 'modules/stic_Web_Forms/Assistant/AssistantView.php';

class ViewDonationstep3 extends stic_Web_FormsAssistantView
{
    /**
     * Do what is needed before showing the view
     */
    public function preDisplay()
    {
        parent::preDisplay();

        $this->templateDir = "modules/stic_Web_Forms/Assistant/Donation/tpls";
        $this->tpl = "Step3.tpl";

        // Email Templates popup
        $popupRequestData = array(
            'call_back_function' => 'set_return',
            'form_name' => 'webforms',
            'field_to_name_array' => array(
                'id' => 'email_template_id',
                'name' => 'email_template_name',
            ),
        );
        $this->view_object_map['EMAIL_TEMPLATES_POPUP_REQ_DATA'] = json_encode($popupRequestData);

        // Users Popup
        $popupRequestData = array(
            'call_back_function' => 'set_return',
            'form_name' => 'webforms',
            'field_to_name_array' => array(
                'id' => 'assigned_user_id',
                'user_name' => 'assigned_user_name',
            ),
        );
        $this->view_object_map['USERS_POPUP_REQ_DATA'] = json_encode($popupRequestData);

        // Events popup
        $popupRequestData = array
            (
            'call_back_function' => 'set_return',
            'form_name' => 'webforms',
            'field_to_name_array' => array(
                'id' => 'campaign_id',
                'name' => 'campaign_name',
            ),
        );
        $this->view_object_map['CAMPAIGNS_POPUP_REQ_DATA'] = json_encode($popupRequestData);
    }

    /**
     * Display the view
     */
    public function display()
    {
        global $app_strings, $mod_strings;
        parent::display();

        $javascript = new javascript();
        $javascript->setFormName('webforms');
        $javascript->addFieldGeneric('campaign_name', '', javascript_escape($mod_strings['LBL_RELATED_CAMPAIGN']), 'true');
        $javascript->addFieldGeneric('assigned_user_name', '', javascript_escape($app_strings['LBL_ASSIGNED_TO']), 'true');
        $javascript->addFieldGeneric('email_template_name', '', javascript_escape($mod_strings['LBL_NOTIFICATION_EMAIL_TEMPLATE']), 'false');
        $javascript->addFieldGeneric('validate_identification_number', '', javascript_escape($app_strings['LBL_CHECK_IDENTIFICATION_NUMBER'] ?? ''), 'true');
        $javascript->addFieldGeneric('allow_card_recurring_payments', '', javascript_escape($app_strings['LBL_ALLOW_CARD_RECURRING_PAYMENTS'] ?? ''), 'true');
        $javascript->addFieldGeneric('allow_paypal_recurring_payments', '', javascript_escape($app_strings['LBL_ALLOW_PAYPAL_RECURRING_PAYMENTS'] ?? ''), 'true');
        $javascript->addFieldGeneric('allow_stripe_recurring_payments', '', javascript_escape($app_strings['LBL_ALLOW_STRIPE_RECURRING_PAYMENTS'] ?? ''), 'true');
        $javascript->addFieldGeneric('redirect_ok_url', '', javascript_escape($mod_strings['LBL_WEBFORMS_REDIRECT_OK_URL']), 'true');
        $javascript->addFieldGeneric('redirect_ko_url', '', javascript_escape($mod_strings['LBL_WEBFORMS_REDIRECT_KO_URL']), 'true');
        $javascript->addFieldGeneric('payment_type', '', javascript_escape($mod_strings['LBL_WEBFORMS_PAYMENT_TYPE']), 'true');
        $javascript->addToValidateBinaryDependency('campaign_name', 'alpha', $app_strings['ERR_SQS_NO_MATCH_FIELD'] . javascript_escape($mod_strings['LBL_RELATED_CAMPAIGN'] . ':'), 'false', '', 'campaign_id');
        $javascript->addToValidateBinaryDependency('assigned_user_name', 'alpha', $app_strings['ERR_SQS_NO_MATCH_FIELD'] . javascript_escape($app_strings['LBL_ASSIGNED_TO']), 'false', '', 'assigned_user_id');
        $javascript->addToValidateBinaryDependency('email_template_name', 'alpha', $app_strings['ERR_SQS_NO_MATCH_FIELD'] . javascript_escape($mod_strings['LBL_WEBFORMS_NOTIFY_EVENT'] . ':'), 'false', '', 'email_template_id');

        echo $javascript->getScript();
    }
}
