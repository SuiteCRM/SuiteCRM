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

require_once "modules/stic_Web_Forms/Assistant/AssistantController.php";

/**
 * Controller of the donations form creation wizard
 */
class DonationController extends stic_Web_FormsAssistantController {
    protected $availableModules = array('Contacts', 'Accounts'); // Available modules

    /**
     * First action controller: initial form parameters
     */
    public function actionStep1() {
        $this->createModulesArray();
        $this->view = 'Donationstep1';
        $this->view_object_map['WEB_MODULE'] = $this->persistentData['WEB_MODULE'] ?? null; // Retrieve the value saved in previous steps
        $this->mapStepNavigation('Step1', 'Step2');

        // Get all reCAPTCHA configurations and put it in view_object_map['PERSISTENT_DATA']['recaptcha_configs']
        $this->view_object_map['PERSISTENT_DATA']['recaptcha_configs'] = $this->getRecaptchaConfigurations();
        $this->view_object_map['PERSISTENT_DATA']['recaptcha_configKeys'] = array_keys($this->view_object_map['PERSISTENT_DATA']['recaptcha_configs']);
        // Get index of selected reCAPTCHA configuration
        $this->view_object_map['PERSISTENT_DATA']['recaptcha_selected'] = $this->persistentData['recaptcha_selected'] ?? null;
    }

    /**
     * Second step controller: choice of person fields
     */
    public function actionStep2() {
        if ($this->prev_step = 'step1') {
            $this->saveRequestParams(array('web_module' => 'WEB_MODULE'));
            $this->persistentData['include_recaptcha'] = isset($this->include_recaptcha) ? $this->include_recaptcha : '';
            $this->persistentData['recaptcha_selected'] = isset($this->recaptcha_selected) ? $this->recaptcha_selected : '';
        } else if ($this->prev_step = 'step3') {
            // If we come from step 3 (option back) save the parameters of the step
            $this->saveRequestParams(
                array(
                    'web_header' => 'FORM_HEADER',
                    'web_description' => 'FORM_DESCRIPTION',
                    'web_submit' => 'FORM_SUBMIT_LABEL',
                    'post_url' => 'FORM_WEB_POST_URL',
                    'redirect_ok_url' => 'FORM_REDIRECT_OK_URL',
                    'redirect_ko_url' => 'FORM_REDIRECT_KO_URL',
                    'validate_identification_number' => 'VALIDATE_IDENTIFICATION_NUMBER',
                    'allow_card_recurring_payments' => 'ALLOW_CARD_RECURRING_PAYMENTS',
                    'allow_paypal_recurring_payments' => 'ALLOW_PAYPAL_RECURRING_PAYMENTS',
                    'allow_stripe_recurring_payments' => 'ALLOW_STRIPE_RECURRING_PAYMENTS',
                    'payment_type' => 'PAYMENT_TYPE',
                    'relation_type' => 'RELATION_TYPE',
                    'email_template_id' => 'EMAIL_TEMPLATE_ID',
                    'email_template_name' => 'EMAIL_TEMPLATE_NAME',
                    'campaign_id' => 'EVENT_ID',
                    'campaign_name' => 'EVENT_NAME',
                    'assigned_user_id' => 'ASSIGNED_USER_ID',
                    'assigned_user_name' => 'ASSIGNED_USER_NAME',
                    'web_footer' => 'FORM_FOOTER',
                )
            );
        }

        if (!in_array($this->persistentData['WEB_MODULE'], $this->availableModules)) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Invalid module value received [{$this->persistentData['WEB_MODULE']}].");
            $this->no_action();
        } else {
            // Build the list of fields to display
            $bean = BeanFactory::getBean($this->persistentData['WEB_MODULE']);
            $this->mapModuleFields($bean, $this->getRequiredFields());
            $this->mapStepNavigation('Step2', 'Step3', 'Step1');
            $this->view = 'Donationselectfields';
        }
    }

    /**
     * Third step controller: form parameters
     * NOTE: it could be unified with the first step, but it is left like this for consistency in usability with the rest of the forms
     */
    public function actionStep3() {
        global $app_list_strings;

        // Generate the default shipping url
        if (!isset($this->persistentData['FORM_WEB_POST_URL']) || $this->persistentData['FORM_WEB_POST_URL'] === '') {
            if ($this->persistentData['include_recaptcha']) {
                $this->persistentData['FORM_WEB_POST_URL'] = $this->getServerURL() . '/index.php?entryPoint=stic_Web_Forms_saveRecaptcha';
            } else {
                $this->persistentData['FORM_WEB_POST_URL'] = $this->getServerURL() . '/index.php?entryPoint=stic_Web_Forms_save';
            }
        }

        // Generate the array with the types of payments available
        $objFP = BeanFactory::newBean('stic_Payment_Commitments');
        $this->view_object_map['PAYMENT_TYPE_OPTIONS'] = $app_list_strings[$objFP->field_defs['payment_type']['options']];

        // Generate the assigned user id
        if (!isset($this->persistentData['ASSIGNED_USER_ID']) || $this->persistentData['ASSIGNED_USER_ID'] === '') {
            global $current_user;
            $this->persistentData['ASSIGNED_USER_ID'] = $current_user->id;
            $this->persistentData['ASSIGNED_USER_NAME'] = $current_user->name;
        }

        // Generate the list of valid relationship types
        $objModel = BeanFactory::newBean($this->persistentData['WEB_MODULE']);
        $this->view_object_map['RELATION_TYPE_OPTIONS'] = $app_list_strings[$objModel->field_defs['stic_relationship_type_c']['options']];

        // Load the data in case we come from step 4
        $this->view_object_map['FORM_WEB_POST_URL'] = $this->persistentData['FORM_WEB_POST_URL'];
        $this->view_object_map['FORM_DESCRIPTION'] = $this->persistentData['FORM_DESCRIPTION'] ?? null;
        $this->view_object_map['FORM_HEADER'] = $this->persistentData['FORM_HEADER'] ?? null;
        $this->view_object_map['FORM_SUBMIT_LABEL'] = $this->persistentData['FORM_SUBMIT_LABEL'] ?? null;
        $this->view_object_map['FORM_REDIRECT_OK_URL'] = $this->persistentData['FORM_REDIRECT_OK_URL'] ?? null;
        $this->view_object_map['FORM_REDIRECT_KO_URL'] = $this->persistentData['FORM_REDIRECT_KO_URL'] ?? null;
        $this->view_object_map['VALIDATE_IDENTIFICATION_NUMBER'] = $this->persistentData['VALIDATE_IDENTIFICATION_NUMBER'] ?? null;
        $this->view_object_map['ALLOW_CARD_RECURRING_PAYMENTS'] = $this->persistentData['ALLOW_CARD_RECURRING_PAYMENTS'] ?? null;
        $this->view_object_map['ALLOW_PAYPAL_RECURRING_PAYMENTS'] = $this->persistentData['ALLOW_PAYPAL_RECURRING_PAYMENTS'] ?? null;
        $this->view_object_map['PAYMENT_TYPE'] = $this->persistentData['PAYMENT_TYPE'] ?? null;
        $this->view_object_map['RELATION_TYPE'] = $this->persistentData['RELATION_TYPE'] ?? null;
        $this->view_object_map['EMAIL_TEMPLATE_ID'] = $this->persistentData['EMAIL_TEMPLATE_ID'] ?? null;
        $this->view_object_map['EMAIL_TEMPLATE_NAME'] = $this->persistentData['EMAIL_TEMPLATE_NAME'] ?? null;
        $this->view_object_map['CAMPAIGN_ID'] = $this->persistentData['CAMPAIGN_ID'] ?? null;
        $this->view_object_map['CAMPAIGN_NAME'] = $this->persistentData['CAMPAIGN_NAME'] ?? null;
        $this->view_object_map['ASSIGNED_USER_ID'] = $this->persistentData['ASSIGNED_USER_ID'] ?? null;
        $this->view_object_map['ASSIGNED_USER_NAME'] = $this->persistentData['ASSIGNED_USER_NAME'] ?? null;
        $this->view_object_map['FORM_FOOTER'] = $this->persistentData['FORM_FOOTER'] ?? null ;

        $this->view = 'Donationstep3';

        $this->mapStepNavigation('Step3', 'StepFormat', 'Step2');
    }

    /**
     * Sixth step controller: formatting the form
     */
    public function actionStepFormat() {
        global $current_language;

        $this->saveRequestParams(array(
            'web_header' => 'FORM_HEADER',
            'web_description' => 'FORM_DESCRIPTION',
            'web_submit' => 'FORM_SUBMIT_LABEL',
            'post_url' => 'FORM_WEB_POST_URL',
            'redirect_ok_url' => 'FORM_REDIRECT_OK_URL',
            'redirect_ko_url' => 'FORM_REDIRECT_KO_URL',
            'validate_identification_number' => 'VALIDATE_IDENTIFICATION_NUMBER',
            'allow_card_recurring_payments' => 'ALLOW_CARD_RECURRING_PAYMENTS',
            'allow_paypal_recurring_payments' => 'ALLOW_PAYPAL_RECURRING_PAYMENTS',
            'allow_stripe_recurring_payments' => 'ALLOW_STRIPE_RECURRING_PAYMENTS',
            'payment_type' => 'PAYMENT_TYPE',
            'relation_type' => 'RELATION_TYPE',
            'email_template_id' => 'EMAIL_TEMPLATE_ID',
            'email_template_name' => 'EMAIL_TEMPLATE_NAME',
            'campaign_id' => 'CAMPAIGN_ID',
            'campaign_name' => 'CAMPAIGN_NAME',
            'assigned_user_id' => 'ASSIGNED_USER_ID',
            'assigned_user_name' => 'ASSIGNED_USER_NAME',
            'web_footer' => 'FORM_FOOTER',
            'form_language' => 'FORM_LANGUAGE',
        ));

        // Save the number of fields for attachments that should be in the form
        $this->persistentData['num_attachment'] = $_REQUEST['num_attachment'];

        // Prepare the parameters of the generated form
        $this->view_object_map['FORM'] = array();

        $serverURL = $this->getServerURL();
        $this->view_object_map['SERVERURL'] = $serverURL;

        // Save the label of the submit button of the form
        $this->view_object_map['FORM']['SUBMIT_LABEL'] = $this->persistentData['FORM_SUBMIT_LABEL'];

        // Form header
        $this->view_object_map['FORM']['HEADER'] = array('TEXT' => $this->persistentData['FORM_HEADER'], 'CSS' => '');

        // Description
        $this->view_object_map['FORM']['DESCRIPTION'] = array('TEXT' => $this->persistentData['FORM_DESCRIPTION'], 'CSS' => '');

        // Form footer
        $this->view_object_map['FORM']['FOOTER'] = array('TEXT' => $this->persistentData['FORM_FOOTER'], 'CSS' => '');

        $reqFields = array(); // Array that will contain, after the call to prepareFieldsToResultForm, the required fields (the forced ones plus those of each module)
        $boolFields = array(); // Array that will contain, after the call to prepareFieldsToResultForm, the Boolean fields

        // Include the payment method fields
        $this->includePCFields();

        // Prepare the visible fields of the form
        $requiredFields[$this->persistentData['WEB_MODULE']] = $this->getRequiredFields();
        $this->prepareFieldsToResultForm($requiredFields, $boolFields, $reqFields);

        // Prepare the list of required fields and Boolean
        $reqFields = (is_array($reqFields) && !empty($reqFields) ? implode(";", $reqFields) . ";" : "");
        $boolFields = (is_array($boolFields) && !empty($boolFields) ? implode(";", $boolFields) . ";" : "");

        // Prepare the form definition data
        $defParams = self::formatJsonData2HiddenField(array(
            'version' => '2',
            'email_template_id' => $this->persistentData['EMAIL_TEMPLATE_ID'],
            'relation_type' => $this->persistentData['RELATION_TYPE'],
            'include_recaptcha' => ($this->persistentData['include_recaptcha'] ? 1 : 0),
            'recaptcha_configKeys' => ($this->persistentData['recaptcha_configKeys']),
            'recaptcha_selected' => ($this->persistentData['recaptcha_selected']),
        ));

        // Hidden fields
        $this->view_object_map['FORM']['HIDDEN'] = array(
            array(
                'NAME' => 'campaign_id',
                'VALUE' => $this->persistentData['CAMPAIGN_ID'],
            ),
            array(
                'NAME' => 'redirect_url',
                'VALUE' => $this->persistentData['FORM_REDIRECT_OK_URL'],
            ),
            array(
                'NAME' => 'redirect_ko_url',
                'VALUE' => $this->persistentData['FORM_REDIRECT_KO_URL'],
            ),
            array(
                'NAME' => 'validate_identification_number',
                'VALUE' => $this->persistentData['VALIDATE_IDENTIFICATION_NUMBER'],
            ),
            array(
                'NAME' => 'allow_card_recurring_payments',
                'VALUE' => $this->persistentData['ALLOW_CARD_RECURRING_PAYMENTS'],
            ),
            array(
                'NAME' => 'allow_paypal_recurring_payments',
                'VALUE' => $this->persistentData['ALLOW_PAYPAL_RECURRING_PAYMENTS'],
            ),
            array(
                'NAME' => 'allow_stripe_recurring_payments',
                'VALUE' => $this->persistentData['ALLOW_STRIPE_RECURRING_PAYMENTS'],
            ),
            array(
                'NAME' => 'stripe_payment_method_types',
                'VALUE' => 'card,sepa_debit',
            ),
            array(
                'NAME' => 'assigned_user_id',
                'VALUE' => $this->persistentData['ASSIGNED_USER_ID'],
            ),
            array(
                'NAME' => 'req_id',
                'VALUE' => $reqFields,
            ),
            array(
                'NAME' => 'bool_id',
                'VALUE' => $boolFields,
            ),
            array(
                'NAME' => 'webFormClass',
                'VALUE' => 'Donation',
            ),
            array(
                'NAME' => 'stic_Payment_Commitments___payment_type',
                'VALUE' => $this->persistentData['PAYMENT_TYPE'],
            ),
            array(
                'NAME' => 'web_module',
                'VALUE' => $this->persistentData['WEB_MODULE'],
            ),
            array(
                'NAME' => 'language',
                'VALUE' => $current_language,
            ),
            array(
                'NAME' => 'defParams',
                'VALUE' => $defParams,
            ),
            array(
                'NAME' => 'timeZone',
                'VALUE' => '',
            ),
        );

        $this->mapStepNavigation('StepFormat', 'StepDownload', 'Step3');
        $this->view = 'Donationstepformat';
    }

    /**
     * Loading the view to downloading the form
     */
    public function actionStepDownload() {
        parent::actionStepDownload();
        // Save the form submission url
        $this->view_object_map['FORM']['URL'] = $this->persistentData['FORM_WEB_POST_URL'];
        $this->view_object_map['FORM']['NUM_ATTACHMENT'] = $this->persistentData['num_attachment'];
        $this->view = 'Donationstepdownload';
    }

    /**
     * Include the payment method fields in the array of selected fields
     */
    public function includePCFields() {
        global $app_list_strings;
        $bean = BeanFactory::getBean('stic_Payment_Commitments');
        $optionList = $app_list_strings[$bean->field_defs['payment_method']['options']];
        $restrictedOptionList = array(
            '' => $optionList[''],
            'direct_debit' => $optionList['direct_debit'],
            'card' => $optionList['card'],
            'ceca_card' => $optionList['ceca_card'],
            'transfer_received' => $optionList['transfer_received'],
            'paypal' => $optionList['paypal'],
            'bizum' => $optionList['bizum'],
            'stripe' => $optionList['stripe'],
        );

        // Add payment method for alternative TPV if exist
        foreach ($optionList as $key => $value) {
            if (substr($key, 0, 5) == 'card_' || substr($key, 0, 10) == 'ceca_card_' || substr($key, 0, 6) == 'bizum_') {
                $restrictedOptionList[$key]=$optionList[$key];
            }
        }

        $this->persistentData['SELECTED_FIELDS']['stic_Payment_Commitments']['COL1_FIELDS'] = array(
            array(
                'name' => 'amount',
                'script' => 'onkeypress="return isNumberKey(event)" onchange="formatCurrency(this)"',
            ),
            array(
                'name' => 'payment_method',
                'options' => $restrictedOptionList,
                'script' => 'onChange="adaptPaymentMethod(this)"',
            ),
            array(
                'name' => 'bank_account',
                'hidden' => true,
                'script' => 'onChange="validateIBAN(this)"',
            ),
            array(
                'name' => 'periodicity',
                'script' => 'onChange="adaptPeriodicity()"',
            ),
            array(
                'name' => 'banking_concept',
            ),
        );
    }

    /**
     * Returns an array with the mandatory parameters depending on the selected destination module
     * @return Array
     */
    protected function getRequiredFields() {
        $requiredFields = array();
        $requiredFields['email1'] = true;

        // Forces as mandatory the CIF or NIF field (and the name) depending on the destination module
        switch ($this->persistentData['WEB_MODULE']) {
        case 'Contacts':
            $requiredFields['stic_identification_number_c'] = true;
            $requiredFields['first_name'] = true;
            break;
        case 'Accounts':
            $requiredFields['stic_identification_number_c'] = true;
            break;
        }
        return $requiredFields;
    }

    /**
     * Create the list of available modules with the corresponding label
     */
    protected function createModulesArray() {
        global $app_list_strings;
        $this->view_object_map['MODULES'] = array();
        foreach ($this->availableModules as $module) {
            $this->view_object_map['MODULES'][$module] = $app_list_strings['moduleList'][$module];
        }
    }
}
