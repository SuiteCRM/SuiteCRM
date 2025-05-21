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
 * Controller of the event registration form creation wizard
 */
class EventInscriptionController extends stic_Web_FormsAssistantController {
    /**
     * First action controller: initial form parameters
     */
    public function actionStep1() {
        $this->view = 'Eventinscriptionstep1';
        $this->mapStepNavigation('step1', 'step2');

        // Get all reCAPTCHA configurations and put it in view_object_map['PERSISTENT_DATA']['recaptcha_configs']
        $this->view_object_map['PERSISTENT_DATA']['recaptcha_configs'] = $this->getRecaptchaConfigurations();
        $this->view_object_map['PERSISTENT_DATA']['recaptcha_configKeys'] = array_keys($this->view_object_map['PERSISTENT_DATA']['recaptcha_configs']);
        // Get index of selected reCAPTCHA configuration
        $this->view_object_map['PERSISTENT_DATA']['recaptcha_selected'] = $this->persistentData['recaptcha_selected'] ?? '';
    }

    /**
     * Ensures consistency of selected saved fields
     */
    public function ensureSelectedFields($modules) {
        $availableModules = array('Contacts');
        if ($this->persistentData['include_organization']) {
            $availableModules[] = 'Accounts';
        }

        if ($this->persistentData['include_registration']) {
            $availableModules[] = 'stic_Registrations';
        }
        parent::ensureSelectedFields($availableModules);
    }

    /**
     * Second step controller: choice of person fields
     */
    public function actionStep2() {
        if ($this->prev_step == 'step1') {
            $this->persistentData['include_payment_commitment'] = isset($this->include_payment_commitment) ? $this->include_payment_commitment : '';
            $this->persistentData['include_organization'] = isset($this->include_organization) ? $this->include_organization : '';
            $this->persistentData['account_code_mandatory'] = isset($this->account_code_mandatory) ? $this->account_code_mandatory : '';
            $this->persistentData['include_registration'] = isset($this->include_registration) ? $this->include_registration : '';
            $this->persistentData['account_name_optional'] = isset($this->account_name_optional) ? $this->account_name_optional : '';
            $this->persistentData['include_recaptcha'] = isset($this->include_recaptcha) ? $this->include_recaptcha : '';
            $this->persistentData['recaptcha_selected'] = isset($this->recaptcha_selected) ? $this->recaptcha_selected : '';
            $this->ensureSelectedFields(null);
        } else if ($this->prev_step = 'step5') {
            // If we come from step 5 (option back) save the parameters of the step
            $this->saveRequestParams(array(
                'web_header' => 'FORM_HEADER',
                'web_description' => 'FORM_DESCRIPTION',
                'web_submit' => 'FORM_SUBMIT_LABEL',
                'post_url' => 'FORM_WEB_POST_URL',
                'redirect_ok_url' => 'FORM_REDIRECT_OK_URL',
                'redirect_ko_url' => 'FORM_REDIRECT_KO_URL',
                'validate_identification_number' => 'VALIDATE_IDENTIFICATION_NUMBER',
                'payment_type' => 'PAYMENT_TYPE',
                'event_id' => 'EVENT_ID',
                'event_name' => 'EVENT_NAME',
                'email_template_id' => 'EMAIL_TEMPLATE_ID',
                'email_template_name' => 'EMAIL_TEMPLATE_NAME',
                'assigned_user_id' => 'ASSIGNED_USER_ID',
                'assigned_user_name' => 'ASSIGNED_USER_NAME',
                'web_footer' => 'FORM_FOOTER',
                'amount' => 'FORM_AMOUNT',
            ));
        }

        // build the list of fields to display
        $bean = BeanFactory::getBean('Contacts');
        $requiredFields = array();

        $requiredFields['email1'] = true; // The email field will be mandatory
        if ($this->persistentData['include_payment_commitment']) // If the form includes payment methods, the person's ID becomes mandatory
        {
            $requiredFields['stic_identification_number_c'] = true;
        }

        $this->mapModuleFields($bean, $requiredFields);

        // If you have to include organizations, the next one will be step 3, otherwise if you have to include registrations, the next step will be 4 or 5
        $nextStep = (!empty($this->persistentData['include_organization']) ? 'step3' : (!empty($this->persistentData['include_registration']) ? 'step4' : 'step5'));

        $this->mapStepNavigation('step2', $nextStep, 'step1');
        $this->view = 'Eventinscriptionselectfields';
    }

    /**
     * Third step controller (optional): choice of organization fields
     */
    public function actionStep3() {
        if ($this->prev_step = 'step5') // If we come from step 5 (option back) save the parameters of the step
        {
            $this->saveRequestParams(array(
                'web_header' => 'FORM_HEADER',
                'web_description' => 'FORM_DESCRIPTION',
                'web_submit' => 'FORM_SUBMIT_LABEL',
                'post_url' => 'FORM_WEB_POST_URL',
                'redirect_ok_url' => 'FORM_REDIRECT_OK_URL',
                'redirect_ko_url' => 'FORM_REDIRECT_KO_URL',
                'validate_identification_number' => 'VALIDATE_IDENTIFICATION_NUMBER',
                'payment_type' => 'PAYMENT_TYPE',
                'event_id' => 'EVENT_ID',
                'event_name' => 'EVENT_NAME',
                'email_template_id' => 'EMAIL_TEMPLATE_ID',
                'email_template_name' => 'EMAIL_TEMPLATE_NAME',
                'assigned_user_id' => 'ASSIGNED_USER_ID',
                'assigned_user_name' => 'ASSIGNED_USER_NAME',
                'web_footer' => 'FORM_FOOTER',
                'amount' => 'FORM_AMOUNT',
            ));
        }

        // Build the list of fields to display
        $bean = BeanFactory::getBean('Accounts');
        $requiredFields = array();
        if ($this->persistentData['account_code_mandatory']) {
            // Check if the account identification number is mandatory or not
            $requiredFields['stic_identification_number_c'] = true;
        }

        if ($this->persistentData['account_name_optional']) {
            $requiredFields['name'] = false;
        }

        $this->mapModuleFields($bean, $requiredFields);
        $this->view = 'EventInscriptionSelectFields';

        // If you have to include inscriptions, the next one will be step 4, otherwise, the next step will be 5
        $nextStep = (!empty($this->persistentData['include_registration']) ? 'step4' : 'step5');
        $this->mapStepNavigation('step3', $nextStep, 'step2');
    }

    /**
     * Fourth step controller: choice of registration fields
     */
    public function actionStep4() {
        // If we come from step 5 (option back) save the parameters of the step
        if ($this->prev_step = 'step5') {
            $this->saveRequestParams(array(
                'web_header' => 'FORM_HEADER',
                'web_description' => 'FORM_DESCRIPTION',
                'web_submit' => 'FORM_SUBMIT_LABEL',
                'post_url' => 'FORM_WEB_POST_URL',
                'redirect_ok_url' => 'FORM_REDIRECT_OK_URL',
                'redirect_ko_url' => 'FORM_REDIRECT_KO_URL',
                'validate_identification_number' => 'VALIDATE_IDENTIFICATION_NUMBER',
                'payment_type' => 'PAYMENT_TYPE',
                'event_id' => 'EVENT_ID',
                'event_name' => 'EVENT_NAME',
                'email_template_id' => 'EMAIL_TEMPLATE_ID',
                'email_template_name' => 'EMAIL_TEMPLATE_NAME',
                'assigned_user_id' => 'ASSIGNED_USER_ID',
                'assigned_user_name' => 'ASSIGNED_USER_NAME',
                'web_footer' => 'FORM_FOOTER',
                'amount' => 'FORM_AMOUNT',
            ));
        }

        // Build the list of fields to display
        $bean = BeanFactory::getBean('stic_Registrations');
        $requiredFields = array();
        $requiredFields['name'] = false;
        $requiredFields['registration_date'] = false;

        $this->mapModuleFields($bean, $requiredFields);

        $this->view = 'EventInscriptionSelectFields';

        // If organizations have to be included, the previous step will be step 3, otherwise the next step will be step 2
        $prevStep = (!empty($this->persistentData['include_organization']) ? 'step3' : 'step2');
        $this->mapStepNavigation('step4', 'step5', $prevStep);
    }

    /**
     * 5th step controller: form parameters
     * NOTE: It could be unified with the first step, but it is left like this for consistency in usability with the rest of the forms
     */
    public function actionStep5() {
        global $app_list_strings;

        // Generate the default shipping url
        if (empty($this->persistentData['FORM_WEB_POST_URL'])) {
            if ($this->persistentData['include_recaptcha']) {
                $this->persistentData['FORM_WEB_POST_URL'] = $this->getServerURL() . '/index.php?entryPoint=stic_Web_Forms_saveRecaptcha';
            } else {
                $this->persistentData['FORM_WEB_POST_URL'] = $this->getServerURL() . '/index.php?entryPoint=stic_Web_Forms_save';
            }
        }

        // If you have to include the payment methods, save the array with the available types
        if ($this->persistentData['include_payment_commitment']) {
            $objFP = BeanFactory::newBean('stic_Payment_Commitments');
            $this->view_object_map['PAYMENT_TYPE_OPTIONS'] = $app_list_strings[$objFP->field_defs['payment_type']['options']];
        }

        // Initialize the user parameter assigned to the form
        if (empty($this->persistentData['ASSIGNED_USER_ID'])) {
            global $current_user;
            $this->persistentData['ASSIGNED_USER_ID'] = $current_user->id;
            $this->persistentData['ASSIGNED_USER_NAME'] = $current_user->name;
        }

        // Load the data in case we come from step 6
        $this->view_object_map['FORM_WEB_POST_URL'] = $this->persistentData['FORM_WEB_POST_URL'];
        $this->view_object_map['FORM_HEADER'] = $this->persistentData['FORM_HEADER'] ?? '';
        $this->view_object_map['FORM_DESCRIPTION'] = $this->persistentData['FORM_DESCRIPTION'] ?? '';
        $this->view_object_map['FORM_SUBMIT_LABEL'] = $this->persistentData['FORM_SUBMIT_LABEL'] ?? '';
        $this->view_object_map['FORM_REDIRECT_OK_URL'] = $this->persistentData['FORM_REDIRECT_OK_URL'] ?? '';
        $this->view_object_map['FORM_REDIRECT_KO_URL'] = $this->persistentData['FORM_REDIRECT_KO_URL'] ?? '';
        $this->view_object_map['VALIDATE_IDENTIFICATION_NUMBER'] = $this->persistentData['VALIDATE_IDENTIFICATION_NUMBER'] ?? '';
        $this->view_object_map['PAYMENT_TYPE'] = $this->persistentData['PAYMENT_TYPE'] ?? '';
        $this->view_object_map['EVENT_ID'] = $this->persistentData['EVENT_ID'] ?? '';
        $this->view_object_map['EVENT_NAME'] = $this->persistentData['EVENT_NAME'] ?? '';
        $this->view_object_map['EMAIL_TEMPLATE_ID'] = $this->persistentData['EMAIL_TEMPLATE_ID'] ?? '';
        $this->view_object_map['EMAIL_TEMPLATE_NAME'] = $this->persistentData['EMAIL_TEMPLATE_NAME'] ?? '';
        $this->view_object_map['ASSIGNED_USER_ID'] = $this->persistentData['ASSIGNED_USER_ID'] ?? '';
        $this->view_object_map['ASSIGNED_USER_NAME'] = $this->persistentData['ASSIGNED_USER_NAME'] ?? '';
        $this->view_object_map['FORM_FOOTER'] = $this->persistentData['FORM_FOOTER'] ?? '';
        $this->view_object_map['FORM_AMOUNT'] = $this->persistentData['FORM_AMOUNT'] ?? '';
        $this->view = 'Eventinscriptionstep5';

        // If you have to include registrations, the previous step will be step 4, otherwise if you have to include organizations, the step will be 3, if not 2
        $prevStep = (!empty($this->persistentData['include_registration']) ? 'step4' : (!empty($this->persistentData['include_organization']) ? 'step3' : 'step2'));
        $this->mapStepNavigation('step5', 'StepFormat', $prevStep);
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
            'payment_type' => 'PAYMENT_TYPE',
            'event_id' => 'EVENT_ID',
            'event_name' => 'EVENT_NAME',
            'email_template_id' => 'EMAIL_TEMPLATE_ID',
            'email_template_name' => 'EMAIL_TEMPLATE_NAME',
            'assigned_user_id' => 'ASSIGNED_USER_ID',
            'assigned_user_name' => 'ASSIGNED_USER_NAME',
            'web_footer' => 'FORM_FOOTER',
            'amount' => 'FORM_AMOUNT',
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
        $this->view_object_map['FORM']['HEADER'] = array(
            'TEXT' => $this->persistentData['FORM_HEADER'],
        );

        // Description
        $this->view_object_map['FORM']['DESCRIPTION'] = array(
            'TEXT' => $this->persistentData['FORM_DESCRIPTION'],
        );

        // Form footer
        $this->view_object_map['FORM']['FOOTER'] = array(
            'TEXT' => $this->persistentData['FORM_FOOTER'],
        );

        // Define the fields that overwrite the obligatory property of each field
        $requiredFields = array();
        $requiredFields['Contacts']['email1'] = true;
        $requiredFields['Contacts']['stic_identification_number_c'] = $this->persistentData['include_payment_commitment']; // If the form includes payment methods, the person's ID becomes mandatory

        // The account identification number will be mandatory if it has been indicated to be. It is checked if the value has been indicated, since if it has not been indicated and is marked as false, the field will not be mandatory even if it is by definition
        if (isset($this->persistentData['account_code_mandatory']) && $this->persistentData['account_code_mandatory']) {
            $requiredFields['Accounts']['stic_identification_number_c'] = true;
        }

        if ($this->persistentData['account_name_optional']) {
            $requiredFields['Accounts']['name'] = false;
        }

        $requiredFields['stic_Registrations']['name'] = false; // Registration name will not be mandatory

        $reqFields = array(); // Array that will contain, after the call to prepareFieldsToResultForm, the required fields (the forced ones plus those of each module)
        $boolFields = array(); // Array that will contain, after the call to prepareFieldsToResultForm, the Boolean fields

        // Include the payment method fields
        $this->includePCFields();

        // Prepare the visible fields of the form
        $this->prepareFieldsToResultForm($requiredFields, $boolFields, $reqFields);

        // Prepare the list of required fields and Boolean
        $reqFields = (is_array($reqFields) && !empty($reqFields) ? implode(";", $reqFields) . ";" : "");
        $boolFields = (is_array($boolFields) && !empty($boolFields) ? implode(";", $boolFields) . ";" : "");

        // Prepare the form definition data
        $defParams = self::formatJsonData2HiddenField(array(
            'include_payment_commitment' => ($this->persistentData['include_payment_commitment'] ? 1 : 0),
            'include_organization' => ($this->persistentData['include_organization'] ? 1 : 0),
            'account_code_mandatory' => ($this->persistentData['account_code_mandatory'] ? 1 : 0),
            'include_registration' => ($this->persistentData['include_registration'] ? 1 : 0),
            'account_name_optional' => ($this->persistentData['account_name_optional'] ? 1 : 0),
            'email_template_id' => $this->persistentData['EMAIL_TEMPLATE_ID'],
            'include_recaptcha' => ($this->persistentData['include_recaptcha'] ? 1 : 0),
            'recaptcha_configKeys' => ($this->persistentData['recaptcha_configKeys']),
            'recaptcha_selected' => ($this->persistentData['recaptcha_selected']),
        ));

        // Hidden Fields
        $this->view_object_map['FORM']['HIDDEN'] = array
            (
            array(
                'NAME' => 'event_id',
                'VALUE' => $this->persistentData['EVENT_ID'],
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
                'VALUE' => 'EventInscription',
            ),
            array(
                'NAME' => 'stic_Payment_Commitments___payment_type',
                'VALUE' => $this->persistentData['PAYMENT_TYPE'] ?? '',
            ),
            array(
                'NAME' => 'stic_Payment_Commitments___periodicity',
                'VALUE' => 'punctual',
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

        // If an amount has been included it is added as a hidden field
        if ($this->isFixedAmount()) {
            $decimalImport = str_replace(',', '.', $this->persistentData['FORM_AMOUNT']);
            $this->view_object_map['FORM']['HIDDEN'][] = array(
                'NAME' => 'stic_Payment_Commitments___amount',
                'VALUE' => $decimalImport,
            );
        }

        $this->mapStepNavigation('step6', 'StepDownload', 'step5');
        $this->view = 'EventInscriptionStepFormat';
    }

    /**
     * Loading the view to downloading the form
     */
    public function actionStepDownload() {
        parent::actionStepDownload();
        // Save the form submission url
        $this->view_object_map['FORM']['URL'] = $this->persistentData['FORM_WEB_POST_URL'];
        $this->view_object_map['FORM']['NUM_ATTACHMENT'] = $this->persistentData['num_attachment'];
        $this->view = 'Eventinscriptionstepdownload';
    }

    /**
     * Include the payment method fields in the array of selected fields
     */
    public function includePCFields() {
        if ($this->persistentData['include_payment_commitment']) {
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
                    $restrictedOptionList[$key] = $optionList[$key];
                }
            }

            $this->persistentData['SELECTED_FIELDS']['stic_Payment_Commitments']['COL1_FIELDS'] = array();

            // If the amount is not preset, include the field in the array of selected fields
            if (!$this->isFixedAmount()) {
                $this->persistentData['SELECTED_FIELDS']['stic_Payment_Commitments']['COL1_FIELDS'][] = array(
                    'name' => 'amount',
                    'script' => 'onkeypress="return isNumberKey(event)" onchange="formatCurrency(this)"',
                );
            }

            $this->persistentData['SELECTED_FIELDS']['stic_Payment_Commitments']['COL1_FIELDS'][] = array(
                'name' => 'payment_method',
                'options' => $restrictedOptionList,
                'script' => 'onChange="adaptPaymentMethod(this)"',
            );
            $this->persistentData['SELECTED_FIELDS']['stic_Payment_Commitments']['COL1_FIELDS'][] = array(
                'name' => 'bank_account',
                'hidden' => true,
                'script' => 'onChange="validateIBAN(this)"',
            );
            $this->persistentData['SELECTED_FIELDS']['stic_Payment_Commitments']['COL1_FIELDS'][] = array(
                'name' => 'banking_concept',
                'hidden' => false,
            );
        } else {
            unset($this->persistentData['SELECTED_FIELDS']['stic_Payment_Commitments']);
        }
    }

    /**
     * Indicates whether the amount field will be fixed or can be included by the user
     */
    protected function isFixedAmount() {
        return !empty($this->persistentData['FORM_AMOUNT']);
    }
}
