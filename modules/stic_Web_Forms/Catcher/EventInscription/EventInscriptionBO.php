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

/**
 * Class that defines the logic of the event registration model
 */
class EventInscriptionBO extends WebFormDataBO
{
  

    // Results constants of the organization's management
    public const ACCOUNT_ERROR = 0;
    public const ACCOUNT_UNIQUE = 1;
    public const ACCOUNT_MULTIPLE = 2;
    public const ACCOUNT_NEW = 3;
    public const ACCOUNT_NO_DATA = 4; // There is no information about the organization

    /**
     * Overwriting identifier arrays for value recovery
     */
    protected $defFields = array('event_id', 'defParams', 'req_id'); // Array with the definition fields of any form

    // Stores the bean associated with the event of the received request
    protected $event = null;

    // Store the bean of the generated inscription
    protected $inscription = null;

    // Data storage properties of Contacts and Organizations
    protected $contactObject = null;
    protected $contactCandidates = null;
    protected $contactResult = self::CONTACT_ERROR;
    protected $accountObject = null;
    protected $accountCandidates = null;
    protected $accountResult = self::ACCOUNT_NO_DATA;


    public function getObjectsCreated() {
        return array(
            'Contacts' => $this->contactObject,
            'stic_Registrations' => $this->inscription,
            'Accounts' => $this->accountObject
        );
    }

    /**
     * Property access methods
     */
    public function getInscriptionObject()
    {
        return $this->inscription;
    }

    public function getContactObject()
    {
        return $this->contactObject;
    }

    public function getContactCandidates()
    {
        return $this->contactCandidates;
    }

    public function getContactResult()
    {
        return $this->contactResult;
    }

    public function getAccountObject()
    {
        return $this->accountObject;
    }

    public function getAccountCandidates()
    {
        return $this->accountCandidates;
    }

    public function getAccountResult()
    {
        return $this->accountResult;
    }

    public function getEvent()
    {
        return $this->event;
    }

    public function getAssignedUserId()
    {
        return $this->actionDefParams['assigned_user_id'];
    }

    /**
     * Decide which fields should be retrieved based on the form definition parameters
     * @return Array Returns an array with the data to retrieve from the form
     */
    public function getFormFields()
    {
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Building list of fields for event registration...");

        $defParams = $this->defParams['decodedDefParams'];

        // All person fields can be received
        $this->addModule2FormFields('Contacts');

        $this->requiredFormFields[] = 'Contacts___email1'; // The email address of the person is mandatory
        $this->requiredFormFields[] = 'Contacts___last_name'; // The person's last name is mandatory

        // If the registration includes a payment add the identification number as a required field unless explicitly unrequired in the form
        if ($defParams['include_payment_commitment'] && (isset($_REQUEST["unrequire_identification_number"]) && $_REQUEST["unrequire_identification_number"] != 1)) {
            $this->requiredFormFields[] = 'Contacts___stic_identification_number_c';
        }

        // If the form includes organizational data add the fields to retrieve
        if ($defParams['include_organization']) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Organization fields are added.");

            $this->addModule2FormFields('Accounts');
            if (empty($defParams['account_name_optional'])) {
                $this->requiredFormFields[] = 'Accounts___name';
            }

            // If the form requires the identification number add it as mandatory
            if ($defParams['account_code_mandatory']) {
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  The identification number of the organization is mandatory.");
                $this->requiredFormFields[] = 'Accounts___stic_identification_number_c';
            }
        }

        // If the form includes registration data add the fields to retrieve
        if ($defParams['include_registration']) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Event registration fields are added.");
            $this->addModule2FormFields('stic_Registrations');
        }

        return $this->formFields;
    }

    /**
     * In case of error returns a string with the error code
     * In case of success returns an empty string
     * In any case, the lastError property is updated
     * @return String
     */
    public function checkDefParams()
    {
        // Check that we have received the form definition parameters
        if (empty($this->defParams['defParams'])) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": defParams array is missing  [{$this->defParams['defParams']}]");
            return $this->returnCode('PARAM_ERROR_MISSING_DEF_PARAMS');
        }

        // Check that the form definition parameters are complete
        $defParams = $this->defParams['decodedDefParams'];
        if (!isset($defParams['include_payment_commitment']) ||
            !isset($defParams['include_organization']) ||
            !isset($defParams['account_code_mandatory']) ||
            !isset($defParams['include_registration']) ||
            !isset($defParams['email_template_id'])) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": At least one of the defParams (include_payment_commitment, include_organization, account_code_mandatory, include_registration, email_template_id) is missing [{$defParams}]");
            return $this->returnCode('PARAM_ERROR_MISSING_DEF_PARAM');
        }

        // Transform form definition parameters into class properties
        foreach ($defParams as $param => $value) {
            $this->defParams[$param] = $value;
        }

        // Check that there is an event ID and is valid
        if (empty($this->defParams['event_id'])) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Event ID is missing.");
            return $this->returnCode('PARAM_ERROR_MISSING_EVENT_ID');
        }

        // Retrieve event bean
        $events = Beanfactory::getBean('stic_Events');
        $this->event = $events->retrieve($this->defParams['event_id']);
        if (empty($this->event)) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Failed to retrieve event [ID {$this->defParams['event_id']}].");
            return $this->returnCode('PARAM_ERROR_INVALID_EVENT_ID');
        }

        // The parameters are valid
        return $this->returnCode();
    }

    /**
     * In case of error returns a string with the error code
     * In case of success returns an empty string
     * In any case, the lastError property is updated
     * @return String The result code
     */
    public function checkParams()
    {
        include_once 'SticInclude/Utils.php';

        /*
        1. Check that general required fields are available
        2. Check that form required fields (req_id param) are available
        3. Validate identification number
         */

        // For code legacy reasons, in this function there are several arrays
        // with required fields and parameters for proper webform data processing.
        // Some fields may even be part of more than one of these arrays.
        // Somewhere in the future this will be properly recoded for better understanding.

        // Initialize the required parameters with those indicated in the class
        $requiredParams = $this->requiredFormFields;

        // If there are required fields, add them to the validation
        $req_id_fields = array();
        if (!empty($this->defParams['req_id'])) {
            $req_id_fields = explode(';', trim($this->defParams['req_id'], ';'));
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Required fields according to req_id: " . var_export($req_id_fields, true));
        }

        // In webforms that send attached files req_id fields won't be analyzed because there is no way to relate
        // field names in req_id with files received in $_FILES. If req_id is analyzed, false positive errors will arise
        // and the data (which might be right) won't reach the CRM.
        // ToDo: Set rule names for file fields in web forms in order to be able to relate fields and files and properly manage this case
        // STIC#569
        if (count($_FILES) == 0 || join($_FILES['documents']['tmp_name']) == '') {
            // Both conditions are tested to ensure no false positives
            // Check that all fields in req_id have a value
            foreach ($req_id_fields as $key => $value) {
                // 20230703 EPS: Amount 0 was signaled, incorrectly, as an error
                // STIC#1151
                // if (empty($_REQUEST[$value])) {
                if (empty($_REQUEST[$value]) && $_REQUEST[$value] !== '0') {
                // END
                    $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": Required field/param [{$value}] is missing or empty.");
                    return $this->returnCode('PARAM_ERROR_MISSING_REQUIRED_FIELD');
                }
            }
        }

        // Validate other required parameters
        foreach ($requiredParams as $param) {
            if (empty($this->formParams[$param])) {
                // If a mandatory field is empty returns error
                $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": Required field/param [{$param}] is missing or empty.");
                return $this->returnCode('PARAM_ERROR_MISSING_REQUIRED_FIELD');
            }
        }

        // Check/Set if identification number should be validated (default is yes)
        if (!isset($_REQUEST["validate_identification_number"])) {
            $_REQUEST["validate_identification_number"] = '1';
        }

        if ($_REQUEST["validate_identification_number"] == '1') {
            // If identification type is not set or it is a NIF/NIE, validate it. In other case, don't validate
            $this->formParams['Contacts___stic_identification_number_c'] = trim($this->formParams['Contacts___stic_identification_number_c'] ?? '');
            if ($this->formParams['Contacts___stic_identification_number_c']
                && (empty($this->formParams['Contacts___stic_identification_type_c'])
                    || $this->formParams['Contacts___stic_identification_type_c'] == 'nif'
                    || $this->formParams['Contacts___stic_identification_type_c'] == 'nie')
                && !SticUtils::isValidNIForNIE($this->formParams['Contacts___stic_identification_number_c'])
            ) {
                $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ":  The person ID [{$this->formParams['Contacts___stic_identification_number_c']}] is not valid.");
                return $this->returnCode('PARAM_ERROR_INVALID_IDENTIFICATION_NUMBER');
            }

            // If organization's id is available, validate it
            $this->formParams['Accounts___stic_identification_number_c'] = trim($this->formParams['Accounts___stic_identification_number_c'] ?? '');
            if (!empty($this->formParams['Accounts___stic_identification_number_c']) &&
                !self::checkTaxIdentity($this->formParams['Accounts___stic_identification_number_c'])) {
                $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ":  The organization ID [{$this->formParams['Accounts___stic_identification_number_c']}] is not valid.");
                return $this->returnCode('PARAM_ERROR_INVALID_IDENTIFICATION_NUMBER');
            }
        }

        // The parameters are valid
        return $this->returnCode();
    }

    /**
     * Generate a new registration associated with the contact and organization
     * @return Object The inscription created
     */
    public function doInscription()
    // Retrieve the contact details to which the registration will be linked,
    // the method to obtain this contact is different if custom_contacts_matching field is set or not
    {
        if (isset($_REQUEST['custom_contacts_matching']) && !empty($_REQUEST['custom_contacts_matching'])) {
            $this->contactResult = $this->getCustomContactMatching($this->contactObject, $this->contactCandidates);
        } else {
            $this->contactResult = $this->getContact($this->contactObject, $this->contactCandidates);
        }

        /*
         * If information about the organization is incorporated, the data is recovered
         * The organization can be included in the form as an optional data, therefore, even if it is included
         * the module as a definition, if we have not received the name it is understood that no information has been specified
         * for the Organization and its information is not sought.
         */
        if ($this->defParams['include_organization'] && !empty($this->formParams['Accounts___name'])) {
            $this->accountResult = $this->getAccount($this->accountObject, $this->accountCandidates);
            // If it is a new contact try to link the organization
            if ($this->contactResult == self::CONTACT_NEW &&
                $this->accountResult != self::ACCOUNT_ERROR) {
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Linking Person [{$this->contactObject->id}] with Organization [{$this->accountObject->id}]...");
                $this->contactObject->account_id = $this->accountObject->id;
                $this->contactObject->save();
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Linking complete.");
            }
        } else {
            $this->accountResult = self::ACCOUNT_NO_DATA;
        }

        $this->inscription = $this->newInscription($this->event, $this->contactObject, $this->accountObject);
        return $this->inscription;
    }

    /**
     * Returns the result code of the operation, this method is used when custom_contacts_matching field is not set.
     * @param ref $objToLink Output parameter, will contain the Person object to which the inscription will be linked
     * @param ref $objCandidates Output parameter will contain the array of candidates to whom the registration could be linked
     *                           (In the last position of the array will be the object generated from the web form data)
     * @return 0 Error, 1 Single, 2 Multiple, 3 New, 4 Single record but with data discrepancy
     */
    protected function getContact(&$objToLink, &$objCandidates)
    {
        /*
        1. If you have an identification number, look for the candidates for that information.
        2. If it exists and is unique, this is returned
        3. If there is and there is more than one, choose one (if there is one that matches in the email, choose this one)
        4. If it does not exist, it is searched by the email address
        5. If it exists and is unique, it returns
        6. If there is and there is more than one, choose one
        7. If it does not exist, a new record is created
         */
        $objToLink = $objCandidates = null;
        $ret = self::CONTACT_ERROR; // By default there is an error
        $idTax = $this->formParams['Contacts___stic_identification_number_c'];
        $email = $this->formParams['Contacts___email1'];

        $contacts = Beanfactory::getBean('Contacts');
        if (!empty($idTax)) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Retrieving list of contacts with identification number = [{$idTax}] ...");
            $objCandidates = $contacts->get_full_list("name", "stic_identification_number_c = '{$idTax}'");

            // If you find people with that identification number, check how many you have found
            if ($objCandidates != null) {
                $nCandidates = count($objCandidates); // Count the number of results obtained
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Found {$nCandidates} candidates.");

                // If there are candidates, we will never generate a new object, so we can fill the objWeb with the received data (false) and not save the result in the database
                $objWeb = $this->newBeanFromParams('Contacts', false, 'Contacts___');

                if ($nCandidates == 1) {
                    // If there are no discrepancies we will return it
                    $objToLink = reset($objCandidates); // We retrieve the first element of the array
                    $ret = self::CONTACT_UNIQUE;
                } else {
                    // If there is more than one candidate we look for the one that matches the email (if there is one) CONTACT_MULTIPLE
                    foreach ($objCandidates as $obj) {
                        if ($obj->email1 == $email || $obj->email2 == $email) {
                            $objToLink = $obj;
                        }
                    }

                    if (!$objToLink) // If none were found, the first one is returned
                    {
                        $objToLink = reset($objCandidates);
                    }
                    $ret = self::CONTACT_MULTIPLE;
                }
            }
        }

        // If we do not have information about the identification number, or we have not found it, we look for it by email
        if ($ret == self::CONTACT_ERROR) // This indicates that it has not been found yet.
        {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Retrieving list of people with mail = [{$email}] ...");
            $objCandidates = $this->getBeansByEmail($email, 'Contacts');

            $nCandidates = count($objCandidates); // Count the number of results obtained
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Found {$nCandidates} candidates");

            // At this point in the search for duplicates it has already been verified that there is no match by identification number,
            // therefore if we find matches now by mail, the identification number only serves us to discard.
            $match = false;
            if (empty($idTax) && $nCandidates) {
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Tax id empty, the first candidate is assumed as valid.");
                $match = true;
            } else {
                $i = 0;
                while ($i < $nCandidates && !$match) {
                    // $idTax is informed but has not found matches, therefore we will assign it only if any of the registers does not have identification number informed either
                    if (empty($objCandidates[$i]->stic_identification_number_c)) {
                        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Found candidate without identification number [{$objCandidates[$i]->id}].");
                        $obj = array_slice($objCandidates, $i, 1);
                        unset($objCandidates[$i]);
                        array_unshift($objCandidates, $obj[0]);
                        $match = true;
                    } else {
                        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Candidate Id: [{$objCandidates[$i]->id}] rejected by difference in idTax [{$objCandidates[$i]->stic_identification_number_c}] != [{$idTax}].");
                    }
                    $i++;
                }
            }

            // It generates the objWeb and saves it in case there is no candidate Bean
            $objWeb = $this->newBeanFromParams('Contacts', $nCandidates == 0 || !$match, 'Contacts___');
            if ($nCandidates == 0 || !$match) {
                $objToLink = $objWeb;
                $ret = self::CONTACT_NEW;
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  A new contact has been created Id: [{$objToLink->id}] Full name [{$objToLink->first_name} {$objToLink->last_name}] ...");
            } else {
                $ret = ($nCandidates == 1 ? self::CONTACT_UNIQUE : self::CONTACT_MULTIPLE);
                $objToLink = reset($objCandidates); // We retrieve the first element of the array (for UNIQUE and MULTIPLE cases)
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  An existing contact has been selected Id: [{$objToLink->id}] Full name [{$objToLink->first_name} {$objToLink->last_name}] ...");
            }
        }
        array_push($objCandidates, $objWeb); // Add the web object to the last position of the array

        return $ret;
    }

    /**
     * Returns the result code of the operation
     * @param ref $objToLink Output parameter will contain the Organization object to which the inscription will be linked
     * @param ref $objCandidates Output parameter will contain the array of candidates to whom the registration could be linked
     *                           (In the last position of the array will be the object generated from the web form data)
     * @return 0 Error, 1 Single, 2 Multiple, 3 New
     */
    protected function getAccount(&$objToLink, &$objCandidates)
    {
        /*
        1. If you have stic_identification_number_c look for the organization for that data.
        2. If it exists and is unique, this is returned
        3. If there is and there is more than one, choose one (if there is one that matches the name, choose this one)
        4. If it does not exist, it is searched by name
        5. If it exists and is unique, it returns
        6. If there is and there is more than one, choose one
        7. If it does not exist, a new record is created
         */

        $ret = self::ACCOUNT_ERROR; // By default there is an error
        $idTax = $this->formParams['Accounts___stic_identification_number_c'];
        $name = $this->formParams['Accounts___name'];

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Account ID = [{$idTax}] ...");

        $accounts = Beanfactory::getBean('Accounts');
        if (!empty($idTax)) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Retrieving list of organizations with identification number = [{$idTax}] ...");
            $objCandidates = $accounts->get_full_list("name", "stic_identification_number_c = '{$idTax}'");

            // If you find organizations with that identification number, check how many you have found
            if ($objCandidates != null) {
                $nCandidates = count($objCandidates); // Count the number of results obtained
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Found {$nCandidates} candidates.");

                // If there are candidates, we will never generate a new object, so we can fill the objWeb with the received data (false) and not save the result in the database
                $objWeb = $this->newBeanFromParams('Accounts', false, 'Accounts___');

                if ($nCandidates == 1) {
                    // If there are no discrepancies we will return it
                    $objToLink = reset($objCandidates); // We retrieve the first element of the array
                    $ret = self::ACCOUNT_UNIQUE;
                } else {
                    // If there is more than one candidate we look for the one that matches the email (if there is one) ACCOUNT_MULTIPLE
                    foreach ($objCandidates as $obj) {
                        if ($obj->name == $name) {
                            $objToLink = $obj;
                        }
                    }

                    if (!$objToLink) // If none were found, the first one is returned
                    {
                        $objToLink = reset($objCandidates);
                    }
                    $ret = self::ACCOUNT_MULTIPLE;
                }
            }
        }

        // If we don't have information about the identification number, or we haven't found it, we look for it by name
        if ($ret == self::ACCOUNT_ERROR) // This indicates that it has not been found yet.
        {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Retrieving list of named organizations = [{$name}] ...");
            $db = DBManagerFactory::getInstance();
            $sqlName = $db->quote($name);
            $objCandidates = $accounts->get_full_list("name", "accounts.name = '{$sqlName}'");

            $nCandidates = count($objCandidates); // Count the number of results obtained
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Found {$nCandidates} candidates.");

            // It generates the objWeb and saves it in case there is no candidate Bean
            $objWeb = $this->newBeanFromParams('Accounts', $nCandidates == 0, 'Accounts___');
            if ($nCandidates == 0) {
                $objToLink = $objWeb;
                $ret = self::ACCOUNT_NEW;
            } else {
                $ret = ($nCandidates == 1 ? self::ACCOUNT_UNIQUE : self::ACCOUNT_MULTIPLE);
                $objToLink = reset($objCandidates); // We retrieve the first element of the array (for UNIQUE and MULTIPLE cases)
            }
        }

        if (empty($objCandidates)) {
            $objCandidates = array();
        }
        array_push($objCandidates, $objWeb); // Add the web object to the last position of the array
        return $ret;
    }

    /**
     * Genera una nueva inscripción y la vincula al Evento, Contacto y, opcionalmente, Organización indicada
     * @return objInscripcion
     */
    protected function newInscription($objEvent, $objContact, $objAccount = null)
    {
        global $timedate;
        global $mod_strings;
        $this->inscription = $inscription = null;

        if ($this->defParams['include_registration']) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Registration data is included, generating Bean from the parameters received...");
            $inscription = $this->newBeanFromParams('stic_Registrations', false, 'stic_Registrations___');
        } else {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  The parameters do not include registration data, generating standard Bean ...");
            $inscription = BeanFactory::newBean('stic_Registrations');
        }

        // Override automatic user setting. This will allow to set specific users for created_by and modified_user_id.
        $inscription->set_created_by = false;
        $inscription->update_modified_by = false;

        // Set users for the new record
        $inscription->created_by = $this->actionDefParams['assigned_user_id'];
        $inscription->modified_user_id = $inscription->created_by;
        $inscription->assigned_user_id = $inscription->created_by;

        // If it is not indicated, generate the name of the registration
        if (empty($inscription->name)) {
            $inscription->name = "{$objContact->name} - {$objEvent->name}";
        }

        // If the status has not been reported, it is assumed to be confirmed.
        if (empty($inscription->status)) {
            $inscription->status = 'confirmed';
        }

        // If the attendees quantity reported is less than one, its value will be one
        if ($inscription->attendees < 1) {
            $inscription->attendees = 1;
        }

        // If the type of participation has not been reported, it is assumed to be an assistant
        if (empty($inscription->participation_type)) {
            $inscription->participation_type = 'attendant';
        }

        // Set the current time in the registration date
        $inscription->registration_date = mb_strtolower($timedate->now());

        // We indicate the linked event
        $inscription->stic_registrations_stic_eventsstic_events_ida = $objEvent->id;
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  The event will be linked {$objEvent->id}.");
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Saving the inscription ...");

        // SinergiaTIC Alberto 06/11/2019 - Workflows are executed before relationships are saved using the
        // functions of "load_relationship" and, consequently, "add". Therefore, they cannot access the ID of the records they want
        // link, in this case Person and Organization. We use the method of directly assigning the Contact/Organization registration ID to
        // relationship field "stic_Registrations _......._ ida"
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Loading relationship with people ...");
        $inscription->stic_registrations_contactscontacts_ida = $objContact->id;
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Contact {$objContact->id} linked correctly.");

        if ($objAccount != null) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Loading relationship with organizations ...");
            $inscription->stic_registrations_accountsaccounts_ida = $objAccount->id;
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Organization {$objAccount->id} linked correctly.");
        }

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Saving changes in relationships ...");
        $inscription->save();
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Changes saved.");

        // Manage attached files
        require_once 'modules/Documents/Document.php'; // Call to the Document class.
        $total_files = (!empty($_FILES['documents']) && !empty($_FILES["documents"]["name"])) ? count($_FILES["documents"]["name"]) : 0; // Assignment of the number of attachments received from the form
        $objContact->load_relationship('documents'); // Loading of the relationship between the contact module and the document module

        // For each document received from the form
        for ($i = 0; $i < $total_files; $i++) {
            // If the document exists
            if ($_FILES["documents"]['name'][$i] != "") {
                // The document is created
                $document = new Document();
                $document->document_name = $_FILES["documents"]['name'][$i];
                $document->filename = $_FILES["documents"]['name'][$i];
                $document->doc_type = "Sugar";
                $document->status_id = "Active";
                $document->revision = 1;
                $document->assigned_user_id = $this->actionDefParams['assigned_user_id'];
                $document->set_created_by = false;
                $document->created_by = $this->actionDefParams['assigned_user_id'];
                $document->update_modified_by = false;
                $document->modified_user_id = $document->created_by;
                $document->description = $this->getMsgString('LBL_EVENT_ATTACHMENT_DESCRIPTION') . ' ' . $objEvent->name;
                $document->save();

                // Version 1 of the document that has just been generated is added
                $revision = new DocumentRevision();
                $revision->id = $document->document_revision_id;
                $revision->set_created_by = false;
                $revision->created_by = $this->actionDefParams['assigned_user_id'];
                $revision->update_modified_by = false;
                $revision->modified_user_id = $revision->created_by;
                $revision->not_use_rel_in_req = true;
                $revision->new_rel_id = $document->id;
                $revision->new_rel_relname = 'Documents';
                $revision->change_log = translate('DEF_CREATE_LOG', 'Documents');
                $revision->revision = 1;
                $revision->document_id = $document->id;
                $revision->doc_type = "Sugar";
                $revision->filename = $_FILES["documents"]['name'][$i];
                $revision->file_ext = pathinfo($_FILES["documents"]['name'][$i], PATHINFO_EXTENSION);
                $revision->file_mime_type = mime_content_type($_FILES["documents"]['tmp_name'][$i]);
                $revision->save();

                move_uploaded_file($_FILES["documents"]['tmp_name'][$i], "upload/$revision->id");
                chmod("upload/" . $revision->id, 0777);

                // Finally, the uploaded document is linked to the person used in the registration
                $objContact->documents->add($document->id);

                unset($document);
                unset($revision);
            }
        }

        return $inscription;
    }

    /**
     * Valid if an account identification number (cif) is valid
     * http://www.michublog.com/informatica/8-funciones-para-la-validacion-de-formularios-con-expresiones-regulares
     */
    private static function checkIdentificationNumberCif($cif)
    {
        $cif = strtoupper($cif);

        $cifRegEx1 = '/^[ABEH][0-9]{8}$/i';
        $cifRegEx2 = '/^[KPQS][0-9]{7}[A-J]$/i';
        $cifRegEx3 = '/^[CDFGJLMNRUVW][0-9]{7}[0-9A-J]$/i';

        if (preg_match($cifRegEx1, $cif) || preg_match($cifRegEx2, $cif) || preg_match($cifRegEx3, $cif)) {
            $control = $cif[strlen($cif) - 1];
            $sum_A = 0;
            $sum_B = 0;

            for ($i = 1; $i < 8; $i++) {
                if ($i % 2 == 0) {
                    $sum_A += intval($cif[$i]);
                } else {
                    $t = (intval($cif[$i]) * 2);
                    $p = 0;

                    for ($j = 0; $j < strlen($t); $j++) {
                        $p += substr($t, $j, 1);
                    }
                    $sum_B += $p;
                }
            }

            $sum_C = (intval($sum_A + $sum_B)) . "";
            $sum_D = (10 - intval($sum_C[strlen($sum_C) - 1])) % 10;

            $letters = "JABCDEFGHI";

            if ($control >= "0" && $control <= "9") {
                return ($control == $sum_D);
            } else {
                return (strtoupper($control) == $letters[$sum_D]);
            }
        } else {
            return false;
        }

    }

    /**
     * Validates that the code entered is an identification number valid (cif (Accounts), nif or nie (Contacts)))
     */
    public static function checkTaxIdentity($str)
    {
        include_once 'SticInclude/Utils.php';
        return SticUtils::isValidNIForNIE($str) || SticUtils::isValidCIF($str);
    }
}
