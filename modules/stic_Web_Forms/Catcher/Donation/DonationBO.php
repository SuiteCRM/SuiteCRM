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
 * Class that defines the logic of the donation model
 */
class DonationBO extends WebFormDataBO
{
    // Donor management result constants
    public const DONATOR_ERROR = 0;
    public const DONATOR_UNIQUE = 1;
    public const DONATOR_MULTIPLE = 2;
    public const DONATOR_NEW = 3;

    // Overwriting identifier arrays for value recovery
    protected $defFields = array('campaign_id', 'defParams', 'web_module', 'req_id'); // Array with the definition fields of any form

    // Store the bean associated with the campaign of the received request
    private $campaign = null;

    /**
     * Property access methods
     * @return Object The campaign related with  the campaign
     */
    public function getCampaign()
    {
        return $this->campaign;
    }

    /**
     * Get the assigned User ID
     * @return String The assigned user id
     */
    public function getAssignedUserId()
    {
        return $this->actionDefParams['assigned_user_id'];
    }

    /**
     * Decide which fields should be retrieved based on the form definition parameters
     * @return Array An array with the data to retrieve from the form
     */
    public function getFormFields()
    {
        $bean = null;
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Building a list of fields for the module [{$this->defParams['web_module']}]...");

        switch ($this->defParams['web_module']) {
            case 'Contacts':
                $bean = Beanfactory::getBean('Contacts');
                $this->requiredFormFields[] = 'stic_identification_number_c'; // Add the identification field to the mandatory fields
                break;
            case 'Accounts':
                $bean = Beanfactory::getBean('Accounts');
                $this->requiredFormFields[] = 'stic_identification_number_c'; // Add the identification field to the mandatory fields
                break;
        }

        // Add the possible fields to retrieve
        foreach ($bean->field_defs as $field_def) {
            $this->formFields[] = $field_def['name'];
        }

        return $this->formFields;
    }

    /**
     * In case of error returns a string with the error code
     * In case of success returns an empty string
     * In any case, the lastError property is updated
     * @return String A string with the error code or null
     */
    public function checkDefParams()
    {
        $this->campaign = null;

        // Check the target main module
        switch ($this->defParams['web_module']) {
            // Valid cases
            case 'Contacts':
            case 'Accounts':
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Main module [{$this->defParams['web_module']}]");
                break;
            // Invalid cases
            default:
                $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": Invalid value for main module [{$this->defParams['web_module']}]");
                return $this->returnCode('PARAM_ERROR_INVALID_MAIN_MODULE');
        }

        // Check that there is a campaign ID and that it is active
        if (empty($this->defParams['campaign_id'])) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": Campaign ID not received.");
            return $this->returnCode('PARAM_ERROR_MISSING_CAMPAIGN_ID');
        } else {
            $campaigns = Beanfactory::getBean('Campaigns');
            $this->campaign = $campaigns->retrieve($this->defParams['campaign_id']); // Retrieve the campaign ID
            if (empty($this->campaign) || $this->campaign->status == 'Inactive') {
                $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": Unable to retrieve campaign data or it is inactive [ID {$this->defParams['campaign_id']}].");
                return $this->returnCode('PARAM_ERROR_INVALID_CAMPAIGN');
            } else {
                // If the campaign is active, return ok
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Campaign is active.");
                return $this->returnCode();
            }
        }
    }

    /**
     * In case of error returns a string with the error code
     * In case of success returns an empty string
     * In any case, the lastError property is updated
     * @return String A string with the error code or null
     */
    public function checkParams()
    {
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

        // Define those required based on the target module.
        $this->formParams['stic_identification_number_c'] = trim($this->formParams['stic_identification_number_c']);
        switch ($this->defParams['web_module']) {
            case 'Contacts':
                $moduleRequiredFields = array('last_name', 'stic_identification_number_c');
                $idTax = $this->formParams['stic_identification_number_c'];
                break;
            case 'Accounts':
                $moduleRequiredFields = array('name', 'stic_identification_number_c');
                $idTax = $this->formParams['stic_identification_number_c'];
                break;
            default:
                $moduleRequiredFields = array();
                break;
        }

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Required fields for the module [{$this->defParams['web_module']}]: " . var_export($moduleRequiredFields, true));

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

        // Validate other required fields and parameters
        $requiredParams = array_unique(array_merge($requiredParams, $moduleRequiredFields));

        // If identification number is not required, remove it from validation array.
        if (!empty($_REQUEST["unrequire_identification_number"]) && $_REQUEST["unrequire_identification_number"] == 1) {
            if (($key = array_search('stic_identification_number_c', $requiredParams)) !== false) {
                unset($requiredParams[$key]);
            }
        }

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Total required fields/params: " . var_export($requiredParams, true));
        foreach ($requiredParams as $param) {
            if (empty($this->formParams[$param])) // If a mandatory field is empty returns error
            {
                $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": Required field/param [{$param}] is missing or empty.");
                return $this->returnCode('PARAM_ERROR_MISSING_REQUIRED_FIELD');
            }
        }

        // Check/Set if identification number should be validated (default is yes)
        if (!isset($_REQUEST["validate_identification_number"])) {
            $_REQUEST["validate_identification_number"] = '1';
        }
        // Identification number validation
        if ($_REQUEST["validate_identification_number"] == '1') {
            // If identification type is not set or it is a NIF/NIE, validate it. In other case, don't validate
            if ((empty($this->formParams['stic_identification_type_c'])
                || $this->formParams['stic_identification_type_c'] == 'nif'
                || $this->formParams['stic_identification_type_c'] == 'nie')
                && !self::checkTaxIdentity($idTax)) {
                $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ":  The tax identification number [{$idTax}] is not valid.");
                return $this->returnCode('PARAM_ERROR_INVALID_IDENTIFICATION_NUMBER');
            }
        }
    }

    /**
     * Returns the identifier
     * @param ref $objToLink Output parameter will contain the object to which the donation will be linked
     * @param ref $objCandidates Output parameter will contain the array of candidates to whom the donation could be linked. (In the last position of the array will be the object generated from the data of the web form)
     * @return Integer 0 Error, 1 Single, 2 Multiple, 3 New, 4 Single record but with data discrepancy
     */
    public function getDonator(&$objToLink, &$objCandidates)
    {
        /*
        1. Search for the identifier in the indicated module
        2. If it exists and is unique, this is returned
        3. If there is and there is more than one, one is chosen and an email is sent warning of the duplicity
        4. If it does not exist, a new record is created and notified by mail
         */

        // Retrieve a bean from the target class
        $bean = Beanfactory::getBean($this->defParams['web_module']);
        $idTaxName = $this->defParams['web_module'] == 'Contacts' ? 'stic_identification_number_c' : 'stic_identification_number_c';
        $idTax = $this->formParams[$idTaxName];

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Searching {$this->defParams['web_module']} with {$idTaxName} = {$idTax}...");

        // Try to retrieve the record by the tax ID
        $objCandidates = $bean->get_full_list("name", "{$idTaxName} = '{$idTax}'");

        $objToLink = null; // Create the variable where we will link the payment
        $nObjects = 0;
        $ret = self::DONATOR_ERROR; // By default there is an error

        if ($objCandidates == null) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  No match was found, we created a new object.");
            $objToLink = $this->newBeanFromParams($this->defParams['web_module']);
            $objCandidates = array($objToLink); // Create the array of candidates to add the object created in the last position
            $ret = self::DONATOR_NEW;
        } else {
            // Count the number of results obtained
            $nCandidates = count($objCandidates);
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Found {$nCandidates} {$this->defParams['web_module']}.");
            $objToLink = reset($objCandidates); // If we have more than one match, let's take the first one and link the campaign

            // Overwrite the $Bean of the resulting donor, since when coming from a query get_full_list does not include the email address
            $objToLink = BeanFactory::getBean($this->defParams['web_module'], $objToLink->id);

            $objWeb = $this->newBeanFromParams($this->defParams['web_module'], false); // Generate a new object from the information received
            array_push($objCandidates, $objWeb); // Add the web object to the last position of the array

            if ($nCandidates > 1) {
                // If there is more than one candidate, return DONATOR_MULTIPLE
                $ret = self::DONATOR_MULTIPLE;
            } else {
                // If there are no discrepancies we will return DONATOR_UNIQUE
                $ret = self::DONATOR_UNIQUE;
            }
        }

        $this->objectsCreated[$this->defParams['web_module']] = $objToLink;

        // Link the campaign
        $this->linkToCampaign($objToLink);

        // Create the person/organization relationship
        $this->createRelationship($objToLink);

        return $ret;
    }

    /**
     * Method that links the object (Contact or Account to the campaign)
     * @param ref $objToLink Output parameter will contain the object to which the campaign will be linked
     */
    private function linkToCampaign($objLink)
    {
        global $timedate;

        //create campaign log
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Creating campaign log ...");
        $camplog = new CampaignLog();
        $camplog->campaign_id = $this->campaign->id;
        $camplog->related_id = $objLink->id;
        $camplog->related_type = $objLink->module_dir;
        $camplog->activity_type = mb_strtolower($objLink->object_name);
        $camplog->target_type = $objLink->module_dir;
        $camplog->activity_date = mb_strtolower($timedate->now());
        $camplog->target_id = $objLink->id;

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Saving object...");
        $camplog->save();

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Loading object relationships {$objLink->module_name} - {$objLink->object_name}...");
        $objLink->load_relationship('campaigns');
        $objLink->campaigns->add($camplog->id);

        return $objLink;
    }

    /**
     * Method that creates the relationship with the new donor (Contact or Account)
     * @return Object The relationship type of the donor
     */
    private function createRelationship($objToLink)
    {
        $defParams = $this->getDefParams();
        if (!isset($defParams['decodedDefParams'])
            || !isset($defParams['decodedDefParams']['relation_type'])
            || empty($defParams['decodedDefParams']['relation_type'])) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  No relationship has been defined to add.");
            return null;
        } else {
            $relType = $defParams['decodedDefParams']['relation_type'];
            switch ($objToLink->module_name) {
                case 'Contacts':
                    $relName = 'stic_contacts_relationships_contacts';
                    break;
                case 'Accounts':
                    $relName = 'stic_accounts_relationships_accounts';
                    break;
                default:
                    $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Invalid value for module name [{$objToLink->module_name}]");
                    return false;
            }

            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Loading relationship [{$relName}] ...");
            if (!$objToLink->load_relationship($relName)) {
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  The relationship could not be loaded.");
                return false;
            }

            $rel = self::hasActiveRelationship($objToLink, $relName, $relType);
            if ($rel) {
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  There are already active relationships, no new relationship will be generated.");
            } else {
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  No active relationships were found, creating new relationship ...");

                $rel = $this->newRelationship($objToLink, $relType);
                if (empty($rel)) {
                    $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ":  The new relationship could not be generated.");
                } else {
                    $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Adding relationship created ...");
                    // 20230908 EPS The relationship bewtween Contact and sticContactRelationships is created in the previous step alongside the new relationship
                    // STIC#1209
                    // $objToLink->$relName->add($rel->id);
                    // END EPS
                    $this->objectsCreated[$relType] = $rel;
                }
            }
            return $rel;
        }
    }

    /**
     * Generates a new relationship object to link it to the person or organization
     * @param SugarBean $contact Linked Person / Organization Object
     * @param string $type Type of relationship (field value)
     * @return boolean|stic_contacts_relationships
     */
    private function newRelationship($bean, $type)
    {
        global $app_list_strings, $timedate;
        switch ($bean->module_name) {
            case 'Contacts':
                $newBeanName = 'stic_Contacts_Relationships';
                //EPS 20230908 We relate the records on the record creation: we need to know the field name
                // STIC#1209
                $relationshipField = 'stic_contacts_relationships_contactscontacts_ida';
                // END EPS
                break;
            case 'Accounts':
                $newBeanName = 'stic_Accounts_Relationships';
                // EPS 20230908 We relate the records on the record creation: we need to know the field name
                // STIC#1209
                $relationshipField = 'stic_accounts_relationships_accountsaccounts_ida';
                // END EPS
                break;
            default:
                $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Invalid value for module name [{$bean->module_name}]");
                return false;
        }

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Generating new relationship object [{$newBeanName}] with relationship value [{$type}]... ");
        $newRel = BeanFactory::newBean($newBeanName);

        // Override automatic user setting. This will allow to set specific users for created_by and modified_user_id.
        $newRel->set_created_by = false;
        $newRel->update_modified_by = false;

        // Set users for the new record
        $newRel->created_by = $this->actionDefParams['assigned_user_id']; // The creator of the form is assigned as creator of the record
        $newRel->modified_user_id = $newRel->created_by;
        $newRel->assigned_user_id = $newRel->created_by;

        // Set values for other fields
        $relTypeName = $app_list_strings[$bean->field_defs['stic_relationship_type_c']['options']][$type];
        $newRel->name = "{$bean->name} - {$relTypeName}";
        $newRel->start_date = $timedate->now();
        $newRel->relationship_type = $type;

        // EPS 20230908: We relate the object with the contact or account
        // STIC#1209
        $newRel->$relationshipField = $bean->id;
        // END EPS

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Name of the new relationship [{$newRel->name}] start date [{$newRel->start_date}] type [{$newRel->relationship_type}].");

        // Save the record
        $newRel->save();

        return $newRel;
    }

    /**
     * It returns an active relationship of the type indicated or false in case of finding none
     * It is a prerequisite to have called the load_relationship method of the bean before calling the method
     * @param Object $bean Main object in which active relationships are sought
     * @param unknown $relName Relationship Name
     * @param unknown $relType Type of relationship to look for (field value)
     * @return unknown|boolean
     */
    private static function hasActiveRelationShip($bean, $relName, $relType)
    {
        // Search for active relationships
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Looking for active type relationships [{$relType}]...");

        $relatedBeans = $bean->$relName->getBeans();
        if (!empty($relatedBeans)) {

            // Include the file and the class of the related module
            $relModuleName = array_values($relatedBeans)[0]->object_name;
            include_once 'modules/' . $relModuleName . '/Utils.php';
            $class = $relModuleName . 'Utils';

            foreach ($relatedBeans as $relBean) {
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Checking relationship [{$relBean->relationship_type}] == [{$relType}]");
                if ($relBean->relationship_type == $relType && $relBean->active) {
                    $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Active relationship found [{$relBean->id} - {$relBean->name}].");
                    return $relBean;
                }
            }
        }
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  No active relationship found.");
        return false;
    }

    /**
     * Validates that the code entered is a valid identification number
     * @return boolean True if is valid the tax identification number
     */
    public static function checkTaxIdentity($str)
    {
    
        include_once 'SticInclude/Utils.php';
        return SticUtils::isValidNIForNIE($str) || SticUtils::isValidCIF($str);

    }
}
