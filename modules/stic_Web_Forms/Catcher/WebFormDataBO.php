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
 * Class with the data model methods of the web forms.
 */
class WebFormDataBO
{
    // Results constants of the contact's management
    public const CONTACT_ERROR = 0;
    public const CONTACT_UNIQUE = 1;
    public const CONTACT_MULTIPLE = 2;
    public const CONTACT_NEW = 3;

    /**
     * Identifier arrays for value recovery
     */
    protected $actionDefFields = array('redirect_url', 'assigned_user_id', 'webFormClass', 'redirect_ko_url'); // Array with invariant form definition fields
    protected $defFields = array(); // Array with the definition fields of any form
    protected $formFields = array(); // Array with the expected field identifiers of the form
    protected $requiredActionFields = array('assigned_user_id', 'webFormClass');
    protected $requiredDefFields = array();
    protected $requiredFormFields = array(); // Array indicating the required fields of the form

    /**
     * Arrays that will contain the values ​​of the parameters received
     */
    protected $actionDefParams = array(); // Array with the mandatory definition values ​​for any form formulario
    protected $defParams = array(); // Array with the form definition values ​​received
    protected $formParams = array(); // Array with the values ​​of the form data fields
    protected $lastError = ''; // Store the last error generated

    /**
     * Properties to manage the language of the messages
     */
    protected $lang = 'en_US'; // Language for user responses
    protected $mod_strings = null; // Module labels
    protected $app_strings = null; // Application labels
    protected $defaultModule = 'stic_Web_Forms'; // Default module to search for labels

    protected $objectsCreated = array();

    public function getObjectsCreated() {
        return $this->objectsCreated;
    }

    /**
     * Class constructor
     */
    public function __construct()
    {
        $this->loadLanguage();
    }

    /**
     * Detect the browser language and load the corresponding tags.
     */
    private function loadLanguage()
    {
        if (!empty($_REQUEST['language'])) {
            $this->lang = $_REQUEST['language'];
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Indicating language [{$this->lang}] from form.");
        } else {
            $_SERVER["HTTP_ACCEPT_LANGUAGE"] =  $_SERVER["HTTP_ACCEPT_LANGUAGE"] ?? '';
            $http_lang = substr($_SERVER["HTTP_ACCEPT_LANGUAGE"], 0, 2);
            switch ($http_lang) {
                case 'es':
                    $this->lang = 'es_ES';
                    break;
                case 'ca':
                    $this->lang = 'ca_ES';
                    break;
                case 'en':
                default:
                    $this->lang = 'en_us';
                    break;
            }
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Indicating language [{$_SERVER["HTTP_ACCEPT_LANGUAGE"]} -> {$http_lang} -> {$this->lang}] from browser parameters.");
        }

        $this->app_strings = return_application_language($this->lang); // Load application tags
        $this->mod_strings[$this->defaultModule] = return_module_language($this->lang, $this->defaultModule, false); // Load the module labels by default
    }

    /**
     * Returns the text of a label in the user's language
     * @return String The text of a label in the user's language
     */
    protected function getMsgString($key, $mod = null)
    {
        // If you receive a module and we do not have it loaded we will load it
        if (!empty($mod) && empty($this->mod_strings[$mod])) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Loading {$mod} module tags...");
            $this->mod_strings[$mod] = return_module_language($this->lang, $mod);
        }

        // If we have not passed a module we use the default module
        if (empty($mod)) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Using the default module, {$this->defaultModule}");
            $mod = $this->defaultModule;
        }

        // We first look for the label in the module list, if it is not, we look in the application and if it is not returned an empty string
        if (!empty($this->mod_strings[$mod][$key])) {
            return $this->mod_strings[$mod][$key];
        } else if (!empty($this->app_strings[$key])) {
            return $this->app_strings[$key];
        } else {
            return '';
        }
    }

    /**
     * Returns the last error
     */
    public function getLastError()
    {
        return $this->lastError;
    }

    /**
     * Field definition access methods
     */
    public function getActionDefFields()
    {
        return $this->actionDefFields;
    }

    public function getDefFields()
    {
        return $this->defFields;
    }

    public function getFormFields()
    {
        return $this->formFields;
    }

    /**
     * Methods of access to form parameters
     */
    public function getActionDefParams()
    {
        return $this->actionDefParams;
    }

    public function setActionDefParams($params)
    {
        $this->actionDefParams = $params;
    }

    public function getDefParams()
    {
        return $this->defParams;
    }

    public function setDefParams($params)
    {
        $this->defParams = $params;
    }

    public function getFormParams()
    {
        return $this->formParams;
    }

    public function setFormParams($params)
    {
        $this->formParams = $params;
    }

    public function getRequiredFields()
    {
        return $this->requiredFormFields;
    }

    public function setRequiredFields($fields)
    {
        $this->requiredFormFields = $fields;
    }

    public function returnCode($code = '')
    {
        return $this->lastError = $code;
    }

    /**
     * Check that the mandatory form definition parameters are correct
     * In case of error returns a string with the error code
     * In case of success returns an empty string
     * In any case, the lastError property is updated
     * @return String
     */
    public function checkActionDefParams()
    {
        /*
         * 1. Check the mandatory data
         * 2. Check that the user id is valid
         */

        // Check that the definition data does not come empty
        $ret = $this->checkNotEmpty($this->requiredActionFields, $this->actionDefParams);
        // If there is an error
        if ($ret) {
            return $ret;
        } else {
            // Check the user ID
            $user = Beanfactory::getBean('Users');
            $user = $user->retrieve($this->actionDefParams['assigned_user_id']);

            // If the user is not found, error is returned
            if (!$user) {
                $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ":  There is no user with id [{$this->actionDefParams['assigned_user_id']}].");
                return $this->returnCode('PARAM_ERROR_INVALID_ASSIGNED_USER');
            }

            return $this->returnCode();
        }
    }

    /**
     * In case of error returns a string with the error code
     * In case of success returns an empty string
     * In any case, the lastError property is updated
     */
    public function checkParams()
    {
        return $this->checkNotEmpty($this->requiredFormFields, $this->formParams);
    }

    /**
     * In case of error returns a string with the error code
     * In case of success returns an empty string
     * In any case, the lastError property is updated
     */
    public function checkDefParams()
    {
        return $this->checkNotEmpty($this->requiredDefFields, $this->defParams);
    }

    /**
     * Generates a new record in the indicated module from the object parameters
     * @param $module Indicates the module where the record will be created
     * @param $save (Optional) Indicates whether they should be saved in the database automatically
     * @param $prefix (Optional) Indicates the prefix for the fields sent in the form. Only fields that include this prefix will be used and will be removed from the field name to add it to the bean.
     * @return Object
     */
    public function newBeanFromParams($module, $save = true, $prefix = '')
    {
        require_once "SticInclude/Utils.php";
        $bean = Beanfactory::newBean($module);

        // Override automatic user setting. This will allow to set specific users for created_by and modified_user_id.
        $bean->set_created_by = false;
        $bean->update_modified_by = false;

        // Set users for the new record
        $bean->created_by = $this->actionDefParams['assigned_user_id']; // The creator of the form is assigned as creator of the record
        $bean->modified_user_id = $bean->created_by;
        $bean->assigned_user_id = $bean->created_by;

        // Assign the form values to the bean
        foreach ($this->formParams as $name => $value) {

            // If the field name has a prefix, remove it
            if (!empty($prefix) && (strpos($name, (string) $prefix) === 0)) {
                $fieldName = preg_replace("/{$prefix}/Au", '', $name);
            } else if (!$prefix) {
                $fieldName = $name;
            } else {
                // Not all form provided values might belong to the same module,
                // so we need to provide a loop way out for those cases
                continue;
            }
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Provided name [{$name}] - Bean name [{$fieldName}]");

            // If a datetime field is present and has some value, we convert it to GMT using the timezone of the user filling the form
            if ($bean->field_name_map[$fieldName]['type'] == 'datetimecombo' && $value) {
                $tz = $_REQUEST['timeZone'] ? $_REQUEST['timeZone'] : 'GMT';
                $hourField = $module . '___' . $fieldName . '___h';
                $minuteField = $module . '___' . $fieldName . '___m';
                $hours = $_REQUEST[$hourField] ? SticUtils::fillLeft($_REQUEST[$hourField], '0', 2) : '00';
                $minutes = $_REQUEST[$minuteField] ? SticUtils::fillLeft($_REQUEST[$minuteField], '0', 2) : '00';
                $datetimeString = $value . ' ' . $hours . ':' . $minutes;
                $userTimezone = new DateTimeZone($tz);
                $gmtTimezone = new DateTimeZone('GMT');
                $myDateTime = new DateTime($datetimeString, $userTimezone);
                $myDateTime->setTimezone($gmtTimezone);
                $value = $myDateTime->format('Y-m-d H:i:s');
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Datetime field conversion", $fieldName, $value, $tz, $datetimeString);
            }

            // Set the form provided value to the bean field
            $bean->$fieldName = $value;
        }

        if ($save) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Saving generated bean...");
            $bean->save();
        }
        return $bean;
    }

    /**
     * Returns the array of beans that match the indicated mail.
     * @param unknown $email Email address to search
     * @param unknown $module (String|Array) Indicates the modules for which you want to recover the beans.
     * For example, if 'Contacts' is indicated, only the Beans of the Contacts module are returned.
     * An array can also be passed, in which case the beans of the modules indicated in the array are returned.
     * If nothing is indicated, beans of any type are returned.
     * @return Array
     */
    protected function getBeansByEmail($email, $module = null)
    {
        $module_search = '';
        if (!empty($module)) {
            if (is_array($module)) {
                $module_search = '|' . implode('|', $module) . '|';
            } else {
                $module_search = "|{$module}|";
            }
        }

        $ret = array(); // If nothing is found, an empty array will be returned

        $emails_addres = new SugarEmailAddress();
        $beans = $emails_addres->getBeansByEmailAddress($email);

        if (empty($module_search)) {
            // If the data does not have to be processed, and the beans array has data, it can already be returned
            $ret = $beans;
        } else {
            foreach ($beans as $bean) {
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Bean Id [{$bean->id}] [{$bean->name}] [{$bean->module_name}] [$bean->deleted]");
                /*
                If the bean is of any of the required types, add it to the return array
                The getBeansByEmailAddress method can return empty Beans in case the address is not marked deleted and the Contact / Organization is deleted. In those cases a completely empty bean returns (with deleted == 0) so, to avoid problems, we discard the beans that come with an empty id.
                 */
                if (strpos("|{$bean->module_name}|", $module_search) !== false && !empty($bean->id)) {
                    $ret[] = $bean;
                }
            }
        }
        return $ret;
    }

    /**
     * Check that the fields passed by parameter are not empty
     */
    protected function checkNotEmpty($requiredNames, $paramsArray)
    {
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Validating the following mandatory fields: " . var_export($requiredNames, true));
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": REQUEST parameters: " . var_export($_REQUEST, true));
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Array parameters: " . var_export($paramsArray, true));

        foreach ($requiredNames as $name) {
            // 20230703 EPS: Amount 0 was signaled, incorrectly, as an error
            // STIC#1151
            // if (empty($paramsArray[$name])) {
            if (empty($paramsArray[$name]) && $paramsArray[$name] !== '0') {
            // END
                $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ":  The required field/param [{$name}] is missing or empty.");
                return $this->returnCode('PARAM_ERROR_MISSING_PARAMETER');
            }
        }
        return $this->returnCode();
    }

    /**
     * Return the url if everything went well
     */
    public function getOKURL()
    {
        return $this->getParam('redirect_url');
    }

    /**
     * It returns the url if something has failed
     */
    public function getKOURL()
    {
        return $this->getParam('redirect_ko_url');
    }

    public function cleanUpUrl($url, $forceHttps = false)
    {
        if ($url !== null && $forceHttps && !str_starts_with(strtolower($url), "https://")) {
            $parsedUrl = parse_url($url);
            if (!isset($parsedUrl["scheme"])) {
                $url = "https://{$url}";
            } else {
                // Replace scheme with https
                $pos = strpos(strtolower($url), strtolower($parsedUrl["scheme"]));
                $url = substr_replace($url, "https", $pos, strlen($parsedUrl["scheme"]));
            }
        }
        return $url;
    }

    /**
     * First it searches if it has a value indicated in the parameters, (in ACTION, DEFS and FORMS)
     * if you have it returns that, otherwise the one that has indicated as object parameter.
     * If there are none returns null.
     */
    public function getParam($paramKey)
    {
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Looking for {$paramKey} in -> " . print_r($this->actionDefParams, true));
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Looking for {$paramKey} in -> " . print_r($this->defParams, true));
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Looking for {$paramKey} in -> " . print_r($this->formParams, true));
        if (!empty($this->actionDefParams[$paramKey])) {
            return $this->actionDefParams[$paramKey];
        } else if (!empty($this->defParams[$paramKey])) {
            return $this->defParams[$paramKey];
        } else if (!empty($this->formParams[$paramKey])) {
            return $this->formParams[$paramKey];
        } else if (!empty($this->$paramKey)) {
            return $this->$paramKey;
        } else {
            return null;
        }
    }

    /**
     * Add the fields of the specified module to the possible fields to retrieve
     */
    protected function addModule2FormFields($module)
    {
        $bean = Beanfactory::getBean($module);
        foreach ($bean->field_defs as $field_def) {
            $this->formFields[] = "{$module}___{$field_def['name']}";
        }
    }

    /**
     * This method is used when custom_contacts_matching field is set. In this case, the fields set in
     * "custom_contacts_matching" will be used for finding pre-existing contacts.
     * @param ref $objToLink Output parameter, will contain the contact object to which the registration/donation will be linked
     * @param ref $objCandidates Output parameter, will contain the array of candidates to whom the registration could be linked
     *                           (In the last position of the array will be the object generated from the web form data)
     * @return 0 Error, 1 Single, 3 New
     */
    public function getCustomContactMatching(&$objToLink, &$objCandidates)
    {
        /*
        1. Candidates are searched using the fields received in the custom_contacts_matching param. Case insensitive.
        2. If no candidate is found, a new contact is created.
        3. If at least one candidate is found, the most recent is selected.
         */

        $objToLink = $objCandidates = null;
        $ret = self::CONTACT_ERROR; // By default there is an error

        // Prepare the WHERE clause based on custom_contacts_matching fields
        $customMatching = str_replace(' ', '', $_REQUEST['custom_contacts_matching']);
        $customSearchWhere = ' ';
        foreach (explode(',', $customMatching) as $customFieldvalue) {
            $concatValue .= $_REQUEST['Contacts___' . trim($customFieldvalue)];
        }
        $customSearchWhere = " CONCAT_WS('',{$customMatching}) = '{$concatValue}'";

        // Build the whole query
        if ($ret == self::CONTACT_ERROR) // This indicates that it has not been found yet.
        {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Retrieving list of people using custom matching [{$customMatching}] ...");

            $sqlCustomMatching = "SELECT
                                    c.*,
                                    cc.*,
                                    ea.email1
                                FROM
                                    contacts c
                                    JOIN contacts_cstm cc ON c.id = cc.id_c
                                    LEFT JOIN email_addr_bean_rel er ON er.bean_id = c.id AND er.deleted = 0
                                    AND er.bean_module = 'Contacts'
                                    LEFT JOIN (select id, deleted, email_address as email1 from email_addresses) ea ON er.email_address_id = ea.id AND ea.deleted = 0
                                WHERE
                                    c.deleted = 0
                                    AND {$customSearchWhere}
                                ORDER BY
                                    date_entered DESC
                                limit 1 ";

            // Get the most recent candidate ID
            $firstCandidate = $GLOBALS['db']->fetchOne($sqlCustomMatching);
            $firstCandidateId = $firstCandidate['id'];
            
            $nCandidates = $firstCandidateId ? 1 : 0;

            // If there is no candidate...
            if (empty($nCandidates)) {
                                
                $objCandidates = [];

                // Generate the objWeb that will be used.
                // The Contacts module prefix should be added only in event registration forms.
                if (get_class($this) == 'EventInscriptionBO') {
                    $objWeb = $this->newBeanFromParams('Contacts', true, 'Contacts___');
                } else if (get_class($this) == 'DonationBO') {
                    $objWeb = $this->newBeanFromParams('Contacts', true);
                }

                $objToLink = $objWeb;
                $ret = self::CONTACT_NEW;

                array_push($objCandidates, $objWeb); // Add the web object to the last position of the array

                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": A new contact has been created: Id [{$objToLink->id}] - Full name [{$objToLink->first_name} {$objToLink->last_name}]");

            } else {

                // If there is any candidate, get its bean
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Found at least one candidate.");
                $firstCandidateBean = Beanfactory::getBean('Contacts', $firstCandidateId);
                $objCandidates = [$firstCandidateBean];

                $objToLink = $firstCandidateBean;
                $ret = self::CONTACT_UNIQUE;
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": An existing contact has been selected: Id [{$objToLink->id}] - Full name [{$objToLink->first_name} {$objToLink->last_name}]");

            }
        }

        return $ret;
    }
}
