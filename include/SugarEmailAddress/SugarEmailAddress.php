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

require_once("include/JSON.php");


class SugarEmailAddress extends SugarBean
{
    const ERR_INVALID_REQUEST_NO_USER_PROFILE_PAGE_SAVE_ACTION = 1;
    const ERR_INVALID_REQUEST_NO_REQUEST = 2;
    const ERR_INVALID_REQUEST_NO_EMAIL_INFOS = 3;
    const ERR_INVALID_REQUEST_NO_VALID_EMAIL_ADDR_IN_REQUEST = 4;
    const ERR_INVALID_REQUEST_VALID_USER_IS_SET_BUT_NO_IN_REQUEST = 5;
    const ERR_PRIMARY_EMAIL_IS_NOT_SELECTED = 6;
    const ERR_REPLYTO_EMAIL_IS_NOT_SELECTED = 7;
    const ERR_INVALID_REQUEST_NO_VALID_USER_IN_REQUEST = 8;
    const ERR_INVALID_REQUEST_MORE_THAN_ONE_USER_IN_REQUEST = 9;
    const ERR_SOME_EMAILS_WERE_NOT_SAVED_OR_UPDATED = 10;

    // Opt In Flags (for Ticks)
    const COI_FLAG_OPT_IN = 'OPT_IN';
    const COI_FLAG_OPT_IN_DISABLED = 'OPT_IN_DISABLED';
    const COI_FLAG_OPT_IN_PENDING_EMAIL_CONFIRMED = 'OPT_IN_PENDING_EMAIL_CONFIRMED';
    const COI_FLAG_OPT_IN_PENDING_EMAIL_FAILED = 'OPT_IN_PENDING_EMAIL_FAILED';
    const COI_FLAG_OPT_IN_PENDING_EMAIL_NOT_SENT = 'OPT_IN_PENDING_EMAIL_NOT_SENT';
    const COI_FLAG_OPT_IN_PENDING_EMAIL_SENT = 'OPT_IN_PENDING_EMAIL_SENT';
    const COI_FLAG_OPT_OUT = 'OPT_OUT';
    const COI_FLAG_UNKNOWN_OPT_IN_STATUS = 'UNKNOWN_OPT_IN_STATUS';
    const COI_FLAG_INVALID = 'INVALID';
    const COI_FLAG_NO_OPT_IN_STATUS = 'NO_OPT_IN_STATUS';

    // Opt In Status
    const COI_STAT_DISABLED = 'not-opt-in';
    const COI_STAT_OPT_IN = 'opt-in';
    const COI_STAT_CONFIRMED_OPT_IN = 'confirmed-opt-in';

    /** @var boolean $tracker_visibility */
    public $tracker_visibility = false;

    /**
     * @var string $table_name
     */
    public $table_name = 'email_addresses';

    /**
     * @var string $module_name
     */
    public $module_name = "EmailAddresses";

    /** @var string $module_dir */
    public $module_dir = 'EmailAddresses';

    /** @var string  $object_name */
    public $object_name = 'EmailAddress';


    /**
     * bug 40068, According to rules in page 6 of http://www.apps.ietf.org/rfc/rfc3696.html#sec-3,
     * allowed special characters ! # $ % & ' * + - / = ?  ^ _ ` . { | } ~ in local part
     * @var string  $regex
     */
    public $regex = "/^(?:['\.\-\+&#!\$\*=\?\^_`\{\}~\/\w]+)@(?:(?:\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3})|\w+(?:[\.-]*\w+)*(?:\.[\w-]{2,})+)\$/";

    /** @var bool $disable_custom_fields */
    public $disable_custom_fields = true;

    /**
     * @var DBManager
     */
    public $db;
    /**
     * @var Sugar_Smarty  $smarty
     */
    public $smarty;

    /** @var EmailAddress[] $addresses email addresses*/
    public $addresses = array();

    /**
     * @var string $view
     */
    public $view = '';

    /**
     * @var
     */
    private $stateBeforeWorkflow;

    /**
     * @var string $email_address
     */
    public $email_address;

    /**
     * @var string $email_address_caps
     */
    public $email_address_caps;

    public static $count = 0;

    /**
     * @var int
     */
    public $index;


    /**
     * @see SugarEmailAddress::COI_STAT_DISABLED
     * @see SugarEmailAddress::COI_STAT_OPT_IN
     * @see SugarEmailAddress::COI_STAT_CONFIRMED_OPT_IN
     * @var string $confirm_opt_in
     */
    public $confirm_opt_in = '';

    /**
     * @var int|bool $opt_out
     */
    public $opt_out;

    /**
     * @var int|bool $invalid_email
     */
    public $invalid_email;

    /**
     * @var TimeDate $confirm_opt_in_date
     */
    public $confirm_opt_in_date;

    /**
     * @var TimeDate $confirm_opt_in_sent_date
     */
    public $confirm_opt_in_sent_date;

    /**
     * @var TimeDate $confirm_opt_in_fail_date
     */
    public $confirm_opt_in_fail_date;

    /**
     *
     * @var string
     */
    public $confirm_opt_in_token;

    /**
     * @var array $doNotDisplayOptInTickForModule
     */
    protected static $doNotDisplayOptInTickForModule = array(
        'Users',
        'Employees'
    );
    
    /**
     * For saveAtUserProfile() method to telling what
     * went wrong at the last call.
     *
     * @var array
     */
    public $lastSaveAtUserProfileErrors = [];

    /**
     * Sole constructor
     */
    public function __construct()
    {
        parent::__construct();
        $this->index = self::$count;
        self::$count++;
    }



    /**
     * Legacy email address handling.  This is to allow support for SOAP or customizations
     * @param SugarBean $bean
     */
    public function handleLegacySave($bean)
    {
        if ($this->needsToParseLegacyAddresses($bean)) {
            $this->parseLegacyEmailAddresses($bean);
        }
        $this->populateAddresses($bean->id, $bean->module_dir, array(), '');
        if (isset($_REQUEST) && isset($_REQUEST[$bean->module_dir . '_email_widget_id'])) {
            $this->populateLegacyFields($bean);
        }
    }

    /**
     * @param SugarBean $bean
     * @return bool
     */
    private function needsToParseLegacyAddresses($bean)
    {
        if (
            !isset($_REQUEST)
            || !isset($_REQUEST[$bean->module_dir . '_email_widget_id'])
            || !isset($_REQUEST['massupdate'])
        ) {
            if (empty($this->addresses)) {
                $this->addresses = array();
                $optOut = (isset($bean->email_opt_out) && $bean->email_opt_out == '1');
                $invalid = (isset($bean->invalid_email) && $bean->invalid_email == '1');

                $isPrimary = true;
                for ($i = 1; $i <= 10; $i++) {
                    $email = 'email' . $i;
                    if (isset($bean->$email) && !empty($bean->$email)) {
                        $opt_out_field = $email . '_opt_out';
                        $invalid_field = $email . '_invalid';
                        $field_optOut = (isset($bean->$opt_out_field)) ? $bean->$opt_out_field : $optOut;
                        $field_invalid = (isset($bean->$invalid_field)) ? $bean->$invalid_field : $invalid;
                        $this->addAddress($bean->$email, $isPrimary, false, $field_invalid, $field_optOut);
                        $isPrimary = false;
                    }
                }
            }
        }
    }

    /**
     * User Profile specific save email addresses,
     * returns:
     * true - success
     * false - error
     *
     * Note:
     * This function could head to many errors but return
     * value is false in each case.
     * It is confusing because the ambiguous return value.
     * This function also stores the error code(s) in
     * array SugarEmailAddress::$lastSaveAtUserProfileErrors
     *
     * @param array $request $_REQUEST
     * @return bool
     */
    public function saveAtUserProfile($request)
    {
        $this->lastSaveAtUserProfileErrors = [];

        // validate the request first

        if (!$this->isUserProfileEditViewPageSaveAction($request)) {
            $GLOBALS['log']->error('Invalid Referrer: '.
                'expected the Save action to be called from the User\'s Profile Edit View');
            $this->lastSaveAtUserProfileErrors[] = self::ERR_INVALID_REQUEST_NO_USER_PROFILE_PAGE_SAVE_ACTION;
            return false;
        }

        if (!$request) {
            $GLOBALS['log']->error('This function requires a request array');
            $this->lastSaveAtUserProfileErrors[] = self::ERR_INVALID_REQUEST_NO_REQUEST;
            return false;
        }

        // first grab the needed information from a messy request

        $neededRequest = array();
        foreach ($request as $key => $value) {
            if (preg_match('/^Users\d+emailAddress/', $key)) {
                $neededRequest[$key] = $value;
            }
        }

        if (!$neededRequest) {
            $GLOBALS['log']->error('Email info is not found in request');
            $this->lastSaveAtUserProfileErrors[] = self::ERR_INVALID_REQUEST_NO_EMAIL_INFOS;
            return false;
        }

        // re-parsing the request and convert into a useful format

        $usefulRequest = array();
        foreach ($neededRequest as $key => $value) {
            if (preg_match('/^Users(\d+)emailAddress(\d+)/', $key, $matches)) {
                $usefulRequest['Users'][$matches[1]]['emailAddress'][$matches[2]] = array(
                    'email' => $neededRequest["Users{$matches[1]}emailAddress{$matches[2]}"],
                    'id' => $neededRequest["Users{$matches[1]}emailAddressId{$matches[2]}"],
                    'primary' => false,
                    'replyTo' => false,
                );
            }
        }

        if (!$usefulRequest) {
            $GLOBALS['log']->error('Cannot find valid email address(es) in request');
            $this->lastSaveAtUserProfileErrors[] = self::ERR_INVALID_REQUEST_NO_VALID_EMAIL_ADDR_IN_REQUEST;
            return false;
        }

        if (!isset($usefulRequest['Users']) || !$usefulRequest['Users']) {
            $GLOBALS['log']->error('Cannot find valid user in request');
            $this->lastSaveAtUserProfileErrors[] = self::ERR_INVALID_REQUEST_VALID_USER_IS_SET_BUT_NO_IN_REQUEST;
            return false;
        }

        // find the selected primary and replyTo

        $primary = null;
        $replyTo = null;
        foreach ($usefulRequest['Users'] as $ukey => $user) {
            if (
                !$primary &&
                isset($neededRequest["Users{$ukey}emailAddressPrimaryFlag"]) &&
                $neededRequest["Users{$ukey}emailAddressPrimaryFlag"]
            ) {
                $primary = $neededRequest["Users{$ukey}emailAddressPrimaryFlag"];
            }

            if (
                !$replyTo &&
                isset($neededRequest["Users{$ukey}emailAddressReplyToFlag"]) &&
                $neededRequest["Users{$ukey}emailAddressReplyToFlag"]
            ) {
                $replyTo = $neededRequest["Users{$ukey}emailAddressReplyToFlag"];
            }

            // founds?
            if ($primary && $replyTo) {
                break;
            }
        }

        // add primary and replyTo into useful formatted request

        if ($primary && preg_match('/^Users(\d+)emailAddress(\d+)$/', $primary, $matches)) {
            $usefulRequest['Users'][$matches[1]]['emailAddress'][$matches[2]]['primary'] = true;
        } else {
            $GLOBALS['log']->warn("Primary email is not selected.");
            $this->lastSaveAtUserProfileErrors[] = self::ERR_PRIMARY_EMAIL_IS_NOT_SELECTED;
        }

        if ($replyTo && preg_match('/^Users(\d+)emailAddress(\d+)$/', $replyTo, $matches)) {
            $usefulRequest['Users'][$matches[1]]['emailAddress'][$matches[2]]['replyTo'] = true;
        } else {
            $GLOBALS['log']->warn("Reply-to email is not selected.");
            $this->lastSaveAtUserProfileErrors[] = self::ERR_REPLYTO_EMAIL_IS_NOT_SELECTED;
        }

        if (count($usefulRequest['Users']) < 1) {
            $GLOBALS['log']->error("Cannot find valid user in request");
            $this->lastSaveAtUserProfileErrors[] = self::ERR_INVALID_REQUEST_NO_VALID_USER_IN_REQUEST;
            return false;
        }

        if (count($usefulRequest['Users']) > 1) {
            $GLOBALS['log']->warn("Expected only one user in request");
            $this->lastSaveAtUserProfileErrors[] = self::ERR_INVALID_REQUEST_MORE_THAN_ONE_USER_IN_REQUEST;
        }

        $return = true;
        foreach ($usefulRequest['Users'] as $user) {
            foreach ($user['emailAddress'] as $email) {
                if (!$this->handleEmailSaveAtUserProfile($email['id'], $email['email'], $email['primary'], $email['replyTo'])) {
                    $GLOBALS['log']->warn("Some emails were not saved or updated: {$email['id']} ({$email['email']})");
                    $return = false;
                }
            }
        }

        if ($return === false) {
            $this->lastSaveAtUserProfileErrors[] = self::ERR_SOME_EMAILS_WERE_NOT_SAVED_OR_UPDATED;
        }

        return $return;
    }


    /**
     * Handle save on User Profile specific Email Addresses,
     * returns:
     * true - success
     * false - error
     *
     * @param string $id Email address ID
     * @param string $address Valid Email address
     * @param bool $primary
     * @param bool $replyTo
     * @return bool
     */
    protected function handleEmailSaveAtUserProfile($id, $address, $primary, $replyTo)
    {
        global $current_user;

        // first validations

        if (!$id) {
            $GLOBALS['log']->error("Missing email ID");
            return false;
        }

        if (!$address) {
            $GLOBALS['log']->error("Missing email address");
            return false;
        }

        if (!$this->isValidEmail($address)) {
            $GLOBALS['log']->error("Invalid email address format");
            return false;
        }

        $email = new SugarEmailAddress();
        if (!$email->retrieve($id)) {
            $GLOBALS['log']->error('Email retrieve error, please ensure that the email ID is correct');
            return false;
        }

        $db = DBManagerFactory::getInstance();
        $query = sprintf("SELECT * FROM email_addresses WHERE id = %s AND deleted = 0", $db->quoted($id));
        if ($db->getOne($query) === false) {
            $GLOBALS['log']->error("Missing Email ID ($id)");
            return false;
        }

        // do we have to update the address?

        if ($email->email_address != $address) {
            $_address = $db->quote($address);
            $_addressCaps = $db->quote(strtoupper($address));
            $_id = $db->quoted($id);
            $query =
                "UPDATE email_addresses 
                  SET 
                    email_address = '$_address', 
                    email_address_caps = '$_addressCaps' 
                  WHERE 
                    id = {$_id} AND
                    deleted = 0";
            $result = $db->query($query);
            if (!$result) {
                $GLOBALS['log']->warn("Undefined behavior: Missing error information about email save (1)");
            }
            if ($db->getAffectedRowCount($result) != 1) {
                $GLOBALS['log']->debug("Email address has not change");
            }
        }

        // update primary and replyTo

        $_primary = (bool)$primary ? '1' : '0';
        $_replyTo = (bool)$replyTo ? '1' : '0';
        $_id = $db->quoted($id);
        $query =
            "UPDATE email_addr_bean_rel 
              SET 
                primary_address = '{$_primary}', 
                reply_to_address = '{$_replyTo}' 
              WHERE 
                email_address_id = {$_id} AND             
                bean_module = 'Users' AND 
                bean_id = '{$current_user->id}' AND
                deleted = 0";
        $result = $db->query($query);
        if (!$result) {
            $GLOBALS['log']->warn("Undefined behavior: Missing error information about email save (2)");
        }
        if ($db->getAffectedRowCount($result) != 1) {
            $GLOBALS['log']->debug("Primary or reply-to Email address has not change");
        }

        return true;
    }


    /**
     * Check a valid email format,
     * return false if the email validation failed
     *
     * @param string $email
     * @return mixed
     */
    protected function isValidEmail($email)
    {
        $return = filter_var($email, FILTER_VALIDATE_EMAIL);
        return $return;
    }


    /**
     * Check for User Profile EditView / Save action for
     * Email Addresses updates
     * returns:
     * true - User Profile Save action called by request
     *
     * @param array $request $_REQUEST
     * @return bool
     */
    protected function isUserProfileEditViewPageSaveAction($request)
    {
        $return =
            (isset($request['page']) && $request['page'] == 'EditView') &&
            (isset($request['module']) && $request['module'] == 'Users') &&
            (isset($request['action']) && $request['action'] == 'Save');

        return $return;
    }


    /**
     * Fills standard email1 legacy fields
     * @param string id
     * @param string module
     * @return object
     */
    public function handleLegacyRetrieve(&$bean)
    {
        $module_dir = $this->getCorrectedModule($bean->module_dir);
        $this->addresses = $this->getAddressesByGUID($bean->id, $module_dir);
        $this->populateLegacyFields($bean);
        if (isset($bean->email1) && !isset($bean->fetched_row['email1'])) {
            $bean->fetched_row['email1'] = $bean->email1;
        }

        return;
    }

    public function populateLegacyFields(&$bean)
    {
        $primary_found = false;
        $alternate_found = false;
        $alternate2_found = false;
        foreach ($this->addresses as $k => $address) {
            if ($primary_found && $alternate_found) {
                break;
            }
            if ($address['primary_address'] == 1 && !$primary_found) {
                $primary_index = $k;
                $primary_found = true;
            } elseif (!$alternate_found) {
                $alternate_index = $k;
                $alternate_found = true;
            } elseif (!$alternate2_found) {
                $alternate2_index = $k;
                $alternate2_found = true;
            }
        }

        if ($primary_found) {
            $bean->email1 = $this->addresses[$primary_index]['email_address'];
            $bean->email_opt_out = $this->addresses[$primary_index]['opt_out'];
            $bean->invalid_email = $this->addresses[$primary_index]['invalid_email'];
            if ($alternate_found) {
                $bean->email2 = $this->addresses[$alternate_index]['email_address'];
            }
        } elseif ($alternate_found) {
            // Use the first found alternate as email1.
            $bean->email1 = $this->addresses[$alternate_index]['email_address'];
            $bean->email_opt_out = $this->addresses[$alternate_index]['opt_out'];
            $bean->invalid_email = $this->addresses[$alternate_index]['invalid_email'];
            if ($alternate2_found) {
                $bean->email2 = $this->addresses[$alternate2_index]['email_address'];
            }
        }
    }

    /**
     * @deprecated
     * @param bool $check_notify
     * @return null
     */
    public function save($check_notify = false)
    {
        $deprecatedMessage = 'SugarEmailAddress::save() function calls are deprecated use SugarEmailAddress::saveEmail() function instead';
        if (isset($GLOBALS['log'])) {
            $GLOBALS['log']->deprecated($deprecatedMessage);
        } else {
            trigger_error($deprecatedMessage, E_USER_DEPRECATED);
        }

        list($id, $module, $new_addrs, $primary, $replyTo, $invalid, $optOut, $in_workflow) = func_get_args();

        return $this->saveEmail($id, $module, $new_addrs, $primary, $replyTo, $invalid, $optOut, $in_workflow);
    }

    /**
     * Saves email addresses for a parent bean.
     * The base class SugarBean::save($check_notify) method is never called from SugarEmailAddresses::saveEmail(...)
     * The method's signature has been changed to correctly represent the save method call for SugarEmailAddress.
     * @param string $id Parent bean ID
     * @param string $module Parent bean's module
     * @param array $new_addrs Override of $_REQUEST vars, used to handle non-standard bean saves
     * @param string $primary GUID of primary address
     * @param string $replyTo GUID of reply-to address
     * @param string $invalid GUID of invalid address
     * @param string $optOut
     * @param bool $in_workflow
     * @param bool|null $opt_in
     * @return null
     */
    public function saveEmail(
        $id,
        $module,
        $new_addrs = array(),
        $primary = '',
        $replyTo = '',
        $invalid = '',
        $optOut = '',
        $in_workflow = false,
        $optIn = null
    ) {
        if (gettype($id) == "boolean") {
            $GLOBALS['log']->fatal('SugarEmailAddress::saveEmail() Invalid arguments - Parent method SugarBean::save
            ($checknotify) is not implemented. Please pass the correct arguments into SugarEmailAddress::saveEmail()');
        }

        if (
            empty($this->addresses)
            || $in_workflow === true
        ) {
            $this->populateAddresses($id, $module, $new_addrs, $primary);
        }

        // handle the Employee/User split
        $module = $this->getCorrectedModule($module);

        // find all email addresses
        $current_links = array();
        $q2 = "SELECT *  FROM email_addr_bean_rel eabr WHERE eabr.bean_id = '" . $this->db->quote($id) . "' AND eabr.bean_module = '" . $this->db->quote($module) . "' AND eabr.deleted=0";
        $r2 = $this->db->query($q2);
        while (($row2 = $this->db->fetchByAssoc($r2)) != null) {
            $current_links[$row2['email_address_id']] = $row2;
        }

        $isConversion = (isset($_REQUEST) && isset($_REQUEST['action']) && $_REQUEST['action'] == 'ConvertLead') ? true : false;

        if (!empty($this->addresses)) {
            // insert new relationships and create email address record, if they don't exist
            foreach ($this->addresses as $address) {
                if (!empty($address['email_address'])) {
                    $guid = create_guid();
                    $emailId = isset($address['email_address_id'])
                    && isset($current_links[$address['email_address_id']])
                        ? $address['email_address_id'] : null;
                    $emailId = $this->AddUpdateEmailAddress(
                        $address['email_address'],
                        $address['invalid_email'],
                        $address['opt_out'],
                        $emailId,
                        !is_null($optIn) ? $address['confirm_opt_in_flag'] : null
                    );// this will save the email address if not found

                    //verify linkage and flags.
                    $upd_eabr = "";
                    if (isset($current_links[$emailId])) {
                        if (!$isConversion) { // do not update anything if this is for lead conversion
                            if ($address['primary_address'] != $current_links[$emailId]['primary_address'] or $address['reply_to_address'] != $current_links[$emailId]['reply_to_address']) {
                                $upd_eabr = "UPDATE email_addr_bean_rel SET primary_address='" . $this->db->quote($address['primary_address']) . "', reply_to_address='" . $this->db->quote($address['reply_to_address']) . "' WHERE id='" . $this->db->quote($current_links[$emailId]['id']) . "'";
                            }

                            unset($current_links[$emailId]);
                        }
                    } else {
                        $primary = $address['primary_address'];
                        if (!empty($current_links) && $isConversion) {
                            foreach ($current_links as $eabr) {
                                if ($eabr['primary_address'] == 1) {
                                    // for lead conversion, if there is already a primary email, do not insert another primary email
                                    $primary = 0;
                                    break;
                                }
                            }
                        }
                        $now = $this->db->now();
                        $upd_eabr = "INSERT INTO email_addr_bean_rel (id, email_address_id,bean_id, bean_module,primary_address,reply_to_address,date_created,date_modified,deleted) VALUES('" . $this->db->quote($guid) . "', '" . $this->db->quote($emailId) . "', '" . $this->db->quote($id) . "', '" . $this->db->quote($module) . "', " . (int)$primary . ", " . (int)$address['reply_to_address'] . ", $now, $now, 0)";
                    }

                    if (!empty($upd_eabr)) {
                        $r2 = $this->db->query($upd_eabr);
                    }
                }
            }
        }

        //delete link to dropped email address.
        // for lead conversion, do not delete email addresses
        if (!empty($current_links) && !$isConversion) {
            $delete = "";
            foreach ($current_links as $eabr) {
                $delete .= empty($delete) ? "'" . $this->db->quote($eabr['id']) . "' " : ",'" . $this->db->quote($eabr['id']) . "'";
            }

            $eabr_unlink = "update email_addr_bean_rel set deleted=1 where id in ({$delete})";
            $this->db->query($eabr_unlink);
        }
        $this->stateBeforeWorkflow = null;
    }

    /**
     * returns the number of email addresses found for a specifed bean
     *
     * @param  string $email Address to match
     * @param  SugarBean $bean Bean to query against
     * @param  string $addressType Optional, pass a 1 to query against the primary address, 0 for the other addresses
     * @return int                 Count of records found
     * @throws \InvalidArgumentException
     */
    public function getCountEmailAddressByBean(
        $email,
        $bean,
        $addressType
    ) {
        $addressTypeInt = (int)$addressType;
        if ($addressType != 0 && $addressType != 1) {
            throw new InvalidArgumentException(
                'Invalid Address Type Argument: ' .
                'pass a 1 to query against the primary address, 0 for the other addresses'
            );
        }
        $emailCaps = strtoupper(trim($email));
        if (empty($emailCaps)) {
            return 0;
        }

        $q = "SELECT *
                FROM email_addr_bean_rel eabl JOIN email_addresses ea
                        ON (ea.id = eabl.email_address_id)
                    JOIN {$bean->table_name} bean
                        ON (eabl.bean_id = bean.id)
                WHERE ea.email_address_caps = '" . $this->db->quote($emailCaps) . "'
                    and eabl.bean_module = '" . $this->db->quote($bean->module_dir) . "'
                    and eabl.primary_address = '" . $this->db->quote($addressTypeInt) . "'
                    and eabl.deleted=0 ";

        $r = $this->db->query($q);

        // do it this way to make the count accurate in oracle
        $i = 0;
        while ($this->db->fetchByAssoc($r)) {
            ++$i;
        }

        return $i;
    }

    /**
     * This function returns a contact or user ID if a matching email is found
     * @param   string $email      the email address to match
     * @param   string $table      which table to query
     */
    public function getRelatedId($email, $module)
    {
        $email = $this->db->quote(trim(strtoupper($email)));
        $module = $this->db->quote(ucfirst($module));

        $q = "SELECT bean_id FROM email_addr_bean_rel eabr
                JOIN email_addresses ea ON (eabr.email_address_id = ea.id)
                WHERE bean_module = '$module' AND ea.email_address_caps = '$email' AND eabr.deleted=0";

        $r = $this->db->query($q, true);

        $returnArray = array();
        while ($a = $this->db->fetchByAssoc($r)) {
            $returnArray[] = $a['bean_id'];
        }
        if (count($returnArray) > 0) {
            return $returnArray;
        } else {
            return false;
        }
    }

    /**
     * returns a collection of beans matching the email address
     * @param string $email Address to match
     * @return array
     */
    public function getBeansByEmailAddress($email)
    {
        global $beanList;
        global $beanFiles;

        $return = array();

        $email = trim($email);

        if (empty($email)) {
            return array();
        }

        $emailCaps = "'" . $this->db->quote(strtoupper($email)) . "'";
        $q = "SELECT * FROM email_addr_bean_rel eabl JOIN email_addresses ea ON (ea.id = eabl.email_address_id and ea.deleted = 0)
                WHERE ea.email_address_caps = $emailCaps and eabl.deleted=0 ";
        $r = $this->db->query($q);

        while ($a = $this->db->fetchByAssoc($r)) {
            if (isset($beanList[$a['bean_module']]) && !empty($beanList[$a['bean_module']])) {
                $className = $beanList[$a['bean_module']];

                if (isset($beanFiles[$className]) && !empty($beanFiles[$className])) {
                    if (!class_exists($className)) {
                        require_once($beanFiles[$className]);
                    }

                    $bean = BeanFactory::getBean($a['bean_module'], $a['bean_id']);
                    if ($bean !== false) {
                        $return[] = $bean;
                    }

                } else {
                    $GLOBALS['log']->fatal("SUGAREMAILADDRESS: could not find valid class file for [ {$className} ]");
                }
            } else {
                $GLOBALS['log']->fatal("SUGAREMAILADDRESS: could not find valid class [ {$a['bean_module']} ]");
            }
        }

        return $return;
    }

    /**
     * Saves email addresses for a parent bean
     * @param string $id Parent bean ID
     * @param string $module  Parent bean's module
     * @param array $new_addrs Override of $_REQUEST vars, used to handle non-standard bean saves
     * @param string $primary GUID of primary address
     * @param string $replyTo GUID of reply-to address
     */
    public function populateAddresses(
        $id,
        $module,
        $new_addrs = array(),
        $primary = '',
        $replyTo = '',
        $invalid = '',
        $optOut = ''
    ) {
        if (!is_array($new_addrs)) {
            $GLOBALS['log']->fatal(
                'Invalid Argument: new address should be an array of strings, ' .
                gettype($new_addrs) . ' given.'
            );
        }
        $module = $this->getCorrectedModule($module);
        //One last check for the ConvertLead action in which case we need to change $module to 'Leads'
        $module = (isset($_REQUEST) && isset($_REQUEST['action']) && $_REQUEST['action'] === 'ConvertLead') ? 'Leads' : $module;

        $post_from_email_address_widget = (isset($_REQUEST[$module . '_email_widget_id']));
        $primaryValue = $primary;
        $widgetCount = 0;
        $widget_id = '';
        $hasEmailValue = false;
        $email_ids = array();

        if (isset($_REQUEST[$module . '_email_widget_id'])) {
            $fromRequest = false;
            // determine which array to process
            foreach ($_REQUEST as $k => $v) {
                if (strpos($k, 'emailAddress') !== false) {
                    $fromRequest = true;
                    break;
                }
                $widget_id = $_REQUEST[$module . '_email_widget_id'];
            }


            if (empty($widget_id)) {
                $GLOBALS['log']->debug('Widget not found, so it should be an update and not a create');
            }

            //Iterate over the widgets for this module, in case there are multiple email widgets for this module
            while (isset($_REQUEST[$module . $widget_id . 'emailAddress' . $widgetCount])) {
                if (empty($_REQUEST[$module . $widget_id . 'emailAddress' . $widgetCount])) {
                    $widgetCount++;
                    continue;
                }

                $hasEmailValue = true;

                $eId = $module . $widget_id;
                if (isset($_REQUEST[$eId . 'emailAddressPrimaryFlag'])) {
                    $primaryValue = $_REQUEST[$eId . 'emailAddressPrimaryFlag'];
                } elseif (isset($_REQUEST[$module . 'emailAddressPrimaryFlag'])) {
                    $primaryValue = $_REQUEST[$module . 'emailAddressPrimaryFlag'];
                }

                $optOutValues = array();
                if (isset($_REQUEST[$eId . 'emailAddressOptOutFlag'])) {
                    $optOutValues = $_REQUEST[$eId . 'emailAddressOptOutFlag'];
                } elseif (isset($_REQUEST[$module . 'emailAddressOptOutFlag'])) {
                    $optOutValues = $_REQUEST[$module . 'emailAddressOptOutFlag'];
                }

                $invalidValues = array();
                if (isset($_REQUEST[$eId . 'emailAddressInvalidFlag'])) {
                    $invalidValues = $_REQUEST[$eId . 'emailAddressInvalidFlag'];
                } elseif (isset($_REQUEST[$module . 'emailAddressInvalidFlag'])) {
                    $invalidValues = $_REQUEST[$module . 'emailAddressInvalidFlag'];
                }


                $optInValues = array();
                if (isset($_REQUEST[$eId . 'emailAddressOptInFlag'])) {
                    $optInValues = $_REQUEST[$eId . 'emailAddressOptInFlag'];
                } elseif (isset($_REQUEST[$module . 'emailAddressOptInFlag'])) {
                    $optInValues = $_REQUEST[$module . 'emailAddressOptInFlag'];
                }

                $deleteValues = array();
                if (isset($_REQUEST[$eId . 'emailAddressDeleteFlag'])) {
                    $deleteValues = $_REQUEST[$eId . 'emailAddressDeleteFlag'];
                } elseif (isset($_REQUEST[$module . 'emailAddressDeleteFlag'])) {
                    $deleteValues = $_REQUEST[$module . 'emailAddressDeleteFlag'];
                }

                // prep from form save
                $replyToField = '';
                $invalidField = '';
                $optOutField = '';
                $optInField = '';
                if ($fromRequest && empty($primary) && isset($primaryValue)) {
                    $primaryField = $primaryValue;
                }

                if ($fromRequest && empty($replyTo)) {
                    if (isset($_REQUEST[$eId . 'emailAddressReplyToFlag'])) {
                        $replyToField = $_REQUEST[$eId . 'emailAddressReplyToFlag'];
                    } elseif (isset($_REQUEST[$module . 'emailAddressReplyToFlag'])) {
                        $replyToField = $_REQUEST[$module . 'emailAddressReplyToFlag'];
                    }
                }
                if ($fromRequest && empty($new_addrs)) {
                    foreach ($_REQUEST as $k => $v) {
                        if (preg_match('/' . $eId . 'emailAddress[0-9]+$/i', $k) && !empty($v)) {
                            $new_addrs[$k] = $v;
                        }
                    }
                }
                if ($fromRequest && empty($email_ids)) {
                    foreach ($_REQUEST as $k => $v) {
                        if (preg_match('/' . $eId . 'emailAddressId[0-9]+$/i', $k) && !empty($v)) {
                            $key = str_replace('emailAddressId', 'emailAddress', $k);
                            $email_ids[$key] = $v;
                        }
                    }
                }

                // NOTE: probably it's never gonna happen:
                // $fromRequest became true if there is any emailAddress in request but
                // $new_addrs never empty because it's got a value if there is any emailAddress
                if ($fromRequest && empty($new_addrs)) {
                    foreach ($_REQUEST as $k => $v) {
                        if (preg_match('/' . $eId . 'emailAddressVerifiedValue[0-9]+$/i', $k) && !empty($v)) {
                            $validateFlag = str_replace("Value", "Flag", $k);
                            if (isset($_REQUEST[$validateFlag]) && $_REQUEST[$validateFlag] == "true") {
                                $new_addrs[$k] = $v;
                            }
                        }
                    }
                }

                //empty the addresses array if the post happened from email address widget.
                if ($post_from_email_address_widget) {
                    $this->addresses = array();  //this gets populated during retrieve of the contact bean.
                } else {
                    $optOutValues = array();
                    $invalidValues = array();
                    foreach ($new_addrs as $k => $email) {
                        preg_match('/emailAddress([0-9])+$/', $k, $matches);
                        $count = $matches[1];
                        $query = "SELECT opt_out, invalid_email, confirm_opt_in FROM email_addresses WHERE email_address_caps = '" . $this->db->quote(strtoupper($email)) . "'";
                        $result = $this->db->query($query);
                        if (!empty($result)) {
                            $row = $this->db->fetchByAssoc($result);
                            if (!empty($row['opt_out'])) {
                                $optOutValues[$k] = "emailAddress$count";
                            }

                            if (!empty($row['invalid_email'])) {
                                $invalidValues[$k] = "emailAddress$count";
                            }

                            if (!empty($row['confirm_opt_in'])) {
                                $optInValues[$k] = "emailAddress$count";
                            }
                        }
                    }
                }
                // Re-populate the addresses class variable if we have new address(es).
                if (!empty($new_addrs)) {
                    foreach ($new_addrs as $k => $reqVar) {
                        //$key = preg_match("/^$eId/s", $k) ? substr($k, strlen($eId)) : $k;
                        $reqVar = trim($reqVar);
                        if (strpos($k, 'emailAddress') !== false) {
                            if (!is_array($deleteValues)) {
                                $GLOBALS['log']->fatal('Invalid Argument: Delete Values to be an array, ' . gettype($deleteValues) . ' given.');
                            } else {
                                if (!empty($reqVar) && !in_array($k, $deleteValues)) {
                                    $email_id = (array_key_exists($k, $email_ids)) ? $email_ids[$k] : null;
                                    $primary = ($k == $primaryValue) ? true : false;
                                    $replyTo = ($k == $replyToField) ? true : false;
                                    $invalid = (in_array($k, (array)$invalidValues)) ? true : false;
                                    $optOut = (in_array($k, (array)$optOutValues)) ? true : false;
                                    $optIn = (in_array($k, $optInValues)) ? true : false;
                                    $this->addAddress(
                                        trim($new_addrs[$k]),
                                        $primary,
                                        $replyTo,
                                        $invalid,
                                        $optOut,
                                        $email_id,
                                        $optIn
                                    );
                                }
                            }
                        }
                    } //foreach
                }

                $widgetCount++;
            }//End of Widget for loop
        }

        //If no widgets, set addresses array to empty
        if ($post_from_email_address_widget && !$hasEmailValue) {
            $this->addresses = array();
        }
    }

    /**
     * Preps internal array structure for email addresses
     * @param string $addr Email address
     * @param bool $primary Default false
     * @param bool $replyTo Default false
     * @param bool $invalid Default false
     * @param bool $optOut Default false
     * @param string $email_id
     * @param bool $optIn Default false
     */
    public function addAddress(
        $addr,
        $primary = false,
        $replyTo = false,
        $invalid = false,
        $optOut = false,
        $email_id = null,
        $optIn = null
    ) {
        $addr = html_entity_decode($addr, ENT_QUOTES);
        if (preg_match($this->regex, $addr)) {
            $primaryFlag = ($primary) ? '1' : '0';
            $replyToFlag = ($replyTo) ? '1' : '0';
            $invalidFlag = ($invalid) ? '1' : '0';
            $optOutFlag = ($optOut) ? '1' : '0';
            if (!is_null($optIn)) {
                $optInFlag = ($optIn) ? '1' : '0';
            }

            $addr = trim($addr);

            // If we have such address already, remove it and add new one in.
            foreach ($this->addresses as $k => $address) {
                if ($address['email_address'] == $addr) {
                    unset($this->addresses[$k]);
                } elseif ($primary && $address['primary_address'] == '1') {
                    // We should only have one primary. If we are adding a primary but
                    // we find an existing primary, reset this one's primary flag.
                    $this->addresses[$k]['primary_address'] = '0';
                }
            }

            $addr = array(
                'email_address' => $addr,
                'primary_address' => $primaryFlag,
                'reply_to_address' => $replyToFlag,
                'invalid_email' => $invalidFlag,
                'opt_out' => $optOutFlag,
                'email_address_id' => $email_id,
                'confirm_opt_in_flag' => null,
            );

            if (!is_null($optIn)) {
                $addr['confirm_opt_in_flag'] = $optInFlag;
            }

            $this->addresses[] = $addr;
        } else {
            $GLOBALS['log']->fatal("SUGAREMAILADDRESS: address did not valid [ {$addr} ]");
        }
    }

    /**
     * Updates invalid_email and opt_out flags for each address
     */
    public function updateFlags()
    {
        if (!empty($this->addresses)) {
            foreach ($this->addresses as $addressMeta) {
                if (isset($addressMeta['email_address']) && !empty($addressMeta['email_address'])) {
                    $address = $this->db->quote($this->_cleanAddress($addressMeta['email_address']));

                    $q = "SELECT * FROM email_addresses WHERE email_address = '{$address}'";
                    $r = $this->db->query($q);
                    $a = $this->db->fetchByAssoc($r);

                    if (
                        !empty($a) &&
                        (
                            isset($a['invalid_email']) &&
                            isset($addressMeta['invalid_email']) &&
                            isset($addressMeta['opt_out']) &&
                            $a['invalid_email'] != $addressMeta['invalid_email'] ||
                            $a['opt_out'] != $addressMeta['opt_out']
                        )
                    ) {
                        $addressMetaInvalidEmailInt = (int)$addressMeta['invalid_email'];
                        $addressMetaOptOutInt = (int)$addressMeta['opt_out'];
                        $now = TimeDate::getInstance()->nowDb();
                        $id = $this->db->quote($a['id']);

                        $qUpdate = /** @lang sql */
                            "UPDATE email_addresses SET 
                              invalid_email = {$addressMetaInvalidEmailInt}, 
                              opt_out = {$addressMetaOptOutInt}, 
                              date_modified = '{$now}' 
                            WHERE id = '{$id}'";

                        $this->db->query($qUpdate);
                    }
                }
            }
        }
    }

    public function splitEmailAddress($addr)
    {
        $email = $this->_cleanAddress($addr);
        if (!preg_match($this->regex, $email)) {
            $email = ''; // remove bad email addr
        }
        $name = trim(str_replace(array($email, '<', '>', '"', "'"), '', $addr));

        return array("name" => $name, "email" => strtolower($email));
    }

    /**
     * PRIVATE UTIL
     * Normalizes an RFC-clean email address, returns a string that is the email address only
     * @param string $addr Dirty email address
     * @return string clean email address
     */
    public function _cleanAddress($addr)
    {
        $addr = trim(from_html($addr));

        if (strpos($addr, "<") !== false && strpos($addr, ">") !== false) {
            $address = trim(substr($addr, strrpos($addr, "<") + 1, strrpos($addr, ">") - strrpos($addr, "<") - 1));
        } else {
            $address = trim($addr);
        }

        return $address;
    }

    /**
     * preps a passed email address for email address storage
     * @param string $addr Address in focus, must be RFC compliant
     * @return string $id email_addresses ID
     */
    public function getEmailGUID($addr)
    {
        $address = $this->db->quote($this->_cleanAddress($addr));
        $addressCaps = strtoupper($address);

        $q = "SELECT id FROM email_addresses WHERE email_address_caps = '{$addressCaps}'";
        $r = $this->db->query($q);
        $a = $this->db->fetchByAssoc($r);

        if (!empty($a) && !empty($a['id'])) {
            return $a['id'];
        } else {
            $guid = '';
            if (!empty($address)) {
                $guid = create_guid();
                $now = TimeDate::getInstance()->nowDb();
                $qa = "INSERT INTO email_addresses (id, email_address, email_address_caps, date_created, date_modified, deleted)
                        VALUES('{$guid}', '{$address}', '{$addressCaps}', '$now', '$now', 0)";
                $ra = $this->db->query($qa);
            }

            return $guid;
        }
    }

    /**
     * Creates or Updates an entry in the email_addresses table, depending
     * on if the email address submitted matches a previous entry (case-insensitive)
     * @param string $addr - email address
     * @param int $invalid - is the email address marked as Invalid?
     * @param int $opt_out - is the email address marked as Opt-Out?
     * @param string $id - the GUID of the original SugarEmailAddress bean,
     *        in case a "email has changed" WorkFlow has triggered - hack to allow workflow-induced changes
     *        to propagate to the new SugarEmailAddress - see bug 39188
     * @param int|null $optInFlag
     * @return string GUID of Email Address or '' if cleaned address was empty.
     */
    public function AddUpdateEmailAddress($addr, $invalid = 0, $opt_out = 0, $id = null, $optInFlag = null)
    {
        // sanity checks to avoid SQL injection.
        $invalid = (int)$invalid;
        $opt_out = (int)$opt_out;

        $address = $this->db->quote($this->_cleanAddress($addr));
        $addressCaps = strtoupper($address);

        // determine if we have a matching email address
        $q = "SELECT * FROM email_addresses WHERE email_address_caps = '{$addressCaps}' and deleted=0";
        $r = $this->db->query($q);
        $duplicate_email = $this->db->fetchByAssoc($r);

        // check if we are changing an email address, where workflow might be in play
        if ($id) {
            $query = "SELECT * FROM email_addresses WHERE id='" . $this->db->quote($id) . "'";
            $r = $this->db->query($query);
            $current_email = $this->db->fetchByAssoc($r);
        } else {
            $current_email = null;
        }

        // unless workflow made changes, assume parameters are what to use.
        $new_opt_out = $opt_out;
        $new_invalid = $invalid;
        if (!empty($current_email['id']) && isset($this->stateBeforeWorkflow[$current_email['id']])) {
            if ($current_email['invalid_email'] != $invalid ||
                $current_email['opt_out'] != $opt_out
            ) {

                // workflow could be in play
                $before_email = $this->stateBeforeWorkflow[$current_email['id']];

                // our logic is as follows: choose from parameter, unless workflow made a change to the value, then choose final value
                if ((int)$before_email['opt_out'] != (int)$current_email['opt_out']) {
                    $new_opt_out = (int)$current_email['opt_out'];
                }
                if ((int)$before_email['invalid_email'] != (int)$current_email['invalid_email']) {
                    $new_invalid = (int)$current_email['invalid_email'];
                }
            }
        }

        // confirmed opt in check
        if (!is_null($optInFlag)) {
            $optInFlag = (int)$optInFlag;
        }

        $isValidEmailAddress = ($opt_out !== 1 && $invalid !== 1);
        $this->retrieve($id);
        $optInIndication = $this->getOptInStatus();
        if (
           $isValidEmailAddress
           && $this->isOptedInStatus($optInIndication)
           && (int)$optInFlag === 1
        ) {
            $new_confirmed_opt_in = $this->getConfirmedOptInState();
        } elseif (
            $isValidEmailAddress
            && (int)$optInFlag === 1
        ) {
            // In case optInFlag is set and there is a duplicate,
            // copy the opt-in state from it if it has some kind of opt-in set.
            // This prevents losing the confirmed opt-in state in case we
            // update an existing record with "confirmed-opt-in"
            if (!empty($duplicate_email['id']) && $duplicate_email['confirm_opt_in'] != self::COI_STAT_DISABLED) {
                $new_confirmed_opt_in = $duplicate_email['confirm_opt_in'];
            } else {
                $new_confirmed_opt_in = self::COI_STAT_OPT_IN;
            }
        } else {
            // Reset the opt in status
            $new_confirmed_opt_in = self::COI_STAT_DISABLED;
        }

        // determine how we are going to put in this address - UPDATE or INSERT
        if (!empty($duplicate_email['id'])) {
            $duplicate = clone $this;
            $duplicate->retrieve($duplicate_email['id']);
            // address_caps matches - see if we're changing fields
            if (
                $duplicate_email['invalid_email'] != $new_invalid
                || $duplicate_email['opt_out'] != $new_opt_out
                || (!is_null($optInFlag) && $duplicate_email['confirm_opt_in'] != $new_confirmed_opt_in)
                || (trim($duplicate_email['email_address']) != $address)
            ) {
                $upd_q = 'UPDATE ' . $this->table_name . ' ' .
                    'SET email_address=\'' . $address . '\', ' .
                    'invalid_email=' . $new_invalid . ', ' .
                    'opt_out=' . $new_opt_out . ', ' .
                    (!is_null($optInFlag) ? ('confirm_opt_in=\'' . $this->db->quote($new_confirmed_opt_in) . '\', ') : '') .
                    'date_modified=' . $this->db->now() . ' ' .
                    'WHERE id=\'' . $this->db->quote($duplicate_email['id']) . '\'';
                // set for audit table detection
                $duplicate->invalid_email = $new_invalid;
                $duplicate->opt_out = $new_opt_out;
                $duplicate->confirm_opt_in = $new_confirmed_opt_in;
                $upd_r = $this->db->query($upd_q);


                if ($new_confirmed_opt_in === self::COI_STAT_DISABLED) {
                    // reset confirm opt in
                    $upd_q = 'UPDATE ' . $this->table_name . ' ' .
                            'SET '.
                            'confirm_opt_in_date=NULL,' .
                            'confirm_opt_in_sent_date=NULL,' .
                            'confirm_opt_in_fail_date=NULL ' .
                            'WHERE id=\'' . $this->db->quote($duplicate_email['id']) . '\'';
                    $upd_r = $this->db->query($upd_q);
                    // set for audit table detection
                    $duplicate->confirm_opt_in = null;
                }
            }

            if (!empty($this->fetched_row)) {
                foreach ($this->fetched_row as $fieldName => $fieldValue) {
                    $this->{$fieldName} = $duplicate->{$fieldName};
                }
            }

            $this->auditBean(true);

            return $duplicate_email['id'];
        } else {
            // no case-insensitive address match - it's new, or undeleted.
            $guid = '';
            $isUpdate = true;
            if (!empty($address)) {
                $guid = create_guid();
                $now = TimeDate::getInstance()->nowDb();
                $qa = "INSERT INTO email_addresses (id, email_address, email_address_caps, date_created, date_modified, deleted, invalid_email, opt_out" . (!is_null($optInFlag) ? ", confirm_opt_in" : '') . ")
                        VALUES('{$guid}', '{$address}', '{$addressCaps}', '$now', '$now', 0 , $new_invalid, $new_opt_out" . (!is_null($optInFlag) ? ", '" . $this->db->quote($new_confirmed_opt_in) ."'" : '') . ")";
                $this->db->query($qa);
                $isUpdate = false;
            }

            $this->auditBean($isUpdate);
            return $guid;
        }
    }

    /**
     * @return string
     */
    public function getConfirmedOptInState()
    {
        return $this->confirm_opt_in;
    }

    /**
     * Returns Primary or newest email address
     * @param object $focus Object in focus
     * @return string email
     */
    public function getPrimaryAddress($focus, $parent_id = null, $parent_type = null)
    {
        $parent_type = empty($parent_type) ? $focus->module_dir : $parent_type;
        // Bug63174: Email address is not shown in the list view for employees
        $parent_type = $this->getCorrectedModule($parent_type);
        $parent_id = empty($parent_id) ? $focus->id : $parent_id;

        $q = "SELECT ea.email_address FROM email_addresses ea
                LEFT JOIN email_addr_bean_rel ear ON ea.id = ear.email_address_id
                WHERE ear.bean_module = '" . $this->db->quote($parent_type) . "'
                AND ear.bean_id = '" . $this->db->quote($parent_id) . "'
                AND ear.deleted = 0
                AND ea.invalid_email = 0
                ORDER BY ear.primary_address DESC";
        $r = $this->db->limitQuery($q, 0, 1);
        $a = $this->db->fetchByAssoc($r);

        if (isset($a['email_address'])) {
            return $a['email_address'];
        }

        return '';
    }

    /**
     * As long as this function is used not only to retrieve user's Reply-To
     * address, but also notification address and so on, there were added
     * $replyToOnly optional parameter used to retrieve only address marked as
     * Reply-To (bug #43643).
     *
     * @param SugarBean $focus
     * @param bool $replyToOnly
     * @return string
     */
    public function getReplyToAddress($focus, $replyToOnly = false)
    {
        $q = "SELECT ea.email_address FROM email_addresses ea
                LEFT JOIN email_addr_bean_rel ear ON ea.id = ear.email_address_id
                WHERE ear.bean_module = '" . $this->db->quote($focus->module_dir) . "'
                AND ear.bean_id = '" . $this->db->quote($focus->id) . "'
                AND ear.deleted = 0
                AND ea.invalid_email = 0";

        if (!$replyToOnly) {
            // retrieve reply-to address if it exists or any other address
            // otherwise
            $q .= "
                ORDER BY ear.reply_to_address DESC";
        } else {
            // retrieve reply-to address only
            $q .= "
                AND ear.reply_to_address = 1";
        }

        $r = $this->db->query($q);
        $a = $this->db->fetchByAssoc($r);

        if (isset($a['email_address'])) {
            return $a['email_address'];
        }

        return '';
    }

    /**
     * Returns all email addresses by parent's GUID
     * @param string $id Parent's GUID
     * @param string $module Parent's module
     * @return array
     */
    public function getAddressesByGUID($id, $module)
    {
        $return = array();
        $module = $this->getCorrectedModule($module);

        $q = "SELECT 
                    ea.email_address,
                    ea.email_address_caps,
                    ea.invalid_email,
                    ea.opt_out,
                    ea.confirm_opt_in,
                    ea.date_created,
                    ea.date_modified,
                    ear.id,
                    ear.email_address_id,
                    ear.bean_id,
                    ear.bean_module,
                    ear.primary_address,
                    ear.reply_to_address,
                    ear.deleted
                FROM email_addresses ea LEFT JOIN email_addr_bean_rel ear ON ea.id = ear.email_address_id
                WHERE 
                    ear.bean_module = '" . $this->db->quote($module) . "'
                    AND ear.bean_id = '" . $this->db->quote($id) . "'
                    AND ear.deleted = 0
                ORDER BY ear.reply_to_address, ear.primary_address DESC";
        $r = $this->db->query($q);

        while ($a = $this->db->fetchByAssoc($r, false)) {
            $return[] = $a;
        }

        return $return;
    }

    /**
     * Returns the HTML/JS for the EmailAddress widget
     * @global LoggerManager $log
     * @global array $app_strings
     * @global array $dictionary
     * @global array $beanList
     * @param string $parent_id ID of parent bean, generally $focus
     * @param string $module $focus' module
     * @param bool asMetadata Default false
     * @return string HTML/JS for widget
     */
    public function getEmailAddressWidgetEditView($id, $module, $asMetadata = false, $tpl = '', $tabindex = '0')
    {
        if (null === $id) {
            $GLOBALS['log']->debug('ID is null so it should be a create and NOT an update');
        }
        if (null === $module) {
            $GLOBALS['log']->fatal('Invalid Argument: module');

            return false;
        }

        if (!($this->smarty instanceof Sugar_Smarty)) {
            $this->smarty = new Sugar_Smarty();
        }

        global $app_strings;
        global $dictionary;
        global $beanList;
        $configurator = new Configurator();

        $prefill = 'false';

        $prefillData = 'new Object()';
        $passedModule = $module;
        $module = $this->getCorrectedModule($module);
        $saveModule = $module;
        if (isset($_POST['is_converted']) && $_POST['is_converted'] == true) {
            if (!isset($_POST['return_id'])) {
                $GLOBALS['log']->fatal('return_id not set');
                $id = null;
            } else {
                $id = $_POST['return_id'];
            }
            if (!isset($_POST['return_module'])) {
                $GLOBALS['log']->fatal('return_module not set');
                $module = '';
            } else {
                $module = $_POST['return_module'];
            }
        }
        $prefillDataArr = array();
        if (!empty($id)) {
            $prefillDataArr = $this->getAddressesByGUID($id, $module);
            //When coming from convert leads, sometimes module is Contacts while the id is for a lead.
            if (empty($prefillDataArr) && $module == "Contacts") {
                $prefillDataArr = $this->getAddressesByGUID($id, "Leads");
            }
        } elseif (isset($_REQUEST['full_form']) && !empty($_REQUEST['emailAddressWidget'])) {
            $widget_id = isset($_REQUEST[$module . '_email_widget_id']) ? $_REQUEST[$module . '_email_widget_id'] : '0';
            $count = 0;
            $key = $module . $widget_id . 'emailAddress' . $count;
            while (isset($_REQUEST[$key])) {
                $email = $_REQUEST[$key];
                $prefillDataArr[] = array(
                    'email_address' => $email,
                    'primary_address' => isset($_REQUEST['emailAddressPrimaryFlag']) && $_REQUEST['emailAddressPrimaryFlag'] == $key,
                    'invalid_email' => isset($_REQUEST['emailAddressInvalidFlag']) && in_array(
                        $key,
                        $_REQUEST['emailAddressInvalidFlag']
                    ),
                    'opt_out' => isset($_REQUEST['emailAddressOptOutFlag']) && in_array(
                        $key,
                        $_REQUEST['emailAddressOptOutFlag']
                    ),
                    'reply_to_address' => false
                );
                $key = $module . $widget_id . 'emailAddress' . ++$count;
            } //while
        }

        if (!empty($prefillDataArr)) {
            $json = new JSON();
            $prefillData = $json->encode($prefillDataArr);
            $prefill = !empty($prefillDataArr) ? 'true' : 'false';
        }

        $required = false;
        $moduleFound = true;
        if (!isset($beanList[$passedModule])) {
            $GLOBALS['log']->fatal('Module not found in bean list: ' . $passedModule);
            $moduleFound = false;
        } elseif (!isset($dictionary[$beanList[$passedModule]])) {
            $GLOBALS['log']->fatal('Module bean not found in dictionary: ' . $beanList[$passedModule]);
            $moduleFound = false;
        }
        if ($moduleFound) {
            $vardefs = $dictionary[$beanList[$passedModule]]['fields'];
        } else {
            return false;
        }
        if (!empty($vardefs['email1']) && isset($vardefs['email1']['required']) && $vardefs['email1']['required']) {
            $required = true;
        }
        $this->smarty->assign('required', $required);

        $this->smarty->assign('module', $saveModule);
        $this->smarty->assign('index', $this->index);
        $this->smarty->assign('app_strings', $app_strings);
        $this->smarty->assign('prefillEmailAddresses', $prefill);
        $this->smarty->assign('prefillData', $prefillData);
        $this->smarty->assign('tabindex', $tabindex);
        //Set addDefaultAddress flag (do not add if it's from the Email module)
        $this->smarty->assign(
            'addDefaultAddress',
            (isset($_REQUEST['module']) && $_REQUEST['module'] == 'Emails') ? 'false' : 'true'
        );
        $form = $this->view;

        //determine if this should be a quickcreate form, or a quick create form under subpanels
        if ($this->view == "QuickCreate") {
            // Fixed #1120 - fixed email validation for: Accounts -> Contacts subpanel -> Select -> Create Contact -> Save.
            // If email is required it should highlight this field and show an error message.
            // It didnt because the form was named form_DCSubpanelQuickCreate_Contacts instead of expected form_SubpanelQuickCreate_Contacts
            if ($this->object_name = 'EmailAddress' && $saveModule == 'Contacts') {
                $form = 'form_' . $this->view . '_' . $module;
            } else {
                $form = 'form_DC' . $this->view . '_' . $module;
            }
            if (isset($_REQUEST['action']) && (isset($_REQUEST['action']) && $_REQUEST['action'] == 'SubpanelCreates' || $_REQUEST['action'] == 'SubpanelEdits')) {
                $form = 'form_Subpanel' . $this->view . '_' . $module;
            }
        }

        $this->smarty->assign('emailView', $form);

        if ($module == 'Users') {
            $this->smarty->assign('useReplyTo', true);
        } else {
            $this->smarty->assign('useOptOut', true);
            $this->smarty->assign('useInvalid', true);
            if (
                $configurator->isOptInEnabled()
                || $configurator->isConfirmOptInEnabled()
            ) {
                $this->smarty->assign('useOptIn', true);
            } else {
                $this->smarty->assign('useOptIn', false);
            }
        }

        $template = empty($tpl) ? "include/SugarEmailAddress/templates/forEditView.tpl" : $tpl;
        $newEmail = $this->smarty->fetch($template);


        if ($asMetadata) {
            // used by Email 2.0
            $return = array();
            $return['prefillData'] = $prefillDataArr;
            $return['html'] = $newEmail;

            return $return;
        }

        return $newEmail;
    }


    /**
     * Returns the HTML/JS for the EmailAddress widget
     * @param object $focus Bean in focus
     * @return string HTML/JS for widget
     */
    public function getEmailAddressWidgetDetailView($focus, $tpl = '')
    {
        if (!($this->smarty instanceof Sugar_Smarty)) {
            $this->smarty = new Sugar_Smarty();
        }

        global $app_strings;
        global $current_user;
        $assign = array();
        if (empty($focus->id)) {
            return '';
        }
        $prefillData = $this->getAddressesByGUID($focus->id, $focus->module_dir);

        foreach ($prefillData as $addressItem) {
            $key = ($addressItem['primary_address'] == 1) ? 'primary' : '';
            $key = ($addressItem['reply_to_address'] == 1) ? 'reply_to' : $key;
            $key = ($addressItem['opt_out'] == 1) ? 'opt_out' : $key;
            $key = ($addressItem['invalid_email'] == 1) ? 'invalid' : $key;
            $key = ($addressItem['opt_out'] == 1) && ($addressItem['invalid_email'] == 1) ? 'opt_out_invalid' : $key;

            $emailAddress = array(
                'key' => $key,
                'address' => $current_user->getEmailLink2($addressItem['email_address'], $focus)
            );

            if (empty($emailAddress['address'])) {
                // Email Link is missing, lets just print the email address in plain text instead.
                $emailAddress['address'] = $addressItem['email_address'];
            }

            $assign[] =$emailAddress;
        }


        $this->smarty->assign('app_strings', $app_strings);
        $this->smarty->assign('emailAddresses', $assign);
        $templateFile = empty($tpl) ? "include/SugarEmailAddress/templates/forDetailView.tpl" : $tpl;
        $return = $this->smarty->fetch($templateFile);

        return $return;
    }


    /**
     * getEmailAddressWidgetDuplicatesView($focus)
     * @param object $focus Bean in focus
     * @return string HTML that contains hidden input values based off of HTML request
     */
    public function getEmailAddressWidgetDuplicatesView($focus)
    {
        if (!($this->smarty instanceof Sugar_Smarty)) {
            $this->smarty = new Sugar_Smarty();
        }

        $count = 0;
        $emails = array();
        $primary = null;
        $optOut = array();
        $invalid = array();
        $mod = isset($focus) ? $focus->module_dir : "";

        if (!isset($_POST) || !isset($_POST[$mod . '_email_widget_id'])) {
            $GLOBALS['log']->fatal("Missing Argument: a required post variable not found: {$mod}_email_widget_id");
            $widget_id = null;
        } else {
            $widget_id = $_POST[$mod . '_email_widget_id'];
        }
        $this->smarty->assign('email_widget_id', $widget_id);

        $emailAddressWidget = null;
        if (isset($_POST['emailAddressWidget'])) {
            $emailAddressWidget = $_POST['emailAddressWidget'];
        } else {
            $GLOBALS['log']->fatal('Missing Argument: a required post variable not found: emailAddressWidget');
        }

        $this->smarty->assign('emailAddressWidget', $emailAddressWidget);

        if (isset($_POST[$mod . $widget_id . 'emailAddressPrimaryFlag'])) {
            $primary = $_POST[$mod . $widget_id . 'emailAddressPrimaryFlag'];
        }

        while (isset($_POST[$mod . $widget_id . "emailAddress" . $count])) {
            $emails[] = $_POST[$mod . $widget_id . 'emailAddress' . $count];
            $count++;
        }

        if ($count == 0) {
            return "";
        }

        if (isset($_POST[$mod . $widget_id . 'emailAddressOptOutFlag'])) {
            if (
                !is_array($_POST[$mod . $widget_id . 'emailAddressOptOutFlag']) ||
                !is_object($_POST[$mod . $widget_id . 'emailAddressOptOutFlag'])
            ) {
                $GLOBALS['log']->fatal(
                    'Invalid Argument: post variable ' .
                    $mod . $widget_id . 'emailAddressOptOutFlag' .
                    ' should be an array, ' .
                    gettype($_POST[$mod . $widget_id . 'emailAddressOptOutFlag']) . ' given'
                );
            }
            foreach ((array)$_POST[$mod . $widget_id . 'emailAddressOptOutFlag'] as $v) {
                $optOut[] = $v;
            }
        }

        if (isset($_POST[$mod . $widget_id . 'emailAddressInvalidFlag'])) {
            if (
                !is_array($_POST[$mod . $widget_id . 'emailAddressInvalidFlag']) ||
                !is_object($_POST[$mod . $widget_id . 'emailAddressInvalidFlag'])
            ) {
                $GLOBALS['log']->fatal(
                    'Invalid Argument: post variable ' .
                    $mod . $widget_id . 'emailAddressInvalidFlag' .
                    ' should be an array, ' .
                    gettype($_POST[$mod . $widget_id . 'emailAddressInvalidFlag']) . ' given'
                );
            }
            foreach ((array)$_POST[$mod . $widget_id . 'emailAddressInvalidFlag'] as $v) {
                $invalid[] = $v;
            }
        }

        if (isset($_POST[$mod . $widget_id . 'emailAddressReplyToFlag'])) {
            if (
                !is_array($_POST[$mod . $widget_id . 'emailAddressReplyToFlag']) ||
                !is_object($_POST[$mod . $widget_id . 'emailAddressReplyToFlag'])
            ) {
                $GLOBALS['log']->fatal(
                    'Invalid Argument: post variable ' .
                    $mod . $widget_id . 'emailAddressReplyToFlag' .
                    ' should be an array, ' .
                    gettype($_POST[$mod . $widget_id . 'emailAddressReplyToFlag']) . ' given'
                );
            }
            foreach ((array)$_POST[$mod . $widget_id . 'emailAddressReplyToFlag'] as $v) {
                $replyTo[] = $v;
            }
        }

        if (isset($_POST[$mod . $widget_id . 'emailAddressDeleteFlag'])) {
            if (
                !is_array($_POST[$mod . $widget_id . 'emailAddressDeleteFlag']) ||
                !is_object($_POST[$mod . $widget_id . 'emailAddressDeleteFlag'])
            ) {
                $GLOBALS['log']->fatal(
                    'Invalid Argument: post variable ' .
                    $mod . $widget_id . 'emailAddressDeleteFlag' .
                    ' should be an array, ' .
                    gettype($_POST[$mod . $widget_id . 'emailAddressDeleteFlag']) . ' given'
                );
            }
            foreach ((array)$_POST[$mod . $widget_id . 'emailAddressDeleteFlag'] as $v) {
                $delete[] = $v;
            }
        }

        while (isset($_POST[$mod . $widget_id . "emailAddressVerifiedValue" . $count])) {
            if (
                !is_array($_POST[$mod . $widget_id . 'emailAddressVerifiedValue' . $count]) ||
                !is_object($_POST[$mod . $widget_id . 'emailAddressVerifiedValue' . $count])
            ) {
                $GLOBALS['log']->fatal(
                    'Invalid Argument: post variable ' .
                    $mod . $widget_id . 'emailAddressVerifiedValue' . $count .
                    ' not found.'
                );
            }
            $verified[] = $_POST[$mod . $widget_id . 'emailAddressVerifiedValue' . $count];
            $count++;
        }

        $this->smarty->assign('emails', $emails);
        $this->smarty->assign('primary', $primary);
        $this->smarty->assign('optOut', $optOut);
        $this->smarty->assign('invalid', $invalid);
        $this->smarty->assign('replyTo', $invalid);
        $this->smarty->assign('delete', $invalid);
        $this->smarty->assign('verified', $invalid);
        $this->smarty->assign('moduleDir', $mod);

        return $this->smarty->fetch("include/SugarEmailAddress/templates/forDuplicatesView.tpl");
    }

    /**
     * getFormBaseURL
     *
     */
    public function getFormBaseURL($focus)
    {
        $get = "";
        $count = 0;
        $mod = isset($focus) ? $focus->module_dir : "";

        if (!$mod) {
            $GLOBALS['log']->fatal('Invalid Argument: Missing module dir.');

            return false;
        }

        $widget_id = '';
        if (!isset($_POST[$mod . '_email_widget_id'])) {
            $GLOBALS['log']->fatal('Invalid Argument: requested argument missing: "' . $mod . '_email_widget_id"');
        } else {
            $widget_id = $_POST[$mod . '_email_widget_id'];
        }

        $get .= '&' . $mod . '_email_widget_id=' . $widget_id;

        if (!isset($_POST['emailAddressWidget'])) {
            $GLOBALS['log']->fatal('Invalid Argument: requested argument missing: "emailAddressWidget"');
            $get .= '&emailAddressWidget=';
        } else {
            $get .= '&emailAddressWidget=' . $_POST['emailAddressWidget'];
        }


        while (isset($_REQUEST[$mod . $widget_id . 'emailAddress' . $count])) {
            $get .= "&" . $mod . $widget_id . "emailAddress" . $count . "=" . urlencode($_REQUEST[$mod . $widget_id . 'emailAddress' . $count]);
            $count++;
        } //while

        while (isset($_REQUEST[$mod . $widget_id . 'emailAddressVerifiedValue' . $count])) {
            $get .= "&" . $mod . $widget_id . "emailAddressVerifiedValue" . $count . "=" . urlencode($_REQUEST[$mod . $widget_id . 'emailAddressVerifiedValue' . $count]);
            $count++;
        } //while

        $options = array(
            'emailAddressPrimaryFlag',
            'emailAddressOptOutFlag',
            'emailAddressInvalidFlag',
            'emailAddressDeleteFlag',
            'emailAddressReplyToFlag'
        );

        foreach ($options as $option) {
            $count = 0;
            $optionIdentifier = $mod . $widget_id . $option;
            if (isset($_REQUEST[$optionIdentifier])) {
                if (is_array($_REQUEST[$optionIdentifier])) {
                    foreach ($_REQUEST[$optionIdentifier] as $optOut) {
                        $get .= "&" . $optionIdentifier . "[" . $count . "]=" . $optOut;
                        $count++;
                    } //foreach
                } else {
                    $get .= "&" . $optionIdentifier . "=" . $_REQUEST[$optionIdentifier];
                }
            } //if
        } //foreach

        return $get;
    }

    public function setView($view)
    {
        $this->view = $view;
    }

    /**
     * This function is here so the Employees/Users division can be handled cleanly in one place
     * @param object $focus SugarBean
     * @return string The value for the bean_module column in the email_addr_bean_rel table
     */
    public function getCorrectedModule(&$module)
    {
        return ($module == "Employees") ? "Users" : $module;
    }

    public function stash($parentBeanId, $moduleName)
    {
        $result = $this->db->query("SELECT email_address_id FROM email_addr_bean_rel eabr WHERE eabr.bean_id = '" . $this->db->quote($parentBeanId) . "' AND eabr.bean_module = '" . $this->db->quote($moduleName) . "' AND eabr.deleted=0");
        $this->stateBeforeWorkflow = array();
        $ids = array();
        while ($row = $this->db->fetchByAssoc($result, false)) {
            $ids[] = $this->db->quote($row['email_address_id']); // avoid 2nd order SQL Injection
        }
        if (!empty($ids)) {
            $ids = implode("', '", $ids);
            $queryEmailData = "SELECT id, email_address, invalid_email, opt_out FROM {$this->table_name} WHERE id IN ('$ids') AND deleted=0";
            $result = $this->db->query($queryEmailData);
            while ($row = $this->db->fetchByAssoc($result, false)) {
                $this->stateBeforeWorkflow[$row['id']] = array_diff_key($row, array('id' => null));
            }
        }
    }


    /**
     * Confirm opt in
     */
    public function confirmOptIn()
    {
        $this->confirm_opt_in_date = TimeDate::getInstance()->nowDb();
        $this->confirm_opt_in = self::COI_STAT_CONFIRMED_OPT_IN;
    }

    /**
     * Reset opt in
     */
    public function resetOptIn()
    {
        $this->confirm_opt_in = self::COI_STAT_DISABLED;
        parent::save();
    }

    /**
     * Update Opt In state to SugarEmailAddress::COI_STAT_OPT_IN
     *
     * @see SugarEmailAddress::COI_STAT_OPT_IN
     * @return string|bool ID or false on failed
     * @throws RuntimeException this function updates an exists SugarEmailAddress bean should have ID
     */
    public function optIn()
    {
        if (!$this->id) {
            $msg = 'Trying to update opt-in email address without email address ID.';
            LoggerManager::getLogger()->fatal($msg);
            throw new RuntimeException($msg);
        }

        if (!$this->retrieve()) {
            $msg = 'Retrieve email address for opt-in failed.';
            LoggerManager::getLogger()->fatal($msg);
            throw new RuntimeException($msg);
        }

        $state = $this->isConfirmedOptIn() ? self::COI_STAT_CONFIRMED_OPT_IN : self::COI_STAT_OPT_IN;
        if (!$this->setConfirmedOptInState($state)) {
            $msg = 'set confirm opt in state of email address "' . $this->email_address . '" failed.';
            LoggerManager::getLogger()->fatal($msg);
            throw new RuntimeException($msg);
        }

        $ret = parent::save();


        return $ret;
    }

    /**
     *
     * @param string $state
     * @return boolean
     */
    public function setConfirmedOptInState($state)
    {
        $this->confirm_opt_in = $state;
        $ret = parent::save();
        return $ret;
    }

    /**
     * It returns a ViewDefs for Confirm Opt In action link on DetailViews, specially for Accounts/Contacts/Leads/Prospects
     *
     * @param string $module module name
     * @param string $returnModule optional, using module name if null
     * @param string $returnAction optional, using module name if null
     * @param string $moduleTab optional, using module name if null
     * @return array ViewDefs for Confirm Opt In action link
     */
    public static function getSendConfirmOptInEmailActionLinkDefs($module, $returnModule = null, $returnAction = null, $moduleTab = null)
    {
        $configurator = new Configurator();
        $configOptInEnabled = $configurator->isConfirmOptInEnabled();

        $disabledAttribute = $configOptInEnabled ? '' : 'disabled="disabled"';
        $hiddenClass = $configOptInEnabled ? '' : 'hidden';

        if (is_null($returnModule)) {
            $returnModule = $module;
        }

        if (is_null($returnAction)) {
            $returnAction = $module;
        }

        if (is_null($moduleTab)) {
            $moduleTab = $module;
        }

        $ret = array(
            'customCode' =>
            '<input type="submit" class="button ' .
            $hiddenClass . '" ' . $disabledAttribute .
            ' title="{$APP.LBL_SEND_CONFIRM_OPT_IN_EMAIL}" onclick="this.form.return_module.value=\'' .
            $returnModule . '\'; this.form.return_action.value=\'' .
            $returnAction . '\'; this.form.return_id.value=\'{$fields.id.value}\'; ' .
            'this.form.action.value=\'sendConfirmOptInEmail\'; this.form.module.value=\'' .
            $module . '\'; this.form.module_tab.value=\'' . $moduleTab .
            '\';" name="send_confirm_opt_in_email" value="{$APP.LBL_SEND_CONFIRM_OPT_IN_EMAIL}"/>',
            'sugar_html' =>
            array(
                'type' => 'submit',
                'value' => '{$APP.LBL_SEND_CONFIRM_OPT_IN_EMAIL}',
                'htmlOptions' =>
                array(
                    'class' => 'button ' . $hiddenClass,
                    'id' => 'send_confirm_opt_in_email',
                    'title' => '{$APP.LBL_SEND_CONFIRM_OPT_IN_EMAIL}',
                    'onclick' => 'this.form.return_module.value=\'' . $returnModule .
                    '\'; this.form.return_action.value=\'DetailView\'; this.form.return_id.value=\'{$fields.id.value}\'; ' .
                    'this.form.action.value=\'sendConfirmOptInEmail\'; this.form.module.value=\'' . $module .
                    '\'; this.form.module_tab.value=\'' . $moduleTab . '\';',
                    'name' => 'send_confirm_opt_in_email',
                ),
            ),
        );

        if (!$configOptInEnabled) {
            $ret['sugar_html']['htmlOptions']['disabled'] = true;
        }

        return $ret;
    }


    /**
     * Uses the configuration to determine opt in status
     * @return string
     */
    public function getOptInStatus()
    {
        $configurator = new Configurator();

        $enableConfirmedOptIn = null;
        if (isset($configurator->config['email_enable_confirm_opt_in'])) {
            $enableConfirmedOptIn = $configurator->config['email_enable_confirm_opt_in'];
        } else {
            LoggerManager::getLogger()->warn('EmailUI::populateComposeViewFields: $configurator->config[email_enable_confirm_opt_in] is not set');
        }

        $optInFromFlags = $this->getOptInIndicationFromFlags();

        if ($enableConfirmedOptIn === self::COI_STAT_DISABLED) {
            $ret = self::COI_FLAG_OPT_IN_DISABLED;
        } elseif (
            $enableConfirmedOptIn === self::COI_STAT_OPT_IN
            && $this->isOptedInStatus($optInFromFlags)
        ) {
            $ret = self::COI_FLAG_OPT_IN;
        } elseif ($enableConfirmedOptIn === self::COI_STAT_CONFIRMED_OPT_IN) {
            $ret = $optInFromFlags;
        } elseif ($optInFromFlags === self::COI_FLAG_INVALID) {
            $ret = $optInFromFlags;
        } elseif ($optInFromFlags === self::COI_FLAG_OPT_OUT) {
            $ret = $optInFromFlags;
        } else {
            $msg = 'Invalid ENUM value of Opt In settings: ' . $enableConfirmedOptIn;
            LoggerManager::getLogger()->warn($msg);
            $ret = self::COI_FLAG_NO_OPT_IN_STATUS;
        }

        return $ret;
    }

    /**
     * Determines the opt in status without considering the configuration
     * @return string
     * @throws  RuntimeException
     */
    private function getOptInIndicationFromFlags()
    {
        $log = LoggerManager::getLogger();

        if (!in_array($this->module_name, self::$doNotDisplayOptInTickForModule, true)) {
            if ((int)$this->invalid_email === 1) {
                $ret = self::COI_FLAG_INVALID;
                return $ret;
            }

            if ((int)$this->opt_out === 1) {
                $ret = self::COI_FLAG_OPT_OUT;
                return $ret;
            }

            if ($this->isNotOptIn()) {
                $ret = self::COI_STAT_DISABLED;
                return $ret;
            }

            $ret = self::COI_FLAG_UNKNOWN_OPT_IN_STATUS;

            if ($this->isConfirmedOptIn()) {
                $ret = self::COI_FLAG_OPT_IN_PENDING_EMAIL_CONFIRMED;
            } elseif (
                $this->isConfirmOptInEmailNotSent()
                && $this->getConfirmedOptInState() !== self::COI_STAT_DISABLED
            ) {
                $ret = self::COI_FLAG_OPT_IN_PENDING_EMAIL_NOT_SENT;
            } elseif ($this->isConfirmOptInEmailSent()) {
                $ret = self::COI_FLAG_OPT_IN_PENDING_EMAIL_SENT;
            } elseif ($this->isConfirmOptInEmailFailed()) {
                $ret = self::COI_FLAG_OPT_IN_PENDING_EMAIL_FAILED;
            }
            return $ret;
        }

        $ret = self::COI_FLAG_NO_OPT_IN_STATUS;
        return $ret;
    }


    /**
     * @return bool true when the an confirm optin email was successfully sent
     * @throws Exception
     */
    private function isConfirmOptInEmailSent()
    {
        if (empty($this->confirm_opt_in_sent_date)) {
            return false;
        }

        try {
            $maxdate = $this->dateMax($this->confirm_opt_in_sent_date, $this->confirm_opt_in_fail_date);
            if ($maxdate === $this->confirm_opt_in_fail_date) {
                return false;
            } elseif ($maxdate === $this->confirm_opt_in_sent_date) {
                return true;
            } else {
                throw new Exception('its impossible email sending state');
            }
        } catch (RuntimeException $e) {
            if (!empty($this->confirm_opt_in_fail_date)) {
                throw $e;
            }
            return true;
        }
    }

    /**
     * @return bool true when the an confirm optin email failed to send
     * @throws Exception
     */
    private function isConfirmOptInEmailFailed()
    {
        if (empty($this->confirm_opt_in_fail_date)) {
            return false;
        }

        try {
            $maxdate = $this->dateMax($this->confirm_opt_in_sent_date, $this->confirm_opt_in_fail_date);
            if ($maxdate === $this->confirm_opt_in_fail_date) {
                return true;
            } elseif ($maxdate === $this->confirm_opt_in_sent_date) {
                return false;
            } else {
                throw new Exception('its impossible email sending state');
            }
        } catch (RuntimeException $e) {
            if (!empty($this->confirm_opt_in_sent_date)) {
                throw $e;
            }
            return false;
        }
    }

    /**
     * @return bool if confirm opt in email has not yet been sent
     */
    private function isConfirmOptInEmailNotSent()
    {
        if (
            empty($this->confirm_opt_in_sent_date)
            && empty($this->confirm_opt_in_fail_date)
        ) {
            return true;
        }
        return false;
    }

    /**
     * @return bool true when confirmed opt in has been set
     */
    private function isConfirmedOptIn()
    {
        $ret =  $this->getConfirmedOptInState() === self::COI_STAT_CONFIRMED_OPT_IN;
        return $ret;
    }

    /**
     * @return bool
     */
    private function isNotOptIn()
    {
        return $this->confirm_opt_in === self::COI_STAT_DISABLED;
    }

    /**
     * @param string $date1
     * @param string $date2
     * @return bool
     * @throws \RuntimeException
     */
    private function dateCompare($date1, $date2)
    {
        $time1 = strtotime($date1);
        $time2 = strtotime($date2);

        $err = 'unable to convert strtotime: ';
        if ($time1 === -1) {
            throw new RuntimeException($err . $date1);
        }

        if ($time2 === -1) {
            throw new RuntimeException($err . $date2);
        }

        return $time1 > $time2;
    }


    /**
     * @param string $date1
     * @param string $date2
     * @return string
     */
    private function dateMax($date1, $date2)
    {
        return $this->dateCompare($date1, $date2) ? $date1 : $date2;
    }

    /**
     * @param string $emailAddressIndicatorStatus
     * @return bool
     */
    private function isOptedInStatus($emailAddressIndicatorStatus = self::COI_FLAG_NO_OPT_IN_STATUS)
    {
        $ret = in_array($emailAddressIndicatorStatus, array(
            self::COI_FLAG_OPT_IN_PENDING_EMAIL_CONFIRMED,
            self::COI_FLAG_OPT_IN_PENDING_EMAIL_SENT,
            self::COI_FLAG_OPT_IN_PENDING_EMAIL_NOT_SENT,
            self::COI_FLAG_OPT_IN_PENDING_EMAIL_FAILED,
        ), true);
        return $ret;
    }



    /**
     * @global array $app_strings
     * @return string
     */
    public function getOptInStatusTickHTML()
    {
        global $app_strings;

        $configurator = new Configurator();
        $sugar_config = $configurator->config;

        $tickHtml = '';

        if (isset($sugar_config['email_enable_confirm_opt_in'])) {
            $emailConfigEnableConfirmOptIn = $sugar_config['email_enable_confirm_opt_in'];

            $template = new Sugar_Smarty();

            $optInStatus = $this->getOptInStatus();
            switch ($optInStatus) {
                    case self::COI_FLAG_OPT_IN:
                        $optInFlagClass = 'email-opt-in-confirmed';
                        $optInFlagTitle = $app_strings['LBL_OPT_IN'];
                        $optInFlagText = '<span class="suitepicon suitepicon-action-confirm"></span>';
                        break;
                    case self::COI_FLAG_OPT_IN_PENDING_EMAIL_CONFIRMED:
                        $optInFlagClass = 'email-opt-in-confirmed';
                        $optInFlagTitle = $app_strings['LBL_OPT_IN_CONFIRMED'];
                        $optInFlagText = '<span class="suitepicon suitepicon-action-confirm">';
                        $optInFlagText .= '</span><span class="suitepicon suitepicon-action-confirm"></span>';
                        break;
                    case self::COI_FLAG_OPT_IN_PENDING_EMAIL_SENT:
                        $optInFlagClass = 'email-opt-in-sent';
                        $optInFlagTitle = $app_strings['LBL_OPT_IN_PENDING_EMAIL_SENT'];
                        $optInFlagText = '<span class="suitepicon suitepicon-action-confirm"></span>';
                        break;
                    case self::COI_FLAG_OPT_IN_PENDING_EMAIL_NOT_SENT:
                        $optInFlagClass = 'email-opt-in-not-sent';
                        $optInFlagTitle = $app_strings['LBL_OPT_IN_PENDING_EMAIL_NOT_SENT'];
                        $optInFlagText = '<span class="suitepicon suitepicon-action-confirm"></span>';
                        break;
                    case self::COI_FLAG_OPT_IN_PENDING_EMAIL_FAILED:
                        $optInFlagClass = 'email-opt-in-failed';
                        $optInFlagTitle = $app_strings['LBL_OPT_IN_PENDING_EMAIL_FAILED'];
                        $optInFlagText = '<span class="suitepicon suitepicon-action-confirm"></span>';
                        break;
                    case self::COI_FLAG_OPT_OUT:
                        $optInFlagClass = 'email-opt-in-opt-out';
                        $optInFlagTitle = $app_strings['LBL_OPT_IN_OPT_OUT'];
                        $optInFlagText = '❌';
                        break;
                    case self::COI_FLAG_INVALID:
                        $optInFlagClass = 'email-opt-in-invalid';
                        $optInFlagTitle = $app_strings['LBL_OPT_IN_INVALID'];
                        $optInFlagText = '?';
                        break;
                    default:
                        $optInFlagClass = '';
                        $optInFlagTitle = '';
                        $optInFlagText = '';
                        break;
                }

            $template->assign('optInFlagClass', $optInFlagClass);
            $template->assign('optInFlagTitle', $optInFlagTitle);
            $template->assign('optInFlagText', $optInFlagText);
            $tickHtml = $template->fetch('include/SugarEmailAddress/templates/optInStatusTick.tpl');
        }

        return $tickHtml;
    }

    /**
     *
     * @return string
     */
    public function getConfirmOptInTokenGenerateIfNotExists()
    {
        if (!$this->confirm_opt_in_token) {
            $this->confirm_opt_in_token = md5(time() . md5($this->email_address) . md5(mt_rand(0, 9999999))) . md5(mt_rand(0, 9999999));
            $this->save();
        }
        return $this->confirm_opt_in_token;
    }
} // end class def

require_once __DIR__.'/getEmailAddressWidget.php';
