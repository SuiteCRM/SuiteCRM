<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2019 SalesAgility Ltd.
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

require_once('include/SugarObjects/templates/person/Person.php');
require_once __DIR__ . '/../../include/EmailInterface.php';
require_once __DIR__ . '/../Emails/EmailUI.php';

// User is used to store customer information.
class User extends Person implements EmailInterface
{

    // Stored fields
    public $name = '';
    public $full_name;
    public $id;
    public $user_name;
    public $user_hash;
    public $salutation;
    public $first_name;
    public $last_name;
    public $date_entered;
    public $date_modified;
    public $modified_user_id;
    public $created_by;
    public $created_by_name;
    public $modified_by_name;
    public $description;
    public $phone_home;
    public $phone_mobile;
    public $phone_work;
    public $phone_other;
    public $phone_fax;
    public $email1;
    public $email2;
    public $address_street;
    public $address_city;
    public $address_state;
    public $address_postalcode;
    public $address_country;
    public $status;
    public $title;
    public $photo;
    public $portal_only;
    public $department;
    public $authenticated = false;
    public $error_string;
    public $is_admin;
    public $employee_status;
    public $messenger_id;
    public $messenger_type;
    public $is_group;
    public $accept_status; // to support Meetings
    //adding a property called team_id so we can populate it for use in the team widget
    public $team_id;
    public $receive_notifications;
    public $reports_to_name;
    public $reports_to_id;
    public $team_exists = false;
    public $table_name = "users";
    public $module_dir = 'Users';
    public $object_name = "User";
    public $user_preferences;
    public $importable = true;
    public $_userPreferenceFocus;
    public $encodeFields = array("first_name", "last_name", "description");
    // This is used to retrieve related fields from form posts.
    public $additional_column_fields = array(
        'reports_to_name'
    );
    public $emailAddress;
    public $new_schema = true;

    /**
     * @var bool
     */
    public $factor_auth;

    /**
     * @var string
     */
    public $factor_auth_interface;

    /**
     * Normally a bean returns ID from save() method if it was
     * success and false (or maybe null) is something went wrong.
     * BUT (for some reason) if User bean saved properly except
     * the email addresses of it, this User::save() method also
     * return a false.
     * It's a confusing ambiguous return value for caller method.
     *
     * To handle this issue when save method can not save email
     * addresses and return false it also set this variable to
     * true.
     *
     * @var bool|null
     */
    public $lastSaveErrorIsEmailAddressSaveError = null;

    public function __construct()
    {
        parent::__construct();

        $this->_loadUserPreferencesFocus();
    }

    public function __set($key, $value)
    {
        $this->$key = $value;
        if ($key == 'id' && $value == '1') {
            $GLOBALS['log']->fatal('DEBUG: User::' . $key . ' set to ' . $value);
        }
    }



    protected function _loadUserPreferencesFocus()
    {
        $this->_userPreferenceFocus = new UserPreference($this);
    }

    /**
     * returns an admin user
     */
    public function getSystemUser()
    {
        if (null === $this->retrieve('1')) { // handle cases where someone deleted user with id "1"
            $this->retrieve_by_string_fields(array(
                'status' => 'Active',
                'is_admin' => '1',
            ));
        }

        return $this;
    }

    /**
     * convenience function to get user's default signature
     * return array
     */
    public function getDefaultSignature()
    {
        if ($defaultId = $this->getPreference('signature_default')) {
            return $this->getSignature($defaultId);
        }
        return array();
    }

    /**
     * retrieves the signatures for a user
     * @param string id ID of user_signature
     * @return array ID, signature, and signature_html
     */
    public function getSignature($id)
    {
        $signatures = $this->getSignaturesArray();

        return isset($signatures[$id]) ? $signatures[$id] : false;
    }

    /**
     * @param bool $useRequestedRecord
     * @return array
     * @throws \RuntimeException
     */
    public function getSignaturesArray($useRequestedRecord = false)
    {
        if ($useRequestedRecord) {
            $user = $this->getRequestedUserRecord();
            $uid = $user->id;
        } else {
            $uid = $this->id;
        }

        $q = 'SELECT * FROM users_signatures WHERE user_id = \'' . $uid . '\' AND deleted = 0 ORDER BY name ASC';
        $r = $this->db->query($q);

        // provide "none"
        $sig = array("" => "");

        while ($a = $this->db->fetchByAssoc($r)) {
            $sig[$a['id']] = $a;
        }

        return $sig;
    }

    /**
     * retrieves any signatures that the User may have created as <select>
     * @param bool $live
     * @param string $defaultSig
     * @param bool $forSettings
     * @param string $elementId
     * @param bool $useRequestedRecord
     * @return string
     * @throws \RuntimeException
     */
    public function getSignatures(
        $live = false,
        $defaultSig = '',
        $forSettings = false,
        $elementId = 'signature_id',
        $useRequestedRecord = false
    ) {
        $sig = $this->getSignaturesArray($useRequestedRecord);
        $sigs = array();
        foreach ($sig as $key => $arr) {
            $sigs[$key] = !empty($arr['name']) ? $arr['name'] : '';
        }

        $change = '';
        if (!$live) {
            $change = ($forSettings) ? "onChange='displaySignatureEdit();'" : "onChange='setSigEditButtonVisibility();'";
        }

        $id = (!$forSettings) ? $elementId : 'signature_idDisplay';

        $out = "<select {$change} id='{$id}' name='{$id}'>";
        $out .= get_select_options_with_id($sigs, $defaultSig) . '</select>';

        return $out;
    }

    /**
     * retrieves any signatures that the User may have created as <select>
     * @param bool $live
     * @param string $defaultSig
     * @param bool $forSettings
     * @param string $elementId
     * @param bool $useRequestedRecord
     * @return string
     * @throws \RuntimeException
     */
    public function getEmailAccountSignatures(
        $live = false,
        $defaultSig = '',
        $forSettings = false,
        $elementId = 'account_signature_id',
        $useRequestedRecord = false
    ) {
        $sig = $this->getSignaturesArray($useRequestedRecord);
        $sigs = array();
        foreach ($sig as $key => $arr) {
            $sigs[$key] = !empty($arr['name']) ? $arr['name'] : '';
        }

        $change = '';
        if (!$live) {
            $change = ($forSettings) ? "onChange='displaySignatureEdit();'" : "onChange='setSigEditButtonVisibility();'";
        }

        $id = (!$forSettings) ? $elementId : 'signature_idDisplay';

        $out = "<select {$change} id='{$id}' name='{$id}'>";
        if (empty($defaultSig)) {
            $out .= get_select_empty_option($defaultSig, true, 'LBL_DEFAULT_EMAIL_SIGNATURES');
        } else {
            $out .= get_select_empty_option($defaultSig, false, 'LBL_DEFAULT_EMAIL_SIGNATURES');
        }
        $out .= get_select_full_options_with_id($sigs, $defaultSig);
        $out .= '</select>';

        return $out;
    }

    /**
     * returns buttons and JS for signatures
     */
    public function getSignatureButtons($jscall = '', $defaultDisplay = false)
    {
        global $mod_strings;

        $jscall = empty($jscall) ? 'open_email_signature_form' : $jscall;

        $butts = "<input class='button' onclick='javascript:{$jscall}(\"\", \"{$this->id}\");' value='{$mod_strings['LBL_BUTTON_CREATE']}' type='button'>&nbsp;";
        if ($defaultDisplay) {
            $butts .= '<span name="edit_sig" id="edit_sig" style="visibility:inherit;"><input class="button" onclick="javascript:' . $jscall . '(document.getElementById(\'signature_id\', \'\').value)" value="' . $mod_strings['LBL_BUTTON_EDIT'] . '" type="button" tabindex="392">&nbsp;
					</span>';
        } else {
            $butts .= '<span name="edit_sig" id="edit_sig" style="visibility:hidden;"><input class="button" onclick="javascript:' . $jscall . '(document.getElementById(\'signature_id\', \'\').value)" value="' . $mod_strings['LBL_BUTTON_EDIT'] . '" type="button" tabindex="392">&nbsp;
					</span>';
        }

        return $butts;
    }

    /**
     * performs a rudimentary check to verify if a given user has setup personal
     * InboundEmail
     *
     * @return bool
     */
    public function hasPersonalEmail()
    {
        $focus = new InboundEmail;
        $focus->retrieve_by_string_fields(array('group_id' => $this->id));

        return !empty($focus->id);
    }

    /* Returns the User's private GUID; this is unassociated with the User's
     * actual GUID.  It is used to secure file names that must be HTTP://
     * accesible, but obfusicated.
     */

    public function getUserPrivGuid()
    {
        $userPrivGuid = $this->getPreference('userPrivGuid', 'global', $this);
        if ($userPrivGuid) {
            return $userPrivGuid;
        }
        $this->setUserPrivGuid();
        if (!isset($_SESSION['setPrivGuid'])) {
            $_SESSION['setPrivGuid'] = true;
            $userPrivGuid = $this->getUserPrivGuid();

            return $userPrivGuid;
        }
        sugar_die("Breaking Infinite Loop Condition: Could not setUserPrivGuid.");
    }

    public function setUserPrivGuid()
    {
        $privGuid = create_guid();
        //($name, $value, $nosession=0)
        $this->setPreference('userPrivGuid', $privGuid, 0, 'global', $this);
    }

    /**
     * Interface for the User object to calling the UserPreference::setPreference() method in modules/UserPreferences/UserPreference.php
     *
     * @see UserPreference::setPreference()
     *
     * @param string $name Name of the preference to set
     * @param string $value Value to set preference to
     * @param null $nosession For BC, ignored
     * @param string $category Name of the category to retrieve
     */
    public function setPreference(
        $name,
        $value,
        $nosession = 0,
        $category = 'global'
    ) {
        // for BC
        if (func_num_args() > 4) {
            $user = func_get_arg(4);
            $GLOBALS['log']->deprecated('User::setPreferences() should not be used statically.');
        } else {
            $user = $this;
        }

        $user->_userPreferenceFocus->setPreference($name, $value, $category);
    }

    /**
     * Interface for the User object to calling the UserPreference::resetPreferences() method in modules/UserPreferences/UserPreference.php
     *
     * @see UserPreference::resetPreferences()
     *
     * @param string $category category to reset
     */
    public function resetPreferences(
        $category = null
    ) {
        // for BC
        if (func_num_args() > 1) {
            $user = func_get_arg(1);
            $GLOBALS['log']->deprecated('User::resetPreferences() should not be used statically.');
        } else {
            $user = $this;
        }

        $user->_userPreferenceFocus->resetPreferences($category);
    }

    /**
     * Interface for the User object to calling the UserPreference::savePreferencesToDB() method in modules/UserPreferences/UserPreference.php
     *
     * @see UserPreference::savePreferencesToDB()
     */
    public function savePreferencesToDB()
    {
        // for BC
        if (func_num_args() > 0) {
            $user = func_get_arg(0);
            $GLOBALS['log']->deprecated('User::savePreferencesToDB() should not be used statically.');
        } else {
            $user = $this;
        }

        $user->_userPreferenceFocus->savePreferencesToDB();
    }

    /**
     * Unconditionally reloads user preferences from the DB and updates the session
     * @param string $category name of the category to retreive, defaults to global scope
     * @return bool successful?
     */
    public function reloadPreferences($category = 'global')
    {
        return $this->_userPreferenceFocus->reloadPreferences($category = 'global');
    }

    /**
     * Interface for the User object to calling the UserPreference::getUserDateTimePreferences() method in modules/UserPreferences/UserPreference.php
     *
     * @see UserPreference::getUserDateTimePreferences()
     *
     * @return array 'date' - date format for user ; 'time' - time format for user
     */
    public function getUserDateTimePreferences()
    {
        // for BC
        if (func_num_args() > 0) {
            $user = func_get_arg(0);
            $GLOBALS['log']->deprecated('User::getUserDateTimePreferences() should not be used statically.');
        } else {
            $user = $this;
        }

        return $user->_userPreferenceFocus->getUserDateTimePreferences();
    }

    /**
     * Interface for the User object to calling the UserPreference::loadPreferences() method in modules/UserPreferences/UserPreference.php
     *
     * @see UserPreference::loadPreferences()
     *
     * @param string $category name of the category to retreive, defaults to global scope
     * @return bool successful?
     */
    public function loadPreferences(
        $category = 'global'
    ) {
        // for BC
        if (func_num_args() > 1) {
            $user = func_get_arg(1);
            $GLOBALS['log']->deprecated('User::loadPreferences() should not be used statically.');
        } else {
            $user = $this;
        }

        return $user->_userPreferenceFocus->loadPreferences($category);
    }

    /**
     * @return bool|SugarBean
     * @throws \RuntimeException
     */
    public function getRequestedUserRecord()
    {
        if (!isset($_REQUEST['record']) || !$_REQUEST['record']) {
            throw new RuntimeException('Error: requested record is not set');
        }
        $user = BeanFactory::getBean('Users', $_REQUEST['record']);
        if (!$user) {
            throw new RuntimeException('Error: retrieve requested user record');
        }
        $uid = $user->id;
        if (!$uid) {
            throw new RuntimeException('Error: retrieve requested user ID');
        }

        return $user;
    }

    /**
     * Interface for the User object to calling the UserPreference::setPreference() method in modules/UserPreferences/UserPreference.php
     *
     * @see UserPreference::getPreference()
     *
     * @param string $name name of the preference to retreive
     * @param string $category name of the category to retreive, defaults to global scope
     * @return mixed the value of the preference (string, array, int etc)
     * @internal param bool $useRequestedRecord
     */
    public function getPreference(
        $name,
        $category = 'global'
    ) {
        // for BC
        if (func_num_args() > 2) {
            $user = func_get_arg(2);
            $GLOBALS['log']->deprecated('User::getPreference() should not be used statically.');
        } else {
            $user = $this;
        }

        return $user->_userPreferenceFocus->getPreference($name, $category);
    }

    /**
     * incrementETag
     *
     * This function increments any ETag seed needed for a particular user's
     * UI. For example, if the user changes their theme, the ETag seed for the
     * main menu needs to be updated, so you call this function with the seed name
     * to do so:
     *
     * UserPreference::incrementETag("mainMenuETag");
     *
     * @param string $tag ETag seed name.
     * @return nothing
     */
    public function incrementETag($tag)
    {
        $val = $this->getETagSeed($tag);
        if ($val == 2147483648) {
            $val = 0;
        }
        $val++;
        $this->setPreference($tag, $val, 0, "ETag");
    }

    /**
     * getETagSeed
     *
     * This function is a wrapper to encapsulate getting the ETag seed and
     * making sure it's sanitized for use in the app.
     *
     * @param string $tag ETag seed name.
     * @return integer numeric value of the seed
     */
    public function getETagSeed($tag)
    {
        $val = $this->getPreference($tag, "ETag");
        if ($val == null) {
            $val = 0;
        }

        return $val;
    }

    /**
     * Get WHERE clause that fetches all users counted for licensing purposes
     * @return string
     */
    public static function getLicensedUsersWhere()
    {
        return "deleted=0 AND status='Active' AND user_name IS NOT NULL AND is_group=0 AND portal_only=0  AND " . DBManagerFactory::getInstance()->convert('user_name', 'length') . ">0";

        return "1<>1";
    }

    /**
     * Normally a bean returns ID from save() method if it was
     * success and false (or maybe null) is something went wrong.
     * BUT (for some reason) if User bean saved properly except
     * the email addresses of it, this User::save() method also
     * return a false.
     * It's a confusing ambiguous return value for caller method.
     *
     * To handle this issue when save method can not save email
     * addresses and return false it also set the variable called
     * User::$lastSaveErrorIsEmailAddressSaveError to true.
     *
     * @global User $current_user
     * @global array $sugar_config
     * @global array $mod_strings
     * @param bool $check_notify
     * @return bool|string
     * @throws SuiteException
     */
    public function save($check_notify = false)
    {
        global $current_user, $mod_strings;

        if (!$this->hasSaveAccess()) {
           throw new RuntimeException('Not authorized');
        }

        $msg = '';

        $isUpdate = $this->isUpdate();

        //No SMTP server is set up Error.
        $admin = BeanFactory::newBean('Administration');
        $smtp_error = $admin->checkSmtpError();

        // only admin user can change 2 factor authentication settings
        if ($smtp_error || $isUpdate && !is_admin($current_user)) {
            $tmpUser = BeanFactory::getBean('Users', $this->id);
            if (!$tmpUser instanceof User) {
                LoggerManager::getLogger()->fatal('User update error: Temp User is not retrieved at ID ' . $this->id . ', ' . gettype($tmpUser) . ' given');
            }

            if ($smtp_error) {
                $msg .= 'SMTP server settings required first.';
                $GLOBALS['log']->warn($msg);
                if (isset($mod_strings['ERR_USER_FACTOR_SMTP_REQUIRED'])) {
                    SugarApplication::appendErrorMessage($mod_strings['ERR_USER_FACTOR_SMTP_REQUIRED']);
                }
            } else {
                if (($tmpUser instanceof User) && ($this->factor_auth !== $tmpUser->factor_auth || $this->factor_auth_interface !== $tmpUser->factor_auth_interface)) {
                    $msg .= 'Current user is not able to change two factor authentication settings.';
                    $GLOBALS['log']->warn($msg);
                    SugarApplication::appendErrorMessage($mod_strings['ERR_USER_FACTOR_CHANGE_DISABLED']);
                }
            }
            if ($tmpUser) {
                $this->factor_auth = $tmpUser->factor_auth;
                $this->factor_auth_interface = $tmpUser->factor_auth_interface;
            }
        }

        if ($this->factor_auth && $isUpdate && is_admin($current_user)) {
            $factorAuthFactory = new FactorAuthFactory();
            $factorAuth = $factorAuthFactory->getFactorAuth($this);

            if (!$factorAuth->validateTokenMessage()) {
                $this->factor_auth = false;
            }
        }

        // is_group & portal should be set to 0 by default
        if (!isset($this->is_group)) {
            $this->is_group = 0;
        }
        if (!isset($this->portal_only)) {
            $this->portal_only = 0;
        }

        // wp: do not save user_preferences in this table, see user_preferences module
        $this->user_preferences = '';


        // If the current user is not an admin, reset the admin flag to the original value.
        $this->setIsAdmin();

        // if this is an admin user, do not allow is_group or portal_only flag to be set.
        if ($this->is_admin) {
            $this->is_group = 0;
            $this->portal_only = 0;
        }

        // set some default preferences when creating a new user
        $setNewUserPreferences = empty($this->id) || !empty($this->new_with_id);

        if (!$this->verify_data()) {
            SugarApplication::appendErrorMessage($this->error_string);
            return SugarApplication::redirect('Location: index.php?action=Error&module=Users');
        }


        $retId = parent::save($check_notify);
        if (!$retId) {
            LoggerManager::getLogger()->fatal('save error: User is not saved, Person ID is not returned.');
        }
        if ($retId !== $this->id) {
            LoggerManager::getLogger()->fatal('save error: User is not saved properly, returned Person ID does not match to User ID.');
        }
        // set some default preferences when creating a new user
        if ($setNewUserPreferences) {
            if (!$this->getPreference('calendar_publish_key')) {
                $this->setPreference('calendar_publish_key', create_guid());
            }
        }

        $this->saveFormPreferences();

        $this->savePreferencesToDB();

        if ((isset($_POST['old_password']) || $this->portal_only) &&
            (isset($_POST['new_password']) && !empty($_POST['new_password'])) &&
            (isset($_POST['password_change']) && $_POST['password_change'] === 'true')) {
            if (!$this->change_password($_POST['old_password'], $_POST['new_password'])) {
                if (isset($_POST['page']) && $_POST['page'] === 'EditView') {
                    SugarApplication::appendErrorMessage($this->error_string);
                    SugarApplication::redirect("Location: index.php?action=EditView&module=Users&record=" . $_POST['record']);
                }
                if (isset($_POST['page']) && $_POST['page'] === 'Change') {
                    SugarApplication::appendErrorMessage($this->error_string);
                    SugarApplication::redirect("Location: index.php?action=ChangePassword&module=Users&record=" . $_POST['record']);
                }
            }
        }

        // User Profile specific save for Email addresses
        $this->lastSaveErrorIsEmailAddressSaveError = false;
        if (!$this->emailAddress->saveAtUserProfile($_REQUEST)) {
            LoggerManager::getLogger()->fatal('Email address save error');
            $this->lastSaveErrorIsEmailAddressSaveError = true;
            return false;
        }

        return $this->id;
    }

    public function saveFormPreferences()
    {
        if (!$this->is_group && !$this->portal_only) {
            require_once('modules/MySettings/TabController.php');

            global $current_user, $sugar_config, $app_strings, $mod_strings, $current_language;

            $display_tabs_def = isset($_REQUEST['display_tabs_def']) ? urldecode($_REQUEST['display_tabs_def']) : '';
            $hide_tabs_def = isset($_REQUEST['hide_tabs_def']) ? urldecode($_REQUEST['hide_tabs_def']) : '';
            $remove_tabs_def = isset($_REQUEST['remove_tabs_def']) ? urldecode($_REQUEST['remove_tabs_def']) : '';

            $DISPLAY_ARR = array();
            $HIDE_ARR = array();
            $REMOVE_ARR = array();

            parse_str($display_tabs_def, $DISPLAY_ARR);
            parse_str($hide_tabs_def, $HIDE_ARR);
            parse_str($remove_tabs_def, $REMOVE_ARR);

            $this->is_group = 0;
            $this->portal_only = 0;

            if (is_admin($current_user) && ((isset($_POST['is_admin']) && ($_POST['is_admin'] === 'on' || $_POST['is_admin'] === '1')) || (isset($_POST['UserType']) && $_POST['UserType'] === 'Administrator'))) {
                $this->is_admin = 1;
            } elseif (isset($_POST['is_admin']) && empty($_POST['is_admin'])) {
                $this->is_admin = 0;
            }

            if (isset($_POST['mailmerge_on']) && !empty($_POST['mailmerge_on'])) {
                $this->setPreference('mailmerge_on', 'on', 0, 'global');
            } else {
                $this->setPreference('mailmerge_on', 'off', 0, 'global');
            }

            if (isset($_POST['user_swap_last_viewed'])) {
                $this->setPreference('swap_last_viewed', $_POST['user_swap_last_viewed'], 0, 'global');
            } else {
                $this->setPreference('swap_last_viewed', '', 0, 'global');
            }

            if (isset($_POST['user_swap_shortcuts'])) {
                $this->setPreference('swap_shortcuts', $_POST['user_swap_shortcuts'], 0, 'global');
            } else {
                $this->setPreference('swap_shortcuts', '', 0, 'global');
            }

            if (isset($_POST['use_group_tabs'])) {
                $this->setPreference('navigation_paradigm', $_POST['use_group_tabs'], 0, 'global');
            } else {
                $this->setPreference('navigation_paradigm', $GLOBALS['sugar_config']['default_navigation_paradigm'], 0, 'global');
            }

            if (isset($_POST['sort_modules_by_name'])) {
                $this->setPreference('sort_modules_by_name', $_POST['sort_modules_by_name'], 0, 'global');
            } else {
                $this->setPreference('sort_modules_by_name', '', 0, 'global');
            }

            if (isset($_POST['user_subpanel_tabs'])) {
                $this->setPreference('subpanel_tabs', $_POST['user_subpanel_tabs'], 0, 'global');
            } else {
                $this->setPreference('subpanel_tabs', '', 0, 'global');
            }

            if (isset($_POST['user_count_collapsed_subpanels'])) {
                $this->setPreference('count_collapsed_subpanels', $_POST['user_count_collapsed_subpanels'], 0, 'global');
            } else {
                $this->setPreference('count_collapsed_subpanels', '', 0, 'global');
            }

            if (isset($_POST['user_theme'])) {
                $this->setPreference('user_theme', $_POST['user_theme'], 0, 'global');
                $_SESSION['authenticated_user_theme'] = $_POST['user_theme'];
            }

            if (isset($_POST['user_module_favicon'])) {
                $this->setPreference('module_favicon', $_POST['user_module_favicon'], 0, 'global');
            } else {
                $this->setPreference('module_favicon', '', 0, 'global');
            }

            $tabs = new TabController();
            if (isset($_POST['display_tabs'])) {
                $tabs->set_user_tabs($DISPLAY_ARR['display_tabs'], $this, 'display');
            }
            if (isset($HIDE_ARR['hide_tabs'])) {
                $tabs->set_user_tabs($HIDE_ARR['hide_tabs'], $this, 'hide');
            } else {
                $tabs->set_user_tabs(array(), $this, 'hide');
            }
            if (is_admin($current_user)) {
                if (isset($REMOVE_ARR['remove_tabs'])) {
                    $tabs->set_user_tabs($REMOVE_ARR['remove_tabs'], $this, 'remove');
                } else {
                    $tabs->set_user_tabs(array(), $this, 'remove');
                }
            }

            if (isset($_POST['no_opps'])) {
                $this->setPreference('no_opps', $_POST['no_opps'], 0, 'global');
            } else {
                $this->setPreference('no_opps', 'off', 0, 'global');
            }

            if (isset($_POST['reminder_time'])) {
                $this->setPreference('reminder_time', $_POST['reminder_time'], 0, 'global');
            }
            if (isset($_POST['email_reminder_time'])) {
                $this->setPreference('email_reminder_time', $_POST['email_reminder_time'], 0, 'global');
            }
            if (isset($_POST['reminder_checked'])) {
                $this->setPreference('reminder_checked', $_POST['reminder_checked'], 0, 'global');
            }
            if (isset($_POST['email_reminder_checked'])) {
                $this->setPreference('email_reminder_checked', $_POST['email_reminder_checked'], 0, 'global');
            }

            if (isset($_POST['timezone'])) {
                $this->setPreference('timezone', $_POST['timezone'], 0, 'global');
            } else {
                $this->setPreference('timezone', 'UTC', 0, 'global');
            }
            if (isset($_POST['ut'])) {
                $this->setPreference('ut', '0', 0, 'global');
            } else {
                $this->setPreference('ut', '1', 0, 'global');
            }
            if (isset($_POST['currency'])) {
                $this->setPreference('currency', $_POST['currency'], 0, 'global');
            }
            if (isset($_POST['default_currency_significant_digits'])) {
                $this->setPreference('default_currency_significant_digits', $_POST['default_currency_significant_digits'], 0, 'global');
            }
            if (isset($_POST['num_grp_sep'])) {
                $this->setPreference('num_grp_sep', $_POST['num_grp_sep'], 0, 'global');
            }
            if (isset($_POST['dec_sep'])) {
                $this->setPreference('dec_sep', $_POST['dec_sep'], 0, 'global');
            }
            if (isset($_POST['fdow'])) {
                $this->setPreference('fdow', $_POST['fdow'], 0, 'global');
            }
            if (isset($_POST['dateformat'])) {
                $this->setPreference('datef', $_POST['dateformat'], 0, 'global');
            }
            if (isset($_POST['timeformat'])) {
                $this->setPreference('timef', $_POST['timeformat'], 0, 'global');
            }
            if (isset($_POST['timezone'])) {
                $this->setPreference('timezone', $_POST['timezone'], 0, 'global');
            }
            if (isset($_POST['language'])) {
                if ($_SESSION['authenticated_user_id'] === $this->id){
                    $_SESSION['authenticated_user_language'] = $_POST['language'];
                }
                $current_language = $_POST['language'];
                $mod_strings = return_module_language($_POST['language'], 'Users');
                $app_strings = return_application_language($_POST['language']);
                $this->setPreference('language', $_POST['language'], 0, 'global');
            }
            if (isset($_POST['mail_fromname'])) {
                $this->setPreference('mail_fromname', $_POST['mail_fromname'], 0, 'global');
            }
            if (isset($_POST['mail_fromaddress'])) {
                $this->setPreference('mail_fromaddress', $_POST['mail_fromaddress'], 0, 'global');
            }
            if (isset($_POST['mail_sendtype'])) {
                $this->setPreference('mail_sendtype', $_POST['mail_sendtype'], 0, 'global');
            }
            if (isset($_POST['mail_smtpserver'])) {
                $this->setPreference('mail_smtpserver', $_POST['mail_smtpserver'], 0, 'global');
            }
            if (isset($_POST['mail_smtpport'])) {
                $this->setPreference('mail_smtpport', $_POST['mail_smtpport'], 0, 'global');
            }
            if (isset($_POST['mail_smtpuser'])) {
                $this->setPreference('mail_smtpuser', $_POST['mail_smtpuser'], 0, 'global');
            }
            if (isset($_POST['mail_smtppass'])) {
                $this->setPreference('mail_smtppass', $_POST['mail_smtppass'], 0, 'global');
            }
            if (isset($_POST['default_locale_name_format'])) {
                $this->setPreference('default_locale_name_format', $_POST['default_locale_name_format'], 0, 'global');
            }
            if (isset($_POST['export_delimiter'])) {
                $this->setPreference('export_delimiter', $_POST['export_delimiter'], 0, 'global');
            }
            if (isset($_POST['default_export_charset'])) {
                $this->setPreference('default_export_charset', $_POST['default_export_charset'], 0, 'global');
            }
            if (isset($_POST['use_real_names'])) {
                $this->setPreference('use_real_names', 'on', 0, 'global');
            } elseif (!isset($_POST['use_real_names']) && !isset($_POST['from_dcmenu'])) {
                // Make sure we're on the full form and not the QuickCreate.
                $this->setPreference('use_real_names', 'off', 0, 'global');
            }

            if (isset($_POST['mail_smtpauth_req'])) {
                $this->setPreference('mail_smtpauth_req', $_POST['mail_smtpauth_req'], 0, 'global');
            } else {
                $this->setPreference('mail_smtpauth_req', '', 0, 'global');
            }

            // SSL-enabled SMTP connection
            if (isset($_POST['mail_smtpssl'])) {
                $this->setPreference('mail_smtpssl', 1, 0, 'global');
            } else {
                $this->setPreference('mail_smtpssl', 0, 0, 'global');
            }
            ///////////////////////////////////////////////////////////////////////////
            ////    PDF SETTINGS
            foreach ($_POST as $k => $v) {
                if (strpos($k, "sugarpdf_pdf") !== false) {
                    $this->setPreference($k, $v, 0, 'global');
                }
            }
            ////    PDF SETTINGS
            ///////////////////////////////////////////////////////////////////////////
            ///////////////////////////////////////////////////////////////////////////
            ////	SIGNATURES
            if (isset($_POST['signature_id'])) {
                $this->setPreference('signature_default', $_POST['signature_id'], 0, 'global');
            }

            if (isset($_POST['signature_prepend'])) {
                $this->setPreference('signature_prepend', $_POST['signature_prepend'], 0, 'global');
            }
            ////	END SIGNATURES
            ///////////////////////////////////////////////////////////////////////////


            if (isset($_POST['email_link_type'])) {
                $this->setPreference('email_link_type', $_REQUEST['email_link_type']);
            }
            if (isset($_POST['editor_type'])) {
                $this->setPreference('editor_type', $_REQUEST['editor_type']);
            }
            if (isset($_REQUEST['email_show_counts'])) {
                $this->setPreference('email_show_counts', $_REQUEST['email_show_counts'], 0, 'global');
            } else {
                $this->setPreference('email_show_counts', 0, 0, 'global');
            }
            if (isset($_REQUEST['email_editor_option'])) {
                $this->setPreference('email_editor_option', $_REQUEST['email_editor_option'], 0, 'global');
            }
            if (isset($_REQUEST['default_email_charset'])) {
                $this->setPreference('default_email_charset', $_REQUEST['default_email_charset'], 0, 'global');
            }

            $isValidator = new \SuiteCRM\Utility\SuiteValidator();

            if (isset($_POST['calendar_publish_key']) && $isValidator->isValidKey($_POST['calendar_publish_key'])) {
                $this->setPreference('calendar_publish_key', $_POST['calendar_publish_key'], 0, 'global');
            } elseif (isset($_POST['calendar_publish_key'])) {
                $_POST['calendar_publish_key'] = '';
            }

            if (isset($_POST['subtheme'])) {
                $this->setPreference('subtheme', $_POST['subtheme'], 0, 'global');
            }
            if (isset($_POST['gsync_cal'])) {
                $this->setPreference('syncGCal', 1, 0, 'GoogleSync');
            } else {
                $this->setPreference('syncGCal', 0, 0, 'GoogleSync');
            }
            if ($this->user_hash === null) {
                $newUser = true;
                clear_register_value('user_array', $this->object_name);
            } else {
                $newUser = false;
            }
            if ($newUser && !$this->is_group && !$this->portal_only && isset($sugar_config['passwordsetting']['SystemGeneratedPasswordON'])) {
                require_once 'modules/Users/GeneratePassword.php';
            }
        }
    }


    /**
     * @return boolean true if the user is a member of the role_name, false otherwise
     * @param string $role_name - Must be the exact name of the acl_role
     * @param string $user_id - The user id to check for the role membership, empty string if current user
     * @desc Determine whether or not a user is a member of an ACL Role. This function caches the
     *       results in the session or to prevent running queries after the first time executed.
     * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc..
     * All Rights Reserved..
     * Contributor(s): ______________________________________..
     */
    public function check_role_membership($role_name, $user_id = '')
    {
        global $current_user;

        if (empty($user_id)) {
            $user_id = $current_user->id;
        }

        // Check the Sugar External Cache to see if this users memberships were cached
        $role_array = sugar_cache_retrieve("RoleMemberships_" . $user_id);

        // If we are pulling the roles for the current user
        if ($user_id == $current_user->id) {
            // If the Session doesn't contain the values
            if (!isset($_SESSION['role_memberships'])) {
                // This means the external cache already had it loaded
                if (!empty($role_array)) {
                    $_SESSION['role_memberships'] = $role_array;
                } else {
                    $_SESSION['role_memberships'] = ACLRole::getUserRoleNames($user_id);
                    $role_array = $_SESSION['role_memberships'];
                }
            } // else the session had the values, so we assign to the role array
            else {
                $role_array = $_SESSION['role_memberships'];
            }
        } else {
            // If the external cache didn't contain the values, we get them and put them in cache
            if (!$role_array) {
                $role_array = ACLRole::getUserRoleNames($user_id);
                sugar_cache_put("RoleMemberships_" . $user_id, $role_array);
            }
        }

        // If the role doesn't exist in the list of the user's roles
        if (!empty($role_array) && in_array($role_name, $role_array)) {
            return true;
        }
        return false;
    }

    public function get_summary_text()
    {
        //$this->_create_proper_name_field();
        return $this->name;
    }

    /**
     * @deprecated
     * @param string $user_name - Must be non null and at least 2 characters
     * @param string $username_password - Must be non null and at least 1 character.
     * @desc Take an unencrypted username and password and return the encrypted password
     * @return string encrypted password for storage in DB and comparison against DB password.
     */
    public function encrypt_password($username_password)
    {
        // encrypt the password.
        $salt = substr((string) $this->user_name, 0, 2);
        $encrypted_password = crypt($username_password, $salt);

        return $encrypted_password;
    }

    /**
     * Authenicates the user; returns true if successful
     *
     * @param string $password MD5-encoded password
     * @return bool
     */
    public function authenticate_user($password)
    {
        $row = self::findUserPassword($this->user_name, $password);
        if (empty($row)) {
            return false;
        }
        $this->id = $row['id'];

        return true;
    }

    /**
     * retrieves an User bean
     * pre-format name & full_name attribute with first/last
     * loads User's preferences
     *
     * @param string|integer $id ID of the User
     * @param bool $encode encode the result
     * @param bool $deleted
     * @return User|SugarBean|null null if no User found
     */
    public function retrieve($id = -1, $encode = true, $deleted = true)
    {
        $ret = parent::retrieve($id, $encode, $deleted);
        if ($ret && isset($_SESSION) && $_SESSION !== null) {
            $this->loadPreferences();
        }
        return $ret;
    }

    public function retrieve_by_email_address($email)
    {
        $email1 = strtoupper($email);
        $q = <<<EOQ

		select id from users where id in ( SELECT  er.bean_id AS id FROM email_addr_bean_rel er,
			email_addresses ea WHERE ea.id = er.email_address_id AND users.deleted = 0
		    AND ea.deleted = 0 AND er.deleted = 0 AND er.bean_module = 'Users' AND email_address_caps IN ('{$email1}') )
EOQ;


        $res = $this->db->query($q);
        $rows = array();
        while ($row = $this->db->fetchByAssoc($res)) {
            $rows[] = $row;
        }

        if (count($rows) > 1) {
            $GLOBALS['log']->fatal('ambiguous user email address');
        }
        if (!empty($rows[0]['id'])) {
            return $this->retrieve($rows[0]['id']);
        }
        return '';
    }

    public function bean_implements($interface)
    {
        switch ($interface) {
            case 'ACL':
                return true;
        }

        return false;
    }

    /**
     * Load a user based on the user_name in $this
     * @param string $username_password Password
     * @param bool $password_encoded Is password md5-encoded or plain text?
     * @return -- this if load was successul and null if load failed.
     */
    public function load_user($username_password, $password_encoded = false)
    {
        global $login_error;
        unset($GLOBALS['login_error']);
        if (isset($_SESSION['loginattempts'])) {
            $_SESSION['loginattempts'] += 1;
        } else {
            $_SESSION['loginattempts'] = 1;
        }
        if ($_SESSION['loginattempts'] > 5) {
            $GLOBALS['log']->fatal('SECURITY: ' . $this->user_name . ' has attempted to login ' . $_SESSION['loginattempts'] . ' times from IP address: ' . $_SERVER['REMOTE_ADDR'] . '.');

            return null;
        }

        $GLOBALS['log']->debug("Starting user load for $this->user_name");

        if (!isset($this->user_name) || $this->user_name == "" || !isset($username_password) || $username_password == "") {
            return null;
        }

        if (!$password_encoded) {
            $username_password = md5($username_password);
        }
        $row = self::findUserPassword($this->user_name, $username_password);
        if (empty($row) || !empty($GLOBALS['login_error'])) {
            $GLOBALS['log']->fatal('SECURITY: User authentication for ' . $this->user_name . ' failed - could not Load User from Database');

            return null;
        }

        // now fill in the fields.
        $this->loadFromRow($row);
        $this->loadPreferences();

        if ($this->status != "Inactive") {
            $this->authenticated = true;
        }

        unset($_SESSION['loginattempts']);

        return $this;
    }

    /**
     * Generate a new hash from plaintext password
     * @param string $password
     * @return bool|string
     */
    public static function getPasswordHash($password)
    {
        return self::getPasswordHashMD5(md5($password));
    }

    /**
     * Generate a new hash from MD5 password
     * @param string $passwordMd5
     * @return bool|string
     */
    public static function getPasswordHashMD5($passwordMd5)
    {
        return password_hash(strtolower($passwordMd5), PASSWORD_DEFAULT);
    }

    /**
     * Check that password matches existing hash
     * @param string $password Plaintext password
     * @param string $userHash DB hash
     * @return bool
     */
    public static function checkPassword($password, $userHash)
    {
        return self::checkPasswordMD5(md5($password), $userHash);
    }

    /**
     * Check that md5-encoded password matches existing hash
     * @param string $passwordMd5 MD5-encoded password
     * @param string $userHash DB hash
     * @return bool Match or not?
     */
    public static function checkPasswordMD5($passwordMd5, $userHash)
    {
        if (empty($userHash)) {
            return false;
        }

        if ($userHash[0] !== '$' && strlen($userHash) === 32) {
            // Legacy md5 password
            $valid = strtolower($passwordMd5) === $userHash;
        } else {
            $valid = password_verify(strtolower($passwordMd5), $userHash);
        }

        return $valid;
    }

    /**
     * Find user with matching password
     * @param string $name Username
     * @param string $password MD5-encoded password
     * @param string $where Limiting query
     * @param bool $checkPasswordMD5 use md5 check for user_hash before return the user data (default is true)
     * @return bool|array the matching User of false if not found
     */
    public static function findUserPassword($name, $password, $where = '', $checkPasswordMD5 = true)
    {
        if (!$name) {
            $GLOBALS['log']->fatal('Invalid Argument: Username is not set');
            return false;
        }
        $db = DBManagerFactory::getInstance();
        $before = $name;
        $name = $db->quote($name);
        if ($before && !$name) {
            $GLOBALS['log']->fatal('DB Quote error: return value is removed, check the Database connection.');
            return false;
        }
        $query = "SELECT * from users where user_name='$name'";
        if (!empty($where)) {
            $query .= " AND $where";
        }
        $query .= " AND deleted=0";
        $result = $db->limitQuery($query, 0, 1, false);
        if (!empty($result)) {
            $row = $db->fetchByAssoc($result);
            if (!$checkPasswordMD5 || self::checkPasswordMD5($password, $row['user_hash'])) {
                return $row;
            }
        }
        return false;
    }

    /**
     * Sets new password and resets password expiration timers
     * @param string $new_password
     */
    public function setNewPassword($new_password, $system_generated = '0')
    {
        $user_hash = self::getPasswordHash($new_password);
        $this->setPreference('loginexpiration', '0');
        $this->setPreference('lockout', '');
        $this->setPreference('loginfailed', '0');
        $this->savePreferencesToDB();
        //set new password
        $now = TimeDate::getInstance()->nowDb();
        $query = "UPDATE $this->table_name SET user_hash='$user_hash', system_generated_password='$system_generated', pwd_last_changed='$now' where id='$this->id'";
        $this->db->query($query, true, "Error setting new password for $this->user_name: ");
        $_SESSION['hasExpiredPassword'] = '0';
    }

    /**
     * Verify that the current password is correct and write the new password to the DB.
     *
     * @param string $username_password - Must be non null and at least 1 character.
     * @param string $new_password - Must be non null and at least 1 character.
     * @param string $system_generated
     * @return boolean - If passwords pass verification and query succeeds, return true, else return false.
     */
    public function change_password($username_password, $new_password, $system_generated = '0')
    {
        global $mod_strings;
        global $current_user;
        $GLOBALS['log']->debug("Starting password change for $this->user_name");

        if (!isset($new_password) || $new_password == "") {
            $this->error_string = $mod_strings['ERR_PASSWORD_CHANGE_FAILED_1'] . $current_user->user_name . $mod_strings['ERR_PASSWORD_CHANGE_FAILED_2'];
            return false;
        }
        if ($this->error_string = $this->passwordValidationCheck($new_password)) {
            return false;
        }


        //check old password current user is not an admin or current user is an admin editing themselves
        if (!$current_user->isAdminForModule('Users') || ($current_user->isAdminForModule('Users') && ($current_user->id == $this->id))) {
            //check old password first
            $row = self::findUserPassword($this->user_name, md5($username_password));
            if (empty($row)) {
                $GLOBALS['log']->warn("Incorrect old password for " . $this->user_name . "");
                $this->error_string = $mod_strings['ERR_PASSWORD_INCORRECT_OLD_1'] . $this->user_name . $mod_strings['ERR_PASSWORD_INCORRECT_OLD_2'];

                return false;
            }
        }

        $this->setNewPassword($new_password, $system_generated);
        return true;
    }

    public function passwordValidationCheck($newPassword)
    {
        global $sugar_config, $mod_strings;

        $messages = array();

        if (!isset($sugar_config['passwordsetting']['minpwdlength'])) {
            LoggerManager::getLogger()->warn('User passwordValidationCheck: Undefined index: minpwdlength ($sugar_config[passwordsetting][minpwdlength])');
            $sugar_config['passwordsetting']['minpwdlength'] = null;
        }

        $minpwdlength = $sugar_config['passwordsetting']['minpwdlength'];


        if (!isset($sugar_config['passwordsetting']['oneupper'])) {
            LoggerManager::getLogger()->warn('User passwordValidationCheck: Undefined index: oneupper ($sugar_config[passwordsetting][oneupper])');
            $sugar_config['passwordsetting']['oneupper'] = null;
        }

        $oneupper = $sugar_config['passwordsetting']['oneupper'];


        if (!isset($sugar_config['passwordsetting']['onelower'])) {
            LoggerManager::getLogger()->warn('User passwordValidationCheck: Undefined index: onelower ($sugar_config[passwordsetting][onelower])');
            $sugar_config['passwordsetting']['onelower'] = null;
        }

        $onelower = $sugar_config['passwordsetting']['onelower'];


        if (!isset($sugar_config['passwordsetting']['onenumber'])) {
            LoggerManager::getLogger()->warn('User passwordValidationCheck: Undefined index: onenumber ($sugar_config[passwordsetting][onenumber])');
            $sugar_config['passwordsetting']['onenumber'] = null;
        }

        $onenumber = $sugar_config['passwordsetting']['onenumber'];


        if (!isset($sugar_config['passwordsetting']['onespecial'])) {
            LoggerManager::getLogger()->warn('User passwordValidationCheck: Undefined index: onespecial ($sugar_config[passwordsetting][onespecial])');
            $sugar_config['passwordsetting']['onespecial'] = null;
        }

        $onespecial = $sugar_config['passwordsetting']['onespecial'];


        if ($minpwdlength && strlen((string) $newPassword) < $minpwdlength) {
            $messages[] = sprintf($mod_strings['ERR_PASSWORD_MINPWDLENGTH'], $minpwdlength);
        }

        if ($oneupper && strtolower($newPassword) === $newPassword) {
            $messages[] = $mod_strings['ERR_PASSWORD_ONEUPPER'];
        }

        if ($onelower && strtoupper($newPassword) === $newPassword) {
            $messages[] = $mod_strings['ERR_PASSWORD_ONELOWER'];
        }

        if ($onenumber && !preg_match('/[0-9]/', (string) $newPassword)) {
            $messages[] = $mod_strings['ERR_PASSWORD_ONENUMBER'];
        }

        if ($onespecial && false === strpbrk($newPassword, "#$%^&*()+=-[]';,./{}|:<>?~")) {
            $messages[] = $mod_strings['ERR_PASSWORD_SPECCHARS'];
        }

        $message = implode('<br>', $messages);

        return $message;
    }

    public function is_authenticated()
    {
        return $this->authenticated;
    }

    public function fill_in_additional_list_fields()
    {
        $this->fill_in_additional_detail_fields();
    }

    public function fill_in_additional_detail_fields()
    {
        // jmorais@dri Bug #56269
        parent::fill_in_additional_detail_fields();
        // ~jmorais@dri
        global $locale;

        $query = "SELECT u1.first_name, u1.last_name from users  u1, users  u2 where u1.id = u2.reports_to_id AND u2.id = '$this->id' and u1.deleted=0";
        $result = $this->db->query($query, true, "Error filling in additional detail fields");

        $row = $this->db->fetchByAssoc($result);

        if ($row != null) {
            $this->reports_to_name = stripslashes($row['first_name'] . ' ' . $row['last_name']);
        } else {
            $this->reports_to_name = '';
        }


        $this->_create_proper_name_field();
    }

    public function retrieve_user_id(
        $user_name
    ) {
        $userFocus = new User;
        $userFocus->retrieve_by_string_fields(array('user_name' => $user_name));
        if (empty($userFocus->id)) {
            return false;
        }

        return $userFocus->id;
    }

    /**
     * @return -- returns a list of all users in the system.
     * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc..
     * All Rights Reserved..
     * Contributor(s): ______________________________________..
     */
    public function verify_data($ieVerified = true)
    {
        global $mod_strings, $current_user;
        $verified = true;

        if (!empty($this->id)) {
            // Make sure the user doesn't report to themselves.
            $reports_to_self = 0;
            $check_user = $this->reports_to_id;
            $already_seen_list = array();
            while (!empty($check_user)) {
                if (isset($already_seen_list[$check_user])) {
                    // This user doesn't actually report to themselves
                    // But someone above them does.
                    $reports_to_self = 1;
                    break;
                }
                if ($check_user == $this->id) {
                    $reports_to_self = 1;
                    break;
                }
                $already_seen_list[$check_user] = 1;
                $query = "SELECT reports_to_id FROM users WHERE id='" . $this->db->quote($check_user) . "'";
                $result = $this->db->query($query, true, "Error checking for reporting-loop");
                $row = $this->db->fetchByAssoc($result);
                $check_user = $row['reports_to_id'];
            }

            if ($reports_to_self == 1) {
                $this->error_string .= $mod_strings['ERR_REPORT_LOOP'];
                $verified = false;
            }
        }

        $query = "SELECT user_name from users where user_name='$this->user_name' AND deleted=0";
        if (!empty($this->id)) {
            $query .= " AND id<>'$this->id'";
        }
        $result = $this->db->query($query, true, "Error selecting possible duplicate users: ");
        $dup_users = $this->db->fetchByAssoc($result);

        if (!empty($dup_users)) {
            $this->error_string .= $mod_strings['ERR_USER_NAME_EXISTS_1'] . $this->user_name . $mod_strings['ERR_USER_NAME_EXISTS_2'];
            $verified = false;
        }

        if (is_admin($current_user)) {
            $remaining_admins = $this->db->getOne("SELECT COUNT(*) as c from users where is_admin = 1 AND deleted=0");

            if (($remaining_admins <= 1) && ($this->is_admin != '1') && ($this->id == $current_user->id)) {
                $GLOBALS['log']->debug("Number of remaining administrator accounts: {$remaining_admins}");
                $this->error_string .= $mod_strings['ERR_LAST_ADMIN_1'] . $this->user_name . $mod_strings['ERR_LAST_ADMIN_2'];
                $verified = false;
            }
        }
        ///////////////////////////////////////////////////////////////////////
        ////	InboundEmail verification failure
        if (!$ieVerified) {
            $verified = false;
            $this->error_string .= '<br />' . $mod_strings['ERR_EMAIL_NO_OPTS'];
        }

        return $verified;
    }

    public function get_list_view_data()
    {
        global $mod_strings;

        $user_fields = parent::get_list_view_data();

        if ($this->is_admin) {
            if (!isset($mod_strings['LBL_CHECKMARK'])) {
                LoggerManager::getLogger()->warn('A language label not found: LBL_CHECKMARK');
            }
            $checkmark = isset($mod_strings['LBL_CHECKMARK']) ? $mod_strings['LBL_CHECKMARK'] : null;
            $user_fields['IS_ADMIN_IMAGE'] = SugarThemeRegistry::current()->getImage('check_inline', '', null, null, '.gif', $checkmark);
        } elseif (!$this->is_admin) {
            $user_fields['IS_ADMIN'] = '';
        }
        if ($this->is_group) {
            $user_fields['IS_GROUP_IMAGE'] = SugarThemeRegistry::current()->getImage('check_inline', '', null, null, '.gif', translate('LBL_CHECKMARK', 'Users'));
        } else {
            $user_fields['IS_GROUP_IMAGE'] = '';
        }


        if ($this->is_admin) {
            $user_fields['IS_ADMIN_IMAGE'] = SugarThemeRegistry::current()->getImage('check_inline', '', null, null, '.gif', translate('LBL_CHECKMARK', 'Users'));
        } elseif (!$this->is_admin) {
            $user_fields['IS_ADMIN'] = '';
        }

        if ($this->is_group) {
            $user_fields['IS_GROUP_IMAGE'] = SugarThemeRegistry::current()->getImage('check_inline', '', null, null, '.gif', translate('LBL_CHECKMARK', 'Users'));
        } else {
            $user_fields['NAME'] = empty($this->name) ? '' : $this->name;
        }

        $user_fields['REPORTS_TO_NAME'] = $this->reports_to_name;


        return $user_fields;
    }

    public function list_view_parse_additional_sections(&$list_form)
    {
        return $list_form;
    }

    /**
     * getAllUsers
     *
     * Returns all active and inactive users
     * @return Array of all users in the system
     */
    public static function getAllUsers()
    {
        $active_users = get_user_array(false);
        $inactive_users = get_user_array(false, "Inactive");
        $result = $active_users + $inactive_users;
        asort($result);

        return $result;
    }

    /**
     * getActiveUsers
     *
     * Returns all active users
     * @return Array of active users in the system
     */
    public static function getActiveUsers()
    {
        $active_users = get_user_array(false);
        asort($active_users);

        return $active_users;
    }

    public function create_export_query($order_by, $where, $relate_link_join = '')
    {
        global $current_user;
        if (!is_admin($current_user)) {
            throw new RuntimeException('Not authorized');
        }

        include('modules/Users/field_arrays.php');

        $cols = '';
        foreach ($fields_array['User']['export_fields'] as $field) {
            $cols .= (empty($cols)) ? '' : ', ';
            $cols .= $field;
        }

        $query = "SELECT {$cols} FROM users ";

        $where_auto = " users.deleted = 0";

        if ($where != "") {
            $query .= " WHERE $where AND " . $where_auto;
        } else {
            $query .= " WHERE " . $where_auto;
        }

        // admin for module user is not be able to export a super-admin
        global $current_user;
        if (!$current_user->is_admin) {
            $query .= " AND users.is_admin=0";
        }

        if ($order_by != "") {
            $query .= " ORDER BY $order_by";
        } else {
            $query .= " ORDER BY users.user_name";
        }

        return $query;
    }

    /** Returns a list of the associated users
     * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc..
     * All Rights Reserved..
     * Contributor(s): ______________________________________..
     */
    public function get_meetings()
    {
        // First, get the list of IDs.
        $query = "SELECT meeting_id as id from meetings_users where user_id='$this->id' AND deleted=0";

        $meeting = BeanFactory::newBean('Meetings');
        return $this->build_related_list($query, $meeting);
    }

    public function get_calls()
    {
        // First, get the list of IDs.
        $query = "SELECT call_id as id from calls_users where user_id='$this->id' AND deleted=0";

        return $this->build_related_list($query, BeanFactory::newBean('Calls'));
    }

    /**
     * generates Javascript to display I-E mail counts, both personal and group
     */
    public function displayEmailCounts()
    {
        global $theme;
        $new = translate('LBL_NEW', 'Emails');
        $default = 'index.php?module=Emails&action=ListView&assigned_user_id=' . $this->id;
        $count = '';
        $verts = array('Love', 'Links', 'Pipeline', 'RipCurl', 'SugarLite');

        if ($this->hasPersonalEmail()) {
            $r = $this->db->query('SELECT count(*) AS c FROM emails WHERE deleted=0 AND assigned_user_id = \'' . $this->id . '\' AND type = \'inbound\' AND status = \'unread\'');
            $a = $this->db->fetchByAssoc($r);
            if (in_array($theme, $verts)) {
                $count .= '<br />';
            } else {
                $count .= '&nbsp;&nbsp;&nbsp;&nbsp;';
            }
            $count .= '<a href=' . $default . '&type=inbound>' . translate('LBL_LIST_TITLE_MY_INBOX', 'Emails') . ': (' . $a['c'] . ' ' . $new . ')</a>';

            if (!in_array($theme, $verts)) {
                $count .= ' - ';
            }
        }

        $r = $this->db->query('SELECT id FROM users WHERE users.is_group = 1 AND deleted = 0');
        $groupIds = '';
        $groupNew = '';
        while ($a = $this->db->fetchByAssoc($r)) {
            if ($groupIds != '') {
                $groupIds .= ', ';
            }
            $groupIds .= "'" . $a['id'] . "'";
        }

        $total = 0;
        if (strlen($groupIds) > 0) {
            $groupQuery = 'SELECT count(*) AS c FROM emails ';
            $groupQuery .= ' WHERE emails.deleted=0 AND emails.assigned_user_id IN (' . $groupIds . ') AND emails.type = \'inbound\' AND emails.status = \'unread\'';
            $r = $this->db->query($groupQuery);
            if (is_resource($r)) {
                $a = $this->db->fetchByAssoc($r);
                if ($a['c'] > 0) {
                    $total = $a['c'];
                }
            }
        }
        if (in_array($theme, $verts)) {
            $count .= '<br />';
        }
        if (empty($count)) {
            $count .= '&nbsp;&nbsp;&nbsp;&nbsp;';
        }
        $count .= '<a href=index.php?module=Emails&action=ListViewGroup>' . translate('LBL_LIST_TITLE_GROUP_INBOX', 'Emails') . ': (' . $total . ' ' . $new . ')</a>';

        $out = '<script type="text/javascript" language="Javascript">';
        $out .= 'var welcome = document.getElementById("welcome");';
        $out .= 'var welcomeContent = welcome.innerHTML;';
        $out .= 'welcome.innerHTML = welcomeContent + "' . $count . '";';
        $out .= '</script>';

        echo $out;
    }

    public function getPreferredEmail()
    {
        $ret = array();
        $nameEmail = $this->getUsersNameAndEmail();
        $prefAddr = $nameEmail['email'];
        $fullName = $nameEmail['name'];
        if (empty($prefAddr)) {
            $nameEmail = $this->getSystemDefaultNameAndEmail();
            $prefAddr = $nameEmail['email'];
            $fullName = $nameEmail['name'];
        } // if
        $fullName = from_html($fullName);
        $ret['name'] = $fullName;
        $ret['email'] = $prefAddr;

        return $ret;
    }

    public function getUsersNameAndEmail()
    {
        // Bug #48555 Not User Name Format of User's locale.
        $this->_create_proper_name_field();

        $prefAddr = $this->emailAddress->getPrimaryAddress($this);

        if (empty($prefAddr)) {
            $prefAddr = $this->emailAddress->getReplyToAddress($this);
        }

        return array('email' => $prefAddr, 'name' => $this->name);
    }

    // fn

    public function getSystemDefaultNameAndEmail()
    {
        $email = BeanFactory::newBean('Emails');
        $return = $email->getSystemDefaultEmail();
        $prefAddr = $return['email'];
        $fullName = $return['name'];

        return array('email' => $prefAddr, 'name' => $fullName);
    }

    // fn

    /**
     * sets User email default in config.php if not already set by install - i.
     * e., upgrades
     */
    public function setDefaultsInConfig()
    {
        global $sugar_config;
        $sugar_config['email_default_client'] = 'sugar';
        $sugar_config['email_default_editor'] = 'html';
        ksort($sugar_config);
        write_array_to_file('sugar_config', $sugar_config, 'config.php');

        return $sugar_config;
    }

    /**
     * returns User's email address based on descending order of preferences
     *
     * @param string id GUID of target user if needed
     * @return array Assoc array for an email and name
     */
    public function getEmailInfo($id = '')
    {
        $user = $this;
        if (!empty($id)) {
            $user = BeanFactory::newBean('Users');
            $user->retrieve($id);
        }

        // from name
        $fromName = $user->getPreference('mail_fromname');
        if (empty($fromName)) {
            // cn: bug 8586 - localized name format
            $fromName = $user->full_name;
        }

        // from address
        $fromaddr = $user->getPreference('mail_fromaddress');
        if (empty($fromaddr)) {
            if (!empty($user->email1) && isset($user->email1)) {
                $fromaddr = $user->email1;
            } elseif (!empty($user->email2) && isset($user->email2)) {
                $fromaddr = $user->email2;
            } else {
                $r = $user->db->query("SELECT value FROM config WHERE name = 'fromaddress'");
                $a = $user->db->fetchByAssoc($r);
                $fromddr = $a['value'];
            }
        }

        $ret = [];
        $ret['name'] = $fromName;
        $ret['email'] = $fromaddr;

        return $ret;
    }

    /**
     * returns opening <a href=xxxx for a contact, account, etc
     * cascades from User set preference to System-wide default
     * @return string    link
     * @param attribute the email addy
     * @param focus the parent bean
     * @param contact_id
     * @param return_module
     * @param return_action
     * @param return_id
     * @param class
     */
    public function getEmailLink2(
        $emailAddress,
        &$focus,
        $contact_id = '',
        $ret_module = '',
        $ret_action = 'DetailView',
        $ret_id = '',
        $class = ''
    ) {
        $emailLink = '';

        $emailUI = new EmailUI();
        $addressesCount = is_countable($focus->emailAddress->addresses) ? count($focus->emailAddress->addresses) : 0;
        for ($i = 0; $i < $addressesCount; $i++) {
            $emailField = 'email' . ($i + 1);
            $optOut = (bool)$focus->emailAddress->addresses[$i]['opt_out'];
            if (!$optOut && $focus->emailAddress->addresses[$i]['email_address'] === $emailAddress) {
                $focus->$emailField = $emailAddress;
                $emailLink = $emailUI->populateComposeViewFields($focus, $emailField);
                break;
            }
        }

        return $emailLink;
    }

    /**
     * Returns the email client type that should be used for this user.
     * Either "sugar" for the "SuiteCRM E-mail Client" or "mailto" for the
     * "External Email Client".
     *
     * @return string
     */
    public function getEmailClient()
    {
        global $sugar_config;

        if (!isset($sugar_config['email_default_client'])) {
            $this->setDefaultsInConfig();
        }

        $userPref = $this->getPreference('email_link_type');
        $defaultPref = $sugar_config['email_default_client'];
        if ($userPref != '') {
            $client = $userPref;
        } else {
            $client = $defaultPref;
        }

        return $client;
    }

    /**
     * returns opening <a href=xxxx for a contact, account, etc
     * cascades from User set preference to System-wide default
     * @return string    link
     * @param attribute the email addy
     * @param focus the parent bean
     * @param contact_id
     * @param return_module
     * @param return_action
     * @param return_id
     * @param class
     */
    public function getEmailLink(
        $attribute,
        &$focus,
        $contact_id = '',
        $ret_module = '',
        $ret_action = 'DetailView',
        $ret_id = '',
        $class = ''
    ) {
        $emailUI = new EmailUI();
        $emailLink = $emailUI->populateComposeViewFields($focus);

        return $emailLink;
    }

    /**
     * gets a human-readable explanation of the format macro
     * @return string Human readable name format
     */
    public function getLocaleFormatDesc()
    {
        global $locale;
        global $mod_strings;
        global $app_strings;

        $format = [];
        $format['f'] = $mod_strings['LBL_LOCALE_DESC_FIRST'];
        $format['l'] = $mod_strings['LBL_LOCALE_DESC_LAST'];
        $format['s'] = $mod_strings['LBL_LOCALE_DESC_SALUTATION'];
        $format['t'] = $mod_strings['LBL_LOCALE_DESC_TITLE'];

        $name = [];
        $name['f'] = $app_strings['LBL_LOCALE_NAME_EXAMPLE_FIRST'];
        $name['l'] = $app_strings['LBL_LOCALE_NAME_EXAMPLE_LAST'];
        $name['s'] = $app_strings['LBL_LOCALE_NAME_EXAMPLE_SALUTATION'];
        $name['t'] = $app_strings['LBL_LOCALE_NAME_EXAMPLE_TITLE'];

        $macro = $locale->getLocaleFormatMacro();

        $ret1 = '';
        $ret2 = '';
        for ($i = 0, $iMax = strlen((string) $macro); $i < $iMax; $i++) {
            if (array_key_exists($macro[$i], $format)) {
                $ret1 .= "<i>" . $format[$macro[$i]] . "</i>";
                $ret2 .= "<i>" . $name[$macro[$i]] . "</i>";
            } else {
                $ret1 .= $macro[$i];
                $ret2 .= $macro[$i];
            }
        }

        return $ret1 . "<br />" . $ret2;
    }

    /*
     *
     * Here are the multi level admin access check functions.
     *
     */

    /**
     * Helper function to remap some modules around ACL wise
     *
     * @return string
     */
    protected function _fixupModuleForACL($module)
    {
        if ($module == 'ContractTypes') {
            $module = 'Contracts';
        }
        if (preg_match('/Product[a-zA-Z]*/', (string) $module)) {
            $module = 'Products';
        }

        return $module;
    }

    /**
     * Helper function that enumerates the list of modules and checks if they are an admin/dev.
     * The code was just too similar to copy and paste.
     *
     * @return array
     */
    protected function _getModulesForACL($type = 'dev')
    {
        $isDev = $type == 'dev';
        $isAdmin = $type == 'admin';

        global $beanList;
        $myModules = array();

        if (!is_array($beanList)) {
            return $myModules;
        }

        // These modules don't take kindly to the studio trying to play about with them.
        static $ignoredModuleList = array('iFrames', 'Feeds', 'Home', 'Dashboard', 'Calendar', 'Activities', 'Reports');


        $actions = ACLAction::getUserActions($this->id);

        foreach ($beanList as $module => $val) {
            // Remap the module name
            $module = $this->_fixupModuleForACL($module);
            if (in_array($module, $myModules)) {
                // Already have the module in the list
                continue;
            }
            if (in_array($module, $ignoredModuleList)) {
                // You can't develop on these modules.
                continue;
            }

            $focus = SugarModule::get($module)->loadBean();
            if ($focus instanceof SugarBean) {
                $key = $focus->acltype;
            } else {
                $key = 'module';
            }

            if (($this->isAdmin() && isset($actions[$module][$key]))
            ) {
                $myModules[] = $module;
            }
        }

        return $myModules;
    }

    /**
     * Is user enabled
     *
     * @return bool
     */
    public function isEnabled()
    {
        return ($this->status !== 'Inactive') && ($this->employee_status === 'Active');
    }

    /**
     * Is this user a system wide admin
     *
     * @return bool
     */
    public function isAdmin()
    {
        if (isset($this->is_admin) && ($this->is_admin == '1' || $this->is_admin === 'on')
        ) {
            return true;
        }

        return false;
    }

    /**
     * Is this user a developer for any module
     *
     * @return bool
     */
    public function isDeveloperForAnyModule()
    {
        if (empty($this->id)) {
            // empty user is no developer
            return false;
        }
        if ($this->isAdmin()) {
            return true;
        }

        return false;
    }

    /**
     * List the modules a user has developer access to
     *
     * @return array
     */
    public function getDeveloperModules()
    {
        static $developerModules;
        if (!isset($_SESSION[$this->user_name . '_get_developer_modules_for_user'])) {
            $_SESSION[$this->user_name . '_get_developer_modules_for_user'] = $this->_getModulesForACL('dev');
        }

        return $_SESSION[$this->user_name . '_get_developer_modules_for_user'];
    }

    /**
     * Is this user a developer for the specified module
     *
     * @return bool
     */
    public function isDeveloperForModule($module)
    {
        if (empty($this->id)) {
            // empty user is no developer
            return false;
        }
        if ($this->isAdmin()) {
            return true;
        }

        $devModules = $this->getDeveloperModules();

        $module = $this->_fixupModuleForACL($module);

        if (in_array($module, $devModules)) {
            return true;
        }

        return false;
    }

    /**
     * List the modules a user has admin access to
     *
     * @return array
     */
    public function getAdminModules()
    {
        if (!isset($_SESSION[$this->user_name . '_get_admin_modules_for_user'])) {
            $_SESSION[$this->user_name . '_get_admin_modules_for_user'] = $this->_getModulesForACL('admin');
        }

        return $_SESSION[$this->user_name . '_get_admin_modules_for_user'];
    }

    /**
     * Is this user an admin for the specified module
     *
     * @return bool
     */
    public function isAdminForModule($module)
    {
        if (empty($this->id)) {
            // empty user is no admin
            return false;
        }
        if ($this->isAdmin()) {
            return true;
        }

        $adminModules = $this->getAdminModules();

        $module = $this->_fixupModuleForACL($module);

        if (in_array($module, $adminModules)) {
            return true;
        }

        return false;
    }

    /**
     * Whether or not based on the user's locale if we should show the last name first.
     *
     * @return bool
     */
    public function showLastNameFirst()
    {
        global $locale;
        $localeFormat = $locale->getLocaleFormatMacro($this);
        if (strpos((string) $localeFormat, 'l') > strpos((string) $localeFormat, 'f')) {
            return false;
        }
        return true;
    }

    public function create_new_list_query(
        $order_by,
        $where,
        $filter = array(),
        $params = array(),
        $show_deleted = 0,
        $join_type = '',
        $return_array = false,
        $parentbean = null,
        $singleSelect = false,
        $ifListForExport = false
    ) {    //call parent method, specifying for array to be returned
        $ret_array = parent::create_new_list_query($order_by, $where, $filter, $params, $show_deleted, $join_type, true, $parentbean, $singleSelect, $ifListForExport);

        //if this is being called from webservices, then run additional code
        if (!empty($GLOBALS['soap_server_object'])) {

            //if this is a single select, then secondary queries are being run that may result in duplicate rows being returned through the
            //left joins with meetings/tasks/call.  We need to change the left joins to include a null check (bug 40250)
            if ($singleSelect) {
                //retrieve the 'from' string and make lowercase for easier manipulation
                $left_str = strtolower($ret_array['from']);
                $lefts = explode('left join', $left_str);
                $new_left_str = '';

                //explode on the left joins and process each one
                foreach ($lefts as $ljVal) {
                    //grab the join alias
                    $onPos = strpos($ljVal, ' on');
                    if ($onPos === false) {
                        $new_left_str .= ' ' . $ljVal . ' ';
                        continue;
                    }
                    $spacePos = strrpos(substr($ljVal, 0, $onPos), ' ');
                    $alias = substr($ljVal, $spacePos, $onPos - $spacePos);

                    //add null check to end of the Join statement
                    // Bug #46390 to use id_c field instead of id field for custom tables
                    if (substr($alias, -5) != '_cstm') {
                        $ljVal = '  LEFT JOIN ' . $ljVal . ' and ' . $alias . '.id is null ';
                    } else {
                        $ljVal = '  LEFT JOIN ' . $ljVal . ' and ' . $alias . '.id_c is null ';
                    }

                    //add statement into new string
                    $new_left_str .= $ljVal;
                }
                //replace the old string with the new one
                $ret_array['from'] = $new_left_str;
            }
        }

        //return array or query string
        if ($return_array) {
            return $ret_array;
        }

        return $ret_array['select'] . $ret_array['from'] . $ret_array['where'] . $ret_array['order_by'];
    }

    /**
     * Get user first day of week.
     *
     * @param [User] $user user object, current user if not specified
     * @return int : 0 = Sunday, 1 = Monday, etc...
     */
    public function get_first_day_of_week()
    {
        $fdow = $this->getPreference('fdow');
        if (empty($fdow)) {
            $fdow = 0;
        }

        return $fdow;
    }

    /**
     * Method for password generation
     *
     * @static
     * @return string password
     */
    public static function generatePassword()
    {
        $res = $GLOBALS['sugar_config']['passwordsetting'];
        $charBKT = '';
        //chars to select from
        $LOWERCASE = "abcdefghijklmnpqrstuvwxyz";
        $NUMBER = "0123456789";
        $UPPERCASE = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        $SPECIAL = '~!@#$%^&*()_+=-{}|';
        $condition = 0;
        $charBKT .= $UPPERCASE . $LOWERCASE . $NUMBER;
        $password = "";
        $length = '6';

        // Create random characters for the ones that doesnt have requirements
        for ($i = 0; $i < $length - $condition; $i++) {  // loop and create password
            $password = $password . substr($charBKT, mt_rand() % strlen($charBKT), 1);
        }

        return $password;
    }

    /**
     * Send new password or link to user
     *
     * @param string $templateId Id of email template
     * @param array $additionalData additional params: link, url, password
     * @return array status: true|false, message: error message, if status = false and message = '' it means that send method has returned false
     */
    public function sendEmailForPassword($templateId, array $additionalData = array())
    {
        global $sugar_config, $current_user;
        $mod_strings = return_module_language('', 'Users');
        $result = array(
            'status' => false,
            'message' => ''
        );

        $emailTemp = BeanFactory::newBean('EmailTemplates');
        $emailTemp->disable_row_level_security = true;
        if ($emailTemp->retrieve($templateId) == '') {
            $result['message'] = $mod_strings['LBL_EMAIL_TEMPLATE_MISSING'];

            return $result;
        }

        //replace instance variables in email templates
        $htmlBody = $emailTemp->body_html;
        $body = $emailTemp->body;
        if (isset($additionalData['link']) && $additionalData['link'] == true) {
            $htmlBody = str_replace('$contact_user_link_guid', $additionalData['url'], (string) $htmlBody);
            $body = str_replace('$contact_user_link_guid', $additionalData['url'], (string) $body);
        } else {
            $htmlBody = str_replace('$contact_user_user_hash', $additionalData['password'], (string) $htmlBody);
            $body = str_replace('$contact_user_user_hash', $additionalData['password'], (string) $body);
        }
        // Bug 36833 - Add replacing of special value $instance_url
        $htmlBody = str_replace('$config_site_url', $sugar_config['site_url'], $htmlBody);
        $body = str_replace('$config_site_url', $sugar_config['site_url'], $body);

        $htmlBody = str_replace('$contact_user_user_name', $this->user_name, $htmlBody);
        $htmlBody = str_replace('$contact_user_pwd_last_changed', TimeDate::getInstance()->nowDb(), $htmlBody);
        $body = str_replace('$contact_user_user_name', $this->user_name, $body);
        $body = str_replace('$contact_user_pwd_last_changed', TimeDate::getInstance()->nowDb(), $body);
        $emailTemp->body_html = $htmlBody;
        $emailTemp->body = $body;

        $itemail = $this->emailAddress->getPrimaryAddress($this);
        //retrieve IT Admin Email
        //retrieve email defaults
        $emailObj = BeanFactory::newBean('Emails');
        $defaults = $emailObj->getSystemDefaultEmail();
        require_once('include/SugarPHPMailer.php');
        $mail = new SugarPHPMailer();
        $mail->setMailerForSystem();
        //$mail->IsHTML(true);
        $mail->From = $defaults['email'];
        isValidEmailAddress($mail->From);
        $mail->FromName = $defaults['name'];
        $mail->ClearAllRecipients();
        $mail->ClearReplyTos();
        $mail->Subject = from_html($emailTemp->subject);
        if ($emailTemp->text_only != 1) {
            $mail->IsHTML(true);
            $mail->Body = from_html($emailTemp->body_html);
            $mail->AltBody = from_html($emailTemp->body);
        } else {
            $mail->Body_html = from_html($emailTemp->body_html);
            $mail->Body = from_html($emailTemp->body);
        }
        if ($mail->Body == '' && $current_user->is_admin) {
            global $app_strings;
            $result['message'] = $app_strings['LBL_EMAIL_TEMPLATE_EDIT_PLAIN_TEXT'];

            return $result;
        }
        if ($mail->Mailer == 'smtp' && $mail->Host == '' && $current_user->is_admin) {
            $result['message'] = $mod_strings['ERR_SERVER_SMTP_EMPTY'];

            return $result;
        }

        $mail->prepForOutbound();
        $hasRecipients = false;

        if (!empty($itemail)) {
            if ($hasRecipients) {
                $mail->AddBCC($itemail);
            } else {
                $mail->AddAddress($itemail);
            }
            $hasRecipients = true;
        }
        if ($hasRecipients) {
            $result['status'] = $mail->Send();
        }

        if ($result['status'] == true) {
            $emailObj->team_id = 1;
            $emailObj->to_addrs = '';
            $emailObj->type = 'archived';
            $emailObj->deleted = '0';
            $emailObj->name = $mail->Subject;
            $emailObj->description = $mail->Body;
            $emailObj->description_html = null;
            $emailObj->from_addr = $mail->From;
            isValidEmailAddress($emailObj->from_addr);
            $emailObj->parent_type = 'User';
            $emailObj->date_sent_received = TimeDate::getInstance()->nowDb();
            $emailObj->modified_user_id = '1';
            $emailObj->created_by = '1';
            $emailObj->status = 'sent';
            $emailObj->save();
            if (!isset($additionalData['link']) || $additionalData['link'] == false) {
                $this->setNewPassword($additionalData['password'], '1');
            }
        }

        return $result;
    }

    // Bug #48014 Must to send password to imported user if this action is required
    public function afterImportSave()
    {
        if (
                $this->user_hash == false && !$this->is_group && !$this->portal_only && isset($GLOBALS['sugar_config']['passwordsetting']['SystemGeneratedPasswordON']) && $GLOBALS['sugar_config']['passwordsetting']['SystemGeneratedPasswordON']
        ) {
            $backUpPost = $_POST;
            $_POST = array(
                'userId' => $this->id
            );
            ob_start();
            require('modules/Users/GeneratePassword.php');
            $result = ob_get_clean();
            $_POST = $backUpPost;

            return $result == true;
        }
    }

    /**
     * Checks if the passed email is primary.
     *
     * @param string $email
     * @return bool Returns TRUE if the passed email is primary.
     */
    public function isPrimaryEmail($email)
    {
        if (!empty($this->email1) && !empty($email) && strcasecmp($this->email1, $email) === 0) {
            return true;
        }
        return false;
    }

    public function getEditorType()
    {
        $editorType = $this->getPreference('editor_type');
        if (!$editorType) {
            $editorType = 'mozaik';
            $this->setPreference('editor_type', $editorType);
        }

        return $editorType;
    }

    public function getSubThemes()
    {
        $sugarTheme = new SugarTheme(array());
        $subThemes = $sugarTheme->getSubThemes();
        return $subThemes;
    }

    public function getSubTheme()
    {
        $subTheme = $this->getPreference('subtheme');
        if (!$subTheme) {
            $sugarTheme = new SugarTheme(array());
            $subTheme = $sugarTheme->getSubThemeDefault();
        }
        return $subTheme;
    }

    /**
     * Check if current user can save the current user record
     * @return bool
     */
    protected function hasSaveAccess(): bool
    {
        global $current_user;

        if (empty($this->id)) {
            return true;
        }

        if (empty($current_user->id)) {
            return true;
        }

        $sameUser = $current_user->id === $this->id;

        return $sameUser || is_admin($current_user);
    }

    /**
     * Reset is_admin if current user is not an admin user
     * @return void
     */
    protected function setIsAdmin(): void
    {
        global $current_user;
        if (!isset($this->is_admin)) {
            return;
        }


        $originalIsAdminValue = $this->is_admin ?? false;
        if ($this->isUpdate() && isset($this->fetched_row['is_admin'])) {
            $originalIsAdminValue = isTrue($this->fetched_row['is_admin'] ?? false);
        }

        $currentUserReloaded = BeanFactory::getReloadedBean('Users', $current_user->id);
        if (!is_admin($currentUserReloaded)) {
            $this->is_admin = $originalIsAdminValue;
        }

    }

    /**
     * @return bool
     */
    protected function isUpdate(): bool
    {
        return !empty($this->id) && !$this->new_with_id;
    }
}
