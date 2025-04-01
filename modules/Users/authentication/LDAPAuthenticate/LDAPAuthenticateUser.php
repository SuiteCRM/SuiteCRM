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

/**
 * This file is where the user authentication occurs. No redirection should happen in this file.
 *
 */
require_once('modules/Users/authentication/LDAPAuthenticate/LDAPConfigs/default.php');
require_once('modules/Users/authentication/SugarAuthenticate/SugarAuthenticateUser.php');

define('DEFAULT_PORT', 389);
class LDAPAuthenticateUser extends SugarAuthenticateUser
{

    public $ldapUserInfo;
    public $user_name;
    /**
     * Does the actual authentication of the user and returns an id that will be used
     * to load the current user (loadUserOnSession)
     *
     * @param string $name
     * @param string $password
     * @param boolean $fallback
     * @return string id - used for loading the user
     *
     * Contributions by Erik Mitchell erikm@logicpd.com
     */
    public function authenticateUser($name, $password, $fallback=false)
    {
        $server = $GLOBALS['ldap_config']->settings['ldap_hostname'];
        $port = $GLOBALS['ldap_config']->settings['ldap_port'];
        if (!$port) {
            $port = DEFAULT_PORT;
        }
        $GLOBALS['log']->debug("ldapauth: Connecting to LDAP server: $server");
        $ldapconn = ldap_connect($server, $port);
        $error = ldap_errno($ldapconn);
        if ($this->loginError($error)) {
            return '';
        }
        @ldap_set_option($ldapconn, LDAP_OPT_PROTOCOL_VERSION, 3);
        @ldap_set_option($ldapconn, LDAP_OPT_REFERRALS, 0); // required for AD
        // Network timeout, lower than PHP and DB timeouts
        @ldap_set_option($ldapconn, LDAP_OPT_NETWORK_TIMEOUT, 60);

        $bind_user = $this->ldap_rdn_lookup($name, $password);
        $GLOBALS['log']->debug("ldapauth.ldap_authenticate_user: ldap_rdn_lookup returned bind_user=" . $bind_user);
        if (!$bind_user) {
            $GLOBALS['log']->fatal("SECURITY: ldapauth: failed LDAP bind (login) by " .
                                    $name . ", could not construct bind_user");
            return '';
        }

        // MRF - Bug #18578 - punctuation was being passed as HTML entities, i.e. &amp;
        $bind_password = html_entity_decode($password, ENT_QUOTES);

        $GLOBALS['log']->info("ldapauth: Binding user " . $bind_user);
        $bind = ldap_bind($ldapconn, $bind_user, $bind_password);
        $error = ldap_errno($ldapconn);
        if ($this->loginError($error)) {
            $full_user = $GLOBALS['ldap_config']->settings['ldap_bind_attr'] . "=" . $bind_user
                . "," . $GLOBALS['ldap_config']->settings['ldap_base_dn'];

            $GLOBALS['log']->info("ldapauth: Binding user " . $full_user);
            $bind = ldap_bind($ldapconn, $full_user, $bind_password);
            $error = ldap_errno($ldapconn);
            if ($this->loginError($error)) {
                return '';
            }
        }

        $GLOBALS['log']->info("ldapauth: Bind attempt complete.");

        if ($bind) {
            // Authentication succeeded, get info from LDAP directory
            $attrs = array_keys($GLOBALS['ldapConfig']['users']['fields']);
            $base_dn = $GLOBALS['ldap_config']->settings['ldap_base_dn'];
            $name_filter = $this->getUserNameFilter($name);

            //add the group user attribute that we will compare to the group attribute for membership validation if group membership is turned on
            if (!empty($GLOBALS['ldap_config']->settings['ldap_group'])
                && !empty($GLOBALS['ldap_config']->settings['ldap_group_user_attr'])
                && !empty($GLOBALS['ldap_config']->settings['ldap_group_attr'])
                && !in_array($GLOBALS['ldap_config']->settings['ldap_group_user_attr'], $attrs, false)
            ) {
                $attrs[] = $GLOBALS['ldap_config']->settings['ldap_group_user_attr'];
            }

            $GLOBALS['log']->debug(
                "ldapauth: Fetching user info from Directory using base dn: "
                . $base_dn . ", name_filter: " . $name_filter . ", attrs: " . var_export($attrs, true)
            );

            $result = @ldap_search($ldapconn, $base_dn, $name_filter, $attrs);
            $error = ldap_errno($ldapconn);
            if ($this->loginError($error)) {
                return '';
            }
            $GLOBALS['log']->debug("ldapauth: ldap_search complete.");

            $info = @ldap_get_entries($ldapconn, $result);
            $error = ldap_errno($ldapconn);
            if ($this->loginError($error)) {
                return '';
            }



            $GLOBALS['log']->debug("ldapauth: User info from Directory fetched.");

            // some of these don't seem to work
            $this->ldapUserInfo = array();
            foreach ($GLOBALS['ldapConfig']['users']['fields'] as $key=>$value) {
                //MRF - BUG:19765
                $key = strtolower($key);
                if (isset($info[0]) && isset($info[0][$key]) && isset($info[0][$key][0])) {
                    $this->ldapUserInfo[$value] = $info[0][$key][0];
                }
            }

            //we should check that a user is a member of a specific group
            if (!empty($GLOBALS['ldap_config']->settings['ldap_group'])) {
                $GLOBALS['log']->debug("LDAPAuth: scanning group for user membership");
                $group_user_attr = $GLOBALS['ldap_config']->settings['ldap_group_user_attr'];
                $group_attr = $GLOBALS['ldap_config']->settings['ldap_group_attr'];
                if (!isset($info[0][$group_user_attr])) {
                    $GLOBALS['log']->fatal("ldapauth: $group_user_attr not found for user $name cannot authenticate against an LDAP group");
                    ldap_unbind($ldapconn);
                    return '';
                }
                $user_uid = $info[0][$group_user_attr];
                if (is_array($user_uid)) {
                    $user_uid = $user_uid[0];
                }
                // If user_uid contains special characters (for LDAP) we need to escape them !
                $user_uid = str_replace(array("(", ")"), array("\(", "\)"), (string) $user_uid);


                // build search query and determine if we are searching for a bare id or the full dn path
                $group_name = $GLOBALS['ldap_config']->settings['ldap_group_name'] . ","
                    . $GLOBALS['ldap_config']->settings['ldap_group_dn'];
                $GLOBALS['log']->debug("ldapauth: Searching for group name: " . $group_name);
                $user_search = "";
                if (!empty($GLOBALS['ldap_config']->settings['ldap_group_attr_req_dn'])
                    && $GLOBALS['ldap_config']->settings['ldap_group_attr_req_dn'] == 1) {
                    $GLOBALS['log']->debug("ldapauth: Checking for group membership using full user dn");
                    $user_search = "($group_attr=" . $group_user_attr . "=" . $user_uid . "," . $base_dn . ")";
                } else {
                    $user_search = "($group_attr=" . $user_uid . ")";
                }
                $GLOBALS['log']->debug("ldapauth: Searching for user: " . $user_search);

                //user is not a member of the group if the count is zero get the logs and return no id so it fails login
                if (!isset($user_uid)
                    || ldap_count_entries($ldapconn, ldap_search($ldapconn, $group_name, $user_search)) ==  0) {
                    $GLOBALS['log']->fatal("ldapauth: User ($name) is not a member of the LDAP group");
                    $user_id = var_export($user_uid, true);
                    $GLOBALS['log']->debug(
                        "ldapauth: Group DN:{$GLOBALS['ldap_config']->settings['ldap_group_dn']}"
                        . " Group Name: " . $GLOBALS['ldap_config']->settings['ldap_group_name']
                        . " Group Attribute: $group_attr  User Attribute: $group_user_attr :(" . $user_uid . ")"
                    );

                    ldap_unbind($ldapconn);
                    return '';
                }
            }



            ldap_unbind($ldapconn);
            $dbresult = DBManagerFactory::getInstance()->query("SELECT id, status FROM users WHERE user_name='" . $name . "' AND deleted = 0");

            //user already exists use this one
            if ($row = DBManagerFactory::getInstance()->fetchByAssoc($dbresult)) {
                if ($row['status'] != 'Inactive') {
                    return $row['id'];
                }
                return '';
            }

            //create a new user and return the user
            if ($GLOBALS['ldap_config']->settings['ldap_auto_create_users']) {
                return $this->createUser($name);
            }
            return '';
        }
        $GLOBALS['log']->fatal("SECURITY: failed LDAP bind (login) by $this->user_name using bind_user=$bind_user");
        $GLOBALS['log']->fatal("ldapauth: failed LDAP bind (login) by $this->user_name using bind_user=$bind_user");
        ldap_unbind($ldapconn);
        return '';
    }

    /**
     * takes in a name and creates the appropriate search filter for that user name including any additional filters specified in the system settings page
     * @param $name
     * @return String
     */
    public function getUserNameFilter($name)
    {
        $name_filter = "(" . $GLOBALS['ldap_config']->settings['ldap_login_attr']. "=" . $name . ")";
        //add the additional user filter if it is specified
        if (!empty($GLOBALS['ldap_config']->settings['ldap_login_filter'])) {
            $add_filter = $GLOBALS['ldap_config']->settings['ldap_login_filter'];
            if (substr((string) $add_filter, 0, 1) !== "(") {
                $add_filter = "(" . $add_filter . ")";
            }
            $name_filter = "(&" . $name_filter . $add_filter . ")";
        }
        return $name_filter;
    }

    /**
     * Creates a user with the given User Name and returns the id of that new user
     * populates the user with what was set in ldapUserInfo
     *
     * @param STRING $name
     * @return STRING $id
     */
    public function createUser($name)
    {
        $user = BeanFactory::newBean('Users');
        $user->user_name = $name;
        foreach ($this->ldapUserInfo as $key=>$value) {
            $user->$key = $value;
        }
        $user->employee_status = 'Active';
        $user->status = 'Active';
        $user->is_admin = 0;
        $user->external_auth_only = 1;
        $user->save();
        return $user->id;
    }
    /**
     * this is called when a user logs in
     *
     * @param string $name
     * @param string $password
     * @param boolean $fallback - is this authentication a fallback from a failed authentication
     * @param array $params
     * @return boolean
     */
    public function loadUserOnLogin($name, $password, $fallback = false, $params = array())
    {
        global $mod_strings;

        // Check if the LDAP extensions are loaded
        if (!function_exists('ldap_connect')) {
            $error = $mod_strings['LBL_LDAP_EXTENSION_ERROR'];
            $GLOBALS['log']->fatal($error);
            $_SESSION['login_error'] = $error;
            return false;
        }

        global $login_error;
        $GLOBALS['ldap_config']  = BeanFactory::newBean('Administration');
        $GLOBALS['ldap_config']->retrieveSettings('ldap');
        $GLOBALS['log']->debug("Starting user load for ". $name);
        if (empty($name) || empty($password)) {
            return false;
        }
        checkAuthUserStatus();

        $user_id = $this->authenticateUser($name, $password);
        if (empty($user_id)) {
            //check if the user can login as a normal sugar user
            $GLOBALS['log']->fatal('SECURITY: User authentication for '.$name.' failed');
            return false;
        }
        $this->loadUserOnSession($user_id);
        return true;
    }


    /**
     * Called with the error number of the last call if the error number is 0
     * there was no error otherwise it converts the error to a string and logs it as fatal
     *
     * @param INT $error
     * @return boolean
     */
    public function loginError($error)
    {
        if (empty($error)) {
            return false;
        }
        $errorstr = ldap_err2str($error);
        // BEGIN SUGAR INT
        $_SESSION['login_error'] = $errorstr;
        /*
        // END SUGAR INT
        $_SESSION['login_error'] = translate('ERR_INVALID_PASSWORD', 'Users');
        // BEGIN SUGAR INT
        */
        // END SUGAR INT
        $GLOBALS['log']->fatal('[LDAP ERROR]['. $error . ']'.$errorstr);
        return true;
    }

    /**
    * @return string appropriate value for username when binding to directory server.
    * @param string $user_name the value provided in login form
    * @desc Take the login username and return either said username for AD or lookup
     * distinguished name using anonymous credentials for OpenLDAP.
     * Contributions by Erik Mitchell erikm@logicpd.com
    */
    public function ldap_rdn_lookup($user_name, $password)
    {

        // MFH BUG# 14547 - Added htmlspecialchars_decode()
        $server = $GLOBALS['ldap_config']->settings['ldap_hostname'];
        $base_dn = htmlspecialchars_decode((string) $GLOBALS['ldap_config']->settings['ldap_base_dn']);
        if (!empty($GLOBALS['ldap_config']->settings['ldap_authentication'])) {
            $admin_user = htmlspecialchars_decode((string) $GLOBALS['ldap_config']->settings['ldap_admin_user']);
            $admin_password = htmlspecialchars_decode((string) $GLOBALS['ldap_config']->settings['ldap_admin_password']);
        } else {
            $admin_user = '';
            $admin_password = '';
        }
        $user_attr = $GLOBALS['ldap_config']->settings['ldap_login_attr'];
        $bind_attr = $GLOBALS['ldap_config']->settings['ldap_bind_attr'];
        $port = $GLOBALS['ldap_config']->settings['ldap_port'];
        if (!$port) {
            $port = DEFAULT_PORT;
        }
        $ldapconn = ldap_connect($server, $port);
        $error = ldap_errno($ldapconn);
        if ($this->loginError($error)) {
            return false;
        }
        ldap_set_option($ldapconn, LDAP_OPT_PROTOCOL_VERSION, 3);
        ldap_set_option($ldapconn, LDAP_OPT_REFERRALS, 0); // required for AD
        //if we are going to connect anonymously lets at least try to connect with the user connecting
        if (empty($admin_user)) {
            $bind = @ldap_bind($ldapconn, $user_name, $password);
            $error = ldap_errno($ldapconn);
        }
        if (empty($bind)) {
            $bind = @ldap_bind($ldapconn, $admin_user, $admin_password);
            $error = ldap_errno($ldapconn);
        }

        if ($this->loginError($error)) {
            return false;
        }
        if (!$bind) {
            $GLOBALS['log']->warn("ldapauth.ldap_rdn_lookup: Could not bind with admin user, trying to bind anonymously");
            $bind = @ldap_bind($ldapconn);
            $error = ldap_errno($ldapconn);

            if ($this->loginError($error)) {
                return false;
            }
            if (!$bind) {
                $GLOBALS['log']->warn("ldapauth.ldap_rdn_lookup: Could not bind anonymously, returning username");
                return $user_name;
            }
        }

        // If we get here we were able to bind somehow
        $search_filter = $this->getUserNameFilter($user_name);

        $GLOBALS['log']->info("ldapauth.ldap_rdn_lookup: Bind succeeded, searching for $user_attr=$user_name");
        $GLOBALS['log']->debug("ldapauth.ldap_rdn_lookup: base_dn:$base_dn , search_filter:$search_filter");

        $result = @ldap_search($ldapconn, $base_dn, $search_filter, array("dn", $bind_attr));
        $error = ldap_errno($ldapconn);
        if ($this->loginError($error)) {
            return false;
        }
        $info = ldap_get_entries($ldapconn, $result);
        if ($info['count'] == 0) {
            return false;
        }
        ldap_unbind($ldapconn);

        $GLOBALS['log']->info("ldapauth.ldap_rdn_lookup: Search result:\nldapauth.ldap_rdn_lookup: " . (is_countable($info) ? count($info) : 0));

        if ($bind_attr == "dn") {
            $found_bind_user = $info[0]['dn'];
        } else {
            $found_bind_user = $info[0][strtolower($bind_attr)][0];
        }

        $GLOBALS['log']->info("ldapauth.ldap_rdn_lookup: found_bind_user=" . $found_bind_user);

        if (!empty($found_bind_user)) {
            return $found_bind_user;
        } elseif ($user_attr == $bind_attr) {
            return $user_name;
        }
        return false;
    }
}
