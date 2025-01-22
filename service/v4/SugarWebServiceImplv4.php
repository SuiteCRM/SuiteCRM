<?php
if (!defined('sugarEntry')) {
    define('sugarEntry', true);
}
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


/**
 * This class is an implementation class for all the rest services
 */
require_once('service/v3_1/SugarWebServiceImplv3_1.php');
require_once('SugarWebServiceUtilv4.php');


class SugarWebServiceImplv4 extends SugarWebServiceImplv3_1
{
    public function __construct()
    {
        self::$helperObject = new SugarWebServiceUtilv4();
    }

    /**
     * Log the user into the application
     *
     * @param UserAuth array $user_auth -- Set user_name and password (password needs to be
     *      in the right encoding for the type of authentication the user is setup for.  For Base
     *      sugar validation, password is the MD5 sum of the plain text password.
     * @param String $application -- The name of the application you are logging in from.  (Currently unused).
     * @param array $name_value_list -- Array of name value pair of extra parameters. As of today only 'language' and 'notifyonsave' is supported
     * @return Array - id - String id is the session_id of the session that was created.
     * 				 - module_name - String - module name of user
     * 				 - name_value_list - Array - The name value pair of user_id, user_name, user_language, user_currency_id, user_currency_name,
     *                                         - user_default_team_id, user_is_admin, user_default_dateformat, user_default_timeformat
     * @exception 'SoapFault' -- The SOAP error, if any
     */
    public function login($user_auth, $application = null, $name_value_list = array(), ...$args)
    {
        $application_name = $args['application_name'] ?? '';
        if ($application_name === '' && $application === null){
            $GLOBALS['log']->info("Application name not set. Please set using 'application_name' or 'application'. ");
        }
        $GLOBALS['log']->info("Begin: SugarWebServiceImpl->login({$user_auth['user_name']}, $application_name, ". print_r($name_value_list, true) .")");
        global $sugar_config, $system_config;
        $error = new SoapError();
        $user = BeanFactory::newBean('Users');
        $success = false;
        //rrs
        $system_config = BeanFactory::newBean('Administration');
        $system_config->retrieveSettings('system');
        $authController = new AuthenticationController();
        //rrs
        if (!empty($user_auth['encryption']) && $user_auth['encryption'] === 'PLAIN' && $authController->authController->userAuthenticateClass != "LDAPAuthenticateUser") {
            $user_auth['password'] = md5($user_auth['password']);
        }
        $isLoginSuccess = $authController->login($user_auth['user_name'], $user_auth['password'], array('passwordEncrypted' => true));
        $usr_id=$user->retrieve_user_id($user_auth['user_name']);
        if ($usr_id) {
            $user->retrieve($usr_id);
        }

        if ($isLoginSuccess) {
            if ($_SESSION['hasExpiredPassword'] =='1') {
                $error->set_error('password_expired');
                $GLOBALS['log']->fatal('password expired for user ' . $user_auth['user_name']);
                LogicHook::initialize();
                $GLOBALS['logic_hook']->call_custom_logic('Users', 'login_failed');
                self::$helperObject->setFaultObject($error);
                return;
            }
            if (!empty($user) && !empty($user->id) && !$user->is_group) {
                $success = true;
                global $current_user;
                $current_user = $user;
            }
        } elseif ($usr_id && isset($user->user_name) && ($user->getPreference('lockout') == '1')) {
            $error->set_error('lockout_reached');
            $GLOBALS['log']->fatal('Lockout reached for user ' . $user_auth['user_name']);
            LogicHook::initialize();
            $GLOBALS['logic_hook']->call_custom_logic('Users', 'login_failed');
            self::$helperObject->setFaultObject($error);
            return;
        } elseif (function_exists('openssl_decrypt') && $authController->authController->userAuthenticateClass == "LDAPAuthenticateUser"
                && (empty($user_auth['encryption']) || $user_auth['encryption'] !== 'PLAIN')) {
            $password = self::$helperObject->decrypt_string($user_auth['password']);
            $authController->loggedIn = false; // reset login attempt to try again with decrypted password
            if ($authController->login($user_auth['user_name'], $password) && isset($_SESSION['authenticated_user_id'])) {
                $success = true;
            }
        } elseif ($authController->authController->userAuthenticateClass == "LDAPAuthenticateUser"
                 && (empty($user_auth['encryption']) || $user_auth['encryption'] == 'PLAIN')) {
            $authController->loggedIn = false; // reset login attempt to try again with md5 password
            if ($authController->login($user_auth['user_name'], md5($user_auth['password']), array('passwordEncrypted' => true))
                && isset($_SESSION['authenticated_user_id'])) {
                $success = true;
            } else {
                $error->set_error('ldap_error');
                LogicHook::initialize();
                $GLOBALS['logic_hook']->call_custom_logic('Users', 'login_failed');
                self::$helperObject->setFaultObject($error);
                return;
            }
        }


        if ($success) {
            session_start();
            global $current_user;
            //$current_user = $user;
            self::$helperObject->login_success($name_value_list);
            $current_user->loadPreferences();
            $_SESSION['is_valid_session']= true;
            $_SESSION['ip_address'] = query_client_ip();
            $_SESSION['user_id'] = $current_user->id;
            $_SESSION['type'] = 'user';
            $_SESSION['avail_modules']= self::$helperObject->get_user_module_list($current_user);
            $_SESSION['authenticated_user_id'] = $current_user->id;
            $_SESSION['unique_key'] = $sugar_config['unique_key'];
            $GLOBALS['log']->info('End: SugarWebServiceImpl->login - successful login');
            $current_user->call_custom_logic('after_login');
            $nameValueArray = array();
            global $current_language;
            $nameValueArray['user_id'] = self::$helperObject->get_name_value('user_id', $current_user->id);
            $nameValueArray['user_name'] = self::$helperObject->get_name_value('user_name', $current_user->user_name);
            $nameValueArray['user_language'] = self::$helperObject->get_name_value('user_language', $current_language);
            $cur_id = $current_user->getPreference('currency');
            $nameValueArray['user_currency_id'] = self::$helperObject->get_name_value('user_currency_id', $cur_id);
            $nameValueArray['user_is_admin'] = self::$helperObject->get_name_value('user_is_admin', is_admin($current_user));
            $nameValueArray['user_default_team_id'] = self::$helperObject->get_name_value('user_default_team_id', $current_user->default_team);
            $nameValueArray['user_default_dateformat'] = self::$helperObject->get_name_value('user_default_dateformat', $current_user->getPreference('datef'));
            $nameValueArray['user_default_timeformat'] = self::$helperObject->get_name_value('user_default_timeformat', $current_user->getPreference('timef'));

            $num_grp_sep = $current_user->getPreference('num_grp_sep');
            $dec_sep = $current_user->getPreference('dec_sep');
            $nameValueArray['user_number_separator'] = self::$helperObject->get_name_value('user_number_separator', empty($num_grp_sep) ? $sugar_config['default_number_grouping_seperator'] : $num_grp_sep);
            $nameValueArray['user_decimal_separator'] = self::$helperObject->get_name_value('user_decimal_separator', empty($dec_sep) ? $sugar_config['default_decimal_seperator'] : $dec_sep);

            $nameValueArray['mobile_max_list_entries'] = self::$helperObject->get_name_value('mobile_max_list_entries', $sugar_config['wl_list_max_entries_per_page']);
            $nameValueArray['mobile_max_subpanel_entries'] = self::$helperObject->get_name_value('mobile_max_subpanel_entries', $sugar_config['wl_list_max_entries_per_subpanel']);


            $currencyObject = BeanFactory::newBean('Currencies');
            $currencyObject->retrieve($cur_id);
            $nameValueArray['user_currency_name'] = self::$helperObject->get_name_value('user_currency_name', $currencyObject->name);
            $_SESSION['user_language'] = $current_language;
            return array('id'=>session_id(), 'module_name'=>'Users', 'name_value_list'=>$nameValueArray);
        }
        LogicHook::initialize();
        $GLOBALS['logic_hook']->call_custom_logic('Users', 'login_failed');
        $error->set_error('invalid_login');
        self::$helperObject->setFaultObject($error);
        $GLOBALS['log']->error('End: SugarWebServiceImpl->login - failed login');
    }


    /**
     * Retrieve a list of SugarBean's based on provided IDs. This API will not wotk with report module
     *
     * @param String $session -- Session ID returned by a previous call to login.
     * @param String $module_name -- The name of the module to return records from.  This name should be the name the module was developed under (changing a tab name is studio does not affect the name that should be passed into this method)..
     * @param Array $ids -- An array of SugarBean IDs.
     * @param Array $select_fields -- A list of the fields to be included in the results. This optional parameter allows for only needed fields to be retrieved.
     * @param Array $link_name_to_fields_array -- A list of link_names and for each link_name, what fields value to be returned. For ex.'link_name_to_fields_array' => array(array('name' =>  'email_addresses', 'value' => array('id', 'email_address', 'opt_out', 'primary_address')))
     * @return Array
     *        'entry_list' -- Array - The records name value pair for the simple data types excluding link field data.
     *	     'relationship_list' -- Array - The records link field data. The example is if asked about accounts email address then return data would look like Array ( [0] => Array ( [name] => email_addresses [records] => Array ( [0] => Array ( [0] => Array ( [name] => id [value] => 3fb16797-8d90-0a94-ac12-490b63a6be67 ) [1] => Array ( [name] => email_address [value] => hr.kid.qa@example.com ) [2] => Array ( [name] => opt_out [value] => 0 ) [3] => Array ( [name] => primary_address [value] => 1 ) ) [1] => Array ( [0] => Array ( [name] => id [value] => 403f8da1-214b-6a88-9cef-490b63d43566 ) [1] => Array ( [name] => email_address [value] => kid.hr@example.name ) [2] => Array ( [name] => opt_out [value] => 0 ) [3] => Array ( [name] => primary_address [value] => 0 ) ) ) ) )
     * @exception 'SoapFault' -- The SOAP error, if any
     */
    public function get_entries(
        $session,
        $module_name,
        $ids,
        $select_fields,
        $link_name_to_fields_array,
        $track_view = false
    ) {
        $result = parent::get_entries(
            $session,
            $module_name,
            $ids,
            $select_fields,
            $link_name_to_fields_array,
            $track_view
        );
        $relationshipList = $result['relationship_list'];
        $returnRelationshipList = array();
        foreach ($relationshipList as $rel) {
            $link_output = array();
            foreach ($rel as $row) {
                $rowArray = array();
                foreach ($row['records'] as $record) {
                    $rowArray[]['link_value'] = $record;
                }
                $link_output[] = array('name' => $row['name'], 'records' => $rowArray);
            }
            $returnRelationshipList[]['link_list'] = $link_output;
        }

        $result['relationship_list'] = $returnRelationshipList;

        return $result;
    }

    /**
     * Retrieve a list of beans.  This is the primary method for getting list of SugarBeans from Sugar using the SOAP API.
     *
     * @param string $session -- Session ID returned by a previous call to login.
     * @param string $module_name -- The name of the module to return records from.  This name should be the name the module was developed under (changing a tab name is studio does not affect the name that should be passed into this method)..
     * @param string $query -- SQL where clause without the word 'where'
     * @param string $order_by -- SQL order by clause without the phrase 'order by'
     * @param integer $offset -- The record offset to start from.
     * @param array $select_fields -- A list of the fields to be included in the results. This optional parameter allows for only needed fields to be retrieved.
     * @param array $link_name_to_fields_array -- A list of link_names and for each link_name, what fields value to be returned. For ex.'link_name_to_fields_array' => array(array('name' =>  'email_addresses', 'value' => array('id', 'email_address', 'opt_out', 'primary_address')))
     * @param integer $max_results -- The maximum number of records to return.  The default is the sugar configuration value for 'list_max_entries_per_page'
     * @param bool $deleted -- false if deleted records should not be include, true if deleted records should be included.
     * @return array 'result_count' -- integer - The number of records returned
     *               'next_offset' -- integer - The start of the next page (This will always be the previous offset plus the number of rows returned.  It does not indicate if there is additional data unless you calculate that the next_offset happens to be closer than it should be.
     *               'entry_list' -- Array - The records that were retrieved
     *                 'relationship_list' -- Array - The records link field data. The example is if asked about accounts email address then return data would look like Array ( [0] => Array ( [name] => email_addresses [records] => Array ( [0] => Array ( [0] => Array ( [name] => id [value] => 3fb16797-8d90-0a94-ac12-490b63a6be67 ) [1] => Array ( [name] => email_address [value] => hr.kid.qa@example.com ) [2] => Array ( [name] => opt_out [value] => 0 ) [3] => Array ( [name] => primary_address [value] => 1 ) ) [1] => Array ( [0] => Array ( [name] => id [value] => 403f8da1-214b-6a88-9cef-490b63d43566 ) [1] => Array ( [name] => email_address [value] => kid.hr@example.name ) [2] => Array ( [name] => opt_out [value] => 0 ) [3] => Array ( [name] => primary_address [value] => 0 ) ) ) ) )
     * @exception 'SoapFault' -- The SOAP error, if any
     */
    public function get_entry_list(
        $session = null,
        $module_name = null,
        $query = null,
        $order_by = null,
        $offset = null,
        $select_fields = null,
        $link_name_to_fields_array = null,
        $max_results = null,
        $deleted = false,
        $favorites = false
    ) {
        $GLOBALS['log']->info('Begin: SugarWebServiceImpl->get_entry_list');
        global  $beanList, $beanFiles;
        $error = new SoapError();
        $using_cp = false;
        if ($module_name == 'CampaignProspects') {
            $module_name = 'Prospects';
            $using_cp = true;
        }
        if (!self::$helperObject->checkSessionAndModuleAccess($session, 'invalid_session', $module_name, 'read', 'no_access', $error)) {
            $GLOBALS['log']->error('End: SugarWebServiceImpl->get_entry_list - FAILED on checkSessionAndModuleAccess');
            return;
        } // if

        if (!self::$helperObject->checkQuery($error, $query, $order_by)) {
            $GLOBALS['log']->info('End: SugarWebServiceImpl->get_entry_list');
            return;
        } // if

        // If the maximum number of entries per page was specified, override the configuration value.
        if ($max_results > 0) {
            global $sugar_config;
            $sugar_config['list_max_entries_per_page'] = $max_results;
        } // if

        $class_name = $beanList[$module_name];
        require_once($beanFiles[$class_name]);
        $seed = new $class_name();

        if (!self::$helperObject->checkACLAccess($seed, 'list', $error, 'no_access')) {
            $GLOBALS['log']->error('End: SugarWebServiceImpl->get_entry_list - FAILED on checkACLAccess');
            return;
        } // if

        if ($query == '') {
            $where = '';
        } // if
        if ($offset == '' || $offset == -1) {
            $offset = 0;
        } // if
        if ($deleted) {
            $deleted = -1;
        }
        if ($using_cp) {
            $response = $seed->retrieveTargetList($query, $select_fields, $offset, -1, -1, $deleted);
        } else {
            $response = self::$helperObject->get_data_list($seed, $order_by, $query, $offset, -1, -1, $deleted, $favorites);
        } // else
        $list = $response['list'];

        $output_list = array();
        $linkoutput_list = array();

        foreach ($list as $value) {
            if (isset($value->emailAddress)) {
                $value->emailAddress->handleLegacyRetrieve($value);
            } // if
            $value->fill_in_additional_detail_fields();

            $output_list[] = self::$helperObject->get_return_value_for_fields($value, $module_name, $select_fields);
            if (!empty($link_name_to_fields_array)) {
                $linkoutput_list[] = self::$helperObject->get_return_value_for_link_fields($value, $module_name, $link_name_to_fields_array);
            }
        } // foreach

        // Calculate the offset for the start of the next page
        $next_offset = $offset + count($output_list);

        $returnRelationshipList = array();
        foreach ($linkoutput_list as $rel) {
            $link_output = array();
            foreach ($rel as $row) {
                $rowArray = array();
                foreach ($row['records'] as $record) {
                    $rowArray[]['link_value'] = $record;
                }
                $link_output[] = array('name' => $row['name'], 'records' => $rowArray);
            }
            $returnRelationshipList[]['link_list'] = $link_output;
        }

        $totalRecordCount = $response['row_count'];
        if (!empty($sugar_config['disable_count_query'])) {
            $totalRecordCount = -1;
        }

        $GLOBALS['log']->info('End: SugarWebServiceImpl->get_entry_list - SUCCESS');
        return array('result_count'=>count($output_list), 'total_count' => $totalRecordCount, 'next_offset'=>$next_offset, 'entry_list'=>$output_list, 'relationship_list' => $returnRelationshipList);
    } // fn

    /**
     * Retrieve the layout metadata for a given module given a specific type and view.
     *
     * @param String $session -- Session ID returned by a previous call to login.
     * @param array $module_name(s) -- The name of the module(s) to return records from.  This name should be the name the module was developed under (changing a tab name is studio does not affect the name that should be passed into this method)..
     * @return array $type The type(s) of views requested.  Current supported types are 'default' (for application) and 'wireless'
     * @return array $view The view(s) requested.  Current supported types are edit, detail, list, and subpanel.
     * @exception 'SoapFault' -- The SOAP error, if any
     */
    public function get_module_layout($session, $a_module_names, $a_type, $a_view, $acl_check = true, $md5 = false)
    {
        $GLOBALS['log']->info('Begin: SugarWebServiceImpl->get_module_layout');

        global  $beanList, $beanFiles;
        $error = new SoapError();
        $results = array();
        foreach ($a_module_names as $module_name) {
            if (!self::$helperObject->checkSessionAndModuleAccess($session, 'invalid_session', $module_name, 'read', 'no_access', $error)) {
                $GLOBALS['log']->error("End: SugarWebServiceImpl->get_module_layout for $module_name - FAILED on checkSessionAndModuleAccess");
                continue;
            }

            if (empty($module_name)) {
                continue;
            }

            $class_name = $beanList[$module_name];
            require_once($beanFiles[$class_name]);
            $seed = new $class_name();

            foreach ($a_view as $view) {
                $aclViewCheck = (strtolower($view) == 'subpanel') ? 'DetailView' : ucfirst(strtolower($view)) . 'View';
                if (!$acl_check || $seed->ACLAccess($aclViewCheck, true)) {
                    foreach ($a_type as $type) {
                        $a_vardefs = self::$helperObject->get_module_view_defs($module_name, $type, $view);
                        if ($md5) {
                            $results[$module_name][$type][$view] = md5(serialize($a_vardefs));
                        } else {
                            $results[$module_name][$type][$view] = $a_vardefs;
                        }
                    }
                }
            }
        }

        $GLOBALS['log']->info('End: SugarWebServiceImpl->get_module_layout ->> '.print_r($results, true));

        return $results;
    }


    /**
     * Given a list of modules to search and a search string, return the id, module_name, along with the fields
     * We will support Accounts, Bugs, Cases, Contacts, Leads, Opportunities, Project, ProjectTask, Quotes
     *
     * @param string $session			- Session ID returned by a previous call to login.
     * @param string $search_string 	- string to search
     * @param string[] $modules			- array of modules to query
     * @param int $offset				- a specified offset in the query
     * @param int $max_results			- max number of records to return
     * @param string $assigned_user_id	- a user id to filter all records by, leave empty to exclude the filter
     * @param string[] $select_fields   - An array of fields to return.  If empty the default return fields will be from the active list view defs.
     * @param bool $unified_search_only - A boolean indicating if we should only search against those modules participating in the unified search.
     * @param bool $favorites           - A boolean indicating if we should only search against records marked as favorites.
     * @return Array return_search_result 	- Array('Accounts' => array(array('name' => 'first_name', 'value' => 'John', 'name' => 'last_name', 'value' => 'Do')))
     * @exception 'SoapFault' -- The SOAP error, if any
     */
    public function search_by_module($session, $search_string, $modules, $offset, $max_results, $assigned_user_id = '', $select_fields = array(), $unified_search_only = true, $favorites = false)
    {
        $GLOBALS['log']->info('Begin: SugarWebServiceImpl->search_by_module');
        global  $beanList, $beanFiles;
        global $sugar_config,$current_language;

        $error = new SoapError();
        $output_list = array();
        if (!self::$helperObject->checkSessionAndModuleAccess($session, 'invalid_session', '', '', '', $error)) {
            $error->set_error('invalid_login');
            $GLOBALS['log']->error('End: SugarWebServiceImpl->search_by_module - FAILED on checkSessionAndModuleAccess');
            return;
        }
        global $current_user;
        if ($max_results > 0) {
            $sugar_config['list_max_entries_per_page'] = $max_results;
        }

        $unified_search_modules = [];
        require_once('modules/Home/UnifiedSearchAdvanced.php');
        require_once 'include/utils.php';
        $usa = new UnifiedSearchAdvanced();
        if (!file_exists($cachefile = sugar_cached('modules/unified_search_modules.php'))) {
            $usa->buildCache();
        }

        include $cachefile;
        $modules_to_search = array();
        $unified_search_modules['Users'] =   array('fields' => array());

        $unified_search_modules['ProjectTask'] =   array('fields' => array());

        //If we are ignoring the unified search flag within the vardef we need to re-create the search fields.  This allows us to search
        //against a specific module even though it is not enabled for the unified search within the application.
        if (!$unified_search_only) {
            foreach ($modules as $singleModule) {
                if (!isset($unified_search_modules[$singleModule])) {
                    $newSearchFields = array('fields' => self::$helperObject->generateUnifiedSearchFields($singleModule) );
                    $unified_search_modules[$singleModule] = $newSearchFields;
                }
            }
        }


        foreach ($unified_search_modules as $module=>$data) {
            if (in_array($module, $modules)) {
                $modules_to_search[$module] = $beanList[$module];
            } // if
        } // foreach

        $GLOBALS['log']->info('SugarWebServiceImpl->search_by_module - search string = ' . $search_string);

        if (!empty($search_string) && isset($search_string)) {
            $search_string = trim(DBManagerFactory::getInstance()->quote(securexss(from_html(clean_string($search_string, 'UNIFIED_SEARCH')))));
            foreach ($modules_to_search as $name => $beanName) {
                $where_clauses_array = array();
                $unifiedSearchFields = array() ;
                foreach ($unified_search_modules[$name]['fields'] as $field=>$def) {
                    $unifiedSearchFields[$name] [ $field ] = $def ;
                    $unifiedSearchFields[$name] [ $field ]['value'] = $search_string;
                }

                require_once $beanFiles[$beanName] ;
                $seed = new $beanName();
                require_once 'include/SearchForm/SearchForm2.php' ;
                if ($beanName == "User"
                    || $beanName == "ProjectTask"
                    ) {
                    if (!self::$helperObject->check_modules_access($current_user, $seed->module_dir, 'read')) {
                        continue;
                    } // if
                    if (!$seed->ACLAccess('ListView')) {
                        continue;
                    } // if
                }

                if ($beanName != "User"
                    && $beanName != "ProjectTask"
                    ) {
                    require_once 'include/SearchForm/SearchForm2.php' ;
                    $searchForm = new SearchForm($seed, $name) ;

                    $searchForm->setup(array($name => array()), $unifiedSearchFields, '', 'saved_views' /* hack to avoid setup doing further unwanted processing */) ;
                    $where_clauses = $searchForm->generateSearchWhere() ;
                    $emailQuery = false;

                    $where = '';
                    if ((is_countable($where_clauses) ? count($where_clauses) : 0) > 0) {
                        $where = '('. implode(' ) OR ( ', $where_clauses) . ')';
                    }

                    $mod_strings = return_module_language($current_language, $seed->module_dir);

                    if (count($select_fields) > 0) {
                        $filterFields = $select_fields;
                    } else {
                        if (file_exists('custom/modules/'.$seed->module_dir.'/metadata/listviewdefs.php')) {
                            require_once('custom/modules/'.$seed->module_dir.'/metadata/listviewdefs.php');
                        } else {
                            require_once('modules/'.$seed->module_dir.'/metadata/listviewdefs.php');
                        }

                        $filterFields = array();
                        foreach ($listViewDefs[$seed->module_dir] as $colName => $param) {
                            if (!empty($param['default']) && $param['default'] == true) {
                                $filterFields[] = strtolower($colName);
                            }
                        }
                        if (!in_array('id', $filterFields)) {
                            $filterFields[] = 'id';
                        }
                    }

                    //Pull in any db fields used for the unified search query so the correct joins will be added
                    $selectOnlyQueryFields = array();
                    foreach ($unifiedSearchFields[$name] as $field => $def) {
                        if (isset($def['db_field']) && !in_array($field, $filterFields)) {
                            $filterFields[] = $field;
                            $selectOnlyQueryFields[] = $field;
                        }
                    }

                    //Add the assigned user filter if applicable
                    if (!empty($assigned_user_id) && isset($seed->field_defs['assigned_user_id'])) {
                        $ownerWhere = $seed->getOwnerWhere($assigned_user_id);
                        $where = "($where) AND $ownerWhere";
                    }

                    if ($beanName == "Employee") {
                        $where = "($where) AND users.deleted = 0 AND users.is_group = 0 AND users.employee_status = 'Active'";
                    }

                    $list_params = array();

                    $ret_array = $seed->create_new_list_query('', $where, $filterFields, $list_params, 0, '', true, $seed, true);
                    if (empty($params) || !is_array($params)) {
                        $params = array();
                    }
                    if (!isset($params['custom_select'])) {
                        $params['custom_select'] = '';
                    }
                    if (!isset($params['custom_from'])) {
                        $params['custom_from'] = '';
                    }
                    if (!isset($params['custom_where'])) {
                        $params['custom_where'] = '';
                    }
                    if (!isset($params['custom_order_by'])) {
                        $params['custom_order_by'] = '';
                    }
                    $main_query = $ret_array['select'] . $params['custom_select'] . $ret_array['from'] . $params['custom_from'] . $ret_array['where'] . $params['custom_where'] . $ret_array['order_by'] . $params['custom_order_by'];
                } else {
                    if ($beanName == "User") {
                        $filterFields = array('id', 'user_name', 'first_name', 'last_name', 'email_address');
                        $main_query = "select users.id, ea.email_address, users.user_name, first_name, last_name from users ";
                        $main_query = $main_query . " LEFT JOIN email_addr_bean_rel eabl ON eabl.bean_module = '{$seed->module_dir}'
    LEFT JOIN email_addresses ea ON (ea.id = eabl.email_address_id) ";
                        $main_query = $main_query . "where ((users.first_name like '{$search_string}') or (users.last_name like '{$search_string}') or (users.user_name like '{$search_string}') or (ea.email_address like '{$search_string}')) and users.deleted = 0 and users.is_group = 0 and users.employee_status = 'Active'";
                    } // if
                    if ($beanName == "ProjectTask") {
                        $filterFields = array('id', 'name', 'project_id', 'project_name');
                        $main_query = "select {$seed->table_name}.project_task_id id,{$seed->table_name}.project_id, {$seed->table_name}.name, project.name project_name from {$seed->table_name} ";
                        $main_query .= "LEFT JOIN project ON $seed->table_name.project_id = project.id ";
                        $main_query .= "where {$seed->table_name}.name like '{$search_string}%'";
                    } // if
                } // else

                $GLOBALS['log']->info('SugarWebServiceImpl->search_by_module - query = ' . $main_query);
                if ($max_results < -1) {
                    $result = $seed->db->query($main_query);
                } else {
                    if ($max_results == -1) {
                        $limit = $sugar_config['list_max_entries_per_page'];
                    } else {
                        $limit = $max_results;
                    }
                    $result = $seed->db->limitQuery($main_query, $offset, $limit + 1);
                }

                $rowArray = array();
                while ($row = $seed->db->fetchByAssoc($result)) {
                    $nameValueArray = array();
                    foreach ($filterFields as $field) {
                        if (in_array($field, $selectOnlyQueryFields)) {
                            continue;
                        }
                        $nameValue = array();
                        if (isset($row[$field])) {
                            $nameValueArray[$field] = self::$helperObject->get_name_value($field, $row[$field]);
                        } // if
                    } // foreach
                    $rowArray[] = $nameValueArray;
                } // while
                $output_list[] = array('name' => $name, 'records' => $rowArray);
            } // foreach

            $GLOBALS['log']->info('End: SugarWebServiceImpl->search_by_module');
            return array('entry_list'=>$output_list);
        } // if
        return array('entry_list'=>$output_list);
    } // fn



    /**
     * Get OAuth reqtest token
     */
    public function oauth_request_token()
    {
        $GLOBALS['log']->info('Begin: SugarWebServiceImpl->oauth_request_token');
        require_once "include/SugarOAuthServer.php";
        try {
            $oauth = new SugarOAuthServer(rtrim($GLOBALS['sugar_config']['site_url'], '/').'/service/v4/rest.php');
            $result = $oauth->requestToken()."&oauth_callback_confirmed=true&authorize_url=".$oauth->authURL();
        } catch (OAuthException $e) {
            $GLOBALS['log']->debug("OAUTH Exception: $e");
            $errorObject = new SoapError();
            $errorObject->set_error('invalid_login');
            self::$helperObject->setFaultObject($errorObject);
            $result = null;
        }
        $GLOBALS['log']->info('End: SugarWebServiceImpl->oauth_request_token');
        return $result;
    }

    /**
     * Get OAuth access token
     */
    public function oauth_access_token()
    {
        $GLOBALS['log']->info('Begin: SugarWebServiceImpl->oauth_access_token');
        require_once "include/SugarOAuthServer.php";
        try {
            $oauth = new SugarOAuthServer();
            $result = $oauth->accessToken();
        } catch (OAuthException $e) {
            $GLOBALS['log']->debug("OAUTH Exception: $e");
            $errorObject = new SoapError();
            $errorObject->set_error('invalid_login');
            self::$helperObject->setFaultObject($errorObject);
            $result = null;
        }
        $GLOBALS['log']->info('End: SugarWebServiceImpl->oauth_access_token');
        return $result;
    }

    public function oauth_access($session='')
    {
        $GLOBALS['log']->info('Begin: SugarWebServiceImpl->oauth_access');
        $error = new SoapError();
        $output_list = array();
        if (!self::$helperObject->checkSessionAndModuleAccess($session, 'invalid_session', '', '', '', $error)) {
            $error->set_error('invalid_login');
            $GLOBALS['log']->info('End: SugarWebServiceImpl->oauth_access');
            $result = $error;
        } else {
            $result = array('id'=>session_id());
        }
        $GLOBALS['log']->info('End: SugarWebServiceImpl->oauth_access');
        return $result;
    }


    /**
     * Get next job from the queue
     * @param string $session
     * @param string $clientid
     */
    public function job_queue_next($session, $clientid)
    {
        $GLOBALS['log']->info('Begin: SugarWebServiceImpl->job_queue_next');
        $error = new SoapError();
        if (! self::$helperObject->checkSessionAndModuleAccess($session, 'invalid_session', '', 'read', 'no_access', $error)) {
            $GLOBALS['log']->info('End: SugarWebServiceImpl->job_queue_next denied.');
            return;
        }
        require_once 'include/SugarQueue/SugarJobQueue.php';
        $queue = new SugarJobQueue();
        $job = $queue->nextJob($clientid);
        if (!empty($job)) {
            $jobid = $job->id;
        } else {
            $jobid = null;
        }
        $GLOBALS['log']->info('End: SugarWebServiceImpl->job_queue_next');
        return array("results" => $jobid);
    }

    /**
     * Run cleanup and schedule
     * @param string $session
     * @param string $clientid
     */
    public function job_queue_cycle($session, $clientid)
    {
        $GLOBALS['log']->info('Begin: SugarWebServiceImpl->job_queue_cycle');
        $error = new SoapError();
        if (! self::$helperObject->checkSessionAndModuleAccess($session, 'invalid_session', '', 'read', 'no_access', $error)) {
            $GLOBALS['log']->info('End: SugarWebServiceImpl->job_queue_cycle denied.');
            return;
        }
        require_once 'include/SugarQueue/SugarJobQueue.php';
        $queue = new SugarJobQueue();
        $queue->cleanup();
        $queue->runSchedulers();
        $GLOBALS['log']->info('End: SugarWebServiceImpl->job_queue_cycle');
        return array("results" => "ok");
    }

    /**
     * Run job from queue
     * @param string $session
     * @param string $jobid
     * @param string $clientid
     */
    public function job_queue_run($session, $jobid, $clientid)
    {
        $GLOBALS['log']->info('Begin: SugarWebServiceImpl->job_queue_run');
        $error = new SoapError();
        if (! self::$helperObject->checkSessionAndModuleAccess($session, 'invalid_session', '', 'read', 'no_access', $error)) {
            $GLOBALS['log']->info('End: SugarWebServiceImpl->job_queue_run denied.');
            return;
        }
        $GLOBALS['log']->debug('Starting job $jobid execution as $clientid');
        require_once 'modules/SchedulersJobs/SchedulersJob.php';
        $result = SchedulersJob::runJobId($jobid, $clientid);
        $GLOBALS['log']->info('End: SugarWebServiceImpl->job_queue_run');
        if ($result === true) {
            return array("results" => true);
        }
        return array("results" => false, "message" => $result);
    }
}

SugarWebServiceImplv4::$helperObject = new SugarWebServiceUtilv4();
