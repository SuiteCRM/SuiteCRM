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
    die('Not A Valid Entry Point');
}


global $disable_date_format;
$disable_date_format = true;

#[\AllowDynamicProperties]
class SoapHelperWebServices
{
    public function get_field_list($value, $fields, $translate = true)
    {
        $GLOBALS['log']->info('Begin: SoapHelperWebServices->get_field_list(' . print_r(
            $value,
            true
        ) . ', ' . print_r($fields, true) . ", $translate");
        $module_fields = array();
        $link_fields = array();
        if (!empty($value->field_defs)) {
            foreach ($value->field_defs as $var) {
                if (!empty($fields) && !in_array($var['name'], $fields)) {
                    continue;
                }
                if (isset($var['source']) && ($var['source'] != 'db' && $var['source'] != 'non-db' && $var['source'] != 'custom_fields') && $var['name'] != 'email1' && $var['name'] != 'email2' && (!isset($var['type']) || $var['type'] != 'relate')) {
                    continue;
                }
                if ($var['source'] == 'non_db' && (isset($var['type']) && $var['type'] != 'link')) {
                    continue;
                }
                $required = 0;
                $options_dom = array();
                $options_ret = array();
                // Apparently the only purpose of this check is to make sure we only return fields
                //   when we've read a record.  Otherwise this function is identical to get_module_field_list

                if (isset($var['required']) && $var['required'] && $var['required'] !== 'false') {
                    $required = 1;
                }
                if (isset($var['options'])) {
                    $options_dom = translate($var['options'], $value->module_dir);
                    if (!is_array($options_dom)) {
                        $options_dom = array();
                    }
                    foreach ($options_dom as $key => $oneOption) {
                        $options_ret[$key] = $this->get_name_value($key, $oneOption);
                    }
                }

                if (!empty($var['dbType']) && $var['type'] == 'bool') {
                    $options_ret['type'] = $this->get_name_value('type', $var['dbType']);
                }

                $entry = array();
                $entry['name'] = $var['name'];
                $entry['type'] = $var['type'];
                if ($var['type'] == 'link') {
                    $entry['relationship'] = (isset($var['relationship']) ? $var['relationship'] : '');
                    $entry['module'] = (isset($var['module']) ? $var['module'] : '');
                    $entry['bean_name'] = (isset($var['bean_name']) ? $var['bean_name'] : '');
                    $link_fields[$var['name']] = $entry;
                } else {
                    if ($translate) {
                        $entry['label'] = isset($var['vname']) ? translate(
                            $var['vname'],
                            $value->module_dir
                        ) : $var['name'];
                    } else {
                        $entry['label'] = isset($var['vname']) ? $var['vname'] : $var['name'];
                    }
                    $entry['required'] = $required;
                    $entry['options'] = $options_ret;
                    if (isset($var['default'])) {
                        $entry['default_value'] = $var['default'];
                    }
                    $module_fields[$var['name']] = $entry;
                } // else
            } //foreach
        } //if

        if ($value->module_dir == 'Bugs') {
            require_once('modules/Releases/Release.php');
            $seedRelease = BeanFactory::newBean('Releases');
            $options = $seedRelease->get_releases(true, "Active");
            $options_ret = array();
            foreach ($options as $name => $value) {
                $options_ret[] = array('name' => $name, 'value' => $value);
            }
            if (isset($module_fields['fixed_in_release'])) {
                $module_fields['fixed_in_release']['type'] = 'enum';
                $module_fields['fixed_in_release']['options'] = $options_ret;
            }
            if (isset($module_fields['release'])) {
                $module_fields['release']['type'] = 'enum';
                $module_fields['release']['options'] = $options_ret;
            }
            if (isset($module_fields['release_name'])) {
                $module_fields['release_name']['type'] = 'enum';
                $module_fields['release_name']['options'] = $options_ret;
            }
        }

        if (isset($value->assigned_user_name) && isset($module_fields['assigned_user_id'])) {
            $module_fields['assigned_user_name'] = $module_fields['assigned_user_id'];
            $module_fields['assigned_user_name']['name'] = 'assigned_user_name';
        }
        if (isset($module_fields['modified_user_id'])) {
            $module_fields['modified_by_name'] = $module_fields['modified_user_id'];
            $module_fields['modified_by_name']['name'] = 'modified_by_name';
        }
        if (isset($module_fields['created_by'])) {
            $module_fields['created_by_name'] = $module_fields['created_by'];
            $module_fields['created_by_name']['name'] = 'created_by_name';
        }

        $return = array('module_fields' => $module_fields, 'link_fields' => $link_fields);
        $GLOBALS['log']->info('End: SoapHelperWebServices->get_field_list ->> ' . print_r($return, true));

        return $return;
    } // fn

    public function setFaultObject($errorObject)
    {
        if ($this->isLogLevelDebug()) {
            $GLOBALS['log']->debug('SoapHelperWebServices->setFaultObject - ' . var_export($errorObject, true));
        }
        global $service_object;
        $service_object->error($errorObject);
    } // fn

    /**
     * Validate the user session based on user name and password hash.
     *
     * @param string $user_name -- The user name to create a session for
     * @param string $password -- The MD5 sum of the user's password
     * @return true -- If the session is created
     * @return false -- If the session is not created
     */
    public function validate_user($user_name, $password)
    {
        $GLOBALS['log']->info('Begin: SoapHelperWebServices->validate_user');
        global $server, $current_user, $sugar_config, $system_config;
        $user = BeanFactory::newBean('Users');
        $user->user_name = $user_name;
        $system_config = BeanFactory::newBean('Administration');
        $system_config->retrieveSettings('system');
        $authController = new AuthenticationController();
        // Check to see if the user name and password are consistent.
        if ($user->authenticate_user($password)) {
            // we also need to set the current_user.
            $user->retrieve($user->id);
            $current_user = $user;

            $GLOBALS['log']->info('End: SoapHelperWebServices->validate_user - validation passed');

            return true;
        } elseif (function_exists('openssl_decrypt')) {
            $password = $this->decrypt_string($password);
            if ($authController->login($user_name, $password) && isset($_SESSION['authenticated_user_id'])) {
                $user->retrieve($_SESSION['authenticated_user_id']);
                $current_user = $user;
                $GLOBALS['log']->info('End: SoapHelperWebServices->validate_user - validation passed');

                return true;
            }
        } else {
            $GLOBALS['log']->fatal("SECURITY: failed attempted login for $user_name using SOAP api");
            $server->setError("Invalid username and/or password");

            return false;
        }
    }

    /**
     * Validate the provided session information is correct and current.  Load the session.
     *
     * @param String $session_id -- The session ID that was returned by a call to login.
     * @return true -- If the session is valid and loaded.
     * @return false -- if the session is not valid.
     */
    public function validate_authenticated($session_id)
    {
        $GLOBALS['log']->info('Begin: SoapHelperWebServices->validate_authenticated');
        if (!empty($session_id)) {

            // only initialize session once in case this method is called multiple times
            if (!session_id()) {
                session_id($session_id);
                session_start();
            }

            if (!empty($_SESSION['is_valid_session']) && $this->is_valid_ip_address('ip_address') && $_SESSION['type'] == 'user') {
                global $current_user;
                require_once('modules/Users/User.php');
                $current_user = BeanFactory::newBean('Users');
                $current_user->retrieve($_SESSION['user_id']);
                $this->login_success();
                $GLOBALS['log']->info('Begin: SoapHelperWebServices->validate_authenticated - passed');
                $GLOBALS['log']->info('End: SoapHelperWebServices->validate_authenticated');

                return true;
            }

            $GLOBALS['log']->debug("calling destroy");
            session_destroy();
        }
        LogicHook::initialize();
        $GLOBALS['logic_hook']->call_custom_logic('Users', 'login_failed');
        $GLOBALS['log']->info('End: SoapHelperWebServices->validate_authenticated - validation failed');

        return false;
    }

    /**
     * Use the same logic as in SugarAuthenticate to validate the ip address
     *
     * @param string $session_var
     * @return bool - true if the ip address is valid, false otherwise.
     */
    public function is_valid_ip_address($session_var)
    {
        global $sugar_config;
        // grab client ip address
        $clientIP = query_client_ip();
        $classCheck = 0;
        // check to see if config entry is present, if not, verify client ip
        if (!isset($sugar_config['verify_client_ip']) || $sugar_config['verify_client_ip'] == true) {
            // check to see if we've got a current ip address in $_SESSION
            // and check to see if the session has been hijacked by a foreign ip
            if (isset($_SESSION[$session_var])) {
                $session_parts = explode(".", $_SESSION[$session_var]);
                $client_parts = explode(".", $clientIP);
                if (count($session_parts) < 4) {
                    $classCheck = 0;
                } else {
                    // match class C IP addresses
                    for ($i = 0; $i < 3; $i++) {
                        if ($session_parts[$i] === $client_parts[$i]) {
                            $classCheck = 1;
                            continue;
                        }
                        $classCheck = 0;
                        break;
                    }
                }
                // we have a different IP address
                if ($_SESSION[$session_var] != $clientIP && empty($classCheck)) {
                    $GLOBALS['log']->fatal("IP Address mismatch: SESSION IP: {$_SESSION[$session_var]} CLIENT IP: {$clientIP}");

                    return false;
                }
            } else {
                return false;
            }
        }

        return true;
    }

    public function checkSessionAndModuleAccess(
        $session,
        $login_error_key,
        $module_name,
        $access_level,
        $module_access_level_error_key,
        $errorObject
    ) {
        $GLOBALS['log']->info('Begin: SoapHelperWebServices->checkSessionAndModuleAccess - ' . $module_name);
        if (!$this->validate_authenticated($session)) {
            $GLOBALS['log']->error('SoapHelperWebServices->checkSessionAndModuleAccess - validate_authenticated failed - ' . $module_name);
            $errorObject->set_error('invalid_session');
            $this->setFaultObject($errorObject);
            $GLOBALS['log']->info('End: SoapHelperWebServices->checkSessionAndModuleAccess -' . $module_name);

            return false;
        } // if

        global $beanList, $beanFiles;
        if (!empty($module_name)) {
            if (empty($beanList[$module_name])) {
                $GLOBALS['log']->error('SoapHelperWebServices->checkSessionAndModuleAccess - module does not exists - ' . $module_name);
                $errorObject->set_error('no_module');
                $this->setFaultObject($errorObject);
                $GLOBALS['log']->info('End: SoapHelperWebServices->checkSessionAndModuleAccess -' . $module_name);

                return false;
            } // if
            global $current_user;
            if (!$this->check_modules_access($current_user, $module_name, $access_level)) {
                $GLOBALS['log']->error('SoapHelperWebServices->checkSessionAndModuleAccess - no module access - ' . $module_name);
                $errorObject->set_error('no_access');
                $this->setFaultObject($errorObject);
                $GLOBALS['log']->info('End: SoapHelperWebServices->checkSessionAndModuleAccess - ' . $module_name);

                return false;
            }
        } // if
        $GLOBALS['log']->info('End: SoapHelperWebServices->checkSessionAndModuleAccess - ' . $module_name);

        return true;
    } // fn

    public function checkACLAccess($bean, $viewType, $errorObject, $error_key)
    {
        $GLOBALS['log']->info('Begin: SoapHelperWebServices->checkACLAccess');
        if (!$bean->ACLAccess($viewType)) {
            $GLOBALS['log']->error('SoapHelperWebServices->checkACLAccess - no ACLAccess');
            $errorObject->set_error($error_key);
            $this->setFaultObject($errorObject);
            $GLOBALS['log']->info('End: SoapHelperWebServices->checkACLAccess');

            return false;
        } // if
        $GLOBALS['log']->info('End: SoapHelperWebServices->checkACLAccess');

        return true;
    } // fn

    public function checkQuery($errorObject, $query, $order_by = '')
    {
        require_once 'include/SugarSQLValidate.php';
        $valid = new SugarSQLValidate();
        if (!$valid->validateQueryClauses($query, $order_by)) {
            $GLOBALS['log']->error("SoapHelperWebServices->checkQuery - bad query: $query $order_by");
            $errorObject->set_error('no_access');
            $this->setFaultObject($errorObject);

            return false;
        }

        return true;
    }

    public function get_name_value($field, $value)
    {
        return array('name' => $field, 'value' => $value);
    }

    public function get_user_module_list($user)
    {
        $GLOBALS['log']->info('Begin: SoapHelperWebServices->get_user_module_list');
        global $app_list_strings, $current_language;
        $app_list_strings = return_app_list_strings_language($current_language);
        $modules = query_module_access_list($user);
        ACLController:: filterModuleList($modules, false);
        global $modInvisList;

        foreach ($modInvisList as $invis) {
            $modules[$invis] = 'read_only';
        }

        $actions = ACLAction::getUserActions($user->id, true);
        foreach ($actions as $key => $value) {
            if (isset($value['module']) && $value['module']['access']['aclaccess'] < ACL_ALLOW_ENABLED) {
                if (!is_admin($user) && $value['module']['access']['aclaccess'] == ACL_ALLOW_DISABLED) {
                    unset($modules[$key]);
                } else {
                    $modules[$key] = 'read_only';
                } // else
            } else {
                $modules[$key] = '';
            } // else
        } // foreach
        $GLOBALS['log']->info('End: SoapHelperWebServices->get_user_module_list');

        return $modules;
    }

    public function check_modules_access($user, $module_name, $action = 'write')
    {
        $GLOBALS['log']->info('Begin: SoapHelperWebServices->check_modules_access');
        if (!isset($_SESSION['avail_modules'])) {
            $_SESSION['avail_modules'] = $this->get_user_module_list($user);
        }
        if (isset($_SESSION['avail_modules'][$module_name])) {
            if ($action == 'write' && $_SESSION['avail_modules'][$module_name] == 'read_only') {
                if (is_admin($user)) {
                    $GLOBALS['log']->info('End: SoapHelperWebServices->check_modules_access - SUCCESS: Admin can even write to read_only module');

                    return true;
                } // if
                $GLOBALS['log']->info('End: SoapHelperWebServices->check_modules_access - FAILED: write action on read_only module only available to admins');

                return false;
            } elseif ($action == 'write' && strcmp(
                strtolower($module_name),
                'users'
            ) == 0 && !$user->isAdminForModule($module_name)
            ) {
                //rrs bug: 46000 - If the client is trying to write to the Users module and is not an admin then we need to stop them
                return false;
            }
            $GLOBALS['log']->info('End: SoapHelperWebServices->check_modules_access - SUCCESS');

            return true;
        }
        $GLOBALS['log']->info('End: SoapHelperWebServices->check_modules_access - FAILED: Module info not available in $_SESSION');

        return false;
    }


    public function get_name_value_list($value)
    {
        $GLOBALS['log']->info('Begin: SoapHelperWebServices->get_name_value_list');
        global $app_list_strings;
        $list = array();
        if (!empty($value->field_defs)) {
            if (isset($value->assigned_user_name)) {
                $list['assigned_user_name'] = $this->get_name_value('assigned_user_name', $value->assigned_user_name);
            }
            if (isset($value->modified_by_name)) {
                $list['modified_by_name'] = $this->get_name_value('modified_by_name', $value->modified_by_name);
            }
            if (isset($value->created_by_name)) {
                $list['created_by_name'] = $this->get_name_value('created_by_name', $value->created_by_name);
            }
            foreach ($value->field_defs as $var) {
                if (isset($var['source']) && ($var['source'] != 'db' && $var['source'] != 'custom_fields') && $var['name'] != 'email1' && $var['name'] != 'email2' && (!isset($var['type']) || $var['type'] != 'relate')) {
                    continue;
                }

                if (isset($value->{$var['name']})) {
                    $val = $value->{$var['name']};
                    $type = $var['type'];

                    if (strcmp($type, 'date') == 0) {
                        $val = substr((string) $val, 0, 10);
                    } elseif (strcmp($type, 'enum') == 0 && !empty($var['options'])) {
                        //$val = $app_list_strings[$var['options']][$val];
                    }

                    $list[$var['name']] = $this->get_name_value($var['name'], $val);
                }
            }
        }
        $GLOBALS['log']->info('End: SoapHelperWebServices->get_name_value_list');

        return $list;
    }

    public function filter_fields($value, $fields)
    {
        $GLOBALS['log']->info('Begin: SoapHelperWebServices->filter_fields');
        global $invalid_contact_fields;
        $filterFields = array();
        foreach ($fields as $field) {
            if (is_array($invalid_contact_fields)) {
                if (in_array($field, $invalid_contact_fields)) {
                    continue;
                } // if
            } // if
            if (isset($value->field_defs[$field])) {
                $var = $value->field_defs[$field];
                if (isset($var['source']) && ($var['source'] != 'db' && $var['source'] != 'custom_fields') && $var['name'] != 'email1' && $var['name'] != 'email2' && (!isset($var['type']) || $var['type'] != 'relate')) {
                    if ($value->module_dir == 'Emails' && (($var['name'] == 'description') || ($var['name'] == 'description_html') || ($var['name'] == 'from_addr_name') || ($var['name'] == 'reply_to_addr') || ($var['name'] == 'to_addrs_names') || ($var['name'] == 'cc_addrs_names') || ($var['name'] == 'bcc_addrs_names') || ($var['name'] == 'raw_source'))) {
                    } else {
                        continue;
                    }
                }
            } // if
            $filterFields[] = $field;
        } // foreach
        $GLOBALS['log']->info('End: SoapHelperWebServices->filter_fields');

        return $filterFields;
    } // fn

    public function get_name_value_list_for_fields($value, $fields)
    {
        $GLOBALS['log']->info('Begin: SoapHelperWebServices->get_name_value_list_for_fields');
        global $app_list_strings;
        global $invalid_contact_fields;

        $list = array();
        if (!empty($value->field_defs)) {
            if (empty($fields)) {
                $fields = array_keys($value->field_defs);
            }
            if (isset($value->assigned_user_name) && in_array('assigned_user_name', $fields)) {
                $list['assigned_user_name'] = $this->get_name_value('assigned_user_name', $value->assigned_user_name);
            }
            if (isset($value->modified_by_name) && in_array('modified_by_name', $fields)) {
                $list['modified_by_name'] = $this->get_name_value('modified_by_name', $value->modified_by_name);
            }
            if (isset($value->created_by_name) && in_array('created_by_name', $fields)) {
                $list['created_by_name'] = $this->get_name_value('created_by_name', $value->created_by_name);
            }

            $filterFields = $this->filter_fields($value, $fields);


            foreach ($filterFields as $field) {
                $var = $value->field_defs[$field];
                if (isset($value->{$var['name']})) {
                    $val = $value->{$var['name']};
                    $type = $var['type'];

                    if (strcmp($type, 'date') == 0) {
                        $val = substr((string) $val, 0, 10);
                    } elseif (strcmp($type, 'enum') == 0 && !empty($var['options'])) {
                        //$val = $app_list_strings[$var['options']][$val];
                    }

                    $list[$var['name']] = $this->get_name_value($var['name'], $val);
                } // if
            } // foreach
        } // if
        $GLOBALS['log']->info('End: SoapHelperWebServices->get_name_value_list_for_fields');
        if ($this->isLogLevelDebug()) {
            $GLOBALS['log']->debug('SoapHelperWebServices->get_name_value_list_for_fields - return data = ' . var_export(
                $list,
                true
            ));
        } // if

        return $list;
    } // fn

    public function array_get_name_value_list($array)
    {
        $GLOBALS['log']->info('Begin: SoapHelperWebServices->array_get_name_value_list');
        $list = array();
        foreach ($array as $name => $value) {
            $list[$name] = $this->get_name_value($name, $value);
        }
        $GLOBALS['log']->info('End: SoapHelperWebServices->array_get_name_value_list');

        return $list;
    }

    public function array_get_name_value_lists($array)
    {
        $GLOBALS['log']->info('Begin: SoapHelperWebServices->array_get_name_value_lists');
        $list = array();
        foreach ($array as $name => $value) {
            $tmp_value = $value;
            if (is_array($value)) {
                $tmp_value = array();
                foreach ($value as $k => $v) {
                    $tmp_value[$k] = $this->get_name_value($k, $v);
                }
            }
            $list[$name] = $this->get_name_value($name, $tmp_value);
        }
        $GLOBALS['log']->info('End: SoapHelperWebServices->array_get_name_value_lists');

        return $list;
    }

    public function name_value_lists_get_array($list)
    {
        $GLOBALS['log']->info('Begin: SoapHelperWebServices->name_value_lists_get_array');
        $array = array();
        foreach ($list as $key => $value) {
            if (isset($value['value']) && isset($value['name'])) {
                if (is_array($value['value'])) {
                    $array[$value['name']] = array();
                    foreach ($value['value'] as $v) {
                        $array[$value['name']][$v['name']] = $v['value'];
                    }
                } else {
                    $array[$value['name']] = $value['value'];
                }
            }
        }
        $GLOBALS['log']->info('End: SoapHelperWebServices->name_value_lists_get_array');

        return $array;
    }

    public function array_get_return_value($array, $module)
    {
        $GLOBALS['log']->info('Begin/End: SoapHelperWebServices->array_get_return_value');

        return array(
            'id' => $array['id'],
            'module_name' => $module,
            'name_value_list' => $this->array_get_name_value_list($array)
        );
    }

    public function get_return_value_for_fields($value, $module, $fields)
    {
        $GLOBALS['log']->info('Begin: SoapHelperWebServices->get_return_value_for_fields');
        global $module_name, $current_user;
        $module_name = $module;
        if ($module == 'Users' && $value->id != $current_user->id) {
            $value->user_hash = '';
        }
        $value = clean_sensitive_data($value->field_defs, $value);
        $GLOBALS['log']->info('End: SoapHelperWebServices->get_return_value_for_fields');

        return array(
            'id' => $value->id,
            'module_name' => $module,
            'name_value_list' => $this->get_name_value_list_for_fields($value, $fields)
        );
    }

    /**
     * Fetch and array of related records
     *
     * @param String $bean -- Primary record
     * @param String $link_field_name -- The name of the relationship
     * @param Array $link_module_fields -- The keys of the array are the SugarBean attributes, the values of the array are the values the attributes should have.
     * @param String $optional_where -- IGNORED
     * @return Array 'rows/fields_set_on_rows' -- The list of records and what fields were actually set for thos erecords
     */

    public function getRelationshipResults($bean, $link_field_name, $link_module_fields, $optional_where = '')
    {
        $GLOBALS['log']->info('Begin: SoapHelperWebServices->getRelationshipResults');
        global $current_user, $disable_date_format, $timedate;

        $bean->load_relationship($link_field_name);
        if (isset($bean->$link_field_name)) {
            $params = array();
            if (!empty($optional_where)) {
                $params['where'] = $optional_where;
            }
            //First get all the related beans
            $related_beans = $bean->$link_field_name->getBeans($params);
            if (isset($related_beans[0])) {
                // use first bean to filter fields since all records have same module
                // and  $this->filter_fields doesn't use ACLs
                $filterFields = $this->filter_fields($related_beans[0], $link_module_fields);
            } else {
                $filterFields = $this->filter_fields(null, $link_module_fields);
            }
            $list = array();
            foreach ($related_beans as $id => $bean) {
                $row = array();
                //Create a list of field/value rows based on $link_module_fields

                foreach ($filterFields as $field) {
                    if (isset($bean->$field)) {
                        if (isset($bean->field_defs[$field]['type']) && $bean->field_defs[$field]['type'] == 'date') {
                            $row[$field] = $timedate->to_display_date_time($bean->$field);
                        }
                        $row[$field] = $bean->$field;
                    } else {
                        $row[$field] = "";
                    }
                }
                //Users can't see other user's hashes
                if (is_a($bean, 'User') && $current_user->id != $bean->id && isset($row['user_hash'])) {
                    $row['user_hash'] = "";
                }
                $row = clean_sensitive_data($bean->field_defs, $row);
                $list[] = $row;
            }
            $GLOBALS['log']->info('End: SoapHelperWebServices->getRelationshipResults');

            return array('rows' => $list, 'fields_set_on_rows' => $filterFields);
        }
        $GLOBALS['log']->info('End: SoapHelperWebServices->getRelationshipResults - ' . $link_field_name . ' relationship does not exists');

        return false;
        // else
    } // fn

    public function get_return_value_for_link_fields($bean, $module, $link_name_to_value_fields_array)
    {
        $GLOBALS['log']->info('Begin: SoapHelperWebServices->get_return_value_for_link_fields');
        global $module_name, $current_user;
        $module_name = $module;
        if ($module == 'Users' && $bean->id != $current_user->id) {
            $bean->user_hash = '';
        }
        $bean = clean_sensitive_data($bean->field_defs, $bean);

        if (empty($link_name_to_value_fields_array) || !is_array($link_name_to_value_fields_array)) {
            $GLOBALS['log']->debug('End: SoapHelperWebServices->get_return_value_for_link_fields - Invalid link information passed ');

            return array();
        }

        if ($this->isLogLevelDebug()) {
            $GLOBALS['log']->debug('SoapHelperWebServices->get_return_value_for_link_fields - link info = ' . var_export(
                $link_name_to_value_fields_array,
                true
            ));
        } // if
        $link_output = array();
        foreach ($link_name_to_value_fields_array as $link_name_value_fields) {
            if (!is_array($link_name_value_fields) || !isset($link_name_value_fields['name']) || !isset($link_name_value_fields['value'])) {
                continue;
            }
            $link_field_name = $link_name_value_fields['name'];
            $link_module_fields = $link_name_value_fields['value'];
            if (is_array($link_module_fields) && !empty($link_module_fields)) {
                $result = $this->getRelationshipResults($bean, $link_field_name, $link_module_fields);
                if (!$result) {
                    $link_output[] = array('name' => $link_field_name, 'records' => array());
                    continue;
                }
                $list = $result['rows'];
                $filterFields = $result['fields_set_on_rows'];
                if ($list) {
                    $rowArray = array();
                    foreach ($list as $row) {
                        $nameValueArray = array();
                        foreach ($filterFields as $field) {
                            $nameValue = array();
                            if (isset($row[$field])) {
                                $nameValueArray[$field] = $this->get_name_value($field, $row[$field]);
                            } // if
                        } // foreach
                        $rowArray[] = $nameValueArray;
                    } // foreach
                    $link_output[] = array('name' => $link_field_name, 'records' => $rowArray);
                } // if
            } // if
        } // foreach
        $GLOBALS['log']->debug('End: SoapHelperWebServices->get_return_value_for_link_fields');
        if ($this->isLogLevelDebug()) {
            $GLOBALS['log']->debug('SoapHelperWebServices->get_return_value_for_link_fields - output = ' . var_export(
                $link_output,
                true
            ));
        } // if

        return $link_output;
    } // fn

    /**
     *
     * @param String $module_name -- The name of the module that the primary record is from.  This name should be the name the module was developed under (changing a tab name is studio does not affect the name that should be passed into this method).
     * @param String $module_id -- The ID of the bean in the specified module
     * @param String $link_field_name - The relationship name for which to create realtionships.
     * @param Array $related_ids -- The array of ids for which we want to create relationships
     * @param Array $name_value_list -- The array of name value pair of additional attributes to be set when adding this relationship
     * @param int delete -- If 0 then add relationship else delete this relationship data
     * @return true on success, false on failure
     */
    public function new_handle_set_relationship(
        $module_name,
        $module_id,
        $link_field_name,
        $related_ids,
        $name_value_list,
        $delete
    ) {
        $GLOBALS['log']->info('Begin: SoapHelperWebServices->new_handle_set_relationship');
        global $beanList, $beanFiles;

        if (empty($beanList[$module_name])) {
            $GLOBALS['log']->debug('SoapHelperWebServices->new_handle_set_relationship - module ' . $module_name . ' does not exists');
            $GLOBALS['log']->info('End: SoapHelperWebServices->new_handle_set_relationship');

            return false;
        } // if
        $class_name = $beanList[$module_name];
        require_once($beanFiles[$class_name]);
        $mod = new $class_name();
        $mod->retrieve($module_id);
        if (!$mod->ACLAccess('DetailView')) {
            $GLOBALS['log']->info('End: SoapHelperWebServices->new_handle_set_relationship');

            return false;
        }

        if ($mod->load_relationship($link_field_name)) {
            if (!$delete) {
                $name_value_pair = array();
                if (!empty($name_value_list)) {
                    $relFields = $mod->$link_field_name->getRelatedFields();
                    if (!empty($relFields)) {
                        $relFieldsKeys = array_keys($relFields);
                        foreach ($name_value_list as $key => $value) {
                            if (in_array($value['name'], $relFieldsKeys)) {
                                $name_value_pair[$value['name']] = $value['value'];
                            } // if
                        } // foreach
                    } // if
                }
                $mod->$link_field_name->add($related_ids, $name_value_pair);
            } else {
                foreach ($related_ids as $id) {
                    $mod->$link_field_name->delete($module_id, $id);
                } // foreach
            } // else
            $GLOBALS['log']->info('End: SoapHelperWebServices->new_handle_set_relationship');

            return true;
        }
        $GLOBALS['log']->info('End: SoapHelperWebServices->new_handle_set_relationship');

        return false;
    }

    public function new_handle_set_entries($module_name, $name_value_lists, $select_fields = false)
    {
        $GLOBALS['log']->info('Begin: SoapHelperWebServices->new_handle_set_entries');
        global $beanList, $beanFiles, $current_user, $app_list_strings;

        $ret_values = array();

        $class_name = $beanList[$module_name];
        require_once($beanFiles[$class_name]);
        $ids = array();
        $count = 1;
        $total = is_countable($name_value_lists) ? count($name_value_lists) : 0;
        foreach ($name_value_lists as $name_value_list) {
            $seed = new $class_name();

            $seed->update_vcal = false;
            foreach ($name_value_list as $value) {
                if ($value['name'] == 'id') {
                    $seed->retrieve($value['value']);
                    break;
                }
            }

            foreach ($name_value_list as $value) {
                $val = $value['value'];
                if ($seed->field_name_map[$value['name']]['type'] == 'enum') {
                    $vardef = $seed->field_name_map[$value['name']];
                    if (isset($app_list_strings[$vardef['options']]) && !isset($app_list_strings[$vardef['options']][$value])) {
                        if (in_array($val, $app_list_strings[$vardef['options']])) {
                            $val = array_search($val, $app_list_strings[$vardef['options']], true);
                        }
                    }
                }
                if ($module_name == 'Users' && !empty($seed->id) && ($seed->id != $current_user->id) && $value['name'] == 'user_hash') {
                    continue;
                }
                if (!empty($seed->field_name_map[$value['name']]['sensitive'])) {
                    continue;
                }
                $seed->{$value['name']} = $val;
            }

            if ($count == $total) {
                $seed->update_vcal = false;
            }
            $count++;

            //Add the account to a contact
            if ($module_name == 'Contacts') {
                $GLOBALS['log']->debug('Creating Contact Account');
                $this->add_create_account($seed);
                $duplicate_id = $this->check_for_duplicate_contacts($seed);
                if ($duplicate_id == null) {
                    if ($seed->ACLAccess('Save') && ($seed->deleted != 1 || $seed->ACLAccess('Delete'))) {
                        $seed->save();
                        if ($seed->deleted == 1) {
                            $seed->mark_deleted($seed->id);
                        }
                        $ids[] = $seed->id;
                    }
                } else {
                    //since we found a duplicate we should set the sync flag
                    if ($seed->ACLAccess('Save')) {
                        $seed = new $class_name();
                        $seed->id = $duplicate_id;
                        $seed->contacts_users_id = $current_user->id;
                        $seed->save();
                        $ids[] = $duplicate_id;//we have a conflict
                    }
                }
            } elseif ($module_name == 'Meetings' || $module_name == 'Calls') {
                //we are going to check if we have a meeting in the system
                //with the same outlook_id. If we do find one then we will grab that
                //id and save it
                if ($seed->ACLAccess('Save') && ($seed->deleted != 1 || $seed->ACLAccess('Delete'))) {
                    if (empty($seed->id) && !isset($seed->id)) {
                        if (!empty($seed->outlook_id) && isset($seed->outlook_id)) {
                            //at this point we have an object that does not have
                            //the id set, but does have the outlook_id set
                            //so we need to query the db to find if we already
                            //have an object with this outlook_id, if we do
                            //then we can set the id, otherwise this is a new object
                            $order_by = "";
                            $query = $seed->table_name . ".outlook_id = '" . DBManagerFactory::getInstance()->quote($seed->outlook_id) . "'";
                            $response = $seed->get_list($order_by, $query, 0, -1, -1, 0);
                            $list = $response['list'];
                            if ((is_countable($list) ? count($list) : 0) > 0) {
                                foreach ($list as $value) {
                                    $seed->id = $value->id;
                                    break;
                                }
                            }//fi
                        }//fi
                    }//fi
                    if (empty($seed->reminder_time)) {
                        $seed->reminder_time = -1;
                    }
                    if ($seed->reminder_time == -1) {
                        $defaultRemindrTime = $current_user->getPreference('reminder_time');
                        if ($defaultRemindrTime != -1) {
                            $seed->reminder_checked = '1';
                            $seed->reminder_time = $defaultRemindrTime;
                        }
                    }
                    $seed->save();
                    if ($seed->deleted == 1) {
                        $seed->mark_deleted($seed->id);
                    }
                    $ids[] = $seed->id;
                }//fi
            } else {
                if ($seed->ACLAccess('Save') && ($seed->deleted != 1 || $seed->ACLAccess('Delete'))) {
                    $seed->save();
                    $ids[] = $seed->id;
                }
            }

            // if somebody is calling set_entries_detail() and wants fields returned...
            if ($select_fields !== false) {
                $ret_values[$count] = array();

                foreach ($select_fields as $select_field) {
                    if (isset($seed->$select_field)) {
                        $ret_values[$count][$select_field] = $this->get_name_value($select_field, $seed->$select_field);
                    }
                }
            }
        }

        // handle returns for set_entries_detail() and set_entries()
        if ($select_fields !== false) {
            $GLOBALS['log']->info('End: SoapHelperWebServices->new_handle_set_entries');

            return array(
                'name_value_lists' => $ret_values,
            );
        }
        $GLOBALS['log']->info('End: SoapHelperWebServices->new_handle_set_entries');

        return array(
                'ids' => $ids,
            );
    }

    public function get_return_value($value, $module)
    {
        $GLOBALS['log']->info('Begin: SoapHelperWebServices->get_return_value');
        global $module_name, $current_user;
        $module_name = $module;
        if ($module == 'Users' && $value->id != $current_user->id) {
            $value->user_hash = '';
        }
        $value = clean_sensitive_data($value->field_defs, $value);
        $GLOBALS['log']->info('End: SoapHelperWebServices->new_handle_set_entries');

        return array(
            'id' => $value->id,
            'module_name' => $module,
            'name_value_list' => $this->get_name_value_list($value)
        );
    }


    public function get_return_module_fields($value, $module, $fields, $translate = true)
    {
        $GLOBALS['log']->info('Begin: SoapHelperWebServices->get_return_module_fields');
        global $module_name;
        $module_name = $module;
        $result = $this->get_field_list($value, $fields, $translate);
        $GLOBALS['log']->info('End: SoapHelperWebServices->get_return_module_fields');

        return array(
            'module_name' => $module,
            'module_fields' => $result['module_fields'],
            'link_fields' => $result['link_fields'],
        );
    } // fn

    public function login_success($name_value_list = array())
    {
        $GLOBALS['log']->info('Begin: SoapHelperWebServices->login_success');
        global $current_language, $sugar_config, $app_strings, $app_list_strings;
        $current_language = $sugar_config['default_language'];
        if (is_array($name_value_list) && !empty($name_value_list)) {
            foreach ($name_value_list as $key => $value) {
                if (isset($value['name']) && ($value['name'] == 'language')) {
                    $language = $value['value'];
                    $supportedLanguages = $sugar_config['languages'];
                    if (array_key_exists($language, $supportedLanguages)) {
                        $current_language = $language;
                    } // if
                } // if
                if (isset($value['name']) && ($value['name'] == 'notifyonsave')) {
                    if ($value['value']) {
                        $_SESSION['notifyonsave'] = true;
                    }
                } // if
            } // foreach
        } else {
            if (isset($_SESSION['user_language'])) {
                $current_language = $_SESSION['user_language'];
            } // if
        }
        $GLOBALS['log']->info("Users language is = " . $current_language);
        $app_strings = return_application_language($current_language);
        $app_list_strings = return_app_list_strings_language($current_language);
        $GLOBALS['log']->info('End: SoapHelperWebServices->login_success');
    } // fn


    public function checkSaveOnNotify()
    {
        $notifyonsave = false;
        if (isset($_SESSION['notifyonsave']) && $_SESSION['notifyonsave'] == true) {
            $notifyonsave = true;
        } // if

        return $notifyonsave;
    }

    /*
     *	Given an account_name, either create the account or assign to a contact.
     */
    public function add_create_account($seed)
    {
        $GLOBALS['log']->info('Begin: SoapHelperWebServices->add_create_account');
        global $current_user;
        $account_name = $seed->account_name;
        $account_id = $seed->account_id;
        $assigned_user_id = $current_user->id;

        // check if it already exists
        $focus = BeanFactory::newBean('Accounts');
        if ($focus->ACLAccess('Save')) {
            $class = get_class($seed);
            $temp = new $class();
            $temp->retrieve($seed->id);
            if (empty($account_name) && empty($account_id)) {
                return;
            } // if
            if (!isset($seed->accounts)) {
                $seed->load_relationship('accounts');
            } // if

            if ($seed->account_name == '' && isset($temp->account_id)) {
                $seed->accounts->delete($seed->id, $temp->account_id);
                $GLOBALS['log']->info('End: SoapHelperWebServices->add_create_account');

                return;
            }
            $arr = array();

            if (!empty($account_id)) {  // bug # 44280
                $query = "select id, deleted from {$focus->table_name} WHERE id='" . $seed->db->quote($account_id) . "'";
            } else {
                $query = "select id, deleted from {$focus->table_name} WHERE name='" . $seed->db->quote($account_name) . "'";
            }
            $result = $seed->db->query($query, true);

            $row = $seed->db->fetchByAssoc($result, false);

            // we found a row with that id
            if (isset($row['id']) && $row['id'] != -1) {
                // if it exists but was deleted, just remove it entirely
                if (isset($row['deleted']) && $row['deleted'] == 1) {
                    $query2 = "delete from {$focus->table_name} WHERE id='" . $seed->db->quote($row['id']) . "'";
                    $result2 = $seed->db->query($query2, true);
                } // else just use this id to link the contact to the account
                else {
                    $focus->id = $row['id'];
                }
            }

            // if we didnt find the account, so create it
            if (!isset($focus->id) || $focus->id == '') {
                $focus->name = $account_name;

                if (isset($assigned_user_id)) {
                    $focus->assigned_user_id = $assigned_user_id;
                    $focus->modified_user_id = $assigned_user_id;
                }
                $focus->save();
            }

            if ($seed->accounts != null && $temp->account_id != null && $temp->account_id != $focus->id) {
                $seed->accounts->delete($seed->id, $temp->account_id);
            }

            if (isset($focus->id) && $focus->id != '') {
                $seed->account_id = $focus->id;
            } // if
            $GLOBALS['log']->info('End: SoapHelperWebServices->add_create_account');
        } else {
            $GLOBALS['log']->info('End: SoapHelperWebServices->add_create_account - Insufficient ACLAccess');
        } // else
    } // fn

    public function check_for_duplicate_contacts($seed)
    {
        $GLOBALS['log']->info('Begin: SoapHelperWebServices->check_for_duplicate_contacts');
        require_once('modules/Contacts/Contact.php');

        if (isset($seed->id)) {
            $GLOBALS['log']->info('End: SoapHelperWebServices->check_for_duplicate_contacts - no duplicte found');

            return null;
        }

        $query = '';

        $trimmed_email = trim($seed->email1);
        $trimmed_email2 = trim($seed->email2);
        $trimmed_last = trim($seed->last_name);
        $trimmed_first = trim($seed->first_name);
        if (!empty($trimmed_email) || !empty($trimmed_email2)) {

            //obtain a list of contacts which contain the same email address
            $contacts = $seed->emailAddress->getBeansByEmailAddress($trimmed_email);
            $contacts2 = $seed->emailAddress->getBeansByEmailAddress($trimmed_email2);
            $contacts = array_merge($contacts, $contacts2);
            if (count($contacts) == 0) {
                $GLOBALS['log']->info('End: SoapHelperWebServices->check_for_duplicate_contacts - no duplicte found');

                return null;
            }
            foreach ($contacts as $contact) {
                if (!empty($trimmed_last) && strcmp($trimmed_last, $contact->last_name) == 0) {
                    if ((!empty($trimmed_email) || !empty($trimmed_email2)) && (strcmp(
                        $trimmed_email,
                        $contact->email1
                        ) == 0 || strcmp(
                            $trimmed_email,
                            $contact->email2
                                    ) == 0 || strcmp(
                                        $trimmed_email2,
                                        $contact->email
                                    ) == 0 || strcmp($trimmed_email2, $contact->email2) == 0)
                        ) {
                        $contact->load_relationship('accounts');
                        if (empty($seed->account_name) || strcmp(
                            $seed->account_name,
                            $contact->account_name
                            ) == 0
                            ) {
                            $GLOBALS['log']->info('End: SoapHelperWebServices->check_for_duplicate_contacts - duplicte found ' . $contact->id);

                            return $contact->id;
                        }
                    }
                }
            }
            $GLOBALS['log']->info('End: SoapHelperWebServices->check_for_duplicate_contacts - no duplicte found');

            return null;
        }
        $GLOBALS['log']->info('End: SoapHelperWebServices->check_for_duplicate_contacts - no duplicte found');


        return null;
    }


    /**
     * decrypt a string use the TripleDES algorithm. This meant to be
     * modified if the end user chooses a different algorithm
     *
     * @param $string - the string to decrypt
     *
     * @return a decrypted string if we can decrypt, the original string otherwise
     */
    public function decrypt_string($string)
    {
        $GLOBALS['log']->info('Begin: SoapHelperWebServices->decrypt_string');
        if (function_exists('openssl_decrypt')) {
            require_once('modules/Administration/Administration.php');
            $focus = BeanFactory::newBean('Administration');
            $focus->retrieveSettings();
            $key = '';
            if (!empty($focus->settings['ldap_enc_key'])) {
                $key = $focus->settings['ldap_enc_key'];
            }
            if (empty($key)) {
                $GLOBALS['log']->info('End: SoapHelperWebServices->decrypt_string - empty key');
                return $string;
            } // if
            $buffer = $string;
            $key = substr(md5($key), 0, 24);
            $iv = "password";
            $GLOBALS['log']->info('End: SoapHelperWebServices->decrypt_string');
            $decrypted = openssl_decrypt(pack("H*", $buffer), 'des-ede3-cbc', $key, OPENSSL_RAW_DATA|OPENSSL_ZERO_PADDING, $iv);
            $decrypted = str_replace("\0", "", $decrypted);
            return $decrypted;
        }
        $GLOBALS['log']->info('End: SoapHelperWebServices->decrypt_string');
        return $string;
    }

    public function isLogLevelDebug()
    {
        if (isset($GLOBALS['sugar_config']['logger'])) {
            if (isset($GLOBALS['sugar_config']['logger']['level'])) {
                return ($GLOBALS['sugar_config']['logger']['level'] == 'debug');
            } // if
        }

        return false;
    } // fn
} // clazz
