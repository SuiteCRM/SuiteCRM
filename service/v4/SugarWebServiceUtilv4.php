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

require_once('service/v3_1/SugarWebServiceUtilv3_1.php');

class SugarWebServiceUtilv4 extends SugarWebServiceUtilv3_1
{
    public function get_module_view_defs($moduleName, $type, $view)
    {
        $listViewDefs = [];
        $viewdefs = [];
        require_once('include/MVC/View/SugarView.php');
        $metadataFile = null;
        $results = array();
        if (empty($moduleName)) {
            return $results;
        }

        $view = strtolower($view);
        switch (strtolower($type)) {
            case 'default':
            default:
                if ($view == 'subpanel') {
                    $results = $this->get_subpanel_defs($moduleName, $type);
                } else {
                    $v = new SugarView(null, array());
                    $v->module = $moduleName;
                    $v->type = $view;
                    $fullView = ucfirst($view) . 'View';
                    $metadataFile = $v->getMetaDataFile();
                    require_once($metadataFile);
                    if ($view == 'list') {
                        $results = $listViewDefs[$moduleName];
                    } else {
                        $results = $viewdefs[$moduleName][$fullView];
                    }
                }
        }

        //Add field level acls.
        $results = $this->addFieldLevelACLs($moduleName, $type, $view, $results);

        return $results;
    }


    /**
     * Equivalent of get_list function within SugarBean but allows the possibility to pass in an indicator
     * if the list should filter for favorites.  Should eventually update the SugarBean function as well.
     *
     */
    public function get_data_list(
        $seed,
        $order_by = "",
        $where = "",
        $row_offset = 0,
        $limit = -1,
        $max = -1,
        $show_deleted = 0,
        $favorites = false,
        $single_select = false
    ) {
        $GLOBALS['log']->debug("get_list:  order_by = '$order_by' and where = '$where' and limit = '$limit'");
        if (isset($_SESSION['show_deleted'])) {
            $show_deleted = 1;
        }

        $params = array();
        if (!empty($favorites)) {
            $params['favorites'] = true;
        }

        $query = $seed->create_new_list_query(
            $order_by,
            $where,
            array(),
            $params,
            $show_deleted,
            '',
            false,
            null,
            $single_select
        );

        return $seed->process_list_query($query, $row_offset, $limit, $max, $where);
    }

    /**
     * Convert modules list to Web services result
     *
     * @param array $list List of module candidates (only keys are used)
     * @param array $availModules List of module availability from Session
     */
    public function getModulesFromList($list, $availModules)
    {
        global $app_list_strings;
        $enabled_modules = array();
        $availModulesKey = array_flip($availModules);
        foreach ($list as $key=>$value) {
            if (isset($availModulesKey[$key])) {
                $label = !empty($app_list_strings['moduleList'][$key]) ? $app_list_strings['moduleList'][$key] : '';
                $acl = $this->checkModuleRoleAccess($key);
                $fav = $this->is_favorites_enabled($key);
                $enabled_modules[] = array('module_key' => $key,'module_label' => $label, 'favorite_enabled' => $fav, 'acls' => $acl);
            }
        }
        return $enabled_modules;
    }

    /**
     * Return a boolean indicating if the bean name is favorites enabled.
     *
     * @param string The module name
     * @return bool true indicating bean is favorites enabled
     */
    public function is_favorites_enabled($module_name)
    {
        global $beanList, $beanFiles;

        $fav = false;
        return $fav;
    }


    /**
     * Processes the filter_fields attribute to use with SugarBean::create_new_list_query()
     *
     * @param object $value SugarBean
     * @param array $fields
     * @return array
     */
    protected function filter_fields_for_query(SugarBean $value, array $fields)
    {
        $GLOBALS['log']->info('Begin: SoapHelperWebServices->filter_fields_for_query');
        $filterFields = array();
        foreach ($fields as $field) {
            if (isset($value->field_defs[$field])) {
                $filterFields[$field] = $value->field_defs[$field];
            }
        }
        $GLOBALS['log']->info('End: SoapHelperWebServices->filter_fields_for_query');
        return $filterFields;
    }

    public function get_field_list($value, $fields, $translate=true)
    {
        $GLOBALS['log']->info('Begin: SoapHelperWebServices->get_field_list(too large a struct, '.print_r($fields, true).", $translate");
        $module_fields = array();
        $link_fields = array();
        if (!empty($value->field_defs)) {
            foreach ($value->field_defs as $var) {
                if (!empty($fields) && !in_array($var['name'], $fields)) {
                    continue;
                }
                if (isset($var['source']) && ($var['source'] != 'db' && $var['source'] != 'non-db' &&$var['source'] != 'custom_fields') && $var['name'] != 'email1' && $var['name'] != 'email2' && (!isset($var['type'])|| $var['type'] != 'relate')) {
                    continue;
                }
                if ((isset($var['source']) && $var['source'] == 'non_db') && (isset($var['type']) && $var['type'] != 'link')) {
                    continue;
                }
                $required = 0;
                $options_dom = array();
                $options_ret = array();
                // Apparently the only purpose of this check is to make sure we only return fields
                //   when we've read a record.  Otherwise this function is identical to get_module_field_list
                if (isset($var['required']) && ($var['required'] || $var['required'] == 'true')) {
                    $required = 1;
                }

                if ($var['type'] == 'bool') {
                    $var['options'] = 'checkbox_dom';
                }

                if (isset($var['options'])) {
                    $options_dom = translate($var['options'], $value->module_dir);
                    if (!is_array($options_dom)) {
                        $options_dom = array();
                    }
                    foreach ($options_dom as $key=>$oneOption) {
                        $options_ret[$key] = $this->get_name_value($key, $oneOption);
                    }
                }

                if (!empty($var['dbType']) && $var['type'] == 'bool') {
                    $options_ret['type'] = $this->get_name_value('type', $var['dbType']);
                }

                $entry = array();
                $entry['name'] = $var['name'];
                $entry['type'] = $var['type'];
                $entry['group'] = isset($var['group']) ? $var['group'] : '';
                $entry['id_name'] = isset($var['id_name']) ? $var['id_name'] : '';
                if (isset($var['parentenum'])) {
                    $entry['parentenum'] = $var['parentenum'];
                }

                if ($var['type'] == 'link') {
                    $entry['relationship'] = (isset($var['relationship']) ? $var['relationship'] : '');
                    $entry['module'] = (isset($var['module']) ? $var['module'] : '');
                    $entry['bean_name'] = (isset($var['bean_name']) ? $var['bean_name'] : '');
                    $link_fields[$var['name']] = $entry;
                } else {
                    if ($translate) {
                        $entry['label'] = isset($var['vname']) ? translate($var['vname'], $value->module_dir) : $var['name'];
                    } else {
                        $entry['label'] = isset($var['vname']) ? $var['vname'] : $var['name'];
                    }
                    $entry['required'] = $required;
                    $entry['options'] = $options_ret;
                    $entry['related_module'] = (isset($var['id_name']) && isset($var['module'])) ? $var['module'] : '';
                    $entry['calculated'] =  (isset($var['calculated']) && $var['calculated']) ? true : false;
                    $entry['len'] =  isset($var['len']) ? $var['len'] : '';

                    if (isset($var['default'])) {
                        $entry['default_value'] = $var['default'];
                    }
                    if ($var['type'] == 'parent' && isset($var['type_name'])) {
                        $entry['type_name'] = $var['type_name'];
                    }

                    $module_fields[$var['name']] = $entry;
                } // else
            } //foreach
        } //if

        if ($value->module_dir == 'Meetings' || $value->module_dir == 'Calls') {
            if (isset($module_fields['duration_minutes']) && isset($GLOBALS['app_list_strings']['duration_intervals'])) {
                $options_dom = $GLOBALS['app_list_strings']['duration_intervals'];
                $options_ret = array();
                foreach ($options_dom as $key=>$oneOption) {
                    $options_ret[$key] = $this->get_name_value($key, $oneOption);
                }

                $module_fields['duration_minutes']['options'] = $options_ret;
            }
        }

        if ($value->module_dir == 'Bugs') {
            require_once('modules/Releases/Release.php');
            $seedRelease = BeanFactory::newBean('Releases');
            $options = $seedRelease->get_releases(true, "Active");
            $options_ret = array();
            foreach ($options as $name=>$value) {
                $options_ret[] =  array('name'=> $name , 'value'=>$value);
            }
            if (isset($module_fields['fixed_in_release'])) {
                $module_fields['fixed_in_release']['type'] = 'enum';
                $module_fields['fixed_in_release']['options'] = $options_ret;
            }
            if (isset($module_fields['found_in_release'])) {
                $module_fields['found_in_release']['type'] = 'enum';
                $module_fields['found_in_release']['options'] = $options_ret;
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
        if (isset($value->assigned_name) && isset($module_fields['team_id'])) {
            $module_fields['team_name'] = $module_fields['team_id'];
            $module_fields['team_name']['name'] = 'team_name';
        }
        if (isset($module_fields['modified_user_id'])) {
            $module_fields['modified_by_name'] = $module_fields['modified_user_id'];
            $module_fields['modified_by_name']['name'] = 'modified_by_name';
        }
        if (isset($module_fields['created_by'])) {
            $module_fields['created_by_name'] = $module_fields['created_by'];
            $module_fields['created_by_name']['name'] = 'created_by_name';
        }

        $GLOBALS['log']->info('End: SoapHelperWebServices->get_field_list');
        return array('module_fields' => $module_fields, 'link_fields' => $link_fields);
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
            foreach ($name_value_list as $name => $value) {
                if (is_array($value) &&  $value['name'] == 'id') {
                    $seed->retrieve($value['value']);
                    break;
                } elseif ($name === 'id') {
                    $seed->retrieve($value);
                }
            }

            foreach ($name_value_list as $name => $value) {
                //Normalize the input
                if (!is_array($value)) {
                    $field_name = $name;
                    $val = $value;
                } else {
                    $field_name = $value['name'];
                    $val = $value['value'];
                }

                if ($seed->field_name_map[$field_name]['type'] == 'enum') {
                    $vardef = $seed->field_name_map[$field_name];
                    if (isset($app_list_strings[$vardef['options']]) && !isset($app_list_strings[$vardef['options']][$val])) {
                        if (in_array($val, $app_list_strings[$vardef['options']])) {
                            $val = array_search($val, $app_list_strings[$vardef['options']], true);
                        }
                    }
                }
                if ($module_name == 'Users' && !empty($seed->id) && ($seed->id != $current_user->id) && $field_name == 'user_hash') {
                    continue;
                }
                if (!empty($seed->field_name_map[$field_name]['sensitive'])) {
                    continue;
                }
                $seed->$field_name = $val;
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
                            $query = $seed->table_name.".outlook_id = '".$seed->outlook_id."'";
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


    public function checkSessionAndModuleAccess($session, $login_error_key, $module_name, $access_level, $module_access_level_error_key, $errorObject)
    {
        if (isset($_REQUEST['oauth_token'])) {
            $session = $this->checkOAuthAccess($errorObject);
        }
        if (!$session) {
            return false;
        }
        return parent::checkSessionAndModuleAccess($session, $login_error_key, $module_name, $access_level, $module_access_level_error_key, $errorObject);
    }

    public function checkOAuthAccess($errorObject)
    {
        require_once "include/SugarOAuthServer.php";
        try {
            $oauth = new SugarOAuthServer();
            $token = $oauth->authorizedToken();
            if (empty($token) || empty($token->assigned_user_id)) {
                return false;
            }
        } catch (OAuthException $e) {
            $GLOBALS['log']->debug("OAUTH Exception: $e");
            $errorObject->set_error('invalid_login');
            $this->setFaultObject($errorObject);
            return false;
        }

        $user = BeanFactory::newBean('Users');
        $user->retrieve($token->assigned_user_id);
        if (empty($user->id)) {
            return false;
        }
        global $current_user;
        $current_user = $user;
        ini_set("session.use_cookies", 0); // disable cookies to prevent session ID from going out
        session_start();
        session_regenerate_id();
        $_SESSION['oauth'] = $oauth->authorization();
        $_SESSION['avail_modules'] = $this->get_user_module_list($user);
        // TODO: handle role
        // handle session
        $_SESSION['is_valid_session']= true;
        $_SESSION['ip_address'] = query_client_ip();
        $_SESSION['user_id'] = $current_user->id;
        $_SESSION['type'] = 'user';
        $_SESSION['authenticated_user_id'] = $current_user->id;
        return session_id();
    }


    /**
     * get_subpanel_defs
     *
     * @param String $module The name of the module to get the subpanel definition for
     * @param String $type The type of subpanel definition ('wireless' or 'default')
     * @return array Array of the subpanel definition; empty array if no matching definition found
     */
    public function get_subpanel_defs($module, $type)
    {
        global $beanList, $beanFiles;
        $results = array();
        switch ($type) {
            case 'wireless':

                if (file_exists('custom/modules/'.$module.'/metadata/wireless.subpaneldefs.php')) {
                    require_once('custom/modules/'.$module.'/metadata/wireless.subpaneldefs.php');
                } elseif (file_exists('modules/'.$module.'/metadata/wireless.subpaneldefs.php')) {
                    require_once('modules/'.$module.'/metadata/wireless.subpaneldefs.php');
                }

                //If an Ext/WirelessLayoutdefs/wireless.subpaneldefs.ext.php file exists, then also load it as well
                if (file_exists('custom/modules/'.$module.'/Ext/WirelessLayoutdefs/wireless.subpaneldefs.ext.php')) {
                    require_once('custom/modules/'.$module.'/Ext/WirelessLayoutdefs/wireless.subpaneldefs.ext.php');
                }
                break;

            case 'default':
            default:
                if (file_exists('modules/'.$module.'/metadata/subpaneldefs.php')) {
                    require('modules/'.$module.'/metadata/subpaneldefs.php');
                }
                if (file_exists('custom/modules/'.$module.'/Ext/Layoutdefs/layoutdefs.ext.php')) {
                    require('custom/modules/'.$module.'/Ext/Layoutdefs/layoutdefs.ext.php');
                }
        }

        //Filter results for permissions
        foreach ($layout_defs[$module]['subpanel_setup'] as $subpanel => $subpaneldefs) {
            $moduleToCheck = $subpaneldefs['module'];
            if (!isset($beanList[$moduleToCheck])) {
                continue;
            }
            $class_name = $beanList[$moduleToCheck];
            $bean = new $class_name();
            if ($bean->ACLAccess('list')) {
                $results[$subpanel] = $subpaneldefs;
            }
        }

        return $results;
    }
}
