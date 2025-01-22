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


/**
 * Retrieve field data for a provided SugarBean.
 *
 * @param SugarBean $value -- The bean to retrieve the field information for.
 * @return Array -- 'field'=>   'name' -- the name of the field
 *                              'type' -- the data type of the field
 *                              'label' -- the translation key for the label of the field
 *                              'required' -- Is the field required?
 *                              'options' -- Possible values for a drop down field
 */
function get_field_list($value, $translate = true)
{
    $list = array();

    if (!empty($value->field_defs)) {
        foreach ($value->field_defs as $var) {
            if (isset($var['source']) && ($var['source'] != 'db' && $var['source'] != 'custom_fields') && $var['name'] != 'email1' && $var['name'] != 'email2' && (!isset($var['type']) || $var['type'] != 'relate')) {
                continue;
            }
            $required = 0;
            $options_dom = array();
            $options_ret = array();
            // Apparently the only purpose of this check is to make sure we only return fields
            //   when we've read a record.  Otherwise this function is identical to get_module_field_list
            if (!empty($var['required'])) {
                $required = 1;
            }
            if (isset($var['options'])) {
                $options_dom = translate($var['options'], $value->module_dir);
                if (!is_array($options_dom)) {
                    $options_dom = array();
                }
                foreach ($options_dom as $key => $oneOption) {
                    $options_ret[] = get_name_value($key, $oneOption);
                }
            }

            if (!empty($var['dbType']) && $var['type'] == 'bool') {
                $options_ret[] = get_name_value('type', $var['dbType']);
            }

            $entry = array();
            $entry['name'] = $var['name'];
            $entry['type'] = $var['type'];
            if ($translate) {
                $entry['label'] = isset($var['vname']) ? translate($var['vname'], $value->module_dir) : $var['name'];
            } else {
                $entry['label'] = isset($var['vname']) ? $var['vname'] : $var['name'];
            }
            $entry['required'] = $required;
            $entry['options'] = $options_ret;
            if (isset($var['default'])) {
                $entry['default_value'] = $var['default'];
            }

            $list[$var['name']] = $entry;
        } //foreach
    } //if

    if (isset($value->module_dir) && $value->module_dir == 'Bugs') {
        $seedRelease = BeanFactory::newBean('Releases');
        $options = $seedRelease->get_releases(true, "Active");
        $options_ret = array();
        foreach ($options as $name => $value) {
            $options_ret[] = array('name' => $name, 'value' => $value);
        }
        if (isset($list['fixed_in_release'])) {
            $list['fixed_in_release']['type'] = 'enum';
            $list['fixed_in_release']['options'] = $options_ret;
        }
        if (isset($list['release'])) {
            $list['release']['type'] = 'enum';
            $list['release']['options'] = $options_ret;
        }
        if (isset($list['release_name'])) {
            $list['release_name']['type'] = 'enum';
            $list['release_name']['options'] = $options_ret;
        }
    }
    if (isset($value->module_dir) && $value->module_dir == 'Emails') {
        $fields = array('from_addr_name', 'reply_to_addr', 'to_addrs_names', 'cc_addrs_names', 'bcc_addrs_names');
        foreach ($fields as $field) {
            $var = $value->field_defs[$field];

            $required = 0;
            $entry = array();
            $entry['name'] = $var['name'];
            $entry['type'] = $var['type'];
            if ($translate) {
                $entry['label'] = isset($var['vname']) ? translate($var['vname'], $value->module_dir) : $var['name'];
            } else {
                $entry['label'] = isset($var['vname']) ? $var['vname'] : $var['name'];
            }
            $entry['required'] = $required;
            $entry['options'] = array();
            if (isset($var['default'])) {
                $entry['default_value'] = $var['default'];
            }

            $list[$var['name']] = $entry;
        }
    }

    if (isset($value->assigned_user_name) && isset($list['assigned_user_id'])) {
        $list['assigned_user_name'] = $list['assigned_user_id'];
        $list['assigned_user_name']['name'] = 'assigned_user_name';
    }
    if (isset($list['modified_user_id'])) {
        $list['modified_by_name'] = $list['modified_user_id'];
        $list['modified_by_name']['name'] = 'modified_by_name';
    }
    if (isset($list['created_by'])) {
        $list['created_by_name'] = $list['created_by'];
        $list['created_by_name']['name'] = 'created_by_name';
    }

    return $list;
}

function new_get_field_list($value, $translate = true)
{
    $module_fields = array();
    $link_fields = array();

    if (!empty($value->field_defs)) {
        foreach ($value->field_defs as $var) {
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
            if (!empty($var['required'])) {
                $required = 1;
            }
            if (isset($var['options'])) {
                $options_dom = translate($var['options'], $value->module_dir);
                if (!is_array($options_dom)) {
                    $options_dom = array();
                }
                foreach ($options_dom as $key => $oneOption) {
                    $options_ret[] = get_name_value($key, $oneOption);
                }
            }

            if (!empty($var['dbType']) && $var['type'] == 'bool') {
                $options_ret[] = get_name_value('type', $var['dbType']);
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

    return array('module_fields' => $module_fields, 'link_fields' => $link_fields);
} // fn

function setFaultObject($errorObject)
{
    global $soap_server_object;
    $soap_server_object->fault(
        $errorObject->getFaultCode(),
        $errorObject->getName(),
        '',
        $errorObject->getDescription()
    );
} // fn

function checkSessionAndModuleAccess(
    $session,
    $login_error_key,
    $module_name,
    $access_level,
    $module_access_level_error_key,
    $errorObject
) {
    if (!validate_authenticated($session)) {
        $errorObject->set_error('invalid_login');
        setFaultObject($errorObject);

        return false;
    } // if

    global $beanList, $beanFiles;
    if (!empty($module_name)) {
        if (empty($beanList[$module_name])) {
            $errorObject->set_error('no_module');
            setFaultObject($errorObject);

            return false;
        } // if
        global $current_user;
        if (!check_modules_access($current_user, $module_name, $access_level)) {
            $errorObject->set_error('no_access');
            setFaultObject($errorObject);

            return false;
        }
    } // if

    return true;
} // fn

function checkACLAccess($bean, $viewType, $errorObject, $error_key)
{
    if (!$bean->ACLAccess($viewType)) {
        $errorObject->set_error($error_key);
        setFaultObject($errorObject);

        return false;
    } // if

    return true;
} // fn

function get_name_value($field, $value)
{
    return array('name' => $field, 'value' => $value);
}

function get_user_module_list($user)
{
    global $app_list_strings, $current_language, $beanList, $beanFiles;

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
            if ($value['module']['access']['aclaccess'] == ACL_ALLOW_DISABLED) {
                unset($modules[$key]);
            } else {
                $modules[$key] = 'read_only';
            } // else
        } else {
            $modules[$key] = '';
        } // else
    } // foreach

    //Remove all modules that don't have a beanFiles entry associated with it
    foreach ($modules as $module_name => $module) {
        if (isset($beanList[$module_name])) {
            $class_name = $beanList[$module_name];
            if (empty($beanFiles[$class_name])) {
                unset($modules[$module_name]);
            }
        } else {
            unset($modules[$module_name]);
        }
    }

    return $modules;
}

function check_modules_access($user, $module_name, $action = 'write')
{
    if (!isset($_SESSION['avail_modules'])) {
        $_SESSION['avail_modules'] = get_user_module_list($user);
    }
    if (isset($_SESSION['avail_modules'][$module_name])) {
        if ($action == 'write' && $_SESSION['avail_modules'][$module_name] == 'read_only') {
            if (is_admin($user)) {
                return true;
            }

            return false;
        } elseif ($action == 'write' && strcmp(
            strtolower($module_name),
            'users'
        ) == 0 && !$user->isAdminForModule($module_name)
        ) {
            //rrs bug: 46000 - If the client is trying to write to the Users module and is not an admin then we need to stop them
            return false;
        }

        return true;
    }

    return false;
}

function get_name_value_list($value, $returnDomValue = false)
{
    global $app_list_strings;
    $list = array();
    if (!empty($value->field_defs)) {
        if (isset($value->assigned_user_name)) {
            $list['assigned_user_name'] = get_name_value('assigned_user_name', $value->assigned_user_name);
        }
        if (isset($value->modified_by_name)) {
            $list['modified_by_name'] = get_name_value('modified_by_name', $value->modified_by_name);
        }
        if (isset($value->created_by_name)) {
            $list['created_by_name'] = get_name_value('created_by_name', $value->created_by_name);
        }
        foreach ($value->field_defs as $var) {
            if (isset($var['source']) && ($var['source'] != 'db' && $var['source'] != 'custom_fields') && $var['name'] != 'email1' && $var['name'] != 'email2' && (!isset($var['type']) || $var['type'] != 'relate')) {
                if ($value->module_dir == 'Emails' && (($var['name'] == 'description') || ($var['name'] == 'description_html') || ($var['name'] == 'from_addr_name') || ($var['name'] == 'reply_to_addr') || ($var['name'] == 'to_addrs_names') || ($var['name'] == 'cc_addrs_names') || ($var['name'] == 'bcc_addrs_names') || ($var['name'] == 'raw_source'))) {
                } else {
                    continue;
                }
            }

            if (isset($value->{$var['name']})) {
                $val = $value->{$var['name']};
                $type = $var['type'];

                if (strcmp($type, 'date') == 0) {
                    $val = substr((string) $val, 0, 10);
                } elseif (strcmp($type, 'enum') == 0 && !empty($var['options']) && $returnDomValue) {
                    $val = $app_list_strings[$var['options']][$val];
                } elseif (strcmp($type, 'currency') == 0) {
                    $params = array('currency_symbol' => false);
                    $val = currency_format_number($val, $params);
                }

                $list[$var['name']] = get_name_value($var['name'], $val);
            }
        }
    }

    return $list;
}

function filter_fields($value, $fields)
{
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
                continue;
            }
        } // if
        // No valid field should be caught by this quoting.
        $filterFields[] = getValidDBName($field);
    } // foreach

    return $filterFields;
} // fn

function get_name_value_list_for_fields($value, $fields)
{
    global $app_list_strings;
    global $invalid_contact_fields;

    $list = array();
    if (!empty($value->field_defs)) {
        if (isset($value->assigned_user_name) && in_array('assigned_user_name', $fields)) {
            $list['assigned_user_name'] = get_name_value('assigned_user_name', $value->assigned_user_name);
        }
        if (isset($value->modified_by_name) && in_array('modified_by_name', $fields)) {
            $list['modified_by_name'] = get_name_value('modified_by_name', $value->modified_by_name);
        }
        if (isset($value->created_by_name) && in_array('created_by_name', $fields)) {
            $list['created_by_name'] = get_name_value('created_by_name', $value->created_by_name);
        }

        $filterFields = filter_fields($value, $fields);
        foreach ($filterFields as $field) {
            $var = $value->field_defs[$field];
            if (isset($value->{$var['name']})) {
                $val = $value->{$var['name']};
                $type = $var['type'];

                if (strcmp($type, 'date') == 0) {
                    $val = substr((string) $val, 0, 10);
                } elseif (strcmp($type, 'enum') == 0 && !empty($var['options'])) {
                    $val = $app_list_strings[$var['options']][$val];
                }

                $list[$var['name']] = get_name_value($var['name'], $val);
            } // if
        } // foreach
    } // if

    return $list;
} // fn


function array_get_name_value_list($array)
{
    $list = array();
    foreach ($array as $name => $value) {
        $list[$name] = get_name_value($name, $value);
    }

    return $list;
}

function array_get_name_value_lists($array)
{
    $list = array();
    foreach ($array as $name => $value) {
        $tmp_value = $value;
        if (is_array($value)) {
            $tmp_value = array();
            foreach ($value as $k => $v) {
                $tmp_value[] = get_name_value($k, $v);
            }
        }
        $list[] = get_name_value($name, $tmp_value);
    }

    return $list;
}

function name_value_lists_get_array($list)
{
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

    return $array;
}

function array_get_return_value($array, $module)
{
    return array(
        'id' => $array['id'],
        'module_name' => $module,
        'name_value_list' => array_get_name_value_list($array)
    );
}

function get_return_value_for_fields($value, $module, $fields)
{
    global $module_name, $current_user;
    $module_name = $module;
    if ($module == 'Users' && $value->id != $current_user->id) {
        $value->user_hash = '';
    }
    $value = clean_sensitive_data($value->field_defs, $value);

    return array(
        'id' => $value->id,
        'module_name' => $module,
        'name_value_list' => get_name_value_list_for_fields($value, $fields)
    );
}

function getRelationshipResults($bean, $link_field_name, $link_module_fields)
{
    global $beanList, $beanFiles;
    $bean->load_relationship($link_field_name);
    if (isset($bean->$link_field_name)) {
        // get the query object for this link field
        $query_array = $bean->$link_field_name->getQuery(true, array(), 0, '', true);
        $params = array();
        $params['joined_tables'] = $query_array['join_tables'];

        // get the related module name and instantiate a bean for that.
        $submodulename = $bean->$link_field_name->getRelatedModuleName();
        $submoduleclass = $beanList[$submodulename];
        require_once($beanFiles[$submoduleclass]);

        $submodule = new $submoduleclass();
        $filterFields = filter_fields($submodule, $link_module_fields);
        $relFields = $bean->$link_field_name->getRelatedFields();
        $roleSelect = '';

        if (!empty($relFields)) {
            foreach ($link_module_fields as $field) {
                if (!empty($relFields[$field])) {
                    $roleSelect .= ', ' . $query_array['join_tables'][0] . '.' . $field;
                }
            }
        }
        // create a query
        $subquery = $submodule->create_new_list_query('', '', $filterFields, $params, 0, '', true, $bean);
        $query = $subquery['select'] . $roleSelect . $subquery['from'] . $query_array['join'] . $subquery['where'];

        $result = $submodule->db->query($query, true);
        $list = array();
        while ($row = $submodule->db->fetchByAssoc($result)) {
            $list[] = $row;
        }

        return array('rows' => $list, 'fields_set_on_rows' => $filterFields);
    }
    return false;
    // else
} // fn

function get_return_value_for_link_fields($bean, $module, $link_name_to_value_fields_array)
{
    global $module_name, $current_user;
    $module_name = $module;
    if ($module == 'Users' && $bean->id != $current_user->id) {
        $bean->user_hash = '';
    }
    $bean = clean_sensitive_data($bean->field_defs, $bean);

    if (empty($link_name_to_value_fields_array) || !is_array($link_name_to_value_fields_array)) {
        return array();
    }

    $link_output = array();
    foreach ($link_name_to_value_fields_array as $link_name_value_fields) {
        if (!is_array($link_name_value_fields) || !isset($link_name_value_fields['name']) || !isset($link_name_value_fields['value'])) {
            continue;
        }
        $link_field_name = $link_name_value_fields['name'];
        $link_module_fields = $link_name_value_fields['value'];
        if (is_array($link_module_fields) && !empty($link_module_fields)) {
            $result = getRelationshipResults($bean, $link_field_name, $link_module_fields);
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
                            $nameValue['name'] = $field;
                            $nameValue['value'] = $row[$field];
                            $nameValueArray[] = $nameValue;
                        } // if
                    } // foreach
                    $rowArray[] = $nameValueArray;
                } // foreach
                $link_output[] = array('name' => $link_field_name, 'records' => $rowArray);
            } // if
        } // if
    } // foreach

    return $link_output;
} // fn

/**
 *
 * @param String $module_name -- The name of the module that the primary record is from.  This name should be the name the module was developed under (changing a tab name is studio does not affect the name that should be passed into this method).
 * @param String $module_id -- The ID of the bean in the specified module
 * @param String $link_field_name - The relationship name for which to create relationships.
 * @param Array $related_ids -- The array of ids for which we want to create relationships
 * @return true on success, false on failure
 */
function new_handle_set_relationship($module_name, $module_id, $link_field_name, $related_ids)
{
    global $beanList, $beanFiles;

    if (empty($beanList[$module_name])) {
        return false;
    } // if
    $class_name = $beanList[$module_name];
    require_once($beanFiles[$class_name]);
    $mod = new $class_name();
    $mod->retrieve($module_id);
    if (!$mod->ACLAccess('DetailView')) {
        return false;
    }

    foreach ($related_ids as $ids) {
        $GLOBALS['log']->debug("ids = " . $ids);
    }

    if ($mod->load_relationship($link_field_name)) {
        $mod->$link_field_name->add($related_ids);

        return true;
    }
    return false;
}

function new_handle_set_entries($module_name, $name_value_lists, $select_fields = false)
{
    global $beanList, $beanFiles, $app_list_strings;
    global $current_user;

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
            $seed->{$value['name']} = $val;
        }

        if ($count == $total) {
            $seed->update_vcal = false;
        }
        $count++;

        //Add the account to a contact
        if ($module_name == 'Contacts') {
            $GLOBALS['log']->debug('Creating Contact Account');
            add_create_account($seed);
            $duplicate_id = check_for_duplicate_contacts($seed);
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
        } else {
            if ($module_name == 'Meetings' || $module_name == 'Calls') {
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
        }

        // if somebody is calling set_entries_detail() and wants fields returned...
        if ($select_fields !== false) {
            $ret_values[$count] = array();

            foreach ($select_fields as $select_field) {
                if (isset($seed->$select_field)) {
                    $ret_values[$count][] = get_name_value($select_field, $seed->$select_field);
                }
            }
        }
    }

    // handle returns for set_entries_detail() and set_entries()
    if ($select_fields !== false) {
        return array(
            'name_value_lists' => $ret_values,
        );
    }
    return array(
            'ids' => $ids,
        );
}

function get_return_value($value, $module, $returnDomValue = false)
{
    global $module_name, $current_user;
    $module_name = $module;
    if ($module == 'Users' && $value->id != $current_user->id) {
        $value->user_hash = '';
    }
    $value = clean_sensitive_data($value->field_defs, $value);

    return array(
        'id' => $value->id,
        'module_name' => $module,
        'name_value_list' => get_name_value_list($value, $returnDomValue)
    );
}


function get_encoded_Value($value)
{

    // XML 1.0 doesn't allow those...
    $value = preg_replace("/([\\x00-\\x08\\x0B\\x0C\\x0E-\\x1F])/", '', (string) $value);
    $value = htmlspecialchars($value, ENT_NOQUOTES, "utf-8");

    return "<value>$value</value>";
}

function get_name_value_xml($val, $module_name)
{
    $xml = '<item>';
    $xml .= '<id>' . $val['id'] . '</id>';
    $xml .= '<module>' . $module_name . '</module>';
    $xml .= '<name_value_list>';
    foreach ($val['name_value_list'] as $name => $nv) {
        $xml .= '<name_value>';
        $xml .= '<name>' . htmlspecialchars((string) $nv['name']) . '</name>';
        $xml .= get_encoded_Value($nv['value']);
        $xml .= '</name_value>';
    }
    $xml .= '</name_value_list>';
    $xml .= '</item>';

    return $xml;
}

function new_get_return_module_fields($value, $module, $translate = true)
{
    global $module_name;
    $module_name = $module;
    $result = new_get_field_list($value, $translate);

    return array(
        'module_name' => $module,
        'module_fields' => $result['module_fields'],
        'link_fields' => $result['link_fields'],
    );
}

function get_return_module_fields($value, $module, $error, $translate = true)
{
    global $module_name;
    $module_name = $module;

    return array(
        'module_name' => $module,
        'module_fields' => get_field_list($value, $translate),
        'error' => get_name_value_list($value)
    );
}

function get_return_error_value($error_num, $error_name, $error_description)
{
    return array(
        'number' => $error_num,
        'name' => $error_name,
        'description' => $error_description
    );
}

function filter_field_list(&$field_list, $select_fields, $module_name)
{
    return filter_return_list($field_list, $select_fields, $module_name);
}


/**
 * Filter the results of a list query.  Limit the fields returned.
 *
 * @param Array $output_list -- The array of list data
 * @param Array $select_fields -- The list of fields that should be returned.  If this array is specified, only the fields in the array will be returned.
 * @param String $module_name -- The name of the module this being worked on
 * @return The filtered array of list data.
 */
function filter_return_list(&$output_list, $select_fields, $module_name)
{
    $output_listCount = count($output_list);
    for ($sug = 0; $sug < $output_listCount; $sug++) {
        if ($module_name == 'Contacts') {
            global $invalid_contact_fields;
            if (is_array($invalid_contact_fields)) {
                foreach ($invalid_contact_fields as $name => $val) {
                    unset($output_list[$sug]['field_list'][$name]);
                    unset($output_list[$sug]['name_value_list'][$name]);
                }
            }
        }

        if (!empty($output_list[$sug]['name_value_list']) && is_array($output_list[$sug]['name_value_list']) && !empty($select_fields) && is_array($select_fields)) {
            foreach ($output_list[$sug]['name_value_list'] as $name => $value) {
                if (!in_array($value['name'], $select_fields)) {
                    unset($output_list[$sug]['name_value_list'][$name]);
                    unset($output_list[$sug]['field_list'][$name]);
                }
            }
        }
    }

    return $output_list;
}

function login_success()
{
    global $current_language, $sugar_config, $app_strings, $app_list_strings;
    $current_language = $sugar_config['default_language'];
    $app_strings = return_application_language($current_language);
    $app_list_strings = return_app_list_strings_language($current_language);
}


/*
 *	Given an account_name, either create the account or assign to a contact.
 */
function add_create_account($seed)
{
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
        if ((!isset($account_name) || $account_name == '')) {
            return;
        }
        if (!isset($seed->accounts)) {
            $seed->load_relationship('accounts');
        }

        if ($seed->account_name == '' && isset($temp->account_id)) {
            $seed->accounts->delete($seed->id, $temp->account_id);

            return;
        }

        // attempt to find by id first
        $ret = $focus->retrieve($account_id, true, false);

        // if it doesn't exist by id, attempt to find by name (non-deleted)
        if (empty($ret)) {
            $query = "select {$focus->table_name}.id, {$focus->table_name}.deleted from {$focus->table_name} ";
            $query .= " WHERE name='" . $seed->db->quote($account_name) . "'";
            $query .= " ORDER BY deleted ASC";
            $result = $seed->db->query($query, true);

            $row = $seed->db->fetchByAssoc($result, false);

            if (!empty($row['id'])) {
                $focus->retrieve($row['id']);
            }
        } // if it exists by id but was deleted, just remove it entirely
        elseif ($focus->deleted) {
            $query2 = "delete from {$focus->table_name} WHERE id='" . $seed->db->quote($focus->id) . "'";
            $seed->db->query($query2, true);
            // it was deleted, create new
            $focus = BeanFactory::newBean('Accounts');
        }

        // if we didnt find the account, so create it
        if (empty($focus->id)) {
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
        }
    }
}

function check_for_duplicate_contacts($seed)
{
    if (isset($seed->id)) {
        return null;
    }

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
                    //bug: 39234 - check if the account names are the same
                    //if the incoming contact's account_name is empty OR it is not empty and is the same
                    //as an existing contact's account name, then find the match.
                    $contact->load_relationship('accounts');
                    if (empty($seed->account_name) || strcmp($seed->account_name, $contact->account_name) == 0) {
                        $GLOBALS['log']->info('End: SoapHelperWebServices->check_for_duplicate_contacts - duplicate found ' . $contact->id);

                        return $contact->id;
                    }
                }
            }
        }

        return null;
    }
    //This section of code is executed if no emails are supplied in the $seed instance

    //This query is looking for the id of Contact records that do not have a primary email address based on the matching
    //first and last name and the record being not deleted.  If any such records are found we will take the first one and assume
    //that it is the duplicate record
    $trimmed_first = DBManagerFactory::getInstance()->quoted($trimmed_first);
    $trimmed_last = DBManagerFactory::getInstance()->quoted($trimmed_last);

    $query = "SELECT c.id as id FROM contacts c
LEFT OUTER JOIN email_addr_bean_rel eabr ON eabr.bean_id = c.id
WHERE c.first_name = $trimmed_first AND c.last_name = $trimmed_last AND c.deleted = 0 AND eabr.id IS NULL";

    //Apply the limit query filter to this since we only need the first record
    $result = DBManagerFactory::getInstance()->getOne($query);

    return !empty($result) ? $result : null;
}

/*
 * Given a client version and a server version, determine if the right hand side(server version) is greater
 *
 * @param left           the client sugar version
 * @param right          the server version
 *
 * return               true if the server version is greater or they are equal
 *                      false if the client version is greater
 */
function is_server_version_greater($left, $right)
{
    if ((is_countable($left) ? count($left) : 0) == 0 && (is_countable($right) ? count($right) : 0) == 0) {
        return false;
    } elseif ((is_countable($left) ? count($left) : 0) == 0 || (is_countable($right) ? count($right) : 0) == 0) {
        return true;
    } elseif ($left[0] == $right[0]) {
        array_shift($left);
        array_shift($right);

        return is_server_version_greater($left, $right);
    } elseif ($left[0] < $right[0]) {
        return true;
    }
    return false;
}

function getFile($zip_file, $file_in_zip)
{
    $base_upgrade_dir = sugar_cached("/upgrades");
    $base_tmp_upgrade_dir = "$base_upgrade_dir/temp";
    $my_zip_dir = mk_temp_dir($base_tmp_upgrade_dir);
    unzip_file($zip_file, $file_in_zip, $my_zip_dir);

    return ("$my_zip_dir/$file_in_zip");
}

function getManifest($zip_file)
{
    ini_set("max_execution_time", "3600");

    return (getFile($zip_file, "manifest.php"));
}

if (!function_exists("get_encoded")) {
    /*HELPER FUNCTIONS*/
    function get_encoded($object)
    {
        return base64_encode(serialize($object));
    }

    function get_decoded($object)
    {
        return unserialize(base64_decode($object));
    }

    /**
     * decrypt a string use the TripleDES algorithm. This meant to be
     * modified if the end user chooses a different algorithm
     *
     * @param $string - the string to decrypt
     *
     * @return string decrypted string if we can decrypt, the original string otherwise
     */
    function decrypt_string($string)
    {
        if (function_exists('openssl_decrypt')) {
            $focus = BeanFactory::newBean('Administration');
            $focus->retrieveSettings();
            $key = '';
            if (!empty($focus->settings['ldap_enc_key'])) {
                $key = $focus->settings['ldap_enc_key'];
            }
            if (empty($key)) {
                return $string;
            }
            $buffer = $string;
            $key = substr(md5($key), 0, 24);
            $iv = "password";
            return openssl_decrypt(pack("H*", $buffer), 'des-ede3-cbc', $key, OPENSSL_NO_PADDING, $iv);
        }
    }
}

function canViewPath($path, $base)
{
    $path = realpath($path);
    $base = realpath($base);

    return 0 !== strncmp($path, $base, strlen($base));
}


/**
 * apply_values
 *
 * This function applies the given values to the bean object.  If it is a first time sync
 * then empty values will not be copied over.
 *
 * @param Mixed $seed Object representing SugarBean instance
 * @param Array $dataValues Array of fields/values to set on the SugarBean instance
 * @param boolean $firstSync Boolean indicating whether or not this is a first time sync
 */
function apply_values($seed, $dataValues, $firstSync)
{
    if (!$seed instanceof SugarBean || !is_array($dataValues)) {
        return;
    }

    foreach ($dataValues as $field => $value) {
        if ($firstSync) {
            //If this is a first sync AND the value is not empty then we set it
            if (!empty($value)) {
                $seed->$field = $value;
            }
        } else {
            $seed->$field = $value;
        }
    }
}

/*END HELPER*/
