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
 * This class is an implementation class for all the web services
 */
require_once('service/core/SoapHelperWebService.php');
SugarWebServiceImpl::$helperObject = new SoapHelperWebServices();

#[\AllowDynamicProperties]
class SugarWebServiceImpl
{
    public static $helperObject = null;

    /**
     * Retrieve a single SugarBean based on ID.
     *
     * @param String $session -- Session ID returned by a previous call to login.
     * @param String $module_name -- The name of the module to return records from.  This name should be the name the module was developed under (changing a tab name is studio does not affect the name that should be passed into this method)..
     * @param String $id -- The SugarBean's ID value.
     * @param Array  $select_fields -- A list of the fields to be included in the results. This optional parameter allows for only needed fields to be retrieved.
    * @param Array $link_name_to_fields_array -- A list of link_names and for each link_name, what fields value to be returned. For ex.'link_name_to_fields_array' => array(array('name' =>  'email_addresses', 'value' => array('id', 'email_address', 'opt_out', 'primary_address')))
    * @return Array
    *        'entry_list' -- Array - The records name value pair for the simple data types excluding link field data.
    *	     'relationship_list' -- Array - The records link field data. The example is if asked about accounts email address then return data would look like Array ( [0] => Array ( [name] => email_addresses [records] => Array ( [0] => Array ( [0] => Array ( [name] => id [value] => 3fb16797-8d90-0a94-ac12-490b63a6be67 ) [1] => Array ( [name] => email_address [value] => hr.kid.qa@example.com ) [2] => Array ( [name] => opt_out [value] => 0 ) [3] => Array ( [name] => primary_address [value] => 1 ) ) [1] => Array ( [0] => Array ( [name] => id [value] => 403f8da1-214b-6a88-9cef-490b63d43566 ) [1] => Array ( [name] => email_address [value] => kid.hr@example.name ) [2] => Array ( [name] => opt_out [value] => 0 ) [3] => Array ( [name] => primary_address [value] => 0 ) ) ) ) )
    * @exception 'SoapFault' -- The SOAP error, if any
    */
    public function get_entry($session, $module_name, $id, $select_fields, $link_name_to_fields_array)
    {
        $GLOBALS['log']->info('Begin: SugarWebServiceImpl->get_entry');
        return self::get_entries($session, $module_name, array($id), $select_fields, $link_name_to_fields_array);
        $GLOBALS['log']->info('end: SugarWebServiceImpl->get_entry');
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
    public function get_entries($session, $module_name, $ids, $select_fields, $link_name_to_fields_array)
    {
        $GLOBALS['log']->info('Begin: SugarWebServiceImpl->get_entries');
        global  $beanList, $beanFiles;
        $error = new SoapError();

        $linkoutput_list = array();
        $output_list = array();
        $using_cp = false;
        if ($module_name == 'CampaignProspects') {
            $module_name = 'Prospects';
            $using_cp = true;
        }
        if (!self::$helperObject->checkSessionAndModuleAccess($session, 'invalid_session', $module_name, 'read', 'no_access', $error)) {
            $GLOBALS['log']->info('End: SugarWebServiceImpl->get_entries');
            return;
        } // if

        if ($module_name == 'Reports') {
            $error->set_error('invalid_call_error');
            self::$helperObject->setFaultObject($error);
            $GLOBALS['log']->info('End: SugarWebServiceImpl->get_entries');
            return;
        }

        $class_name = $beanList[$module_name];
        require_once($beanFiles[$class_name]);

        $temp = new $class_name();
        foreach ($ids as $id) {
            $seed = @clone($temp);
            if ($using_cp) {
                $seed = $seed->retrieveTarget($id);
            } else {
                if ($seed->retrieve($id) == null) {
                    $seed->deleted = 1;
                }
            }

            if ($seed->deleted == 1) {
                $list = array();
                $list[] = array('name'=>'warning', 'value'=>'Access to this object is denied since it has been deleted or does not exist');
                $list[] = array('name'=>'deleted', 'value'=>'1');
                $output_list[] = array('id'=>$id,
                                    'module_name'=> $module_name,
                                    'name_value_list'=>$list,
                                    );
                continue;
            }
            if (!self::$helperObject->checkACLAccess($seed, 'DetailView', $error, 'no_access')) {
                return;
            }
            $output_list[] = self::$helperObject->get_return_value_for_fields($seed, $module_name, $select_fields);
            if (!empty($link_name_to_fields_array)) {
                $linkoutput_list[] = self::$helperObject->get_return_value_for_link_fields($seed, $module_name, $link_name_to_fields_array);
            }
        }
        $GLOBALS['log']->info('End: SugarWebServiceImpl->get_entries');
        return array('entry_list'=>$output_list, 'relationship_list' => $linkoutput_list);
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
        $deleted = false
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

        if (!self::$helperObject->checkQuery($error, $query, $order_by)) {
            $GLOBALS['log']->info('End: SugarWebServiceImpl->get_entry_list');
            return;
        } // if

        if (!self::$helperObject->checkACLAccess($seed, 'Export', $error, 'no_access')) {
            $GLOBALS['log']->info('End: SugarWebServiceImpl->get_entry_list');
            return;
        } // if

        if (!self::$helperObject->checkACLAccess($seed, 'list', $error, 'no_access')) {
            $GLOBALS['log']->info('End: SugarWebServiceImpl->get_entry_list');
            return;
        } // if

        if ($query == '') {
            $where = '';
        } // if
        if ($offset == '' || $offset == -1) {
            $offset = 0;
        } // if
        if ($using_cp) {
            $response = $seed->retrieveTargetList($query, $select_fields, $offset, -1, -1, $deleted);
        } else {
            /* @var $seed SugarBean */
            $response = $seed->get_list($order_by, $query, $offset, -1, -1, $deleted, false, $select_fields);
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

        $GLOBALS['log']->info('End: SugarWebServiceImpl->get_entry_list');
        return array('result_count'=>count($output_list), 'next_offset'=>$next_offset, 'entry_list'=>$output_list, 'relationship_list' => $linkoutput_list);
    } // fn


    /**
     * Set a single relationship between two beans.  The items are related by module name and id.
     *
     * @param String $session -- Session ID returned by a previous call to login.
     * @param String $module_name -- name of the module that the primary record is from.  This name should be the name the module was developed under (changing a tab name is studio does not affect the name that should be passed into this method)..
     * @param String $module_id - The ID of the bean in the specified module_name
     * @param String link_field_name -- name of the link field which relates to the other module for which the relationship needs to be generated.
     * @param array related_ids -- array of related record ids for which relationships needs to be generated
     * @param array $name_value_list -- The keys of the array are the SugarBean attributes, the values of the array are the values the attributes should have.
     * @param integer $delete -- Optional, if the value 0 or nothing is passed then it will add the relationship for related_ids and if 1 is passed, it will delete this relationship for related_ids
     * @return Array - created - integer - How many relationships has been created
     *               - failed - integer - How many relationsip creation failed
     * 				 - deleted - integer - How many relationships were deleted
     * @exception 'SoapFault' -- The SOAP error, if any
     */
    public function set_relationship($session, $module_name, $module_id, $link_field_name, $related_ids, $name_value_list=array(), $delete=false)
    {
        $GLOBALS['log']->info('Begin: SugarWebServiceImpl->set_relationship');
        $error = new SoapError();
        if (!self::$helperObject->checkSessionAndModuleAccess($session, 'invalid_session', '', '', '', $error)) {
            $GLOBALS['log']->info('End: SugarWebServiceImpl->set_relationship');
            return;
        } // if

        $count = 0;
        $deletedCount = 0;
        $failed = 0;
        $deleted = 0;
        $name_value_array = array();
        if (is_array($name_value_list)) {
            $name_value_array = $name_value_list;
        }

        if (isset($delete)) {
            $deleted = $delete;
        }
        if (self::$helperObject->new_handle_set_relationship($module_name, $module_id, $link_field_name, $related_ids, $name_value_array, $deleted)) {
            if ($deleted) {
                $deletedCount++;
            } else {
                $count++;
            }
        } else {
            $failed++;
        } // else
        $GLOBALS['log']->info('End: SugarWebServiceImpl->set_relationship');
        return array('created'=>$count , 'failed'=>$failed, 'deleted' => $deletedCount);
    }

    /**
     * Set a single relationship between two beans.  The items are related by module name and id.
     *
     * @param String $session -- Session ID returned by a previous call to login.
     * @param array $module_names -- Array of the name of the module that the primary record is from.  This name should be the name the module was developed under (changing a tab name is studio does not affect the name that should be passed into this method)..
     * @param array $module_ids - The array of ID of the bean in the specified module_name
     * @param array $link_field_names -- Array of the name of the link field which relates to the other module for which the relationships needs to be generated.
     * @param array $related_ids -- array of an array of related record ids for which relationships needs to be generated
     * @param array $name_value_lists -- Array of Array. The keys of the inner array are the SugarBean attributes, the values of the inner array are the values the attributes should have.
     * @param array int $delete_array -- Optional, array of 0 or 1. If the value 0 or nothing is passed then it will add the relationship for related_ids and if 1 is passed, it will delete this relationship for related_ids
     * @return Array - created - integer - How many relationships has been created
     *               - failed - integer - How many relationsip creation failed
     * 				 - deleted - integer - How many relationships were deleted
    *
     * @exception 'SoapFault' -- The SOAP error, if any
    */
    public function set_relationships($session, $module_names, $module_ids, $link_field_names, $related_ids, $name_value_lists, $delete_array)
    {
        $GLOBALS['log']->info('Begin: SugarWebServiceImpl->set_relationships');
        $error = new SoapError();
        if (!self::$helperObject->checkSessionAndModuleAccess($session, 'invalid_session', '', '', '', $error)) {
            $GLOBALS['log']->info('End: SugarWebServiceImpl->set_relationships');
            return;
        } // if

        if ((empty($module_names) || empty($module_ids) || empty($link_field_names) || empty($related_ids)) ||
        (count($module_names) != (count($module_ids) || count($link_field_names) || count($related_ids)))) {
            $error->set_error('invalid_data_format');
            self::$helperObject->setFaultObject($error);
            $GLOBALS['log']->info('End: SugarWebServiceImpl->set_relationships');
            return;
        } // if

        $count = 0;
        $deletedCount = 0;
        $failed = 0;
        $counter = 0;
        $deleted = 0;
        foreach ($module_names as $module_name) {
            $name_value_list = array();
            if (is_array($name_value_lists) && isset($name_value_lists[$counter])) {
                $name_value_list = $name_value_lists[$counter];
            }
            if (is_array($delete_array) && isset($delete_array[$counter])) {
                $deleted = $delete_array[$counter];
            }
            if (self::$helperObject->new_handle_set_relationship($module_name, $module_ids[$counter], $link_field_names[$counter], $related_ids[$counter], $name_value_list, $deleted)) {
                if ($deleted) {
                    $deletedCount++;
                } else {
                    $count++;
                }
            } else {
                $failed++;
            } // else
            $counter++;
        } // foreach
        $GLOBALS['log']->info('End: SugarWebServiceImpl->set_relationships');
        return array('created'=>$count , 'failed'=>$failed, 'deleted' => $deletedCount);
    } // fn

    /**
     * Retrieve a collection of beans that are related to the specified bean and optionally return relationship data for those related beans.
     * So in this API you can get contacts info for an account and also return all those contact's email address or an opportunity info also.
     *
     * @param String $session -- Session ID returned by a previous call to login.
     * @param String $module_name -- The name of the module that the primary record is from.  This name should be the name the module was developed under (changing a tab name is studio does not affect the name that should be passed into this method)..
     * @param String $module_id -- The ID of the bean in the specified module
     * @param String $link_field_name -- The name of the lnk field to return records from.  This name should be the name the relationship.
     * @param String $related_module_query -- A portion of the where clause of the SQL statement to find the related items.  The SQL query will already be filtered to only include the beans that are related to the specified bean. (IGNORED)
     * @param Array $related_fields - Array of related bean fields to be returned.
     * @param Array $related_module_link_name_to_fields_array - For every related bean returrned, specify link fields name to fields info for that bean to be returned. For ex.'link_name_to_fields_array' => array(array('name' =>  'email_addresses', 'value' => array('id', 'email_address', 'opt_out', 'primary_address'))).
     * @param Number $deleted -- false if deleted records should not be include, true if deleted records should be included.
     * @return Array 'entry_list' -- Array - The records that were retrieved
     *	     		 'relationship_list' -- Array - The records link field data. The example is if asked about accounts contacts email address then return data would look like Array ( [0] => Array ( [name] => email_addresses [records] => Array ( [0] => Array ( [0] => Array ( [name] => id [value] => 3fb16797-8d90-0a94-ac12-490b63a6be67 ) [1] => Array ( [name] => email_address [value] => hr.kid.qa@example.com ) [2] => Array ( [name] => opt_out [value] => 0 ) [3] => Array ( [name] => primary_address [value] => 1 ) ) [1] => Array ( [0] => Array ( [name] => id [value] => 403f8da1-214b-6a88-9cef-490b63d43566 ) [1] => Array ( [name] => email_address [value] => kid.hr@example.name ) [2] => Array ( [name] => opt_out [value] => 0 ) [3] => Array ( [name] => primary_address [value] => 0 ) ) ) ) )
    * @exception 'SoapFault' -- The SOAP error, if any
    */
    public function get_relationships($session, $module_name, $module_id, $link_field_name, $related_module_query, $related_fields, $related_module_link_name_to_fields_array, $deleted)
    {
        $GLOBALS['log']->info('Begin: SugarWebServiceImpl->get_relationships');
        global  $beanList, $beanFiles;
        $error = new SoapError();
        if (!self::$helperObject->checkSessionAndModuleAccess($session, 'invalid_session', $module_name, 'read', 'no_access', $error)) {
            $GLOBALS['log']->info('End: SugarWebServiceImpl->get_relationships');
            return;
        } // if

        $class_name = $beanList[$module_name];
        require_once($beanFiles[$class_name]);
        $mod = new $class_name();
        $mod->retrieve($module_id);

        if (!self::$helperObject->checkQuery($error, $related_module_query)) {
            $GLOBALS['log']->info('End: SugarWebServiceImpl->get_relationships');
            return;
        } // if

        if (!self::$helperObject->checkACLAccess($mod, 'DetailView', $error, 'no_access')) {
            $GLOBALS['log']->info('End: SugarWebServiceImpl->get_relationships');
            return;
        } // if

        $output_list = array();
        $linkoutput_list = array();

        // get all the related mmodules data.
        $result = self::$helperObject->getRelationshipResults($mod, $link_field_name, $related_fields, $related_module_query);
        if (self::$helperObject->isLogLevelDebug()) {
            $GLOBALS['log']->debug('SoapHelperWebServices->get_relationships - return data for getRelationshipResults is ' . var_export($result, true));
        } // if
        if ($result) {
            $list = $result['rows'];
            $filterFields = $result['fields_set_on_rows'];

            if ((is_countable($list) ? count($list) : 0) > 0) {
                // get the related module name and instantiate a bean for that.
                $submodulename = $mod->$link_field_name->getRelatedModuleName();
                $submoduleclass = $beanList[$submodulename];
                require_once($beanFiles[$submoduleclass]);

                $submoduletemp = new $submoduleclass();
                foreach ($list as $row) {
                    $submoduleobject = @clone($submoduletemp);
                    // set all the database data to this object
                    foreach ($filterFields as $field) {
                        $submoduleobject->$field = $row[$field];
                    } // foreach
                    if (isset($row['id'])) {
                        $submoduleobject->id = $row['id'];
                    }
                    $output_list[] = self::$helperObject->get_return_value_for_fields($submoduleobject, $submodulename, $filterFields);
                    if (!empty($related_module_link_name_to_fields_array)) {
                        $linkoutput_list[] = self::$helperObject->get_return_value_for_link_fields($submoduleobject, $submodulename, $related_module_link_name_to_fields_array);
                    } // if
                } // foreach
            }
        } // if

        $GLOBALS['log']->info('End: SugarWebServiceImpl->get_relationships');
        return array('entry_list'=>$output_list, 'relationship_list' => $linkoutput_list);
    } // fn

    /**
     * Update or create a single SugarBean.
     *
     * @param String $session -- Session ID returned by a previous call to login.
     * @param String $module_name -- The name of the module to return records from.  This name should be the name the module was developed under (changing a tab name is studio does not affect the name that should be passed into this method)..
     * @param Array $name_value_list -- The keys of the array are the SugarBean attributes, the values of the array are the values the attributes should have.
     * @return Array    'id' -- the ID of the bean that was written to (-1 on error)
     * @exception 'SoapFault' -- The SOAP error, if any
    */
    public function set_entry($session, $module_name, $name_value_list)
    {
        global  $beanList, $beanFiles, $current_user;

        $GLOBALS['log']->info('Begin: SugarWebServiceImpl->set_entry');
        if (self::$helperObject->isLogLevelDebug()) {
            $GLOBALS['log']->debug('SoapHelperWebServices->set_entry - input data is ' . var_export($name_value_list, true));
        } // if
        $error = new SoapError();
        if (!self::$helperObject->checkSessionAndModuleAccess($session, 'invalid_session', $module_name, 'write', 'no_access', $error)) {
            $GLOBALS['log']->info('End: SugarWebServiceImpl->set_entry');
            return;
        } // if
        $class_name = $beanList[$module_name];
        require_once($beanFiles[$class_name]);
        $seed = new $class_name();
        foreach ($name_value_list as $name=>$value) {
            if (is_array($value) &&  $value['name'] == 'id') {
                $seed->retrieve($value['value']);
                break;
            } elseif ($name === 'id') {
                $seed->retrieve($value);
            }
        }

        foreach ($name_value_list as $name=>$value) {
            if ($module_name == 'Users' && !empty($seed->id) && ($seed->id != $current_user->id) && $name == 'user_hash') {
                continue;
            }
            if (!empty($seed->field_name_map[$name]['sensitive'])) {
                continue;
            }
            if (!is_array($value)) {
                $seed->$name = $value;
            } else {
                $seed->{$value['name']} = $value['value'];
            }
        }
        if (!self::$helperObject->checkACLAccess($seed, 'Save', $error, 'no_access') || ($seed->deleted == 1  && !self::$helperObject->checkACLAccess($seed, 'Delete', $error, 'no_access'))) {
            $GLOBALS['log']->info('End: SugarWebServiceImpl->set_entry');
            return;
        } // if

        $seed->save(self::$helperObject->checkSaveOnNotify());
        if ($seed->deleted == 1) {
            $seed->mark_deleted($seed->id);
        }
        $GLOBALS['log']->info('End: SugarWebServiceImpl->set_entry');
        return array('id'=>$seed->id);
    } // fn

    /**
     * Update or create a list of SugarBeans
     *
     * @param String $session -- Session ID returned by a previous call to login.
     * @param String $module_name -- The name of the module to return records from.  This name should be the name the module was developed under (changing a tab name is studio does not affect the name that should be passed into this method)..
     * @param Array $name_value_lists -- Array of Bean specific Arrays where the keys of the array are the SugarBean attributes, the values of the array are the values the attributes should have.
     * @return Array    'ids' -- Array of the IDs of the beans that was written to (-1 on error)
     * @exception 'SoapFault' -- The SOAP error, if any
     */
    public function set_entries($session, $module_name, $name_value_lists)
    {
        $GLOBALS['log']->info('Begin: SugarWebServiceImpl->set_entries');
        if (self::$helperObject->isLogLevelDebug()) {
            $GLOBALS['log']->debug('SoapHelperWebServices->set_entries - input data is ' . var_export($name_value_lists, true));
        } // if
        $error = new SoapError();
        if (!self::$helperObject->checkSessionAndModuleAccess($session, 'invalid_session', $module_name, 'write', 'no_access', $error)) {
            $GLOBALS['log']->info('End: SugarWebServiceImpl->set_entries');
            return;
        } // if

        $GLOBALS['log']->info('End: SugarWebServiceImpl->set_entries');
        return self::$helperObject->new_handle_set_entries($module_name, $name_value_lists, false);
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
     * 				 - name_value_list - Array - The name value pair of user_id, user_name, user_language, user_currency_id, user_currency_name
     * @exception 'SoapFault' -- The SOAP error, if any
     */
    public function login($user_auth, $application, $name_value_list)
    {
        $GLOBALS['log']->info('Begin: SugarWebServiceImpl->login');
        global $sugar_config, $system_config;
        $error = new SoapError();
        $user = BeanFactory::newBean('Users');
        $success = false;
        if (!empty($user_auth['encryption']) && $user_auth['encryption'] === 'PLAIN') {
            $user_auth['password'] = md5($user_auth['password']);
        }
        //rrs
        $system_config = BeanFactory::newBean('Administration');
        $system_config->retrieveSettings('system');
        $authController = new AuthenticationController();
        //rrs
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
            } // if
            if (!empty($user) && !empty($user->id) && !$user->is_group) {
                $success = true;
                global $current_user;
                $current_user = $user;
            } // if
        } elseif ($usr_id && isset($user->user_name) && ($user->getPreference('lockout') == '1')) {
            $error->set_error('lockout_reached');
            $GLOBALS['log']->fatal('Lockout reached for user ' . $user_auth['user_name']);
            LogicHook::initialize();
            $GLOBALS['logic_hook']->call_custom_logic('Users', 'login_failed');
            self::$helperObject->setFaultObject($error);
            return;
        } elseif (function_exists('openssl_decrypt')) {
            $password = self::$helperObject->decrypt_string($user_auth['password']);
            if ($authController->login($user_auth['user_name'], $password) && isset($_SESSION['authenticated_user_id'])) {
                $success = true;
            } // if
        } // else if

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
        $current_user->call_custom_logic('after_login');
        $GLOBALS['log']->info('End: SugarWebServiceImpl->login - successful login');
        $nameValueArray = array();
        global $current_language;
        $nameValueArray['user_id'] = self::$helperObject->get_name_value('user_id', $current_user->id);
        $nameValueArray['user_name'] = self::$helperObject->get_name_value('user_name', $current_user->user_name);
        $nameValueArray['user_language'] = self::$helperObject->get_name_value('user_language', $current_language);
        $cur_id = $current_user->getPreference('currency');
        $nameValueArray['user_currency_id'] = self::$helperObject->get_name_value('user_currency_id', $cur_id);
        $currencyObject = BeanFactory::newBean('Currencies');
        $currencyObject->retrieve($cur_id);
        $nameValueArray['user_currency_name'] = self::$helperObject->get_name_value('user_currency_name', $currencyObject->name);
        $_SESSION['user_language'] = $current_language;
        return array('id'=>session_id(), 'module_name'=>'Users', 'name_value_list'=>$nameValueArray);
    } // if
        LogicHook::initialize();
        $GLOBALS['logic_hook']->call_custom_logic('Users', 'login_failed');
        $error->set_error('invalid_login');
        self::$helperObject->setFaultObject($error);
        $GLOBALS['log']->info('End: SugarWebServiceImpl->login - failed login');
    }

    /**
     * Log out of the session.  This will destroy the session and prevent other's from using it.
     *
     * @param String $session -- Session ID returned by a previous call to login.
     * @return Empty
     * @exception 'SoapFault' -- The SOAP error, if any
     */
    public function logout($session)
    {
        global $current_user;

        $GLOBALS['log']->info('Begin: SugarWebServiceImpl->logout');
        $error = new SoapError();
        LogicHook::initialize();
        if (!self::$helperObject->checkSessionAndModuleAccess($session, 'invalid_session', '', '', '', $error)) {
            $GLOBALS['logic_hook']->call_custom_logic('Users', 'after_logout');
            $GLOBALS['log']->info('End: SugarWebServiceImpl->logout');
            return;
        } // if

        $current_user->call_custom_logic('before_logout');
        session_destroy();
        $GLOBALS['logic_hook']->call_custom_logic('Users', 'after_logout');
        $GLOBALS['log']->info('End: SugarWebServiceImpl->logout');
    } // fn

    /**
     * Gets server info. This will return information like version, flavor and gmt_time.
     * @return Array - flavor - String - Retrieve the specific flavor of sugar.
     * 				 - version - String - Retrieve the version number of Sugar that the server is running.
     * 				 - gmt_time - String - Return the current time on the server in the format 'Y-m-d H:i:s'. This time is in GMT.
     * @exception 'SoapFault' -- The SOAP error, if any
     */
    public function get_server_info()
    {
        $GLOBALS['log']->info('Begin: SugarWebServiceImpl->get_server_info');
        global $sugar_flavor;
        require_once('sugar_version.php');
        require_once('modules/Administration/Administration.php');

        $admin  = BeanFactory::newBean('Administration');
        $admin->retrieveSettings('info');
        $sugar_version = '';
        if (isset($admin->settings['info_sugar_version'])) {
            $sugar_version = $admin->settings['info_sugar_version'];
        } else {
            $sugar_version = '1.0';
        }

        $GLOBALS['log']->info('End: SugarWebServiceImpl->get_server_info');
        return array('flavor' => $sugar_flavor, 'version' => $sugar_version, 'gmt_time' => TimeDate::getInstance()->nowDb());
    } // fn

    /**
     * Return the user_id of the user that is logged into the current session.
     *
     * @param String $session -- Session ID returned by a previous call to login.
     * @return String -- the User ID of the current session
     * @exception 'SoapFault' -- The SOAP error, if any
     */
    public function get_user_id($session)
    {
        $GLOBALS['log']->info('Begin: SugarWebServiceImpl->get_user_id');
        $error = new SoapError();
        if (!self::$helperObject->checkSessionAndModuleAccess($session, 'invalid_session', '', '', '', $error)) {
            return;
        } // if
        global $current_user;
        $GLOBALS['log']->info('End: SugarWebServiceImpl->get_user_id');
        return $current_user->id;
    } // fn

    /**
     * Retrieve vardef information on the fields of the specified bean.
     *
     * @param String $session -- Session ID returned by a previous call to login.
     * @param String $module_name -- The name of the module to return records from.  This name should be the name the module was developed under (changing a tab name is studio does not affect the name that should be passed into this method)..
     * @param Array $fields -- Optional, if passed then retrieve vardef information on these fields only.
     * @return Array    'module_fields' -- Array - The vardef information on the selected fields.
     *                  'link_fields' -- Array - The vardef information on the link fields
     * @exception 'SoapFault' -- The SOAP error, if any
     */
    public function get_module_fields($session, $module_name, $fields = array())
    {
        $GLOBALS['log']->info('Begin: SugarWebServiceImpl->get_module_fields for ' . $module_name);
        global  $beanList, $beanFiles;
        $error = new SoapError();
        $module_fields = array();

        if (!self::$helperObject->checkSessionAndModuleAccess($session, 'invalid_session', $module_name, 'read', 'no_access', $error)) {
            $GLOBALS['log']->error('End: SugarWebServiceImpl->get_module_fields FAILED on checkSessionAndModuleAccess for ' . $module_name);
            return;
        } // if

        $class_name = $beanList[$module_name];
        require_once($beanFiles[$class_name]);
        $seed = new $class_name();
        if ($seed->ACLAccess('ListView', true) || $seed->ACLAccess('DetailView', true) || 	$seed->ACLAccess('EditView', true)) {
            $return = self::$helperObject->get_return_module_fields($seed, $module_name, $fields);
            $GLOBALS['log']->info('End: SugarWebServiceImpl->get_module_fields SUCCESS for ' . $module_name);
            return $return;
        }
        $error->set_error('no_access');
        self::$helperObject->setFaultObject($error);
        $GLOBALS['log']->error('End: SugarWebServiceImpl->get_module_fields FAILED NO ACCESS to ListView, DetailView or EditView for ' . $module_name);
    }

    /**
     * Perform a seamless login. This is used internally during the sync process.
     *
     * @param String $session -- Session ID returned by a previous call to login.
     * @return 1 -- integer - if the session was authenticated
     * @return 0 -- integer - if the session could not be authenticated
     */
    public function seamless_login($session)
    {
        $GLOBALS['log']->info('Begin: SugarWebServiceImpl->seamless_login');
        if (!self::$helperObject->validate_authenticated($session)) {
            return 0;
        }

        $GLOBALS['log']->info('End: SugarWebServiceImpl->seamless_login');
        return 1;
    }

    /**
     * Add or replace the attachment on a Note.
     * Optionally you can set the relationship of this note to Accounts/Contacts and so on by setting related_module_id, related_module_name
     *
     * @param String $session -- Session ID returned by a previous call to login.
     * @param Array 'note' -- Array String 'id' -- The ID of the Note containing the attachment
     *                              String 'filename' -- The file name of the attachment
     *                              Binary 'file' -- The binary contents of the file.
     * 								String 'related_module_id' -- module id to which this note to related to
     * 								String 'related_module_name' - module name to which this note to related to
     *
     * @return Array 'id' -- String - The ID of the Note
     * @exception 'SoapFault' -- The SOAP error, if any
     */
    public function set_note_attachment($session, $note)
    {
        $GLOBALS['log']->info('Begin: SugarWebServiceImpl->set_note_attachment');
        $error = new SoapError();
        $module_name = '';
        $module_access = '';
        $module_id = '';
        if (!empty($note['related_module_id']) && !empty($note['related_module_name'])) {
            $module_name = $note['related_module_name'];
            $module_id = $note['related_module_id'];
            $module_access = 'read';
        }
        if (!self::$helperObject->checkSessionAndModuleAccess($session, 'invalid_session', $module_name, $module_access, 'no_access', $error)) {
            $GLOBALS['log']->info('End: SugarWebServiceImpl->set_note_attachment');
            return;
        } // if

        require_once('modules/Notes/NoteSoap.php');
        $ns = new NoteSoap();
        $GLOBALS['log']->info('End: SugarWebServiceImpl->set_note_attachment');
        return array('id'=>$ns->newSaveFile($note));
    } // fn

    /**
     * Retrieve an attachment from a note
     * @param String $session -- Session ID returned by a previous call to login.
     * @param String $id -- The ID of the appropriate Note.
     * @return Array 'note_attachment' -- Array String 'id' -- The ID of the Note containing the attachment
     *                                          String 'filename' -- The file name of the attachment
     *                                          Binary 'file' -- The binary contents of the file.
     * 											String 'related_module_id' -- module id to which this note is related
     * 											String 'related_module_name' - module name to which this note is related
     * @exception 'SoapFault' -- The SOAP error, if any
     */
    public function get_note_attachment($session, $id)
    {
        $GLOBALS['log']->info('Begin: SugarWebServiceImpl->get_note_attachment');
        $error = new SoapError();
        if (!self::$helperObject->checkSessionAndModuleAccess($session, 'invalid_session', '', '', '', $error)) {
            $GLOBALS['log']->info('End: SugarWebServiceImpl->get_note_attachment');
            return;
        } // if
        require_once('modules/Notes/Note.php');
        $note = BeanFactory::newBean('Notes');

        $note->retrieve($id);
        if (!self::$helperObject->checkACLAccess($note, 'DetailView', $error, 'no_access')) {
            $GLOBALS['log']->info('End: SugarWebServiceImpl->get_note_attachment');
            return;
        } // if

        require_once('modules/Notes/NoteSoap.php');
        $ns = new NoteSoap();
        if (!isset($note->filename)) {
            $note->filename = '';
        }
        $file= $ns->retrieveFile($id, $note->filename);
        if ($file == -1) {
            $file = '';
        }

        $GLOBALS['log']->info('End: SugarWebServiceImpl->get_note_attachment');
        return array('note_attachment'=>array('id'=>$id, 'filename'=>$note->filename, 'file'=>$file, 'related_module_id' => $note->parent_id, 'related_module_name' => $note->parent_type));
    } // fn

    /**
     * sets a new revision for this document
     *
     * @param String $session -- Session ID returned by a previous call to login.
     * @param Array $document_revision -- Array String 'id' -- 	The ID of the document object
     * 											String 'document_name' - The name of the document
     * 											String 'revision' - The revision value for this revision
     *                                         	String 'filename' -- The file name of the attachment
     *                                          String 'file' -- The binary contents of the file.
     * @return Array - 'id' - String - document revision id
     * @exception 'SoapFault' -- The SOAP error, if any
     */
    public function set_document_revision($session, $document_revision)
    {
        $GLOBALS['log']->info('Begin: SugarWebServiceImpl->set_document_revision');
        $error = new SoapError();
        if (!self::$helperObject->checkSessionAndModuleAccess($session, 'invalid_session', '', '', '', $error)) {
            $GLOBALS['log']->info('End: SugarWebServiceImpl->set_document_revision');
            return;
        } // if

        require_once('modules/Documents/DocumentSoap.php');
        $dr = new DocumentSoap();
        $GLOBALS['log']->info('End: SugarWebServiceImpl->set_document_revision');
        return array('id'=>$dr->saveFile($document_revision));
    }

    /**
     * This method is used as a result of the .htaccess lock down on the cache directory. It will allow a
     * properly authenticated user to download a document that they have proper rights to download.
     *
     * @param String $session -- Session ID returned by a previous call to login.
     * @param String $id      -- ID of the document revision to obtain
     * @return new_return_document_revision - Array String 'id' -- The ID of the document revision containing the attachment
     * 												String document_name - The name of the document
     * 												String revision - The revision value for this revision
     *                                         		String 'filename' -- The file name of the attachment
     *                                          	Binary 'file' -- The binary contents of the file.
     * @exception 'SoapFault' -- The SOAP error, if any
     */
    public function get_document_revision($session, $id)
    {
        $GLOBALS['log']->info('Begin: SugarWebServiceImpl->get_document_revision');
        global $sugar_config;

        $error = new SoapError();
        if (!self::$helperObject->checkSessionAndModuleAccess($session, 'invalid_session', '', '', '', $error)) {
            $GLOBALS['log']->info('End: SugarWebServiceImpl->get_document_revision');
            return;
        } // if

        require_once('modules/DocumentRevisions/DocumentRevision.php');
        $dr = BeanFactory::newBean('DocumentRevisions');
        $dr->retrieve($id);
        if (!empty($dr->filename)) {
            $filename = "upload://{$dr->id}";
            if (filesize($filename) > 0) {
                $contents = sugar_file_get_contents($filename);
            } else {
                $contents = '';
            }
            $contents = base64_encode($contents);
            $GLOBALS['log']->info('End: SugarWebServiceImpl->get_document_revision');
            return array('document_revision'=>array('id' => $dr->id, 'document_name' => $dr->document_name, 'revision' => $dr->revision, 'filename' => $dr->filename, 'file' => $contents));
        }
        $error->set_error('no_records');
        self::$helperObject->setFaultObject($error);
        $GLOBALS['log']->info('End: SugarWebServiceImpl->get_document_revision');
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
     * @return Array 'entry_list' -- Array('Accounts' => array(array('name' => 'first_name', 'value' => 'John', 'name' => 'last_name', 'value' => 'Do')))
     * @exception 'SoapFault' -- The SOAP error, if any
     */
    public function search_by_module($session, $search_string, $modules, $offset, $max_results)
    {
        $GLOBALS['log']->info('Begin: SugarWebServiceImpl->search_by_module');
        global  $beanList, $beanFiles;
        global $sugar_config,$current_language;

        $error = new SoapError();
        $output_list = array();
        if (!self::$helperObject->checkSessionAndModuleAccess($session, 'invalid_session', '', '', '', $error)) {
            $error->set_error('invalid_login');
            $GLOBALS['log']->info('End: SugarWebServiceImpl->search_by_module');
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
        if (!file_exists($cachedfile = sugar_cached('modules/unified_search_modules.php'))) {
            $usa->buildCache();
        }

        include($cachedfile);
        $modules_to_search = array();
        $unified_search_modules['Users'] =   array('fields' => array());

        $unified_search_modules['ProjectTask'] =   array('fields' => array());

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
                    $searchForm = new SearchForm($seed, $name) ;

                    $searchForm->setup(array($name => array()), $unifiedSearchFields, '', 'saved_views' /* hack to avoid setup doing further unwanted processing */) ;
                    $where_clauses = $searchForm->generateSearchWhere() ;
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
                    if (file_exists('custom/modules/'.$seed->module_dir.'/metadata/listviewdefs.php')) {
                        require_once('custom/modules/'.$seed->module_dir.'/metadata/listviewdefs.php');
                    } else {
                        require_once('modules/'.$seed->module_dir.'/metadata/listviewdefs.php');
                    }
                    $filterFields = array();
                    foreach ($listViewDefs[$seed->module_dir] as $colName => $param) {
                        if (!empty($param['default']) && $param['default'] == true) {
                            $filterFields[] = strtolower($colName);
                        } // if
                    } // foreach

                if (!in_array('id', $filterFields)) {
                    $filterFields[] = 'id';
                } // if
                    $ret_array = $seed->create_new_list_query('', $where, $filterFields, array(), 0, '', true, $seed, true);
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
                        // $search_string gets cleaned above, so we can use it here
                        $filterFields = array('id', 'user_name', 'first_name', 'last_name', 'email_address');
                        $main_query = "select users.id, ea.email_address, users.user_name, first_name, last_name from users ";
                        $main_query = $main_query . " LEFT JOIN email_addr_bean_rel eabl ON eabl.bean_module = '{$seed->module_dir}'
LEFT JOIN email_addresses ea ON (ea.id = eabl.email_address_id) ";
                        $main_query = $main_query . "where ((users.first_name like '{$search_string}') or (users.last_name like '{$search_string}') or (users.user_name like '{$search_string}') or (ea.email_address like '{$search_string}')) and users.deleted = 0 and users.is_group = 0 and users.employee_status = 'Active'";
                    } // if
                    if ($beanName == "ProjectTask") {
                        // $search_string gets cleaned above, so we can use it here
                        $filterFields = array('id', 'name', 'project_id', 'project_name');
                        $main_query = "select {$seed->table_name}.project_task_id id,{$seed->table_name}.project_id, {$seed->table_name}.name, project.name project_name from {$seed->table_name} ";
                        $seed->add_team_security_where_clause($main_query);
                        $main_query .= "LEFT JOIN teams ON $seed->table_name.team_id=teams.id AND (teams.deleted=0) ";
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
     * Retrieve the list of available modules on the system available to the currently logged in user.
     *
     * @param String $session -- Session ID returned by a previous call to login.
     * @return Array    'modules' -- Array - An array of module names
     * @exception 'SoapFault' -- The SOAP error, if any
     */
    public function get_available_modules($session)
    {
        $GLOBALS['log']->info('Begin: SugarWebServiceImpl->get_available_modules');

        $error = new SoapError();
        if (!self::$helperObject->checkSessionAndModuleAccess($session, 'invalid_session', '', '', '', $error)) {
            $error->set_error('invalid_login');
            $GLOBALS['log']->info('End: SugarWebServiceImpl->get_available_modules');
            return;
        } // if

        $modules = array();
        $modules = array_keys($_SESSION['avail_modules']);

        $GLOBALS['log']->info('End: SugarWebServiceImpl->get_available_modules');
        return array('modules'=> $modules);
    } // fn


    /**
    *   Once we have successfully done a mail merge on a campaign, we need to notify Sugar of the targets
    *   and the campaign_id for tracking purposes
    *
    * @param String session  -- Session ID returned by a previous call to login.
    * @param Array targets   -- a string array of ids identifying the targets used in the merge
    * @param String campaign_id  --  the campaign_id used for the merge
    * @return - No output
    *
    * @exception 'SoapFault' -- The SOAP error, if any
    */
    public function set_campaign_merge($session, $targets, $campaign_id)
    {
        $GLOBALS['log']->info('Begin: SugarWebServiceImpl->set_campaign_merge');

        $error = new SoapError();
        if (!self::$helperObject->checkSessionAndModuleAccess($session, 'invalid_session', '', '', '', $error)) {
            $error->set_error('invalid_login');
            $GLOBALS['log']->info('End: SugarWebServiceImpl->set_campaign_merge');
            return;
        } // if
        if (empty($campaign_id) || !is_array($targets) || count($targets) == 0) {
            $error->set_error('invalid_set_campaign_merge_data');
            self::$helperObject->setFaultObject($error);
            $GLOBALS['log']->debug('set_campaign_merge: Merge action status will not be updated, because, campaign_id is null or no targets were selected.');
            $GLOBALS['log']->info('End: SugarWebServiceImpl->set_campaign_merge');
            return;
        }
        require_once('modules/Campaigns/utils.php');
        campaign_log_mail_merge($campaign_id, $targets);
        // else
    } // fn
/**
*   Retrieve number of records in a given module
*
* @param String session      -- Session ID returned by a previous call to login.
* @param String module_name  -- module to retrieve number of records from
* @param String query        -- allows webservice user to provide a WHERE clause
* @param int deleted         -- specify whether or not to include deleted records
*
* @return Array  result_count - integer - Total number of records for a given module and query
* @exception 'SoapFault' -- The SOAP error, if any
*/
    public function get_entries_count($session, $module_name, $query, $deleted)
    {
        $GLOBALS['log']->info('Begin: SugarWebServiceImpl->get_entries_count');

        $error = new SoapError();
        if (!self::$helperObject->checkSessionAndModuleAccess($session, 'invalid_session', $module_name, 'list', 'no_access', $error)) {
            $GLOBALS['log']->info('End: SugarWebServiceImpl->get_entries_count');
            return;
        } // if

        global $beanList, $beanFiles, $current_user;

        $class_name = $beanList[$module_name];
        require_once($beanFiles[$class_name]);
        $seed = new $class_name();

        if (!self::$helperObject->checkQuery($error, $query)) {
            $GLOBALS['log']->info('End: SugarWebServiceImpl->get_entries_count');
            return;
        } // if

        if (!self::$helperObject->checkACLAccess($seed, 'ListView', $error, 'no_access')) {
            return;
        }

        $sql = 'SELECT COUNT(*) result_count FROM ' . $seed->table_name . ' ';


        $customJoin = $seed->getCustomJoin();
        $sql .= $customJoin['join'];

        // build WHERE clauses, if any
        $where_clauses = array();
        if (!empty($query)) {
            $where_clauses[] = $query;
        }
        if ($deleted == 0) {
            $where_clauses[] = $seed->table_name . '.deleted = 0';
        }

        // if WHERE clauses exist, add them to query
        if (!empty($where_clauses)) {
            $sql .= ' WHERE ' . implode(' AND ', $where_clauses);
        }

        $res = DBManagerFactory::getInstance()->query($sql);
        $row = DBManagerFactory::getInstance()->fetchByAssoc($res);

        $GLOBALS['log']->info('End: SugarWebServiceImpl->get_entries_count');
        return array(
        'result_count' => $row['result_count'],
    );
    }
} // clazz
