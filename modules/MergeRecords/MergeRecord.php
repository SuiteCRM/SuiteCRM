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
 * Class MergeRecord
 */
#[\AllowDynamicProperties]
class MergeRecord extends SugarBean
{
    public $object_name = 'MergeRecord';
    public $module_dir = 'MergeRecords';
    public $acl_display_only = true;
    public $merge_module;
    public $merge_bean_class;
    public $merge_bean_file_path;

    public $merge_module2;
    public $merge_bean_class2;
    public $merge_bean_file_path2;

    public $master_id;

    //these arrays store the fields and params to search on
    public $field_search_params = array();

    //this is a object for the bean you are merging on
    public $merge_bean;
    public $merge_bean2;

    //store a copy of the merge bean related strings
    public $merge_bean_strings = array();
    public $merge_bean_strings2 = array();

    /**
     * MergeRecord constructor.
     *
     * @param string $merge_module
     * @param string $merge_id
     */
    public function __construct($merge_module = '', $merge_id = '')
    {
        if ($merge_module != '') {
            $this->load_merge_bean($merge_module, $merge_id);
        }
    }

    /**
     * @param int $id
     * @param bool $encode
     * @param bool $deleted
     * @return SugarBean|void
     */
    public function retrieve($id = -1, $encode = true, $deleted = true)
    {
        if (isset($_REQUEST['action']) && $_REQUEST['action'] == 'Step2') {
            $this->load_merge_bean($this->merge_bean, false, $id);
        } else {
            parent::retrieve($id, $encode, $deleted);
        }

        return $this;
    }

    /**
     * @param $merge_module
     * @param bool $load_module_strings
     * @param string $merge_id
     */
    public function load_merge_bean($merge_module, $load_module_strings = false, $merge_id = '')
    {
        global $beanList;
        global $beanFiles;
        global $current_language;

        $this->merge_module = $merge_module;
        $this->merge_bean_class = $beanList[$this->merge_module];
        $this->merge_bean_file_path = $beanFiles[$this->merge_bean_class];

        require_once $this->merge_bean_file_path;
        $this->merge_bean = new $this->merge_bean_class();
        if ($merge_id != '') {
            $this->merge_bean->retrieve($merge_id);
        }

        // Bug 18853 - Disable this view if the user doesn't have edit and delete permissions
        if (!$this->merge_bean->ACLAccess('edit') || !$this->merge_bean->ACLAccess('delete')) {
            ACLController::displayNoAccess();
            sugar_die('');
        }

        //load master module strings
        if ($load_module_strings) {
            $this->merge_bean_strings = return_module_language($current_language, $merge_module);
        }
    }

    /**
     * @param $merge_module
     * @param bool $load_module_strings
     * @param string $merge_id
     */
    public function load_merge_bean2($merge_module, $load_module_strings = false, $merge_id = '')
    {
        global $beanList;
        global $beanFiles;
        global $current_language;

        $this->merge_module2 = $merge_module;
        $this->merge_bean_class2 = $beanList[$this->merge_module2];
        $this->merge_bean_file_path2 = $beanFiles[$this->merge_bean_class2];

        require_once $this->merge_bean_file_path2;
        $this->merge_bean2 = new $this->merge_bean_class2();
        if ($merge_id != '') {
            $this->merge_bean2->retrieve($merge_id);
        }
        //load master module strings
        if ($load_module_strings) {
            $this->merge_bean_strings2 = return_module_language($current_language, $merge_module);
        }
    }

    public $new_schema = true;

    //-----------------------------------------------------------------------
    //-------------Wrapping Necessary Merge Bean Calls-----------------------
    //-----------------------------------------------------------------------

    public function fill_in_additional_list_fields()
    {
        $this->merge_bean->fill_in_additional_list_fields();
    }

    public function fill_in_additional_detail_fields()
    {
        $this->merge_bean->fill_in_additional_detail_fields();
    }

    /**
     * @return string
     */
    public function get_summary_text()
    {
        return $this->merge_bean->get_summary_text();
    }

    /**
     * @return array
     */
    public function get_list_view_data()
    {
        return $this->merge_bean->get_list_view_data();
    }

    /**
     * do not include any $this-> because this is called on without having the class instantiated
     *
     * @param $the_query_string
     * @return string
     */
    public function build_generic_where_clause($the_query_string)
    {
        return $this->merge_bean->build_generic_where_clause($the_query_string);
    }

    /**
     * @param $interface
     * @return bool
     */
    public function bean_implements($interface)
    {
        switch ($interface) {
            case 'ACL':
                return true;
            default:
                return false;
        }
    }

    /**
     * @param string $view
     * @param string $is_owner
     * @param string $in_group
     * @return bool
     */
    public function ACLAccess($view, $is_owner = 'not_set', $in_group = 'not_set')
    {
        if (empty($this->merge_bean) || !$this->merge_bean->bean_implements('ACL')) {
            return true;
        }

        return $this->merge_bean->ACLAccess($view, $is_owner, $in_group);
    }

    /**
     * @param $search_params
     */
    public function populate_search_params($search_params)
    {
        foreach ($this->merge_bean->field_defs as $key => $value) {
            $searchFieldString = $key.'SearchField';
            $searchTypeString = $key.'SearchType';

            if (isset($search_params[$searchFieldString])) {
                if (isset($search_params[$searchFieldString]) == '') {
                    $this->field_search_params[$key]['value'] = 'NULL';
                } else {
                    $this->field_search_params[$key]['value'] = $search_params[$searchFieldString];
                }
                if (isset($search_params[$searchTypeString])) {
                    $this->field_search_params[$key]['search_type'] = $search_params[$searchTypeString];
                } else {
                    $this->field_search_params[$key]['search_type'] = 'Exact';
                }
                //add field_def to the array.
                $this->field_search_params[$key] = array_merge($value, $this->field_search_params[$key]);
            }
        }
    }

    /**
     * @param $search_params
     * @return string
     */
    public function get_inputs_for_search_params($search_params)
    {
        $returnString = '';
        foreach ($this->merge_bean->field_defs as $key => $value) {
            $searchFieldString = $key.'SearchField';
            $searchTypeString = $key.'SearchType';

            if (isset($search_params[$searchFieldString])) {
                $searchParamSearchTypeString = null;
                if (isset($search_params[$searchTypeString])) {
                    $searchParamSearchTypeString = $search_params[$searchTypeString];
                } else {
                    LoggerManager::getLogger('MergeRecord::get_inputs_for_search_params: search params type string is not defined');
                }
                
                $returnString .= "<input type='hidden' name='$searchFieldString' value='{$search_params[$searchFieldString]}' />\n";
                $returnString .= "<input type='hidden' name='$searchTypeString' value='{$searchParamSearchTypeString}' />\n";
            }
        }

        return $returnString;
    }

    /**
     * @param $table
     * @param $module
     * @param $bean_id
     * @return string
     */
    public function email_addresses_query($table, $module, $bean_id)
    {
        return $table.".id IN (SELECT ear.bean_id FROM email_addresses ea
                                LEFT JOIN email_addr_bean_rel ear ON ea.id = ear.email_address_id
                                WHERE ear.bean_module = '{$module}'
                                AND ear.bean_id != '{$bean_id}'
                                AND ear.deleted = 0";
    }

    /**
     * @param $search_type
     * @param $value
     * @return string
     */
    public function release_name_query($search_type, $value)
    {
        $this->load_merge_bean2('Releases');
        if ($search_type == 'like') {
            $where = "releases.name LIKE '%".DBManagerFactory::getInstance()->quote($value)."%'";
        } elseif ($search_type == 'start') {
            $where = "releases.name LIKE '".DBManagerFactory::getInstance()->quote($value)."%'";
        } else {
            $where = "releases.name = '".DBManagerFactory::getInstance()->quote($value)."'";
        }
        $list = $this->merge_bean2->get_releases(false, 'Active', $where);
        $list_to_join = array();
        foreach ($list as $key => $value) {
            $list_to_join[] = "'".DBManagerFactory::getInstance()->quote($key)."'";
        }

        return implode(', ', $list_to_join);
    }

    /**
     * @return array
     */
    public function create_where_statement()
    {
        $where_clauses = array();
        foreach ($this->field_search_params as $merge_field => $vDefArray) {
            if (isset($vDefArray['source']) && $vDefArray['source'] == 'custom_fields') {
                $table_name = $this->merge_bean->table_name.'_cstm';
            } else {
                $table_name = $this->merge_bean->table_name;
            }

            //Should move these if's into a central location for extensibility and addition for other search filters
            //Must do the same for pulling values in js dropdown
            if (isset($vDefArray['search_type']) && $vDefArray['search_type'] == 'like') {
                if ($merge_field != 'email1' && $merge_field != 'email2' && $merge_field != 'release_name') {
                    if ($vDefArray['value'] != '') {
                        array_push($where_clauses, $table_name.'.'.$merge_field." LIKE '%".DBManagerFactory::getInstance()->quote($vDefArray['value'])."%'");
                    }
                } elseif ($merge_field == 'release_name') {
                    if (isset($vDefArray['value'])) {
                        $in = $this->release_name_query('like', $vDefArray['value']);
                        array_push($where_clauses, $table_name.".found_in_release IN ($in)");
                    }
                } else {
                    $query = $this->email_addresses_query($table_name, $this->merge_module, $this->merge_bean->id);
                    $query .= " AND ea.email_address LIKE '%".DBManagerFactory::getInstance()->quote($vDefArray['value'])."%')";
                    $where_clauses[] = $query;
                }
            } elseif (isset($vDefArray['search_type']) && $vDefArray['search_type'] == 'start') {
                if ($merge_field != 'email1' && $merge_field != 'email2' && $merge_field != 'release_name') {
                    array_push($where_clauses, $table_name.'.'.$merge_field." LIKE '".DBManagerFactory::getInstance()->quote($vDefArray['value'])."%'");
                } elseif ($merge_field == 'release_name') {
                    if (isset($vDefArray['value'])) {
                        $in = $this->release_name_query('start', $vDefArray['value']);
                        array_push($where_clauses, $table_name.".found_in_release IN ($in)");
                    }
                } else {
                    $query = $this->email_addresses_query($table_name, $this->merge_module, $this->merge_bean->id);
                    $query .= " AND ea.email_address LIKE '".DBManagerFactory::getInstance()->quote($vDefArray['value'])."%')";
                    $where_clauses[] = $query;
                }
            } else {
                if ($merge_field != 'email1' && $merge_field != 'email2' && $merge_field != 'release_name') {
                    array_push($where_clauses, $table_name.'.'.$merge_field."='".DBManagerFactory::getInstance()->quote($vDefArray['value'])."'");
                } elseif ($merge_field == 'release_name') {
                    if (isset($vDefArray['value'])) {
                        $in = $this->release_name_query('exact', $vDefArray['value']);
                        array_push($where_clauses, $table_name.".found_in_release IN ($in)");
                    }
                } else {
                    $query = $this->email_addresses_query($table_name, $this->merge_module, $this->merge_bean->id);
                    $query .= " AND ea.email_address = '".DBManagerFactory::getInstance()->quote($vDefArray['value'])."')";
                    $where_clauses[] = $query;
                }
            }
        }
        // Add ACL Check
        $accessWhere = $this->merge_bean->buildAccessWhere('delete');
        if (!empty($accessWhere)) {
            $where_clauses[] = $accessWhere;
        }
        array_push($where_clauses, $this->merge_bean->table_name.".id !='".DBManagerFactory::getInstance()->quote($this->merge_bean->id)."'");

        return $where_clauses;
    }

    /**
     * duplicating utils function for now for possibility of future or/and and other functionality
     *
     * @param $where_clauses
     * @return string
     */
    public function generate_where_statement($where_clauses)
    {
        $where = '';

        foreach ($where_clauses as $clause) {
            if ($where != '') {
                $where .= ' AND ';
            }
            $where .= $clause;
        }

        return $where;
    }
}
