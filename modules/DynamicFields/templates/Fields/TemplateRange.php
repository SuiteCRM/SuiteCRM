<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
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


class TemplateRange extends TemplateText
{

    public $enable_range_search;
    /**
     * @var string
     */
    public $options;
    /**
     * __construct
     *
     * Constructor for class.  This constructor ensures that TemplateRanage instances have the
     * enable_range_search vardef value.
     */
    public function __construct()
    {
        $this->vardef_map['enable_range_search'] = 'enable_range_search';
        $this->vardef_map['options'] = 'options';
    }


    /**
     * populateFromPost
     *
     * @see parent::populateFromPost
     * This method checks to see if enable_range_search is set.  If so, ensure that the
     * searchdefs for the module include the additional range fields.
     */
    public function populateFromPost()
    {
        parent::populateFromPost();
        $searchFields = [];
        //If we are enabling range search, make sure we add the start and end range fields
        if (!empty($this->enable_range_search)) {
            //If range search is enabled, set the options attribute for the dropdown choice selections
            $this->options = ($this->type == 'date' || $this->type == 'datetimecombo' || $this->type == 'datetime') ? 'date_range_search_dom' : 'numeric_range_search_dom';

            if (isset($_REQUEST['view_module'])) {
                $module = $_REQUEST['view_module'];
                if (file_exists('modules/'.$module.'/metadata/SearchFields.php')) {
                    require('modules/'.$module.'/metadata/SearchFields.php');
                }

                if (file_exists('custom/modules/'.$module.'/metadata/SearchFields.php')) {
                    require('custom/modules/'.$module.'/metadata/SearchFields.php');
                }

                $field_name = $this->get_field_name($module, $_REQUEST['name']);

                if (isset($searchFields[$module])) {
                    $field_name_range = 'range_' . $field_name;
                    $field_name_start = 'start_range_' . $field_name;
                    $field_name_end = 'end_range_' . $field_name;

                    $isDateField = $this->type == 'date' || $this->type == 'datetimecombo' || $this->type == 'datetime';


                    $searchFields[$module][$field_name_range] = array('query_type'=>'default', 'enable_range_search'=>true);
                    if ($isDateField) {
                        $searchFields[$module][$field_name_range]['is_date_field'] = true;
                    }

                    $searchFields[$module][$field_name_start] = array('query_type'=>'default', 'enable_range_search'=>true);
                    if ($isDateField) {
                        $searchFields[$module][$field_name_start]['is_date_field'] = true;
                    }

                    $searchFields[$module][$field_name_end] = array('query_type'=>'default', 'enable_range_search'=>true);
                    if ($isDateField) {
                        $searchFields[$module][$field_name_end]['is_date_field'] = true;
                    }

                    if (!file_exists('custom/modules/'.$module.'/metadata/SearchFields.php')) {
                        mkdir_recursive('custom/modules/'.$module.'/metadata');
                    }
                    write_array_to_file("searchFields['{$module}']", $searchFields[$module], 'custom/modules/'.$module.'/metadata/SearchFields.php');
                }

                if (file_exists($cachefile = sugar_cached("modules/$module/SearchForm_basic.tpl"))) {
                    unlink($cachefile);
                }

                if (file_exists($cachefile = sugar_cached("modules/$module/SearchForm_advanced.tpl"))) {
                    unlink($cachefile);
                }
            }
        } else {
            //Otherwise, try to restore the searchFields to their state prior to being enabled
            if (isset($_REQUEST['view_module'])) {
                $module = $_REQUEST['view_module'];
                if (file_exists('modules/'.$module.'/metadata/SearchFields.php')) {
                    require('modules/'.$module.'/metadata/SearchFields.php');
                }

                if (file_exists('custom/modules/'.$module.'/metadata/SearchFields.php')) {
                    require('custom/modules/'.$module.'/metadata/SearchFields.php');
                }

                $field_name = $this->get_field_name($module, $_REQUEST['name']);

                if (isset($searchFields[$module])) {
                    $field_name_range = 'range_' . $field_name;
                    $field_name_start = 'start_range_' . $field_name;
                    $field_name_end = 'end_range_' . $field_name;


                    if (isset($searchFields[$module][$field_name_range])) {
                        unset($searchFields[$module][$field_name_range]);
                    }

                    if (isset($searchFields[$module][$field_name_start])) {
                        unset($searchFields[$module][$field_name_start]);
                    }

                    if (isset($searchFields[$module][$field_name_end])) {
                        unset($searchFields[$module][$field_name_end]);
                    }

                    if (!file_exists('custom/modules/'.$module.'/metadata/SearchFields.php')) {
                        mkdir_recursive('custom/modules/'.$module.'/metadata');
                    }
                    write_array_to_file("searchFields['{$module}']", $searchFields[$module], 'custom/modules/'.$module.'/metadata/SearchFields.php');
                }

                if (file_exists($cachefile = sugar_cached("modules/$module/SearchForm_basic.tpl"))) {
                    unlink($cachefile);
                }

                if (file_exists($cachefile = sugar_cached("modules/$module/SearchForm_advanced.tpl"))) {
                    unlink($cachefile);
                }
            }
        }
    }


    /**
     * get_field_def
     *
     * @see parent::get_field_def
     * This method checks to see if the enable_range_search key/value entry should be
     * added to the vardef entry representing the module
     */
    public function get_field_def()
    {
        $vardef = parent::get_field_def();
        if (!empty($this->enable_range_search)) {
            $vardef['enable_range_search'] = $this->enable_range_search;
            $vardef['options'] = ($this->type == 'date' || $this->type == 'datetimecombo' || $this->type == 'datetime') ? 'date_range_search_dom' : 'numeric_range_search_dom';
        } else {
            $vardef['enable_range_search'] = false;
        }
        return $vardef;
    }


    public static function repairCustomSearchFields($vardefs, $module, $package='')
    {
        $fields = array();

        //Find any range search enabled fields
        foreach ($vardefs as $key=>$field) {
            if (!empty($field['enable_range_search'])) {
                $fields[$field['name']] = $field;
            }
        }

        if (!empty($fields)) {
            if (file_exists('custom/modules/'.$module.'/metadata/SearchFields.php')) {
                require('custom/modules/'.$module.'/metadata/SearchFields.php');
            } else {
                if (file_exists('modules/'.$module.'/metadata/SearchFields.php')) {
                    require('modules/'.$module.'/metadata/SearchFields.php');
                } else {
                    if (file_exists('custom/modulebuilder/' . $package . '/modules/' . $module . '/metadata/SearchFields.php')) {
                        require('custom/modulebuilder/' . $package . '/modules/' . $module . '/metadata/SearchFields.php');
                    }
                }
            }

            foreach ($fields as $field_name=>$field) {
                $field_name_range = 'range_' . $field_name;
                $field_name_start = 'start_range_' . $field_name;
                $field_name_end = 'end_range_' . $field_name;

                $type = $field['type'];

                $isDateField = $type == 'date' || $type == 'datetimecombo' || $type == 'datetime';

                $searchFields[$module][$field_name_range] = array('query_type'=>'default', 'enable_range_search'=>true);
                if ($isDateField) {
                    $searchFields[$module][$field_name_range]['is_date_field'] = true;
                }

                $searchFields[$module][$field_name_start] = array('query_type'=>'default', 'enable_range_search'=>true);
                if ($isDateField) {
                    $searchFields[$module][$field_name_start]['is_date_field'] = true;
                }

                $searchFields[$module][$field_name_end] = array('query_type'=>'default', 'enable_range_search'=>true);
                if ($isDateField) {
                    $searchFields[$module][$field_name_end]['is_date_field'] = true;
                }
            }

            if (!file_exists('custom/modules/'.$module.'/metadata/SearchFields.php')) {
                mkdir_recursive('custom/modules/'.$module.'/metadata');
            }

            write_array_to_file("searchFields['{$module}']", $searchFields[$module], 'custom/modules/'.$module.'/metadata/SearchFields.php');
        }
    }
}
