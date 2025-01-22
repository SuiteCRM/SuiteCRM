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
 * Vardef Handler Object
 * @api
 */
#[\AllowDynamicProperties]
class VarDefHandler
{
    public $meta_array_name;
    public $target_meta_array = false;
    public $start_none = false;
    public $extra_array = array();					//used to add custom items
    public $options_array = array();
    public $module_object;
    public $start_none_lbl = null;


    public function __construct($module, $meta_array_name=null)
    {
        $this->meta_array_name = $meta_array_name;
        $this->module_object = $module;
        if ($meta_array_name!=null) {
            global $vardef_meta_array;
            include("include/VarDefHandler/vardef_meta_arrays.php");
            $this->target_meta_array = $vardef_meta_array[$meta_array_name];
        }

        //end function setup
    }

    public function get_vardef_array($use_singular=false, $remove_dups = false, $use_field_name = false, $use_field_label = false)
    {
        global $dictionary;
        global $current_language;
        global $app_strings;
        global $app_list_strings;

        $temp_module_strings = return_module_language($current_language, $this->module_object->module_dir);

        $base_array = $this->module_object->field_defs;
        //$base_array = $dictionary[$this->module_object->object_name]['fields'];

        ///Inclue empty none set or not
        if ($this->start_none==true) {
            if (!empty($this->start_none_lbl)) {
                $this->options_array[''] = $this->start_none_lbl;
            } else {
                $this->options_array[''] = $app_strings['LBL_NONE'];
            }
        }

        ///used for special one off items added to filter array	 ex. would be href link for alert templates
        if (!empty($this->extra_array)) {
            foreach ($this->extra_array as $key => $value) {
                $this->options_array[$key] = $value;
            }
        }
        /////////end special one off//////////////////////////////////


        foreach ($base_array as $key => $value_array) {
            $compare_results = $this->compare_type($value_array);

            if ($compare_results == true) {
                if ($value_array['type'] == 'link' && !$use_field_label) {
                    $relName = $value_array['name'];
                    $this->module_object->load_relationship($relName);
                    if (!empty($app_list_strings['moduleList'][$this->module_object->$relName->getRelatedModuleName()])) {
                        $label_name = $app_list_strings['moduleList'][$this->module_object->$relName->getRelatedModuleName()];
                    } else {
                        $label_name = $this->module_object->$relName->getRelatedModuleName();
                    }
                } else {
                    if (!empty($value_array['vname'])) {
                        $label_name = $value_array['vname'];
                    } else {
                        $label_name = $value_array['name'];
                    }
                }


                $label_name = get_label($label_name, $temp_module_strings);

                if (!empty($value_array['table'])) {
                    //Custom Field
                    $column_table = $value_array['table'];
                } else {
                    //Non-Custom Field
                    $column_table = $this->module_object->table_name;
                }

                if ($value_array['type'] == 'link') {
                    if ($use_field_name) {
                        $index = $value_array['name'];
                    } else {
                        $index = $this->module_object->$key->getRelatedModuleName();
                    }
                } else {
                    $index = $key;
                }

                $value = trim($label_name, ':');
                if ($remove_dups) {
                    if (!in_array($value, $this->options_array)) {
                        $this->options_array[$index] = $value;
                    }
                } else {
                    $this->options_array[$index] = $value;
                }

                //end if field is included
            }

            //end foreach
        }
        if ($use_singular == true) {
            return convert_module_to_singular($this->options_array);
        } else {
            return $this->options_array;
        }

        //end get_vardef_array
    }


    public function compare_type($value_array)
    {

        //Filter nothing?
        if (!is_array($this->target_meta_array)) {
            return true;
        }

        ////////Use the $target_meta_array;
        if (isset($this->target_meta_array['inc_override'])) {
            foreach ($this->target_meta_array['inc_override'] as $attribute => $value) {
                foreach ($value as $actual_value) {
                    if (isset($value_array[$attribute]) && $value_array[$attribute] == $actual_value) {
                        return true;
                    }
                }
                if (isset($value_array[$attribute]) && $value_array[$attribute] == $value) {
                    return true;
                }
            }
        }
        if (isset($this->target_meta_array['ex_override'])) {
            foreach ($this->target_meta_array['ex_override'] as $attribute => $value) {
                foreach ($value as $actual_value) {
                    if (isset($value_array[$attribute]) && $value_array[$attribute] == $actual_value) {
                        return false;
                    }

                    if (isset($value_array[$attribute]) && $value_array[$attribute] == $value) {
                        return false;
                    }
                }

                //end foreach inclusion array
            }
        }

        if (isset($this->target_meta_array['inclusion'])) {
            foreach ($this->target_meta_array['inclusion'] as $attribute => $value) {
                if ($attribute=="type") {
                    foreach ($value as $actual_value) {
                        if (isset($value_array[$attribute]) && $value_array[$attribute] != $actual_value) {
                            return false;
                        }
                    }
                } else {
                    if (isset($value_array[$attribute]) && $value_array[$attribute] != $value) {
                        return false;
                    }
                }
                //end foreach inclusion array
            }
        }

        if (isset($this->target_meta_array['exclusion'])) {
            foreach ($this->target_meta_array['exclusion'] as $attribute => $value) {
                foreach ($value as $actual_value) {
                    if (isset($value_array[$attribute]) && $value_array[$attribute] == $actual_value) {
                        return false;
                    }
                }

                //end foreach inclusion array
            }
        }


        return true;

        //end function compare_type
    }

    //end class VarDefHandler
}
