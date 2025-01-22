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


class SugarWidgetFieldEnum extends SugarWidgetReportField
{
    public function __construct($layout_manager)
    {
        parent::__construct($layout_manager);
    }

    public function queryFilterEmpty($layout_def)
    {
        $column = $this->_get_column_select($layout_def);
        return "(coalesce(" . $this->reporter->db->convert($column, "length") . ",0) = 0 OR $column = '^^')";
    }

    public function queryFilterNot_Empty($layout_def)
    {
        $column = $this->_get_column_select($layout_def);
        return "(coalesce(" . $this->reporter->db->convert($column, "length") . ",0) > 0 AND $column != '^^' )\n";
    }

    public function queryFilteris($layout_def)
    {
        $input_name0 = $layout_def['input_name0'];
        if (is_array($layout_def['input_name0'])) {
            $input_name0 = $layout_def['input_name0'][0];
        }
        return $this->_get_column_select($layout_def)." = ".$this->reporter->db->quoted($input_name0)."\n";
    }

    public function queryFilteris_not($layout_def)
    {
        $input_name0 = $layout_def['input_name0'];
        if (is_array($layout_def['input_name0'])) {
            $input_name0 = $layout_def['input_name0'][0];
        }
        return $this->_get_column_select($layout_def)." <> ".$this->reporter->db->quoted($input_name0)."\n";
    }

    public function queryFilterone_of($layout_def, $rename_columns = true)
    {
        $arr = array();
        foreach ($layout_def['input_name0'] as $value) {
            $arr[] = $this->reporter->db->quoted($value);
        }
        $str = implode(",", $arr);
        return $this->_get_column_select($layout_def)." IN (".$str.")\n";
    }

    public function queryFilternot_one_of($layout_def)
    {
        $arr = array();
        foreach ($layout_def['input_name0'] as $value) {
            $arr[] = $this->reporter->db->quoted($value);
        }
        $reporter = $this->layout_manager->getAttribute("reporter");
        $str = implode(",", $arr);
        return $this->_get_column_select($layout_def)." NOT IN (".$str.")\n";
    }

    public function & displayList(&$layout_def)
    {
        $field_def = [];
        if (!empty($layout_def['column_key'])) {
            $field_def = $this->reporter->all_fields[$layout_def['column_key']];
        } else {
            if (!empty($layout_def['fields'])) {
                $field_def = $layout_def['fields'];
            }
        }
        $cell = $this->displayListPlain($layout_def);
        $str = $cell;
        global $sugar_config;
        if (isset($sugar_config['enable_inline_reports_edit']) && $sugar_config['enable_inline_reports_edit']) {
            $module = $this->reporter->all_fields[$layout_def['column_key']]['module'];
            $name = $layout_def['name'];
            $layout_def['name'] = 'id';
            $key = $this->_get_column_alias($layout_def);
            $key = strtoupper($key);

            //If the key isn't in the layout fields, skip it
            if (!empty($layout_def['fields'][$key])) {
                $record = $layout_def['fields'][$key];
                $field_name = $field_def['name'];
                $field_type = $field_def['type'];
                $div_id = $field_def['module'] ."&$record&$field_name";
                $str = "<div id='$div_id'>" . $cell . "&nbsp;"
                     . SugarThemeRegistry::current()->getImage(
                         "edit_inline",
                         "border='0' alt='Edit Layout' align='bottom' onClick='SUGAR.reportsInlineEdit.inlineEdit(" .
                        "\"$div_id\",\"$cell\",\"$module\",\"$record\",\"$field_name\",\"$field_type\");'"
                       )
                     . "</div>";
            }
        }
        return $str;
    }
    public function & displayListPlain($layout_def)
    {
        $field_def = [];
        if (!empty($layout_def['column_key'])) {
            $field_def = $this->reporter->all_fields[$layout_def['column_key']];
        } else {
            if (!empty($layout_def['fields'])) {
                $field_def = $layout_def['fields'];
            }
        }

        $value = '';
        if (!empty($layout_def['table_key']) &&(empty($field_def['fields']) || empty($field_def['fields'][0]) || empty($field_def['fields'][1]))) {
            $value = $this->_get_list_value($layout_def);
        } else {
            if (!empty($layout_def['name']) && !empty($layout_def['fields'])) {
                $key = strtoupper($layout_def['name']);
                $value = $layout_def['fields'][$key];
            }
        }
        $cell = '';

        if (isset($field_def['options'])) {
            $cell = translate($field_def['options'], $field_def['module'], $value);
        } else {
            if (isset($field_def['type']) && $field_def['type'] == 'enum' && isset($field_def['function'])) {
                global $beanFiles;
                if (empty($beanFiles)) {
                    include('include/modules.php');
                }
                $bean_name = get_singular_bean_name($field_def['module']);
                require_once($beanFiles[$bean_name]);
                $list = $field_def['function']();
                $cell = $list[$value];
            }
        }
        if (is_array($cell)) {

            //#22632
            $value = unencodeMultienum($value);
            $cell=array();
            foreach ($value as $val) {
                $returnVal = translate($field_def['options'], $field_def['module'], $val);
                if (!is_array($returnVal)) {
                    array_push($cell, translate($field_def['options'], $field_def['module'], $val));
                }
            }
            $cell = implode(", ", $cell);
        }
        return $cell;
    }

    public function queryOrderBy($layout_def)
    {
        $field_def = $this->reporter->all_fields[$layout_def['column_key']];
        if (!empty($field_def['sort_on'])) {
            $order_by = $layout_def['table_alias'].".".$field_def['sort_on'];
        } else {
            $order_by = $this->_get_column_select($layout_def);
        }
        $list = array();
        if (isset($field_def['options'])) {
            $list = translate($field_def['options'], $field_def['module']);
        } elseif (isset($field_def['type']) && $field_def['type'] == 'enum' && isset($field_def['function'])) {
            global $beanFiles;
            if (empty($beanFiles)) {
                include('include/modules.php');
            }
            $bean_name = get_singular_bean_name($field_def['module']);
            require_once($beanFiles[$bean_name]);
            $list = $field_def['function']();
        }
        if (empty($layout_def['sort_dir']) || $layout_def['sort_dir'] == 'a') {
            $order_dir = "ASC";
        } else {
            $order_dir = "DESC";
        }
        return $this->reporter->db->orderByEnum($order_by, $list, $order_dir);
    }

    public function displayInput($layout_def)
    {
        global $app_list_strings;

        if (!empty($layout_def['remove_blank']) && $layout_def['remove_blank']) {
            if (isset($layout_def['options']) &&  is_array($layout_def['options'])) {
                $ops = $layout_def['options'];
            } elseif (isset($layout_def['options']) && isset($app_list_strings[$layout_def['options']])) {
                $ops = $app_list_strings[$layout_def['options']];
                if (array_key_exists('', $app_list_strings[$layout_def['options']])) {
                    unset($ops['']);
                }
            } else {
                $ops = array();
            }
        } else {
            $ops = $app_list_strings[$layout_def['options']];
        }

        $str = '<select multiple="true" size="3" name="' . $layout_def['name'] . '[]">';
        $str .= get_select_options_with_id($ops, $layout_def['input_name0']);
        $str .= '</select>';
        return $str;
    }
}
