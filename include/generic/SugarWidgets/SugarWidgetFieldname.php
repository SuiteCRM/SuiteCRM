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






#[\AllowDynamicProperties]
class SugarWidgetFieldName extends SugarWidgetFieldVarchar
{
    protected static $moduleSavePermissions = array();
    protected $sugarWidgetFieldId;

    public function __construct(&$layout_manager)
    {
        parent::__construct($layout_manager);
        $this->reporter = $this->layout_manager->getAttribute('reporter');
        $this->sugarWidgetFieldId = new SugarWidgetFieldId($layout_manager);
    }

    public function displayList(&$layout_def)
    {
        $field_def = [];
        if (empty($layout_def['column_key'])) {
            return $this->displayListPlain($layout_def);
        }

        $module = $this->reporter->all_fields[$layout_def['column_key']]['module'];
        $name = $layout_def['name'];
        $layout_def['name'] = 'id';
        $key = $this->_get_column_alias($layout_def);
        $key = strtoupper($key);

        if (empty($layout_def['fields'][$key])) {
            $layout_def['name'] = $name;
            return $this->displayListPlain($layout_def);
        }

        $record = $layout_def['fields'][$key];
        $layout_def['name'] = $name;
        global $current_user;
        if ($module == 'Users' && !is_admin($current_user)) {
            $module = 'Employees';
        }
        $str = "<a target='_blank' href=\"index.php?action=DetailView&module=$module&record=$record\">";
        $str .= $this->displayListPlain($layout_def);
        $str .= "</a>";


        global $sugar_config;
        if (isset($sugar_config['enable_inline_reports_edit']) && $sugar_config['enable_inline_reports_edit'] && !empty($record)) {
            $div_id = "$module&$record&$name";
            $str = "<div id='$div_id'><a target='_blank' href=\"index.php?action=DetailView&module=$module&record=$record\">";
            $value = $this->displayListPlain($layout_def);
            $str .= $value;
            $field_name = $layout_def['name'];
            $field_type = $field_def['type'] ?? '';
            $str .= "</a>";
            if ($field_name == 'name') {
                $str .= "&nbsp;" .SugarThemeRegistry::current()->getImage("edit_inline", "border='0' alt='Edit Layout' align='bottom' onClick='SUGAR.reportsInlineEdit.inlineEdit(\"$div_id\",\"$value\",\"$module\",\"$record\",\"$field_name\",\"$field_type\");'");
            }
            $str .= "</div>";
        }
        return $str;
    }

    public function _get_normal_column_select($layout_def)
    {
        if (isset($this->reporter->all_fields)) {
            $field_def = $this->reporter->all_fields[$layout_def['column_key']];
        } else {
            $field_def = array();
        }

        if (empty($field_def['fields']) || empty($field_def['fields'][0]) || empty($field_def['fields'][1])) {
            return parent::_get_column_select($layout_def);
        }

        //	 'fields' are the two fields to concatenate to create the name.
        if (! empty($layout_def['table_alias'])) {
            $alias = $this->reporter->db->concat($layout_def['table_alias'], $field_def['fields']);
        } elseif (! empty($layout_def['name'])) {
            $alias = $layout_def['name'];
        } else {
            $alias = "*";
        }

        return $alias;
    }

    public function _get_column_select($layout_def)
    {
        global $locale, $current_user;

        if (isset($this->reporter->all_fields)) {
            $field_def = $this->reporter->all_fields[$layout_def['column_key']];
        } else {
            $field_def = array();
        }

        //	 'fields' are the two fields to concatenate to create the name
        if (!isset($field_def['fields'])) {
            return $this->_get_normal_column_select($layout_def);
        }
        $localeNameFormat = $locale->getLocaleFormatMacro($current_user);
        $localeNameFormat = trim(preg_replace('/s/i', '', (string) $localeNameFormat));

        if (empty($field_def['fields']) || empty($field_def['fields'][0]) || empty($field_def['fields'][1])) {
            return parent::_get_column_select($layout_def);
        }

        if (! empty($layout_def['table_alias'])) {
            $comps = preg_split("/([fl])/", $localeNameFormat, null, PREG_SPLIT_DELIM_CAPTURE);
            $name = array();
            foreach ($comps as $val) {
                if ($val == 'f') {
                    $name[] = $this->reporter->db->convert($layout_def['table_alias'].".".$field_def['fields'][0], 'IFNULL', array("''"));
                } elseif ($val == 'l') {
                    $name[] = $this->reporter->db->convert($layout_def['table_alias'].".".$field_def['fields'][1], 'IFNULL', array("''"));
                } else {
                    if (!empty($val)) {
                        $name[] = $this->reporter->db->quoted($val);
                    }
                }
            }
            $alias = $this->reporter->db->convert($name, "CONCAT");
        } elseif (! empty($layout_def['name'])) {
            $alias = $layout_def['name'];
        } else {
            $alias = "*";
        }

        return $alias;
    }

    public function queryFilterIs($layout_def)
    {
        $layout_def['name'] = 'id';
        $layout_def['type'] = 'id';
        $input_name0 = $layout_def['input_name0'];

        if (is_array($layout_def['input_name0'])) {
            $input_name0 = $layout_def['input_name0'][0];
        }
        if ($input_name0 == 'Current User') {
            global $current_user;
            $input_name0 = $current_user->id;
        }

        return $this->sugarWidgetFieldId->_get_column_select($layout_def)."="
            .$this->reporter->db->quoted($input_name0)."\n";
    }

    public function queryFilteris_not($layout_def)
    {
        $layout_def['name'] = 'id';
        $layout_def['type'] = 'id';
        $input_name0 = $layout_def['input_name0'];

        if (is_array($layout_def['input_name0'])) {
            $input_name0 = $layout_def['input_name0'][0];
        }
        if ($input_name0 == 'Current User') {
            global $current_user;
            $input_name0 = $current_user->id;
        }

        return $this->sugarWidgetFieldId->_get_column_select($layout_def)."<>"
            .$this->reporter->db->quoted($input_name0)."\n";
    }

    // $rename_columns, if true then you're coming from reports
    public function queryFilterone_of(&$layout_def, $rename_columns = true)
    {
        if ($rename_columns) { // this was a hack to get reports working, sugarwidgets should not be renaming $name!
            $layout_def['name'] = 'id';
            $layout_def['type'] = 'id';
        }
        $arr = array();

        foreach ($layout_def['input_name0'] as $value) {
            if ($value == 'Current User') {
                global $current_user;
                array_push($arr, $this->reporter->db->quoted($current_user->id));
            } else {
                array_push($arr, $this->reporter->db->quoted($value));
            }
        }

        $str = implode(",", $arr);

        return $this->sugarWidgetFieldId->_get_column_select($layout_def)." IN (".$str.")\n";
    }
    // $rename_columns, if true then you're coming from reports
    public function queryFilternot_one_of($layout_def, $rename_columns = true)
    {
        if ($rename_columns) { // this was a hack to get reports working, sugarwidgets should not be renaming $name!
            $layout_def['name'] = 'id';
            $layout_def['type'] = 'id';
        }
        $arr = array();

        foreach ($layout_def['input_name0'] as $value) {
            if ($value == 'Current User') {
                global $current_user;
                array_push($arr, $this->reporter->db->quoted($current_user->id));
            } else {
                array_push($arr, $this->reporter->db->quoted($value));
            }
        }

        $str = implode(",", $arr);

        return $this->sugarWidgetFieldId->_get_column_select($layout_def)." NOT IN (".$str.")\n";
    }

    public function &queryGroupBy($layout_def)
    {
        if ($layout_def['name'] == 'full_name') {
            $layout_def['name'] = 'id';
            $layout_def['type'] = 'id';

            $group_by =  $this->sugarWidgetFieldId->_get_column_select($layout_def)."\n";
        } else {
            // group by clause for user name passes through here.
            $group_by = $this->_get_column_select($layout_def)."\n";
        }
        return $group_by;
    }
}
