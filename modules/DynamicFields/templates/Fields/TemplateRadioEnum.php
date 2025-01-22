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

require_once('modules/DynamicFields/templates/Fields/TemplateEnum.php');
require_once('include/utils/array_utils.php');
class TemplateRadioEnum extends TemplateEnum
{
    public $type = 'radioenum';

    public function get_html_edit()
    {
        $this->prepare();
        $xtpl_var = strtoupper($this->name);
        return "{RADIOOPTIONS_".$xtpl_var. "}";
    }

    public function get_field_def()
    {
        $def = parent::get_field_def();
        $def['dbType'] = 'enum';
        $def['separator'] = '<br>';
        return $def;
    }


    public function get_xtpl_edit($add_blank = false)
    {
        global $app_list_strings;

        $returnXTPL = [];
        $name = $this->name;
        $value = '';
        if (isset($this->bean->$name)) {
            $value = $this->bean->$name;
        } else {
            if (empty($this->bean->id)) {
                $value= $this->default_value;
            }
        }
        if (!empty($this->help)) {
            $returnXTPL[$this->name . '_help'] = translate($this->help, $this->bean->module_dir);
        }

        $returnXTPL[strtoupper($this->name)] = $value;

        $returnXTPL[strtoupper('RADIOOPTIONS_'.$this->name)] = $this->generateRadioButtons($value, false);

        return $returnXTPL;
    }


    public function generateRadioButtons($value = '', $add_blank =false)
    {
        global $app_list_strings;
        $radiooptions = '';
        $keyvalues = $app_list_strings[$this->ext1];
        if ($add_blank) {
            $keyvalues = add_blank_option($keyvalues);
        }
        $help = (!empty($this->help))?"title='". translate($this->help, $this->bean->module_dir) . "'": '';
        foreach ($keyvalues as $key=>$displayText) {
            $selected = ($value == $key)?'checked': '';
            $radiooptions .= "<input type='radio' id='{$this->name}{$key}' name='$this->name'  $help value='$key' $selected><span onclick='document.getElementById(\"{$this->name}{$key}\").checked = true' style='cursor:default' onmousedown='return false;'>$displayText</span><br>\n";
        }
        return $radiooptions;
    }

    public function get_xtpl_search()
    {
        $searchFor = '';
        if (!empty($_REQUEST[$this->name])) {
            $searchFor = $_REQUEST[$this->name];
        }
        global $app_list_strings;
        $returnXTPL = array();
        $returnXTPL[strtoupper($this->name)] = $searchFor;
        $returnXTPL[strtoupper('RADIOOPTIONS_'.$this->name)] = $this->generateRadioButtons($searchFor, true);
        return $returnXTPL;
    }

    public function get_xtpl_detail()
    {
        $name = $this->name;
        if (isset($this->bean->$name)) {
            global $app_list_strings;
            if (isset($app_list_strings[$this->ext1])) {
                if (isset($app_list_strings[$this->ext1][$this->bean->$name])) {
                    return $app_list_strings[$this->ext1][$this->bean->$name];
                }
            }
        } else {
            if (empty($this->bean->id)) {
                return $this->default_value;
            }
        }
        return '';
    }

    public function get_db_default($modify = false)
    {
        return '';
    }
}
