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

/**

 * Description:  Creates the runtime database connection.
 */
#[\AllowDynamicProperties]
class javascript
{
    public $formname = 'form';
    public $script = '';
    public $sugarbean = null;
    public function setFormName($name)
    {
        $this->formname = $name;
    }

    public function __construct()
    {
        global $app_strings, $current_user, $sugar_config;

        // Bug 24730 - default initialize the bean object in case we never set it to the current bean object
        $this->sugarbean = new stdClass;
        $this->sugarbean->field_name_map = array();
        $this->sugarbean->module_dir = '';
    }




    public function setSugarBean($sugar)
    {
        $this->sugarbean = $sugar;
    }

    public function addRequiredFields($prefix='')
    {
        if (isset($this->sugarbean->required_fields)) {
            foreach ($this->sugarbean->required_fields as $field=>$value) {
                $this->addField($field, 'true', $prefix);
            }
        }
    }

    public function addSpecialField($dispField, $realField, $type, $required, $prefix = '')
    {
        if (isset($this->sugarbean->field_name_map[$realField]['vname'])) {
            $this->addFieldGeneric($dispField, 'date', $this->sugarbean->field_name_map[$realField]['vname'], $required, $prefix);
        }
    }

    public function addField($field, $required, $prefix='', $displayField='', $translate = false)
    {
        if ($field == "id") {
            return;
        }
        if (isset($this->sugarbean->field_name_map[$field]['vname'])) {
            $vname = $this->sugarbean->field_name_map[$field]['vname'];
            if ($translate) {
                $vname = $this->buildStringToTranslateInSmarty($this->sugarbean->field_name_map[$field]['vname']);
            }
            if (empty($required)) {
                if (isset($this->sugarbean->field_name_map[$field]['required']) && $this->sugarbean->field_name_map[$field]['required']) {
                    $required = 'true';
                } else {
                    $required = 'false';
                }
                if (isset($this->sugarbean->required_fields[$field]) && $this->sugarbean->required_fields[$field]) {
                    $required = 'true';
                }
                if ($field == 'id') {
                    $required = 'false';
                }
            }
            if (isset($this->sugarbean->field_name_map[$field]['validation'])) {
                switch ($this->sugarbean->field_name_map[$field]['validation']['type']) {
                    case 'range':
                        $min = false;
                        $max = false;
                        if (isset($this->sugarbean->field_name_map[$field]['validation']['min'])) {
                            $min = filter_var($this->sugarbean->field_name_map[$field]['validation']['min'], FILTER_VALIDATE_INT);
                        }
                        if (isset($this->sugarbean->field_name_map[$field]['validation']['max'])) {
                            $max = filter_var($this->sugarbean->field_name_map[$field]['validation']['max'], FILTER_VALIDATE_INT);
                        }
                        if ($min !== false && $max !== false && $min > $max) {
                            $max = $min;
                        }
                        if (!empty($displayField)) {
                            $dispField = $displayField;
                        } else {
                            $dispField = $field;
                        }
                        $this->addFieldRange($dispField, $this->sugarbean->field_name_map[$field]['type'], $vname, $required, $prefix, $min, $max);
                        break;
                    case 'isbefore':
                        $compareTo = $this->sugarbean->field_name_map[$field]['validation']['compareto'];
                        if (!empty($displayField)) {
                            $dispField = $displayField;
                        } else {
                            $dispField = $field;
                        }
                        if (!empty($this->sugarbean->field_name_map[$field]['validation']['blank']) && $this->sugarbean->field_name_map[$field]['validation']['blank']) {
                            $this->addFieldDateBeforeAllowBlank($dispField, $this->sugarbean->field_name_map[$field]['type'], $vname, $required, $prefix, $compareTo);
                        } else {
                            $this->addFieldDateBefore($dispField, $this->sugarbean->field_name_map[$field]['type'], $vname, $required, $prefix, $compareTo);
                        }
                        break;
                    // Bug #47961 Adding new type of validation: through callback function
                    case 'callback':
                        $dispField = $displayField ? $displayField : $field;
                        $this->addFieldCallback($dispField, $this->sugarbean->field_name_map[$field]['type'], $vname, $required, $prefix, $this->sugarbean->field_name_map[$field]['validation']['callback']);
                        break;
                    default:
                        if (!empty($displayField)) {
                            $dispField = $displayField;
                        } else {
                            $dispField = $field;
                        }

                        $type = (!empty($this->sugarbean->field_name_map[$field]['custom_type']))?$this->sugarbean->field_name_map[$field]['custom_type']:$this->sugarbean->field_name_map[$field]['type'];

                        $this->addFieldGeneric($dispField, $type, $vname, $required, $prefix);
                        break;
                }
            } else {
                if (!empty($displayField)) {
                    $dispField = $displayField;
                } else {
                    $dispField = $field;
                }
                $type = (!empty($this->sugarbean->field_name_map[$field]['custom_type']))?$this->sugarbean->field_name_map[$field]['custom_type']:$this->sugarbean->field_name_map[$field]['type'];
                if (!empty($this->sugarbean->field_name_map[$dispField]['isMultiSelect'])) {
                    $dispField .='[]';
                }
                $this->addFieldGeneric($dispField, $type, $vname, $required, $prefix);
            }
        } else {
            $GLOBALS['log']->debug('No VarDef Label For ' . $field . ' in module ' . $this->sugarbean->module_dir);
        }
    }


    public function stripEndColon($modString)
    {
        if (substr((string) $modString, -1, 1) == ":") {
            $modString = substr((string) $modString, 0, (strlen((string) $modString) - 1));
        }
        if (substr((string) $modString, -2, 2) == ": ") {
            $modString = substr((string) $modString, 0, (strlen((string) $modString) - 2));
        }
        return $modString;
    }

    public function addFieldGeneric($field, $type, $displayName, $required, $prefix='')
    {
        $this->script .= "addToValidate('".$this->formname."', '".$prefix.$field."', '".$type . "', {$this->getRequiredString($required)},'"
                       . $this->stripEndColon(translate($displayName, $this->sugarbean->module_dir)) . "' );\n";
    }

    // Bug #47961 Generator of callback validator
    public function addFieldCallback($field, $type, $displayName, $required, $prefix, $callback)
    {
        $this->script .= 'addToValidateCallback("'
            . $this->formname . '", "'
            . $prefix.$field . '", "'
            . $type . '", '
            . $this->getRequiredString($required) . ', "'
            . $this->stripEndColon(translate($displayName, $this->sugarbean->module_dir)).'", '
            .$callback
        .');'."\n";
    }

    public function addFieldRange($field, $type, $displayName, $required, $prefix, $min, $max)
    {
        $this->script .= "addToValidateRange("
            . "'" . $this->formname . "', "
            . "'" . $prefix . $field . "', '"
            . $type . "', "
            . $this->getRequiredString($required) . ", '"
            . $this->stripEndColon(translate($displayName, $this->sugarbean->module_dir)) . "', "
            . ($min === false ? 'false' : $min) . ", "
            . ($max === false ? 'false' : $max)
            . ");\n";
    }

    public function addFieldIsValidDate($field, $type, $displayName, $msg, $required, $prefix='')
    {
        $name = $prefix.$field;
        $req = $this->getRequiredString($required);
        $this->script .= "addToValidateIsValidDate('{$this->formname}', '{$name}', '{$type}', {$req}, '{$msg}');\n";
    }

    public function addFieldIsValidTime($field, $type, $displayName, $msg, $required, $prefix='')
    {
        $name = $prefix.$field;
        $req = $this->getRequiredString($required);
        $this->script .= "addToValidateIsValidTime('{$this->formname}', '{$name}', '{$type}', {$req}, '{$msg}');\n";
    }

    public function addFieldDateBefore($field, $type, $displayName, $required, $prefix, $compareTo)
    {
        $this->script .= "addToValidateDateBefore('".$this->formname."', '".$prefix.$field."', '".$type . "', {$this->getRequiredString($required)},'"
                       . $this->stripEndColon(translate($displayName, $this->sugarbean->module_dir)) . "', '$compareTo' );\n";
    }

    public function addFieldDateBeforeAllowBlank($field, $type, $displayName, $required, $prefix, $compareTo, $allowBlank='true')
    {
        $this->script .= "addToValidateDateBeforeAllowBlank('".$this->formname."', '".$prefix.$field."', '".$type . "', {$this->getRequiredString($required)},'"
                       . $this->stripEndColon(translate($displayName, $this->sugarbean->module_dir)) . "', '$compareTo', '$allowBlank' );\n";
    }

    public function addToValidateBinaryDependency($field, $type, $displayName, $required, $prefix, $compareTo)
    {
        $this->script .= "addToValidateBinaryDependency('".$this->formname."', '".$prefix.$field."', '".$type . "', {$this->getRequiredString($required)},'"
                       . $this->stripEndColon(translate($displayName, $this->sugarbean->module_dir)) . "', '$compareTo' );\n";
    }

    public function addToValidateComparison($field, $type, $displayName, $required, $prefix, $compareTo)
    {
        $this->script .= "addToValidateComparison('".$this->formname."', '".$prefix.$field."', '".$type . "', {$this->getRequiredString($required)},'"
                       . $this->stripEndColon(translate($displayName, $this->sugarbean->module_dir)) . "', '$compareTo' );\n";
    }

    public function addFieldIsInArray($field, $type, $displayName, $required, $prefix, $arr, $operator)
    {
        $name = $prefix.$field;
        $req = $this->getRequiredString($required);
        $json = getJSONobj();
        $arr = $json->encode($arr);
        $this->script .= "addToValidateIsInArray('{$this->formname}', '{$name}', '{$type}', {$req}, '".$this->stripEndColon(translate($displayName, $this->sugarbean->module_dir))."', '{$arr}', '{$operator}');\n";
    }

    public function addAllFields($prefix, $skip_fields=null, $translate = false)
    {
        if (!isset($skip_fields)) {
            $skip_fields = array();
        }
        foreach ($this->sugarbean->field_name_map as $field=>$value) {
            if (!isset($skip_fields[$field])) {
                if (isset($value['type']) && ($value['type'] == 'datetimecombo' || $value['type'] == 'datetime')) {
                    $isRequired = (isset($value['required']) && $value['required']) ? 'true' : 'false';
                    $this->addSpecialField($value['name'] . '_date', $value['name'], 'datetime', $isRequired);
                    if ($value['type'] != 'link'  && isset($this->sugarbean->field_name_map[$field]['validation'])) {
                        //datetime should also support the isbefore or other type of validate
                        $this->addField($field, '', $prefix, '', $translate);
                    }
                } else {
                    if (isset($value['type'])) {
                        if ($value['type'] != 'link') {
                            $this->addField($field, '', $prefix, '', $translate);
                        }
                    }
                }
            }
        }
    }

    public function addActionMenu()
    {
        $this->script .= "$(document).ready(SUGAR.themes.actionMenu);";
    }

    public function getScript($showScriptTag = true, $clearValidateFields = true)
    {
        $tempScript = $this->script;
        $this->script = "";
        if ($showScriptTag) {
            $this->script = "<script type=\"text/javascript\">\n";
        }

        if ($clearValidateFields) {
            $this->script .= "addForm('{$this->formname}');";
        }

        $this->script .= $tempScript;

        if ($showScriptTag) {
            $this->script .= "</script>";
        }
        return $this->script;
    }

    public function buildStringToTranslateInSmarty(
        $string
        ) {
        if (is_array($string)) {
            $returnstring = '';
            foreach ($string as $astring) {
                $returnstring .= $this->buildStringToTranslateInSmarty($astring);
            }
            return $returnstring;
        }
        return "{/literal}{sugar_translate label='$string' module='{$this->sugarbean->module_dir}' for_js=true}{literal}";
    }

    protected function getRequiredString($required)
    {
        if (is_string($required) && strtolower($required) == "false") {
            return "false";
        }
        return empty($required) ? "false" : "true";
    }
}
