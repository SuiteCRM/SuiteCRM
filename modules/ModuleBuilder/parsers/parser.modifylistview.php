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


require_once('modules/ModuleBuilder/parsers/ModuleBuilderParser.php');

/**
 * Class ParserModifyListView
 */
#[\AllowDynamicProperties]
class ParserModifyListView extends ModuleBuilderParser
{
    /**
     * @var bool $listViewDefs
     */
    public $listViewDefs = false;
    /**
     * @var array $defaults
     */
    public $defaults = array();
    /**
     * @var array $additional
     */
    public $additional = array();
    /**
     * @var array $available
     */
    public $available = array();
    /**
     * @var array $reserved
     * fields marked by 'studio'=>false in the listviewdefs; need to be preserved
     */
    public $reserved = array();
    /**
     * @var array $columns
     */
    public $columns = array(
        'LBL_DEFAULT' => 'getDefaultFields',
        'LBL_AVAILABLE' => 'getAdditionalFields',
        'LBL_HIDDEN' => 'getAvailableFields'
    );

    /**
     * @param string $module_name
     * @param string $submodule
     */
    public function init($module_name, $submodule = '')
    {
        global $app_list_strings;
        $this->module_name = $module_name;
        $mod_strings = return_module_language(
            $GLOBALS ['current_language'],
            $this->module_name
        ); // needed solely so that listviewdefs that reference this can be included without error
        $class = $GLOBALS ['beanList'] [$this->module_name];
        require_once($GLOBALS ['beanFiles'] [$class]);
        $this->module = new $class();

        $loaded = $this->_loadFromFile(
            'ListView',
            'modules/' . $this->module_name . '/metadata/listviewdefs.php',
            $this->module_name
        );
        $this->originalListViewDefs = $loaded['viewdefs'] [$this->module_name];
        $this->_variables = $loaded['variables'];
        $this->customFile = 'custom/modules/' . $this->module_name . '/metadata/listviewdefs.php';
        if (file_exists($this->customFile)) {
            $loaded = $this->_loadFromFile('ListView', $this->customFile, $this->module_name);
            $this->listViewDefs = $loaded['viewdefs'] [$this->module_name];
            $this->_variables = $loaded['variables'];
        } else {
            $this->listViewDefs = &$this->originalListViewDefs;
        }

        $this->fixKeys($this->originalListViewDefs);
        $this->fixKeys($this->listViewDefs);
        $this->language_module = $this->module_name;
    }

    /**
     * @return string
     */
    public function getLanguage()
    {
        return $this->language_module;
    }

    /**
     * @param $defs re-key array so that every entry has a key=name and all keys are lowercase - makes it easier in handleSave() later...
     */
    public function fixKeys(&$defs)
    {
        $temp = array();
        foreach ($defs as $key => $value) {
            if (!is_array($value)) {
                $key = $value;
                $def = array();
                $def ['name'] = (isset($this->module->field_defs [$key])) ? $this->module->field_defs [$key] ['name'] : $key;
                $value = $def;
            }
            if (isset($value ['name'])) {
                $key = $value ['name']; // override key with name, needed when the entry lacks a key
            }
            $temp [strtolower($key)] = $value;
        }
        $defs = $temp;
    }

    /**
     * returns the default fields for a listview
     * Called only when displaying the listview for editing; not called when saving
     */
    public function getDefaultFields()
    {
        $this->defaults = array();
        foreach ($this->listViewDefs as $key => $def) {
            // add in the default fields from the listviewdefs, stripping out any field with 'studio' set to a value other than true
            // Important: the 'studio' fields must be added back into the layout on save, as they're not editable rather than hidden
            if (!empty($def ['default'])) {
                if (!isset($def['studio']) || $def['studio'] === true) {
                    $this->defaults [$key] = $def;
                } else { // anything which doesn't go into the defaults is a reserved field - this makes sure we don't miss anything
                    $this->reserved [$key] = $def;
                }
            }
        }

        return $this->defaults;
    }

    /**
     * returns additional fields available for users to create fields
     */
    public function getAdditionalFields()
    {
        $this->additional = array();
        foreach ($this->listViewDefs as $key => $def) {
            if (empty($def ['default'])) {
                $key = strtolower($key);
                $this->additional [$key] = $def;
            }
        }

        return $this->additional;
    }

    /**
     * returns unused fields that are available for using in either default or additional list views
     */
    public function getAvailableFields()
    {
        $this->availableFields = array();
        $lowerFieldList = array_change_key_case($this->listViewDefs);
        foreach ($this->originalListViewDefs as $key => $def) {
            $key = strtolower($key);
            if (!isset($lowerFieldList [$key])) {
                $this->availableFields [$key] = $def;
            }
        }
        $GLOBALS['log']->debug('parser.modifylistview.php->getAvailableFields(): field_defs=' . print_r(
            $this->availableFields,
            true
        ));
        $modFields = !empty($this->module->field_name_map) ? $this->module->field_name_map : $this->module->field_defs;
        foreach ($modFields as $key => $def) {
            $fieldName = strtolower($key);
            if ($fieldName == 'currency_id') {
                continue;
            }
            if (!isset($lowerFieldList [$fieldName])) { // bug 16728 - check this first, so that other conditions (e.g., studio == visible) can't override and add duplicate entries
                // bug 19656: this test changed after 5.0.0b - we now remove all ID type fields - whether set as type, or dbtype, from the fielddefs
                if ($this->isValidField($key, $def)) {
                    $label = (isset($def ['vname'])) ? $def ['vname'] : (isset($def ['label']) ? $def['label'] : $def['name']);
                    $this->availableFields [$fieldName] = array('width' => '10', 'label' => $label);
                }
            }
        }

        return $this->availableFields;
    }

    /**
     * @return array
     */
    public function getFieldDefs()
    {
        return $this->module->field_defs;
    }

    /**
     * @param string $key
     * @param array $def
     * @return bool
     */
    public function isValidField($key, $def)
    {
        //Allow fields that are studio visible
        if (!empty($def ['studio']) && $def ['studio'] == 'visible') {
            return true;
        }

        //No ID fields
        if ((!empty($def ['dbType']) && $def ['dbType'] == 'id') || (!empty($def ['type']) && $def ['type'] == 'id')) {
            return false;
        }

        //only allow DB and custom fields (if a source is specified)
        if (!empty($def ['source']) && $def ['source'] != 'db' && $def ['source'] != 'custom_fields') {
            return false;
        }

        //Dont ever show the "deleted" fields or "_name" fields
        if (strcmp($key, 'deleted') == 0 || (isset($def ['name']) && strpos((string) $def ['name'], "_name") !== false)) {
            return false;
        }

        //If none of the "ifs" are true, the field is valid
        return true;
    }


    /**
     * @param string $fieldName
     * @return array
     */
    public function getField($fieldName)
    {
        $fieldName = strtolower($fieldName);
        foreach ($this->listViewDefs as $key => $def) {
            $key = strtolower($key);
            if ($key == $fieldName) {
                return $def;
            }
        }
        foreach ($this->module->field_defs as $key => $def) {
            $key = strtolower($key);
            if ($key == $fieldName) {
                return $def;
            }
        }

        return array();
    }

    /**
     * @param $fieldname
     * @param $listfielddef
     * @return mixed
     */
    public function addRelateData($fieldname, $listfielddef)
    {
        $modFieldDef = $this->module->field_defs [strtolower($fieldname)];
        if (!empty($modFieldDef['module']) && !empty($modFieldDef['id_name'])) {
            $listfielddef['module'] = $modFieldDef['module'];
            $listfielddef['id'] = strtoupper($modFieldDef['id_name']);
            $listfielddef['link'] = true;
            $listfielddef['related_fields'] = array(strtolower($modFieldDef['id_name']));
        }

        return $listfielddef;
    }

    /**
     * @return array
     */
    public function _loadLayoutFromRequest()
    {
        $GLOBALS['log']->debug("ParserModifyListView->_loadLayoutFromRequest()");
        $fields = array();
        $rejectTypes = array('html', 'enum', 'text');
        for ($i = 0; isset($_POST ['group_' . $i]) && $i < 2; $i++) {
            //echo "\n***group-$i Size:".sizeof($_POST['group_' . $i])."\n";
            foreach ($_POST ['group_' . $i] as $field) {
                $fieldname = strtoupper($field);
                //originalListViewDefs are all lower case
                $lowerFieldName = strtolower($field);
                if (isset($this->originalListViewDefs [$lowerFieldName])) {
                    $fields [$fieldname] = $this->originalListViewDefs [$lowerFieldName];
                } else {
                    //check if we have the case wrong for custom fields
                    if (!isset($this->module->field_defs [$fieldname])) {
                        foreach ($this->module->field_defs as $key => $value) {
                            if (strtoupper($key) === $fieldname) {
                                $fields [$fieldname] = array(
                                    'width' => 10,
                                    'label' => $this->module->field_defs [$key] ['vname']
                                );
                                break;
                            }
                        }
                    } else {
                        $fields [$fieldname] = array(
                            'width' => 10,
                            'label' => $this->module->field_defs [$fieldname] ['vname']
                        );
                    }
                    // sorting fields of certain types will cause a database engine problems
                    // we only check this for custom fields, as we assume that OOB fields have been independently confirmed as ok
                    if (isset($this->module->field_defs [strtolower($fieldname)]) && (in_array(
                        $this->module->field_defs [strtolower($fieldname)] ['type'],
                        $rejectTypes
                    ) || isset($this->module->field_defs [strtolower($fieldname)]['custom_module']))
                    ) {
                        $fields [$fieldname] ['sortable'] = false;
                    }
                    // Bug 23728 - Make adding a currency type field default to setting the 'currency_format' to true
                    if (isset($this->module->field_defs [strtolower($fieldname)] ['type ']) && $this->module->field_defs [strtolower($fieldname)] ['type'] == 'currency') {
                        $fields [$fieldname] ['currency_format'] = true;
                    }
                }
                if (isset($_REQUEST [strtolower($fieldname) . 'width'])) {
                    $width = substr((string) $_REQUEST [strtolower($fieldname) . 'width'], 6, 3);
                    if (strpos($width, "%") !== false) {
                        $width = substr($width, 0, 2);
                    }
                    if ($width < 101 && $width > 0) {
                        $fields [$fieldname] ['width'] = $width;
                    }
                } else {
                    if (isset($this->listViewDefs [$fieldname] ['width'])) {
                        $fields [$fieldname] ['width'] = $this->listViewDefs [$fieldname] ['width'];
                    }
                }
                //Get additional Data for relate fields
                if (isset($this->module->field_defs [strtolower($fieldname)] ['type']) && $this->module->field_defs [strtolower($fieldname)] ['type'] == 'relate') {
                    $fields [$fieldname] = $this->addRelateData($field, $fields [$fieldname]);
                }
                $fields [$fieldname] ['default'] = ($i == 0);
            }
        }
        // Add the reserved fields back in to the end of the default fields in the layout
        // ASSUMPTION: reserved fields go back at the end
        // First, load the reserved fields - we cannot assume that getDefaultFields has been called earlier when saving
        $this->getDefaultFields();
        foreach ($this->reserved as $key => $def) {
            $fields[$key] = $def;
        }

        return $fields;
    }

    /**
     * handleSave
     */
    public function handleSave()
    {
        $fields = $this->_loadLayoutFromRequest();
        $this->_writeToFile($this->customFile, 'ListView', $this->module_name, $fields, $this->_variables);

        $GLOBALS ["listViewDefs"] [$this->module_name] = $fields;
        // now clear the cache so that the results are immediately visible
        include_once('include/TemplateHandler/TemplateHandler.php');
        TemplateHandler::clearCache(
            $this->module_name,
            "ListView.tpl"
        ); // not currently cached, but here for the future
    }
}
