<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2021 SalesAgility Ltd.
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

 * Description: view handler for step 3 of the import process
 * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
 * All Rights Reserved.
 */
require_once('modules/Import/views/ImportView.php');
require_once('modules/Import/sources/ImportFile.php');
require_once('modules/Import/ImportFileSplitter.php');
require_once('modules/Import/ImportCacheFiles.php');
require_once('modules/Import/ImportDuplicateCheck.php');

require_once('include/upload_file.php');

#[\AllowDynamicProperties]
class ImportViewStep3 extends ImportView
{
    protected $pageTitleKey = 'LBL_STEP_3_TITLE';
    protected $currentFormID = 'importstep3';
    protected $previousAction = 'Confirm';
    protected $nextAction = 'dupcheck';

    /**
     * @see SugarView::display()
     */
    public function display()
    {
        global $mod_strings, $app_strings, $current_user, $sugar_config, $app_list_strings, $locale;

        $this->ss->assign("IMPORT_MODULE", $_REQUEST['import_module']);
        $has_header = (isset($_REQUEST['has_header']) ? 1 : 0);
        $sugar_config['import_max_records_per_file'] = (empty($sugar_config['import_max_records_per_file']) ? 1000 : $sugar_config['import_max_records_per_file']);
        $this->ss->assign("CURRENT_STEP", $this->currentStep);
        // attempt to lookup a preexisting field map
        // use the custom one if specfied to do so in step 1
        $mapping_file = BeanFactory::newBean('Import_1');
        $field_map = $mapping_file->set_get_import_wizard_fields();
        $default_values = array();
        $ignored_fields = array();
        $fields = [];

        if (!empty($_REQUEST['source_id'])) {
            $GLOBALS['log']->fatal("Loading import map properties.");
            $mapping_file = BeanFactory::newBean('Import_1');
            $mapping_file->retrieve($_REQUEST['source_id'], false);
            $_REQUEST['source'] = $mapping_file->source;
            $has_header = $mapping_file->has_header;
            if (isset($mapping_file->delimiter)) {
                $_REQUEST['custom_delimiter'] = $mapping_file->delimiter;
            }
            if (isset($mapping_file->enclosure)) {
                $_REQUEST['custom_enclosure'] = htmlentities((string) $mapping_file->enclosure);
            }
            $field_map = $mapping_file->getMapping();
            //print_r($field_map);die();
            $default_values = $mapping_file->getDefaultValues();
            $this->ss->assign("MAPNAME", $mapping_file->name);
            $this->ss->assign("CHECKMAP", 'checked="checked" value="on"');
        } else {
            $classname = $this->getMappingClassName(ucfirst($_REQUEST['source']));

            //Set the $_REQUEST['source'] to be 'other' for ImportMapOther special case
            if ($classname == 'ImportMapOther') {
                $_REQUEST['source'] = 'other';
            }

            if (class_exists($classname)) {
                $mapping_file = new $classname;
                $ignored_fields = $mapping_file->getIgnoredFields($_REQUEST['import_module']);
                $field_map2 = $mapping_file->getMapping($_REQUEST['import_module']);
                $field_map = array_merge($field_map, $field_map2);
            }
        }

        $delimiter = $this->getRequestDelimiter();

        $this->ss->assign("CUSTOM_DELIMITER", $delimiter);
        $this->ss->assign("CUSTOM_ENCLOSURE", (!empty($_REQUEST['custom_enclosure']) ? $_REQUEST['custom_enclosure'] : ""));

        //populate import locale  values from import mapping if available, these values will be used througout the rest of the code path

        $uploadFileName = $_REQUEST['file_name'];

        if (isset($uploadFileName) && strpos((string) $uploadFileName, '..') !== false) {
            LoggerManager::getLogger()->security('Directory navigation attack denied');
            return;
        }


        if (isset($uploadFileName) && !hasValidFileName('import_upload_file_name', str_replace('upload://', '', (string) $uploadFileName))) {
            echo $app_strings['LBL_LOGGER_INVALID_FILENAME'];
            echo $uploadFileName;
            LoggerManager::getLogger()->fatal('Invalid import file name');
            return;
        }


        if (strpos((string) $uploadFileName, 'phar://') !== false) {
            return;
        }

        // Now parse the file and look for errors
        $importFile = new ImportFile($uploadFileName, $delimiter, html_entity_decode((string) $_REQUEST['custom_enclosure'], ENT_QUOTES), false);

        if (!$importFile->fileExists()) {
            $this->_showImportError($mod_strings['LBL_CANNOT_OPEN'], $_REQUEST['import_module'], 'Step2');
            return;
        }

        $charset = $importFile->autoDetectCharacterSet();

        // retrieve first 3 rows
        $rows = array();

        //Keep track of the largest row count found.
        $maxFieldCount = 0;
        for ($i = 0; $i < 3; $i++) {
            $rows[] = $importFile->getNextRow();
            $maxFieldCount = $importFile->getFieldCount() > $maxFieldCount ?  $importFile->getFieldCount() : $maxFieldCount;
        }
        $ret_field_count = $maxFieldCount;

        // Bug 14689 - Parse the first data row to make sure it has non-empty data in it
        $isempty = true;
        if ($rows[(int)$has_header] != false) {
            foreach ($rows[(int)$has_header] as $value) {
                if (strlen(trim($value)) > 0) {
                    $isempty = false;
                    break;
                }
            }
        }

        if ($isempty || $rows[(int)$has_header] == false) {
            $this->_showImportError($mod_strings['LBL_NO_LINES'], $_REQUEST['import_module'], 'Step2');
            return;
        }

        // save first row to send to step 4
        $this->ss->assign("FIRSTROW", htmlentities(json_encode($rows[0])));

        // Now build template
        $this->ss->assign("TMP_FILE", $uploadFileName);
        $this->ss->assign("SOURCE", $_REQUEST['source']);
        $this->ss->assign("TYPE", $_REQUEST['type']);
        $this->ss->assign("DELETE_INLINE_PNG", SugarThemeRegistry::current()->getImage('basic_search', 'align="absmiddle" alt="'.$app_strings['LNK_DELETE'].'" border="0"'));
        $this->ss->assign("PUBLISH_INLINE_PNG", SugarThemeRegistry::current()->getImage('advanced_search', 'align="absmiddle" alt="'.$mod_strings['LBL_PUBLISH'].'" border="0"'));

        $this->instruction = 'LBL_SELECT_MAPPING_INSTRUCTION';
        $this->ss->assign('INSTRUCTION', $this->getInstruction());

        $this->ss->assign("MODULE_TITLE", $this->getModuleTitle(false));
        $this->ss->assign(
            "STEP4_TITLE",
            strip_tags(str_replace("\n", "", (string) getClassicModuleTitle(
                $mod_strings['LBL_MODULE_NAME'],
                array($mod_strings['LBL_MODULE_NAME'],$mod_strings['LBL_STEP_4_TITLE']),
                false
                )))
            );
        $this->ss->assign("HEADER", $app_strings['LBL_IMPORT']." ". $mod_strings['LBL_MODULE_NAME']);

        // we export it as email_address, but import as email1
        $field_map['email_address'] = 'email1';

        // build each row; row count is determined by the number of fields in the import file
        $columns = array();
        $mappedFields = array();

        // this should be populated if the request comes from a 'Back' button click
        $importColumns = $this->getImportColumns();
        $column_sel_from_req = false;
        if (!empty($importColumns)) {
            $column_sel_from_req = true;
        }

        for ($field_count = 0; $field_count < $ret_field_count; $field_count++) {
            // See if we have any field map matches
            $defaultValue = "";
            // Bug 31260 - If the data rows have more columns than the header row, then just add a new header column
            if (!isset($rows[0][$field_count])) {
                $rows[0][$field_count] = '';
            }
            // See if we can match the import row to a field in the list of fields to import
            $firstrow_name = trim(str_replace(":", "", (string) $rows[0][$field_count]));
            if ($has_header && isset($field_map[$firstrow_name])) {
                $defaultValue = $field_map[$firstrow_name];
            } elseif (isset($field_map[$field_count])) {
                $defaultValue = $field_map[$field_count];
            } elseif (empty($_REQUEST['source_id'])) {
                $defaultValue = trim($rows[0][$field_count]);
            }

            // build string of options
            $fields  = $this->bean->get_importable_fields();
            $options = array();
            $defaultField = '';
            global $current_language;
            $moduleStrings = return_module_language($current_language, $this->bean->module_dir);

            foreach ($fields as $fieldname => $properties) {
                // get field name
                if (!empty($moduleStrings['LBL_EXPORT_'.strtoupper($fieldname)])) {
                    $displayname = str_replace(":", "", (string) $moduleStrings['LBL_EXPORT_'.strtoupper($fieldname)]);
                } else {
                    if (!empty($properties['vname'])) {
                        $displayname = str_replace(":", "", (string) translate($properties['vname'], $this->bean->module_dir));
                    } else {
                        $displayname = str_replace(":", "", (string) translate($properties['name'], $this->bean->module_dir));
                    }
                }
                // see if this is required
                $req_mark  = "";
                $req_class = "";
                if (array_key_exists($fieldname, $this->bean->get_import_required_fields())) {
                    $req_mark  = ' ' . $app_strings['LBL_REQUIRED_SYMBOL'];
                    $req_class = ' class="required" ';
                }
                // see if we have a match
                $selected = '';
                if ($column_sel_from_req && isset($importColumns[$field_count])) {
                    if ($fieldname == $importColumns[$field_count]) {
                        $selected = ' selected="selected" ';
                        $defaultField = $fieldname;
                        $mappedFields[] = $fieldname;
                    }
                } else {
                    if (!empty($defaultValue) && !in_array($fieldname, $mappedFields)
                                                    && !in_array($fieldname, $ignored_fields)) {
                        if (strtolower($fieldname) === strtolower($defaultValue)
                            || strtolower($fieldname) === str_replace(" ", "_", strtolower($defaultValue))
                            || strtolower($displayname) === strtolower($defaultValue)
                            || strtolower($displayname) === str_replace(" ", "_", strtolower($defaultValue))) {
                            $selected = ' selected="selected" ';
                            $defaultField = $fieldname;
                            $mappedFields[] = $fieldname;
                        }
                    }
                }
                // get field type information
                $fieldtype = '';
                if (isset($properties['type'])
                        && isset($mod_strings['LBL_IMPORT_FIELDDEF_' . strtoupper($properties['type'])])) {
                    $fieldtype = ' [' . $mod_strings['LBL_IMPORT_FIELDDEF_' . strtoupper($properties['type'])] . '] ';
                }
                if (isset($properties['comment'])) {
                    $fieldtype .= ' - ' . $properties['comment'];
                }
                $options[$displayname.$fieldname] = '<option value="'.$fieldname.'" title="'. $displayname . htmlentities($fieldtype) . '"'
                    . $selected . $req_class . '>' . $displayname . $req_mark . '</option>\n';
            }

            // get default field value
            $defaultFieldHTML = '';
            if (!empty($defaultField)) {
                $defaultFieldHTML = getControl(
                    $_REQUEST['import_module'],
                    $defaultField,
                    $fields[$defaultField],
                    (isset($default_values[$defaultField]) ? $default_values[$defaultField] : '')
                    );
            }

            if (isset($default_values[$defaultField])) {
                unset($default_values[$defaultField]);
            }

            // Bug 27046 - Sort the column name picker alphabetically
            ksort($options);

            $cellOneData = isset($rows[0][$field_count]) ? $rows[0][$field_count] : '';
            $cellTwoData = isset($rows[1][$field_count]) ? $rows[1][$field_count] : '';
            $cellThreeData = isset($rows[2][$field_count]) ? $rows[2][$field_count] : '';
            $columns[] = array(
                'field_choices' => implode('', $options),
                'default_field' => $defaultFieldHTML,
                'cell1'         => strip_tags($cellOneData),
                'cell2'         => strip_tags($cellTwoData),
                'cell3'         => strip_tags($cellThreeData),
                'show_remove'   => false,
                );
        }

        // add in extra defaulted fields if they are in the mapping record
        if ((is_countable($default_values) ? count($default_values) : 0) > 0) {
            foreach ($default_values as $field_name => $default_value) {
                // build string of options
                $fields  = $this->bean->get_importable_fields();
                $options = array();
                $defaultField = '';
                foreach ($fields as $fieldname => $properties) {
                    // get field name
                    if (!empty($properties['vname'])) {
                        $displayname = str_replace(":", "", (string) translate($properties['vname'], $this->bean->module_dir));
                    } else {
                        $displayname = str_replace(":", "", (string) translate($properties['name'], $this->bean->module_dir));
                    }
                    // see if this is required
                    $req_mark  = "";
                    $req_class = "";
                    if (array_key_exists($fieldname, $this->bean->get_import_required_fields())) {
                        $req_mark  = ' ' . $app_strings['LBL_REQUIRED_SYMBOL'];
                        $req_class = ' class="required" ';
                    }
                    // see if we have a match
                    $selected = '';
                    if (strtolower($fieldname) === strtolower($field_name)
                            && !in_array($fieldname, $mappedFields)
                            && !in_array($fieldname, $ignored_fields)) {
                        $selected = ' selected="selected" ';
                        $defaultField = $fieldname;
                        $mappedFields[] = $fieldname;
                    }
                    // get field type information
                    $fieldtype = '';
                    if (isset($properties['type'])
                            && isset($mod_strings['LBL_IMPORT_FIELDDEF_' . strtoupper($properties['type'])])) {
                        $fieldtype = ' [' . $mod_strings['LBL_IMPORT_FIELDDEF_' . strtoupper($properties['type'])] . '] ';
                    }
                    if (isset($properties['comment'])) {
                        $fieldtype .= ' - ' . $properties['comment'];
                    }
                    $options[$displayname.$fieldname] = '<option value="'.$fieldname.'" title="'. $displayname . $fieldtype . '"' . $selected . $req_class . '>'
                        . $displayname . $req_mark . '</option>\n';
                }

                // get default field value
                $defaultFieldHTML = '';
                if (!empty($defaultField)) {
                    $defaultFieldHTML = getControl(
                        $_REQUEST['import_module'],
                        $defaultField,
                        $fields[$defaultField],
                        $default_value
                        );
                }

                // Bug 27046 - Sort the column name picker alphabetically
                ksort($options);

                $columns[] = array(
                    'field_choices' => implode('', $options),
                    'default_field' => $defaultFieldHTML,
                    'show_remove'   => true,
                    );

                $ret_field_count++;
            }
        }

        $this->ss->assign("COLUMNCOUNT", $ret_field_count);
        $this->ss->assign("rows", $columns);

        $this->ss->assign('datetimeformat', $GLOBALS['timedate']->get_cal_date_time_format());

        // handle building index selector
        global $dictionary, $current_language;

        // show notes
        if ($this->bean instanceof Person) {
            $module_key = "LBL_CONTACTS_NOTE_";
        } elseif ($this->bean instanceof Company) {
            $module_key = "LBL_ACCOUNTS_NOTE_";
        } else {
            $module_key = "LBL_".strtoupper($_REQUEST['import_module'])."_NOTE_";
        }
        $notetext = '';
        for ($i = 1;isset($mod_strings[$module_key.$i]);$i++) {
            $notetext .= '<li>' . $mod_strings[$module_key.$i] . '</li>';
        }
        $this->ss->assign("NOTETEXT", $notetext);
        $this->ss->assign("HAS_HEADER", ($has_header ? 'on' : 'off'));

        // get list of required fields
        $required = array();
        foreach (array_keys($this->bean->get_import_required_fields()) as $name) {
            $properties = $this->bean->getFieldDefinition($name);
            if (!empty($properties['vname'])) {
                $required[$name] = str_replace(":", "", (string) translate($properties['vname'], $this->bean->module_dir));
            } else {
                $required[$name] = str_replace(":", "", (string) translate($properties['name'], $this->bean->module_dir));
            }
        }
        // include anything needed for quicksearch to work
        require_once("include/TemplateHandler/TemplateHandler.php");
        // Bug #46879 : createQuickSearchCode() function in IBM RTC call function getQuickSearchDefaults() to get instance and then getQSDLookup() function
        // if we call this function as static it replaces context and use ImportViewStep3 as $this in getQSDLookup()
        $template_handler = new TemplateHandler();
        $quicksearch_js = $template_handler->createQuickSearchCode($fields, $fields, 'importstep3');

        $this->ss->assign("QS_JS", $quicksearch_js);
        $this->ss->assign("JAVASCRIPT", $this->_getJS($required));

        $this->ss->assign('required_fields', implode(', ', $required));
        $this->ss->assign('CSS', $this->_getCSS());

        $content = $this->ss->fetch($this->getCustomFilePathIfExists('modules/Import/tpls/step3.tpl'));
        $this->ss->assign("CONTENT", $content);
        $this->ss->display($this->getCustomFilePathIfExists('modules/Import/tpls/wizardWrapper.tpl'));
    }


    /**
     * getMappingClassName
     *
     * This function returns the name of a mapping class used to generate the mapping of an import source.
     * It first checks to see if an equivalent custom source map exists in custom/modules/Imports/maps directory
     * and returns this class name if found.  Searches are made for sources with a ImportMapCustom suffix first
     * and then ImportMap suffix.
     *
     * If no such custom file is found, the method then checks the modules/Imports/maps directory for a source
     * mapping file.
     *
     * Lastly, if a source mapping file is still not located, it checks in
     * custom/modules/Import/maps/ImportMapOther.php file exists, it uses the ImportMapOther class.
     *
     * @see display()
     * @param string $source String name of the source mapping prefix
     * @return string name of the mapping class name
     */
    protected function getMappingClassName($source)
    {
        // Try to see if we have a custom mapping we can use
        // based upon the where the records are coming from
        // and what module we are importing into
        $name = 'ImportMap' . $source;
        $customName = 'ImportMapCustom' . $source;

        if (file_exists("custom/modules/Import/maps/{$customName}.php")) {
            require_once("custom/modules/Import/maps/{$customName}.php");
            return $customName;
        } else {
            if (file_exists("custom/modules/Import/maps/{$name}.php")) {
                require_once("custom/modules/Import/maps/{$name}.php");
            } else {
                if (file_exists("modules/Import/maps/{$name}.php")) {
                    require_once("modules/Import/maps/{$name}.php");
                } else {
                    if (file_exists('custom/modules/Import/maps/ImportMapOther.php')) {
                        require_once('custom/modules/Import/maps/ImportMapOther.php');
                        return 'ImportMapOther';
                    }
                }
            }
        }

        return $name;
    }


    protected function getRequestDelimiter()
    {
        $delimiter = !empty($_REQUEST['custom_delimiter']) ? $_REQUEST['custom_delimiter'] : ",";

        switch ($delimiter) {
            case "other":
                $delimiter = $_REQUEST['custom_delimiter_other'];
                break;
            case '\t':
                $delimiter = "\t";
                break;
        }
        return $delimiter;
    }

    protected function getImportColumns()
    {
        $importColumns = array();
        foreach ($_REQUEST as $name => $value) {
            // only look for var names that start with "fieldNum"
            if (strncasecmp($name, "colnum_", 7) != 0) {
                continue;
            }

            // pull out the column position for this field name
            $pos = substr($name, 7);

            // now mark that we've seen this field
            $importColumns[$pos] = $value;
        }

        return $importColumns;
    }

    protected function _getCSS()
    {
        return <<<EOCSS
            <style>
                textarea { width: 20em }
				.detail tr td[scope="row"] {
					text-align:left
				}
                span.collapse{
                    background: transparent url('index.php?entryPoint=getImage&themeName=Sugar&themeName=Sugar&imageName=sugar-yui-sprites.png') no-repeat 0 -90px;
                    padding-left: 10px;
                    cursor: pointer;
		    display: inline;
                }

                span.expand{
                    background: transparent url('index.php?entryPoint=getImage&themeName=Sugar&themeName=Sugar&imageName=sugar-yui-sprites.png') no-repeat -0 -110px;
                    padding-left: 10px;
                     cursor: pointer;
                }
                .removeButton{
                    border: none !important;
                    background-image: none !important;
                    background-color: transparent;
                    padding: 0px;
                }

                #importNotes ul{
                	margin: 0px;
                	margin-top: 10px;
                	padding-left: 20px;
                }

            </style>
EOCSS;
    }
    /**
     * Returns JS used in this view
     *
     * @param  array $required fields that are required for the import
     * @return string HTML output with JS code
     */
    protected function _getJS($required)
    {
        global $mod_strings;

        $print_required_array = "";
        foreach ($required as $name=>$display) {
            $print_required_array .= "required['$name'] = '". sanitize($display) . "';\n";
        }
        $sqsWaitImage = SugarThemeRegistry::current()->getImageURL('sqsWait.gif');

        return <<<EOJAVASCRIPT

    document.getElementById('goback').onclick = function()
    {
        document.getElementById('{$this->currentFormID}').action.value = '{$this->previousAction}';
        //bug #48960: CSS didn't load when use click back in the step2 (external sources are selected for contacts)
        //need to unset 'to_pdf' in extstep1.tpl
        if (document.getElementById('{$this->currentFormID}').to_pdf)
        {
            document.getElementById('{$this->currentFormID}').to_pdf.value = '';
        }
        return true;
    }

ImportView = {

    validateMappings : function()
    {
        // validate form
        clear_all_errors();
        var form = document.getElementById('{$this->currentFormID}');
        var hash = new Object();
        var required = new Object();
        $print_required_array
        var isError = false;
        for ( i = 0; i < form.length; i++ ) {
            if ( form.elements[i].name.indexOf("colnum",0) == 0) {
                if ( form.elements[i].value == "-1") {
                    continue;
                }
                if ( hash[ form.elements[i].value ] == 1) {
                    isError = true;
                    add_error_style('{$this->currentFormID}',form.elements[i].name,"{$mod_strings['ERR_MULTIPLE']}");
                }
                hash[form.elements[i].value] = 1;
            }
        }

        // check for required fields
        for(var field_name in required) {
            // contacts hack to bypass errors if full_name is set
            if (field_name == 'last_name' &&
                    hash['full_name'] == 1) {
                continue;
            }
            if ( hash[ field_name ] != 1 ) {
                isError = true;
                add_error_style('{$this->currentFormID}',form.colnum_0.name,
                    "{$mod_strings['ERR_MISSING_REQUIRED_FIELDS']} " + required[field_name]);
            }
        }

        // return false if we got errors
        if (isError == true) {
            return false;
        }


        return true;

    }

}

if( document.getElementById('gonext') )
{
    document.getElementById('gonext').onclick = function(){

        if( ImportView.validateMappings() )
        {
            // Move on to next step
            document.getElementById('{$this->currentFormID}').action.value = '{$this->nextAction}';
            return true;
        }
        else
            return false;
    }
}

// handle adding new row
document.getElementById('addrow').onclick = function(){

    toggleDefaultColumnVisibility(false);
    rownum = document.getElementById('{$this->currentFormID}').columncount.value;
    newrow = document.createElement("tr");

    column0 = document.getElementById('row_0_col_0').cloneNode(true);
    column0.id = 'row_' + rownum + '_col_0';
    for ( i = 0; i < column0.childNodes.length; i++ ) {
        if ( column0.childNodes[i].name == 'colnum_0' ) {
            column0.childNodes[i].name = 'colnum_' + rownum;
            column0.childNodes[i].onchange = function(){
                var module    = document.getElementById('{$this->currentFormID}').import_module.value;
                var fieldname = this.value;
                var matches   = /colnum_([0-9]+)/i.exec(this.name);
                var fieldnum  = matches[1];
                if ( fieldname == -1 ) {
                    document.getElementById('defaultvaluepicker_'+fieldnum).innerHTML = '';
                    return;
                }
                document.getElementById('defaultvaluepicker_'+fieldnum).innerHTML = '<img src="{$sqsWaitImage}" />'
                YAHOO.util.Connect.asyncRequest('GET', 'index.php?module=Import&action=GetControl&import_module='+module+'&field_name='+fieldname,
                    {
                        success: function(o)
                        {
                        	document.getElementById('defaultvaluepicker_'+fieldnum).innerHTML = o.responseText;
                            SUGAR.util.evalScript(o.responseText);
                            enableQS(true);
                        },
                        failure: function(o) {/*failure handler code*/}
                    });
            }
        }
    }

    var removeButton = document.createElement("button");
    removeButton.title = "{$mod_strings['LBL_REMOVE_ROW']}";
    removeButton.id = 'deleterow_' + rownum;
    removeButton.className = "removeButton";
    var imgButton = document.createElement("span");
    imgButton.className = 'suitepicon suitepicon-action-minus';
    removeButton.appendChild(imgButton);


    if ( document.getElementById('row_0_header') ) {
        column1 = document.getElementById('row_0_header').cloneNode(true);
        column1.innerHTML = '&nbsp;';
        column1.style.textAlign = "right";
        newrow.appendChild(column1);
        column1.appendChild(removeButton);
    }

    newrow.appendChild(column0);



    column3 = document.createElement('td');
    column3.className = 'tabDetailViewDL';
    if ( !document.getElementById('row_0_header') ) {
        column3.colSpan = 2;
    }

    newrow.appendChild(column3);

    column2 = document.getElementById('defaultvaluepicker_0').cloneNode(true);
    column2.id = 'defaultvaluepicker_' + rownum;
    newrow.appendChild(column2);

    document.getElementById('{$this->currentFormID}').columncount.value = parseInt(document.getElementById('{$this->currentFormID}').columncount.value) + 1;

    document.getElementById('row_0_col_0').parentNode.parentNode.insertBefore(newrow,this.parentNode.parentNode);

    document.getElementById('deleterow_' + rownum).onclick = function(){
        this.parentNode.parentNode.parentNode.removeChild(this.parentNode.parentNode);
    }


}

function toggleDefaultColumnVisibility(hide)
{
    if( typeof(hide) != 'undefined' && typeof(hide) == 'boolean')
    {
        var currentStyle = hide ? '' : 'none';
    }
    else
    {
        var currentStyle = YAHOO.util.Dom.getStyle('default_column_header_span', 'display');
    }
    if(currentStyle == 'none')
    {
        var newStyle = '';
        var bgColor = '#eeeeee';
        YAHOO.util.Dom.addClass('hide_default_link', 'collapse');
        YAHOO.util.Dom.removeClass('hide_default_link', 'expand');
        var col2Rowspan = "1";
    }
    else
    {
        var newStyle = 'none';
        var bgColor = '#dddddd';
        YAHOO.util.Dom.addClass('hide_default_link', 'expand');
        YAHOO.util.Dom.removeClass('hide_default_link', 'collapse');
        var col2Rowspan = "2";
    }

    YAHOO.util.Dom.setStyle('default_column_header_span', 'display', newStyle);
    YAHOO.util.Dom.setStyle('default_column_header', 'backgroundColor', bgColor);

    //Toggle all rows.
    var columnCount = document.getElementById('{$this->currentFormID}').columncount.value;
    for(i=0;i<columnCount;i++)
    {
        YAHOO.util.Dom.setStyle('defaultvaluepicker_' + i, 'display', newStyle);
        YAHOO.util.Dom.setAttribute('row_'+i+'_col_2', 'colspan', col2Rowspan);
    }
}

var notesEl = document.getElementById('toggleNotes');
if(notesEl)
{
    notesEl.onclick = function() {
        if (document.getElementById('importNotes').style.display == 'none'){
            document.getElementById('importNotes').style.display = '';
            document.getElementById('toggleNotes').value='{$mod_strings['LBL_HIDE_NOTES']}';
        }
        else {
            document.getElementById('importNotes').style.display = 'none';
            document.getElementById('toggleNotes').value='{$mod_strings['LBL_SHOW_NOTES']}';
        }
    }
}


YAHOO.util.Event.onDOMReady(function(){
    toggleDefaultColumnVisibility();
    YAHOO.util.Event.addListener('hide_default_link', "click", toggleDefaultColumnVisibility);

    var selects = document.getElementsByTagName('select');
    for (var i = 0; i < selects.length; ++i ){
        if (selects[i].name.indexOf("colnum_") != -1 ) {
            // fetch the field input control via ajax
            selects[i].onchange = function(){
                var module    = document.getElementById('{$this->currentFormID}').import_module.value;
                var fieldname = this.value;
                var matches   = /colnum_([0-9]+)/i.exec(this.name);
                var fieldnum  = matches[1];
                if ( fieldname == -1 ) {
                    document.getElementById('defaultvaluepicker_'+fieldnum).innerHTML = '';
                    return;
                }

                document.getElementById('defaultvaluepicker_'+fieldnum).innerHTML = '<img src="{$sqsWaitImage}" />'
                YAHOO.util.Connect.asyncRequest('GET', 'index.php?module=Import&action=GetControl&import_module='+module+'&field_name='+fieldname,
                    {
                        success: function(o)
                        {
                            document.getElementById('defaultvaluepicker_'+fieldnum).innerHTML = o.responseText;
                            SUGAR.util.evalScript(o.responseText);
                            enableQS(true);
                        },
                        failure: function(o) {/*failure handler code*/}
                    });
            }
        }
    }
    var inputs = document.getElementsByTagName('input');
    for (var i = 0; i < inputs.length; ++i ){
        if (inputs[i].id.indexOf("deleterow_") != -1 ) {
            inputs[i].onclick = function(){
                this.parentNode.parentNode.parentNode.removeChild(this.parentNode.parentNode);
            }
        }
    }
});

enableQS(false);


EOJAVASCRIPT;
    }
}
