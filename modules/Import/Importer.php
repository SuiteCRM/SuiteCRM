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


require_once('modules/Import/ImportCacheFiles.php');
require_once('modules/Import/ImportFieldSanitize.php');
require_once('modules/Import/ImportDuplicateCheck.php');


#[\AllowDynamicProperties]
class Importer
{
    /**
     * @var ImportFieldSanitizer
     */
    protected $ifs;

    /**
     * @var Currency
     */
    protected $defaultUserCurrency;

    /**
     * @var importColumns
     */
    protected $importColumns;

    /**
     * @var importSource
     */
    protected $importSource;

    /**
     * @var $isUpdateOnly
     */
    protected $isUpdateOnly;

    /**
     * @var  $bean
     */
    protected $bean;

    /**
     * @var sugarToExternalSourceFieldMap
     */
    protected $sugarToExternalSourceFieldMap = array();


    public function __construct($importSource, $bean)
    {
        global $mod_strings, $sugar_config;

        $this->importSource = $importSource;

        //Vanilla copy of the bean object.
        $this->bean = $bean;

        // use our own error handler
        set_error_handler(array('Importer','handleImportErrors'), E_ALL);

        // Increase the max_execution_time since this step can take awhile
        ini_set("max_execution_time", max($sugar_config['import_max_execution_time'], 3600));

        // stop the tracker
        TrackerManager::getInstance()->pause();

        // set the default locale settings
        $this->ifs = $this->getFieldSanitizer();

        //Get the default user currency
        $this->defaultUserCurrency = BeanFactory::newBean('Currencies');
        $this->defaultUserCurrency->retrieve('-99');

        //Get our import column definitions
        $this->importColumns = $this->getImportColumns();
        $this->isUpdateOnly = (isset($_REQUEST['import_type']) && $_REQUEST['import_type'] == 'update');
    }

    public function import()
    {
        foreach ($this->importSource as $row) {
            $this->importRow($row);
        }

        // save mapping if requested
        if (isset($_REQUEST['save_map_as']) && $_REQUEST['save_map_as'] != '') {
            $this->saveMappingFile();
        }

        $this->importSource->writeStatus();

        //All done, remove file.
    }


    protected function importRow($row)
    {
        global $sugar_config, $mod_strings, $current_user;

        $focus = BeanFactory::getBean($this->bean->module_name);
        $focus->unPopulateDefaultValues();
        $focus->save_from_post = false;
        $focus->team_id = null;
        ImportFieldSanitize::$createdBeans = array();
        $this->importSource->resetRowErrorCounter();
        $do_save = true;

        for ($fieldNum = 0; $fieldNum < $_REQUEST['columncount']; $fieldNum++) {
            // loop if this column isn't set
            if (!isset($this->importColumns[$fieldNum])) {
                continue;
            }

            // get this field's properties
            $field           = $this->importColumns[$fieldNum];
            $fieldDef        = $focus->getFieldDefinition($field);
            $fieldTranslated = translate((isset($fieldDef['vname'])?$fieldDef['vname']:$fieldDef['name']), $focus->module_dir)." (".$fieldDef['name'].")";
            $defaultRowValue = '';
            // Bug 37241 - Don't re-import over a field we already set during the importing of another field
            if (!empty($focus->$field)) {
                continue;
            }

            // translate strings
            global $locale;
            if (empty($locale)) {
                $locale = new Localization();
            }
            if (isset($row[$fieldNum])) {
                // issue #6442 - translateCharset was already executed in an earlier step
                $rowValue = strip_tags(trim($row[$fieldNum]));
            } elseif (isset($this->sugarToExternalSourceFieldMap[$field]) && isset($row[$this->sugarToExternalSourceFieldMap[$field]])) {
                $rowValue = $locale->translateCharset(strip_tags(trim($row[$this->sugarToExternalSourceFieldMap[$field]])), $this->importSource->importlocale_charset, $sugar_config['default_charset']);
            } else {
                $rowValue = '';
            }

            // If there is an default value then use it instead
            if (!empty($_REQUEST[$field])) {
                $defaultRowValue = $this->populateDefaultMapValue($field, $_REQUEST[$field], $fieldDef);


                if (empty($rowValue)) {
                    $rowValue = $defaultRowValue;
                    //reset the default value to empty
                    $defaultRowValue='';
                }
            }

            // Bug 22705 - Don't update the First Name or Last Name value if Full Name is set
            if (in_array($field, array('first_name','last_name')) && !empty($focus->full_name)) {
                continue;
            }

            // loop if this value has not been set
            if (!isset($rowValue)) {
                continue;
            }

            // If the field is required and blank then error out
            if (array_key_exists($field, $focus->get_import_required_fields()) && empty($rowValue) && $rowValue!='0') {
                $this->importSource->writeError($mod_strings['LBL_REQUIRED_VALUE'], $fieldTranslated, 'NULL');
                $do_save = false;
            }

            // Handle the special case "Sync to Outlook"
            if ($focus->object_name == "Contact" && $field == 'sync_contact') {
                /**
                 * Bug #41194 : if true used as value of sync_contact - add curent user to list to sync
                 */
                if (true == $rowValue || 'true' == strtolower($rowValue)) {
                    $focus->sync_contact = $focus->id;
                } elseif (false == $rowValue || 'false' == strtolower($rowValue)) {
                    $focus->sync_contact = '';
                } else {
                    $bad_names = array();
                    $returnValue = $this->ifs->synctooutlook($rowValue, $fieldDef, $bad_names);
                    // try the default value on fail
                    if (!$returnValue && !empty($defaultRowValue)) {
                        $returnValue = $this->ifs->synctooutlook($defaultRowValue, $fieldDef, $bad_names);
                    }
                    if (!$returnValue) {
                        $this->importSource->writeError($mod_strings['LBL_ERROR_SYNC_USERS'], $fieldTranslated, $bad_names);
                        $do_save = 0;
                    } else {
                        $focus->sync_contact = $returnValue;
                    }
                }
            }

            // Handle email field, if it's a semi-colon separated export
            if ($field == 'email_addresses_non_primary' && !empty($rowValue)) {
                if (strpos((string) $rowValue, ';') !== false) {
                    $rowValue = explode(';', $rowValue);
                } else {
                    $rowValue = array($rowValue);
                }
            }

            // Handle email1 and email2 fields ( these don't have the type of email )
            if ($field == 'email1' || $field == 'email2') {
                $returnValue = $this->ifs->email($rowValue, $fieldDef, $focus);
                // try the default value on fail
                if (!$returnValue && !empty($defaultRowValue)) {
                    $returnValue = $this->ifs->email($defaultRowValue, $fieldDef);
                }
                if ($returnValue === false) {
                    $do_save=0;
                    $this->importSource->writeError($mod_strings['LBL_ERROR_INVALID_EMAIL'], $fieldTranslated, $rowValue);
                } else {
                    $rowValue = $returnValue;
                    // check for current opt_out and invalid email settings for this email address
                    // if we find any, set them now
                    $emailres = $focus->db->query("SELECT opt_out, invalid_email FROM email_addresses WHERE email_address = '".$focus->db->quote($rowValue)."'");
                    if ($emailrow = $focus->db->fetchByAssoc($emailres)) {
                        $focus->email_opt_out = $emailrow['opt_out'];
                        $focus->invalid_email = $emailrow['invalid_email'];
                    }
                }
            }

            // Handle splitting Full Name into First and Last Name parts
            if ($field == 'full_name' && !empty($rowValue)) {
                $this->ifs->fullname($rowValue, $fieldDef, $focus);
            }

            // to maintain 451 compatiblity
            if (!isset($fieldDef['module']) && $fieldDef['type']=='relate') {
                $fieldDef['module'] = ucfirst($fieldDef['table']);
            }

            if (isset($fieldDef['custom_type']) && !empty($fieldDef['custom_type'])) {
                $fieldDef['type'] = $fieldDef['custom_type'];
            }

            // If the field is empty then there is no need to check the data
            if (!empty($rowValue)) {
                // If it's an array of non-primary e-mails, check each mail
                if ($field == "email_addresses_non_primary" && is_array($rowValue)) {
                    foreach ($rowValue as $tempRow) {
                        $tempRow = $this->sanitizeFieldValueByType($tempRow, $fieldDef, $defaultRowValue, $focus, $fieldTranslated);
                        if ($tempRow === false) {
                            $rowValue = false;
                            $do_save = false;
                            break;
                        }
                    }
                } else {
                    $rowValue = $this->sanitizeFieldValueByType($rowValue, $fieldDef, $defaultRowValue, $focus, $fieldTranslated);
                }

                if ($rowValue === false) {
                    /* BUG 51213 - jeff @ neposystems.com */
                    $do_save = false;
                    continue;
                }
            }

            // if the parent type is in singular form, get the real module name for parent_type
            if (isset($fieldDef['type']) && $fieldDef['type']=='parent_type') {
                $rowValue = get_module_from_singular($rowValue);
            }

            $focus->$field = $rowValue;
            unset($defaultRowValue);
        }

        // Now try to validate flex relate fields
        if (isset($focus->field_defs['parent_name']) && isset($focus->parent_name) && ($focus->field_defs['parent_name']['type'] == 'parent')) {
            // populate values from the picker widget if the import file doesn't have them
            $parent_idField = $focus->field_defs['parent_name']['id_name'];
            if (empty($focus->$parent_idField) && !empty($_REQUEST[$parent_idField])) {
                $focus->$parent_idField = $_REQUEST[$parent_idField];
            }

            $parent_typeField = $focus->field_defs['parent_name']['type_name'];

            if (empty($focus->$parent_typeField) && !empty($_REQUEST[$parent_typeField])) {
                $focus->$parent_typeField = $_REQUEST[$parent_typeField];
            }
            // now validate it
            $returnValue = $this->ifs->parent($focus->parent_name, $focus->field_defs['parent_name'], $focus, empty($_REQUEST['parent_name']));
            if (!$returnValue && !empty($_REQUEST['parent_name'])) {
                $returnValue = $this->ifs->parent($_REQUEST['parent_name'], $focus->field_defs['parent_name'], $focus);
            }
        }

        // check to see that the indexes being entered are unique.
        if (isset($_REQUEST['enabled_dupes']) && $_REQUEST['enabled_dupes'] != "") {
            $toDecode = html_entity_decode((string) $_REQUEST['enabled_dupes'], ENT_QUOTES);
            $enabled_dupes = json_decode($toDecode);
            $idc = new ImportDuplicateCheck($focus);

            if ($idc->isADuplicateRecord($enabled_dupes)) {
                $this->importSource->markRowAsDuplicate($idc->_dupedFields);
                $this->_undoCreatedBeans(ImportFieldSanitize::$createdBeans);
                return;
            }
        }
        //Allow fields to be passed in for dup check as well (used by external adapters)
        elseif (!empty($_REQUEST['enabled_dup_fields'])) {
            $toDecode = html_entity_decode((string) $_REQUEST['enabled_dup_fields'], ENT_QUOTES);
            $enabled_dup_fields = json_decode($toDecode);
            $idc = new ImportDuplicateCheck($focus);
            if ($idc->isADuplicateRecordByFields($enabled_dup_fields)) {
                $this->importSource->markRowAsDuplicate($idc->_dupedFields);
                $this->_undoCreatedBeans(ImportFieldSanitize::$createdBeans);
                return;
            }
        }

        // if the id was specified
        $newRecord = true;
        if (!empty($focus->id)) {
            $focus->id = $this->_convertId($focus->id);

            // check if it already exists
            $query = "SELECT * FROM {$focus->table_name} WHERE id='".$focus->db->quote($focus->id)."'";
            ($result = $focus->db->query($query)) || sugar_die("Error selecting sugarbean: ");

            $dbrow = $focus->db->fetchByAssoc($result);

            if (isset($dbrow['id']) && $dbrow['id'] != -1) {
                // if it exists but was deleted, just remove it
                if (isset($dbrow['deleted']) && $dbrow['deleted'] == 1) {
                    $this->removeDeletedBean($focus);
                    $focus->new_with_id = true;
                } else {
                    if (! $this->isUpdateOnly) {
                        $this->importSource->writeError($mod_strings['LBL_ID_EXISTS_ALREADY'], 'ID', $focus->id);
                        $this->_undoCreatedBeans(ImportFieldSanitize::$createdBeans);
                        return;
                    }

                    $clonedBean = $this->cloneExistingBean($focus);
                    if ($clonedBean === false) {
                        $this->importSource->writeError($mod_strings['LBL_RECORD_CANNOT_BE_UPDATED'], 'ID', $focus->id);
                        $this->_undoCreatedBeans(ImportFieldSanitize::$createdBeans);
                        return;
                    }
                    $focus = $clonedBean;
                    $newRecord = false;
                }
            } else {
                $focus->new_with_id = true;
            }
        }

        if ($do_save) {
            $this->saveImportBean($focus, $newRecord);
            // Update the created/updated counter
            $this->importSource->markRowAsImported($newRecord);
        } else {
            $this->_undoCreatedBeans(ImportFieldSanitize::$createdBeans);
        }

        unset($defaultRowValue);
    }


    protected function sanitizeFieldValueByType($rowValue, $fieldDef, $defaultRowValue, $focus, $fieldTranslated)
    {
        $fieldtype = $fieldDef['type'];
        global $mod_strings, $app_list_strings;
        switch ($fieldtype) {
            case 'enum':
            case 'dynamicenum':
            case 'multienum':
                $returnValue = $this->ifs->$fieldtype($rowValue, $fieldDef);

                // try the default value on fail
                if (!$returnValue && !empty($defaultRowValue)) {
                    $returnValue = $this->ifs->$fieldtype($defaultRowValue, $fieldDef);
                }

                if ($returnValue === false) {
                    $this->importSource->writeError(
                        $mod_strings['LBL_ERROR_NOT_IN_ENUM'] . implode(",", $app_list_strings[$fieldDef['options']]),
                        $fieldTranslated,
                        $rowValue
                    );
                    return false;
                }
                return $returnValue;

            case 'relate':
            case 'parent':
                $returnValue = $this->ifs->relate($rowValue, $fieldDef, $focus, empty($defaultRowValue));
                if (!$returnValue && !empty($defaultRowValue)) {
                    $returnValue = $this->ifs->relate($defaultRowValue, $fieldDef, $focus);
                }
                // Bug 33623 - Set the id value found from the above method call as an importColumn
                if ($returnValue !== false) {
                    $this->importColumns[] = $fieldDef['id_name'];
                }
                return $rowValue;
                break;
            case 'teamset':
                $this->ifs->teamset($rowValue, $fieldDef, $focus);
                $this->importColumns[] = 'team_set_id';
                $this->importColumns[] = 'team_id';
                return $rowValue;
                break;
            case 'fullname':
                return $rowValue;
                break;
            default:
                $returnValue = $this->ifs->$fieldtype($rowValue, $fieldDef, $focus);
                // try the default value on fail
                if (!$returnValue && !empty($defaultRowValue)) {
                    $returnValue = $this->ifs->$fieldtype($defaultRowValue, $fieldDef, $focus);
                }
                if (!$returnValue) {
                    $this->importSource->writeError($mod_strings['LBL_ERROR_INVALID_'.strtoupper($fieldtype)], $fieldTranslated, $rowValue, $focus);
                    return false;
                }
                return $returnValue;
        }
    }

    protected function cloneExistingBean($focus)
    {
        $existing_focus = clone $this->bean;
        if (!($existing_focus->retrieve($focus->id) instanceof SugarBean)) {
            return false;
        }
        $newData = $focus->toArray();
        foreach ($newData as $focus_key => $focus_value) {
            if (in_array($focus_key, $this->importColumns)) {
                $existing_focus->$focus_key = $focus_value;
            }
        }

        return $existing_focus;
    }

    protected function removeDeletedBean($focus)
    {
        global $mod_strings;

        $query2 = "DELETE FROM {$focus->table_name} WHERE id='".$focus->db->quote($focus->id)."'";
        ($result2 = $focus->db->query($query2)) || sugar_die($mod_strings['LBL_ERROR_DELETING_RECORD']." ".$focus->id);
        if ($focus->hasCustomFields()) {
            $query3 = "DELETE FROM {$focus->table_name}_cstm WHERE id_c='".$focus->db->quote($focus->id)."'";
            $result2 = $focus->db->query($query3);
        }
    }

    protected function saveImportBean($focus, $newRecord)
    {
        global $timedate, $current_user;

        // Populate in any default values to the bean
        $focus->populateDefaultValues();

        if (!isset($focus->assigned_user_id) || $focus->assigned_user_id == '' && $newRecord) {
            $focus->assigned_user_id = $current_user->id;
        }
        /*
        * Bug 34854: Added all conditions besides the empty check on date modified.
        */
        if ((!empty($focus->new_with_id) && !empty($focus->date_modified)) ||
             (is_array($focus->fetched_row) && empty($focus->new_with_id) && $timedate->to_db($focus->date_modified) != $timedate->to_db($timedate->to_display_date_time($focus->fetched_row['date_modified'])))
        ) {
            $focus->update_date_modified = false;
        }

        // Bug 53636 - Allow update of "Date Created"
        if (!empty($focus->date_entered)) {
            $focus->update_date_entered = true;
        }

        $focus->optimistic_lock = false;
        if ($focus->object_name == "Contact" && isset($focus->sync_contact)) {
            //copy the potential sync list to another varible
            $list_of_users=$focus->sync_contact;
            //and set it to false for the save
            $focus->sync_contact=false;
        } elseif ($focus->object_name == "User" && !empty($current_user) && $focus->is_admin && !is_admin($current_user) && is_admin_for_module($current_user, 'Users')) {
            sugar_die($GLOBALS['mod_strings']['ERR_IMPORT_SYSTEM_ADMININSTRATOR']);
        }
        //bug# 46411 importing Calls will not populate Leads or Contacts Subpanel
        if (!empty($focus->parent_type) && !empty($focus->parent_id)) {
            foreach ($focus->relationship_fields as $key => $val) {
                if ($val == strtolower($focus->parent_type)) {
                    $focus->$key = $focus->parent_id;
                }
            }
        }
        //bug# 40260 setting it true as the module in focus is involved in an import
        $focus->in_import=true;
        // call any logic needed for the module preSave
        $focus->beforeImportSave();

        // Bug51192: check if there are any changes in the imported data
        $hasDataChanges = false;
        $dataChanges=$focus->db->getAuditDataChanges($focus);

        if (!empty($dataChanges)) {
            foreach ($dataChanges as $field=>$fieldData) {
                if ($fieldData['data_type'] != 'date' || strtotime($fieldData['before']) !== strtotime($fieldData['after'])) {
                    $hasDataChanges = true;
                    break;
                }
            }
        }

        // if modified_user_id is set, set the flag to false so SugarBEan will not reset it
        if (isset($focus->modified_user_id) && $focus->modified_user_id && !$hasDataChanges) {
            $focus->update_modified_by = false;
        }
        // if created_by is set, set the flag to false so SugarBEan will not reset it
        if (isset($focus->created_by) && $focus->created_by) {
            $focus->set_created_by = false;
        }

        if ($focus->object_name == "Contact" && isset($list_of_users)) {
            $focus->process_sync_to_outlook($list_of_users);
        }

        $focus->save(false);

        //now that save is done, let's make sure that parent and related id's were saved as relationships
        //this takes place before the afterImportSave()
        $this->checkRelatedIDsAfterSave($focus);

        // call any logic needed for the module postSave
        $focus->afterImportSave();

        // Add ID to User's Last Import records
        if ($newRecord) {
            $this->importSource->writeRowToLastImport($_REQUEST['import_module'], ($focus->object_name == 'Case' ? 'aCase' : $focus->object_name), $focus->id);
        }
    }

    protected function saveMappingFile()
    {
        global $current_user;

        $firstrow    = json_decode(html_entity_decode((string) $_REQUEST['firstrow']), true);
        $mappingValsArr = $this->importColumns;
        $mapping_file = BeanFactory::newBean('Import_1');
        if (isset($_REQUEST['has_header']) && $_REQUEST['has_header'] == 'on') {
            $header_to_field = array();
            foreach ($this->importColumns as $pos => $field_name) {
                if (isset($firstrow[$pos]) && isset($field_name)) {
                    $header_to_field[$firstrow[$pos]] = $field_name;
                }
            }

            $mappingValsArr = $header_to_field;
        }
        //get array of values to save for duplicate and locale settings
        $advMapping = $this->retrieveAdvancedMapping();

        //merge with mappingVals array
        if (!empty($advMapping) && is_array($advMapping)) {
            $mappingValsArr = $advMapping + $mappingValsArr;
        }

        //set mapping
        $mapping_file->setMapping($mappingValsArr);

        // save default fields
        $defaultValues = array();
        for ($i = 0; $i < $_REQUEST['columncount']; $i++) {
            if (isset($this->importColumns[$i]) && !empty($_REQUEST[$this->importColumns[$i]])) {
                $field = $this->importColumns[$i];
                $fieldDef = $this->bean->getFieldDefinition($field);
                if (!empty($fieldDef['custom_type']) && $fieldDef['custom_type'] == 'teamset') {
                    require_once('include/SugarFields/Fields/Teamset/SugarFieldTeamset.php');
                    $sugar_field = new SugarFieldTeamset('Teamset');
                    $teams = $sugar_field->getTeamsFromRequest($field);
                    if (isset($_REQUEST['primary_team_name_collection'])) {
                        $primary_index = $_REQUEST['primary_team_name_collection'];
                    }

                    //If primary_index was selected, ensure that the first Array entry is the primary team
                    if (isset($primary_index)) {
                        $count = 0;
                        $new_teams = array();
                        foreach ($teams as $id=>$name) {
                            if ($primary_index == $count++) {
                                $new_teams[$id] = $name;
                                unset($teams[$id]);
                                break;
                            }
                        }

                        foreach ($teams as $id=>$name) {
                            $new_teams[$id] = $name;
                        }
                        $teams = $new_teams;
                    } //if

                    $json = getJSONobj();
                    $defaultValues[$field] = $json->encode($teams);
                } else {
                    $defaultValues[$field] = $_REQUEST[$this->importColumns[$i]];
                }
            }
        }
        $mapping_file->setDefaultValues($defaultValues);
        $result = $mapping_file->save(
            $current_user->id,
            $_REQUEST['save_map_as'],
            $_REQUEST['import_module'],
            $_REQUEST['source'],
            (isset($_REQUEST['has_header']) && $_REQUEST['has_header'] == 'on'),
            $_REQUEST['custom_delimiter'],
            html_entity_decode((string) $_REQUEST['custom_enclosure'], ENT_QUOTES)
        );
    }


    protected function populateDefaultMapValue($field, $fieldValue, $fieldDef)
    {
        global $timedate, $current_user;

        if (is_array($fieldValue)) {
            $defaultRowValue = encodeMultienumValue($fieldValue);
        } else {
            $defaultRowValue = $_REQUEST[$field];
        }
        // translate default values to the date/time format for the import file
        if ($fieldDef['type'] == 'date' && $this->ifs->dateformat != $timedate->get_date_format()) {
            $defaultRowValue = $timedate->swap_formats($defaultRowValue, $this->ifs->dateformat, $timedate->get_date_format());
        }

        if ($fieldDef['type'] == 'time' && $this->ifs->timeformat != $timedate->get_time_format()) {
            $defaultRowValue = $timedate->swap_formats($defaultRowValue, $this->ifs->timeformat, $timedate->get_time_format());
        }

        if (($fieldDef['type'] == 'datetime' || $fieldDef['type'] == 'datetimecombo') && $this->ifs->dateformat.' '.$this->ifs->timeformat != $timedate->get_date_time_format()) {
            $defaultRowValue = $timedate->swap_formats($defaultRowValue, $this->ifs->dateformat.' '.$this->ifs->timeformat, $timedate->get_date_time_format());
        }

        if (in_array($fieldDef['type'], array('currency','float','int','num')) && $this->ifs->num_grp_sep != $current_user->getPreference('num_grp_sep')) {
            $defaultRowValue = str_replace($current_user->getPreference('num_grp_sep'), $this->ifs->num_grp_sep, (string) $defaultRowValue);
        }

        if (in_array($fieldDef['type'], array('currency','float')) && $this->ifs->dec_sep != $current_user->getPreference('dec_sep')) {
            $defaultRowValue = str_replace($current_user->getPreference('dec_sep'), $this->ifs->dec_sep, (string) $defaultRowValue);
        }

        $user_currency_symbol = $this->defaultUserCurrency->symbol;
        if ($fieldDef['type'] == 'currency' && $this->ifs->currency_symbol != $user_currency_symbol) {
            $defaultRowValue = str_replace($user_currency_symbol, $this->ifs->currency_symbol, (string) $defaultRowValue);
        }

        return $defaultRowValue;
    }

    protected function getImportColumns()
    {
        $importable_fields = $this->bean->get_importable_fields();
        $importColumns = array();
        foreach ($_REQUEST as $name => $value) {
            // only look for var names that start with "fieldNum"
            if (strncasecmp($name, "colnum_", 7) != 0) {
                continue;
            }

            // pull out the column position for this field name
            $pos = substr($name, 7);

            if (isset($importable_fields[$value])) {
                // now mark that we've seen this field
                $importColumns[$pos] = $value;
            }
        }

        return $importColumns;
    }

    protected function getFieldSanitizer()
    {
        $ifs = new ImportFieldSanitize();
        $copyFields = array('dateformat','timeformat','timezone','default_currency_significant_digits','num_grp_sep','dec_sep','default_locale_name_format');
        foreach ($copyFields as $field) {
            $fieldKey = "importlocale_$field";
            $ifs->$field = $this->importSource->$fieldKey;
        }

        $currency = BeanFactory::newBean('Currencies');
        $currency->retrieve($this->importSource->importlocale_currency);
        $ifs->currency_symbol = $currency->symbol;

        return $ifs;
    }

    /**
     * Sets a translation map from sugar field key to external source key used while importing a row.  This allows external sources
     * to return a data set that is an associative array rather than numerically indexed.
     *
     * @param  $translator
     * @return void
     */
    public function setFieldKeyTranslator($translator)
    {
        $this->sugarToExternalSourceFieldMap = $translator;
    }

    /**
     * If a bean save is not done for some reason, this method will undo any of the beans that were created
     *
     * @param array $ids ids of user_last_import records created
     */
    protected function _undoCreatedBeans(array $ids)
    {
        $focus = BeanFactory::newBean('Import_2');
        foreach ($ids as $id) {
            $focus->undoById($id);
        }
    }

    /**
     * clean id's when being imported
     *
     * @param  string $string
     * @return string
     */
    protected function _convertId($string)
    {
        $function = function ($matches) {
            return ord($matches[0]);
        };

        return preg_replace_callback(
            '|[^A-Za-z0-9\-\_]|',
            $function,
            $string
        );
    }

    public function retrieveAdvancedMapping()
    {
        $advancedMappingSettings = array();

        //harvest the dupe index settings
        if (isset($_REQUEST['enabled_dupes'])) {
            $toDecode = html_entity_decode((string) $_REQUEST['enabled_dupes'], ENT_QUOTES);
            $dupe_ind = json_decode($toDecode);

            foreach ($dupe_ind as $dupe) {
                $advancedMappingSettings['dupe_'.$dupe] = $dupe;
            }
        }

        foreach ($_REQUEST as $rk=>$rv) {
            //harvest the import locale settings
            if (strpos($rk, 'portlocale_')>0) {
                $advancedMappingSettings[$rk] = $rv;
            }
        }
        return $advancedMappingSettings;
    }

    public static function getImportableModules()
    {
        global $beanList;
        $importableModules = array();
        foreach ($beanList as $moduleName => $beanName) {
            if (class_exists($beanName)) {
                $tmp = new $beanName();
                if (isset($tmp->importable) && $tmp->importable) {
                    $label = isset($GLOBALS['app_list_strings']['moduleList'][$moduleName]) ? $GLOBALS['app_list_strings']['moduleList'][$moduleName] : $moduleName;
                    $importableModules[$moduleName] = $label;
                }
            }
        }

        asort($importableModules);
        return $importableModules;
    }


    /**
     * Replaces PHP error handler in Step4
     *
     * @param int    $errno
     * @param string $errstr
     * @param string $errfile
     * @param string $errline
     */
    public static function handleImportErrors($errno, $errstr, $errfile, $errline)
    {
        $GLOBALS['log']->fatal("Caught error: $errstr");

        if (!defined('E_DEPRECATED')) {
            define('E_DEPRECATED', '8192');
        }
        if (!defined('E_USER_DEPRECATED')) {
            define('E_USER_DEPRECATED', '16384');
        }

        $isFatal = false;
        switch ($errno) {
            case E_USER_ERROR:
                $message = "ERROR: [$errno] $errstr on line $errline in file $errfile<br />\n";
                $isFatal = true;
                break;
            case E_USER_WARNING:
            case E_WARNING:
                $message = "WARNING: [$errno] $errstr on line $errline in file $errfile<br />\n";
                break;
            case E_USER_NOTICE:
            case E_NOTICE:
                $message = "NOTICE: [$errno] $errstr on line $errline in file $errfile<br />\n";
                break;
            case E_STRICT:
            case E_DEPRECATED:
            case E_USER_DEPRECATED:
                // don't worry about these
                // $message = "STRICT ERROR: [$errno] $errstr on line $errline in file $errfile<br />\n";
                $message = "";
                break;
            default:
                $message = "Unknown error type: [$errno] $errstr on line $errline in file $errfile<br />\n";
                break;
        }

        // check to see if current reporting level should be included based upon error_reporting() setting, if not
        // then just return
        if (error_reporting() & $errno) {
            echo $message;
        }

        if ($isFatal) {
            throw new Exception('Handle import error' . ($message ? ": $message" : ''));
        }
    }


    /**
     * upon bean save, the relationships are saved by SugarBean->save_relationship_changes() method, but those values depend on
     * the request object and is not reliable during import.  This function makes sure any defined related or parent id's are processed
     * and their relationship saved.
     */
    public function checkRelatedIDsAfterSave($focus)
    {
        if (empty($focus)) {
            return false;
        }

        //check relationship fields first
        if (!empty($focus->parent_id) && !empty($focus->parent_type)) {
            $relParentName = strtolower($focus->parent_type);
            $relParentID = strtolower($focus->parent_id);
        }
        if (!empty($focus->related_id) && !empty($focus->related_type)) {
            $relName = strtolower($focus->related_type);
            $relID = strtolower($focus->related_id);
        }

        //now refresh the bean and process for parent relationship
        $focus->retrieve($focus->id);
        if (!empty($relParentName) && !empty($relParentID)) {

            //grab the relationship and any available ids
            if (!empty($focus->$relParentName)) {
                $rel_ids=array();
                $focus->load_relationship($relParentName);
                $rel_ids = $focus->$relParentName->get();

                //if the current parent_id is not part of the stored rels, then add it
                if (!in_array($relParentID, $rel_ids)) {
                    $focus->$relParentName->add($relParentID);
                }
            }
        }

        //now lets process any related fields
        if (!empty($relName) && !empty($relID)) {
            if (!empty($focus->$relName)) {
                $rel_ids=array();
                $focus->load_relationship($relName);
                $rel_ids = $focus->$relName->get();

                //if the related_id is not part of the stored rels, then add it
                if (!in_array($relID, $rel_ids)) {
                    $focus->$relName->add($relID);
                }
            }
        }
    }
}
