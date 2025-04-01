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

 * Description: Handles getting a list of fields to duplicate check and doing the duplicate checks
 * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
 * All Rights Reserved.
 */

#[\AllowDynamicProperties]
class ImportDuplicateCheck
{
    /**
     * Private reference to the bean we're dealing with
     */
    private $_focus;

    /*
     * holds current field when a duplicate has been found
     */
    public $_dupedFields =array();

    /**
     * Constructor
     *
     * @param object $focus bean
     */
    public function __construct($focus)
    {
        $this->_focus = $focus;
    }

    /**
     * Returns an array of indices for the current module
     *
     * @return array
     */
    private function _getIndexVardefs()
    {
        $indexes = $this->_focus->getIndices();

        //grab any custom indexes if they exist
        if ($this->_focus->hasCustomFields()) {
            $custmIndexes = $this->_focus->db->helper->get_indices($this->_focus->table_name.'_cstm');
            $indexes = array_merge($custmIndexes, $indexes);
        }

        if ($this->_focus->getFieldDefinition('email1')) {
            $indexes[] = array(
                'name' => 'special_idx_email1',
                'type' => 'index',
                'fields' => array('email1')
                );
        }
        if ($this->_focus->getFieldDefinition('email2')) {
            $indexes[] = array(
                'name' => 'special_idx_email2',
                'type' => 'index',
                'fields' => array('email2')
                );
        }

        return $indexes;
    }

    /**
     * Returns an array with an element for each index
     *
     * @return array
     */
    public function getDuplicateCheckIndexes()
    {
        $super_language_pack = sugarLangArrayMerge(
            return_module_language($GLOBALS['current_language'], $this->_focus->module_dir),
            $GLOBALS['app_strings']
            );

        $index_array = array();
        foreach ($this->_getIndexVardefs() as $index) {
            if ($index['type'] == "index") {
                $labelsArray = array();
                foreach ($index['fields'] as $field) {
                    if ($field == 'deleted') {
                        continue;
                    }
                    $fieldDef = $this->_focus->getFieldDefinition($field);
                    if ($fieldDef) {
                        if (isset($fieldDef['vname']) && isset($super_language_pack[$fieldDef['vname']])) {
                            $labelsArray[$fieldDef['name']] = $super_language_pack[$fieldDef['vname']];
                        } else {
                            $labelsArray[$fieldDef['name']] = $fieldDef['name'];
                        }
                    }
                }
                $index_array[$index['name']] = str_replace(":", "", implode(", ", $labelsArray));
            }
        }

        return $index_array;
    }

    /**
     * Checks to see if the given bean is a duplicate based off the given fields
     *
     * @param  array $indexlist
     * @return bool true if this bean is a duplicate or false if it isn't
     */
    public function isADuplicateRecordByFields($fieldList)
    {
        foreach ($fieldList as $field) {
            if ($field == 'email1' || $field == 'email2') {
                $emailAddress = new SugarEmailAddress();
                $email = $field;
                if ($emailAddress->getCountEmailAddressByBean($this->_focus->$email, $this->_focus, ($field == 'email1')) > 0) {
                    return true;
                }
            } else {
                $index_fields = array('deleted' => '0');
                if (is_array($field)) {
                    foreach ($field as $tmpField) {
                        if ($tmpField == 'deleted') {
                            continue;
                        }
                        if (strlen((string) $this->_focus->$tmpField) > 0) {
                            $index_fields[$tmpField] = $this->_focus->$tmpField;
                        }
                    }
                } elseif ($field != 'deleted' && strlen((string) $this->_focus->$field) > 0) {
                    $index_fields[$field] = $this->_focus->$field;
                }

                if (count($index_fields) <= 1) {
                    continue;
                }

                $newfocus = loadBean($this->_focus->module_dir);
                $result = $newfocus->retrieve_by_string_fields($index_fields, true);

                if (!is_null($result)) {
                    return true;
                }
            }
        }

        return false;
    }

    /**
     * Checks to see if the given bean is a duplicate based off the given indexes
     *
     * @param  array $indexlist
     * @return bool true if this bean is a duplicate or false if it isn't
     */
    public function isADuplicateRecord($indexlist)
    {
        // Bug #51264 : Importing updates to rows prevented by duplicates check
        if (!empty($this->_focus) && ($this->_focus instanceof SugarBean) && !empty($this->_focus->id)) {
            $_focus = clone $this->_focus;
            $_focus->id = null;
            $_focus->retrieve($this->_focus->id);
            if (!empty($_focus->id)) {
                return false;
            }
            unset($_focus);
        }

        //lets strip the indexes of the name field in the value and leave only the index name
        $origIndexList = $indexlist;
        $indexlist=array();
        $fieldlist=array();
        $customIndexlist=array();
        foreach ($origIndexList as $iv) {
            if (empty($iv)) {
                continue;
            }
            $field_index_array = explode('::', $iv);
            if ($field_index_array[0] == 'customfield') {
                //this is a custom field, so place in custom array
                $customIndexlist[] = $field_index_array[1];
            } else {
                //this is not a custom field, so place in index list
                $indexlist[] = $field_index_array[0];
                if (isset($field_index_array[1])) {
                    $fieldlist[] = $field_index_array[1];
                }
            }
        }

        //if full_name is set, then manually search on the first and last name fields before iterating through rest of fields
        //this is a special handling of the name fields on people objects, the rest of the fields are checked individually
        if (in_array('full_name', $indexlist)) {
            $newfocus = loadBean($this->_focus->module_dir);
            $result = $newfocus->retrieve_by_string_fields(array('deleted' =>'0', 'first_name'=>$this->_focus->first_name, 'last_name'=>$this->_focus->last_name), true);

            if (!is_null($result)) {
                //set dupe field to full_name and name fields
                $this->_dupedFields[] = 'full_name';
                $this->_dupedFields[] = 'first_name';
                $this->_dupedFields[] = 'last_name';
            }
        }

        // loop through var def indexes and compare with selected indexes
        foreach ($this->_getIndexVardefs() as $index) {
            // if we get an index not in the indexlist, loop
            if (!in_array($index['name'], $indexlist)) {
                continue;
            }

            // This handles the special case of duplicate email checking
            if ($index['name'] == 'special_idx_email1' || $index['name'] == 'special_idx_email2') {
                $emailAddress = new SugarEmailAddress();
                $email = $index['fields'][0];
                if ($emailAddress->getCountEmailAddressByBean(
                    $this->_focus->$email,
                    $this->_focus,
                    ($index['name'] == 'special_idx_email1')
                        ) > 0) {
                    foreach ($index['fields'] as $field) {
                        if ($field !='deleted') {
                            $this->_dupedFields[] = $field;
                        }
                    }
                }
            }
            // Adds a hook so you can define a method in the bean to handle dupe checking
            elseif (isset($index['dupeCheckFunction'])) {
                $functionName = substr_replace($index['dupeCheckFunction'], '', 0, 9);
                if (method_exists($this->_focus, $functionName) && $this->_focus->$functionName($index) === true) {
                    return $this->_focus->$functionName($index);
                }
            } else {
                $index_fields = array('deleted' => '0');
                //search only for the field we have selected
                foreach ($index['fields'] as $field) {
                    if ($field == 'deleted' ||  !in_array($field, $fieldlist)) {
                        continue;
                    }
                    if (!in_array($field, $index_fields)) {
                        if (isset($this->_focus->$field) && strlen((string) $this->_focus->$field) > 0) {
                            $index_fields[$field] = $this->_focus->$field;
                        }
                    }
                }

                // if there are no valid fields in the index field list, loop
                if (count($index_fields) <= 1) {
                    continue;
                }

                $newfocus = loadBean($this->_focus->module_dir);
                $result = $newfocus->retrieve_by_string_fields($index_fields, true);

                if (!is_null($result)) {
                    //remove deleted as a duped field
                    unset($index_fields['deleted']);

                    //create string based on array of dupe fields
                    $this->_dupedFields = array_merge(array_keys($index_fields), $this->_dupedFields);
                }
            }
        }

        //return true if any dupes were found
        if (!empty($this->_dupedFields)) {
            return true;
        }

        return false;
    }


    public function getDuplicateCheckIndexedFiles()
    {
        require_once('include/export_utils.php');
        $import_fields = $this->_focus->get_importable_fields();
        $importable_keys = array_keys($import_fields);//

        $index_array = array();
        $fields_used = array();
        $mstr_exclude_array = array('all'=>array('team_set_id','id','deleted'),'contacts'=>array('email2'), array('leads'=>'reports_to_id'), array('prospects'=>'tracker_key'));

        //create exclude array from subset of applicable mstr_exclude_array elements
        $exclude_array =  isset($mstr_exclude_array[strtolower($this->_focus->module_dir)])?array_merge($mstr_exclude_array[strtolower($this->_focus->module_dir)], $mstr_exclude_array['all']) : $mstr_exclude_array['all'];



        //process all fields belonging to indexes
        foreach ($this->_getIndexVardefs() as $index) {
            if ($index['type'] == "index") {
                foreach ($index['fields'] as $field) {
                    $fieldName='';

                    //skip this field if it is the deleted field, not in the importable keys array, or a field in the exclude array
                    if (!in_array($field, $importable_keys) || in_array($field, $exclude_array)) {
                        continue;
                    }
                    $fieldDef = $this->_focus->getFieldDefinition($field);

                    //skip if this field is already defined (from another index)
                    if (in_array($fieldDef['name'], $fields_used)) {
                        continue;
                    }

                    //get the proper export label
                    $fieldName = translateForExport($fieldDef['name'], $this->_focus);


                    $index_array[$index['name'].'::'.$fieldDef['name']] = $fieldName;
                    $fields_used[] = $fieldDef['name'];
                }
            }
        }

        //special handling for beans with first_name and last_name
        if (in_array('first_name', $fields_used) && in_array('last_name', $fields_used)) {
            //since both full name and last name fields have been mapped, add full name index
            $index_array['full_name::full_name'] = translateForExport('full_name', $this->_focus);
            $fields_used[] = 'full_name';
        }

        asort($index_array);
        return $index_array;
    }
}
