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




require_once('modules/Audit/field_assoc.php');

#[\AllowDynamicProperties]
class Audit extends SugarBean
{
    public $module_dir = "Audit";
    public $object_name = "Audit";


    // This is used to retrieve related fields from form posts.
    public $additional_column_fields = array();

    public function __construct()
    {
        parent::__construct();
    }




    public $new_schema = true;

    /**
     * @param int $id
     * @param bool $encode
     * @param bool $deleted
     * @return SugarBean|null
     */
    public function retrieve($id = -1, $encode = true, $deleted = true)
    {
        return null;
    }

    public function get_summary_text()
    {
        return $this->name;
    }

    public function create_export_query($order_by, $where)
    {
    }

    public function fill_in_additional_list_fields()
    {
    }

    public function fill_in_additional_detail_fields()
    {
    }

    public function fill_in_additional_parent_fields()
    {
    }

    public function get_list_view_data()
    {
    }

    public function get_audit_link()
    {
    }

    public function get_audit_list()
    {
        global $focus, $genericAssocFieldsArray, $moduleAssocFieldsArray, $current_user, $timedate, $app_strings, $dictionary;

        $dictionary = $dictionary ?? [];

        $audit_list = array();
        if (!empty($_REQUEST['record'])) {
            $result = $focus->retrieve($_REQUEST['record']);

            if ($result == null || !$focus->ACLAccess('', $focus->isOwner($current_user->id))) {
                sugar_die($app_strings['ERROR_NO_RECORD']);
            }
        }

        if ($focus && $focus->is_AuditEnabled()) {
            $order= ' order by '.$focus->get_audit_table_name().'.date_created desc' ;//order by contacts_audit.date_created desc
            $unknown_label = translate('LBL_UNKNOWN', 'Users');
            $query = "SELECT ".$focus->get_audit_table_name().".*, IFNULL(users.user_name, '".$unknown_label."') AS user_name FROM ".$focus->get_audit_table_name() . " LEFT JOIN users ON ".$focus->get_audit_table_name().".created_by = users.id WHERE ".$focus->get_audit_table_name().".parent_id = '$focus->id'".$order;

            $result = $focus->db->query($query);
            // We have some data.
            require('metadata/audit_templateMetaData.php');
            $fieldDefs = $dictionary['audit']['fields'];
            while (($row = $focus->db->fetchByAssoc($result))!= null) {
                $temp_list = array();

                foreach ($fieldDefs as $field) {
                    if (isset($row[$field['name']])) {
                        if (($field['name'] == 'before_value_string' || $field['name'] == 'after_value_string') &&
                                    (array_key_exists($row['field_name'], $genericAssocFieldsArray) || (!empty($moduleAssocFieldsArray[$focus->object_name]) && array_key_exists($row['field_name'], $moduleAssocFieldsArray[$focus->object_name])))
                                   ) {
                            $temp_list[$field['name']] = (new Audit())->getAssociatedFieldName($row['field_name'], $row[$field['name']]);
                        } else {
                            $temp_list[$field['name']] = $row[$field['name']];
                        }

                        if ($field['name'] == 'date_created') {
                            $date_created = '';
                            if (!empty($temp_list[$field['name']])) {
                                $date_created = $timedate->to_display_date_time($temp_list[$field['name']]);
                                $date_created = !empty($date_created)?$date_created:$temp_list[$field['name']];
                            }
                            $temp_list[$field['name']]=$date_created;
                        }
                        if (($field['name'] == 'before_value_string' || $field['name'] == 'after_value_string' || $field['name'] == 'before_value_text' || $field['name'] == 'after_value_text') && ($row['data_type'] == "enum" || $row['data_type'] == "multienum")) {
                            global $app_list_strings;
                            $enum_keys = unencodeMultienum($temp_list[$field['name']]);
                            $enum_values = array();
                            foreach ($enum_keys as $enum_key) {
                                if (isset($focus->field_defs[$row['field_name']]['options'])) {
                                    $domain = $focus->field_defs[$row['field_name']]['options'];
                                    if (isset($app_list_strings[$domain][$enum_key])) {
                                        $enum_values[] = $app_list_strings[$domain][$enum_key];
                                    }
                                }
                            }
                            if (!empty($enum_values)) {
                                $temp_list[$field['name']] = implode(', ', $enum_values);
                            }
                            if ($temp_list['data_type']==='date') {
                                $temp_list[$field['name']]=$timedate->to_display_date($temp_list[$field['name']], false);
                            }
                        } elseif (($field['name'] == 'before_value_string' || $field['name'] == 'after_value_string') && ($row['data_type'] == "datetimecombo")) {
                            if (!empty($temp_list[$field['name']]) && $temp_list[$field['name']] != 'NULL') {
                                $temp_list[$field['name']]=$timedate->to_display_date_time($temp_list[$field['name']]);
                            } else {
                                $temp_list[$field['name']] = '';
                            }
                        } elseif ($field['name'] == 'field_name') {
                            global $mod_strings;
                            if (isset($focus->field_defs[$row['field_name']]['vname'])) {
                                $label = $focus->field_defs[$row['field_name']]['vname'];
                                $temp_list[$field['name']] = translate($label, $focus->module_dir);
                            }
                        }
                    }
                }

                $temp_list['created_by'] = $row['user_name'];
                $audit_list[] = $temp_list;
            }
        }
        return $audit_list;
    }

    public function getAssociatedFieldName($fieldName, $fieldValue)
    {
        global $focus,  $genericAssocFieldsArray, $moduleAssocFieldsArray;

        if (!empty($moduleAssocFieldsArray[$focus->object_name]) && array_key_exists($fieldName, $moduleAssocFieldsArray[$focus->object_name])) {
            $assocFieldsArray =  $moduleAssocFieldsArray[$focus->object_name];
        } elseif (array_key_exists($fieldName, $genericAssocFieldsArray)) {
            $assocFieldsArray =  $genericAssocFieldsArray;
        } else {
            return $fieldValue;
        }
        $query = "";
        $field_arr = $assocFieldsArray[$fieldName];
        $query = "SELECT ";
        if (is_array($field_arr['select_field_name'])) {
            $count = count($field_arr['select_field_name']);
            $index = 1;
            foreach ($field_arr['select_field_name'] as $col) {
                $query .= $col;
                if ($index < $count) {
                    $query .= ", ";
                }
                $index++;
            }
        } else {
            $query .= $field_arr['select_field_name'];
        }

        $query .= " FROM ".$field_arr['table_name']." WHERE ".$field_arr['select_field_join']." = '".$fieldValue."'";

        $result = $focus->db->query($query);
        if (!empty($result)) {
            if ($row = $focus->db->fetchByAssoc($result)) {
                if (is_array($field_arr['select_field_name'])) {
                    $returnVal = "";
                    foreach ($field_arr['select_field_name'] as $col) {
                        $returnVal .= $row[$col]." ";
                    }
                    return $returnVal;
                }
                return $row[$field_arr['select_field_name']];
            }
        }
    }
}
