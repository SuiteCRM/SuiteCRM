<?php
/**
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation.
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */

#[\AllowDynamicProperties]
class stic_Custom_View_Conditions extends Basic
{
    public $new_schema = true;
    public $module_dir = 'stic_Custom_View_Conditions';
    public $object_name = 'stic_Custom_View_Conditions';
    public $table_name = 'stic_custom_view_conditions';
    public $importable = false;

    public $id;
    public $name;
    public $date_entered;
    public $date_modified;
    public $modified_user_id;
    public $modified_by_name;
    public $created_by;
    public $created_by_name;
    public $description;
    public $deleted;
    public $created_by_link;
    public $modified_user_link;
    public $SecurityGroups;
    public $condition_order;
    public $field;
    public $operator;
    public $condition_type;
    public $value;
    public $value_type;

    public function bean_implements($interface)
    {
        switch ($interface) {
            case 'ACL':
                return true;
        }

        return false;
    }

    public function __construct()
    {
        parent::__construct();
    }
    public function save_lines($post_data, $view_module, $parent, $key = '')
    {
        require_once 'modules/AOW_WorkFlow/aow_utils.php';

        $field = $key . 'field';
        $postedField = null;
        if (isset($post_data[$field])) {
            $postedField = $post_data[$field];
        } else {
            $GLOBALS['log']->warn('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . 'Posted field is undefined: ' . $field);
        }

        $line_count = count((array) $postedField);
        $j = 0;
        for ($i = 0; $i < $line_count; ++$i) {
            if (!isset($post_data[$key . 'deleted'][$i])) {
                $GLOBALS['log']->warn('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . 'stic Custom View Condition trying to save lines but POST data does not contains the key "' . $key . 'deleted' . '" at index: ' . $i);
            }

            if (isset($post_data[$key . 'deleted'][$i]) && $post_data[$key . 'deleted'][$i] == 1) {
                $this->mark_deleted($post_data[$key . 'id'][$i]);
            } else {
                $condition = BeanFactory::newBean('stic_Custom_View_Conditions');
                foreach ($this->field_defs as $field_def) {
                    $field_name = $field_def['name'];
                    if (isset($post_data[$key . $field_name][$i])) {
                        if (is_array($post_data[$key . $field_name][$i])) {
                            $post_data[$key . $field_name][$i] = encodeMultienumValue($post_data[$key . $field_name][$i]);
                        } else {
                            if ($field_name === 'value') {
                                if (isset($post_data[$key . "condition_type"][$i]) && $post_data[$key . "condition_type"][$i] === "value") {
                                    $post_data[$key . $field_name][$i] = fixUpFormatting($view_module, $condition->field, $post_data[$key . $field_name][$i]);
                                }
                                if (isset($post_data["display_" . $key . $field_name]) && isset($post_data["display_" . $key . $field_name][$i])) {
                                    $post_data[$key . $field_name][$i] .= "|" . $post_data["display_" . $key . $field_name][$i];
                                }
                            }
                        }
                        $condition->$field_name = $post_data[$key . $field_name][$i];
                    }
                }
                if (trim($condition->field) != '') {
                    $condition->condition_order = ++$j;
                    $condition->name = $parent->name . '-' . $condition->condition_order;
                    $condition->stic_custo233dzations_ida = $parent->id;
                    if ($condition->value===true) {
                        $condition->value = 1;
                    } else if ($condition->value===false) {
                        $condition->value = 0;
                    }
                    $condition->save();
                }
            }
        }
    }
}
