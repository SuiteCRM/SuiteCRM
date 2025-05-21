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
class stic_Custom_View_Actions extends Basic
{
    public $new_schema = true;
    public $module_dir = 'stic_Custom_View_Actions';
    public $object_name = 'stic_Custom_View_Actions';
    public $table_name = 'stic_custom_view_actions';
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
    public $action_order;
    public $type;
    public $field;
    public $field_change_type;
    public $panel;
    public $panel_change_type;
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

        $type = $key . 'type';
        $postedType = null;
        if (isset($post_data[$type])) {
            $postedType = $post_data[$type];
        } else {
            $GLOBALS['log']->warn('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . 'Posted field is undefined: ' . $type);
        }

        $line_count = count((array) $postedType);
        $j = 0;
        for ($i = 0; $i < $line_count; ++$i) {
            if (!isset($post_data[$key . 'deleted'][$i])) {
                $GLOBALS['log']->warn('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . 'stic Custom View Action trying to save lines but POST data does not contains the key "' . $key . 'deleted' . '" at index: ' . $i);
            }

            if (isset($post_data[$key . 'deleted'][$i]) && $post_data[$key . 'deleted'][$i] == 1) {
                $this->mark_deleted($post_data[$key . 'id'][$i]);
            } else {
                $action = BeanFactory::newBean('stic_Custom_View_Actions');
                foreach ($this->field_defs as $field_def) {
                    $field_name = $field_def['name'];
                    if (isset($post_data[$key . $field_name][$i])) {
                        if (is_array($post_data[$key . $field_name][$i])) {
                            $post_data[$key . $field_name][$i] = encodeMultienumValue($post_data[$key . $field_name][$i]);
                        } else {
                            if ($field_name === 'value' &&
                                $post_data[$key . 'type'][$i] === 'field_modification' && $post_data[$key . 'action'][$i] === 'fixed_value') {
                                $post_data[$key . $field_name][$i] = fixUpFormatting($view_module, $action->element, $post_data[$key . $field_name][$i]);
                                if (isset($post_data["display_" . $key . $field_name]) && isset($post_data["display_" . $key . $field_name][$i])) {
                                    $post_data[$key . $field_name][$i] .= "|" . $post_data["display_" . $key . $field_name][$i];
                                }
                            }
                        }
                        $action->$field_name = $post_data[$key . $field_name][$i];
                    }
                }
                if (trim($action->type) != '') {
                    $action->action_order = ++$j;
                    $action->name = $parent->name . '-' . $action->action_order;
                    $action->stic_custo077ezations_ida = $parent->id;
                    $action->save();
                }
            }
        }
    }
}
