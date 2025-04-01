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
#[\AllowDynamicProperties]
class AOW_Condition extends Basic
{
    public $new_schema = true;
    public $module_dir = 'AOW_Conditions';
    public $object_name = 'AOW_Condition';
    public $table_name = 'aow_conditions';
    public $tracker_visibility = false;
    public $importable = false;
    public $disable_row_level_security = true;

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
    public $aow_workflow_id;
    public $condition_order;
    public $module_path;
    public $field;
    public $operator;
    public $value;
    public $value_type;
    public $condition_operator;

    public function __construct()
    {
        parent::__construct();
    }




    public function bean_implements($interface)
    {
        return false;
    }

    public function save_lines($post_data, $parent, $key = '')
    {
        require_once('modules/AOW_WorkFlow/aow_utils.php');

        $field = $key . 'field';
        $postedField = null;
        if (isset($post_data[$field])) {
            $postedField = $post_data[$field];
        } else {
            LoggerManager::getLogger()->warn('Posted field is undefined: ' . $field);
        }

        $line_count = count((array)$postedField);
        $j = 0;
        for ($i = 0; $i < $line_count; ++$i) {
            if (!isset($post_data[$key . 'deleted'][$i])) {
                LoggerManager::getLogger()->warn('AOR Condition trying to save lines but POST data does not contains the key "' . $key . 'deleted' . '" at index: ' . $i);
            }

            if (isset($post_data[$key . 'deleted'][$i]) && $post_data[$key . 'deleted'][$i] == 1) {
                $this->mark_deleted($post_data[$key . 'id'][$i] ?? '');
            } else {
                $condition = BeanFactory::newBean('AOW_Conditions');
                foreach ($this->field_defs as $field_def) {
                    $field_name = $field_def['name'];
                    if (isset($post_data[$key . $field_name][$i])) {
                        if (is_array($post_data[$key . $field_name][$i])) {
                            if ($field_name == 'module_path') {
                                $post_data[$key . $field_name][$i] = base64_encode(serialize($post_data[$key . $field_name][$i]));
                            } else {
                                switch ($condition->value_type) {
                                    case 'Date':
                                        $post_data[$key . $field_name][$i] = base64_encode(serialize($post_data[$key . $field_name][$i]));
                                        break;
                                    default:
                                        $post_data[$key . $field_name][$i] = encodeMultienumValue($post_data[$key . $field_name][$i]);
                                }
                            }
                        } else {
                            if ($field_name === 'value' && $post_data[$key . 'value_type'][$i] === 'Value') {
                                $post_data[$key . $field_name][$i] = fixUpFormatting($_REQUEST['flow_module'], $condition->field, $post_data[$key . $field_name][$i]);
                            }
                        }
                        $condition->$field_name = $post_data[$key . $field_name][$i];
                    }
                }
                if (trim($condition->field) != '') {
                    $condition->condition_order = ++$j;
                    $condition->aow_workflow_id = $parent->id;
                    $condition->save();
                }
            }
        }
    }
}
