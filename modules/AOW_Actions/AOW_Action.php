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
class AOW_Action extends Basic
{
    public $new_schema = true;
    public $module_dir = 'AOW_Actions';
    public $object_name = 'AOW_Action';
    public $table_name = 'aow_actions';
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
    public $action_order;
    public $action;
    public $parameters;

    public function __construct()
    {
        parent::__construct();
    }




    public function save_lines($post_data, $parent, $key = '')
    {
        if (!isset($post_data[$key . 'action'])) {
            LoggerManager::getLogger()->warn('Post data not found at key action. Key was: ' . $key);
            $postDataAtKeyAction = null;
        } else {
            $postDataAtKeyAction = $post_data[$key . 'action'];
        }

        $line_count = count((array)$postDataAtKeyAction);
        $j = 0;
        for ($i = 0; $i < $line_count; ++$i) {
            if (isset($post_data[$key . 'deleted'][$i]) && $post_data[$key . 'deleted'][$i] == 1) {
                $this->mark_deleted($post_data[$key . 'id'][$i] ?? '');
            } else {
                $action = BeanFactory::newBean('AOW_Actions');
                foreach ($this->field_defs as $field_def) {
                    $field_name = $field_def['name'];
                    if (isset($post_data[$key . $field_name][$i])) {
                        $action->$field_name = $post_data[$key . $field_name][$i];
                    }
                }
                $params = array();
                $postData = $post_data[$key . 'param'][$i] ?? [];
                foreach ($postData as $param_name => $param_value) {
                    if ($param_name == 'value') {
                        foreach ($param_value as $p_id => $p_value) {
                            if (!isset($post_data[$key . 'param'][$i]['value_type'])) {
                                LoggerManager::getLogger()->warn('AOW action error when trying to save lines, value type is undefined in post data, key and index was: ' . $key . ', ' . $i);
                            } elseif (!isset($post_data[$key . 'param'][$i]['value_type'][$p_id])) {
                                LoggerManager::getLogger()->warn('AOW action error when trying to save lines, parameter id not found in post data, parameter id was: ' . $p_id);
                            } else {
                                if ($post_data[$key . 'param'][$i]['value_type'][$p_id] == 'Value' && is_array($p_value)) {
                                    $param_value[$p_id] = encodeMultienumValue($p_value);
                                }elseif($post_data[$key . 'param'][$i]['value_type'][$p_id] == 'Value'){
                                    if (isset($params['rel_type']) && !empty($params['rel_type']) && ($params['rel_type'] !== $params['record_type'])) {
                                        $relName = $params['rel_type'];
                                        $moduleBean = BeanFactory::getBean($params['record_type']);
                                        if (!$moduleBean->load_relationship($relName)) {
                                            $GLOBALS['log']->fatal('Line '.__LINE__.': '.__METHOD__.': '."Relationship ".$relName." doesn't exist.");
                                            continue;
                                        }
                                        $moduleName = $moduleBean->$relName->getRelatedModuleName();
                                    } else {
                                        $moduleName = $params["record_type"];
                                    }
                                    $param_value[$p_id] = fixUpFormatting($moduleName, $post_data[$key . 'param'][$i]["field"][$p_id], $p_value);
                                }
                            }
                        }
                    }
                    $params[$param_name] = $param_value;
                }
                $action->parameters = base64_encode(serialize($params));
                if (trim($action->action) != '') {
                    $action->action_order = ++$j;
                    $action->aow_workflow_id = $parent->id;
                    $action->save();
                }
            }
        }
    }

    public function bean_implements($interface)
    {
        return false;
    }
}
