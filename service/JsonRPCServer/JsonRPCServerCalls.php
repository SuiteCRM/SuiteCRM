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

require_once __DIR__ . '/../../soap/SoapHelperFunctions.php';
require_once __DIR__ . '/../../include/json_config.php';
require_once __DIR__ . '/../../include/utils.php';
require_once __DIR__ . '/JsonRPCServerUtils.php';

/**
 * Class JsonServerCalls
 */
#[\AllowDynamicProperties]
class JsonRPCServerCalls
{
    /**
     * Prints out a SugarBean
     * @param string $request_id
     * @param array $params
     * @return array
     */
    public function retrieve($request_id, $params)
    {
        $jsonConfig = new json_config();

        $module = $params[0]['module'];
        $record = $params[0]['record'];

        $focus = BeanFactory::getBean($module, $record);
        $focus->retrieve($record);

        // to get a simplified version of the SugarBean
        $module_arr = $jsonConfig->populateBean($focus);

        $response = array();
        $response['id'] = $request_id;
        $response['result'] = array(
            'status' => 'success',
            'record' => $module_arr
        );

        return $response;
    }

    /**
     *
     * @param $request_id
     * @param array $params
     * @return array
     */
    public function query($request_id, $params)
    {
        global $response, $sugar_config, $db;

        $jsonParser = getJSONobj();
        $jsonConfig = new json_config();
        $jsonServerUtils = new JsonRPCServerUtils();
        $list_arr = array();
        // override query limits
        if ($sugar_config['list_max_entries_per_page'] < 31)
        {
            $sugar_config['list_max_entries_per_page'] = 31;
        }

        $args = $params[0];

        //decode condition parameter values..
        if (is_array($args['conditions'])) {
            foreach ($args['conditions'] as $key => $condition) {
                if (!empty($condition['value'])) {
                    $where = $jsonParser::decode(mb_convert_encoding($condition['value'], 'UTF-8', 'ISO-8859-1'));
                    $where = empty($where) ? $condition['value'] : $where;
                    $args['conditions'][$key]['value'] = $db->quote($where);
                }
            }
        }

        $list_return = array();

        if (!empty($args['module'])) {
            $args['modules'] = array($args['module']);
        }

        foreach ($args['modules'] as $module) {
            $focus = BeanFactory::getBean($module);

            $query_orderby = '';
            if (!empty($args['order'])) {
                $query_orderby = preg_replace('/[^\w_.-]+/i', '', (string) $args['order']['by']);
                if (!empty($args['order']['desc'])) {
                    $query_orderby .= ' DESC';
                } else {
                    $query_orderby .= ' ASC';
                }
            }

            $query_limit = '';
            if (!empty($args['limit'])) {
                $query_limit = (int)$args['limit'];
            }

            if (isset($args['field_list'])) {
                $args['field_list'] = $jsonConfig->listFilter($module, $args['field_list']);
            }

            $query_where = $jsonServerUtils->constructWhere($args, $focus->table_name, $module);
            $list_arr = array();
            if ($focus->ACLAccess('ListView', true)) {
                $focus->ungreedy_count = false;
                $curlist = $focus->get_list($query_orderby, $query_where, 0, $query_limit, -1, 0);
                $list_return = array_merge($list_return, $curlist['list']);
            }
        }

        $app_list_strings = null;

        $max = count($list_return);
        for ($i = 0; $i < $max; $i++) {
            if (isset($list_return[$i]->emailAddress) && is_object($list_return[$i]->emailAddress)) {
                $list_return[$i]->emailAddress->handleLegacyRetrieve($list_return[$i]);
            }

            $list_arr[$i] = array();
            $list_arr[$i]['fields'] = array();
            $list_arr[$i]['module'] = $list_return[$i]->object_name;

            foreach ($args['field_list'] as $field) {
                if (!empty($list_return[$i]->field_name_map[$field]['sensitive'])) {
                    continue;
                }
                // handle enums
                if ((isset($list_return[$i]->field_name_map[$field]['type']) && $list_return[$i]->field_name_map[$field]['type'] === 'enum') ||
                    (isset($list_return[$i]->field_name_map[$field]['custom_type']) && $list_return[$i]->field_name_map[$field]['custom_type'] === 'enum')
                ) {

                    // get fields to match enum vals
                    if (empty($app_list_strings)) {
                        $current_language = get_current_language();
                        $app_list_strings = return_app_list_strings_language($current_language);
                    }

                    // match enum vals to text vals in language pack for return
                    if (!empty($app_list_strings[$list_return[$i]->field_name_map[$field]['options']])) {
                        $list_return[$i]->$field = $app_list_strings[$list_return[$i]->field_name_map[$field]['options']][$list_return[$i]->$field];
                    }
                }

                $list_arr[$i]['fields'][$field] = $list_return[$i]->$field;
            }
        }

        $response['id'] = $request_id;
        $response['result'] = array('list' => $list_arr);

        return $response;
    }
}