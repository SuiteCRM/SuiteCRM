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


require_once('include/SugarObjects/templates/person/Person.php');
require_once('include/MVC/SugarModule.php');
require_once('include/utils.php');

/**
 * quicksearchQuery class, handles AJAX calls from quicksearch.js
 *
 * @copyright  2004-2007 SugarCRM Inc.
 * @license    http://www.sugarcrm.com/crm/products/sugar-professional-eula.html  SugarCRM Professional End User License
 * @since      Class available since Release 4.5.1
 */
#[\AllowDynamicProperties]
class quicksearchQuery
{
    /**
     * Condition operators
     * @var string
     */
    public const CONDITION_CONTAINS    = 'contains';
    public const CONDITION_LIKE_CUSTOM = 'like_custom';
    public const CONDITION_EQUAL       = 'equal';

    protected $extra_where;

    /**
     * Query a module for a list of items
     *
     * @param array $args
     * example for querying Account module with 'a':
     * array ('modules' => array('Accounts'), // module to use
     *        'field_list' => array('name', 'id'), // fields to select
     *        'group' => 'or', // how the conditions should be combined
     *        'conditions' => array(array( // array of where conditions to use
     *                              'name' => 'name', // field
     *                              'op' => 'like_custom', // operation
     *                              'end' => '%', // end of the query
     *                              'value' => 'a',  // query value
     *                              )
     *                        ),
     *        'order' => 'name', // order by
     *        'limit' => '30', // limit, number of records to return
     *       )
     * @return array list of elements returned
     */
    public function query($args)
    {
        $args = $this->prepareArguments($args);
        $args = $this->updateQueryArguments($args);
        $data = $this->getRawResults($args);

        return $this->getFormattedJsonResults($data, $args);
    }

    /**
     * get_contact_array
     *
     */
    public function get_contact_array($args)
    {
        $args    = $this->prepareArguments($args);
        $args    = $this->updateContactArrayArguments($args);
        $data    = $this->getRawResults($args);
        $results = $this->prepareResults($data, $args);

        return $this->getFilteredJsonResults($results);
    }

    /**
     * Returns the list of users, faster than using query method for Users module
     *
     * @param array $args arguments used to construct query, see query() for example
     * @return array list of users returned
     */
    public function get_user_array($args)
    {
        $condition = $args['conditions'][0]['value'];
        $results   = $this->getUserResults($condition);

        return $this->getJsonEncodedData($results);
    }


    /**
     * Returns search results from external API
     *
     * @param array $args
     * @return array
     */
    public function externalApi($args)
    {
        require_once('include/externalAPI/ExternalAPIFactory.php');
        $data = array();
        try {
            $api = ExternalAPIFactory::loadAPI($args['api']);
            $data['fields']     = $api->searchDoc($_REQUEST['query']);
            $data['totalCount'] = is_countable($data['fields']) ? count($data['fields']) : 0;
        } catch (Exception $ex) {
            $GLOBALS['log']->error($ex->getMessage());
        }

        return $this->getJsonEncodedData($data);
    }


    /**
     * Internal function to construct where clauses
     *
     * @param Object $focus
     * @param array $args
     * @return string
     */
    protected function constructWhere($focus, $args)
    {
        global $locale, $current_user;
        $db = DBManagerFactory::getInstance();

        $table = $focus->getTableName();
        if (!empty($table)) {
            $table_prefix = $db->getValidDBName($table).".";
        } else {
            $table_prefix = '';
        }
        $conditionArray = array();

        if (!isset($args['conditions']) || !is_array($args['conditions'])) {
            $args['conditions'] = array();
        }

        foreach ($args['conditions'] as $condition) {
            if (isset($condition['op'])) {
                $operator = $condition['op'];
            } else {
                $operator = null;
            }

            switch ($operator) {
                case self::CONDITION_CONTAINS:
                    array_push(
                        $conditionArray,
                        sprintf(
                            "%s like '%%%s%%'",
                            $table_prefix . $db->getValidDBName($condition['name']),
                            $db->quote(
                                $condition['value']
                    )
                        )
                    );
                    break;

                case self::CONDITION_LIKE_CUSTOM:
                    $like = '';
                    if (!empty($condition['begin'])) {
                        $like .= $db->quote($condition['begin']);
                    }
                    $like .= $db->quote($condition['value']);

                    if (!empty($condition['end'])) {
                        $like .= $db->quote($condition['end']);
                    }

                    if ($focus instanceof Person) {
                        $nameFormat = $locale->getLocaleFormatMacro($current_user);

                        if (strpos((string) $nameFormat, 'l') > strpos((string) $nameFormat, 'f')) {
                            array_push(
                                $conditionArray,
                                $db->concat($table, array('first_name','last_name')) . " like '$like'"
                            );
                        } else {
                            array_push(
                                $conditionArray,
                                $db->concat($table, array('last_name','first_name')) . " like '$like'"
                            );
                        }
                    } else {
                        array_push(
                            $conditionArray,
                            $table_prefix . $db->getValidDBName($condition['name']) . sprintf(" like '%s'", $like)
                        );
                    }
                    break;

                case self::CONDITION_EQUAL:
                    if ($condition['value']) {
                        array_push(
                            $conditionArray,
                            sprintf("(%s = '%s')", $db->getValidDBName($condition['name']), $db->quote($condition['value']))
                            );
                    }
                    break;

                default:
                    array_push(
                        $conditionArray,
                        $table_prefix.$db->getValidDBName($condition['name']) . sprintf(" like '%s%%'", $db->quote($condition['value']))
                    );
            }
        }

        $whereClauseArray = array();
        if (!empty($conditionArray)) {
            $whereClauseArray[] = sprintf('(%s)', implode(" {$args['group']} ", $conditionArray));
        }
        if (!empty($this->extra_where)) {
            $whereClauseArray[] = "({$this->extra_where})";
        }

        if ($table == 'users') {
            $whereClauseArray[] = "users.status='Active'";
        }

        return implode(' AND ', $whereClauseArray);
    }

    /**
     * Returns formatted data
     *
     * @param array $results
     * @param array $args
     * @return array
     */
    protected function formatResults($results, $args)
    {
        global $sugar_config;

        $app_list_strings = null;
        $data = [];
        $data['totalCount'] = count($results);
        $data['fields']     = array();
        $resultsCount = count($results);

        for ($i = 0; $i < $resultsCount; $i++) {
            $data['fields'][$i] = array();
            $data['fields'][$i]['module'] = $results[$i]->object_name;

            //C.L.: Bug 43395 - For Quicksearch, do not return values with salutation and title formatting
            if ($results[$i] instanceof Person) {
                $results[$i]->createLocaleFormattedName = false;
            }
            $listData = $results[$i]->get_list_view_data();

            foreach ($args['field_list'] as $field) {
                if ($field == "user_hash") {
                    continue;
                }
                // handle enums
                if ((isset($results[$i]->field_name_map[$field]['type']) && $results[$i]->field_name_map[$field]['type'] == 'enum')
                    || (isset($results[$i]->field_name_map[$field]['custom_type']) && $results[$i]->field_name_map[$field]['custom_type'] == 'enum')) {

                    // get fields to match enum vals
                    if (empty($app_list_strings)) {
                        $current_language = get_current_language();
                        $app_list_strings = return_app_list_strings_language($current_language);
                    }

                    // match enum vals to text vals in language pack for return
                    if (!empty($app_list_strings[$results[$i]->field_name_map[$field]['options']])) {
                        $results[$i]->$field = $app_list_strings[$results[$i]->field_name_map[$field]['options']][$results[$i]->$field];
                    }
                }


                if (isset($listData[$field])) {
                    $data['fields'][$i][$field] = $listData[$field];
                } else {
                    if (isset($results[$i]->$field)) {
                        $data['fields'][$i][$field] = $results[$i]->$field;
                    } else {
                        $data['fields'][$i][$field] = '';
                    }
                }
            }
        }

        if (is_array($data['fields'])) {
            foreach ($data['fields'] as $i => $recordIn) {
                if (!is_array($recordIn)) {
                    continue;
                }

                foreach ($recordIn as $col => $dataIn) {
                    if (!is_scalar($dataIn)) {
                        continue;
                    }

                    $data['fields'][$i][$col] = html_entity_decode($dataIn, ENT_QUOTES, 'UTF-8');
                }
            }
        }

        return $data;
    }

    /**
     * Filter duplicate results from the list
     *
     * @param array $list
     * @return	array
     */
    protected function filterResults($list)
    {
        $fieldsFiltered = array();
        foreach ($list['fields'] as $field) {
            $found = false;
            foreach ($fieldsFiltered as $item) {
                if ($item === $field) {
                    $found = true;
                    break;
                }
            }

            if (!$found) {
                $fieldsFiltered[] = $field;
            }
        }

        $list['totalCount'] = count($fieldsFiltered);
        $list['fields']     = $fieldsFiltered;

        return $list;
    }

    /**
     * Returns raw search results. Filters should be applied later.
     *
     * @param array $args
     * @param boolean $singleSelect
     * @return array
     */
    protected function getRawResults($args, $singleSelect = false)
    {
        $orderBy = !empty($args['order']) ? $args['order'] : '';
        $limit   = !empty($args['limit']) ? (int)$args['limit'] : '';
        $data    = array();

        foreach ($args['modules'] as $module) {
            $focus = SugarModule::get($module)->loadBean();

            $orderBy = $focus->db->getValidDBName(($args['order_by_name'] && $focus instanceof Person && $args['order'] == 'name') ? 'last_name' : $orderBy);

            if ($focus->ACLAccess('ListView', true)) {
                $where = $this->constructWhere($focus, $args);
                $data  = $this->updateData($data, $focus, $orderBy, $where, $limit, $singleSelect);
            }
        }


        return $data;
    }

    /**
     * Returns search results with all fixes applied
     *
     * @param array $data
     * @param array $args
     * @return array
     */
    protected function prepareResults($data, $args)
    {
        $results = [];
        $results['totalCount'] = $count = count($data);
        $results['fields']     = array();

        for ($i = 0; $i < $count; $i++) {
            $field = array();
            $field['module'] = $data[$i]->object_name;

            $field = $this->overrideContactId($field, $data[$i], $args);
            $field = $this->updateContactName($field, $args);

            $results['fields'][$i] = $this->prepareField($field, $args);
        }

        return $results;
    }

    /**
     * Returns user search results
     *
     * @param string $condition
     * @return array
     */
    protected function getUserResults($condition)
    {
        $users = $this->getUserArray($condition);

        $results = array();
        $results['totalCount'] = count($users);
        $results['fields']     = array();

        foreach ($users as $id => $name) {
            array_push(
                $results['fields'],
                array(
                    'id' => (string) $id,
                    'user_name' => $name,
                    'module' => 'Users'
            )
            );
        }

        return $results;
    }

    /**
     * Merges current module search results to given list and returns it
     *
     * @param array $data
     * @param SugarBean $focus
     * @param string $orderBy
     * @param string $where
     * @param string $limit
     * @param boolean $singleSelect
     * @return array
     */
    protected function updateData($data, $focus, $orderBy, $where, $limit, $singleSelect = false)
    {
        $result = $focus->get_list($orderBy, $where, 0, $limit, -1, 0, $singleSelect);

        return array_merge($data, $result['list']);
    }

    /**
     * Updates search result with proper contact name
     *
     * @param array $result
     * @param array $args
     * @return string
     */
    protected function updateContactName($result, $args)
    {
        global $locale;

        $result[$args['field_list'][0]] = $locale->getLocaleFormattedName(
            $result['first_name'],
            $result['last_name'],
            $result['salutation']
        );

        return $result;
    }

    /**
     * Overrides contact_id and reports_to_id params (to 'id')
     *
     * @param array $result
     * @param object $data
     * @param array $args
     * @return array
     */
    protected function overrideContactId($result, $data, $args)
    {
        foreach ($args['field_list'] as $field) {
            $result[$field] = (preg_match('/reports_to_id$/s', (string) $field)
                               || preg_match('/contact_id$/s', (string) $field))
                ? $data->id // "reports_to_id" to "id"
                : $data->$field;
        }

        return $result;
    }

    /**
     * Returns prepared arguments. Should be redefined in child classes.
     *
     * @param array $arguments
     * @return array
     */
    protected function prepareArguments($args)
    {
        global $sugar_config;

        // override query limits
        if (isset($args['limit']) && $sugar_config['list_max_entries_per_page'] < ($args['limit'] + 1)) {
            $sugar_config['list_max_entries_per_page'] = ($args['limit'] + 1);
        }

        $defaults = array(
            'order_by_name' => false,
        );
        $this->extra_where = '';

        // Sanitize group
        /* BUG: 52684 properly check for 'and' jeff@neposystems.com */
        if (!empty($args['group'])  && strcasecmp($args['group'], 'and') == 0) {
            $args['group'] = 'AND';
        } else {
            $args['group'] = 'OR';
        }

        return array_merge($defaults, $args);
    }

    /**
     * Returns prepared field array. Should be redefined in child classes.
     *
     * @param array $field
     * @param array $args
     * @return array
     */
    protected function prepareField($field, $args)
    {
        return $field;
    }

    /**
     * Returns user array
     *
     * @param string $condition
     * @return array
     */
    protected function getUserArray($condition)
    {
        return (showFullName())
            // utils.php, if system is configured to show full name
            ? getUserArrayFromFullName($condition, true)
            : get_user_array(false, 'Active', '', false, $condition, ' AND portal_only=0 ', false);
    }

    /**
     * Returns additional where condition for non private teams and removes arguments that have been replaced with
     * custom where clauses
     *
     * @param array $args
     * @return string
     */
    protected function getNonPrivateTeamsWhere(&$args)
    {
        $db = DBManagerFactory::getInstance();

        $where = array();
        $teams_filtered = false;

        if (isset($args['conditions']) && is_array($args['conditions'])) {
            foreach ($args['conditions'] as $i => $condition) {
                if (isset($condition['name'], $condition['value'])) {
                    switch ($condition['name']) {
                        case 'name':
                            $where[] = sprintf(
                                "(teams.name like '%s%%' OR teams.name_2 like '%s%%')",
                                $db->quote($condition['value']),
                                $db->quote($condition['value'])
                            );
                            unset($args['conditions'][$i]);
                            break;
                        case 'user_id':
                            $where[] = sprintf(
                                "teams.id IN (SELECT team_id FROM team_memberships WHERE user_id = '%s' AND deleted = 0)",
                                $db->quote($condition['value'])
                            );
                            unset($args['conditions'][$i]);
                            $teams_filtered = true;
                    }
                }
            }
        }

        if (!$teams_filtered) {
            $where[] ='teams.private = 0';
        }

        return implode(' AND ', $where);
    }

    /**
     * Returns JSON encoded data
     *
     * @param array $data
     * @return string
     */
    protected function getJsonEncodedData($data)
    {
        $json = getJSONobj();

        return $json->encodeReal($data);
    }

    /**
     * Returns formatted JSON encoded search results
     *
     * @param array $args
     * @param array $results
     * @return string
     */
    protected function getFormattedJsonResults($results, $args)
    {
        $results = $this->formatResults($results, $args);

        return $this->getJsonEncodedData($results);
    }

    /**
     * Returns filtered JSON encoded search results
     *
     * @param array $results
     * @return string
     */
    protected function getFilteredJsonResults($results)
    {
        $results = $this->filterResults($results);

        return $this->getJsonEncodedData($results);
    }

    /**
     * Returns updated arguments array
     *
     * @param array $args
     * @return array
     */
    protected function updateQueryArguments($args)
    {
        $args['order_by_name'] = true;

        return $args;
    }

    /**
     * Returns updated arguments array for contact query
     *
     * @param array $args
     * @return array
     */
    protected function updateContactArrayArguments($args)
    {
        return $args;
    }

    /**
     * Returns updated arguments array for team query
     *
     * @param array $args
     * @return array
     */
    protected function updateTeamArrayArguments($args)
    {
        $this->extra_where = $this->getNonPrivateTeamsWhere($args);
        $args['modules'] = array('Teams');

        return $args;
    }
}
