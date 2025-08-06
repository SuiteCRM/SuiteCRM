<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2023 SalesAgility Ltd.
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
class AOW_WorkFlow extends Basic
{
    public $new_schema = true;
    public $module_dir = 'AOW_WorkFlow';
    public $object_name = 'AOW_WorkFlow';
    public $table_name = 'aow_workflow';
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
    public $assigned_user_id;
    public $assigned_user_name;
    public $assigned_user_link;
    public $flow_module;
    public $status;
    public $run_when;
    public $flow_run_on;
    public $multiple_runs;
    public static $doNotRunInSaveLogic = false;

    /**
     * return an SQL operator
     * @param $key name of SQL operator
     * @return mixed SQL operator or false if $key not found
     */
    private function getSQLOperator($key)
    {
        $sqlOperatorList = [];
        $sqlOperatorList['Equal_To'] = '=';
        $sqlOperatorList['Not_Equal_To'] = '!=';
        $sqlOperatorList['Greater_Than'] = '>';
        $sqlOperatorList['Less_Than'] = '<';
        $sqlOperatorList['Greater_Than_or_Equal_To'] = '>=';
        $sqlOperatorList['Less_Than_or_Equal_To'] = '<=';
        $sqlOperatorList['Contains'] = 'LIKE';
        $sqlOperatorList['Starts_With'] = 'LIKE';
        $sqlOperatorList['Ends_With'] = 'LIKE';
        $sqlOperatorList['is_null'] = 'IS NULL';
        if (!isset($sqlOperatorList[$key])) {
            return false;
        }
        return $sqlOperatorList[$key];
    }

    /**
     * check an SQL operator is exists
     * @param $key name of SQL operator
     * @return bool true if operator exists otherwise false
     */
    private function isSQLOperator($key)
    {
        return $this->getSQLOperator($key) ? true : false;
    }

    /**
     * AOW_WorkFlow constructor.
     * @param bool $init
     */
    public function __construct($init = true)
    {
        parent::__construct();
        if ($init) {
            $this->load_flow_beans();
            require_once('modules/AOW_WorkFlow/aow_utils.php');
        }
    }

    /**
     * @param $interface
     * @return bool
     */
    public function bean_implements($interface)
    {
        switch ($interface) {
            case 'ACL':
                return true;
        }

        return false;
    }

    public function save($check_notify = false)
    {
        if (empty($this->id) || (isset($_POST['duplicateSave']) && $_POST['duplicateSave'] == 'true')) {
            unset($_POST['aow_conditions_id']);
            unset($_POST['aow_actions_id']);
        }

        $return_id = parent::save($check_notify);

        require_once('modules/AOW_Conditions/AOW_Condition.php');
        $condition = BeanFactory::newBean('AOW_Conditions');
        $condition->save_lines($_POST, $this, 'aow_conditions_');

        require_once('modules/AOW_Actions/AOW_Action.php');
        $action = BeanFactory::newBean('AOW_Actions');
        $action->save_lines($_POST, $this, 'aow_actions_');

        return $return_id;
    }

    public function mark_deleted($id)
    {
        // These are owned by the workflow, so delete them instead of just removing the link
        $beans = $this->get_linked_beans('aow_conditions');
        $beans = array_merge($beans, $this->get_linked_beans('aow_actions'));
        $beans = array_merge($beans, $this->get_linked_beans('aow_processed'));
        foreach ($beans as $bean) {
            $bean->mark_deleted($bean->id);
        }

        parent::mark_deleted($id);
    }

    public function load_flow_beans()
    {
        global $beanList, $app_list_strings;

        if (!empty($app_list_strings['moduleList'])) {
            $app_list_strings['aow_moduleList'] = $app_list_strings['moduleList'];
            foreach ($app_list_strings['aow_moduleList'] as $mkey => $mvalue) {
                if (!isset($beanList[$mkey]) || str_begin($mkey, 'AOW_')) {
                    unset($app_list_strings['aow_moduleList'][$mkey]);
                }
            }
        }

        $app_list_strings['aow_moduleList'] = array_merge(array(''=>''), (array)($app_list_strings['aow_moduleList'] ?? []));

        asort($app_list_strings['aow_moduleList']);
    }

    /**
     * Select and run all active flows
     * @return bool
     */
    public function run_flows()
    {
        $flows = $this->get_full_list('', " aow_workflow.status = 'Active'  AND (aow_workflow.run_when = 'Always' OR aow_workflow.run_when = 'In_Scheduler' OR aow_workflow.run_when = 'Create') ");

        if (empty($flows)) {
            LoggerManager::getLogger()->warn('There is no any workflow to run');
        }

        foreach ((array)$flows as $flow) {
            $flow->run_flow();
        }

        return true;
    }

    /**
     * Retrieve the beans to actioned and run the actions
     */
    public function run_flow()
    {
        AOW_WorkFlow::$doNotRunInSaveLogic = true;
        $beans = $this->get_flow_beans();
        if (!empty($beans)) {
            foreach ($beans as $bean) {
                $bean->retrieve($bean->id);
                $this->run_actions($bean);
            }
        }
        AOW_WorkFlow::$doNotRunInSaveLogic = false;
    }

    /**
     * Select and run all active flows for the specified bean
     */
    public function run_bean_flows(SugarBean $bean)
    {
        if (AOW_WorkFlow::$doNotRunInSaveLogic) {
            return;
        }

        $query = "SELECT id FROM aow_workflow WHERE aow_workflow.flow_module = '" . $bean->module_dir . "' AND aow_workflow.status = 'Active' AND (aow_workflow.run_when = 'Always' OR aow_workflow.run_when = 'On_Save' OR aow_workflow.run_when = 'Create') AND aow_workflow.deleted = 0 ";

        $result = $this->db->query($query, false);
        $flow = BeanFactory::newBean('AOW_WorkFlow');
        while (($row = $bean->db->fetchByAssoc($result)) != null) {
            $flow->retrieve($row['id']);

            if ((!defined('SUGARCRM_IS_INSTALLING') && (!isset($_REQUEST['module'])) || $_REQUEST['module'] != 'Import') || $flow->run_on_import) {
                if ($flow->check_valid_bean($bean)) {
                    $flow->run_actions($bean, true);
                }
            }
        }

        return true;
    }

    /**
     * Use the condition statements and processed table to build query to retrieve beans to be actioned
     */
    public function get_flow_beans()
    {
        global $beanList;

        $flowModule = null;
        if (isset($beanList[$this->flow_module])) {
            $flowModule = $beanList[$this->flow_module];
        } else {
            LoggerManager::getLogger()->warn('Undefined flow module in bean list: ' . $this->flow_module);
        }

        if ($flowModule) {
            $module = new $beanList[$this->flow_module]();

            $query = '';
            $query_array = array();

            $query_array['select'][] = $module->table_name.".id AS id";
            $query_array = $this->build_flow_query_where($query_array);

            if (!empty($query_array)) {
                foreach ($query_array['select'] as $select) {
                    $query .=  ($query == '' ? 'SELECT ' : ', ').$select;
                }

                $query .= ' FROM '.$module->table_name.' ';

                if (isset($query_array['join'])) {
                    foreach ($query_array['join'] as $join) {
                        $query .= $join;
                    }
                }
                if (isset($query_array['where'])) {
                    $query_where = '';
                    foreach ($query_array['where'] as $where) {
                        $query_where .=  ($query_where == '' ? 'WHERE ' : ' AND ').$where;
                    }
                    $query .= ' '.$query_where;
                }
                return $module->process_full_list_query($query);
            }
        }
        return null;
    }

    public function build_flow_custom_query_join(
        $name,
        $custom_name,
        SugarBean $module,
        $query = array()
    ) {
        if (!isset($query['join'][$custom_name])) {
            $query['join'][$custom_name] = 'LEFT JOIN '.$module->get_custom_table_name()
                    .' '.$custom_name.' ON '.$name.'.id = '. $custom_name.'.id_c ';
        }
        return $query;
    }

    public function build_flow_relationship_query_join(
        $name,
        SugarBean $module,
        $query = array()
    ) {
     global $db;
     $params = [];
        if (!isset($query['join'][$name])) {
            if ($module->load_relationship($name)) {
                $params['join_type'] = 'LEFT JOIN';
                $params['join_table_alias'] = $db->quoteIdentifier($name);
                $join = $module->$name->getJoin($params, true);

                $query['join'][$name] = $join['join'];
                $query['select'][] = $join['select']." AS '".$name."_id'";
            }
        }
        return $query;
    }

    public function build_flow_query_where($query = array())
    {
        global $beanList;

        $flowModule = null;
        if (isset($beanList[$this->flow_module])) {
            $flowModule = $beanList[$this->flow_module];
        } else {
            LoggerManager::getLogger()->warn('Undefined flow module in bean list: ' . $this->flow_module);
        }

        if ($flowModule) {
            $module = new $beanList[$this->flow_module]();

            $sql = "SELECT id FROM aow_conditions WHERE aow_workflow_id = '".$this->id."' AND deleted = 0 ORDER BY condition_order ASC";
            $result = $this->db->query($sql);

            while ($row = $this->db->fetchByAssoc($result)) {
                $condition = BeanFactory::newBean('AOW_Conditions');
                $condition->retrieve($row['id']);
                $query = $this->build_query_where($condition, $module, $query);
                if (empty($query)) {
                    return $query;
                }
            }
            if ($this->flow_run_on) {
                switch ($this->flow_run_on) {

                    case'New_Records':
                        if ($module->table_name === 'campaign_log') {
                            $query['where'][] = $module->table_name . '.' . 'activity_date' . ' > ' . "'" . $this->activity_date . "'";
                        } else {
                            $query['where'][] = $module->table_name . '.' . 'date_entered' . ' > ' . "'" . $this->date_entered . "'";
                        }
                        break;

                    case'Modified_Records':
                        if ($module->table_name === 'campaign_log') {
                            $query['where'][] = $module->table_name . '.' . 'date_modified' . ' > ' . "'" . $this->activity_date . "'" . ' AND ' . $module->table_name . '.' . 'activity_date' . ' <> ' . $module->table_name . '.' . 'date_modified';
                        } else {
                            $query['where'][] = $module->table_name . '.' . 'date_modified' . ' > ' . "'" . $this->date_entered . "'" . ' AND ' . $module->table_name . '.' . 'date_entered' . ' <> ' . $module->table_name . '.' . 'date_modified';
                        }
                        break;

                }
            }

            if (!$this->multiple_runs) {
                if (!isset($query['where'])) {
                    LoggerManager::getLogger()->warn('Undefined index: where');
                    $query['where'] = [];
                }

                $query['where'][] .= "NOT EXISTS (SELECT * FROM aow_processed WHERE aow_processed.aow_workflow_id='".$this->id."' AND aow_processed.parent_id=".$module->table_name.".id AND aow_processed.status = 'Complete' AND aow_processed.deleted = 0)";
            }

            $query['where'][] = $module->table_name.".deleted = 0 ";
        }

        return $query;
    }

    public function build_query_where(AOW_Condition $condition, $module, $query = array())
    {
        global $beanList, $app_list_strings, $sugar_config, $timedate;
        $path = unserialize(base64_decode($condition->module_path), ['allowed_classes' => false]);

        $condition_module = $module;
        $table_alias = $condition_module->table_name;
        if (isset($path[0]) && $path[0] != $module->module_dir) {
            foreach ($path as $rel) {
                $query = $this->build_flow_relationship_query_join(
                    $rel,
                    $condition_module,
                    $query
                );
                $condition_module = new $beanList[getRelatedModule($condition_module->module_dir, $rel)];
                $table_alias = $rel;
            }
        }

        $value = '';

        if ($this->isSQLOperator($condition->operator)) {
            $where_set = false;

            $data = $condition_module->field_defs[$condition->field];

            if ($data['type'] == 'relate' && isset($data['id_name'])) {
                $condition->field = $data['id_name'];
            }
            if ((isset($data['source']) && $data['source'] == 'custom_fields')) {
                $field = $table_alias.'_cstm.'.$condition->field;
                $query = $this->build_flow_custom_query_join(
                    $table_alias,
                    $table_alias . '_cstm',
                    $condition_module,
                    $query
                );
            } else if (isset($data['source']) && $data['source'] == 'non-db' && $data['type'] == 'relate' && !empty($data['link'])) {
                $rel = $data['link'];
                if (!isset($query['join'][$rel])) {
                    if ($condition_module->load_relationship($rel)) {
                        $join = $condition_module->$rel->getJoin([
                            'join_type' => 'LEFT JOIN',
                            'join_table_alias' => str_replace('.', '_', $rel),
                            'left_join_table_alias' => $table_alias,
                            'right_join_table_alias' => $table_alias
                        ], true);
                        $query['join'][$rel] = $join['join'];
                        $query['select'][] = $join['select'] . " AS '" . str_replace('.', '_', $rel) . "_id'";
                    }
                }
                $relObject = $condition_module->$rel;

                if (!empty($relObject)) {
                    if ($relObject->getRelationshipObject()->type == 'one-to-many') {
                        $field = $table_alias . '.' . $data['id_name'];
                    } else {
                        $targetTable = $relObject->getRelationshipObject()->getRelationshipTable();
                        $field = $targetTable . '.' . $data['id_name'];
                    }
                } else {
                    $field = $table_alias . '.' . $condition->field;
                }
            } else if (isset($data['source']) && $data['source'] == 'non-db' && $data['type'] == 'relate') {
                $relModule = $data['module'];
                $relBean = BeanFactory::getBean($relModule);
                $relAlias = $data['id'];
                $id_name = $data['id_name'];
                $parentFieldDef = $condition_module->getFieldDefinition($data['id_name']);
                if (!empty($parentFieldDef['source']) && $parentFieldDef['source'] == 'custom_fields') {
                    $query = $this->build_flow_custom_query_join(
                        $table_alias,
                        $table_alias . '_cstm',
                        $condition_module,
                        $query
                    );
                    $table_alias = $table_alias . '_cstm';
                }
                $primaryKey = $relBean->getPrimaryFieldDefinition();
                if (!isset($query['join'][$relAlias])) {
                    $query['join'][$relAlias] = ' LEFT JOIN ' . $relBean->getTableName()
                        . ' ' . $relAlias . ' ON ' . $table_alias . '.' . $id_name . ' = ' . $relAlias . '.' . $primaryKey['name'];
                }
                $field = $relAlias . '.' . $data['rname'];
                $relFieldDef = $relBean->getFieldDefinition($data['rname']);

                if (isset($relFieldDef['db_concat_fields'])) {
                    $field = $this->db->concat($relAlias, $relFieldDef['db_concat_fields']);
                }
            } else {
                $field = $table_alias . '.' . $condition->field;
            }

            if ($condition->operator == 'is_null') {
                $query['where'][] = '(' . $field . ' ' . $this->getSQLOperator($condition->operator) . ' OR ' . $field . ' ' . $this->getSQLOperator('Equal_To') . " '')";
                return $query;
            }

            switch ($condition->value_type) {
                case 'Field':

                    $data = null;
                    if (isset($module->field_defs[$condition->value])) {
                        $data = $module->field_defs[$condition->value];
                    } else {
                        LoggerManager::getLogger()->warn('Undefined field def for condition value in module: ' . get_class($module) . '::field_defs[' . $condition->value . ']');
                    }

                    if ($data['type'] == 'relate' && isset($data['id_name'])) {
                        $condition->value = $data['id_name'];
                    }
                    if ((isset($data['source']) && $data['source'] == 'custom_fields')) {
                        $value = $module->table_name.'_cstm.'.$condition->value;
                        $query = $this->build_flow_custom_query_join(
                            $module->table_name,
                            $module->table_name.'_cstm',
                            $module,
                            $query
                        );
                    } else {
                        $value = $module->table_name.'.'.$condition->value;
                    }
                    break;
                case 'Any_Change':
                    //can't detect in scheduler so return
                    return array();
                case 'Date':

                    $params = @unserialize(base64_decode($condition->value), ['allowed_classes' => false]);
                    if ($params === false) {
                        LoggerManager::getLogger()->error('Unserializable data given');
                        $params = [null];
                    }

                    if ($params[0] == 'now') {
                        if ($sugar_config['dbconfig']['db_type'] == 'mssql') {
                            $value  = 'GetUTCDate()';
                        } else {
                            $value = 'UTC_TIMESTAMP()';
                        }
                    } elseif (isset($params[0]) && $params[0] == 'today') {
                        if ($sugar_config['dbconfig']['db_type'] == 'mssql') {
                            //$field =
                            $value  = 'CAST(GETDATE() AS DATE)';
                        } else {
                            $field = 'DATE('.$field.')';
                            $value = 'Curdate()';
                        }
                    } else {
                        if (isset($params[0]) && $params[0] == 'today') {
                            if ($sugar_config['dbconfig']['db_type'] == 'mssql') {
                                //$field =
                                $value  = 'CAST(GETDATE() AS DATE)';
                            } else {
                                $field = 'DATE('.$field.')';
                                $value = 'Curdate()';
                            }
                        } else {
                            $data = null;
                            if (isset($module->field_defs[$params[0]])) {
                                $data = $module->field_defs[$params[0]];
                            } else {
                                LoggerManager::getLogger()->warn('Filed def data is missing: ' . get_class($module) . '::$field_defs[' . $params[0] . ']');
                            }

                        if ((isset($data['source']) && $data['source'] == 'custom_fields')) {
                            $value = $module->table_name.'_cstm.'.$params[0];
                            $query = $this->build_flow_custom_query_join(
                                $module->table_name,
                                $module->table_name.'_cstm',
                                $module,
                                $query
                            );
                            } else {
                                $value = $module->table_name.'.'.$params[0];
                            }
                        }
                    }

                    if ($params[1] != 'now') {
                        switch ($params[3]) {
                            case 'business_hours':
                                if (file_exists('modules/AOBH_BusinessHours/AOBH_BusinessHours.php') && $params[0] == 'now') {
                                    require_once('modules/AOBH_BusinessHours/AOBH_BusinessHours.php');

                                    $businessHours = BeanFactory::newBean('AOBH_BusinessHours');

                                    $amount = $params[2];

                                    if ($params[1] != "plus") {
                                        $amount = 0-$amount;
                                    }
                                    $value = $businessHours->addBusinessHours($amount);
                                    $value = "'".$timedate->asDb($value)."'";
                                    break;
                                }
                                //No business hours module found - fall through.
                                $params[3] = 'hour';
                                // no break
                            default:
                                if ($sugar_config['dbconfig']['db_type'] == 'mssql') {
                                    $value = "DATEADD(".$params[3].",  ".$app_list_strings['aow_date_operator'][$params[1]]." $params[2], $value)";
                                } else {
                                    if (!isset($params)) {
                                        LoggerManager::getLogger()->warn('Undefined variable: param');
                                        $params = [null, null, null, null];
                                    }

                                    $params1 = $params[1];
                                    $params2 = $params[2];
                                    $params3 = $params[3];

                                    $dateOp = null;
                                    if (isset($app_list_strings['aow_date_operator'][$params1])) {
                                        $dateOp = $app_list_strings['aow_date_operator'][$params1];
                                    } else {
                                        LoggerManager::getLogger()->warn('Date operator is not set in app_list_string[' . $params1 . ']');
                                    }

                                    $field = 'DATE_FORMAT('.$field.", '%Y-%m-%d %H:%i')";
                                    $value = "DATE_FORMAT(DATE_ADD($value, INTERVAL ".$dateOp." $params2 ".$params3."), '%Y-%m-%d %H:%i')";
                                }
                                break;
                        }
                    }
                    break;

                case 'Multi':
                    $sep = ' AND ';
                    if ($condition->operator == 'Equal_To') {
                        $sep = ' OR ';
                    }
                    $multi_values = unencodeMultienum($condition->value);
                    if (!empty($multi_values)) {
                        $value = '(';
                        if ($data['type'] == 'multienum') {
                            $multi_operator =  $condition->operator == 'Equal_To' ? 'LIKE' : 'NOT LIKE';
                            foreach ($multi_values as $multi_value) {
                                if ($value != '(') {
                                    $value .= $sep;
                                }
                                $value .= $field." $multi_operator '%^".$multi_value."^%'";
                            }
                        } else {
                            foreach ($multi_values as $multi_value) {
                                if ($value != '(') {
                                    $value .= $sep;
                                }
                                $value .= $field.' '.$this->getSQLOperator($condition->operator)." '".$multi_value."'";
                            }
                        }
                        $value .= ')';
                        $query['where'][] = $value;
                    }
                    $where_set = true;
                    break;
                case 'SecurityGroup':
                    $sgModule = $condition_module->module_dir;
                    if (isset($data['module']) && $data['module'] !== '') {
                        $sgModule = $data['module'];
                    }
                    $sql = 'EXISTS (SELECT 1 FROM securitygroups_records WHERE record_id = ' . $field . " AND module = '" . $sgModule . "' AND securitygroup_id = '" . $condition->value . "' AND deleted=0)";
                    if ($sgModule === 'Users') {
                        $sql = 'EXISTS (SELECT 1 FROM securitygroups_users WHERE user_id = ' . $field . " AND securitygroup_id = '" . $condition->value . "' AND deleted=0)";
                    }
                    $query['where'][] = $sql;
                    $where_set = true;
                    break;
                case 'Value':
                default:
                    $value = "'".$condition->value."'";
                    break;
            }

            //handle like conditions
            switch ($condition->operator) {
                case 'Contains':
                    $value = "CONCAT('%', ".$value." ,'%')";
                    break;
                case 'Starts_With':
                    $value = "CONCAT(".$value." ,'%')";
                    break;
                case 'Ends_With':
                    $value = "CONCAT('%', ".$value.")";
                    break;
            }


            if (!$where_set) {
                $query['where'][] = $field.' '.$this->getSQLOperator($condition->operator).' '.$value;
            }
        }

        return $query;
    }

    /**
     * @param SugarBean $bean
     * @return bool
     */
    public function check_valid_bean(SugarBean $bean)
    {
        global $app_list_strings, $timedate;

        if (!$this->multiple_runs) {
            $processed = BeanFactory::getBean('AOW_Processed');
            $processed->retrieve_by_string_fields(array('aow_workflow_id' => $this->id, 'parent_id' => $bean->id));

            if ($processed->status === 'Complete') {
                //has already run so return false
                return false;
            }
        }

        if (!isset($bean->date_entered) && $bean->fetched_row !== false) {
            $bean->date_entered = $bean->fetched_row['date_entered'];
        }

        if ($this->flow_run_on) {
            $dateEntered = $timedate->fromUserType($this->date_entered, 'datetime')
                ?: $timedate->fromDbType($this->date_entered, 'datetime');
            $beanDateEntered = $timedate->fromUserType($bean->date_entered, 'datetime')
                ?: $timedate->fromDbType($bean->date_entered, 'datetime');
            $beanDateModified = $timedate->fromUserType($bean->date_modified, 'datetime')
                ?: $timedate->fromDbType($bean->date_modified, 'datetime');

            switch ($this->flow_run_on) {
                case'New_Records':
                    if (!empty($bean->fetched_row) || $beanDateEntered < $dateEntered) {
                        return false;
                    }
                    break;

                case'Modified_Records':
                    if (empty($bean->fetched_row) ||
                        ($beanDateModified < $dateEntered && $beanDateModified !== $beanDateEntered)) {
                        return false;
                    }
                    break;
            }
        }

        $sql = "SELECT id FROM aow_conditions WHERE aow_workflow_id = '".$this->id."' AND deleted = 0 ORDER BY condition_order ASC";

        $result = $this->db->query($sql);
        $query_array = array();

        while ($row = $this->db->fetchByAssoc($result)) {
            $condition = BeanFactory::newBean('AOW_Conditions');
            $condition->retrieve($row['id']);

            $path = unserialize(base64_decode($condition->module_path), ['allowed_classes' => false]);

            $condition_bean = $bean;

            if (isset($path[0]) && $path[0] != $bean->module_dir) {
                $query_array = $this->build_query_where($condition, $condition_bean, $query_array);
                continue;
            }

            $field = $condition->field;
            $value = $condition->value;

            $dateFields = array('date','datetime', 'datetimecombo');
            if ($this->isSQLOperator($condition->operator)) {
                $data = $condition_bean->field_defs[$field];

                if ($data['type'] === 'relate' && isset($data['id_name'])) {
                    $field = $data['id_name'];
                    $condition->field = $data['id_name'];
                }
                $field = $condition_bean->$field;

                if (in_array($data['type'], $dateFields)) {
                    $field = strtotime($field);
                }

                switch ($condition->value_type) {
                    case 'Field':
                        $data = $condition_bean->field_defs[$value];

                        if ($data['type'] === 'relate' && isset($data['id_name'])) {
                            $value = $data['id_name'];
                        }
                        $value = $condition_bean->$value;

                        if (in_array($data['type'], $dateFields)) {
                            $value = strtotime($value);
                        }

                        break;

                    case 'Any_Change':
                        if ($data['type'] === 'relate' && isset($data['name'])
                            && isset($condition_bean->rel_fields_before_value[$condition->field])) {
                            $value = $condition_bean->rel_fields_before_value[$condition->field];
                        } else {
                            $conditionField = $condition_bean->fetched_row[$condition->field] ?? '';
                            $value = from_html($conditionField);
                            // Bug - on delete bean action CRM load bean in a different way and bean can contain html characters
                            $field = from_html($field);
                        }
                        if (in_array($data['type'], $dateFields)) {
                            $value = strtotime($value);
                        }
                        switch ($condition->operator) {
                            case 'Not_Equal_To':
                                $condition->operator = 'Equal_To';
                                break;
                            case 'Equal_To':
                            default:
                                $condition->operator = 'Not_Equal_To';
                                break;
                        }
                        break;

                    case 'Date':
                        $params =  unserialize(base64_decode($value), ['allowed_classes' => false]);
                        $dateType = 'datetime';
                        if ($params[0] == 'now') {
                            $value = date('Y-m-d H:i:s');
                        } elseif ($params[0] == 'today') {
                            $dateType = 'date';
                            $value = date('Y-m-d');
                            $field = strtotime(date('Y-m-d', $field));
                        } else {
                            if ($params[0] == 'today') {
                                $dateType = 'date';
                                $value = date('Y-m-d');
                                $field = strtotime(date('Y-m-d', $field));
                            } else {
                                $fieldName = $params[0];
                                $value = $condition_bean->$fieldName;
                            }
                        }

                        if ($params[1] != 'now') {
                            switch ($params[3]) {
                                case 'business_hours':
                                    if (file_exists('modules/AOBH_BusinessHours/AOBH_BusinessHours.php')) {
                                        require_once('modules/AOBH_BusinessHours/AOBH_BusinessHours.php');

                                        $businessHours = BeanFactory::newBean('AOBH_BusinessHours');

                                        $amount = $params[2];
                                        if ($params[1] != "plus") {
                                            $amount = 0-$amount;
                                        }

                                        $value = $businessHours->addBusinessHours($amount, $timedate->fromDb($value));
                                        $value = strtotime($timedate->asDbType($value, $dateType));
                                        break;
                                    }
                                    //No business hours module found - fall through.
                                    $params[3] = 'hours';
                                    // no break
                                default:
                                    $value = strtotime($value.' '.$app_list_strings['aow_date_operator'][$params[1]]." $params[2] ".$params[3]);
                                    if ($dateType == 'date') {
                                        $value = strtotime(date('Y-m-d', $value));
                                    }
                                    break;
                            }
                        } else {
                            $value = strtotime($value);
                        }
                        break;

                    case 'Multi':

                        $value = unencodeMultienum($value);
                        if ($data['type'] == 'multienum') {
                            $field = unencodeMultienum($field);
                        }
                        switch ($condition->operator) {
                            case 'Not_Equal_To':
                                $condition->operator = 'Not_One_of';
                                break;
                            case 'Equal_To':
                            default:
                                $condition->operator = 'One_of';
                                break;
                        }
                        break;
                    case 'SecurityGroup':
                        if (file_exists('modules/SecurityGroups/SecurityGroup.php')) {
                            $sg_module = $condition_bean->module_dir;
                            if (isset($data['module']) && $data['module'] != '') {
                                $sg_module = $data['module'];
                            }
                            $value = $this->check_in_group($field, $sg_module, $value);
                            $field = true;
                            break;
                        }
                        // no break
                    case 'Value':
                    default:
                        if (in_array($data['type'], $dateFields) && trim($value) != '') {
                            $value = strtotime($value);
                        } elseif ($data['type'] == 'bool' && (!(bool)$value || strtolower($value) == 'false')) {
                            $value = 0;
                        } elseif($data['type'] == 'multienum') {
                            $value = unencodeMultienum($value);
                            $field = unencodeMultienum($field);
                            switch ($condition->operator) {
                                case 'Not_Equal_To':
                                    $condition->operator = 'Not_One_of';
                                    break;
                                case 'Equal_To':
                                default:
                                    $condition->operator = 'One_of';
                                    break;
                            }
                        }
                        $type = $data['dbType'] ?? $data['type'];
                        if ((strpos((string) $type, 'char') !== false || strpos((string) $type, 'text') !== false) && !empty($field)) {
                            $field = from_html($field);
                        }
                        break;
                }

                if (!($this->compare_condition($field, $value, $condition->operator))) {
                    return false;
                }
            }
        }

        if (isset($query_array['where'])) {
            $query = 'SELECT '.$bean->table_name.'.id AS id FROM '.$bean->table_name.' ';

            if (isset($query_array['join'])) {
                foreach ($query_array['join'] as $join) {
                    $query .= $join;
                }
            }
            $query_where = '';
            $query_array['where'][] = $bean->table_name.'.id = '."'".$bean->id."'";
            foreach ($query_array['where'] as $where) {
                $query_where .=  ($query_where == '' ? 'WHERE ' : ' AND ').$where;
            }
            $query .= ' '.$query_where;

            $rel_check = $bean->db->getOne($query);

            if ($rel_check == '') {
                return false;
            }
        }

        return true;
    }

    public function compare_condition($var1, $var2, $operator = 'Equal_To')
    {
        switch ($operator) {
            case "Not_Equal_To": return $var1 != $var2;
            case "Greater_Than":  return $var1 >  $var2;
            case "Less_Than":  return $var1 <  $var2;
            case "Greater_Than_or_Equal_To": return $var1 >= $var2;
            case "Less_Than_or_Equal_To": return $var1 <= $var2;
            case "Contains": return strpos(strtolower($var1), strtolower($var2)) !== false;
            case "Starts_With": return substr(strtolower($var1), 0, strlen((string) $var2) ) === strtolower($var2);
            case "Ends_With": return substr(strtolower($var1), -strlen((string) $var2) ) === strtolower($var2);
            case "is_null": return $var1 == '';
            case "One_of":
                if (is_array($var1)) {
                    foreach ($var1 as $var) {
                        if (in_array($var, $var2)) {
                            return true;
                        }
                    }
                    return false;
                }
                    return in_array($var1, $var2);

            case "Not_One_of":
                if (is_array($var1)) {
                    foreach ($var1 as $var) {
                        if (in_array($var, $var2)) {
                            return false;
                        }
                    }
                    return true;
                }
                    return !in_array($var1, $var2);

            case "Equal_To":
            default: return $var1 == $var2;
        }
    }

    public function check_in_group($bean_id, $module, $group)
    {
        $sql = "SELECT id FROM securitygroups_records WHERE record_id = '".$bean_id."' AND module = '".$module."' AND securitygroup_id = '".$group."' AND deleted=0";
        if ($module == 'Users') {
            $sql = "SELECT id FROM securitygroups_users WHERE user_id = '".$bean_id."' AND securitygroup_id = '".$group."' AND deleted=0";
        }
        $id = $this->db->getOne($sql);
        if ($id != '') {
            return true;
        }
        return false;
    }

    /**
     * Run the actions against the passed $bean
     */
    public function run_actions(SugarBean &$bean, $in_save = false)
    {
        require_once('modules/AOW_Processed/AOW_Processed.php');
        $processed = BeanFactory::newBean('AOW_Processed');
        if (!$this->multiple_runs) {
            $processed->retrieve_by_string_fields(array('aow_workflow_id' => $this->id,'parent_id' => $bean->id));

            if ($processed->status == 'Complete') {
                //should not have gotten this far, so return
                return true;
            }
        }
        $processed->aow_workflow_id = $this->id;
        $processed->parent_id = $bean->id;
        $processed->parent_type = $bean->module_dir;
        $processed->status = 'Running';
        $processed->save(false);
        $processed->load_relationship('aow_actions');

        $pass = true;

        $sql = "SELECT id FROM aow_actions WHERE aow_workflow_id = '".$this->id."' AND deleted = 0 ORDER BY action_order ASC";
        $result = $this->db->query($sql);

        while ($row = $this->db->fetchByAssoc($result)) {
            $action = BeanFactory::newBean('AOW_Actions');
            $action->retrieve($row['id']);

            if ($this->multiple_runs || !$processed->db->getOne("select id from aow_processed_aow_actions where aow_processed_id = '".$processed->id."' AND aow_action_id = '".$action->id."' AND status = 'Complete'")) {
                $action_name = 'action'.$action->action;

                if (file_exists('custom/modules/AOW_Actions/actions/'.$action_name.'.php')) {
                    require_once('custom/modules/AOW_Actions/actions/'.$action_name.'.php');
                } elseif (file_exists('modules/AOW_Actions/actions/'.$action_name.'.php')) {
                    require_once('modules/AOW_Actions/actions/'.$action_name.'.php');
                } else {
                    if (file_exists('modules/AOW_Actions/actions/'.$action_name.'.php')) {
                        require_once('modules/AOW_Actions/actions/'.$action_name.'.php');
                    } else {
                        return false;
                    }
                }

                $custom_action_name = "custom" . $action_name;
                if (class_exists($custom_action_name)) {
                    $action_name = $custom_action_name;
                }


                $flow_action = new $action_name($action->id);
                if (!$flow_action->run_action($bean, unserialize(base64_decode($action->parameters), ['allowed_classes' => false]), $in_save)) {
                    $pass = false;
                    $processed->aow_actions->add($action->id, array('status' => 'Failed'));
                } else {
                    $processed->aow_actions->add($action->id, array('status' => 'Complete'));
                }
            }
        }

        if ($pass) {
            $processed->status = 'Complete';
        } else {
            $processed->status = 'Failed';
        }
        $processed->save(false);

        return $pass;
    }
}
