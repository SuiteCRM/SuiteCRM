<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2021 SalesAgility Ltd.
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

#[\AllowDynamicProperties]
class ProjectTask extends SugarBean
{
    // database table columns
    public $id;
    public $date_entered;
    public $date_modified;
    public $name;
    public $description;
    public $project_id;
    public $project_task_id;
    public $date_start;
    public $date_finish;
    public $duration;
    public $duration_unit;
    public $percent_complete;
    public $parent_task_id;
    public $predecessors;
    public $priority;
    public $importable = true;

    // related information
    public $assigned_user_name;
    public $parent_name;
    public $depends_on_name;
    public $email_id;

    public $table_name = 'project_task';
    public $object_name = 'ProjectTask';
    public $module_dir = 'ProjectTask';

    public $field_name_map;
    public $new_schema = true;

    public $relationship_fields = array(
        'email_id' => 'emails',
    );
    /**
     * @var bool skip updating parent percent complete
     */
    protected $_skipParentUpdate = false;


    /**
     * ProjectTask constructor.
     * @param bool $init
     */
    public function __construct($init = true)
    {
        parent::__construct();
        if ($init) {
            // default value for a clean instantiation
            $this->utilization = 100;

            global $current_user;
            if (empty($current_user)) {
                $this->assigned_user_id = 1;
                $admin_user = BeanFactory::newBean('Users');
                $admin_user->retrieve($this->assigned_user_id);
                $this->assigned_user_name = $admin_user->user_name;
            } else {
                $this->assigned_user_id = $current_user->id;
                $this->assigned_user_name = $current_user->user_name;
            }
        }
    }

    /**
     * @param bool $skip updating parent percent complete
     */
    public function skipParentUpdate($skip = true)
    {
        $this->_skipParentUpdate = $skip;
    }

    public function save($check_notify = false)
    {
        //Bug 46012.  When saving new Project Tasks instance in a workflow, make sure we set a project_task_id value
        //associated with the Project if there is no project_task_id specified.
        if ($this->in_workflow && empty($this->id) && empty($this->project_task_id) && !empty($this->project_id)) {
            $this->project_task_id = $this->getNumberOfTasksInProject($this->project_id) + 1;
        }

        $id = parent::save($check_notify);
        if ($this->_skipParentUpdate == false) {
            $this->updateStatistic();
        }

        return $id;
    }

    /*
     *
     */
    public function get_summary_text()
    {
        return $this->name;
    }

    /*
     *
     */
    public function _get_depends_on_name($depends_on_id)
    {
        $return_value = '';

        $query = "SELECT name, assigned_user_id FROM {$this->table_name} WHERE id='{$depends_on_id}'";
        $result = $this->db->query($query, true, " Error filling in additional detail fields: ");
        $row = $this->db->fetchByAssoc($result);
        if ($row != null) {
            $this->depends_on_name_owner = $row['assigned_user_id'];
            $this->depends_on_name_mod = 'ProjectTask';
            $return_value = $row['name'];
        }

        return $return_value;
    }

    public function _get_project_name($project_id)
    {
        $return_value = '';

        $query = "SELECT name, assigned_user_id FROM project WHERE id='{$project_id}'";
        $result = $this->db->query($query, true, " Error filling in additional detail fields: ");
        $row = $this->db->fetchByAssoc($result);
        if ($row != null) {
            //$this->parent_name_owner = $row['assigned_user_id'];
            //$this->parent_name_mod = 'Project';
            $return_value = $row['name'];
        }

        return $return_value;
    }

    /*
     *
     */
    public function _get_parent_name($parent_id)
    {
        $return_value = '';

        $query = "SELECT name, assigned_user_id FROM project WHERE id='{$parent_id}'";
        $result = $this->db->query($query, true, " Error filling in additional detail fields: ");
        $row = $this->db->fetchByAssoc($result);
        if ($row != null) {
            $this->parent_name_owner = $row['assigned_user_id'];
            $this->parent_name_mod = 'Project';
            $return_value = $row['name'];
        }

        return $return_value;
    }

    /*
     *
     */
    public function build_generic_where_clause($the_query_string)
    {
        $where_clauses = array();
        $the_query_string = DBManagerFactory::getInstance()->quote($the_query_string);
        $where_clauses[] = "project_task.name like '$the_query_string%'";

        $the_where = "";
        foreach ($where_clauses as $clause) {
            if ($the_where != "") {
                $the_where .= " or ";
            }
            $the_where .= $clause;
        }

        return $the_where;
    }

    public function bean_implements($interface)
    {
        switch ($interface) {
            case 'ACL':
                return true;
        }

        return false;
    }

    public function listviewACLHelper()
    {
        $array_assign = parent::listviewACLHelper();
        $is_owner = false;
        $in_group = false; //SECURITY GROUPS
        if (!empty($this->parent_name)) {
            if (!empty($this->parent_name_owner)) {
                global $current_user;
                $is_owner = $current_user->id == $this->parent_name_owner;
            }
            /* BEGIN - SECURITY GROUPS */
            //parent_name_owner not being set for whatever reason so we need to figure this out
            else {
                if (!empty($this->parent_type) && !empty($this->parent_id)) {
                    global $current_user;
                    $parent_bean = BeanFactory::getBean($this->parent_type, $this->parent_id);
                    if ($parent_bean !== false) {
                        $is_owner = $current_user->id == $parent_bean->assigned_user_id;
                    }
                }
            }
            require_once("modules/SecurityGroups/SecurityGroup.php");
            $in_group = SecurityGroup::groupHasAccess($this->parent_type, $this->parent_id, 'view');
            /* END - SECURITY GROUPS */
        }
        /* BEGIN - SECURITY GROUPS */
        /**
         * if(ACLController::checkAccess('Project', 'view', $is_owner)){
         */
        if (ACLController::checkAccess('Project', 'view', $is_owner, 'module', $in_group)) {
            /* END - SECURITY GROUPS */
            $array_assign['PARENT'] = 'a';
        } else {
            $array_assign['PARENT'] = 'span';
        }
        $is_owner = false;
        if (!empty($this->depends_on_name)) {
            if (!empty($this->depends_on_name_owner)) {
                global $current_user;
                $is_owner = $current_user->id == $this->depends_on_name_owner;
            }
        }
        if (ACLController::checkAccess('ProjectTask', 'view', $is_owner)) {
            $array_assign['PARENT_TASK'] = 'a';
        } else {
            $array_assign['PARENT_TASK'] = 'span';
        }

        return $array_assign;
    }

    public function create_export_query($order_by, $where, $relate_link_join = '')
    {
        $custom_join = $this->getCustomJoin(true, true, $where);
        $custom_join['join'] .= $relate_link_join;
        $query = "SELECT
				project_task.*,
                users.user_name as assigned_user_name ";
        $query .= $custom_join['select'];

        $query .= " FROM project_task LEFT JOIN project ON project_task.project_id=project.id AND project.deleted=0 ";

        $query .= $custom_join['join'];
        $query .= " LEFT JOIN users
                   	ON project_task.assigned_user_id=users.id ";

        $where_auto = " project_task.deleted=0 ";

        if ($where != "") {
            $query .= "where ($where) AND " . $where_auto;
        } else {
            $query .= "where " . $where_auto;
        }

        if (!empty($order_by)) {
            //check to see if order by variable already has table name by looking for dot "."
            $table_defined_already = strpos((string) $order_by, ".");

            if ($table_defined_already === false) {
                //table not defined yet, define accounts to avoid "ambigous column" SQL error
                $query .= " ORDER BY $order_by";
            } else {
                //table already defined, just add it to end of query
                $query .= " ORDER BY $order_by";
            }
        }

        return $query;
    }


    /**
     * This method recalculates the percent complete of a parent task
     */
    public function updateParentProjectTaskPercentage()
    {
        if (empty($this->parent_task_id)) {
            return;
        }

        if (!empty($this->project_id)) {
            //determine parent task
            $parentProjectTask = $this->getProjectTaskParent();

            //get task children
            if ($parentProjectTask) {
                $subProjectTasks = $parentProjectTask->getAllSubProjectTasks();
                $tasks = array();
                foreach ($subProjectTasks as &$task) {
                    $tasks[] = $task->toArray(true);
                }
                $parentProjectTask->percent_complete = $this->_calculateCompletePercent($tasks);
                unset($tasks);
                $parentProjectTask->save(isset($GLOBALS['check_notify']) ? $GLOBALS['check_notify'] : '');
            }
        }
    }

    /**
     * Calculate percent complete for parent task based on it's children tasks
     * @param $subProjectTasks mixed Array of children tasks
     * @return int percent complete
     */
    private function _calculateCompletePercent(&$subProjectTasks)
    {
        $totalHours = 0;
        $cumulativeDone = 0;
        //update cumulative calculation - mimics gantt calculation
        foreach ($subProjectTasks as $key => &$value) {
            if ($value['duration'] == "") {
                $value['duration'] = 0;
            }

            if ($value['percent_complete'] == "") {
                $value['percent_complete'] = 0;
            }

            if ($value['duration_unit'] == "Hours") {
                $totalHours += $value['duration'];
                $cumulativeDone += $value['duration'] * ($value['percent_complete'] / 100);
            } else {
                $totalHours += ($value['duration'] * 8);
                $cumulativeDone += ($value['duration'] * 8) * ($value['percent_complete'] / 100);
            }
        }

        $cumulativePercentage = 0;
        if ($totalHours != 0) {
            $cumulativePercentage = round(($cumulativeDone / $totalHours) * 100);
        }

        return $cumulativePercentage;
    }

    /**
     * Retrieves the parent project task of a project task
     * returns project task bean
     */
    public function getProjectTaskParent()
    {
        $projectTaskParent = false;

        if (!empty($this->parent_task_id) && !empty($this->project_id)) {
            $query = "SELECT id FROM project_task WHERE project_id = '{$this->project_id}' AND project_task_id = '{$this->parent_task_id}' AND deleted = 0 ORDER BY date_modified DESC";
            $project_task_id = $this->db->getOne($query, true, "Error retrieving parent project task");

            if (!empty($project_task_id)) {
                $projectTaskParent = BeanFactory::getBean('ProjectTask', $project_task_id);
            }
        }

        return $projectTaskParent;
    }

    /**
     * Retrieves all the child project tasks of a project task
     * returns project task bean array
     */
    public function getAllSubProjectTasks()
    {
        $potentialParentTaskIds = [];
        $projectTasksBeans = array();

        if (!empty($this->project_task_id) && !empty($this->project_id)) {
            //select all tasks from a project
            $query = "SELECT id, project_task_id, parent_task_id FROM project_task WHERE project_id = '{$this->project_id}' AND deleted = 0 ORDER BY project_task_id";

            $result = $this->db->query($query, true, "Error retrieving child project tasks");

            $projectTasks = array();
            while ($row = $this->db->fetchByAssoc($result)) {
                $projectTasks[$row['id']]['project_task_id'] = $row['project_task_id'];
                $projectTasks[$row['id']]['parent_task_id'] = $row['parent_task_id'];
            }

            $potentialParentTaskIds[$this->project_task_id] = $this->project_task_id;
            $actualParentTaskIds = array();
            $subProjectTasks = array();

            $startProjectTasksCount = 0;
            $endProjectTasksCount = 0;

            //get all child tasks
            $run = true;
            while ($run) {
                $count = 0;

                foreach ($projectTasks as $id => $values) {
                    if (in_array($values['parent_task_id'], $potentialParentTaskIds)) {
                        $potentialParentTaskIds[$values['project_task_id']] = $values['project_task_id'];
                        $actualParentTaskIds[$values['parent_task_id']] = $values['parent_task_id'];

                        $subProjectTasks[$id] = $values;
                        $count = $count + 1;
                    }
                }

                $endProjectTasksCount = count($subProjectTasks);

                if ($startProjectTasksCount === $endProjectTasksCount) {
                    $run = false;
                } else {
                    $startProjectTasksCount = $endProjectTasksCount;
                }
            }

            foreach ($subProjectTasks as $id => $values) {
                //ignore tasks that are parents
                if (!in_array($values['project_task_id'], $actualParentTaskIds)) {
                    $projectTaskBean = BeanFactory::getBean('ProjectTask', $id);
                    $projectTasksBeans[] = $projectTaskBean;
                }
            }
        }

        return $projectTasksBeans;
    }


    /**
     * getNumberOfTasksInProject
     *
     * Returns the count of project_tasks for the given project_id
     *
     * This is a private helper function to get the number of project tasks for a given project_id.
     *
     * @param $project_id integer value of the project_id associated with this ProjectTask instance
     * @return int total integer value of the count of project tasks, 0 if none found
     */
    private function getNumberOfTasksInProject($project_id = '')
    {
        if (!empty($project_id)) {
            $query = "SELECT count(project_task_id) AS total FROM project_task WHERE project_id = '{$project_id}'";
            $result = $this->db->query($query, true);
            if ($result) {
                $row = $this->db->fetchByAssoc($result);
                if (!empty($row['total'])) {
                    return $row['total'];
                }
            }
        }

        return 0;
    }

    /**
     * Update percent complete for project tasks with children tasks based on children's values
     */
    public function updateStatistic()
    {
        /**
         * @var array Array of tasks for current project
         */
        $list = array();
        /**
         * @var array Key-value array of project_task_id => parent_task_id
         */
        $tree = array();
        /**
         * @var array Array with nodes which have childrens
         */
        $nodes = array();
        /**
         * @var array Array with IDs of list which have been changed
         */
        $changed = array();

        $db = DBManagerFactory::getInstance();
        $this->disable_row_level_security = true;
        $query = $this->create_new_list_query('', "project_id = {$db->quoted($this->project_id)}");
        $this->disable_row_level_security = false;
        $res = $db->query($query);
        while ($row = $db->fetchByAssoc($res)) {
            $list[] = $row;
        }
        // fill in $tree
        foreach ($list as $k => &$v) {
            if (isset($v['project_task_id']) && $v['project_task_id'] != '') {
                $tree[$v['project_task_id']] = $v['parent_task_id'];
                if (isset($v['parent_task_id']) && $v['parent_task_id']) {
                    if (!isset($nodes[$v['parent_task_id']])) {
                        $nodes[$v['parent_task_id']] = 1;
                    }
                }
            }
        }
        unset($v);
        // fill in $nodes array
        foreach ($nodes as $k => &$v) {
            $run = true;
            $i = $k;
            while ($run) {
                if (isset($tree[$i]) && $tree[$i] != '') {
                    $i = $tree[$i];
                    $v++;
                } else {
                    $run = false;
                }
            }
        }
        arsort($nodes);
        unset($v);
        // calculating of percentages and comparing calculated value with database one
        foreach ($nodes as $k => &$v) {
            $currRow = null;
            $currChildren = array();
            $run = true;
            $tmp = array();
            $i = $k;
            while ($run) {
                foreach ($list as $id => &$taskRow) {
                    if ($taskRow['project_task_id'] == $i && $currRow === null) {
                        $currRow = $id;
                    }
                    if ($taskRow['parent_task_id'] == $i) {
                        if (!array_key_exists($taskRow['project_task_id'], $nodes)) {
                            $currChildren[] = $taskRow;
                        } else {
                            $tmp[] = $taskRow['project_task_id'];
                        }
                    }
                }
                unset($taskRow);
                if (count($tmp) == 0) {
                    $run = false;
                } else {
                    $i = array_shift($tmp);
                }
            }
            $subres = $this->_calculateCompletePercent($currChildren);
            if ($subres != $list[$currRow]['percent_complete']) {
                $list[$currRow]['percent_complete'] = $subres;
                $changed[] = $currRow;
            }
        }
        unset($v);
        // updating data in database for changed tasks
        foreach ($changed as $k => &$v) {
            $task = BeanFactory::getBean('ProjectTask');
            $task->populateFromRow($list[$v]);
            $task->skipParentUpdate();
            $task->save(false);
        }
    }
}

function getUtilizationDropdown($focus, $field, $value, $view)
{
    if ($view === 'EditView') {
        global $app_list_strings;
        $html = '<select name="' . $field . '">';
        $html .= get_select_options_with_id($app_list_strings['project_task_utilization_options'], $value);
        $html .= '</select>';

        return $html;
    }

    return translate('project_task_utilization_options', '', $focus->$field);
}
