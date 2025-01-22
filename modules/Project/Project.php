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


#[\AllowDynamicProperties]
class Project extends SugarBean
{
    // database table columns
    public $id;
    public $date_entered;
    public $date_modified;
    public $assigned_user_id;
    public $modified_user_id;
    public $created_by;
    public $name;
    public $description;
    public $deleted;


    // related information
    public $assigned_user_name;
    public $modified_by_name;
    public $created_by_name;

    public $account_id;
    public $contact_id;
    public $opportunity_id;
    public $email_id;
    public $estimated_start_date;
    public $team_id;

    // calculated information
    public $total_estimated_effort;
    public $total_actual_effort;

    public $object_name = 'Project';
    public $module_dir = 'Project';
    public $new_schema = true;
    public $table_name = 'project';

    public $importable = true;

    // This is used to retrieve related fields from form posts.
    public $additional_column_fields = array(
        'account_id',
        'contact_id',
        'opportunity_id',
    );

    public $relationship_fields = array(
        'account_id' => 'accounts',
        'contact_id'=>'contacts',
        'opportunity_id'=>'opportunities',
        'email_id' => 'emails',
    );

    //////////////////////////////////////////////////////////////////
    // METHODS
    //////////////////////////////////////////////////////////////////

    /**
     *
     */
    public function __construct()
    {
        parent::__construct();
    }




    /**
     * overriding the base class function to do a join with users table
     */

    /**
     *
     */
    public function fill_in_additional_detail_fields()
    {
        parent::fill_in_additional_detail_fields();

        $this->assigned_user_name = get_assigned_user_name($this->assigned_user_id);
        //$this->total_estimated_effort = $this->_get_total_estimated_effort($this->id);
        //$this->total_actual_effort = $this->_get_total_actual_effort($this->id);
    }

    /**
     *
     */
    public function fill_in_additional_list_fields()
    {
        parent::fill_in_additional_list_fields();
        $this->assigned_user_name = get_assigned_user_name($this->assigned_user_id);
        //$this->total_estimated_effort = $this->_get_total_estimated_effort($this->id);
        //$this->total_actual_effort = $this->_get_total_actual_effort($this->id);
    }

    /**
    * Save changes that have been made to a relationship.
    *
    * @param $is_update true if this save is an update.
    */
    public function save_relationship_changes($is_update, $exclude=array())
    {
        parent::save_relationship_changes($is_update, $exclude);
        $new_rel_id = false;
        $new_rel_link = false;
        //this allows us to dynamically relate modules without adding it to the relationship_fields array
        if (!empty($_REQUEST['relate_id']) && !in_array($_REQUEST['relate_to'], $exclude) && $_REQUEST['relate_id'] != $this->id) {
            $new_rel_id = $_REQUEST['relate_id'];
            $new_rel_relname = $_REQUEST['relate_to'];
            if (!empty($this->in_workflow) && !empty($this->not_use_rel_in_req)) {
                $new_rel_id = $this->new_rel_id;
                $new_rel_relname = $this->new_rel_relname;
            }
            $new_rel_link = $new_rel_relname;
            //Try to find the link in this bean based on the relationship
            foreach ($this->field_defs as $key => $def) {
                if (isset($def['type']) && $def['type'] == 'link'
                && isset($def['relationship']) && $def['relationship'] == $new_rel_relname) {
                    $new_rel_link = $key;
                }
            }
            if ($new_rel_link == 'contacts') {
                $accountId = $this->db->getOne('SELECT account_id FROM accounts_contacts WHERE contact_id=' . $this->db->quoted($new_rel_id));
                if ($accountId !== false) {
                    if ($this->load_relationship('accounts')) {
                        $this->accounts->add($accountId);
                    }
                }
            }
        }
    }
    /**
     *
     */
    public function _get_total_estimated_effort($project_id)
    {
        $return_value = '';

        $query = 'SELECT SUM('.$this->db->convert('estimated_effort', "IFNULL", 0).') total_estimated_effort';
        $query.= ' FROM project_task';
        $query.= " WHERE parent_id='{$project_id}' AND deleted=0";

        $result = $this->db->query($query, true, " Error filling in additional detail fields: ");
        $row = $this->db->fetchByAssoc($result);
        if ($row != null) {
            $return_value = $row['total_estimated_effort'];
        }

        return $return_value;
    }

    /**
     *
     */
    public function _get_total_actual_effort($project_id)
    {
        $return_value = '';

        $query = 'SELECT SUM('.$this->db->convert('actual_effort', "IFNULL", 0).') total_actual_effort';
        $query.=  ' FROM project_task';
        $query.=  " WHERE parent_id='{$project_id}' AND deleted=0";

        $result = $this->db->query($query, true, " Error filling in additional detail fields: ");
        $row = $this->db->fetchByAssoc($result);
        if ($row != null) {
            $return_value = $row['total_actual_effort'];
        }

        return $return_value;
    }

    /**
     *
     */
    public function get_summary_text()
    {
        return $this->name;
    }

    /**
     *
     */
    public function build_generic_where_clause($the_query_string)
    {
        $where_clauses = array();
        $the_query_string = DBManagerFactory::getInstance()->quote($the_query_string);
        array_push($where_clauses, "project.name LIKE '%$the_query_string%'");

        $the_where = '';
        foreach ($where_clauses as $clause) {
            if ($the_where != '') {
                $the_where .= " OR ";
            }
            $the_where .= $clause;
        }

        return $the_where;
    }

    public function get_list_view_data()
    {
        $field_list = $this->get_list_view_array();
        $field_list['USER_NAME'] = empty($this->user_name) ? '' : $this->user_name;
        $field_list['ASSIGNED_USER_NAME'] = $this->assigned_user_name;
        return $field_list;
    }
    public function bean_implements($interface)
    {
        switch ($interface) {
            case 'ACL':return true;
        }
        return false;
    }

    public function create_export_query($order_by, $where, $relate_link_join='')
    {
        $custom_join = $this->getCustomJoin(true, true, $where);
        $custom_join['join'] .= $relate_link_join;
        $query = "SELECT
				project.*,
                users.user_name as assigned_user_name ";
        $query .=  $custom_join['select'];
        $query .= " FROM project ";

        $query .=  $custom_join['join'];
        $query .= " LEFT JOIN users
                   	ON project.assigned_user_id=users.id ";

        $where_auto = " project.deleted=0 ";

        if ($where != "") {
            $query .= "where ($where) AND ".$where_auto;
        } else {
            $query .= "where ".$where_auto;
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
    public function getAllProjectTasks()
    {
        $projectTasks = array();

        $query = "SELECT * FROM project_task WHERE project_id = '" . $this->id. "' AND deleted = 0 ORDER BY project_task_id";
        $result = $this->db->query($query, true, "Error retrieving project tasks");
        $row = $this->db->fetchByAssoc($result);

        while ($row != null) {
            $projectTaskBean = BeanFactory::newBean('ProjectTask');
            $projectTaskBean->id = $row['id'];
            $projectTaskBean->retrieve();
            array_push($projectTasks, $projectTaskBean);

            $row = $this->db->fetchByAssoc($result);
        }

        return $projectTasks;
    }

    public function getDefaultStatus()
    {
        $def = $this->field_defs['status'];
        if (isset($def['default'])) {
            return $def['default'];
        }
        $app = return_app_list_strings_language($GLOBALS['current_language']);
        if (isset($def['options']) && isset($app[$def['options']])) {
            $keys = array_keys($app[$def['options']]);
            return $keys[0];
        }

        return '';
    }

    public function save($check_notify = false)
    {
        global $current_user;
        $db = DBManagerFactory::getInstance();
        $focus = $this;
        $enddate_array = [];

        //--- check if project template is same or changed.
        $new_template_id = property_exists($focus, 'am_projecttemplates_project_1am_projecttemplates_ida') ?
            $focus->am_projecttemplates_project_1am_projecttemplates_ida : null;
        $current_template_id = "";

        $focus->load_relationship('am_projecttemplates_project_1');
        $project_template = $focus->get_linked_beans('am_projecttemplates_project_1', 'AM_ProjectTemplates');
        foreach ($project_template as $ptemplate) {
            $current_template_id = $ptemplate->id;
        }
        //----------------------------------------------------------------



        //if(!empty($this->id))
        //	$focus->retrieve($this->id);

        if ((isset($_POST['isSaveFromDetailView']) && $_POST['isSaveFromDetailView'] == 'true') ||
            (isset($_POST['is_ajax_call']) && !empty($_POST['is_ajax_call']) && !empty($focus->id) ||
            (isset($_POST['return_action']) && $_POST['return_action'] == 'SubPanelViewer') && !empty($focus->id))||
             !isset($_POST['user_invitees']) // we need to check that user_invitees exists before processing, it is ok to be empty
        ) {
            parent::save($check_notify) ; //$focus->save(true);
            $return_id = $focus->id;
        } else {
            if (!empty($_POST['user_invitees'])) {
                $userInvitees = explode(',', trim($_POST['user_invitees'], ','));
            } else {
                $userInvitees = array();
            }


            if (!empty($_POST['contact_invitees'])) {
                $contactInvitees = explode(',', trim($_POST['contact_invitees'], ','));
            } else {
                $contactInvitees = array();
            }


            $deleteUsers = array();
            $existingUsers = array();

            $deleteContacts = array();
            $existingContacts = array();

            if (!empty($this->id)) {

                //$focus->retrieve($this->id);

                ////	REMOVE RESOURCE RELATIONSHIPS
                // Calculate which users to flag as deleted and which to add

                // Get all users for the project
                $focus->load_relationship('users');
                $users = $focus->get_linked_beans('project_users_1', 'User');
                foreach ($users as $a) {
                    if (!in_array($a->id, $userInvitees)) {
                        $deleteUsers[$a->id] = $a->id;
                    } else {
                        $existingUsers[$a->id] = $a->id;
                    }
                }

                if (count($deleteUsers) > 0) {
                    $sql = '';
                    foreach ($deleteUsers as $u) {
                        $sql .= ",'" . $u . "'";
                    }
                    $sql = substr($sql, 1);
                    // We could run a delete SQL statement here, but will just mark as deleted instead
                    $sql = "UPDATE project_users_1_c set deleted = 1 where project_users_1users_idb in ($sql) AND project_users_1project_ida = '". $focus->id . "'";
                    $focus->db->query($sql);
                    echo $sql;
                }

                // Get all contacts for the project
                $focus->load_relationship('contacts');
                $contacts = $focus->get_linked_beans('project_contacts_1', 'Contact');
                foreach ($contacts as $a) {
                    if (!in_array($a->id, $contactInvitees)) {
                        $deleteContacts[$a->id] = $a->id;
                    } else {
                        $existingContacts[$a->id] = $a->id;
                    }
                }

                if (count($deleteContacts) > 0) {
                    $sql = '';
                    foreach ($deleteContacts as $u) {
                        $sql .= ",'" . $u . "'";
                    }
                    $sql = substr($sql, 1);
                    // We could run a delete SQL statement here, but will just mark as deleted instead
                    $sql = "UPDATE project_contacts_1_c set deleted = 1 where project_contacts_1contacts_idb in ($sql) AND project_contacts_1project_ida = '". $focus->id . "'";
                    $focus->db->query($sql);
                    echo $sql;
                }

                ////END REMOVE
            }

            $return_id = parent::save($check_notify);
            $focus->retrieve($return_id);

            ////REBUILD INVITEE RELATIONSHIPS

            // Process users
            $focus->load_relationship('users');
            $focus->get_linked_beans('project_users_1', 'User');
            foreach ($userInvitees as $user_id) {
                if (empty($user_id) || isset($existingUsers[$user_id]) || isset($deleteUsers[$user_id])) {
                    continue;
                }
                $focus->project_users_1->add($user_id);
            }

            // Process contacts
            $focus->load_relationship('contacts');
            $focus->get_linked_beans('project_contacts_1', 'Contact');
            foreach ($contactInvitees as $contact_id) {
                if (empty($contact_id) || isset($existingContacts[$contact_id]) || isset($deleteContacts[$contact_id])) {
                    continue;
                }
                $focus->project_contacts_1->add($contact_id);
            }

            ////	END REBUILD INVITEE RELATIONSHIPS
            ///////////////////////////////////////////////////////////////////////////
        }



        ///////////////////////////////
        // Code Block to handle the template selection at project edit.
        ////////////////////////////////////////

        if ($current_template_id != $new_template_id) {
            $project_start = $focus->estimated_start_date;
            //Get project start date
            if ($project_start!='') {
                $dateformat = $current_user->getPreference('datef');
                $startdate = DateTime::createFromFormat($dateformat, $project_start);
                if ($startdate == false) {
                    $startdate = DateTime::createFromFormat('Y-m-d', $project_start);
                }

                $start = $startdate->format('Y-m-d');
            }

            $duration_unit = 'Days';

            //Get the project template
            $template = BeanFactory::newBean('AM_ProjectTemplates');
            $template->retrieve($new_template_id);

            $override_business_hours = (int)$template->override_business_hours;


            //------ build business hours array

            $dateformat = $current_user->getPreference('datef');

            $days = array("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday");
            $businessHours = BeanFactory::getBean("AOBH_BusinessHours");
            $bhours = array();
            foreach ($days as $day) {
                $bh = $businessHours->getBusinessHoursForDay($day);

                if ($bh) {
                    $bh = $bh[0];
                    if ($bh->open) {
                        $open_h = $bh ? $bh->opening_hours : 9;
                        $close_h = $bh ? $bh->closing_hours : 17;

                        $start_time = DateTime::createFromFormat('Y-m-d', $start);

                        $start_time = $start_time->modify('+'.$open_h.' Hours');

                        $end_time = DateTime::createFromFormat('Y-m-d', $start);
                        $end_time = $end_time->modify('+'.$close_h.' Hours');

                        $hours = ($end_time->getTimestamp() - $start_time->getTimestamp())/(60*60);
                        if ($hours < 0) {
                            $hours = 0 - $hours ;
                        }

                        $bhours[$day] = $hours;
                    } else {
                        $bhours[$day] = 0;
                    }
                }
            }
            //-----------------------------------


            //default business hours array
            if ($override_business_hours != 1 || empty($bhours)) {
                $bhours = array('Monday' => 8,'Tuesday' => 8,'Wednesday' => 8, 'Thursday' => 8, 'Friday' => 8, 'Saturday' => 0, 'Sunday' => 0);
            }
            //---------------------------

            //copy all resources from template to project
            $template->load_relationship('am_projecttemplates_users_1');
            $template_users = $template->get_linked_beans('am_projecttemplates_users_1', 'User');

            $template->load_relationship('am_projecttemplates_contacts_1');
            $template_contacts = $template->get_linked_beans('am_projecttemplates_contacts_1', 'Contact');


            foreach ($template_users as $user) {
                $focus->project_users_1->add($user->id);
            }

            foreach ($template_contacts as $contact) {
                $focus->project_contacts_1->add($contact->id);
            }


            //Get related project template tasks. Using sql query so that the results can be ordered.
            $get_tasks_sql = "SELECT * FROM am_tasktemplates
							WHERE id
							IN (
								SELECT am_tasktemplates_am_projecttemplatesam_tasktemplates_idb
								FROM am_tasktemplates_am_projecttemplates_c
								WHERE am_tasktemplates_am_projecttemplatesam_projecttemplates_ida = '".$new_template_id."'
								AND deleted =0
							)
							AND deleted =0
							ORDER BY am_tasktemplates.order_number ASC";
            $tasks = $db->query($get_tasks_sql);

            //Create new project tasks from the template tasks
            $count=1;
            while ($row = $db->fetchByAssoc($tasks)) {
                $project_task = BeanFactory::newBean('ProjectTask');
                $project_task->name = $row['name'];
                $project_task->status = $row['status'];
                $project_task->priority = strtolower($row['priority']);
                $project_task->percent_complete = $row['percent_complete'];
                $project_task->predecessors = $row['predecessors'];
                $project_task->milestone_flag = $row['milestone_flag'];
                $project_task->relationship_type = $row['relationship_type'];
                $project_task->task_number = $row['task_number'];
                $project_task->order_number = $row['order_number'];
                $project_task->estimated_effort = $row['estimated_effort'];
                $project_task->utilization = $row['utilization'];
                $project_task->assigned_user_id = $row['assigned_user_id'];
                $project_task->description = $row['description'];
                $project_task->duration = $row['duration'];
                $project_task->duration_unit = $duration_unit;
                $project_task->project_task_id = $count;

                //Flag to prevent after save logichook running when project_tasks are created (see custom/modules/ProjectTask/updateProject.php)
                $project_task->set_project_end_date = 0;

                //
                //code block to calculate end date based on user's business hours
                //

                $duration = $project_task->duration;
                $enddate = $startdate;

                $d = 0;

                while ($duration > $d) {
                    $day = $enddate->format('l');

                    if ($bhours[$day] != 0) {
                        $d += 1;
                    }

                    $enddate = $enddate->modify('+1 Days');
                }
                $enddate = $enddate->modify('-1 Days');//readjust it back to remove 1 additional day added


                //----------------------------------


                if ($count == '1') {
                    $project_task->date_start = $start;
                    $end = $enddate->format('Y-m-d');
                    $project_task->date_finish = $end;

                    //add one day to let the next task start on next day of it's finish.
                    $enddate_array[$count] = $enddate->modify('+1 Days')->format('Y-m-d');
                } else {
                    $start_date = $count - 1;
                    $startdate = DateTime::createFromFormat('Y-m-d', $enddate_array[$start_date]);
                    $start = $startdate->format('Y-m-d');
                    $project_task->date_start = $start;
                    $end = $enddate->format('Y-m-d');
                    $project_task->date_finish = $end;

                    $startdate = $enddate;
                    //add one day to let the next task start on next day of it's finish.
                    $enddate_array[$count] = $enddate->modify('+1 Days')->format('Y-m-d');

                    $enddate = $end;
                }

                $project_task->save();

                //link tasks to the newly created project
                $project_task->load_relationship('projects');
                $project_task->projects->add($focus->id);

                //Add assinged users from each task to the project resourses subpanel
                $focus->load_relationship('project_users_1');
                $focus->project_users_1->add($row['assigned_user_id']);
                $count++;
            }
        }
        /// End Template Selection handling
        ////////////////////////////////////////////////////////////
        return $return_id;
    }
}
