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






require_once('include/formbase.php');

global $current_user;

$sugarbean = BeanFactory::newBean('Project');
$sugarbean = populateFromPost('', $sugarbean);

$projectTasks = array();
if (isset($_REQUEST['duplicateSave']) && $_REQUEST['duplicateSave'] === "true") {
    $base_project_id = $_REQUEST['duplicateId'];
} else {
    $base_project_id = $sugarbean->id;
}
if (isset($_REQUEST['save_type']) || isset($_REQUEST['duplicateSave']) && $_REQUEST['duplicateSave'] === "true") {
    $query = "SELECT id FROM project_task WHERE project_id = '" . $base_project_id . "' AND deleted = 0";
    $result = $sugarbean->db->query($query, true, "Error retrieving project tasks");
    $row = $sugarbean->db->fetchByAssoc($result);

    while ($row != null) {
        $projectTaskBean = BeanFactory::newBean('ProjectTask');
        $projectTaskBean->id = $row['id'];
        $projectTaskBean->retrieve();
        $projectTaskBean->date_entered = '';
        $projectTaskBean->date_modified = '';
        array_push($projectTasks, $projectTaskBean);
        $row = $sugarbean->db->fetchByAssoc($result);
    }
}
if (isset($_REQUEST['save_type'])) {
    $sugarbean->id = '';
    $sugarbean->assigned_user_id = $current_user->id;

    if ($_REQUEST['save_type'] == 'TemplateToProject') {
        $sugarbean->name = $_REQUEST['project_name'];
        $sugarbean->is_template = 0;
    } else {
        if ($_REQUEST['save_type'] == 'ProjectToTemplate') {
            $sugarbean->name = $_REQUEST['template_name'];
            $sugarbean->is_template = true;
        }
    }
} else {
    if (isset($_REQUEST['is_template']) && $_REQUEST['is_template'] == '1') {
        $sugarbean->is_template = true;
    } else {
        $sugarbean->is_template = 0;
    }
}

if (isset($_REQUEST['email_id'])) {
    $sugarbean->email_id = $_REQUEST['email_id'];
}

if (!$sugarbean->ACLAccess('Save')) {
    ACLController::displayNoAccess(true);
    sugar_cleanup(true);
}

if (isset($GLOBALS['check_notify'])) {
    $check_notify = $GLOBALS['check_notify'];
} else {
    $check_notify = false;
}
$sugarbean->save($check_notify);
$return_id = $sugarbean->id;

if (isset($_REQUEST['save_type']) || isset($_REQUEST['duplicateSave']) && $_REQUEST['duplicateSave'] === "true") {
    $projectTasksCount = count($projectTasks);
    for ($i = 0; $i < $projectTasksCount; $i++) {
        if (isset($_REQUEST['save_type']) || (isset($_REQUEST['duplicateSave']) && $_REQUEST['duplicateSave'] === "true")) {
            $projectTasks[$i]->id = '';
            $projectTasks[$i]->project_id = $sugarbean->id;
        }
        if ($sugarbean->is_template) {
            $projectTasks[$i]->assigned_user_id = '';
        }
        $projectTasks[$i]->team_id = $sugarbean->team_id ?? '';
        if (empty($projectTasks[$i]->duration_unit)) {
            $projectTasks[$i]->duration_unit = " ";
        } //Since duration_unit cannot be null.
        $projectTasks[$i]->save(false);
    }
}

if ($sugarbean->is_template) {
    header("Location: index.php?action=ProjectTemplatesDetailView&module=Project&record=$return_id&return_module=Project&return_action=ProjectTemplatesEditView");
} else {
    //customize default retrun view to make it to redirect to GanttChart view
    $_REQUEST['return_url'] = "index.php?module=Project&action=view_GanttChart&record=" . $return_id;
    handleRedirect($return_id, 'Project');
}
