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
require_once 'modules/Calendar/controller.php';

class CustomCalendarController extends CalendarController
{
    /**
     * This action is triggered when new filters are added in the Calendar.
     * It saves the filters in the User's preferences
     */ 
    function action_SaveFilters()
    {
        global $current_user;
        $current_user->setPreference('calendar_stic_sessions_color', $_POST['stic_sessions_color']);
        $current_user->setPreference('calendar_stic_sessions_activity_type', $_POST['stic_sessions_activity_type']);
        $current_user->setPreference('calendar_stic_sessions_stic_events_type', $_POST['stic_sessions_stic_events_type']);
        $current_user->setPreference('calendar_stic_sessions_stic_events_id', $_POST['stic_sessions_stic_events_id']);
        $current_user->setPreference('calendar_stic_sessions_stic_centers_id', $_POST['stic_sessions_stic_centers_id']);
        $current_user->setPreference('calendar_stic_sessions_responsible_id', $_POST['stic_sessions_responsible_id']);
        $current_user->setPreference('calendar_stic_sessions_contacts_id', $_POST['stic_sessions_contacts_id']);
        $current_user->setPreference('calendar_stic_sessions_projects_id', $_POST['stic_sessions_projects_id']);
        $current_user->setPreference('calendar_stic_followups_color', $_POST['stic_followups_color']);
        $current_user->setPreference('calendar_stic_followups_type', $_POST['stic_followups_type']);
        $current_user->setPreference('calendar_stic_followups_contacts_id', $_POST['stic_followups_contacts_id']);
        $current_user->setPreference('calendar_stic_followups_projects_id', $_POST['stic_followups_projects_id']);
        $current_user->setPreference('calendar_stic_work_calendar_type', $_POST['stic_work_calendar_type']);
        $current_user->setPreference('calendar_stic_work_calendar_assigned_user_department', $_POST['stic_work_calendar_assigned_user_department']);
        
        if (isset($_REQUEST['day']) && !empty($_REQUEST['day'])) {
            header("Location: index.php?module=Calendar&action=index&view=".$_REQUEST['view']."&hour=0&day=".$_REQUEST['day']."&month=".$_REQUEST['month']."&year=".$_REQUEST['year']);
        } else {
            header("Location: index.php?module=Calendar&action=index");
        }

    }
    /**
     * This action is triggered when there are dynamic enums in the EditView, 
     * and the parent enum needs to be retrieved from the vardef definition of the module.
     */ 
    function action_getFieldVardef() {
    
        $this->view = 'json';
        header('Content-Type: application/json');
        
        $fieldName = $_REQUEST['field'];
        
        $module = $_REQUEST['record_module'] ?? null;

        $bean = BeanFactory::newBean($module);
        $vardef = isset($bean->field_defs[$fieldName]) ? $bean->field_defs[$fieldName] : null;
        
        echo json_encode(array(
            'success' => true,
            'vardef' => $vardef
        ));
        
        exit();
    }    
}
