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
$layout_defs["stic_Job_Offers"]["subpanel_setup"]['stic_job_offers_documents'] = array(
    'order' => 100,
    'module' => 'Documents',
    'subpanel_name' => 'default',
    'sort_order' => 'asc',
    'sort_by' => 'id',
    'title_key' => 'LBL_STIC_JOB_OFFERS_DOCUMENTS_FROM_DOCUMENTS_TITLE',
    'get_subpanel_data' => 'stic_job_offers_documents',
    'top_buttons' => array(
        0 => array(
            'widget_class' => 'SubPanelTopButtonQuickCreate',
        ),
        1 => array(
            'widget_class' => 'SubPanelTopSelectButton',
            'mode' => 'MultiSelect',
        ),
    ),
);

$layout_defs["stic_Job_Offers"]["subpanel_setup"]['activities'] = array(
    'order' => 10,
    'sort_order' => 'asc',
    'sort_by' => 'date_due',
    'title_key' => 'LBL_ACTIVITIES_SUBPANEL_TITLE',
    'type' => 'collection',
    'subpanel_name' => 'activities',
    'module' => 'Activities',
    'top_buttons' => array(
        0 => array(
            'widget_class' => 'SubPanelTopCreateTaskButton',
        ),
        1 => array(
            'widget_class' => 'SubPanelTopScheduleMeetingButton',
        ),
        2 => array(
            'widget_class' => 'SubPanelTopScheduleCallButton',
        ),
        3 => array(
            'widget_class' => 'SubPanelTopComposeEmailButton',
        ),
    ),
    'collection_list' => array(
        'meetings' => array(
            'module' => 'Meetings',
            'subpanel_name' => 'ForActivities',
            'get_subpanel_data' => 'stic_job_offers_activities_meetings',
        ),
        'tasks' => array(
            'module' => 'Tasks',
            'subpanel_name' => 'ForActivities',
            'get_subpanel_data' => 'stic_job_offers_activities_tasks',
        ),
        'calls' => array(
            'module' => 'Calls',
            'subpanel_name' => 'ForActivities',
            'get_subpanel_data' => 'stic_job_offers_activities_calls',
        ),
    ),
    'get_subpanel_data' => 'activities',
);
$layout_defs["stic_Job_Offers"]["subpanel_setup"]['history'] = array(
    'order' => 20,
    'sort_order' => 'desc',
    'sort_by' => 'date_modified',
    'title_key' => 'LBL_HISTORY',
    'type' => 'collection',
    'subpanel_name' => 'history',
    'module' => 'History',
    'top_buttons' => array(
        0 => array(
            'widget_class' => 'SubPanelTopCreateNoteButton',
        ),
        1 => array(
            'widget_class' => 'SubPanelTopArchiveEmailButton',
        ),
        2 => array(
            'widget_class' => 'SubPanelTopSummaryButton',
        ),
    ),
    'collection_list' => array(
        'meetings' => array(
            'module' => 'Meetings',
            'subpanel_name' => 'ForHistory',
            'get_subpanel_data' => 'stic_job_offers_activities_meetings',
        ),
        'tasks' => array(
            'module' => 'Tasks',
            'subpanel_name' => 'ForHistory',
            'get_subpanel_data' => 'stic_job_offers_activities_tasks',
        ),
        'calls' => array(
            'module' => 'Calls',
            'subpanel_name' => 'ForHistory',
            'get_subpanel_data' => 'stic_job_offers_activities_calls',
        ),
        'notes' => array(
            'module' => 'Notes',
            'subpanel_name' => 'ForHistory',
            'get_subpanel_data' => 'stic_job_offers_activities_notes',
        ),
        'emails' => array(
            'module' => 'Emails',
            'subpanel_name' => 'ForHistory',
            'get_subpanel_data' => 'stic_job_offers_activities_emails',
        ),
    ),
    'get_subpanel_data' => 'history',
);

$layout_defs["stic_Job_Offers"]["subpanel_setup"]['stic_job_applications_stic_job_offers'] = array(
    'order' => 1,
    'module' => 'stic_Job_Applications',
    'subpanel_name' => 'default',
    'sort_order' => 'desc',
    'sort_by' => 'start_date',
    'title_key' => 'LBL_STIC_JOB_APPLICATIONS_STIC_JOB_OFFERS_FROM_STIC_JOB_APPLICATIONS_TITLE',
    'get_subpanel_data' => 'stic_job_applications_stic_job_offers',
    'top_buttons' => array(
        0 => array(
            'widget_class' => 'SubPanelTopButtonQuickCreate',
        ),
        1 => array(
            'widget_class' => 'SubPanelTopSelectButton',
            'mode' => 'MultiSelect',
        ),
    ),
);

$layout_defs['stic_Job_Offers']['subpanel_setup']['securitygroups'] = array(
    'top_buttons' => array(array('widget_class' => 'SubPanelTopSelectButton', 'popup_module' => 'SecurityGroups', 'mode' => 'MultiSelect')),
    'order' => 900,
    'sort_by' => 'name',
    'sort_order' => 'asc',
    'module' => 'SecurityGroups',
    'refresh_page' => 1,
    'subpanel_name' => 'default',
    'get_subpanel_data' => 'SecurityGroups',
    'add_subpanel_data' => 'securitygroup_id',
    'title_key' => 'LBL_SECURITYGROUPS_SUBPANEL_TITLE',
);

// Notifications subpanel
$layout_defs['stic_Job_Offers']['subpanel_setup']['stic_campaigns_notification'] = array(
    'order' => 110,
    'module' => 'Campaigns',
    'subpanel_name' => 'SticForNotifications',
    'sort_order' => 'asc',
    'sort_by' => 'name',
    'get_subpanel_data' => 'function:getNotificationsFromParent',
    'function_parameters' => array(
        'import_function_file' => 'custom/modules/Campaigns/SticUtils.php',
        'parent_id' => $this->_focus->id,
        'parent_type' => 'stic_Job_Offers',
        'return_as_array' => false,
    ),
    'title_key' => 'LBL_STIC_CAMPAIGNS_NOTIFICATION_FROM_STIC_JOB_OFFERS_TITLE',
    'top_buttons' => array(
        0 => array(
            'widget_class' => 'CustomSubPanelTopButtonQuickCreate',
            'title' => 'LBL_NEW_NOTIFICATION',
            'id'=> 'NEW_NOTIFICATION',
            'additional_form_fields' => array(
                'campaign_type' => 'Notification', // This will pre-populate the 'type' field
                // 'another_field' => 'another_value', // You can add more here
            ),
        ),
        1 => array(
            'widget_class' => 'CustomSubPanelTopButtonQuickCreate',
            'id' => 'NEW_MSG_NOTIFICATION',
            'title' => 'LBL_NEW_MSG_NOTIFICATION',
            'additional_form_fields' => array(
                'campaign_type' => 'NotifMsg', // This will pre-populate the 'type' field
                // 'another_field' => 'another_value', // You can add more here
            ),
        ),
    ),
);
