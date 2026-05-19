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
$module_name = 'stic_Job_Offers';
$viewdefs[$module_name] =
array(
    'QuickCreate' => array(
        'templateMeta' => array(
            'maxColumns' => '2',
            'widths' => array(
                0 => array(
                    'label' => '10',
                    'field' => '30',
                ),
                1 => array(
                    'label' => '10',
                    'field' => '30',
                ),
            ),
            'useTabs' => true,
            'tabDefs' => array(
                'LBL_DEFAULT_PANEL' => array(
                    'newTab' => true,
                    'panelDefault' => 'expanded',
                ),
                'LBL_EDITVIEW_PANEL5' => array(
                    'newTab' => false,
                    'panelDefault' => 'expanded',
                ),
                'LBL_EDITVIEW_PANEL8' => array(
                    'newTab' => true,
                    'panelDefault' => 'expanded',
                ),                
                'LBL_EDITVIEW_PANEL2' => array(
                    'newTab' => true,
                    'panelDefault' => 'expanded',
                ),
                'LBL_EDITVIEW_PANEL3' => array(
                    'newTab' => true,
                    'panelDefault' => 'expanded',
                ),
                'LBL_EDITVIEW_PANEL7' => array(
                    'newTab' => false,
                    'panelDefault' => 'expanded',
                ),
                'LBL_PANEL_NOTIFICATION_TEMPLATES' => array(
                    'newTab' => false,
                    'panelDefault' => 'expanded',
                ),
            ),
            'syncDetailEditViews' => false,
        ),
        'panels' => array(
            'LBL_DEFAULT_PANEL' => array(
                0 => array (
                    0 => 'name',
                    1 => 'assigned_user_name',
                ),
                1 => array (
                    0 => 
                    array (
                        'name' => 'stic_job_offers_accounts_name',
                    ),
                    1 => 
                    array (
                        'name' => 'interlocutor',
                        'studio' => 'visible',
                        'label' => 'LBL_INTERLOCUTOR',
                    ),
                ),
                2 => array (
                    0 => 
                    array (
                        'name' => 'offer_type',
                        'studio' => 'visible',
                        'label' => 'LBL_OFFER_TYPE',
                    ),
                    1 => 
                    array (
                        'name' => 'status',
                        'studio' => 'visible',
                        'label' => 'LBL_STATUS',
                    ),
                ),
                3 => array (
                    0 => 
                    array (
                        'name' => 'type',
                        'studio' => 'visible',
                        'label' => 'LBL_TYPE',
                    ),
                    1 => 
                    array (
                        'name' => 'territorial_scope',
                        'studio' => 'visible',
                        'label' => 'LBL_TERRITORIAL_SCOPE',
                    ),
                ),
                4 => array (
                    0 => 
                    array (
                        'name' => 'offer_origin',
                        'studio' => 'visible',
                        'label' => 'LBL_OFFER_ORIGIN',
                    ),
                    1 => 
                    array (
                        'name' => 'offered_positions',
                        'label' => 'LBL_OFFERED_POSITIONS',
                    ),
                ),
                5 => array (
                    0 => 
                    array (
                        'name' => 'process_start_date',
                        'label' => 'LBL_PROCESS_START_DATE',
                    ),
                    1 => 
                    array (
                        'name' => 'process_end_date',
                        'label' => 'LBL_PROCESS_END_DATE',
                    ),
                ),
                6 => array (
                    0 => 
                    array (
                        'name' => 'applications_start_date',
                        'label' => 'LBL_APPLICATIONS_START_DATE',
                    ),
                    1 => 
                    array (
                        'name' => 'applications_end_date',
                        'label' => 'LBL_APPLICATIONS_END_DATE',
                    ),
                ),
                7 => array (
                    0 => 
                    array (
                        'name' => 'offer_code',
                        'label' => 'LBL_OFFER_CODE',
                    ),
                    1 => 
                    array (
                        'name' => 'offer_mode',
                        'studio' => 'visible',
                        'label' => 'LBL_OFFER_MODE',
                    ),
                ),
                8 => array (
                    0 => 
                    array (
                        'name' => 'weekdays',
                        'studio' => 'visible',
                        'label' => 'LBL_WEEKDAYS',
                    ),
                    1 => 
                    array (
                        'name' => 'timetable',
                        'label' => 'LBL_TIMETABLE',
                    ),
                ),
                9 => array (
                    0 => 
                    array (
                        'name' => 'evaluation',
                        'label' => 'LBL_EVALUATION',
                    ),
                    1 => 
                    array (
                        'name' => 'responsible',
                        'studio' => 'visible',
                        'label' => 'LBL_RESPONSIBLE',
                    ),
                ),
                10 => array (
                    0 => 
                    array (
                        'name' => 'center',
                        'studio' => 'visible',
                        'label' => 'LBL_CENTER',
                    ),
                    1 => 
                    array (
                        'name' => 'project_stic_job_offers_name',
                    ),
                ),
                11 => array (
                    0 => 'description',
                ),
            ),
            'LBL_PANEL_NOTIFICATION_TEMPLATES' => array(
                0 => array(
                    0 => array(
                        'name' => 'notification_status',
                        'studio' => 'visible',
                        'label' => 'LBL_NOTIFICATIONS_STATUS',
                    ),
                    1 => '',
                ),
                1 => array(
                    0 => array(
                        'name' => 'notification_template_assigned_user',
                        'studio' => 'visible',
                        'label' => 'LBL_NOTIFICATION_TEMPLATE_ASSIGNED_USER',
                    ),
                    1 => array(
                        'name' => 'notification_template_candidates',
                        'studio' => 'visible',
                        'label' => 'LBL_NOTIFICATION_TEMPLATE_CANDIDATES',
                    ),
                ),
                2 => array(
                    0 => array(
                        'name' => 'notification_template_interlocutor',
                        'studio' => 'visible',
                        'label' => 'LBL_NOTIFICATION_TEMPLATE_INTERLOCUTOR',
                    ),
                    1 => array(),
                ),
                3 => array(
                    0 => array(
                        'name' => 'notification_template_presented_interlocutor',
                        'studio' => 'visible',
                        'label' => 'LBL_NOTIFICATION_TEMPLATE_INTERLOCUTOR_PRESENTED',
                    ),
                    1 => array(
                        'name' => 'notification_template_accepted_interlocutor',
                        'studio' => 'visible',
                        'label' => 'LBL_NOTIFICATION_TEMPLATE_INTERLOCUTOR_ACCEPTED',
                    ),
                ),
                4 => array(
                    0 => array(
                        'name' => 'notification_template_rejected_interlocutor',
                        'studio' => 'visible',
                        'label' => 'LBL_NOTIFICATION_TEMPLATE_INTERLOCUTOR_REJECTED',
                    ),
                    1 => array(),
                ),
                5 => array(
                    0 => array(
                        'name' => 'notification_template_cancelled_assigned_user',
                        'studio' => 'visible',
                        'label' => 'LBL_NOTIFICATION_TEMPLATE_CANCELLED_ASSIGNED_USER',
                    ),
                    1 => array(
                        'name' => 'notification_template_cancelled_interlocutor',
                        'studio' => 'visible',
                        'label' => 'LBL_NOTIFICATION_TEMPLATE_CANCELLED_INTERLOCUTOR',
                    ),
                ),
            ),
            'lbl_editview_panel5' => array(
                0 => array(
                    0 => array(
                        'name' => 'professional_profile',
                        'studio' => 'visible',
                        'label' => 'LBL_PROFESSIONAL_PROFILE',
                    ),
                    1 => array(
                        'name' => 'job_requirements',
                        'studio' => 'visible',
                        'label' => 'LBL_JOB_REQUIREMENTS',
                    ),
                ),
                1 => array(
                    0 => array(
                        'name' => 'hours_per_week',
                        'label' => 'LBL_HOURS_PER_WEEK',
                    ),
                    1 => array(
                        'name' => 'contract_description',
                        'studio' => 'visible',
                        'label' => 'LBL_CONTRACT_DESCRIPTION',
                    ),
                ),
                2 => 
                array (
                  0 => 
                  array (
                    'name' => 'retribution',
                    'label' => 'LBL_RETRIBUTION',
                  ),
                  1 => 
                  array (
                    'name' => 'retribution_conditions',
                    'studio' => 'visible',
                    'label' => 'LBL_RETRIBUTION_CONDITIONS',
                  ),
                ),
                3 => array(
                    0 => array(
                        'name' => 'sector',
                        'studio' => 'visible',
                        'label' => 'LBL_SECTOR',
                    ),
                    1 => array(
                        'name' => 'subsector',
                        'studio' => 'visible',
                        'label' => 'LBL_SUBSECTOR',
                    ),
                ),
                4 => array(
                    0 => array(
                        'name' => 'position_type',
                        'studio' => 'visible',
                        'label' => 'LBL_POSITION_TYPE',
                    ),
                    1 => array(
                        'name' => 'contract_type',
                        'studio' => 'visible',
                        'label' => 'LBL_CONTRACT_TYPE',
                    ),
                ),
                5 => array(
                    0 => array(
                        'name' => 'workday_type',
                        'studio' => 'visible',
                        'label' => 'LBL_WORKDAY_TYPE',
                    ),
                    1 => array(
                    ),
                ),
            ),
            'lbl_editview_panel8' => array(
                0 => array(
                    0 => array(
                        'name' => 'temporality',
                        'studio' => 'visible',
                        'label' => 'LBL_TEMPORALITY',
                    ),
                    1 => array(
                        'name' => 'expected_duration',
                        'label' => 'LBL_EXPECTED_DURATION',
                    ),
                ),
                1 => array(
                    0 => array(
                        'name' => 'goals',
                        'label' => 'LBL_GOALS',
                    ),
                ),  
            ),
            'lbl_editview_panel2' => array(
                0 => array(
                    0 => array(
                        'name' => 'sepe_contract_type',
                        'studio' => 'visible',
                        'label' => 'LBL_SEPE_CONTRACT_TYPE',
                    ),
                ),
                1 => array(
                    0 => array(
                        'name' => 'sepe_activation_date',
                        'label' => 'LBL_SEPE_ACTIVATION_DATE',
                    ),
                    1 => array(
                        'name' => 'sepe_covered_date',
                        'label' => 'LBL_SEPE_COVERED_DATE',
                    ),
                ),
            ),
            'lbl_editview_panel3' => array(
                0 => array(
                    0 => array(
                        'name' => 'inc_id',
                        'label' => 'LBL_INC_ID',
                    ),
                    1 => array(
                        'name' => 'inc_incorpora_record',
                        'label' => 'LBL_INC_INCORPORA_RECORD',
                    ),
                ),
                1 => array(
                    0 => array(
                        'name' => 'inc_status',
                        'studio' => 'visible',
                        'label' => 'LBL_INC_STATUS',
                    ),
                ),
                2 => array(
                    0 => array(
                        'name' => 'inc_officer_email',
                        'label' => 'LBL_INC_OFFICER_EMAIL',
                    ),
                    1 => array(
                        'name' => 'inc_officer_telephone',
                        'label' => 'LBL_INC_OFFICER_TELEPHONE',
                    ),
                ),
                3 => array(
                    0 => array(
                        'name' => 'inc_offer_origin',
                        'studio' => 'visible',
                        'label' => 'LBL_INC_OFFER_ORIGIN',
                    ),
                    1 => array(
                        'name' => 'inc_checkin_date',
                        'label' => 'LBL_INC_CHECKIN_DATE',
                    ),
                ),
                4 => array(
                    0 => array(
                        'name' => 'inc_register_start_date',
                        'label' => 'LBL_INC_REGISTER_START_DATE',
                    ),
                    1 => array(
                        'name' => 'inc_register_end_date',
                        'label' => 'LBL_INC_REGISTER_END_DATE',
                    ),
                ),
                5 => array(
                    0 => array(
                        'name' => 'inc_contract_start_date',
                        'label' => 'LBL_INC_CONTRACT_START_DATE',
                    ),
                    1 => array(
                        'name' => 'inc_contract_type',
                        'studio' => 'visible',
                        'label' => 'LBL_INC_CONTRACT_TYPE',
                    ),
                ),
                6 => array(
                    0 => array(
                        'name' => 'inc_contract_duration',
                        'studio' => 'visible',
                        'label' => 'LBL_INC_CONTRACT_DURATION',
                    ),
                    1 => array(
                        'name' => 'contract_duration_details',
                        'comment' => 'Full text of the note',
                        'studio' => 'visible',
                        'label' => 'LBL_CONTRACT_DURATION_DETAILS',
                    ),
                ),
                7 => array(
                    0 => array(
                        'name' => 'inc_remuneration',
                        'label' => 'LBL_INC_REMUNERATION',
                    ),
                    1 => array(
                        'name' => 'inc_tasks_responsabilities',
                        'comment' => 'Full text of the note',
                        'studio' => 'visible',
                        'label' => 'LBL_INC_TASKS_RESPONSABILITIES',
                    ),
                ),
                8 => array(
                    0 => array(
                        'name' => 'inc_cno_n1',
                        'studio' => 'visible',
                        'label' => 'LBL_INC_CNO_N1',
                    ),
                ),
                9 => array(
                    0 => array(
                        'name' => 'inc_cno_n2',
                        'studio' => 'visible',
                        'label' => 'LBL_INC_CNO_N2',
                    ),
                ),
                10 => array(
                    0 => array(
                        'name' => 'inc_cno_n3',
                        'studio' => 'visible',
                        'label' => 'LBL_INC_CNO_N3',
                    ),
                ),
                11 => array(
                    0 => array(
                        'name' => 'inc_working_day',
                        'studio' => 'visible',
                        'label' => 'LBL_INC_WORKING_DAY',
                    ),
                    1 => array(
                        'name' => 'inc_observations',
                        'studio' => 'visible',
                        'label' => 'LBL_INC_OBSERVATIONS',
                    ),
                ),
                12 => array(
                    0 => array(
                        'name' => 'inc_location',
                        'studio' => 'visible',
                        'label' => 'LBL_INC_LOCATION',
                    ),
                    1 => array(
                        'name' => 'inc_country',
                        'studio' => 'visible',
                        'label' => 'LBL_INC_COUNTRY',
                    ),
                ),
            ),
            'LBL_EDITVIEW_PANEL7' => array(
                0 => array(
                    0 => array(
                        'name' => 'inc_collective_requirements',
                        'studio' => 'visible',
                        'label' => 'LBL_INC_COLLECTIVE_REQUIREMENTS',
                    ),
                    1 => array(
                        'name' => 'inc_education_languages',
                        'studio' => 'visible',
                        'label' => 'LBL_INC_EDUCATION_LANGUAGES',
                    ),
                ),
                1 => array(
                    0 => array(
                        'name' => 'inc_working_experience',
                        'comment' => '',
                        'studio' => 'visible',
                        'label' => 'LBL_INC_WORKING_EXPERIENCE',
                    ),
                    1 => array(
                        'name' => 'inc_education',
                        'studio' => 'visible',
                        'label' => 'LBL_INC_EDUCATION',
                    ),
                ),
                2 => array(
                    0 => array(
                        'name' => 'inc_minimum_age',
                        'label' => 'LBL_INC_MINIMUM_AGE',
                    ),
                    1 => array(
                        'name' => 'inc_maximum_age',
                        'label' => 'LBL_INC_MAXIMUM_AGE',
                    ),
                ),
                3 => array(
                    0 => array(
                        'name' => 'inc_professional_licenses',
                        'label' => 'LBL_INC_PROFESSIONAL_LICENSES',
                    ),
                    1 => array(
                        'name' => 'inc_driving_licenses',
                        'label' => 'LBL_INC_DRIVING_LICENSES',
                    ),
                ),
            ),
        ),
    ),
);
