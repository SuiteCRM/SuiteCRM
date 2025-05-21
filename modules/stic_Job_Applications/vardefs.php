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

$dictionary['stic_Job_Applications'] = array(
    'table' => 'stic_job_applications',
    'audited' => true,
    'duplicate_merge' => true,
    'fields' => array(
        'start_date' =>
        array(
            'required' => true,
            'name' => 'start_date',
            'vname' => 'LBL_START_DATE',
            'type' => 'date',
            'massupdate' => 1,
            'no_default' => false,
            'comments' => '',
            'help' => '',
            'importable' => 'true',
            'duplicate_merge' => 'enabled',
            'duplicate_merge_dom_value' => '2',
            'audited' => false,
            'reportable' => true,
            'unified_search' => false,
            'merge_filter' => 'enabled',
            'size' => '20',
            'enable_range_search' => '1',
            'options' => 'date_range_search_dom',
        ),
        'end_date' =>
        array(
            'required' => false,
            'name' => 'end_date',
            'vname' => 'LBL_END_DATE',
            'type' => 'date',
            'massupdate' => 1,
            'no_default' => false,
            'comments' => '',
            'help' => '',
            'importable' => 'true',
            'duplicate_merge' => 'enabled',
            'duplicate_merge_dom_value' => '2',
            'audited' => false,
            'reportable' => true,
            'unified_search' => false,
            'merge_filter' => 'enabled',
            'size' => '20',
            'enable_range_search' => '1',
            'options' => 'date_range_search_dom',
        ),
        'status' =>
        array(
            'required' => true,
            'name' => 'status',
            'vname' => 'LBL_STATUS',
            'type' => 'enum',
            'massupdate' => 1,
            'no_default' => false,
            'comments' => '',
            'help' => '',
            'importable' => 'true',
            'duplicate_merge' => 'enabled',
            'duplicate_merge_dom_value' => '2',
            'audited' => false,
            'reportable' => true,
            'unified_search' => false,
            'merge_filter' => 'enabled',
            'len' => 100,
            'size' => '20',
            'options' => 'stic_job_applications_status_list',
            'studio' => 'visible',
            'dependency' => false,
        ),
        'status_details' =>
        array(
            'required' => false,
            'name' => 'status_details',
            'vname' => 'LBL_STATUS_DETAILS',
            'type' => 'text',
            'massupdate' => 0,
            'no_default' => false,
            'comments' => '',
            'help' => '',
            'importable' => 'true',
            'duplicate_merge' => 'enabled',
            'duplicate_merge_dom_value' => '2',
            'audited' => false,
            'reportable' => true,
            'unified_search' => false,
            'merge_filter' => 'enabled',
            'size' => '20',
            'studio' => 'visible',
            'rows' => '2',
            'cols' => '40',
        ),
        'motivations' =>
        array(
            'required' => false,
            'name' => 'motivations',
            'vname' => 'LBL_MOTIVATIONS',
            'type' => 'text',
            'massupdate' => 0,
            'no_default' => false,
            'comments' => '',
            'help' => '',
            'importable' => 'true',
            'duplicate_merge' => 'enabled',
            'duplicate_merge_dom_value' => '2',
            'audited' => false,
            'reportable' => true,
            'unified_search' => false,
            'merge_filter' => 'enabled',
            'size' => '20',
            'studio' => 'visible',
            'rows' => '2',
            'cols' => '40',
        ),
        'preinsertion_observations' =>
        array(
            'required' => false,
            'name' => 'preinsertion_observations',
            'vname' => 'LBL_PREINSERTION_OBSERVATIONS',
            'type' => 'text',
            'massupdate' => 0,
            'no_default' => false,
            'comments' => '',
            'help' => '',
            'importable' => 'true',
            'duplicate_merge' => 'enabled',
            'duplicate_merge_dom_value' => '2',
            'audited' => false,
            'reportable' => true,
            'unified_search' => false,
            'merge_filter' => 'enabled',
            'size' => '20',
            'studio' => 'visible',
            'rows' => '2',
            'cols' => '40',
        ),
        'contract_start_date' =>
        array(
            'required' => false,
            'name' => 'contract_start_date',
            'vname' => 'LBL_CONTRACT_START_DATE',
            'type' => 'date',
            'massupdate' => 1,
            'no_default' => false,
            'comments' => '',
            'help' => '',
            'importable' => 'true',
            'duplicate_merge' => 'enabled',
            'duplicate_merge_dom_value' => '2',
            'audited' => false,
            'reportable' => true,
            'unified_search' => false,
            'merge_filter' => 'enabled',
            'size' => '20',
            'enable_range_search' => '1',
            'options' => 'date_range_search_dom',
        ),
        'contract_end_date' =>
        array(
            'required' => false,
            'name' => 'contract_end_date',
            'vname' => 'LBL_CONTRACT_END_DATE',
            'type' => 'date',
            'massupdate' => 1,
            'no_default' => false,
            'comments' => '',
            'help' => '',
            'importable' => 'true',
            'duplicate_merge' => 'enabled',
            'duplicate_merge_dom_value' => '2',
            'audited' => false,
            'reportable' => true,
            'unified_search' => false,
            'merge_filter' => 'enabled',
            'size' => '20',
            'enable_range_search' => '1',
            'options' => 'date_range_search_dom',
        ),
        'contract_end_reason' =>
        array(
            'required' => false,
            'name' => 'contract_end_reason',
            'vname' => 'LBL_CONTRACT_END_REASON',
            'type' => 'enum',
            'massupdate' => 1,
            'no_default' => false,
            'comments' => '',
            'help' => '',
            'importable' => 'true',
            'duplicate_merge' => 'enabled',
            'duplicate_merge_dom_value' => '2',
            'audited' => false,
            'reportable' => true,
            'unified_search' => false,
            'merge_filter' => 'enabled',
            'len' => 100,
            'size' => '20',
            'options' => 'stic_contract_end_reason_list',
            'studio' => 'visible',
            'dependency' => false,
        ),
        'postinsertion_observations' =>
        array(
            'required' => false,
            'name' => 'postinsertion_observations',
            'vname' => 'LBL_POSTINSERTION_OBSERVATIONS',
            'type' => 'text',
            'massupdate' => 0,
            'no_default' => false,
            'comments' => '',
            'help' => '',
            'importable' => 'true',
            'duplicate_merge' => 'enabled',
            'duplicate_merge_dom_value' => '2',
            'audited' => false,
            'reportable' => true,
            'unified_search' => false,
            'merge_filter' => 'enabled',
            'size' => '20',
            'studio' => 'visible',
            'rows' => '2',
            'cols' => '40',
        ),
        'rejection_reason' =>
        array(
            'required' => false,
            'name' => 'rejection_reason',
            'vname' => 'LBL_REJECTION_REASON',
            'type' => 'enum',
            'massupdate' => 1,
            'no_default' => false,
            'comments' => '',
            'help' => '',
            'importable' => 'true',
            'duplicate_merge' => 'enabled',
            'duplicate_merge_dom_value' => '2',
            'audited' => false,
            'reportable' => true,
            'unified_search' => false,
            'merge_filter' => 'enabled',
            'len' => 100,
            'size' => '20',
            'options' => 'stic_job_applications_rejection_reasons_list',
            'studio' => 'visible',
            'dependency' => false,
            'popupHelp' => 'LBL_REJECTION_REASON_HELP',
        ),
        'stic_job_applications_activities_calls' =>
        array(
            'name' => 'stic_job_applications_activities_calls',
            'type' => 'link',
            'relationship' => 'stic_job_applications_activities_calls',
            'source' => 'non-db',
            'vname' => 'LBL_STIC_JOB_APPLICATIONS_ACTIVITIES_CALLS_FROM_CALLS_TITLE',
        ),
        'stic_job_applications_activities_emails' =>
        array(
            'name' => 'stic_job_applications_activities_emails',
            'type' => 'link',
            'relationship' => 'stic_job_applications_activities_emails',
            'source' => 'non-db',
            'vname' => 'LBL_STIC_JOB_APPLICATIONS_ACTIVITIES_EMAILS_FROM_EMAILS_TITLE',
        ),
        'stic_job_applications_activities_meetings' =>
        array(
            'name' => 'stic_job_applications_activities_meetings',
            'type' => 'link',
            'relationship' => 'stic_job_applications_activities_meetings',
            'source' => 'non-db',
            'vname' => 'LBL_STIC_JOB_APPLICATIONS_ACTIVITIES_MEETINGS_FROM_MEETINGS_TITLE',
        ),
        'stic_job_applications_activities_notes' =>
        array(
            'name' => 'stic_job_applications_activities_notes',
            'type' => 'link',
            'relationship' => 'stic_job_applications_activities_notes',
            'source' => 'non-db',
            'vname' => 'LBL_STIC_JOB_APPLICATIONS_ACTIVITIES_NOTES_FROM_NOTES_TITLE',
        ),
        'stic_job_applications_activities_tasks' =>
        array(
            'name' => 'stic_job_applications_activities_tasks',
            'type' => 'link',
            'relationship' => 'stic_job_applications_activities_tasks',
            'source' => 'non-db',
            'vname' => 'LBL_STIC_JOB_APPLICATIONS_ACTIVITIES_TASKS_FROM_TASKS_TITLE',
        ),
        'stic_job_applications_contacts' =>
        array(
            'name' => 'stic_job_applications_contacts',
            'type' => 'link',
            'relationship' => 'stic_job_applications_contacts',
            'source' => 'non-db',
            'vname' => 'LBL_STIC_JOB_APPLICATIONS_CONTACTS_FROM_CONTACTS_TITLE',
            'id_name' => 'stic_job_applications_contactscontacts_ida',
        ),
        'stic_job_applications_contacts_name' =>
        array(
            'required' => true,
            'name' => 'stic_job_applications_contacts_name',
            'type' => 'relate',
            'source' => 'non-db',
            'vname' => 'LBL_STIC_JOB_APPLICATIONS_CONTACTS_FROM_CONTACTS_TITLE',
            'save' => true,
            'id_name' => 'stic_job_applications_contactscontacts_ida',
            'link' => 'stic_job_applications_contacts',
            'table' => 'contacts',
            'module' => 'Contacts',
            'rname' => 'name',
            'db_concat_fields' =>
            array(
                0 => 'first_name',
                1 => 'last_name',
            ),
        ),
        'stic_job_applications_contactscontacts_ida' =>
        array(
            'name' => 'stic_job_applications_contactscontacts_ida',
            'type' => 'link',
            'relationship' => 'stic_job_applications_contacts',
            'source' => 'non-db',
            'reportable' => false,
            'side' => 'right',
            'vname' => 'LBL_STIC_JOB_APPLICATIONS_CONTACTS_FROM_STIC_JOB_APPLICATIONS_TITLE',
        ),
        'stic_job_applications_documents' =>
        array(
            'name' => 'stic_job_applications_documents',
            'type' => 'link',
            'relationship' => 'stic_job_applications_documents',
            'source' => 'non-db',
            'vname' => 'LBL_STIC_JOB_APPLICATIONS_DOCUMENTS_FROM_DOCUMENTS_TITLE',
        ),
        'stic_job_applications_stic_job_offers' =>
        array(
            'name' => 'stic_job_applications_stic_job_offers',
            'type' => 'link',
            'relationship' => 'stic_job_applications_stic_job_offers',
            'source' => 'non-db',
            'vname' => 'LBL_STIC_JOB_APPLICATIONS_STIC_JOB_OFFERS_FROM_STIC_JOB_OFFERS_TITLE',
            'id_name' => 'stic_job_applications_stic_job_offersstic_job_offers_ida',
        ),
        'stic_job_applications_stic_job_offers_name' =>
        array(
            'required' => true,
            'name' => 'stic_job_applications_stic_job_offers_name',
            'type' => 'relate',
            'source' => 'non-db',
            'vname' => 'LBL_STIC_JOB_APPLICATIONS_STIC_JOB_OFFERS_FROM_STIC_JOB_OFFERS_TITLE',
            'save' => true,
            'id_name' => 'stic_job_applications_stic_job_offersstic_job_offers_ida',
            'link' => 'stic_job_applications_stic_job_offers',
            'table' => 'stic_job_offers',
            'module' => 'stic_Job_Offers',
            'rname' => 'name',
        ),
        'stic_job_applications_stic_job_offersstic_job_offers_ida' =>
        array(
            'name' => 'stic_job_applications_stic_job_offersstic_job_offers_ida',
            'type' => 'link',
            'relationship' => 'stic_job_applications_stic_job_offers',
            'source' => 'non-db',
            'reportable' => false,
            'side' => 'right',
            'vname' => 'LBL_STIC_JOB_APPLICATIONS_STIC_JOB_OFFERS_FROM_STIC_JOB_APPLICATIONS_TITLE',
        ),
        'stic_work_experience_stic_job_applications' => array(
            'name' => 'stic_work_experience_stic_job_applications',
            'type' => 'link',
            'relationship' => 'stic_work_experience_stic_job_applications',
            'source' => 'non-db',
            'module' => 'stic_Work_Experience',
            'bean_name' => 'stic_Work_Experience',
            'vname' => 'LBL_STIC_WORK_EXPERIENCE_STIC_JOB_APPLICATIONS_FROM_STIC_WORK_EXPERIENCE_TITLE',
            'id_name' => 'stic_work_537ferience_ida',
        ),
        'stic_work_experience_stic_job_applications_name' => array(
            'name' => 'stic_work_experience_stic_job_applications_name',
            'type' => 'relate',
            'source' => 'non-db',
            'vname' => 'LBL_STIC_WORK_EXPERIENCE_STIC_JOB_APPLICATIONS_FROM_STIC_WORK_EXPERIENCE_TITLE',
            'save' => 1,
            'id_name' => 'stic_work_537ferience_ida',
            'link' => 'stic_work_experience_stic_job_applications',
            'table' => 'stic_work_experience',
            'module' => 'stic_Work_Experience',
            'rname' => 'name',
            'massupdate' => 0, // unusual
            'inline_edit' => 1,
        ),
        'stic_work_537ferience_ida' => array(
            'name' => 'stic_work_537ferience_ida',
            'type' => 'link',
            'relationship' => 'stic_work_experience_stic_job_applications',
            'source' => 'non-db',
            'reportable' => 0,
            'side' => 'left',
            'vname' => 'LBL_STIC_WORK_EXPERIENCE_STIC_JOB_APPLICATIONS_FROM_STIC_WORK_EXPERIENCE_TITLE',
        ),

    ),
    'relationships' => array(),
    'optimistic_locking' => true,
    'unified_search' => true,
    'unified_search_default_enabled' => true,
);
if (!class_exists('VardefManager')) {
    require_once('include/SugarObjects/VardefManager.php');
}
VardefManager::createVardef('stic_Job_Applications', 'stic_Job_Applications', array('basic', 'assignable', 'security_groups'));

// Set special values for SuiteCRM base fields
$dictionary['stic_Job_Applications']['fields']['name']['required'] = '0'; // Name is not required in this module
$dictionary['stic_Job_Applications']['fields']['name']['importable'] = true; // Name is importable but not required in this module
$dictionary['stic_Job_Applications']['fields']['description']['rows'] = '2'; // Make textarea fields shorter
