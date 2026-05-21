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

$dictionary['Contact'] = array('table' => 'contacts', 'audited' => true,

    'unified_search' => true, 'full_text_search' => true, 'unified_search_default_enabled' => true, 'duplicate_merge' => true, 'fields' =>
        array(

            'email_and_name1' =>
                array(
                    'name' => 'email_and_name1',
                    'rname' => 'email_and_name1',
                    'vname' => 'LBL_NAME',
                    'type' => 'varchar',
                    'source' => 'non-db',
                    'len' => '510',
                    'importable' => 'false',
                ),
            'lead_source' =>
                array(
                    'name' => 'lead_source',
                    'vname' => 'LBL_LEAD_SOURCE',
                    'type' => 'enum',
                    'options' => 'lead_source_dom',
                    'len' => '255',
                    'comment' => 'How did the contact come about',
                ),

            'account_name' =>
                array(
                    'name' => 'account_name',
                    'rname' => 'name',
                    'id_name' => 'account_id',
                    'vname' => 'LBL_ACCOUNT_NAME',
                    'join_name' => 'accounts',
                    'type' => 'relate',
                    'link' => 'accounts',
                    'table' => 'accounts',
                    'isnull' => 'true',
                    'module' => 'Accounts',
                    'dbType' => 'varchar',
                    'len' => '255',
                    'source' => 'non-db',
                    'unified_search' => true,
                ),
            'account_id' =>
                array(
                    'name' => 'account_id',
                    'rname' => 'id',
                    'id_name' => 'account_id',
                    'vname' => 'LBL_ACCOUNT_ID',
                    'type' => 'relate',
                    'table' => 'accounts',
                    'isnull' => 'true',
                    'module' => 'Accounts',
                    'dbType' => 'id',
                    'reportable' => false,
                    'source' => 'non-db',
                    'massupdate' => false,
                    'duplicate_merge' => 'disabled',
                    'hideacl' => true,
                ),
            'opportunity_role_fields' =>
                array(
                    'name' => 'opportunity_role_fields',
                    'rname' => 'id',
                    'relationship_fields' => array('id' => 'opportunity_role_id', 'contact_role' => 'opportunity_role'),
                    'vname' => 'LBL_ACCOUNT_NAME',
                    'type' => 'relate',
                    'link' => 'opportunities',
                    'link_type' => 'relationship_info',
                    'join_link_name' => 'opportunities_contacts',
                    'source' => 'non-db',
                    'importable' => 'false',
                    'duplicate_merge' => 'disabled',
                    'studio' => false,
                    'join_primary' => false,
                ),
            'opportunity_role_id' =>
                array(
                    'name' => 'opportunity_role_id',
                    'type' => 'varchar',
                    'source' => 'non-db',
                    'vname' => 'LBL_OPPORTUNITY_ROLE_ID',
                    'studio' => array('listview' => false),
                ),
            'opportunity_role' =>
                array(
                    'name' => 'opportunity_role',
                    'type' => 'enum',
                    'source' => 'non-db',
                    'vname' => 'LBL_OPPORTUNITY_ROLE',
                    'options' => 'opportunity_relationship_type_dom',
                ),
            'reports_to_id' =>
                array(
                    'name' => 'reports_to_id',
                    'vname' => 'LBL_REPORTS_TO_ID',
                    'type' => 'id',
                    'required' => false,
                    'reportable' => false,
                    'comment' => 'The contact this contact reports to'
                ),
            'report_to_name' =>
                array(
                    'name' => 'report_to_name',
                    'rname' => 'last_name',
                    'id_name' => 'reports_to_id',
                    'vname' => 'LBL_REPORTS_TO',
                    'type' => 'relate',
                    'link' => 'reports_to_link',
                    'table' => 'contacts',
                    'isnull' => 'true',
                    'module' => 'Contacts',
                    'dbType' => 'varchar',
                    'len' => 'id',
                    'reportable' => false,
                    'source' => 'non-db',
                ),
            'birthdate' =>
                array(
                    'name' => 'birthdate',
                    'vname' => 'LBL_BIRTHDATE',
                    'massupdate' => false,
                    'type' => 'date',
                    'comment' => 'The birthdate of the contact'
                ),
            'accounts' =>
                array(
                    'name' => 'accounts',
                    'type' => 'link',
                    'relationship' => 'accounts_contacts',
                    'link_type' => 'one',
                    'source' => 'non-db',
                    'vname' => 'LBL_ACCOUNT',
                    'duplicate_merge' => 'disabled',
                ),
            'reports_to_link' =>
                array(
                    'name' => 'reports_to_link',
                    'type' => 'link',
                    'relationship' => 'contact_direct_reports',
                    'link_type' => 'one',
                    'side' => 'right',
                    'source' => 'non-db',
                    'vname' => 'LBL_REPORTS_TO',
                ),
            'opportunities' =>
                array(
                    'name' => 'opportunities',
                    'type' => 'link',
                    'relationship' => 'opportunities_contacts',
                    'source' => 'non-db',
                    'module' => 'Opportunities',
                    'bean_name' => 'Opportunity',
                    'vname' => 'LBL_OPPORTUNITIES',
                ),
            'email_addresses' =>
                array(
                    'name' => 'email_addresses',
                    'type' => 'link',
                    'relationship' => 'contacts_email_addresses',
                    'module' => 'EmailAddress',
                    'bean_name' => 'EmailAddress',
                    'source' => 'non-db',
                    'vname' => 'LBL_EMAIL_ADDRESSES',
                    'reportable' => false,
                    'rel_fields' => array('primary_address' => array('type' => 'bool')),
                    'unified_search' => true,
                ),
            'email_addresses_primary' =>
                array(
                    'name' => 'email_addresses_primary',
                    'type' => 'link',
                    'relationship' => 'contacts_email_addresses_primary',
                    'source' => 'non-db',
                    'vname' => 'LBL_EMAIL_ADDRESS_PRIMARY',
                    'duplicate_merge' => 'disabled',
                ),
            'bugs' =>
                array(
                    'name' => 'bugs',
                    'type' => 'link',
                    'relationship' => 'contacts_bugs',
                    'source' => 'non-db',
                    'vname' => 'LBL_BUGS',
                ),
            'calls' =>
                array(
                    'name' => 'calls',
                    'type' => 'link',
                    'relationship' => 'calls_contacts',
                    'source' => 'non-db',
                    'vname' => 'LBL_CALLS',
                ),
            'cases' =>
                array(
                    'name' => 'cases',
                    'type' => 'link',
                    'relationship' => 'contacts_cases',
                    'source' => 'non-db',
                    'vname' => 'LBL_CASES',
                ),
            'direct_reports' =>
                array(
                    'name' => 'direct_reports',
                    'type' => 'link',
                    'relationship' => 'contact_direct_reports',
                    'source' => 'non-db',
                    'vname' => 'LBL_DIRECT_REPORTS',
                ),
            'emails' =>
                array(
                    'name' => 'emails',
                    'type' => 'link',
                    'relationship' => 'emails_contacts_rel',
                    'source' => 'non-db',
                    'vname' => 'LBL_EMAILS',
                ),
            'documents' =>
                array(
                    'name' => 'documents',
                    'type' => 'link',
                    'relationship' => 'documents_contacts',
                    'source' => 'non-db',
                    'vname' => 'LBL_DOCUMENTS_SUBPANEL_TITLE',
                ),
            'leads' =>
                array(
                    'name' => 'leads',
                    'type' => 'link',
                    'relationship' => 'contact_leads',
                    'source' => 'non-db',
                    'vname' => 'LBL_LEADS',
                ),
            'meetings' =>
                array(
                    'name' => 'meetings',
                    'type' => 'link',
                    'relationship' => 'meetings_contacts',
                    'source' => 'non-db',
                    'vname' => 'LBL_MEETINGS',
                ),
            'notes' =>
                array(
                    'name' => 'notes',
                    'type' => 'link',
                    'relationship' => 'contact_notes',
                    'source' => 'non-db',
                    'vname' => 'LBL_NOTES',
                ),
            'project' =>
                array(
                    'name' => 'project',
                    'type' => 'link',
                    'relationship' => 'projects_contacts',
                    'source' => 'non-db',
                    'vname' => 'LBL_PROJECTS',
                ),

            'am_projecttemplates_contacts_1' =>
                array(
                    'name' => 'am_projecttemplates_contacts_1',
                    'type' => 'link',
                    'relationship' => 'am_projecttemplates_contacts_1',
                    'source' => 'non-db',
                    'module' => 'AM_ProjectTemplates',
                    'bean_name' => 'AM_ProjectTemplates',
                    'vname' => 'LBL_PROJECT_CONTACTS_1_FROM_PROJECT_TITLE',
                ),

            'tasks' =>
                array(
                    'name' => 'tasks',
                    'type' => 'link',
                    'relationship' => 'contact_tasks',
                    'source' => 'non-db',
                    'vname' => 'LBL_TASKS',
                ),
            'tasks_parent' => array(
                'name' => 'tasks_parent',
                'type' => 'link',
                'relationship' => 'contact_tasks_parent',
                'source' => 'non-db',
                'vname' => 'LBL_TASKS',
                'reportable' => false
            ),
            'notes_parent' => array(
                'name' => 'notes_parent',
                'type' => 'link',
                'relationship' => 'contact_notes_parent',
                'source' => 'non-db',
                'vname' => 'LBL_TASKS',
                'reportable' => false
            ),
            'user_sync' =>
                array(
                    'name' => 'user_sync',
                    'type' => 'link',
                    'relationship' => 'contacts_users',
                    'source' => 'non-db',
                    'vname' => 'LBL_USER_SYNC',
                ),
            'created_by_link' =>
                array(
                    'name' => 'created_by_link',
                    'type' => 'link',
                    'relationship' => 'contacts_created_by',
                    'vname' => 'LBL_CREATED_BY_USER',
                    'link_type' => 'one',
                    'module' => 'Users',
                    'bean_name' => 'User',
                    'source' => 'non-db',
                ),
            'modified_user_link' =>
                array(
                    'name' => 'modified_user_link',
                    'type' => 'link',
                    'relationship' => 'contacts_modified_user',
                    'vname' => 'LBL_MODIFIED_BY_USER',
                    'link_type' => 'one',
                    'module' => 'Users',
                    'bean_name' => 'User',
                    'source' => 'non-db',
                ),
            'assigned_user_link' =>
                array(
                    'name' => 'assigned_user_link',
                    'type' => 'link',
                    'relationship' => 'contacts_assigned_user',
                    'vname' => 'LBL_ASSIGNED_TO_USER',
                    'link_type' => 'one',
                    'module' => 'Users',
                    'bean_name' => 'User',
                    'source' => 'non-db',
                    'rname' => 'user_name',
                    'id_name' => 'assigned_user_id',
                    'table' => 'users',
                    'duplicate_merge' => 'enabled'
                ),
            'campaign_id' =>
                array(
                    'name' => 'campaign_id',
                    'comment' => 'Campaign that generated lead',
                    'vname' => 'LBL_CAMPAIGN_ID',
                    'rname' => 'id',
                    'id_name' => 'campaign_id',
                    'type' => 'id',
                    //'dbType' => 'char',
                    'table' => 'campaigns',
                    'isnull' => 'true',
                    'module' => 'Campaigns',
//            'reportable'=>false,
                    'massupdate' => false,
                    'duplicate_merge' => 'disabled',
                ),

            'campaign_name' =>
                array(
                    'name' => 'campaign_name',
                    'rname' => 'name',
                    'vname' => 'LBL_CAMPAIGN',
                    'type' => 'relate',
                    'link' => 'campaign_contacts',
                    'isnull' => 'true',
                    'reportable' => false,
                    'source' => 'non-db',
                    'table' => 'campaigns',
                    'id_name' => 'campaign_id',
                    'module' => 'Campaigns',
                    'duplicate_merge' => 'disabled',
                    'comment' => 'The first campaign name for Contact (Meta-data only)',
                ),

            'campaigns' =>
                array(
                    'name' => 'campaigns',
                    'type' => 'link',
                    'relationship' => 'contact_campaign_log',
                    'module' => 'CampaignLog',
                    'bean_name' => 'CampaignLog',
                    'source' => 'non-db',
                    'vname' => 'LBL_CAMPAIGNLOG',
                ),

            'campaign_contacts' =>
                array(
                    'name' => 'campaign_contacts',
                    'type' => 'link',
                    'vname' => 'LBL_CAMPAIGN_CONTACT',
                    'relationship' => 'campaign_contacts',
                    'source' => 'non-db',
                ),
            'c_accept_status_fields' =>
                array(
                    'name' => 'c_accept_status_fields',
                    'rname' => 'id',
                    'relationship_fields' => array('id' => 'accept_status_id', 'accept_status' => 'accept_status_name'),
                    'vname' => 'LBL_LIST_ACCEPT_STATUS',
                    'type' => 'relate',
                    'link' => 'calls',
                    'link_type' => 'relationship_info',
                    'source' => 'non-db',
                    'importable' => 'false',
                    'duplicate_merge' => 'disabled',
                    'studio' => false,
                ),
            'm_accept_status_fields' =>
                array(
                    'name' => 'm_accept_status_fields',
                    'rname' => 'id',
                    'relationship_fields' => array('id' => 'accept_status_id', 'accept_status' => 'accept_status_name'),
                    'vname' => 'LBL_LIST_ACCEPT_STATUS',
                    'type' => 'relate',
                    'link' => 'meetings',
                    'link_type' => 'relationship_info',
                    'source' => 'non-db',
                    'importable' => 'false',
                    'hideacl' => true,
                    'duplicate_merge' => 'disabled',
                    'studio' => false,
                ),
            'accept_status_id' =>
                array(
                    'name' => 'accept_status_id',
                    'type' => 'varchar',
                    'source' => 'non-db',
                    'vname' => 'LBL_LIST_ACCEPT_STATUS',
                    'studio' => array('listview' => false),
                ),
            'accept_status_name' =>
                array(
                    'massupdate' => false,
                    'name' => 'accept_status_name',
                    'type' => 'enum',
                    'studio' => 'false',
                    'source' => 'non-db',
                    'vname' => 'LBL_LIST_ACCEPT_STATUS',
                    'options' => 'dom_meeting_accept_status',
                    'importable' => 'false',
                ),
            'prospect_lists' =>
                array(
                    'name' => 'prospect_lists',
                    'type' => 'link',
                    'relationship' => 'prospect_list_contacts',
                    'module' => 'ProspectLists',
                    'source' => 'non-db',
                    'vname' => 'LBL_PROSPECT_LIST',
                ),
            'sync_contact' =>
                array(
                    'massupdate' => false,
                    'name' => 'sync_contact',
                    'vname' => 'LBL_SYNC_CONTACT',
                    'type' => 'bool',
                    'source' => 'non-db',
                    'comment' => 'Synch to outlook?  (Meta-Data only)',
                    'studio' => 'true',
                ),
            'fp_events_contacts' =>
                array(
                    'name' => 'fp_events_contacts',
                    'type' => 'link',
                    'relationship' => 'fp_events_contacts',
                    'source' => 'non-db',
                    'vname' => 'LBL_FP_EVENTS_CONTACTS_FROM_FP_EVENTS_TITLE',
                ),
            'aos_quotes' =>
                array(
                    'name' => 'aos_quotes',
                    'vname' => 'LBL_AOS_QUOTES',
                    'type' => 'link',
                    'relationship' => 'contact_aos_quotes',
                    'module' => 'AOS_Quotes',
                    'bean_name' => 'AOS_Quotes',
                    'source' => 'non-db',
                ),
            'aos_invoices' =>
                array(
                    'name' => 'aos_invoices',
                    'vname' => 'LBL_AOS_INVOICES',
                    'type' => 'link',
                    'relationship' => 'contact_aos_invoices',
                    'module' => 'AOS_Invoices',
                    'bean_name' => 'AOS_Invoices',
                    'source' => 'non-db',
                ),
            'aos_contracts' =>
                array(
                    'name' => 'aos_contracts',
                    'vname' => 'LBL_AOS_CONTRACTS',
                    'type' => 'link',
                    'relationship' => 'contact_aos_contracts',
                    'module' => 'AOS_Contracts',
                    'bean_name' => 'AOS_Contracts',
                    'source' => 'non-db',
                ),
            'e_invite_status_fields' =>
                array(
                    'name' => 'e_invite_status_fields',
                    'rname' => 'id',
                    'relationship_fields' =>
                        array(
                            'id' => 'event_invite_id',
                            'invite_status' => 'event_status_name',
                        ),
                    'vname' => 'LBL_CONT_INVITE_STATUS',
                    'type' => 'relate',
                    'link' => 'fp_events_contacts',
                    'link_type' => 'relationship_info',
                    'join_link_name' => 'fp_events_contacts',
                    'source' => 'non-db',
                    'importable' => 'false',
                    'duplicate_merge' => 'disabled',
                    'studio' => false,
                ),
            'event_status_name' =>
                array(
                    'massupdate' => false,
                    'name' => 'event_status_name',
                    'type' => 'enum',
                    'studio' => 'false',
                    'source' => 'non-db',
                    'vname' => 'LBL_LIST_INVITE_STATUS_EVENT',
                    'options' => 'fp_event_invite_status_dom',
                    'importable' => 'false',
                ),
            'event_invite_id' =>
                array(
                    'name' => 'event_invite_id',
                    'type' => 'varchar',
                    'source' => 'non-db',
                    'vname' => 'LBL_LIST_INVITE_STATUS',
                    'studio' =>
                        array(
                            'listview' => false,
                        ),
                ),
            'e_accept_status_fields' =>
                array(
                    'name' => 'e_accept_status_fields',
                    'rname' => 'id',
                    'relationship_fields' =>
                        array(
                            'id' => 'event_status_id',
                            'accept_status' => 'event_accept_status',
                        ),
                    'vname' => 'LBL_CONT_ACCEPT_STATUS',
                    'type' => 'relate',
                    'link' => 'fp_events_contacts',
                    'link_type' => 'relationship_info',
                    'join_link_name' => 'fp_events_contacts',
                    'source' => 'non-db',
                    'importable' => 'false',
                    'duplicate_merge' => 'disabled',
                    'studio' => false,
                ),
            'event_accept_status' =>
                array(
                    'massupdate' => false,
                    'name' => 'event_accept_status',
                    'type' => 'enum',
                    'studio' => 'false',
                    'source' => 'non-db',
                    'vname' => 'LBL_LIST_ACCEPT_STATUS_EVENT',
                    'options' => 'fp_event_status_dom',
                    'importable' => 'false',
                ),
            'event_status_id' =>
                array(
                    'name' => 'event_status_id',
                    'type' => 'varchar',
                    'source' => 'non-db',
                    'vname' => 'LBL_LIST_ACCEPT_STATUS',
                    'studio' =>
                        array(
                            'listview' => false,
                        ),
                ),
            'project_contacts_1' =>
                array(
                    'name' => 'project_contacts_1',
                    'type' => 'link',
                    'relationship' => 'project_contacts_1',
                    'source' => 'non-db',
                    'module' => 'Project',
                    'bean_name' => 'Project',
                    'vname' => 'LBL_PROJECT_CONTACTS_1_FROM_PROJECT_TITLE',
                ),
            'aop_case_updates' =>
                array(
                    'name' => 'aop_case_updates',
                    'type' => 'link',
                    'relationship' => 'contacts_aop_case_updates',
                    'source' => 'non-db',
                    'id_name' => 'contact_id',
                    'vname' => 'LBL_AOP_CASE_UPDATES',
                ),
            'joomla_account_id' =>
                array(
                    'name' => 'joomla_account_id',
                    'vname' => 'LBL_JOOMLA_ACCOUNT_ID',
                    'type' => 'varchar',
                    'len' => '255',
                    'importable' => 'false',
                    'studio' => 'true',
                ),
            'portal_account_disabled' =>
                array(
                    'name' => 'portal_account_disabled',
                    'vname' => 'LBL_PORTAL_ACCOUNT_DISABLED',
                    'type' => 'bool',
                    'importable' => 'false',
                    'studio' => 'false',
                ),
            'joomla_account_access' =>
                array(
                    'name' => 'joomla_account_access',
                    'vname' => 'LBL_JOOMLA_ACCOUNT_ACCESS',
                    'type' => 'varchar',
                    'source' => 'non-db',
                    'len' => '255',
                    'importable' => 'false',
                    'studio' => 'false',
                ),
            'portal_user_type' =>
                array(
                    'name' => 'portal_user_type',
                    'vname' => 'LBL_PORTAL_USER_TYPE',
                    'type' => 'enum',
                    'options' => 'contact_portal_user_type_dom',
                    'len' => '100',
                    'default' => 'Single',
                ),
        ),
    'indices' => array(
        array(
            'name' => 'idx_cont_last_first',
            'type' => 'index',
            'fields' => array('last_name', 'first_name', 'deleted')
        ),
        array(
            'name' => 'idx_contacts_del_last',
            'type' => 'index',
            'fields' => array('deleted', 'last_name'),
        ),
        array(
            'name' => 'idx_cont_del_reports',
            'type' => 'index',
            'fields' => array('deleted', 'reports_to_id', 'last_name')
        ),
        array(
            'name' => 'idx_reports_to_id',
            'type' => 'index',
            'fields' => array('reports_to_id'),
        ),
        array(
            'name' => 'idx_del_id_user',
            'type' => 'index',
            'fields' => array('deleted', 'id', 'assigned_user_id'),
        ),
        array(
            'name' => 'idx_cont_assigned',
            'type' => 'index',
            'fields' => array('assigned_user_id')
        ),
    ),
    'relationships' => array(
        'contact_direct_reports' => array('lhs_module' => 'Contacts',
            'lhs_table' => 'contacts',
            'lhs_key' => 'id',
            'rhs_module' => 'Contacts',
            'rhs_table' => 'contacts',
            'rhs_key' => 'reports_to_id',
            'relationship_type' => 'one-to-many'),
        'contact_leads' => array('lhs_module' => 'Contacts',
            'lhs_table' => 'contacts',
            'lhs_key' => 'id',
            'rhs_module' => 'Leads',
            'rhs_table' => 'leads',
            'rhs_key' => 'contact_id',
            'relationship_type' => 'one-to-many'),
        'contact_notes' => array('lhs_module' => 'Contacts',
            'lhs_table' => 'contacts',
            'lhs_key' => 'id',
            'rhs_module' => 'Notes',
            'rhs_table' => 'notes',
            'rhs_key' => 'contact_id',
            'relationship_type' => 'one-to-many'),
        'contact_tasks' => array('lhs_module' => 'Contacts',
            'lhs_table' => 'contacts',
            'lhs_key' => 'id',
            'rhs_module' => 'Tasks',
            'rhs_table' => 'tasks',
            'rhs_key' => 'contact_id',
            'relationship_type' => 'one-to-many'),
        'contact_tasks_parent' => array('lhs_module' => 'Contacts',
            'lhs_table' => 'contacts',
            'lhs_key' => 'id',
            'rhs_module' => 'Tasks',
            'rhs_table' => 'tasks',
            'rhs_key' => 'parent_id',
            'relationship_type' => 'one-to-many',
            'relationship_role_column' => 'parent_type',
            'relationship_role_column_value' => 'Contacts'
        ),
        'contact_notes_parent' => array('lhs_module' => 'Contacts',
            'lhs_table' => 'contacts',
            'lhs_key' => 'id',
            'rhs_module' => 'Notes',
            'rhs_table' => 'notes',
            'rhs_key' => 'parent_id',
            'relationship_type' => 'one-to-many',
            'relationship_role_column' => 'parent_type',
            'relationship_role_column_value' => 'Contacts'
        ),
        'contacts_assigned_user' => array('lhs_module' => 'Users',
            'lhs_table' => 'users',
            'lhs_key' => 'id',
            'rhs_module' => 'Contacts',
            'rhs_table' => 'contacts',
            'rhs_key' => 'assigned_user_id',
            'relationship_type' => 'one-to-many'),
        'contacts_modified_user' => array('lhs_module' => 'Users',
            'lhs_table' => 'users',
            'lhs_key' => 'id',
            'rhs_module' => 'Contacts',
            'rhs_table' => 'contacts',
            'rhs_key' => 'modified_user_id',
            'relationship_type' => 'one-to-many'),
        'contacts_created_by' => array('lhs_module' => 'Users',
            'lhs_table' => 'users',
            'lhs_key' => 'id',
            'rhs_module' => 'Contacts',
            'rhs_table' => 'contacts',
            'rhs_key' => 'created_by',
            'relationship_type' => 'one-to-many'),
        'contact_campaign_log' => array(
            'lhs_module' => 'Contacts',
            'lhs_table' => 'contacts',
            'lhs_key' => 'id',
            'rhs_module' => 'CampaignLog',
            'rhs_table' => 'campaign_log',
            'rhs_key' => 'target_id',
            'relationship_type' => 'one-to-many',
            'relationship_role_column' => 'target_type',
            'relationship_role_column_value' => 'Contacts'
        ),
        'contact_aos_quotes' =>
            array(
                'lhs_module' => 'Contacts',
                'lhs_table' => 'contacts',
                'lhs_key' => 'id',
                'rhs_module' => 'AOS_Quotes',
                'rhs_table' => 'aos_quotes',
                'rhs_key' => 'billing_contact_id',
                'relationship_type' => 'one-to-many',
            ),
        'contact_aos_invoices' =>
            array(
                'lhs_module' => 'Contacts',
                'lhs_table' => 'contacts',
                'lhs_key' => 'id',
                'rhs_module' => 'AOS_Invoices',
                'rhs_table' => 'aos_invoices',
                'rhs_key' => 'billing_contact_id',
                'relationship_type' => 'one-to-many',
            ),
        'contact_aos_contracts' =>
            array(
                'lhs_module' => 'Contacts',
                'lhs_table' => 'contacts',
                'lhs_key' => 'id',
                'rhs_module' => 'AOS_Contracts',
                'rhs_table' => 'aos_contracts',
                'rhs_key' => 'contact_id',
                'relationship_type' => 'one-to-many',
            ),
        'contacts_aop_case_updates' =>
            array(
                'lhs_module' => 'Contacts',
                'lhs_table' => 'contacts',
                'lhs_key' => 'id',
                'rhs_module' => 'AOP_Case_Updates',
                'rhs_table' => 'aop_case_updates',
                'rhs_key' => 'contact_id',
                'relationship_type' => 'one-to-many',
            ),
    ),

//This enables optimistic locking for Saves From EditView
    'optimistic_locking' => true,
);

VardefManager::createVardef('Contacts', 'Contact', array('default', 'assignable', 'security_groups',
    'person'));
