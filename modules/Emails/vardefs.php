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

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

$dictionary['Email'] = array(
    'table' => 'emails',
    'acl_fields' => false,
    'comment' => 'Contains a record of emails sent to and from the Sugar application',
    'fields' => array(
        'orphaned' => array(
            'name' => 'orphaned',
            'vname' => 'LBL_ORPHANED',
            'type' => 'bool',
            'required' => false,
            'reportable' => false,
            'comment' => 'Emails which exists in the SuiteCRM but have been deleted from the email server',
            'inline_edit' => false,
        ),
        'last_synced' => array(
            'name' => 'last_synced',
            'vname' => 'LBL_LAST_SYNCED',
            'type' => 'datetime',
            'comment' => 'The last date and time the email was synced with the server',
            'inline_edit' => false,
            'required' => false,
            'reportable' => false,
        ),
        'from_addr_name' => array(
            'name' => 'from_addr_name',
            'type' => 'varchar',
            'source' => 'non-db',
            'inline_edit' => false,
        ),

        'reply_to_addr' => array(
            'name' => 'reply_to_addr',
            'type' => 'varchar',
            'vname' => 'reply_to_addr',
            'source' => 'non-db',
            'inline_edit' => false,
        ),

        'to_addrs_names' => array(
            'name' => 'to_addrs_names',
            'type' => 'varchar',
            'vname' => 'to_addrs_names',
            'source' => 'non-db',
            'inline_edit' => false,
        ),
        'cc_addrs_names' => array(
            'name' => 'cc_addrs_names',
            'type' => 'varchar',
            'vname' => 'cc_addrs_names',
            'source' => 'non-db',
            'inline_edit' => false,
        ),
        'bcc_addrs_names' => array(
            'name' => 'bcc_addrs_names',
            'type' => 'varchar',
            'vname' => 'bcc_addrs_names',
            'source' => 'non-db',
            'inline_edit' => false,
        ),
        'imap_keywords' => array(
            'name' => 'imap_keywords',
            'type' => 'varchar',
            'vname' => 'LBL_IMAP_KEYWORDS',
            'source' => 'non-db',
            'inline_edit' => false,
        ),

        'raw_source' => array(
            'name' => 'raw_source',
            'type' => 'varchar',
            'vname' => 'raw_source',
            'source' => 'non-db',
            'inline_edit' => false,
        ),
        'description_html' => array(
            'name' => 'description_html',
            'type' => 'emailbody',
            'vname' => 'description_html',
            'source' => 'non-db',
            'inline_edit' => false,
        ),
        'description' => array(
            'name' => 'description',
            'type' => 'text',
            'vname' => 'description',
            'source' => 'non-db',
            'inline_edit' => false,
            'rows' => 6,
            'cols' => 80,

        ),
        'date_sent_received' => array(
            'name' => 'date_sent_received',
            'vname' => 'LBL_DATE_SENT_RECEIVED',
            'type' => 'datetime',
            'inline_edit' => false,
        ),
        'message_id' => array(
            'name' => 'message_id',
            'vname' => 'LBL_MESSAGE_ID',
            'type' => 'varchar',
            'len' => 255,
            'comment' => 'ID of the email item obtained from the email transport system',
            'inline_edit' => false,

        ),

        'name' => array(
            'name' => 'name',
            'vname' => 'LBL_SUBJECT',
            'type' => 'name',
            'dbType' => 'varchar',
            'required' => false,
            'len' => '255',
            'comment' => 'The subject of the email',
            'inline_edit' => false,

        ),
        'type' => array(
            'name' => 'type',
            'vname' => 'LBL_LIST_TYPE',
            'type' => 'enum',
            'options' => 'dom_email_types',
            'len' => 100,
            'massupdate' => false,
            'comment' => 'Type of email (ex: draft)',
            'inline_edit' => false,

        ),
        'status' => array(
            'name' => 'status',
            'vname' => 'LBL_STATUS',
            'type' => 'enum',
            'len' => 100,
            'options' => 'dom_email_status',
            'inline_edit' => false,

        ),
        'flagged' => array(
            'name' => 'flagged',
            'vname' => 'LBL_EMAIL_FLAGGED',
            'type' => 'bool',
            'required' => false,
            'reportable' => false,
            'comment' => 'flagged status',
            'inline_edit' => false,

        ),
        'reply_to_status' => array(
            'name' => 'reply_to_status',
            'vname' => 'LBL_EMAIL_REPLY_TO_STATUS',
            'type' => 'bool',
            'required' => false,
            'reportable' => false,
            'comment' => 'I you reply to an email then reply to status of original email is set',
            'inline_edit' => false,

        ),
        'intent' => array(
            'name' => 'intent',
            'vname' => 'LBL_INTENT',
            'type' => 'varchar',
            'len' => 100,
            'default' => 'pick',
            'comment' => 'Target of action used in Inbound Email assignment',
            'inline_edit' => false,

        ),
        'mailbox_id' => array(
            'name' => 'mailbox_id',
            'vname' => 'LBL_MAILBOX_ID',
            'type' => 'id',
            'len' => '36',
            'reportable' => false,
            'inline_edit' => false,

        ),
        'created_by_link' => array(
            'name' => 'created_by_link',
            'type' => 'link',
            'relationship' => 'emails_created_by',
            'vname' => 'LBL_CREATED_BY_USER',
            'link_type' => 'one',
            'module' => 'Users',
            'bean_name' => 'User',
            'source' => 'non-db',
            'inline_edit' => false,

        ),
        'modified_user_link' => array(
            'name' => 'modified_user_link',
            'type' => 'link',
            'relationship' => 'emails_modified_user',
            'vname' => 'LBL_MODIFIED_BY_USER',
            'link_type' => 'one',
            'module' => 'Users',
            'bean_name' => 'User',
            'source' => 'non-db',
            'inline_edit' => false,

        ),
        'assigned_user_link' => array(
            'name' => 'assigned_user_link',
            'type' => 'link',
            'relationship' => 'emails_assigned_user',
            'vname' => 'LBL_ASSIGNED_TO_USER',
            'link_type' => 'one',
            'module' => 'Users',
            'bean_name' => 'User',
            'source' => 'non-db',
            'inline_edit' => false,

        ),

        'parent_name' => array(
            'name'=> 'parent_name',
            'parent_type'=>'record_type_display' ,
            'type_name'=>'parent_type',
            'id_name'=>'parent_id',
            'vname'=>'LBL_EMAIL_RELATE',
            'type'=>'parent',
            'source'=>'non-db',
            'options'=> 'record_type_display',
            'inline_edit' => false,
        ),
        'parent_type' => array(
            'name' => 'parent_type',
            'vname' => 'LBL_PARENT_TYPE',
            'type' => 'varchar',
            'reportable' => false,
            'len' => 100,
            'comment' => 'Identifier of Sugar module to which this email is associated (deprecated as of 4.2)',
            'inline_edit' => false,

        ),
        'parent_id' => array(
            'name' => 'parent_id',
            'vname' => 'LBL_PARENT_ID',
            'type' => 'id',
            'len' => '36',
            'reportable' => false,
            'comment' => 'ID of Sugar object referenced by parent_type (deprecated as of 4.2)',
            'inline_edit' => false,

        ),

        'indicator' => array(
            'name' => 'indicator',
            'vname' => 'LBL_INDICATOR',
            'type' => 'function',
            'source' => 'non-db',
            'massupdate' => 0,
            'importable' => 'false',
            'duplicate_merge' => 'disabled',
            'studio' => 'visible',
            'inline_edit' => false,
            'function' => array(
                'name' => 'displayIndicatorField',
                'returns' => 'html',
                'include' => 'modules/Emails/include/displayIndicatorField.php',
                'onListView' =>  true
            ),
        ),

        'subject' => array(
            'name' => 'subject',
            'vname' => 'LBL_SUBJECT',
            'type' => 'function',
            'source' => 'non-db',
            'massupdate' => 0,
            'importable' => 'false',
            'duplicate_merge' => 'disabled',
            'studio' => 'visible',
            'inline_edit' => false,
            'function' => array(
                'name' => 'displaySubjectField',
                'returns' => 'html',
                'include' => 'modules/Emails/include/displaySubjectField.php',
                'onListView' =>  true
            ),
        ),

        'attachment' => array(
            'name' => 'attachment',
            'vname' => 'LBL_ATTACHMENTS',
            'type' => 'function',
            'source' => 'non-db',
            'massupdate' => 0,
            'importable' => 'false',
            'duplicate_merge' => 'disabled',
            'studio' => 'visible',
            'inline_edit' => false,
            'function' => array(
                'name' => 'displayAttachmentField',
                'returns' => 'html',
                'include' => 'modules/Emails/include/displayAttachmentField.php',
                'onListView' =>  true
            ),
        ),

        'uid' => array(
            'name' => 'uid',
            'type' => 'varchar',
            'massupdate' => 0,
            'importable' => 'false',
            'duplicate_merge' => 'disabled',
            'inline_edit' => false,
        ),


        'msgno' => array(
            'name' => 'msgno',
            'type' => 'varchar',
            'source' => 'non-db',
            'massupdate' => 0,
            'importable' => 'false',
            'duplicate_merge' => 'disabled',
            'inline_edit' => false,
        ),


        'folder' => array(
            'name' => 'folder',
            'type' => 'varchar',
            'source' => 'non-db',
            'massupdate' => 0,
            'importable' => 'false',
            'duplicate_merge' => 'disabled',
            'inline_edit' => false,
        ),

        'folder_type' => array(
            'name' => 'folder_type',
            'type' => 'varchar',
            'source' => 'non-db',
            'massupdate' => 0,
            'importable' => 'false',
            'duplicate_merge' => 'disabled',
            'inline_edit' => false,
        ),
        'inbound_email_record' => array(
            'name' => 'inbound_email_record',
            'type' => 'varchar',
            'source' => 'non-db',
            'massupdate' => 0,
            'importable' => 'false',
            'duplicate_merge' => 'disabled',
            'inline_edit' => false,
        ),

        'is_imported' => array(
            'name' => 'is_imported',
            'type' => 'varchar',
            'source' => 'non-db',
            'massupdate' => 0,
            'importable' => 'false',
            'duplicate_merge' => 'disabled',
            'inline_edit' => false,
        ),


        'has_attachment' => array(
            'name' => 'has_attachment',
            'vname' => 'LBL_HAS_ATTACHMENT_INDICATOR',
            'type' => 'function',
            'source' => 'non-db',
            'massupdate' => 0,
            'importable' => 'false',
            'duplicate_merge' => 'disabled',
            'studio' => 'visible',
            'inline_edit' => false,
            'function' => array(
                'name' => 'displayHasAttachmentField',
                'returns' => 'html',
                'include' => 'modules/Emails/include/displayHasAttachmentField.php',
                'onListView' =>  true
            ),
        ),

        'is_only_plain_text' => array(
            'name' => 'is_only_plain_text',
            'type' => 'bool',
            'default' => false,
            'massupdate' => 0,
            'importable' => 'false',
            'duplicate_merge' => 'disabled',
            'inline_edit' => false,
            'source' => 'non-db',
        ),
        /* relationship collection attributes */
        /* added to support InboundEmail */
        'accounts' => array(
            'name' => 'accounts',
            'vname' => 'LBL_EMAILS_ACCOUNTS_REL',
            'type' => 'link',
            'relationship' => 'emails_accounts_rel',
            'module' => 'Accounts',
            'bean_name' => 'Account',
            'source' => 'non-db',
        ),
        'bugs' => array(
            'name' => 'bugs',
            'vname' => 'LBL_EMAILS_BUGS_REL',
            'type' => 'link',
            'relationship' => 'emails_bugs_rel',
            'module' => 'Bugs',
            'bean_name' => 'Bug',
            'source' => 'non-db',
        ),
        'cases' => array(
            'name' => 'cases',
            'vname' => 'LBL_EMAILS_CASES_REL',
            'type' => 'link',
            'relationship' => 'emails_cases_rel',
            'module' => 'Cases',
            'bean_name' => 'Case',
            'source' => 'non-db',
        ),
        'contacts' => array(
            'name' => 'contacts',
            'vname' => 'LBL_EMAILS_CONTACTS_REL',
            'type' => 'link',
            'relationship' => 'emails_contacts_rel',
            'module' => 'Contacts',
            'bean_name' => 'Contact',
            'source' => 'non-db',
        ),
        'leads' => array(
            'name' => 'leads',
            'vname' => 'LBL_EMAILS_LEADS_REL',
            'type' => 'link',
            'relationship' => 'emails_leads_rel',
            'module' => 'Leads',
            'bean_name' => 'Lead',
            'source' => 'non-db',
        ),
        'opportunities' => array(
            'name' => 'opportunities',
            'vname' => 'LBL_EMAILS_OPPORTUNITIES_REL',
            'type' => 'link',
            'relationship' => 'emails_opportunities_rel',
            'module' => 'Opportunities',
            'bean_name' => 'Opportunity',
            'source' => 'non-db',
        ),
        'project' => array(
            'name' => 'project',
            'vname' => 'LBL_EMAILS_PROJECT_REL',
            'type' => 'link',
            'relationship' => 'emails_projects_rel',
            'module' => 'Project',
            'bean_name' => 'Project',
            'source' => 'non-db',
        ),
        'projecttask' => array(
            'name' => 'projecttask',
            'vname' => 'LBL_EMAILS_PROJECT_TASK_REL',
            'type' => 'link',
            'relationship' => 'emails_project_task_rel',
            'module' => 'ProjectTask',
            'bean_name' => 'ProjectTask',
            'source' => 'non-db',
        ),
        'prospects' => array(
            'name' => 'prospects',
            'vname' => 'LBL_EMAILS_PROSPECT_REL',
            'type' => 'link',
            'relationship' => 'emails_prospects_rel',
            'module' => 'Prospects',
            'bean_name' => 'Prospect',
            'source' => 'non-db',
        ),
        'aos_contracts' => array(
            'name' => 'aos_contracts',
            'vname' => 'LBL_EMAILS_CONTRACTS_REL',
            'type' => 'link',
            'relationship' => 'emails_aos_contracts_rel',
            'module' => 'AOS_Contracts',
            'bean_name' => 'AOS_Contracts',
            'source' => 'non-db',
        ),

        'tasks' => array(
            'name' => 'tasks',
            'vname' => 'LBL_EMAILS_TASKS_REL',
            'type' => 'link',
            'relationship' => 'emails_tasks_rel',
            'module' => 'Tasks',
            'bean_name' => 'Task',
            'source' => 'non-db',
        ),
        'users' => array(
            'name' => 'users',
            'vname' => 'LBL_EMAILS_USERS_REL',
            'type' => 'link',
            'relationship' => 'emails_users_rel',
            'module' => 'Users',
            'bean_name' => 'User',
            'source' => 'non-db',
        ),
        'notes' => array(
            'name' => 'notes',
            'vname' => 'LBL_EMAILS_NOTES_REL',
            'type' => 'link',
            'relationship' => 'emails_notes_rel',
            'module' => 'Notes',
            'bean_name' => 'Note',
            'source' => 'non-db',
        ),
        // SNIP
        'meetings' => array(
            'name' => 'meetings',
            'vname' => 'LBL_EMAILS_MEETINGS_REL',
            'type' => 'link',
            'relationship' => 'emails_meetings_rel',
            'module' => 'Meetings',
            'bean_name' => 'Meeting',
            'source' => 'non-db',
        ),
        /* end relationship collections */

        'category_id' => array(
            'name' => 'category_id',
            'vname' => 'LBL_CATEGORY',
            'type' => 'enum',
            'len' => 100,
            'options' => 'email_category_dom',
            'reportable' => true,
        ),

        "emails_email_templates" => array(
            'name' => 'emails_email_templates',
            'type' => 'link',
            'relationship' => 'emails_email_templates',
            'source' => 'non-db',
            'module' => 'EmailTemplates',
            'bean_name' => 'EmailTemplate',
            'vname' => 'LBL_EMAIL_TEMPLATE',
            'id_name' => 'emails_email_templates_idb',
        ),
        "emails_email_templates_name" => array(
            'name' => 'emails_email_templates_name',
            'type' => 'relate',
            'source' => 'non-db',
            'vname' => 'LBL_EMAIL_TEMPLATE',
            'save' => true,
            'id_name' => 'emails_email_templates_idb',
            'link' => 'emails_email_templates',
            'table' => 'email_templates',
            'module' => 'EmailTemplates',
            'rname' => 'name',
        ),
        "emails_email_templates_idb" => array(
            'name' => 'emails_email_templates_idb',
            'type' => 'link',
            'relationship' => 'emails_email_templates',
            'source' => 'non-db',
            'reportable' => false,
            'side' => 'left',
            'vname' => 'LBL_EMAIL_TEMPLATE',
        ),
        'opt_in' => array(
            'name' => 'opt_in',
            'vname' => 'LBL_OPT_IN',
            'type' => 'function',
            'source' => 'non-db',
            'massupdate' => 0,
            'importable' => 'false',
            'duplicate_merge' => 'disabled',
            'studio' => 'visible',
            'inline_edit' => false,
            'function' => array(
                'name' => 'displayEmailAddressOptInField',
                'returns' => 'html',
                'include' => 'modules/Emails/include/displayEmailAddressOptInField.php',
                'onListView' =>  true
            ),
        ),
    ), /* end fields() array */
    'relationships' => array(
        'emails_assigned_user' => array(
            'lhs_module' => 'Users',
            'lhs_table' => 'users',
            'lhs_key' => 'id',
            'rhs_module' => 'Emails',
            'rhs_table' => 'emails',
            'rhs_key' => 'assigned_user_id',
            'relationship_type' => 'one-to-many',
        ),
        'emails_modified_user' => array(
            'lhs_module' => 'Users',
            'lhs_table' => 'users',
            'lhs_key' => 'id',
            'rhs_module' => 'Emails',
            'rhs_table' => 'emails',
            'rhs_key' => 'modified_user_id',
            'relationship_type' => 'one-to-many',
        ),
        'emails_created_by' => array(
            'lhs_module' => 'Users',
            'lhs_table' => 'users',
            'lhs_key' => 'id',
            'rhs_module' => 'Emails',
            'rhs_table' => 'emails',
            'rhs_key' => 'created_by',
            'relationship_type' => 'one-to-many',
        ),
        'emails_notes_rel' => array(
            'lhs_module' => 'Emails',
            'lhs_table' => 'emails',
            'lhs_key' => 'id',
            'rhs_module' => 'Notes',
            'rhs_table' => 'notes',
            'rhs_key' => 'parent_id',
            'relationship_type' => 'one-to-many',
        ),
        'emails_contacts_rel' => array(
            'lhs_module' => 'Emails',
            'lhs_table' => 'emails',
            'lhs_key' => 'id',
            'rhs_module' => 'Contacts',
            'rhs_table' => 'contacts',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'emails_beans',
            'join_key_lhs' => 'email_id',
            'join_key_rhs' => 'bean_id',
            'relationship_role_column' => 'bean_module',
            'relationship_role_column_value' => 'Contacts',
        ),
        'emails_accounts_rel' => array(
            'lhs_module' => 'Emails',
            'lhs_table' => 'emails',
            'lhs_key' => 'id',
            'rhs_module' => 'Accounts',
            'rhs_table' => 'accounts',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'emails_beans',
            'join_key_lhs' => 'email_id',
            'join_key_rhs' => 'bean_id',
            'relationship_role_column' => 'bean_module',
            'relationship_role_column_value' => 'Accounts',
        ),
        'emails_leads_rel' => array(
            'lhs_module' => 'Emails',
            'lhs_table' => 'emails',
            'lhs_key' => 'id',
            'rhs_module' => 'Leads',
            'rhs_table' => 'leads',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'emails_beans',
            'join_key_lhs' => 'email_id',
            'join_key_rhs' => 'bean_id',
            'relationship_role_column' => 'bean_module',
            'relationship_role_column_value' => 'Leads',
        ),
        'emails_aos_contracts_rel' => array(
            'lhs_module' => 'Emails',
            'lhs_table' => 'emails',
            'lhs_key' => 'id',
            'rhs_module' => 'AOS_Contracts',
            'rhs_table' => 'aos_contracts',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'emails_beans',
            'join_key_lhs' => 'email_id',
            'join_key_rhs' => 'bean_id',
            'relationship_role_column' => 'bean_module',
            'relationship_role_column_value' => 'AOS_Contracts',
        ),
        // SNIP
        'emails_meetings_rel' => array(
            'lhs_module' => 'Emails',
            'lhs_table' => 'emails',
            'lhs_key' => 'id',
            'rhs_module' => 'Meetings',
            'rhs_table' => 'meetings',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'emails_beans',
            'join_key_lhs' => 'email_id',
            'join_key_rhs' => 'bean_id',
            'relationship_role_column' => 'bean_module',
            'relationship_role_column_value' => 'Meetings',
        ),
    ), // end relationships
    'indices' => array(
        array(
            'name' => 'idx_email_name',
            'type' => 'index',
            'fields' => array('name'),
        ),
        array(
            'name' => 'idx_message_id',
            'type' => 'index',
            'fields' => array('message_id'),
        ),
        array(
            'name' => 'idx_email_parent_id',
            'type' => 'index',
            'fields' => array('parent_id'),
        ),
        array(
            'name' => 'idx_email_assigned',
            'type' => 'index',
            'fields' => array('assigned_user_id', 'type', 'status'),
        ),
        array(
            'name' => 'idx_email_cat',
            'type' => 'index',
            'fields' => array('category_id')
        ),
        array(
            'name' => 'idx_email_uid',
            'type' => 'index',
            'fields' => array('uid')
        ),
    ) // end indices
);

VardefManager::createVardef('Emails', 'Email', array('default',
        'basic',
        'assignable','security_groups',
));
