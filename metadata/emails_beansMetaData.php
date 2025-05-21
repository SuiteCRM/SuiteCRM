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



/**
 * Relationship table linking emails with 1 or more SugarBeans
 */
$dictionary['emails_beans'] = array('table' => 'emails_beans',
    'fields' => array(
        array(
            'name'		=> 'id',
            'type'		=> 'varchar',
            'dbType'	=> 'id',
            'len'		=> '36'
        ),
        array(
            'name'		=> 'email_id',
            'type'		=> 'varchar',
            'dbType'	=> 'id',
            'len'		=> '36',
            'comment' 	=> 'FK to emails table',
        ),
        array(
            'name'		=> 'bean_id',
            'dbType'	=> 'id',
            'type'		=> 'varchar',
            'len'		=> '36',
            'comment' 	=> 'FK to various beans\'s tables',
        ),
        array(
            'name'		=> 'bean_module',
            'type'		=> 'varchar',
            'len'		=> '100',
            'comment' 	=> 'bean\'s Module',
        ),
        array(  'name'      => 'campaign_data',
                'type'      => 'text',
        ),
        array(
            'name'		=> 'date_modified',
            'type'		=>	'datetime'
        ),
        array(
            'name'		=> 'deleted',
            'type'		=> 'bool',
            'len'		=> '1',
            'default'	=> '0',
            'required'	=> false
        )
    ),
    'indices' => array(
        array(
            'name'		=> 'emails_beanspk',
            'type'		=> 'primary',
            'fields'	=> array('id')
        ),
        array(
            'name'		=> 'idx_emails_beans_bean_id',
            'type'		=> 'index',
            'fields'	=> array('bean_id')
        ),
        array(
            'name'		=> 'idx_emails_beans_email_bean',
            'type'		=> 'alternate_key',
            'fields'	=> array('email_id', 'bean_id', 'deleted')
        ),
    ),
    'relationships' => array(
        'emails_accounts_rel' => array(
            'lhs_module'		=> 'Emails',
            'lhs_table'			=> 'emails',
            'lhs_key'			=> 'id',
            'rhs_module'		=> 'Accounts',
            'rhs_table'			=> 'accounts',
            'rhs_key'			=> 'id',
            'relationship_type'	=> 'many-to-many',
            'join_table'		=> 'emails_beans',
            'join_key_lhs'		=> 'email_id',
            'join_key_rhs'		=> 'bean_id',
            'relationship_role_column' => 'bean_module',
            'relationship_role_column_value' => 'Accounts',
        ),
        'emails_bugs_rel' => array(
            'lhs_module'		=> 'Emails',
            'lhs_table'			=> 'emails',
            'lhs_key'			=> 'id',
            'rhs_module'		=> 'Bugs',
            'rhs_table'			=> 'bugs',
            'rhs_key'			=> 'id',
            'relationship_type'	=> 'many-to-many',
            'join_table'		=> 'emails_beans',
            'join_key_lhs'		=> 'email_id',
            'join_key_rhs'		=> 'bean_id',
            'relationship_role_column' => 'bean_module',
            'relationship_role_column_value' => 'Bugs',
        ),
        'emails_cases_rel' => array(
            'lhs_module'		=> 'Emails',
            'lhs_table'			=> 'emails',
            'lhs_key'			=> 'id',
            'rhs_module'		=> 'Cases',
            'rhs_table'			=> 'cases',
            'rhs_key'			=> 'id',
            'relationship_type'	=> 'many-to-many',
            'join_table'		=> 'emails_beans',
            'join_key_lhs'		=> 'email_id',
            'join_key_rhs'		=> 'bean_id',
            'relationship_role_column' => 'bean_module',
            'relationship_role_column_value' => 'Cases',
        ),
        'emails_contacts_rel'	=> array(
            'lhs_module'		=> 'Emails',
            'lhs_table'			=> 'emails',
            'lhs_key'			=> 'id',
            'rhs_module'		=> 'Contacts',
            'rhs_table'			=> 'contacts',
            'rhs_key'			=> 'id',
            'relationship_type'	=> 'many-to-many',
            'relationship_role_column' => 'bean_module',
            'relationship_role_column_value' => 'Contacts',
            'join_table'		=> 'emails_beans',
            'join_key_lhs'		=> 'email_id',
            'join_key_rhs'		=> 'bean_id',
        ),
        'emails_leads_rel' => array(
            'lhs_module'		=> 'Emails',
            'lhs_table'			=> 'emails',
            'lhs_key'			=> 'id',
            'rhs_module'		=> 'Leads',
            'rhs_table'			=> 'leads',
            'rhs_key'			=> 'id',
            'relationship_type'	=> 'many-to-many',
            'join_table'		=> 'emails_beans',
            'join_key_lhs'		=> 'email_id',
            'join_key_rhs'		=> 'bean_id',
            'relationship_role_column' => 'bean_module',
            'relationship_role_column_value' => 'Leads',
        ),
        'emails_opportunities_rel' => array(
            'lhs_module'		=> 'Emails',
            'lhs_table'			=> 'emails',
            'lhs_key'			=> 'id',
            'rhs_module'		=> 'Opportunities',
            'rhs_table'			=> 'opportunities',
            'rhs_key'			=> 'id',
            'relationship_type'	=> 'many-to-many',
            'join_table'		=> 'emails_beans',
            'join_key_lhs'		=> 'email_id',
            'join_key_rhs'		=> 'bean_id',
            'relationship_role_column' => 'bean_module',
            'relationship_role_column_value' => 'Opportunities',
        ),
        'emails_tasks_rel' => array(
            'lhs_module'		=> 'Emails',
            'lhs_table'			=> 'emails',
            'lhs_key'			=> 'id',
            'rhs_module'		=> 'Tasks',
            'rhs_table'			=> 'tasks',
            'rhs_key'			=> 'id',
            'relationship_type'	=> 'many-to-many',
            'join_table'		=> 'emails_beans',
            'join_key_lhs'		=> 'email_id',
            'join_key_rhs'		=> 'bean_id',
            'relationship_role_column' => 'bean_module',
            'relationship_role_column_value' => 'Tasks',
        ),
        'emails_users_rel' => array(
            'lhs_module'		=> 'Emails',
            'lhs_table'			=> 'emails',
            'lhs_key'			=> 'id',
            'rhs_module'		=> 'Users',
            'rhs_table'			=> 'users',
            'rhs_key'			=> 'id',
            'relationship_type'	=> 'many-to-many',
            'join_table'		=> 'emails_beans',
            'join_key_lhs'		=> 'email_id',
            'join_key_rhs'		=> 'bean_id',
            'relationship_role_column' => 'bean_module',
            'relationship_role_column_value' => 'Users',
        ),
        'emails_project_task_rel' => array(
            'lhs_module'		=> 'Emails',
            'lhs_table'			=> 'emails',
            'lhs_key'			=> 'id',
            'rhs_module'		=> 'ProjectTask',
            'rhs_table'			=> 'project_task',
            'rhs_key'			=> 'id',
            'relationship_type'	=> 'many-to-many',
            'join_table'		=> 'emails_beans',
            'join_key_lhs'		=> 'email_id',
            'join_key_rhs'		=> 'bean_id',
            'relationship_role_column' => 'bean_module',
            'relationship_role_column_value' => 'ProjectTask',
        ),
        'emails_projects_rel' => array(
            'lhs_module'		=> 'Emails',
            'lhs_table'			=> 'emails',
            'lhs_key'			=> 'id',
            'rhs_module'		=> 'Project',
            'rhs_table'			=> 'project',
            'rhs_key'			=> 'id',
            'relationship_type'	=> 'many-to-many',
            'join_table'		=> 'emails_beans',
            'join_key_lhs'		=> 'email_id',
            'join_key_rhs'		=> 'bean_id',
            'relationship_role_column' => 'bean_module',
            'relationship_role_column_value' => 'Project',
        ),
        'emails_prospects_rel' => array(
            'lhs_module'		=> 'Emails',
            'lhs_table'			=> 'emails',
            'lhs_key'			=> 'id',
            'rhs_module'		=> 'Prospects',
            'rhs_table'			=> 'prospects',
            'rhs_key'			=> 'id',
            'relationship_type'	=> 'many-to-many',
            'join_table'		=> 'emails_beans',
            'join_key_lhs'		=> 'email_id',
            'join_key_rhs'		=> 'bean_id',
            'relationship_role_column' => 'bean_module',
            'relationship_role_column_value' => 'Prospects',
        ),
        'emails_quotes' => array(
            'lhs_module'		=> 'Emails',
            'lhs_table'			=> 'emails',
            'lhs_key'			=> 'id',
            'rhs_module'		=> 'Quotes',
            'rhs_table'			=> 'quotes',
            'rhs_key'			=> 'id',
            'relationship_type'	=> 'many-to-many',
            'join_table'		=> 'emails_beans',
            'join_key_lhs'		=> 'email_id',
            'join_key_rhs'		=> 'bean_id',
            'relationship_role_column' => 'bean_module',
            'relationship_role_column_value' => 'Quotes',
        ),
    )
);


/**
 * Large text field table, shares a 1:1 with the emails table.  Moving all longtext fields to this table allows more
 * effiencient email management and full-text search capabilities.
 */
$dictionary['emails_text'] = array(
    'table' => 'emails_text',
    'comment' => 'Large email text fields',
    // email text using InnoDB as default. Add following code to use MyISAM:
//    'mysqlengine' => 'MyISAM',
//    'engine' => 'MyISAM',
    'fields' => array(
        'email_id' => array(
            'name'			=> 'email_id',
            'vname'			=> 'LBL_ID',
            'type'			=> 'id',
            'dbType'		=> 'id',
            'len'			=> 36,
            'required'		=> true,
            'reportable'	=> true,
            'comment' 		=> 'Foriegn key to emails table',
        ),
        'from_addr' => array(
            'name'			=> 'from_addr',
            'vname'			=> 'LBL_FROM',
            'type'			=> 'varchar',
            'len'			=> 255,
            'comment'		=> 'Email address of person who send the email',
        ),
        'reply_to_addr' => array(
            'name'			=> 'reply_to_addr',
            'vname'			=> 'LBL_REPLY_TO',
            'type'			=> 'varchar',
            'len'			=> 255,
            'comment'		=> 'reply to email address',
        ),
        'to_addrs' => array(
            'name'			=> 'to_addrs',
            'vname'			=> 'LBL_TO',
            'type'			=> 'text',
            'comment'		=> 'Email address(es) of person(s) to receive the email',
        ),
        'cc_addrs' => array(
            'name'			=> 'cc_addrs',
            'vname'			=> 'LBL_CC',
            'type'			=> 'text',
            'comment'		=> 'Email address(es) of person(s) to receive a carbon copy of the email',
        ),
        'bcc_addrs' => array(
            'name'			=> 'bcc_addrs',
            'vname'			=> 'LBL_BCC',
            'type'			=> 'text',
            'comment'		=> 'Email address(es) of person(s) to receive a blind carbon copy of the email',
        ),
        'description' => array(
            'name'			=> 'description',
            'vname'			=> 'LBL_TEXT_BODY',
            'type'			=> 'longtext',
            'reportable'	=> false,
            'comment'		=> 'Email body in plain text',
        ),
        'description_html' => array(
            'name'			=> 'description_html',
            'vname'			=> 'LBL_HTML_BODY',
            'type'			=> 'longhtml',
            'reportable'	=> false,
            'comment'		=> 'Email body in HTML format',
        ),
        'raw_source' => array(
            'name'			=> 'raw_source',
            'vname'			=> 'LBL_RAW',
            'type'			=> 'longtext',
            'reportable'	=> false,
            'comment'		=> 'Full raw source of email',
        ),
        'deleted' => array(
            'name'			=> 'deleted',
            'type'			=> 'bool',
            'default'		=> 0,
        ),
    ),
    'indices' => array(
        array(
            'name'			=> 'emails_textpk',
            'type'			=> 'primary',
            'fields'		=> array('email_id')
        ),
        array(
            'name'			=> 'emails_textfromaddr',
            'type'			=> 'index',
            'fields'		=> array('from_addr')
        ),
    ),
);
