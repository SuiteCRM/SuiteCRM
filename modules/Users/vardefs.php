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

$dictionary['User'] = array(
    'table' => 'users',
    'fields' => array(
        'id' => array(
            'name' => 'id',
            'vname' => 'LBL_ID',
            'type' => 'id',
            'required' => true,
        ),
        'user_name' => array(
            'name' => 'user_name',
            'vname' => 'LBL_USER_NAME',
            'type' => 'user_name',
            'dbType' => 'varchar',
            'len' => '60',
            'importable' => 'required',
            'required' => true,
            'studio' => array(
                'no_duplicate' => true,
                'editview' => false,
                'detailview' => true,
                'quickcreate' => false,
                'basic_search' => false,
                'advanced_search' => false,
            ),
            'api-visible' => false,
        ),
        'user_hash' => array(
            'name' => 'user_hash',
            'vname' => 'LBL_USER_HASH',
            'type' => 'varchar',
            'len' => '255',
            'reportable' => false,
            'importable' => 'false',
            'sensitive' => true,
            'studio' => array(
                'no_duplicate' => true,
                'listview' => false,
                'searchview' => false,
            ),
            'api-visible' => false,
        ),
        'system_generated_password' => array(
            'name' => 'system_generated_password',
            'vname' => 'LBL_SYSTEM_GENERATED_PASSWORD',
            'type' => 'bool',
            'required' => true,
            'reportable' => false,
            'massupdate' => false,
            'studio' => array(
                'listview' => false,
                'searchview' => false,
                'editview' => false,
                'quickcreate' => false,
            ),
            'api-visible' => false,
        ),

        'pwd_last_changed' => array(
            'name' => 'pwd_last_changed',
            'vname' => 'LBL_PSW_MODIFIED',
            'type' => 'datetime',
            'required' => false,
            'massupdate' => false,
            'studio' => array('formula' => false),
            'api-visible' => false,
        ),
        /**
         * authenticate_id is used by authentication plugins so they may place a quick lookup key for looking up a given user after authenticating through the plugin
         */
        'authenticate_id' => array(
            'name' => 'authenticate_id',
            'vname' => 'LBL_AUTHENTICATE_ID',
            'type' => 'varchar',
            'len' => '100',
            'reportable' => false,
            'importable' => 'false',
            'studio' => array('listview' => false, 'searchview' => false, 'related' => false),
            'api-visible' => false,
        ),
        /**
         * sugar_login will force the user to use sugar authentication
         * regardless of what authentication the system is configured to use
         */
        'sugar_login' => array(
            'name' => 'sugar_login',
            'vname' => 'LBL_SUITE_LOGIN',
            'type' => 'bool',
            'default' => '1',
            'reportable' => false,
            'massupdate' => false,
            'importable' => false,
            'studio' => array('listview' => false, 'searchview' => false, 'formula' => false),
            'api-visible' => false,
        ),
        'first_name' => array(
            'name' => 'first_name',
            'vname' => 'LBL_FIRST_NAME',
            'dbType' => 'varchar',
            'type' => 'name',
            'len' => '255',
        ),
        'last_name' => array(
            'name' => 'last_name',
            'vname' => 'LBL_LAST_NAME',
            'dbType' => 'varchar',
            'type' => 'name',
            'len' => '255',
            'importable' => 'required',
            'required' => true,
        ),
        'full_name' => array(
            'name' => 'full_name',
            'rname' => 'full_name',
            'vname' => 'LBL_NAME',
            'type' => 'name',
            'fields' => array(
                'first_name',
                'last_name'
            ),
            'source' => 'non-db',
            'sort_on' => 'last_name',
            'sort_on2' => 'first_name',
            'db_concat_fields' => array(
                0 => 'first_name',
                1 => 'last_name'
            ),
            'len' => '510',
            'studio' => array('formula' => false),
        ),
        'name' => array(
            'name' => 'name',
            'rname' => 'name',
            'vname' => 'LBL_NAME',
            'type' => 'varchar',
            'source' => 'non-db',
            'len' => '510',
            'db_concat_fields' => array(
                0 => 'first_name',
                1 => 'last_name'
            ),
            'importable' => 'false',
        ),
        'is_admin' => array(
            'name' => 'is_admin',
            'vname' => 'LBL_IS_ADMIN',
            'type' => 'bool',
            'default' => '0',
            'studio' => array('listview' => false, 'searchview' => false, 'related' => false),
            'api-visible' => false,
        ),
        'external_auth_only' => array(
            'name' => 'external_auth_only',
            'vname' => 'LBL_EXT_AUTHENTICATE',
            'type' => 'bool',
            'reportable' => false,
            'massupdate' => false,
            'default' => '0',
            'studio' => array('listview' => false, 'searchview' => false, 'related' => false),
            'api-visible' => false,
        ),
        'receive_notifications' => array(
            'name' => 'receive_notifications',
            'vname' => 'LBL_RECEIVE_NOTIFICATIONS',
            'type' => 'bool',
            'default' => '1',
            'massupdate' => false,
            'studio' => false,
        ),
        'description' => array(
            'name' => 'description',
            'vname' => 'LBL_DESCRIPTION',
            'type' => 'text',
        ),
        'date_entered' => array(
            'name' => 'date_entered',
            'vname' => 'LBL_DATE_ENTERED',
            'type' => 'datetime',
            'required' => true,
            'studio' => array(
                'editview' => false,
                'quickcreate' => false,
            ),
        ),
        'date_modified' => array(
            'name' => 'date_modified',
            'vname' => 'LBL_DATE_MODIFIED',
            'type' => 'datetime',
            'required' => true,
            'studio' => array(
                'editview' => false,
                'quickcreate' => false,
            ),
        ),
        'modified_user_id' => array(
            'name' => 'modified_user_id',
            'rname' => 'user_name',
            'id_name' => 'modified_user_id',
            'vname' => 'LBL_MODIFIED_BY_ID',
            'type' => 'assigned_user_name',
            'table' => 'users',
            'isnull' => 'false',
            'dbType' => 'id',
        ),
        'modified_by_name' => array(
            'name' => 'modified_by_name',
            'vname' => 'LBL_MODIFIED_BY',
            'type' => 'varchar',
            'source' => 'non-db',
            'studio' => false,
        ),
        'created_by' => array(
            'name' => 'created_by',
            'rname' => 'user_name',
            'id_name' => 'modified_user_id',
            'vname' => 'LBL_ASSIGNED_TO',
            'type' => 'assigned_user_name',
            'table' => 'users',
            'isnull' => 'false',
            'dbType' => 'id',
            'studio' => false,
        ),
        'created_by_name' => array(
            'name' => 'created_by_name',
            'vname' => 'LBL_CREATED_BY_NAME', //bug 48978
            'type' => 'varchar',
            'source' => 'non-db',
            'importable' => 'false',
        ),
        'title' => array(
            'name' => 'title',
            'vname' => 'LBL_TITLE',
            'type' => 'varchar',
            'len' => '50',
        ),
        'photo' => array(
            'name' => 'photo',
            'vname' => 'LBL_PHOTO',
            'type' => 'image',
            'massupdate' => false,
            'comments' => '',
            'help' => '',
            'importable' => false,
            'reportable' => true,
            'len' => 255,
            'dbType' => 'varchar',
            'width' => '160',
            'height' => '160',
        ),
        'department' => array(
            'name' => 'department',
            'vname' => 'LBL_DEPARTMENT',
            'type' => 'varchar',
            'len' => '50',
        ),
        'phone_home' => array(
            'name' => 'phone_home',
            'vname' => 'LBL_HOME_PHONE',
            'type' => 'phone',
            'dbType' => 'varchar',
            'len' => '50',
        ),
        'phone_mobile' => array(
            'name' => 'phone_mobile',
            'vname' => 'LBL_MOBILE_PHONE',
            'type' => 'phone',
            'dbType' => 'varchar',
            'len' => '50',
        ),
        'phone_work' => array(
            'name' => 'phone_work',
            'vname' => 'LBL_WORK_PHONE',
            'type' => 'phone',
            'dbType' => 'varchar',
            'len' => '50',
        ),
        'phone_other' => array(
            'name' => 'phone_other',
            'vname' => 'LBL_OTHER_PHONE',
            'type' => 'phone',
            'dbType' => 'varchar',
            'len' => '50',
        ),
        'phone_fax' => array(
            'name' => 'phone_fax',
            'vname' => 'LBL_FAX_PHONE',
            'type' => 'phone',
            'dbType' => 'varchar',
            'len' => '50',
        ),
        'status' => array(
            'name' => 'status',
            'vname' => 'LBL_STATUS',
            'type' => 'enum',
            'len' => 100,
            'options' => 'user_status_dom',
            'importable' => 'required',
            'required' => true,
            'api-visible' => false,
        ),
        'address_street' => array(
            'name' => 'address_street',
            'vname' => 'LBL_ADDRESS_STREET',
            'type' => 'varchar',
            'len' => '150',
        ),
        'address_city' => array(
            'name' => 'address_city',
            'vname' => 'LBL_ADDRESS_CITY',
            'type' => 'varchar',
            'len' => '100',
        ),
        'address_state' => array(
            'name' => 'address_state',
            'vname' => 'LBL_ADDRESS_STATE',
            'type' => 'varchar',
            'len' => '100',
        ),
        'address_country' => array(
            'name' => 'address_country',
            'vname' => 'LBL_ADDRESS_COUNTRY',
            'type' => 'varchar',
            'len' => 100,
        ),
        'address_postalcode' => array(
            'name' => 'address_postalcode',
            'vname' => 'LBL_ADDRESS_POSTALCODE',
            'type' => 'varchar',
            'len' => '20',
        ),
        // This is a fake field for the edit view
        'UserType' => array(
            'name' => 'UserType',
            'vname' => 'LBL_USER_TYPE',
            'type' => 'enum',
            'len' => 50,
            'options' => 'user_type_dom',
            'source' => 'non-db',
            'import' => false,
            'reportable' => false,
            'inline_edit' => false,
            'studio' => array('formula' => false),
        ),
        'deleted' => array(
            'name' => 'deleted',
            'vname' => 'LBL_DELETED',
            'type' => 'bool',
            'required' => false,
            'reportable' => false,
        ),
        'portal_only' => array(
            'name' => 'portal_only',
            'vname' => 'LBL_PORTAL_ONLY_USER',
            'type' => 'bool',
            'massupdate' => false,
            'default' => '0',
            'studio' => array('listview' => false, 'searchview' => false, 'formula' => false),
        ),
        'show_on_employees' => array(
            'name' => 'show_on_employees',
            'vname' => 'LBL_SHOW_ON_EMPLOYEES',
            'type' => 'bool',
            'massupdate' => true,
            'importable' => true,
            'default' => true,
            'studio' => array('formula' => false),
        ),
        'employee_status' => array(
            'name' => 'employee_status',
            'vname' => 'LBL_EMPLOYEE_STATUS',
            'type' => 'varchar',
            'function' => array(
                'name' => 'getEmployeeStatusOptions',
                'returns' => 'html',
                'include' => 'modules/Employees/EmployeeStatus.php'
            ),
            'len' => 100,
        ),
        'messenger_id' => array(
            'name' => 'messenger_id',
            'vname' => 'LBL_MESSENGER_ID',
            'type' => 'varchar',
            'len' => 100,
        ),
        'messenger_type' => array(
            'name' => 'messenger_type',
            'vname' => 'LBL_MESSENGER_TYPE',
            'type' => 'enum',
            'options' => 'messenger_type_dom',
            'len' => 100,
        ),
        'calls' => array(
            'name' => 'calls',
            'type' => 'link',
            'relationship' => 'calls_users',
            'source' => 'non-db',
            'vname' => 'LBL_CALLS'
        ),
        'meetings' => array(
            'name' => 'meetings',
            'type' => 'link',
            'relationship' => 'meetings_users',
            'source' => 'non-db',
            'vname' => 'LBL_MEETINGS'
        ),
        'contacts_sync' => array(
            'name' => 'contacts_sync',
            'type' => 'link',
            'relationship' => 'contacts_users',
            'source' => 'non-db',
            'vname' => 'LBL_CONTACTS_SYNC',
            'reportable' => false,
        ),
        'reports_to_id' => array(
            'name' => 'reports_to_id',
            'vname' => 'LBL_REPORTS_TO_ID',
            'type' => 'id',
            'required' => false,
        ),
        'reports_to_name' => array(
            'name' => 'reports_to_name',
            'rname' => 'last_name',
            'id_name' => 'reports_to_id',
            'vname' => 'LBL_REPORTS_TO_NAME',
            'type' => 'relate',
            'isnull' => 'true',
            'module' => 'Users',
            'table' => 'users',
            'link' => 'reports_to_link',
            'reportable' => false,
            'source' => 'non-db',
            'duplicate_merge' => 'disabled',
            'side' => 'right',
        ),
        'reports_to_link' => array(
            'name' => 'reports_to_link',
            'type' => 'link',
            'relationship' => 'user_direct_reports',
            'link_type' => 'one',
            'side' => 'right',
            'source' => 'non-db',
            'vname' => 'LBL_REPORTS_TO',
        ),
        'reportees' => array(
            'name' => 'reportees',
            'type' => 'link',
            'relationship' => 'user_direct_reports',
            'link_type' => 'many',
            'side' => 'left',
            'source' => 'non-db',
            'vname' => 'LBL_REPORTS_TO',
            'reportable' => false,
        ),
        'email1' => array(
            'name' => 'email1',
            'vname' => 'LBL_EMAIL',
            'type' => 'varchar',
            'function' => array(
                'name' => 'getEmailAddressWidget',
                'returns' => 'html'
            ),
            'source' => 'non-db',
            'group' => 'email1',
            'merge_filter' => 'enabled',
            'required' => true,
        ),
        'email_addresses' => array(
            'name' => 'email_addresses',
            'type' => 'link',
            'relationship' => 'users_email_addresses',
            'module' => 'EmailAddress',
            'bean_name' => 'EmailAddress',
            'source' => 'non-db',
            'vname' => 'LBL_EMAIL_ADDRESSES',
            'reportable' => false,
            'required' => true,
        ),
        'email_addresses_primary' => array(
            'name' => 'email_addresses_primary',
            'type' => 'link',
            'relationship' => 'users_email_addresses_primary',
            'source' => 'non-db',
            'vname' => 'LBL_EMAIL_ADDRESS_PRIMARY',
            'duplicate_merge' => 'disabled',
            'required' => true,
        ),
        /* Virtual email fields so they will display on the main user page */
        'email_link_type' => array(
            'name' => 'email_link_type',
            'vname' => 'LBL_EMAIL_LINK_TYPE',
            'type' => 'enum',
            'options' => 'dom_email_link_type',
            'importable' => false,
            'reportable' => false,
            'source' => 'non-db',
            'studio' => false,
        ),

        'editor_type' => array(
            'name' => 'editor_type',
            'vname' => 'LBL_EDITOR_TYPE',
            'type' => 'enum',
            'options' => 'dom_editor_type',
            'importable' => false,
            'reportable' => false,
            'source' => 'non-db',
            'studio' => false,
        ),

        'aclroles' => array(
            'name' => 'aclroles',
            'type' => 'link',
            'relationship' => 'acl_roles_users',
            'source' => 'non-db',
            'side' => 'right',
            'vname' => 'LBL_ROLES',
        ),
        'is_group' => array(
            'name' => 'is_group',
            'vname' => 'LBL_GROUP_USER',
            'type' => 'bool',
            'massupdate' => false,
            'studio' => array('listview' => false, 'searchview' => false, 'formula' => false),
        ),
        /* to support Meetings SubPanels */
        'c_accept_status_fields' => array(
            'name' => 'c_accept_status_fields',
            'rname' => 'id',
            'relationship_fields' => array(
                'id' => 'accept_status_id',
                'accept_status' => 'accept_status_name'
            ),
            'vname' => 'LBL_LIST_ACCEPT_STATUS',
            'type' => 'relate',
            'link' => 'calls',
            'link_type' => 'relationship_info',
            'source' => 'non-db',
            'importable' => 'false',
            'studio' => array('listview' => false, 'searchview' => false, 'formula' => false),
        ),
        'm_accept_status_fields' => array(
            'name' => 'm_accept_status_fields',
            'rname' => 'id',
            'relationship_fields' => array(
                'id' => 'accept_status_id',
                'accept_status' => 'accept_status_name'
            ),
            'vname' => 'LBL_LIST_ACCEPT_STATUS',
            'type' => 'relate',
            'link' => 'meetings',
            'link_type' => 'relationship_info',
            'source' => 'non-db',
            'importable' => 'false',
            'studio' => array('listview' => false, 'searchview' => false, 'formula' => false),
        ),
        'accept_status_id' => array(
            'name' => 'accept_status_id',
            'type' => 'varchar',
            'source' => 'non-db',
            'vname' => 'LBL_LIST_ACCEPT_STATUS',
            'importable' => 'false',
            'studio' => array('listview' => false, 'searchview' => false, 'formula' => false),
        ),
        'accept_status_name' => array(
            'name' => 'accept_status_name',
            'type' => 'enum',
            'source' => 'non-db',
            'vname' => 'LBL_LIST_ACCEPT_STATUS',
            'options' => 'dom_meeting_accept_status',
            'massupdate' => false,
            'studio' => array('listview' => false, 'searchview' => false, 'formula' => false),
        ),
        'prospect_lists' => array(
            'name' => 'prospect_lists',
            'type' => 'link',
            'relationship' => 'prospect_list_users',
            'module' => 'ProspectLists',
            'source' => 'non-db',
            'vname' => 'LBL_PROSPECT_LIST',
        ),
        'emails_users' => array(
            'name' => 'emails_users',
            'type' => 'link',
            'relationship' => 'emails_users_rel',
            'module' => 'Emails',
            'source' => 'non-db',
            'vname' => 'LBL_EMAILS'
        ),
        'eapm' =>
            array(
                'name' => 'eapm',
                'type' => 'link',
                'relationship' => 'eapm_assigned_user',
                'vname' => 'LBL_ASSIGNED_TO_USER',
                'source' => 'non-db',
            ),
        'oauth_tokens' =>
            array(
                'name' => 'oauth_tokens',
                'type' => 'link',
                'relationship' => 'oauthtokens_assigned_user',
                'vname' => 'LBL_OAUTH_TOKENS',
                'link_type' => 'one',
                'module' => 'OAuthTokens',
                'bean_name' => 'OAuthToken',
                'source' => 'non-db',
                'side' => 'left',
            ),
        'project_resource' =>
            array(
                'name' => 'project_resource',
                'type' => 'link',
                'relationship' => 'projects_users_resources',
                'source' => 'non-db',
                'vname' => 'LBL_PROJECTS',
            ),
        'project_users_1' =>
            array(
                'name' => 'project_users_1',
                'type' => 'link',
                'relationship' => 'project_users_1',
                'source' => 'non-db',
                'module' => 'Project',
                'bean_name' => 'Project',
                'vname' => 'LBL_PROJECT_USERS_1_FROM_PROJECT_TITLE',
            ),
        'am_projecttemplates_resources' =>
            array(
                'name' => 'am_projecttemplates_resources',
                'type' => 'link',
                'relationship' => 'am_projecttemplates_users_resources',
                'source' => 'non-db',
                'vname' => 'LBL_PROJECTS',
            ),
        'am_projecttemplates_users_1' =>
            array(
                'name' => 'am_projecttemplates_users_1',
                'type' => 'link',
                'relationship' => 'am_projecttemplates_users_1',
                'source' => 'non-db',
                'module' => 'AM_ProjectTemplates',
                'bean_name' => 'AM_ProjectTemplates',
                'vname' => 'LBL_PROJECT_USERS_1_FROM_PROJECT_TITLE',
            ),
        'SecurityGroups' =>
            array(
                'name' => 'SecurityGroups',
                'type' => 'link',
                'relationship' => 'securitygroups_users',
                'source' => 'non-db',
                'module' => 'SecurityGroups',
                'bean_name' => 'SecurityGroup',
                'vname' => 'LBL_SECURITYGROUPS',
            ),
        'securitygroup_noninher_fields' =>
            array(
                'name' => 'securitygroup_noninher_fields',
                'rname' => 'id',
                'relationship_fields' =>
                    array(
                        'id' => 'securitygroup_noninherit_id',
                        'noninheritable' => 'securitygroup_noninheritable',
                        'primary_group' => 'securitygroup_primary_group',
                    ),
                'vname' => 'LBL_USER_NAME',
                'type' => 'relate',
                'link' => 'SecurityGroups',
                'link_type' => 'relationship_info',
                'source' => 'non-db',
                'Importable' => false,
                'duplicate_merge' => 'disabled',
            ),
        'securitygroup_noninherit_id' =>
            array(
                'name' => 'securitygroup_noninherit_id',
                'type' => 'varchar',
                'source' => 'non-db',
                'vname' => 'LBL_securitygroup_noninherit_id',
            ),
        'securitygroup_noninheritable' =>
            array(
                'name' => 'securitygroup_noninheritable',
                'type' => 'bool',
                'source' => 'non-db',
                'vname' => 'LBL_SECURITYGROUP_NONINHERITABLE',
            ),
        'securitygroup_primary_group' =>
            array(
                'name' => 'securitygroup_primary_group',
                'type' => 'bool',
                'source' => 'non-db',
                'vname' => 'LBL_PRIMARY_GROUP',
            ),
        'factor_auth' => array(
            'name' => 'factor_auth',
            'type' => 'bool',
            'vname' => 'LBL_FACTOR_AUTH',
            'massupdate' => false,
        ),
        'factor_auth_interface' => array(
            'name' => 'factor_auth_interface',
            'vname' => 'LBL_FACTOR_AUTH_INTERFACE',
            'type' => 'enum',
            'options' => 'user_factor_auth_interface_dom',
        ),

    ),
    'indices' => array(
        array(
            'name' => 'userspk',
            'type' => 'primary',
            'fields' => array(
                'id'
            )
        ),
        array(
            'name' => 'idx_user_name',
            'type' => 'index',
            'fields' => array(
                'user_name',
                'is_group',
                'status',
                'last_name (30)',
                'first_name (30)',
                'id'
            )
        ),
    ),
    'relationships' => [
        'user_direct_reports' => [
            'lhs_module' => 'Users',
            'lhs_table' => 'users',
            'lhs_key' => 'id',
            'rhs_module' => 'Users',
            'rhs_table' => 'users',
            'rhs_key' => 'reports_to_id',
            'relationship_type' => 'one-to-many'
        ],
        'users_users_signatures' =>
            [
                'lhs_module' => 'Users',
                'lhs_table' => 'users',
                'lhs_key' => 'id',
                'rhs_module' => 'UserSignature',
                'rhs_table' => 'users_signatures',
                'rhs_key' => 'user_id',
                'relationship_type' => 'one-to-many'
            ],
        'users_users_password_link' =>
            [
                'lhs_module' => 'Users',
                'lhs_table' => 'users',
                'lhs_key' => 'id',
                'rhs_table' => 'users_signatures',
                'rhs_key' => 'user_id',
                'relationship_type' => 'one-to-many'
            ],
        'users_email_addresses' =>
            [
                'lhs_module' => "Users",
                'lhs_table' => 'users',
                'lhs_key' => 'id',
                'rhs_module' => 'EmailAddresses',
                'rhs_table' => 'email_addresses',
                'rhs_key' => 'id',
                'relationship_type' => 'many-to-many',
                'join_table' => 'email_addr_bean_rel',
                'join_key_lhs' => 'bean_id',
                'join_key_rhs' => 'email_address_id',
                'relationship_role_column' => 'bean_module',
                'relationship_role_column_value' => "Users"
            ],
        'users_email_addresses_primary' =>
            [
                'lhs_module' => "Users",
                'lhs_table' => 'users',
                'lhs_key' => 'id',
                'rhs_module' => 'EmailAddresses',
                'rhs_table' => 'email_addresses',
                'rhs_key' => 'id',
                'relationship_type' => 'many-to-many',
                'join_table' => 'email_addr_bean_rel',
                'join_key_lhs' => 'bean_id',
                'join_key_rhs' => 'email_address_id',
                'relationship_role_column' => 'primary_address',
                'relationship_role_column_value' => '1'
            ],
    ],
);
