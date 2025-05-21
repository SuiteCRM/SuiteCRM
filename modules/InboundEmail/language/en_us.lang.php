<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2019 SalesAgility Ltd.
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

$mod_strings = array(


    'LBL_RE' => 'RE:',

    'ERR_BAD_LOGIN_PASSWORD' => 'Login or Password Incorrect',
    'ERR_INI_ZLIB' => 'Could not turn off Zlib compression temporarily. "Test Settings" may fail.',
    'ERR_NO_IMAP' => 'No IMAP libraries found. Please resolve this before continuing with Inbound Email',
    'ERR_NO_OPTS_SAVED' => 'No optimums were saved with your Inbound Email account. Please review the settings',
    'ERR_TEST_MAILBOX' => 'Please check your settings and try again.',
    'ERR_INVALID_PORT' => 'Invalid Port',

    'LBL_ASSIGN_TO_USER' => 'Assign To User',
    'LBL_AUTOREPLY' => 'Auto-Reply Template',
    'LBL_AUTOREPLY_HELP' => 'Select an automated response to notify email senders that their response has been received.',
    'LBL_BASIC' => 'Mail Account Information',
    'LBL_CASE_MACRO' => 'Case Macro',
    'LBL_CASE_MACRO_DESC' => 'Set the macro which will be parsed and used to link imported email to a Case.',
    'LBL_CASE_MACRO_DESC2' => 'Set this to any value, but preserve the <b>"%1"</b>.',
    'LBL_CLOSE_POPUP' => 'Close Window',
    'LBL_CREATE_TEMPLATE' => 'Create',
    'LBL_DELETE_SEEN' => 'Delete Read Emails After Import',
    'LBL_EDIT_TEMPLATE' => 'Edit',
    'LBL_EMAIL_OPTIONS' => 'Email Handling Options',
    'LBL_EMAIL_BOUNCE_OPTIONS' => 'Bounce Handling Options',
    'LBL_FILTER_DOMAIN_DESC' => 'Specify a domain to which no auto-replies will be sent.',
    'LBL_ASSIGN_TO_GROUP_FOLDER_DESC' => 'Select to automatically create email records in SuiteCRM for all incoming emails.',
    'LBL_FILTER_DOMAIN' => 'No Auto-Reply to this Domain',
    'LBL_FIND_SSL_WARN' => '<br>Testing SSL may take a long time. Please be patient.<br>',
    'LBL_FROM_ADDR' => '"From" Address',
    'LBL_FROM_ADDR_DESC' => 'The email address provided here might not appear in the &quot;From&quot; address section of the email sent due to restrictions imposed by the mail service provider. In these circumstances, the email address defined in the outgoing mail server will be used.', // as long as XTemplate doesn't support output escaping, transform quotes to html-entities right here (bug #48913)
    'LBL_FROM_NAME' => '"From" Name',
    'LBL_GROUP_QUEUE' => 'Assign To Group',
    'LBL_HOME' => 'Home',
    'LBL_LIST_MAILBOX_TYPE' => 'Mail Account Usage',
    'LBL_LIST_NAME' => 'Name:',
    'LBL_LIST_GLOBAL_PERSONAL' => 'Type',
    'LBL_LIST_SERVER_URL' => 'Mail Server',
    'LBL_SERVER_ADDRESS' => 'Server Address',
    'LBL_LIST_STATUS' => 'Status',
    'LBL_LOGIN' => 'User Name',
    'LBL_USERNAME' => 'User Name',
    'LBL_MAILBOX_DEFAULT' => 'INBOX',
    'LBL_MAILBOX_SSL' => 'Use SSL',
    'LBL_MAILBOX_TYPE' => 'Possible Actions',
    'LBL_DISTRIBUTION_METHOD' => 'Distribution Method',
    'LBL_CREATE_CASE_REPLY_TEMPLATE' => 'New Case Auto-Reply Template',
    'LBL_CREATE_CASE_REPLY_TEMPLATE_HELP' => 'Select an automated response to notify email senders that a case has been created. The email contains the case number in the Subject line which adheres to the Case Macro setting. This response is only sent when the first email is received from the recipient.',
    'LBL_MAILBOX' => 'Monitored Folders',
    'LBL_TRASH_FOLDER' => 'Trash Folder',
    'LBL_SENT_FOLDER' => 'Sent Folder',
    'LBL_SELECT' => 'Select',
    'LBL_MARK_READ_NO' => 'Email marked deleted after import',
    'LBL_MARK_READ_YES' => 'Email left on server after import',
    'LBL_MARK_READ' => 'Leave Messages On Server',
    'LBL_MAX_AUTO_REPLIES' => 'Number of Auto-responses',
    'LBL_MAX_AUTO_REPLIES_DESC' => 'Set the maximum number of auto-responses to send to a unique email address during a period of 24 hours.',
    'LBL_PERSONAL_MODULE_NAME' => 'Personal Mail Account',
    'LBL_CREATE_CASE' => 'Create Case from Email',
    'LBL_CREATE_CASE_HELP' => 'Select to automatically create case records in SuiteCRM from incoming emails.',
    'LBL_MODULE_NAME' => 'Group Mail Account',
    'LBL_BOUNCE_MODULE_NAME' => 'Bounce Handling Mailbox',
    'LBL_MODULE_TITLE' => 'Inbound Email',
    'LBL_NAME' => 'Name',
    'LBL_NONE' => 'None',
    'LBL_ONLY_SINCE_NO' => 'No. Check against all emails on mail server.',
    'LBL_ONLY_SINCE_YES' => 'Yes.',
    'LBL_PASSWORD' => 'Password',
    'LBL_EMAIL_PASSWORD' => 'Password',
    'LBL_POP3_SUCCESS' => 'Your POP3 test connection was successful.',
    'LBL_POPUP_TITLE' => 'Test Settings',
    'LBL_SELECT_SUBSCRIBED_FOLDERS' => 'Select Subscribed Folder(s)',
    'LBL_SELECT_TRASH_FOLDERS' => 'Select Trash Folder',
    'LBL_SELECT_SENT_FOLDERS' => 'Select Sent Folder',
    'LBL_DELETED_FOLDERS_LIST' => 'The following folder(s) %s either does not exist or has been deleted from server',
    'LBL_PORT' => 'Mail Server Port',
    'LBL_REPLY_TO_NAME' => '"Reply-to" Name',
    'LBL_REPLY_TO_ADDR' => '"Reply-to" Address',
    'LBL_SAME_AS_ABOVE' => 'Using From Name/Address',
    'LBL_SERVER_OPTIONS' => 'Advanced Setup',
    'LBL_SERVER_TYPE' => 'Mail Server Protocol',
    'LBL_SERVER_PORT' => 'Mail Server Port',
    'LBL_SERVER_URL' => 'Mail Server Address',
    'LBL_SSL_DESC' => 'If your mail server supports secure socket connections, enabling this will force SSL connections when importing email.',
    'LBL_ASSIGN_TO_TEAM_DESC' => 'The selected team has access to the mail account.',
    'LBL_SSL' => 'Use SSL',
    'LBL_STATUS' => 'Status',
    'LBL_EMAIL_BODY_FILTERING' => 'Email Body Filter Type',
    'LBL_SYSTEM_DEFAULT' => 'System Default',
    'LBL_TEST_BUTTON_TITLE' => 'Test',
    'LBL_TEST_SETTINGS' => 'Test Settings',
    'LBL_TEST_CONNECTION_SETTINGS' => 'Test Connection Settings',
    'LBL_TEST_SUCCESSFUL' => 'Connection completed successfully.',
    'LBL_TEST_WAIT_MESSAGE' => 'One moment please...',
    'LBL_WARN_IMAP_TITLE' => 'Inbound Email Disabled',
    'LBL_WARN_IMAP' => 'Warnings:',
    'LBL_WARN_NO_IMAP' => 'Inbound Email <b>cannot</b> function without the IMAP c-client libraries enabled/compiled with the PHP module. Please contact your administrator to resolve this issue.',

    'LNK_LIST_CREATE_NEW_PERSONAL' => 'New Personal Inbound Email Account',
    'LNK_LIST_CREATE_NEW_GROUP' => 'New Group Inbound Email Account',
    'LNK_LIST_CREATE_NEW_CASES_TYPE' => 'New Case Handling Email Account',
    'LNK_LIST_CREATE_NEW_BOUNCE' => 'New Bounce Handling Email Account',
    'LNK_LIST_MAILBOXES' => 'Inbound Email Accounts',
    'LNK_LIST_OUTBOUND_EMAILS' => 'Outbound Email Accounts',
    'LNK_LIST_SCHEDULER' => 'Schedulers',
    'LNK_SEED_QUEUES' => 'Seed Queues From Teams',
    'LBL_GROUPFOLDER_ID' => 'Group Folder Id',

    'LBL_ALLOW_OUTBOUND_GROUP_USAGE' => 'Allow users to send emails using the "From" Name and Address as the reply to address',
    'LBL_ALLOW_OUTBOUND_GROUP_USAGE_DESC' => 'When this option is selected, the From Name and From Email Address associated with this group mail account will appear as an option for the From field when composing emails for users that have access to the group mail account.',
    'LBL_STATUS_ACTIVE' => 'Active',
    'LBL_STATUS_INACTIVE' => 'Inactive',
    'LBL_IS_PERSONAL' => 'Personal',
    'LBL_IS_GROUP' => 'group',
    'LBL_ENABLE_AUTO_IMPORT' => 'Import Emails Automatically',
    'LBL_WARNING_CHANGING_AUTO_IMPORT' => 'Warning: You are modifying your automatic import setting which may result in loss of data.',
    'LBL_WARNING_CHANGING_AUTO_IMPORT_WITH_CREATE_CASE' => 'Warning: Auto import must be enabled when automatically creating cases.',
    'LBL_LIST_TITLE_MY_DRAFTS' => 'Drafts',
    'LBL_LIST_TITLE_MY_INBOX' => 'Inbox',
    'LBL_LIST_TITLE_MY_SENT' => 'Sent Email',
    'LBL_LIST_TITLE_MY_ARCHIVES' => 'Archived Emails',
    'LNK_MY_DRAFTS' => 'Drafts',
    'LNK_MY_INBOX' => 'Email',
    'LNK_VIEW_MY_INBOX' => 'View Email',
    'LNK_QUICK_REPLY' => 'Reply',
    'LNK_SENT_EMAIL_LIST' => 'Sent Emails',
    'LBL_EDIT_LAYOUT' => 'Edit Layout' /*for 508 compliance fix*/,
    'LBL_TYPE_DIFFERENT' => 'External OAuth Connection type must be the SAME as the Inbound Email Account type',

    'LBL_MODIFIED_BY' => 'Modified By',
    'LBL_SERVICE' => 'Service',
    'LBL_STORED_OPTIONS' => 'Stored Options',
    'LBL_GROUP_ID' => 'Group ID',

    'LBL_OUTBOUND_CONFIGURATION' => 'Outbound Configuration',
    'LBL_CONNECTION_CONFIGURATION' => 'Server Configuration',
    'LBL_AUTO_REPLY_CONFIGURATION' => 'Auto Reply Configuration',
    'LBL_CASE_CONFIGURATION' => 'Case Configuration',
    'LBL_GROUP_CONFIGURATION' => 'Group Configuration',

    'LBL_SECURITYGROUPS_SUBPANEL_TITLE' => 'Security Groups',


    'LBL_OUTBOUND_EMAIL_ACCOUNT' => 'Outbound Email Account',
    'LBL_OUTBOUND_EMAIL_ACCOUNT_ID' => 'Outbound Email Account id',
    'LBL_OUTBOUND_EMAIL_ACCOUNT_NAME' => 'Outbound Email Account',

    'LBL_AUTOREPLY_EMAIL_TEMPLATE' => 'Auto Reply Email Template',
    'LBL_AUTOREPLY_EMAIL_TEMPLATE_ID' => 'Auto Reply Email Template id',
    'LBL_AUTOREPLY_EMAIL_TEMPLATE_NAME' => 'Auto Reply Email Template',

    'LBL_CASE_EMAIL_TEMPLATE' => 'Case Email Template',
    'LBL_CASE_EMAIL_TEMPLATE_ID' => 'Case Email Template id',
    'LBL_CASE_EMAIL_TEMPLATE_NAME' => 'Case Email Template',

    'LBL_PROTOCOL' => 'Protocol',
    'LBL_CONNECTION_STRING' => 'Connection String',
    'LBL_DISTRIB_METHOD' => 'Distribution Method',
    'LBL_DISTRIB_OPTIONS' => 'Distribution Options',

    'LBL_DISTRIBUTION_USER' => 'Distribution User',
    'LBL_DISTRIBUTION_USER_ID' => 'Distribution User id',
    'LBL_DISTRIBUTION_USER_NAME' => 'Distribution User',

    'LBL_EXTERNAL_OAUTH_CONNECTION' => 'External OAuth Connection',
    'LBL_EXTERNAL_OAUTH_CONNECTION_ID' => 'External OAuth Connection id',
    'LBL_EXTERNAL_OAUTH_CONNECTION_NAME' => 'External OAuth Connection',
    'LNK_EXTERNAL_OAUTH_CONNECTIONS' => 'External OAuth Connections',

    'LBL_TYPE' => 'Type',
    'LBL_AUTH_TYPE' => 'Auth Type',
    'LBL_IS_DEFAULT' => 'Default',
    'LBL_SIGNATURE' => 'Signature',

    'LBL_OWNER_NAME' => 'Owner',

    'LBL_SET_AS_DEFAULT_BUTTON' => 'Set as default',

    'LBL_MOVE_MESSAGES_TO_TRASH_AFTER_IMPORT' => 'Move Messages To Trash After Import?',
);
