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
$mod_strings = array (
  'LBL_ASSIGNED_TO_ID' => 'Assigned to (ID)',
  'LBL_ASSIGNED_TO_NAME' => 'Assigned to',
  'LBL_ASSIGNED_TO' => 'Assigned to',
  'LBL_LIST_ASSIGNED_TO_NAME' => 'Assigned to',
  'LBL_LIST_ASSIGNED_USER' => 'Assigned to',
  'LBL_CREATED' => 'Created By',
  'LBL_CREATED_USER' => 'Created By',
  'LBL_CREATED_ID' => 'Created By (ID)',
  'LBL_MODIFIED' => 'Modified By',
  'LBL_MODIFIED_NAME' => 'Modified By',
  'LBL_MODIFIED_USER' => 'Modified By',
  'LBL_MODIFIED_ID' => 'Modified By (ID)',
  'LBL_SECURITYGROUPS' => 'Security Groups',
  'LBL_SECURITYGROUPS_SUBPANEL_TITLE' => 'Security Groups',
  'LBL_ID' => 'ID',
  'LBL_DATE_ENTERED' => 'Date Created',
  'LBL_DATE_MODIFIED' => 'Date Modified',
  'LBL_DESCRIPTION' => 'Description',
  'LBL_DELETED' => 'Deleted',
  'LBL_NAME' => 'Name',
  'LBL_LIST_NAME' => 'Name',
  'LBL_EDIT_BUTTON' => 'Edit',
  'LBL_QUICKEDIT_BUTTON' => '↙ Edit',
  'LBL_REMOVE' => 'Remove',
  'LBL_ASCENDING' => 'Ascending',
  'LBL_DESCENDING' => 'Descending',

  'LBL_LIST_FORM_TITLE' => 'Form Responses List',
  'LBL_MODULE_NAME' => 'Form Responses',
  'LBL_MODULE_TITLE' => 'Form Responses',
  'LBL_HOMEPAGE_TITLE' => 'My Form Responses',
  'LNK_NEW_RECORD' => 'Create a Form Response',
  'LNK_LIST' => 'View Form Responses',
  'LNK_IMPORT_stic_AWF_Responses' => 'Import Form Responses',
  'LBL_SEARCH_FORM_TITLE' => 'Search Form Responses',
  'LBL_HISTORY_SUBPANEL_TITLE' => 'View History',
  'LBL_ACTIVITIES_SUBPANEL_TITLE' => 'Activities',
  'LBL_NEW_FORM_TITLE' => 'New Form Response',
  'LBL_EMPTY' => 'Empty',
   
  'LBL_DEFAULT_PANEL' => 'Overview',
  'LBL_PANEL_RECORD_DETAILS' => 'Record details',

  // Module fields
  'LBL_FORM' => 'Form',
  'LBL_FORM_URL' => 'Form URL',
  'LBL_CLEAN_REFERRER' => 'Source page',
  'LBL_USER_AGENT' => 'Browser and operating system',
  'LBL_REMOTE_IP' => 'IP address',
  'LBL_RAW_PAYLOAD' => 'JSON response (hidden)',
  'LBL_RESPONSE_HASH' => 'Response hash',
  'LBL_HTML_SUMMARY' => 'Response',
  'LBL_STATUS' => 'Status',
  'LBL_EXECUTION_LOG' => 'Execution log',
  
  // Execution log: action results
  'LBL_EXECUTION_ITEM_OK' => '✅ [OK]',
  'LBL_EXECUTION_ITEM_SKIPPED' => '⏭️ [SKIPPED]',
  'LBL_EXECUTION_ITEM_ERROR' => '❌ [ERROR]',
  'LBL_EXECUTION_DEFERRED' => 'Deferred execution',

  // General
  'LBL_FIELD' => 'Field',
  
  // Generic response messages
  'LBL_DUPLICATE_RESPONSE_TITLE' => 'Warning',
  'LBL_DUPLICATE_RESPONSE_MSG' => 'This response has already been submitted and processed.',

  'LBL_ERROR_GENERIC_TITLE' => 'Error',
  'LBL_ERROR_GENERIC_MSG' => 'An error occurred while processing your response.',

  'LBL_ERROR_FORM_VALIDATION' => 'Form data validation error',
  'LBL_ERROR_FORM_VALIDATION_MSG' => 'Errors were detected in the submitted data.',
  'LBL_BUTTON_GO_BACK_AND_FIX' => 'Go back and edit the form',

  // Internal processing errors
  'LBL_RESPONSE_NO_PUBLIC_STATUS' => 'Response received while the form was unpublished.',
  'LBL_RESPONSE_HONEYPOT_SPAM' => 'Spam detected: hidden honeypot field was filled.',
  'LBL_RESPONSE_TIMETRAP_SPAM' => 'Spam detected: form was submitted too quickly.',
  'LBL_RESPONSE_USERAGENT_SPAM' => 'Spam detected: form was submitted by an application.',
  'LBL_ERROR_FORM_CONFIG' => 'Form configuration error',
  'LBL_ERROR_GENERATING_HTML_SUMMARY' => 'Error generating the response summary table',

  // User-facing validation errors
  'LBL_ERROR_REQUIRED_FIELD' => 'The field is required.',
  'LBL_ERROR_INTEGER_FIELD' => 'Value must be an integer.',
  'LBL_ERROR_NUMERIC_FIELD' => 'Value must be numeric.',
  'LBL_ERROR_DATE_FIELD' => 'Value must be a valid date.',
  'LBL_ERROR_BOOL_FIELD' => 'Value must be true or false.',
  'LBL_ERROR_ENUM_FIELD' => 'Invalid option for the dropdown.',
  'LBL_ERROR_EMAIL_FIELD' => 'Value must be a valid email address.',
  'LBL_ERROR_VALUE_FIELD' => 'Value is not valid for the field.',
  
  // Subpanels
  'LBL_STIC_AWF_RESPONSES_STIC_AWF_LINKS_FROM_STIC_AWF_LINKS_TITLE' => 'Form Response Links',
  'LBL_STIC_AWF_FORMS_STIC_AWF_RESPONSES_FROM_STIC_AWF_FORMS_TITLE' => 'Advanced Web Form',
  'LBL_ANSWERS_SUBPANEL_TITLE' => 'Response details',
);
