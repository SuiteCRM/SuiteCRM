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

  'LBL_LIST_FORM_TITLE' => 'Advanced Web Forms List',
  'LBL_MODULE_NAME' => 'Advanced Web Forms',
  'LBL_MODULE_TITLE' => 'Advanced Web Forms',
  'LBL_HOMEPAGE_TITLE' => 'My Advanced Web Forms',
  'LNK_NEW_RECORD' => 'Create an Advanced Web Form',
  'LNK_LIST' => 'View Advanced Web Forms',
  'LNK_IMPORT_stic_AWF_Forms' => 'Import Advanced Web Forms',
  'LBL_SEARCH_FORM_TITLE' => 'Search Advanced Web Forms',
  'LBL_HISTORY_SUBPANEL_TITLE' => 'History',
  'LBL_ACTIVITIES_SUBPANEL_TITLE' => 'Activities',
  'LBL_NEW_FORM_TITLE' => 'New Advanced Web Form',

  'LBL_DEFAULT_PANEL' => 'Overview',
  'LBL_PANEL_RECORD_DETAILS' => 'Record details',

  // -- MODULE FIELDS --
  'LBL_STATUS' => 'Status',
  'LBL_START_DATE' => 'Start date',
  'LBL_END_DATE' => 'End date',
  'LBL_PUBLIC_URL' => 'URL',
  'LBL_PROCESSING_MODE' => 'Processing mode',
  'LBL_PROCESSING_MODE_DESC' => 'Defines how received responses are managed. In synchronous mode, responses are stored and processed upon receipt. In asynchronous mode, responses are stored upon receipt and processed later through a background process, which may be of interest to optimize system performance in case of large response influx.',
  'LBL_FORM_TYPE' => 'Form type',
  'LBL_FORM_TYPE_DESC' => 'Defines the type of form. Web forms are accessible without restrictions, while internal CRM forms can only be filled out by system users.',
  'LBL_CONFIGURATION' => 'Configuration',
  'LBL_ANALYTICS_VIEWS' => 'Total views',
  'LBL_ANALYTICS_BLOCKED' => 'Blocked views (not public)',
  'LBL_ANALYTICS_SUBMISSIONS' => 'Valid responses',
  'LBL_ANALYTICS_SPAM' => 'Spam detected',
  'LBL_ANALYTICS_REFERRERS' => 'Traffic sources (domains)',

  // -- WIZARD --
  // Main actions
  'LBL_WIZARD_PREVIOUS' => '<< Previous',
  'LBL_WIZARD_NEXT' => 'Next >>',
  'LBL_WIZARD_FINISH' => 'Finish',
  'LBL_WIZARD_SAVE' => 'Save',
  'LBL_WIZARD_SAVED_DRAFT' => 'Form saved',
  'LBL_WIZARD_SHOW_DEBUG_INFO' => 'Show details',

  // Edit Warning
  'LBL_WIZARD_FORM_EDIT_WARNING_TITLE' => '⚠️ Caution when editing!',
  'LBL_WIZARD_FORM_EDIT_WARNING_PUBLIC' => 'This form is public: someone could be filling it out right now.',
  'LBL_WIZARD_FORM_EDIT_WARNING_RESPONSES' => 'There are %s recorded responses: changing the form structure could cause inconsistencies.',
  'LBL_WIZARD_FORM_EDIT_WARNING_PROCEED' => 'If you want to make significant changes, it is recommended to consider duplicating the form.',
  
  // Corrupted form warning
  'LBL_WIZARD_FORM_CORRUPTED_WARNING_MESSAGE' => '⚠️ Critical error: Configuration failed to load properly. The form will appear empty and auto-save has been disabled to prevent data loss.',

  // Steps
  'LBL_WIZARD_TITLE_STEP1' => 'General information',
  'LBL_WIZARD_DESC_STEP1' => 'Definition of the form general properties.',
  'LBL_WIZARD_TITLE_STEP2' => 'Structure and fields',
  'LBL_WIZARD_DESC_STEP2' => 'Definition of the form content. Data blocks can be linked to SinergiaCRM modules or run on their own. Additionally, it is possible to configure data validation, duplicate detection, etc.',
  'LBL_WIZARD_TITLE_STEP2_START' => 'Start building the form',
  'LBL_WIZARD_TITLE_STEP3' => 'Logic and automation',
  'LBL_WIZARD_DESC_STEP3' => 'Definition of the actions that will be executed when a response is received: create or update records, send emails, redirect to web pages, etc.',
  'LBL_WIZARD_TITLE_STEP4' => 'Layout',
  'LBL_WIZARD_DESC_STEP4' => "Design of the form's appearance. Allows defining the visual style, organizing data blocks, customizing the header and footer of the form, etc.",
  'LBL_WIZARD_TITLE_STEP5' => 'Summary and publication',
  'LBL_WIZARD_DESC_STEP5' => 'Review and activation of the form. Allows activating the reception of responses, obtaining the public access link or downloading the HTML code to integrate it into an external website.',

  // General buttons
  'LBL_BUTTON_ADD' => 'Add',
  'LBL_BUTTON_EDIT' => 'Edit',
  'LBL_BUTTON_DELETE' => 'Delete',
  'LBL_BUTTON_MOVE_UP' => 'Move up',
  'LBL_BUTTON_MOVE_DOWN' => 'Move down',
  'LBL_BUTTON_DUPLICATE' => 'Duplicate',
  'LBL_BUTTON_RELOAD' => 'Reload',
  'LBL_BUTTON_COPY' => 'Copy',
  'LBL_BUTTON_OPEN' => 'Open',
  'LBL_BUTTON_DOWNLOAD' => 'Download',

  // General editors
  'LBL_SELECT_PLACEHOLDER' => 'Select an item...',
  'LBL_SELECT_WRITE_TO_SEARCH' => 'Type to filter the list...',
  'LBL_SELECT_NO_RESULTS' => 'No matches found',

  // DataBlocks
  'LBL_DATABLOCK' => 'Data block',
  'LBL_DATABLOCKS' => 'Data blocks',
  'LBL_DATABLOCK_DETACHED' => 'Unlinked data',
  'LBL_DATABLOCK_ADD' => 'Add a data block',
  'LBL_DATABLOCK_ADD_TITLE' => 'Configure a data block related to a system module',
  'LBL_DATABLOCK_ADD_UNLINKED' => 'Add an unlinked data block',
  'LBL_DATABLOCK_ADD_UNLINKED_TITLE' => 'Configure a data block without relating it to any system module',
  'LBL_DATABLOCK_NEW' => 'New data block',
  'LBL_DATABLOCK_NEW_UNLINKED' => 'New unlinked data block',
  'LBL_DATABLOCK_MODULE' => 'Module',
  'LBL_DATABLOCK_NAME' => 'Name',
  'LBL_DATABLOCK_INTERNAL_NAME' => 'Internal name',

  // DataBlock -> Fields
  'LBL_FIELDS' => 'Fields',
  'LBL_FIELDS_FORM_TAB' => 'Form',
  'LBL_FIELDS_FORM_TAB_DESC' => 'Fields that will be included in the public form.',
  'LBL_FIELDS_FIXED_TAB' => 'Server',
  'LBL_FIELDS_FIXED_TAB_DESC' => 'Fields with a preset value that will be added to the data received from the forms.',
  'LBL_FIELD_FORM' => 'Field in the form',
  'LBL_FIELD_FORM_ADD' => 'Add field',
  'LBL_FIELD_FORM_NEW' => 'New field',
  'LBL_FIELD_UNLINKED' => 'Unlinked field',
  'LBL_FIELD_UNLINKED_ADD' => 'Add unlinked field',
  'LBL_FIELD_UNLINKED_NEW' => 'New unlinked field',
  'LBL_FIELD_FIXED' => 'Fixed value',
  'LBL_FIELD_FIXED_ADD' => 'Add server field',
  'LBL_FIELD_FIXED_NEW' => 'New server field',
  'LBL_FIELD_CONVERT_TO_FIELD_FORM' => 'Convert to form field',
  'LBL_FIELD_CONVERT_TO_FIELD_FIXED' => 'Convert to server field',
  'LBL_FIELD_DEFINITION' => 'Definition',
  'LBL_FIELD_DEFINITION_FORM' => 'Representation in the form',
  'LBL_FIELD_DEFINITION_VALIDATIONS' => 'Data validation',

  'LBL_FIELD' => 'Field',
  'LBL_FIELD_NAME' => 'Name',
  'LBL_FIELD_INTERNAL_NAME' => 'Internal name',
  'LBL_FIELDS_SHOW_ALL' => 'Show all fields',
  'LBL_FIELDS_SHOW_ALL_DESC' => 'Include all module fields in the dropdown even if they are not shown in edit or detail views.',
  'LBL_FIELD_LABEL' => 'Label',
  'LBL_FIELD_REQUIRED_IN_FORM' => 'Required field',
  'LBL_FIELD_IN_FORM' => 'In the form',
  'LBL_FIELD_TYPE_IN_FORM' => 'Input type',
  'LBL_FIELD_SUBTYPE_IN_FORM' => 'Editor type',
  'LBL_FIELD_PLACEHOLDER' => 'Background text',
  'LBL_FIELD_DESCRIPTION' => 'Help text',
  'LBL_FIELD_ADD_LINK' => 'Add link',
  'LBL_FIELD_LINK_CREATE' => 'Create link',
  'LBL_FIELD_LINK_TEXT' => 'Link text',
  'LBL_FIELD_LINK_URL' => 'Destination URL',
  'LBL_FIELD_VALUE_TYPE' => 'Value type',
  'LBL_FIELD_VALUE_OPTIONS_LIST' => 'Related list',
  'LBL_FIELD_VALUE_OPTIONS_CUSTOMIZE' => 'Customize options',
  'LBL_FIELD_VALUE_OPTIONS_CUSTOMIZED' => '(Modified)',
  'LBL_FIELD_VALUE_OPTIONS_SELECT_REGS' => 'Select records',
  'LBL_FIELD_VALUE_OPTIONS' => 'Possible values',
  'LBL_FIELD_VALUE_OPTION_SHOW_NAME' => 'Show internal values',
  'LBL_FIELD_VALUE_OPTION_NAME' => 'Internal value',
  'LBL_FIELD_VALUE_OPTION_ORIGINAL_LABEL' => 'Original text',
  'LBL_FIELD_VALUE_OPTION_LABEL' => 'Text',
  'LBL_FIELD_VALUE_OPTION_SHOW' => 'Show',
  'LBL_FIELD_VALUE_OPTION_ACTIONS' => 'Actions',
  'LBL_FIELD_VALUE' => 'Value',
  'LBL_FIELD_VALUE_RELATIVE_DATE_OPTION' => 'Relative date',
  'LBL_FIELD_VALUE_RELATIVE_DATE_OPTION_DESC' => 'Allows specifying a date relative to the date of received responses.',
  'LBL_FIELD_VALUE_RELATIVE_DATE_CUSTOM' => 'Custom relative date',
  'LBL_FIELD_VALUE_RELATIVE_DATE_CUSTOM_DESC' => 'English expression compatible with PHP strtotime() function. Examples: tomorrow 14:00, next monday, +2 weeks, first day of next month.',
  'LBL_FIELD_ACTIONS' => 'Actions',
  
  // DataBlock -> Field -> Validations
  'LBL_FIELD_VALIDATION' => 'Validation',
  'LBL_FIELD_VALIDATOR' => 'Validation action',
  'LBL_FIELD_VALIDATION_ADD' => 'Add validation',
  'LBL_FIELD_VALIDATION_NEW' => 'New validation',
  'LBL_FIELD_VALIDATION_EDIT' => 'Edit validation',
  'LBL_FIELD_VALIDATION_PARAMETERS' => 'Parameters',
  'LBL_FIELD_VALIDATION_ERROR_MESSAGE' => 'Error message',
  'LBL_FIELD_VALIDATION_ACTIONS' => 'Actions',
  'LBL_FIELD_ACTIVE_VALIDATIONS' => 'Active validations',

  // DataBlock -> Duplicate checks
  'LBL_DUPLICATE_CHECK' => 'Duplicate detection',
  'LBL_DUPLICATE_CHECK_ADD' => 'Add duplicate detection',
  'LBL_DUPLICATE_FIELDS' => 'Fields to verify',
  'LBL_DUPLICATE_FIELDS_SEL_FIELDS' => 'Select field(s)...',
  'LBL_ONDUPLICATE_ACTION' => 'Action in case of duplicate',

  // DataBlock -> Relationships
  'LBL_RELATIONSHIP' => 'Relationship',
  'LBL_RELATIONSHIPS' => 'Data block relationships',
  'LBL_RELATIONSHIP_ADD' => 'Add relationship',
  'LBL_RELATIONSHIP_NEW' => 'New data block relationship',  
  'LBL_RELATIONSHIP_NO_MODULE_RELATED' => 'No related module',
  'LBL_RELATIONSHIP_NEW_DATABLOCK' => 'New data block',
  'LBL_RELATIONSHIP_DATABLOCK_ORIG' => 'Origin data block',
  'LBL_RELATIONSHIP_DATABLOCK_DEST' => 'Destination data block',

  // Flows
  'LBL_FLOWS' => 'Action flows',
  'LBL_FLOW' => 'Action flow',
  'LBL_FLOW_RECEIPT' => 'Automatic response',
  'LBL_FLOW_MAIN' => 'Main',
  'LBL_FLOW_ONERROR' => 'Error',
  'LBL_FLOW_DEFERRED_MAIN' => 'Finished',

  // Flow -> Action
  'LBL_ACTION' => 'Action',
  'LBL_ACTION_ADD' => 'Add action',
  'LBL_ACTION_NEW' => 'New action',
  'LBL_ACTION_TERMINAL' => 'Final action',
  'LBL_ACTION_TERMINAL_ADD' => 'Add final action',
  'LBL_ACTION_TERMINAL_NEW' => 'New final action',
  'LBL_ACTION_SELECT' => 'Select action',
  'LBL_ACTION_TO_VIEW_DETAILS' => 'Select an action to view its details.',
  'LBL_ACTION_CONTINUE' => 'Continue',
  'LBL_ACTION_BACK' => 'Back',
  'LBL_ACTION_NAME' => 'Name',
  'LBL_ACTION_CATEGORY' => 'Category',
  'LBL_ACTION_PARAMETERS' => 'Parameters',
  'LBL_ACTION_ACTIONS' => 'Actions',
  'LBL_ACTIONS_SHOW_ALL' => 'Show all actions',
  'LBL_ACTIONS_SHOW_ALL_DESC' => 'Shows all actions defined in the form, including automatically created actions.',
  'LBL_ACTION_NO_PARAMS' => 'No parameters',
  'LBL_ACTION_PARAM_SELECT_NO_OPTION' => '-- Select --',
  'LBL_ACTION_PARAM_CRM_RECORD_MODULE' => 'Module',
  'LBL_ACTION_PARAM_CRM_RECORD_RECORD' => 'Record',
  'LBL_ACTION_PARAM_OPTION_SELECTOR_OPTION' => 'Option',
  'LBL_ACTION_PARAM_MISSING_MESSAGE' => 'Some required parameters have no value. Review action settings and set values to required parameters before saving.',
  'LBL_ACTION_CONDITION_TEXT' => 'This action will only execute if the condition is met.',
  'LBL_ACTION_CONTINUE_ON_ERROR' => 'Continue on error',
  'LBL_ACTION_CONTINUE_ON_ERROR_DESC' => 'If activated, the flow will continue even if the action fails.',

  // Conditions (Datablock field validations & Actions)
  'LBL_CONDITION' => 'Execution condition',
  'LBL_CONDITION_SUMMARY' => 'Condition',
  'LBL_APPLY_CONDITION' => 'Condition on another field value',
  'LBL_CONDITION_FIELD_NAME' => 'Field',
  'LBL_CONDITION_VALUE' => 'Value',

  // Layout 
  'LBL_LAYOUT_SETTINGS' => 'Configuration',
  'LBL_LAYOUT_FORM_DESIGN' => 'Form design',
  'LBL_LAYOUT_PREVIEW' => 'Preview',
  'LBL_LAYOUT_HEADER' => 'Header',
  'LBL_LAYOUT_FOOTER' => 'Footer',

  // Layout -> Theme
  'LBL_THEME_GENERAL' => 'General',
  'LBL_THEME_WEB_TITLE_TEXT' => 'Page title',
  'LBL_THEME_WEB_TITLE_VALUE' => 'Advanced Web Form',
  'LBL_THEME_SUBMIT_BUTTON_TEXT' => 'Submit button text',
  'LBL_THEME_SUBMIT_BUTTON_TEXT_VALUE' => 'Submit',
  'LBL_THEME_MAIN_COLORS' => 'Colors',
  'LBL_THEME_PRIMARY_COLOR' => 'Main',
  'LBL_THEME_PAGE_BG_COLOR' => 'Page background',
  'LBL_THEME_FORM_BG_COLOR' => 'Form background',
  'LBL_THEME_TYPOGRAPGY_TEXT' => 'Text format',
  'LBL_THEME_FONT_FAMILY' => 'Font',
  'LBL_THEME_FONT_SIZE' => 'Size',
  'LBL_THEME_TEXT_COLOR' => 'Color',
  'LBL_THEME_FIELDS_LABELS' => 'Fields and labels',
  'LBL_THEME_BORDERS' => 'Borders and shadows',
  'LBL_THEME_BORDER_RADIUS_CONTAINER' => 'Rounding (boxes)',
  'LBL_THEME_BORDER_RADIUS_CONTROLS' => 'Rounding (buttons/controls)',
  'LBL_THEME_BORDER_WIDTH' => 'Border width',
  'LBL_THEME_BORDER_COLOR' => 'Border color',
  'LBL_THEME_FLOATING_LABELS' => 'Floating labels',
  'LBL_THEME_FLOATING_LABELS_DESC' => 'If activated, labels are placed inside fields and will move when typing.',
  'LBL_THEME_LABEL_WEIGHT_BOLD' => 'Bold labels',
  'LBL_THEME_SHADOW_INTENSITY' => 'Shading',
  'LBL_THEME_INPUT_STYLE' => 'Field style',
  'LBL_THEME_STRUCTURE' => 'Structure and distribution',
  'LBL_THEME_FORM_WIDTH' => 'Maximum width',
  'LBL_THEME_FIELD_SPACING' => 'Field spacing',
  'LBL_THEME_SUBMIT_FULL_WIDTH' => 'Full width submit button',
  'LBL_THEME_EQUAL_HEIGHT_SECTIONS' => 'Equalize section heights',
  'LBL_THEME_SECTIONS_PER_ROW' => 'Columns (sections)',
  'LBL_THEME_FIELDS_PER_ROW' => 'Columns (fields)',
  'LBL_THEME_PER_ROW_DESC' => 'The number of columns will automatically adapt to the screen width.',
  'LBL_THEME_ADVANCED' => 'Advanced',
  'LBL_THEME_ADVANCED_NO_ADMIN_DESC' => 'Advanced settings (CSS and JS) are restricted to admin users.',
  'LBL_THEME_CUSTOM_CSS' => 'Custom CSS',
  'LBL_THEME_CUSTOM_CSS_DESC' => 'Will be injected into a <style> block.',
  'LBL_THEME_CUSTOM_JS' => 'Custom JS',
  'LBL_THEME_CUSTOM_JS_DESC' => 'Will be processed when the entire form has loaded.',
  
  'LBL_THEME_CLOSED_FORM' => 'Notice: Form closed',
  'LBL_THEME_CLOSED_FORM_DESC' => 'Notice that will appear on the form when it no longer accepts responses.',
  'LBL_THEME_CLOSED_FORM_TITLE' => 'Notice title',
  'LBL_THEME_CLOSED_FORM_TITLE_VALUE' => '⛔ Form closed',
  'LBL_THEME_CLOSED_FORM_TEXT' => 'Notice text',
  'LBL_THEME_CLOSED_FORM_TEXT_VALUE' => 'This form does not accept responses.',

  'LBL_THEME_PROCESSED_FORM' => 'Message: Data processed',
  'LBL_THEME_PROCESSED_FORM_DESC' => 'Message shown by default when correctly processing a form response. It will not be shown if a final action is defined.',
  'LBL_THEME_PROCESSED_FORM_TITLE' => 'Message title',
  'LBL_THEME_PROCESSED_FORM_TITLE_VALUE' => 'Processed',
  'LBL_THEME_PROCESSED_FORM_TEXT' => 'Message text',
  'LBL_THEME_PROCESSED_FORM_TEXT_VALUE' => 'Your data has been processed correctly.',
  
  'LBL_THEME_RECEIPT_FORM' => 'Message: Data received',
  'LBL_THEME_RECEIPT_FORM_DESC' => 'Message shown by default when receiving a response and saving it to be processed later. This message will not be shown if a final action is defined.',
  'LBL_THEME_RECEIPT_FORM_TITLE' => 'Message title',
  'LBL_THEME_RECEIPT_FORM_TITLE_VALUE' => 'Received',
  'LBL_THEME_RECEIPT_FORM_TEXT' => 'Message text',
  'LBL_THEME_RECEIPT_FORM_TEXT_VALUE' => 'Your data has been received correctly and will be processed as soon as possible.',

  'LBL_THEME_RESET_BUTTON' => 'Default settings',
  
  // Layout -> Sections
  'LBL_SECTIONS' => 'Sections',
  'LBL_SECTION_ADD' => 'Add section',
  'LBL_SECTION_NEW' => 'New section',
  'LBL_SECTION_CONFIG' => 'Configuration',
  'LBL_SECTION_CONTENT' => 'Content',
  'LBL_SECTION_TITLE' => 'Title',
  'LBL_SECTION_SUBTITLE' => 'Subtitle',
  'LBL_SECTION_NO_TITLE' => '< No title >',
  'LBL_SECTION_SHOW_TITLE' => 'Show title',
  'LBL_SECTION_CONTAINER' => 'Visual container',
  'LBL_SECTION_IS_COLLAPSIBLE' => 'Collapsible',
  'LBL_SECTION_ISCOLLAPSED' => 'Initially collapsed',
  'LBL_SECTION_MOVE_ELEMENT_NO_OPTION' => 'Move to...',
  'LBL_SECTION_EMPTY_DESC' => 'This section is empty. Move blocks here from other sections.',

  // Form generation
  'LBL_CODE_GENERATING' => 'Generating code...',
  'LBL_CODE_GENERATING_ERROR' => 'Error generating code',
  'LBL_CODE_LOADING' => 'Loading code...',
  'LBL_CODE_LOADING_ERROR' => 'Connection error',

  'LBL_REQUIRED_FIELD_MESSAGE' => 'This field must be informed',
  
  'LBL_PREVIEW_RIBBON' => 'Preview',
  'LBL_PREVIEW_LOADING' => 'Loading...',
  'LBL_PREVIEW_LOAD_ERROR' => 'Error loading preview',
  'LBL_PREVIEW_DESC' => 'Real view generated by the server.',
  'LBL_PREVIEW_MODE_ALERT' => 'The form is in preview mode. Data submission is not enabled.',
  'LBL_PREVIEW_TOOLBAR' => 'Preview',
  'LBL_PREVIEW_ACTIVE_TEXT' => 'Active',
  'LBL_PREVIEW_INACTIVE_TEXT' => 'Inactive',
  'LBL_PREVIEW_IN_NEW_TAB' => 'Preview in new tab',

  'LBL_FORM_PUBLISH_OPTIONS' => 'Publishing options',
  'LBL_FORM_PUBLISH_LINK' => 'Public link',
  'LBL_FORM_PUBLISH_LINK_DESC' => 'Copy and share this link to access the form.',
  'LBL_FORM_PUBLISH_IFRAME' => 'Embed (iframe)',
  'LBL_FORM_PUBLISH_IFRAME_DESC' => 'Copy this code to embed the form on an external website while keeping it hosted on SinergiaCRM.',
  'LBL_FORM_PUBLISH_HTML' => 'HTML code',
  'LBL_FORM_PUBLISH_HTML_DESC' => 'Use this code to host the form on an external website.',

  'LBL_COPY_TO_CLIPBOARD_DONE' => 'Copied to clipboard',

  'LBL_RATE_ARIA' => 'Rate with a %s',

  // Errors
  'LBL_ERROR_DATABLOCK_IS_INVALID' => 'The data block has errors',
  'LBL_ERROR_DATABLOCK_NAME' => 'The data block internal name is empty',
  'LBL_ERROR_DATABLOCK_TITLE' => 'The data block must have a public name',
  'LBL_ERROR_NO_DATABLOCKS' => 'At least one data block must be defined to continue',
  'LBL_ERROR_FIELD_IS_INVALID' => 'The field has errors',
  'LBL_ERROR_FIELD_NAME' => 'The field internal name is empty',
  'LBL_ERROR_FIELD_LABEL' => 'No label exists for the field',
  'LBL_ERROR_FIELD_TYPE' => 'Field type or editor not defined in form',
  'LBL_ERROR_FIELD_OPTIONS' => 'Dropdown without defined options',
  'LBL_ERROR_FIELD_FIXED_EMPTY' => 'Fixed field without assigned value',
  'LBL_OK_FIELD_IS_VALID' => 'The field is correct',

  // -- SUBPANELS --
  'LBL_STIC_AWF_FORMS_STIC_AWF_RESPONSES_FROM_STIC_AWF_RESPONSES_TITLE' => 'Form responses',

  // -- HOOK ACTIONS --
  // Generic 
  'LBL_CUSTOM_ACTION_DATABLOCK_PARAM_TEXT' => 'Data block',
  'LBL_CUSTOM_ACTION_DATABLOCK_PARAM_DESC' => 'Select the data block to be used as a parameter in the action',

  // SaveRecordAction
  'LBL_SAVE_RECORD_ACTION_TITLE' => 'Save record',
  'LBL_SAVE_RECORD_ACTION_DESC' => 'Saves or updates a record from form data',
  'LBL_SAVE_RECORD_ACTION_DUPLICATE_RULE_MATCHED_TEXT' => 'Field matching',

  // RelateRecordsAction
  'LBL_RELATE_RECORDS_ACTION_TITLE' => 'Create relationship',
  'LBL_RELATE_RECORDS_ACTION_DESC' => 'Creates a relationship between two records',
  'LBL_RELATE_RECORDS_ACTION_TARGET_OBJECT_TEXT' => 'Relationship destination',
  'LBL_RELATE_RECORDS_ACTION_TARGET_OBJECT_DESC' => 'The destination data block or record for the relationship to save',
  'LBL_RELATE_RECORDS_ACTION_OPTION_BLOCK_TEXT' => 'Destination data block',
  'LBL_RELATE_RECORDS_ACTION_OPTION_VALUE_TEXT' => 'Destination record ID',
  'LBL_RELATE_RECORDS_ACTION_RELATIONSHIP_TEXT' => 'Relationship to update',
  'LBL_RELATE_RECORDS_ACTION_RELATIONSHIP_DESC' => 'The internal name of the relationship linking to the destination data block',

  // AddToTargetListAction
  'LBL_ADD_TO_TARGET_LIST_ACTION_TITLE' => 'Add to Target List',
  'LBL_ADD_TO_TARGET_LIST_ACTION_DESC' => 'Adds the processed record (person, prospect, user or organization) to an existing Target List',
  'LBL_ADD_TO_TARGET_LIST_ACTION_CONTACT_TO_ADD_TEXT' => 'Recipient',
  'LBL_ADD_TO_TARGET_LIST_ACTION_CONTACT_TO_ADD_DESC' => 'Indicates the data block containing the recipient to be added to the Target List',
  'LBL_ADD_TO_TARGET_LIST_ACTION_TARGET_LIST_RECORD_TEXT' => 'Target List',
  
  // SendEmailToDataBlockAction
  'LBL_SEND_EMAIL_TO_DATABLOCK_ACTION_TITLE' => 'Send email to form sender',
  'LBL_SEND_EMAIL_TO_DATABLOCK_ACTION_DESC' => 'Sends an email to the processed record (person, prospect, user or organization) contained in a data block',
  'LBL_SEND_EMAIL_TO_DATABLOCK_ACTION_RECIPIENT_BLOCK_TEXT' => 'Recipient',
  'LBL_SEND_EMAIL_TO_DATABLOCK_ACTION_RECIPIENT_BLOCK_DESC' => 'Indicates the data block containing the email recipient',
  'LBL_SEND_EMAIL_TO_DATABLOCK_ACTION_TEMPLATE_TEXT' => 'Email template',

  // SendEmailToAddressAction
  'LBL_SEND_EMAIL_TO_ADDRESS_ACTION_TITLE' => 'Send email to an address',
  'LBL_SEND_EMAIL_TO_ADDRESS_ACTION_DESC' => 'Sends an email to a specific email address',
  'LBL_SEND_EMAIL_TO_ADDRESS_ACTION_EMAIL_TEXT' => 'Email',
  'LBL_SEND_EMAIL_TO_ADDRESS_ACTION_TEMPLATE_TEXT' => 'Email template',
  
  // SendEmailToAssignedAction
  'LBL_SEND_EMAIL_TO_ASSIGNED_ACTION_TITLE' => 'Send email to assigned user',
  'LBL_SEND_EMAIL_TO_ASSIGNED_ACTION_DESC' => 'Sends an email to the assigned user of the form or a record',
  'LBL_SEND_EMAIL_TO_ASSIGNED_ACTION_SOURCE_TEXT' => 'Assigned user source',
  'LBL_SEND_EMAIL_TO_ASSIGNED_ACTION_SOURCE_DESC' => 'Indicates the record from which the assigned user will be obtained',
  'LBL_SEND_EMAIL_TO_ASSIGNED_ACTION_OPT_OWNER_TEXT' => 'Form',
  'LBL_SEND_EMAIL_TO_ASSIGNED_ACTION_OPT_RESPONSE_TEXT' => 'Form response',
  'LBL_SEND_EMAIL_TO_ASSIGNED_ACTION_OPT_DATABLOCK_TEXT' => 'Data block',
  'LBL_SEND_EMAIL_TO_ASSIGNED_ACTION_OPT_RECORD_TEXT' => 'Fixed record',
  'LBL_SEND_EMAIL_TO_ASSIGNED_ACTION_OPT_RELATE_TEXT' => 'Related field',
  'LBL_SEND_EMAIL_TO_ASSIGNED_ACTION_TEMPLATE_TEXT' => 'Email template',
  
  // RedirectAction
  'LBL_REDIRECT_ACTION_TITLE' => 'Go to a web page',
  'LBL_REDIRECT_ACTION_DESC' => "Redirects the end user's browser to a specific web page",
  'LBL_REDIRECT_ACTION_URL_TEXT' => 'Redirect URL',
  'LBL_REDIRECT_ACTION_URL_DESC' => 'Indicates the address of the web page to redirect the end user. Must include the protocol (http:// or https://).',
  'LBL_REDIRECT_ACTION_METHOD_TEXT' => 'Submission method',
  'LBL_REDIRECT_ACTION_METHOD_DESC' => 'Indicates, if necessary, how data should be sent to the redirect page.',
  'LBL_REDIRECT_ACTION_METHOD_GET_TEXT' => 'GET (data added to redirect URL)',
  'LBL_REDIRECT_ACTION_METHOD_POST_TEXT' => 'POST (data sent via hidden form)',
  'LBL_REDIRECT_ACTION_FIELDS_TEXT' => 'Fields to send',
  'LBL_REDIRECT_ACTION_FIELDS_DESC' => 'Indicates the fields to send to the redirect URL. Leave blank if there is no data to send.',
  'LBL_REDIRECT_ACTION_REDIRECTING' => 'Redirecting...',
  'LBL_REDIRECT_ACTION_SUBMIT_BUTTON' => 'Click here to continue',

  // RedirectSummaryPageAction
  'LBL_REDIRECT_SUMMARY_PAGE_ACTION_TITLE' => 'Show data summary',
  'LBL_REDIRECT_SUMMARY_PAGE_ACTION_DESC' => "Redirects the end user's browser to a page where provided data is shown",
  'LBL_REDIRECT_SUMMARY_PAGE_ACTION_TITLE_TEXT' => 'Page title',
  'LBL_REDIRECT_SUMMARY_PAGE_ACTION_TITLE_DEFAULT' => 'Summary of provided data',

  // CheckSessionAction
  'LBL_CHECK_SESSION_ACTION_TITLE' => 'Verify active session and permissions',
  'LBL_CHECK_SESSION_ACTION_DESC' => 'Blocks form processing if there is no active user session or if the user lacks permissions to create records associated with the form',
  'LBL_CHECK_SESSION_ACTION_PERMISSIONS_ERROR_MSG_TEXT' => 'Message for lack of permissions',
  'LBL_CHECK_SESSION_ACTION_PERMISSIONS_ERROR_MSG_TEXT_DEFAULT' => 'Unauthorized access. You do not have the necessary permissions to continue.',
  'LBL_CHECK_SESSION_ACTION_CHECKING' => 'Verifying access and permissions...',
  'LBL_CHECK_SESSION_ACTION_DENIED_TITLE' => '🚫 Access denied',
  'LBL_CHECK_SESSION_ACTION_ACTIVE_SESSION' => 'Active session',

  // -- DEFERRED ACTIONS --
  'LBL_PARAM_EXPIRATION_DAYS' => 'Expiration days',
  'LBL_PARAM_EXPIRATION_DAYS_DESC' => 'Number of days after which the deferred action will expire.',
  'LBL_PARAM_ALREADY_PROCESSED_TITLE' => 'Title for link already used',
  'LBL_PARAM_ALREADY_PROCESSED_TITLE_DESC' => 'Warning title that will be displayed when accessing the link if it has already been used.',
  'LBL_PARAM_ALREADY_PROCESSED_TITLE_DEFAULT' => 'Action already performed',
  'LBL_PARAM_ALREADY_PROCESSED_TEXT' => 'Text for link already used',
  'LBL_PARAM_ALREADY_PROCESSED_TEXT_DESC' => 'Warning text that will be displayed when the link is accessed if it has already been used.',
  'LBL_PARAM_ALREADY_PROCESSED_TEXT_DEFAULT' => 'This action has already been completed successfully before and does not need to be repeated.',
  'LBL_PARAM_EXPIRED_TITLE' => 'Title for expired link',
  'LBL_PARAM_EXPIRED_TITLE_DESC' => 'Warning title that will be displayed when accessing the expired link.',
  'LBL_PARAM_EXPIRED_TITLE_DEFAULT' => 'Expired link',
  'LBL_PARAM_EXPIRED_TEXT' => 'Text for expired link',
  'LBL_PARAM_EXPIRED_TEXT_DESC' => 'Warning text that will be displayed when accessing the expired link.',
  'LBL_PARAM_EXPIRED_TEXT_DEFAULT' => 'This link has expired for security reasons.',

  // EmailConfirmationAction
  'LBL_EMAIL_CONFIRMATION_ACTION_TITLE' => 'Confirm email',
  'LBL_EMAIL_CONFIRMATION_ACTION_DESC' => 'Generates a unique link and sends it via email so the user can confirm the email address.',
  'LBL_EMAIL_CONFIRMATION_ACTION_FLOW_SUCCESS' => 'Email confirmed',
  'LBL_EMAIL_CONFIRMATION_ACTION_FLOW_ERROR' => 'Error',
  'LBL_EMAIL_CONFIRMATION_ACTION_RECIPIENT_BLOCK_TEXT' => 'Recipient',
  'LBL_EMAIL_CONFIRMATION_ACTION_RECIPIENT_BLOCK_DESC' => 'Specifies the data block containing the email to be verified and to which the confirmation link will be sent.',
  'LBL_EMAIL_CONFIRMATION_ACTION_TEMPLATE_TEXT' => 'Email template',
  'LBL_EMAIL_CONFIRMATION_ACTION_TEMPLATE_DESC' => 'The email template must contain the variable {::confirmation_url::} in the message body to generate the confirmation link.',

  // PaymentRouterAction
  'LBL_PAYMENT_ROUTER_ACTION_TITLE' => 'Make payment',
  'LBL_PAYMENT_ROUTER_ACTION_DESC' => 'Processes the payment corresponding to a data block on an external platform.',
  'LBL_PAYMENT_ROUTER_ACTION_FLOW_SUCCESS' => 'Confirmed payment',
  'LBL_PAYMENT_ROUTER_ACTION_FLOW_ERROR' => 'Error',
  'LBL_PAYMENT_ROUTER_ACTION_PAYMENT_COMMITMENT_TEXT' => 'Payment Commitment',
  'LBL_PAYMENT_ROUTER_ACTION_PAYMENT_COMMITMENT_DESC' => 'Selects the data block that contains the payment commitment for which payment must be made on an external platform.',

  // -- VALIDATOR ACTIONS --
  // RegexValidatorAction
  'LBL_REGEX_VALIDATOR_ACTION_TITLE' => 'Regex validator',
  'LBL_REGEX_VALIDATOR_ACTION_DESC' => 'Validates a field according to a regular expression',
  'LBL_REGEX_VALIDATOR_ACTION_PATTERN_TEXT' => 'Regular expression',
  'LBL_REGEX_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => 'Field format is invalid',

  // EmailValidatorAction
  'LBL_EMAIL_VALIDATOR_ACTION_TITLE' => 'Email validator',
  'LBL_EMAIL_VALIDATOR_ACTION_DESC' => 'Validates that a field contains a valid email',
  'LBL_EMAIL_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => 'The email address is invalid',

  // DniValidatorAction
  'LBL_DNI_VALIDATOR_ACTION_TITLE' => 'DNI/NIF validator',
  'LBL_DNI_VALIDATOR_ACTION_DESC' => 'Validates that a field contains a valid Spanish DNI/NIF',
  'LBL_DNI_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => 'The DNI/NIF is invalid',

  // CifValidatorAction
  'LBL_CIF_VALIDATOR_ACTION_TITLE' => 'Legal entity NIF validator',
  'LBL_CIF_VALIDATOR_ACTION_DESC' => 'Validates that a field contains a valid Spanish legal entity NIF',
  'LBL_CIF_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => 'The NIF is invalid',

  // NieValidatorAction
  'LBL_NIE_VALIDATOR_ACTION_TITLE' => 'NIE validator',
  'LBL_NIE_VALIDATOR_ACTION_DESC' => 'Validates that a field contains a valid Spanish NIE',
  'LBL_NIE_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => 'The NIE is invalid',

  // CatSalutCipValidatorAction
  'LBL_CATSALUT_CIP_VALIDATOR_ACTION_TITLE' => 'CIP validator (CatSalut)',
  'LBL_CATSALUT_CIP_VALIDATOR_ACTION_DESC' => 'Validates that a field contains a valid CatSalut Personal Identification Code (CIP)',
  'LBL_CATSALUT_CIP_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => 'The CIP is invalid',

  // NafValidatorAction
  'LBL_NAF_VALIDATOR_ACTION_TITLE' => 'NUSS validator',
  'LBL_NAF_VALIDATOR_ACTION_DESC' => 'Validates that a field contains a valid Social Security affiliation number (NUSS)',
  'LBL_NAF_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => 'The Social Security affiliation number is invalid',

  // NumericValidatorAction
  'LBL_NUMERIC_VALIDATOR_ACTION_TITLE' => 'Numeric validator',
  'LBL_NUMERIC_VALIDATOR_ACTION_DESC' => 'Validates that a field contains a numeric value and optionally within a range',
  'LBL_NUMERIC_VALIDATOR_ACTION_MIN_TEXT' => 'Minimum value (optional)',
  'LBL_NUMERIC_VALIDATOR_ACTION_MAX_TEXT' => 'Maximum value (optional)',
  'LBL_NUMERIC_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => 'Value must be numeric and between the allowed values',

  // TextLengthValidatorAction
  'LBL_TEXT_LENGTH_VALIDATOR_ACTION_TITLE' => 'Text length validator',
  'LBL_TEXT_LENGTH_VALIDATOR_ACTION_DESC' => 'Validates that a field contains text with length within a range',
  'LBL_TEXT_LENGTH_VALIDATOR_ACTION_MIN_LENGTH_TEXT' => 'Minimum length (optional)',
  'LBL_TEXT_LENGTH_VALIDATOR_ACTION_MAX_LENGTH_TEXT' => 'Maximum length (optional)',
  'LBL_TEXT_LENGTH_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => 'Text must have a length between the allowed values',

  // IbanValidatorAction
  'LBL_IBAN_VALIDATOR_ACTION_TITLE' => 'IBAN validator',
  'LBL_IBAN_VALIDATOR_ACTION_DESC' => 'Validates that a field contains a valid IBAN',
  'LBL_IBAN_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => 'The IBAN is invalid',

  // PhoneValidatorAction
  'LBL_PHONE_VALIDATOR_ACTION_TITLE' => 'Phone validator',
  'LBL_PHONE_VALIDATOR_ACTION_DESC' => 'Validates that a field contains a valid Spanish phone number (at least 9 numeric digits)',
  'LBL_PHONE_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => 'The phone number is invalid',

  // SpanishZipValidatorAction
  'LBL_SPANISH_ZIP_VALIDATOR_ACTION_TITLE' => 'Zip code validator',
  'LBL_SPANISH_ZIP_VALIDATOR_ACTION_DESC' => 'Validates that a field contains a valid Spanish zip code (5 numeric digits)',
  'LBL_SPANISH_ZIP_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => 'The zip code is invalid',

  // TrueValidatorAction
  'LBL_TRUE_VALIDATOR_ACTION_TITLE' => 'Mandatory selection',
  'LBL_TRUE_VALIDATOR_ACTION_DESC' => 'Ensures a checkbox is checked (e.g., acceptance of conditions)',
  'LBL_TRUE_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => 'Acceptance of this field is necessary to continue',

  // AgeValidatorAction
  'LBL_AGE_VALIDATOR_ACTION_TITLE' => 'Age validator',
  'LBL_AGE_VALIDATOR_ACTION_DESC' => 'Calculates age from birth date and verifies it is between the allowed minimum and maximum',
  'LBL_AGE_VALIDATOR_ACTION_MIN_YEARS_TEXT' => 'Minimum age (optional)',
  'LBL_AGE_VALIDATOR_ACTION_MAX_YEARS_TEXT' => 'Maximum age (optional)',
  'LBL_AGE_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => 'Age is not within the allowed range',

  // UrlValidatorAction
  'LBL_URL_VALIDATOR_ACTION_TITLE' => 'URL validator',
  'LBL_URL_VALIDATOR_ACTION_DESC' => 'Validates that a field contains a valid URL',
  'LBL_URL_VALIDATOR_ACTION_ERROR_MESSAGE_TEXT' => 'The URL is invalid',
 );
