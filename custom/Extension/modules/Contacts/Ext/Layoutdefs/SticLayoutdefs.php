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

$layout_defs["Contacts"]["subpanel_setup"]['stic_payment_commitments_contacts'] = array(
    'order' => 100,
    'module' => 'stic_Payment_Commitments',
    'subpanel_name' => 'defaultWithoutRemove',
    'sort_order' => 'desc',
    'sort_by' => 'first_payment_date',
    'title_key' => 'LBL_STIC_PAYMENT_COMMITMENTS_CONTACTS_FROM_STIC_PAYMENT_COMMITMENTS_TITLE',
    'get_subpanel_data' => 'stic_payment_commitments_contacts',
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

$layout_defs["Contacts"]["subpanel_setup"]['stic_payment_commitments_contacts_1'] = array(
    'order' => 100,
    'module' => 'stic_Payment_Commitments',
    'subpanel_name' => 'recipientPaymentCommitmentWithoutRemove',
    'sort_order' => 'desc',
    'sort_by' => 'first_payment_date',
    'title_key' => 'LBL_STIC_PAYMENT_COMMITMENTS_CONTACTS_1_FROM_STIC_PAYMENT_COMMITMENTS_TITLE',
    'get_subpanel_data' => 'stic_payment_commitments_contacts_1',
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

$layout_defs["Contacts"]["subpanel_setup"]['stic_payments_contacts'] = array(
    'order' => 100,
    'module' => 'stic_Payments',
    'subpanel_name' => 'defaultWithoutRemove',
    'sort_order' => 'desc',
    'sort_by' => 'payment_date',
    'title_key' => 'LBL_STIC_PAYMENTS_CONTACTS_FROM_STIC_PAYMENTS_TITLE',
    'get_subpanel_data' => 'stic_payments_contacts',
    'top_buttons' => array(
        // 0 =>
        // array (
        //   'widget_class' => 'SubPanelTopButtonQuickCreate',
        // ),
        0 => array(
            'widget_class' => 'SubPanelTopSelectButton',
            'mode' => 'MultiSelect',
        ),
    ),
);

$layout_defs["Contacts"]["subpanel_setup"]['stic_registrations_contacts'] = array(
    'order' => 100,
    'module' => 'stic_Registrations',
    'subpanel_name' => 'default',
    'sort_order' => 'desc',
    'sort_by' => 'registration_date',
    'title_key' => 'LBL_STIC_REGISTRATIONS_CONTACTS_FROM_STIC_REGISTRATIONS_TITLE',
    'get_subpanel_data' => 'stic_registrations_contacts',
    'top_buttons' => array(
        0 => array(
            'widget_class' => 'SubPanelTopButtonQuickCreate',
        ),
        // 1 => array(
        //     'widget_class' => 'SubPanelTopSelectButton',
        //     'mode' => 'MultiSelect',
        // ),
    ),
);

$layout_defs["Contacts"]["subpanel_setup"]['stic_contacts_relationships_contacts'] = array(
    'order' => 100,
    'module' => 'stic_Contacts_Relationships',
    'subpanel_name' => 'default',
    'sort_order' => 'desc',
    'sort_by' => 'start_date',
    'title_key' => 'LBL_STIC_CONTACTS_RELATIONSHIPS_CONTACTS_FROM_STIC_CONTACTS_RELATIONSHIPS_TITLE',
    'get_subpanel_data' => 'stic_contacts_relationships_contacts',
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

// stic_Assessments subpanel
$layout_defs["Contacts"]["subpanel_setup"]['stic_assessments_contacts'] = array(
    'order' => 100,
    'module' => 'stic_Assessments',
    'subpanel_name' => 'default',
    'sort_order' => 'desc',
    'sort_by' => 'assessment_date',
    'title_key' => 'LBL_STIC_ASSESSMENTS_CONTACTS_FROM_STIC_ASSESSMENTS_TITLE',
    'get_subpanel_data' => 'stic_assessments_contacts',
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

// stic_FollowUps subpanel
$layout_defs["Contacts"]["subpanel_setup"]['stic_followups_contacts'] = array(
    'order' => 100,
    'module' => 'stic_FollowUps',
    'subpanel_name' => 'default',
    'sort_order' => 'desc',
    'sort_by' => 'start_date',
    'title_key' => 'LBL_STIC_FOLLOWUPS_CONTACTS_FROM_STIC_FOLLOWUPS_TITLE',
    'get_subpanel_data' => 'stic_followups_contacts',
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

// stic_Goals subpanel
$layout_defs["Contacts"]["subpanel_setup"]['stic_goals_contacts'] = array(
    'order' => 100,
    'module' => 'stic_Goals',
    'subpanel_name' => 'default',
    'sort_order' => 'desc',
    'sort_by' => 'start_date',
    'title_key' => 'LBL_STIC_GOALS_CONTACTS_FROM_STIC_GOALS_TITLE',
    'get_subpanel_data' => 'stic_goals_contacts',
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

// stic_Personal_Environment subpanel
$layout_defs["Contacts"]["subpanel_setup"]['stic_personal_environment_contacts'] = array(
    'order' => 100,
    'module' => 'stic_Personal_Environment',
    'subpanel_name' => 'default',
    'sort_order' => 'desc',
    'sort_by' => 'start_date',
    'title_key' => 'LBL_STIC_PERSONAL_ENVIRONMENT_CONTACTS_FROM_STIC_PERSONAL_ENVIRONMENT_TITLE',
    'get_subpanel_data' => 'stic_personal_environment_contacts',
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

// stic_Personal_Environment subpanel
$layout_defs["Contacts"]["subpanel_setup"]['stic_personal_environment_contacts_1'] = array(
    'order' => 100,
    'module' => 'stic_Personal_Environment',
    'subpanel_name' => 'ForEnvironmentContacts',
    'sort_order' => 'desc',
    'sort_by' => 'start_date',
    'title_key' => 'LBL_STIC_PERSONAL_ENVIRONMENT_CONTACTS_1_FROM_STIC_PERSONAL_ENVIRONMENT_TITLE',
    'get_subpanel_data' => 'stic_personal_environment_contacts_1',
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

// Prospect Lists subpanel
$layout_defs["Contacts"]["subpanel_setup"]["prospect_lists"] = array(
    'get_subpanel_data' => 'prospect_lists',
    'module' => 'ProspectLists',
    'order' => 10,
    'sort_by' => 'name',
    'sort_order' => 'asc',
    'subpanel_name' => 'default',
    'title_key' => 'LBL_STIC_PROSPECT_LISTS_SUBPANEL_TITLE',
    'top_buttons' => array(
        array(
            'widget_class' => 'SubPanelTopSelectButton',
            'mode' => 'MultiSelect',
        ),
    ),
);

$layout_defs["Contacts"]["subpanel_setup"]['stic_sepe_actions_contacts'] = array(
    'order' => 100,
    'module' => 'stic_Sepe_Actions',
    'subpanel_name' => 'default',
    'sort_order' => 'desc',
    'sort_by' => 'start_date',
    'title_key' => 'LBL_STIC_SEPE_ACTIONS_CONTACTS_FROM_STIC_SEPE_ACTIONS_TITLE',
    'get_subpanel_data' => 'stic_sepe_actions_contacts',
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

$layout_defs["Contacts"]["subpanel_setup"]['stic_sepe_incidents_contacts'] = array(
    'order' => 100,
    'module' => 'stic_Sepe_Incidents',
    'subpanel_name' => 'default',
    'sort_order' => 'desc',
    'sort_by' => 'incident_date',
    'title_key' => 'LBL_STIC_SEPE_INCIDENTS_CONTACTS_FROM_STIC_SEPE_INCIDENTS_TITLE',
    'get_subpanel_data' => 'stic_sepe_incidents_contacts',
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

$layout_defs["Contacts"]["subpanel_setup"]['stic_job_applications_contacts'] = array(
    'order' => 100,
    'module' => 'stic_Job_Applications',
    'subpanel_name' => 'default',
    'sort_order' => 'desc',
    'sort_by' => 'start_date',
    'title_key' => 'LBL_STIC_JOB_APPLICATIONS_CONTACTS_FROM_STIC_JOB_APPLICATIONS_TITLE',
    'get_subpanel_data' => 'stic_job_applications_contacts',
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

// Bookings subpanel
$layout_defs["Contacts"]["subpanel_setup"]['stic_bookings_contacts'] = array(
    'order' => 100,
    'module' => 'stic_Bookings',
    'subpanel_name' => 'default',
    'sort_order' => 'asc',
    'sort_by' => 'id',
    'title_key' => 'LBL_STIC_BOOKINGS_CONTACTS_FROM_STIC_BOOKINGS_TITLE',
    'get_subpanel_data' => 'stic_bookings_contacts',
    'top_buttons' => array(
        0 => array(
            'widget_class' => 'SubPanelTopCreateButton',
        ),
        1 => array(
            'widget_class' => 'SubPanelTopSelectButton',
            'mode' => 'MultiSelect',
        ),
    ),
);

// Medication subpanels
$layout_defs["Contacts"]["subpanel_setup"]['stic_medication_log_contacts'] = array(
    'order' => 100,
    'module' => 'stic_Medication_Log',
    'subpanel_name' => 'default',
    'sort_order' => 'asc',
    'sort_by' => 'id',
    'title_key' => 'LBL_STIC_MEDICATION_LOG_CONTACTS_FROM_STIC_MEDICATION_LOG_TITLE',
    'get_subpanel_data' => 'stic_medication_log_contacts',
    'top_buttons' => array(
        0 => array(
            'widget_class' => 'SubPanelTopButtonQuickCreate',
        ),
    ),
);
$layout_defs["Contacts"]["subpanel_setup"]['stic_prescription_contacts'] = array(
    'order' => 100,
    'module' => 'stic_Prescription',
    'subpanel_name' => 'default',
    'sort_order' => 'asc',
    'sort_by' => 'id',
    'title_key' => 'LBL_STIC_PRESCRIPTION_CONTACTS_FROM_STIC_PRESCRIPTION_TITLE',
    'get_subpanel_data' => 'stic_prescription_contacts',
    'top_buttons' => array(
        0 => array(
            'widget_class' => 'SubPanelTopButtonQuickCreate',
        ),
    ),
);

//Grants subpanel
$layout_defs["Contacts"]["subpanel_setup"]['stic_grants_contacts'] = array(
    'order' => 100,
    'module' => 'stic_Grants',
    'subpanel_name' => 'default',
    'sort_order' => 'asc',
    'sort_by' => 'id',
    'title_key' => 'LBL_STIC_GRANTS_CONTACTS_FROM_STIC_GRANTS_TITLE',
    'get_subpanel_data' => 'stic_grants_contacts',
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

// Journal subpanel
$layout_defs["Contacts"]["subpanel_setup"]['stic_journal_contacts'] = array(
    'order' => 100,
    'module' => 'stic_Journal',
    'subpanel_name' => 'default',
    'sort_order' => 'desc',
    'sort_by' => 'name',
    'title_key' => 'LBL_STIC_JOURNAL_CONTACTS_FROM_STIC_JOURNAL_TITLE',
    'get_subpanel_data' => 'stic_journal_contacts',
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

// Training subpanel
$layout_defs["Contacts"]["subpanel_setup"]['stic_training_contacts'] = array(
    'order' => 100,
    'module' => 'stic_Training',
    'subpanel_name' => 'default',
    'sort_order' => 'asc',
    'sort_by' => 'id',
    'title_key' => 'LBL_STIC_TRAINING_CONTACTS_FROM_STIC_TRAINING_TITLE',
    'get_subpanel_data' => 'stic_training_contacts',
    'top_buttons' => array(
        0 => array(
            'widget_class' => 'SubPanelTopButtonQuickCreate',
        ),
    ),
);

// Work Experience subpanel
$layout_defs["Contacts"]["subpanel_setup"]['stic_work_experience_contacts'] = array(
    'order' => 100,
    'module' => 'stic_Work_Experience',
    'subpanel_name' => 'default',
    'sort_order' => 'asc',
    'sort_by' => 'id',
    'title_key' => 'LBL_STIC_WORK_EXPERIENCE_CONTACTS_FROM_STIC_WORK_EXPERIENCE_TITLE',
    'get_subpanel_data' => 'stic_work_experience_contacts',
    'top_buttons' => array(
        0 => array(
            'widget_class' => 'SubPanelTopButtonQuickCreate',
        ),
    ),
);

// Skills subpanel
$layout_defs["Contacts"]["subpanel_setup"]['stic_skills_contacts'] = array(
    'order' => 100,
    'module' => 'stic_Skills',
    'subpanel_name' => 'default',
    'sort_order' => 'asc',
    'sort_by' => 'id',
    'title_key' => 'LBL_STIC_SKILLS_CONTACTS_FROM_STIC_SKILLS_TITLE',
    'get_subpanel_data' => 'stic_skills_contacts',
    'top_buttons' => array(
        0 => array(
            'widget_class' => 'SubPanelTopButtonQuickCreate',
        ),
    ),
);

// New collection in activities/history
$layout_defs['Contacts']['subpanel_setup']['activities']['top_buttons'][] = array('widget_class' => 'SubPanelEditMessagesButton');
$layout_defs['Contacts']['subpanel_setup']['activities']['collection_list']['stic_Messages'] = array(
    'module' => 'stic_Messages',
    'subpanel_name' => 'ForHistory',
    'get_subpanel_data' => 'function:stic_MessagesUtils::get_stic_messages',
    'generate_select' => true,
    'function_parameters' => array('import_function_file' => 'modules/stic_Messages/Utils.php', 'return_as_array' => 'true', 'status' => "'draft'"),
);
$layout_defs['Contacts']['subpanel_setup']['history']['collection_list']['stic_Messages'] = array(
    'module' => 'stic_Messages',
    'subpanel_name' => 'ForHistory',
    'get_subpanel_data' => 'function:stic_MessagesUtils::get_stic_messages',
    'generate_select' => true,
    'function_parameters' => array('import_function_file' => 'modules/stic_Messages/Utils.php', 'return_as_array' => 'true', 'status' => "'sent', 'error'"),
);

// Signatures subpanel
$layout_defs["Contacts"]["subpanel_setup"]['stic_signers_contacts'] = array(
    'order' => 100,
    'module' => 'stic_Signers',
    'subpanel_name' => 'default',
    'sort_order' => 'desc',
    'sort_by' => 'date_modified',
    'title_key' => 'LBL_STIC_SIGNERS_CONTACTS_FROM_STIC_SIGNERS_TITLE',
    'get_subpanel_data' => 'function:stic_SignersUtils::getSticSignersForContacts',
    'function_parameters' => array(
        'import_function_file' => 'modules/stic_Signers/Utils.php',
    ),
    'top_buttons' => [], // must be defined
);


// Financial Products subpanel
$layout_defs["Contacts"]["subpanel_setup"]['stic_financial_products_contacts'] = array (
    'order' => 100,
    'module' => 'stic_Financial_Products',
    'subpanel_name' => 'default',
    'sort_order' => 'asc',
    'sort_by' => 'id',
    'title_key' => 'LBL_STIC_FINANCIAL_PRODUCTS_CONTACTS_FROM_STIC_FINANCIAL_PRODUCTS_TITLE',
    'get_subpanel_data' => 'stic_financial_products_contacts',
    'top_buttons' => 
    array (
      0 => 
      array (
        'widget_class' => 'SubPanelTopButtonQuickCreate',
      ),
      1 => 
      array (
        'widget_class' => 'SubPanelTopSelectButton',
        'mode' => 'MultiSelect',
      ),
    ),
  );

// stic_Assets subpanel
$layout_defs["Contacts"]["subpanel_setup"]['stic_assets_contacts'] = array (
  'order' => 100,
  'module' => 'stic_Assets',
  'subpanel_name' => 'default',
  'sort_order' => 'asc',
  'sort_by' => 'id',
  'title_key' => 'LBL_STIC_ASSETS_CONTACTS_FROM_STIC_ASSETS_TITLE',
  'get_subpanel_data' => 'stic_assets_contacts',
  'top_buttons' => 
  array (
    0 => 
    array (
      'widget_class' => 'SubPanelTopButtonQuickCreate',
    ),
    1 => 
    array (
      'widget_class' => 'SubPanelTopSelectButton',
      'mode' => 'MultiSelect',
    ),
  ),
);

  // Conversations subpanel
  $layout_defs["Contacts"]["subpanel_setup"]['contacts_stic_conversations'] = array (
    'order' => 100,
    'module' => 'stic_Conversations',
    'subpanel_name' => 'default',
    'sort_order' => 'asc',
    'sort_by' => 'id',
    'title_key' => 'LBL_STIC_CONVERSATIONS_SUBPANEL_TITLE',
    'get_subpanel_data' => 'contacts_stic_conversations',
    'top_buttons' => array (
        // array('widget_class' => 'SubPanelTopButtonQuickCreate'),
        // array('widget_class' => 'SubPanelTopSelectButton', 'mode' => 'MultiSelect'),
    ),
 );

$layout_defs['Contacts']['subpanel_setup']['leads']['override_subpanel_name'] = 'SticDefault';
$layout_defs['Contacts']['subpanel_setup']['opportunities']['override_subpanel_name'] = 'SticDefault';
$layout_defs['Contacts']['subpanel_setup']['documents']['override_subpanel_name'] = 'SticDefault';

// Subpanels default sorting
$layout_defs['Contacts']['subpanel_setup']['activities']['sort_order'] = 'asc';
$layout_defs['Contacts']['subpanel_setup']['activities']['sort_by'] = 'date_due';
$layout_defs['Contacts']['subpanel_setup']['history']['sort_order'] = 'desc';
$layout_defs['Contacts']['subpanel_setup']['history']['sort_by'] = 'date_modified';
$layout_defs['Contacts']['subpanel_setup']['campaigns']['sort_order'] = 'desc';
$layout_defs['Contacts']['subpanel_setup']['campaigns']['sort_by'] = 'activity_date';
$layout_defs['Contacts']['subpanel_setup']['opportunities']['sort_order'] = 'desc';
$layout_defs['Contacts']['subpanel_setup']['opportunities']['sort_by'] = 'stic_presentation_date_c';

// Hide SinergiaCRM unused subpanels
unset($layout_defs["Contacts"]["subpanel_setup"]['project']);
unset($layout_defs["Contacts"]["subpanel_setup"]['leads']);
unset($layout_defs["Contacts"]["subpanel_setup"]['contacts']);
