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

$layout_defs["Accounts"]["subpanel_setup"]['stic_accounts_relationships_accounts'] = array(
    'order' => 100,
    'module' => 'stic_Accounts_Relationships',
    'subpanel_name' => 'default',
    'sort_order' => 'desc',
    'sort_by' => 'start_date',
    'title_key' => 'LBL_STIC_ACCOUNTS_RELATIONSHIPS_ACCOUNTS_FROM_STIC_ACCOUNTS_RELATIONSHIPS_TITLE',
    'get_subpanel_data' => 'stic_accounts_relationships_accounts',
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

$layout_defs["Accounts"]["subpanel_setup"]['stic_payment_commitments_accounts'] = array(
    'order' => 100,
    'module' => 'stic_Payment_Commitments',
    'subpanel_name' => 'defaultWithoutRemove',
    'sort_order' => 'desc',
    'sort_by' => 'first_payment_date',
    'title_key' => 'LBL_STIC_PAYMENT_COMMITMENTS_ACCOUNTS_FROM_STIC_PAYMENT_COMMITMENTS_TITLE',
    'get_subpanel_data' => 'stic_payment_commitments_accounts',
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

$layout_defs["Accounts"]["subpanel_setup"]['stic_payment_commitments_accounts_1'] = array(
    'order' => 100,
    'module' => 'stic_Payment_Commitments',
    'subpanel_name' => 'recipientPaymentCommitmentWithoutRemove',
    'sort_order' => 'desc',
    'sort_by' => 'first_payment_date',
    'title_key' => 'LBL_STIC_PAYMENT_COMMITMENTS_ACCOUNTS_1_FROM_STIC_PAYMENT_COMMITMENTS_TITLE',
    'get_subpanel_data' => 'stic_payment_commitments_accounts_1',
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

$layout_defs["Accounts"]["subpanel_setup"]['stic_payments_accounts'] = array(
    'order' => 100,
    'module' => 'stic_Payments',
    'subpanel_name' => 'defaultWithoutRemove',
    'sort_order' => 'desc',
    'sort_by' => 'payment_date',
    'title_key' => 'LBL_STIC_PAYMENTS_ACCOUNTS_FROM_STIC_PAYMENTS_TITLE',
    'get_subpanel_data' => 'stic_payments_accounts',
    'top_buttons' => array(
        0 => array(
            'widget_class' => 'SubPanelTopSelectButton',
            'mode' => 'MultiSelect',
        ),
    ),
);

$layout_defs["Accounts"]["subpanel_setup"]['stic_registrations_accounts'] = array(
    'order' => 100,
    'module' => 'stic_Registrations',
    'subpanel_name' => 'default',
    'sort_order' => 'desc',
    'sort_by' => 'registration_date',
    'title_key' => 'LBL_STIC_REGISTRATIONS_ACCOUNTS_FROM_STIC_REGISTRATIONS_TITLE',
    'get_subpanel_data' => 'stic_registrations_accounts',
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

// Prospect Lists subpanel
$layout_defs["Accounts"]["subpanel_setup"]["prospect_lists"] = array(
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

// stic_Personal_Environment subpanel
$layout_defs["Accounts"]["subpanel_setup"]['stic_personal_environment_accounts'] = array(
    'order' => 100,
    'module' => 'stic_Personal_Environment',
    'subpanel_name' => 'ForEnvironmentAccounts',
    'sort_order' => 'desc',
    'sort_by' => 'start_date',
    'title_key' => 'LBL_STIC_PERSONAL_ENVIRONMENT_ACCOUNTS_FROM_STIC_PERSONAL_ENVIRONMENT_TITLE',
    'get_subpanel_data' => 'stic_personal_environment_accounts',
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

$layout_defs["Accounts"]["subpanel_setup"]['stic_job_offers_accounts'] = array(
    'order' => 100,
    'module' => 'stic_Job_Offers',
    'subpanel_name' => 'default',
    'sort_order' => 'desc',
    'sort_by' => 'process_end_date',
    'title_key' => 'LBL_STIC_JOB_OFFERS_ACCOUNTS_FROM_STIC_JOB_OFFERS_TITLE',
    'get_subpanel_data' => 'stic_job_offers_accounts',
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
$layout_defs["Accounts"]["subpanel_setup"]['stic_bookings_accounts'] = array(
    'order' => 100,
    'module' => 'stic_Bookings',
    'subpanel_name' => 'default',
    'sort_order' => 'asc',
    'sort_by' => 'id',
    'title_key' => 'LBL_STIC_BOOKINGS_ACCOUNTS_FROM_STIC_BOOKINGS_TITLE',
    'get_subpanel_data' => 'stic_bookings_accounts',
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

// Centers subpanel
$layout_defs["Accounts"]["subpanel_setup"]['stic_centers_accounts'] = array(
    'order' => 100,
    'module' => 'stic_Centers',
    'subpanel_name' => 'ForAccounts',
    'sort_order' => 'asc',
    'sort_by' => 'name',
    'title_key' => 'LBL_STIC_CENTERS_ACCOUNTS_FROM_STIC_CENTERS_TITLE',
    'get_subpanel_data' => 'stic_centers_accounts',
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
$layout_defs["Accounts"]["subpanel_setup"]['stic_training_accounts'] = array(
    'order' => 100,
    'module' => 'stic_Training',
    'subpanel_name' => 'default',
    'sort_order' => 'asc',
    'sort_by' => 'id',
    'title_key' => 'LBL_STIC_TRAINING_ACCOUNTS_FROM_STIC_TRAINING_TITLE',
    'get_subpanel_data' => 'stic_training_accounts',
    'top_buttons' => array(
        0 => array(
            'widget_class' => 'SubPanelTopButtonQuickCreate',
        ),
    ),
);

// Work experience subpanel
$layout_defs["Accounts"]["subpanel_setup"]['stic_work_experience_accounts'] = array(
    'order' => 100,
    'module' => 'stic_Work_Experience',
    'subpanel_name' => 'default',
    'sort_order' => 'asc',
    'sort_by' => 'id',
    'title_key' => 'LBL_STIC_WORK_EXPERIENCE_ACCOUNTS_FROM_STIC_WORK_EXPERIENCE_TITLE',
    'get_subpanel_data' => 'stic_work_experience_accounts',
    'top_buttons' => array(
        0 => array(
            'widget_class' => 'SubPanelTopButtonQuickCreate',
        ),
    ),
);

// Participants subpanel
$layout_defs["Accounts"]["subpanel_setup"]['stic_group_opportunities_accounts'] = array (
    'order' => 100,
    'module' => 'stic_Group_Opportunities',
    'subpanel_name' => 'ForAccounts',
    'sort_order' => 'asc',
    'sort_by' => 'id',
    'title_key' => 'LBL_STIC_GROUP_OPPORTUNITIES_ACCOUNTS_FROM_STIC_GROUP_OPPORTUNITIES_TITLE',
    'get_subpanel_data' => 'stic_group_opportunities_accounts',
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

$layout_defs['Accounts']['subpanel_setup']['contacts']['override_subpanel_name'] = 'SticDefault';
$layout_defs['Accounts']['subpanel_setup']['leads']['override_subpanel_name'] = 'SticDefault';
$layout_defs['Accounts']['subpanel_setup']['opportunities']['override_subpanel_name'] = 'SticDefault';
$layout_defs['Accounts']['subpanel_setup']['accounts']['override_subpanel_name'] = 'SticDefault';
$layout_defs['Accounts']['subpanel_setup']['documents']['override_subpanel_name'] = 'SticDefault';

// Subpanels default sorting
$layout_defs['Accounts']['subpanel_setup']['activities']['sort_order'] = 'asc';
$layout_defs['Accounts']['subpanel_setup']['activities']['sort_by'] = 'date_due';
$layout_defs['Accounts']['subpanel_setup']['history']['sort_order'] = 'desc';
$layout_defs['Accounts']['subpanel_setup']['history']['sort_by'] = 'date_modified';
$layout_defs['Accounts']['subpanel_setup']['campaigns']['sort_order'] = 'desc';
$layout_defs['Accounts']['subpanel_setup']['campaigns']['sort_by'] = 'activity_date';
$layout_defs['Accounts']['subpanel_setup']['opportunities']['sort_order'] = 'desc';
$layout_defs['Accounts']['subpanel_setup']['opportunities']['sort_by'] = 'stic_presentation_date_c';
$layout_defs['Accounts']['subpanel_setup']['accounts']['sort_order'] = 'asc';
$layout_defs['Accounts']['subpanel_setup']['accounts']['sort_by'] = 'name';
$layout_defs['Accounts']['subpanel_setup']['contacts']['sort_order'] = 'asc';
$layout_defs['Accounts']['subpanel_setup']['contacts']['sort_by'] = 'last_name, first_name';
$layout_defs['Accounts']['subpanel_setup']['leads']['sort_order'] = 'asc';
$layout_defs['Accounts']['subpanel_setup']['leads']['sort_by'] = 'last_name, first_name';

// Hide SinergiaCRM unudes project subpanel
unset($layout_defs["Accounts"]["subpanel_setup"]['project']);

//Grants subpanel
$layout_defs["Accounts"]["subpanel_setup"]['stic_grants_accounts'] = array(
    'order' => 100,
    'module' => 'stic_Grants',
    'subpanel_name' => 'default',
    'sort_order' => 'asc',
    'sort_by' => 'id',
    'title_key' => 'LBL_STIC_GRANTS_ACCOUNTS_FROM_STIC_GRANTS_TITLE',
    'get_subpanel_data' => 'stic_grants_accounts',
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


// New collection in activities/history
$layout_defs['Accounts']['subpanel_setup']['activities']['top_buttons'][] = array('widget_class' => 'SubPanelEditMessagesButton');
$layout_defs['Accounts']['subpanel_setup']['activities']['collection_list']['stic_Messages'] = array(
    'module' => 'stic_Messages',
    'subpanel_name' => 'ForHistory',
    'get_subpanel_data' => 'function:stic_MessagesUtils::get_stic_messages',
    'generate_select' => true,
    'function_parameters' => array('import_function_file' => 'modules/stic_Messages/Utils.php', 'return_as_array' => 'true', 'status' => "'draft'"),
);
$layout_defs['Accounts']['subpanel_setup']['history']['collection_list']['stic_Messages'] = array(
    'module' => 'stic_Messages',
    'subpanel_name' => 'ForHistory',
    'get_subpanel_data' => 'function:stic_MessagesUtils::get_stic_messages',
    'generate_select' => true,
    'function_parameters' => array('import_function_file' => 'modules/stic_Messages/Utils.php', 'return_as_array' => 'true', 'status' => "'sent', 'error', 'received', 'redirected'"),
);