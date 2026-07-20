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

// Prospect Lists subpanel
$layout_defs["Leads"]["subpanel_setup"]["prospect_lists"] = array(
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

// Registrations subpanel
$layout_defs["Leads"]["subpanel_setup"]['stic_registrations_leads'] = array(
    'module' => 'stic_Registrations',
    'order' => 100,
    'subpanel_name' => 'default',
    'sort_order' => 'desc',
    'sort_by' => 'registration_date',
    'title_key' => 'LBL_STIC_REGISTRATIONS_LEADS_FROM_STIC_REGISTRATIONS_TITLE',
    'get_subpanel_data' => 'stic_registrations_leads',
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

// Documents subpanel
$layout_defs["Leads"]["subpanel_setup"]['leads_documents_1'] = array(
    'order' => 100,
    'module' => 'Documents',
    'subpanel_name' => 'SticDefault',
    'sort_order' => 'asc',
    'sort_by' => 'id',
    'title_key' => 'LBL_LEADS_DOCUMENTS_1_FROM_DOCUMENTS_TITLE',
    'get_subpanel_data' => 'leads_documents_1',
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
$layout_defs['Leads']['subpanel_setup']['activities']['top_buttons'][] = array('widget_class' => 'SubPanelEditMessagesButton');
$layout_defs['Leads']['subpanel_setup']['activities']['collection_list']['stic_Messages'] = array(
    'module' => 'stic_Messages',
    'subpanel_name' => 'ForHistory',
    'get_subpanel_data' => 'function:stic_MessagesUtils::get_stic_messages',
    'generate_select' => true,
    'function_parameters' => array('import_function_file' => 'modules/stic_Messages/Utils.php', 'return_as_array' => 'true', 'status' => "'draft'"),
);
$layout_defs['Leads']['subpanel_setup']['history']['collection_list']['stic_Messages'] = array(
    'module' => 'stic_Messages',
    'subpanel_name' => 'ForHistory',
    'get_subpanel_data' => 'function:stic_MessagesUtils::get_stic_messages',
    'generate_select' => true,
    'function_parameters' => array('import_function_file' => 'modules/stic_Messages/Utils.php', 'return_as_array' => 'true', 'status' => "'sent', 'error', 'received', 'redirected'"),
);

// Subpanels default sorting
$layout_defs['Leads']['subpanel_setup']['activities']['sort_order'] = 'asc';
$layout_defs['Leads']['subpanel_setup']['activities']['sort_by'] = 'date_due';
$layout_defs['Leads']['subpanel_setup']['history']['sort_order'] = 'desc';
$layout_defs['Leads']['subpanel_setup']['history']['sort_by'] = 'date_modified';
$layout_defs['Leads']['subpanel_setup']['campaigns']['sort_order'] = 'desc';
$layout_defs['Leads']['subpanel_setup']['campaigns']['sort_by'] = 'activity_date';
