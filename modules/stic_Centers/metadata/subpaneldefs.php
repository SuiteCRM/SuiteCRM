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
$module_name = 'stic_Centers';

// Security Groups subpanel
$layout_defs[$module_name]['subpanel_setup']['securitygroups'] = array(
    'top_buttons' => array(array('widget_class' => 'SubPanelTopSelectButton', 'popup_module' => 'SecurityGroups', 'mode' => 'MultiSelect')),
    'order' => 900,
    'sort_by' => 'name',
    'sort_order' => 'asc',
    'module' => 'SecurityGroups',
    'refresh_page' => 1,
    'subpanel_name' => 'default',
    'get_subpanel_data' => 'SecurityGroups',
    'add_subpanel_data' => 'securitygroup_id',
    'title_key' => 'LBL_SECURITYGROUPS_SUBPANEL_TITLE',
);

// Projects subpanel
$layout_defs[$module_name]["subpanel_setup"]['stic_centers_project'] = array(
    'order' => 100,
    'module' => 'Project',
    'subpanel_name' => 'default',
    'sort_order' => 'asc',
    'sort_by' => 'name',
    'title_key' => 'LBL_STIC_CENTERS_PROJECT_FROM_PROJECT_TITLE',
    'get_subpanel_data' => 'stic_centers_project',
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

// Contacts subpanel
$layout_defs[$module_name]["subpanel_setup"]['stic_centers_contacts'] = array(
    'order' => 300,
    'module' => 'Contacts',
    'subpanel_name' => 'SticDefault',
    'sort_order' => 'asc',
    'sort_by' => 'name',
    'title_key' => 'LBL_STIC_CENTERS_CONTACTS_FROM_CONTACTS_TITLE',
    'get_subpanel_data' => 'stic_centers_contacts',
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

// Contacts Relationships subpanel
$layout_defs[$module_name]["subpanel_setup"]['stic_centers_stic_contacts_relationships'] = array(
    'order' => 400,
    'module' => 'stic_Contacts_Relationships',
    'subpanel_name' => 'default',
    'sort_order' => 'asc',
    'sort_by' => 'name',
    'title_key' => 'LBL_STIC_CENTERS_STIC_CONTACTS_RELATIONSHIPS_FROM_STIC_CONTACTS_RELATIONSHIPS_TITLE',
    'get_subpanel_data' => 'stic_centers_stic_contacts_relationships',
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

// Events subpanel
$layout_defs[$module_name]["subpanel_setup"]['stic_centers_stic_events'] = array(
    'order' => 200,
    'module' => 'stic_Events',
    'subpanel_name' => 'default',
    'sort_order' => 'asc',
    'sort_by' => 'name',
    'title_key' => 'LBL_STIC_CENTERS_STIC_EVENTS_FROM_STIC_EVENTS_TITLE',
    'get_subpanel_data' => 'stic_centers_stic_events',
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
$layout_defs[$module_name]["subpanel_setup"]['stic_journal_stic_centers'] = array(
    'order' => 100,
    'module' => 'stic_Journal',
    'subpanel_name' => 'default',
    'sort_order' => 'desc',
    'sort_by' => 'name',
    'title_key' => 'LBL_STIC_JOURNAL_STIC_CENTERS_FROM_STIC_JOURNAL_TITLE',
    'get_subpanel_data' => 'stic_journal_stic_centers',
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
// Resources subpanel
$layout_defs[$module_name]["subpanel_setup"]['stic_resources_stic_centers'] = array(
    'order' => 100,
    'module' => 'stic_Resources',
    'subpanel_name' => 'default',
    'sort_order' => 'asc',
    'sort_by' => 'name',
    'title_key' => 'LBL_STIC_RESOURCES_STIC_CENTERS_FROM_STIC_RESOURCES_TITLE',
    'get_subpanel_data' => 'stic_resources_stic_centers',
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
