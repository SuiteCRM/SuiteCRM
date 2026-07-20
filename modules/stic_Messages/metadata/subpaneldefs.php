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
$module_name = 'stic_Messages';
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
$layout_defs[$module_name]['subpanel_setup']['notes'] = array(
    'order' => 10,
    'module' => 'Notes',
    'subpanel_name' => 'default',
    'sort_order' => 'desc',
    'sort_by' => 'date_entered',
    'title_key' => 'LBL_ATTACHMENTS_SUBPANEL_TITLE',
    'get_subpanel_data' => 'notes',
    'top_buttons' => array(),
    'list_fields' => array(
        'name' => array(
            'vname' => 'LBL_NOTE_NAME',
            'widget_class' => 'SubpanelDetailViewLink',
        ),
        'date_entered' => array(
            'vname' => 'LBL_DATE_ENTERED',
            'width' => '15%',
        ),
        'file_mime_type' => array(
            'vname' => 'LBL_FILE_MIME_TYPE',
            'width' => '15%',
        ),
        'filename' => array(
            'vname' => 'LBL_FILENAME',
            'widget_class' => 'SubpanelDetailViewLink',
            'width' => '20%',
        ),
        'created_by_name' => array(
            'vname' => 'LBL_CREATED',
            'width' => '15%',
        ),
        'edit_button' => array(
            'vname' => 'LBL_EDIT_BUTTON',
            'widget_class' => 'SubpanelEditButton',
            'module' => 'Notes',
            'width' => '5%',
        ),
        'remove_button' => array(
            'vname' => 'LBL_REMOVE',
            'widget_class' => 'SubpanelRemoveButtonNotes',
            'module' => 'Notes',
            'width' => '5%',
        ),
    ),
);