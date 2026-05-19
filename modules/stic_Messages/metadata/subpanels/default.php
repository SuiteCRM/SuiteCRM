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

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

$module_name = 'stic_Messages';
$subpanel_layout = array(
    'top_buttons' => array(
        array('widget_class' => 'SubPanelTopCreateButton'),
        // array('widget_class' => 'SubPanelTopSelectButton', 'popup_module' => $module_name),
    ),

    'where' => '',

    'list_fields' => array(
        'name' => array(
            'vname' => 'LBL_NAME',
            'widget_class' => 'SubPanelDetailViewLink',
            'width' => '20%',
        ),
        'phone' => array(
            'vname' => 'LBL_PHONE',
            'width' => '10%',
        ),
        'template' => array (
            'vname' => 'LBL_TEMPLATE',
            'width' => '10%',
        ),
        'type' => array(
            'vname' => 'LBL_TYPE',
            'width' => '10%',
        ),
        'status' => array(
            'vname' => 'LBL_STATUS',
            'width' => '10%',
        ),
        'sent_date' => array(
            'vname' => 'LBL_SENT_DATE',
            'width' => '10%',
        ),
        'assigned_user_name' => array(
            'vname' => 'LBL_ASSIGNED_TO_NAME',
            'width' => '5%',
        ),
        'edit_button' => array(
            'vname' => 'LBL_EDIT_BUTTON',
            'widget_class' => 'SubPanelEditButton',
            'module' => $module_name,
            'width' => '4%',
        ),
        'quickedit_button' => array(
            'vname' => 'LBL_QUICKEDIT_BUTTON',
            'widget_class' => 'SubPanelQuickEditButton',
            'module' => $module_name,
            'width' => '4%',
        ),
        'remove_button' => array(
            'vname' => 'LBL_REMOVE',
            'widget_class' => 'SubPanelRemoveButton',
            'module' => $module_name,
            'width' => '2%',
        ),
    ),
);
