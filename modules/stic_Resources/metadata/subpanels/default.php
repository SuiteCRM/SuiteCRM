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
$module_name = 'stic_Resources';
$subpanel_layout = array(
    'top_buttons' => array(
        0 => array(
            'widget_class' => 'SubPanelTopCreateButton',
        ),
        1 => array(
            'widget_class' => 'SubPanelTopSelectButton',
            'popup_module' => 'stic_Resources',
        ),
    ),
    'where' => '',
    'list_fields' => array(
        'name' => array(
            'vname' => 'LBL_NAME',
            'widget_class' => 'SubPanelDetailViewLink',
            'width' => '15%',
            'default' => true,
        ),
        'status' => array(
            'vname' => 'LBL_STATUS',
            'width' => '10%',
            'default' => true,
        ),
        'code' => array(
            'type' => 'varchar',
            'vname' => 'LBL_CODE',
            'width' => '10%',
            'default' => true,
        ),
        'type' => array(
            'type' => 'enum',
            'studio' => 'visible',
            'vname' => 'LBL_TYPE',
            'width' => '10%',
            'default' => true,
        ),
        'color' => array(
            'type' => 'ColorPicker',
            'studio' => 'visible',
            'vname' => 'LBL_COLOR',
            'width' => '10%',
            'default' => true,
        ),
        'hourly_rate' => array(
            'type' => 'decimal',
            'vname' => 'LBL_HOURLY_RATE',
            'width' => '10%',
            'default' => true,
        ),
        'daily_rate' => array(
            'type' => 'decimal',
            'vname' => 'LBL_DAILY_RATE',
            'width' => '10%',
            'default' => true,
        ),
        'amount_day_occupied' => array(
            'type' => 'decimal',
            'vname' => 'LBL_AMOUNT_DAY_OCCUPIED',
            'width' => '10%',
            'default' => true,
        ),
        'amount_copayment' => array(
            'type' => 'decimal',
            'vname' => 'LBL_AMOUNT_COPAYMENT',
            'width' => '10%',
            'default' => true,
        ),   
        'assigned_user_name' => array(
            'link' => true,
            'type' => 'relate',
            'vname' => 'LBL_ASSIGNED_TO_NAME',
            'id' => 'ASSIGNED_USER_ID',
            'width' => '10%',
            'default' => true,
            'widget_class' => 'SubPanelDetailViewLink',
            'target_module' => 'Users',
            'target_record_key' => 'assigned_user_id',
        ),
        'close_button' => [
            'widget_class' => 'SubPanelCloseButtonstic_Resources',
            'vname' => 'LBL_LIST_CLOSE',
            'record_id' => '{stic_resources_stic_bookings_stic_resources_ida}',
            'link' => true,
            'sortable' => false,
            'default' => true,
                'width' => '1%',
        ],
        'edit_button' => array(
            'vname' => 'LBL_EDIT_BUTTON',
            'widget_class' => 'SubPanelEditButton',
            'module' => 'stic_Resources',
            'width' => '4%',
            'default' => true,
        ),
        'remove_button' => array(
            'vname' => 'LBL_REMOVE',
            'widget_class' => 'SubPanelRemoveButton',
            'module' => 'stic_Resources',
            'width' => '5%',
            'default' => true,
        ),
    ),
);
