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
$viewdefs[$module_name] =
array(
    'DetailView' => array(
        'templateMeta' => array(
            'form' => array(
                'buttons' => array(
                    0 => 'EDIT',
                    1 => 'DUPLICATE',
                    2 => 'DELETE',
                    3 => 'FIND_DUPLICATES',
                ),
            ),
            'maxColumns' => '2',
            'widths' => array(
                0 => array(
                    'label' => '10',
                    'field' => '30',
                ),
                1 => array(
                    'label' => '10',
                    'field' => '30',
                ),
            ),
            'useTabs' => true,
            'tabDefs' => array(
                'LBL_PANEL_STIC_RESOURCES_INFORMATION' => array(
                    'newTab' => true,
                    'panelDefault' => 'expanded',
                ),
                'LBL_PANEL_PLACES' => array(
                    'newTab' => false,
                    'panelDefault' => 'expanded',
                ),
                'LBL_PANEL_RECORD_DETAILS' => array(
                    'newTab' => true,
                    'panelDefault' => 'expanded',
                ),
            ),
        ),
        'panels' => array(
            'LBL_PANEL_STIC_RESOURCES_INFORMATION' => array(
                0 => array(
                    0 => 'name',
                    1 => 'assigned_user_name',
                ),
                1 => array(
                    0 => 'code',
                    1 => array(
                        'name' => 'color',
                        'label' => 'LBL_COLOR',
                    ),
                ),
                2 => array(
                    0 => array(
                        'name' => 'status',
                        'label' => 'LBL_STATUS',
                    ),
                    1 => array(
                        'name' => 'type',
                        'studio' => 'visible',
                        'label' => 'LBL_TYPE',
                    ),
                ),
                3 => array(
                    0 => array(
                        'name' => 'hourly_rate',
                        'label' => 'LBL_HOURLY_RATE',
                    ),
                    1 => array(
                        'name' => 'daily_rate',
                        'label' => 'LBL_DAILY_RATE',
                    ),
                ),
                4 => array(
                    0 => array(
                        'name' => 'owner_contact',
                        'studio' => 'visible',
                        'label' => 'LBL_OWNER_CONTACT',
                    ),
                    1 => array(
                        'name' => 'owner_account',
                        'studio' => 'visible',
                        'label' => 'LBL_OWNER_ACCOUNT',
                    ),
                ),
                5 => array(
                    0 => array(
                        'name' => 'stic_resources_project_name',
                    ),
                    1 => array(
                        'name' => 'stic_resources_stic_centers_name',
                    ),
                ),
                6 => array(
                    0 => 'description',
                )),
            'LBL_PANEL_PLACES' => array(
                0 => array(
                    0 => array(
                        'name' => 'place_type',
                        'studio' => 'visible',
                        'label' => 'LBL_PLACE_TYPE',
                    ),
                    1 => array(
                        'name' => 'user_type',
                        'studio' => 'visible',
                        'label' => 'LBL_USER_TYPE',
                    ),
                ),
                1 => array(
                    0 => array(
                        'name' => 'gender',
                        'studio' => 'visible',
                        'label' => 'LBL_GENDER',
                    ),
                    1 => array(
                    ),
                ),
                2 => array(
                    0 => array(
                        'name' => 'amount_day_occupied',
                        'studio' => 'visible',
                        'label' => 'LBL_AMOUNT_DAY_OCCUPIED',
                    ),
                    1 => array(
                        'name' => 'amount_day_unoccupied',
                        'studio' => 'visible',
                        'label' => 'LBL_AMOUNT_DAY_UNOCCUPIED',
                    ),
                ),
                3 => array(
                    0 => array(
                        'name' => 'amount_copayment',
                        'studio' => 'visible',
                        'label' => 'LBL_AMOUNT_COPAYMENT',
                    ),
                    1 => array(
                    ),
                ),

            ),
            'LBL_PANEL_RECORD_DETAILS' => array(
                0 => array(
                    0 => array(
                        'name' => 'created_by_name',
                        'label' => 'LBL_CREATED',
                    ),
                    1 => array(
                        'name' => 'date_entered',
                        'customCode' => '{$fields.date_entered.value}',
                    ),
                ),
                1 => array(
                    0 => array(
                        'name' => 'modified_by_name',
                        'label' => 'LBL_MODIFIED_NAME',
                    ),
                    1 => array(
                        'name' => 'date_modified',
                        'customCode' => '{$fields.date_modified.value}',
                    ),
                ),
            ),
        ),
    ),
);
