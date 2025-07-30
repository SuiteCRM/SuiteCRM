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
$module_name = 'stic_Bookings';
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
                'LBL_PANEL_STIC_BOOKINGS_INFORMATION' => array(
                    'newTab' => true,
                    'panelDefault' => 'expanded',
                ),
                'LBL_DETAILVIEW_PANEL1' => array(
                    'newTab' => true,
                    'panelDefault' => 'expanded',
                ),
            ),
        ),
        'panels' => array(
            'LBL_PANEL_STIC_BOOKINGS_INFORMATION' => array(
                0 => array(
                    0 => 'name',
                    1 => 'assigned_user_name',
                ),
                1=> array(
                    0 => array(
                        'name' => 'code',
                    ),
                    1 => array(
                    ),
                ),
                2 => array(
                    0 => array(
                        'name' => 'stic_bookings_contacts_name',
                    ),
                    1 => array(
                        'name' => 'stic_bookings_accounts_name',
                    ),
                ),
                3 => array(
                    0 => array(
                        'name' => 'stic_bookings_stic_families_name',
                    ),
                    1 => array(
                        'name' => 'stic_bookings_stic_payment_commitments_name',
                    ),
                ),
                4 => array(
                    0 => array(
                        'name' => 'all_day',
                        'label' => 'LBL_ALL_DAY',
                    ),
                    1 => array(
                        'name' => 'place_booking',
                        'label' => 'LBL_PLACE_BOOKING',
                    ),
                ),
                5 => array(
                    0 => array(
                        'name' => 'start_date',
                        'label' => 'LBL_START_DATE',
                    ),
                    1 => array(
                        'name' => 'end_date',
                        'label' => 'LBL_END_DATE',
                    ),
                ),
                6 => array(
                    0 => array(
                        'name' => 'planned_start_date',
                        'label' => 'LBL_PLANNED_START_DATE',
                    ),
                    1 => array(
                        'name' => 'planned_end_date',
                        'label' => 'LBL_PLANNED_END_DATE',
                    ),
                ),
                7 => array(
                    0 => array(
                        'name' => 'status',
                        'label' => 'LBL_STATUS',
                    ),
                    1 => array(
                        'name' => 'parent_name',
                        'studio' => 'visible',
                        'label' => 'LBL_FLEX_RELATE',
                    ),
                ),
                8 => array(
                    0 => array(
                        'name' => 'total_amount',
                        'label' => 'LBL_TOTAL_AMOUNT',
                    ),
                    1 => array(
                        'name' => 'copayment_amount',
                        'label' => 'LBL_TOTAL_COPAYMENT_AMOUNT',
                    ),
                ),
                9 => array(
                    0 => 'description',
                ),
            ),
            'lbl_detailview_panel1' => array(
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
