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
$module_name = 'stic_Payment_Commitments';
$viewdefs[$module_name] =
array(
    'EditView' => array(
        'templateMeta' => array(
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
                'LBL_DEFAULT_PANEL' => array(
                    'newTab' => true,
                    'panelDefault' => 'expanded',
                ),
            ),
            'syncDetailEditViews' => false,
        ),
        'panels' => array(
            'lbl_default_panel' => array(
                0 => array(
                    0 => 'name',
                    1 => 'assigned_user_name',
                ),
                1 => array(
                    0 => array(
                        'name' => 'stic_payment_commitments_contacts_name',
                        'label' => 'LBL_STIC_PAYMENT_COMMITMENTS_CONTACTS_FROM_CONTACTS_TITLE',
                    ),
                    1 => array(
                        'name' => 'stic_payment_commitments_accounts_name',
                        'label' => 'LBL_STIC_PAYMENT_COMMITMENTS_ACCOUNTS_FROM_ACCOUNTS_TITLE',
                    ),
                ),
                2 => array(
                    0 => array(
                        'name' => 'amount',
                        'label' => 'LBL_AMOUNT',
                    ),
                    1 => array(
                        'name' => 'periodicity',
                        'studio' => 'visible',
                        'label' => 'LBL_PERIODICITY',
                    ),
                ),
                3 => array(
                    0 => array(
                        'name' => 'first_payment_date',
                        'label' => 'LBL_FIRST_PAYMENT_DATE',
                    ),
                    1 => array(
                        'name' => 'end_date',
                        'label' => 'LBL_END_DATE',
                    ),
                ),
                4 => array(
                    0 => array(
                        'name' => 'payment_method',
                        'studio' => 'visible',
                        'label' => 'LBL_PAYMENT_METHOD',
                    ),
                    1 => array(
                        'name' => 'payment_type',
                        'studio' => 'visible',
                        'label' => 'LBL_PAYMENT_TYPE',
                    ),
                ),
                5 => array(
                    0 => array(
                        'name' => 'bank_account',
                        'label' => 'LBL_BANK_ACCOUNT',
                    ),
                    1 => array(
                        'name' => 'banking_concept',
                        'label' => 'LBL_BANKING_CONCEPT',
                    ),
                ),
                6 => array(
                    0 => array(
                        'name' => 'stic_payment_commitments_campaigns_name',
                        'label' => 'LBL_STIC_PAYMENT_COMMITMENTS_CAMPAIGNS_FROM_CAMPAIGNS_TITLE',
                    ),
                    1 => array(
                        'name' => 'channel',
                        'studio' => 'visible',
                        'label' => 'LBL_CHANNEL',
                    ),
                ),
                7 => array(
                    0 => array(
                        'name' => 'segmentation',
                        'studio' => 'visible',
                        'label' => 'LBL_SEGMENTATION',
                    ),
                    1 => array(
                        'name' => 'destination',
                        'studio' => 'visible',
                        'label' => 'LBL_DESTINATION',
                    ),
                ),
                8 => array(
                    0 => array(
                        'name' => 'stic_bookings_stic_payment_commitments_name',
                        'label' => 'LBL_STIC_BOOKINGS_STIC_PAYMENT_COMMITMENTS_FROM_STIC_BOOKINGS_TITLE',
                    ),
                    1 => array(
                        'name' => 'stic_payment_commitments_project_name',
                        'label' => 'LBL_STIC_PAYMENT_COMMITMENTS_PROJECT_FROM_PROJECT_TITLE',
                    ),
                ),
                9 => array(
                    0 => array(
                        'name' => 'stic_payment_commitments_contacts_1_name',
                        'label' => 'LBL_STIC_PAYMENT_COMMITMENTS_CONTACTS_1_FROM_CONTACTS_TITLE',
                    ),
                    1 => array(
                        'name' => 'stic_payment_commitments_accounts_1_name',
                        'label' => 'LBL_STIC_PAYMENT_COMMITMENTS_ACCOUNTS_1_FROM_ACCOUNTS_TITLE',
                    ),
                ),
                10 => array(
                    0 => array(
                        'name' => 'in_kind_donation',
                        'label' => 'LBL_IN_KIND_DONATION',
                    ),
                    1 => array(
                        'name' => 'transaction_type',
                        'studio' => 'visible',
                        'label' => 'LBL_TRANSACTION_TYPE',
                    ),
                ),
                11 => array(
                    0 => 'description',
                ),
            ),
        ),
    ),
);
