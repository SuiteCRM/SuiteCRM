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
if (isset($_REQUEST['action']) && $_REQUEST['action'] == 'listplaces') {
    include 'modules/stic_Resources/metadata/listviewdefs2.php';
    $listViewDefs[$module_name] = $placesListViewDefs[$module_name];
} else {
    $listViewDefs[$module_name] =
    array(
        'NAME' => array(
            'width' => '20%',
            'label' => 'LBL_NAME',
            'default' => true,
            'link' => true,
        ),
        'CODE' => array(
            'type' => 'varchar',
            'label' => 'LBL_CODE',
            'width' => '10%',
            'default' => true,
        ),
        'STATUS' => array(
            'label' => 'LBL_STATUS',
            'width' => '10%',
            'default' => true,
        ),
        'TYPE' => array(
            'type' => 'enum',
            'studio' => 'visible',
            'label' => 'LBL_TYPE',
            'width' => '10%',
            'default' => true,
        ),
        'COLOR' => array(
            'type' => 'ColorPicker',
            'label' => 'LBL_COLOR',
            'width' => '10%',
            'default' => true,
        ),
        'HOURLY_RATE' => array(
            'type' => 'decimal',
            'label' => 'LBL_HOURLY_RATE',
            'width' => '10%',
            'default' => true,
        ),
        'DAILY_RATE' => array(
            'type' => 'decimal',
            'label' => 'LBL_DAILY_RATE',
            'width' => '10%',
            'default' => true,
        ),
        'ASSIGNED_USER_NAME' => array(
            'width' => '9%',
            'label' => 'LBL_ASSIGNED_TO_NAME',
            'module' => 'Employees',
            'id' => 'ASSIGNED_USER_ID',
            'default' => true,
        ),
        'GENDER' => array(
            'type' => 'enum',
            'studio' => 'visible',
            'label' => 'LBL_GENDER',
            'width' => '10%',
            'default' => false,
        ),
        'AMOUNT_DAY_OCCUPIED' => array(
            'type' => 'decimal',
            'label' => 'LBL_AMOUNT_DAY_OCCUPIED',
            'width' => '10%',
            'default' => false,
        ),
        'AMOUNT_DAY_UNOCCUPIED' => array(
            'type' => 'decimal',
            'label' => 'LBL_AMOUNT_DAY_UNOCCUPIED',
            'width' => '10%',
            'default' => false,
        ),
        'AMOUNT_COPAYMENT' => array(
            'type' => 'decimal',
            'label' => 'LBL_AMOUNT_COPAYMENT',
            'width' => '10%',
            'default' => false,
        ),
        'OWNER_CONTACT' => array(
            'type' => 'relate',
            'label' => 'LBL_OWNER_CONTACT',
            'id' => 'CONTACT_ID_C',
            'link' => true,
            'width' => '10%',
            'default' => false,
        ),
        'OWNER_ACCOUNT' => array(
            'type' => 'relate',
            'label' => 'LBL_OWNER_ACCOUNT',
            'id' => 'ACCOUNT_ID_C',
            'link' => true,
            'width' => '10%',
            'default' => false,
        ),
        'DESCRIPTION' => array(
            'type' => 'text',
            'label' => 'LBL_DESCRIPTION',
            'sortable' => false,
            'width' => '10%',
            'default' => false,
        ),
        'PLACE_TYPE' => array(
            'type' => 'enum',
            'studio' => 'visible',
            'label' => 'LBL_PLACE_TYPE',
            'width' => '10%',
            'default' => false,
        ),
        'USER_TYPE' => array(
            'type' => 'enum',
            'studio' => 'visible',
            'label' => 'LBL_USER_TYPE',
            'width' => '10%',
            'default' => false,
        ),
        'GENDER' => array(
            'type' => 'enum',
            'studio' => 'visible',
            'label' => 'LBL_GENDER',
            'width' => '10%',
            'default' => false,
        ),
        'AMOUNT_DAY_OCCUPIED' => array(
            'type' => 'decimal',
            'label' => 'LBL_AMOUNT_DAY_OCCUPIED',
            'width' => '10%',
            'default' => false,
        ),
        'AMOUNT_DAY_UNOCCUPIED' => array(
            'type' => 'decimal',
            'label' => 'LBL_AMOUNT_DAY_UNOCCUPIED',
            'width' => '10%',
            'default' => false,
        ),
        'AMOUNT_COPAYMENT' => array(
            'type' => 'decimal',
            'label' => 'LBL_AMOUNT_COPAYMENT',
            'width' => '10%',
            'default' => false,
        ),
        'CREATED_BY_NAME' => array(
            'type' => 'relate',
            'link' => true,
            'label' => 'LBL_CREATED',
            'id' => 'CREATED_BY',
            'width' => '10%',
            'default' => false,
        ),
        'DATE_ENTERED' => array(
            'type' => 'datetime',
            'label' => 'LBL_DATE_ENTERED',
            'width' => '10%',
            'default' => false,
        ),
        'MODIFIED_BY_NAME' => array(
            'type' => 'relate',
            'link' => true,
            'label' => 'LBL_MODIFIED_NAME',
            'id' => 'MODIFIED_USER_ID',
            'width' => '10%',
            'default' => false,
        ),
        'DATE_MODIFIED' => array(
            'type' => 'datetime',
            'label' => 'LBL_DATE_MODIFIED',
            'width' => '10%',
            'default' => false,
        ),
    );
}