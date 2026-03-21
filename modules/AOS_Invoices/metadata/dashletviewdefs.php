<?php
/**
 * SuiteCRM is a customer relationship management program developed by SuiteCRM Ltd.
 * Copyright (C) 2011 - 2025 SuiteCRM Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUITECRM, SUITECRM DISCLAIMS THE
 * WARRANTY OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License
 * version 3, these Appropriate Legal Notices must retain the display of the
 * "Supercharged by SuiteCRM" logo. If the display of the logos is not reasonably
 * feasible for technical reasons, the Appropriate Legal Notices must display
 * the words "Supercharged by SuiteCRM".
 */
 
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

global $current_user;

$dashletData['AOS_InvoicesDashlet']['searchFields'] =
array(
'date_entered' =>
    array(
        'default' => ''
        ),
'billing_account' =>
    array(
        'default' => ''
        ),
'assigned_user_id' =>
    array(
        'type' => 'assigned_user_name',
        'default' => $current_user->name
        )
    );
$dashletData['AOS_InvoicesDashlet']['columns'] =
array(
'number'=>
    array(
        'width' => '5',
        'label'   => 'LBL_LIST_NUM',
        'default' => true
        ),
'name' =>
    array(
        'width'   => '20',
        'label'   => 'LBL_LIST_NAME',
        'link'    => true,
        'default' => true
        ),
        
'billing_account' =>
    array(
        'width' => '20',
        'label'   => 'LBL_BILLING_ACCOUNT'
        ),
'billing_contact' =>
    array(
        'width' => '15',
        'label'   => 'LBL_BILLING_CONTACT'
        ),
'status' =>
    array(
        'width'   => '15',
        'label'   => 'LBL_STATUS',
        'default' => true
        ),
'total_amount' =>
    array(
        'width'   => '15',
        'label'   => 'LBL_GRAND_TOTAL',
        'currency_format' => true,
        'default' => true
        ),
'due_date' =>
    array(
        'width' => '15',
        'label'   => 'LBL_DUE_DATE',
        'default' => true
        ),
'invoice_date' =>
    array(
        'width' => '15',
        'label'   => 'LBL_INVOICE_DATE'
        ),
'date_entered' =>
    array(
        'width'   => '15',
        'label'   => 'LBL_DATE_ENTERED'
        ),
'date_modified' =>
    array(
        'width'   => '15',
        'label'   => 'LBL_DATE_MODIFIED'
        ),
'created_by' =>
    array(
        'width'   => '8',
        'label'   => 'LBL_CREATED'
        ),
'assigned_user_name' =>
    array(
        'width'   => '8',
        'label'   => 'LBL_LIST_ASSIGNED_USER'
        ),
    );
