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

$dictionary['stic_Remittances'] = array(
    'table' => 'stic_remittances',
    'audited' => 1,
    'inline_edit' => 1,
    'duplicate_merge' => 1,
    'fields' => array(
        'charge_date' => array(
            'required' => 1,
            'name' => 'charge_date',
            'vname' => 'LBL_CHARGE_DATE',
            'duplicate_merge' => 'enabled',
            'merge_filter' => 'enabled',
            'type' => 'date',
            'massupdate' => 0,
            'no_default' => 0,
            'comments' => '',
            'help' => '',
            'importable' => 'required',
            'audited' => 0,
            'inline_edit' => 1,
            'reportable' => 1,
            'unified_search' => 0,
            'size' => '20',
            'options' => 'date_range_search_dom',
            'enable_range_search' => 1,
        ),
        'status' => array(
            'required' => 1,
            'name' => 'status',
            'vname' => 'LBL_STATUS',
            'duplicate_merge' => 'enabled',
            'merge_filter' => 'enabled',
            'type' => 'enum',
            'massupdate' => 0, // Status is manage with logic hooks and JS validations
            'no_default' => 0,
            'comments' => '',
            'help' => '',
            'importable' => 'required',
            'audited' => 0,
            'inline_edit' => 1,
            'reportable' => 1,
            'unified_search' => 0,
            'len' => 100,
            'size' => '20',
            'options' => 'stic_remittances_status_list',
            'studio' => 'visible',
            'dependency' => 0,
            'default' => 'open',
        ),
        'bank_account' => array(
            'required' => 0,
            'name' => 'bank_account',
            'vname' => 'LBL_BANK_ACCOUNT',
            'duplicate_merge' => 'enabled',
            'merge_filter' => 'enabled',
            'type' => 'enum',
            'massupdate' => 1,
            'importable' => 'required',
            'options' => 'stic_remittances_ibans_list',
            'no_default' => 0,
            'comments' => '',
            'help' => '',
            'audited' => 0,
            'inline_edit' => 1,
            'reportable' => 1,
            'unified_search' => 0,
            'len' => 100,
            'size' => '20',
            'studio' => 'visible',
            'dependency' => 0,
        ),
        'type' => array(
            'required' => 1,
            'name' => 'type',
            'vname' => 'LBL_TYPE',
            'duplicate_merge' => 'enabled',
            'merge_filter' => 'enabled',
            'type' => 'enum',
            'massupdate' => 1,
            'no_default' => 0,
            'comments' => '',
            'help' => '',
            'importable' => 'required',
            'audited' => 0,
            'inline_edit' => 1,
            'reportable' => 1,
            'unified_search' => 0,
            'len' => 100,
            'size' => '20',
            'options' => 'stic_remittances_types_list',
            'studio' => 'visible',
            'dependency' => 0,
        ),
        'issuing_organization' => array(
            'required' => false,
            'name' => 'issuing_organization',
            'vname' => 'LBL_ISSUING_ORGANIZATION',
            'type' => 'enum',
            'massupdate' => 1,
            'no_default' => false,
            'comments' => '',
            'help' => '',
            'importable' => 'true',
            'duplicate_merge' => 'disabled',
            'duplicate_merge_dom_value' => '0',
            'audited' => true,
            'inline_edit' => false,
            'reportable' => true,
            'unified_search' => false,
            'merge_filter' => 'disabled',
            'size' => '20',
            'options' => 'dynamic_issuing_organization_list',
            'studio' => 'visible',
        ),
        'log' => array(
            'required' => 0,
            'name' => 'log',
            'vname' => 'LBL_LOG',
            'duplicate_merge' => 'enabled',
            'merge_filter' => 'enabled',
            'type' => 'html',
            'massupdate' => 0,
            'no_default' => 0,
            'comments' => 'Errors or warnings found in the last generation of the remittance',
            'help' => '',
            'importable' => 0,
            'audited' => 0,
            'inline_edit' => 0,
            'reportable' => 1,
            'unified_search' => 0,
            'rows' => 40,
            'cols' => 90,
            'studio' => array(
                'editview' => false,
                'quickcreate' => false,
            ),
        ),
        'stic_payments_stic_remittances' => array(
            'name' => 'stic_payments_stic_remittances',
            'type' => 'link',
            'relationship' => 'stic_payments_stic_remittances',
            'source' => 'non-db',
            'module' => 'stic_Payments',
            'bean_name' => 'stic_Payments',
            'side' => 'right',
            'vname' => 'LBL_STIC_PAYMENTS_STIC_REMITTANCES_FROM_STIC_PAYMENTS_TITLE',
        ),
    ),
    'relationships' => array(
    ),
    'optimistic_locking' => 1,
    'unified_search' => true,
    'unified_search_default_enabled' => true,
);
if (!class_exists('VardefManager')) {
    require_once 'include/SugarObjects/VardefManager.php';
}
VardefManager::createVardef('stic_Remittances', 'stic_Remittances', array('basic', 'assignable', 'security_groups'));

// Set special values for SuiteCRM base fields
$dictionary['stic_Remittances']['fields']['description']['rows'] = '2'; // Make textarea fields shorter
