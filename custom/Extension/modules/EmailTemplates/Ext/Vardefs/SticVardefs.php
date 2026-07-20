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

$dictionary['EmailTemplate']['fields']['stic_whatsapp_twilio_id_c'] = array(
    'name' => 'stic_whatsapp_twilio_id_c',
    'vname' => 'LBL_STIC_WHATSAPP_TWILIO_ID',
    'type' => 'varchar',
    'source' => 'custom_fields',
    'required' => false,
    'massupdate' => '0',
    'default' => null,
    'no_default' => false,
    'comments' => '',
    'help' => '',
    'importable' => 'false',
    'duplicate_merge' => 'enabled',
    'duplicate_merge_dom_value' => '0',
    'audited' => true,
    'reportable' => true,
    'unified_search' => false,
    'merge_filter' => 'enabled',
    'len' => 50,
    'size' => '20',
    'readonly' => true,
);

$dictionary['EmailTemplate']['fields']['stic_whatsapp_status_c'] = array(
    'name' => 'stic_whatsapp_status_c',
    'vname' => 'LBL_STIC_WHATSAPP_STATUS',
    'type' => 'enum',
    'options' => 'stic_whatsapp_status_list',
    'source' => 'custom_fields',
    'required' => false,
    'massupdate' => '0',
    'default' => null,
    'no_default' => false,
    'comments' => '',
    'help' => '',
    'importable' => 'false',
    'duplicate_merge' => 'enabled',
    'duplicate_merge_dom_value' => '0',
    'audited' => true,
    'reportable' => true,
    'unified_search' => false,
    'merge_filter' => 'enabled',
    'len' => 50,
    'size' => '20',
    'readonly' => true,
);


$dictionary['EmailTemplate']['fields']['stic_whatsapp_category_c'] = array(
    'name' => 'stic_whatsapp_category_c',
    'vname' => 'LBL_STIC_WHATSAPP_CATEGORY',
    'type' => 'enum',
    'options' => 'stic_whatsapp_category_list',
    'source' => 'custom_fields',
    'required' => false,
    'massupdate' => '0',
    'default' => '',
    'no_default' => false,
    'comments' => '',
    'help' => '',
    'importable' => 'true',
    'duplicate_merge' => 'enabled',
    'duplicate_merge_dom_value' => '1',
    'audited' => false,
    'reportable' => true,
    'unified_search' => false,
    'merge_filter' => 'enabled',
    'len' => 50,
    'size' => '20',
);

?>