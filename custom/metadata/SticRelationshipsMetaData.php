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

$dictionary["leads_documents_1"] = array(
    'true_relationship_type' => 'many-to-many',
    'from_studio' => true,
    'relationships' => array(
        'leads_documents_1' => array(
            'lhs_module' => 'Leads',
            'lhs_table' => 'leads',
            'lhs_key' => 'id',
            'rhs_module' => 'Documents',
            'rhs_table' => 'documents',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'leads_documents_1_c',
            'join_key_lhs' => 'leads_documents_1leads_ida',
            'join_key_rhs' => 'leads_documents_1documents_idb',
        ),
    ),
    'table' => 'leads_documents_1_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'leads_documents_1leads_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'leads_documents_1documents_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
        5 => array(
            'name' => 'document_revision_id',
            'type' => 'varchar',
            'len' => '36',
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'leads_documents_1spk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'leads_documents_1_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'leads_documents_1leads_ida',
                1 => 'leads_documents_1documents_idb',
            ),
        ),
    ),
);

$dictionary["prospects_documents_1"] = array(
    'true_relationship_type' => 'many-to-many',
    'from_studio' => true,
    'relationships' => array(
        'prospects_documents_1' => array(
            'lhs_module' => 'Prospects',
            'lhs_table' => 'prospects',
            'lhs_key' => 'id',
            'rhs_module' => 'Documents',
            'rhs_table' => 'documents',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'prospects_documents_1_c',
            'join_key_lhs' => 'prospects_documents_1prospects_ida',
            'join_key_rhs' => 'prospects_documents_1documents_idb',
        ),
    ),
    'table' => 'prospects_documents_1_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'prospects_documents_1prospects_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'prospects_documents_1documents_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
        5 => array(
            'name' => 'document_revision_id',
            'type' => 'varchar',
            'len' => '36',
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'prospects_documents_1spk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'prospects_documents_1_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'prospects_documents_1prospects_ida',
                1 => 'prospects_documents_1documents_idb',
            ),
        ),
    ),
);

$dictionary["project_opportunities_1"] = array(
    'true_relationship_type' => 'one-to-many',
    'from_studio' => true,
    'relationships' => array(
        'project_opportunities_1' => array(
            'lhs_module' => 'Project',
            'lhs_table' => 'project',
            'lhs_key' => 'id',
            'rhs_module' => 'Opportunities',
            'rhs_table' => 'opportunities',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'project_opportunities_1_c',
            'join_key_lhs' => 'project_opportunities_1project_ida',
            'join_key_rhs' => 'project_opportunities_1opportunities_idb',
        ),
    ),
    'table' => 'project_opportunities_1_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'project_opportunities_1project_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'project_opportunities_1opportunities_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'project_opportunities_1spk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'project_opportunities_1_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'project_opportunities_1project_ida',
            ),
        ),
        2 => array(
            'name' => 'project_opportunities_1_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'project_opportunities_1opportunities_idb',
            ),
        ),
    ),
);

$dictionary["stic_accounts_relationships_accounts"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_accounts_relationships_accounts' => array(
            'lhs_module' => 'Accounts',
            'lhs_table' => 'accounts',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Accounts_Relationships',
            'rhs_table' => 'stic_accounts_relationships',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_accounts_relationships_accounts_c',
            'join_key_lhs' => 'stic_accounts_relationships_accountsaccounts_ida',
            'join_key_rhs' => 'stic_accoub36donships_idb',
        ),
    ),
    'table' => 'stic_accounts_relationships_accounts_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_accounts_relationships_accountsaccounts_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_accoub36donships_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_accounts_relationships_accountsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_accounts_relationships_accounts_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_accounts_relationships_accountsaccounts_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_accounts_relationships_accounts_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_accoub36donships_idb',
            ),
        ),
    ),
);

$dictionary["stic_accounts_relationships_project"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_accounts_relationships_project' => array(
            'lhs_module' => 'Project',
            'lhs_table' => 'project',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Accounts_Relationships',
            'rhs_table' => 'stic_accounts_relationships',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_accounts_relationships_project_c',
            'join_key_lhs' => 'stic_accounts_relationships_projectproject_ida',
            'join_key_rhs' => 'stic_accou2675onships_idb',
        ),
    ),
    'table' => 'stic_accounts_relationships_project_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_accounts_relationships_projectproject_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_accou2675onships_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_accounts_relationships_projectspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_accounts_relationships_project_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_accounts_relationships_projectproject_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_accounts_relationships_project_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_accou2675onships_idb',
            ),
        ),
    ),
);

$dictionary["stic_assessments_contacts"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_assessments_contacts' => array(
            'lhs_module' => 'Contacts',
            'lhs_table' => 'contacts',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Assessments',
            'rhs_table' => 'stic_assessments',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_assessments_contacts_c',
            'join_key_lhs' => 'stic_assessments_contactscontacts_ida',
            'join_key_rhs' => 'stic_assessments_contactsstic_assessments_idb',
        ),
    ),
    'table' => 'stic_assessments_contacts_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_assessments_contactscontacts_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_assessments_contactsstic_assessments_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_assessments_contactsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_assessments_contacts_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_assessments_contactscontacts_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_assessments_contacts_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_assessments_contactsstic_assessments_idb',
            ),
        ),
    ),
);

$dictionary["stic_attendances_stic_registrations"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_attendances_stic_registrations' => array(
            'lhs_module' => 'stic_Registrations',
            'lhs_table' => 'stic_registrations',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Attendances',
            'rhs_table' => 'stic_attendances',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_attendances_stic_registrations_c',
            'join_key_lhs' => 'stic_attendances_stic_registrationsstic_registrations_ida',
            'join_key_rhs' => 'stic_attendances_stic_registrationsstic_attendances_idb',
        ),
    ),
    'table' => 'stic_attendances_stic_registrations_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_attendances_stic_registrationsstic_registrations_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_attendances_stic_registrationsstic_attendances_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_attendances_stic_registrationsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_attendances_stic_registrations_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_attendances_stic_registrationsstic_registrations_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_attendances_stic_registrations_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_attendances_stic_registrationsstic_attendances_idb',
            ),
        ),
    ),
);

$dictionary["stic_attendances_stic_sessions"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_attendances_stic_sessions' => array(
            'lhs_module' => 'stic_Sessions',
            'lhs_table' => 'stic_sessions',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Attendances',
            'rhs_table' => 'stic_attendances',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_attendances_stic_sessions_c',
            'join_key_lhs' => 'stic_attendances_stic_sessionsstic_sessions_ida',
            'join_key_rhs' => 'stic_attendances_stic_sessionsstic_attendances_idb',
        ),
    ),
    'table' => 'stic_attendances_stic_sessions_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_attendances_stic_sessionsstic_sessions_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_attendances_stic_sessionsstic_attendances_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_attendances_stic_sessionsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_attendances_stic_sessions_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_attendances_stic_sessionsstic_sessions_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_attendances_stic_sessions_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_attendances_stic_sessionsstic_attendances_idb',
            ),
        ),
    ),
);

$dictionary["stic_contacts_relationships_contacts"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_contacts_relationships_contacts' => array(
            'lhs_module' => 'Contacts',
            'lhs_table' => 'contacts',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Contacts_Relationships',
            'rhs_table' => 'stic_contacts_relationships',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_contacts_relationships_contacts_c',
            'join_key_lhs' => 'stic_contacts_relationships_contactscontacts_ida',
            'join_key_rhs' => 'stic_contae394onships_idb',
        ),
    ),
    'table' => 'stic_contacts_relationships_contacts_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_contacts_relationships_contactscontacts_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_contae394onships_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_contacts_relationships_contactsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_contacts_relationships_contacts_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_contacts_relationships_contactscontacts_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_contacts_relationships_contacts_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_contae394onships_idb',
            ),
        ),
    ),
);

$dictionary["stic_contacts_relationships_project"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_contacts_relationships_project' => array(
            'lhs_module' => 'Project',
            'lhs_table' => 'project',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Contacts_Relationships',
            'rhs_table' => 'stic_contacts_relationships',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_contacts_relationships_project_c',
            'join_key_lhs' => 'stic_contacts_relationships_projectproject_ida',
            'join_key_rhs' => 'stic_conta0d5aonships_idb',
        ),
    ),
    'table' => 'stic_contacts_relationships_project_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_contacts_relationships_projectproject_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_conta0d5aonships_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_contacts_relationships_projectspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_contacts_relationships_project_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_contacts_relationships_projectproject_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_contacts_relationships_project_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_conta0d5aonships_idb',
            ),
        ),
    ),
);

$dictionary["stic_events_fp_event_locations"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_events_fp_event_locations' => array(
            'lhs_module' => 'FP_Event_Locations',
            'lhs_table' => 'fp_event_locations',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Events',
            'rhs_table' => 'stic_events',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_events_fp_event_locations_c',
            'join_key_lhs' => 'stic_events_fp_event_locationsfp_event_locations_ida',
            'join_key_rhs' => 'stic_events_fp_event_locationsstic_events_idb',
        ),
    ),
    'table' => 'stic_events_fp_event_locations_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_events_fp_event_locationsfp_event_locations_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_events_fp_event_locationsstic_events_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_events_fp_event_locationsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_events_fp_event_locations_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_events_fp_event_locationsfp_event_locations_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_events_fp_event_locations_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_events_fp_event_locationsstic_events_idb',
            ),
        ),
    ),
);

$dictionary["stic_events_project"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_events_project' => array(
            'lhs_module' => 'Project',
            'lhs_table' => 'project',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Events',
            'rhs_table' => 'stic_events',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_events_project_c',
            'join_key_lhs' => 'stic_events_projectproject_ida',
            'join_key_rhs' => 'stic_events_projectstic_events_idb',
        ),
    ),
    'table' => 'stic_events_project_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_events_projectproject_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_events_projectstic_events_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_events_projectspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_events_project_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_events_projectproject_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_events_project_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_events_projectstic_events_idb',
            ),
        ),
    ),
);

$dictionary["stic_followups_contacts"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_followups_contacts' => array(
            'lhs_module' => 'Contacts',
            'lhs_table' => 'contacts',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_FollowUps',
            'rhs_table' => 'stic_followups',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_followups_contacts_c',
            'join_key_lhs' => 'stic_followups_contactscontacts_ida',
            'join_key_rhs' => 'stic_followups_contactsstic_followups_idb',
        ),
    ),
    'table' => 'stic_followups_contacts_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_followups_contactscontacts_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_followups_contactsstic_followups_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_followups_contactsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_followups_contacts_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_followups_contactscontacts_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_followups_contacts_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_followups_contactsstic_followups_idb',
            ),
        ),
    ),
);

$dictionary["stic_followups_project"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_followups_project' => array(
            'lhs_module' => 'Project',
            'lhs_table' => 'project',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_FollowUps',
            'rhs_table' => 'stic_followups',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_followups_project_c',
            'join_key_lhs' => 'stic_followups_projectproject_ida',
            'join_key_rhs' => 'stic_followups_projectstic_followups_idb',
        ),
    ),
    'table' => 'stic_followups_project_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_followups_projectproject_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_followups_projectstic_followups_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_followups_projectspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_followups_project_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_followups_projectproject_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_followups_project_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_followups_projectstic_followups_idb',
            ),
        ),
    ),
);

$dictionary["stic_followups_stic_registrations"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_followups_stic_registrations' => array(
            'lhs_module' => 'stic_Registrations',
            'lhs_table' => 'stic_registrations',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_FollowUps',
            'rhs_table' => 'stic_followups',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_followups_stic_registrations_c',
            'join_key_lhs' => 'stic_followups_stic_registrationsstic_registrations_ida',
            'join_key_rhs' => 'stic_followups_stic_registrationsstic_followups_idb',
        ),
    ),
    'table' => 'stic_followups_stic_registrations_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_followups_stic_registrationsstic_registrations_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_followups_stic_registrationsstic_followups_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_followups_stic_registrationsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_followups_stic_registrations_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_followups_stic_registrationsstic_registrations_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_followups_stic_registrations_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_followups_stic_registrationsstic_followups_idb',
            ),
        ),
    ),
);

$dictionary["stic_goals_contacts"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_goals_contacts' => array(
            'lhs_module' => 'Contacts',
            'lhs_table' => 'contacts',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Goals',
            'rhs_table' => 'stic_goals',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_goals_contacts_c',
            'join_key_lhs' => 'stic_goals_contactscontacts_ida',
            'join_key_rhs' => 'stic_goals_contactsstic_goals_idb',
        ),
    ),
    'table' => 'stic_goals_contacts_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_goals_contactscontacts_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_goals_contactsstic_goals_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_goals_contactsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_goals_contacts_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_goals_contactscontacts_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_goals_contacts_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_goals_contactsstic_goals_idb',
            ),
        ),
    ),
);

$dictionary["stic_goals_project"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_goals_project' => array(
            'lhs_module' => 'Project',
            'lhs_table' => 'project',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Goals',
            'rhs_table' => 'stic_goals',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_goals_project_c',
            'join_key_lhs' => 'stic_goals_projectproject_ida',
            'join_key_rhs' => 'stic_goals_projectstic_goals_idb',
        ),
    ),
    'table' => 'stic_goals_project_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_goals_projectproject_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_goals_projectstic_goals_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_goals_projectspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_goals_project_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_goals_projectproject_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_goals_project_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_goals_projectstic_goals_idb',
            ),
        ),
    ),
);

$dictionary["stic_goals_stic_assessments"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_goals_stic_assessments' => array(
            'lhs_module' => 'stic_Assessments',
            'lhs_table' => 'stic_assessments',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Goals',
            'rhs_table' => 'stic_goals',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_goals_stic_assessments_c',
            'join_key_lhs' => 'stic_goals_stic_assessmentsstic_assessments_ida',
            'join_key_rhs' => 'stic_goals_stic_assessmentsstic_goals_idb',
        ),
    ),
    'table' => 'stic_goals_stic_assessments_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_goals_stic_assessmentsstic_assessments_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_goals_stic_assessmentsstic_goals_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_goals_stic_assessmentsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_goals_stic_assessments_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_goals_stic_assessmentsstic_assessments_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_goals_stic_assessments_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_goals_stic_assessmentsstic_goals_idb',
            ),
        ),
    ),
);

$dictionary["stic_goals_stic_followups"] = array(
    'true_relationship_type' => 'many-to-many',
    'relationships' => array(
        'stic_goals_stic_followups' => array(
            'lhs_module' => 'stic_Goals',
            'lhs_table' => 'stic_goals',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_FollowUps',
            'rhs_table' => 'stic_followups',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_goals_stic_followups_c',
            'join_key_lhs' => 'stic_goals_stic_followupsstic_goals_ida',
            'join_key_rhs' => 'stic_goals_stic_followupsstic_followups_idb',
        ),
    ),
    'table' => 'stic_goals_stic_followups_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_goals_stic_followupsstic_goals_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_goals_stic_followupsstic_followups_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_goals_stic_followupsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_goals_stic_followups_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_goals_stic_followupsstic_goals_ida',
                1 => 'stic_goals_stic_followupsstic_followups_idb',
            ),
        ),
    ),
);

$dictionary["stic_goals_stic_goals"] = array(
    'true_relationship_type' => 'many-to-many',
    'relationships' => array(
        'stic_goals_stic_goals' => array(
            'lhs_module' => 'stic_Goals',
            'lhs_table' => 'stic_goals',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Goals',
            'rhs_table' => 'stic_goals',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_goals_stic_goals_c',
            'join_key_lhs' => 'stic_goals_stic_goalsstic_goals_ida',
            'join_key_rhs' => 'stic_goals_stic_goalsstic_goals_idb',
        ),
    ),
    'table' => 'stic_goals_stic_goals_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_goals_stic_goalsstic_goals_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_goals_stic_goalsstic_goals_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_goals_stic_goalsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_goals_stic_goals_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_goals_stic_goalsstic_goals_ida',
                1 => 'stic_goals_stic_goalsstic_goals_idb',
            ),
        ),
    ),
);

$dictionary["stic_goals_stic_registrations"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_goals_stic_registrations' => array(
            'lhs_module' => 'stic_Registrations',
            'lhs_table' => 'stic_registrations',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Goals',
            'rhs_table' => 'stic_goals',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_goals_stic_registrations_c',
            'join_key_lhs' => 'stic_goals_stic_registrationsstic_registrations_ida',
            'join_key_rhs' => 'stic_goals_stic_registrationsstic_goals_idb',
        ),
    ),
    'table' => 'stic_goals_stic_registrations_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_goals_stic_registrationsstic_registrations_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_goals_stic_registrationsstic_goals_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_goals_stic_registrationsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_goals_stic_registrations_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_goals_stic_registrationsstic_registrations_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_goals_stic_registrations_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_goals_stic_registrationsstic_goals_idb',
            ),
        ),
    ),
);

$dictionary["stic_job_applications_activities_calls"] = array(
    'relationships' => array(
        'stic_job_applications_activities_calls' => array(
            'lhs_module' => 'stic_Job_Applications',
            'lhs_table' => 'stic_job_applications',
            'lhs_key' => 'id',
            'rhs_module' => 'Calls',
            'rhs_table' => 'calls',
            'rhs_key' => 'parent_id',
            'relationship_type' => 'one-to-many',
            'relationship_role_column' => 'parent_type',
            'relationship_role_column_value' => 'stic_Job_Applications',
        ),
    ),
    'fields' => '',
    'indices' => '',
    'table' => '',
);

$dictionary["stic_job_applications_activities_emails"] = array(
    'relationships' => array(
        'stic_job_applications_activities_emails' => array(
            'lhs_module' => 'stic_Job_Applications',
            'lhs_table' => 'stic_job_applications',
            'lhs_key' => 'id',
            'rhs_module' => 'Emails',
            'rhs_table' => 'emails',
            'rhs_key' => 'parent_id',
            'relationship_type' => 'one-to-many',
            'relationship_role_column' => 'parent_type',
            'relationship_role_column_value' => 'stic_Job_Applications',
        ),
    ),
    'fields' => '',
    'indices' => '',
    'table' => '',
);

$dictionary["stic_job_applications_activities_meetings"] = array(
    'relationships' => array(
        'stic_job_applications_activities_meetings' => array(
            'lhs_module' => 'stic_Job_Applications',
            'lhs_table' => 'stic_job_applications',
            'lhs_key' => 'id',
            'rhs_module' => 'Meetings',
            'rhs_table' => 'meetings',
            'rhs_key' => 'parent_id',
            'relationship_type' => 'one-to-many',
            'relationship_role_column' => 'parent_type',
            'relationship_role_column_value' => 'stic_Job_Applications',
        ),
    ),
    'fields' => '',
    'indices' => '',
    'table' => '',
);

$dictionary["stic_job_applications_activities_notes"] = array(
    'relationships' => array(
        'stic_job_applications_activities_notes' => array(
            'lhs_module' => 'stic_Job_Applications',
            'lhs_table' => 'stic_job_applications',
            'lhs_key' => 'id',
            'rhs_module' => 'Notes',
            'rhs_table' => 'notes',
            'rhs_key' => 'parent_id',
            'relationship_type' => 'one-to-many',
            'relationship_role_column' => 'parent_type',
            'relationship_role_column_value' => 'stic_Job_Applications',
        ),
    ),
    'fields' => '',
    'indices' => '',
    'table' => '',
);

$dictionary["stic_job_applications_activities_tasks"] = array(
    'relationships' => array(
        'stic_job_applications_activities_tasks' => array(
            'lhs_module' => 'stic_Job_Applications',
            'lhs_table' => 'stic_job_applications',
            'lhs_key' => 'id',
            'rhs_module' => 'Tasks',
            'rhs_table' => 'tasks',
            'rhs_key' => 'parent_id',
            'relationship_type' => 'one-to-many',
            'relationship_role_column' => 'parent_type',
            'relationship_role_column_value' => 'stic_Job_Applications',
        ),
    ),
    'fields' => '',
    'indices' => '',
    'table' => '',
);

$dictionary["stic_job_applications_contacts"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_job_applications_contacts' => array(
            'lhs_module' => 'Contacts',
            'lhs_table' => 'contacts',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Job_Applications',
            'rhs_table' => 'stic_job_applications',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_job_applications_contacts_c',
            'join_key_lhs' => 'stic_job_applications_contactscontacts_ida',
            'join_key_rhs' => 'stic_job_applications_contactsstic_job_applications_idb',
        ),
    ),
    'table' => 'stic_job_applications_contacts_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_job_applications_contactscontacts_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_job_applications_contactsstic_job_applications_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_job_applications_contactsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_job_applications_contacts_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_job_applications_contactscontacts_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_job_applications_contacts_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_job_applications_contactsstic_job_applications_idb',
            ),
        ),
    ),
);

$dictionary["stic_job_applications_documents"] = array(
    'true_relationship_type' => 'many-to-many',
    'relationships' => array(
        'stic_job_applications_documents' => array(
            'lhs_module' => 'stic_Job_Applications',
            'lhs_table' => 'stic_job_applications',
            'lhs_key' => 'id',
            'rhs_module' => 'Documents',
            'rhs_table' => 'documents',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_job_applications_documents_c',
            'join_key_lhs' => 'stic_job_applications_documentsstic_job_applications_ida',
            'join_key_rhs' => 'stic_job_applications_documentsdocuments_idb',
        ),
    ),
    'table' => 'stic_job_applications_documents_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_job_applications_documentsstic_job_applications_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_job_applications_documentsdocuments_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
        5 => array(
            'name' => 'document_revision_id',
            'type' => 'varchar',
            'len' => '36',
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_job_applications_documentsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_job_applications_documents_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_job_applications_documentsstic_job_applications_ida',
                1 => 'stic_job_applications_documentsdocuments_idb',
            ),
        ),
    ),
);

$dictionary["stic_job_applications_stic_job_offers"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_job_applications_stic_job_offers' => array(
            'lhs_module' => 'stic_Job_Offers',
            'lhs_table' => 'stic_job_offers',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Job_Applications',
            'rhs_table' => 'stic_job_applications',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_job_applications_stic_job_offers_c',
            'join_key_lhs' => 'stic_job_applications_stic_job_offersstic_job_offers_ida',
            'join_key_rhs' => 'stic_job_applications_stic_job_offersstic_job_applications_idb',
        ),
    ),
    'table' => 'stic_job_applications_stic_job_offers_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_job_applications_stic_job_offersstic_job_offers_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_job_applications_stic_job_offersstic_job_applications_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_job_applications_stic_job_offersspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_job_applications_stic_job_offers_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_job_applications_stic_job_offersstic_job_offers_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_job_applications_stic_job_offers_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_job_applications_stic_job_offersstic_job_applications_idb',
            ),
        ),
    ),
);

$dictionary["stic_job_offers_accounts"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_job_offers_accounts' => array(
            'lhs_module' => 'Accounts',
            'lhs_table' => 'accounts',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Job_Offers',
            'rhs_table' => 'stic_job_offers',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_job_offers_accounts_c',
            'join_key_lhs' => 'stic_job_offers_accountsaccounts_ida',
            'join_key_rhs' => 'stic_job_offers_accountsstic_job_offers_idb',
        ),
    ),
    'table' => 'stic_job_offers_accounts_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_job_offers_accountsaccounts_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_job_offers_accountsstic_job_offers_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_job_offers_accountsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_job_offers_accounts_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_job_offers_accountsaccounts_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_job_offers_accounts_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_job_offers_accountsstic_job_offers_idb',
            ),
        ),
    ),
);

$dictionary["stic_job_offers_activities_calls"] = array(
    'relationships' => array(
        'stic_job_offers_activities_calls' => array(
            'lhs_module' => 'stic_Job_Offers',
            'lhs_table' => 'stic_job_offers',
            'lhs_key' => 'id',
            'rhs_module' => 'Calls',
            'rhs_table' => 'calls',
            'rhs_key' => 'parent_id',
            'relationship_type' => 'one-to-many',
            'relationship_role_column' => 'parent_type',
            'relationship_role_column_value' => 'stic_Job_Offers',
        ),
    ),
    'fields' => '',
    'indices' => '',
    'table' => '',
);

$dictionary["stic_job_offers_activities_emails"] = array(
    'relationships' => array(
        'stic_job_offers_activities_emails' => array(
            'lhs_module' => 'stic_Job_Offers',
            'lhs_table' => 'stic_job_offers',
            'lhs_key' => 'id',
            'rhs_module' => 'Emails',
            'rhs_table' => 'emails',
            'rhs_key' => 'parent_id',
            'relationship_type' => 'one-to-many',
            'relationship_role_column' => 'parent_type',
            'relationship_role_column_value' => 'stic_Job_Offers',
        ),
    ),
    'fields' => '',
    'indices' => '',
    'table' => '',
);

$dictionary["stic_job_offers_activities_meetings"] = array(
    'relationships' => array(
        'stic_job_offers_activities_meetings' => array(
            'lhs_module' => 'stic_Job_Offers',
            'lhs_table' => 'stic_job_offers',
            'lhs_key' => 'id',
            'rhs_module' => 'Meetings',
            'rhs_table' => 'meetings',
            'rhs_key' => 'parent_id',
            'relationship_type' => 'one-to-many',
            'relationship_role_column' => 'parent_type',
            'relationship_role_column_value' => 'stic_Job_Offers',
        ),
    ),
    'fields' => '',
    'indices' => '',
    'table' => '',
);

$dictionary["stic_job_offers_activities_notes"] = array(
    'relationships' => array(
        'stic_job_offers_activities_notes' => array(
            'lhs_module' => 'stic_Job_Offers',
            'lhs_table' => 'stic_job_offers',
            'lhs_key' => 'id',
            'rhs_module' => 'Notes',
            'rhs_table' => 'notes',
            'rhs_key' => 'parent_id',
            'relationship_type' => 'one-to-many',
            'relationship_role_column' => 'parent_type',
            'relationship_role_column_value' => 'stic_Job_Offers',
        ),
    ),
    'fields' => '',
    'indices' => '',
    'table' => '',
);

$dictionary["stic_job_offers_activities_tasks"] = array(
    'relationships' => array(
        'stic_job_offers_activities_tasks' => array(
            'lhs_module' => 'stic_Job_Offers',
            'lhs_table' => 'stic_job_offers',
            'lhs_key' => 'id',
            'rhs_module' => 'Tasks',
            'rhs_table' => 'tasks',
            'rhs_key' => 'parent_id',
            'relationship_type' => 'one-to-many',
            'relationship_role_column' => 'parent_type',
            'relationship_role_column_value' => 'stic_Job_Offers',
        ),
    ),
    'fields' => '',
    'indices' => '',
    'table' => '',
);

$dictionary["stic_job_offers_documents"] = array(
    'true_relationship_type' => 'many-to-many',
    'relationships' => array(
        'stic_job_offers_documents' => array(
            'lhs_module' => 'stic_Job_Offers',
            'lhs_table' => 'stic_job_offers',
            'lhs_key' => 'id',
            'rhs_module' => 'Documents',
            'rhs_table' => 'documents',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_job_offers_documents_c',
            'join_key_lhs' => 'stic_job_offers_documentsstic_job_offers_ida',
            'join_key_rhs' => 'stic_job_offers_documentsdocuments_idb',
        ),
    ),
    'table' => 'stic_job_offers_documents_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_job_offers_documentsstic_job_offers_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_job_offers_documentsdocuments_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
        5 => array(
            'name' => 'document_revision_id',
            'type' => 'varchar',
            'len' => '36',
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_job_offers_documentsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_job_offers_documents_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_job_offers_documentsstic_job_offers_ida',
                1 => 'stic_job_offers_documentsdocuments_idb',
            ),
        ),
    ),
);

$dictionary["stic_payment_commitments_accounts_1"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_payment_commitments_accounts_1' => array(
            'lhs_module' => 'Accounts',
            'lhs_table' => 'accounts',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Payment_Commitments',
            'rhs_table' => 'stic_payment_commitments',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_payment_commitments_accounts_1_c',
            'join_key_lhs' => 'stic_payment_commitments_accounts_1accounts_ida',
            'join_key_rhs' => 'stic_payment_commitments_accounts_1stic_payment_commitments_idb',
        ),
    ),
    'table' => 'stic_payment_commitments_accounts_1_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_payment_commitments_accounts_1accounts_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_payment_commitments_accounts_1stic_payment_commitments_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_payment_commitments_accounts_1spk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_payment_commitments_accounts_1_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_payment_commitments_accounts_1accounts_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_payment_commitments_accounts_1_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_payment_commitments_accounts_1stic_payment_commitments_idb',
            ),
        ),
    ),
);

$dictionary["stic_payment_commitments_accounts"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_payment_commitments_accounts' => array(
            'lhs_module' => 'Accounts',
            'lhs_table' => 'accounts',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Payment_Commitments',
            'rhs_table' => 'stic_payment_commitments',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_payment_commitments_accounts_c',
            'join_key_lhs' => 'stic_payment_commitments_accountsaccounts_ida',
            'join_key_rhs' => 'stic_payment_commitments_accountsstic_payment_commitments_idb',
        ),
    ),
    'table' => 'stic_payment_commitments_accounts_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_payment_commitments_accountsaccounts_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_payment_commitments_accountsstic_payment_commitments_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_payment_commitments_accountsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_payment_commitments_accounts_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_payment_commitments_accountsaccounts_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_payment_commitments_accounts_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_payment_commitments_accountsstic_payment_commitments_idb',
            ),
        ),
    ),
);

$dictionary["stic_payment_commitments_campaigns"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_payment_commitments_campaigns' => array(
            'lhs_module' => 'Campaigns',
            'lhs_table' => 'campaigns',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Payment_Commitments',
            'rhs_table' => 'stic_payment_commitments',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_payment_commitments_campaigns_c',
            'join_key_lhs' => 'stic_payment_commitments_campaignscampaigns_ida',
            'join_key_rhs' => 'stic_payment_commitments_campaignsstic_payment_commitments_idb',
        ),
    ),
    'table' => 'stic_payment_commitments_campaigns_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_payment_commitments_campaignscampaigns_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_payment_commitments_campaignsstic_payment_commitments_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_payment_commitments_campaignsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_payment_commitments_campaigns_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_payment_commitments_campaignscampaigns_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_payment_commitments_campaigns_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_payment_commitments_campaignsstic_payment_commitments_idb',
            ),
        ),
    ),
);

$dictionary["stic_payment_commitments_contacts_1"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_payment_commitments_contacts_1' => array(
            'lhs_module' => 'Contacts',
            'lhs_table' => 'contacts',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Payment_Commitments',
            'rhs_table' => 'stic_payment_commitments',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_payment_commitments_contacts_1_c',
            'join_key_lhs' => 'stic_payment_commitments_contacts_1contacts_ida',
            'join_key_rhs' => 'stic_payment_commitments_contacts_1stic_payment_commitments_idb',
        ),
    ),
    'table' => 'stic_payment_commitments_contacts_1_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_payment_commitments_contacts_1contacts_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_payment_commitments_contacts_1stic_payment_commitments_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_payment_commitments_contacts_1spk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_payment_commitments_contacts_1_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_payment_commitments_contacts_1contacts_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_payment_commitments_contacts_1_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_payment_commitments_contacts_1stic_payment_commitments_idb',
            ),
        ),
    ),
);

$dictionary["stic_payment_commitments_contacts"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_payment_commitments_contacts' => array(
            'lhs_module' => 'Contacts',
            'lhs_table' => 'contacts',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Payment_Commitments',
            'rhs_table' => 'stic_payment_commitments',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_payment_commitments_contacts_c',
            'join_key_lhs' => 'stic_payment_commitments_contactscontacts_ida',
            'join_key_rhs' => 'stic_payment_commitments_contactsstic_payment_commitments_idb',
        ),
    ),
    'table' => 'stic_payment_commitments_contacts_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_payment_commitments_contactscontacts_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_payment_commitments_contactsstic_payment_commitments_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_payment_commitments_contactsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_payment_commitments_contacts_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_payment_commitments_contactscontacts_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_payment_commitments_contacts_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_payment_commitments_contactsstic_payment_commitments_idb',
            ),
        ),
    ),
);

$dictionary["stic_payment_commitments_project"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_payment_commitments_project' => array(
            'lhs_module' => 'Project',
            'lhs_table' => 'project',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Payment_Commitments',
            'rhs_table' => 'stic_payment_commitments',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_payment_commitments_project_c',
            'join_key_lhs' => 'stic_payment_commitments_projectproject_ida',
            'join_key_rhs' => 'stic_payment_commitments_projectstic_payment_commitments_idb',
        ),
    ),
    'table' => 'stic_payment_commitments_project_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_payment_commitments_projectproject_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_payment_commitments_projectstic_payment_commitments_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_payment_commitments_projectspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_payment_commitments_project_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_payment_commitments_projectproject_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_payment_commitments_project_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_payment_commitments_projectstic_payment_commitments_idb',
            ),
        ),
    ),
);

$dictionary["stic_payments_accounts"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_payments_accounts' => array(
            'lhs_module' => 'Accounts',
            'lhs_table' => 'accounts',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Payments',
            'rhs_table' => 'stic_payments',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_payments_accounts_c',
            'join_key_lhs' => 'stic_payments_accountsaccounts_ida',
            'join_key_rhs' => 'stic_payments_accountsstic_payments_idb',
        ),
    ),
    'table' => 'stic_payments_accounts_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_payments_accountsaccounts_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_payments_accountsstic_payments_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_payments_accountsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_payments_accounts_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_payments_accountsaccounts_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_payments_accounts_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_payments_accountsstic_payments_idb',
            ),
        ),
    ),
);

$dictionary["stic_payments_activities_calls"] = array(
    'relationships' => array(
        'stic_payments_activities_calls' => array(
            'lhs_module' => 'stic_Payments',
            'lhs_table' => 'stic_payments',
            'lhs_key' => 'id',
            'rhs_module' => 'Calls',
            'rhs_table' => 'calls',
            'rhs_key' => 'parent_id',
            'relationship_type' => 'one-to-many',
            'relationship_role_column' => 'parent_type',
            'relationship_role_column_value' => 'stic_Payments',
        ),
    ),
    'fields' => '',
    'indices' => '',
    'table' => '',
);

$dictionary["stic_payments_activities_emails"] = array(
    'relationships' => array(
        'stic_payments_activities_emails' => array(
            'lhs_module' => 'stic_Payments',
            'lhs_table' => 'stic_payments',
            'lhs_key' => 'id',
            'rhs_module' => 'Emails',
            'rhs_table' => 'emails',
            'rhs_key' => 'parent_id',
            'relationship_type' => 'one-to-many',
            'relationship_role_column' => 'parent_type',
            'relationship_role_column_value' => 'stic_Payments',
        ),
    ),
    'fields' => '',
    'indices' => '',
    'table' => '',
);

$dictionary["stic_payments_activities_meetings"] = array(
    'relationships' => array(
        'stic_payments_activities_meetings' => array(
            'lhs_module' => 'stic_Payments',
            'lhs_table' => 'stic_payments',
            'lhs_key' => 'id',
            'rhs_module' => 'Meetings',
            'rhs_table' => 'meetings',
            'rhs_key' => 'parent_id',
            'relationship_type' => 'one-to-many',
            'relationship_role_column' => 'parent_type',
            'relationship_role_column_value' => 'stic_Payments',
        ),
    ),
    'fields' => '',
    'indices' => '',
    'table' => '',
);

$dictionary["stic_payments_activities_notes"] = array(
    'relationships' => array(
        'stic_payments_activities_notes' => array(
            'lhs_module' => 'stic_Payments',
            'lhs_table' => 'stic_payments',
            'lhs_key' => 'id',
            'rhs_module' => 'Notes',
            'rhs_table' => 'notes',
            'rhs_key' => 'parent_id',
            'relationship_type' => 'one-to-many',
            'relationship_role_column' => 'parent_type',
            'relationship_role_column_value' => 'stic_Payments',
        ),
    ),
    'fields' => '',
    'indices' => '',
    'table' => '',
);

$dictionary["stic_payments_activities_tasks"] = array(
    'relationships' => array(
        'stic_payments_activities_tasks' => array(
            'lhs_module' => 'stic_Payments',
            'lhs_table' => 'stic_payments',
            'lhs_key' => 'id',
            'rhs_module' => 'Tasks',
            'rhs_table' => 'tasks',
            'rhs_key' => 'parent_id',
            'relationship_type' => 'one-to-many',
            'relationship_role_column' => 'parent_type',
            'relationship_role_column_value' => 'stic_Payments',
        ),
    ),
    'fields' => '',
    'indices' => '',
    'table' => '',
);

$dictionary["stic_payments_contacts"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_payments_contacts' => array(
            'lhs_module' => 'Contacts',
            'lhs_table' => 'contacts',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Payments',
            'rhs_table' => 'stic_payments',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_payments_contacts_c',
            'join_key_lhs' => 'stic_payments_contactscontacts_ida',
            'join_key_rhs' => 'stic_payments_contactsstic_payments_idb',
        ),
    ),
    'table' => 'stic_payments_contacts_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_payments_contactscontacts_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_payments_contactsstic_payments_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_payments_contactsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_payments_contacts_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_payments_contactscontacts_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_payments_contacts_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_payments_contactsstic_payments_idb',
            ),
        ),
    ),
);

$dictionary["stic_payments_stic_payment_commitments"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_payments_stic_payment_commitments' => array(
            'lhs_module' => 'stic_Payment_Commitments',
            'lhs_table' => 'stic_payment_commitments',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Payments',
            'rhs_table' => 'stic_payments',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_payments_stic_payment_commitments_c',
            'join_key_lhs' => 'stic_paymebfe2itments_ida',
            'join_key_rhs' => 'stic_payments_stic_payment_commitmentsstic_payments_idb',
        ),
    ),
    'table' => 'stic_payments_stic_payment_commitments_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_paymebfe2itments_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_payments_stic_payment_commitmentsstic_payments_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_payments_stic_payment_commitmentsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_payments_stic_payment_commitments_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_paymebfe2itments_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_payments_stic_payment_commitments_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_payments_stic_payment_commitmentsstic_payments_idb',
            ),
        ),
    ),
);

$dictionary["stic_payments_stic_registrations"] = array(
    'true_relationship_type' => 'one-to-one',
    'relationships' => array(
        'stic_payments_stic_registrations' => array(
            'lhs_module' => 'stic_Payments',
            'lhs_table' => 'stic_payments',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Registrations',
            'rhs_table' => 'stic_registrations',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_payments_stic_registrations_c',
            'join_key_lhs' => 'stic_payments_stic_registrationsstic_payments_ida',
            'join_key_rhs' => 'stic_payments_stic_registrationsstic_registrations_idb',
        ),
    ),
    'table' => 'stic_payments_stic_registrations_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_payments_stic_registrationsstic_payments_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_payments_stic_registrationsstic_registrations_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_payments_stic_registrationsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_payments_stic_registrations_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_payments_stic_registrationsstic_payments_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_payments_stic_registrations_idb2',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_payments_stic_registrationsstic_registrations_idb',
            ),
        ),
    ),
);

$dictionary["stic_payments_stic_remittances"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_payments_stic_remittances' => array(
            'lhs_module' => 'stic_Remittances',
            'lhs_table' => 'stic_remittances',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Payments',
            'rhs_table' => 'stic_payments',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_payments_stic_remittances_c',
            'join_key_lhs' => 'stic_payments_stic_remittancesstic_remittances_ida',
            'join_key_rhs' => 'stic_payments_stic_remittancesstic_payments_idb',
        ),
    ),
    'table' => 'stic_payments_stic_remittances_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_payments_stic_remittancesstic_remittances_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_payments_stic_remittancesstic_payments_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_payments_stic_remittancesspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_payments_stic_remittances_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_payments_stic_remittancesstic_remittances_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_payments_stic_remittances_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_payments_stic_remittancesstic_payments_idb',
            ),
        ),
    ),
);

$dictionary["stic_personal_environment_accounts"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_personal_environment_accounts' => array(
            'lhs_module' => 'Accounts',
            'lhs_table' => 'accounts',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Personal_Environment',
            'rhs_table' => 'stic_personal_environment',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_personal_environment_accounts_c',
            'join_key_lhs' => 'stic_personal_environment_accountsaccounts_ida',
            'join_key_rhs' => 'stic_persofe40onment_idb',
        ),
    ),
    'table' => 'stic_personal_environment_accounts_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_personal_environment_accountsaccounts_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_persofe40onment_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_personal_environment_accountsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_personal_environment_accounts_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_personal_environment_accountsaccounts_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_personal_environment_accounts_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_persofe40onment_idb',
            ),
        ),
    ),
);

$dictionary["stic_personal_environment_contacts_1"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_personal_environment_contacts_1' => array(
            'lhs_module' => 'Contacts',
            'lhs_table' => 'contacts',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Personal_Environment',
            'rhs_table' => 'stic_personal_environment',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_personal_environment_contacts_1_c',
            'join_key_lhs' => 'stic_personal_environment_contacts_1contacts_ida',
            'join_key_rhs' => 'stic_perso369eonment_idb',
        ),
    ),
    'table' => 'stic_payments_stic_registrations_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_payments_stic_registrationsstic_payments_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_payments_stic_registrationsstic_registrations_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_payments_stic_registrationsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_payments_stic_registrations_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_payments_stic_registrationsstic_payments_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_payments_stic_registrations_idb2',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_payments_stic_registrationsstic_registrations_idb',
            ),
        ),
    ),
);

$dictionary["stic_payments_stic_remittances"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_payments_stic_remittances' => array(
            'lhs_module' => 'stic_Remittances',
            'lhs_table' => 'stic_remittances',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Payments',
            'rhs_table' => 'stic_payments',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_payments_stic_remittances_c',
            'join_key_lhs' => 'stic_payments_stic_remittancesstic_remittances_ida',
            'join_key_rhs' => 'stic_payments_stic_remittancesstic_payments_idb',
        ),
    ),
    'table' => 'stic_payments_stic_remittances_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_payments_stic_remittancesstic_remittances_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_payments_stic_remittancesstic_payments_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_payments_stic_remittancesspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_payments_stic_remittances_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_payments_stic_remittancesstic_remittances_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_payments_stic_remittances_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_payments_stic_remittancesstic_payments_idb',
            ),
        ),
    ),
);

$dictionary["stic_personal_environment_accounts"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_personal_environment_accounts' => array(
            'lhs_module' => 'Accounts',
            'lhs_table' => 'accounts',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Personal_Environment',
            'rhs_table' => 'stic_personal_environment',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_personal_environment_accounts_c',
            'join_key_lhs' => 'stic_personal_environment_accountsaccounts_ida',
            'join_key_rhs' => 'stic_persofe40onment_idb',
        ),
    ),
    'table' => 'stic_personal_environment_accounts_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_personal_environment_accountsaccounts_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_persofe40onment_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_personal_environment_accountsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_personal_environment_accounts_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_personal_environment_accountsaccounts_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_personal_environment_accounts_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_persofe40onment_idb',
            ),
        ),
    ),
);

$dictionary["stic_personal_environment_contacts_1"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_personal_environment_contacts_1' => array(
            'lhs_module' => 'Contacts',
            'lhs_table' => 'contacts',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Personal_Environment',
            'rhs_table' => 'stic_personal_environment',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_personal_environment_contacts_1_c',
            'join_key_lhs' => 'stic_personal_environment_contacts_1contacts_ida',
            'join_key_rhs' => 'stic_perso369eonment_idb',
        ),
    ),
    'table' => 'stic_personal_environment_contacts_1_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_personal_environment_contacts_1contacts_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_perso369eonment_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_personal_environment_contacts_1spk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_personal_environment_contacts_1_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_personal_environment_contacts_1contacts_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_personal_environment_contacts_1_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_perso369eonment_idb',
            ),
        ),
    ),
);

$dictionary["stic_personal_environment_contacts"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_personal_environment_contacts' => array(
            'lhs_module' => 'Contacts',
            'lhs_table' => 'contacts',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Personal_Environment',
            'rhs_table' => 'stic_personal_environment',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_personal_environment_contacts_c',
            'join_key_lhs' => 'stic_personal_environment_contactscontacts_ida',
            'join_key_rhs' => 'stic_perso4499onment_idb',
        ),
    ),
    'table' => 'stic_personal_environment_contacts_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_personal_environment_contactscontacts_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_perso4499onment_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_personal_environment_contactsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_personal_environment_contacts_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_personal_environment_contactscontacts_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_personal_environment_contacts_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_perso4499onment_idb',
            ),
        ),
    ),
);

$dictionary["stic_registrations_accounts"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_registrations_accounts' => array(
            'lhs_module' => 'Accounts',
            'lhs_table' => 'accounts',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Registrations',
            'rhs_table' => 'stic_registrations',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_registrations_accounts_c',
            'join_key_lhs' => 'stic_registrations_accountsaccounts_ida',
            'join_key_rhs' => 'stic_registrations_accountsstic_registrations_idb',
        ),
    ),
    'table' => 'stic_registrations_accounts_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_registrations_accountsaccounts_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_registrations_accountsstic_registrations_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_registrations_accountsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_registrations_accounts_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_registrations_accountsaccounts_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_registrations_accounts_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_registrations_accountsstic_registrations_idb',
            ),
        ),
    ),
);

$dictionary["stic_registrations_activities_calls"] = array(
    'relationships' => array(
        'stic_registrations_activities_calls' => array(
            'lhs_module' => 'stic_Registrations',
            'lhs_table' => 'stic_registrations',
            'lhs_key' => 'id',
            'rhs_module' => 'Calls',
            'rhs_table' => 'calls',
            'rhs_key' => 'parent_id',
            'relationship_type' => 'one-to-many',
            'relationship_role_column' => 'parent_type',
            'relationship_role_column_value' => 'stic_Registrations',
        ),
    ),
    'fields' => '',
    'indices' => '',
    'table' => '',
);

$dictionary["stic_registrations_activities_emails"] = array(
    'relationships' => array(
        'stic_registrations_activities_emails' => array(
            'lhs_module' => 'stic_Registrations',
            'lhs_table' => 'stic_registrations',
            'lhs_key' => 'id',
            'rhs_module' => 'Emails',
            'rhs_table' => 'emails',
            'rhs_key' => 'parent_id',
            'relationship_type' => 'one-to-many',
            'relationship_role_column' => 'parent_type',
            'relationship_role_column_value' => 'stic_Registrations',
        ),
    ),
    'fields' => '',
    'indices' => '',
    'table' => '',
);

$dictionary["stic_registrations_activities_meetings"] = array(
    'relationships' => array(
        'stic_registrations_activities_meetings' => array(
            'lhs_module' => 'stic_Registrations',
            'lhs_table' => 'stic_registrations',
            'lhs_key' => 'id',
            'rhs_module' => 'Meetings',
            'rhs_table' => 'meetings',
            'rhs_key' => 'parent_id',
            'relationship_type' => 'one-to-many',
            'relationship_role_column' => 'parent_type',
            'relationship_role_column_value' => 'stic_Registrations',
        ),
    ),
    'fields' => '',
    'indices' => '',
    'table' => '',
);

$dictionary["stic_registrations_activities_notes"] = array(
    'relationships' => array(
        'stic_registrations_activities_notes' => array(
            'lhs_module' => 'stic_Registrations',
            'lhs_table' => 'stic_registrations',
            'lhs_key' => 'id',
            'rhs_module' => 'Notes',
            'rhs_table' => 'notes',
            'rhs_key' => 'parent_id',
            'relationship_type' => 'one-to-many',
            'relationship_role_column' => 'parent_type',
            'relationship_role_column_value' => 'stic_Registrations',
        ),
    ),
    'fields' => '',
    'indices' => '',
    'table' => '',
);

$dictionary["stic_registrations_activities_tasks"] = array(
    'relationships' => array(
        'stic_registrations_activities_tasks' => array(
            'lhs_module' => 'stic_Registrations',
            'lhs_table' => 'stic_registrations',
            'lhs_key' => 'id',
            'rhs_module' => 'Tasks',
            'rhs_table' => 'tasks',
            'rhs_key' => 'parent_id',
            'relationship_type' => 'one-to-many',
            'relationship_role_column' => 'parent_type',
            'relationship_role_column_value' => 'stic_Registrations',
        ),
    ),
    'fields' => '',
    'indices' => '',
    'table' => '',
);

$dictionary["stic_registrations_contacts"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_registrations_contacts' => array(
            'lhs_module' => 'Contacts',
            'lhs_table' => 'contacts',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Registrations',
            'rhs_table' => 'stic_registrations',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_registrations_contacts_c',
            'join_key_lhs' => 'stic_registrations_contactscontacts_ida',
            'join_key_rhs' => 'stic_registrations_contactsstic_registrations_idb',
        ),
    ),
    'table' => 'stic_registrations_contacts_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_registrations_contactscontacts_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_registrations_contactsstic_registrations_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_registrations_contactsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_registrations_contacts_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_registrations_contactscontacts_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_registrations_contacts_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_registrations_contactsstic_registrations_idb',
            ),
        ),
    ),
);

$dictionary["stic_registrations_leads"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_registrations_leads' => array(
            'lhs_module' => 'Leads',
            'lhs_table' => 'leads',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Registrations',
            'rhs_table' => 'stic_registrations',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_registrations_leads_c',
            'join_key_lhs' => 'stic_registrations_leadsleads_ida',
            'join_key_rhs' => 'stic_registrations_leadsstic_registrations_idb',
        ),
    ),
    'table' => 'stic_registrations_leads_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_registrations_leadsleads_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_registrations_leadsstic_registrations_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_registrations_leadsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_registrations_leads_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_registrations_leadsleads_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_registrations_leads_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_registrations_leadsstic_registrations_idb',
            ),
        ),
    ),
);

$dictionary["stic_registrations_stic_events"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_registrations_stic_events' => array(
            'lhs_module' => 'stic_Events',
            'lhs_table' => 'stic_events',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Registrations',
            'rhs_table' => 'stic_registrations',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_registrations_stic_events_c',
            'join_key_lhs' => 'stic_registrations_stic_eventsstic_events_ida',
            'join_key_rhs' => 'stic_registrations_stic_eventsstic_registrations_idb',
        ),
    ),
    'table' => 'stic_registrations_stic_events_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_registrations_stic_eventsstic_events_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_registrations_stic_eventsstic_registrations_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_registrations_stic_eventsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_registrations_stic_events_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_registrations_stic_eventsstic_events_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_registrations_stic_events_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_registrations_stic_eventsstic_registrations_idb',
            ),
        ),
    ),
);

$dictionary["stic_sepe_actions_contacts"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_sepe_actions_contacts' => array(
            'lhs_module' => 'Contacts',
            'lhs_table' => 'contacts',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Sepe_Actions',
            'rhs_table' => 'stic_sepe_actions',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_sepe_actions_contacts_c',
            'join_key_lhs' => 'stic_sepe_actions_contactscontacts_ida',
            'join_key_rhs' => 'stic_sepe_actions_contactsstic_sepe_actions_idb',
        ),
    ),
    'table' => 'stic_sepe_actions_contacts_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_sepe_actions_contactscontacts_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_sepe_actions_contactsstic_sepe_actions_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_sepe_actions_contactsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_sepe_actions_contacts_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_sepe_actions_contactscontacts_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_sepe_actions_contacts_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_sepe_actions_contactsstic_sepe_actions_idb',
            ),
        ),
    ),
);

$dictionary["stic_sepe_incidents_contacts"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_sepe_incidents_contacts' => array(
            'lhs_module' => 'Contacts',
            'lhs_table' => 'contacts',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Sepe_Incidents',
            'rhs_table' => 'stic_sepe_incidents',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_sepe_incidents_contacts_c',
            'join_key_lhs' => 'stic_sepe_incidents_contactscontacts_ida',
            'join_key_rhs' => 'stic_sepe_incidents_contactsstic_sepe_incidents_idb',
        ),
    ),
    'table' => 'stic_sepe_incidents_contacts_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_sepe_incidents_contactscontacts_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_sepe_incidents_contactsstic_sepe_incidents_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_sepe_incidents_contactsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_sepe_incidents_contacts_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_sepe_incidents_contactscontacts_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_sepe_incidents_contacts_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_sepe_incidents_contactsstic_sepe_incidents_idb',
            ),
        ),
    ),
);

$dictionary["stic_sepe_actions_contacts"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_sepe_actions_contacts' => array(
            'lhs_module' => 'Contacts',
            'lhs_table' => 'contacts',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Sepe_Actions',
            'rhs_table' => 'stic_sepe_actions',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_sepe_actions_contacts_c',
            'join_key_lhs' => 'stic_sepe_actions_contactscontacts_ida',
            'join_key_rhs' => 'stic_sepe_actions_contactsstic_sepe_actions_idb',
        ),
    ),
    'table' => 'stic_sepe_actions_contacts_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_sepe_actions_contactscontacts_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_sepe_actions_contactsstic_sepe_actions_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_sepe_actions_contactsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_sepe_actions_contacts_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_sepe_actions_contactscontacts_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_sepe_actions_contacts_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_sepe_actions_contactsstic_sepe_actions_idb',
            ),
        ),
    ),
);

$dictionary["stic_sepe_incidents_contacts"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_sepe_incidents_contacts' => array(
            'lhs_module' => 'Contacts',
            'lhs_table' => 'contacts',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Sepe_Incidents',
            'rhs_table' => 'stic_sepe_incidents',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_sepe_incidents_contacts_c',
            'join_key_lhs' => 'stic_sepe_incidents_contactscontacts_ida',
            'join_key_rhs' => 'stic_sepe_incidents_contactsstic_sepe_incidents_idb',
        ),
    ),
    'table' => 'stic_sepe_incidents_contacts_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_sepe_incidents_contactscontacts_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_sepe_incidents_contactsstic_sepe_incidents_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_sepe_incidents_contactsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_sepe_incidents_contacts_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_sepe_incidents_contactscontacts_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_sepe_incidents_contacts_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_sepe_incidents_contactsstic_sepe_incidents_idb',
            ),
        ),
    ),
);

$dictionary["stic_sessions_documents"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_sessions_documents' => array(
            'lhs_module' => 'stic_Sessions',
            'lhs_table' => 'stic_sessions',
            'lhs_key' => 'id',
            'rhs_module' => 'Documents',
            'rhs_table' => 'documents',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_sessions_documents_c',
            'join_key_lhs' => 'stic_sessions_documentsstic_sessions_ida',
            'join_key_rhs' => 'stic_sessions_documentsdocuments_idb',
        ),
    ),
    'table' => 'stic_sessions_documents_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_sessions_documentsstic_sessions_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_sessions_documentsdocuments_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
        5 => array(
            'name' => 'document_revision_id',
            'type' => 'varchar',
            'len' => '36',
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_sessions_documentsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_sessions_documents_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_sessions_documentsstic_sessions_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_sessions_documents_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_sessions_documentsdocuments_idb',
            ),
        ),
    ),
);

$dictionary["stic_sessions_stic_events"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_sessions_stic_events' => array(
            'lhs_module' => 'stic_Events',
            'lhs_table' => 'stic_events',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Sessions',
            'rhs_table' => 'stic_sessions',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_sessions_stic_events_c',
            'join_key_lhs' => 'stic_sessions_stic_eventsstic_events_ida',
            'join_key_rhs' => 'stic_sessions_stic_eventsstic_sessions_idb',
        ),
    ),
    'table' => 'stic_sessions_stic_events_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_sessions_stic_eventsstic_events_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_sessions_stic_eventsstic_sessions_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_sessions_stic_eventsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_sessions_stic_events_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_sessions_stic_eventsstic_events_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_sessions_stic_events_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_sessions_stic_eventsstic_sessions_idb',
            ),
        ),
    ),
);

$dictionary["stic_validation_actions_schedulers"] = array(
    'true_relationship_type' => 'many-to-many',
    'relationships' => array(
        'stic_validation_actions_schedulers' => array(
            'lhs_module' => 'stic_Validation_Actions',
            'lhs_table' => 'stic_validation_actions',
            'lhs_key' => 'id',
            'rhs_module' => 'Schedulers',
            'rhs_table' => 'schedulers',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_validation_actions_schedulers_c',
            'join_key_lhs' => 'stic_validation_actions_schedulersstic_validation_actions_ida',
            'join_key_rhs' => 'stic_validation_actions_schedulersschedulers_idb',
        ),
    ),
    'table' => 'stic_validation_actions_schedulers_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_validation_actions_schedulersstic_validation_actions_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_validation_actions_schedulersschedulers_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_validation_actions_schedulersspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_validation_actions_schedulers_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_validation_actions_schedulersstic_validation_actions_ida',
                1 => 'stic_validation_actions_schedulersschedulers_idb',
            ),
        ),
    ),
);

$dictionary["stic_payments_stic_attendances"] = array(
    'true_relationship_type' => 'one-to-many',
    'from_studio' => true,
    'relationships' => array(
        'stic_payments_stic_attendances' => array(
            'lhs_module' => 'stic_Payments',
            'lhs_table' => 'stic_payments',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Attendances',
            'rhs_table' => 'stic_attendances',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_payments_stic_attendances_c',
            'join_key_lhs' => 'stic_payments_stic_attendancesstic_payments_ida',
            'join_key_rhs' => 'stic_payments_stic_attendancesstic_attendances_idb',
        ),
    ),
    'table' => 'stic_payments_stic_attendances_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_payments_stic_attendancesstic_payments_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_payments_stic_attendancesstic_attendances_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_payments_stic_attendancesspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_payments_stic_attendances_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_payments_stic_attendancesstic_payments_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_payments_stic_attendances_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_payments_stic_attendancesstic_attendances_idb',
            ),
        ),
    ),
);

$dictionary["stic_payment_commitments_stic_registrations"] = array(
    'true_relationship_type' => 'one-to-many',
    'from_studio' => true,
    'relationships' => array(
        'stic_payment_commitments_stic_registrations' => array(
            'lhs_module' => 'stic_Payment_Commitments',
            'lhs_table' => 'stic_payment_commitments',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Registrations',
            'rhs_table' => 'stic_registrations',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_payment_commitments_stic_registrations_c',
            'join_key_lhs' => 'stic_payme96d2itments_ida',
            'join_key_rhs' => 'stic_paymee0afrations_idb',
        ),
    ),
    'table' => 'stic_payment_commitments_stic_registrations_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_payme96d2itments_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_paymee0afrations_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_payment_commitments_stic_registrationsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_payment_commitments_stic_registrations_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_payme96d2itments_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_payment_commitments_stic_registrations_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_paymee0afrations_idb',
            ),
        ),
    ),
);

$dictionary["stic_bookings_accounts"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_bookings_accounts' => array(
            'lhs_module' => 'Accounts',
            'lhs_table' => 'accounts',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Bookings',
            'rhs_table' => 'stic_bookings',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_bookings_accounts_c',
            'join_key_lhs' => 'stic_bookings_accountsaccounts_ida',
            'join_key_rhs' => 'stic_bookings_accountsstic_bookings_idb',
        ),
    ),
    'table' => 'stic_bookings_accounts_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_bookings_accountsaccounts_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_bookings_accountsstic_bookings_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_bookings_accountsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_bookings_accounts_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_bookings_accountsaccounts_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_bookings_accounts_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_bookings_accountsstic_bookings_idb',
            ),
        ),
    ),
);

$dictionary["stic_bookings_contacts"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_bookings_contacts' => array(
            'lhs_module' => 'Contacts',
            'lhs_table' => 'contacts',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Bookings',
            'rhs_table' => 'stic_bookings',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_bookings_contacts_c',
            'join_key_lhs' => 'stic_bookings_contactscontacts_ida',
            'join_key_rhs' => 'stic_bookings_contactsstic_bookings_idb',
        ),
    ),
    'table' => 'stic_bookings_contacts_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_bookings_contactscontacts_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_bookings_contactsstic_bookings_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_bookings_contactsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_bookings_contacts_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_bookings_contactscontacts_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_bookings_contacts_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_bookings_contactsstic_bookings_idb',
            ),
        ),
    ),
);
$dictionary["stic_bookings_stic_families"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_bookings_stic_families' => array(
            'lhs_module' => 'stic_Families',
            'lhs_table' => 'stic_families',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Bookings',
            'rhs_table' => 'stic_bookings',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_bookings_stic_families_c',
            'join_key_lhs' => 'stic_bookings_stic_familiesstic_families_ida',
            'join_key_rhs' => 'stic_bookings_stic_familiesstic_bookings_idb',
        ),
    ),
    'table' => 'stic_bookings_stic_families_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_bookings_stic_familiesstic_families_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_bookings_stic_familiesstic_bookings_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_bookings_stic_familiesspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_bookings_stic_families_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_bookings_stic_familiesstic_families_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_bookings_stic_families_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_bookings_stic_familiesstic_bookings_idb',
            ),
        ),
    ),
);
$dictionary["stic_bookings_stic_payment_commitments"] = array (
    'true_relationship_type' => 'one-to-one',
    'from_studio' => true,
    'relationships' => 
    array (
      'stic_bookings_stic_payment_commitments' => 
      array (
        'lhs_module' => 'stic_Bookings',
        'lhs_table' => 'stic_bookings',
        'lhs_key' => 'id',
        'rhs_module' => 'stic_Payment_Commitments',
        'rhs_table' => 'stic_payment_commitments',
        'rhs_key' => 'id',
        'relationship_type' => 'many-to-many',
        'join_table' => 'stic_bookings_stic_payment_commitments_c',
        'join_key_lhs' => 'stic_bookings_stic_payment_commitmentsstic_bookings_ida',
        'join_key_rhs' => 'stic_booki93ceitments_idb',
      ),
    ),
    'table' => 'stic_bookings_stic_payment_commitments_c',
    'fields' => 
    array (
      0 => 
      array (
        'name' => 'id',
        'type' => 'varchar',
        'len' => 36,
      ),
      1 => 
      array (
        'name' => 'date_modified',
        'type' => 'datetime',
      ),
      2 => 
      array (
        'name' => 'deleted',
        'type' => 'bool',
        'len' => '1',
        'default' => '0',
        'required' => true,
      ),
      3 => 
      array (
        'name' => 'stic_bookings_stic_payment_commitmentsstic_bookings_ida',
        'type' => 'varchar',
        'len' => 36,
      ),
      4 => 
      array (
        'name' => 'stic_booki93ceitments_idb',
        'type' => 'varchar',
        'len' => 36,
      ),
    ),
    'indices' => 
    array (
      0 => 
      array (
        'name' => 'stic_bookings_stic_payment_commitmentsspk',
        'type' => 'primary',
        'fields' => 
        array (
          0 => 'id',
        ),
      ),
      1 => 
      array (
        'name' => 'stic_bookings_stic_payment_commitments_ida1',
        'type' => 'index',
        'fields' => 
        array (
          0 => 'stic_bookings_stic_payment_commitmentsstic_bookings_ida',
        ),
      ),
      2 => 
      array (
        'name' => 'stic_bookings_stic_payment_commitments_idb2',
        'type' => 'index',
        'fields' => 
        array (
          0 => 'stic_booki93ceitments_idb',
        ),
      ),
    ),
  );
$dictionary["stic_resources_stic_bookings"] = array(
    'true_relationship_type' => 'many-to-many',
    'relationships' => array(
        'stic_resources_stic_bookings' => array(
            'lhs_module' => 'stic_Resources',
            'lhs_table' => 'stic_resources',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Bookings',
            'rhs_table' => 'stic_bookings',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_resources_stic_bookings_c',
            'join_key_lhs' => 'stic_resources_stic_bookingsstic_resources_ida',
            'join_key_rhs' => 'stic_resources_stic_bookingsstic_bookings_idb',
        ),
    ),
    'table' => 'stic_resources_stic_bookings_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_resources_stic_bookingsstic_resources_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_resources_stic_bookingsstic_bookings_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_resources_stic_bookingsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_resources_stic_bookings_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_resources_stic_bookingsstic_resources_ida',
                1 => 'stic_resources_stic_bookingsstic_bookings_idb',
            ),
        ),
    ),
);

$dictionary["stic_families_documents"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_families_documents' => array(
            'lhs_module' => 'stic_Families',
            'lhs_table' => 'stic_families',
            'lhs_key' => 'id',
            'rhs_module' => 'Documents',
            'rhs_table' => 'documents',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_families_documents_c',
            'join_key_lhs' => 'stic_families_documentsstic_families_ida',
            'join_key_rhs' => 'stic_families_documentsdocuments_idb',
        ),
    ),
    'table' => 'stic_families_documents_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_families_documentsstic_families_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_families_documentsdocuments_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
        5 => array(
            'name' => 'document_revision_id',
            'type' => 'varchar',
            'len' => '36',
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_families_documentsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_families_documents_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_families_documentsstic_families_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_families_documents_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_families_documentsdocuments_idb',
            ),
        ),
    ),
);

$dictionary["stic_families_stic_followups"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_families_stic_followups' => array(
            'lhs_module' => 'stic_Families',
            'lhs_table' => 'stic_families',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_FollowUps',
            'rhs_table' => 'stic_followups',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_families_stic_followups_c',
            'join_key_lhs' => 'stic_families_stic_followupsstic_families_ida',
            'join_key_rhs' => 'stic_families_stic_followupsstic_followups_idb',
        ),
    ),
    'table' => 'stic_families_stic_followups_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_families_stic_followupsstic_families_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_families_stic_followupsstic_followups_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_families_stic_followupsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_families_stic_followups_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_families_stic_followupsstic_families_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_families_stic_followups_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_families_stic_followupsstic_followups_idb',
            ),
        ),
    ),
);

$dictionary["stic_families_stic_personal_environment"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_families_stic_personal_environment' => array(
            'lhs_module' => 'stic_Families',
            'lhs_table' => 'stic_families',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Personal_Environment',
            'rhs_table' => 'stic_personal_environment',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_families_stic_personal_environment_c',
            'join_key_lhs' => 'stic_families_stic_personal_environmentstic_families_ida',
            'join_key_rhs' => 'stic_famil7664ronment_idb',
        ),
    ),
    'table' => 'stic_families_stic_personal_environment_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_families_stic_personal_environmentstic_families_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_famil7664ronment_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_families_stic_personal_environmentspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_families_stic_personal_environment_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_families_stic_personal_environmentstic_families_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_families_stic_personal_environment_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_famil7664ronment_idb',
            ),
        ),
    ),
);

$dictionary["stic_families_stic_assessments"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_families_stic_assessments' => array(
            'lhs_module' => 'stic_Families',
            'lhs_table' => 'stic_families',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Assessments',
            'rhs_table' => 'stic_assessments',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_families_stic_assessments_c',
            'join_key_lhs' => 'stic_families_stic_assessmentsstic_families_ida',
            'join_key_rhs' => 'stic_families_stic_assessmentsstic_assessments_idb',
        ),
    ),
    'table' => 'stic_families_stic_assessments_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_families_stic_assessmentsstic_families_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_families_stic_assessmentsstic_assessments_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_families_stic_assessmentsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_families_stic_assessments_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_families_stic_assessmentsstic_families_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_families_stic_assessments_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_families_stic_assessmentsstic_assessments_idb',
            ),
        ),
    ),
);

$dictionary["stic_families_stic_goals"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_families_stic_goals' => array(
            'lhs_module' => 'stic_Families',
            'lhs_table' => 'stic_families',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Goals',
            'rhs_table' => 'stic_goals',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_families_stic_goals_c',
            'join_key_lhs' => 'stic_families_stic_goalsstic_families_ida',
            'join_key_rhs' => 'stic_families_stic_goalsstic_goals_idb',
        ),
    ),
    'table' => 'stic_families_stic_goals_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_families_stic_goalsstic_families_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_families_stic_goalsstic_goals_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_families_stic_goalsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_families_stic_goals_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_families_stic_goalsstic_families_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_families_stic_goals_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_families_stic_goalsstic_goals_idb',
            ),
        ),
    ),
);

$dictionary["stic_medication_log_contacts"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_medication_log_contacts' => array(
            'lhs_module' => 'Contacts',
            'lhs_table' => 'contacts',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Medication_Log',
            'rhs_table' => 'stic_medication_log',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_medication_log_contacts_c',
            'join_key_lhs' => 'stic_medication_log_contactscontacts_ida',
            'join_key_rhs' => 'stic_medication_log_contactsstic_medication_log_idb',
        ),
    ),
    'table' => 'stic_medication_log_contacts_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_medication_log_contactscontacts_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_medication_log_contactsstic_medication_log_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_medication_log_contactsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_medication_log_contacts_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_medication_log_contactscontacts_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_medication_log_contacts_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_medication_log_contactsstic_medication_log_idb',
            ),
        ),
    ),
);

$dictionary["stic_prescription_contacts"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_prescription_contacts' => array(
            'lhs_module' => 'Contacts',
            'lhs_table' => 'contacts',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Prescription',
            'rhs_table' => 'stic_prescription',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_prescription_contacts_c',
            'join_key_lhs' => 'stic_prescription_contactscontacts_ida',
            'join_key_rhs' => 'stic_prescription_contactsstic_prescription_idb',
        ),
    ),
    'table' => 'stic_prescription_contacts_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_prescription_contactscontacts_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_prescription_contactsstic_prescription_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_prescription_contactsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_prescription_contacts_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_prescription_contactscontacts_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_prescription_contacts_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_prescription_contactsstic_prescription_idb',
            ),
        ),
    ),
);

$dictionary["stic_prescription_stic_medication"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_prescription_stic_medication' => array(
            'lhs_module' => 'stic_Medication',
            'lhs_table' => 'stic_medication',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Prescription',
            'rhs_table' => 'stic_prescription',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_prescription_stic_medication_c',
            'join_key_lhs' => 'stic_prescription_stic_medicationstic_medication_ida',
            'join_key_rhs' => 'stic_prescription_stic_medicationstic_prescription_idb',
        ),
    ),
    'table' => 'stic_prescription_stic_medication_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_prescription_stic_medicationstic_medication_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_prescription_stic_medicationstic_prescription_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_prescription_stic_medicationspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_prescription_stic_medication_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_prescription_stic_medicationstic_medication_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_prescription_stic_medication_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_prescription_stic_medicationstic_prescription_idb',
            ),
        ),
    ),
);
$dictionary["stic_medication_log_stic_prescription"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_medication_log_stic_prescription' => array(
            'lhs_module' => 'stic_Prescription',
            'lhs_table' => 'stic_prescription',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Medication_Log',
            'rhs_table' => 'stic_medication_log',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_medication_log_stic_prescription_c',
            'join_key_lhs' => 'stic_medication_log_stic_prescriptionstic_prescription_ida',
            'join_key_rhs' => 'stic_medication_log_stic_prescriptionstic_medication_log_idb',
        ),
    ),
    'table' => 'stic_medication_log_stic_prescription_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_medication_log_stic_prescriptionstic_prescription_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_medication_log_stic_prescriptionstic_medication_log_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_medication_log_stic_prescriptionspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_medication_log_stic_prescription_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_medication_log_stic_prescriptionstic_prescription_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_medication_log_stic_prescription_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_medication_log_stic_prescriptionstic_medication_log_idb',
            ),
        ),
    ),
);
$dictionary["stic_centers_accounts"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_centers_accounts' => array(
            'lhs_module' => 'Accounts',
            'lhs_table' => 'accounts',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Centers',
            'rhs_table' => 'stic_centers',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_centers_accounts_c',
            'join_key_lhs' => 'stic_centers_accountsaccounts_ida',
            'join_key_rhs' => 'stic_centers_accountsstic_centers_idb',
        ),
    ),
    'table' => 'stic_centers_accounts_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_centers_accountsaccounts_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_centers_accountsstic_centers_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_centers_accountsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_centers_accounts_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_centers_accountsaccounts_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_centers_accounts_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_centers_accountsstic_centers_idb',
            ),
        ),
    ),
);
$dictionary["stic_centers_contacts"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_centers_contacts' => array(
            'lhs_module' => 'stic_Centers',
            'lhs_table' => 'stic_centers',
            'lhs_key' => 'id',
            'rhs_module' => 'Contacts',
            'rhs_table' => 'contacts',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_centers_contacts_c',
            'join_key_lhs' => 'stic_centers_contactsstic_centers_ida',
            'join_key_rhs' => 'stic_centers_contactscontacts_idb',
        ),
    ),
    'table' => 'stic_centers_contacts_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_centers_contactsstic_centers_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_centers_contactscontacts_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_centers_contactsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_centers_contacts_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_centers_contactsstic_centers_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_centers_contacts_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_centers_contactscontacts_idb',
            ),
        ),
    ),
);
$dictionary["stic_centers_project"] = array(
    'true_relationship_type' => 'many-to-many',
    'relationships' => array(
        'stic_centers_project' => array(
            'lhs_module' => 'stic_Centers',
            'lhs_table' => 'stic_centers',
            'lhs_key' => 'id',
            'rhs_module' => 'Project',
            'rhs_table' => 'project',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_centers_project_c',
            'join_key_lhs' => 'stic_centers_projectstic_centers_ida',
            'join_key_rhs' => 'stic_centers_projectproject_idb',
        ),
    ),
    'table' => 'stic_centers_project_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_centers_projectstic_centers_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_centers_projectproject_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_centers_projectspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_centers_project_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_centers_projectstic_centers_ida',
                1 => 'stic_centers_projectproject_idb',
            ),
        ),
    ),
);
$dictionary["stic_centers_stic_contacts_relationships"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_centers_stic_contacts_relationships' => array(
            'lhs_module' => 'stic_Centers',
            'lhs_table' => 'stic_centers',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Contacts_Relationships',
            'rhs_table' => 'stic_contacts_relationships',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_centers_stic_contacts_relationships_c',
            'join_key_lhs' => 'stic_centers_stic_contacts_relationshipsstic_centers_ida',
            'join_key_rhs' => 'stic_centea017onships_idb',
        ),
    ),
    'table' => 'stic_centers_stic_contacts_relationships_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_centers_stic_contacts_relationshipsstic_centers_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_centea017onships_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_centers_stic_contacts_relationshipsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_centers_stic_contacts_relationships_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_centers_stic_contacts_relationshipsstic_centers_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_centers_stic_contacts_relationships_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_centea017onships_idb',
            ),
        ),
    ),
);
$dictionary["stic_centers_stic_events"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_centers_stic_events' => array(
            'lhs_module' => 'stic_Centers',
            'lhs_table' => 'stic_centers',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Events',
            'rhs_table' => 'stic_events',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_centers_stic_events_c',
            'join_key_lhs' => 'stic_centers_stic_eventsstic_centers_ida',
            'join_key_rhs' => 'stic_centers_stic_eventsstic_events_idb',
        ),
    ),
    'table' => 'stic_centers_stic_events_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_centers_stic_eventsstic_centers_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_centers_stic_eventsstic_events_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_centers_stic_eventsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_centers_stic_events_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_centers_stic_eventsstic_centers_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_centers_stic_events_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_centers_stic_eventsstic_events_idb',
            ),
        ),
    ),
);
$dictionary["stic_grants_accounts"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_grants_accounts' => array(
            'lhs_module' => 'Accounts',
            'lhs_table' => 'accounts',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Grants',
            'rhs_table' => 'stic_grants',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_grants_accounts_c',
            'join_key_lhs' => 'stic_grants_accountsaccounts_ida',
            'join_key_rhs' => 'stic_grants_accountsstic_grants_idb',
        ),
    ),
    'table' => 'stic_grants_accounts_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_grants_accountsaccounts_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_grants_accountsstic_grants_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_grants_accountsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_grants_accounts_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_grants_accountsaccounts_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_grants_accounts_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_grants_accountsstic_grants_idb',
            ),
        ),
    ),
);
$dictionary["stic_grants_contacts"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_grants_contacts' => array(
            'lhs_module' => 'Contacts',
            'lhs_table' => 'contacts',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Grants',
            'rhs_table' => 'stic_grants',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_grants_contacts_c',
            'join_key_lhs' => 'stic_grants_contactscontacts_ida',
            'join_key_rhs' => 'stic_grants_contactsstic_grants_idb',
        ),
    ),
    'table' => 'stic_grants_contacts_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_grants_contactscontacts_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_grants_contactsstic_grants_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_grants_contactsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_grants_contacts_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_grants_contactscontacts_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_grants_contacts_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_grants_contactsstic_grants_idb',
            ),
        ),
    ),
);
$dictionary["stic_grants_opportunities"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_grants_opportunities' => array(
            'lhs_module' => 'Opportunities',
            'lhs_table' => 'opportunities',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Grants',
            'rhs_table' => 'stic_grants',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_grants_opportunities_c',
            'join_key_lhs' => 'stic_grants_opportunitiesopportunities_ida',
            'join_key_rhs' => 'stic_grants_opportunitiesstic_grants_idb',
        ),
    ),
    'table' => 'stic_grants_opportunities_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_grants_opportunitiesopportunities_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_grants_opportunitiesstic_grants_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_grants_opportunitiesspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_grants_opportunities_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_grants_opportunitiesopportunities_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_grants_opportunities_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_grants_opportunitiesstic_grants_idb',
            ),
        ),
    ),
);
$dictionary["stic_grants_project"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_grants_project' => array(
            'lhs_module' => 'Project',
            'lhs_table' => 'project',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Grants',
            'rhs_table' => 'stic_grants',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_grants_project_c',
            'join_key_lhs' => 'stic_grants_projectproject_ida',
            'join_key_rhs' => 'stic_grants_projectstic_grants_idb',
        ),
    ),
    'table' => 'stic_grants_project_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_grants_projectproject_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_grants_projectstic_grants_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_grants_projectspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_grants_project_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_grants_projectproject_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_grants_project_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_grants_projectstic_grants_idb',
            ),
        ),
    ),
);
$dictionary["stic_grants_stic_families"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_grants_stic_families' => array(
            'lhs_module' => 'stic_Families',
            'lhs_table' => 'stic_families',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Grants',
            'rhs_table' => 'stic_grants',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_grants_stic_families_c',
            'join_key_lhs' => 'stic_grants_stic_familiesstic_families_ida',
            'join_key_rhs' => 'stic_grants_stic_familiesstic_grants_idb',
        ),
    ),
    'table' => 'stic_grants_stic_families_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_grants_stic_familiesstic_families_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_grants_stic_familiesstic_grants_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_grants_stic_familiesspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_grants_stic_families_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_grants_stic_familiesstic_families_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_grants_stic_families_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_grants_stic_familiesstic_grants_idb',
            ),
        ),
    ),
);
$dictionary["stic_journal_contacts"] = array(
    'true_relationship_type' => 'many-to-many',
    'relationships' => array(
        'stic_journal_contacts' => array(
            'lhs_module' => 'stic_Journal',
            'lhs_table' => 'stic_journal',
            'lhs_key' => 'id',
            'rhs_module' => 'Contacts',
            'rhs_table' => 'contacts',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_journal_contacts_c',
            'join_key_lhs' => 'stic_journal_contactsstic_journal_ida',
            'join_key_rhs' => 'stic_journal_contactscontacts_idb',
        ),
    ),
    'table' => 'stic_journal_contacts_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_journal_contactsstic_journal_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_journal_contactscontacts_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_journal_contactsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_journal_contacts_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_journal_contactsstic_journal_ida',
                1 => 'stic_journal_contactscontacts_idb',
            ),
        ),
        2 => array(
            'name' => 'stic_journal_contacts_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_journal_contactscontacts_idb',
                1 => 'stic_journal_contactsstic_journal_ida',
            ),
        ),
    ),
);
$dictionary["stic_journal_stic_centers"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_journal_stic_centers' => array(
            'lhs_module' => 'stic_Centers',
            'lhs_table' => 'stic_centers',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Journal',
            'rhs_table' => 'stic_journal',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_journal_stic_centers_c',
            'join_key_lhs' => 'stic_journal_stic_centersstic_centers_ida',
            'join_key_rhs' => 'stic_journal_stic_centersstic_journal_idb',
        ),
    ),
    'table' => 'stic_journal_stic_centers_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_journal_stic_centersstic_centers_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_journal_stic_centersstic_journal_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_journal_stic_centersspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_journal_stic_centers_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_journal_stic_centersstic_centers_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_journal_stic_centers_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_journal_stic_centersstic_journal_idb',
            ),
        ),
    ),
);
$dictionary["stic_training_contacts"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_training_contacts' => array(
            'lhs_module' => 'Contacts',
            'lhs_table' => 'contacts',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Training',
            'rhs_table' => 'stic_training',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_training_contacts_c',
            'join_key_lhs' => 'stic_training_contactscontacts_ida',
            'join_key_rhs' => 'stic_training_contactsstic_training_idb',
        ),
    ),
    'table' => 'stic_training_contacts_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_training_contactscontacts_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_training_contactsstic_training_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_training_contactsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_training_contacts_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_training_contactscontacts_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_training_contacts_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_training_contactsstic_training_idb',
            ),
        ),
    ),
);
$dictionary["stic_training_accounts"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_training_accounts' => array(
            'lhs_module' => 'Accounts',
            'lhs_table' => 'accounts',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Training',
            'rhs_table' => 'stic_training',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_training_accounts_c',
            'join_key_lhs' => 'stic_training_accountsaccounts_ida',
            'join_key_rhs' => 'stic_training_accountsstic_training_idb',
        ),
    ),
    'table' => 'stic_training_accounts_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_training_accountsaccounts_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_training_accountsstic_training_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_training_accountsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_training_accounts_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_training_accountsaccounts_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_training_accounts_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_training_accountsstic_training_idb',
            ),
        ),
    ),
);
$dictionary["stic_training_stic_registrations"] = array(
    'true_relationship_type' => 'one-to-one',
    'relationships' => array(
        'stic_training_stic_registrations' => array(
            'lhs_module' => 'stic_Training',
            'lhs_table' => 'stic_training',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Registrations',
            'rhs_table' => 'stic_registrations',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_training_stic_registrations_c',
            'join_key_lhs' => 'stic_training_stic_registrationsstic_training_ida',
            'join_key_rhs' => 'stic_training_stic_registrationsstic_registrations_idb',
        ),
    ),
    'table' => 'stic_training_stic_registrations_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_training_stic_registrationsstic_training_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_training_stic_registrationsstic_registrations_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_training_stic_registrationsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_training_stic_registrations_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_training_stic_registrationsstic_training_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_training_stic_registrations_idb2',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_training_stic_registrationsstic_registrations_idb',
            ),
        ),
    ),
);
$dictionary["stic_work_experience_contacts"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_work_experience_contacts' => array(
            'lhs_module' => 'Contacts',
            'lhs_table' => 'contacts',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Work_Experience',
            'rhs_table' => 'stic_work_experience',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_work_experience_contacts_c',
            'join_key_lhs' => 'stic_work_experience_contactscontacts_ida',
            'join_key_rhs' => 'stic_work_experience_contactsstic_work_experience_idb',
        ),
    ),
    'table' => 'stic_work_experience_contacts_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_work_experience_contactscontacts_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_work_experience_contactsstic_work_experience_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_work_experience_contactsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_work_experience_contacts_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_work_experience_contactscontacts_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_work_experience_contacts_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_work_experience_contactsstic_work_experience_idb',
            ),
        ),
    ),
);
$dictionary["stic_work_experience_accounts"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_work_experience_accounts' => array(
            'lhs_module' => 'Accounts',
            'lhs_table' => 'accounts',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Work_Experience',
            'rhs_table' => 'stic_work_experience',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_work_experience_accounts_c',
            'join_key_lhs' => 'stic_work_experience_accountsaccounts_ida',
            'join_key_rhs' => 'stic_work_experience_accountsstic_work_experience_idb',
        ),
    ),
    'table' => 'stic_work_experience_accounts_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_work_experience_accountsaccounts_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_work_experience_accountsstic_work_experience_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_work_experience_accountsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_work_experience_accounts_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_work_experience_accountsaccounts_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_work_experience_accounts_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_work_experience_accountsstic_work_experience_idb',
            ),
        ),
    ),
);
$dictionary["stic_work_experience_stic_job_applications"] = array(
    'true_relationship_type' => 'one-to-one',
    'relationships' => array(
        'stic_work_experience_stic_job_applications' => array(
            'lhs_module' => 'stic_Work_Experience',
            'lhs_table' => 'stic_work_experience',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Job_Applications',
            'rhs_table' => 'stic_job_applications',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_work_experience_stic_job_applications_c',
            'join_key_lhs' => 'stic_work_537ferience_ida',
            'join_key_rhs' => 'stic_work_9fefcations_idb',
        ),
    ),
    'table' => 'stic_work_experience_stic_job_applications_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_work_537ferience_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_work_9fefcations_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_work_experience_stic_job_applicationsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_work_experience_stic_job_applications_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_work_537ferience_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_work_experience_stic_job_applications_idb2',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_work_9fefcations_idb',
            ),
        ),
    ),
);

$dictionary["stic_skills_contacts"] = array(
    'true_relationship_type' => 'one-to-many',
    'relationships' => array(
        'stic_skills_contacts' => array(
            'lhs_module' => 'Contacts',
            'lhs_table' => 'contacts',
            'lhs_key' => 'id',
            'rhs_module' => 'stic_Skills',
            'rhs_table' => 'stic_skills',
            'rhs_key' => 'id',
            'relationship_type' => 'many-to-many',
            'join_table' => 'stic_skills_contacts_c',
            'join_key_lhs' => 'stic_skills_contactscontacts_ida',
            'join_key_rhs' => 'stic_skills_contactsstic_skills_idb',
        ),
    ),
    'table' => 'stic_skills_contacts_c',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => 36,
        ),
        1 => array(
            'name' => 'date_modified',
            'type' => 'datetime',
        ),
        2 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => true,
        ),
        3 => array(
            'name' => 'stic_skills_contactscontacts_ida',
            'type' => 'varchar',
            'len' => 36,
        ),
        4 => array(
            'name' => 'stic_skills_contactsstic_skills_idb',
            'type' => 'varchar',
            'len' => 36,
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'stic_skills_contactsspk',
            'type' => 'primary',
            'fields' => array(
                0 => 'id',
            ),
        ),
        1 => array(
            'name' => 'stic_skills_contacts_ida1',
            'type' => 'index',
            'fields' => array(
                0 => 'stic_skills_contactscontacts_ida',
            ),
        ),
        2 => array(
            'name' => 'stic_skills_contacts_alt',
            'type' => 'alternate_key',
            'fields' => array(
                0 => 'stic_skills_contactsstic_skills_idb',
            ),
        ),
    ),
);

$dictionary["stic_custom_view_customizations_stic_custom_view_actions"] = array (
    'true_relationship_type' => 'one-to-many',
    'relationships' => 
    array (
      'stic_custom_view_customizations_stic_custom_view_actions' => 
      array (
        'lhs_module' => 'stic_Custom_View_Customizations',
        'lhs_table' => 'stic_custom_view_customizations',
        'lhs_key' => 'id',
        'rhs_module' => 'stic_Custom_View_Actions',
        'rhs_table' => 'stic_custom_view_actions',
        'rhs_key' => 'id',
        'relationship_type' => 'many-to-many',
        'join_table' => 'stic_custom_view_customizations_stic_custom_view_actions_c',
        'join_key_lhs' => 'stic_custo077ezations_ida',
        'join_key_rhs' => 'stic_custo6c56actions_idb',
      ),
    ),
    'table' => 'stic_custom_view_customizations_stic_custom_view_actions_c',
    'fields' => 
    array (
      0 => 
      array (
        'name' => 'id',
        'type' => 'varchar',
        'len' => 36,
      ),
      1 => 
      array (
        'name' => 'date_modified',
        'type' => 'datetime',
      ),
      2 => 
      array (
        'name' => 'deleted',
        'type' => 'bool',
        'len' => '1',
        'default' => '0',
        'required' => true,
      ),
      3 => 
      array (
        'name' => 'stic_custo077ezations_ida',
        'type' => 'varchar',
        'len' => 36,
      ),
      4 => 
      array (
        'name' => 'stic_custo6c56actions_idb',
        'type' => 'varchar',
        'len' => 36,
      ),
    ),
    'indices' => 
    array (
      0 => 
      array (
        'name' => 'stic_custom_view_customizations_stic_custom_view_actionsspk',
        'type' => 'primary',
        'fields' => 
        array (
          0 => 'id',
        ),
      ),
      1 => 
      array (
        'name' => 'stic_custom_view_customizations_stic_custom_view_actions_ida1',
        'type' => 'index',
        'fields' => 
        array (
          0 => 'stic_custo077ezations_ida',
        ),
      ),
      2 => 
      array (
        'name' => 'stic_custom_view_customizations_stic_custom_view_actions_alt',
        'type' => 'alternate_key',
        'fields' => 
        array (
          0 => 'stic_custo6c56actions_idb',
        ),
      ),
    ),
);
$dictionary["stic_custom_view_customizations_stic_custom_view_conditions"] = array (
    'true_relationship_type' => 'one-to-many',
    'relationships' => 
    array (
      'stic_custom_view_customizations_stic_custom_view_conditions' => 
      array (
        'lhs_module' => 'stic_Custom_View_Customizations',
        'lhs_table' => 'stic_custom_view_customizations',
        'lhs_key' => 'id',
        'rhs_module' => 'stic_Custom_View_Conditions',
        'rhs_table' => 'stic_custom_view_conditions',
        'rhs_key' => 'id',
        'relationship_type' => 'many-to-many',
        'join_table' => 'stic_custom_view_customizations_stic_custom_view_conditions_c',
        'join_key_lhs' => 'stic_custo233dzations_ida',
        'join_key_rhs' => 'stic_custo7221ditions_idb',
      ),
    ),
    'table' => 'stic_custom_view_customizations_stic_custom_view_conditions_c',
    'fields' => 
    array (
      0 => 
      array (
        'name' => 'id',
        'type' => 'varchar',
        'len' => 36,
      ),
      1 => 
      array (
        'name' => 'date_modified',
        'type' => 'datetime',
      ),
      2 => 
      array (
        'name' => 'deleted',
        'type' => 'bool',
        'len' => '1',
        'default' => '0',
        'required' => true,
      ),
      3 => 
      array (
        'name' => 'stic_custo233dzations_ida',
        'type' => 'varchar',
        'len' => 36,
      ),
      4 => 
      array (
        'name' => 'stic_custo7221ditions_idb',
        'type' => 'varchar',
        'len' => 36,
      ),
    ),
    'indices' => 
    array (
      0 => 
      array (
        'name' => 'stic_custom_view_customizations_stic_custom_view_conditionsspk',
        'type' => 'primary',
        'fields' => 
        array (
          0 => 'id',
        ),
      ),
      1 => 
      array (
        'name' => 'stic_custom_view_customizations_stic_custom_view_conditions_ida1',
        'type' => 'index',
        'fields' => 
        array (
          0 => 'stic_custo233dzations_ida',
        ),
      ),
      2 => 
      array (
        'name' => 'stic_custom_view_customizations_stic_custom_view_conditions_alt',
        'type' => 'alternate_key',
        'fields' => 
        array (
          0 => 'stic_custo7221ditions_idb',
        ),
      ),
    ),
);
$dictionary["stic_custom_views_stic_custom_view_customizations"] = array (
    'true_relationship_type' => 'one-to-many',
    'relationships' => 
    array (
      'stic_custom_views_stic_custom_view_customizations' => 
      array (
        'lhs_module' => 'stic_Custom_Views',
        'lhs_table' => 'stic_custom_views',
        'lhs_key' => 'id',
        'rhs_module' => 'stic_Custom_View_Customizations',
        'rhs_table' => 'stic_custom_view_customizations',
        'rhs_key' => 'id',
        'relationship_type' => 'many-to-many',
        'join_table' => 'stic_custom_views_stic_custom_view_customizations_c',
        'join_key_lhs' => 'stic_custo45d1m_views_ida',
        'join_key_rhs' => 'stic_custobdd5zations_idb',
      ),
    ),
    'table' => 'stic_custom_views_stic_custom_view_customizations_c',
    'fields' => 
    array (
      0 => 
      array (
        'name' => 'id',
        'type' => 'varchar',
        'len' => 36,
      ),
      1 => 
      array (
        'name' => 'date_modified',
        'type' => 'datetime',
      ),
      2 => 
      array (
        'name' => 'deleted',
        'type' => 'bool',
        'len' => '1',
        'default' => '0',
        'required' => true,
      ),
      3 => 
      array (
        'name' => 'stic_custo45d1m_views_ida',
        'type' => 'varchar',
        'len' => 36,
      ),
      4 => 
      array (
        'name' => 'stic_custobdd5zations_idb',
        'type' => 'varchar',
        'len' => 36,
      ),
    ),
    'indices' => 
    array (
      0 => 
      array (
        'name' => 'stic_custom_views_stic_custom_view_customizationsspk',
        'type' => 'primary',
        'fields' => 
        array (
          0 => 'id',
        ),
      ),
      1 => 
      array (
        'name' => 'stic_custom_views_stic_custom_view_customizations_ida1',
        'type' => 'index',
        'fields' => 
        array (
          0 => 'stic_custo45d1m_views_ida',
        ),
      ),
      2 => 
      array (
        'name' => 'stic_custom_views_stic_custom_view_customizations_alt',
        'type' => 'alternate_key',
        'fields' => 
        array (
          0 => 'stic_custobdd5zations_idb',
        ),
      ),
    ),
);
$dictionary["stic_group_opportunities_accounts"] = array (
    'true_relationship_type' => 'one-to-many',
    'relationships' => 
    array (
      'stic_group_opportunities_accounts' => 
      array (
        'lhs_module' => 'Accounts',
        'lhs_table' => 'accounts',
        'lhs_key' => 'id',
        'rhs_module' => 'stic_Group_Opportunities',
        'rhs_table' => 'stic_group_opportunities',
        'rhs_key' => 'id',
        'relationship_type' => 'many-to-many',
        'join_table' => 'stic_group_opportunities_accounts_c',
        'join_key_lhs' => 'stic_group_opportunities_accountsaccounts_ida',
        'join_key_rhs' => 'stic_group_opportunities_accountsstic_group_opportunities_idb',
      ),
    ),
    'table' => 'stic_group_opportunities_accounts_c',
    'fields' => 
    array (
      0 => 
      array (
        'name' => 'id',
        'type' => 'varchar',
        'len' => 36,
      ),
      1 => 
      array (
        'name' => 'date_modified',
        'type' => 'datetime',
      ),
      2 => 
      array (
        'name' => 'deleted',
        'type' => 'bool',
        'len' => '1',
        'default' => '0',
        'required' => true,
      ),
      3 => 
      array (
        'name' => 'stic_group_opportunities_accountsaccounts_ida',
        'type' => 'varchar',
        'len' => 36,
      ),
      4 => 
      array (
        'name' => 'stic_group_opportunities_accountsstic_group_opportunities_idb',
        'type' => 'varchar',
        'len' => 36,
      ),
    ),
    'indices' => 
    array (
      0 => 
      array (
        'name' => 'stic_group_opportunities_accountsspk',
        'type' => 'primary',
        'fields' => 
        array (
          0 => 'id',
        ),
      ),
      1 => 
      array (
        'name' => 'stic_group_opportunities_accounts_ida1',
        'type' => 'index',
        'fields' => 
        array (
          0 => 'stic_group_opportunities_accountsaccounts_ida',
        ),
      ),
      2 => 
      array (
        'name' => 'stic_group_opportunities_accounts_alt',
        'type' => 'alternate_key',
        'fields' => 
        array (
          0 => 'stic_group_opportunities_accountsstic_group_opportunities_idb',
        ),
      ),
    ),
  );

  $dictionary["stic_group_opportunities_documents_1"] = array (
    'true_relationship_type' => 'many-to-many',
    'from_studio' => true,
    'relationships' => 
    array (
      'stic_group_opportunities_documents_1' => 
      array (
        'lhs_module' => 'stic_Group_Opportunities',
        'lhs_table' => 'stic_group_opportunities',
        'lhs_key' => 'id',
        'rhs_module' => 'Documents',
        'rhs_table' => 'documents',
        'rhs_key' => 'id',
        'relationship_type' => 'many-to-many',
        'join_table' => 'stic_group_opportunities_documents_1_c',
        'join_key_lhs' => 'stic_group_opportunities_documents_1stic_group_opportunities_ida',
        'join_key_rhs' => 'stic_group_opportunities_documents_1documents_idb',
      ),
    ),
    'table' => 'stic_group_opportunities_documents_1_c',
    'fields' => 
    array (
      0 => 
      array (
        'name' => 'id',
        'type' => 'varchar',
        'len' => 36,
      ),
      1 => 
      array (
        'name' => 'date_modified',
        'type' => 'datetime',
      ),
      2 => 
      array (
        'name' => 'deleted',
        'type' => 'bool',
        'len' => '1',
        'default' => '0',
        'required' => true,
      ),
      3 => 
      array (
        'name' => 'stic_group_opportunities_documents_1stic_group_opportunities_ida',
        'type' => 'varchar',
        'len' => 36,
      ),
      4 => 
      array (
        'name' => 'stic_group_opportunities_documents_1documents_idb',
        'type' => 'varchar',
        'len' => 36,
      ),
      5 => 
      array (
        'name' => 'document_revision_id',
        'type' => 'varchar',
        'len' => '36',
      ),
    ),
    'indices' => 
    array (
      0 => 
      array (
        'name' => 'stic_group_opportunities_documents_1spk',
        'type' => 'primary',
        'fields' => 
        array (
          0 => 'id',
        ),
      ),
      1 => 
      array (
        'name' => 'stic_group_opportunities_documents_1_alt',
        'type' => 'alternate_key',
        'fields' => 
        array (
          0 => 'stic_group_opportunities_documents_1stic_group_opportunities_ida',
          1 => 'stic_group_opportunities_documents_1documents_idb',
        ),
      ),
    ),
  );

  $dictionary["stic_group_opportunities_opportunities"] = array (
    'true_relationship_type' => 'one-to-many',
    'relationships' => 
    array (
      'stic_group_opportunities_opportunities' => 
      array (
        'lhs_module' => 'Opportunities',
        'lhs_table' => 'opportunities',
        'lhs_key' => 'id',
        'rhs_module' => 'stic_Group_Opportunities',
        'rhs_table' => 'stic_group_opportunities',
        'rhs_key' => 'id',
        'relationship_type' => 'many-to-many',
        'join_table' => 'stic_group_opportunities_opportunities_c',
        'join_key_lhs' => 'stic_group_opportunities_opportunitiesopportunities_ida',
        'join_key_rhs' => 'stic_grp_opportunities_opportunitiesstic_grp_opportunities_idb',
      ),
    ),
    'table' => 'stic_group_opportunities_opportunities_c',
    'fields' => 
    array (
      0 => 
      array (
        'name' => 'id',
        'type' => 'varchar',
        'len' => 36,
      ),
      1 => 
      array (
        'name' => 'date_modified',
        'type' => 'datetime',
      ),
      2 => 
      array (
        'name' => 'deleted',
        'type' => 'bool',
        'len' => '1',
        'default' => '0',
        'required' => true,
      ),
      3 => 
      array (
        'name' => 'stic_group_opportunities_opportunitiesopportunities_ida',
        'type' => 'varchar',
        'len' => 36,
      ),
      4 => 
      array (
        'name' => 'stic_grp_opportunities_opportunitiesstic_grp_opportunities_idb',
        'type' => 'varchar',
        'len' => 36,
      ),
    ),
    'indices' => 
    array (
      0 => 
      array (
        'name' => 'stic_group_opportunities_opportunitiesspk',
        'type' => 'primary',
        'fields' => 
        array (
          0 => 'id',
        ),
      ),
      1 => 
      array (
        'name' => 'stic_group_opportunities_opportunities_ida1',
        'type' => 'index',
        'fields' => 
        array (
          0 => 'stic_group_opportunities_opportunitiesopportunities_ida',
        ),
      ),
      2 => 
      array (
        'name' => 'stic_group_opportunities_opportunities_alt',
        'type' => 'alternate_key',
        'fields' => 
        array (
          0 => 'stic_grp_opportunities_opportunitiesstic_grp_opportunities_idb',
        ),
      ),
    ),
  );

$dictionary["stic_resources_stic_centers"] = array (
    'true_relationship_type' => 'one-to-many',
    'from_studio' => true,
    'relationships' => 
    array (
      'stic_resources_stic_centers' => 
      array (
        'lhs_module' => 'stic_Centers',
        'lhs_table' => 'stic_centers',
        'lhs_key' => 'id',
        'rhs_module' => 'stic_Resources',
        'rhs_table' => 'stic_resources',
        'rhs_key' => 'id',
        'relationship_type' => 'many-to-many',
        'join_table' => 'stic_resources_stic_centers_c',
        'join_key_lhs' => 'stic_resources_stic_centersstic_centers_ida',
        'join_key_rhs' => 'stic_resources_stic_centersstic_resources_idb',
      ),
    ),
    'table' => 'stic_resources_stic_centers_c',
    'fields' => 
    array (
      0 => 
      array (
        'name' => 'id',
        'type' => 'varchar',
        'len' => 36,
      ),
      1 => 
      array (
        'name' => 'date_modified',
        'type' => 'datetime',
      ),
      2 => 
      array (
        'name' => 'deleted',
        'type' => 'bool',
        'len' => '1',
        'default' => '0',
        'required' => true,
      ),
      3 => 
      array (
        'name' => 'stic_resources_stic_centersstic_centers_ida',
        'type' => 'varchar',
        'len' => 36,
      ),
      4 => 
      array (
        'name' => 'stic_resources_stic_centersstic_resources_idb',
        'type' => 'varchar',
        'len' => 36,
      ),
    ),
    'indices' => 
    array (
      0 => 
      array (
        'name' => 'stic_resources_stic_centersspk',
        'type' => 'primary',
        'fields' => 
        array (
          0 => 'id',
        ),
      ),
      1 => 
      array (
        'name' => 'stic_resources_stic_centers_ida1',
        'type' => 'index',
        'fields' => 
        array (
          0 => 'stic_resources_stic_centersstic_centers_ida',
        ),
      ),
      2 => 
      array (
        'name' => 'stic_resources_stic_centers_alt',
        'type' => 'alternate_key',
        'fields' => 
        array (
          0 => 'stic_resources_stic_centersstic_resources_idb',
        ),
      ),
    ),
  );
  $dictionary["stic_resources_project"] = array (
    'true_relationship_type' => 'one-to-many',
    'from_studio' => true,
    'relationships' => 
    array (
      'stic_resources_project' => 
      array (
        'lhs_module' => 'Project',
        'lhs_table' => 'project',
        'lhs_key' => 'id',
        'rhs_module' => 'stic_Resources',
        'rhs_table' => 'stic_resources',
        'rhs_key' => 'id',
        'relationship_type' => 'many-to-many',
        'join_table' => 'stic_resources_project_c',
        'join_key_lhs' => 'stic_resources_projectproject_ida',
        'join_key_rhs' => 'stic_resources_projectstic_resources_idb',
      ),
    ),
    'table' => 'stic_resources_project_c',
    'fields' => 
    array (
      0 => 
      array (
        'name' => 'id',
        'type' => 'varchar',
        'len' => 36,
      ),
      1 => 
      array (
        'name' => 'date_modified',
        'type' => 'datetime',
      ),
      2 => 
      array (
        'name' => 'deleted',
        'type' => 'bool',
        'len' => '1',
        'default' => '0',
        'required' => true,
      ),
      3 => 
      array (
        'name' => 'stic_resources_projectproject_ida',
        'type' => 'varchar',
        'len' => 36,
      ),
      4 => 
      array (
        'name' => 'stic_resources_projectstic_resources_idb',
        'type' => 'varchar',
        'len' => 36,
      ),
    ),
    'indices' => 
    array (
      0 => 
      array (
        'name' => 'stic_resources_projectspk',
        'type' => 'primary',
        'fields' => 
        array (
          0 => 'id',
        ),
      ),
      1 => 
      array (
        'name' => 'stic_resources_projectida1',
        'type' => 'index',
        'fields' => 
        array (
          0 => 'stic_resources_projectproject_ida',
        ),
      ),
      2 => 
      array (
        'name' => 'stic_resources_project_alt',
        'type' => 'alternate_key',
        'fields' => 
        array (
          0 => 'stic_resources_projectstic_resources_idb',
        ),
      ),
    ),
  );