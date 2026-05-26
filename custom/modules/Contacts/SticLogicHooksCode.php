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

class ContactsLogicHooks {
    public function before_save(&$bean, $event, $arguments) {
        // Calculate age
        if (empty($bean->fetched_row) || $bean->birthdate != $bean->fetched_row['birthdate']) {
            include_once 'custom/modules/Contacts/SticUtils.php';
            $bean->stic_age_c = ContactsUtils::getAge($bean->birthdate);
        }

        // Bring Incorpora location data, if there is any
        if (empty($bean->fetched_row) || $bean->fetched_row['stic_incorpora_locations_id_c'] != $bean->stic_incorpora_locations_id_c) {
            include_once 'modules/stic_Incorpora_Locations/Utils.php';
            stic_Incorpora_LocationsUtils::transferLocationData($bean);
        }
    }

    public function after_save(&$bean, $event, $arguments) {
        // This code is added due to an error detected in SuiteCRM code.
        // https://github.com/salesagility/SuiteCRM/issues/8765
        // When a new Contact Relationship is added, the Contact is saved and the stic_relationship_type_c field overwritten with the values
        // added with the SQL
        // Please, delete these lines when the issue is resolved
        if (isset($_REQUEST['child_field']) && $_REQUEST['child_field'] == 'stic_contacts_relationships_contacts') {
            include_once 'modules/stic_Contacts_Relationships/Utils.php';
            stic_Contacts_RelationshipsUtils::setRelationshipType($bean->id);
        }
        // End of Patch issue

        // Generate automatic Call
        if (empty($bean->fetched_row) || $bean->stic_postal_mail_return_reason_c != $bean->fetched_row['stic_postal_mail_return_reason_c']) {
            include_once 'custom/modules/Contacts/SticUtils.php';
            ContactsUtils::generateCallFromReturnMailReason($bean);
        }
    }

    public function after_retrieve(&$bean, $event, $arguments) {
        // In order to avoid identification number loss due to GUI JS validation, set the right identification type
        // when this field is empty and the identification number is set.
        if (!empty($bean->stic_identification_number_c) && (empty($bean->stic_identification_type_c) || trim($bean->stic_identification_type_c) == '')) {
            include_once 'SticInclude/Utils.php';
            if (!SticUtils::isValidNIForNIE($bean->stic_identification_number_c)) {
                $bean->stic_identification_type_c = 'other';
            } else {
                $firstCharacter = strtoupper($bean->stic_identification_number_c[0]);
                if (is_numeric($firstCharacter) || in_array($firstCharacter, array('K', 'L', 'M'))) {
                    $bean->stic_identification_type_c = 'nif';
                } else {
                    $bean->stic_identification_type_c = 'nie';
                }
            };
            $bean->save();
        }
    }
}
