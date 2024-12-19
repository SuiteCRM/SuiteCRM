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

require_once 'SticInclude/Utils.php';

/**
 * Checks the value entered in the file identification number column with the import data of a person or an organization
 */
function identificationNumberValidation($module, $rowValue, $row)
{
    switch ($module) 
    {
        case 'Accounts':
            return SticUtils::isValidCIF($rowValue) ? $rowValue : 'LBL_ERROR_INVALID_IDENTIFICATION_NUMBER';
            break;

        case 'Contacts':
            // We check if any of the possible values of the Identification Type field exist in the user's language 
            // since in $row we have the label and not the key
            global $app_list_strings;
            $identification_types_list = $app_list_strings['stic_contacts_identification_types_list'];
            // $existLabel = false;
            foreach ($identification_types_list as $key => $value) {
                if (!empty($value) && in_array($value, $row)){
                    // $existLabel = true;   
                    // If the stic_identification_type_c field is mapped and its value is NIF or NIE
                    if ($value == $identification_types_list['nif'] || $value == $identification_types_list['nie']) {
                        return SticUtils::isValidNIForNIE($rowValue) ? $rowValue : 'LBL_ERROR_INVALID_IDENTIFICATION_NUMBER';
                    } else {
                        return true;
                    } 
                }
            }

            // This validation is cancelled because the CRM has a LH that calculates the Identification Type in case this field is not informed and the Identification Number field is.
            // More information at:
            // If there is no label, it means that the Identification type field has not been mapped
            // if (!$existLabel) {
            //     return 'LBL_ERROR_INVALID_IDENTIFICATION_NUMBER_MISSING_TYPE_FIELD';
            // }
            return true;
            break;

        default:
            return true;
            break;
    }
}


/**
 * Checks the value entered in the bank account number column of the file with the import data
 */
function bankAccountValidation($rowValue)
{
    return SticUtils::checkIBAN($rowValue) ? $rowValue : 'LBL_ERROR_INVALID_BANK_ACCOUNT';
}