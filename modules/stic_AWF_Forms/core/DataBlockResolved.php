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
// Prevents directly accessing this file from a web browser
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

/**
 * Class representing the resolved data of a form's Data Block, including the mapping of form fields to CRM fields and their values.
 */
class DataBlockResolved {
    public FormDataBlock $dataBlock;     // The Data Block configuration

    /** @var array<string, DataBlockFieldResolved> */
    public array $formData = [];         // Data mapped to CRM [crm_field_name => DataBlockFieldResolved]
    
    /** @var array<string, DataBlockFieldResolved> */
    public array $detachedData = [];     // Unmapped data [detached_field_name => DataBlockFieldResolved]

    public function __construct(FormDataBlock $config, array $fullFormData, ExecutionContext $context) {
        // Warning: PHP POST replaces all '.' with '_'
        // DataBlock names use PascalCase without '_'
        // Form field names:
        //   DataBlockName0_field_name              ->  "field_name" from DataBlockName0 TO CRM
        //   _detached_DataBlockName0_field_name    ->  "field_name" from DataBlockName0 DETACHED

        $this->dataBlock = $config;

        // Load default values (fixed/hidden) from the configuration
        // These values can be overridden by form-submitted values
        foreach ($config->fields as $fieldName => $fieldDef) {
            if ($fieldDef->value_type === DataBlockFieldValueType::FIXED) {
                // Get the CRM field type to perform casting
                $castedValue = stic_AWFUtils::castCrmValue($fieldDef->value, $fieldDef->type, $context);

                // Field is not present in the form; compute the logical key it would have
                $formKey = ""; 
                $logicalKey = $fieldDef->getKey();
                $fieldResolved = new DataBlockFieldResolved($logicalKey, $fieldName, $fieldDef, $castedValue);

                // Store the field in the appropriate array
                if ($fieldDef->type_field === DataBlockFieldType::UNLINKED) {
                     $this->detachedData[$fieldName] = $fieldResolved;
                } else {
                     $this->formData[$fieldName] = $fieldResolved;
                }
            }
        }

        // Process the form data
        // Form-submitted values always take precedence over fixed/hidden defaults
        $blockPrefix = $config->name . '_'; 
        $detachedPrefix = '_detached_' . $blockPrefix;
        foreach ($fullFormData as $formKey => $value) {
            $fieldName = null;
            $isUnlinked = false;

            // Identify if the field belongs to the block or is detached
            if (str_starts_with($formKey, $blockPrefix)) {
                $fieldName = substr($formKey, strlen($blockPrefix));
            } else if (str_starts_with($formKey, $detachedPrefix)) {
                $fieldName = substr($formKey, strlen($detachedPrefix));
                $isUnlinked = true;
            }

            // If the field belongs to this block, process it
            if ($fieldName) {
                // Find the field configuration to determine its type; otherwise assume text
                $definition = $config->fields[$fieldName] ?? null;
                $crmFieldType = $definition?->type;
                
                // Cast the value to the appropriate type
                $castedValue = stic_AWFUtils::castCrmValue($value, $crmFieldType, $context);

                // Rebuild the original logical key
                $logicalKey = ($isUnlinked ? '_detached.' : '') . $config->name . '.' . $fieldName;
                $fieldResolved = new DataBlockFieldResolved($logicalKey, $fieldName, $definition, $castedValue);

                // Store the field in the appropriate array
                if ($isUnlinked) {
                    $this->detachedData[$fieldName] = $fieldResolved;
                } else {
                    $this->formData[$fieldName] = $fieldResolved;
                }
            }
        }

        // Handling unchecked checkboxes: HTML does not send unchecked checkboxes, so they would not be updated in the CRM when unchecked.
        foreach ($config->fields as $fieldName => $fieldDef) {
            if ($fieldDef->type_field === DataBlockFieldType::FIXED) continue;

            $isUnlinked = ($fieldDef->type_field === DataBlockFieldType::UNLINKED);
            if ($isUnlinked && isset($this->detachedData[$fieldName])) continue;
            if (!$isUnlinked && isset($this->formData[$fieldName])) continue;

            // The field was expected but did NOT arrive in the POST.
            if ($fieldDef->type === 'bool' || $fieldDef->type === 'checkbox') {
                // Rebuild the original logical key
                $logicalKey = ($isUnlinked ? '_detached.' : '') . $config->name . '.' . $fieldName;
                $fieldResolved = new DataBlockFieldResolved($logicalKey, $fieldName, $fieldDef, 0); // 0 = False en DB
                
                // Store the field in the appropriate array
                if ($isUnlinked) {
                    $this->detachedData[$fieldName] = $fieldResolved;
                } else {
                    $this->formData[$fieldName] = $fieldResolved;
                }
            }
        }

    }

    /**
     * Gets the resolved field data for a given CRM field name. Returns null if the field is not present in the form.
     * @param string $fieldName The CRM field name to retrieve
     * @return ?DataBlockFieldResolved The resolved field data, or null if not found
     */
    public function getFieldValue($fieldName): ?DataBlockFieldResolved {
        return $this->formData[$fieldName] ?? null;
    }

    /**
     * Gets the resolved field data for a given detached field name. Returns null if the field is not present in the form.
     * @param string $fieldName The detached field name to retrieve
     * @return ?DataBlockFieldResolved The resolved field data, or null if not found
     */
    public function getDetachedFieldValue($fieldName): ?DataBlockFieldResolved {
        return $this->detachedData[$fieldName] ?? null;
    }

}
