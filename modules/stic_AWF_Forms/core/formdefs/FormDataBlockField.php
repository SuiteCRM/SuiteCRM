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

enum DataBlockFieldType: string {
    case UNLINKED  = 'unlinked';
    case FORM      = 'form';
    case FIXED     = 'fixed';
}
enum DataBlockFieldValueType: string {
    case EDITABLE   = 'editable';
    case SELECTABLE = 'selectable';
    case FIXED      = 'fixed';
    case DATABLOCK  = 'dataBlock';
}

class FormDataBlockField {
    public FormDataBlock $data_block;             // The data block it belongs to

    public string $name;                         // Field name
    public string $text_original;                // Original text of the field
    public string $label;                        // Label that will appear with the field
    public string $description;                  // Field description
    public string $placeholder = '';             // Placeholder for the field
    public DataBlockFieldType $type_field;       // Field type: unlinked, form, fixed
    public bool $required_in_form;               // Indicates if the field is required in the form
    public string $type_in_form;                 // Type of editor in the form: stic_awf_forms_field_type_in_form_list
    public string $subtype_in_form;              // Subtype of editor in the form: stic_awf_forms_field_subtype_in_form_list
    public string $type;                         // Field data type
    public DataBlockFieldValueType $value_type;  // Type of value: editable, selectable, fixed, dataBlock
    public string $value;                        // The value of the field
    public string $value_text;                   // The text to display for the field value
    public ?string $related_module = null;       // Related module (if applicable)
    /** @var FormValueOption[] */
    public array $value_options = [];            // Field options
    /** @var FormFieldValidation[] */
    public array $validations = [];              // Field validations

    /**
     * Creates an instance of FormDataBlockField from a JSON array.
     * @param FormDataBlock $dataBlock The data block it belongs to
     * @param array $data The data in array format
     * @return FormDataBlockField The created instance
     */
    public static function fromJsonArray(FormDataBlock $dataBlock, array $data): self {
        $dto = new self();
        $dto->data_block = $dataBlock;

        $dto->name = $data['name'];
        $dto->text_original = $data['text_original'];
        $dto->label = $data['label'];
        $dto->description = $data['description'];
        $dto->placeholder = $data['placeholder'] ?? '';
        $dto->type_field = DataBlockFieldType::from($data['type_field']);
        $dto->required_in_form = $data['required_in_form'];
        $dto->type_in_form = $data['type_in_form'];
        $dto->subtype_in_form = $data['subtype_in_form'];
        $dto->type = $data['type'];
        $dto->value_type = DataBlockFieldValueType::from($data['value_type']);
        $dto->value = $data['value'];
        $dto->value_text = $data['value_text'];
        $dto->related_module = $data['related_module'] ?? null;

        if (isset($data['value_options'])) {
            foreach ($data['value_options'] as $optionData) {
                $dto->value_options[] = FormValueOption::fromJsonArray($optionData);
            }
        }

        if (isset($data['validations']) && is_array($data['validations'])) {
            foreach ($data['validations'] as $valData) {
                $dto->validations[] = FormFieldValidation::fromJsonArray($dto, $valData);
            }
        }
        
        return $dto;
    }

    public function getKey(): string {
        $prefix = $this->type_field === DataBlockFieldType::UNLINKED ? '_detached.' : '';
        return $prefix . $this->data_block->name . '.' . $this->name;
    }

    public function getPhpKey(): string {
        return str_replace('.', '_', $this->getKey());
    }
}