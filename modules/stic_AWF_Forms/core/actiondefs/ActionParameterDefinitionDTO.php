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
 * Data Transfer Object (DTO) for the definition of an action parameter
 * Will be sent to the Frontend to display the parameter configuration
 */
class ActionParameterDefinitionDTO {
    public string $name;
    public string $text;
    public string $description;
    public string $type;         // Ex: 'value','dataBlock','field','crmRecord','optionSelector'
    public ?string $dataType;    // Ex: 'text', 'boolean'
    public bool $required;
    public string $defaultValue;
    /** @var ActionParameterOption[] */
    public array $options = [];
    /** @var ActionSelectorOptionDefinitionDTO[] */
    public array $selectorOptions;
    /** @var string[] */
    public array $supportedModules;
    /** @var ActionDataType[] */
    public array $supportedDataTypes;
    public string $colSize; // The size of the column in the UI (Bootstrap grid classes)
        
    public function __construct(ActionParameterDefinition $def) {
        $this->name = $def->name;
        $this->text = $def->text;
        $this->description = $def->description;
        $this->type = $def->type->value; // Convert enum to string
        $this->dataType = $def->dataType?->value; // Convert enum to string
        $this->required = $def->required;
        $this->defaultValue = $def->defaultValue;
        $this->colSize = $def->colSize;
        
        $this->options = array_map(
            fn($option) => new ActionParameterOption($option->value, $option->text),
            $def->options
        );
        
        $this->selectorOptions = array_map(
            fn($optionDef) => new ActionSelectorOptionDefinitionDTO($optionDef),
            $def->selectorOptions
        );
        
        $this->supportedModules = array_slice($def->supportedModules, 0);

        $this->supportedDataTypes = array_map(
            fn($dt) => $dt->value, 
            $def->supportedDataTypes
        );
    }
}