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

enum ActionParameterType: string {
    case VALUE           = 'value';
    case DATA_BLOCK      = 'dataBlock';
    case FIELD           = 'field';
    case FIELD_LIST      = 'field_list';
    case CRM_RECORD      = 'crmRecord';
    case OPTION_SELECTOR = 'optionSelector';
    case EMPTY           = 'empty'; // The parameter or option does not require a value
}

enum ActionDataType: string {
    // Primive types
    case TEXT          = 'text';
    case TEXTAREA      = 'textarea';
    case INTEGER       = 'integer';
    case FLOAT         = 'float';
    case BOOLEAN       = 'boolean';

    // File type
    case FILE          = 'file';

    // Date types
    case DATE          = 'date';
    case DATETIME      = 'datetime-local';
    case TIME          = 'time';

    // Specific formats
    case EMAIL         = 'email';
    case TEL           = 'tel';
    case URL           = 'url';

    // Simple list of options
    case SELECT        = 'select';

    // Related (for filtering fields)
    case RELATE        = 'relate';

}

/**
 * Class to define a parameter of an action
 */
class ActionParameterDefinition {
    public string $name;                   // Parameter name
    public string $text;                   // The text to display
    public string $description = '';       // The description of the parameter
    public ActionParameterType $type = ActionParameterType::VALUE;  // The type of parameter
    public ?ActionDataType $dataType = ActionDataType::TEXT;         // The data type of the parameter: Required if $type is VALUE
    public bool $required = false;         // Indicates if the parameter is required
    public string $defaultValue = '';      // Default value of the parameter
    /** @var ActionParameterOption[] */
    public array $options = [];            // The options of the parameter if the type is SELECT
    /** @var ActionSelectorOptionDefinition[] */
    public array $selectorOptions = [];    // Additional options for object selectors
    /** @var string[] */
    public array $supportedModules = [];   // Supported modules if the destination is a CRM record
    /** @var ActionDataType[] */
    public array $supportedDataTypes = []; // List of data types of the fields pointed to, if applicable (for FIELD or FIELD_LIST)
    public string $colSize = 'col-4';      // The size of the column in the UI (Bootstrap grid classes)
}

