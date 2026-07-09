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

class ParameterResolverService {

    /**
     * Resolves all parameters for an action, starting from definitions and the wizard configuration
     *
     * @param FormAction $actionConfig The action configuration
     * @param ActionParameterDefinition[] $paramDefinitions Parameter definitions
     * @param FormActionParameter[] $paramConfigurations Parameter configurations
     * @param ExecutionContext $context The execution context
     * @return array A map [param_name => resolved_value]
     */
    public function resolveAll(FormAction $actionConfig, array $paramDefinitions, array $paramConfigurations, ExecutionContext $context): array {
        $resolvedParameters = [];

        $paramConfigMap = [];
        foreach ($paramConfigurations as $paramConfig) {
            $paramConfigMap[$paramConfig->name] = $paramConfig;
        }

        // Iterate over paramDefinifions
        foreach ($paramDefinitions as $paramDef) {
            $paramName = $paramDef->name;
            $paramConfig = $paramConfigMap[$paramName] ?? null;
            $resolvedValue = $this->resolveSingleParam($paramDef, $paramConfig, $context);
            if ($paramDef->required && $resolvedValue === null) {
                throw new RequiredParameterException($paramName, $actionConfig->name);
            }
            $resolvedParameters[$paramName] = $resolvedValue;
        }
        
        return $resolvedParameters;
    }

    /**
     * Resolves a single parameter using the full context
     * @param ActionParameterDefinition $def The parameter definition
     * @param ?FormActionParameter $config The parameter configuration (can be null)
     * @param ExecutionContext $context The execution context
     * @return mixed The resolved value, or null if it can not be resolved
     */
    private function resolveSingleParam(ActionParameterDefinition $def, ?FormActionParameter $config, ExecutionContext $context): mixed {
        // Handle missing parameter configuration
        $value = $config?->value;
        switch ($def->type) {
            case ActionParameterType::VALUE:
                // The parameter is a fixed value. Value contains the value
                return $this->resolveFixedValue($def, $value, $context);

            case ActionParameterType::DATA_BLOCK:
                // The parameter is a Data Block. Value contains the block id
                return $this->resolveDataBlock($def, $value, $context);
                
            case ActionParameterType::FIELD:
                // The parameter is a form field. Value contains the field name
                return $this->resolveFormField($def, $value, $context);

            case ActionParameterType::FIELD_LIST:
                // The parameter is a list of form fields. Value contains comma-separated field names
                $listString = $value !== null ? $value : $def->defaultValue;
                return $this->resolveFieldList($def, $listString, $context);

            case ActionParameterType::CRM_RECORD:
                // The parameter is a CRM record. Value contains 'module|id'
                return $this->resolveBean($def, $value, $context);

            case ActionParameterType::OPTION_SELECTOR:
                // The parameter is a selector; 'selectedOption' contains the selection
                $selectedOption = $config?->selectedOption;
                return $this->resolveOptionSelector($def, $selectedOption, $value, $context);
        }

        // No resolution has been returned
        $GLOBALS['log']->warn("Line ".__LINE__.": ".__METHOD__.": Parameter {$def->name} not informed in form configuration.");
        return null;
    }

    /**
     * Resolves a fixed value parameter, applying the corresponding data type casting.
     * If the value is null, it uses the default value from the definition.
     * @param ActionParameterDefinition $def The parameter definition
     * @param ?string $value The parameter value from configuration (can be null)
     * @param ExecutionContext $context The execution context (used for date parsing)
     * @return mixed The resolved and casted value, or null if it can not be resolved
     */
    private function resolveFixedValue(ActionParameterDefinition $def, ?string $value, ExecutionContext $context): mixed {
        $valueToCast = $value !== null ? $value : $def->defaultValue;
        if ($valueToCast === null) {
            return null;
        }

        switch ($def->dataType) { 
            case ActionDataType::INTEGER:
                return (int)$valueToCast;

            case ActionDataType::FLOAT:
                return (float)$valueToCast;

            case ActionDataType::BOOLEAN:
                $lowerValue = strtolower(trim($valueToCast));
                if ($lowerValue === 'false' || $lowerValue === '0' || $lowerValue === 'off' || $lowerValue === '') {
                    return false;
                }
                return true;

            case ActionDataType::DATE:
            case ActionDataType::DATETIME:
            case ActionDataType::TIME:
                $baseTimestamp = (int)$context->submissionTimestamp;
                // Use strtotime to parse both fixed dates ("2025-10-31") and relative dates ("today", "+1 day")
                $parsedTime = @strtotime($valueToCast, $baseTimestamp);
                if ($parsedTime === false) {
                    $GLOBALS['log']->warn("Line ".__LINE__.": ".__METHOD__.": Can not parse date '{$valueToCast}' with base timestamp '{$baseTimestamp}'.");
                    return null;
                }
                try {
                    $dateTimeObj = new \DateTime();
                    $dateTimeObj->setTimestamp($parsedTime);
                    return $dateTimeObj;
                } catch (\Exception $e) {
                    $GLOBALS['log']->error("Line ".__LINE__.": ".__METHOD__.": Error creating DateTime from timestamp '{$parsedTime}': " . $e->getMessage());
                    return null;
                }

            case ActionDataType::TEXT:
            case ActionDataType::SELECT:
                return $valueToCast;
            default:
                return (string)$valueToCast;
        }
            
        return $valueToCast;
    }

    /**
     * Resolves a parameter of type Data Block, returning the corresponding DataBlockResolved object.
     * @param ActionParameterDefinition $def The parameter definition
     * @param ?string $value The parameter value from configuration (can be null)
     * @param ExecutionContext $context The execution context
     * @return ?DataBlockResolved The resolved Data Block, or null if it can not be resolved
     */
    private function resolveDataBlock(ActionParameterDefinition $def, ?string $value, ExecutionContext $context): ?DataBlockResolved {
        $dataBlockId = $value !== null ? $value : $def->defaultValue;
        if ($dataBlockId === null) {
            return null;
        }

        $dataBlockConfig = $context->formConfig->data_blocks[$dataBlockId] ?? null;
        if ($dataBlockConfig === null) {
            $GLOBALS['log']->warn("Line ".__LINE__.": ".__METHOD__.": DataBlock config not found with Id: '{$dataBlockId}'.");
            return null;
        }

        return new DataBlockResolved($dataBlockConfig, $context->formData, $context);
    }

    private function resolveBean(ActionParameterDefinition $def, ?string $value, ExecutionContext $context): ?BeanReference {
        $recordString = $value !== null ? $value : $def->defaultValue;
        if ($recordString === null) {
            return null;
        }

        // recordString: 'module|id'
        $parts = explode('|', $recordString, 2);
        if (count($parts) !== 2 || empty($parts[0]) || empty($parts[1])) {
            $GLOBALS['log']->warn("Line ".__LINE__.": ".__METHOD__.": Format error. Expected 'Module|id', but received '{$recordString}'.");
            return null;
        }

        return new BeanReference($parts[0], $parts[1]);
    }

    /**
     * Resolves a parameter of type FIELD, which points to a form field. The value is the form key of the field (ex: "Block.field1").
     * Returns a DataBlockFieldResolved object with the field definition and the value filled from form
     * If the field is not found in form data, it looks if it's a fixed value field in the DataBlock and uses that value instead.
     * @param ActionParameterDefinition $def The parameter definition
     * @param ?string $value The parameter value from configuration (can be null)
     * @param ExecutionContext $context The execution context (used to access form data and configuration)
     * @return ?DataBlockFieldResolved The resolved
     */
    private function resolveFormField(ActionParameterDefinition $def, ?string $value, ExecutionContext $context): ?DataBlockFieldResolved {
        $formKey = $value !== null ? $value : $def->defaultValue;
        if ($formKey === null || $formKey == '') {
            $GLOBALS['log']->warn("Line ".__LINE__.": ".__METHOD__.": Field name is null or empty.");
            return null;
        }

        $phpKey = str_replace('.', '_', $formKey);
        $fieldDefinition = null;
        $fieldName = $formKey;

        // Parse formKey to find the dataBlock and dataBlockField definitions
        $isDetached = str_starts_with($formKey, '_detached.');
        $keyToParse = $isDetached ? substr($formKey, 10) : $formKey; // 10 = strlen('_detached.')
        $foundBlock = null;
        foreach ($context->formConfig->data_blocks as $block) {
            $prefix = $block->name . '.';
            if (str_starts_with($keyToParse, $prefix)) {
                $foundBlock = $block;
                // The field name is the rest of the string
                $fieldName = substr($keyToParse, strlen($prefix));
                $fieldDefinition = $block->fields[$fieldName] ?? null;
                break;
            }
        }
        if (!$foundBlock) {
            $lastDot = strrpos($keyToParse, '.');
            if ($lastDot !== false) {
                $fieldName = substr($keyToParse, $lastDot + 1);
            } else {
                $fieldName = $keyToParse;
            }
        }

        $crmFieldType = $fieldDefinition?->type ?? 'text';
        $finalValue = null;
        if (array_key_exists($phpKey, $context->formData)) {
            // Fill value from form data
            $finalValue = stic_AWFUtils::castCrmValue($context->formData[$phpKey], $crmFieldType, $context);
        } else {
            // If not set in form data, then find if is a field with fixed value in DataBlock
            if ($fieldDefinition !== null && $fieldDefinition->value_type === DataBlockFieldValueType::FIXED) {
                $finalValue = stic_AWFUtils::castCrmValue($fieldDefinition->value, $crmFieldType, $context);
            }
        }
        return new DataBlockFieldResolved($formKey, $fieldName, $fieldDefinition, $finalValue);
    }

    /**
     * Resolves a parameter of type OPTION_SELECTOR, which allows to select between different options with different resolution logic. The selected option is in 'selectedOption' and the value can be used as input for resolution.
     * @param ActionParameterDefinition $def The parameter definition
     * @param ?string $selectedOption The selected option name (can be null)
     * @param ?string $value The parameter value from configuration (can be null)
     * @param ExecutionContext $context The execution context (used to access form data and configuration)
     * @return ?OptionSelectorResolved The resolved option, or null if it can not be
     */
    private function resolveOptionSelector(ActionParameterDefinition $def, ?string $selectedOption, ?string $value, ExecutionContext $context): ?OptionSelectorResolved {
        if ($selectedOption === null) {
            $GLOBALS['log']->warn("Line ".__LINE__.": ".__METHOD__.": Selected option is null");
            return null;
        }
        
        // Find the SelectedOption definition
        $optionDef = null;
        foreach ($def->selectorOptions as $o) {
            if ($o->name === $selectedOption) {
                $optionDef = $o;
                break;
            }
        }
        
        if ($optionDef === null) {
            $GLOBALS['log']->warn("Line ".__LINE__.": ".__METHOD__.": Option '{$selectedOption}' not found.");
            return null;
        }

        // Resolve parameter with resolvedType
        $resolvedValue = null;
        switch ($optionDef->resolvedType) {
            case ActionParameterType::VALUE:
                // The parameter is a fixed value. Value contains the value
                $resolvedValue = $this->resolveFixedValue($def, $value, $context);
                break;

            case ActionParameterType::DATA_BLOCK:
                // The parameter is a Data Block. Value contains the block id
                $resolvedValue = $this->resolveDataBlock($def, $value, $context);
                break;
                
            case ActionParameterType::FIELD:
                // The parameter is a form field. Value contains the field name
                $resolvedValue = $this->resolveFormField($def, $value, $context);
                break;

            case ActionParameterType::CRM_RECORD:
                // The parameter is a CRM Record. Value contains 'module|id'
                $resolvedValue = $this->resolveBean($def, $value, $context);
                break;

            case ActionParameterType::EMPTY:
                // The option does not require an additional value; use the option name as the resolved value
                $resolvedValue = $selectedOption;
                break;

            default:
                $GLOBALS['log']->warn("Line ".__LINE__.": ".__METHOD__.": Unknown ResolvedType '{$optionDef->resolvedType->value}'.");
                return null;
        }
        
        return new OptionSelectorResolved($selectedOption, $resolvedValue);        
    }

    /**
     * Converts a comma-separated string of fields (ex: "Block.field1, Block.field2")
     * into an associative array [formKey => resolvedValue].
     * 
     * @param ActionParameterDefinition $def The definition of the parameter
     * @param string $fieldListString The string with fields separated by commas (ex: "Block.field1, Block.field2")
     * @param ExecutionContext $context The execution context
     * @return array The resolved associative array
     */
    private function resolveFieldList(ActionParameterDefinition $def, string $fieldListString, ExecutionContext $context): array
    {
        $resolvedData = [];
        if (empty($fieldListString)) {
            return $resolvedData;
        }

        $fieldNames = explode(',', $fieldListString);
        foreach ($fieldNames as $formKey) {
            $formKey = trim($formKey);
            if (empty($formKey)) {
                continue;
            }

            // For each field we resolve it and add it to the results array
            // The key of each element will be the full field name "Block.field1"
            $fieldResolved = $this->resolveFormField($def, $formKey, $context);
            if ($fieldResolved !== null && $fieldResolved->value !== null) {
                $resolvedData[$formKey] = $fieldResolved->value;
            }
        }

        return $resolvedData;
    }
}

