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

class FormAction {
    public FormFlow $flow;           // The action flow it belongs to

    public string $id;               // ID of the action
    public string $name;             // Name of the action
    public string $text;             // The text to display
    public string $description;      // The description of the action
    /** @var string[] */
    public array $requisite_actions; // Array with the identifiers of the actions prior to the current one
    /** @var FormActionParameter[] */
    public array $parameters;        // The parameters of the action

    private array $resolvedParameters = []; // Resolved parameters, with the final value

    /** @var FormCondition[] */
    public array $conditions = [];        // Conditions to execute the validation (all must be accomplished)

    public bool $continue_on_error = false; // Indicates if the flow should continue if this action fails (throws an exception or returns an error result)

    // For deferred actions
    public ?string $flow_success_id = null; // Flow to execute if the deferred action returns successfully
    public ?string $flow_error_id = null;   // Flow to execute if the deferred action returns with an error


    /**
     * Creates an instance of FormAction from a JSON array.
     * @param FormFlow $flow The action flow it belongs to 
     * @param array $data The data in array format
     * @return FormAction The created instance
     */
    public static function fromJsonArray(FormFlow $flow, array $data): self {
        $dto = new self();
        $dto->flow = $flow;

        $dto->id = $data['id'];
        $dto->name = $data['name'];
        $dto->text = $data['text'];
        $dto->description = $data['description'];
        $dto->requisite_actions = $data['requisite_actions'] ?? [];
        $dto->continue_on_error = !empty($data['continue_on_error']);
        
        // Deferred actions
        $dto->flow_success_id = $data['flow_success_id'] ?? '';
        $dto->flow_error_id = $data['flow_error_id'] ?? '';

        // Condition
        if (isset($data['conditions'])) {
            foreach ($data['conditions'] as $conditionData) {
                $dto->conditions[] = FormCondition::fromJsonArray($conditionData);
            }
        }

        $dto->parameters = [];
        if (isset($data['parameters'])) {
            foreach ($data['parameters'] as $parameterData) {
                $formActionParameter = FormActionParameter::fromJsonArray($dto, $parameterData);
                $dto->parameters[$formActionParameter->name] = $formActionParameter;
            }
        }

        return $dto;
    }

    /**
     * Method to save the resolved parameters
     * @param array $params [name => resolved_value]
     */
    public function setResolvedParameters(array $params): void {
        $this->resolvedParameters = $params;
    }

    /**
     * Method for actions to obtain the resolved values
     * @param string $name The name of the parameter
     * @param mixed $default A default value if not found
     * @return mixed The resolved value
     */
    public function getResolvedParameter(string $name, mixed $default = null): mixed { 
        return $this->resolvedParameters[$name] ?? $default;
    }
}