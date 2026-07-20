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
 * Data Transfer Object (DTO) for the definition of an action
 * Will be sent to the Frontend to display the action configuration
 */
class ActionDefinitionDTO {
    public string $name;
    public string $title;
    public string $description;

    public bool $isActive;
    public bool $isUserSelectable;
    public bool $isAutomatic;
    public bool $isTerminal;
    public bool $defaultContinueOnError;
    public ?string $resumptionContext = null;

    public string $category;
    public string $type;

    public ?string $defaultErrorMessage = null;
    
    public string $scope; // 'form', 'field'
    
    /** @var string[] */
    public array $supportedModules;
    
    /** @var string[] */
    public array $supportedFieldSubTypes;

    /** @var string[] */
    public array $supportedFormTypes = [];

    /** @var string[] */
    public array $supportedDataTypes = [];
    
    /** @var array[] */
    public array $autoApplyRules = [];

    public int $order;

    public string $flowSuccessLabel;
    public string $flowErrorLabel;

    /** @var ActionParameterDefinitionDTO[] */
    public array $parameters;

    public function __construct(ActionDefinition $def) {
        $this->name = $def->getName();
        $this->title = $def->getTitle();
        $this->description = $def->getDescription();
        $this->type = $def->getType()->value;
        $this->defaultContinueOnError = $def->defaultContinueOnError;
        
        if ($def instanceof ValidatorActionDefinition) {
            $this->defaultErrorMessage = $def->getDefaultErrorMessage();
        }

        $this->isActive = $def->isActive;
        $this->isUserSelectable = $def->isUserSelectable;
        $this->isAutomatic = $def->isAutomatic;
        $this->isTerminal = $def instanceof ITerminalAction;

        if ($def instanceof IDeferredAction) {
            $this->resumptionContext = $def->getResumptionContext()->value;
        } else {
            $this->resumptionContext = null;
        }

        $this->category = $def->category;
        $this->scope = $def->scope->value; // Convert enum to string
        $this->supportedModules = $def->supportedModules;
        $this->supportedFieldSubTypes = $def->supportedFieldSubTypes;
        $this->supportedFormTypes = $def->supportedFormTypes;

        if ($def instanceof ValidatorActionDefinition) {
            $this->supportedDataTypes = array_map(
                fn($dt) => $dt->value, 
                $def->supportedDataTypes
            );
            $this->autoApplyRules = $def->getAutoApplyRules();
        }
        
        $this->order = $def->order;

        if ($def instanceof IDeferredAction) {
            $this->flowSuccessLabel = $def->getFlowSuccessLabel();
            $this->flowErrorLabel = $def->getFlowErrorLabel();
        }
        
        $this->parameters = array_map(
            fn($paramDef) => new ActionParameterDefinitionDTO($paramDef), 
            $def->getParameters()
        );
    }
}