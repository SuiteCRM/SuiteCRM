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

enum ActionScope: string {
    case FORM  = 'form';
    case FIELD = 'field';
}


enum ActionType: string {
    case UI            = 'UI';
    case VALIDATOR     = 'Validator';
    case DATAPROVIDER  = 'DataProvider';
    case HOOK          = 'Hook';
    case DEFERRED      = 'Deferred';
    case GROUP         = 'Group';
}

/**
 * Abstract class representing a generic action definition, which serves as the base for all specific action types in the form system.
 * This class defines common properties and methods that all action definitions share, such as activation status, user selection capability, and supported modules.
 * It also includes abstract methods that must be implemented by subclasses to specify the parameters and type of the action.
 */
abstract class ActionDefinition {
    public bool $isActive = false;
    public bool $isUserSelectable = true;
    public bool $isAutomatic = false;
   
    public string $category;
    public ActionScope $scope = ActionScope::FORM;
    public bool $defaultContinueOnError = false; // Default value for the "Continue on Error" property of the action
    /** @var string[] */
    public array $supportedModules = [];       // moduleList
    /** @var string[] */
    public array $supportedFieldSubTypes = []; // stic_awf_forms_field_subtype_in_form_list
    /** @var string[] */
    public array $supportedFormTypes = []; // Empty = all form types (web, crm, etc.)
    public int $order = 0;

    protected string $baseLabel = 'LBL_CUSTOM_ACTION'; // Base label for translations, subclasses should set this to their specific base label

    /**
     * Translates a subkey to the current language (using the baseLabel of the action and the stic_AWF_Forms module)
     * @param string $subkey The subkey to translate
     * @return string The translated text
     */
    protected function translate(string $subkey): string {
        return translate($this->baseLabel.'_'.$subkey, 'stic_AWF_Forms');
    }

    /**
     * Returns the name of the action (it is the action file name without extension)
     * @return string The name of the action file
     */
    final public function getName(): string {
        $class = static::class;
        $reflect = new ReflectionClass($class);
        
        // ClassName without namespace
        $shortName = $reflect->getShortName();

        // File name without extension
        $fileName = pathinfo($reflect->getFileName(), PATHINFO_FILENAME);

        // If they match (usual case), return short name, else return file name
        if (strcasecmp($shortName, $fileName) === 0) {
            return $shortName;
        }
        return $fileName;
    }

    /**
     * Returns the descriptive title of the action
     * @return string The title of the action
     */
    public function getTitle(): string { return $this->translate('TITLE'); }

    /**
     * Returns the description of the action
     * @return string The description of the action
     */
    public function getDescription(): string { return $this->translate('DESC'); }

    /**
     * Returns the parameters defined for the action
     * @return ActionParameterDefinition[] The parameters of the action
     */
    public abstract function getParameters(): array;

    /**
     * Returns the type of the action
     * @return ActionType The type of the action
     */
    public abstract function getType(): ActionType;
}
