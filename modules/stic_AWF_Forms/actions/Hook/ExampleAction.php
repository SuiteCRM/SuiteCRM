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
if (! defined('sugarEntry') || ! sugarEntry) {
    die('Not A Valid Entry Point');
}

// Load all the necessary classes for the Actions process
include_once "modules/stic_AWF_Forms/actions/coreActions.php";

/**
 * ExampleAction (Complete Guide for Developers)
 * - This is the implementation of an example action that serves as a template for developers of new actions.
 * - This action is of type Hook, it will be executed on the server once the form response is received.
 *
 * * This class is an exhaustive template that shows HOW to implement all types
 * of parameters and logic available in the Advanced Web Forms system.
 * 
 * * FILE NAMING:
 *   - The class name (Ex: 'ExampleAction') must match exactly with the file name (Ex: 'ExampleAction.php').
 *
 * * LOCATION:
 *   - The file should be placed in a subfolder of 'modules/stic_AWF_Forms/actions/' according to its type. For this action, it would be:
 *     .../actions/Hook/ExampleAction.php
 *   - Or in the 'custom' folder for extensions:
 *     .../custom/modules/stic_AWF_Forms/actions/Hook/ExampleAction.php
 * 
 * * ACTION TYPES (Inheritance):
 *   1. HookActionDefinition: Generic action (eg: Send Email, Redirect).
 *   2. HookDataBlockActionDefinition: Action linked to a Data Block (eg: Create Record).
 *   3. HookBeanActionDefinition: Action that requires an already saved Bean (eg: Relate, Add to LPO).
 * 
 * - In this example we use the generic base (HookActionDefinition) to show all cases.
 */
class ExampleAction extends HookActionDefinition
{

    /**
     * Constructor
     */
    public function __construct()
    {
        // Indicate whether the action is active or not.
        $this->isActive = true;

        // Indicate whether the user can select this action in the wizard when creating a form.
        $this->isUserSelectable = false;

        // Indicate the category of the action (from the list stic_awf_forms_action_definition_category_list)
        $this->category = 'data';

        // The prefix to use for multi-language labels used by the application
        // These labels should be defined in the language files of the stic_AWF_Forms module
        $this->baseLabel = 'LBL_EXAMPLE_ACTION';

        // Set the supported form types for this action. (Empty = all form types, or specify ['web', 'crm'] for specific types)
        $this->supportedFormTypes = ['web', 'crm'];
    }

    /**
     * getTitle()
     * (Optional) Returns the title of the action.
     * It is recommended NOT to override this function and use the translation of 'LBL_EXAMPLE_ACTION_TITLE'.
     * @return string The title to display in the wizard.
     */
    public function getTitle(): string
    {
        // return $this->translate('TITLE'); // Recommended method: will translate the label "LBL_EXAMPLE_ACTION_TITLE"
        return "Example Action (Commented)"; // Fixed value for the example
    }

    /**
     * getDescription()
     * (Optional) Returns the description of the action.
     * It is recommended NOT to override this function and use the translation of 'LBL_EXAMPLE_ACTION_DESC'.
     * @return string The description to display in the wizard.
     */
    public function getDescription(): string
    {
        return "This is a template for developers. It shows how to define and use all parameter types.";
    }

    /**
     * getParameters()
     * Defines THE PARAMETERS that this action needs.
     * The wizard will use this definition to build the action configuration interface.
     * @return ActionParameterDefinition[] An array of parameter definitions.
     */
    public function getParameters(): array
    {
        // In this function we define the parameters that the action expects.
        // The ParameterResolver will use this definition to know what value to deliver in the execute() function.

        $parameters = [];

        // =========================================================================================
        // TYPE 1: SIMPLE VALUES (ActionParameterType::VALUE)
        // =========================================================================================

        // 1.1. Text / Number / Email / Date
        // The Wizard will show an <input> according to the data type.
        $paramText = new ActionParameterDefinition();
        $paramText->name = 'simple_text';
        $paramText->text = 'Simple Text (VALUE)';
        $paramText->type = ActionParameterType::VALUE; 
        $paramText->dataType = ActionDataType::TEXT; // Also: INTEGER, FLOAT, BOOLEAN, DATE, EMAIL...
        $paramText->defaultValue = "Default value";
        $paramText->required = true;
        $parameters[] = $paramText;

        // 1.2. Boolean
        // The Wizard will show a checkbox.
        $paramBool = new ActionParameterDefinition();
        $paramBool->name = 'simple_boolean';
        $paramBool->text = 'Boolean value (VALUE)';
        $paramBool->type = ActionParameterType::VALUE; 
        $paramBool->dataType = ActionDataType::BOOLEAN; 
        $parameters[] = $paramBool;

        // 1.3. Integer
        // The Wizard will show a text field for numerics.
        $paramInt = new ActionParameterDefinition();
        $paramInt->name = 'simple_integer';
        $paramInt->text = 'Integer value (VALUE)';
        $paramInt->type = ActionParameterType::VALUE; 
        $paramInt->dataType = ActionDataType::INTEGER; 
        $parameters[] = $paramInt;

        // 1.4. Date
        // The Wizard will show a text field for dates.
        $paramDate = new ActionParameterDefinition();
        $paramDate->name = 'simple_date';
        $paramDate->text = 'Date value (VALUE)';
        $paramDate->type = ActionParameterType::VALUE; 
        $paramDate->dataType = ActionDataType::DATE; 
        $parameters[] = $paramDate;

        // 1.5. Simple Dropdown List (SELECT)
        // The Wizard will show a <select> with fixed options defined here.
        $paramSelect = new ActionParameterDefinition();
        $paramSelect->name = 'simple_select';
        $paramSelect->text = 'Simple Selection (VALUE + SELECT)';
        $paramSelect->type = ActionParameterType::VALUE;
        $paramSelect->dataType = ActionDataType::SELECT; // Tells the Wizard to paint a combo
        $paramSelect->options = [
            new ActionParameterOption('option_a', 'Option A'),
            new ActionParameterOption('option_b', 'Option B (Recommended)')
        ];
        $paramSelect->defaultValue = 'option_b';
        $parameters[] = $paramSelect;

        // 1.6. List of Fields (FIELD_LIST)
        // The Wizard will show a selector of form fields that will be translated into a comma-separated list of fields.
        // The Backend will automatically resolve this into an associative array [field_name => value].
        $paramFieldList = new ActionParameterDefinition();
        $paramFieldList->name = 'fields_to_process';
        $paramFieldList->text = 'List of Text Fields (FIELD_LIST)';
        $paramFieldList->type = ActionParameterType::FIELD_LIST;
        $paramFieldList->supportedDataTypes = [ActionDataType::TEXT, ActionDataType::TEXTAREA];
        $parameters[] = $paramFieldList;

        // 1.7. Related field (FIELD)
        // The Wizard will show a selector of form fields that will be translated into a comma-separated list of fields.
        // The Backend will automatically resolve this into an associative array [field_name => value].
        $paramRelate = new ActionParameterDefinition();
        $paramRelate->name = 'field_related';
        $paramRelate->text = 'Related field (FIELD)';
        $paramRelate->type = ActionParameterType::FIELD;
        $paramRelate->supportedDataTypes = [ActionDataType::RELATE];
        $parameters[] = $paramRelate;


        // =========================================================================================
        // TYPE 2: FORM ELEMENTS
        // =========================================================================================

        // 2.1. Data Block (DATA_BLOCK)
        // The Wizard shows a dropdown with the defined blocks.
        // The Backend delivers a 'DataBlockResolved' object with configuration and data.
        $paramBlock = new ActionParameterDefinition();
        $paramBlock->name = 'target_block';
        $paramBlock->text = 'Data Block (DATA_BLOCK)';
        $paramBlock->type = ActionParameterType::DATA_BLOCK;
        $paramBlock->supportedModules = ['Contacts', 'Leads']; // Optional: Filter by module
        $paramBlock->required = true;
        $parameters[] = $paramBlock;

        // 2.2. Single Field (FIELD)
        // The Wizard shows a dropdown with all form fields.
        // The Backend delivers a 'DataBlockFieldResolved' object.
        $paramField = new ActionParameterDefinition();
        $paramField->name = 'source_field';
        $paramField->text = 'Source Field (FIELD)';
        $paramField->type = ActionParameterType::FIELD;
        // Optional: Filter which fields the Wizard displays according to their type in CRM
        $paramField->supportedDataTypes = [ActionDataType::EMAIL, ActionDataType::TEXT]; 
        $paramField->required = true;
        $parameters[] = $paramField;


        // =========================================================================================
        // TYPE 3: CRM REFERENCES (CRM_RECORD)
        // =========================================================================================

        // 3.1. CRM Record
        // The Wizard allows selecting an existing record (fixed ID) or using a compatible field.
        // The Backend delivers a 'BeanReference' object (Module + ID).
        $paramRecord = new ActionParameterDefinition();
        $paramRecord->name = 'template_record';
        $paramRecord->text = 'Template / Record (CRM_RECORD)';
        $paramRecord->type = ActionParameterType::CRM_RECORD;
        $paramRecord->supportedModules = ['EmailTemplates', 'Contacts', 'Campaigns']; // Vital for the Wizard to know what to search for
        $paramRecord->required = false;
        $parameters[] = $paramRecord;


        // =========================================================================================
        // TYPE 4: OPTION SELECTOR (OPTION_SELECTOR)
        // Allows the user to choose the "source" of the data (polymorphism).
        // =========================================================================================

        $paramSelector = new ActionParameterDefinition();
        $paramSelector->name = 'dynamic_source';
        $paramSelector->text = 'Dynamic Source (OPTION_SELECTOR)';
        $paramSelector->description = 'Select where to get the information from.';
        $paramSelector->type = ActionParameterType::OPTION_SELECTOR;
                
        // We define the options available to the user:
        $paramSelector->selectorOptions =[];

        // Option A: Context (EMPTY) - No user input required, the value is implicit.
        $optOwner = new ActionSelectorOptionDefinition();
        $optOwner->name = 'opt_owner';
        $optOwner->text = 'The current user (Context)';
        $optOwner->resolvedType = ActionParameterType::EMPTY; // Backend will receive NULL as value
        $paramSelector->selectorOptions[] = $optOwner;

        // Option B: Block (DATA_BLOCK)
        $optBlockSrc = new ActionSelectorOptionDefinition();
        $optBlockSrc->name = 'opt_block';
        $optBlockSrc->text = 'From a Data Block';
        $optBlockSrc->resolvedType = ActionParameterType::DATA_BLOCK;
        $optBlockSrc->supportedModules = ['Contacts', 'Leads']; // Optional: Filter by module
        $paramSelector->selectorOptions[] = $optBlockSrc;

        // Option C: Fixed Value (VALUE): TextArea
        $optFixedTextArea = new ActionSelectorOptionDefinition();
        $optFixedTextArea->name = 'opt_fixed_textarea';
        $optFixedTextArea->text = 'Manual Value: Long text';
        $optFixedTextArea->resolvedType = ActionParameterType::VALUE;
        $optFixedTextArea->resolvedDataType = ActionDataType::TEXTAREA;  // The data type of the parameter
        $paramSelector->selectorOptions[] = $optFixedTextArea;

        // Option D: Fixed Value (VALUE): Boolean
        $optFixedBoolean = new ActionSelectorOptionDefinition();
        $optFixedBoolean->name = 'opt_fixed_boolean';
        $optFixedBoolean->text = 'Manual Value: Boolean';
        $optFixedBoolean->resolvedType = ActionParameterType::VALUE;
        $optFixedBoolean->resolvedDataType = ActionDataType::BOOLEAN;  // The data type of the parameter
        $paramSelector->selectorOptions[] = $optFixedBoolean;

        // Option E: Fixed Value (VALUE): Text
        $optFixedText = new ActionSelectorOptionDefinition();
        $optFixedText->name = 'opt_fixed_text';
        $optFixedText->text = 'Manual Value: Text';
        $optFixedText->resolvedType = ActionParameterType::VALUE;
        $optFixedText->resolvedDataType = ActionDataType::TEXT;  // The data type of the parameter
        $paramSelector->selectorOptions[] = $optFixedText;
        
        // Option F: Fixed Value (VALUE): DateTime
        $optFixedDateTime = new ActionSelectorOptionDefinition();
        $optFixedDateTime->name = 'opt_fixed_dateTime';
        $optFixedDateTime->text = 'Manual Value: Date and time';
        $optFixedDateTime->resolvedType = ActionParameterType::VALUE;
        $optFixedDateTime->resolvedDataType = ActionDataType::DATETIME;  // The data type of the parameter
        $paramSelector->selectorOptions[] = $optFixedDateTime;
        $parameters[] = $paramSelector;

        return $parameters;
    }

    /**
     * execute()
     * THIS IS THE CORE OF THE ACTION.
     * This function is executed when the flow reaches this action.
     *
     * @param ExecutionContext $context The global execution context.
     * @param FormAction $actionConfig The specific configuration of THIS action.
     * @return ActionResult The result of the execution.
     */
    public function execute(ExecutionContext $context, FormAction $actionConfig): ActionResult
    {
        // ------------------------------------------------------------------------
        // 1. PARAMETER RECOVERY (Already resolved by ParameterResolverService)
        // ------------------------------------------------------------------------

        // VALUE (Simple) -> string|int|bool
        $textValue = $actionConfig->getResolvedParameter('simple_text');

        // VALUE (Select) -> string (the 'value' of the selected option)
        $selectValue = $actionConfig->getResolvedParameter('simple_select'); // ex: 'option_a'

        // VALUE (Field List) -> associative array ['Block.Field' => 'Resolved Value']
        $fieldsMap = $actionConfig->getResolvedParameter('fields_to_process') ?? [];
        foreach ($fieldsMap as $fieldKey => $fieldVal) {
            $GLOBALS['log']->debug("Field List Item: $fieldKey = $fieldVal");
        }

        // DATA_BLOCK -> DataBlockResolved object
        /** @var DataBlockResolved $block */
        $block = $actionConfig->getResolvedParameter('target_block');
        // Data access: $block->formData['crm_field']
        // Definition access: $block->dataBlock->module

        // FIELD -> DataBlockFieldResolved object
        /** @var DataBlockFieldResolved $field */
        $field = $actionConfig->getResolvedParameter('source_field');
        // Value access: $field->value
        // Definition access: $field->dataBlockField (can be null if _detached)

        // CRM_RECORD -> BeanReference object
        /** @var BeanReference $recordRef */
        $recordRef = $actionConfig->getResolvedParameter('template_record');
        // Access to bean (Lazy Loading): $recordRef?->getBean()

        // OPTION_SELECTOR -> OptionSelectorResolved object
        /** @var OptionSelectorResolved $selector */
        $selector = $actionConfig->getResolvedParameter('dynamic_source');
        
        // Logic according to the option chosen by the user
        if ($selector) {
            switch ($selector->selectedOptionName) {
                case 'opt_owner':
                    // EMPTY: The value is null, we use context logic
                    $userId = $GLOBALS['current_user']->id;
                    break;
                case 'opt_block':
                    // DATA_BLOCK: The value is DataBlockResolved
                    /** @var DataBlockResolved $selBlock */
                    $selBlock = $selector->resolvedValue;
                    break;
                case 'opt_fixed':
                    // VALUE: The value is string
                    $fixedVal = $selector->resolvedValue;
                    break;
            }
        }

        // ------------------------------------------------------------------------
        // 2. BUSINESS LOGIC (Example)
        // ------------------------------------------------------------------------

        // Suppose we create a Bean based on the data
        $newBean = BeanFactory::newBean('Notes');
        $newBean->name = "Generated note: " . $textValue;
        $newBean->description = "Option chosen: " . $selectValue;
        
        // We link it to the selected block (if it has a saved bean)
        $parentRef = $block->dataBlock->getBeanReference();
        if ($parentRef) {
            $newBean->parent_type = $parentRef->moduleName;
            $newBean->parent_id = $parentRef->beanId;
        }
        
        $newBean->save();


        // ------------------------------------------------------------------------
        // 3. RESULT AND AUDIT
        // ------------------------------------------------------------------------

        $result = new ActionResult(ResultStatus::OK, $actionConfig, "Example action completed.");

        // REGISTER MODIFICATIONS (Fundamental for traceability)
        
        // Option A: If we have modified/created a "loose" bean (not linked to a form block)
        // We use the new signature that accepts the SugarBean object directly.
        $result->registerBeanModification($newBean, BeanModificationType::CREATED);

        // Option B: If we had modified the bean of the 'target_block'
        // $dataToLog = [
        //      ['key' => 'target_list_record', 'label' => $this->translate('TARGET_LIST_RECORD_TEXT'), 'value' => $lpoRef->beanId],
        // ];
        // $actionResult->registerActionMetadata($bean, $dataToLog);

        return $result;
    }
}