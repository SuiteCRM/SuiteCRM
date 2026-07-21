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

/**
 * DataBlock: {
 *   id, name, text, editable_text, module, required,
 *   fields: [{ name, label, required, required_in_form, type, type_in_form, subtype_in_form, 
 *               show_in_form, value_type, value, value_text }],
 *    duplicate_detections: [{fields: [<field_name>], on_duplicate}],
 *  }
 */
class stic_AwfDataBlock {
  constructor(data = {}) {
    // 1. Set default values
    Object.assign(this, {
      id: utils.newId("awfdb"), // Data Block ID
      name: "",                 // Internal name (UI identifier) of the Data Block
      text: "",                 // Text to display for the Data Block
      editable_text: true,      // Indicates if the text can be modified
      module: "",               // Module name
      required: false,          // Indicates if it is required (internal, cannot be deleted)
      fields: [],               // Fields of the Data Block
      relationships: [],        // Block-to-block relationships [{ name, related_datablock_id }]
      duplicate_detections: [], // Duplicate detection definition
      save_action_id: "",       // ID of the data block save action
    });

    // 2. Overwrite with provided data
    Object.assign(this, data);

    // 3. Map sub-objects and arrays to their classes
    this.fields = (data.fields || this.fields).map(d => new stic_AwfField(d));
    this.duplicate_detections = (data.duplicate_detections || this.duplicate_detections).map(d => new stic_AwfDuplicateDetection(d));

    if (this.duplicate_detections.length == 0) {
      this.duplicate_detections.push(new stic_AwfDuplicateDetection());
    }
  }

  getValidationErrors() {
    let errors = [];

    // Internal Name validation (if needed)
    if ((this.name ?? "").trim() == '') {
      errors.push(utils.translate('LBL_ERROR_DATABLOCK_NAME'));
    }

    // Title validation
    if ((this.text ?? "").trim() == '') {
      errors.push(utils.translate('LBL_ERROR_DATABLOCK_TITLE'));
    }

    // Fields validation
    if (this.fields && this.fields.length > 0) {
      this.fields.forEach((field, index) => {
        const fieldErrors = field.getValidationErrors();
        if (fieldErrors.length > 0) {
          const fieldName = field.label ? `"${utils.fromFieldLabelText(field.label)}"` : `(${field.text_original})`;
          fieldErrors.forEach(err => {
            errors.push(`${utils.translate('LBL_FIELD')} ${fieldName}: ${err}`);
          });
        }
      });
    } 

    return errors;
  }

  isValid() {
    return this.getValidationErrors().length === 0;
  }

  /**
   * Gets the module information of the current DataBlock
   * @returns {object} ModuleInformation 
   * ModuleInformation: [name, text, textSingular, inStudio, icon, fields:[FieldInformation], relationships:[RelationshipInformation]]
   *   FieldInformation: { name, text, type, required, options, inViews }
   *   RelationshipInformation: { name, text, module_orig, field_orig, relationship, module_dest }
   */
  getModuleInformation() {
    return utils.getModuleInformation(this.module);
  }

  /**
   * Gets the complete field name in HTML for a given field
   * Ex: "MyBlock.my_field" or "_detached.MyBlock.my_field"
   * @param {stic_AwfField} field
   * @returns {string}
   */
  getFieldInputName(field) {
    const prefix = field.type_field === 'unlinked' ? '_detached.' : '';
    return `${prefix}${this.name}.${field.name}`;
  }

  /**
   * Gets the text to show in the description of this DataBlock
   * @returns {string}
   */
  getTextDescription() {
    if (this.module) {
      return `${utils.translateForFieldLabel('LBL_DATABLOCK_MODULE')} ${this.getModuleInformation().text} - ${utils.translateForFieldLabel('LBL_DATABLOCK_INTERNAL_NAME')} ${this.name}`;
    }
    return `${utils.translate('LBL_RELATIONSHIP_NO_MODULE_RELATED')} - ${utils.translateForFieldLabel('LBL_DATABLOCK_INTERNAL_NAME')} ${this.name}`;
  }

  getModuleText() {
    if (this.module) {
      return this.getModuleInformation().text;
    }
    return '';
  }

  /**
   * Gets all fields available to be setted in this DataBlock
   * @returns {array} FieldInformation
   * FieldInformation: { name, text, type, required, options, inViews }
   */
  getAvailableFieldsInformation() {
    let allFieldsInfo = this.getModuleInformation()?.fields;
    if (!allFieldsInfo) {
      return [];
    }
    // FieldInformation: { name, text, type, required, options, inViews }

    let availableFields = Object.values(allFieldsInfo).filter(fi => !this.fields.some(f => f.name == fi.name) );
    availableFields.unshift({name:'', text:'', type:'', required:false, options:[], inViews:true });
    return availableFields;
  }

  /**
   * Add a Field to the DataBlock, from a FieldInformation (the summarized field definition in vardefs)
   * @param {object} moduleField: FieldInformation
   * @returns {stic_AwfField} the field added to DataBlock
   * FieldInformation: { name, text, type, required, default, options, inViews }
   */
  addFieldFromModuleField(moduleField) {
    // FieldInformation: { name, text, type, required, default, options, inViews }

    let field = this.fields.find((f) => f.name === moduleField.name);
    if (!field) {
      field = new stic_AwfField();
      let type_field = 'form';
      if (moduleField.required && moduleField.default != null && moduleField.default != '') {
        type_field = 'fixed';
      }
      field.updateWithFieldInformation(moduleField, type_field);
      field.setValueOptions(utils.getFieldOptions(moduleField));
      field.syncAutomaticValidators();

      field = this.addField(field);
    }
    // Update field info
    field.required = moduleField.required;

    return field;
  }

  /**
   * Add a Field with DuplicateDetection to the DataBlock, from a FieldInformation (the summarized field definition in vardefs)
   * @param {object} moduleField FieldInformation
   * @returns {stic_AwfField} the field added to DataBlock
   * FieldInformation: { name, text, type, required, options, inViews }
   */
  addDuplicateDetectionFromModuleField(moduleField) {
    let field = this.addFieldFromModuleField(moduleField);
    field.required_in_form = true;
    
    if (!this.duplicate_detections[0].fields.find(f => f === field.name)) {
      this.duplicate_detections[0].fields.push(field.name);
    }

    return field;
  }

  addField(field) {
    let newField = new stic_AwfField(field);
    if (newField.type_field == 'fixed') {
      this.fields.unshift(newField);
    }
    else {
      this.fields.push(newField);
    }

    return newField;
  }

  deleteField(fieldName) {
    const index = this.fields.findIndex(f => f.name == fieldName);
    if (index !== -1) {
      this.fields.splice(index, 1);
    }
  }

  updateField(oldName, newField) {
    const index = this.fields.findIndex(f => f.name === oldName);
    
    if (index == -1) {
      return this.addField(newField);
    } else {
      this.fields[index] = newField;
      return newField;
    }
  }

  /**
   * Checks current DataBlock
   */
  checkDataBlock(){
    this.checkDuplicateDetectionFields();
  }

  /**
   * Checks current DataBlock with DuplicateDetection directives
   */
  checkDuplicateDetectionFields(){
    this.duplicate_detections[0].fields.forEach(d => {
      if (!this.fields.find(f => f.name === d)) {
        let field = this.addFieldFromModuleField(this.getModuleInformation().fields[d]);
        field.required_in_form = true;
      }
    });
  }

  /**
   * Gets a suggested name for a new Field for this DataBlock
   * @param {*} fieldName The fieldName 
   * @returns {string} The suggested text for a new DataBlock
   */
  suggestFieldName(fieldName) {
    let index = 0;
    let name = fieldName;
    while(this.fields.some((f) => f.name === name)) {
      index++;
      name = `${fieldName}${index}`;
    }
    return name;
  }

  fixFieldName(field) {
    let index = 0;
    let originalName = field.name;

    while(this.fields.filter((f) => f.name === field.name).length > 1) {
      index++;
      field.name = `${originalName}${index}`;
    }
    return field.name;
  }

  /**
   * Gets a suggested text for a new DataBlock for a module
   * @param {string} moduleName The module
   * @returns {string} The suggested text for a new DataBlock
   */
  suggestDataBlockText(moduleName) {
    let module = utils.getModuleInformation(moduleName);
    if (!module || !module.textSingular) {
      return "";
    }

    let text = module.textSingular;
    let index = 0;
    while(this.data_blocks.some((b) => b.text === text || b.name === name)) {
      index++;
      text = `${module.textSingular} ${index}`;
    }
    return text;
  }

  addRelationship(relName, relatedBlockId, relationshipType = 'many-to-many') {
    // Prevent exact duplicates (same name + same block)
    if (this.relationships.some(r => r.name === relName && r.related_datablock_id === relatedBlockId)) return false;
    this.relationships.push({ name: relName, related_datablock_id: relatedBlockId });
    return true;
  }

  removeRelationship(relName, relatedBlockId = null) {
    if (relatedBlockId) {
      this.relationships = this.relationships.filter(r => !(r.name === relName && r.related_datablock_id === relatedBlockId));
    } else {
      this.relationships = this.relationships.filter(r => r.name !== relName);
    }
  }
}

/**
 * Field: { 
 *   name, label, required, required_in_form, type, in_form, 
 *   type_in_form, value_type, value_options: [{value, text}], value, value_text 
 * }
 */
class stic_AwfField {
  constructor(data = {}) {
    // 1. Set default values
    Object.assign(this, {
      name: '',                // Field name
      text_original: '',       // Original field text
      label: '',               // Label that will appear with the field
      description: '',         // Field description
      required: false,         // Indicates if the field is required in the data block (cannot be deleted)
      merge_filter: '',        // Merge filter in vardefs
      type_field: 'form',      // Field type: unlinked, form, fixed
      required_in_form: false, // Indicates if the field will be required in the form
      in_form: true,           // Indicates if the field will be in the form
      type_in_form: 'text',    // Editor type in the form: text, textarea, number, date, select
      subtype_in_form: 'text', // SubType of editor in the form: text, text_email, text_tel, text_url, text_password, textarea, number, data, date_time, date_datetime...
      type: '',                // Field data type
      value_type: 'editable',  // Value type: editable, selectable, fixed, dataBlock
      value_options: [],       // Options for the field value
      placeholder: '',         // The placeholder or background text in the editor
      value: '',               // The field value
      value_text: '',          // The text to display for the field value
      related_module: '',      // Related module (if applicable)
      validations: [],         // Field validations
    });

    // 2. Overwrite with provided data
    Object.assign(this, data);

    // 3. Map sub-objects and arrays to their classes
    this.value_options = (data.value_options || this.value_options).map(d => new stic_AwfValueOption(d));
    this.validations = (data.validations || this.validations).map(v => new stic_AwfFieldValidation(v));

    if (!data.in_form) {
      this.in_form = this.type_field != 'fixed';
    }
  }

  /**
   * Updates current Field with FieldInformation
   * @param {object} fieldInfo FieldInformation
   * FieldInformation: { name, text, type, required, default, options, inViews }
   */
  updateWithFieldInformation(fieldInfo, typeField) {
    // FieldInformation: { name, text, type, required, default, options, inViews }
    if (!fieldInfo){
      return;
    }
    typeField = typeField || this.type_field;

    this.name = fieldInfo.name;
    this.text_original = fieldInfo.text;
    this.label = utils.toFieldLabelText(fieldInfo.text);
    this.type_field = typeField;
    this.required = fieldInfo.required;
    this.required_in_form = typeField == 'form' && fieldInfo.required;
    this.type = fieldInfo.type;
    this.merge_filter = fieldInfo.merge_filter;


    this.value = "";
    this.value_text = "";

    this.type_in_form = this.getAvailableTypesInForm()[0]?.id;
    this.subtype_in_form = this.getAvailableSubtypesInForm()[0]?.id;
    this.value_type = this.getAvailableValueTypes()[0]?.id;
    if (this.value_type != 'selectable' && this.value_type != 'fixed') {
      // Reset value_options
      this.value_options = [];
    }
    if (fieldInfo.module) {
        this.related_module = fieldInfo.module;
    } else {
        this.related_module = '';
    }
    if (!this.isFieldInForm()) {
      this.label = '';
      this.required_in_form = false;
      this.type_in_form = '';
      this.subtype_in_form = '';
    }

    this.in_form = this.type_field != 'fixed';
    if (this.value_type == 'fixed') {
      if (fieldInfo.default != null && fieldInfo.default != '') {
        this.value = fieldInfo.default;
        this.value_text = fieldInfo.default;
      }
    }

    return this;
  }

  mustInForm() {
    return this.isFieldInForm();
  }

  isFieldInForm() {
    return this.type_field != 'fixed';
  }

  getValidationErrors() {
    let errors = [];
    
    // Name validation
    if ((this.name ?? "").trim() == '') {
      errors.push(utils.translate('LBL_ERROR_FIELD_NAME'));
    }

    if (this.isFieldInForm()) {
      if ((this.label ?? "").trim() == '') {
        errors.push(utils.translate('LBL_ERROR_FIELD_LABEL'));
      }
      if ((this.type_in_form ?? "").trim() == '') {
        errors.push(utils.translate('LBL_ERROR_FIELD_TYPE'));
      }
      if (this.value_type == 'selectable') {
        if (this.value_options.length == 0 || this.value_options.every(o => !o.is_visible)) {
          errors.push(utils.translate('LBL_ERROR_FIELD_OPTIONS'));
        }
      }
    } else {
      if (this.value == '') {
        errors.push(utils.translate('LBL_ERROR_FIELD_FIXED_EMPTY'));
      }
    }
    return errors;
  }

  isValid() {
    return this.getValidationErrors().length === 0;
  }

  isSelectCustomOptions() {
    if (this.type_field == 'unlinked' && this.type_in_form == "select") {
      return true;
    }
    if (this.type_in_form == "select" && 
        this.type != "relate" && 
        this.type != "enum" && this.type != "radioenum" && this.type != "multienum" && this.type != "bool" && this.type != "checkbox") {
      return true;
    }
    return false;
  }

  getAvailableValueTypes() {
    if (this.name == "") {
      return [];
    }
    if (this.type_field == 'fixed') {
      // if (this.type == 'relate') {
      //   return stic_AwfField.value_typeList().filter(t => t.id == 'fixed' || t.id == 'dataBlock');
      // }
      return stic_AwfField.value_typeList().filter(t => t.id == 'fixed');
    }

    // Form or unlinked
    if (this.type == 'relate' || this.type == 'enum' || this.type == 'multienum') {
      return stic_AwfField.value_typeList().filter(t => t.id == 'selectable');
    }

    return stic_AwfField.value_typeList().filter(t => t.id == 'editable');
  }

  getAvailableTypesInForm() {
    if (this.name == "") {
      return [];
    }
    if (!this.isFieldInForm()) {
      return [];
    }
    if (this.type_field == 'unlinked') {
      return stic_AwfField.type_in_formList();
    }

    let availableTypes = [];

    // text, textarea, number, date, select, hidden
    if (this.type == "enum" || this.type == "radioenum" || this.type == "multienum" || this.type == "bool" || this.type == "checkbox") {
      availableTypes = stic_AwfField.type_in_formList().filter(t => t.id == "select" || t.id == "hidden");
    } 
    else if (this.type == "relate") {
      availableTypes = stic_AwfField.type_in_formList().filter(t => t.id == "select" || t.id == "hidden");
    }
    else if (this.type == "date" || this.type == "time" || this.type == "datetime" || this.type == "datetimecombo") {
      availableTypes = stic_AwfField.type_in_formList().filter(t => t.id == "date" || t.id == "hidden");
    }
    else if (this.type == "int" || this.type == "float" || this.type == "double" || this.type == "decimal") {
      availableTypes = stic_AwfField.type_in_formList().filter(t => t.id == "number" || t.id == "select" || t.id == "hidden");
    }
    else if (this.type == "json" || this.type == "textarea" || this.type == "longtext") {
      availableTypes = stic_AwfField.type_in_formList().filter(t => t.id == "textarea" || t.id == "hidden");
    }
    else if (this.type == "name" || this.type == "phone" || this.type == "email" || this.type == "url" || 
             this.type == "password" || this.type == "encrypt") {
      availableTypes = stic_AwfField.type_in_formList().filter(t => t.id == "text" || t.id == "select" || t.id == "hidden");
    }
    else  {
      availableTypes = stic_AwfField.type_in_formList().filter(t => t.id == "text" || t.id == "textarea" || t.id == "number" || t.id == "select" || t.id == "hidden");
    }
    
    if (this.required) {
      availableTypes = availableTypes.filter(t => t.id !== "hidden");
    }

    return availableTypes;
  }

  getTypeInActions() {
    switch (this.type) {
      case "textarea":
      case "json":
        return "textarea";
      case "int":
        return "integer";
      case "float":
      case "double":
      case "decimal":
        return "float";
      case "bool":
        return "boolean";
      case "date":
        return "date";
      case "time":
        return "time";
      case "datetime":
      case "datetimecombo":
        return "datetime-local";
      case "email":
        return "email";
      case "phone":
        return "tel";
      case "url":
        return "url";
      case "id":
      case "link":
      case "relate":
        return "relate";
      default:
        return "text";
    }
  }

  getAvailableSubtypesInForm() {
    if (this.name == "") {
      return [];
    }
    if (!this.isFieldInForm()) {
      return [];
    }
    if (this.type_in_form == "") {
      return [];
    }

    let base_subtypes = stic_AwfField.subtype_in_formList().filter(s => s.id == this.type_in_form || s.id.startsWith(this.type_in_form + '_'));

    if (this.type_field == 'unlinked') {
      return base_subtypes;
    }

    if (base_subtypes.length <= 1) {
      return base_subtypes;
    }

    let list = [];
    if (this.isSelectCustomOptions()) {
      list.push(base_subtypes.find(s => s.id == "select"));
      list.push(base_subtypes.find(s => s.id == "select_radio"));
      
    } else if (this.type == "phone") {
      list.push(base_subtypes.find(s => s.id == "text_tel"));
      list.push(base_subtypes.find(s => s.id == "text"));

    } else if (this.type == "url") {
      list.push(base_subtypes.find(s => s.id == "text_url"));
      list.push(base_subtypes.find(s => s.id == "text"));

    } else if (this.type == "email" || (this.type == "varchar" && this.name.toLowerCase().startsWith("email"))) {
      list.push(base_subtypes.find(s => s.id == "text_email"));
      list.push(base_subtypes.find(s => s.id == "text"));

    } else if (this.type == "password" || this.type == "encrypt") {
      list.push(base_subtypes.find(s => s.id == "text_password"));
      list.push(base_subtypes.find(s => s.id == "text"));

    } else if (this.type == "date") {
      list.push(base_subtypes.find(s => s.id == "date"));

    } else if (this.type == "time") {
      list.push(base_subtypes.find(s => s.id == "date_time"));

    } else if (this.type == "datetime" || this.type == "datetimecombo") {
      list.push(base_subtypes.find(s => s.id == "date_datetime"));
      list.push(base_subtypes.find(s => s.id == "date"));

    } else if (this.type == "enum" || this.type == "radioenum" || this.type == "relate") {
      list.push(base_subtypes.find(s => s.id == "select"));
      list.push(base_subtypes.find(s => s.id == "select_radio"));

    } else if (this.type == "bool" || this.type == "check") {
      list.push(base_subtypes.find(s => s.id == "select_checkbox"));
      list.push(base_subtypes.find(s => s.id == "select_switch"));
      list.push(base_subtypes.find(s => s.id == "select"));
      list.push(base_subtypes.find(s => s.id == "select_radio"));

    } else if (this.type == "multienum") {
      list.push(base_subtypes.find(s => s.id == "select_multiple"));
      list.push(base_subtypes.find(s => s.id == "select_checkbox_list"));
      list.push(base_subtypes.find(s => s.id == "select"));
      list.push(base_subtypes.find(s => s.id == "select_radio"));
    }

    if (list.length > 0) {
        return list.filter(item => item); // Remove undefined items
    }
    return base_subtypes;
  }

  acceptPlaceholder() {
    return this.type_in_form == "text" || this.type_in_form == "textarea" || this.type_in_form == "number";
  }

  acceptValueOptions() {
    return this.type_in_form == "select" && this.subtype_in_form != "select_checkbox" && this.subtype_in_form != "select_switch";
  }

  setValueOptions(originalOptions) {
    if (this.isSelectCustomOptions() && this.acceptValueOptions()) {
      if ((this.value_options?.length ?? 0) == 0) {
        this.value_options = [new stic_AwfValueOption()];
      }
      return this.value_options;
    }
    if (this.type_field == 'form' && this.type == 'relate' && (originalOptions?.length ?? 0) == 0 ) {
      return this.value_options;
    }

    this.value_options = [];
    if (originalOptions) {
      originalOptions.forEach(o => {
        this.value_options.push(new stic_AwfValueOption({
          value: o.id,
          is_visible: true,
          text_original: o.text,
          text: o.text,
        }));
      });
    }
    return this.value_options;
  }
  
  isOptionValueModified() {
    return this.value_options.some(o => !o.is_visible || o.text_original !== o.text);
  }

  addOrUpdateValidation(validation) {
    let newValidation = new stic_AwfFieldValidation(validation);

    const index = this.validations.findIndex(v => v.name === validation.name);
    if (index == -1) {
      this.validations.push(newValidation);
    } else {
      this.validations[index] = newValidation;
    }
    return newValidation;
  }

  /**
   * Synchronize automatic validators based on field name, type and subtype in form.
   * Add those that apply and remove automatic ones that no longer apply.
   */
  syncAutomaticValidators() {
    const definedActions = utils.getDefinedActions();
    if (!definedActions) return;

    const validatorActions = definedActions.filter(a => a.type === 'Validator');

    validatorActions.forEach(actionDef => {
      const rules = actionDef.autoApplyRules;
      if (!rules) return;

      let isMatch = false;

      // Check by Field Type (vardef type) - ex: 'email', 'phone'
      if (rules.types && rules.types.includes(this.type)) {
        isMatch = true;
      }

      // Check by Subtype in form (editor) - ex: 'text_email', 'number'
      if (!isMatch && rules.subtypes_in_form && rules.subtypes_in_form.includes(this.subtype_in_form)) {
        isMatch = true;
      }

      // Check by Name pattern
      if (!isMatch && rules.name_patterns && rules.name_patterns.length > 0) {
        rules.name_patterns.forEach(pattern => {
          try {
            // Clean PHP strings like "/^email/i"
            const parts = pattern.match(/^\/(.*?)\/([a-z]*)$/);
            let regex;
            if (parts) {
                regex = new RegExp(parts[1], parts[2]);
            } else {
                regex = new RegExp(pattern);
            }
            
            if (regex.test(this.name)) {
              isMatch = true;
            }
          } catch(e) { console.warn("Invalid Regex in AutoApplyRules", pattern); }
        });
      }

      // Actions
      const existingIndex = this.validations.findIndex(v => v.validator === actionDef.name);
      if (isMatch) {
        // If it needs to be applied and it doesn't exist, we add it.
        if (existingIndex === -1) {
          this.validations.push(new stic_AwfFieldValidation({
            name: utils.newId('val_'),
            validator: actionDef.name,
            message: actionDef.defaultErrorMessage || '',
            params: {},
            is_automatic: true // Mark as automatic
          }));
        }
      } else {
        // If we do NOT have to apply it, but it exists...
        if (existingIndex !== -1) {
          // Delete it if it was created automatically.
          // Respect it if user added it manually.
          if (this.validations[existingIndex].is_automatic) {
            this.validations.splice(existingIndex, 1);
          }
        }
      }
    });
  }

  static type_fieldList(asString = false) {
    return utils.getList("stic_awf_forms_field_type_field_list", asString);
  }
  get type_fieldText(){
    return stic_AwfField.type_fieldList().find(i => i.id == this.type_field)?.text;  
  }

  static type_in_formList(asString = false){
    return utils.getList("stic_awf_forms_field_type_in_form_list", asString);
  }
  get type_in_formText(){
    return stic_AwfField.type_in_formList().find(i => i.id == this.type_in_form)?.text;  
  }

  static subtype_in_formList(asString = false){
    return utils.getList("stic_awf_forms_field_subtype_in_form_list", asString);
  }
  get subtype_in_formText(){
    return stic_AwfField.subtype_in_formList().find(i => i.id == this.subtype_in_form)?.text;  
  }

  static value_typeList(asString = false){
    return utils.getList("stic_awf_forms_field_value_type_list", asString);
  }
  get value_typeText(){
    return stic_AwfField.value_typeList().find(i => i.id == this.value_type)?.text;  
  }
}

class stic_AwfFieldValidation {
  constructor(data = {}) {
    // 1. Set default values
    Object.assign(this, {
      name: '',         // Validation name
      validator: '',    // Name of the validation action (ex: RegexValidatorAction)
      message: '',      // Custom error message
      params: {},       // Parameters (ex: { pattern: '...' })
      conditions: [],   // Conditions to execute the validation (all must be accomplished)

      is_automatic: false, // Indicates if the validation is automatic
    });

    // 2. Overwrite with provided data
    Object.assign(this, data);

    // 3. Map sub-objects and arrays to their classes
    this.conditions = (data.conditions || this.conditions).map(c => new stic_AwfCondition(c));
  }

  isValid() {
    if ((this.name??"").trim()=='') return false;
    if ((this.validator??"").trim()=='') return false;

    return true;
  }
}

/**
 * ValueOption: { 
 *    value, is_visible, text_original, text
 *  }
 */
class stic_AwfValueOption{
  constructor(data = {}) {
    // 1. Set default values
    Object.assign(this, {
      value: '',          // Option value
      is_visible: true,   // Indicates if it will be shown
      text_original: '',  // Original option text
      text: '',           // Option text
    });

    // 2. Overwrite with provided data
    Object.assign(this, data);

    // 3. Map sub-objects and arrays to their classes
  }
}

/**
 * DuplicateDetection: {
 *   fields: [<field_name>], on_duplicate
 * }
 */
class stic_AwfDuplicateDetection {
  constructor(data = {}) {
    // 1. Set default values
    Object.assign(this, {
      fields: [],              // Array with the name of fields for duplicate detection
      on_duplicate: "enrich"   // Action to perform with duplicates: update, enrich, skip, error
    });

    // 2. Overwrite with provided data
    Object.assign(this, data);

    // 3. Map sub-objects and arrays to their classes
  }

  static on_duplicateList(asString = false){
    return utils.getList("stic_awf_forms_datablock_duplicate_action_list", asString);
  }
  get on_duplicateText(){
    return stic_AwfDuplicateDetection.on_duplicateList().find(i => i.id == this.on_duplicate)?.text;  
  }
}

/**
 * Flow: {
 *   name,
 *   actions: [{ order, action_name, params: [{name, source, value}],
 * } 
 */
class stic_AwfFlow {
  constructor(data = {}) {
    // 1. Set default values
    Object.assign(this, {
      id: utils.newId("awffa"), // ID of the action flow
      name: "",                 // Name of the action flow
      label: "",                // The label to translate for the name
      text: "",                 // The text to display
      actions: [],              // The actions of the Flow
    });

    // 2. Overwrite with provided data
    Object.assign(this, data);

    // 3. Map sub-objects and arrays to their classes
    this.actions = (data.actions || this.actions).map(f => new stic_AwfAction(f));
  }

  getText() {
    if (this.id && (String(this.id).endsWith('_ok') || String(this.id).endsWith('_err'))) {
      const isOk = String(this.id).endsWith('_ok');
      const parentActionId = isOk ? this.id.slice(0, -3) : this.id.slice(0, -4);

      if (window.alpineComponent && window.alpineComponent.formConfig) {
        for (const f of window.alpineComponent.formConfig.flows) {
          const parentAction = f.actions.find(a => a.id === parentActionId);
          if (parentAction) {
            const suffixText = isOk ? parentAction.flow_success_text : parentAction.flow_error_text;
            return (parentAction.text || parentAction.title) + ": " + (suffixText || "");
          }
        }
      }
    }
    
    return this.label != "" ? utils.translate(this.label) : this.text;
  }

  hasTerminalAction() {
    return this.actions.some(a => a.is_terminal);
  }
}

class stic_AwfCondition {
    constructor(data = {}) {
      // 1. Set default values
      Object.assign(this, {
        field_name: '',          // Field name to evaluate
        operator: 'Equal_To',    // Operator 
        value: '',               // Value to compare
    });

    // 2. Overwrite with provided data
    Object.assign(this, data);

    // 3. Map sub-objects
    }

    isEmpty() {
        return this.field_name === '' || this.value === '';
    }

    // TODO: Expand the list of operators (see: stic_custom_views_operator_list) 
}

class stic_AwfAction {
  constructor(data = {}) {
    // 1. Set default values
    Object.assign(this, {
      id: utils.newId("awfa"),  // Action ID
      name: "",                 // Internal name of the action
      title: "",                // Action title (generic name)
      text: "",                 // Text to display for the action
      description: "",          // Action description
      requisite_actions: [],    // Array with the identifiers of the actions prior to the current one
      category: 'data',         // Action category
      parameters: [],           // Action parameters
      is_user_selectable: true, // Indicates if the action is user selectable
      is_automatic: false,      // Indicates if the action is automatic
      is_terminal: false,       // Indicates if the action is terminal
      order: 0,                 // Execution order of the action
      conditions: [],           // Conditions to execute the action (all must be accomplished)
      continue_on_error: false, // Indicates if the flow should continue if this action fails
      flow_success_id: null,    // ID of the success flow
      flow_error_id: null,      // ID of the failure flow
      flow_success_text: null,  // Final text for the success flow
      flow_error_text: null,    // Final text for the failure flow
    });

    this.flow_success_text = utils.translate("LBL_FLOW_DEFERRED_MAIN");
    this.flow_error_text = utils.translate("LBL_FLOW_ONERROR");

    // 2. Overwrite with provided data
    Object.assign(this, data);

    // 3. Map sub-objects and arrays to their classes
    this.parameters = (data.parameters || this.parameters).map(a => new stic_AwfActionParameter(a));
    this.conditions = (data.conditions || this.conditions).map(c => new stic_AwfCondition(c));
  }

  get is_fixed_order() {
    if (this.is_automatic && this.order == -1) return false;
    return this.order != 0;
  }

  isValid() {
    const allActions = utils.getDefinedActions();
    const actionDef = allActions.find(a => a.name === this.name);
    return this.parameters.every(param => {
      if (!param.required) return true;
      if (param.type === 'optionSelector' && actionDef) {
        const paramDef = actionDef.parameters.find(p => p.name === param.name);
        const optDef = (paramDef?.selectorOptions || []).find(o => o.name === param.selectedOption);
        if (optDef?.resolvedType === 'empty') {
          param.value = optDef.name;
          param.value_text = optDef.text;
          return true;
        }
      }
      return param.value !== null && param.value !== '';
    });
  }

  static category_in_formList(asString = false){
    return utils.getList("stic_awf_forms_action_definition_category_list", asString);
  }
  get category_in_formText(){
    return stic_AwfAction.category_in_formList().find(i => i.id == this.category)?.text;  
  }
}

class stic_AwfActionParameter {
  constructor(data = {}) {
    // 1. Set default values
    Object.assign(this, {
      name: '',                // Parameter name
      text: '',                // Parameter text
      type: '',                // Parameter type: value, field, dataBlock, crmRecord, optionSelector
      dataType: '',            // Data type for value parameters: text, number, date, etc.
      required: false,         // Indicates if the parameter is required
      value: '',               // Parameter value
      value_text: '',          // Text to display for the parameter value
      selectedOption: '',      // Selected option (if applicable)
    });

    // 2. Overwrite with provided data
    Object.assign(this, data);

    // 3. Map sub-objects and arrays to their classes
  }
}

class stic_AwfLayout {
  constructor(data = {}) {
    // 1. Set default values
    Object.assign(this, {
      theme: new stic_AwfTheme(),      // Visual variables of the form

      web_title: utils.translate('LBL_THEME_WEB_TITLE_VALUE'),  // Title of the web page

      header_html: '',             // Html with the header of the form
      footer_html: '',             // Html with the footer of the form

      // Submit button text
      submit_button_text: utils.translate('LBL_THEME_SUBMIT_BUTTON_TEXT_VALUE'),

      // Text in case of closed form
      closed_form_title: utils.translate('LBL_THEME_CLOSED_FORM_TITLE_VALUE'),
      closed_form_text: utils.translate('LBL_THEME_CLOSED_FORM_TEXT_VALUE'),

      // Default text: Data processed
      processed_form_title: utils.translate('LBL_THEME_PROCESSED_FORM_TITLE_VALUE'),
      processed_form_text: utils.translate('LBL_THEME_PROCESSED_FORM_TEXT_VALUE'),

      // Default text: Data received
      receipt_form_title: utils.translate('LBL_THEME_RECEIPT_FORM_TITLE_VALUE'),
      receipt_form_text: utils.translate('LBL_THEME_RECEIPT_FORM_TEXT_VALUE'),

      custom_css: '',              // Custom CSS
      custom_js: '',               // Custom JS

      structure: [],               // Array of Sections
    });

    // 2. Overwrite with provided data
    Object.assign(this, data);

    // 3. Map sub-objects
    this.theme = new stic_AwfTheme(data.theme ?? {});
    this.structure = (data.structure || this.structure).map(s => new stic_AwfLayoutSection(s));

    // Decode: If it comes from the DB (JSON), it will come in Base64. If it is new, it will be empty.
    this.header_html = utils.fromBase64(this.header_html);
    this.footer_html = utils.fromBase64(this.footer_html);
    if (this.custom_css) this.custom_css = utils.fromBase64(this.custom_css);
    if (this.custom_js) this.custom_js = utils.fromBase64(this.custom_js);
  }

  /**
   * Synchronizes the visual structure with the actual data blocks.
   *  - Removes references to deleted blocks
   *  - Removes duplicate blocks (keeps only the first occurrence)
   *  - Removes/Ignores blocks that have no visible fields
   *  - Adds new data blocks at the end.
   * @param {stic_AwfDataBlock[]} dataBlocks Current list of data blocks
   */
  syncWithDataBlocks(dataBlocks) {
    const placedBlockIds = new Set(); // Set of placed blocks
    const cleanStructure = [];

    // Cleanup of the blocks of the visual structure
    this.structure.forEach(section => {
      const validElements = section.elements = section.elements.filter(el => {
        if (el.type != 'datablock') return true; // It's not a block: keep it

        // We check that the block exists
        const block = dataBlocks.find(b => b.id === el.ref_id);
        
        if (!block) return false; // The block no longer exists
        if (placedBlockIds.has(el.ref_id)) return false; // It's a duplicate
        
        // Check field visibility
        // (if it has only fixed or hidden fields, it should not be in the layout)
        if (!block.fields.some(f => f.type_field !== 'fixed' && f.type_in_form !== 'hidden')) return false;
          
        // Mark the block as placed
        placedBlockIds.add(el.ref_id); 
        return true;
      });
      section.elements = validElements;

      if (section.elements.length > 0) {
        cleanStructure.push(section);
      }
    });

    this.structure = cleanStructure;

    // Add the missing blocks
    const orphanBlocks = dataBlocks.filter(b => {
      if (placedBlockIds.has(b.id)) return false; // The block is placed
      if (!b.fields.some(f => f.type_field !== 'fixed' && f.type_in_form !== 'hidden')) return false; // Only has fixed or hidden fields

      return true;
    });

    if (orphanBlocks.length > 0) {
      // Create a section for each block
      orphanBlocks.forEach(block => {
        this._addSectionWithBlock(block);
      });
    }
  }

  _addSectionWithBlock(block) {
    const section = new stic_AwfLayoutSection({
      title: block.text, 
    });
    
    const element = new stic_AwfLayoutElement({
      type: 'datablock',
      ref_id: block.id
    });

    section.elements.push(element);
    this.structure.push(section);
  }

  addSection(title) {
    this.structure.push(new stic_AwfLayoutSection({ title: title }));
  }
}

class stic_AwfTheme {
  constructor(data = {}) {
    Object.assign(this, {
      primary_color: STIC.mainThemeColor ?? '#0d6efd',  // Default corporate color
      page_bg_color: '#f8f9fa',  // Page background (very light gray)
      form_bg_color: '#ffffff',  // Form background (white)

      border_radius_container: 10, // Rounding for containers in px (10px). Range: [0..40]
      border_radius_controls: 4,   // Rounding for containers in px (4px). Range: [0..20]

      text_color: '#212529',     // Text color (dark gray)
      border_color: '#dee2e6',   // Border color (light gray)
      border_width: 1,             // Border width in px

      floating_labels: true,       // Indicates if floating labels will be used in the controls (true)
      
      // Typography
      font_family: "system-ui, -apple-system, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif",

      font_size: 16,               // Font size in px.
      form_width: '800px',         // Maximum width of the form. String to allow %, px, rem
      shadow_intensity: 'normal',  // Shadow intensity: 'none', 'light', 'normal', 'heavy'
      
      // Structure (Grid)
      sections_per_row: 1,         // Sections per row (1, 2 or 3)
      fields_per_row: 2,           // Fields per row (1, 2, 3 or 4)

      field_spacing: '1rem',       // Spacing between fields
      equal_height_sections: true, // Indicates if sections will have the same height
      label_weight_bold: false,    // Bold in the labels
      submit_full_width: false,    // Full width of the submit button
      input_style: 'standard',     // Style of the fields: 'standard', 'flat', 'filled'
    });

    // 2. Overwrite with data
    Object.assign(this, data);
  }

  static shadow_intensity_in_formList(asString = false){
    return utils.getList("stic_awf_forms_layout_theme_shadow_intensity_list", asString);
  }
  get shadow_intensity_in_formText(){
    return stic_AwfTheme.shadow_intensity_in_formList().find(i => i.id == this.shadow_intensity)?.text;  
  }

  static input_style_in_formList(asString = false){
    return utils.getList("stic_awf_forms_layout_theme_input_style_list", asString);
  }
  get input_style_in_formText(){
    return stic_AwfTheme.input_style_in_formList().find(i => i.id == this.input_style)?.text;  
  }

  static form_width_in_formList(asString = false){
    return utils.getList("stic_awf_forms_layout_theme_form_width_list", asString);
  }
  get form_width_in_formText(){
    return stic_AwfTheme.form_width_in_formList().find(i => i.id == this.form_width)?.text;  
  }

  static field_spacing_in_formList(asString = false){
    return utils.getList("stic_awf_forms_layout_theme_field_spacing_list", asString);
  }
  get field_spacing_in_formText(){
    return stic_AwfTheme.field_spacing_in_formList().find(i => i.id == this.field_spacing)?.text;  
  }
}

/**
 * Defines a visual container
 */
class stic_AwfLayoutSection {
  constructor(data = {}) {
    Object.assign(this, {
      id: utils.newId('sect'), // ID of the section
      title: "",               // Title to display
      subtitle: "",            // Subtitle to display
      showTitle: true,         // Indicates if the title will be shown
      isCollapsible: false,    // Indicates if the section can be collapsed
      isCollapsed: false,      // Indicates if the section will appear initially collapsed
      
      containerType: 'panel',  // Type of visual container: 'panel' (simple), 'card' (with border), 'tabs', 'accordion'
      elements: [],
    });

    Object.assign(this, data);

    this.elements = (data.elements || this.elements).map(e => new stic_AwfLayoutElement(e));
  }

  static containerType_in_formList(asString = false){
    return utils.getList("stic_awf_forms_layout_structure_container_type_list", asString);
  }
  get containerType_in_formText(){
    return stic_AwfLayoutSection.containerType_in_formList().find(i => i.id == this.containerType)?.text;  
  }
}

/**
 * Element inside a visual container
 */
class stic_AwfLayoutElement {
  constructor(data = {}) {
    Object.assign(this, {
      id: utils.newId('el'),  // ID of the element
      
      type: 'datablock',      // Element type: 'datablock' (possible extensions: 'line', etc)
      ref_id: '',             // Reference ID (the ID of the stic_AwfDataBlock)
    });

    Object.assign(this, data);
  }
}

class stic_AwfConfiguration {
  constructor(data = {}) {
    // 1. Set default values
    Object.assign(this, {
      data_blocks: [],          // The Data Blocks
      flows: [],                // The Action Flows
      layout: new stic_AwfLayout(), // The Layout

      _lastDataBlocksHash: "",  // Internal hash to control changes in data blocks
    });

    // 2. Overwrite with provided data
    Object.assign(this, data);

    // 3. Map sub-objects and arrays to their classes
    this.data_blocks = (data.data_blocks || this.data_blocks).map(d => new stic_AwfDataBlock(d));
    this.flows = (data.flows || this.flows).map(d => new stic_AwfFlow(d));
    this.layout = new stic_AwfLayout(data.layout || {})

    // 4. Ensure default objects
    this._ensureDefaultDataBlocks();
    this._ensureDefaultFlows();
    this._ensureDefaultLayout();
  }
  static fromJSON(jsonString){
    const config = new stic_AwfConfiguration(JSON.parse(jsonString));
    config._lastDataBlocksHash = config._computeDataBlocksHash();
    
    return config;
  }

  /**
   * Generates a simple hash/string representation of the DataBlocks.
   * Used to detect changes in structure that require Action regeneration.
   */
  _computeDataBlocksHash() {
    return JSON.stringify(this.data_blocks);
  }

  /**
   * Ensures internal consistency before saving.
   * Checks if DataBlocks have changed before triggering regeneration.
   */
  prepareForSave() {
    const currentHash = this._computeDataBlocksHash();

    // Only regenerate actions if the hash has changed since last time
    if (currentHash !== this._lastDataBlocksHash) {
      console.log("AWF: DataBlocks structure changed. Regenerating automatic actions...");
      this.regenerateAutomaticActions();
        
      // Update the known hash
      this._lastDataBlocksHash = currentHash;
    }
  }

  toJSONString() {
    // Check for changes and regenerate if needed
    this.prepareForSave();

    const clone = JSON.parse(JSON.stringify(this));

    // Delete internal properties
    delete clone._lastDataBlocksHash;

    if (clone.layout) {
        clone.layout.header_html = utils.toBase64(clone.layout.header_html);
        clone.layout.footer_html = utils.toBase64(clone.layout.footer_html);
        if (clone.layout.custom_css) clone.layout.custom_css = utils.toBase64(clone.layout.custom_css);
        if (clone.layout.custom_js) clone.layout.custom_js = utils.toBase64(clone.layout.custom_js);
    }
    return JSON.stringify(clone);
  }

  _ensureDefaultDataBlocks() {
    // No default DataBlocks!
  }
  _ensureDefaultFlows() {
    // Check exists Main Flow
    if (!this.flows.some(f => f.id == '0')) {
      this.flows.push(new stic_AwfFlow({ id: 0, name: "main", label: "LBL_FLOW_MAIN" }));
    }

    // Check exists OnError Flow
    if (!this.flows.some(f => f.id == '-1')) {
      this.flows.push(new stic_AwfFlow({ id: -1, name: "onError", label: "LBL_FLOW_ONERROR" }));
    }
    
    // Check exists Receipt Flow
    if (!this.flows.some(f => f.id == '1')) {
      this.flows.push(new stic_AwfFlow({ id: 1, name: "receipt", label: "LBL_FLOW_RECEIPT" }));
    }

    // Sort flows: Receipt, Main, Error
    this.flows.sort((a, b) => {
        const order = { '1': 0, '0': 1, '-1': 2 }; // Receipt -> Main -> Error
        return (order[a.id] ?? 99) - (order[b.id] ?? 99);
    });
  }
  _ensureDefaultLayout() {
    // No default Layout!!
  }

 /**
   * Gets a suggested text for a new DataBlock for a module
   * @param {string} moduleName The module
   * @returns {string} The suggested text for a new DataBlock
   */
  suggestDataBlockText(moduleName) {
    let module = utils.getModuleInformation(moduleName);
    if (!module || !module.textSingular) {
      return "";
    }

    let text = module.textSingular;
    let index = 0;
    while(this.data_blocks.some((b) => b.text === text || b.name === name)) {
      index++;
      text = `${module.textSingular} ${index}`;
    }
    return text;
  }

  prepareProcessingMode(mode) { 
    if (mode == 'async') {
      // Remove terminal actions from main flow
      const mainFlow = this.flows.find(f => f.id == '0');
      if (mainFlow) {
        mainFlow.actions = mainFlow.actions.filter(a => !a.is_terminal);
      }  
    }
  }

  /**
   * Gets a new string cleaned to be used as internal name of fields
   * @param {string} name 
   * @returns {string}
   */
  static cleanName(name){
    // Convert to lowercase and normalize accents
    name = name.toLowerCase().normalize("NFD").replace(/[\u0300-\u036f]/g, "");

    // Replace any non valid char
    let nameClean = name.replace(/[^a-z0-9-]/g, "_");

    // Remove repeated _ or at the end
    nameClean = nameClean.replace(/_+/g, "_").replace(/_$/g, "");

    // If first char is a number add a preffix
    if (nameClean.match(/^[0-9]/)) {
      nameClean = "_" + nameClean;
    }

    return nameClean;
  }

  /**
   * Gets a safe name for a DataBlock using PascalCase of the moduleName.
   * Ex: "stic_AWF_Forms" -> "SticAdvancedWebForms"
   * Ex: "Contacts" -> "Contacts"
   */
  static getSafeNameFromModule(moduleName) {
    if (!moduleName) 
      return "Module";

    return moduleName
      .split(/[^a-zA-Z0-9]/)                                     // Split by "_" or non alphanumeric chars
      .filter(part => part.length > 0)                           // Remove empty parts
      .map(part => part.charAt(0).toUpperCase() + part.slice(1)) // Capitalize every part
      .join('');                                                 // Join without separators
  }

  updateDataBlockText(dataBlock, newText) {
    const oldName = dataBlock.name;
    let text = newText;
    let baseText = text.trim();
    let index = 0;
    while(this.data_blocks.some((b) => b.id != dataBlock.id && (b.text === text))) {
      index++;
      text = `${baseText} ${index}`;
    }
    dataBlock.text = text;

    // Update references in Actions
    this.flows.forEach(flow => {
      flow.actions.forEach(action => {
        action.parameters.forEach(param => {
          // Update value_text to this dataBlock
          if (param.value == dataBlock.id) {
            param.value_text = text;
          }

          //Update references to fields ("OldBlockName.Field" -> "NewBlockName.Field")
          if (typeof param.value === 'string') {
            const prefixOld = `${oldName}.`;
            const prefixNew = `${dataBlock.name}.`;
            
            const prefixDetachedOld = `_detached.${oldName}.`;
            const prefixDetachedNew = `_detached.${dataBlock.name}.`;

            if (param.value.startsWith(prefixOld)) {
              param.value = param.value.replace(prefixOld, prefixNew);
            } else if (param.value.startsWith(prefixDetachedOld)) {
              param.value = param.value.replace(prefixDetachedOld, prefixDetachedNew);
            }
          }

        });
      });
    });
  }

  /**
   * Gets a new DataBlock for specified module
   * @param {string} moduleName Module
   * @param {boolean} force 
   * @param {string} text 
   * @returns {stic_AwfDataBlock}
   */
  addDataBlockModule(moduleName, force = false, text = "") {
    let module = utils.getModuleInformation(moduleName);
    if (text == "") text = module.textSingular;

    // Find DataBlock for module
    let dataBlock = null;
    if (!force) {
      dataBlock = this.data_blocks.find((d) => d.module == moduleName);
      if (dataBlock) {
        return dataBlock;
      }
    }

    // Create DataBlock for module

    // Set unique text
    let baseText = text;
    let index = 0;
    while(this.data_blocks.some((b) => b.text === text)) {
      index++;
      text = `${baseText} ${index}`;
    }

    // Set unique name with Module name
    let baseName = stic_AwfConfiguration.getSafeNameFromModule(moduleName);
    index = 0;
    let name = `${baseName}${index}`; // Ex: SticAdvancedWebForms0
    while(this.data_blocks.some((b) => b.name === name)) {
      index++;
      name = `${baseName}${index}`;
    }

    dataBlock = new stic_AwfDataBlock({
      name: name,
      text: text,
      module: moduleName,
    });

    // Set initial fields 
    let hasRequiredRelate = false;
    for (const fieldDef of Object.values(module.fields)) {
      if (fieldDef.required && fieldDef.type === 'relate') {
        hasRequiredRelate = true;
      }
      if (fieldDef.required) {
        let newField = new stic_AwfField();
        let type_field = 'form';
        if (fieldDef.required && fieldDef.default != null && fieldDef.default != '') {
            type_field = 'fixed';
        }
        newField.updateWithFieldInformation(fieldDef, type_field);
        newField.setValueOptions(utils.getFieldOptions(fieldDef));

        newField.required = fieldDef.required;
        this.addDataBlockField(dataBlock, newField);

        // Add in Duplicate detection (only if merge_filter == 'selected')
        if (fieldDef.merge_filter == 'selected') {
          dataBlock.addDuplicateDetectionFromModuleField(fieldDef);
        }
      }
    }

    this.data_blocks.push(dataBlock);

    return dataBlock;
  }

  /**
   * Gets a new DataBlock unlinked (without module)
   * @param {string} text 
   * @returns {stic_AwfDataBlock}
   */
  addUnlinkedDataBlock(text) {
    // Generate a secure unique name
    let baseName = "NoModuleBlock";
    let index = 0;
    let name = `${baseName}${index}`;

    while(this.data_blocks.some((b) => b.name === name)) {
      index++;
      name = `${baseName}${index}`;
    }

    // Create DataBlock
    let dataBlock = new stic_AwfDataBlock({
      name: name,
      text: text,
      module: "", // No module set
      editable_text: true,
      required: false
    });

    this.data_blocks.push(dataBlock);

    return dataBlock;
  }

  /**
   * Adds a field to a DataBlock
   * @param {stic_AwfDataBlock} dataBlock 
   * @param {stic_AwfField} field 
   * @returns {stic_AwfField}
   */
  addDataBlockField(dataBlock, field) {
    return dataBlock.addField(field);
  }

  /**
   * Adds or Updates a validation to a field
   * @param {stic_AwfField} field 
   * @param {stic_AwfFieldValidation} validation 
   * @returns {stic_AwfFieldValidation}
   */
  addOrUpdateFieldValidation(field, validation) {
    return field.addOrUpdateValidation(validation);
  }

  syncLayoutWithDataBlocks() {
    this.layout.syncWithDataBlocks(this.data_blocks);
  }

  /**
   * Deletes a DataBlock, removing all field references to the DataBlock
   * @param {stic_AwfDataBlock} dataBlock 
   */
  deleteDataBlock(dataBlock) {
    // Remove fields that reference this DataBlock (prevents "Fixed field without assigned value" errors)
    this.data_blocks.forEach(d => {
      let fieldsToRemove = d.fields.filter(f => f.value_type == 'dataBlock' && f.value == dataBlock.id);
      fieldsToRemove.forEach(f => {
        let moduleInfo = d.getModuleInformation();
        let relName = moduleInfo?.fields[f.name]?.options || '';
        if (!relName) {
          let relEntry = d.relationships.find(r => r.related_datablock_id === f.value);
          relName = relEntry?.name || '';
        }
        if (f.required) {
          // Field is required: keep it but convert to fixed (block no longer exists)
          f.value = '';
          f.value_text = '';
          f.value_type = 'fixed';
        } else {
          d.deleteField(f.name);
        }
        if (relName) {
          let targetBlock = this.data_blocks.find(tb => tb.id === f.value);
          if (targetBlock) {
            targetBlock.removeRelationship(relName, d.id);
          }
        }
      });
    });

    // Remove relationships pointing to this DataBlock
    this.data_blocks.forEach(d => {
      d.relationships = d.relationships.filter(r => r.related_datablock_id !== dataBlock.id);
    });

    // Remove DataBlock
    this.data_blocks = this.data_blocks.filter(d => d.id != dataBlock.id);
  }

  deleteDataBlockField(dataBlock, field) {
    dataBlock.deleteField(field.name);

    if (field.type == 'relate' && field.value_type == 'dataBlock') {
      // Remove Relationship Action
      const relateAction = this.flows.flatMap(f => f.actions).find(a => {
        if (a.name == 'RelateRecordsAction') {
          return a.parameters.find(p => p.name == 'data_block_id' && p.value == dataBlock.id) &&
                 a.parameters.find(p => p.name == 'target_object' && p.value == field.value) &&
                 a.parameters.find(p => p.name == 'field_to_update' && p.value == `${dataBlock.name}.${field.name}`);
        }
        return false;
      });
      if (relateAction) {
        this.flows.forEach(flow => {
          flow.actions = flow.actions.filter(a => a.id != relateAction.id);
        });
      }

      // Remove the relationship from this block (N side)
      let moduleInfo = dataBlock.getModuleInformation();
      let relName = moduleInfo?.fields[field.name]?.options || '';
      if (!relName) {
        let relEntry = dataBlock.relationships.find(r => r.related_datablock_id === field.value);
        relName = relEntry?.name || '';
      }
      if (relName) {
        dataBlock.removeRelationship(relName, field.value);
        let targetBlock = this.data_blocks.find(d => d.id == field.value);
        if (targetBlock) {
          targetBlock.removeRelationship(relName, dataBlock.id);
        }
      }
    }

    // Invalidate paramter actions depending on this field
    let fieldRef = `${dataBlock.name}.${field.name}`;
    if (field.type_field === 'unlinked') { 
      fieldRef = `_detached.${fieldRef}`;
    }
    this.flows.forEach(flow => {
      flow.actions.forEach(action => {
        action.parameters.forEach(param => {
          if (param.value === fieldRef) {
            param.value = "";
            param.value_text = "";
          }
        });
      });
    });
  }

  /**
   * Returns a flat list of all fields in the form to be used in selectors (conditions, parameters, etc.)
   * @param {string} excludeField (Optional) The field name to exclude from the list (to avoid circular references)
   * @returns {Array} [{name: 'Block.Field', text: 'BlockName » Field label'}]
   */
  getAllFieldsInForm(excludeName = null) {
    let allFields = [];

    this.data_blocks.forEach(block => {
      block.fields.forEach(field => {
        if (field.type_field === 'fixed') return;
        let fullName = block.getFieldInputName(field);
        if (excludeName && fullName === excludeName) return;

        // Get display text: "Block Text » Field Label"
        let label = field.label || field.text_original;
        let displayText = `${block.text} » ${utils.fromFieldLabelText(label)}`;

        allFields.push({
          name: fullName,
          text: displayText
        });
      });
    });

    return allFields;
  }

  /**
   * Gets the field definition by its full HTML name
   * @param {string} fullName The full HTML name of the field (BlockName.FieldName)
   * @returns {stic_AwfField|null} The field definition or null if not found
   */
  getFieldDefinitionByHtmlName(fullName) {
    if (!fullName) return null;

    for (const block of this.data_blocks) {
      for (const field of block.fields) {
        if (block.getFieldInputName(field) === fullName) {
          return field;
        }
      }
    }
    return null;
  }

  /**
   * 
   * @param {string} datablockId 
   * @param {string} relationshipName 
   * @param {string} relatedDatablockId 
   * @param {string} newDataBlockText 
   * @returns 
   */
  addDataBlockRelationship(datablockId, relationshipName, relatedDatablockId, newDataBlockText) {

    // Find Relationship
    let rel = this.getAllDataBlockRelationships()[datablockId].find(r => r.name == relationshipName);
    if (!rel) return null;

    // Find Datablock
    let dataBlock = this.data_blocks.find(d => d.id == datablockId);
    if (!dataBlock) return null;

    // Find related Datablock
    let relDatablock = null;
    if (relatedDatablockId != -1) {
      relDatablock = this.data_blocks.find(d => d.id == relatedDatablockId);
    } else {
      relDatablock = this.addDataBlockModule(rel.module, true, newDataBlockText);
    }
    if (!relDatablock) return null;

    let relationshipType = utils.getModuleInformation(dataBlock.module).relationships[relationshipName]?.relationship_type || 'many-to-many';
    let moduleInfo = utils.getModuleInformation(dataBlock.module);
    let targetModuleInfo = utils.getModuleInformation(relDatablock.module);
    let hasRelateField = moduleInfo && Object.values(moduleInfo.fields).some(f => f.type === 'relate' && f.options === relationshipName);
    let targetHasRelateField = targetModuleInfo && Object.values(targetModuleInfo.fields).some(f => f.type === 'relate' && f.options === relationshipName);
    // Fallback: check relationship metadata for virtual 1-N relationships
    if (!hasRelateField && !targetHasRelateField) {
      let relData = (moduleInfo?.relationships?.[relationshipName]) || (targetModuleInfo?.relationships?.[relationshipName]);
      if (relData?.type === '1-N' || relData?.relationship_type === 'one-to-many') {
        if (relData.module_orig === dataBlock.module) hasRelateField = true;
        if (relData.module_orig === relDatablock.module) targetHasRelateField = true;
      }
    }
    let bothHaveRelate = hasRelateField && targetHasRelateField;

    // If the initiating block has the relate field and already has an initiator relationship
    // with this name, block it (a block can only be the N side once per relationship).
    // Entries with role 'target' are inverse (1 side) and don't block.
    if (hasRelateField && dataBlock.relationships.some(r => r.name === relationshipName && r.role !== 'target')) return dataBlock;

    // Determine the actual N-side (the block that has the relate field with FK).
    // initiator_id must point to the N-side regardless of which block initiated the UI action,
    // otherwise direction logic (requisites, arrows) fails.
    let nSideId;
    if (hasRelateField) {
      nSideId = dataBlock.id;
    } else if (targetHasRelateField) {
      nSideId = relDatablock.id;
    } else {
      nSideId = dataBlock.id;
    }

    // Store relationship on both blocks, tagging each with the initiator's id so downstream
    // direction logic (arrows, labels) can tell N side from 1 side.
    // When origin and destination are the same block, only store once to avoid duplicates.
    dataBlock.relationships.push({
      name: relationshipName,
      related_datablock_id: relDatablock.id,
      initiator_id: nSideId,
    });
    if (dataBlock.id !== relDatablock.id) {
      if (bothHaveRelate) {
        relDatablock.relationships.push({
          name: relationshipName,
          related_datablock_id: dataBlock.id,
          role: 'target',
          initiator_id: nSideId,
        });
      } else {
        relDatablock.relationships.push({
          name: relationshipName,
          related_datablock_id: dataBlock.id,
          initiator_id: nSideId,
        });
      }
    }

    // For 1-N relationships with relate fields: add server field on the block that owns
    // the relate field (the "N" side), pointing to the other block (the "1" side).
    // For self-referencing (both have relate), only the initiating block gets the server field.
    [{block: dataBlock, target: relDatablock}, {block: relDatablock, target: dataBlock}].forEach(({block, target}) => {
      if (bothHaveRelate && block.id !== datablockId) return;
      let blockModuleInfo = utils.getModuleInformation(block.module);
      if (!blockModuleInfo) return;
      let relateFields = Object.values(blockModuleInfo.fields).filter(
        f => f.type === 'relate' && f.options === relationshipName
      );
      relateFields.forEach(fieldInfo => {
        let fieldIndex = block.fields.findIndex(f => f.name === fieldInfo.name);
        let existingField;
        if (fieldIndex >= 0) {
          existingField = block.fields[fieldIndex];
        } else {
          existingField = block.addFieldFromModuleField(fieldInfo);
          fieldIndex = block.fields.findIndex(f => f.name === fieldInfo.name);
        }
        existingField.type_field = 'fixed';
        existingField.in_form = false;
        existingField.value_type = 'dataBlock';
        existingField.value = target.id;
        existingField.value_text = target.text;
        if (fieldIndex >= 0) {
          block.fields.splice(fieldIndex, 1, existingField);
        }
      });
    });

    return dataBlock;
  }

  /**
   * Deletes a relationship between two DataBlocks, removing any auto-created relate field
   * and the associated RelateRecordsAction.
   * @param {string} datablockId One of the DataBlock ids in the relationship
   * @param {string} relName The relationship name
   * @param {string} relatedDatablockId The other DataBlock id in the relationship
   */
  deleteRelationship(datablockId, relName, relatedDatablockId) {
    let dataBlock = this.data_blocks.find(d => d.id == datablockId);
    if (!dataBlock) return;
    let relDatablock = this.data_blocks.find(d => d.id == relatedDatablockId);

    // Find the auto-created relate field (if any) on either block
    let relateField = null;
    let relateFieldBlock = null;
    [dataBlock, relDatablock].forEach(block => {
      if (!block || relateField) return;
      let moduleInfo = block.getModuleInformation();
      let relateFieldNames = Object.values(moduleInfo?.fields || {}).filter(
        f => f.type === 'relate' && f.options === relName
      ).map(f => f.name);
      relateField = block.fields.find(f =>
        relateFieldNames.includes(f.name) && f.value_type == 'dataBlock' &&
        (f.value == datablockId || f.value == relatedDatablockId)
      );
      if (relateField) relateFieldBlock = block;
    });

    if (relateField && !relateField.required) {
      // Field is not required: delete field + relationship + action
      this.deleteDataBlockField(relateFieldBlock, relateField);
    } else {
      // No relate field (N-M), or field is required (keep field, convert to fixed, remove relationship + action)
      if (relateField) {
        relateField.value = '';
        relateField.value_text = '';
        relateField.value_type = 'fixed';
      }
      dataBlock.removeRelationship(relName, relatedDatablockId);
      if (relDatablock) {
        relDatablock.removeRelationship(relName, datablockId);
      }
      // Remove RelateRecordsAction for this relationship
      this.flows.forEach(flow => {
        flow.actions = flow.actions.filter(a => {
          if (a.name == 'RelateRecordsAction') {
            let p1 = a.parameters.find(p => p.name == 'data_block_id' && (p.value == datablockId || p.value == relatedDatablockId));
            let p2 = a.parameters.find(p => p.name == 'relationship_name' && p.value == relName);
            return !(p1 && p2);
          }
          return true;
        });
      });
    }
  }

  /**
   * Get all defined Relationships in all modules represented in data_blocks array
   * @returns {object} map with all DataBlock relationships, indexed by DataBlock id
   * DataBlockRelationship: { name, text, module_orig, field_orig, relationship, module_dest, datablock, module, textExtended, datablock_orig, datablock_dest }
   */
  getAllDataBlockRelationships() {
    let allRelationships = {};

    this.data_blocks.forEach(d => {
      if (!d.module) return;
      allRelationships[d.id] = [];
      let moduleInfo = utils.getModuleInformation(d.module);

      // Base list: ALL module relationships (used + unused), deduplicated by name
      let seenNames = new Set();
      Object.values(moduleInfo.relationships).forEach(r => {
        if (seenNames.has(r.name)) return;
        seenNames.add(r.name);
        let rel = {...r};
        rel.datablock = d.id;
        rel.module = rel.module_orig == d.module ? rel.module_dest : rel.module_orig;
        rel.textExtended = `${rel.text} (${STIC.enabledModules[rel.module]?.text || rel.module})`;
        rel.datablock_orig = "";
        rel.datablock_dest = "";
        allRelationships[d.id].push(rel);
      });

      // Sort alphabetically
      allRelationships[d.id].sort((a, b) => a.textExtended.localeCompare(b.textExtended));

      // Mark used relationships from block.relationships[]
      d.relationships.forEach(dbrel => {
        let rel = allRelationships[d.id].find(r => r.name === dbrel.name && !r.datablock_orig);
        if (rel) {
          // First usage: mark the base entry
          rel.datablock_orig = d.id;
          rel.datablock_dest = dbrel.related_datablock_id;
          if (dbrel.initiator_id) rel.initiator_id = dbrel.initiator_id;
        } else {
          // Subsequent usage (N-M reuse): clone a fresh entry
          let baseRel = allRelationships[d.id].find(r => r.name === dbrel.name);
          if (!baseRel) return;
          let newRel = {
            ...baseRel,
            datablock_orig: d.id,
            datablock_dest: dbrel.related_datablock_id,
          };
          if (dbrel.initiator_id) newRel.initiator_id = dbrel.initiator_id;
          allRelationships[d.id].push(newRel);
        }
      });
    });

    return allRelationships;
  }

  /**
   * Gets the module related with relationship in current DataBlock
   * @param {string} datablockId 
   * @param {string} relationshipName 
   * @returns {string} The module name
   */
  getRelationshipModule(datablockId, relationshipName) {
    return this.getAllDataBlockRelationships()[datablockId].find(r => r.name==relationshipName)?.module;
  }

  /**
   * Get all available DataBlocks that can be related with current DataBlock in the relationship
   * @param {string} datablockId 
   * @param {string} relationshipName 
   * @returns {array}
   * AvailableDataBlock: { id, text }
   */
  getAvailableDataBlocksForRelationship(datablockId, relationshipName) {
    let module = this.getRelationshipModule(datablockId, relationshipName);
    if (!module) return [];

    // Exclude blocks already related via this relationship
    let block = this.data_blocks.find(d => d.id == datablockId);
    let alreadyRelatedIds = new Set();
    if (block) {
      block.relationships
        .filter(r => r.name === relationshipName)
        .forEach(r => alreadyRelatedIds.add(r.related_datablock_id));
    }

    let dataBlocks = [];
    this.data_blocks
      .filter(d => d.module == module && !alreadyRelatedIds.has(d.id))
      .forEach(db => {
        dataBlocks.push({id: db.id, text: db.text});
      });
    dataBlocks.push({ id: -1, text: utils.translate('LBL_DATABLOCK_NEW') });

    return dataBlocks;
  }

  /**
   * Regenerates automatic actions (Save and Relate) based on current Data Blocks.
   * It should be called before entering action configuration (Step 3).
   */
  regenerateAutomaticActions() {
    const mainFlow = this.flows.find(f => f.id == '0');
    if (!mainFlow) return;

    // Clean: Remove automatic SaveRecord and RelateRecords actions
    // (they will be regenerated below). Other automatic actions (e.g., CheckSessionAction)
    // are managed separately and must be preserved.
    const regeneratedActionNames = ['SaveRecordAction', 'RelateRecordsAction'];
    mainFlow.actions = mainFlow.actions.filter(a => 
      !a.is_automatic || !regeneratedActionNames.includes(a.name)
    );
    
    // Preserve old save_action_ids so manual actions' requisite_actions stay valid
    const oldSaveActionIds = {};
    this.data_blocks.forEach(b => {
        if (b.save_action_id) oldSaveActionIds[b.id] = b.save_action_id;
    });

    // Reset saved action IDs on blocks before regenerating
    this.data_blocks.forEach(b => b.save_action_id = "");

    // Define the standard order for automatic actions
    // Using -1 ensures they will be inserted before default manual actions (0)
    const AUTO_ACTION_ORDER = -1;

    // Relationships handled by SaveRecordAction (FK pre-injected before save).
    // These are skipped when generating RelateRecordsAction later.
    const handledRelationshipNames = new Set();

    // Generate SAVE actions for each DataBlock.
    // Uses SaveRecordAction when the block has outgoing 1-N relationships,
    // so that FK values are injected before the first save.
    this.data_blocks.forEach(block => {
      if (!block.module) return;

      const allRels = this.getAllDataBlockRelationships();
      const blockRels = allRels[block.id] || [];
      const activeRels = blockRels.filter(r => r.datablock_orig && r.datablock_dest);
      const moduleInfo = utils.getModuleInformation(block.module);

      // Find outgoing 1-N relationships by the presence of a relate field with id_name,
      // which represents the FK column. Relate fields carry the resolved relationship name
      // in their 'options' property (populated by getModuleInformation() in Utils.php).
      const outgoing1n = activeRels.filter(r => {
        if (r.datablock_orig !== block.id) return false;
        if (!r.name) return false;
        // Only the initiator (N-side) should inject FKs
        if (r.initiator_id && r.initiator_id !== block.id) return false;
        // Self-referencing: cannot inject FK before save (target not yet saved)
        if (r.datablock_orig === r.datablock_dest) return false;
        const relateField = moduleInfo && Object.values(moduleInfo.fields).find(
          f => f.type === 'relate' && f.options === r.name
        );
        return relateField && relateField.id_name;
      }).map(r => {
        const relateField = Object.values(moduleInfo.fields).find(
          f => f.type === 'relate' && f.options === r.name
        );
        return { ...r, id_name: relateField.id_name };
      });

      const originalDef = utils.getDefinedAction('SaveRecordAction');
      if (originalDef) {
        const actionDef = { ...originalDef, isAutomatic: true, order: AUTO_ACTION_ORDER };

        // Base parameters
        const params = {
          'data_block_id': { value: block.id, valueText: block.text, selectedOption: '' }
        };
        // Inject relationship configurations ONLY if they exist
        if (outgoing1n.length > 0) {
          params['relation_configs'] = {
            value: JSON.stringify(outgoing1n.map(r => ({
              id_name: r.id_name,
              target_block_id: r.datablock_dest,
              relationship_name: r.name
            }))),
            valueText: outgoing1n.map(r => {
              const targetBlock = this.data_blocks.find(b => b.id === r.datablock_dest);
              const relText = r.text || r.name;
              return `${relText} (${r.name}): ${targetBlock ? targetBlock.text : r.datablock_dest}`;
            }).join('\n'),
            selectedOption: ''
          };
        }
        const newAction = this.addAction(actionDef, params, '0', oldSaveActionIds[block.id]);
        if (newAction) {
          block.save_action_id = newAction.id;
          newAction.text = `${utils.translate('LBL_SAVE_RECORD_ACTION_TITLE')}: ${block.text}`;
          outgoing1n.forEach(r => handledRelationshipNames.add(r.name));
        }
      }
    });

    // Add requisites ensuring the 1-side is saved before the N-side (FK injection target).
    // Only the N-side (initiator) adds a requisite on the 1-side (target).
    this.data_blocks.forEach(block => {
      // Add requisites for FK-injected relationships (1-N): the N-side (initiator)
      // must execute after the 1-side (target) save.
      if (block.save_action_id) {
        const allRels = this.getAllDataBlockRelationships();
        (allRels[block.id] || [])
          .filter(r => r.datablock_orig && r.datablock_dest && handledRelationshipNames.has(r.name) && r.initiator_id === block.id)
          .forEach(r => {
            const targetBlock = this.data_blocks.find(b => b.id === r.datablock_dest);
            if (targetBlock && targetBlock.save_action_id && targetBlock.save_action_id !== block.save_action_id) {
              const saveAction = mainFlow.actions.find(a => a.id === block.save_action_id);
              if (saveAction && !saveAction.requisite_actions.includes(targetBlock.save_action_id)) {
                saveAction.requisite_actions.push(targetBlock.save_action_id);
              }
            }
          });
      }

      // Generate RelateRecordsActions for relate fields with fixed values.
      const moduleInfo = block.getModuleInformation();
      if (moduleInfo) {
        block.fields.forEach(field => {
          if (field.type === 'relate' && field.value_type === 'fixed' && field.value) {
            let relationshipName = '';
            const moduleFieldInfo = moduleInfo.fields[field.name];
            if (moduleFieldInfo && moduleFieldInfo.type === 'relate' && moduleFieldInfo.options) {
              relationshipName = moduleFieldInfo.options;
            }
            if (relationshipName) {
              const originalDef = utils.getDefinedAction('RelateRecordsAction');
              if (originalDef) {
                const actionDef = {
                  ...originalDef,
                  isAutomatic: true,
                  order: AUTO_ACTION_ORDER
                };
                const params = {
                  'data_block_id': { value: block.id, valueText: block.text, selectedOption: '' },
                  'target_object': { value: field.value, valueText: field.value_text || field.value, selectedOption: 'value' },
                  'relationship_name': { value: relationshipName, valueText: relationshipName, selectedOption: '' }
                };
                const newAction = this.addAction(actionDef, params, '0');
                if (newAction) {
                  newAction.text = `${utils.translate('LBL_RELATE_RECORDS_ACTION_TITLE')}: ${block.text}.${field.text_original || field.name} = ${field.value_text || field.value}`;
                }
              }
            }
          }
        });
      }
    });

    // Generate RELATE actions for Block-to-Block relationships.
    // Skip those already handled by SaveRecordAction.
    const allRels = this.getAllDataBlockRelationships();
    Object.keys(allRels).forEach(blockId => {
      const blockRels = allRels[blockId];
      const activeRels = blockRels.filter(r => r.datablock_orig && r.datablock_dest);
      
      // Skip relationships already handled by SaveRecordAction.
      // Both directions are skipped since the initiator's FK injection is sufficient.
      activeRels.forEach(rel => {
        if (rel.datablock_orig === blockId) {
          if (handledRelationshipNames.has(rel.name)) return;
          // Skip non-initiator: N-M is bidirectional (one action suffices),
          // 1-N fallback FK injection is done on the initiator side.
          if (rel.initiator_id && rel.initiator_id !== blockId) return;
          
          const originalDef = utils.getDefinedAction('RelateRecordsAction');
          if (originalDef) {
            const blockOrig = this.data_blocks.find(b => b.id == rel.datablock_orig);
            const blockDest = this.data_blocks.find(b => b.id == rel.datablock_dest);
            
            if (blockOrig && blockDest) {
              const actionDef = { 
                ...originalDef, 
                isAutomatic: true, 
                order: AUTO_ACTION_ORDER 
              };
              
              const params = {
                'data_block_id': { value: blockOrig.id, valueText: blockOrig.text, selectedOption: '' },
                'target_object': { value: blockDest.id, valueText: blockDest.text, selectedOption: 'datablock' },
                'relationship_name': { value: rel.link_name || rel.name, valueText: rel.text, selectedOption: '' }
              };
              
              // Determine if this is 1-N (has a relate field) or N-M (no relate field)
              const moduleOrigInfo = utils.getModuleInformation(blockOrig.module);
              let isOneToMany = false;
              if (moduleOrigInfo) {
                const relateField = Object.values(moduleOrigInfo.fields).find(
                  f => f.type === 'relate' && f.options === rel.name
                );
                if (relateField && relateField.id_name) {
                  isOneToMany = true;
                  if (!rel.initiator_id || rel.initiator_id === blockOrig.id) {
                    params['relation_id_name'] = { value: relateField.id_name, valueText: relateField.id_name, selectedOption: '' };
                  }
                }
              }
              
              const arrow = isOneToMany ? '\u27f6' : '\u27f7';
              const newAction = this.addAction(actionDef, params, '0');
              if (newAction) {
                newAction.text = `${utils.translate('LBL_RELATE_RECORDS_ACTION_TITLE')}: ${blockOrig.text} ${arrow} ${blockDest.text}`;
              }
            }
          }
        }
      });
    });

    // Sort actions topologically based on requisite_actions dependencies
    this.sortFlowTopologically(mainFlow);
  }

  /**
   * Sorts the actions of a flow topologically using Kahn's algorithm based on requisite_actions.
   * Detects cycles and marks the closing edge as deferred (removes it from requisites) so the
   * topological sort can complete. Deferred edges are returned so the caller knows which
   * RelateRecordsAction dependencies were broken.
   * @param {stic_AwfFlow} flow The flow to sort
   * @returns {Array} List of deferred edges: { from: actionId, to: actionId }
   */
  sortFlowTopologically(flow) {
    if (!flow || !flow.actions || flow.actions.length === 0) return [];

    // Build action map for name resolution
    let actionMap = new Map();
    flow.actions.forEach(a => actionMap.set(a.id, a));

    // Build adjacency list and in-degree count from requisite_actions
    // Edge: requisite -> action (action depends on requisite)
    let inDegree = new Map();
    let dependents = new Map(); // actionId -> [actionIds that depend on it]
    flow.actions.forEach(a => {
      inDegree.set(a.id, 0);
      dependents.set(a.id, []);
    });

    flow.actions.forEach(a => {
      (a.requisite_actions || []).forEach(reqId => {
        if (actionMap.has(reqId)) {
          dependents.get(reqId).push(a.id);
          inDegree.set(a.id, inDegree.get(a.id) + 1);
        }
      });
    });

    // Kahn's algorithm (BFS)
    let sorted = [];
    let queue = [];
    inDegree.forEach((deg, id) => { if (deg === 0) queue.push(id); });

    while (queue.length > 0) {
      let currentId = queue.shift();
      sorted.push(currentId);
      dependents.get(currentId).forEach(depId => {
        inDegree.set(depId, inDegree.get(depId) - 1);
        if (inDegree.get(depId) === 0) queue.push(depId);
      });
    }

    // Cycle detection: if sorted < total, there is a cycle
    let deferred = [];
    if (sorted.length < flow.actions.length) {
      // Find actions in the cycle (those not yet sorted)
      let remaining = new Set(flow.actions.filter(a => !sorted.includes(a.id)).map(a => a.id));

      // For each remaining action, find a requisite that is also remaining (the cycle edge)
      remaining.forEach(actionId => {
        let action = actionMap.get(actionId);
        let cycleReq = (action.requisite_actions || []).find(r => remaining.has(r));
        if (cycleReq) {
          deferred.push({ from: cycleReq, to: actionId });
          // Remove the deferred edge so the sort can proceed
          action.requisite_actions = action.requisite_actions.filter(r => r !== cycleReq);
          inDegree.set(actionId, inDegree.get(actionId) - 1);
          if (inDegree.get(actionId) === 0) queue.push(actionId);
        }
      });

      // Continue BFS after breaking cycles
      while (queue.length > 0) {
        let currentId = queue.shift();
        sorted.push(currentId);
        dependents.get(currentId).forEach(depId => {
          inDegree.set(depId, inDegree.get(depId) - 1);
          if (inDegree.get(depId) === 0) queue.push(depId);
        });
      }

      // If there are still unsorted actions, force them at the end (resilience against multi-edge cycles)
      let forced = [];
      flow.actions.forEach(a => {
        if (!sorted.includes(a.id)) {
          sorted.push(a.id);
          forced.push(a.id);
        }
      });
    }

    // Reorder flow.actions according to topological order
    let sortedMap = new Map();
    sorted.forEach((id, index) => sortedMap.set(id, index));
    flow.actions.sort((a, b) => (sortedMap.get(a.id) ?? 0) - (sortedMap.get(b.id) ?? 0));

    // Group actions: pre-auto (order < -1) → auto (is_automatic) → manual → terminal.
    // Preserves topological order within each group but prevents actions
    // from being interleaved across groups.
    const preAutoActions = flow.actions.filter(a => !a.is_automatic && !a.is_terminal && a.order < -1);
    const autoActions = flow.actions.filter(a => a.is_automatic);
    const manualActions = flow.actions.filter(a => !a.is_automatic && !a.is_terminal && a.order >= -1);
    const terminalActions = flow.actions.filter(a => a.is_terminal);
    flow.actions = [...preAutoActions, ...autoActions, ...manualActions, ...terminalActions];

    // Reassign order property.
    // Pre-auto actions keep their original negative order (fixed, before saves).
    // Automatic actions keep order -1 (before default manual actions).
    // Manual actions stay at order 0 so is_fixed_order remains false (reorderable).
    // Terminal actions keep their order (999).
    flow.actions.forEach((a) => {
      if (a.is_automatic) a.order = -1;
      else if (a.is_terminal) { /* keep 999 */ }
      else if (a.order < -1) { /* keep pre-auto negative order */ }
      else a.order = 0;
    });

    return deferred;
  }

  /**
   * Add new action to flow
   *
   * @param {object} actionDef The Action definition (from ActionDefinitionDTO)
   * @param {object} params A map of parameters, ex: { 'param_name': { value: 'value', selectedOption: 'opt' } }
   * @param {string} flowId Id of the flow where action will be added (ex: '0' for main flow)
   * @returns {stic_AwfAction} The new action
   */
  addAction(actionDef, params = {}, flowId = '0', existingId = null) {
    const flow = this.flows.find(f => f.id == flowId);
    if (!flow) {
      console.error(`Flow with ID ${flowId} not found.`);
      return null;
    }

    // If it is a terminal action, we assign order to 999
    const defaultOrder = actionDef.isTerminal ? 999 : (actionDef.order ?? 0);

    const newAction = new stic_AwfAction({
      ...(existingId ? { id: existingId } : {}),
      name: actionDef.name,
      title: actionDef.title, 
      text: actionDef.title,
      description: actionDef.description,
      category: actionDef.category,
      is_user_selectable: actionDef.isUserSelectable,
      is_automatic: actionDef.isAutomatic,
      is_terminal: actionDef.isTerminal,
      continue_on_error: actionDef.defaultContinueOnError || false,
      order: defaultOrder,
      flow_success_text: actionDef.flowSuccessLabel || utils.translate("LBL_FLOW_DEFERRED_MAIN"),
      flow_error_text: actionDef.flowErrorLabel || utils.translate("LBL_FLOW_ONERROR"),
    });

    const requisiteActions = new Set(); 
    (actionDef.parameters || []).forEach(paramDef => {      
      const paramConfig = params[paramDef.name];       
      const paramValue = paramConfig?.value ?? paramDef.defaultValue;
      const newParam = new stic_AwfActionParameter({
        name: paramDef.name,
        text: paramDef.text,
        type: paramDef.type,
        required: paramDef.required,
        value: paramValue,
        value_text: paramConfig?.valueText ?? paramValue,
        selectedOption: paramConfig?.selectedOption ?? '',
      });

      newAction.parameters.push(newParam);

      // Requisites: If param is Datablock or resolvedType is DataBlock
      const paramIsDataBlock = (paramDef.type === 'dataBlock') || 
                               (paramDef.selectorOptions || []).find(o => o.name == newParam.selectedOption)?.resolvedType === 'dataBlock';

      if (paramIsDataBlock && newParam.value) {
        const requiredBlock = this.data_blocks.find(b => b.id == newParam.value);

        if (requiredBlock && requiredBlock.save_action_id) {
          requisiteActions.add(requiredBlock.save_action_id);
        }
      }
    });

    // Assign requisites
    newAction.requisite_actions = Array.from(requisiteActions);

    // Add Action to flow
    this.upsertAction(newAction, actionDef.type, flow, null);

    return newAction;
  }

  /**
   * Central logic for manipulating actions and flows: Insert or Update an action to a Flow
   * @param {stic_AwfAction} action The Action 
   * @param {string} actionType The Action definition type (from ActionDefinitionDTO)
   * @param {stic_AwfFlow} flow the flow where action will be added or updated
   * @param {string} originalId Id of the action (null if is a new action)
   */
  upsertAction(action, actionType, flow, originalId = null) {
    // Deferred Actions management
    if (actionType === 'Deferred') {
      const okFlowId = `${action.id}_ok`;
      const errorFlowId = `${action.id}_err`;
      action.flow_success_id = okFlowId;
      action.flow_error_id = errorFlowId;

      // We create sub-flows if they don't exist
      let okFlow = this.flows.find(f => f.id === okFlowId);
      if (!okFlow) {
        okFlow = new stic_AwfFlow({ id: okFlowId, name: `${action.name}_Ok` });
        this.flows.push(okFlow);

        // Look for action definition and move terminal actions if new aresumptionContext is original user browser
        const def = utils.getDefinedActions().find(d => d.name === action.name);
        if (action.is_terminal && def && def.resumptionContext === 'original_user') {
          // Look for any action that is terminal and NOT the one we are editing to move it to the new flow
          const terminalIndex = flow.actions.findIndex(a => a.order === 999 && a.id !== (originalId || action.id));
          if (terminalIndex !== -1) {
            const terminalAction = flow.actions.splice(terminalIndex, 1)[0];
            okFlow.actions.push(terminalAction);
          }
        }
      }

      let errFlow = this.flows.find(f => f.id === errorFlowId);
      if (!errFlow) {
        errFlow = new stic_AwfFlow({ id: errorFlowId, name: `${action.name}_Error` });
        this.flows.push(errFlow);
      }

      okFlow.label = action.text + ": " + action.flow_success_text;
      errFlow.label = action.text + ": " + action.flow_error_text;
    }

    // Insert or update to the flow
    if (!originalId) {
      // New action: Order-based insertion
      let insertIndex = flow.actions.length;
      for (let i = 0; i < flow.actions.length; i++) {
        if ((flow.actions[i].order ?? 0) > (action.order ?? 0)) {
          insertIndex = i;
          break;
        }
      }
      flow.actions.splice(insertIndex, 0, action);
    } else {
      // Edit: We update the array reference
      const index = flow.actions.findIndex(a => a.id == originalId);
      if (index !== -1) {
        flow.actions[index] = action;
      }
    }
  }

  removeAction(flowId, actionId) {
    const flow = this.flows.find(f => f.id == flowId);
    if (!flow) {
      console.error(`Flow with ID ${flowId} not found.`);
      return false;
    }
    const action = flow.actions.find(a => a.id == actionId);
    if (!action) {
      console.error(`Action with ID ${actionId} not found.`);
      return false;
    }
    if (action.flow_success_id) {
      const okFlow = this.flows.find(f => f.id === action.flow_success_id);
      if (okFlow) {
        // Move the actions to parent flow
        okFlow.actions.forEach(a => flow.actions.push(a));
      }
      // Remove success and error flows
      const okFlowIndex = this.flows.findIndex(f => f.id === action.flow_success_id);
      if (okFlowIndex !== -1) {
        this.flows.splice(okFlowIndex, 1);
      }
      const errFlowIndex = this.flows.findIndex(f => f.id === action.flow_error_id);
      if (errFlowIndex !== -1) {
        this.flows.splice(errFlowIndex, 1);
      }
    }

    flow.actions = flow.actions.filter(a => a.id != actionId);
    return true;
  }
}
