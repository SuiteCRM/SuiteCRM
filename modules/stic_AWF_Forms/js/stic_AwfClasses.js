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
    });

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
    // TODO: Add Detached DataBlock
    // // Check exists Detached DataBlock
    // const detachedDataBlockExists = this.data_blocks.some((b) => b.name === "_Detached");
    // if (!detachedDataBlockExists) {
    //   this.data_blocks.push(new stic_AwfDataBlock({
    //     name: "_Detached",
    //     text: utils.translate("LBL_DATABLOCK_DETACHED"),
    //     module: "",
    //     editable_text: false,
    //     required: true,
    //   }));
    // }
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
  _ensureDefaultLayout() {}

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
   * Add new action to flow
   *
   * @param {object} actionDef The Action definition (from ActionDefinitionDTO)
   * @param {object} params A map of parameters, ex: { 'param_name': { value: 'value', selectedOption: 'opt' } }
   * @param {string} flowId Id of the flow where action will be added (ex: '0' for main flow)
   * @returns {stic_AwfAction} The new action
   */
  addAction(actionDef, params = {}, flowId = '0') {
    const flow = this.flows.find(f => f.id == flowId);
    if (!flow) {
      console.error(`Flow with ID ${flowId} not found.`);
      return null;
    }

    // If it is a terminal action, we assign order to 999
    const defaultOrder = actionDef.isTerminal ? 999 : (actionDef.order ?? 0);

    const newAction = new stic_AwfAction({
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

    // Add Action to flow: Insertion based on order
    let insertIndex = flow.actions.length;
    for (let i = 0; i < flow.actions.length; i++) {
      if ((flow.actions[i].order ?? 0) > newAction.order) {
        insertIndex = i;
        break;
      }
    }
    flow.actions.splice(insertIndex, 0, newAction);

    return newAction;
  }

  /**
   * Deletes a DataBlock, removing all field references to the DataBlock
   * @param {stic_AwfDataBlock} dataBlock 
   */
  deleteDataBlock(dataBlock) {
    // Reset all fields pointing to this DataBlock
    this.data_blocks.forEach(d => {
      d.fields.filter(f => f.value_type == 'dataBlock' && f.value == dataBlock.id).forEach(f => {
        f.value = '';
        f.value_text = '';
      });
    });

    // Remove DataBlock
    this.data_blocks = this.data_blocks.filter(d => d.id != dataBlock.id);
  }

  deleteDataBlockField(dataBlock, field) {
    dataBlock.deleteField(field.name);

    if (field.type == 'relate' && field.value_type == 'dataBlock') {
      // Remove Relationship Action
      // TODO: Review Remove Relationship Action: Parameters: data_block_id, target_object, relationship_name
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
    // DataBlockRelationship: { name, text, module_orig, field_orig, relationship, module_dest, datablock, module, textExtended, datablock_orig, datablock_dest }

    // Find Relationship
    let rel = this.getAllDataBlockRelationships()[datablockId].find(r => r.name == relationshipName);
    if (!rel) {
      return null;
    }

    // Find Datablock
    let dataBlock = this.data_blocks.find(d => d.id == datablockId);
    if (!dataBlock) {
      return null;
    }

    // Find related Datablock
    let relDatablock = null;
    if (relatedDatablockId != -1) {
      // Use existant related Datablock
      relDatablock = this.data_blocks.find(d => d.id == relatedDatablockId);
    } else {
      // Create new related Datablock
      relDatablock = this.addDataBlockModule(rel.module, true, newDataBlockText);
    }
    if (!relDatablock) {
      return null;
    }

    // Set field value in origin
    let dataBlock_orig = dataBlock;
    let dataBlock_dest = relDatablock;
    if (dataBlock_orig.module != rel.module_orig) {
      dataBlock_orig = relDatablock;
      dataBlock_dest = dataBlock;
    }
    let module_orig = utils.getModuleInformation(rel.module_orig);

    /**
     * Field: { 
     *    name, label, required, required_in_form, type, in_form, 
     *    type_in_form, value_type, value_options: [{value, text}], value, value_text 
     *  }
     */
    let dataField_orig = dataBlock_orig.addFieldFromModuleField(module_orig.fields[rel.field_orig]);
    dataField_orig.type_field = 'fixed';
    dataField_orig.in_form = false;
    dataField_orig.value_type = "dataBlock";
    dataField_orig.value = dataBlock_dest.id;

    return dataBlock;
  }


  /**
   * Get all defined Relationships in all modules represented in data_blocks array
   * @returns {object} map with all DataBlock relationships, indexed by DataBlock id
   * DataBlockRelationship: { name, text, module_orig, field_orig, relationship, module_dest, datablock, module, textExtended, datablock_orig, datablock_dest }
   */
  getAllDataBlockRelationships() {
    // Relationship: {name, text, module_orig, field_orig, relationship, module_dest}
    // DataBlockRelationship: {name, text, module_orig, field_orig, relationship, module_dest, datablock, module, textExtended, datablock_orig, datablock_dest}
    let allRelationships = {};
    let relsToReview = [];

    this.data_blocks.forEach(d => {
      if (d.module) {
        allRelationships[d.id] = [];
        Object.values(utils.getModuleInformation(d.module).relationships).forEach(r => {
          let rel = {...r}; // Copy relationship object

          // Fill all available relationships for every DataBlock
          rel.datablock = d.id;
          rel.module = rel.module_orig == d.module ? rel.module_dest : rel.module_orig;
          rel.textExtended = `${rel.text} (${STIC.enabledModules[rel.module].text})`;
          rel.datablock_orig = "";
          rel.datablock_dest = "";

          allRelationships[d.id].push(rel);
        });

        // Sort relationships
        allRelationships[d.id].sort((a, b) => {
          return a.textExtended.localeCompare(b.textExtended);
        });

        // Find and fill defined relationships in datablock fields
        d.fields.filter(f => f.value_type == "dataBlock").forEach(f => {
          let rel = allRelationships[d.id].find(r => r.module_orig == d.module && r.field_orig == f.name);
          if (rel) {
            // Fill Orig -> Dest info
            rel.datablock_orig = d.id;
            rel.datablock_dest = f.value;

            // Mark to review to fill Dest <- Orig info
            relsToReview.push(rel);
          }
        });
      }
    });

    // Review and fill Dest <- Orig info
    relsToReview.forEach(r => {
      let rel = allRelationships[r.datablock_dest].find(v => v.name == r.name);
      if (rel) {
        // Fill Dest <- Orig info
        rel.datablock_orig = r.datablock_orig;
        rel.datablock_dest = r.datablock_dest;
      }
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
    if (!module) {
      return [];
    }

    let dataBlocks = [];
    this.data_blocks.filter(d => d.module == module).forEach(db => {
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

    // Clean: Remove existing automatic actions from the main flow
    mainFlow.actions = mainFlow.actions.filter(a => !a.is_automatic);
    
    // Reset saved action IDs on blocks before regenerating
    this.data_blocks.forEach(b => b.save_action_id = "");

    // Define the standard order for automatic actions
    // Using -1 ensures they will be inserted before default manual actions (0)
    const AUTO_ACTION_ORDER = -1;

    // Generate SAVE actions for each DataBlock
    this.data_blocks.forEach(block => {
      if (!block.module) return;
      
      const originalDef = utils.getDefinedActions().find(a => a.name == 'SaveRecordAction');
      if (originalDef) {
        // Prepare definition override
        const actionDef = { 
          ...originalDef, 
          isAutomatic: true, 
          order: AUTO_ACTION_ORDER 
        };

        const params = {
          'data_block_id': { value: block.id, valueText: block.text, selectedOption: '' }
        };
        
        const newAction = this.addAction(actionDef, params, '0');
        if (newAction) {
            // Store the ID so subsequent Relate actions can depend on it
            block.save_action_id = newAction.id;
            
            // Override text for clarity
            newAction.text = `${utils.translate('LBL_SAVE_RECORD_ACTION_TITLE')}: ${block.text}`;
        }
      }
    });

    // Generate RELATE actions for FiXED fields
    this.data_blocks.forEach(block => {
      const moduleInfo = block.getModuleInformation(); 
        
      block.fields.forEach(field => {
        if (field.type === 'relate' && field.value_type === 'fixed' && field.value) {
          let relationshipName = '';
          const moduleFieldInfo = moduleInfo.fields[field.name];
          
          // Use 'options' property which contains the link name
          if (moduleFieldInfo && moduleFieldInfo.type === 'relate' && moduleFieldInfo.options) {
            relationshipName = moduleFieldInfo.options;
          }
          
          if (relationshipName) {
            const originalDef = utils.getDefinedActions().find(a => a.name == 'RelateRecordsAction');
            if (originalDef) {
              const actionDef = { 
                ...originalDef, 
                isAutomatic: true, 
                order: AUTO_ACTION_ORDER 
              };
              
              const params = {
                'data_block_id': { value: block.id, valueText: block.text, selectedOption: '' },
                'target_object': { value: field.value, valueText: field.value_text || field.value, selectedOption: 'value' }, // target_object is a fixed ID value
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
    });

    // Generate RELATE actions for Block-to-Block relationships
    const allRels = this.getAllDataBlockRelationships();
    Object.keys(allRels).forEach(blockId => {
      const blockRels = allRels[blockId];
      const activeRels = blockRels.filter(r => r.datablock_orig && r.datablock_dest);
      
      activeRels.forEach(rel => {
        if (rel.datablock_orig === blockId) {
          const originalDef = utils.getDefinedActions().find(a => a.name == 'RelateRecordsAction');
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
                'relationship_name': { value: rel.name, valueText: rel.text, selectedOption: '' }
              };
              
              const newAction = this.addAction(actionDef, params, '0');
              if (newAction) {
                newAction.text = `${utils.translate('LBL_RELATE_RECORDS_ACTION_TITLE')}: ${blockOrig.text} ⟶ ${blockDest.text}`;
              }
            }
          }
        }
      });
    });
  }

}
