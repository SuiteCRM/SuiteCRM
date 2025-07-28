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
 * This file contains logic and functions needed to manage custom views behaviour
 *
 */

var sticCV_View_Record_Base = class sticCV_View_Record_Base {
  constructor(view) {
    this.view = view;

    this.undoFunctions = []; // List of functions to undo all actions
    this.customizations = []; // List of {conditions, actions, lastResult}

    this.$elementView = null; // jQuery element with all View

    this._fields = [];
    this._panels = [];
    this._tabs = [];
  }

  field(fieldName, forceReloadField = false) {
    if (forceReloadField || fieldName in this._fields === false) {
      this._fields[fieldName] = new sticCV_Record_Field(this, fieldName);
    }
    return this._fields[fieldName];
  }

  panel(panelName) {
    if (panelName in this._panels === false) {
      this._panels[panelName] = new sticCV_Record_Panel(this, panelName);
    }
    return this._panels[panelName];
  }

  tab(tabName) {
    if (tabName in this._tabs === false) {
      this._tabs[tabName] = new sticCV_Record_Tab(this, tabName);
    }
    return this._tabs[tabName];
  }

  /**
     * Process the View customization
     * @param {*} jsonRules : The rules to apply in a string with json structure. The rules will be applied in order
     * json format for rules: list of customizations, each with conditions and actions. 
     *  [
     *   {
     *    conditions: [], 
     *    actions: [{type: tab_modification, element: 4, action: visible, value: 0, element_section: tab}],
     *   },
     *   {
     *    conditions: [{field: stic_referral_agent_c, operator: Equal_To, value: social_services}],
     *    actions: [{type: tab_modification, element: 4, action: visible, value: 1, element_section: tab}],
     *   },
     *  ]
     */
  processSticCustomView(jsonRules) {
    console.log("sticCustomView - Processing rules: " + jsonRules);
    this.customizations = [];
    var customizationsObject = JSON.parse(jsonRules);
    if (Array.isArray(customizationsObject) && customizationsObject.length) {
      var self = this;
      customizationsObject.forEach(customization => {
        self.addCustomization(customization.conditions, customization.actions);
      });
    }
  }

  /**
     * Adds a customization: a group of Conditions to apply a list of actions
     */
  addCustomization(conditions, actions) {
    // Set customization in list
    this.customizations.push({ conditions: conditions, actions: actions, lastResult: false });
    var index = this.customizations.length - 1;

    // Bind every change involved in condition set
    if (Array.isArray(conditions) && conditions.length) {
      var self = this;
      conditions.forEach(condition => {
        self.field(condition.field).onChange(function() {
          self.processCustomization(index, false, true);
        });
      });
    }
    // Process Customization with current values
    this.processCustomization(index);
  }

  /**
     * Applies an action defined in an object
     * Example:
     * {
     *  type: tab_modification,
     *  element: 4,
     *  action: visible,
     *  value: 0,
     *  element_section: tab,
     * },
     */
  applyAction(action) {
    switch (action.type) {
      case "field_modification":
        return this.field(action.element).exists() ? this.field(action.element).applyAction(action) : false;
      case "panel_modification":
        return this.panel(action.element).exists() ? this.panel(action.element).applyAction(action) : false;
      case "tab_modification":
        return this.tab(action.element).exists() ? this.tab(action.element).applyAction(action) : false;
    }
  }

  /**
     * Adds a function to undo an applied action
     * Reverse: says that the function must be applied at the end (to preserve last value)
     */
  addUndoFunction(func, reverse = false) {
    if (reverse) {
      this.undoFunctions.push(func);
    } else {
      this.undoFunctions.unshift(func);
    }
  }

  /**
     * Undo all registered changes and clear undo list
     */
  undoChanges() {
    console.log("sticCustomView - Undoing customizations");
    var undoCopy = [];
    for (let i = 0; i < this.undoFunctions.length; i++) {
      undoCopy[i] = this.undoFunctions[i];
    }
    undoCopy.forEach(func => {
      func();
    });
    this.undoFunctions = [];
  }

  clearUndoList() {
    this.undoFunctions = [];
  }

  /**
     * Check a condition defined in an object
     * Example:
     * {
     *  field: stic_referral_agent_c,
     *  operator: Equal_To
     *  value: social_services
     * }
     */
  checkCondition(condition, forceReloadField = false) {
    return this.field(condition.field, forceReloadField).checkCondition(condition);
  }

  /**
     * Undo all changes and process all customizations
     */
  undoChangesAndProcessCustomizations() {
    this.undoChanges();
    for (let i = 0; i < this.customizations.length; i++) {
      this.processCustomization(i, true);
    }
  }

  /**
     * Process a Customization: Checks all conditions in order to apply all actions
     */
  processCustomization(index, resetLastResult = false, forceReloadField = false) {
    var customization = this.customizations[index];
    var value = true;
    if (Array.isArray(customization.conditions) && customization.conditions.length) {
      var self = this;
      customization.conditions.forEach(
        condition => (value = value ? self.checkCondition(condition, forceReloadField) : value)
      );
    }
    if (resetLastResult) {
      customization.lastResult = false;
    }

    if (value) {
      if (!customization.lastResult) {
        console.log("sticCustomView - Conditions OK: " + JSON.stringify(customization.conditions));
        console.log("sticCustomView - Applying actions: " + JSON.stringify(customization.actions));
        if (Array.isArray(customization.actions) && customization.actions.length) {
          var self = this;
          customization.actions.forEach(action => self.applyAction(action));
        }
      }
    } else {
      if (customization.lastResult) {
        console.log("sticCustomView - Conditions KO: " + JSON.stringify(customization.conditions));
        // Last evaluation pass all conditions: undo actions
        this.undoChangesAndProcessCustomizations();
      }
    }
    customization.lastResult = value;
  }
};
