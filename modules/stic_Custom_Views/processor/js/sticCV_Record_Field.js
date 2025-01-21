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

var sticCV_Record_Field = class sticCV_Record_Field extends sticCV_Record_Container {
  constructor(customView, fieldName) {
    super(customView, fieldName);

    var $fieldElement = this.customView.$elementView.find('*[data-field="' + this.name + '"]');

    this.container = new sticCV_Record_Field_Container(this, $fieldElement);
    this.header = new sticCV_Record_Field_Header(this.customView, $fieldElement);
    this.content = new sticCV_Record_Field_Content(this, $fieldElement, fieldName);

    this.validateName = this.name;
    if (this.content.type == "multienum") {
      this.validateName += "[]";
    }
  }
  readonly(readonly = true) {
    return this.applyAction({ action: "readonly", value: readonly });
  }
  required(required = true) {
    return this.applyAction({ action: "required", value: required });
  }
  is_required() {
    return sticCVUtils.getRequiredStatus(this);
  }
  inline(inline = true) {
    return this.applyAction({ action: "inline", value: inline });
  }

  fixed_value(fixed_value) {
    return this.applyAction({ action: "fixed_value", value: fixed_value });
  }
  value(newValue) {
    return this.fixed_value(newValue);
  }

  applyAction(action) {
    switch (action.action) {
      case "visible":
      case "visible_auto":
        if (action.element_section != "header" && action.element_section != "field_label") {
          super.applyAction(action);
          sticCVUtils.check_required_visible(this);
          return this;
        }
        break;
      case "readonly":
      case "inline":
      case "fixed_value":
        return this.content ? this.content.applyAction(action) : null;
      case "required":
        if (this.customView.view == "editview" || this.customView.view == "quickcreate") {
          sticCVUtils.required(this, action.value);
          return this;
        } else {
          return false;
        }
    }
    return super.applyAction(action);
  }

  checkCondition(condition) {
    return this.content.checkCondition(condition);
  }

  onChange(callback) {
    this.content.onChange(callback);
  }
  change() {
    this.content.change();
  }
};
