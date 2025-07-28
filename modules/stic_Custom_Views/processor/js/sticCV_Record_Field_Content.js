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
var sticCV_Record_Field_Content = class sticCV_Record_Field_Content extends sticCV_Element_Label {
  constructor(field, $fieldElement, fieldName) {
    var $contentElement = $fieldElement.children('[field="' + fieldName + '"]');
    if ($contentElement.length == 0) {
      $contentElement = $fieldElement.children(".stic-FieldContent").children('[field="' + fieldName + '"]');
    }
    super(field.customView, $contentElement);

    this.$element.css("height", "auto");

    this.field = field;
    this.fieldName = fieldName;
    this.type = this.$element.attr("type") ? this.$element.attr("type") : "";
    this.type = this.type.toLowerCase();

    switch (this.type) {
      case "bool":
        this.$editor = this.$element.find("[type='checkbox']:input");
        break;
      case "enum":
      case "dynamicenum":
      case "multienum":
      case "currency_id":
        this.$editor = this.$element.find("select");
        break;
      case "datetimecombo":
        this.$editor = this.$element.find("input,select");
        break;
      default:
        this.$editor = this.$element.find("input,textarea,select");
        break;
    }
    if (this.customView.view == "detailview") {
      if (this.$editor.length === 0) {
        this.$editor = this.$element.find("span");
      } else {
        this.$editor.add(this.$element.find("span"));
      }
    }

    this.$buttons = this.$element.find("button");
    this.$items = this.$element.find(".items,table,option,label");
    this.$fieldText = this.$element.find(".sugar_field");

    this.$readonlyLabel = this.$element.parent().find(".stic-ReadonlyInput");
    if (this.exists() && (this.field.customView.view == "editview" || this.field.customView.view == "quickcreate")) {
      // Create $readonlyLabel
      var classes = this.$element.attr("class").replace(/\bhidden\b/g, "").replace(/\s+/g, " ").trim();
      if (this.$readonlyLabel.length == 0 && this.$element.length > 0) {
        this.$element
          .parent()
          .append(
            '<div class="' +
              classes +
              ' stic-ReadonlyInput hidden" ' +
              'style="min-height:30px; display: inline-flex; align-items:center; padding-left:5px; border-radius:0.25em; width:90%">' +
              "</div>"
          );
        this.$readonlyLabel = this.$element.parent().find(".stic-ReadonlyInput");
        sticCVUtils.fillReadonlyText(this);

        // Update label when value is changed
        var self = this;
        this.onChange(function() {
          sticCVUtils.fillReadonlyText(self);
        });
      }

      // Move $element and $readonlyLabel inside new $element div
      this.$elementEditor = this.$element;
      this.$element = $fieldElement.find(".stic-FieldContent");
      if (this.$element.length == 0 && this.$elementEditor.length > 0) {
        this.$element = $('<div class="' + classes + ' stic-FieldContent" ' + "></div>");
        this.$elementEditor.after(this.$element);
        this.$element.append(this.$readonlyLabel);
        this.$element.append(this.$elementEditor);

        // Remove "col-" classes
        this.$elementEditor.removeClass(function(index, className) {
          return (className.match(/\bcol-\S+/g) || []).join(" ");
        });
        this.$readonlyLabel.removeClass(function(index, className) {
          return (className.match(/\bcol-\S+/g) || []).join(" ");
        });
      }
    }
  }

  readonly(readonly = true) {
    return this.applyAction({ action: "readonly", value: readonly });
  }

  inline_edit(inline_edit = true) {
    return this.applyAction({ action: "inline", value: inline_edit });
  }

  value(newValue, value_list) {
    return sticCVUtils.value(this, newValue, value_list);
  }
  _getValue(value_list) {
    return this.value(undefined, value_list);
  }

  text(newText) {
    if (this.customView.view == "editview" || this.customView.view == "quickcreate") {
      switch (this.type) {
        case "enum":
        case "dynamicenum":
        case "multienum":
        case "currency_id":
          return sticCVUtils.text(this.$editor.find("option:selected"), this.customView);
        case "radioenum":
          return sticCVUtils.text(this.$editor.parent().find("[type='radio']:checked").parent(), this.customView);
        case "bool":
          return this.value() ? "☒" : "☐";
        case "relate":
          return this.value().split("|").slice(-1)[0];
        default:
          return this.value();
      }
    }
    var text = "";
    if (this.customView.view == "detailview") {
      text = this.$element.text().trim();
    } else {
      text = sticCVUtils.text(this.$element, this.customView);
    }

    return text;
  }

  is_readonly() {
    return this.$readonlyLabel.length != 0 && !this.$readonlyLabel.hasClass("hidden");
  }

  applyAction(action) {
    switch (action.action) {
      case "color":
        sticCVUtils.color(this.$editor, this.customView, action.value);
        sticCVUtils.color(this.$items, this.customView, action.value, true);
        sticCVUtils.color(this.$readonlyLabel, this.customView, action.value);
        sticCVUtils.color(this.$element, this.customView, action.value);
        return this;
      case "background":
        sticCVUtils.background(this.$editor, this.customView, action.value);
        sticCVUtils.background(this.$items, this.customView, action.value);
        sticCVUtils.background(this.$readonlyLabel, this.customView, action.value);
        if (this.customView.view == "detailview" || this.type == "radioenum") {
          sticCVUtils.background(this.$element, this.customView, action.value);
        }
        return this;
      case "bold":
        sticCVUtils.bold(this.$editor, this.customView, action.value);
        sticCVUtils.bold(this.$items, this.customView, action.value);
        sticCVUtils.bold(this.$readonlyLabel, this.customView, action.value);
        sticCVUtils.bold(this.$element, this.customView, action.value);
        return this;
      case "italic":
        sticCVUtils.italic(this.$editor, this.customView, action.value);
        sticCVUtils.italic(this.$items, this.customView, action.value);
        sticCVUtils.italic(this.$readonlyLabel, this.customView, action.value);
        sticCVUtils.italic(this.$element, this.customView, action.value);
        return this;
      case "underline":
        sticCVUtils.underline(this.$editor, this.customView, action.value);
        sticCVUtils.underline(this.$items, this.customView, action.value);
        sticCVUtils.underline(this.$readonlyLabel, this.customView, action.value);
        sticCVUtils.underline(this.$element, this.customView, action.value);
        return this;
      case "readonly":
        if (this.customView.view != "editview" && this.customView.view != "quickcreate") {
          return false;
        }
        var readonly = sticCVUtils.isTrue(action.value);
        sticCVUtils.show(this.$elementEditor, this.customView, !readonly);
        sticCVUtils.show(this.$readonlyLabel, this.customView, readonly);
        return this;
      case "inline":
        if (this.customView.view != "detailview") {
          return false;
        }
        sticCVUtils.inline_edit(this, action.value);
        return this;
      case "fixed_value":
        return this.value(action.value);
    }
    return super.applyAction(action);
  }

  onChange(callback) {
    if (this.customView.view == "detailview") {
      return this.field.container.onChange(callback);
    } else {
      return sticCVUtils.onChange(this.$editor, callback) || super.onChange(callback);
    }
  }
  change() {
    return sticCVUtils.change(this.$editor) || super.change();
  }

  checkCondition(condition) {
    if (condition.operator == "is_null" || condition.operator == "is_not_null") {
      condition.condition_type = "value";
    }

    switch (condition.condition_type) {
      case "value":
        return this.checkCondition_value(condition);
      case "date":
        return this.checkCondition_date(condition);
      case "user":
        return this.checkCondition_user(condition);
      case "field":
        return this.checkCondition_field(condition);
    }
    return false;
  }

  _isMultienumCancelledInline() {
    return (
      this.type == "multienum" &&
      this.customView.view == "detailview" &&
      !this.$element.hasClass("inlineEditActive") &&
      this.$fieldText.length == 0
    );
  }

  checkCondition_value(condition) {
    switch (condition.operator) {
      case "Not_Equal_To":
        condition.operator = "Equal_To";
        var ret = !this.checkCondition_value(condition);
        condition.operator = "Not_Equal_To";
        return ret;
      case "Not_Contains":
        condition.operator = "Contains";
        var ret = !this.checkCondition_value(condition);
        condition.operator = "Not_Contains";
        return ret;
      case "Not_Starts_With":
        condition.operator = "Starts_With";
        var ret = !this.checkCondition_value(condition);
        condition.operator = "Not_Starts_With";
        return ret;
      case "Not_Ends_With":
        condition.operator = "Ends_With";
        var ret = !this.checkCondition_value(condition);
        condition.operator = "Not_Ends_With";
        return ret;
      case "is_not_null":
        condition.operator = "is_null";
        var ret = !this.checkCondition_value(condition);
        condition.operator = "is_not_null";
        return ret;
    }

    var value_list = condition.value_list;

    var currentValue = this._getValue(value_list);
    if (!this._isMultienumCancelledInline()) {
      currentValue = sticCVUtils.normalizeToCompare(currentValue);
    }
    if (currentValue === undefined) {
      currentValue = "";
    }
    if(this.type == "date") {
      currentValue = currentValue.split(" ")[0];
    }

    var conditionValue = condition.value ? condition.value : "";
    if (this.type == "multienum") {
      if (this._isMultienumCancelledInline()) {
        conditionValue = sticCVUtils.getMultienumLabelFromKeys(value_list, conditionValue);
      } else {
        conditionValue = conditionValue.replaceAll("^", "").split(",").sort().join(",");
      }
    }
    if (this.type == "date") {
      conditionValue = conditionValue.split(" ")[0];
    }
    if (this.type == "int" || this.type == "decimal" || this.type == "double") {
      conditionValue = parseFloat(conditionValue);
      currentValue = parseFloat(currentValue);
    }
    if (!this._isMultienumCancelledInline()) {
      conditionValue = sticCVUtils.normalizeToCompare(conditionValue);
    }
    switch (condition.operator) {
      case "Equal_To":
        if (this.type == "relate") {
          var valueSplit = currentValue.split("|");
          if (this.customView.view == "detailview" && valueSplit[0] == "undefined") {
            return valueSplit[1] == conditionValue.split("|")[1];
          } else {
            return valueSplit[0] == conditionValue.split("|")[0];
          }
        } else {
          return currentValue == conditionValue;
        }
      case "Greater_Than":
        if (this.type == "date" || this.type == "datetime" || this.type == "datetimecombo") {
          return moment(currentValue, condition.date_format).isAfter(moment(conditionValue, condition.date_format));
        } else {
          return currentValue > conditionValue;
        }
      case "Less_Than":
        if (this.type == "date" || this.type == "datetime" || this.type == "datetimecombo") {
          return moment(currentValue, condition.date_format).isBefore(moment(conditionValue, condition.date_format));
        } else {
          return currentValue < conditionValue;
        }
      case "Greater_Than_or_Equal_To":
        if (this.type == "date" || this.type == "datetime" || this.type == "datetimecombo") {
          return moment(currentValue, condition.date_format).isSameOrAfter(
            moment(conditionValue, condition.date_format)
          );
        } else {
          return currentValue >= conditionValue;
        }
      case "Less_Than_or_Equal_To":
        if (this.type == "date" || this.type == "datetime" || this.type == "datetimecombo") {
          return moment(currentValue, condition.date_format).isSameOrBefore(
            moment(conditionValue, condition.date_format)
          );
        } else {
          return currentValue <= conditionValue;
        }
      case "Contains":
        if (this.type == "multienum") {
          var valueArray = currentValue.split(",");
          var conditionValueArray = conditionValue.split(",");
          for (var conditionValue of conditionValueArray) {
            if (!valueArray.includes(conditionValue)) {
              return false;
            }
          }
          return true;
        } else {
          currentValue = currentValue ? currentValue : "";
          return currentValue.includes(conditionValue);
        }
      case "Starts_With":
        currentValue = currentValue ? currentValue : "";
        return currentValue.startsWith(conditionValue);
      case "Ends_With":
        currentValue = currentValue ? currentValue : "";
        return currentValue.endsWith(conditionValue);
      case "is_null":
        currentValue = currentValue ? currentValue : "";
        var value = currentValue.split("|")[0];
        if (this.type == "date" || this.type == "datetime" || this.type == "datetimecombo") {
          value = value.replace(" 00:00", "");
        }
        return value == "";
    }
    return false;
  }

  checkCondition_date(condition) {
    if (this.type != "date" && this.type != "datetime" && this.type != "datetimecombo") {
      return false;
    }
    switch (condition.operator) {
      case "Not_Equal_To":
        condition.operator = "Equal_To";
        var ret = !this.checkCondition_value(condition);
        condition.operator = "Not_Equal_To";
        return ret;
    }

    var value_list = condition.value_list;

    var valueMoment = moment(sticCVUtils.normalizeToCompare(this._getValue(value_list)), condition.date_format);
    var conditionMoment = null;
    switch (condition.value) {
      case "now":
        conditionMoment = moment();
        break;
      case "today":
        conditionMoment = moment().startOf("day");
        break;
      case "tomorrow":
        conditionMoment = moment().add(1, "days").startOf("day");
        break;
      case "yesterday":
        conditionMoment = moment().add(-1, "days").startOf("day");
        break;
      case "anniversary":
        conditionMoment = moment().startOf("day");
        valueMoment.year(conditionMoment.year());
        break;
    }
    if (conditionMoment == null) {
      return false;
    }
    switch (condition.operator) {
      case "Equal_To":
        switch (condition.value) {
          case "now":
            return valueMoment.isSame(conditionMoment);

          case "today":
          case "tomorrow":
          case "yesterday":
          case "anniversary":
            return valueMoment.isSame(conditionMoment, "day");
        }
      case "Greater_Than":
        switch (condition.value) {
          case "now":
            return valueMoment.isAfter(conditionMoment);

          case "today":
          case "tomorrow":
          case "yesterday":
          case "anniversary":
            return valueMoment.isAfter(conditionMoment, "day");
        }
      case "Less_Than":
        switch (condition.value) {
          case "now":
            return valueMoment.isBefore(conditionMoment);

          case "today":
          case "tomorrow":
          case "yesterday":
          case "anniversary":
            return valueMoment.isBefore(conditionMoment, "day");
        }
      case "Greater_Than_or_Equal_To":
        switch (condition.value) {
          case "now":
            return valueMoment.isSameOrAfter(conditionMoment);

          case "today":
          case "tomorrow":
          case "yesterday":
          case "anniversary":
            return valueMoment.isSameOrAfter(conditionMoment, "day");
        }
      case "Less_Than_or_Equal_To":
        switch (condition.value) {
          case "now":
            return valueMoment.isSameOrBefore(conditionMoment);

          case "today":
          case "tomorrow":
          case "yesterday":
          case "anniversary":
            return valueMoment.isSameOrBefore(conditionMoment, "day");
        }
    }

    return false;
  }

  checkCondition_user(condition) {
    if (this.type != "relate") {
      return false;
    }

    condition.value = SUGAR.sticCV_currentUser;
    return this.checkCondition_value(condition);
  }

  checkCondition_field(condition) {
    return false;
  }
};
