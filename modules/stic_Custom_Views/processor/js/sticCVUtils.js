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

var sticCVUtils = class sticCVUtils {
  static show($elem, customView = null, show = true) {
    show = sticCVUtils.isTrue(show);
    $elem.each(function() {
      if (show) {
        if (!$(this).hasClass("auto-hidden")) {
          sticCVUtils.removeClass($(this), customView, "hidden");
        }
      } else {
        sticCVUtils.addClass($(this), customView, "hidden");
        sticCVUtils.removeClass($(this), customView, "auto-hidden");
      }
    });
  }
  static show_auto($elem, customView = null, show = true) {
    show = sticCVUtils.isTrue(show);
    $elem.each(function() {
      if (show) {
        if ($(this).hasClass("auto-hidden")) {
          sticCVUtils.removeClass($(this), customView, "hidden");
          sticCVUtils.removeClass($(this), customView, "auto-hidden");
        }
      } else {
        if (!$(this).hasClass("hidden")) {
          sticCVUtils.addClass($(this), customView, "hidden");
          sticCVUtils.addClass($(this), customView, "auto-hidden");
        }
      }
    });
  }
  static is_visible($elem) {
    var visible = true;
    $elem.each(function() {
      visible &= !$(this).hasClass("hidden");
    });
    return visible;
  }
  static color($elem, customView = null, color = "", important = false) {
    $elem.each(function() {
      if (important) {
        var currentStyle = $(this).attr("style");
        if (currentStyle) {
          var match = currentStyle.match(/color\s*:\s*[^;]+/);
          if (match) {
            var newStyle = currentStyle.replace(match[0], "color: " + color + " !important;");
          } else {
            var newStyle = currentStyle + " color: " + color + " !important;";
          }
        } else {
          var newStyle = "color: " + color + " !important;";
        }
        $(this).attr("style", newStyle);
      } else {
        $(this).css("color", color);
      }
      var $self = $(this);
      customView &&
        customView.addUndoFunction(function() {
          $self.css("color", "");
        });
    });
  }
  static background($elem, customView = null, color = "", important = false) {
    $elem.each(function() {
      if (important) {
        var currentStyle = $(this).attr("style");
        if (currentStyle) {
          var match = currentStyle.match(/background-color\s*:\s*[^;]+/);
          if (match) {
            var newStyle = currentStyle.replace(match[0], "background-color: " + color + " !important;");
          } else {
            var newStyle = currentStyle + " background-color: " + color + " !important;";
          }
        } else {
          var newStyle = "background-color: " + color + " !important;";
        }
        $(this).attr("style", newStyle);
      } else {
        $(this).css("background-color", color);
      }
      var $self = $(this);
      customView &&
        customView.addUndoFunction(function() {
          $self.css("background-color", "");
        });
    });
  }
  static bold($elem, customView = null, bold = true) {
    bold = sticCVUtils.isTrue(bold);
    $elem.each(function() {
      if (bold) {
        $(this).css("font-weight", "bold");
        var $self = $(this);
        customView &&
          customView.addUndoFunction(function() {
            $self.css("font-weight", "");
          });
      } else {
        $(this).css("font-weight", "normal");
        var $self = $(this);
        customView &&
          customView.addUndoFunction(function() {
            $self.css("font-weight", "");
          });
      }
    });
  }
  static italic($elem, customView = null, italic = true) {
    italic = sticCVUtils.isTrue(italic);
    $elem.each(function() {
      if (italic) {
        $(this).css("font-style", "italic");
        var $self = $(this);
        customView &&
          customView.addUndoFunction(function() {
            $self.css("font-style", "");
          });
      } else {
        $(this).css("font-style", "normal");
        var $self = $(this);
        customView &&
          customView.addUndoFunction(function() {
            $self.css("font-style", "");
          });
      }
    });
  }
  static underline($elem, customView = null, underline = true) {
    underline = sticCVUtils.isTrue(underline);
    $elem.each(function() {
      if (underline) {
        $(this).css("text-decoration", "underline");
        var $self = $(this);
        customView &&
          customView.addUndoFunction(function() {
            $self.css("text-decoration", "");
          });
      } else {
        $(this).css("text-decoration", "none");
        var $self = $(this);
        customView &&
          customView.addUndoFunction(function() {
            $self.css("text-decoration", "");
          });
      }
    });
  }
  static style($elem, customView = null, style = "") {
    $elem.each(function() {
      var oldStyle = $(this).attr("style");
      if (oldStyle === undefined) {
        oldStyle = "";
      }
      $(this).css(style);
      var $self = $(this);
      customView &&
        customView.addUndoFunction(function() {
          $self.attr("style", oldStyle);
        });
    });
  }
  static frame($elem, customView = null, frame = true) {
    frame = sticCVUtils.isTrue(frame);
    $elem.each(function() {
      if (frame) {
        $(this).css({ "border-color": "orangered", "border-style": "dashed" });
        var $self = $(this);
        customView &&
          customView.addUndoFunction(function() {
            $self.css({ "border-color": "", "border-style": "" });
          });
      } else {
        $(this).css({ "border-color": "", "border-style": "" });
        var $self = $(this);
        customView &&
          customView.addUndoFunction(function() {
            $self.css({ "border-color": "", "border-style": "" });
          });
      }
    });
  }
  static text($elem, customView = null, newText) {
    if (newText !== undefined) {
      newText = newText.trim();
    }
    var textArray = [];
    $elem.each(function() {
      // Only text inside the parent element
      var oldTextArray = $(this).contents().filter(function() {
        return this.nodeType == Node.TEXT_NODE;
      });
      var oldText = "";
      if (
        oldTextArray &&
        oldTextArray.length > 0 &&
        oldTextArray[0] &&
        oldTextArray[0] !== undefined &&
        oldTextArray[0].nodeValue &&
        oldTextArray[0].nodeValue !== undefined
      ) {
        oldText = oldTextArray[0].nodeValue;
      }
      oldText = oldText.trim();
      if (newText === undefined || newText == oldText) {
        textArray.push(oldText);
      } else {
        var newTextArray = $(this).contents().filter(function() {
          return this.nodeType == Node.TEXT_NODE;
        });
        if (
          newTextArray &&
          newTextArray.length > 0 &&
          newTextArray[0] &&
          newTextArray[0] !== undefined &&
          newTextArray[0].nodeValue &&
          newTextArray[0].nodeValue !== undefined
        ) {
          newTextArray[0].nodeValue = newText;
          textArray.push(newText);
          var $self = $(this);
          customView &&
            customView.addUndoFunction(function() {
              $self.text(oldText);
            });
        }
      }
    });
    return textArray.join(", ");
  }
  static addClass($elem, customView = null, className) {
    if (className == "") return;
    $elem.each(function() {
      if (!$(this).hasClass(className)) {
        $(this).addClass(className);
        var $self = $(this);
        customView &&
          customView.addUndoFunction(function() {
            $self.removeClass(className);
          });
      }
    });
  }
  static removeClass($elem, customView = null, className) {
    if (className == "") return;
    $elem.each(function() {
      if ($(this).hasClass(className)) {
        $(this).removeClass(className);
        var $self = $(this);
        customView &&
          customView.addUndoFunction(function() {
            $self.addClass(className);
          });
      }
    });
  }
  static value(fieldContent, newValue, value_list) {
    sticCVUtils.setValue(fieldContent, newValue, value_list);
    return sticCVUtils.getValue(fieldContent, value_list);
  }

  static getValue(fieldContent, value_list) {
    var $elem = fieldContent.$editor;
    if (
      fieldContent.customView.view == "detailview" &&
      fieldContent.type != "bool" &&
      !fieldContent.$element.hasClass("inlineEditActive")
    ) {
      $elem = fieldContent.$fieldText;
      if ($elem.length > 0 && (fieldContent.type == "multienum" || fieldContent.type == "enum")) {
        return $elem.val().replaceAll("^", "").split(",").sort().join(",");
      }
    }
    if ($elem.length == 0 || $elem.get(0).parentNode === null) {
      $elem = fieldContent.$element;
    }

    if (fieldContent.customView.view == "detailview" && !fieldContent.$element.hasClass("inlineEditActive")) {
      if (fieldContent.type == "relate") {
        return $elem.attr("data-id-value") + "|" + $elem.text().trim();
      }
      if (fieldContent.type == "bool") {
        return $elem.prop("checked");
      }
      var text = fieldContent.text();
      if (
        value_list != undefined &&
        value_list != "" &&
        fieldContent.type != "date" &&
        fieldContent.type != "datetime" &&
        fieldContent.type != "datetimecombo" &&
        fieldContent.type != "multienum"
      ) {
        return sticCVUtils.getListValueFromLabel(value_list, text);
      }
      return text;
    }
    switch (fieldContent.type) {
      case "radioenum":
        var $radio = $elem.parent().find("[type='radio']:checked");
        if ($radio.length != 0) {
          return $radio.val();
        }
      case "multienum":
        return $elem.val().sort().join(",");
      case "bool":
        return $elem.prop("checked");
      case "datetimecombo":
        return $elem.last().val();
      case "date":
        return $elem.val() + " 00:00";
      case "relate":
        return $elem.eq(1).val() + "|" + $elem.eq(0).val();
    }
    return $elem.val();
  }

  static setValue(fieldContent, newValue) {
    if (newValue === undefined) return;
    if (fieldContent.customView.view == "detailview") return;

    var $elem = fieldContent.$editor;
    if ($elem.length == 0) {
      $elem = fieldContent.$element;
    }
    if (fieldContent.type == "multienum") {
      var newValueArray = [];
      for (var newValueSingle of newValue.split(",")) {
        if (newValueSingle[0] == "^") {
          newValueArray.push(newValueSingle.substring(1, newValueSingle.length - 1));
        } else {
          newValueArray.push(newValueSingle);
        }
      }
      newValue = newValueArray.sort().join(",");
    }

    var oldValue = sticCVUtils.getValue(fieldContent);
    if (newValue != oldValue) {
      // Set new value
      var customView = fieldContent.customView;

      switch (fieldContent.type) {
        case "radioenum":
          var $radio = $elem.parent().parent().find("[type='radio'][value='" + newValue + "']");
          if ($radio.length != 0) {
            $radio.prop("checked", true);
          } else {
            $elem.val(newValue);
          }
          break;
        case "bool":
          $elem.prop("checked", newValue);
          break;
        case "multienum":
          $elem.val(newValue.split(","));
          break;
        case "datetimecombo":
          var dateTimeArray = newValue.split(" ");
          $elem.eq(0).val(dateTimeArray[0]);
          var timeArray = dateTimeArray[1].split(":");
          $elem.eq(1).val(timeArray[0]);
          $elem.eq(2).val(timeArray[1]);
          $elem.eq(3).val(newValue);
          break;
        case "relate":
          var idNameArray = newValue.split("|");
          $elem.eq(0).val(idNameArray[1]);
          $elem.eq(1).val(idNameArray[0]);
          break;
        default:
          $elem.val(newValue);
          break;
      }

      // Unset value modified by user
      var attr = $elem.attr("data-changedByUser");
      if (typeof attr === "undefined" || attr === false) {
        $elem.removeAttr("data-changedByUser");
      }
      sticCVUtils.change($elem);

      // Set last setted value by this Api to be undoed
      $elem.attr("data-lastChangeByApi", newValue);

      // Add undo function
      customView.addUndoFunction(function() {
        var currentValue = sticCVUtils.value(fieldContent);

        // Check if the last value change with Api is processed
        var attrApi = $elem.attr("data-lastChangeByApi");
        if (typeof attrApi !== "undefined" && attrApi !== false) {
          // The last value change with Api, is the current value?
          if (attrApi != currentValue) {
            // Set data is changed by User
            $elem.attr("data-changedByUser", currentValue);
          } else {
            // Data is not changed by User
            var attrUser = $elem.attr("data-changedByUser");
            if (!(typeof attrUser === "undefined" || attrUser === false)) {
              $elem.removeAttr("data-changedByUser");
            }
          }
          // The last value change with Api is processed
          $elem.removeAttr("data-lastChangeByApi");
        }
        // Undo only if last change is not made by user
        var attrUser = $elem.attr("data-changedByUser");
        if (typeof attrUser === "undefined" || attrUser === false) {
          sticCVUtils.value(fieldContent, oldValue);
        }
      }, true);
    }
  }

  static inline_edit(fieldContent, inline_edit = true) {
    // TODO
    console.log("Inline not available. Requested:" + inline_edit);
    return false;
  }

  static check_required_visible(field) {
    if (sticCVUtils.is_visible(field.content.$element)) {
      if (!sticCVUtils.getRequiredStatus(field)) {
        // Set required when Visible and has "data-requiredIfVisible"
        var attrRequired = field.container.$element.attr("data-requiredIfVisible");
        if (!(typeof attrRequired === "undefined" || attrRequired === false)) {
          sticCVUtils.required(field, true);
          field.container.$element.removeAttr("data-requiredIfVisible");
          field.customView.addUndoFunction(function() {
            field.container.$element.attr("data-requiredIfVisible", attrRequired);
          });
        }
      }
    } else {
      if (sticCVUtils.getRequiredStatus(field)) {
        // Set unrequired when not visible and add "data-requiredIfVisible"
        sticCVUtils.required(field, false);
        var attrRequired = field.container.$element.attr("data-requiredIfVisible");
        if (typeof attrRequired === "undefined" || attrRequired === false) {
          field.container.$element.attr("data-requiredIfVisible", true);
          field.customView.addUndoFunction(function() {
            field.container.$element.removeAttr("data-requiredIfVisible");
          });
        }
      }
    }
  }
  static required(field, required = true) {
    var required = sticCVUtils.isTrue(required);
    var oldRequired = sticCVUtils.getRequiredStatus(field);

    var customView = field.customView;
    sticCVUtils.show(field.header.$element.find("span.required"), customView, false);
    if (required) {
      sticCVUtils.addClass(field.header.$element, customView, "conditional-required");
      removeFromValidate(customView.formName, field.validateName);
      var fieldName = field.header.text();
      if (fieldName === null || fieldName === undefined || fieldName.trim() === "") {
        fieldName = SUGAR.language.get("app_strings", "ERR_MISSING_REQUIRED_FIELDS");
      } else {
        fieldName = fieldName.trim();
        if (fieldName.endsWith(":")) {
          fieldName = fieldName.slice(0, -1);
        }
      }
      addToValidate(
        customView.formName,
        field.validateName,
        field.content.type,
        true,
        fieldName
      );
      if (!oldRequired) {
        customView.addUndoFunction(function() {
          removeFromValidate(customView.formName, field.validateName);
        });
      }
    } else {
      removeFromValidate(customView.formName, field.validateName);
      sticCVUtils.removeClass(field.header.$element, customView, "conditional-required");
      if (oldRequired) {
        customView.addUndoFunction(function() {
          addToValidate(
            customView.formName,
            field.validateName,
            field.content.type,
            true,
            SUGAR.language.get("app_strings", "ERR_MISSING_REQUIRED_FIELDS")
          );
        });
      }
    }
    sticCVUtils.check_required_visible(field);
    return field;
  }
  static getRequiredStatus(field) {
    if (
      field.customView &&
      field.customView.formName &&
      validate[field.customView.formName] &&
      validate[field.customView.formName].length
    ) {
      var validateFields = validate[field.customView.formName];
      for (var i = 0; i < validateFields.length; i++) {
        // Array(name, type, required, msg);
        if (validateFields[i][0] == field.validateName) {
          return validateFields[i][2];
        }
      }
    }
    return false;
  }

  static createObserverCallback($elem, callback) {
    return function() {
      sticCVUtils.onChange($elem.find("input"), callback);
      callback();
    };
  }

  static onChange($elem, callback) {
    $elem.each(function() {
      $(this).on("change paste keyup", callback);
      YAHOO.util.Event.on($(this)[0], "change", callback);
      if (!$(this).is(":input")) {
        var observer = new MutationObserver(sticCVUtils.createObserverCallback($(this), callback));
        observer.observe($(this)[0], { attributes: true, childList: true, subtree: true, characterData: true });
      }
    });
    return $elem.length > 0;
  }
  static change($elem) {
    $elem.each(function() {
      $(this).change();
    });
    return $elem.length > 0;
  }

  /**
     * Return key value for label in app_list_stringsName array
     *
     * @param String app_list_stringsName $app_list_string to search in
     * @param String label The label to be searched (in current language)
    */
  static getListValueFromLabel(app_list_stringsName, label) {
    var res;
    $.each(SUGAR.language.languages.app_list_strings[app_list_stringsName], function(l, k) {
      if (k == label) {
        res = l;
      }
    });
    return res;
  }

  static getMultienumLabelFromKeys(app_list_stringsName, keyValues) {
    var keyValueArray = keyValues.replaceAll("^", "").split(",");
    var labelValueArray = [];
  
    for (var i = 0; i < keyValueArray.length; i++) {
      var label = SUGAR.language.languages.app_list_strings[app_list_stringsName][keyValueArray[i]];
      if(label !== undefined) {
        labelValueArray.push(label);  
      }
    }
    return labelValueArray.join(", ");
  }

  static isTrue(value) {
    return value === true || value === "1" || value === 1 || value === "yes" || value === "true";
  }

  static normalizeToCompare(value) {
    if (typeof value === "string" || value instanceof String) {
      value = value !== null ? value : "";
      return value.toLowerCase().normalize("NFD").replace(/[\u0300-\u036f]/g, "");
    }
    return value;
  }

  static fillReadonlyText(fieldContent) {
    fieldContent.$readonlyLabel.empty();

    var lines = fieldContent.text().split("\n");
    var first = true;
    lines.forEach(function(line) {
      if (first) {
        first = false;
      } else {
        fieldContent.$readonlyLabel.append("<br />");
      }
      fieldContent.$readonlyLabel.append(line);
    });
  }
};
