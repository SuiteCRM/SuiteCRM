/**
 * This file contains logic and functions needed to manage validations and views behaviour
 * and will be included in most view.*.php files.
 *
 * File index:
 * - Code to be run on/after view load
 * - Functions for setting field behaviour and aspect
 * - Aux functions for views/forms management
 * - Functions for getting the value of a specific field
 * - Functions for creating detail/list view buttons
 * - Functions for specific data validation (NIF/NIE, CIF, IBAN, dates)
 * - Aux functions for general purposes
 */

/*************************************
 * Code to be run on/after view load *
 *************************************/

// Actions to run when page load is completed
$(document).ready(function () {
  markNoInlineEdit();
  runFunctionsAfterAjaxLoadEnd();
});

// Create empty object for dinamic validations to prevent execution errors
validateFunctions = {};

// Create empty object for dependencies,to overwrite the object if it is inherited from the previous page
validationDependencies = {};

/**
 * Add parent css stylesheets to iframe wysiwyg fields (only in detail view)
 */
function setStylesToWysywigIframe() {
  if (viewType() == "detail") {
    $("link[rel=stylesheet]").each(function () {
      $(".detail-view-row-item iframe")
        .contents()
        .find("head")
        .append(
          $("<link>", {
            rel: "stylesheet",
            href: $(this).attr("href"),
            type: "text/css"
          })
        );
    });
    // after (500ms) appends stylesheets, show the iframe element, previously hidden by css
    setTimeout(() => {
      $(".detail-view-row-item iframe").css("visibility", "visible");
    }, 500);
  }
}

/**
 *  Set specific class and text to fields that are not editable inline in detail and list views
 */
function markNoInlineEdit() {
  switch (viewType()) {
    case "detail":
      $(".detail-view-field").each(function () {
        if ($(this).is(":not(.inlineEdit)")) {
          $(this)
            .addClass("no-inlineEdit")
            .attr("title", SUGAR.language.languages.app_strings["LBL_STIC_NOT_EDITABLE_INLINE"]);
        }
      });
      break;

    case "list":
      $("table.list.view tbody td[type]:not(.inlineEdit) ")
        .addClass("no-inlineEdit")
        .attr("title", SUGAR.language.languages.app_strings["LBL_STIC_NOT_EDITABLE_INLINE"]);
      break;

    default:
      break;
  }
}

/**
 * Run functions that can only be executed when there is no ajax call in progress
 */
function runFunctionsAfterAjaxLoadEnd() {
  var runIfViewIsFullRendered = setInterval(function () {
    if (SUGAR_callsInProgress === 0) {
      validationDependenciesCheck();
      validateFunctionsInit();
      setStylesToWysywigIframe();
      clearInterval(runIfViewIsFullRendered);
    }
  }, 200);
}

/**
 * Check that all fields needed for the defined validations are on the screen.
 * Otherwise display a warning message and disable the validations.
 *
 * The object validationDependencies should be defined in the module's Utils.js file and should
 * contain the name of the field to be validated followed by the fields on which it depends.
 *
 * If a field depends on two or more fields it must be indicated as an array.
 * If two fields depend on each other both pairs (1,2) (2,1) should be declared in the object.
 *
 * Example:
 *
 *   var validationDependencies={
 *      bank_account:["payment_method","another_field"],
 *      end_date:"start_date",
 *      start_date:"end_date"
 *   }
 */
function validationDependenciesCheck() {
  // Create the empty object if it does not exist, to avoid errors
  validationDependencies = typeof validationDependencies != "undefined" ? validationDependencies : {};

  $.each(validationDependencies, function (l, k) {
    // If k is string, convert to array
    k = typeof k == "string" ? [k] : k;

    // Create jQuery Selector
    kSelector = $.map(k, function (kElement) {
      return "[field=" + kElement + "]";
    });

    // Create field labels
    kDependLabels = $.map(k, function (kElement) {
      var lbl = "LBL_" + kElement;

      // If it is a custom field then remove the suffix "_c" to match the standard label construction
      if (lbl.endsWith("_c")) {
        lbl = lbl.substr(0, lbl.length - 2);
      }
      lbl = SUGAR.language.languages[module][lbl.toUpperCase()];
      lbl = lbl == undefined ? kElement : lbl;

      return lbl;
    });

    // Determine the $context where search necessary fields availability
    switch (viewType()) {
      case "detail":
        $searchContext = $(".detail-view");
        break;
      case "list":
        $searchContext = $(".list-view-rounded-corners table.list.view:visible>tbody>tr:first");
        break;
      case "edit":
        $searchContext = $(".edit-view-row");
        break;
      default:
        $searchContext = undefined;
        break;
    }

    // If a field (l) depends on another field (k) and this one is not available...
    if ($(kSelector.join(), $searchContext).length < k.length) {
      $("[field=" + l + "]")
        .first()
        .each(function () {
          $this = $(this);
          var fieldMsg = SUGAR.language.languages.app_strings["LBL_STIC_SINGULAR_VALIDATE_FIELDS_NOT_VISIBLE"] + " \n\n · " + kDependLabels.join("\n · ");
          // Show a warning icon and a message (view-dependent)
          switch (viewType()) {
            case "detail":
              $this
                .closest(".detail-view-row-item")
                .addClass("depend-not-exist")
                .attr("title", fieldMsg);
              break;
            case "list":
              $("table.list.view th").each(function () {
                if (
                  $(this)
                    .html()
                    .indexOf(l) > 0
                ) {
                  if ($("[field=" + l + "].no-inlineEdit").length == 0) {
                    $(this)
                      .addClass("depend-not-exist")
                      .attr("title", fieldMsg);
                  }
                }
              });
              break;
            case "edit":
              $this
                .closest(".edit-view-row-item")
                .addClass("depend-not-exist")
                .attr("title", fieldMsg);
              break;
            default:
              break;
          }
          // Remove field validations
          removeFromValidate(getFormName(), l);
        });
    }
  });
}

/**
 * Initialize validate functions declared in validateFunctions object.
 * Required for validations in inline edition.
 */
function validateFunctionsInit() {
  if (["list", "detail"].indexOf(viewType()) >= 0) {
    $.each(validateFunctions, function (l, k) {
      k();
    });
  }
}

/****************************************************
 * Functions for setting field behaviour and aspect *
 ****************************************************/

/**
 * Define some fields behaviour depending on currentValue. Every possible currentValue
 * should have two items: enabled and disabled, that will include the fields to be treated.
 *
 * Example:
 *
 * customStatus = {
 *   direct_debit: {
 *     enabled: ['bank_account', 'mandate', 'banking_concept'],
 *     disabled: [],
 *   },
 *   transfer_issued: {
 *     enabled: ['bank_account', 'banking_concept'],
 *     disabled: ['mandate'],
 *   },
 *   default: {
 *     enabled: [],
 *     disabled: ['bank_account', 'banking_concept', 'mandate'],
 *   }
 * }
 *
 * @param Object customStatus, like the example
 * @param String currentValue, Expect a key in the customStatus object. If the key is not found, the "default" key will be used
 * @param Boolean clearField, if true, the field will be emptied when disabled
 */
function setCustomStatus(customStatus, currentValue, clearField = false) {
  cl("Running setCustomStatus");

  // Set default value if currentValue is not defined in customStatus object
  if (customStatus[currentValue] == undefined) {
    currentValue = "default";
  }

  // Enable fields and add validations
  customStatus[currentValue].enabled.forEach(elementId => {
    // If there is a field validation function defined, execute it
    if (validateFunctions[elementId] != undefined) {
      // Ensure that setEnabledStatus is executed once validations are done
      $.when(validateFunctions[elementId]()).then(setEnabledStatus(elementId, clearField));
    } else {
      setEnabledStatus(elementId, clearField);
    }
  });

  // Disable fields and remove validations
  customStatus[currentValue].disabled.forEach(elementId => {
    setDisabledStatus(elementId, clearField);
    removeFromValidate(getFormName(), elementId);
  });
}

/**
 * Enable fields and apply appropriate classes and styles
 *
 * @param {String} elementId id of the element to enable
 * @param Boolean clearField, if true, the field will be emptied when disabled
 */
function setEnabledStatus(elementId, clearField) {
  var $form = $("form#" + getFormName());
  // Add styles and properties
  if (clearField) {
    $("#" + elementId, $form)
      .prop("hidden", false);
  } else {
    $("#" + elementId, $form)
      .prop("readonly", false);
    $("#" + elementId, $form)
    .removeClass("pseudo-disabled")
    .off('.pseudoDisabled'); // delete all pseudoDisabled events
    $("#" + elementId, $form).parent().find("button")
    .removeClass("pseudo-disabled")
    .off('.pseudoDisabled'); // delete all pseudoDisabled events
  }
  $("#" + elementId, $form)
    .closest(".edit-view-row-item")
    .removeClass("stic-disabled");

  // If a field is required show the mark.
  // IMPORTANT: The function searches in the "validate" array to determine if the field is required,
  // so it must have been previously added to the validation array using the addToValidate function
  validate[getFormName()].forEach((val) => {
    if (val[0] == elementId) {
      if (val[2] == true) {
        addRequiredMark(elementId);
      }
    }
  });
}

/**
 * Disable fields and apply appropriate classes and styles
 *
 * @param {String} elementId id of element to disable
 * @param Boolean clearField, if true, the field will be emptied when disabled
 */
function setDisabledStatus(elementId, clearField) {
  var $form = $("form#" + getFormName());
  if (clearField) {
    $("#" + elementId, $form)
      .prop("hidden", true)
      .val("");
  } else {
    $("#" + elementId, $form)
      .prop("readonly", true);
    $("#" + elementId, $form)
      .addClass("pseudo-disabled")
      .off('.pseudoDisabled') // delete all pseudoDisabled events
      .on('mousedown.pseudoDisabled mouseup.pseudoDisabled focus.pseudoDisabled click.pseudoDisabled change.pseudoDisabled keydown.pseudoDisabled keyup.pseudoDisabled', function (e) {
        e.preventDefault()
        e.stopPropagation();
      });
    $("#" + elementId, $form).parent().find("button")
    .addClass("pseudo-disabled")
    .on('mousedown.pseudoDisabled mouseup.pseudoDisabled focus.pseudoDisabled click.pseudoDisabled change.pseudoDisabled keydown.pseudoDisabled keyup.pseudoDisabled', function (e) {
      e.preventDefault()
      e.stopPropagation();
    });
  }
  $("#" + elementId, $form)
    .closest(".edit-view-row-item")
    .addClass("stic-disabled")
    .removeAttr("style");
  removeRequiredMark(elementId);
}

/**
 * Add the class that shows the required field mark
 *
 * @param {*} elementId id of element to mark
 */
function addRequiredMark(elementId, conditionalClass = "conditional-required") {
  var $form = $("form#" + getFormName());
  $("#" + elementId + "", $form)
    .closest(".edit-view-row-item")
    .find(".label")
    .addClass(conditionalClass);
}

/**
 * Remove the class that shows the required field mark
 *
 * @param {*} elementId id of the element
 */
function removeRequiredMark(elementId, conditionalClass = "conditional-required") {
  var $form = $("form#" + getFormName());
  $("#" + elementId, $form)
    .closest(".edit-view-row-item")
    .find(".label")
    .removeClass(conditionalClass);
}

/**
 * Add required validations to fieldId and show the required field mark
 *
 * @param {*} fieldId id of field
 * @param {*} fieldType  type of field (text, date, decimal, etc)
 * @param {*} fieldMessage validation error message
 */
function setRequiredStatus(fieldId, fieldType, fieldMessage) {
  addToValidate(getFormName(), fieldId, fieldType, true, fieldMessage);
  addRequiredMark(fieldId);
}

/**
 * Remove all validations and hide the required field mark for fieldId
 *
 * @param {*} fieldId id of field
 */
function setUnrequiredStatus(fieldId) {
  removeFromValidate(getFormName(), fieldId);
  removeRequiredMark(fieldId);
}

/**
 * Returns true if the field is required
 * 
 * @param {*} fieldId id of field
 */
function getRequiredStatus(fieldId) {
  var validateFields = validate[getFormName()];
  for (i = 0; i < validateFields.length; i++) {
    // Array(name, type, required, msg);
    if (validateFields[i][0] == fieldId) {
      return validateFields[i][2];
    }
  }
  return false;
}

/**
 * Mark fields that will be autofilled
 */
function setAutofill(fieldList) {
  fieldList.forEach(element => {
    var $el = $("form #" + element);
    $row = $el.closest(".edit-view-row-item");
    $label = $row.find(".label");
    $label.addClass("autofill");
    $row.attr("title", SUGAR.language.languages.app_strings["LBL_STIC_AUTOFILL_FIELDS_INFO"]);
  });
}

/** 
 * Allows filtering the content of a select type field (childId, which can be multiple or not) depending on
 * the value of another field (parentId). Filtering is done through the "change" event of the parentId field.
 * 
 * childId field elements must follow the same rules that apply for SuiteCRM dynamicenum fields.
 * 
 * Parameters:
 * @param {String} parentId: Id of the master field
 * @param {String} childId: Id of the dependent select
 */
function filterSelectField(parentId, childId) {
  $parent = $("select#" + parentId);
  $parent.off("change");
  $parent.on("change", function () {
    filterSelectField(parentId, childId);
  });
  $child = $("select#" + childId);
  parentVal = $parent.val();

  $("option", $child).each(function () {
    $this = $(this);
    if ($this.val().startsWith(parentVal + "_")) {
      $this.show();
    } else {
      $this.hide();
    }
  });
}


/********************************************
 * Aux functions for views/forms management *
 ********************************************/

/**
 * Returns the name of the active form (used only for editable views)
 */
function getFormName() {
  var formNames = ["form_DCQuickCreate_" + module, "form_SubpanelQuickCreate_" + module, "form_QuickCreate_" + module, "EditView"];
  var form = null;
  for (var i = 0; i < formNames.length && !form; i++) {
    form = document.forms[formNames[i]];
  }
  return form != null ? form.id : "EditView";
}

/**
 * Get the view type
 */
function viewType() {
  if ($(".listViewBody").length == 1) {
    return "list";
  } else if ($(".sub-panel .quickcreate form").length == 1) {
    return "quickcreate";
  } else if ($(".detail-view").length == 1) {
    return "detail";
  } else if ($("form[name=EditView]").length == 1) {
    return "edit";
  } else if ($("#popup_query_form").length == 1) {
    return "popup";
  }
}

/*******************************************************
 * Functions for getting the value of a specific field *
 *******************************************************/

/**
 * Get the value of a field in any of the edit|detail|list views (view-dependent function).
 * In case of enum type fields value will be get through the visible label.
 *
 * @param String fieldName Required The name of the field
 * @param String listName Required when type is enum
 * @return String found value or '' if a value has not been found for any reason.
 */
function getFieldValue(fieldName, listName) {
  // If fieldName is the active inline edit field, obtain the value directly
  $activeField = $(".inlineEditActive [name=" + fieldName + "]");
  if ($activeField.length == 1) {
    var res = $activeField.val();
    return res === undefined ? '' : res;
  }

  switch (viewType()) {
    case "edit":
      var res = $("form#EditView #" + fieldName).val();
      return res === undefined ? '' : res;

    case "quickcreate":
      var res = $("#" + fieldName, ".sub-panel .quickcreate form").val();
      return res === undefined ? '' : res;

    case "popup":
      var res = $("#" + fieldName, "form .edit-view-row").val();
      return res === undefined ? '' : res;

    case "detail":
      var $field = $(".detail-view-field[field=" + fieldName + "]");
      if ($field.length == 1) {
        var fieldType = $field.attr("type");
        if (fieldType == "enum") {
          if (!listName) {
            console.error("In enum type fields it is necessary to indicate the full name of the list");
            return '';
          }
          var res = getListValueFromLabel(listName, trim($field.text()));
          return res === undefined ? '' : res;
        } else {
          var res = trim($field.text());
          return res === undefined ? '' : res;
        }
      } else {
        console.error("It was not possible to obtain the value of the field [" + fieldName + "]");
        return '';
      }

    case "list":
      if ($(".inlineEditActive").length === 0) {
        console.error("The getFieldValue() function has been called in a list, but there is no active inline-edit field, so it cannot be evaluated");
        return '';
      }

      var $field = $("td[field=" + fieldName + "] ", $(".inlineEditActive").closest("tr"));

      if ($field.length == 1) {
        if ($field.closest("form[name=EditView]") > 0) {
          return $field.val() === undefined ? '' : $field.val();
        } else {
          var fieldType = $field.attr("type");
          if (fieldType == "enum") {
            var res = getListValueFromLabel(listName, trim($field.text()));
            if (!listName) {
              console.error("In enum type fields it is necessary to indicate the full name of the list");
            }
            return res === undefined ? '' : res;
          } else {
            var res = trim($field.text());
            return res === undefined ? '' : res;
          }
        }
      } else {
        console.error("It was not possible to obtain the value of the field [" + fieldName + "]");
        return '';
      }

    default:
      return '';
  }
}




/**
 * Return key value for label in app_list_stringsName array
 *
 * @param String app_list_stringsName $app_list_string to search in
 * @param String label The label to be searched (in current language)
 */
function getListValueFromLabel(app_list_stringsName, label) {
  var res;
  $.each(SUGAR.language.languages.app_list_strings[app_list_stringsName], function (l, k) {
    if (k == label) {
      res = l;
    }
  });
  return res;
}

/***************************************************
 * Functions for creating detail/list view buttons *
 ***************************************************/

/**
 * Add a custom button at the end of the detail view actions menu
 *
 * Example 1 - Basic button:
 *    showSessionAssistant = {
 *      id: "show_session_assistant",
 *      title: SUGAR.language.get('stic_Events', 'LBL_PERIODIC_SESSIONS'),
 *      onclick: "location.href='" + STIC.siteUrl + "/index.php?module=stic_Events&action=showSessionAssistant&event_id=" + STIC.record.id + "'",
 *    },
 *
 * Example 2 - Advanced button:
 *   showSessionAssistant2 = {
 *      id: "show_session_assistant2",
 *      type: "date",
 *      title: "My button",
 *      value: "2020-05-10",
 *      class: "button disabled",
 *      onclick: "doSomething()",
 *      style: "background-color:white; color:black;"
 *    },
 *
 * @param Object data Contains the HTML attributes to assign to the button that will be created
 */
function createDetailViewButton(data) {
  // Set default values in case they are not defined in data object
  data.type = data.type || "button";
  // Type of element to create. Default is button.
  data.value = data.value || data.title;
  // If value attribute is not provided, title is used.
  data.class = data.class || "button";
  // Css class(es) to apply. Default is button.
  // Get a reference for the actions menu
  $container = $("#tab-actions ul.dropdown-menu");
  // Create an empty list item
  $li = $("<li>");
  // Append the list item to the actions menu
  $container.append($li);
  // Create an input with all the attributes set in data object (type="button", etc.)
  $input = $("<input>", data);
  // Append the button to the list item
  $li.append($input);
}

/**
 * Create a button adapted to the list view actions menu, and add it to the end of the list.
 *
 * Example:
 *    data: {
 *      id: 'show_session_assistant',
 *      text: SUGAR.language.get('stic_Events', 'LBL_PERIODIC_SESSIONS'),
 *      onclick: "function()", (optional)
 *      href: "url", (optional)
 *      class: "class_name", (optional)
 *    },
 * @param Object data Contains the properties to assign to the button
 */

function createListViewButton(data) {
  // Set default values in case they are not defined in data object
  data.href = data.href || "javascript:void(0)";
  data.class = data.class || "parent-dropdown-action-handler";
  // Css class(es) to apply. Default is button.
  // Get a reference for the actions menu
  $container = $("#actionLinkTop ul.subnav");
  // Create an empty list item
  $li = $("<li>");
  // Append the list item to the actions menu
  $container.append($li);
  // Create an <a> with all the attributes set in data object
  $a = $("<a>", data);
  // Append the button to the list item
  $li.append($a);
}

/**
 * Used by detail view action button that sends PDF templates by email
 */
function showPopupPdfEmail() {
  var form = document.getElementById('popupForm');
  var ppd = document.getElementById('popupDiv_ara');
  if (ppd != null) {
    ppd.style.display = 'block';
    $("#popupDiv_ara").modal("show", { backdrop: "static" });
  } else {
    alert(SUGAR.language.languages.app_strings["LBL_NO_TEMPLATE"]);
  }
  $('[name=module]').remove();
  form.action = 'index.php?&module=AOS_PDF_Templates&action=AddPDFLinkToEmail&targetModule=' + module;
}

/******************************************************************
 * Specific data validation functions (NIF/NIE, CIF, IBAN, dates) *
 ******************************************************************/

/**
 * IBAN validation (by using the PHP function action_checkIBAN in stic_Payments module controller)
 * @param {String} iban
 */
function checkIBAN(iban) {
  var res;
  $.ajax({
    url: "index.php?module=stic_Payments&action=checkIBAN&iban=" + iban,
    type: "POST",
    async: false
  })
    .done(function (data) {
      res = data;
    })
    .fail(function (data) {
      console.error("It was not possible to verify the validity of the IBAN");
      res = "error";
    });

  return res;
}

/**
 * DNI/NIF/NIE validation.
 * Valid DNI/NIF should have from 5 to 8 numbers followed by a proper check letter.
 * NIE (for foreigners) have a letter (X, Y or Z) in the first place, not a number
 * Source: http://trellat.es/funcion-para-validar-dni-o-nie-en-javascript/
 *
 * param {String} identificationNumber Required The number to validate
 * param {String} type Optional The type of identification number to validate.
 *                If type is 'nie' require starts with letter X,Y or Z.
 *                If type is 'nif' can start only with letter K,L or M: 
 *                     [K|L|M]+ 7 digits + 1 control character
 *                      or 
 *                     8 digits + 1 control character          
 * 
 *                If 'type' is not provided it will be validated if it is a valid 'nie' or 'dni'
 *
 */
function isValidIdentificationNumber(identificationNumber, type) {
  var number, let, letter;
  var DNIRegexp = /^[XYZMLK]?\d{5,8}[A-Z]$/;
  identificationNumber = identificationNumber.toUpperCase();

  var firstCharacter = identificationNumber[0]

  switch (firstCharacter) {
    case "X":
    case "Y":
    case "Z":
      if (type == 'nif') {
        return false;
      }
      break;
    case "K":
    case "L":
    case "M":
      if (type == 'nie') {
        return false;
      }
      break;
    default:
      if (isNumeric(firstCharacter) && type == 'nie') {
        return false;
      }
  }

  if (DNIRegexp.test(identificationNumber) === true) {
    number = identificationNumber.substr(0, identificationNumber.length - 1);
    number = number.replace("X", 0);
    number = number.replace("M", 0);
    number = number.replace("L", 0);
    number = number.replace("K", 0);
    number = number.replace("Y", 1);
    number = number.replace("Z", 2);
    let = identificationNumber.substr(identificationNumber.length - 1, 1);
    number = number % 23;
    letter = "TRWAGMYFPDXBNJZSQVHLCKET";
    letter = letter.substring(number, number + 1);
    if (letter != let) {
      return false;
    } else {
      return true;
    }
  } else {
    return false;
  }
}


/**
 * CIF validation
 * Adapted to javascript from: http://www.michublog.com/informatica/8-funciones-para-la-validacion-de-formularios-con-expresiones-regulares
 */
function isValidCif(cif) {
  cif.toUpperCase();

  cifRegEx1 = /^[ABEH][0-9]{8}/i;
  cifRegEx2 = /^[KPQS][0-9]{7}[A-J]/i;
  cifRegEx3 = /^[CDFGJLMNRUVW][0-9]{7}[0-9A-J]/i;

  if (cif.match(cifRegEx1) || cif.match(cifRegEx2) || cif.match(cifRegEx3)) {
    control = cif.charAt(cif.length - 1);
    sumA = 0;
    sumB = 0;

    for (i = 1; i < 8; i++) {
      if (i % 2 == 0) sumA += parseInt(cif.charAt(i));
      else {
        t = (parseInt(cif.charAt(i)) * 2).toString();
        p = 0;

        for (j = 0; j < t.length; j++) {
          p += parseInt(t.charAt(j));
        }
        sumB += p;
      }
    }

    sumC = parseInt(sumA + sumB) + "";
    // Así se convierte en cadena
    sumD = (10 - parseInt(sumC.charAt(sumC.length - 1))) % 10;

    letters = "JABCDEFGHI";

    if (control >= "0" && control <= "9") return control == sumD;
    else return control.toUpperCase() == letters[sumD];
  } else return false;
}

/**
 * Check that end_date is prior to start_date. Return true if one of the two dates does not exist.
 * It is necessary to load at the beginning of the page moment.js by "loadScript("include/javascript/moment.min.js");"
 * It is assumed that if start_date and end_date include hours and minutes, they will be in H: i (php) or HH: MM (momentjs) format
 *
 * @param {String} startDate name of the field whose date must be previous
 * @param {String} endDate name of the field whose date must be prior
 * @param {Boolean} notSame condition to reject same dates
 *
 */
function checkStartAndEndDatesCoherence(startDate, endDate, $notSame = false) {
  var startDate = getFieldValue(startDate);
  var endDate = getFieldValue(endDate);
  var userDateFormat = STIC.userDateFormat.toUpperCase();
  // Set to upper to match php and js nomenclatures

  if (startDate == "" || endDate == "") {
    return true;
  }
  if ($notSame) {
    return moment(startDate, userDateFormat + "HH:mm").isBefore(moment(endDate, userDateFormat + "HH:mm"));
  } else {
    return moment(startDate, userDateFormat + "HH:mm").isSameOrBefore(moment(endDate, userDateFormat + "HH:mm"));
  }
}

/**************************************
 * Aux functions for general purposes *
 **************************************/

/**
 * Write info to console log. Useful for debugging. Should be disabled otherwise.
 */
function cl(data, type = "log") {
  // Set active to true/false to enable/disable console log.
  var active = true;
  if (active == true) {
    console[type](data);
  }
}

/**
 * Loads a script from a given url and calls the callback function when it is loaded.
 */
function loadScript(url, callback) {
  cl("Loading script [" + url + "].");

  // Add the script tag to the head
  var head = document.getElementsByTagName("head")[0];
  var script = document.createElement("script");
  script.type = "text/javascript";
  script.src = url;

  // Bind events to the callback function if exist
  if (callback) {
    // There are several events for cross browser compatibility.
    script.onreadystatechange = callback;
    script.onload = callback;
  }

  // Fire the loading
  head.appendChild(script);
}

/**
 * Format decimal value according to user's preferences
 */
function myFormatNumber(element) {
  format = formatNumber(element ? element : "0", num_grp_sep, dec_sep, 2, 2);
  return format;
}

/**
 * Transform a Javascript Date into a DB date format
 */
function dateToYMDHM(date) {
  var d = date.getDate();
  var m = date.getMonth() + 1;
  var y = date.getFullYear();
  var h = date.getHours();
  var min = date.getMinutes();
  return "" + y + "-" + (m <= 9 ? "0" + m : m) + "-" + (d <= 9 ? "0" + d : d) + " " + h + ":" + (min <= 9 ? "0" + min : min) + ":00";
}

/**
 * Reloads the options for a dynamic enum select element and updates the selectize plugin.
 * Also adds a change event listener to the parent element's selectize instance, so that
 * when the parent element's value changes, the child element's options will be reloaded and
 * its selectize instance will be updated.
 *
 * @param {string} childName - The ID of the child select element to update.
 */
function reloadLastDinamicEnumSelectize(childName) {
  //Select the child element using the passed in childName
  var $child = $('select#' + childName);
  //Get the parent name from the child's "parent_enum" attribute
  var parentName = $child.attr('parent_enum');
  //Select the parent element using the parentName
  var $parent = $('#' + parentName)
  //Load the dynamic enum options for the child element
  loadDynamicEnum(parentName, childName);

  // Selectize child control
  $child.selectize({
    allowEmptyOption: true,
    selectOnTab: true,
  });

  //Add an event listener to the parent element's selectize instance
  if ($parent && $parent[0] && $parent[0].selectize) {
    $parent[0].selectize.on('change', function () {

      // destroy previous child selectize control
      if ($child[0].selectize) {
        $child[0].selectize.destroy()
      }

      //Reload the dynamic enum options for the child element
      loadDynamicEnum(parentName, childName);

      // Highlight the dependent field by blinking it
      var blinkTime = 125; //time in ms for each blink
      var blinkCount = 2; //number of times to blink
      var blink = 0;
      var intervalId = setInterval(function () {
        $child.parent([type = "dynamicenum"]).animate({ opacity: 0 }, blinkTime).animate({ opacity: 1 }, blinkTime);
        blink++;
        if (blink === blinkCount) {
          clearInterval(intervalId);
        }
      }, blinkTime);

      // Re selectize child control
      $child.selectize({
        allowEmptyOption: true,
        selectOnTab: true,
      });

    })
  }
}
/**
 * This function is used for the "Color" field of the EISA modules to display the colored events in the Activities Calendar.
 * It can be used both in EditView or ListView (for filter and mass update fields).
 * It transform the "select" html element into selectize and adds a color dot on the left side of the label, using the color 
 * hexadecimal code assigned in the internal value of the item/option.
 * @param {String} fieldName 
 */
function buildEditableColorFieldSelectize(fieldName) {
  if ($('#'+fieldName).length) {
    // In case the field is already "selectized" (ex: filter fields), first we destroy the selectization
    $.each($('#'+fieldName),function (i, el) {
      if (el.selectize) el.selectize.destroy();
    });
    // Selectizing the field and adding the color dot
    $(document).ready(function() {
      $('#'+fieldName).selectize({
        plugins: ["remove_button"],
        allowEmptyOption: true,
        render: {
          option: function(item, escape) {
            return '<div class="option" style="display: flex; align-items: center; padding: 5px;">' +
                    '<div style="background-color: #' + escape(item.value) + '; width: 20px; height: 20px; border-radius: 50%; margin-right: 10px; margin-left: 10px;"></div>' +
                    escape(item.text) +
                  '</div>';
          },
          item: function(item, escape) {
            return '<div class="item" style="display: inline-flex; align-items: left; padding: 5px;">' +
              '<div style="background-color: #' + escape(item.value) + '; width: 20px; height: 20px; border-radius: 50%; margin-right: 10px; margin-left: 5px;"></div>' +
              escape(item.text) +
            '</div>';
          }
        }
      });
    });
  }
}

/**
 * This function is used for the "Color" field of the EISA modules to display the colored events in the Activities Calendar.
 * It can be used in the DetailView.
 * It removes all the elements related with the "color" field and creates them again, adding the color dot on the left side of 
 * the label.
 * @param {String} fieldName 
 */
function buildDetailedColorFieldSelectize(fieldName) {
  if ($('#'+fieldName).length) {
    $color = $('#'+fieldName);
    colorVal = $color.val();
    $parent = $color.parent()
    text = $parent.text();
    $parent.empty();
    $parent.append(
      '<div style="display: flex; align-items: center; padding: 5px;">' +
      '<div style="width: 20px; height: 20px; border-radius: 50%; margin-right: 10px; background-color: #' + escape(colorVal) + ';"></div>' +
      $color[0].outerHTML + text + '</div>'
    );
  }
}

/**
 * Add qtip (info-popup) functionality to an html element.
 * Normally used in this element:
 * <span id="id_selector" style='position: relative;'class="inline-help glyphicon glyphicon-info-sign data-hasqtip"></span>
 * @param {String} selector qtip selector ('#' for id, '.' for class)
 * @param {String} module 
 * @param {String} label LBL...
 */
function addQtipFunctionality(selector, module, label) {
  $(selector).qtip({
    content: {
      text: function (api) {
        return SUGAR.language.translate(module, label);
      },
      title: {
        text: SUGAR.language.languages.app_strings.LBL_ALT_INFO,
      },
      style: {
        classes: 'qtip-inline-help'
      }
    },
    hide: { 
    event: 'mouseleave unfocus',
    fixed: true,
    delay: 200,
    }
  });
}