/* HEADER */
// Set module name
var module = "Contacts";

/* INCLUDES */

/* VALIDATION DEPENDENCIES */
var validationDependencies = {
  stic_identification_type_c: "stic_identification_number_c",
  stic_identification_number_c: "stic_identification_type_c"
};

/* DIRECT VALIDATION CALLBACKS */
validateFunctions.stic_identification_number_c = function() {
  addToValidateCallback(
    getFormName(),
    "stic_identification_number_c",
    "text",
    ["edit", "quickcreate"].indexOf(viewType()) >= 0, // only required in viewType() = edit or quickcreate
    SUGAR.language.get(module, "LBL_STIC_INVALID_IDENTIFICATION_NUMBER_OR_TYPE"),
    function() {
      return JSON.parse(checkIdentificationNumber());
    }
  );
};

addToValidateCallback(getFormName(), "stic_identification_type_c", "text", false, SUGAR.language.get(module, "LBL_STIC_INVALID_IDENTIFICATION_NUMBER_OR_TYPE"), function () {
  if (!getFieldValue('stic_identification_type_c') && getFieldValue('stic_identification_number_c')) {
    return false;
  }
});

addToValidateCallback(getFormName(), "stic_identification_type_c", "text", false, SUGAR.language.get(module, "LBL_STIC_INVALID_IDENTIFICATION_NUMBER_OR_TYPE"), function () {
  return JSON.parse(checkIdentificationNumber());
});

/* VIEWS CUSTOM CODE */
switch (viewType()) {
  case "edit":
  case "quickcreate":
  case "popup":    
    // Definition of the behavior of field identification number
    identificationNumberStatus = {
      "": {
        enabled: [],
        disabled: ["stic_identification_number_c"]
      },
      default: {
        enabled: ["stic_identification_number_c"],
        disabled: []
      }
    };

    setCustomStatus(identificationNumberStatus, $("#stic_identification_type_c", "form").val());
    $("form").on("change", "#stic_identification_type_c", function() {
      clear_all_errors();
      setCustomStatus(identificationNumberStatus, $("#stic_identification_type_c", "form").val());
    });

    // Labour Insertion functionality
    if ($("#inc_incorpora_record_c", "form").is(":checked")) {
      for (var incField in STIC.incorporaRequiredFieldsArray) {
        addRequiredMark(incField, 'conditional-required-alternative');
      }
    }

    $("#inc_incorpora_record_c", "form").on("change", function() {
      if ($("#inc_incorpora_record_c", "form").is(":checked")) {
        for (var incField in STIC.incorporaRequiredFieldsArray) {
          addRequiredMark(incField, 'conditional-required-alternative');
        }
      } 
      else {
        for (var incField in STIC.incorporaRequiredFieldsArray) {
          removeRequiredMark(incField, 'conditional-required-alternative');
        }
      }
    });

    break;

  case "detail":
    // Set autofill mark beside field label
    setAutofill(["stic_age_c"]);

    // Labour Insertion functionality
    recordId = $("#formDetailView input[type=hidden][name=record]").val();
    // Define button content
    var buttons = {
      syncIncorpora: {
        id: "bt_sync_incorpora_detailview",
        title: SUGAR.language.get("app_strings", "LBL_INCORPORA_BUTTON_TITTLE"),
        onclick: "window.location='index.php?module=stic_Incorpora&action=fromDetailView&record=" + recordId + "&return_module="+ module +"'"
      },
      pdfEmail: {
        id: "bt_pdf_email_detailview",
        title: SUGAR.language.get("app_strings", "LBL_EMAIL_PDF_ACTION_BUTTON"),
        onclick: "showPopupPdfEmail()"
      }
    };
    createDetailViewButton(buttons.syncIncorpora);
    createDetailViewButton(buttons.pdfEmail);

    break;

    case "list":
        var buttons = {
            syncIncorpora: {
                id: "bt_sync_incorpora_listview",
                title: SUGAR.language.get("app_strings", "LBL_INCORPORA_BUTTON_TITTLE"),
                text: SUGAR.language.get("app_strings", "LBL_INCORPORA_BUTTON_TITTLE"),
                onclick: "onClickIncorporaSyncButton()"
            },
            // Job applications mass creation feature
            // STIC#959
            massJobApplications: {
                id: "bt_massjobapplications_listview",
                title: SUGAR.language.get("Contacts", "LBL_MASS_JOB_APPLICATIONS_BUTTON_TITTLE"),
                text: SUGAR.language.get("Contacts", "LBL_MASS_JOB_APPLICATIONS_BUTTON_TITTLE"),
                onclick: "onClickMassJobApplicationsButton()"
            }
        };
    
        createListViewButton(buttons.syncIncorpora);
        createListViewButton(buttons.massJobApplications);
        break;
  default:
    break;
}

/* AUX FUNCTIONS */

/**
 * Check the stic_identification_number_c field
 */
function checkIdentificationNumber() {
  var identificationNumberValue = getFieldValue("stic_identification_number_c");
  var identificationTypeValue = getFieldValue("stic_identification_type_c", "stic_contacts_identification_types_list");
  cl("Validating: " + identificationTypeValue + " | " + identificationNumberValue);
  switch (identificationTypeValue) {
    case "nif":
      return isValidIdentificationNumber(identificationNumberValue, "nif");
      break;
    case "nie":
      return isValidIdentificationNumber(identificationNumberValue, "nie");
      break;
    case "":
    case undefined:
      return trim(identificationNumberValue) == "";
      break;
    default:
      return trim(identificationNumberValue).length >= 1;
  }
}

/**
 * Used as a callback for the Incorpora Synchronization button
 */
function onClickIncorporaSyncButton() {
  sugarListView.get_checks();
  if(sugarListView.get_checks_count() < 1) {
      alert(SUGAR.language.get('app_strings', 'LBL_LISTVIEW_NO_SELECTED'));
      return false;
  }
  document.MassUpdate.action.value='fromMassUpdate';
  document.MassUpdate.module.value='stic_Incorpora';
  document.MassUpdate.submit();
}

/**
 * 
 * Used as a callback for the Mass Job Applications generation button
 */
function onClickMassJobApplicationsButton() {
    selectJobOfferAlert = SUGAR.language.get("Contacts", "LBL_MASS_JOB_APPLICATIONS_INFO_ALERT");
    alertListView = SUGAR.language.languages.app_strings.LBL_LISTVIEW_NO_SELECTED;
    sugarListView.get_checks();
    if (sugarListView.get_checks_count() < 1) {
        alert(alertListView);
        return false;
    }
    var win = open_popup(
        "stic_Job_Offers",
        800,
        600,
        "",
        true,
        true,
        {
            call_back_function: "setReturnMassJobApplications",
            form_name: "ListView",
            field_to_name_array: { id: "offerId", name: "offerName" },
            passthru_data: {}
        },
        "single",
        true,
        "lvso=DES&stic_Job_Offers2_STIC_JOB_OFFERS_ORDER_BY=process_end_date"
    );
    win.onload = function(){ 
      win.onbeforeunload = function() {
        SUGAR.ajaxUI.loadingPanel.hide();
        ajaxStatus.hideStatus();
      };
    }
    // In order to initialize the function loadingPanel.show(), it needs to be called the showLoadingPanel() first.
    SUGAR.ajaxUI.showLoadingPanel();
    SUGAR.ajaxUI.hideLoadingPanel();
    SUGAR.ajaxUI.loadingPanel.show();
    ajaxStatus.showStatus(selectJobOfferAlert);
    document.getElementById("ajaxStatusDiv").style.zIndex = 1040; // No need this line when this PR is merged: https://github.com/salesagility/SuiteCRM/issues/8266
}

/**
 * 
 * Runs the controller action after a job offer is selected in the job applications mass creation process
 */
function setReturnMassJobApplications(popupReplyData) {
    SUGAR.ajaxUI.loadingPanel.hide();
    ajaxStatus.hideStatus();
    sugarListView.get_checks();
    var offerId = popupReplyData.name_to_value_array.offerId;
    var offerName = popupReplyData.name_to_value_array.offerName;
    var inputId = $("<input>").attr("type", "hidden").attr("name", "offerId").attr("id", "offerId").val(offerId);
    $("#MassUpdate").append(inputId);
    var inputName = $("<input>").attr("type", "hidden").attr("name", "offerName").attr("id", "offerName").val(offerName);
    $("#MassUpdate").append(inputName);
    document.MassUpdate.action.value = "createMassJobApplications";
    document.MassUpdate.module.value = "stic_Job_Applications";
    document.MassUpdate.submit();
}

/**
 * Function to hide or show the placeholders and the value of the private area password field
 */
function privateAreaPasswordField() {
  var field = document.getElementById("stic_pa_password_c");
  if (!field) {
    return;
  }

  var config = window.STIC && window.STIC.privateAreaPassword;
  var isValueSet = field.getAttribute("data-is-value-set") === "true";
  var hasRenderedPlaceholder = !!(field.getAttribute("placeholder") || "").trim();
  var hasRenderedValue = !!field.value;
  var hasStoredPassword =
    (config && typeof config.hasStoredPassword === "boolean" ? config.hasStoredPassword : false) ||
    isValueSet ||
    hasRenderedPlaceholder ||
    hasRenderedValue ||
    field.getAttribute("data-stic-had-password") === "true";
  field.setAttribute("data-stic-had-password", hasStoredPassword ? "true" : "false");
  var currentPlaceholder = field.getAttribute("placeholder") || "";
  var placeholder =
    (config && config.placeholder) ||
    SUGAR.language.get("app_strings", "LBL_PASSWORD_SET_NEW_VALUE_TO_RESET") ||
    (SUGAR.language.languages &&
      SUGAR.language.languages.app_strings &&
      SUGAR.language.languages.app_strings.LBL_PASSWORD_SET_NEW_VALUE_TO_RESET) ||
    currentPlaceholder;

  if (field.type !== "password") {
    try {
      field.type = "password";
    } catch (e) {
      // Keep silent; some legacy browsers may throw here
    }
  }

  field.value = "";
  field.setAttribute("autocomplete", "new-password");

  if (hasStoredPassword) {
    field.setAttribute("placeholder", placeholder);
  } else {
    field.removeAttribute("placeholder");
  }

  var checkbox = document.getElementById('stic_pa_enable_c');
  if (checkbox) {
    field.disabled = !checkbox.checked;
    if (!checkbox.checked) {
      $(field).attr('readonly', true).css({'background-color': '#eeeeee', 'color': '#999999'});
    } else {
      $(field).removeAttr('readonly').css({'background-color': '', 'color': ''});
    }
  }
}

/**
 * Function to setup the private area password field when the DOM is loaded and when the field is rendered in the form
 */
function setupPrivateAreaPasswordField() {
  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", privateAreaPasswordField);
  } else {
    privateAreaPasswordField();
  }

  var observer = new MutationObserver(function(mutations, obs) {
    var field = document.getElementById("stic_pa_password_c");
    if (field) {
      privateAreaPasswordField();
    }
  });

  observer.observe(document.body, { childList: true, subtree: true });
}

/**
 * Function to enable or disable private area password field based on stic_pa_enable_c
 */
function setupPrivateAreaFields() {
  var checkbox = document.getElementById('stic_pa_enable_c');
  var password = document.getElementById('stic_pa_password_c');

  if (!checkbox || !password) {
    return;
  }

  var togglePassword = function() {
    var isEnabled = checkbox.checked;
    password.disabled = !isEnabled;
    if (!isEnabled) {
      password.value = '';
      password.removeAttribute('required');
      $(password).attr('readonly', true).css({'background-color': '#eeeeee', 'color': '#999999'});
    } else {
      $(password).removeAttr('readonly').css({'background-color': '', 'color': ''});
    }
    if (typeof privateAreaPasswordField === 'function') {
      privateAreaPasswordField();
    }
  };

  togglePassword();
  checkbox.addEventListener('change', togglePassword);
}
