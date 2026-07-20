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
/* HEADER */
// Set module name
var module = "Leads";

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

    break;

  case "detail":
    recordId = $("#formDetailView input[type=hidden][name=record]").val();
    var buttons = {
      pdfEmail: {
        id: "bt_pdf_email_detailview",
        title: SUGAR.language.get("app_strings", "LBL_EMAIL_PDF_ACTION_BUTTON"),
        onclick: "showPopupPdfEmail()"
      },
      whatsappConversation: {
        id: "bt_whatsapp_conversation_detailview",
        title: SUGAR.language.get("app_strings", "LBL_WHATSAPP_CONVERSATION_BUTTON"),
        onclick: "openWhatsAppConversation('" + recordId + "', '" + module + "')"
      }
    };
    createDetailViewButton(buttons.pdfEmail);
    createDetailViewButton(buttons.whatsappConversation);
    break;

  case "list":
    break;

  default:
    // Function that is executed when the document is loaded
    $(document).ready(function() {
    
      // Get the form of ConvertLead
      if($("#ConvertLead").length == 1){
        // Get the value of the newAccounts checkbox
        var newAccountsValue = document.getElementById("newAccounts").checked;

        // If newAccounts is checked, validates if Accountsname is not empty
        if (newAccountsValue) {
          addToValidate('ConvertLead', 'Accountsname', 'text', true, SUGAR.convert.requiredFields.Accounts.name);
        } else {
          removeFromValidate('ConvertLead', 'Accountsname');
        }
      }

    });
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
