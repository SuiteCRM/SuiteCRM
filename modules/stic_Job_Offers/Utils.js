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
var module = "stic_Job_Offers";

// Load moment.js to use in validations
loadScript("include/javascript/moment.min.js");

/* VALIDATION DEPENDENCIES */
var validationDependencies = {
  sepe_covered_date: "sepe_activation_date",
  sepe_activation_date: "sepe_covered_date",
};

/* VALIDATION CALLBACKS */
addToValidateCallback(getFormName(), "sepe_covered_date", "date", false, SUGAR.language.get(module, "LBL_SEPE_COVERED_DATE_ERROR"), function () {
  if (!getFieldValue('sepe_activation_date') && getFieldValue('sepe_covered_date')) {
    return false;
  }
});

addToValidateCallback(getFormName(), "sepe_activation_date", "date", false, SUGAR.language.get(module, "LBL_SEPE_ACTIVATION_DATE_ERROR"), function () {
  if (!getFieldValue('sepe_activation_date') && getFieldValue('sepe_covered_date')) {
    return false;
  }
});

validateFunctions.sepe_covered_date = function () {
  addToValidateCallback(getFormName(), "sepe_covered_date", "date", false, SUGAR.language.get(module, "LBL_SEPE_COVERED_DATE_ERROR"), function () {
    return checkStartAndEndDatesCoherence("sepe_activation_date", "sepe_covered_date");
  });
}

addToValidateCallback(getFormName(), "sepe_activation_date", "date", false, SUGAR.language.get(module, "LBL_SEPE_ACTIVATION_DATE_ERROR"), function () {
  return checkStartAndEndDatesCoherence("sepe_activation_date", "sepe_covered_date");
});

addToValidateMoreThan(getFormName(), "offered_positions", "int", true, SUGAR.language.languages.app_strings.ERR_INVALID_VALUE, 1);


/* VIEWS CUSTOM CODE */

switch (viewType()) {
  case "edit":
  case "quickcreate":
  case "popup":    

      // Depending on the type of assessment, add or remove validations in other fields
    type = document.getElementById('offer_type');
    manageValidatationsAndTabsIfVolunteering(type.value);
    type.addEventListener("change", function () {
        clear_all_errors();
        manageValidatationsAndTabsIfVolunteering(type.value);
    });
    // Definition of the behavior of fields that are conditionally enabled or disabled
    sepeActivationDate = {
      true: {
        enabled: ["sepe_covered_date"],
        disabled: [],
      },
      default: {
        enabled: [],
        disabled: ["sepe_covered_date"],
      },
    };

    setCustomStatus(sepeActivationDate, $("#sepe_activation_date", "form").val() ? true : false);

    // The date field can't be selected using the $() from jQuery
    document.getElementById('sepe_activation_date').onchange = function () {
      clear_all_errors();
      setCustomStatus(sepeActivationDate, $("#sepe_activation_date", "form").val() ? true : false);
    };

    if ($("#inc_incorpora_record", "form").is(":checked")) {
      for (var incField in STIC.incorporaRequiredFieldsArray) {
        addRequiredMark(incField, 'conditional-required-alternative');
      }
    }

    $("#inc_incorpora_record", "form").on("change", function() {
      if ($("#inc_incorpora_record", "form").is(":checked")) {
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
    recordId = $("#formDetailView input[type=hidden][name=record]").val();
    // Define button content
    var buttons = {
      syncIncorpora: {
        id: "bt_sync_incorpora_detailview", 
        title: SUGAR.language.get("app_strings", "LBL_INCORPORA_BUTTON_TITTLE"),
        onclick: "window.location='index.php?module=stic_Incorpora&action=fromDetailView&record=" + recordId + "&return_module="+ module + "'"
      }
    };
    createDetailViewButton(buttons.syncIncorpora);

    // Show or hide the "Notifications enabled" field is inline editing
    if (typeof window.handleSave === "function" && !window.sticJobOffersInlineSavePatched) {
      window.sticJobOffersInlineSavePatched = true;
      var originalHandleSave = window.handleSave;
      window.handleSave = function(field, id, moduleName, type) {
        if (moduleName === "stic_Job_Offers" && field === "notification_status" && type === "bool") {
          var value = getInputValue(field, type);
          if (typeof value === "undefined") {
            value = "";
          }
          saveFieldHTML(field, moduleName, id, value, "");
          var checked = (value === "on" || value === "1");
          var outputValue = "<input name='checkbox_display' class='checkbox' type='checkbox' disabled " + (checked ? "CHECKED" : "") + ">";
          setValueClose(outputValue, false);
          return;
        }

        return originalHandleSave.apply(this, arguments);
      };
    }

    break;
  case "list":
    button = {
      id: "bt_sync_incorpora_listview",
      title: SUGAR.language.get("app_strings", "LBL_INCORPORA_BUTTON_TITTLE"),
      text: SUGAR.language.get("app_strings", "LBL_INCORPORA_BUTTON_TITTLE"),
      onclick: "onClickIncorporaSyncButton()",
    };

    createListViewButton(button);
    break;

  default:
    break;
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
 * Depending on the type of offer, add or remove validations in project field
 */
function manageValidatationsAndTabsIfVolunteering(type) 
{
    const laboralTab = $('#EditView').find('ul.nav.nav-tabs li[role="presentation"] > a[data-toggle="tab"][data-label="LBL_EDITVIEW_PANEL5"]');
    const volunteeringTab = $('#EditView').find('ul.nav.nav-tabs li[role="presentation"] > a[data-toggle="tab"][data-label="LBL_EDITVIEW_PANEL8"]');

    if (type == 'volunteering') {
      // manage validations
      addToValidateCallback(getFormName(), "project_stic_job_offers_name", "varchar", true, SUGAR.language.get(module, "LBL_STIC_JOB_OFFERS_PROJECT_FROM_PROJECT_TITLE"));
      addRequiredMark('project_stic_job_offers_name');
      removeFromValidate('EditView', 'type');
      removeRequiredMark('type');
      removeFromValidate('EditView', 'sector');
      removeRequiredMark('sector');
      removeFromValidate('EditView', 'position_type');
      removeRequiredMark('position_type');
      removeFromValidate('EditView', 'contract_type');
      removeRequiredMark('contract_type');

      // manage tabs
      laboralTab.addClass('hidden');
      volunteeringTab.removeClass('hidden');
    } else {
      // manage validations
      removeFromValidate('EditView', 'project_stic_job_offers_name');
      removeRequiredMark('project_stic_job_offers_name');
      addToValidateCallback(getFormName(), "type", "varchar", true, SUGAR.language.get(module, "LBL_TYPE"));
      addRequiredMark('type');
      addToValidateCallback(getFormName(), "sector", "varchar", true, SUGAR.language.get(module, "LBL_SECTOR"));
      addRequiredMark('sector');
      addToValidateCallback(getFormName(), "position_type", "varchar", true, SUGAR.language.get(module, "LBL_POSITION_TYPE"));
      addRequiredMark('position_type');
      addToValidateCallback(getFormName(), "contract_type", "varchar", true, SUGAR.language.get(module, "LBL_CONTRACT_TYPE"));
      addRequiredMark('contract_type');

      // manage tabs
      laboralTab.removeClass('hidden');
      volunteeringTab.addClass('hidden');
    }
    return true;
}